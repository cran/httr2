#' Read a streaming body a chunk at a time
#'
#' @description
#' * `resp_stream_raw()` retrieves bytes (`raw` vectors).
#' * `resp_stream_lines()` retrieves lines of text (`character` vectors).
#' * `resp_stream_sse()` retrieves a single [server-sent
#'   event](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events).
#' * `resp_stream_aws()` retrieves a single event from an AWS stream
#'   (i.e. mime type `application/vnd.amazon.eventstream``).
#'
#' Use `resp_stream_is_complete()` to determine if there is further data
#' waiting on the stream.
#'
#' @returns
#' * `resp_stream_raw()`: a raw vector.
#' * `resp_stream_lines()`: a character vector.
#' * `resp_stream_sse()`: a list with components `type`, `data`, and `id`.
#'   `type`, `data`, and `id` are always strings; `data` and `id` may be empty
#'   strings.
#' * `resp_stream_aws()`: a list with components `headers` and `body`.
#'   `body` will be automatically parsed if the event contents a `:content-type`
#'   header with `application/json`.
#'
#' `resp_stream_sse()` and `resp_stream_aws()` will return `NULL` to signal that
#' the end of the stream has been reached or, if in nonblocking mode, that
#' no event is currently available.
#' @export
#' @param resp,con A streaming [response] created by [req_perform_connection()].
#' @param kb How many kilobytes (1024 bytes) of data to read.
#' @order 1
#' @examples
#' req <- request(example_url()) |>
#'   req_template("GET /stream/:n", n = 5)
#'
#' con <- req |> req_perform_connection()
#' while (!resp_stream_is_complete(con)) {
#'   lines <- con |> resp_stream_lines(2)
#'   cat(length(lines), " lines received\n", sep = "")
#' }
#' close(con)
#'
#' # You can also see what's happening by setting verbosity
#' con <- req |> req_perform_connection(verbosity = 2)
#' while (!resp_stream_is_complete(con)) {
#'   lines <- con |> resp_stream_lines(2)
#' }
#' close(con)
resp_stream_raw <- function(resp, kb = 32) {
  check_streaming_response(resp)

  out <- resp$body$read(kb * 1024)
  if (resp_stream_show_body(resp)) {
    log_stream("Streamed ", length(out), " bytes")
    cli::cat_line()
  }
  out
}

#' @export
#' @rdname resp_stream_raw
#' @param lines The maximum number of lines to return at once.
#' @param warn Like [readLines()]: warn if the connection ends without a final
#'   EOL.
#' @order 1
resp_stream_lines <- function(resp, lines = 1, max_size = Inf, warn = TRUE) {
  check_streaming_response(resp)
  check_number_whole(lines, min = 0, allow_infinite = TRUE)
  check_number_whole(max_size, min = 1, allow_infinite = TRUE)
  check_logical(warn)

  if (lines == 0) {
    # If you want to do that, who am I to judge?
    return(character())
  }

  encoding <- resp_encoding(resp)

  lines_read <- character(0)
  while (lines > 0) {
    line <- resp_stream_oneline(resp, max_size, warn, encoding)
    if (length(line) == 0) {
      # No more data, either because EOF or req_perform_connection(blocking=FALSE).
      # Either way we're done
      break
    }
    lines_read <- c(lines_read, line)
    lines <- lines - 1
  }

  if (resp_stream_show_body(resp)) {
    log_stream(lines_read)
  }

  lines_read
}


#' @param max_size The maximum number of bytes to buffer; once this number of
#'   bytes has been exceeded without a line/event boundary, an error is thrown.
#' @export
#' @rdname resp_stream_raw
#' @order 1
resp_stream_sse <- function(resp, max_size = Inf) {
  repeat {
    event_bytes <- resp_boundary_pushback(
      resp,
      max_size,
      find_event_boundary,
      include_trailer = FALSE
    )
    if (is.null(event_bytes)) {
      return()
    }

    if (resp_stream_show_buffer(resp)) {
      log_stream(
        cli::rule("Raw server sent event"),
        "\n",
        rawToChar(event_bytes),
        prefix = "*  "
      )
    }

    event <- parse_event(event_bytes)
    if (!is.null(event)) break
  }

  if (resp_stream_show_body(resp)) {
    for (key in names(event)) {
      log_stream(cli::style_bold(key), ": ", pretty_json(event[[key]]))
    }
    cli::cat_line()
  }
  event
}

#' @export
#' @rdname resp_stream_raw
resp_stream_is_complete <- function(resp) {
  check_response(resp)
  length(resp$cache$push_back) == 0 && resp$body$is_complete()
}

#' @export
#' @param ... Not used; included for compatibility with generic.
#' @rdname resp_stream_raw
#' @order 3
close.httr2_response <- function(con, ...) {
  check_response(con)

  if (inherits(con$body, "StreamingBody")) {
    con$body$close()
  }

  invisible()
}

resp_stream_oneline <- function(resp, max_size, warn, encoding) {
  repeat {
    line_bytes <- resp_boundary_pushback(
      resp,
      max_size,
      find_line_boundary,
      include_trailer = TRUE
    )
    if (is.null(line_bytes)) {
      return(character())
    }

    eat_next_lf <- resp$cache$resp_stream_oneline_eat_next_lf
    resp$cache$resp_stream_oneline_eat_next_lf <- FALSE

    if (identical(line_bytes, as.raw(0x0A)) && isTRUE(eat_next_lf)) {
      # We hit that special edge case, see below
      next
    }

    # If ending on \r, there's a special edge case here where if the
    # next line begins with \n, that byte should be eaten.
    if (utils::tail(line_bytes, 1) == 0x0D) {
      resp$cache$resp_stream_oneline_eat_next_lf <- TRUE
    }

    # Use `resp$body` as the variable name so that if warn=TRUE, you get
    # "incomplete final line found on 'resp$body'" as the warning message
    `resp$body` <- line_bytes
    line_con <- rawConnection(`resp$body`)
    on.exit(close(line_con))

    # readLines chomps the trailing newline. I assume this is desirable.
    raw_text <- readLines(line_con, n = 1, warn = warn)

    # Use iconv to convert from whatever encoding is specified in the
    # response header, to UTF-8
    return(iconv(raw_text, encoding, "UTF-8"))
  }
}

find_line_boundary <- function(buffer) {
  if (length(buffer) == 0) {
    return(NULL)
  }

  # Look left 1 byte
  right1 <- c(utils::tail(buffer, -1), 0x00)

  crlf <- buffer == 0x0D & right1 == 0x0A
  cr <- buffer == 0x0D
  lf <- buffer == 0x0A

  all <- which(crlf | cr | lf)
  if (length(all) == 0) {
    return(NULL)
  }

  first <- all[[1]]
  if (crlf[first]) {
    return(first + 2)
  } else {
    return(first + 1)
  }
}

# Function to find the first double line ending in a buffer, or NULL if no
# double line ending is found
#
# Example:
#   find_event_boundary(charToRaw("data: 1\n\nid: 12345"))
# Returns:
#   list(
#     matched = charToRaw("data: 1\n\n"),
#     remaining = charToRaw("id: 12345")
#   )
find_event_boundary <- function(buffer) {
  if (length(buffer) < 2) {
    return(NULL)
  }

  # leftX means look behind by X bytes. For example, left1[2] equals buffer[1].
  # Any attempt to read past the beginning of the buffer results in 0x00.
  left1 <- c(0x00, utils::head(buffer, -1))
  left2 <- c(0x00, utils::head(left1, -1))
  left3 <- c(0x00, utils::head(left2, -1))

  boundary_end <- which(
    (left1 == 0x0A & buffer == 0x0A) | # \n\n
      (left1 == 0x0D & buffer == 0x0D) | # \r\r
      (left3 == 0x0D & left2 == 0x0A & left1 == 0x0D & buffer == 0x0A) # \r\n\r\n
  )

  if (length(boundary_end) == 0) {
    return(NULL) # No event boundary found
  }

  boundary_end <- boundary_end[1] # Take the first occurrence
  split_at <- boundary_end + 1 # Split at one after the boundary
  split_at
}

# Splits a buffer into the part before `split_at`, and the part starting at
# `split_at`. It's possible for either of the returned parts to be zero-length
# (i.e. if `split_at` is 1 or length(buffer)+1).
split_buffer <- function(buffer, split_at) {
  # Return a list with the event data and the remaining buffer
  list(
    matched = slice(buffer, end = split_at),
    remaining = slice(buffer, start = split_at)
  )
}

# @param max_size Maximum number of bytes to look for a boundary before throwing an error
# @param boundary_func A function that takes a raw vector and returns NULL if no
#   boundary was detected, or one position PAST the end of the first boundary in
#   the vector
# @param include_trailer If TRUE, at the end of the response, if there are
#   bytes after the last boundary, then return those bytes; if FALSE, then those
#   bytes are discarded with a warning.
resp_boundary_pushback <- function(
  resp,
  max_size,
  boundary_func,
  include_trailer
) {
  check_streaming_response(resp)
  check_number_whole(max_size, min = 1, allow_infinite = TRUE)

  chunk_size <- min(max_size + 1, 1024)

  # Grab data left over from last resp_stream_sse() call (if any)
  buffer <- resp$cache$push_back %||% raw()
  resp$cache$push_back <- raw()

  if (resp_stream_show_buffer(resp)) {
    log_stream(cli::rule("Buffer"), prefix = "*  ")
    print_buffer <- function(buf, label) {
      log_stream(
        label,
        ": ",
        paste(as.character(buf), collapse = " "),
        prefix = "*  "
      )
    }
  } else {
    print_buffer <- function(buf, label) {}
  }

  # Read chunks until we find an event or reach the end of input
  repeat {
    # Try to find an event boundary using the data we have
    print_buffer(buffer, "Buffer to parse")
    split_at <- boundary_func(buffer)

    if (!is.null(split_at)) {
      result <- split_buffer(buffer, split_at)
      # We found a complete event
      print_buffer(result$matched, "Matched data")
      print_buffer(result$remaining, "Remaining buffer")
      resp$cache$push_back <- result$remaining
      return(result$matched)
    }

    if (length(buffer) > max_size) {
      # Keep the buffer in place, so that if the user tries resp_stream_sse
      # again, they'll get the same error rather than reading the stream
      # having missed a bunch of bytes.
      resp$cache$push_back <- buffer
      cli::cli_abort(
        "Streaming read exceeded size limit of {max_size}",
        class = "httr2_streaming_error"
      )
    }

    # We didn't have enough data. Attempt to read more, but don't let us exceed
    # the max size by more than one byte; we do allow the one extra byte so we
    # know to error.
    chunk <- resp$body$read(min(chunk_size, max_size - length(buffer) + 1))
    print_buffer(chunk, "Received chunk")

    if (length(chunk) == 0) {
      if (resp$body$is_complete()) {
        # We've truly reached the end of the connection; no more data is coming
        if (length(buffer) == 0) {
          return(NULL)
        } else {
          if (include_trailer) {
            return(buffer)
          } else {
            cli::cli_warn(
              "Premature end of input; ignoring final partial chunk"
            )
            return(NULL)
          }
        }
      } else {
        # More data might come later; store the buffer and return NULL
        print_buffer(buffer, "Storing incomplete buffer")
        resp$cache$push_back <- buffer
        return(NULL)
      }
    }

    # More data was received; combine it with existing buffer and continue the
    # loop to try parsing again
    buffer <- c(buffer, chunk)
    print_buffer(buffer, "Combined buffer")
  }
}

# https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation
parse_event <- function(event_data) {
  if (is.raw(event_data)) {
    # Streams must be decoded using the UTF-8 decode algorithm.
    str_data <- rawToChar(event_data)
    Encoding(str_data) <- "UTF-8"
  } else {
    # for testing
    str_data <- event_data
  }

  # The stream must then be parsed by reading everything line by line, with a
  # U+000D CARRIAGE RETURN U+000A LINE FEED (CRLF) character pair, a single
  # U+000A LINE FEED (LF) character not preceded by a U+000D CARRIAGE RETURN
  # (CR) character, and a single U+000D CARRIAGE RETURN (CR) character not
  # followed by a U+000A LINE FEED (LF) character being the ways in
  # which a line can end.
  lines <- strsplit(str_data, "\r\n|\r|\n")[[1]]

  # When a stream is parsed, a data buffer, an event type buffer, and a
  # last event ID buffer must be associated with it. They must be initialized
  # to the empty string.
  data <- ""
  type <- ""
  last_id <- ""

  # If the line starts with a U+003A COLON character (:) - Ignore the line.
  lines <- lines[!grepl("^:", lines)]

  # If the line contains a U+003A COLON character (:)
  # * Collect the characters on the line before the first U+003A COLON
  #  character (:), and let field be that string.
  # * Collect the characters on the line after the first U+003A COLON character
  #  (:), and let value be that string. If value starts with a U+0020 SPACE
  #  character, remove it from value.
  m <- regexec("([^:]*)(: ?)?(.*)", lines)
  matches <- regmatches(lines, m)
  keys <- c("event", vapply(matches, function(x) x[2], character(1)))
  values <- c("message", vapply(matches, function(x) x[4], character(1)))

  for (i in seq_along(matches)) {
    key <- matches[[i]][2]
    value <- matches[[i]][4]

    if (key == "event") {
      # Set the event type buffer to field value.
      type <- value
    } else if (key == "data") {
      # Append the field value to the data buffer, then append a single
      # U+000A LINE FEED (LF) character to the data buffer.
      data <- paste0(data, value, "\n")
    } else if (key == "id") {
      # If the field value does not contain U+0000 NULL, then set the last
      # event ID buffer to the field value. Otherwise, ignore the field.
      last_id <- value
    }
  }

  # If the data buffer is an empty string, set the data buffer and the event
  # type buffer to the empty string and return.
  if (data == "") {
    return()
  }

  # If the data buffer's last character is a U+000A LINE FEED (LF) character,
  # then remove the last character from the data buffer.
  if (grepl("\n$", data)) {
    data <- substr(data, 1, nchar(data) - 1)
  }
  if (type == "") {
    type <- "message"
  }

  list(
    type = type,
    data = data,
    id = last_id
  )
}

# Helpers ----------------------------------------------------

check_streaming_response <- function(
  resp,
  arg = caller_arg(resp),
  call = caller_env()
) {
  check_response(resp, arg = arg, call = call)

  if (resp_body_type(resp) != "stream") {
    stop_input_type(
      resp,
      "a streaming HTTP response object",
      allow_null = FALSE,
      arg = arg,
      call = call
    )
  }

  if (!resp$body$is_open()) {
    cli::cli_abort("{.arg {arg}} has already been closed.", call = call)
  }
}

# isOpen doesn't work for two reasons:
# 1. It errors if con has been closed, rather than returning FALSE
# 2. If returns TRUE if con has been closed and a new connection opened
#
# So instead we retrieve the connection from its number and compare to the
# original connection. This works because connections have an undocumented
# external pointer.
isValid <- function(con) {
  tryCatch(
    identical(getConnection(con), con),
    error = function(cnd) FALSE
  )
}

resp_stream_show_body <- function(resp) {
  resp$request$policies$show_streaming_body %||% FALSE
}
resp_stream_show_buffer <- function(resp) {
  resp$request$policies$show_streaming_buffer %||% FALSE
}
