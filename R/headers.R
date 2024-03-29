as_headers <- function(x, error_call = caller_env()) {
  if (is.character(x) || is.raw(x)) {
    headers <- curl::parse_headers(x)
    headers <- headers[grepl(":", headers, fixed = TRUE)]

    equals <- regexpr(":", headers, fixed = TRUE)
    pieces <- regmatches(headers, equals, invert = TRUE)

    names <- map_chr(pieces, "[[", 1)
    values <- as.list(trimws(map_chr(pieces, "[[", 2)))

    new_headers(set_names(values, names), error_call = error_call)
  } else if (is.list(x)) {
    new_headers(x, error_call = error_call)
  } else {
    cli::cli_abort(
      "{.arg headers} must be a list, character vector, or raw.",
      call = error_call
    )
  }
}

new_headers <- function(x, error_call = caller_env()) {
  if (!is_list(x)) {
    cli::cli_abort("{.arg x} must be a list.", call = error_call)
  }
  if (length(x) > 0 && !is_named(x)) {
    cli::cli_abort("All elements of {.arg x} must be named.", call = error_call)
  }

  structure(x, class = "httr2_headers")
}

#' @export
print.httr2_headers <- function(x, ..., redact = TRUE) {
  cli::cli_text("{.cls {class(x)}}")
  if (length(x) > 0) {
    cli::cat_line(cli::style_bold(names(x)), ": ", headers_redact(x, redact))
  }
  invisible(x)
}

headers_redact <- function(x, redact = TRUE, to_redact = NULL) {
  if (!redact) {
    x
  } else {
    to_redact <- union(attr(x, "redact"), to_redact)
    attr(x, "redact") <- NULL

    list_redact(x, to_redact, case_sensitive = FALSE)
  }
}

headers_flatten <- function(x) {
  set_names(as.character(unlist(x, use.names = FALSE)), rep(names(x), lengths(x)))
}

list_redact <- function(x, names, case_sensitive = TRUE) {
  if (case_sensitive) {
    i <- match(names, names(x))
  } else {
    i <- match(tolower(names), tolower(names(x)))
  }
  x[i] <- cli::col_grey("<REDACTED>")
  x
}


#' @export
`[.httr2_headers` <- function(x, i, ...) {
  if (is.character(i)) {
    i <- match(tolower(i), tolower(names(x)))
  }

  new_headers(NextMethod())
}

#' @export
`[[.httr2_headers` <- function(x, i) {
  if (is.character(i)) {
    i <- match(tolower(i), tolower(names(x)))
  }
  NextMethod()
}

#' @export
"$.httr2_headers" <- function(x, name) {
  i <- match(tolower(name), tolower(names(x)))
  x[[i]]
}
