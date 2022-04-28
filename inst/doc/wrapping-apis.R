## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(httr2)

## ---- include = FALSE---------------------------------------------------------
# Seems to return 500s from time-to-time, so avoid any problems
# by only evaluating other chunks if a simple request succeeds.
faker_status_images <- request("https://fakerapi.it/api/v1") %>% 
  req_url_path_append("images") %>% 
  req_error(is_error = ~ FALSE) %>% 
  req_perform() %>% 
  resp_status()
faker_status_persons <- request("https://fakerapi.it/api/v1") %>% 
  req_url_path_append("persons") %>% 
  req_error(is_error = ~ FALSE) %>% 
  req_perform() %>% 
  resp_status()

faker_ok <- faker_status_images < 400 && faker_status_persons < 400

## ---- eval = faker_ok---------------------------------------------------------
# We start by creating a request that uses the base API url
req <- request("https://fakerapi.it/api/v1")
resp <- req %>% 
  # Then we add on the images path
  req_url_path_append("images") %>% 
  # Add query parameters _width and _quantity
  req_url_query(`_width` = 380, `_quantity` = 1) %>% 
  req_perform()

# The result comes back as JSON
resp %>% resp_body_json() %>% str()

## ---- error = TRUE, eval = faker_ok-------------------------------------------
req %>% 
  req_url_path_append("invalid") %>% 
  req_perform()

## ---- eval = faker_ok---------------------------------------------------------
resp <- last_response()
resp %>% resp_body_json()

## ---- eval = faker_ok---------------------------------------------------------
resp %>% resp_headers()

## ---- eval = faker_ok---------------------------------------------------------
req %>%
  req_user_agent("my_package_name (http://my.package.web.site)") %>% 
  req_dry_run()

## ---- eval = faker_ok---------------------------------------------------------
faker <- function(resource, ..., quantity = 1, locale = "en_US", seed = NULL) {
  params <- list(
    ...,
    quantity = quantity,
    locale = locale,
    seed = seed
  )
  names(params) <- paste0("_", names(params))
  
  request("https://fakerapi.it/api/v1") %>% 
    req_url_path_append(resource) %>% 
    req_url_query(!!!params) %>% 
    req_user_agent("my_package_name (http://my.package.web.site)") %>% 
    req_perform() %>% 
    resp_body_json()
}

str(faker("images", width = 300))

## ---- eval = faker_ok---------------------------------------------------------
faker_person <- function(gender = NULL, birthday_start = NULL, birthday_end = NULL, quantity = 1, locale = "en_US", seed = NULL) {
  faker(
    "persons",
    gender = gender,
    birthday_start = birthday_start,
    birthday_end = birthday_end,
    quantity = quantity,
    locale = locale,
    seed = seed
  )  
}
str(faker_person("male"))

## ---- eval = faker_ok---------------------------------------------------------
library(purrr)

faker_person <- function(gender = NULL, birthday_start = NULL, birthday_end = NULL, quantity = 1, locale = "en_US", seed = NULL) {
  if (!is.null(gender)) {
    gender <- match.arg(gender, c("male", "female"))
  }
  if (!is.null(birthday_start)) {
    if (!inherits(birthday_start, "Date")) {
      stop("`birthday_start` must be a date")
    }
    birthday_start <- format(birthday_start, "%Y-%m-%d")
  }
  if (!is.null(birthday_end)) {
    if (!inherits(birthday_end, "Date")) {
      stop("`birthday_end` must be a date")
    }
    birthday_end <- format(birthday_end, "%Y-%m-%d")
  }
  
  json <- faker(
    "persons",
    gender = gender,
    birthday_start = birthday_start,
    birthday_end = birthday_end,
    quantity = quantity,
    locale = locale,
    seed = seed
  )  
  
  tibble::tibble(
    firstname = map_chr(json$data, "firstname"),
    lastname = map_chr(json$data, "lastname"),
    email = map_chr(json$data, "email"),
    gender = map_chr(json$data, "gender")
  )
}
faker_person("male", quantity = 5)

## -----------------------------------------------------------------------------
key <- secret_make_key()
key

## -----------------------------------------------------------------------------
secret_scrambled <- secret_encrypt("secret I need to work with an API", key)
secret_scrambled

## -----------------------------------------------------------------------------
secret_decrypt(secret_scrambled, key)

## ---- include = FALSE---------------------------------------------------------
Sys.setenv(YOURPACKAGE_KEY = secret_make_key())

## -----------------------------------------------------------------------------
secret_scrambled <- secret_encrypt("secret I need to work with an API", "YOURPACKAGE_KEY")
secret_scrambled
secret_decrypt(secret_scrambled, "YOURPACKAGE_KEY")

## ----include = FALSE----------------------------------------------------------
has_auth <- secret_has_key("HTTR2_KEY") && 
  # Don't run in R CMD check on GHA because it hits NYTimes rate limits
  !identical(Sys.getenv("GITHUB_WORKFLOW"), "R-CMD-check")
knitr::opts_chunk$set(eval = has_auth)

## -----------------------------------------------------------------------------
my_key <- secret_decrypt("4Nx84VPa83dMt3X6bv0fNBlLbv3U4D1kHM76YisKEfpCarBm1UHJHARwJHCFXQSV", "HTTR2_KEY")

## -----------------------------------------------------------------------------
resp <- request("https://api.nytimes.com/svc/books/v3") %>% 
  req_url_path_append("/reviews.json") %>% 
  req_url_query(`api-key` = my_key, isbn = 9780307476463) %>% 
  req_perform()
resp

## -----------------------------------------------------------------------------
resp %>% 
  resp_body_json() %>% 
  str()

## ---- error = TRUE------------------------------------------------------------
resp <- request("https://api.nytimes.com/svc/books/v3") %>% 
  req_url_path_append("/reviews.json") %>% 
  req_url_query(`api-key` = "invalid", isbn = 9780307476463) %>% 
  req_perform()

## -----------------------------------------------------------------------------
resp <- last_response()
resp
resp %>% resp_body_json()

## -----------------------------------------------------------------------------
resp %>% resp_body_json() %>% .$fault %>% .$faultstring

## ---- error = TRUE------------------------------------------------------------
nytimes_error_body <- function(resp) {
  resp %>% resp_body_json() %>% .$fault %>% .$faultstring
}

resp <- request("https://api.nytimes.com/svc/books/v3") %>% 
  req_url_path_append("/reviews.json") %>% 
  req_url_query(`api-key` = "invalid", isbn = 9780307476463) %>% 
  req_error(body = nytimes_error_body) %>% 
  req_perform()

## -----------------------------------------------------------------------------
req <- request("https://api.nytimes.com/svc/books/v3") %>% 
  req_url_path_append("/reviews.json") %>% 
  req_url_query(`api-key` = "invalid", isbn = 9780307476463) %>% 
  req_throttle(10 / 60)

## -----------------------------------------------------------------------------
req <- request("https://api.nytimes.com/svc/books/v3") %>% 
  req_url_path_append("/reviews.json") %>% 
  req_url_query(`api-key` = "invalid", isbn = 9780307476463) %>% 
  req_throttle(10 / 60, realm = "https://api.nytimes.com/svc/books")

## -----------------------------------------------------------------------------
nytimes_books <- function(api_key, path, ...) {
  request("https://api.nytimes.com/svc/books/v3") %>% 
    req_url_path_append("/reviews.json") %>% 
    req_url_query(..., `api-key` = api_key) %>% 
    req_error(body = nytimes_error_body) %>% 
    req_throttle(10 / 60, realm = "https://api.nytimes.com/svc/books") %>% 
    req_perform() %>% 
    resp_body_json()
}

drunk <- nytimes_books(my_key, "/reviews.json", isbn = "0316453382")
drunk$results[[1]]$summary

## -----------------------------------------------------------------------------
get_api_key <- function() {
  key <- Sys.getenv("NYTIMES_KEY")
  if (identical(key, "")) {
    stop("No API key found, please supply with `api_key` argument or with NYTIMES_KEY env var")
  }
  key
}

## ---- eval = FALSE------------------------------------------------------------
#  nytimes_books <- function(path, ..., api_key = get_api_key()) {
#    ...
#  }

## -----------------------------------------------------------------------------
set_api_key <- function(key = NULL) {
  if (is.null(key)) {
    key <- askpass::askpass("Please enter your API key")
  }
  Sys.setenv("NYTIMES_KEY" = key)
}

## -----------------------------------------------------------------------------
get_api_key <- function() {
  key <- Sys.getenv("NYTIMES_KEY")
  if (!identical(key, "")) {
    return(key)
  }
  
  if (is_testing()) {
    return(testing_key())
  } else {
    stop("No API key found, please supply with `api_key` argument or with NYTIMES_KEY env var") 
  }
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

testing_key <- function() {
  secret_decrypt("4Nx84VPa83dMt3X6bv0fNBlLbv3U4D1kHM76YisKEfpCarBm1UHJHARwJHCFXQSV", "HTTR2_KEY")
}

## -----------------------------------------------------------------------------
token <- secret_decrypt("Guz59woxKoIO_JVtp2IzU3mFIU3ULtaUEa8xvvpYUBdVthR8jhxzc3bMZFhA9HL-ZK6YZudOI6g", "HTTR2_KEY")

## -----------------------------------------------------------------------------
req <- request("https://api.github.com/gists") %>% 
  req_headers(Authorization = paste("token", token))

req %>% req_perform()

## -----------------------------------------------------------------------------
req
req %>% req_dry_run()

## ---- error = TRUE------------------------------------------------------------
resp <- request("https://api.github.com/gists") %>% 
  req_url_query(since = "abcdef") %>% 
  req_headers(Authorization = paste("token", token)) %>% 
  req_perform()

## -----------------------------------------------------------------------------
resp <- last_response()
resp
resp %>% resp_body_json()

## -----------------------------------------------------------------------------
gist_error_body <- function(resp) {
  body <- resp_body_json(resp)
  
  message <- body$message
  if (!is.null(body$documentation_url)) {
    message <- c(message, paste0("See docs at <", body$documentation_url, ">"))
  }
  message
}
gist_error_body(resp)

## ---- error = TRUE------------------------------------------------------------
request("https://api.github.com/gists") %>% 
  req_url_query(since = "yesterday") %>% 
  req_headers(Authorization = paste("token", token)) %>% 
  req_error(body = gist_error_body) %>% 
  req_perform()

## -----------------------------------------------------------------------------
resp <- req %>% req_perform() 
resp %>% resp_headers("ratelimit")

## -----------------------------------------------------------------------------
gist_is_transient <- function(resp) {
  resp_status(resp) == 403 && 
    resp_header(resp, "X-RateLimit-Remaining") == "0"
}
gist_is_transient(resp)

## -----------------------------------------------------------------------------
gist_after <- function(resp) {
  time <- as.numeric(resp_header(resp, "X-RateLimit-Reset"))
  time - unclass(Sys.time())
}
gist_after(resp)

## -----------------------------------------------------------------------------
request("http://api.github.com") %>%
  req_retry(
    is_transient = gist_is_transient,
    after = gist_after,
    max_seconds = 60
  )

## -----------------------------------------------------------------------------
req_gist <- function(token) {
  request("https://api.github.com/gists") %>% 
    req_headers(Authorization = paste("token", token)) %>% 
    req_error(body = gist_error_body) %>% 
    req_retry(
      is_transient = gist_is_transient,
      after = gist_after
    )
}

# Check it works:
req_gist(token) %>% 
  req_perform()

## -----------------------------------------------------------------------------
req <- req_gist(token) %>% 
  req_body_json(list(
    description = "This is my cool gist!",
    files = list(test.R = list(content = "print('Hi!')")),
    public = FALSE
  ))
req %>% req_dry_run()

## -----------------------------------------------------------------------------
resp <- req %>% req_perform()
id <- resp %>% resp_body_json() %>% .$id
id

## -----------------------------------------------------------------------------
req <- req_gist(token) %>% 
  req_url_path_append(id) %>% 
  req_body_json(list(description = "This is a simple gist")) %>% 
  req_method("PATCH")
req %>% req_dry_run()

## -----------------------------------------------------------------------------
req <- req_gist(token) %>% 
  req_url_path_append(id) %>% 
  req_method("DELETE")
req %>% req_dry_run()
req %>% req_perform()

## -----------------------------------------------------------------------------
obfuscate("secret")

## -----------------------------------------------------------------------------
client <- oauth_client(
  id = "28acfec0674bb3da9f38",
  secret = obfuscated("J9iiGmyelHltyxqrHXW41ZZPZamyUNxSX1_uKnvPeinhhxET_7FfUs2X0LLKotXY2bpgOMoHRCo"),
  token_url = "https://github.com/login/oauth/access_token",
  name = "hadley-oauth-test"
)

## -----------------------------------------------------------------------------
client

## ---- eval = FALSE------------------------------------------------------------
#  token <- oauth_flow_auth_code(client, auth_url = "https://github.com/login/oauth/authorize")

## ---- eval = FALSE------------------------------------------------------------
#  request("https://api.github.com/user") %>%
#    req_auth_bearer_token(token$access_token) %>%
#    req_perform() %>%
#    resp_body_json() %>%
#    .$name
#  #> [1] "Hadley Wickham"

## ---- eval = FALSE------------------------------------------------------------
#  request("https://api.github.com/user") %>%
#    req_oauth_auth_code(client, auth_url = "https://github.com/login/oauth/authorize") %>%
#    req_perform() %>%
#    resp_body_json()

## -----------------------------------------------------------------------------
dir(rappdirs::user_cache_dir("httr2"), recursive = TRUE)

