test_that("can get and set url", {
  req1 <- request("http://example.com/")
  req2 <- req_url(req1, "http://foo.com:10")
  expect_equal(req_get_url(req2), "http://foo.com:10")
})


test_that("automatically adds /", {
  req1 <- request("http://example.com")
  req2 <- request("http://example.com/")

  expect_equal(
    req_url_path(req1, "/index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path(req1, "index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path(req2, "/index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path(req2, "index.html")$url,
    "http://example.com/index.html"
  )

  expect_equal(
    req_url_path_append(req1, "index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path_append(req1, "/index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path_append(req2, "index.html")$url,
    "http://example.com/index.html"
  )
  expect_equal(
    req_url_path_append(req2, "/index.html")$url,
    "http://example.com/index.html"
  )
})

test_that("can append multiple components", {
  req <- request("http://example.com/x")
  expect_equal(req_url_path(req, "a", "b")$url, "http://example.com/a/b")
  expect_equal(
    req_url_path_append(req, "a", "b")$url,
    "http://example.com/x/a/b"
  )
})

test_that("can handle empty path", {
  req <- request("http://example.com/x")
  expect_equal(req_url_path(req)$url, "http://example.com/")
  expect_equal(req_url_path_append(req)$url, "http://example.com/x")
  expect_equal(req_url_path(req, NULL)$url, "http://example.com/")
  expect_equal(req_url_path_append(req, NULL)$url, "http://example.com/x")

  expect_equal(req_url_path(req, "")$url, "http://example.com/")
  expect_equal(req_url_path_append(req, "")$url, "http://example.com/x")
})

test_that("can handle path vector", {
  req <- request("http://example.com/x")
  expect_equal(req_url_path(req, c("a", "b"))$url, "http://example.com/a/b")
  expect_equal(
    req_url_path_append(req, c("a", "b"))$url,
    "http://example.com/x/a/b"
  )
  expect_equal(
    req_url_path_append(req, c("a", "b"), NULL)$url,
    "http://example.com/x/a/b"
  )
})

test_that("can set query params", {
  req <- request("http://example.com/")
  expect_equal(
    req_url_query(req, a = 1, b = 2)$url,
    "http://example.com/?a=1&b=2"
  )
  expect_equal(
    req_url_query(req, a = 1, b = 2, c = NULL)$url,
    "http://example.com/?a=1&b=2"
  )
  expect_equal(
    req_url_query(req, !!!list(a = 1, b = 2))$url,
    "http://example.com/?a=1&b=2"
  )

  expect_equal(
    req_url_query(req, a = 1, a = 2)$url,
    "http://example.com/?a=1&a=2"
  )
  expect_equal(
    req_url_query(req, !!!list(a = 1, a = 2))$url,
    "http://example.com/?a=1&a=2"
  )
})

test_that("can control space handling", {
  req <- request("http://example.com/")
  expect_equal(req_url_query(req, a = " ")$url, "http://example.com/?a=%20")
  expect_equal(
    req_url_query(req, a = " ", .space = "form")$url,
    "http://example.com/?a=+"
  )

  expect_snapshot(
    req_url_query(req, a = " ", .space = "bar"),
    error = TRUE
  )
})

test_that("can handle multi query params", {
  req <- request("http://example.com/")

  req_url_query_multi <- function(multi) {
    req_url_query(req, a = 1:2, .multi = multi)$url
  }

  expect_snapshot(req_url_query_multi("error"), error = TRUE)

  expect_equal(req_url_query_multi("explode"), "http://example.com/?a=1&a=2")
  expect_equal(req_url_query_multi("comma"), "http://example.com/?a=1,2")
  expect_equal(req_url_query_multi("pipe"), "http://example.com/?a=1|2")
  expect_equal(req_url_query_multi(function(x) "X"), "http://example.com/?a=X")
})

test_that("errors are forwarded correctly", {
  req <- request("http://example.com/")
  expect_snapshot(error = TRUE, {
    req |> req_url_query(1)
    req |> req_url_query(a = I(1))
    req |> req_url_query(a = 1:2)
    req |> req_url_query(a = mean)
  })
})

test_that("empty query doesn't affect url", {
  req <- request("http://example.com/")
  expect_equal(req_url_query(req)$url, "http://example.com/")
  expect_equal(req_url_query(req, a = NULL)$url, "http://example.com/")
})

test_that("can modify query params iteratively", {
  req <- request("http://example.com/?a=1&b=2")
  expect_equal(req_url_query(req, c = 3)$url, "http://example.com/?a=1&b=2&c=3")
  expect_equal(req_url_query(req, a = 2)$url, "http://example.com/?b=2&a=2")
  expect_equal(
    req_url_query(req, a = 1, a = 2)$url,
    "http://example.com/?b=2&a=1&a=2"
  )
  expect_equal(req_url_query(req, b = NULL)$url, "http://example.com/?a=1")
})

test_that("can opt-out of query escaping", {
  req <- request("http://example.com/")
  expect_equal(req_url_query(req, a = I(","))$url, "http://example.com/?a=,")
})

test_that("can construct relative urls", {
  req <- request("http://example.com/a/b/c.html")
  expect_equal(req_url_relative(req, ".")$url, "http://example.com/a/b/")
  expect_equal(req_url_relative(req, "..")$url, "http://example.com/a/")
  expect_equal(req_url_relative(req, "/d/e/f")$url, "http://example.com/d/e/f")
})
# explode -----------------------------------------------------------------

test_that("explode handles expected inputs", {
  expect_equal(
    explode(list(a = NULL, b = 1, c = 2:3)),
    list(a = NULL, b = 1, c = 2, c = 3)
  )
})
