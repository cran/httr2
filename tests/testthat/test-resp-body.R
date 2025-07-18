test_that("read body from disk/memory", {
  resp1 <- request_test("base64/:value", value = "SGk=") |> req_perform()
  expect_true(resp_has_body(resp1))
  expect_equal(resp_body_raw(resp1), charToRaw("Hi"))
  expect_equal(resp_body_string(resp1), "Hi")

  resp2 <- request_test("base64/:value", value = "SGk=") |>
    req_perform(tempfile())
  expect_true(resp_has_body(resp2))
  expect_equal(resp_body_string(resp2), "Hi")
})

test_that("empty body generates error", {
  resp1 <- request_test("HEAD /get") |> req_perform()
  expect_false(resp_has_body(resp1))
  expect_snapshot(resp_body_raw(resp1), error = TRUE)

  resp2 <- request_test("HEAD /get") |> req_perform(tempfile())
  expect_false(resp_has_body(resp2))
  expect_snapshot(resp_body_raw(resp2), error = TRUE)
})

test_that("can retrieve parsed body", {
  resp <- request_test("/json") |> req_perform()
  expect_type(resp_body_json(resp), "list")

  resp <- request_test("/html") |> req_perform()
  expect_s3_class(resp_body_html(resp), "xml_document")

  resp <- request_test("/xml") |> req_perform()
  expect_s3_class(resp_body_xml(resp), "xml_document")
})

test_that("can retrieve parsed body when saved to a file", {
  path <- withr::local_tempfile()
  resp <- request_test("/json") |> req_perform(path)
  expect_type(resp_body_json(resp), "list")

  resp <- request_test("/html") |> req_perform(path)
  expect_s3_class(resp_body_html(resp), "xml_document")

  resp <- request_test("/xml") |> req_perform(path)
  expect_s3_class(resp_body_xml(resp), "xml_document")
})

test_that("resp_body_json stores parsed result", {
  resp <- request_test("/json") |> req_perform()
  json1 <- resp_body_json(resp)
  # check it's saved
  expect_length(resp$cache, 1)

  # check it's not recomputed
  json2 <- resp_body_json(resp)
  expect_true(is_reference(json2, json1))

  # check the arguments matter
  json3 <- resp_body_json(resp, simplifyVector = TRUE)
  expect_false(is_reference(json3, json1))
  expect_length(resp$cache, 2)
})

test_that("resp_body_xml stores parsed result", {
  resp <- request_test("/xml") |> req_perform()
  xml1 <- resp_body_xml(resp)
  # check it's saved
  expect_length(resp$cache, 1)

  # check it's not recomputed
  xml2 <- resp_body_xml(resp)
  expect_true(is_reference(xml2, xml1))
})

test_that("check argument types before caching", {
  expect_snapshot(error = TRUE, {
    resp_body_json(1)
    resp_body_xml(1)
  })
})

test_that("content types are checked", {
  expect_snapshot(error = TRUE, {
    request_test("/xml") |> req_perform() |> resp_body_json()
    request_test("/json") |> req_perform() |> resp_body_xml()
  })

  resp <- request_test("/json") |> req_perform()
  resp$headers$`Content-Type` <- "application/xml"
  expect_error(resp_body_json(resp))
  expect_no_error(resp_body_json(resp, check_type = FALSE))
})
