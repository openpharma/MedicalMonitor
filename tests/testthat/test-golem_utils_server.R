test_that("not_in works", {
  expect_true(1 %not_in% 2:10)
  expect_false(1 %not_in% 1:10)
})

test_that("not_null works", {
  expect_true(not_null(1))
  expect_false(not_null(NULL))
})

test_that("not_na works", {
  expect_true(not_na(1))
  expect_false(not_na(NA))
})

test_that("drop_nulls works", {
  expect_equal(
    drop_nulls(
      list(x = NULL, y = 2)
    ),
    list(y = 2)
  )
})

test_that("%||% works", {
  expect_equal(
    NULL %||% 1,
    1
  )
  expect_equal(
    2 %||% 1,
    2
  )
})

test_that("%|NA|% works", {
  expect_equal(
    NA %|NA|% 1,
    1
  )
  expect_equal(
    2 %|NA|% 1,
    2
  )
})

test_that("%|_|% works", {
  expect_equal(
    mtcarrs %|_|% iris,
    iris
  )
  test_wrn <- capture_output_lines(
    test_out <- mtcars %|_|% iris
  )
  expect_equal(
    test_wrn,
    'Using user supplied "mtcars" instead of deriving.'
  )
  expect_equal(
    test_out,
    mtcars
  )
  test_wrn <- capture_output_lines(
    test_out <- dplyr::mutate(mtcars, test = mpg %|_|% "RHS")
  )
  expect_equal(
    test_wrn,
    'Using user supplied "mpg" instead of deriving.'
  )
  expect_equal(
    test_out,
    cbind(mtcars, list(test = mtcars$mpg))
  )
  expect_equal(
    dplyr::mutate(mtcars, test = mpg2 %|_|% "RHS"),
    cbind(mtcars, list(test = "RHS"))
  )
})

