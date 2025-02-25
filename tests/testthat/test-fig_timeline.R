
test_that("fig_timeline() creates a valid ggplot2 figure object", {
  expect_true(ggplot2::is.ggplot(fig_timeline(clinsightful_data)))
  # TODO: check if it should error with empty dataframe? 
  #expect_error(fig_timeline(data = appdata[0,]))
})
