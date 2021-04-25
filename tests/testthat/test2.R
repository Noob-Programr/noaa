context("To check functionality of eq_clean_data ")

testthat::test_that("Add a Date Column with class Date", {

  data <- eq_clean_data(eaq_data)
  cols <- ncol(eaq_data) + 2
  testthat::expect_that(ncol(data), testthat::equals(cols))  # Checking the number of columns are same
  testthat::expect_that(nrow(data), testthat::equals(nrow(eaq_data)))  # Checking the number of rows are same
  testthat::expect_equal(length(data$Date), nrow(eaq_data))  # The column for Date is created
  testthat::expect_that(data$Longitude, testthat::is_a("numeric"))
  testthat::expect_that(data$Latitude, testthat::is_a("numeric"))
  testthat::expect_that(data$Date, testthat::is_a("Date"))

})
