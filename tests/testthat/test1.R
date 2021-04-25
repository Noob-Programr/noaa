context("To check the functionality of eq_location_clean")

testthat::test_that("Cleaning the Location Name is done perfectly", {

  data <- eq_location_clean(eaq_data)
  cols <- ncol(eaq_data) + 1
  testthat::expect_that(ncol(data), testthat::equals(cols))  # Checking the number of columns are same
  testthat::expect_that(nrow(data), testthat::equals(nrow(eaq_data)))  # Checking the number of rows are same
  testthat::expect_equal(length(data$Country), nrow(eaq_data))  # The column for Country is created

})
