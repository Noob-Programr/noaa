context("To test the functionality of eq_create_label")

testthat::test_that("Annotation Labels", {

  data <- eq_clean_data(eaq_data)
  data <- eq_create_label(data)
  testthat::expect_that(data, testthat::is_a("character"))
  testthat::expect_equal(length(data), nrow(eaq_data))  # The column for Date is created

})
