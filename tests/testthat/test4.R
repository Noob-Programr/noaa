context("To test the functionality of eq_map")

testthat::test_that("Plotting the Earthquake Data on World Map", {

  testthat::expect_that(eq_map(eq_clean_data(eaq_data), annot_col = "Date"), testthat::is_a("leaflet"))

})
