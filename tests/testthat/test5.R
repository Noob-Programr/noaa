context("To test the functionality of geom_timeline")

testthat::test_that("Plotting the Earthquake Data on a Timeline", {

  library(magrittr)
  d <- eaq_data %>%
        eq_clean_data()%>%
        dplyr::filter(`Country` %in% c("MEXICO") & !(is.na(Mag))) %>%
        ggplot(aes(x = Year, y = Country, size = Mag, colour = `Total Deaths`)) +
          geom_timeline(alpha = 0.3)
  testthat::expect_that(d, testthat::is_a("ggplot"))
  testthat::expect_that(d, testthat::is_a("gg"))

})
