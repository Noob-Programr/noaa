context("To test the functionality of geom_timeline_label")

testthat::test_that("Plotting the Earthquake Data on a Timeline (annoted)", {

  library(magrittr)
  d <- eaq_data %>%
    eq_clean_data()%>%
    dplyr::filter(`Country` %in% c("MEXICO") & !(is.na(Mag))) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    ggplot(aes(x = Year, y = Country, size = Mag, colour = `Total Deaths`, label = `Location Name`)) +
    geom_timeline_label(alpha = 0.3)
  testthat::expect_that(d, testthat::is_a("ggplot"))
  testthat::expect_that(d, testthat::is_a("gg"))

})
