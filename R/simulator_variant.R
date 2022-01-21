#' Simulator: Predict the variant
#'
#' @param start
#' @param end
#' @param trainset
#' @param testset
#' @param country_code
#'
#' @return
#' @export
#' @import tidyverse
#' @import class
#' @examples
#'
#'
simulator_variant <- function(trainset, testset, start = "2021-01", end = "2022-01", country_code){


  testset <- testset %>% filter(year_week > start & year_week < end)

  # For each country
  stat_allcountry <- data.frame()
  for (pays in country_code) {

    # Data train
    train_country <- trainset %>% filter(country_code == pays)
    train_country$year_week_num <- as.numeric(factor(train_country$year_week))
    train_country$year_week_jiter <- jitter(train_country$year_week_num, factor = 0.1)

    # Data test
    test_country <- testset %>% filter(country_code == pays)
    test_country$year_week_num <- as.numeric(factor(test_country$year_week,
                                                    levels = levels(factor(train_country$year_week))))
    test_country$year_week_jiter <- jitter(test_country$year_week_num,factor = 0.1)

    # Prediction with KNN model
    pr <- knn(train = train_country$year_week_jiter,
              test = data.frame(year_week = test_country$year_week_jiter),
              cl = train_country$variant,
              k = 1)

    # Create variant variable
    test_country$variant <- as.character(pr)

    # Concatenate prediction file for all countries

    test_country <- test_country %>%
      select(-c(year_week_jiter, year_week_num))
    stat_allcountry <- union_all(stat_allcountry, test_country)

  }
  return(stat_allcountry)
}
