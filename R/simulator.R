#' Simulator: Predict the variant from a training set with a column time and a column variant to a testset with a column time
#'
#' @param start the first week for the prediction
#' @param end the last week for the prediction
#' @param trainset the data with a column year_week and variant
#' @param testset the data where the variant will be predicted. Need a year_week column
#' @param country_code a list with the countries you want to predict. In the form of country codes. (example: Belgium = BE)
#' @param outcome
#'
#' @return
#' @export
#' @examples
#'
#'
simulator <- function(trainset, testset, start = "2021-01", end = "2022-01", country_code, outcome= "variant"){
  set.seed(1)
  trainset <- trainset %>% filter(time >= start & time <= end)
  testset <- testset %>% filter(time >= start & time <= end)
  stat_allcountry <- data.frame()
  # For each country
  for (pays in country_code) {

    # Data train
    train_country <- trainset %>% filter(country_code == pays)
    train_country$time_num <- as.numeric(factor(train_country$time))
    train_country$time_jiter <- jitter(x = train_country$time_num,
                                       factor = 0.1)
    V1 <- c("time_jiter")
    train_country_V1<- train_country[V1]

    classificateur = train_country[outcome]


    # Data test
    test_country <- testset %>%
      filter(country_code == pays)
    test_country$time_num <- as.numeric(factor(x = test_country$time,
                                               levels = levels(factor(train_country$time))))
    test_country$time_jiter <- jitter(x = test_country$time_num,
                                      factor = 0.1)
    test_country_V1 <- test_country[V1]
    # Prediction with KNN model
    pr <- knn(train = data.frame(train_country_V1),
              test = data.frame(test_country_V1),
              cl = classificateur[[1]],
              k = 1)

    # Create variant variable
    test_country[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    test_country <- test_country %>%
      select(-c(time_jiter, time_num))
    stat_allcountry <- union_all(stat_allcountry, test_country)

  }
  return(stat_allcountry)
}
