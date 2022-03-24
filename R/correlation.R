#' Title
#'
#' @param trainset
#' @param testset
#'
#' @return
#' @export
#'
#' @examples
correlation <- function(trainset, testset) {
  train <- trainset %>%
    group_by(country_code, time) %>%
    mutate(somme = sum(new_cases)) %>%
    mutate(pourcentagetrain = new_cases / somme)

  test <- testset %>%
    group_by(country_code, time, variant) %>%
    summarise(nb = sum(nb)) %>%
    group_by(country_code, time) %>%
    mutate(somme = sum(nb)) %>%
    mutate(pourcentagetest = nb / somme)
  variant <- unique(train$variant)
  alldatatest <- c()
  alldatatrain <- c()

  for (var in variant) {
    datatrain <- train %>%
      filter(variant == var) %>%
      select(country_code, pourcentagetrain, time)
    dataset <- test %>%
      filter(variant == var) %>%
      select(country_code, pourcentagetest, time)


    vecteur <- datatrain %>%
      left_join(dataset, by = c("country_code", "time")) %>%
      mutate(pourcentagetest = ifelse(is.na(pourcentagetest), yes = 0, no = pourcentagetest))
    alldatatrain <- c(alldatatrain, vecteur$pourcentagetrain)
    alldatatest <- c(alldatatest, vecteur$pourcentagetest)
  }
  return(cor(alldatatrain, alldatatest))
}
