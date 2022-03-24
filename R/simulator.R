#' Simulator: Predict the variant from a training set with a column time and a column variant to a testset with a column time
#'
#' @param trainset the data with a column year_week and variant
#' @param testset the data where the variant will be predicted. Need a year_week column
#' @param time
#' @param geolocalisation
#' @param count
#' @param outcome
#' @param factor
#'
#' @return
#' @export
#' @examples
simulator <- function(trainset, testset, time, geolocalisation, outcome, count, factor) {
  if (!any(names(trainset) %in% geolocalisation)) {
    if (!any(names(testset) %in% geolocalisation)) {
      stop("wrong geolocalisation in testset and trainset")
    }
    stop("wrong geolocalisation in  trainset")
  }
  if (!any(names(testset) %in% geolocalisation)) {
    stop("wrong geolocalisation in testset")
  }

  if (!any(names(trainset) %in% time)) {
    if (!any(names(testset) %in% time)) {
      stop("wrong time in testset and trainset")
    }
    stop("wrong time in  trainset")
  }
  if (!any(names(testset) %in% time)) {
    stop("wrong time in testset")
  }

  if (!any(names(trainset) %in% outcome)) {
    stop("wrong outcome in trainset")
  }

  if (!any(names(trainset) %in% count)) {
    if (!any(names(testset) %in% count)) {
      stop("wrong count in testset and trainset")
    }
    stop("wrong count in  trainset")
  }
  if (!any(names(testset) %in% count)) {
    stop("wrong count in testset")
  }

  names(trainset)[names(trainset) %in% geolocalisation] <- "geolocalisation"
  names(trainset)[names(trainset) %in% time] <- "time"
  names(testset)[names(testset) %in% geolocalisation] <- "geolocalisation"
  names(testset)[names(testset) %in% time] <- "time"

  ####### split the dataframe in a list of dataframe (one per geolocalisation) and keep common geolocalisation

  trainset_list <- trainset %>% split(.$geolocalisation)
  testset_list <- testset %>% split(.$geolocalisation)

  common_geo <- intersect(names(trainset_list), names(testset_list))
  if (is_empty(common_geo)) {
    stop("not the same country")
  }

  message(paste(c("Countries exclusive to trainset : ", setdiff(names(trainset_list), names(testset_list))), collapse = " "))
  message(paste(c("Countries exclusive to testset : ", setdiff(names(testset_list), names(trainset_list))), collapse = " "))
  message(paste(c("Countries simulated : ", common_geo), collapse = " "))


  trainset_list <- trainset_list[is.element(names(trainset_list), common_geo)]
  testset_list <- testset_list[is.element(names(testset_list), common_geo)]

  ###### reduce the training dataset

  trainset_reduce <- function(trainset_df, factor) {
    trainset_df <- trainset_df %>%
      group_by(time) %>%
      mutate(sum = sum(new_cases)) %>%
      mutate(new_cases = as.integer(round(factor * new_cases / sum))) %>%
      filter(new_cases > 0) %>%
      select(-sum)
    return(trainset_df)
  }

  trainset_list <- map(.x = trainset_list, .f = trainset_reduce, factor = factor)


  ##### fragmentation by chunk of 1 month

  splitbychunk <- function(trainset_df) {
    trainset_df <- trainset_df %>%
      mutate(year_month = format(as.Date(time), "%Y-%m")) %>%
      split(.$year_month) %>%
      map(.f = function(x) x %>% select(-year_month))
    return(trainset_df)
  }

  trainset_list <- map(.x = trainset_list, .f = splitbychunk)
  testset_list <- map(.x = testset_list, .f = splitbychunk)
  trainset_list <- unlist(trainset_list, recursive = F)
  testset_list <- unlist(testset_list, recursive = F)

  common_date <- intersect(names(trainset_list), names(testset_list))
  if (is_empty(common_geo)) {
    stop("not the same country")
  }

  message(paste(c("Date exclusive to trainset : ", setdiff(names(trainset_list), names(testset_list))), collapse = " "))
  message(paste(c("Date exclusive to testset : ", setdiff(names(testset_list), names(trainset_list))), collapse = " "))




  ######### definition of the function to simulate on 1 geolocalisation

  simulator_1geo <- function(trainset_1geo, testset_1geo, time, geolocalisation, outcome, count = NULL) {
    set.seed(2)

    trainset_1geo <- trainset_1geo %>%
      expandRows(count = count, drop = T) %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))


    testset_1geo <- testset_1geo %>%
      expandRows(count = count, drop = T) %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))

    ######## knn prediction

    trainset_predictor <- trainset_1geo["time_jitter"]
    testset_predictor <- testset_1geo["time_jitter"]
    trainset_class <- trainset_1geo[outcome]


    pr <- knn(
      train = data.frame(trainset_predictor),
      test = testset_predictor,
      cl = trainset_class$variant,
      k = 1
    )

    # Create variant variable and aggregation
    testset_1geo[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    testset_1geo <- testset_1geo %>%
      select(-c(time_jitter, time_num)) %>%
      group_by_all() %>%
      summarise(nb = n(), .groups = "drop")

    return(testset_1geo)
  }

  ###### apply the knn prediction on each compoent of the lists

  testset_predicted <- map2_df(.x = trainset_list, .y = testset_list, .f = function(.x, .y) simulator_1geo(trainset = .x, testset = .y, time = time, geolocalisation = geolocalisation, outcome = outcome, count = count))

  names(testset_predicted)[names(testset_predicted) %in% "geolocalisation"] <- geolocalisation
  names(testset_predicted)[names(testset_predicted) %in% "time"] <- time
  return(testset_predicted)
}
