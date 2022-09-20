#' Predict the variant from a training set
#'
#' Predict the variant from a training set with a column time and a column variant to a testset with a column time
#' The trainset and the testset must have the columns geolocation, time, count. You define the name of these columns in input and the columns must have the same name in the 2 data sets.
#' The trainset must have in addition an outcome column that you also define in input.
#'
#' @param trainset the dataset used to train the classifier
#' @param testset the dataset to which we will add a metadata
#' @param var_names_time the name of the column where the dates are found format = "%Y-%m-%d"
#' @param var_names_geolocalisation the name of the column where the different regions are located
#' @param var_names_count the name of the column used to desaggregate the data
#' @param var_names_outcome the name of the trainset column where the metadata to be added to the testset is located
#' @param factor The number of sequence used by time for the trainset to reduce the execution time
#' @param bymonth if you want to split the trainset by month
#'
#' @return The function returns the testset dataset with an outcome column based on the trainset. The output dataset is well aggregated.
#' @export simulator
#' @import class
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @import rlang
#' @importFrom splitstackshape expandRows

simulator <- function(trainset, testset, var_names_time, var_names_geolocalisation, var_names_outcome, var_names_count, factor, bymonth = T) {
  var_names_time <- enquo(var_names_time)
  var_names_geolocalisation <- enquo(var_names_geolocalisation)
  var_names_outcome <- enquo(var_names_outcome)
  var_names_count <- enquo(var_names_count)


  if (!any(names(trainset) %in% as_label(var_names_geolocalisation))) {
    if (!any(names(testset) %in% as_label(var_names_geolocalisation))) {
      stop("error : wrong geolocalisation in testset and trainset")
    }
    stop("error : wrong geolocalisation in  trainset")
  }
  if (!any(names(testset) %in% as_label(var_names_geolocalisation))) {
    stop("error : wrong geolocalisation in testset")
  }

  if (!any(names(trainset) %in% as_label(var_names_time))) {
    if (!any(names(testset) %in% as_label(var_names_time))) {
      stop("error : wrong time in testset and trainset")
    }
    stop("error : wrong time in  trainset")
  }
  if (!any(names(testset) %in% as_label(var_names_time))) {
    stop("error : wrong time in testset")
  }

  if (!any(names(trainset) %in% as_label(var_names_outcome))) {
    stop("error : wrong outcome in trainset")
  }

  if (!any(names(trainset) %in% as_label(var_names_count))) {
    if (!any(names(testset) %in% as_label(var_names_count))) {
      stop("error : wrong count in testset and trainset")
    }
    stop("error : wrong count in  trainset")
  }
  if (!any(names(testset) %in% as_label(var_names_count))) {
    stop("error : wrong count in testset")
  }


  ####### split the dataframe in a list of dataframe (one per geolocalisation) and keep common geolocalisation

  trainset_list <- na.omit(trainset) %>% split(.[[as_label(var_names_geolocalisation)]])
  testset_list <- na.omit(testset) %>%
    filter({{ var_names_count }} > 0) %>%
    split(.[[as_label(var_names_geolocalisation)]])

  common_geo <- intersect(names(trainset_list), names(testset_list))
  if (is_empty(common_geo)) {
    stop("not the same country")
  }

  message(paste(c("Countries exclusive to trainset : ", setdiff(names(trainset_list), names(testset_list))), collapse = " "))
  message(paste(c("Countries exclusive to testset : ", setdiff(names(testset_list), names(trainset_list))), collapse = " "))
  message(paste(c("Countries simulated : ", common_geo), collapse = " "))


  trainset_list <- trainset_list[is.element(names(trainset_list), common_geo)]
  testset_nosimulated <- do.call(rbind, testset_list[!is.element(names(testset_list), common_geo)])
  testset_list <- testset_list[is.element(names(testset_list), common_geo)]


  ###### reduce the training dataset

  trainset_reduce <- function(trainset_df, factor, var_names_count, var_names_time) {
    trainset_df <- trainset_df %>%
      group_by(!!var_names_time) %>%
      mutate(sum = sum(!!var_names_count)) %>%
      rowwise() %>%
      mutate({{ var_names_count }} := ifelse(sum < {{ factor }}, {{ var_names_count }}, as.integer(round({{ factor }} * {{ var_names_count }} / sum)))) %>%
      filter({{ var_names_count }} > 0) %>%
      select(-sum)
    return(trainset_df)
  }

  trainset_list <- map(.x = trainset_list, .f = trainset_reduce, factor = factor, var_names_count = var_names_count, var_names_time = var_names_time)

  ##### fragmentation by chunk of 1 month
  if (bymonth == T) {
    splitbychunk <- function(trainset_df, var_names_time) {
      trainset_df <- trainset_df %>%
        mutate(year_month = format(as.Date(!!var_names_time), "%Y-%m")) %>%
        split(.$year_month) %>%
        map(.f = function(x) x %>% select(-year_month))
      return(trainset_df)
    }

    trainset_list <- map(.x = trainset_list, .f = function(.x) splitbychunk(trainset_df = .x, var_names_time = var_names_time))
    testset_list <- map(.x = testset_list, .f = function(.x) splitbychunk(trainset_df = .x, var_names_time = var_names_time))
    trainset_list <- unlist(trainset_list, recursive = F)
    testset_list <- unlist(testset_list, recursive = F)

    common_date <- intersect(names(trainset_list), names(testset_list))
    if (is_empty(common_date)) {
      stop("not the same date")
    }


    message(paste(c("Date exclusive to trainset : ", setdiff(names(trainset_list), names(testset_list))), collapse = " "))
    message(paste(c("Date exclusive to testset : ", setdiff(names(testset_list), names(trainset_list))), collapse = " "))
    trainset_list <- trainset_list[is.element(names(trainset_list), common_date)]
    testset_list <- testset_list[is.element(names(testset_list), common_date)]
  }




  ######### definition of the function to simulate on 1 geolocalisation

  simulator_1geo <- function(trainset_1geo, testset_1geo, var_names_time, var_names_outcome, var_names_count = NULL) {
    set.seed(2)

    trainset_1geo <- trainset_1geo %>%
      expandRows(count = as_label(var_names_count), drop = T) %>%
      rowwise() %>%
      mutate(time_num = as.numeric(as.Date(!!var_names_time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))


    testset_1geo <- testset_1geo %>%
      expandRows(count = as_label(var_names_count), drop = T) %>%
      mutate(time_num = as.numeric(as.Date(!!var_names_time))) %>%
      mutate(time_jitter = time_num + rnorm(length(time_num), 0, 3))

    ######## knn prediction

    trainset_predictor <- trainset_1geo["time_jitter"]
    testset_predictor <- testset_1geo["time_jitter"]
    trainset_class <- trainset_1geo[as_label(var_names_outcome)]


    pr <- knn(
      train = data.frame(trainset_predictor),
      test = testset_predictor,
      cl = unlist(trainset_class),
      k = 1
    )

    # Create variant variable and aggregation
    testset_1geo[as_label(var_names_outcome)] <- as.character(pr)

    # Concatenate prediction file for all countries

    testset_1geo <- testset_1geo %>%
      select(-c(time_jitter, time_num)) %>%
      group_by_all() %>%
      summarise({{ var_names_count }} := n(), .groups = "drop")
    return(testset_1geo)
  }

  ###### apply the knn prediction on each compoent of the lists
  testset_predicted <- map2_df(.x = trainset_list, .y = testset_list, .f = function(.x, .y) simulator_1geo(trainset_1geo = .x, testset_1geo = .y, var_names_time = var_names_time, var_names_outcome = var_names_outcome, var_names_count = var_names_count))
  return(testset_predicted)
}
