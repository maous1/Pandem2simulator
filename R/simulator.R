#' Predict the variant from a training set
#'
#' Predict the variant from a training set with a column time and a column variant to a testset with a column time
#' The trainset and the testset must have the columns geolocation, time, count. You define the name of these columns in input and the columns must have the same name in the 2 data sets.
#' The trainset must have in addition an outcome column that you also define in input.
#'
#' @param trainset the dataset used to train the classifier
#' @param testset the dataset to which we will add a metadata
#' @param time the name of the column where the dates are found format = "%Y-%m-%d"
#' @param geolocalisation the name of the column where the different regions are located
#' @param count the name of the column used to desaggregate the data
#' @param outcome the name of the trainset column where the metadata to be added to the testset is located
#' @param factor The number of sequence used by time for the trainset to reduce the execution time
#' @param bymonth if you want to split the trainset by month
#'
#' @return The function returns the testset dataset with an outcome column based on the trainset. The output dataset is well aggregated.
#' @export simulator
#' @import class
#' @import dplyr
#' @import purrr
#' @import lubridate
#' @importFrom splitstackshape expandRows
simulator <- function(trainset, testset, time, geolocalisation, outcome, count, factor, bymonth =T) {
  if (!any(names(trainset) %in% geolocalisation)) {
    if (!any(names(testset) %in% geolocalisation)) {
      stop("error : wrong geolocalisation in testset and trainset")
    }
    stop("error : wrong geolocalisation in  trainset")
  }
  if (!any(names(testset) %in% geolocalisation)) {
    stop("error : wrong geolocalisation in testset")
  }

  if (!any(names(trainset) %in% time)) {
    if (!any(names(testset) %in% time)) {
      stop("error : wrong time in testset and trainset")
    }
    stop("error : wrong time in  trainset")
  }
  if (!any(names(testset) %in% time)) {
    stop("error : wrong time in testset")
  }

  if (!any(names(trainset) %in% outcome)) {
    stop("error : wrong outcome in trainset")
  }

  if (!any(names(trainset) %in% count)) {
    if (!any(names(testset) %in% count)) {
      stop("error : wrong count in testset and trainset")
    }
    stop("error : wrong count in  trainset")
  }
  if (!any(names(testset) %in% count)) {
    stop("error : wrong count in testset")
  }

  names(trainset)[names(trainset) %in% geolocalisation] <- "geolocalisation"
  names(trainset)[names(trainset) %in% count] <- "new_cases"
  names(trainset)[names(trainset) %in% time] <- "time"
  names(testset)[names(testset) %in% geolocalisation] <- "geolocalisation"
  names(testset)[names(testset) %in% count] <- "new_cases"
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
  testset_nosimulated <- do.call(rbind,testset_list[!is.element(names(testset_list), common_geo)])
  testset_list <- testset_list[is.element(names(testset_list), common_geo)]


  ###### reduce the training dataset

  trainset_reduce <- function(trainset_df, factor) {
    trainset_df <- trainset_df %>%
      group_by(time) %>%
      mutate(sum = sum(new_cases)) %>%
      rowwise()%>%
      mutate(new_cases = ifelse(sum<factor,new_cases,as.integer(round(factor * new_cases / sum)))) %>%
      filter(new_cases > 0) %>%
      select(-sum)
    return(trainset_df)
  }

  trainset_list <- map(.x = trainset_list, .f = trainset_reduce, factor = factor)


  ##### fragmentation by chunk of 1 month
  if (bymonth ==T) {
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

  }




  ######### definition of the function to simulate on 1 geolocalisation

  simulator_1geo <- function(trainset_1geo, testset_1geo, time, geolocalisation, outcome, count = NULL) {
    set.seed(2)

    trainset_1geo <- trainset_1geo %>%
      expandRows(count = count, drop = T) %>%
      rowwise()%>%
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
      cl = unlist(trainset_class),
      k = 1
    )

    # Create variant variable and aggregation
    testset_1geo[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    testset_1geo <- testset_1geo %>%
      select(-c(time_jitter, time_num)) %>%
      group_by_all() %>%
      summarise(new_cases = n(), .groups = "drop")

    return(testset_1geo)
  }

  ###### apply the knn prediction on each compoent of the lists

  testset_predicted <- map2_df(.x = trainset_list, .y = testset_list, .f = function(.x, .y) simulator_1geo(trainset_1geo = .x, testset_1geo = .y, time = "time", geolocalisation = "geolocalisation", outcome = outcome, count = "new_cases"))

  testset_predicted <- union_all(testset_predicted,testset_nosimulated)
  names(testset_predicted)[names(testset_predicted) %in% "geolocalisation"] <- geolocalisation
  names(testset_predicted)[names(testset_predicted) %in% "time"] <- time
  names(testset_predicted)[names(testset_predicted) %in% "new_cases"] <- count
  return(testset_predicted)
}
