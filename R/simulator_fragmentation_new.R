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
#'
#'
simulator_fragmentation_new <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL,factor){



  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% time]<-"time"

  if(is.numeric(factor)){
    date <- trainset %>% group_by(time)%>%
      summarise(case_week= sum(new_cases))%>%
      mutate(cum= cumsum(case_week))%>%mutate(index = floor(cum/factor)) %>% select(-c(case_week,cum))
    trainset <-left_join(trainset,date,trainset,by = "time")
    y <-left_join(y,date,trainset,by = "time")
    trainset_list <- trainset %>% split(.$index)
    testset_list <- testset %>% split(.$index)

  }else{
    trainset <- trainset%>%mutate(month = format(as.Date(time),"%Y-%m"))
    trainset_list <- trainset %>% split(.$month)
    testset <- testset%>%mutate(month = format(as.Date(time),"%Y-%m"))
    testset_list <- testset %>% split(.$month)
    common_date <- intersect(names(trainset_list),names(testset_list))
    if(is_empty(common_date)){stop("not the same period")}
    trainset_list <- trainset_list[is.element(names(trainset_list),common_date)]
    testset_list <- testset_list[is.element(names(testset_list),common_date)]
  }

  alldata <- map2_df(.x = trainset_list,.y = testset_list,function(.x,.y) simulator(trainset = .x,testset = .y,time = time,geolocalisation = geolocalisation,outcome = outcome,count=count))

  return(alldata)
}

