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
simulator_fragmentation <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL,factor){
  set.seed(2)

  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% time]<-"time"
  if(is.numeric(factor)){
    date <- as.character((trainset %>% group_by(time)%>%
                            summarise(case_week= sum(new_cases))%>%
                            mutate(cum= cumsum(case_week))%>%mutate(index = floor(cum/factor))%>%group_by(index)%>%slice(1))$time)
    date[length(date)+1] <- as.character(as.Date(as.numeric(as.Date(trainset$time[length(trainset$time)]))+1,origin = '1970-01-01'))
  }else{
    date <- unique((trainset%>%mutate(time = format(as.Date(time),"%Y-%m-01")))$time)
    date[length(date)+1] <- as.character(as.Date(as.numeric(as.Date(trainset$time[length(trainset$time)]))+1,origin = '1970-01-01'))
  }

  datedepart <- date[-length(date)]
  dateend <- date[-1]
  filtretrain <- map2(.x = datedepart,.y = dateend, function(.x,.y) trainset %>% filter(as.Date(time)>=as.Date(.x) & as.Date(time)<as.Date(.y)))
  filtretestset <- map2(.x = datedepart,.y = dateend, function(.x,.y) testset %>% filter(as.Date(time)>=as.Date(.x) & as.Date(time)<as.Date(.y)))
  alldata <- map2_df(.x = filtretrain,.y = filtretestset,function(.x,.y) simulator(trainset = .x,testset = .y,time = time,geolocalisation = geolocalisation,outcome = outcome,count=count))

return(alldata)
}

