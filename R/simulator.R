#' Simulator: Predict the variant from a training set with a column time and a column variant to a testset with a column time
#'
#' @param trainset the data with a column year_week and variant
#' @param testset the data where the variant will be predicted. Need a year_week column
#' @param time
#' @param geolocalisation
#' @param count
#' @param outcome
#'
#' @return
#' @export
#' @examples
#'
#'
simulator <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL){
  set.seed(2)

  names(trainset)[names(trainset) %in% geolocalisation]<-"geolocalisation"
  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% geolocalisation]<-"geolocalisation"
  names(testset)[names(testset) %in% time]<-"time"

  unique_geo <- unique(trainset$geolocalisation)

  if(length(count)>0){
    trainset <- expandRows(trainset,count=count,drop=T)
    testset <- expandRows(testset,count=count,drop=T)
  }


  stat_allgeolocalisation <- data.frame()
  # For each geolocalisation
  for (location in unique_geo) {

    # Data train
    train_geolocalisation <- trainset %>% filter(geolocalisation == location)
    train_geolocalisation <- train_geolocalisation %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_unif =time_num + runif(1,-5,5))

    V1 <- c("time_unif")
    train_geolocalisation_V1<- train_geolocalisation[V1]

    class = train_geolocalisation[outcome]


    # Data test
    test_geolocalisation <- testset %>% filter(geolocalisation == location)
    test_geolocalisation <- test_geolocalisation %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_unif =time_num + runif(1,-5,5))
    test_geolocalisation_V1 <- test_geolocalisation[V1]
    # Prediction with KNN model
    pr <- knn(train = data.frame(train_geolocalisation_V1),
              test = data.frame(test_geolocalisation_V1),
              cl = class[[1]],
              k = 1)

    # Create variant variable
    test_geolocalisation[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    test_geolocalisation <- test_geolocalisation %>%
      select(-c(time_unif, time_num)) %>% group_by_all()%>% summarise(nb=n())
    stat_allgeolocalisation <- union_all(stat_allgeolocalisation, test_geolocalisation)

  }
  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "geolocalisation"]<-geolocalisation
  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "time"]<-time
  return(stat_allgeolocalisation)
}

