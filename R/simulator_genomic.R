#' Title
#'
#' @param trainset
#' @param testset
#' @param time
#' @param geolocalisation
#' @param outcome
#' @param count
#' @param factor
#' @param bymonth
#' @param genomic_data
#' @param numberMutation
#'
#' @return
#' @export
#' @import dplyr
#' @examples
simulator_genomic <- function(trainset, testset, time, geolocalisation, outcome, count, factor, bymonth =T,genomic_data,numberMutation,dateStart,dateEnd)
{
  data_simulator <- simulator(trainset, testset, time, geolocalisation, outcome, count, factor)
  data_mutation <- find_mutation(allmutation)
  alldata <- add_genomic_data(data_simulator, data_mutation, col_merge, count, time, mutation = T)
  alldata <- alldata %>%filter(between(x = as.Date(time),left = as.Date(dateStart),right = as.Date(dateEnd)))


  alldata <- mutate_all(alldata, ~replace(., is.na(.), "absent"))
  datavaleur <- data.frame()
  for (variable in 12:dim(alldata)[2]) {
    print(variable)
    mu <- unique(alldata[,variable])
    mu <- mu[mu!= "missing"]
    mu <- mu[mu!= "absent"]
    mu <- mu[mu!= "deletion"]
    for (i in mu) {
      valeur<- mutation_regression(data = alldata,numero_mutation = names(alldata)[variable],start = "2021-05-17",end = "2021-08-01",mutation =i)
      if (dim(valeur)[1] > 1) {
        valeur <- valeur[2,]
        nom = names(valeur)
        valeur = t(matrix(valeur))
        colnames(valeur) = nom
        rownames(valeur) = paste(names(alldata)[variable],i)
        valeur = data.frame(valeur)
        datavaleur <- union_all(datavaleur,valeur)
      }
    }
  }
  test = datavaleur %>% rownames_to_column(var = "pos_nucl")%>% mutate(position = as.numeric(gsub("[^0-9]", "", pos_nucl)))
}
