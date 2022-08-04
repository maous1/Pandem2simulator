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
#'
#' @examples
simulator_genomic <- function(trainset, testset, time, geolocalisation, outcome, count, factor, bymonth =T,genomic_data,numberMutation)
{
  data_simulator <- simulator(trainset, testset, time, geolocalisation, outcome, count, factor)
  data_mutation <- find_mutation(allmutation)
  alldata <- add_genomic_data(data_simulator, data_mutation, col_merge, count, time, mutation = T)

}
