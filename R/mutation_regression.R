#' Title
#'
#' @param data
#' @param numero_mutation
#' @param start
#' @param end
#' @param mutation
#'
#' @return
#' @export
#' @import rlang
#' @examples
mutation_regression <- function(data,numero_mutation,start,end,mutation){
  data_regression = data %>% select(time,!!sym(numero_mutation))%>%
    filter(between(x = as.Date(time),left = as.Date(start),right = as.Date( end)))%>%
    filter(!!sym(numero_mutation) != "missing")%>%
    rowwise()%>%
    mutate(tested:= ifelse(test = !!sym(numero_mutation)==mutation,yes = 1,no = 0))%>%
    mutate(time = as.Date(time))


  myreg=glm(formula = tested~time, family=binomial(link=logit),data = data_regression)

  x = summary(myreg)$coefficients
  return(x)
}
