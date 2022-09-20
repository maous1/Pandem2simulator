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
mutation_regression <- function(data, numero_mutation, mutation, dateEnd,var_names_time) {
  var_names_time = enquo(var_names_time)
  data_regression <- data %>%
    filter(!!sym(numero_mutation) != "missing") %>%
    rowwise() %>%
    mutate(tested := ifelse(test = !!sym(numero_mutation) == mutation, yes = 1, no = 0)) %>%
    select(-!!sym(numero_mutation))%>%
    mutate({{var_names_time}} := as.Date({{var_names_time}}))
  # datatested <- data_regression %>% filter(time == dateEnd)
  # if (sum(datatested$tested) / length(datatested$tested) > 0.7) {
  myreg <- glm(formula = tested ~., family = binomial(link = logit), data = data_regression)
  x <- summary(myreg)$coefficients
  return(x)
  # } else {
  # return(matrix())
  # }
}
