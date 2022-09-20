#' Title
#'
#' @param data
#' @param dateStart
#' @param dateEnd
#' @param count
#' @param number
#'
#' @return
#' @export
#' @import dplyr tibble rlang
#' @importFrom splitstackshape expandRows
#' @examples
select_mutation <- function(data, dateStart, dateEnd, var_names_count,var_names_time, number) {
  var_names_count <- enquo(var_names_count)
  currentdata <- data %>%
    filter(between(x = as.Date({{var_names_time}}), left = as.Date(dateStart), right = as.Date(dateEnd))) %>%
    expandRows(count =as_label(var_names_count), drop = T)

  column = colnames(currentdata)[!is.na(as.numeric(colnames(currentdata)))]
  names_metadata = colnames(currentdata)[is.na(as.numeric(colnames(currentdata)))]
  datavaleur <- data.frame()
  for (variable in column) {
    currentcolumn = currentdata %>% select({{var_names_time}},!!as.character(variable))
    mu <- unique(currentcolumn[variable])
    mu <- mu[mu != "missing"]
    mu <- mu[mu != "wild type"]
    mu <- mu[mu != "deletion"]
    for (i in mu) {
      valeur <- mutation_regression(data = currentcolumn, numero_mutation = variable, mutation = i, dateEnd = dateEnd,var_names_time = {{var_names_time}})

      if (dim(valeur)[1] > 1) {
        valeur <- valeur[2, ]
        nom <- names(valeur)
        valeur <- t(matrix(valeur))
        colnames(valeur) <- nom
        rownames(valeur) <- paste(variable, i)
        valeur <- data.frame(valeur)
        datavaleur <- union_all(datavaleur, valeur)
      }
    }

  }
  datavaleur <- datavaleur %>%
    rownames_to_column(var = "pos_nucl") %>%
    mutate(position = as.numeric(gsub("[^0-9]", "", pos_nucl)))
  test <- datavaleur %>%
    mutate(z.value = abs(z.value)) %>%
    arrange(z.value) %>%
    slice(1:number)

  data <- data %>%
    select(c(!!as.character(names_metadata), !!as.character(test$position))) %>%
    pivot_longer(!!as.character(test$position), names_to = "position", values_to = "mutation") %>%
    group_by_all() %>%
    summarise({{var_names_count}} := n(), .groups = "drop")
  return(data)
}
