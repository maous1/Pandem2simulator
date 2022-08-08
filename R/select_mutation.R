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
#' @import dplyr tibble
#' @importFrom splitstackshape expandRows
#' @examples
select_mutation<- function(data,dateStart,dateEnd,count,number)
{
  data <- mutate_all(data, ~replace(., is.na(.), "missing"))
  currentdata <- data %>%filter(between(x = as.Date(time),left = as.Date(dateStart),right = as.Date(dateEnd))) %>% expandRows(count = count, drop = T)


  datavaleur <- data.frame()
  for (variable in 6:dim(currentdata)[2]) {
    mu <- unique(currentdata[,variable])
    mu <- mu[mu!= "missing"]
    mu <- mu[mu!= "wild type"]
    mu <- mu[mu!= "deletion"]
    for (i in mu) {

      valeur<- mutation_regression(data = currentdata,numero_mutation = names(currentdata)[variable],mutation =i,dateEnd = dateEnd)

      if (dim(valeur)[1] > 1) {
        valeur <- valeur[2,]
        nom = names(valeur)
        valeur = t(matrix(valeur))
        colnames(valeur) = nom
        rownames(valeur) = paste(names(currentdata)[variable],i)
        valeur = data.frame(valeur)
        datavaleur <- union_all(datavaleur,valeur)
      }
    }
  }
  datavaleur = datavaleur %>% rownames_to_column(var = "pos_nucl")%>% mutate(position = as.numeric(gsub("[^0-9]", "", pos_nucl)))
  test = datavaleur %>%  mutate(z.value = abs(z.value))%>%arrange(z.value)%>% slice(1:number)

  data = data %>% select(c(country_code,year_week,time,age_group,variant,!!as.character(test$position))) %>%
    pivot_longer(!!as.character(test$position), names_to = "position", values_to = "mutation")
  return(data)
}
