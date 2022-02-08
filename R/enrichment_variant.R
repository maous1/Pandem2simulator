#' The function increases the number of convictions of a variant over an age group. The increase is managed by the multiplicateur input (example 1.3 = 30% more)
#'
#' @param data_aggregated
#' @param group
#' @param variants
#' @param multiplicateur
#'
#' @return
#' @export
#'
#' @examples
enrichment_variant <- function(data_aggregated , group , variants,multiplicateur){

  semaine = unique(data_aggregated$time)
  country = unique(data_aggregated$country_code)
  full_aggregated <- data.frame()
  for (week in semaine) {
    data_aggregated_week <- data_aggregated %>% filter(time == week)

    full_desaggregated_week <- expandRows(data_aggregated_week,count='nb',drop=T)

    pourcentage_wanted <- full_desaggregated_week %>% group_by(country_code,time,age_group,variant) %>% summarise(nb = n()) %>%
      group_by(country_code,time,age_group) %>% mutate(pourcentage = nb/sum(nb))%>% filter(variant==variants & age_group==group)

    targetted_category <- full_desaggregated_week %>% filter(age_group==group)
    other_category <- full_desaggregated_week %>% filter(age_group!=group)
    targetted_category_unwanted <- targetted_category %>% filter(variant!=variants)

    other_category_wanted <- other_category %>% filter(variant==variants)
    other_category_unwanted <- other_category %>% filter(variant!=variants)

    fullother <- rbind(other_category_wanted,other_category_unwanted)


    pourcentage_other <- fullother %>% group_by(country_code,time,age_group,variant) %>% summarise(nb = n()) %>%
      group_by(country_code,time) %>% mutate(pourcentage = nb/sum(nb))%>% filter(variant==variants)

    while (pourcentage_wanted$pourcentage < sum(pourcentage_other$pourcentage)*multiplicateur | pourcentage_wanted$pourcentage==1 |sum(pourcentage_other$pourcentage) == 0) {


      targetted_category <- full_desaggregated_week %>% filter(age_group==group)
      other_category <- full_desaggregated_week %>% filter(age_group!=group)
      targetted_category_wanted <- targetted_category %>% filter(variant==variants)
      targetted_category_unwanted <- targetted_category %>% filter(variant!=variants)

      other_category_wanted <- other_category %>% filter(variant==variants)
      other_category_unwanted <- other_category %>% filter(variant!=variants)

      row_wanted <- targetted_category_wanted%>%group_by(country_code,time)%>% slice(1)
      random = sample(1:length(targetted_category_unwanted$country_code),1)
      variant_delete <- targetted_category_unwanted%>%group_by(country_code,time) %>% slice(random)
      targetted_category_unwanted <- targetted_category_unwanted%>%group_by(country_code,time) %>% slice(-random)

      random = sample(1:length(other_category_wanted$country_code),1)
      group_delete = other_category_wanted %>%group_by(country_code,time) %>%slice(random)
      other_category_wanted <- other_category_wanted %>%group_by(country_code,time) %>%slice(-random)
      row_unwanted = tibble(country_code = country,time = week,age_group=group_delete$age_group,variant=variant_delete$variant)
      full_desaggregated_week  <- rbind(targetted_category_unwanted,targetted_category_wanted,row_wanted,other_category_wanted,other_category_unwanted,row_unwanted)
      fullother <- rbind(other_category_wanted,other_category_unwanted,row_unwanted)

      pourcentage_wanted <- full_desaggregated_week %>% group_by(country_code,time,age_group,variant) %>% summarise(nb = n()) %>%
        group_by(country_code,time,age_group) %>% mutate(pourcentage = nb/sum(nb))%>% filter(variant==variants & age_group==group)

      pourcentage_other <- fullother %>% group_by(country_code,time,age_group,variant) %>% summarise(nb = n()) %>%
        group_by(country_code,time) %>% mutate(pourcentage = nb/sum(nb))%>% filter(variant==variants)

    }

    full_aggregated_week <- full_desaggregated_week %>% group_by(age_group,variant) %>% summarise(nb=n())%>% mutate(country_code = country) %>% mutate(time = week)
    full_aggregated <- union_all(full_aggregated, full_aggregated_week)
    print(paste("Proportion in week ", week, "=",pourcentage_wanted$pourcentage/sum(pourcentage_other$pourcentage)))

  }

  return(full_aggregated)
}
