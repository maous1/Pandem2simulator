#' Title
#'
#' @param data_aggregated
#' @param age_group
#' @param variant
#'
#' @return
#' @export
#'
#' @examples
distortion_enrichment <- function(data_aggregated , group , variants){

  semaine = unique(data_aggregated$year_week)
  country = unique(data_aggregated$country_code)
  NSQ = data_aggregated %>% filter(variant == "NSQ")
  data_aggregated <- data_aggregated %>% filter(variant != "NSQ")
  full_aggregated <- data.frame(NSQ)
  for (week in semaine) {
    data_aggregated_week <- data_aggregated %>% filter(year_week == week)

    data_desaggregated_week <- expandRows(data_aggregated_week,count='nb',drop=T)

    targetted_category <- data_desaggregated_week %>% filter(age_group==group)
    other_category <- data_desaggregated_week %>% filter(age_group!=group)
    targetted_category_wanted <- targetted_category %>% filter(variant==variants)
    targetted_category_unwanted <- targetted_category %>% filter(variant!=variants)
    other_category_wanted <- other_category %>% filter(variant==variants)
    other_category_unwanted <- other_category %>% filter(variant!=variants)
    partial <- rbind(targetted_category_unwanted,other_category_wanted)
    partial <- partial %>% mutate(variant = sample(variant,n(),replace=F))
    full_desaggregated_week  <- rbind(partial,targetted_category_wanted,other_category_unwanted)

    full_aggregated_week <- full_desaggregated_week %>% group_by(age_group,variant) %>% summarise(nb=n())%>% mutate(country_code = country) %>% mutate(year_week = week)
    full_aggregated <- union_all(full_aggregated, full_aggregated_week)
  }

  return(full_aggregated)
}
