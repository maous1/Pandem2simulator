#' disaggregate variant files
#'
#' @return disaggregate data in "data/alldata.csv"
#' @export
#' @import tidyverse
#' @examples
variant_format <- function(variants) {


  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for non sequenced sequence
  no_sequenced_detected = variants %>%
    select(country_code, year_week,new_cases,number_sequenced)%>%
    distinct()%>%
    group_by(country_code, year_week,new_cases)%>%
    summarise(sequenced = max(number_sequenced)) %>%
    summarise(new_cases = new_cases-sequenced)%>%
    mutate(variant="NSQ") %>%
    select(country_code, year_week,variant,new_cases)%>%
    filter(new_cases > 0)

  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for sequenced sequence
  detected_variants <- variants %>%
    group_by(country_code, year_week, variant) %>%
    summarise(new_cases = max(number_detections_variant)) %>%
    filter(new_cases > 0)

  variants_aggregated <- union_all(detected_variants,no_sequenced_detected) %>% group_by(country_code, year_week, variant)

  return(variants_aggregated)
}
