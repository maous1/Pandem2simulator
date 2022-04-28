#' Formatting the files from ecdc :"https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv"- and only keep usefull columns
#'
#' @return
#' @export
#' @examples
format_variant <- function(variants_aggregated, start, end) {
  date <- format_date_pandem(datedepart = start, datefin = end)
  variants_aggregated <- right_join(x = variants_aggregated, y = date, "year_week")
  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for non sequenced sequence
  no_sequenced_detected <- variants_aggregated %>%
    select(country_code, year_week, new_cases, number_sequenced, time) %>%
    distinct() %>%
    group_by(country_code, year_week, time, new_cases) %>%
    summarise(sequenced = max(number_sequenced)) %>%
    summarise(new_cases = new_cases - sequenced) %>%
    mutate(variant = "NSQ") %>%
    select(country_code, year_week, variant, new_cases, time) %>%
    filter(new_cases > 0)

  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for sequenced sequence
  detected_variants <- variants_aggregated %>%
    group_by(country_code, year_week, time, variant) %>%
    summarise(new_cases = max(number_detections_variant)) %>%
    filter(new_cases > 0)

  variants_aggregated_format <- union_all(detected_variants, no_sequenced_detected) %>% group_by(country_code, year_week, time, variant)

  return(variants_aggregated_format)
}
