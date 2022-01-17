#' disaggregate variant files
#'
#' @return disaggregate data in "data/alldata.csv"
#' @export
#' @import tidyverse
#' @examples
Table_data <- function() {

  variants <- read.csv("data/ecdc/variants.csv")

  # rm BG country as no daily cases from ecdc (see below) are not given by age for this country
  variants <- variants %>%
    filter(country_code != "BG")

  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for non sequenced sequence
  no_sequenced_detected = variants %>%
    select(country_code, year_week,new_cases,number_sequenced)%>%
    distinct()%>%
    group_by(country_code, year_week,new_cases)%>%
    summarise(sequenced = max(number_sequenced)) %>%
    summarise(n = new_cases-sequenced)%>%
    mutate(variant="NSQ") %>%
    select(country_code, year_week,variant,n)%>%
    filter(n > 0)

  # GISAID and TESSy sources are encoded separately
  # => Merge both sources for sequenced sequence
  detected_variants <- variants %>%
    group_by(country_code, year_week, variant) %>%
    summarise(n = max(number_detections_variant)) %>%
    filter(n > 0)

  # Create table with one variant per line
  variant_data <- tibble()
  no_sequenced_data <- tibble()
  entries_per_variant <- function(x){
    country <- x[1]
    yw <- x[2]
    var <- x[3]
    n <- x[4]
    tibble(country_code = rep(country, n),
           year_week = rep(yw, n),
           variant = rep(var, n))
  }
  variant_data <- bind_rows(apply(detected_variants, 1, entries_per_variant))
  no_sequenced_data <- bind_rows(apply(no_sequenced_detected, 1, entries_per_variant))

  # Final table with variant and no sequenced data
  alldata = union_all(variant_data,no_sequenced_data) %>% group_by(country_code, year_week, variant)

  # Save final table (alldata)
  write_csv(alldata,"data/alldata.csv")
  return(alldata)
}
