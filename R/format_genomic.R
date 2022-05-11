#' Title
#'
#' @param file
#'
#' @return
#' @export
#' @import usethis
#' @examples
format_genomic <- function(file)
{
  genomic <- read.csv(file,sep = "\t")
  data("Genomic_data_ECDCvariants")

  genomic <- genomic %>% select(collection_date, variant,substitutions,missing,deletions,insertions) %>% rename(year_week = collection_date)

  date <- format_date_pandem(start = "2020-12-01",end = Sys.Date())

  genomic <- left_join(x = genomic, y = date, "year_week") %>% filter(!is.na(time))

  usethis::use_data()

}
