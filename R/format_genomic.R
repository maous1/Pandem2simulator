#' Title
#'
#' @param file
#' @param olddata
#'
#' @return
#' @export
#' @import usethis
#' @import dplyr
#' @export format_genomic
#' @examples
format_genomic <- function(file,olddata=data.frame())
{
  genomic <- read.csv(file,sep = "\t")

  genomic <- genomic %>% select(collection_date, variant,substitutions,missing,deletions,insertions) %>% rename(year_week = collection_date)

  format_date_pandem <- function(start, end) {
    start <- as.Date(start)
    end <- as.Date(end)
    time <- start:end
    class(time) <- "Date"
    result <- data.frame(
      time,
      cut_Date = cut(as.Date(time), "week"),
      stringsAsFactors = FALSE
    )


    result <- result %>%
      mutate(temp = strptime(cut_Date, format = "%Y-%m-%d")) %>%
      mutate(temp2 = format(temp, format = "%Y")) %>%
      mutate(temp3 = format(temp, format = "%U")) %>%
      mutate(year_week = paste(temp2, temp3, sep = "-")) %>%
      mutate(time = cut_Date) %>%
      select(time, year_week) %>%
      distinct()

    return(result)
  }
date <- format_date_pandem(start = "2020-12-01",end = Sys.Date())

genomic <- left_join(x = genomic, y = date, "year_week") %>% filter(!is.na(time))





  genomic_data <- union_all(olddata,genomic)

 return(genomic_data)

}
