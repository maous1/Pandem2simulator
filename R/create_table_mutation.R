#' create_table_mutation
#'
#' @param nextclade_data
#'
#' @return
#' @export create_table_mutation
#'
#' @examples
create_table_mutation <- function(nextclade_data){
  alldata <- data.frame()
  for (i in 1:dim(nextclade_data)[1]) {
    current <- nextclade_data[i,]

    frame <- data.frame(substitutions = unlist(strsplit(current$substitutions,split = ","))) %>%
      mutate(place = gsub("[^0-9]", "", substitutions)) %>%mutate(nucleotide = gsub("[0-9]", "", substitutions)) %>% select(-substitutions) %>%
      pivot_wider(names_from = "place",values_from = "nucleotide")

    current <- tibble::data_frame(current,frame) %>% select(-substitutions)
    alldata <- union_all(alldata,current)

  }

  alldata_deletion <- data.frame()
  for (i in 1:dim(nextclade_data)[1]) {

    current<- alldata[i,]
    frame_deletion <-  data.frame(deletions = unlist(strsplit(current$deletions,','))) %>%
      group_by(deletions) %>%
      mutate(start = as.numeric(unlist(strsplit(deletions, '-'))[1]))%>%
      mutate(end = as.numeric(unlist(strsplit(deletions, '-'))[2]))%>%
      mutate(end = ifelse(is.na(end),start,end))


    frame_missing <-  data.frame(missing = unlist(strsplit(current$missing,','))) %>%
      group_by(missing) %>%
      mutate(start = as.numeric(unlist(strsplit(missing, '-'))[1]))%>%
      mutate(end = as.numeric(unlist(strsplit(missing, '-'))[2]))%>%
      mutate(end = ifelse(is.na(end),start,end))

    if (nrow(frame_deletion) != 0) {
      vecteur_deletion = c()
      for (k in 1:length(frame_deletion$end)) {
        vecteur_deletion = c(vecteur_deletion,frame_deletion$start[k]:frame_deletion$end[k])
      }
      common <- intersect(names(current), as.character(vecteur_deletion))
      current[common]="deletion"

    }
    if (nrow(frame_missing) != 0) {
      vecteur_missing = c()
      for (k in 1:length(frame_missing$end)) {
        vecteur_missing = c(vecteur_missing,frame_missing$start[k]:frame_missing$end[k])
      }
      common <- intersect(names(current), as.character(vecteur_missing))
      current[common]="missing"

    }
    alldata_deletion = union_all(alldata_deletion,current)
  }

  alldata_deletion <- alldata_deletion%>% filter(!is.na(time)) %>%mutate_all( ~replace(., is.na(.), "wild type")) %>% select(-c(missing,deletions,insertions))

  return(alldata_deletion)
}
