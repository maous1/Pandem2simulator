#' The function adds a column to genomic_data with a special mutation
#'
#' The function adds a column to genomic_data containing the presence or not of a mutation.
#' You have to choose between a position and a mutation, you can't complete both entries.
#'
#' @param genomic_data The dataset containing the substitutions, deletions and missing data. The form of this dataset is based on the output of nextclade.
#' @param position The position of the mutation you want to analyze. The input must be an integer. Example : 2022
#' @param mutation The mutation you want to analyze. The entry must be a character. Example : "C2022G"
#'
#' @return Adds a column to genomic_data with the analyzed mutation.
#' @export mutation
#' @import dplyr
#' @importFrom rlang :=
mutation <- function(genomic_data,position = NULL,mutation = NULL) {
  if (!is.null(position)) {
    if (!is.null(mutation)) {
      return("You can not enter position and mutation")
    }
  }

  if (!is.null(position)) {


    delmissub<- function(position,substitutions,missing,deletion)
    {
      frame1 <- data.frame(substitutions = unlist(strsplit(substitutions,','))) %>%
        mutate(place = gsub("[^0-9]", "", substitutions))%>%
        filter(place == (position))
      if (!is_empty(frame1$substitutions)) {
        return(frame1$substitutions)
      }

      if (!is.na(deletion)) {


        frame2 <-  data.frame(deletions = unlist(strsplit(deletion,','))) %>%
          group_by(deletions) %>%
          mutate(start = as.numeric(unlist(strsplit(deletions, '-'))[1]))%>%
          mutate(end = as.numeric(unlist(strsplit(deletions, '-'))[2]))%>%
          mutate(end = ifelse(is.na(end),start,end))%>%
          mutate(inside = between((position),start,end))

        if(any(frame2$inside==T)  )
        {
          return("deletion")
        }
      }
      if(!is.na(missing))
      {
        frame3 <-  data.frame(missing = unlist(strsplit(missing,','))) %>%
          group_by(missing) %>%
          mutate(start = as.numeric(unlist(strsplit(missing, '-'))[1]))%>%
          mutate(end = as.numeric(unlist(strsplit(missing, '-'))[2]))%>%
          mutate(end = ifelse(is.na(end),start,end))%>%
          mutate(inside = between((position),start,end))


        if (any(frame3$inside==T)) {
          return("missing")
        }
      }
      return("Wild Type")
    }
    genomic_data = genomic_data%>% rowwise() %>% mutate(!!as.character(position) := delmissub(position,substitutions,missing,deletions))
    return(genomic_data)
  }
  if (!is.null(mutation)) {
    delmissub<- function(mutation,substitutions,missing,deletion)
    {
      if (grepl(pattern = mutation,x = substitutions)) {
        return("Present")
      }
      place = as.numeric(gsub("[^0-9]", "", mutation))
      if (!is.na(deletion)) {


        frame2 <-  data.frame(deletions = unlist(strsplit(deletion,','))) %>%
          group_by(deletions) %>%
          mutate(start = as.numeric(unlist(strsplit(deletions, '-'))[1]))%>%
          mutate(end = as.numeric(unlist(strsplit(deletions, '-'))[2]))%>%
          mutate(end = ifelse(is.na(end),start,end))%>%
          mutate(inside = between((place),start,end))

        if(any(frame2$inside==T)  )
        {
          return("Absent")
        }
      }
      if(!is.na(missing))
      {
        frame3 <-  data.frame(missing = unlist(strsplit(missing,','))) %>%
          group_by(missing) %>%
          mutate(start = as.numeric(unlist(strsplit(missing, '-'))[1]))%>%
          mutate(end = as.numeric(unlist(strsplit(missing, '-'))[2]))%>%
          mutate(end = ifelse(is.na(end),start,end))%>%
          mutate(inside = between((place),start,end))


        if (any(frame3$inside==T)) {
          return("missing")
        }
      }
      return("Absent")
    }
    genomic_data = genomic_data%>% rowwise() %>% mutate(!!mutation := delmissub(mutation,substitutions,missing,deletions))
    return(genomic_data)
  }
}
