#' mutation
#'
#' @param case_variants_genomic
#' @param position
#'
#' @return
#' @export
#' @examples
mutation <- function(case_variants_genomic,position = NULL,mutation = NULL) {
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
      return("No mutation")
    }
    case_variants_genomic = case_variants_genomic%>% rowwise() %>% mutate(!!as.character(position) := delmissub(position,substitutions,missing,deletions))
    return(case_variants_genomic)
  }
  if (!is.null(mutation)) {
    delmissub<- function(position,substitutions,missing,deletion)
    {
      if (grepl(pattern = position,x = substitutions)) {
        return(position)
      }
      place = as.numeric(gsub("[^0-9]", "", position))
      if (!is.na(deletion)) {


        frame2 <-  data.frame(deletions = unlist(strsplit(deletion,','))) %>%
          group_by(deletions) %>%
          mutate(start = as.numeric(unlist(strsplit(deletions, '-'))[1]))%>%
          mutate(end = as.numeric(unlist(strsplit(deletions, '-'))[2]))%>%
          mutate(end = ifelse(is.na(end),start,end))%>%
          mutate(inside = between((place),start,end))

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
          mutate(inside = between((place),start,end))


        if (any(frame3$inside==T)) {
          return("missing")
        }
      }
      return("Wild Type")
    }
    case_variants_genomic = case_variants_genomic%>% rowwise() %>% mutate(!!position := delmissub(position,substitutions,missing,deletions))
    return(case_variants_genomic)
  }
}
