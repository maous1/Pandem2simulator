#' mutation
#'
#' @param case_variants_genomic
#' @param numero
#'
#' @return
#' @export
#' @examples
mutation <- function(case_variants_genomic,numero) {
  delmissub<- function(numero,substitutions,missing,deletion)
  {
    startspike = 21562+numero
    frame1 <- data.frame(substitutions = unlist(strsplit(substitutions,','))) %>%
      mutate(place = gsub("[^0-9]", "", substitutions))%>%
      filter(place == (startspike))%>%
      mutate(letter = gsub("[0-9]", "", substitutions) )
    if (!is_empty(frame1$substitutions)) {
      return(frame$letter)
    }

    if (!is.na(deletion)) {


      frame2 <-  data.frame(deletions = unlist(strsplit(deletion,','))) %>%
        group_by(deletions) %>%
        mutate(start = as.numeric(unlist(strsplit(deletions, '-'))[1]))%>%
        mutate(end = as.numeric(unlist(strsplit(deletions, '-'))[2]))%>%
        mutate(end = ifelse(is.na(end),start,end))%>%
        mutate(inside = between((startspike),start,end))

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
        mutate(inside = between((startspike),start,end))


      if (any(frame3$inside==T)) {
        return("missing")
      }
    }
    return("No mutation")
  }

  frame <- data.frame()
  for (i in 1:length(case_variants_genomic$substitutions)) {
    test = case_variants_genomic%>% slice(i) %>% mutate(!!as.character(numero) := delmissub(numero,substitutions,missing,deletions))
    frame= union_all(frame,test)
  }



  return(frame)
}
