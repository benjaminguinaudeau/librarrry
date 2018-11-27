#' gentry_core
#' @param .x a tibble with a dc_identifier column indicating the ids to scrape
#' @return entries, is a list containing the publications of each scrapped author id
#' @export

entry_core <- function(.x){
  message(glue::glue("Retrieving entry {.x$pub_id} \t\t {.x$id} from {.x$n} \n"))
  #message(glue(" {.x$dc_title}\n "))
  response <- abstract_retrieval(id = .x$pub_id, 
                                 identifier = "scopus_id", 
                                 api_key = api_key, 
                                 verbose = F)
  
  if(response$get_statement$status_code == 429){
    #cat(glue("Quota Exceeded \n Stopped\n "))
    stop(glue("Quota Exceeded \n Stopped\n "))
  } else if (response$get_statement$status_code == 200) {
    cat("Retrieval was successfull\n \n")
    return(response$content)
  } else {
    cat("\n Retrieval failed \n\n")
    return(NULL)
  }
}

#' scopus_get_publication
#' @param data tibble with a column scopus id
#' @return entries, is a list containing the publications of each scrapped author id
#' @export


scopus_get_publication <- function(data){
  
  entry_core_prog <- progressively(entry_core, .n = nrow(data))
  
  entry <- data %>%
    #slice(1:4) %>%
  #  mutate(dc_identifier = dc_identifier %>% str_extract("\\d+")) %>%
    mutate(n = n()) %>%
    mutate(id = 1:n()) %>%
    split(1:nrow(.)) %>%
    map(entry_core)  %>% 
    tibble(entries = ., scrapped_ids = data$pub_id)
  
  return(entry)
}
