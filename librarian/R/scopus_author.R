#' scopus_get_authors
#' @param ids vector of ids author ids to scrape
#' @param n_batchs number of batch to scrape
#' @param batch_size size of scraped batch. Max 25
#' @param api_key api_key for Scopus API
#' @return response is a list with two elements. scrapped_ids a vector with the scrapped ids. content is the parsed json response.
#' @export

scopus_get_authors <- function(ids = NULL, n_batchs = 99, batch_size = 25, api_key = NULL){
  if(is.null(api_key)){message("API KEY missing") ; return()}
  if(batch_size > 25){message("Batch_size set to maximum (25)") ; batch_size <- 25}

  ids_batch <- ids  %>%
    tibble(id = .) %>%
    mutate(index = 1:n()) %>%
    mutate(batch = ((index-1) %/% batch_size)+1) %>%
    filter(batch %in% 1:n_batchs) %>%
    na.omit %>%
    split(.$batch)

  response <- ids_batch %>%
    map(~{
      message(glue::glue("Retrieving batch {unique(.x$batch)}"))
      tmp <- rscopus::complete_multi_author_info(au_id = .x$id,
                                                 all_author_info = T,
                                                 api_key = api_key, 
                                                 verbose = F)
      if(tmp$get_statement$status_code == 429){
        cat(glue("Quota Exceeded \n Stopped at index {.x$batch}"))
        return(break)
      } else if (tmp$get_statement$status_code == 200) {
        cat("\t\t Retrieval was successfull\n \n")
        return(tmp)
      } else {
        cat("\t\t Retrieval failed \n")
        return(NULL)
      }
    }) %>%
    map(~{
      list(content = .x$content$`author-retrieval-response-list`$`author-retrieval-response`,
           scrapped_ids = str_split(.x$au_id, ",") %>% unlist)
      }) %>%
    reduce(bind_rows) %>%
    mutate(
      api_key = api_key, 
      retrieval_time = Sys.time()
    )
  return(response)
}


#' 
#' #' scopus_parse_authors
#' #' @param data a list containing the json response of the API
#' #' @return The function returns a tibble containing the parsed information of authors.
#' #' @export
#' 
#' scopus_parse_authors <- function(data){
#'   scopus_parse_author_information_prog <- progressively(scopus_parse_author_information, .n = length(data$content))
#' 
#'   data$content %>%
#'     map2_df(.y = as.list(data$scrapped_ids),~{
#'       .x %>%
#'         scopus_parse_author_information_prog %>%
#'         mutate(scrapped_ids = .y)
#'     }) %>%
#'     mutate(scopus_id = str_extract(dc_identifier, "\\d+")) %>%
#'     left_join(data %>% filter(scrapped_ids, api_key, retrieval_time), by(scrapped_ids))
#' }
#' 
#' #' scopus_parse_author_information
#' #' @param data a list containing the json response of the API
#' #' @return The function parsed the coredata element of the Json response
#' #' @export
#' 
#' scopus_parse_author_information <- function(data){
#' 
#'   coredata <- data$coredata %>%
#'     as_tibble_c %>%
#'     mutate(link = link %>%
#'              map_df(as_tibble_c) %>%
#'              list
#'     ) %>%
#'     slice(1)
#' 
#'   x <-  ifelse(length(data$`author-profile`) != 0, data$`author-profile` , NA)
#'   if(is.na(x)[1]){return(tibble(author_names_variant = list()))}
#' 
#'   if(is.null(x$`name-variant`)){author_name_pref <- tibble(x = NA)} else {
#'     author_name_pref <- x$`preferred-name` %>%
#'       as_tibble_c()
#'   }
#' 
#'   if(is.null(x$`name-variant`)){author_names_variant <- list(author_name_pref)} else {
#'     author_names_variant <- x$`name-variant` %>%
#'       listify %>%
#'       map_df(~{.x %>%
#'           map_if(is.null, ~"") %>%
#'           as_tibble_c}) %>%
#'       mutate(pref = F) %>%
#'       bind_rows(
#'         author_name_pref %>%
#'           mutate(pref = T)
#'       ) %>%
#'       list
#'   }
#' 
#'   if(is.null(x$`publication-range`)){author_range <- NA} else {
#'     author_range <- x$`publication-range` %>%
#'       as_tibble %>%
#'       janitor::clean_names() %>%
#'       mutate_all(as.numeric) %>%
#'       list
#'   }
#' 
#'   if(is.null(x$classificationgroup$classifications)){area_count <- tibble(x = NA_character_)} else {
#'     aera_count <- x$classificationgroup$classifications %>%
#'       listify %>%
#'       map_df(as_tibble_c) %>%
#'       mutate(classification = classification %>%
#'                map(~{.x %>%
#'                    as_tibble %>%
#'                    janitor::clean_names()
#'                }
#'                )
#'       ) %>%
#'       unnest(classification) %>%
#'       janitor::clean_names() %>%
#'       rename_if_included("value", "x")
#'   }
#' 
#'   if(is.null(data$`subject-areas`)){subject_area <- list(area_count %>% rename(code = x))} else {
#'     subject_area <- data$`subject-areas` %>%
#'       listify %>%
#'       map_df(~{
#'         .x %>%
#'           map_df(as_tibble) %>%
#'           janitor::clean_names()
#'       }) %>%
#'       left_join(area_count, by = c("code" = "x")) %>%
#'       list
#'   }
#' 
#'   if(is.null(x$`journal-history`$journal)){author_journal_history <- NA} else {
#'     author_journal_history <- x$`journal-history`$journal %>%
#'       map_df(~{
#'         .x %>%
#'           as_tibble %>%
#'           janitor::clean_names()
#'       }) %>%
#'       list
#'   }
#' 
#'   if(is.null(x$`affiliation-current`$affiliation)){aff_current <- NA} else {
#'     aff_current <- x$`affiliation-current`$affiliation %>%
#'       listify %>%
#'       map_df(~{.x %>%
#'           as_tibble_c %>%
#'           mutate(ip_doc = ip_doc %>%
#'                    listify %>%
#'                    map(~{.x %>%
#'                        unlist %>%
#'                        as.list %>%
#'                        as_tibble_c()
#'                    })) %>%
#'           slice(1)
#'       }) %>%
#'       list
#'   }
#' 
#'   if(is.null(x$`affiliation-history`$affiliation)){aff_history <- NA} else {
#'     aff_history <- x$`affiliation-history`$affiliation %>%
#'       listify %>%
#'       map(~{.x %>%
#'           as_tibble_c() %>%
#'           mutate(ip_doc = ip_doc %>%
#'                    listify %>%
#'                    map(~{.x %>%
#'                        unlist %>%
#'                        as.list %>%
#'                        as_tibble_c()
#'                    })) %>%
#'           slice(1)
#'       }) %>%
#'       list
#'   }
#' 
#'   return(
#'     coredata %>%
#'       bind_cols(tibble(
#'         author_names_variant,
#'         author_range,
#'         subject_area,
#'         author_journal_history,
#'         aff_current,
#'         aff_history
#'       )
#'       )
#'   )
#' }
