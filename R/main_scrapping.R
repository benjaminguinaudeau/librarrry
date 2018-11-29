#' add_authors
#' @description This function automatially update the current dataset authors by geting new information on not scrapped authors
#' @param n_batchs Corresponds to the number of batch to scrape
#' @param batch_size Corresponds to the size of the batchs (max25) 
#' @param path Path ot the current working directory
#' @param ids A tibble containing a column dc_identifier
#' @return A sample of the dataset which has been scrapped
#' @export

add_authors <- function(ids, n_batchs, batch_size, path = NULL){
  
  message(glue("Getting info on {(n_batchs +1)  * batch_size} authors\n"))
  
  step2 <- scopus_get_authors(ids = ids, n_batchs = n_batchs, 
                              batch_size = batch_size, api_key = api_key)
  
  
  transform_list_prog <- progressively(transform_list, .n = length(step2$content))
  
  #message(glue("Loading data..."))
  #load(glue("{path}/../data/authors.Rdata"))
  
  message(glue("\n\n\nParsing information...\n"))
  authors <- step2$content %>% 
    map(transform_list_prog, nam = "authors") %>%
    bind_rows %>%
    unnest(authors) %>%
    mutate(scrapped_ids = step2$scrapped_ids, 
           api_key = step2$api_key, retrieval_time = step2$retrieval_time) %>% 
    # rowwise %>% 
    # mutate(status = list(`@status`)) %>% 
    # mutate(fa = list(`@_fa`)) %>% 
    # ungroup %>% 
    # select(-`@status`, -`@_fa`) %>% 
    janitor::clean_names() #%>% 
  #bind_rows(authors) %>% filter(!duplicated(scrapped_ids))
  #message(glue("{nrow(authors) - init_len} entries were added to ~authors3.Rdata"))
  return(authors)
  
}

#' add_publications_authors
#' @description This function automatially update the current dataset "publications_authors" by geting new information on non scrapped authors
#' @param n_authors Number of authors whose publications have to be scraped
#' @param path Path ot the current working directory
#' @param data A tibble containing a column dc_identifier
#' @param shuffle Logical value, if T then order of scrape is random
#' @return A sample of the dataset which has been scrapped
#' @export

add_publications_authors <- function(data, n_authors, path = NULL, shuffle = T){
  
  if(shuffle){
    tmp <- data %>% sample_n(nrow(.))
  } else {
    tmp <- data
  }
  
  
  step3 <- tmp %>% 
    slice(1:n_authors) %>% 
    select(auid) %>%
    scopus_get_publication_by_author()
  #rm(authors)
  gc()
  
  #save(step3, file = glue("{path}/../data/tmp/step3.Rdata"))
  # message(glue("Loading data..."))
  # load(glue("{path}/../data/author_publications.Rdata"))
  author_publications <- step3$publication_information %>%
    map2_df(.y = step3 %>% 
              mutate(
                id = 1:n(), 
                total = n()) %>%
              split(1:nrow(.)),
            ~{
              message(glue("Parsing {length(.x$entries)} entries from {.y$scrapped_ids} \t\t {.y$id}/{.y$total}"))
              
              transform_list_prog <- progressively(transform_list, .n = length(.x$entries))
              
              .x$entries %>% map_df({transform_list_prog}) %>% mutate(scrapped_ids = .y$scrapped_ids, 
                                                                     api_key = .y$api_key, 
                                                                     retrieval_time = .y$retrieval_time) %>%
                janitor::clean_names(.)
            })  #%>%
    #mutate(dc_identifier  = coredata %>% map_chr(~.x$dc_identifier)) %>%
    #bind_rows(author_publications) %>%
    #filter(!duplicated(dc_identifier))
  
  #message(glue("{nrow(author_publications) - init_len} entries were added to ~author_publications.Rdata"))
  return(author_publications)
}


#' get_publication_authors
#' @param n_authors Number of authors whose publications have to be scraped
#' @param path Path ot the current working directory
#' @param data A tibble containing a column dc_identifier
#' @param shuffle Logical value, if T then order of scrape is random
#' @return Raw Scrapped Informations
#' @export

get_publication_authors <- function(data, n_authors, path = NULL, shuffle = T){
  
  if(shuffle){
    tmp <- data %>% sample_n(nrow(.))
  } else {
    tmp <- data
  }
  
  step3 <- tmp %>% 
    slice(1:n_authors) %>% 
    select(auid) %>%
    scopus_get_publication_by_author()
  #rm(authors)
  gc()
  
  return(step3)
}


#' parse_publication_authors
#' @param step1 Logical value, if T then order of scrape is random
#' @param path Path ot the current working directory
#' @return Parsed Scrapped Informations
#' @export

parse_publication_authors <- function(step1){
  
  #save(step3, file = glue("{path}/../data/tmp/step3.Rdata"))
  # message(glue("Loading data..."))
  # load(glue("{path}/../data/author_publications.Rdata"))
  author_publications <- step3$publication_information %>%
    map2_df(.y = step3 %>% 
              mutate(
                id = 1:n(), 
                total = n()) %>%
              split(1:nrow(.)),
            ~{
              message(glue("Parsing {length(.x$entries)} entries from {.y$scrapped_ids} \t\t {.y$id}/{.y$total}"))
              
              transform_list_prog <- progressively(transform_list, .n = length(.x$entries))
              
              .x$entries %>% map_df({transform_list_prog}) %>% mutate(scrapped_ids = .y$scrapped_ids, 
                                                                      api_key = .y$api_key, 
                                                                      retrieval_time = .y$retrieval_time) %>%
                janitor::clean_names(.)
            })  #%>%
  #mutate(dc_identifier  = coredata %>% map_chr(~.x$dc_identifier)) %>%
  #bind_rows(author_publications) %>%
  #filter(!duplicated(dc_identifier))
  
  #message(glue("{nrow(author_publications) - init_len} entries were added to ~author_publications.Rdata"))
  return(author_publications)
}

#' add_abstracts
#' @description This function automatially update the current dataset "publications" by geting new information on non scrapped entries
#' @param n_abstracts Number of abstracts to scrape
#' @param path Path ot the current working directory
#' @param data A tibble containing a column dc_identifier
#' @return A sample of the dataset which has been scrapped
#' @export

add_abstracts <- function(data, n_abstracts, path = NULL){
 
  step4 <- data %>%
    sample_n(nrow(.)) %>%
    slice(1:n_abstracts) %>%
    #select(dc_identifier) %>%
    scopus_get_publication
  #rm(author_publications)
  
  
  #save(step4, file = glue("{path}/../data/tmp/step4.Rdata"))
  transform_list_prog <- progressively(possibly(transform_list, otherwise = NULL), n_abstracts)
  
  # message(glue("Parsing information..."))
  
  # message(glue("Loading data..."))
  # load(glue("{path}/../data/publications.Rdata"))
  
  publications <- step4$entries %>%
    map2(.y = step4$scrapped_ids, ~{
      if(length(.x$`abstracts-retrieval-response`$coredata) == 0){return(NULL)}
      tmp <- .x$`abstracts-retrieval-response` %>% transform_list_prog(nam = "abstracts")
      if(!is.null(tmp)){
        tmp$scrapped_ids <- .y
        return(tmp)
      }
    }) %>%
    bind_rows %>%
    mutate(api_key = api_key, 
           retrieval_time = Sys.time()) %>%  
    unnest(abstracts) %>%
    mutate(dc_identifier  = coredata %>% map_chr(~.x$dc_identifier)) %>%
    # bind_rows(publications) %>%
    filter(!duplicated(dc_identifier))
  
  #message(glue("{nrow(publications) - init_len} entries were added to ~publications.Rdata"))
  return(publications)

}
