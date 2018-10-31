#' add_authors
#' @description This function automatially update the current dataset authors by geting new information on not scrapped authors
#' @param n_batchs Corresponds to the number of batch to scrape
#' @param batch_size Corresponds to the size of the batchs (max25) 
#' @param path Path ot the current working directory
#' @return A sample of the dataset which has been scrapped
#' @export

add_authors <- function(n_batchs, batch_size, path = NULL){
  
  load(glue("{path}/../data/authors_ids_to_scrape.Rdata"))
  load(glue("{path}/../data/authors.Rdata"))
  init_len <- nrow(authors)
  
  ids <- authors_ids_to_scrape %>% unique %>%
    setdiff(authors$scrapped_ids)
  
  message(glue("Getting info on {(n_batchs +1)  * batch_size} authors\n"))
  
  step2 <- scopus_get_authors(ids = ids, n_batchs = n_batchs, batch_size = batch_size, api_key = api_key) 
  #save(step2, file = glue("{path}/../data/tmp/step2.Rdata"))
  
  unlist_tibble_prog <- progressively(possibly(unlist_tibble, otherwise = NULL), .n = length(step2$content))
  
  message(glue("\n\n\nParsing information...\n"))
  
  authors <<- authors <- step2$content %>%
    map(unlist_tibble_prog) %>%
    map(~{tibblize_columns(.x)}) %>%
    bind_rows %>% 
    mutate(scrapped_ids = step2$scrapped_ids,
           api_key = step2$api_key, 
           retrieval_time = step2$retrieval_time) %>%
    bind_rows(authors) %>%
    filter(!duplicated(scrapped_ids))  
  
  save(authors, file = glue("{path}/../data/authors.Rdata"))
  message(glue("{nrow(authors) - init_len} entries were added to ~authors3.Rdata"))
  
  authors %>%
    filter(scrapped_ids %in% ids) %>%
    sample_n(min(c(50, n_batchs*batch_size)))
}

#' add_publications_authors
#' @description This function automatially update the current dataset "publications_authors" by geting new information on non scrapped authors
#' @param n_authors Number of authors whose publications have to be scraped
#' @param path Path ot the current working directory
#' @return A sample of the dataset which has been scrapped
#' @export

add_publications_authors <- function(n_authors, path = NULL){
  
  load(glue("{path}/../data/authors.Rdata"))
  load(glue("{path}/../data/author_publications.Rdata"))
  init_len <- nrow(author_publications)
  
  message(glue("Getting publications from {n_authors} authors..."))
  
  already <- author_publications$scrapped_ids
  
  step3 <- authors %>% 
    mutate(dc_identifier = coredata %>% map_chr(~.x$dc_identifier), 
           cited_by_count = coredata %>% map_chr(~.x$cited_by_count), 
           ) %>%
    dplyr::select(dc_identifier, cited_by_count) %>%
    mutate(cited_by_count = as.numeric(cited_by_count)) %>%
    mutate(dc_identifier = dc_identifier %>% str_extract("\\d+")) %>%
    filter(!dc_identifier %in% already) %>%
    #arrange(desc(cited_by_count)) %>%
    slice(1:n_authors) %>%
    scopus_get_publication_by_author() 
  
  #save(step3, file = glue("{path}/../data/tmp/step3.Rdata"))
  
  author_publications <<- author_publications <- step3$publication_information %>%
    map2_df(.y = step3 %>% 
              mutate(
                id = 1:n(), 
                total = n()) %>%
              split(1:nrow(.)),
            ~{
              message(glue("Parsing {length(.x$entries)} entries from {.y$scrapped_ids} \t\t {.y$id}/{.y$total}"))
              unlist_tibble_prog <- progressively(possibly(unlist_tibble, otherwise = NULL), .n = length(.x$entries))
              .x$entries %>% map_df({unlist_tibble_prog}) %>% mutate(scrapped_ids = .y$scrapped_ids, 
                                                                     api_key = .y$api_key, 
                                                                     retrieval_time = .y$retrieval_time)
            })  %>%
    bind_rows(author_publications) %>%
    filter(!duplicated(dc_identifier))
  
  save(author_publications, file = glue("{path}/../data/author_publications.Rdata"))
  message(glue("{nrow(author_publications) - init_len} entries were added to ~author_publications.Rdata"))
  
  return(
   # list(
   #    main = author_publications, 
    #  sample = 
        author_publications %>%
          filter(scrapped_ids %in% step3$scrapped_ids) %>%
          sample_n(min(c(50, nrow(author_publications) - init_len)))
    #)
  )
}

#' add_abstracts
#' @description This function automatially update the current dataset "publications" by geting new information on non scrapped entries
#' @param n_abstracts Number of abstracts to scrape
#' @param path Path ot the current working directory
#' @return A sample of the dataset which has been scrapped
#' @export

add_abstracts <- function(n_abstracts, path = NULL){
  
  load(glue("{path}/../data/publications.Rdata"))
  load(glue("{path}/../data/author_publications.Rdata"))
  init_len <- nrow(publications)
  
  message(glue("Getting abstract of {n_abstracts} entries..."))
  
  already <- publications$dc_identifier %>% map_chr(1)
  
  step4 <- author_publications %>%
    filter(subtype_description %in% c("Chapter", "Article", "Book", "Conference Paper")) %>%
    filter(!dc_identifier %in% already) %>%
    slice(1:n_abstracts) %>%
    scopus_get_publication
  
  #save(step4, file = glue("{path}/../data/tmp/step4.Rdata"))
  unlist_tibble_prog <- progressively(possibly(unlist_tibble, otherwise = NULL), length(step4$entries))
  
  message(glue("Parsing information..."))
  
  publications <<- publications <- step4$entries %>%
    map(~{
      .x$`abstracts-retrieval-response` %>% unlist_tibble_prog
    }) %>%
    map(~{tibblize_columns(.x)}) %>%
    bind_rows %>%
    mutate(api_key = api_key, 
           retrieval_time = Sys.time()) %>%
    bind_rows(publications) %>%
    filter(!duplicated(dc_identifier))
  
  message(glue("{nrow(publications) - init_len} entries were added to ~publications.Rdata"))
  save(publications, file = glue("{path}/../data/publications.Rdata"))
  
  publications %>%
    filter(!dc_identifier %in% already) %>%
    sample_n(min(c(50, nrow(publications) - init_len)))
}
