#' scopus_get_publication_by_author
#' @param data tibble with a column scopus id
#' @return entries, is a list containing the publications of each scrapped author id
#' @export

scopus_get_publication_by_author <- function(data){

  author_search_prog <- progressively(author_search, .n = nrow(data))

  entries <- data %>%
    # rename(scopus_id = dc_identifier) %>%
    # mutate(scopus_id = scopus_id %>% str_extract("\\d+")) %>%
    # select(scopus_id) %>%
    mutate(publication_information = auid %>% map(~{
      author_search_prog(au_id = .x,
                           api_key = api_key,
                           verbose = F, view = "COMPLETE", count = 25,
                           http = "https://api.elsevier.com/content/search/scopus",
                           facets = "subjarea(sort=fd)")
    }), 
    api_key = api_key, 
    retrieval_time = Sys.time()) %>%
    mutate(scrapped_ids = auid)
  return(entries)
}




# 
# #' scopus_parse_author_publication_core
# #' @param entries an entrie : json respone from get_publication_by_author
# #' @return a tibble containing the detailed information of the entries
# #' @export
# 
# scopus_parse_author_publication_core <- function(entries){
#   entries %>%
#     map2_df(.y = 1:length(entries), ~ {
#       flat_info <- .x %>%
#         map_lgl( ~ !is.list(.x)) %>%
#         .x[.] %>%
#         map(~{
#           ifelse(is.null(.x), "", .x)
#         }) %>%
#         as_tibble_c
# 
#       list_info <- .x %>%
#         map_lgl( ~ is.list(.x)) %>%
#         .x[.] %>%
#         map( ~ {
#           .x %>%
#             listify %>%
#             map_df(list_to_tibble)
#         })
#       ### trick: break
#       list_info <- list_info %>%
#         map( ~ {
#           .x %>% list %>% tibble()
#         }) %>%
#         bind_cols %>%
#         janitor::clean_names() %>%
#         set_names(names(list_info))
# 
#       out <- bind_cols(flat_info, list_info)
# 
#       return(out)
#     })
# }
# 
# #' scopus_parse_author_publication
# #' @param data tibble obtained from get_publication_by_author
# #' @return a tibble containing all information on all publications of each scraped author
# #' @export
# 
# scopus_parse_author_publication <- function(data){
#   scopus_parse_author_publication_core_pro <- progressively(scopus_parse_author_publication_core, .n = length(data$publication_information))
# 
#   data$publication_information %>%
#     map2_df(.y = data$scopus_id, ~{
#       .x$entries %>%
#         scopus_parse_author_publication_core_pro  %>%
#         mutate(scraped_id = .y)})
# }
