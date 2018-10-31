#' scopus_clean_authors
#' @param authors a tibble containing authors to clean (response from authors scrapping)
#' @return the cleaned tibble
#' @export

scopus_clean_authors <- function(authors){
  authors %>% 
    slice(1:100) %>% 
    dplyr::select(coredata, affiliation_current, affiliation_history, subject_areas, author_profile) %>%
    unnest(coredata) %>% 
    mutate(scopus_id = dc_identifier %>% str_extract("\\d+")) %>% 
    mutate(aff_current_id = map_chr(affiliation_current, ~.x$id)) %>%
    select(-affiliation_current) %>% 
    mutate(affiliation_history = author_profile %>% 
             map(~{
               .x$affiliation_history[[1]] %>% 
                 gather_group %>%
                 unnest_pos %>%
                 unnest_pos(address) 
             })
    ) %>% 
    mutate(subject_areas = subject_areas %>% 
             map(gather_group)
    ) %>% 
    mutate(pref_names = author_profile %>% map(~{.x$preferred_name[[1]]})) %>%
    unnest(pref_names) %>%
    mutate(classification = author_profile %>%
             map(~{
               .x$classificationgroup[[1]] %>% 
                 mutate(classification = classification %>% map_df(gather_group) %>% list) %>% 
                 unnest_pos(classification)
             })
    ) %>% 
    mutate(
      subject_areas = map2(subject_areas, classification, ~{
        .y %>%
          rename(code = x) %>% 
          right_join(.x, by = "code") %>% 
          rename(field = x) %>% 
          dplyr::select(-type, -fa)
      })
    ) %>% 
    dplyr::select(scopus_id, first_name = given_name, last_name = surname, subject_areas, aff_current_id,
                  affiliation_history, document_count, cited_by_count, citation_count, eid, orcid)
}

#' scopus_clean_author_publications
#' @param author_publications a tibble containing author_publications to clean (response from author_publications scrapping)
#' @return the cleaned tibble
#' @export

scopus_clean_author_publications <- function(author_publications){
  author_publications %>% 
    slice(1:100) %>% 
    mutate(link = link %>% map(gather_group)) %>%
    mutate(afid = affiliation %>% 
             map(~{
               if(is.null(.x)) return(NULL)
               .x %>% 
                 gather_group %>%
                 dplyr::select(afid)
             })
    ) %>% 
    mutate(author_count = author_count %>% map_chr(2) %>% as.numeric) %>%
    mutate(scopus_id = dc_identifier %>% str_extract("\\d+")) %>% 
    dplyr::select(
      scopus_id,
      title = dc_title,
      type = subtype_description,
      citedby_count,
      abstract = dc_description,
      source_type = prism_aggregation_type,
      source = prism_publication_name,
      source_issn = prism_issn,
      source_volume = prism_volume,
      source_issue_identifier = prism_issue_identifier,
      source_page_range = prism_page_range,
      source_cover_date = prism_cover_date,
      source_cover_display_date = prism_cover_display_date,
      source_doi = prism_doi, 
      pii, 
      eid, 
      fund_sponsor,
      link, 
      afid
    )
}

#' scopus_clean_publications
#' @param publications a tibble containing publications to clean (response from publications scrapping)
#' @return the cleaned tibble
#' @export

scopus_clean_publications <- function(publications){
  publications %>% 
    mutate(
      authors = bibrecord %>% 
        map(~{
          if(is.null(.x$author_group)) return(NULL)
          if(length(.x$author_group) > 1){
            tmp <- .x$author_group[[1]] %>% 
              gather_group %>% 
              select(author) %>% 
              unnest
            
            if(!any(colnames(tmp) == "auid"))  return(NULL)
            
            tmp <- tmp %>% 
              gather_group() %>% 
              drop_na(auid) %>% 
              arrange(seq) %>% 
              map_select(c("seq", "ce_given_name", "ce_surname", "auid")) %>%
              set_names(c("seq", "first_name", "last_name", "auid"))
            return(tmp)
          } else {
            tmp <- .x$author_group[[1]][,length(.x$author_group[[1]])] %>%
              unnest 
            
            if(!any(colnames(tmp) == "auid"))  return(NULL)
            tmp <- tmp %>%               
              map_select(c("seq", "ce_given_name", "ce_surname", "auid")) %>% 
              set_names(c("seq", "first_name", "last_name", "auid"))
            return(tmp)
          }
        })
    ) %>% 
    mutate(source_list = bibrecord %>%
             map( ~ {
               .x$source[[1]] %>%
                 map_select(c("codencode", "srcid")) %>%
                 set_names("source_short", "source_list") %>% 
                 unnest_pos(source_list)
             })
    ) %>%
    unnest(source_list) %>%
    mutate(classification = bibrecord %>%
             map( ~ {
               .x$enhancement %>% 
                 listify() %>%
                 gather_group()
             })
    ) %>%
    mutate(ids = bibrecord %>%
             map( ~ {
               tmp <- .x$itemidlist[[1]] %>% 
                 gather_group() 
               
               if(!(any(colnames(tmp) == "V"))) return(NULL)
               
               tmp %>% 
                 mutate(is_tibble = map_lgl(V, is.data.frame)) %>%
                 filter(is_tibble) %>% 
                 unnest() %>% 
                 map_select(c("idtype", "x")) %>% 
                 set_names(c("id_type", "id"))
             })
    ) %>%
    mutate(ref = bibrecord %>%
             map(~{
               if(!any(colnames(.x) == "bibliography")) return(NULL)
               .x$bibliography[[1]]$reference[[1]] %>% 
                 gather_group() %>% 
                 unnest %>% 
                 unnest_pos(itemid) %>% 
                 map_select(c("x")) %>% 
                 set_names("scopus_cited_id")
             })
    ) %>%
    mutate(lang = xml_lang %>% map_chr(~ifelse(is.null(.x[1]), NA_character_, as.character(.x[1])))) %>%
    mutate(publisher = dc_publisher %>% map_chr(~ifelse(is.null(.x[1]), NA_character_, as.character(.x[1])))) %>%
    mutate(keywords = author_keyword %>%
             map(~{
               .x %>% 
                 gather_group %>% 
                 map_select("x") %>% 
                 set_names("keyword")
             })
    ) %>%
    mutate(isbns = prism_isbn %>% 
             map(~{
               .x %>%
                 map_select(c("x", "x1")) %>% 
                 set_names(c("online_isbn", "print_isbn"))
             })
    ) %>% 
    mutate(scopus_id = ids %>% map_chr(~{
      if(is.null(.x)) return(NA_character_)
      .x %>% filter(id_type == "SCP") %>% 
        .$id %>% .[1] %>% as.character()
    })
    ) %>% 
    dplyr::select(
      scopus_id,
      authors,
      classification,
      source_short,
      source_id,
      ids,
      ref,
      lang,
      keywords,
      publisher,
      isbns
    )
}

#' scopus_clean_citations
#' @param publications a tibble containing publications from which citation matrix should be extracted (response from publications scrapping)
#' @return the cleaned citation matrix as tibble with citing and cited ids
#' @export

scopus_clean_citations <- function(publications){
  publications_cleaned %>% 
    select(scopus_id, ref) %>% 
    drop_na(scopus_id) %>% 
    mutate(is_tibble = ref %>% map_lgl(is.data.frame)) %>% 
    filter(is_tibble) %>% 
    unnest %>% 
    select(-is_tibble)
}