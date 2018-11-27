
#' clean_authors
#' @param authors a tibble containing authors to clean (response from authors scrapping)
#' @return the cleaned tibble
#' @export

clean_authors <- function(authors, n, path, loading_main){
  
  # message("Loading authors_final")
  # load(glue("{path}/../data/authors_final.Rdata"))
  # already <- authors_final$auid
  # 
  # message(glue("{length(already)} authors were already parsed"))
  # 
  # if(loading_main){
  #   message("Loading authors")
  #   load(glue("{path}/../data/authors.Rdata"))
  # }
  
  # message(glue("{nrow(authors) - length(already)} authors still must be parsed"))
  message(glue("Parsing {n} authors"))
  
  unnest_tibble_prog <- progressively(unnest_tibble, .n = n)
  
  step1 <- authors %>%
    mutate(auid = coredata %>% 
             map_chr(~.x[["dc_identifier"]]) %>% 
             str_extract("\\d+")) %>%
    #filter(!auid %in% already) %>%
    slice(1:n) %>%
    unselect_pos(c("retrieval_time", "api_key", "status", "fa")) %>%
    split(1:nrow(.)) %>%
    map(unnest_tibble_prog) %>%
    map(~{
      .x[["affiliation_history"]] <- ifelse(
        class(.x[["affiliation_history"]]) == "character",
        tibble(x = NA), 
        .x[["affiliation_history"]]
      )
      .x
    }) %>%
    bind_rows
  
  step2 <- step1 %>%
    unnest(coredata) %>%
    mutate(auid = dc_identifier) %>%
    unselect_pos(c("prism_url", "dc_identifier", "link")) %>%
    #select(eid, document_count, cited_by_count, citation_count, orcid) %>%
    mutate(aff_current = affiliation_current %>% map_chr(~.x[["id"]])) %>%
    dplyr::select(-affiliation_current) %>%
    mutate(aff_hist_id = affiliation_history %>%
             map(~{
               if(is.null(.x) | !("id" %in% names(.x))) return(NULL)
               .x["id"]
             })) %>%
    #dplyr::select(author_profile) %>%
    dplyr::select(-affiliation_history) %>%
    unnest(author_profile) %>%
    select(-status, -date_created) %>%
    mutate(publication_range = publication_range %>%
             map(~{if(is.null(.x))  return(tibble(end = NA)) else return(.x)})) %>%
    unnest(publication_range) %>%
    mutate(preferred_name = preferred_name %>%
             map(~{if(is.null(.x))  return(tibble(surname = NA)) else return(.x)})) %>%
    unnest(preferred_name) %>%
    #select(surname, given_name) %>%
    unselect_pos(c("source", "initials", "indexed_name", "date_locked"))  %>%
    mutate(affiliation_history = affiliation_history %>% 
             map(~{
               if(is.null(.x))  return(NULL)
               tmp <- .x %>%
                 unselect_pos("source") %>%
                 mutate(ip_doc = ip_doc %>%
                          map(~{
                            if(class(.x[["address"]]) == "character") .x[["address"]] <- list(tibble(city = NA_character_))
                            return(.x)
                          })
                 ) %>%
                 unnest(ip_doc) %>%
                 select(-relationship) %>%
                 mutate(preferred_name = preferred_name %>%
                          map(~{
                            if(is.null(.x)) return(tibble(x = NA)) else return(.x)
                          })
                 ) %>%
                 unnest(preferred_name) %>%
                 mutate_pos(address = address %>%
                              map(~{
                                if(is.null(.x)) return(tibble(x = NA)) else return(.x)
                              })
                 ) %>%
                 unnest_pos("address") %>%
                 rename_pos("aff_id" = "id") %>%
                 rename_pos("par_id" = "parent") %>%
                 rename_pos("par_name" = afdispname) %>%
                 rename_pos("url" = "org_url") %>%
                 rename_pos("countryname" = "country_2") %>%
                 select_pos(c("aff_id", "par_id" , "type", "sort_name", "par_name",
                              "url", "country", "countryname", "city", "state", "postal_code", "address_part"))
             })) %>%
    mutate(name_variant = name_variant %>% map(~{
      .x %>%
        select_pos(c("doc_count", "surname", "given_name"))
    })) %>%
    mutate(fields = classificationgroup %>%
             map(~ .x %>%unnest_pos("classification"))) %>%
    dplyr::select(-classificationgroup) %>%
    mutate(journal_history = journal_history  %>% map(~{
      .x %>%
        select_pos("type") %>%
        unnest_pos("journal")
    })) %>%
    mutate(subject_areas = subject_areas %>% map(~.x %>% unselect_pos("fa"))) %>%
    mutate(fields = subject_areas %>%
             map2(.y = fields,~{
               if(is.null(.x) & is.null(.y)) return(NULL)
               if(is.null(.y)) return(.x)
               if(is.null(.x)) return(.y)
               .x %>% left_join(.y, by = c("code" = "x"))
             })) %>%
    mutate(auid = auid %>% str_extract("\\d+")) %>%
    select_pos(c("auid", "surname", "given_name", "start", "end",
                 "document_count", "cited_by_count", "citation_count",
                 "eid", "orcid",
                 "aff_current", "aff_hist_id", "affiliation_history",
                 "journal_history", "fields", "name_variant"))
  
  step3 <- step2 %>%
    #bind_rows(authors_final) %>%
    filter(!duplicated(auid))
  
  #message(glue("{nrow(step3) - length(already)} authors were parsed"))
  
  return(step3)
  
}

#' clean_publications_authors
#' @description This function parses new scrapped publications and update the publications_final file
#' @param path Path of the working directory
#' @param n Number of authors whose affiliations should be parsed
#' @return the updated version of affiliations_final
#' @export

clean_author_publications <- function(author_publications, n, path){
  unnest_tibble_prog <- progressively(unnest_tibble, .n = nrow(author_publications))
  
  return(author_publications %>%
    unnest(x) %>%
    split(1:nrow(.)) %>%
    map(unnest_tibble_prog) )
}

#' clean_publications
#' @description This function parses new scrapped publications and update the publications_final file
#' @param path Path of the working directory
#' @param n Number of authors whose affiliations should be parsed
#' @return the updated version of affiliations_final
#' @export

clean_publications <- function(publications, n, path, loading_main = T){
  
  # message("Loading publications_final")
  # load(glue("{path}/../data/publications_final.Rdata"))
  # already <- publications_final$pub_id
  # rm(publications_final)
  # 
  # message(glue("{length(already)} publications were already parsed"))
  # 
  # if(loading_main){
  #   message("Loading publications")
  #   load(glue("{path}/../data/publications.Rdata"))
  # }
  # 
  # message(glue("{nrow(publications) - length(already)} publications still must be parsed"))
  # n <- min(nrow(publications) - length(already), n, na.rm = T)
  message(glue("Parsing {n} publications"))
  
  unnest_tibble_prog <- progressively(unnest_tibble, .n = n)
  
  message(glue("Cleaning nested structures"))
  step1 <- publications %>%
    # mutate(scopus_id = dc_identifier %>% str_extract("\\d+")) %>%
    # filter(!scopus_id %in% already) %>%
    # sample_n(nrow(.)) %>%
    slice(1:n) %>%
    janitor::clean_names(.) %>%
    unselect_pos(c("retrieval_time", "api_key", "idxterms")) %>%
    split(1:nrow(.)) %>%
    map(unnest_tibble_prog) 
  
  message(glue("Cleaning variables"))
  
  
  
  step2 <- step1 %>%
    map(~{
      if(class(.x[["subject_areas"]]) == "character") .x[["subject_areas"]] <- NULL
      if(class(.x[["authors"]]) == "character") .x[["authors"]] <- NULL
      if(class(.x[["language"]]) == "list") .x[["language"]] <- NULL
      return(.x)
    }) %>%
    reduce(bind_rows) %>%
    mutate_pos(keywords = authkeywords %>% map_chr(~.x[["x"]] %>% simplify_vector)) %>%
    mutate(authors = authors %>%
             map(~{
               tmp <- .x %>%
                 unselect_pos(c( "ce_surname", "ce_given_name")) %>%
                 select_pos(c("preferred_name", "auid", "affiliation")) %>%
                 unnest_pos("preferred_name") 
               if(is.null(tmp)){return(NULL)}
               if("affiliation" %in% names(tmp)){
                 tmp <- tmp %>%
                   mutate(affiliation = affiliation %>% 
                            map_chr(~.x[["id"]] %>% simplify_vector))
               }
               tmp %>%
                 rename_pos("first"="ce_given_name", "last" = "ce_surname") %>%
                 select_pos(c("first", "last", "affiliation", "auid"))
             })) %>%
    mutate(fields = subject_areas %>% 
             map(~{
               .x %>%
                 unselect_pos("fa") %>%
                 rename_pos(area = x)
             })
    ) %>%
    mutate(coredata = coredata %>% map(~{.x[["prism_isbn"]] <- .x[["prism_isbn"]] %>% 
      simplify_vector
    .x
    })) %>%
    unnest_pos("coredata") %>%
    unnest_pos("item") %>%
    unnest_pos("bibrecord") %>%
    mutate(head = head %>%
             map(~{
               if(class(.x[["source"]]) == "character") .x[["source"]] <- list(tibble(sourcetitle = .x[["source"]]))
               return(.x)
             })) %>%
    unnest_pos("head") %>%
    mutate(citation_info = citation_info %>%
             map(~{
               if(class(.x)[1] == "character") .x <- tibble(citation_type = .x)
               if(is.null(.x)) .x <- tibble(citation_type = NA_character_)
               return(.x %>% unselect_pos("author_keywords"))
             })) %>%
    unnest(citation_info) %>%
    unselect_pos(c("correspondence", "abstract_language", "citation_language")) %>%
    mutate(classifications = enhancement %>%
             map(~{
               if(length(.x[["classificationgroup"]]) == 0){return(NULL)}
               .x %>% 
                 unnest_pos("classificationgroup") %>%
                 mutate(classification =  classification %>% map_chr(~.x %>% simplify_vector))
             })) %>%
    mutate(refs = tail %>% map(~.x[["reference"]][[1]])) %>%
    unselect_pos(c("link", "dc_creator",  "prism_ending_page",  "prism_starting_page", 
                   "pubmed_id",  "openaccess",  "openaccess_flag",  "ait_process_info", 
                   "xocs_meta",  "item_info", 
                   "authkeywords",  "grantlist",  "enhancement",  "tail", 
                   "affiliation",  "subject_areas", "author_group", "dc_description",
                   "dc_title", "srctype", "subtype", "citation_type")) %>%
    mutate(author_affiliation = authors %>%
             map_chr(~{
               if(is.null(.x)){return(NA_character_)}
               tmp <- .x %>%
                 mutate_pos(author_affiliation = paste(auid,affiliation, sep = "_")) 
               if(is.null(tmp[["author_affiliation"]])) return(NA_character_)
               tmp %>%
                 .$author_affiliation %>%
                 glue_collapse(";")})
    )  %>%
    rename_pos("date" = "prism_cover_date") %>%
    rename_pos("lang" = "language") %>%
    rename_pos( "source_name" = "prism_publication_name") %>%
    rename_pos("src_type" = "prism_aggregation_type") %>%
    rename_pos( "page_range" = "prism_page_range") %>%
    rename_pos( "volum" = "prism_volume") %>%
    rename_pos( "issue" = "prism_issue_identifier") %>%
    rename_pos("issn" = "prism_issn") %>%
    rename_pos( "isbn" = "prism_isbn") %>%
    rename_pos( "pub_id" = "scrapped_ids") %>%
    rename_pos( "pii") %>%
    rename_pos( "doi" = "prism_doi") %>%
    select_pos(
      vars = c("citation_title", "citedby_count", "date",  "keywords", "abstracts", "lang" , 
               "authors", "author_affiliation",
               "fields", "classifications", "refs", 
               "source_name" , "src_type" , "subtype_description", "publisher" , 
               "page_range", "volum", "issue" ,
               "source_id", "issn", "isbn",
               "eid", "pub_id", "pii", "doi")
    )
  
  # message("Loading publications_final")
  # load("../data/publications_final.Rdata")
  
  #step3 <- step2 #%>%
  #   bind_rows(publications_final)
  
  #message(glue("{nrow(step3) - length(already)} publications were parsed"))
  
  return(step2)
  
}


#' clean_affiliations
#' @description This function updates the affiliations file in regard of updated authors
#' @param path Path of the working directory
#' @param n Number of authors whose affiliations should be parsed
#' @return the updated version of affiliations_final
#' @export



clean_affiliations <- function(path, n = 1000, load_main = T){
  
  message("Loading affiliations...")
  load(glue("{path}/../data/affiliations_final.Rdata"))
  already <- affiliations_final$auids %>%
    str_split("_") %>% unlist %>%
    unique
  
  message(glue("Affiliations of {length(already)} authors were were already parsed"))
  
  if(load_main){
    message("Loading authors_final...")
    load(glue("{path}/../data/authors_final.Rdata"))
  }
  
  message(glue("Affiliations of {nrow(authors_final) - length(already)} authors  still must be parsed"))
  n <- min(nrow(authors_final) - length(already), n, na.rm = T)
  
  message(glue("Parsing affiliations of {n} authors"))
  
  unnest_tibble_prog <- progressively(unnest_tibble, .n = n)
  
  message(glue("Extracting affiliation histories and cleaning nested structures"))
  
  step1 <- authors_final %>%
    slice(1:n) %>%
    dplyr::select(auid, affiliation_history) %>%
    mutate(affiliation_history = affiliation_history %>% map(~{
      if(is.null(.x)) return(tibble(aff_id = NA)) else return(.x)
    })) %>%
    unnest_pos("affiliation_history") %>%
    filter(!is.na(aff_id))
  
  message("Loading affiliations")
  load(glue("{path}/../data/affiliations_final.Rdata"))  
  
  message(glue("Merging data"))
  
  step2 <- affiliations_final %>%
    unnest_tokens(auid, auids, token = "regex", pattern = "_") %>%
    bind_rows(step1) %>%
    group_by(aff_id) %>%
    mutate(auids = suppressWarnings(auid %>% paste(collapse = "_"))) %>%
    ungroup %>%
    dplyr::select(-auid) %>%
    filter(!duplicated(aff_id))
  
  message("Cleaning was successfull")
  
  return(step2)
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
               if(is.null(.x)) return(tibble(NA))
               .x %>% 
                 gather_group %>%
                 dplyr::select(afid)
             })
    ) %>%
    mutate(author_count = author_count %>% 
             map_dbl(~{
               if(is.null(.x)){
                 NA_integer_
               } else {
                 .x[2] %>% 
                   as.numeric
               }
             })
    ) %>%
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
      link ,
      afid
    ) %>%
    mutate_at(c(4), as.numeric) %>%
    unnest(afid)
}

#' update_citation_mat
#' @description This function takes the updated publications_final and extract the new citation matrix
#' @return the cleaned citation matrix as tibble with citing and cited ids
#' @export

update_citation_mat <- function(){
  
  message("Loding datasets")
  load("../data/publications_final.Rdata")
  load(glue("{path}/../data/citation_matrix_final.Rdata"))
  already <- citation_matrix$citing_id %>% unique
  
  step1 <- publications_final %>%
    select(pub_id, refs) %>%
    filter(!pub_id %in% already) %>%
    mutate(ref_info = refs %>%
             map(~{
               tmp <- .x[["ref_info"]]
               if(is.null(tmp)) return(NULL)
               return(tmp)
             })
    ) %>%
    mutate(id = ref_info %>%
             map(~{
               if(is.null(.x)) return(NA)
               .x %>%
                 map(~{
                   if(is.null(.x)) return(NA_character_)
                   tmp <- .x[["refd_itemidlist"]][[1]][["x"]] %>% 
                     as.character
                   
                   if(length(tmp) != 1) return(NA_character_)
                   if(is.na(tmp)) return(NA_character_)
                   return(tmp)
                   
                 }) %>%
                 reduce(c) %>%
                 tibble(cited_id = .) %>%
                 filter(!is.na(cited_id))
             })) %>%
    select(citing_id = pub_id, id) %>%
    filter(!is.na(id)) %>%
    unnest(id)
  
  citation_matrix <- step1 %>% 
    bind_rows(citation_matrix) %>%
    unique
  
  message(glue("Citation matrix has now {nrow(citation_matrix)} rows."))
  
  return(citation_matrix)
}
