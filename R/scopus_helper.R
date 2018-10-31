#' get_publication_by_author
#' @param x a list of named elements
#' @return a tibble whose names have been cleaned by janitor::clean_names()
#' @export

as_tibble_c <- function(x) {
  if(is.null(names(x))){
    names(x) <- rep("value", length(x))
  }
  x %>% as_tibble %>% janitor::clean_names()
}
#' listify
#' @description This function check whether the input is a list on which we could map. If this is not the case, it encapsualit the input in a list.
#' @param x a list or a tibble
#' @return a list
#' @export

listify <- function(x) if(!is.list(x[[1]])){list(x)} else {x}

#' rename_if_included
#' @param data a tibble
#' @param x a string as a variable name to be checked for
#' @param y a string replacing x, if x machtes
#' @return a tibble with x renamed y
#' @export

rename_if_included <- function(data, x, y){
  trig <- data %>%
    colnames %>%
    str_detect(x) %>%
    any
  if(trig){
    data <- data %>%
      rename(!!y:=!!sym(x))
  }
  return(data)
}


#' is_no_list
#' @param .x a list
#' @return a logicoal vector showing each non-list elements
#' @export

is_not_list <- function(x) return(!is.list(x))

#' tibblize_columns
#' @param x a list whoce non list columns have to become lists
#' @return a tibble wherer each column is a list
#' @export

tibblize_columns <- function(x){
  x %>%
    mutate_if(is_not_list, function(x) {
      x %>% tibble(content = .) %>% listify
    }) #%>%
}

#' unlist_tibble_core
#' @param .x a list to transform into a tibble
#' @return a tibble whose structure matchs the input structure
#' @export

unlist_tibble_core <- function(.x){
  #if(!is.null(names(x))){
  #  if(names(.x)[1] == "affiliation"){return(.x$affiliation %>% map_df(unlist_tibble_core) %>% list)}
  #}

  if(length(.x) != length(names(.x)) & !is.null(names(.x))){
    return(.x %>%
             unlist %>%
             set_names(glue("var_{1:length(x)}"))  %>%
             unlist_tibble %>%
             bind_cols)
  }

  flat_info <- .x %>%
    map_lgl( ~ !is.list(.x)) %>%
    .x[.] %>%
    map(~{
      ifelse(is.null(.x), "", .x)
    }) %>%
    as_tibble_c

  list_info <- .x %>%
    map_lgl( ~ is.list(.x)) %>%
    .x[.] %>%
    map(unlist_tibble)

  list_info <- list_info %>%
    map( ~ {
      .x %>% list %>% tibble()
    }) %>%
    bind_cols %>%
    set_names(names(list_info)) %>%
    janitor::clean_names()

  if(length(list_info) != 0 & length(flat_info) != 0){
    out <- bind_cols(flat_info, list_info)
  } else if(length(list_info) == 0){
    out <- bind_cols(flat_info)
  } else {
    out <- bind_cols(list_info)
  }
  return(out)
}


#' unlist_tibble
#' @description This function check whether the input is a list on which we could map. If this is not the case, it encapsualit the input in a list.
#' @param x a list or a tibble
#' @return a list
#' @export

unlist_tibble <- function(x){

  if(length(x) != length(names(x)) & !is.null(names(x))){
    return(x %>%
      unlist %>%
      set_names(glue("var_{1:length(x)}"))  %>%
      unlist_tibble)
  }

  if(!is.null(names(x))){
    x %<>% listify
    }
  if(length(names(x)) == 1){return(unlist_tibble(x[[1]]))}

  x %>%
    map(unlist_tibble_core) %>%
    bind_cols
}

#' progressively
#' @param .f a function to make progressive
#' @param .n input length
#' @param ... further parameters for the input function
#' @return the same function but progressively
#' @export

progressively <- function(.f, .n, ...) {
  pb <- progress::progress_bar$new(total = .n, ...)
  function(...) {
    pb$tick()
    .f(...)
  }
}

#' listify
#' @description This function check whether the input is a list on which we could map. If this is not the case, it encapsualit the input in a list.
#' @param x a list to be transformed into a tibble
#' @return a tibble with unique and cleaned column names
#' @export

list_to_tibble <- function(x){
  x <- x  %>%
    unlist %>%
    ifelse(is.null(.), "", .)
  x <- x[!duplicated(names(x))]
  x %>% as.list %>% as_tibble_c
}

#' gather_group
#' @param x a tibble falsely parsed to gather
#' @return a gathered tibble
#' @export

gather_group <- function(x){
  unnest_pos <- purrr::possibly(unnest, NULL)

  var_names <- x %>% names
  var_num <- var_names %>% str_extract("\\d+$") %>% as.numeric %>% ifelse(is.na(.), 0, .)
  var_num %>%
    unique %>%
    map_df(~{
      tmp <- x[,var_num == .x] %>%
        set_names(colnames(.) %>% str_remove("\\d+$"))
      return(tmp)
    })
}


#' map_select
#' @param x a list from which columns should be selected
#' @param vars a vector of names to select in x
#' @return the selected columns (if not contained, replaced by NA)
#' @export

map_select <- function(x, vars){
  x <- x %>% unlist %>% as.list
  vars %>% map(~{
    if(any(names(x) == .x)){
      return(x[[.x]])
    } else {
      return(NA)
    }
  }) %>%
    set_names(vars) %>%
    bind_cols
}
