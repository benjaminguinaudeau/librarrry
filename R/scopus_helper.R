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

#' %=>%
#' @export

`%=>%` <- function(input, .otherwise){
  ifelse(is.null(input), .otherwise, input)
}

#' is_tibble
#' @export

is_tibble <- function(x) is.data.frame(x)

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

#' @export
if_case_1 <- function(x) length(names(x)) == 0 & is.list(x) # This should be bind_row
#if_case_2 <- function(x, nam) length(nam) == 0 & is.list(x) # This should be simplify
#' @export
if_case_3 <- function(x) length(names(x)) == 0 & !is.list(x) # Character case
#' @export
if_case_4 <- function(x) length(names(x)) != 0 & is.list(x) # Apply Transform List and Bind-Col
#' @export
if_case_5 <- function(x) length(names(x)) != 0 & !is.list(x) # as_tibble

#' @export
case_1 <- function(element, nam){ # Take element and return tibble
  element %>%
    map(~{
      .x %>%
        transform_list() %>%
        janitor::clean_names(.) %>%
        unnest(x) #%>%
        #tibblize_columns()
    }) %>%
    bind_rows %>%
    list %>%
    tibble(.) %>%
    set_names(nam) %>%
    janitor::clean_names(.)
}

#' @export
case_3 <- function(element, nam = "x"){
  element %>%
    as_tibble %>%
    set_names(nam) %>%
    janitor::clean_names(.)
}

#' @export
case_4 <- function(element, nam){
  element %>%
    map2(.y = names(element), ~transform_list(.x, .y)) %>% 
    bind_cols %>%
    list %>%
    tibble %>%
    set_names(nam) %>%
    janitor::clean_names(.)
}

#' transform_list
#' @param x a response to parse
#' @return a tibble containing parsed respone
#' @export


transform_list <- function(x, nam = "x"){
  if(is.null(x)) return(NULL)
  if(if_case_1(x)) return(case_1(x, nam))
  #if(if_case_2(x, nam)) return(case_2(x, nam))
  if(if_case_3(x)) return(case_3(x, nam))
  if(if_case_4(x)) return(case_4(x, nam))
  if(if_case_5(x)) return(case_5(x, nam))
}

#' transform_list_old
#' @param x a response to parse
#' @return a tibble containing parsed respone
#' @export

transform_list_old <- function(x){
  if (length(names(x)) == 0 & length(x) > 1) {
    return(x %>% map_df(~{
      .x %>% transform_list %>% bind_rows %>% janitor::clean_names(.) %>% 
        tibblize_columns()
    }) %>% list %>% set_names(names(x)))
  }
  x %>% map2_dfc(.y = names(x), ~{
    if (is.null(.x)) {
      return(NULL)
    }
    if (length(.y) == 0 & length(.x) == 1) {
      return(tibble(.x) %>% as_tibble_c)
    }
    if (is.list(.x)) {
      return(.x %>% transform_list %>% bind_cols %>% janitor::clean_names(.) %>% 
               list %>% tibble %>% set_names(.y))
    }
    else {
      return(.x %>% tibble %>% set_names(.y))
    }
  })
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

#' unnest_pos
#' @export

unnest_pos <- purrr::possibly(tidyr::unnest, otherwise = NULL)

#' gather_group
#' @param x a tibble falsely parsed to gather
#' @return a gathered tibble
#' @export

gather_group <- function(x){

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
  #x <- x %>% split(1:nrow(.))
  vars %>% map(~{
    if(any(names(x) == .x)){
      return(x[[.x]])
    } else {
      return(rep(NA, nrow(x)))
    }
  }) %>%
    set_names(vars) %>%
    bind_cols
}

#' select_pos
#' @param data a dataframe with columns to select
#' @param vars a vector of names to select in x
#' @return selected columns
#' @export

select_pos <- function(data, vars) return(data[,vars[vars %in% names(data)]])

#' unselect_pos
#' @param data a dataframe with columns to unselect
#' @param vars a vector of names to unselect in x
#' @return unselected columns
#' @export

unselect_pos <- function(data, vars) return(data[,!names(data) %in% vars])

#' mutate_pos
#' @param data a dataframe with var to mutate
#' @param ... arguments to be aplly to mutate
#' @return mutated dataframe
#' @export

mutate_pos <- function(data, ...) {
  mutate_p <- possibly(mutate, otherwise = NULL)
  
  tmp <- data %>%
    mutate_p(...)
  if(is.null(tmp)) return(data) else return(tmp)
}

#' rename_pos
#' @param data a dataframe with var to rename
#' @param ... arguments to be aplly to rename
#' @return dataframe with renamed columns
#' @export

rename_pos <- function(data, ...) {
  rename_p <- possibly(rename_, otherwise = NULL)
  
  tmp <- data %>%
    rename_p(...)
  if(is.null(tmp)) return(data) else return(tmp)
}

#' unnest_pos
#' @param data a dataframe with var to unnest
#' @param ... arguments to be aplly to unnest
#' @return dataframe with unnested columns
#' @export


unnest_pos <- function(data, ...){
  unnest_p <- possibly(unnest, otherwise = NULL)
  
  if(!... %in% names(data)){return(data)}
  
  tmp <- data %>%
    unnest_(...)
  if(is.null(tmp)) return(data) else return(tmp)
}

#' simplify_vector
#' @description Use to simplify variables whose elements are unnecessarily nested into tibble. It just get the value embedded in the tibble and return a character vector.
#' @param x a variable with tibble to simplify
#' @return a character vector
#' @export


simplify_vector <- function(x) if(is.null(x)){return(NA_character_)} else{ x %>% unlist %>% paste(., collapse  = "_")}

#' any_f
#' @description The variable check if any element is F
#' @param lgl a logical vector
#' @return a logical vector
#' @export

any_f <- function(lgl){!any(!lgl)}

#' simplifiable
#' @description The variable check whether a list element can be simplified
#' @param x a variable to map on
#' @return a logical vector T if the variable can be simplified
#' @export

simplifiable <- function(x){
  x <- x[!x %>% map_lgl(~is.null(.x)|length(.x) == 0)] 
  if(!is.list(x)){return(F)}
  trig1 <- x %>% map_lgl(~is_tibble(.x)) %>% any # Is the first element a tibble? Should be T
  trig2 <- x %>% map_lgl(~ncol(.x) == 1 & nrow(.x) == 1 ) %>% any_f # Is there any tibble with more than 1 row or 1 column Shoulb be T
  trig3 <- x %>% map_lgl(~.x[[1]] %>% map_lgl(~!is_tibble(.x)) %>% any ) %>% any # Is the nested element other than a tibble Should be T
  
  return(trig1 & trig2 & trig3)
}

#' list_simplifiable
#' @param x a variable to check the type for the parsing process
#' @return a logical vector T if the variable is a list and can be simplifiable
#' @export

list_simplifiable <- function(x) is.list(x) & simplifiable(x)

#' list_simplifiable
#' @param x a variable to check the type for the parsing process
#' @return a logical vector T if the variable is a list and cannot be simplifiable
#' @export

list_unsimplifiable <- function(x) is.list(x) & !simplifiable(x)

#' unnest_tibble
#' @description This function check the internal structure of an element an clean it
#' @param x a tibble whose structure should be cleaned
#' @return a tibble with cleaned structure
#' @export

unnest_tibble <- function(x){ # input a tibble
  if(is.null(x) | length(x) == 0){  # Error Handling
    return(tibble())
  }
  if(is_tibble(x[1,1][[1]][[1]])){ # useless nesting : returns nested tibble
    if(ncol(x) == 1 & nrow(x) == 1){
      return(x[1,1][[1]][[1]] %>% unnest_tibble)
    }
  } 
  if(ncol(x) == 1 & x[[1]] %>% map_lgl(is_tibble) %>% `!` %>% any){ # 1 non tibble column : returns same
    return(x)
  }
  
  tmp <- x %>%
    mutate_if(list_simplifiable, action_3) %>% # Simplifiable Columns: Unnest tibble with only one element
    mutate_if(list_unsimplifiable, action_2) # Unsimplifiable Colmuns: Select the colmuns and return 
  
  return(tmp)
}

#' action_2
#' @param x a list whose structure should be checked and cannot be simplified
#' @return a list containing cleaned data
#' @export

action_2 <- function(x){
  if(is.null(x)|length(x) == 0){return(tibble())}
  x %>%
    map(unnest_tibble)
}

#' action_3
#' @param x a list containing tibble, which should simplified and transform into character
#' @return a character vector with the cleaned data
#' @export

action_3 <- function(x){
  x %>% 
    map_chr(~{
      if(is.null(.x)){return(NA_character_)}
      .x %>% unlist
    })
}
