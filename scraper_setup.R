# Setup -------------------------------------------------------------------
setwd("Q:\\Meine Bibliotheken\\Non-Research\\Sustainability\\CO2_Calculator")


## Packages ===============================================================
library(tidyverse)
library(rvest)
library(RSelenium)
library(beepr)

## Functions ==============================================================


# String cleaner
my_strClean <- function(str){
  str %>% 
    str_remove_all(pattern = regex("\\n", ignore_case = FALSE)) %>% 
    str_remove_all(pattern = regex("\\t", ignore_case = FALSE)) %>% 
    str_remove_all(pattern = regex("\\r", ignore_case = FALSE)) %>% 
    str_trim(side = "both") %>% # remove trailing and leading whitespaces
    str_squish()                # reduces repeated whitespaces
}


# Decode URL to UTF-8
url_decode_utf <- function(x) {
  y <- urltools::url_decode(x)
  Encoding(y) <- "UTF-8"
  y
}


# Distance from character to double
distance_double <- function(x){
  res <- str_remove(x, " km") %>% 
    str_remove("\\.") %>% 
    str_replace(pattern = ",", replacement = ".") %>% 
    as.double()
  return(res)
}

# Function that joins if missing
coalesce_join <- function(x, y, by = NULL, suffix = c(".x", ".y"), join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}