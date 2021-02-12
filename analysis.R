
library(tidyverse)
library(lubridate)
library(readxl)



get_hp <- function(rebase = NULL) {
  
  tmpfile <- tempfile(fileext = ".xls")
  # TODO this has to be an online source
  file <- "https://www.bankofgreece.gr/RelatedDocuments/BG_PRICES_INDICES_HISTORICAL_SERIES.xls"
  download.file(file, tmpfile, mode = "wb")
  
  suppressMessages({
    prices <- read_excel(tmpfile)
  })
  
  categories <- c("Urban Areas","Athens", "Thessaloniki", "Other cities (excluding Athens and Thessaloniki)", "Other Urban Areas (Urban areas other than Athens)")
  ind <- which(prices[[1]] %in% categories)
  ind2 <- c(ind[-1] -1, nrow(prices))
  
  splits <- list()
  for(i in 1:length(ind)) {
    splits[[i]] <- prices[ind[i]:(ind2[i]),]
  }
  names(splits) <- c("Urban", "Athens", "Thessaloniki", "Other cities", "Other Urban Areas")
  
  joined_noath <- imap(splits[c(1,3:5)], preprocess) %>% 
    reduce(full_join, by = "date") 
  athens <- preprocess_athens(splits[[2]], "Athens")
  joined <- full_join(joined_noath, athens, by = "date")
  
  if(!is.null(rebase)) {
    base <- which(joined$date == "1997-01-01")
    base <- which(joined$date == rebase)
    joined <-  mutate_at(joined, vars(-date), ~ .x/.x[base]) 
  }
  pivot_longer(joined, -date, names_ptypes = list(name = factor())) %>% 
    mutate(
      base = case_when(
        name == "Urban" ~ 1997,
        name == "Other Urban Areas" ~ 1993,
        TRUE ~ 2007
      )
    ) %>% 
    drop_na() %>% 
    arrange(name, date)
}

preprocess <- function(x, nm = "value") {
  x[-c(1:4),1:5] %>% 
    drop_na() %>% 
    set_names(c("year", "Q1", "Q2", "Q3", "Q4")) %>% 
    # remove stars(*) from years
    mutate(year = str_replace(year, "\\*", "")) %>% 
    # remove dots although are not equal to "..."
    map_df(~ str_replace(.x, "…", NA_character_)) %>% 
    # coerce freely
    map_df(as.numeric) %>% 
    pivot_longer(-year, names_to = "quarter", values_to = "value") %>% 
    mutate(date = yq(paste0(year, quarter))) %>% 
    select(date, value) %>% 
    set_names(c("date", nm))
}
preprocess_athens <- function(x, nm = "value") {
  x[-c(1:4),c(1, 14:17)] %>% 
    drop_na() %>% 
    set_names(c("year", "Q1", "Q2", "Q3", "Q4")) %>% 
    # remove stars(*) from years
    mutate(year = str_replace(year, "\\*", "")) %>% 
    # remove dots although are not equal to "..."
    map_df(~ str_replace(.x, "…", NA_character_)) %>% 
    # coerce freely
    map_df(as.numeric) %>% 
    pivot_longer(-year, names_to = "quarter", values_to = "value") %>% 
    mutate(date = yq(paste0(year, quarter))) %>% 
    select(date, value) %>% 
    set_names(c("date", nm))
}

