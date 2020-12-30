library(tidyverse) # general handling

library(glue) # for text-string handling
library(slam) # for memory efficient matrix-representations
library(magrittr) #for additional pipes

library(furrr)
setwd('/home/brede/fh_nextcloud/Data Science/dataViz/covid_story/')
load('data/word_freqs.RData')

translate_with_mymemory <- function(word, from, to = 'en'){
  .GlobalEnv$translated <- 0
  require(httr)
  ret <- GET(glue('https://api.mymemory.translated.net/get?q=',
                  word, '&langpair=',
                  from, '|', to,
                  '&de=max.brede@student.fh-kiel.de'))
  out <- tryCatch({
    content(ret)$matches %>%
      map_dfr( ~ bind_cols(.) %>%  mutate(across(everything(), ~ as.character(.)))) %>%
      dplyr::slice(which.max(quality)) %>%
      select(translation, quality) %>%
      transmute(word = word,
                translation,
                quality)
  },
  error = function(e) {
    word
  })
  return(out)
  
}

plan(multicore(workers = 6))

german_keys <- read_rds('data/german_stem_keys.rds') 

german_word_freq %<>% 
  arrange(desc(n)) %>% 
  slice(1:5000) %>%
  left_join(german_keys, by = c('word' = 'stems')) %>% 
  transmute(word,n,long = map_chr(data,~first(unlist(.)))) %>% 
  mutate(tranlation = future_map(long, ~translate_with_mymemory(.,'de'),.progress = T))

german_word_freq %>% 
  write_rds('data/german_translated.rds', compress = 'gz')



swedish_keys <- read_rds('data/swedish_stem_keys.rds') 

swedish_word_freq %<>% 
  arrange(desc(n)) %>% 
  slice(1:5000) %>%
  left_join(swedish_keys, by = c('word' = 'stems')) %>% 
  transmute(word,n,long = map_chr(data,~first(unlist(.)))) %>% 
  mutate(tranlation = future_map(long, ~translate_with_mymemory(.,'sv'), .progress = T))

swedish_word_freq %>% 
  write_rds('data/swedish_translated.rds', compress = 'gz')
