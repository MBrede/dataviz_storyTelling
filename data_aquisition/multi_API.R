load('/home/brede/fh_nextcloud/Data Science/dataViz/covid_story/data/pre_steps.RData') 

library(httr)
library(glue)
library(slam)
library(furrr)
library(tidyverse)

get_stories <- function(time_range,query,media_ids){
  base_url <- 'https://api.mediacloud.org/api/v2/stories_public/'
  query_string <- glue('?q=text:', query)
  url <- glue(base_url,'word_matrix', query_string, '&key=', mc_key)
  resp <-
    POST(
      glue(
        url,
        '&fq=publish_day:', time_range,'&rows=2000&stopword_length=long'
      ),
      body = jsonlite::toJSON(list(fq = list(media_id = media_ids)))
    )
  cont <- content(resp)
  story_ids <- names(cont$word_matrix)
  get_story <- function(story_id){
    url <- glue(base_url,'single/', story_id, '?key=', mc_key)
    content(GET(url)) %>% 
      bind_cols() %>% 
      mutate(across(everything(),
                    ~as.character(.)))
  }
  v <- lapply(cont$word_matrix, unlist)
  r_names <- story_ids
  c_names <- map_chr(cont$word_list,~nth(.,2))
  if(length(v)>0){
    list(
      triplet =
        simple_triplet_matrix(
          i = unlist(map(seq_along(v),  ~ rep(., length(
            v[[.]]
          ))), use.names = F),
          j = unlist(map(v,  ~ as.numeric(names(
            .
          ))), use.names = F) + 1,
          v = unlist(v, use.names = F),
          dimnames = list(rows = r_names,
                          cols = c_names)
        ),
      story_info =
        story_ids %>%
        map_dfr( ~ get_story(.))
    )
  }else{
    return(list())
  }
}

weeks <- paste0('[',
                as.Date('2020-01-01') +lubridate::weeks(c(0:51)),
                'T00:00:00Z TO ',
                as.Date('2020-01-01') +lubridate::weeks(c(1:52)),
                'T00:00:00Z]')


plan(multicore(workers = 8))

swedish_stories <- weeks %>%
  future_map( ~ get_stories(
    .,
    '(covid OR corona) AND (tyskland OR tysk)',
    swe_sources$media_id
  ),.progress = T)

german_stories <- weeks %>%
  future_map( ~ get_stories(
    .,
    '(covid OR corona) AND (schweden OR schwedisch)',
    c(ger_sources$media_id,19831)
  ), .progress = T)


save(list = ls(),file = '/home/brede/fh_nextcloud/Data Science/dataViz/covid_story/data/collected_data.RData')