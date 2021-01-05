
load('/home/brede/fh_nextcloud/Data Science/dataViz/covid_story/data/pre_steps.RData') 

library(httr)
library(glue)
library(slam)
library(furrr)
library(tidyverse)

get_stories <- function(time_range,query,media_ids){
  require(httr) # GET and POST-requests
  
  base_url <- 'https://api.mediacloud.org/api/v2/stories_public/' # basic API-URL
  
  query_string <- glue('?q=text:', query) # add the query
  
  url <- glue(base_url,'word_matrix', query_string, '&key=', mc_key) # add the endpoint and the API-key
  
  resp <-
    POST(
      glue(
        url,
        '&publish_day=',time_range, # add time range
        '&rows=2000&stopword_length=long' # add the short filters
      ),
      body = jsonlite::toJSON(list(fq = list(media_id = media_ids))) # add the longer filters
    ) # POST the request
  
  cont <- content(resp) # get the response
  
  story_ids <- names(cont$word_matrix) # extract the row-indices to get the meta-data
  
  get_story <- function(story_id){ # helper function to query every story-id
    url <- glue(base_url,'single/', story_id, '?key=', mc_key) # Endpoint and API-key
    content(GET(url)) %>%  # Get the Meta-info and format it prettier
      bind_cols() %>% 
      mutate(across(everything(),
                    ~as.character(.)))
  }
  
  v <- lapply(cont$word_matrix, unlist) # count per document as list of respective vectors
  
  r_names <- story_ids # doc-names for our doc-term matrix
  
  c_names <- map_chr(cont$word_list,
                     ~nth(.,2)) # get the first non-stemmed term as the words for the doc-term matrix
  
  if(length(v)>0){ # check if there even is any word, i.e. if any article was collected
    list( # return a list with
      triplet = # a simple triplet that indicates every nun-zero combination of words and documents
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
      story_info = # and a tibble with all meta-information
        story_ids %>%
        map_dfr( ~ get_story(.))
    )
  }else{
    return(list()) # if there is no word in the matrix return an empty list
  }
}

weeks <- paste0('[',
                as.Date('2020-01-01') +lubridate::weeks(c(0:51)),
                'T00:00:00Z TO ',
                as.Date('2020-01-01') +lubridate::weeks(c(1:52)),
                'T00:00:00Z]')


plan(multicore(workers = 6))

swedish_stories <- weeks %>%
  future_map( ~ get_stories(
    .,
    '(covid OR corona) AND (tyskland OR tysk)',
    swe_sources$media_id
  ),.progress = T)

german_stories <- weeks %>%
  future_map( ~ get_stories(
    .,
    query = '(covid OR corona) AND (schweden OR schwedisch)',
    media_ids = ger_sources$media_id
  ), .progress = T)


save(german_stories, swedish_stories,file = '/home/brede/fh_nextcloud/Data Science/dataViz/covid_story/data/collected_data.RData')
