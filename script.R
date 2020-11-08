
library(dplyr)
library(jsonlite)
library(purrr)
library(janitor)
library(readr)
library(tidyr)
library(stringr)

state_strings <- c("alaska", "texas", "minnesota", "michigan", "west-virginia",
  "virginia", "wisconsin", "kentucky", "louisiana", "mississippi",
  "missouri", "north-carolina", "california", "iowa", "maine",
  "florida", "washington", "illinois", "north-dakota", "maryland",
  "georgia", "tennessee", "new-york", "arkansas", "oklahoma", "nebraska",
  "south-carolina", "idaho", "new-hampshire", "ohio", "south-dakota",
  "vermont", "indiana", "pennsylvania", "montana", "kansas", "oregon",
  "arizona", "alabama", "new-jersey", "hawaii", "massachusetts",
  "nevada", "new-mexico", "colorado", "rhode-island", "wyoming",
  "connecticut", "utah", "delaware", "district-of-columbia")

get_county_dat <- function(x) {

  print(x)

  Sys.sleep(2)

  time <- Sys.time()

  json_url <- glue::glue("https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/race-page/{x}/president.json")

  res <- jsonlite::fromJSON(json_url)

  cleaned <- res[["data"]][["races"]][["counties"]][[1]]  %>%
    rowwise() %>%
    mutate(results = list(as.list(results)),
           results_absentee = list(as.list(results_absentee)),
           state = x,
           retrieved_time = time) %>%
    tidyr::unnest_wider(results, names_sep = "_")  %>%
    tidyr::unnest_wider(results_absentee, names_sep = "_")  %>%
    janitor::clean_names()

  return(cleaned)
}


election_results <- state_strings %>%
  map_dfr(get_county_dat)

real_time <- as.character(election_results$retrieved_time[1]) %>% str_replace_all(":", "-")

dir.create(paste0("data/", real_time))

write_csv(election_results, path = paste0("data/", real_time, "/results_president.csv"))