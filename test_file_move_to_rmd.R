library(dplyr)

# dplyr for general data manipulation

library(ggplot2)

# ggplot2 for visualization of data

library(ggridges)

# ggridges for a specific kind of visual display

library(viridis)

# viridis to get gradient of colors in visual display

library(janitor)

library(readr)
install.packages("gt")
library(gt)
library(tidyverse)
library(lubridate)

elections <- read_csv(file = "ps_4_elections-poll-nc09-3.csv", 
                      col_types =  cols(
                      .default = col_character(),
                      turnout_scale = col_double(),
                      turnout_score = col_double(),
                      w_LV = col_double(),
                      w_RV = col_double(),
                      final_weight = col_double(),
                      timestamp = col_datetime(format = "")))

elections

madlib1 <- elections %>% 
  filter(response == "Dem") %>% 
  count()




fav_rep <- elections %>% 
  filter(hrep_fav == "Favorable") %>% 
  count()
und <- elections %>% 
  filter(response == "Und") %>% 
  count()
madlib2 <- fav_rep$n - und$n

madlib3 <- elections %>% 
  filter(gender != gender_combined) %>% 
  count()
  
madlib4 <- elections %>% 
  filter(race_eth == "White") %>% 
  filter(file_race_black != "White") %>% 
  count()

madlib5 <- group_by
response_dem <- elections %>% 
  filter(response == "Dem") %>% 
  mutate(mins = minutes(timestamp)/minute(1)) %>% 
  min
response_dem$mins
  
  # Again, this is not a course in survey weighting. There is an argument that I
  # should just ignore the topic altogether. But, I really like to replicate
  # published results, and that always requires weights. Hence today's
  # monologue. But only two people fell asleep during it --- not kidding! --- so
  # I count that as a victory.
  
  # All you need to know for this class is: Use sum(weight_var) in place of n().
elections %>% 
  select(response, file_race) %>% 
  filter(file_race != "Unknown") %>% 
  filter(response != "3") %>%
  group_by(response,file_race) %>% 
  count() %>% 
  spread(key =  response, value = n) %>% 
  replace_na(0) %>% 
  mutate(all = Dem + Rep + Und) %>% 
  mutate(Dem = Dem / all) %>% 
  mutate(Rep = Rep / all) %>% 
  mutate(Und = Und / all) %>% 
  select(-all) %>% 
  
  # One of the biggest pieces of black magic incantation in R is ungroup(). (I
  # did not mention this in class.) Summary: Whenever you group a tibble (as we
  # do above) the grouping stays with an resulting object, until you explicitly
  # ungroup() it. That can't ever hurt things (right? TFs?) and it often helps,
  # as in this case.
  
  ungroup() %>% 
  
  # You will have a chance to explore many other gt commands in problem set #4.
  # I added two extras that we did not get to in class.
  
  gt() %>% 
  tab_header(
    title = "Polling Results in Arizon 6th Congressional District") %>% 
  
  cols_label(
    educ4 = "Education",
    Dem = "DEM.",
    Rep = "REP.",
    Und = "UND."
  ) %>%
  
  fmt_percent(columns = vars(Dem, Rep, Und),
              decimals = 0) %>% 
  
  # This little pipe is that incantation to take this pretty table, turn it
  # into html, and send it to the md file we are creating. Future versions of
  # gt will probably have a better way of doing this. Indeed, does anyone know
  # of one?
  
  as_raw_html() %>% as.character() %>% cat()