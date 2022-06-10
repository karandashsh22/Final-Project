library(janitor)
soccer <- read.csv('~/Desktop/Spring 2022/PSTAT 131/final project/data/Final Project Data.csv')
soccer <- clean_names(soccer)

soccer <- soccer %>% 
  mutate(result = replace(result, result == "D", "NW")) %>% 
  mutate(result = replace(result, result == "L", "NW"))
head(soccer)

soccer <- soccer %>%
  mutate(venue = factor(venue, levels = c('Home','Away', 'Neutral'))) %>%
  mutate(result = factor(result, levels = c('W','NW'))) %>% 
  mutate(cmp_2 = cmp_2/100) %>% 
  mutate(poss = poss/100) %>% 
  select(-c(touches, so_t_2, g_sh, g_so_t))

head(is.na.data.frame(soccer))


