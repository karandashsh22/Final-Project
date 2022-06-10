# DATA SPLITTING 
set.seed(2000)
soccer_split <- initial_split(soccer, prop = 0.75, strata = result) # stratified on result
soccer_test <- testing(soccer_split)
soccer_train <- training(soccer_split)

soccer %>% 
  ggplot(aes(x = result)) + geom_bar()

soccer %>% 
  ggplot(aes(x = venue, group = result, color = result)) + geom_histogram(stat = 'count')

cor_soccer <- soccer_train %>%
  select(sh, so_t, poss, cmp, cmp_2, ck, crd_y, crd_r, fls, off, pk) %>%
  correlate()
rplot(cor_soccer)

soccer %>% 
  ggplot(aes(x = poss, y = cmp)) + geom_point()

soccer %>% 
  ggplot(aes(x = poss, y = cmp_2)) + geom_point()

soccer %>% 
  ggplot(aes(x = cmp_2, y = cmp)) + geom_point()

soccer %>% 
  ggplot(aes(x = fls, group = crd_y)) + geom_boxplot()

soccer %>% 
  ggplot(aes(x = sh, group = ck)) + geom_boxplot()







