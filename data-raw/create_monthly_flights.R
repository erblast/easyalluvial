
require(magrittr)
require(dplyr)
require(tidyr)
require(purrr)


quarterly_flights = nycflights13::flights %>%
  group_by(month, tailnum, origin, dest, carrier) %>%
  summarise() %>%
  group_by( tailnum, origin, dest, carrier) %>%
  count() %>%
  filter( n == 12 ) %>%
  select( - n ) %>%
  left_join( nycflights13::flights ) %>%
  .[complete.cases(.), ] %>%
  ungroup() %>%
  mutate( tailnum = pmap_chr(list(tailnum, origin, dest, carrier), paste )
          , qu = cut(month, 4)) %>%
  group_by(tailnum, carrier, origin, dest, qu ) %>%
  summarise( mean_arr_delay = mean(arr_delay) ) %>%
  ungroup() %>%
  mutate( mean_arr_delay = ifelse( mean_arr_delay < 10, 'on_time', 'late' ) )

levels(quarterly_flights$qu) = c('Q1', 'Q2', 'Q3', 'Q4')

usethis::use_data(quarterly_flights)
