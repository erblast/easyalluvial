


quarterly_sunspots = tibble( spots = sunspots
             , time = time(sunspots)
             , year = as.integer(time)
             , month = time %% 1 ) %>%
  mutate( month = month * 1000
          , month = as.integer(month)
          , month = as.factor(month) )

levels(quarterly_sunspots$month) <- month.abb

quarterly_sunspots = quarterly_sunspots %>%
  mutate( qu = case_when( month %in% c('Jan','Feb','Mar') ~ 'Q1'
                     , month %in% c('Apr','May','Jun') ~ 'Q2'
                     , month %in% c('Jul','Aug','Oct') ~ 'Q3'
                     , T ~ 'Q4') ) %>%
  group_by(year, qu) %>%
  summarise( spots = mean(spots) ) %>%
  group_by(year) %>%
  mutate(  mean_spots_per_year = mean(spots) ) %>%
  ungroup()


usethis::use_data( quarterly_sunspots, overwrite = TRUE )
