
require(magrittr)
require(dplyr)
require(tidyr)

mtcars_factor = mtcars %>%
  mutate( id = row.names(.) ) %>%
  mutate_at( vars( cyl, vs, am, gear, carb), as.factor ) %>%
  as_tibble()


usethis::use_data(mtcars_factor)
