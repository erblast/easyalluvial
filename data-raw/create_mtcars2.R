


mtcars2 <- within(mtcars, {
  vs <- factor(vs, labels = c("V", "S"))
  am <- factor(am, labels = c("automatic", "manual"))
  cyl  <- ordered(cyl)
  gear <- ordered(gear)
  carb <- ordered(carb)
})

mtcars2$ids = row.names(mtcars)

mtcars2 = dplyr::as_tibble(mtcars2)

usethis::use_data( mtcars2, overwrite = TRUE )
