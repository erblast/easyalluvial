

context('Test manipulation functions')


test_that( 'manip_factor_2_numeric'
  ,{

    fac_num = factor( c(1,3,8) )
    fac_chr = factor( c('foo','bar') )
    fac_chr_ordered = factor( c('a','b','c'), ordered = T )

    expect_identical( manip_factor_2_numeric( fac_num ), c(1,3,8) )
    expect_identical( manip_factor_2_numeric( fac_chr ), c(2,1) )
    expect_identical( manip_factor_2_numeric( fac_chr_ordered ), c(1,2,3) )

})


test_that('manip_bin_numerics'
  ,{

  categoricals = c('cyl', 'vs', 'am', 'gear', 'carb')

  data = mtcars %>%
    mutate_at( vars(categoricals), as.factor )

  data_new = manip_bin_numerics(data)

  numerics = data_new %>%
    select_if( is.numeric ) %>%
    names()

  expect_true( is_empty(numerics) )
  expect_true( ! is_empty(data_new) )
  expect_identical( names(data_new) , names(data) )

})


test_that('manip_bin_numerics no numerics in data'
          ,{

  data = mtcars %>%
    mutate_all( as.factor )

  data_new = manip_bin_numerics(data)

  expect_identical(data, data_new)

})

test_that('manip_bin_numerics zero variance columns'
          ,{

  data = mtcars %>%
    as_tibble() %>%
    mutate( zero_var = 1
            , zero = 0
            , near_zero_var = c( rep(1,nrow(.)-1), 0.9 ) )

  data_new = manip_bin_numerics(data)

  expect_identical( select(data, zero_var, zero)
                    , select(data_new, zero_var, zero) )

  expect_true( is.factor(data_new$near_zero_var) )

})

test_that('manip_bin_numerics with vector'
          , {

  vec = manip_bin_numerics(mtcars$mpg)
  expect_true( is.factor(vec) )

  vec = manip_bin_numerics( as.factor(mtcars$cyl) )
  expect_identical( vec, as.factor(mtcars$cyl) )

})
