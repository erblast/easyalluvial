# get predictions compatible with the partial dependence plotting method, sequential variant that only works for numeric predictions.

has been replaced by pdp_predictions which can be paralelized and also
handles factor predictions. It is still used to test results.

## Usage

``` r
get_pdp_predictions_seq(df, imp, m, degree = 4, bins = 5, .f_predict = predict)
```

## Arguments

- df:

  dataframe, training data

- imp:

  dataframe, with not more then two columns one of them numeric
  containing importance measures and one character or factor column
  containing corresponding variable names as found in training data.

- m:

  model object

- degree:

  integer, number of top important variables to select. For plotting
  more than 4 will result in two many flows and the alluvial plot will
  not be very readable, Default: 4

- bins:

  integer, number of bins for numeric variables, increasing this number
  might result in too many flows, Default: 5

- .f_predict:

  corresponding model predict() function. Needs to accept \`m\` as the
  first parameter and use the \`newdata\` parameter. Supply a wrapper
  for predict functions with x-y syntax. For parallel processing the
  predict method of object classes will not always get imported
  correctly to the worker environment. We can pass the correct predict
  method via this parameter for example
  randomForest:::predict.randomForest. Note that a lot of modeling
  packages do not export the predict method explicitly and it can only
  be found using :::.

## See also

[`get_pdp_predictions`](https://erblast.github.io/easyalluvial/reference/get_pdp_predictions.md)
