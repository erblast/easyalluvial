

#'Quarterly mean arrival delay times for a set of 402 flights
#'
#'Created from nycflights13::flights
#'
#'@format A data frame with 1608 rows and 6 variables
#'\describe{
#'  \item{tailnum}{
#'  a unique identifier created from tailnum, origin, destination and carrier}
#'  \item{carrier}{carrier code} \item{origin}{origin code}
#'  \item{dest}{destination code} \item{qu}{quarter}
#'  \item{mean_arr_delay}{average delay on arrival as either on_time or late}
#'
#'  }
#'@source nycflights13::flights
#'
"quarterly_flights"




#'mtcars dataset with cyl, vs, am ,gear, carb as factor variables and car model
#'names as id
#'@format A data frame with 32 rows and 12 variables
#'\describe{
#'
#'\item{mpg}{Miles/(US) gallon}
#'\item{cyl}{ Number of cylinders}
#'\item{disp}{Displacement (cu.in.)}
#'\item{hp}{Gross horsepower}
#'\item{drat}{Rear axle ratio}
#'\item{wt}{Weight (1000 lbs)}
#'\item{qsec}{ 1/4 mile time}
#'\item{vs}{Engine}
#'\item{am}{Transmission}
#'\item{gear}{Number of forward gears}
#'\item{carb}{Number of carburetors}
#'\item{id}{car model name}
#'}
#'@source datasets
"mtcars2"