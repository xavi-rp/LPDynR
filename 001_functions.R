

#### Compilation of functions used in the project ####


## Step 02 ####

slp_lm <- function(x){ if (all(is.na(x))){ NA } else { lm(x ~ yrs)$coefficients[2] }}

mtid_function <- function(x, na.rm = TRUE){  if (is.na(x[years])){ NA } else {  ((years - 1) * x[years]) - sum(x[1:(years - 1)]) }}  

#https://gis.stackexchange.com/questions/265717/statistical-comparison-between-different-rasters-using-r
RMSE <- function(x, y) { sqrt(mean((x - y)^2, na.rm = TRUE)) } 




## Step 03 ####

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

mean_years_function <- function(x, na.rm = TRUE){ mean(x[yrs]) }  #where x is the data set and yrs is a vector with the years to be averaged (e.g. yrs = c(1:3))


