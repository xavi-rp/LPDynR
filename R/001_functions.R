# global variables
utils::globalVariables(c("yrs", "EFT", "rn"))


#### Compilation of functions used in the project ####


## Step 02 ####

slp_lm <- function(x){
  if (all(is.na(x))){
    slp <- NA
  } else if (sum(!is.na(x)) == 1){
    slp <- 0
  } else {
    slp <- stats::lm(x ~ yrs)$coefficients[2]
  }
  return(slp)
}

mtid_function <- function(x){
  if (all(is.na(x))){
    mtid <- NA
  } else if (sum(!is.na(x)) == 1){
    mtid <- 0
  } else {
    years1 <- max(which(!is.na(x)))
    mtid <- sum(x[years1] - x[-years1], na.rm = TRUE) # last year with data minus each year; then all summed
  }
  return(mtid)
}



## Step 03 ####

mean_years_function <- function(x){ mean(x[yrs], na.rm = TRUE) }  #where x is the data set and yrs is a vector with the years to be averaged (e.g. yrs = c(1:3))
# If x is NA for all the 'yrs', a condition might be included to take values of
# more recent years. This is not included for now in 'mean_years_function'.

