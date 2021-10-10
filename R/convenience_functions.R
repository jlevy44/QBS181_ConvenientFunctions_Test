library(stats)

#' Drop NAs by Columns
#'
#' Remove NAs based on specified columns in the data
#'
#' @param data data.frame object of observations 
#' @param desiredCols list of columns from which to drop incomplete cases by
#'
#' @return Data frame with removed observations
#'
#' @examples
#' data <- data.frame(a=1:4,b=c("a","b","c","d"),c=c(NA,"keep",NA,"keep"))
#' completeFun(data, c("c"))
#'
#' @export
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#' Mode
#'
#' Function to calculate the mode of a variable
#'
#' @param x numeric vector 
#'
#' @return Numeric vector of modes
#'
#' @examples
#' x <- c(1,1,3,5,6,6)
#' Modes(x)
#'
#' @export
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  return(ux[tab == max(tab)])
}

#' Geometric mean
#'
#' Function to calculate geometric mean
#'
#' @param x numeric vector 
#' @param na.rm bool, should vector be removed? 
#'
#' @return float
#'
#' @examples
#' x <- c(1.4,1.6,NA,5.2,6.5,6.5)
#' gm_mean(x,na.rm=T)
#'
#' @export
gm_mean <- function(x, na.rm=TRUE){
  return(exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x)))
}

#' Factorial
#'
#' Function that calculates factorial of integer
#' Note: this function is recursive!
#'
#' @param x Integer 
#'
#' @return Factorial integer
#'
#' @examples
#' x <- 13
#' factorial(x)
#'
#' @export
factorial <- function(x){
  if(x==0)
    return(1)
  else
    return(x*factorial(x-1))
} 

#' Non-Unique
#'
#' Function that returns a list of non-unique values from a supplied list
#'
#' @param x vector/list 
#'
#' @return vector/list
#'
#' @examples
#' x <- c(1,1,3,5,6,6)
#' nonUnique(x)
#'
#' @export
nonUnique<-function(x){
  u<-unique(x)
  return(x[x%in%u==F])
}