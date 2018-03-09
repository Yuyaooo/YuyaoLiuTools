#' Calculate Mean, Variane, SD
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func1(rnorm(10))
func1 = function(x){
  a = sum(x)/length(x)  
  b = sum((x - a)^2) / length(x)   
  c = sqrt(b)  
  return(list(mean = a, var = b, sd = c))
}
t
#' Calculate Mean, Variane, SD (again)
#'
#' Computes the mean, variance and sd of a vector, but with user checks
#'
#' @param x vector
#'
#' @return list
#' @export
#' @examples
#' func2(rnorm(10))
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))
  
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}

#' MLE of gamma distribution
#'
#' Computes the liklihood of a gamma distribution
#'
#' @param x vector
#'
#' @return scalar
#' @export
#' @examples
#' func3(rnorm(10))
func3 = function(x){
  alpha = pi   
  func3 = function(alpha){
    sum(dgamma(x, shape = alpha, log = TRUE)) 
  }
  interval <- mean(x) + c(-1, 1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)
  
  oout <- optimize(func3, maximum = TRUE, interval)    
  return(oout$maximum)
}

#' Weighted mean, var, sd
#'
#' Computes the weighted mean, var, sd
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' data(d)
#' func4(d)
func4 <- function(d){
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Weighted mean, var, sd with user checkes
#'
#' Computes the weighted mean, var, sd with user checks
#'
#' @param d data.frame
#'
#' @return list
#' @export
#' @examples
#' d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
#' func5(d)
func5 <- function(d){
  
  stopifnot(is.numeric(d$x))
  stopifnot(is.numeric(d$p))
  
  stopifnot(length(d$x)!=0)
  stopifnot(length(d$p)!=0)
  
  stopifnot(is.finite(d$x))
  stopifnot(is.finite(d$p))
  
  stopifnot(!is.na(d$x))
  stopifnot(!is.na(d$p))
  
  stopifnot(!is.nan(d$x))
  stopifnot(!is.nan(d$p))
  
  stopifnot(all.equal(sum(d$p),1))
  
  a = sum(d$x * d$p)
  b = sum(((d$x - a)^2) * d$p)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
  
}

#' Highlevel check function
#'
#' Checks and throws error if not numeric, finit, zero lenth, NA, NAN
#'
#' @param x object
#'
#' @return object
#' @export
#' @examples
#' func6(NA)

func6 <- function(x){
  
  tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
  tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
  tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
  tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
  tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
  
}

#' MLE 
#'
#' Computes the liklihood of a given distribution for data x
#'
#' @param x vector
#' @param func function, e.g., `function(theta, x) dgamma(x, shape = theta, log = TRUE)`
#' @param interval vector, i.e., interval for optimize function
#'
#' @return scalar
#' @export
#' @examples
#' x1 = rgamma(100,3)
#' func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
#' result7_gamma <- func7(x1,func1,c(0,3))
#' result7_gamma
#' 
func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
} 

#' $x^T A^{-1} x$ and GIEMO
#'
#' Given a numeric matrix A and a numeric vector `x`, 
#' calculates $x^T A^{-1} x$
#' Note that this only makes sense when A is a square matrix 
#' and the dimension of x is the same as 
#' the dimensions of the row and column dimensions of A 
#'
#' Follow GIEMO
#'
#' @param x vector
#' @param a numeric matrix
#'
#' @return scalar
#' @export
#' @examples
#' x <- c(1,2,3,4)
#' a <-matrix(rnorm(16),nc=4,nr=4)
#' 
#' result8 <- func8(a, x)
#' result8
#'

func8 = function(a, x){
  stopifnot(is.matrix(a))
  stopifnot(is.vector(x))
  stopifnot(nrow(a) == ncol(a))
  stopifnot(nrow(a) == length(x))
  stopifnot(is.numeric(a))
  stopifnot(is.numeric(x))
  stopifnot(is.finite(a))
  stopifnot(is.finite(x))
  
  m = solve(a, x)
  
  return(t(x) %*% m)
}


#' Standardization
#'
#' Take a numeric matrix and standardizes its columns
#'
#' @param a matrix which has more than one row
#'
#' @return matrix
#' @export
#' @examples
#' a <-matrix(rnorm(16),nc=4,nr=4)
#' result9 <- func9(a)
#' result9
#' 

func9 <- function(a){
  stopifnot(nrow(a) > 1)
  stopifnot(is.matrix(a))
  stopifnot(is.numeric(a))
  stopifnot(is.finite(a))
  
  f = function(x){
    return((x-mean(x))/sd(x))
  }
  
  return (apply(a, 2, f))
}

#' myapply
#'
#' Myapply function just like the function array
#' function(X, MARGIN, FUN, ...)
#'
#' @param  X a matrix of any type a matrix can have 
#' (numeric, character, logical, complex)
#' @param MARGIN either (the number) 1 or (the number) 2
#' @param FUN an R function that maps vectors to vectors
#' 
#' @return array or matrix
#' @export
#' @examples
#' fred <- matrix(1:6, ncol = 2)
#' s = array(rnorm(72),c(3,3,8))
#' result10 <- myapply(fred,1,"mean")
#' result10
#' 

myapply <- function(X, MARGIN, FUN, ...)
{
  
  #stopifnot(length(dim(X))==2)
  
  if(length(dim(X))!=2)
  {
    stop("matrix is not 2d")
  } 
  if(!(MARGIN %in% c(1,2)))
  {
    stop("margin is not in 1 or 2")
  }
  R = dim(X)[1]
  C = dim(X)[2]
  f = match.fun(FUN)
  
  if (MARGIN == 1)
  {
    result = list()
    for(i in 1:R)
    {
      result[[i]] = f(X[i,],...)
    }
  }else if(MARGIN == 2)
  {
    result = list()
    for(j in 1:C)
    {
      result[[j]] = f(X[,j],...)
    }
  }
  return(simplify2array(result))
}


