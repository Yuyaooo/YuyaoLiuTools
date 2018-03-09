context("Homework functions")

test_that("func1 computes mean, var, sd", {
         x <- 1:10
         var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
         x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
         expect_identical(func1(x), x_list)
         })

test_that("func2 computes mean, var, sd", {
  x <- 1:10
  var1<-function(x){(1/length(x))*sum((x-mean(x))^2)}
  x_list<-list(mean=mean(x),var=var1(x),sd=sqrt(var1(x)))
  expect_identical(func2(x), x_list)
  save<-try(func2(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func3 computes MLE of gamma distribution", {
  x <- 1:10
  alpha = pi   
  f = function(alpha){
  sum(dgamma(x, shape = alpha, log = TRUE)) 
 }
 interval <- mean(x) + c(-1, 1) * 3 * sd(x)
 interval <- pmax(mean(x) / 1e3, interval)

 oout <- optimize(f, maximum = TRUE, interval)
  expect_identical(func3(x), oout$maximum)
})

test_that("func4 computes weighted mean, var, sd", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  var1<-function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list<-list(mean= sum(d$x * d$p),var=var1(d),sd=sqrt(var1(d)))
  expect_identical(func4(d), x_list)
})

test_that("func5 computes weighted mean, var, sd with user checkes", {
  d <- read.table(url("http://www.stat.umn.edu/geyer/3701/data/q1p4.txt"),header = TRUE)
  var1<-function(d){sum(((d$x - sum(d$x * d$p))^2) * d$p)}
  x_list<-list(mean= sum(d$x * d$p),var=var1(d),sd=sqrt(var1(d)))
  expect_identical(func5(d), x_list)
  #save<-try(func5(NA),silent=TRUE)
  #save
  #expect_identical(as.character(attr(save,"condition")),"Error: is.numeric(x) is not TRUE\n")
})

test_that("func6 is highlevel check function", {
  func6a <- function(x){
    
    tryCatch(stopifnot(is.numeric(x)), error=function(e){print("not numeric")})
    tryCatch(stopifnot(is.finite(x)), error=function(e){print("not finite")})
    tryCatch(stopifnot(length(x)!=0), error=function(e){print("has 0 length")})
    tryCatch(stopifnot(!is.nan(x)), error=function(e){print("NA or NAN")})
    tryCatch(stopifnot(!is.na(x)), error=function(e){print("NA or NAN")})
    
  }
  expect_identical(func6(NA), func6a(NA))
})

test_that("func7 computes MLE", {
  x1 = rgamma(100,3)
  func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)
  result <- function(x,func,interval){
    f7 <- function(theta, x)
    {sum(func(theta, x))}
    oout<- optimize(f7, maximum = TRUE, interval, x=x)
    return(oout$maximum)
  }
  test <- result(x1, func1, c(0,3))
  expect_identical(test, func7(x1, func1, c(0,3)))
})

test_that("func8 computes $x^T A^{-1} x$ and GIEMO", {
  x <- c(1,2,3,4)
  a <-matrix(rnorm(16),nc=4,nr=4)
  a1 <- solve(a, x)
  p <- t(x) %*% a1
  expect_identical(p, func8(a,x))
  save<-try(func8(a, NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: nrow(a) == length(x) is not TRUE\n")
})

test_that("func9 computes Standardization", {
  a <-matrix(rnorm(16),nc=4,nr=4)
  f = function(x){return((x-mean(x))/sd(x))}
  expect_identical(func9(a), apply(a, 2, f))
  save<-try(func9(NA),silent=TRUE)
  expect_identical(as.character(attr(save,"condition")),"Error: is.matrix(a) is not TRUE\n")
})

  
test_that("myapply computes like the function array function(X, MARGIN, FUN, ...)", {
    fred <- matrix(1:6, ncol = 2)
    s = array(rnorm(72),c(3,3,8))
    p <- apply(fred, 1, mean)
    expect_identical(myapply(fred, 1, mean), p)
  })




