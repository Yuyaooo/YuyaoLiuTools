#' Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyDataP(d)

plotMyDataP<-function(x){
  library(magrittr)
 x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyDataA(d)
#' 

plotMyDataA<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_area()
}

#' Wrapper function for ggplot2 for data d
#'
#' Computes the mean, variance and sd of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(d)
#' plotMyDataL(d)
#' 

plotMyDataL<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_line()
}


#' dplyr Wrapper
#'
#' This is a wrapper for DPLYR
#'
#' @param x A data-frame
#' 
#' @return data-frame
#' @export
#' @examples 
#' data(d)
#' dplyrWrapper(d)
dplyrWrapper <- function(x) {
  library(magrittr)
  xa<- d %>% dplyr::mutate(mean = sum(x*p))
  xa
}

#mutatedData[1:4, ] %>% dplyr::select(x, p, mean)

