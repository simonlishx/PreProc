
#
#
#
# This is an three  function for preprocess
# use dynamic link Library developed by C
#
# LN2, LN1 is logrithm fuction
#
#
# disc is for discretization
#
#

LN2 <-function(A){

  dima = length(A)
  if(is.null(dima)|(dima<=1))stop("A should be a vector with 2 or more components")

  .C("LN2", as.numeric(A), length(A),ans = numeric(length(A)))$ans
}

LN1 <-function(A){

  dima = length(A)
  if(is.null(dima)|(dima<=1))stop("A should be a vector with 2 or more components")

  .C("LN1", as.numeric(A), length(A),ans = numeric(length(A)))$ans

}

disc <-function(A,B){

  dima = length(A)
  if(is.null(dima)|(dima<=1))stop("A should be a vector with 2 or more components")

  dimb = length(B)
  if(is.null(dimb)|(dimb<=1))stop("B should be a vector with 2 or more components")

  .C("disc", as.numeric(A), length(A), as.numeric(B), ans = numeric(length(A)))$ans

}
