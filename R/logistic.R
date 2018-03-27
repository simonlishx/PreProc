
#
# This is glm fit function
#
#
#
# fit <- GLMLogistic(A)
#
# fit <- GLMLogistic(A,50)
#
#
# fit <- GLMLogistic(A,2,3)
#
#

GLMLogistic <- function(A,m=NULL,n=NULL,Maxit =100)
{
  np=dim(A)
  if(is.null(np)|(np[2]<=1))stop("A should be a matrix with 2 or more columns")

  x<- paste(names(A)[1],"~", (names(A)[2]))

  for(i in 3 : length(names(A)))
  {

    x <- paste(x,"+", names(A)[i])

  }
  if(!is.null(m)  & !is.null(n) )
  {
    if(m==1 |n==1) stop("i and j  should not be first column of A")

    if(m >np[2] |n >np[2]) stop("i and j  should not be greater than columns of A")

    if(m==n ) stop("i and j should not be same")

    x <- paste(x, "+", names(A)[m], ":" ,names(A)[n])
  }

  fit <- glm(x, A,family = binomial(link='logit'), control=list(maxit=Maxit) )

  return(fit)
}
