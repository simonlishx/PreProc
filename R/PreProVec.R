

#
#
# This is  five  preprocess methods
#
#
#
# TransA <- preProVec(A,"Na")
# TransA <- preProVec(A,"LnNa")
# TransA <- preProVec(A,"LN2")
# TransA <- preProVec(A,"LN1")
# TransA <- preProVec(A,"disc:5.0087")
#
#

preProVec <-function(A,B){


  dima = length(A)
  if(is.null(dima)|(dima<=1))stop("A should be a matrix with 2 or more columns")

  if(!is.character(B)) stop("B should be a string")

  dyn.load("preproc.dll")

  TransA <- rep(NA,length(A))

  if(B == "Na")
  {
    A[is.na(A)] <- 0
    TransA <- A
  }
  else if(B == "LnNa")
  {
    A[is.na(A)] <- 0
    TransA <- LN1(A)
  }
  else if(B == "LN2")
  {
    TransA <- LN2(A)
  }
  else if(B == "LN1")
  {
    TransA <- LN1(A)
  }
  else if(substr(B,1,4) %in% "disc")
  {
    m <- rep(as.numeric(substr(B,6,nchar(B))),nchar(B))
    TransA <- disc(A,m)
  }




  if(substr(B,1,4) %in% "disc")
  {
    TransA <- as.factor(TransA)
  }



  return(TransA)

  dyn.unload("preproc.dll")
}
