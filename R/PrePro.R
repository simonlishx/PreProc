
#
#
# This is  six  preprocess methods
#
#
# b <- c("","Na","LnNa","LN2","LN1","disc:5.0087")
#
#
# TransA <- prePro(A,b)
#
#
#


prePro <-function(A,B){

  np=dim(A)
  if(is.null(np)|(np[2]<=1))stop("A should be a matrix with 2 or more columns")

  dimb = length(B)
  if(is.null(dimb)|(dimb<=1))stop("B should be a matrix with 2 or more columns")

  if(np[2] != length(B)) stop("column of A and B should be same")

  dyn.load("preproc.dll")

  TransA <- matrix(NA,nrow(C),ncol(C))

  for(i in 1:length(B))
  {

    if(B[i] == "Na")
    {
      A[,i][is.na(A[,i])] <- 0
      TransA[,i] <- A[,i]
    }
    else if(B[i] == "LnNa")
    {
      A[,i][is.na(A[,i])] <- 0
      TransA[,i] <- LN1(A[,i])
    }
    else if(B[i] == "LN2")
    {
      TransA[,i] <- LN2(A[,i])
    }
    else if(B[i] == "LN1")
    {
      TransA[,i] <- LN1(A[,i])
    }
    else if(substr(B[i],1,4) %in% "disc")
    {
      m <- rep(as.numeric(substr(B[i],6,nchar(B[i]))),nchar(B[i]))
      TransA[,i] <- disc(A[,i],m)
    }
    else if(B[i] == "")
      TransA[,i] <- A[,i]

  }

  TransA<- as.data.frame(TransA)

  for(i in 1:length(B))
  {
    if(substr(B[i],1,4) %in% "disc")
    {
      TransA[,i] <- as.factor(TransA[,i])
    }
  }


  return(TransA)

  dyn.unload("preproc.dll")
}
