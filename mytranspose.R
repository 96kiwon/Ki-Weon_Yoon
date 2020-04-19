mytranspose <- function(x){
  if(!is.null(x)){
    initialx <- x
    x <- as.matrix(x)
    y <- matrix(1, nrow = ncol(x), ncol = nrow(x))
    for(i in 0:nrow(x)){
      for(j in 0:ncol(x)){
        y[j,i] <- x[i,j]
        }
      }
    if(is.data.frame(initialx)==TRUE)
       return(as.data.frame(y))
    else if(is.vector(initialx)==TRUE)
       return(as.vector(y))
    else
       return(y)
    }
  else
     return(x)
}
