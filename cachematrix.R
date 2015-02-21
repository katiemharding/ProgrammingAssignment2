#This program calculates the inverse of a matrix.
#It uses the ginv() funciton from the MASS library.
library(MASS)
#The first time you calculate the inverse of the matrix you need to set your calculations as a list.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setginv <- function(ginv) m <<- ginv
  getginv <- function() m
  list(set = set, get = get,
       setginv = setginv,
       getginv = getginv)
}

#This actually gives the inverse of the matrix.  
#The first time this runs, it just reports the calculations.
#The second time this runs you will see the message "getting cached data" and 
#it reports the last calculaiton.  This saves time.
cacheSolve <- function(x, ...) {
  m <- x$getginv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setginv(m)
  m
}
#To impliment this function, create a matrix and run cacheSolve like this
M1=matrix(1:8, byrow=TRUE, nrow=2)
Z1=makeCacheMatrix(M1)
cacheSolve(Z1)
