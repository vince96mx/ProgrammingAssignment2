## Assignment 2: Caching the Inverse of a Matrix
# Matrix inversion is usually a costly computation 
# and there may be some benefit to caching the 
#inverse of a matrix rather than compute it repeatedly.

## This function allows to create a special matrix
# object which can cache the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This function is in charge of computing the inverse 
#of the matrix created by the makeCacheMatrix function
#that is above. If the inverse is already in the cache,
#the process to get the inverse is skipped and 
#returns the cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInv()
  if (!is.null(m)){
    message("Extracting cached data...")
    return(m)
    
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInv(m)
  m
}
