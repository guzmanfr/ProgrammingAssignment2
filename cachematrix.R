##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## FJGC
## 29-May-2020
##

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##
## cacheSolve: this function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
##
## FJGC
## 29-May-2020
##

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if (!is.null(j)){
    message("Getting cached data ...")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
