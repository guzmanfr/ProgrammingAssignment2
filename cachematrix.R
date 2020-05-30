##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
## FJGC
## 29-May-2020
##

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  
## From "R documentation":
## The operators <<- and ->> are normally only used in functions, and cause a search to be made through parent environments
## for an existing definition of the variable being assigned
##
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  
  ## Obtains the function to apply
  get <- function() x
  
  ## Sets the inverse from parent environment
  setInverse <- function(inverse) j <<- inverse
  
  ## Saves the inverse 
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
  
  ## Checks "j" is not NULL and returns it
  if (!is.null(j)) {
    message("Getting cached data ...")
    return(j)
  }
  
  ## Obtains the matrix
  mat <- x$get()
  
  ## From "R" Documentation: generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.
  j <- solve(mat, ...)
  
  ## Obtains the inverse
  x$setInverse(j)
  j
}
