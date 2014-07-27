## These functions set, calculate and cache an invertable matrix and the inverse of that matrix. This avoids the 
## repeated calculation of the matrix's inverse


## Caches a matrix and its inverse. It does not calculate the inverse itself.
## Returns a list of four functions which:
## set - set the matrix
## get - get the matrix
## setInverse - sets the inverse of the matrix
## getInverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates (or fetches from cache if previously calculated) the inverse of a matrix 
## x must be an object returned by a call to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    #message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
