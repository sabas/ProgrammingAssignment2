## This function creates an "object" list containing a matrix and functions to get and set
## its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  ## set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## save the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  ## get the inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setinverse = setInverse,
       getInverse = getInverse)
}


## Given a CacheMatrix "object" (a list!), perform the inverse on its matrix 
## and saves it if it wasn't previously done

cacheSolve <- function(x, ...) {

  ## get the cached result from the "object" x
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }

  ## get the cached matrix
  data <- x$get()

  ## this function calculates the inverse matrix
  inv <- solve(data, ...)

  ## cache the result in the "object" x
  x$setInverse(inv)
  inv
}
