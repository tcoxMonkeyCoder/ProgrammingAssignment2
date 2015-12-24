## Matrix inversion is usually a costly computation.
## Rather than computing the inversion repeatedly, these
## functions provide the ability to cache the result.

## makeCacheMatrix lets you set and get the matrix value,
## it also lets you set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## cacheSolve will return the inverse of a matrix.
## It will only compute the inverse if it needs to, meaning
## once computed, it will store the result so it doesn't 
## have to compute it again in the future.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
