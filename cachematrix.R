## Apparently matrix inversion is hella expensive, so these functions will
## add the ability to cache those results

## Constructor for a matrix object that caches its results for future reuse
##  CacheMatrix funcs:
##    - set - add the non-inverted matrix
##    - get - retrieve the non-inverted matrix
##    - setInverse - add the inverted matrix
##    - getInverse - retrieve the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) invMatrix <<- inv
  getInverse <- function() invMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## MAKE IT RAIN CACHE MONEY, YO!
## Compute and return the inverse of a CacheMatrix object (or recall from the
## cache if the result has already been calculated)
cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Cache hit. Returning stored data...")
    return(invMatrix)
  }
  ## Cache miss. WE'RE DOING IT LIVE...
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}
