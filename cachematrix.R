## These two functions calculate the inverse of a matrix and cache the result.

## makeCacheMatrix
## Arguments: a matrix to invert, if not specific an empty matrix is used
## Returns: the a special vector that is capable of caching the inverse
##   of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve
## Arguments: a vector prepared by makeCacheMatrix
## Returns: the inverse of the matrix
## Checks to see if the matrix has already been cached and if so returns
## the cached value, otherwise calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
