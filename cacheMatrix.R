# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly
# This pair of functions cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  # Instantiate m with null

  m <- NULL

  set <- function(y) {

    # Initialize x in parent environment with value of y

    x <<- y

    # Clears m in parent environment

    m <<- NULL
  }

  get <- function() x

  # Initialize m with inverse matrix

  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()

  # Check for cached invese matrix in variable x (stored in m), and if
  # there is a cache version, exit function returning cached m value

  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  # Otherwise get matrix, solve inverse, and store value back into
  # object x and return inverse

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
