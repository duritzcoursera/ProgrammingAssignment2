# makeCacheMatrix and cacheSolve work together to provide caching capability for matrix inversion.
# makeCacheMatrix provides a list of functions to store and provide a matrix and its inverse.
# When called with different matrices, each's environment is different, and they will be cached
# separately. cacheSolve checks for a cached inverse and returns it if it exists. If not, it takes
# the matrix, inverts it, caches it using makeCacheMatrix$setinverse and returns it.

# makeCacheMatrix provides a list of objects containing functions:
# 1) set - stores the matrix to be used (can also be included via argument)
# 2) get - retrieves the matrix stored earlier
# 3) setinverse - stores the inverse of the matrix (caching it for later use)
# 4) getinverse - retrieves the inverse of the matrix if it has already been cached, otherwise returns null

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Stores the matrix to be used
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Retrieves the matrix stored earlier
  get <- function() x
  # Stores the inverse of the matrix (caching it for later use)
  setinverse <- function(solve) m <<- solve
  # Retrieves the inverse of the matrix if it has already been cached, otherwise returns null
  getinverse <- function() m
  # Creates the list of functions and returns it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve 

cacheSolve <- function(x, ...) {
  # Use the getinverse function to check whether this matrix's inverse is cached.
  m <- x$getinverse()
  # If an inverse already exists, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise get the matrix stored, find its inverse, cache the inverse, and return it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
