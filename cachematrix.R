## These functions take a square matrix and return its inverse
## Once it is called, the given matrix and its inverse are cached
## If the given matrix is changed, it is cached again, and so its inverse

## The first function is a list of functions to handle the caching
## It also returns the given matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The second function checks if the inverse of the given matrix has been
## calculated, if so it just returns it, if not it calculates and returns it
cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}