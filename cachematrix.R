## Functions that define wrapper around a matrix to allow caching matrix inverse
## until the matrix changes, without any additional bookkeeping from a user.

## Wraps given matrix in an object that can cache inverse of the matrix and
## exposes functions to operate on the matrix and its inverse.
## `get` returns the matrix
## `set` sets matrix and invalidates cache
## `setinverse` saves given matrix as inverse
## `getinverse` returns saved inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Returns an inverse of a matrix (assumes non-singularity)
## that is stored inside the wrapper x.
## Retrieves cached inverse if it was computed and matrix didn't change since.
## Otherwise recomputes inverse and saves it in the wrapper.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
cacheSolve(B1)
