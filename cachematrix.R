# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

## Results
## 
## > seq1 <- seq(1:4)
## > mat1 <- matrix(seq1, 2)
## > mat1
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##       
## > mat2 <- solve(mat1)
## > mat2
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##       
## > inv <- makeCacheMatrix(mat1)
## > cacheSolve(inv)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##       
## > cacheSolve(inv)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5   


