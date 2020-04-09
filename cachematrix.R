## Assignment: Caching the Inverse of a Matrix
## R Programming on Cousera Week 3 Assignment
## Date : 9 Apr, 2020 3:40PM IST

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     matinv <- NULL
     set <- function(y) {
          x <<- y
          matinv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) matinv <<- inverse
     getinv <- function() matinv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     matinv <- x$getinv()
     if(!is.null(matinv)) {
          message("getting cached data")
          return(matinv)
     }
     data <- x$get()
     matinv <- solve(data, ...)
     x$setinv(matinv)
     matinv
}
