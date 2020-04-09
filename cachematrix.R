## Assignment: Caching the Inverse of a Matrix
## R Programming on Cousera Week 3 Assignment
## Date : 9 Apr, 2020 3:40PM IST

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
     matinv <- NULL                ## initialize inv as NULL; will hold value of matrix inverse 
     set <- function(y) {          ## define the set function to assign new 
          x <<- y                  ## value of matrix in parent environment
          matinv <<- NULL          ## if there is a new matrix, reset inv to NULL
     }
     get <- function() x           ## define the get fucntion - returns value of the matrix argument
     setinv <- function(inverse) matinv <<- inverse ## assigns value of inv in parent environment
     getinv <- function() matinv   ## gets the value of inv where called
     list(set = set, get = get,    ## returns list
          setinv = setinv,
          getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     matinv <- x$getinv()
     if(!is.null(matinv)) {             ##checks to see if inverse has already been calculated
          message("getting cached data") ##If so, it gets the inverse from the cache and skips the computation
          return(matinv)
     }
     data <- x$get()
     matinv <- solve(data, ...)  ## calculates inverse of matrix
     x$setinv(matinv)         ##sets the value of the inverse in the cache via the setinv function
     matinv                   ## returns inverse of the matrix
}
