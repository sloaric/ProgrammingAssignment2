## These two functions take an invertible matrix and either solve for the inverse or
## take the inverse from the cache if it is already there

## This first function, creates a special "matrix", which is
## really a list containing functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
           m <- NULL
           set <- function(y) {
                   x <<- y
                   m <<- NULL
           }
           get <- function() x
           setinverse <- function(solve) m <<- solve
           getinverse <- function() m
           list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the 'makeCacheMatrix' function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
}
