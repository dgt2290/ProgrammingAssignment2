## Pair of functions that cache the inverse of a matrix
## Usage: Pass the result of a makeCacheMatrix call to cacheSolve 

#' Util function that set the matrix and the inverse in an environment
#' @param x an invertible matrix
#' examples
#' x = makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
#' x$set(matrix(c(2,1,6,4), 2, 2)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#' Compute and cache the inverse of a matrix
#' @param x the result of a previous makeCacheMatrix call
#' @param ... additional arguments to pass to solve function
#' examples
#' x = makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
#' cacheSolve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
