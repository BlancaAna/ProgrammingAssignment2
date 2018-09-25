## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix function

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)  ## Return a matrix that is the inverse of 'x'
    }
    mat <- x$get()
    invrs <- solve(mat, ...)
    x$setinverse(invrs)
    invrs
}
