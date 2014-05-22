## The below functions compute and/or cache the inverse of a squared matrix
## makeCacheMatrix is a function that take a matrix object and can cache its inverse
## cacheSolve computes or caches the inverse of the matrix returned by MakeCacheMatrix

## Function creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ma<- NULL
        set<- function(y) {
                x<<-y
                ma<<-NULL
        }
        get <- function() x # Returns the original matrix
        setinverse <- function(solve) ma <<- solve 
        getinverse <- function () ma # Return the matrix inverse
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## Function computes the inverse of the "special" matrix returned by MakeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ma<- x$getinverse()
        if(!is.null(ma)) {
                message("Getting cached data")
                return(ma)
        }
        data <- x$get()
        ma <- solve(data, ...)
        x$setinverse(ma)
        ma
}