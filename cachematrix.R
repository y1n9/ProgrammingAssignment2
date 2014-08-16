## Pair of functions makeCacheMatrix and cacheSolve to cache the inverse of a matrix.
## 
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the matrix inverse
##     get the value of the matrix inverse
## 
## cacheSolve calculates the matrix inverse. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse and 
## sets the value of the inverse in the cache via the setinverse function.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
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

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
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
