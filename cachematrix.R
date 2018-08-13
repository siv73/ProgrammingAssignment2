## makeCacheMatrix() creates an R object that stores a matrix and inverse. 
## The second function, cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order to retrieve the inverse from the cached value 
## that is stored in the makeCachedMatrix() object's environment.

## makeCacheMatrix() creates an R object that stores a matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
        x.inverse <- NULL
        set <- function(z){
                x <<- z
                x.inverse <<- NULL
        }
        get <- function() x
        set.inverse <- function(inverse) x.inverse <<- inverse
        get.inverse <- function() x.inverse
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## Retrieves the inverse from the cached value 
## that is stored in the makeCachedMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x.inverse <- x$get.inverse()
        if(!is.null(x.inverse)) {
                message("getting cached data")
                return(x.inverse)
        }
        data <- x$get()
        x.inverse <- solve(data,...)
        x$set.inverse <- x.inverse
        x.inverse
}
