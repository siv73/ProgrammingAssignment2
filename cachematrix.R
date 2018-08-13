## makeCacheMatrix() creates an R object that stores a matrix and inverse. 
## The second function, cacheSolve() requires an argument that is returned 
## by makeCacheMatrix() in order to retrieve the inverse from the cached value 
## that is stored in the makeCachedMatrix() object's environment.

## makeCacheMatrix() creates an R object that stores a matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
## initializing inverse to null and also initializing x to matrix in the
## function argument        
        x.inverse <- NULL
## set the matrix
        set <- function(z){
                x <<- z
                x.inverse <<- NULL
        }
## function to get the matrix.
        get <- function() x
## setting the inverse of the matrix
        set.inverse <- function(inverse) x.inverse <<- inverse
## getting the inverse of the matrix.
        get.inverse <- function() x.inverse
##pushing the return values to the list with names for each value
## so that we can access them by using the dollar '$' sign.
        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## Retrieves the inverse from the cached value 
## that is stored in the makeCachedMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
##setting the value from the get.inverse() function to variable
        x.inverse <- x$get.inverse()
## checking whether the value of the above value is NULL
##if not NULL, this will return the value that is already cached
##since return statement is considered the end of the function 
##execution will stop here and computing time is saved by fetching the
##value of inverse from cache.
        if(!is.null(x.inverse)) {
                message("getting cached data")
                return(x.inverse)
        }
##if the condition in the above if condition fails, then the below line
## is executed
        data <- x$get()
##once we set a value to data from the list of makeCacheMatrix
##we can solve the equation for inverse using the solve function
   
        x.inverse <- solve(data,...)
##setting the inverse value to the set.inverse function
        x$set.inverse <- x.inverse
##returning the calculated inverse value
        x.inverse
}
