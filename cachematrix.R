## Program contains two functions. The first, makeCacheMatrix, will cache an input matrix 
## and create functions for retrieving the input matrix and its inverse and for caching
## the inverse matix. The second function, cacheSolve, will return the cached inverse
## matrix if it exists or - if it does not - calculate the inverse matrix and cache it.


## The makeCacheMatrix function creates a list of functions that 1) caches the input 
## matrix and creates functions to 2) return the cached input matrix, 3) cache the inverse
## matrix, and 4) return the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL               # initialize the makeCacheMatrix variable, inv, that will
                                  # contain the cached inverse matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }                         # Set function initalizes the caching variable x to the
                                  # matrix that was input at the makeCachematrix call.
                                  # Set function also initializes to NULL the caching variable
                                  # inv that will contain the cached inverse matrix.
        
        get <- function() x       # Get function returns the cached input matrix in variable x 
                                  # from the makeCacheMatrix environment.
        
        setinv <- function(inverse) inv <<- inverse
                                  # Setinv function sets the cached inverse matrix
                                  # stored in the makeCacheMatrix environment variable inv 
                                  # to the value passed in the setinv call.

        getinv <- function() inv  # Getinv function returns the cached inverse matrix 
                                  # in variable x from the makeCacheMatrix environment.
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     # Returns a list of the four functions
}


## The cacheSolve function either returns the cached inverse matrix if the input
## matrix is unchanged or recalculates and returns the inverse matrix if the input
## matrix has changed.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()         # Retrieves the cached inverse matrix from the 
                                  # makeCacheMatrix inv variable and assigns it to 
                                  # cacheSolve's inv variable.
        
        if(!is.null(inv)) {       # inv will be null if the input matrix has changed.
                message("getting cached data")
                return(inv)       # Function returns cached inverse matrix if it exists.
        }
        data <- x$get()           # Otherwise, the local variable data is populated with
                                  # the cached input matrix.
        inv <- solve(data, ...)   # The inverse for the new input matrix is calculated. 
        x$setinv(inv)             # It is cached in makeCacheMatrix's inv variable.
        inv                       # The newly calculated inverse matrix is returned.
}
