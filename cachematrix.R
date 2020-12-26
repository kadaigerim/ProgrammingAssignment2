# makeCacheMatrix and cacheSolve functions can be used to calculate the inverse
# of a matrix once and cache it so successive requests for the inverse return the
# inverse already cached and solved.


## makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## variable to cached the inverse
    inverseCached <- NULL
    
    ## get and set for the "normal" matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    
    ## get and set for the cached inverse
    setInverse <- function(inverse) inverseCached <<- inverse
    getInverse <- function() inverseCached
    
    ## the "special" matrix is just a list conteining getter and setter functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
    
    ## get the inverse from the cache
    inverse <- x$getInverse()
    
    ## if the inverse is already solved then return it
    if(!is.null(inverse)) {
        
        message("getting cached data")
        return(inverse)
    }
    
    ## Otherwise then get the matrix and solve and calculate its inverse
    data <- x$get()
    
    message("solving inverse for first time")
    inverse <- solve(data, ...)
    
    ## cache the inverse before return it
    x$setInverse(inverse)
    
    inverse
}
