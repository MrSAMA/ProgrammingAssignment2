## makeCacheMatrix returns a list of functions that we can use to 
## cache the inverse of a matrix.
## The function receives a matrix as a parameter, if the parameter is not given
## a one column matrix is provided

makeCacheMatrix <- function(m = matrix()) {
    cache <- NULL    
    set <- function(y) {
        m <<- y
        cache <<- NULL
    }
    
    #This function returns the stored matrix
    get <- function() m
    
    # store the given argument into the cache
    setCache <- function(solve) {
        cache <<- solve
    }
    
    # get the cache value
    getCache <- function() {
        cache
    }
    
    #Return a named list, where each element is a function
    list(set=set, get=get,
         setCache=setCache,
         getCache=getCache)
}


## cacheSolve function calculates and returns the inverse matrix of the matrix provided as parameter

cacheSolve <- function(x, ...) {
    # get cache value
    inverse <- x$getCache()
    # if cache value exists, it is returned
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    #if cache doesn't exists, the inverse is calculated and it is cached
    data <- x$get()
    inverse <- solve(data, ...)
    x$setCache(inverse)
    inverse
}
