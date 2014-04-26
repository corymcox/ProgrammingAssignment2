## Matrix inversion is usually a costly computation and code execution
## can benefit from caching the inverse of a matrix rather than computing it repeatedly.
## The following functions provide such a caching mechanism wrapped around the base R function "solve".

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL                                   ## Load the cache as NULL
        set <- function(y) {
                x <<- y                                 ## Assign matrix from caller argument to x 
                cache <<- NULL                          ## Ensure cache is cleared
        }
        get <- function() x                             ## Get returns cached matrix (or NULL)
        setinverse <- function(solve) cache <<- solve   ## Set inverse 
        getinverse <- function() cache                  ## Get inverse from cache
        list(set = set, get = get,      
             setInverse = setinverse,
             getInverse = getinverse)                   ## Return list of functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()                         ## Assign cached value
        if(!is.null(cache)) {                           ## If cache is not NULL
                message("getting cached data")          ## Indicate to user that value is cached
                return(cache)                           ## Return the cached value without calculation
        }
        original <- x$get()                             ## Otherwise, call list functions to obtain original matrix
        cache <- solve(original)                        ## Perform solve on matrix to obtain inverse
        x$setInverse(cache)                             ## Save back to cache
        cache                                           ## Return the computed value as well
}
