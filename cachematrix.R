## Functions for caching the inverse of a matrix.
# Functions are to be used especcialy to cache time-consuming computations 
#  - especcialy with large matrices

### ===== makeCacheMatrix ======
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	    cacheMatrix <- NULL
        set <- function(y) {
                x <<- y
                cacheMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) cacheMatrix <<- solve
        getinverse <- function() cacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

### ====== cacheSolve ======
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
	
	    cacheMatrix <- x$getinverse()
        if(!is.null(cacheMatrix)) {
                message("getting cached data")
                return(cacheMatrix)
        }
        data <- x$get()
        cacheMatrix <- solve(data)
        x$setinverse(cacheMatrix)
        ## Return a matrix that is the inverse of 'x'        
        cacheMatrix
}
