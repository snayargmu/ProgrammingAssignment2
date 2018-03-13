## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. This script file has a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv        
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check to see if the inverse already exists
        inv <- x$getinverse()
        
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
        ## Check to see if the matrix is invertible (i.e. square), if so then invert it
        if (dim(data)[1] == dim(data)[2]) {
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }
        else {
                message("matrix is not square, cannot be inverted")
        }
}
