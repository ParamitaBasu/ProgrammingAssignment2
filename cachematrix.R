## Caching the Inverse of a matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        set <- function(y)
        {
            x<<- y    
            invMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse)
                invMatrix <<- inverse
        
        getInverse <- function() invMatrix
                list(set = set, get = get, setInverse = setInverse, getInverse =  getInverse)
}                
                


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
                
## This function assumes that the matrix is always invertible.           
cacheSolve <- function(x, ...) {
                
        invMatrix <- x$getInverse()
        
        if(!is.null(invMatrix))
        {
                message("Getting the cached data")
                return (invMatrix)
        }
        
        matx <- x$get()
        invMatrix <- solve(matx,...)
        x$setInverse(invMatrix)
        invMatrix
}
