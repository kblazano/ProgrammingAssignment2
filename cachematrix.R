## The following pair of functions cache the inverse of a matrix.

## The first function,'makeCacheMatrix' creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
        
}

## The following function,'cacheSolve' computes the inverse of the
## special "matrix" returned by 'makeCacheMatrix' above. If the 
## inverse has already been calculated (and the matrix has not changed),
## then 'cacheSolve' retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv_x <- x$getinverse()
        if(!is.null(inv_x)) {
                message("Getting cached data.")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
                ##solve() function computes the inverse of a square matrix in R
        x$setinverse(inv_x)
        return(inv_x)
        
}