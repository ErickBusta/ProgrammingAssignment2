## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly.


## The next function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
                inv<- NULL
        
                set <- function(y) {
                        x <<- y
                        inv<<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                getInverse <- function() inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }


## This function computes the inverse of the matrix created by makeCacheMatrix above. 
## If the inverse has already been calculated, then it should get back the inverse.


cacheSolve <- function(x, ...) {
                inv <- x$getInverse()
                if (!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                inv
}
