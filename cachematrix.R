## Matrix inversion is usually a costly computation, so I am wirting a pair of 
## functions here to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<-inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv )

}

## This function computes the inverse of the special "matrix" created by the above
## function. If first checks if the inverse has already been calculated 
## (and the matrix has not changed), if so then it retrieves the inverse from the 
## cache. Otherwise it computes the inverse of the matrix and set the inverse matrix 
## in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
