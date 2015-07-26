# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix - "set"
# 2. get the value of the matrix - "get"
# 3. set the value of inverse of the matrix - "setinverse"
# 4. get the value of inverse of the matrix - "getinverse"

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL				# initializing the inverse "I"
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. If the inverse
# has already been calculated (and the matrix has not changed)
# then the cacheSolve retrieves the inverse from the cache.
# If not, it computes the inverse and sets the value in the cache using "setinverse"

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if(!is.null(I)){
                message("getting cached data.")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setinverse(I)
        I
}
