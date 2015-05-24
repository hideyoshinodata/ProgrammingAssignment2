# This function creates a special matrix object that can cache its inverse.

# In order to do so, the following steps are taken:
#  1. set the value of "inverse.matrix"
#  2. get the value of the matrix
#  3. set the value of the inverse matrix
#  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.matrix <<- inverse
    getinverse <- function() inverse.matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data)
    x$setinverse(inverse.matrix)
    inverse.matrix
}
