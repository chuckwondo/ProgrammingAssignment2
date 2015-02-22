##
## Creates a special "matrix", which is really a list containing the following
## functions, respectively:
##
## 1. set: Sets the value of the matrix
## 2. get: Gets the value of the matrix
## 3. setinverse: Sets the inverse of the matrix (assumes the matrix is
##    invertible)
## 4. getinverse: Gets the inverse of the matrix
##

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


##
## Returns the inverse of the special "matrix" x (created by makeCacheMatrix),
## assuming x is invertible.
##

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if (!is.null(inv)) {
        print("getting cached data")
    } else {
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
    }

    inv
}
