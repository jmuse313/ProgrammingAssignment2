## The pair of functions below provide a matrix object which can cache its inverse.

##makeCacheMatrix: This function creates a special "matrix" object that can cache 
##its inverse. (1) Sets the value of a matrix, (2) gets the value of a matrix, 
##(3) sets the value of the inverse, (4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
##has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
