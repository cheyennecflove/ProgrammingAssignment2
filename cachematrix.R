##Creates a special matrix that caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(aMatrix) {
        x <<- aMatrix 
        m <<- NULL ##clears prior values
    }
    get <- function() x ##function will get the value of x from makeCacheMatrix
    setinverse <- function(solve) m <<- solve 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
