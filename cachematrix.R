## Put comments here that give an overall description of what your
## functions do

## Creates the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Calculates the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
