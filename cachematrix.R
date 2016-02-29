## Put comments here that give an overall description of what your
## functions do

## Following function calculates the special "matrix"
## load the value for matrix/inverse matrix
## recieve the value for matrix/inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The above function is calculated using the cachesolve function as state below
## checks if the inverse is calculate at first, if not, it calculated the inverse function first
## after obtaining the inverse the setinverse is found

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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

