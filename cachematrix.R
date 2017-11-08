## The functions below calculate the inverse of a matrix. Upon subsequent 
## calculation of the inverse of the same matrix, the result will be retrieved 
## from the cache, rather than performing the calculation again.

## makeCacheMatrix creates a vector through which 4 functions can be executed:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve solves the inverse of a matrix. It will first determine if the 
## inverse of the matrix has already been solved. If so, it will retrieve it 
## from the cache. If not, it will do the calculation and store the result in
## the cache.

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
