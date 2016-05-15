## Coursera R Programming Assignment 2
## twclark 2016-05-15

## makeCacheMatrix creates a list structure the environment and the
## function closures for set, get, setinv and getinv - input is a matrix 
## - cacheSolve operates on one of the list structures created by makeCacheMatrix -  
## first call inverts & caches the matrix - subsequent calls return cached value

## create matrix caching environment and function closures

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## compute, cache and return matrix - or simply retrieve from cache if initialized

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m<- solve(data, ...)
        x$setinverse(m)
        m
}
