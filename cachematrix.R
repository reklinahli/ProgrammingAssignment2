## makeCacheMatrix and cacheSolve functions provide us to save time for calculating
## a matrix's inverse. makeCacheMatrix function creates a matrix that caches its
## inverse. So each time we don't have to re-compute the matrix's inverse if it
## is needed to be call. cacheSolve function computes the inverse of the matrix.
## It calls the matrix from makeCacheMatrix as its parent environment.
## 

## makeCacheMartix function builds a set of listed functions and return these to
## the parent environment. The function contains four functions such as; set(),
## get(), setinv(), getinv(). And it includes two data objects x and inv.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function completes the makeCache Matrix function. As its designs
## the function is required to populate and/or retrieve the inverse of a square
## matrix. the solve() function only defines in the cacheSolve function so when
## cacheSolve function is executed it takes require arguments from makeCacheMatrix
## functions as its parent environment.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
