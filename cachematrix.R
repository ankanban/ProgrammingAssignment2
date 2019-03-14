## makeCacheMatrix creates a cacheable matrix type.
## A cacheable matrix is a list with named elements that are methods that get/set the matrix
## and its inverse
## The matrix and the inverse are saved in the closure
makeCacheMatrix <- function(...) {
    inv <- NULL
    x <- matrix(...)
    set <- function(...) {
        ## <<- accesses the symbol defined in the closure
        x <<- matrix(...)
        # Reset the inverse when the matrix changes
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a cacheable matrix, and returns its inverse
## When called for the first time, it computes the inverse and caches it
## on subsequent calls, it will return the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    x$setInverse(solve(x$get(), ...))
}
