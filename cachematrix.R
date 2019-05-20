## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates an object to cache an inverse matrix, returning a list of functions
## to manipulate this cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Computes the inverse matrix, returning the cached inversed matrix, if it 
## has already been calculated, or executing this mathematical operation
## (and storing this operation in the cache)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    
    # Creating an indetity matrix
    n <- nrow(data)
    matrix.indentity <- matrix(0, n, n)
    diag(matrix.indentity) <- 1
    
    # Computing the inverse matrix
    inv <- solve(data, matrix.indentity)
    
    # Storing computed matrix
    x$setInverse(inv)
    
    # Returning computed value
    inv
}
