#############################################################################
### The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
#############################################################################

## Create a  matrix object
makeCacheMatrix <- function(x = matrix()) {
## Inverse    
    inv <- NULL
## Set matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## Get matrix
    get <- function() x
## Set inverse
    setInverse <- function(inverse) inv <<- inverse
## Get inverse
    getInverse <- function() inv
## List of methods    
    list(set= set, get= get, setInverse= setInverse, getInverse= getInverse)
}

############################################################################
## cacheSolve: This function computes the inverse of the special “matrix” 
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.
#############################################################################

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
## Checking and returning the inverse if it´s already been set
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
## Get the matrix
    db <- x$get()
## Calculating...
    inv <- solve(db, ...)
## Set the inverse
    x$setInverse(inv)
## Matrix
    return (inv)
}
