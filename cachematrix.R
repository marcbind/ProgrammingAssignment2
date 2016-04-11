## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function to create a "closure": we define new functions
# which inherit/copy the environment of "x" and "invcache"
# from the makeCacheMatrix function at the time of creation.
# The access to these variables is possible via these new
# functions.
# The inverse matrix cache is originally NULL and again
# set to NULL every time the matrix value is new set.

makeCacheMatrix <- function(x = matrix()) {
    invcache <- NULL

    # set matrix
    set <- function(y) {
        x <<- y
        invcache <<- NULL
    }
    # get matrix
    get <- function() x

    # set inverse cache
    setinverse <- function(y) invcache <<- y
    # get inverse cache
    getinverse <- function() invcache

    # handing back the "object"
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# cacheSolve tries first to use the cached value, if available
# If no cached value is available the full calculation with
# solve() is done and the result is stored in the case as well
# as provided back to the caller.
# The function parameter "x" is our new "matrix object" created
# with the makeCacheMatrix() function above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrx <- x$get()
    inv <- solve(matrx, ...)
    x$setinverse(inv)
    inv
}
