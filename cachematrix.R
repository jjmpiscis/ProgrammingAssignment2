## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # set the variable inv (inverse) to NULL
    inv <- NULL
    # set m to the argument y and set inv to NULL
    set <- function(y) {
        m <<- y
        inv <<- NULL
    }
    # return the value of m (argument of makeCacheMatrix)
    get <- function() m
    # sets inv in makeCacheMatrix to inverse (argument of makeCacheMatrix)
    setinv <- function(solve) inv <<- solve
    # return the value of inv (from makeCacheMatrix)
    getinv <- function() inv
    # return a labeled vector of functions set, get, setinv and getinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # attempts to get the inverse of m (if it was calculated previously)
    inv <- m$getinv()
    # if not null, a valued was cached, so return inverse 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if it's null, set data to m from makeCacheMatrix
    data <- m$get()
    # calculate the inverse of data
    inv <- solve(data,...)
    #set inv in m to calculated inverse
    m$setinv(inv)
    # return inverse
    inv
}
