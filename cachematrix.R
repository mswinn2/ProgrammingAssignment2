## Two functions are defined below. The first, makeCacheMatrix, creates
# an object which can hold a cache of its inverse, designed to be manipulated
# by other functions. It is not designed to calculate the inverse itself.
# 
# The second function, cacheSolve, calculates the inverse of the matrix in the
# special object created by makeCacheMatrix. It will first try to retrive the
# inverse from cache if it exists for that object, otherwise it will calculate
# the inverse using R's solve() function


## Function to create a matrix object that can cache its inverse
# The object contains functions to:
# - Define the matrix
# - Retrieve the matrix
# - Set the inverse
# - Retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {

    # mInv is the matrix inverse
    mInv = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get = function() x
    setInv = function(matrixInverse) mInv <<- matrixInverse
    getInv = function() mInv
    
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Function to return the inverse matrix of a given invertible matrix
# held as part of an object created by the makeCacheMatrix fuction above.
# Will first try and retrive the result from cache if it has already
# been calculated, otherwise will calculate using R's inbuilt 'solve'
# function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # mInv is the inverse matrix
    mInv = x$getInv()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    data = x$get()
    mInv = solve(data)
    x$setInv(mInv)
    mInv
}