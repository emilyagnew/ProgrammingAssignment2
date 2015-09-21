## ASSIGNMENT 2 : CACHING THE INVERSE OF A MATRIX
## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # changes the matrix stored in the main function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get the matrix 'x' stored in the main function
    get <- function() x
    # change the inverse of the matrix 'x' in the main function
    setinv <- function(solve) {
        inv <<- solve
    }
    # get the inverse of the matrix in the main function
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # get the inverse if already calculated
    inv <- x$getinv()
    # if already calculated return cached value
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # else calculate the inverse of 'x'
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}

##INPUT USED TO TEST
#a<-makeCacheMatrix(matrix(1:4, 2, 2))
#cacheSolve(a)
#b<-makeCacheMatrix(matrix(c(3,2,1,1,1,-1,0,1,2), 3, 3))
#cacheSolve(b)
#cacheSolve(a)
#cacheSolve(b)
