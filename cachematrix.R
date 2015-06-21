## These two functions take an input matrix and compute its inverse. The
## first time it is run, the inverse is computed; subsequent calls with
## the same input matrix will result in the inverse coming from the cache.

##Here is how I tested it:
##> source("cachematrix.R")
##> a<-matrix(4:7,2,2)
##> mcm<-makeCacheMatrix(a)
##> ainv<-cacheSolve(mcm)
##computing the inverse
##> ainv
##     [,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
##> ainv<-cacheSolve(mcm)
##getting cached data
##> ainv<-cacheSolve(mcm)
##getting cached data
##> a%*%ainv                      multipling a and ainv should give I
##     [,1]          [,2]
##[1,]    1 -1.776357e-15         this is basically the identity matrix
##[2,]    0  1.000000e+00         






## This function is basically a copy of the first example.
## It creates a list of functions to set and get the matrix and its inverse.
## If the input matrix is updated, the inverse is reset to NULL.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) xinv <<- inverse
        getinv <- function() xinv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function is basically a copy of the 2nd example.
## It tries to get the cached inverse with the first function; if it can't,
## it computes it with solve(), then caches it with the first function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        message("computing the inverse")
        xinv <- solve(data)
        x$setinv(xinv)
        xinv
}
