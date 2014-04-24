## makeCacheMatrix is a function that creates an object that stores a 
## matrix and corresponding inverted matrix and four functions to manage 
## them.

## set(y) stores the matrix, and sets the inverse to NULL (since the
##          old value is invalid and needs to be recalculated)
## get() retrieves the matrix
## setmatrix(xinv) stores the inverted matrix
## getmatrix() retrieves the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<-NULL
    }
    get <- function() x
    setmatrix <- function(xinv) m <<- xinv
    getmatrix <- function() m
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## cacheSolve checks if there is a cached value and returns the value if 
##      available, otherwise recaculates the inverse and caches the new value


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if (!is.null(m)) {
        message("getting cached matrix...")
        return(m)
    }
    xm <- x$get()           ## xm gets the non-inverted matrix
    m <- solve(xm, ...)     ## m is the inverted matrix solution to be cached
    x$setmatrix(m)
}

## usage example:
# > obj <- makeCacheMatrix()
# > obj$set(matrix(1:4,nrow=2,ncol=2))
# > obj$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(obj)
# > obj$getmatrix()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(obj)
# getting cached matrix...
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 
