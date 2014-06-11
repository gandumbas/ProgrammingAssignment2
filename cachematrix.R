## This functions are used to caching the inverse of a matrix in order not
##    to compute it repeatedly. For this, the matrix is created in a special
##    form using makeCacheMatrix, and the inverse is calculated using
##    cacheSolve.
## This functions only work for invertible matrices, and stop with an error
##    if this is not the case.

## makeCacheMatrix creates a special "matrix", which is a list 
##    containing a function to
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve returns the inverse of the special "matrix" created
##   with makeCacheMatrix. If it has not been calculated, it uses
##   solve and sets the value in the cache; otherwise, it just gets
##   the value from the cache using the getsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                return(s)
        }
        data <-x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
