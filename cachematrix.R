## 'makeCacheMatrix' will build special matrix which has four functions
## 'cacheSolve' will use above structure to return its inverse,
## it will search cache first before doing actual calculation

## This will create special matrix handler which has below
##  - function call itself with matrix, matrix constructor
##  - set : this will replace original matrix from above
##  - get : return matrix itself
##  - setinverse : will store inverse in upper environment,
##                 will be used by cacheSolve
##  - getinverse : return stored inverse,
##                 will be used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This will get matrix inverse by calling solve
## However, before doing actual function call,
## check it is cached first and return it if it is,
## otherwise, call 'solve' and save it into 's' in upper environment
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if (!is.null(s)) {
                message("Getting cached data")
                return(s)
        } else {
                message("Calculate solve now")
        }
        data <-x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
