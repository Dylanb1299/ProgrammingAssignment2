## Finding the inverse of a matrix can be computationally expensive, below are two functions which
## use a special type of 'matrix' which can store its own inverse once it has been found in order 
## to avoid computing it again.

## This function makeCacheMatrix creates a special "matrix", which is really a list containing
## 4 separate functions to:
#       set the value of the matrix
#       get the value of the matrix
#       set the inverse matrix
#       get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        #xi stands for x inverse
        xi <- NULL
        set <- function(y) {
                x <<- y
                xi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xi <<- inverse
        getinverse <- function() xi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a 'cache matrix' created by the makeCacheMatrix function above and if the inverse
## is cached, returns it or if the inverse has not been found yet, computes it and sets it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinverse()
        if(!is.null(xi)) {
                message("getting cached data")
                return(xi)
        }
        data <- x$get()
        xi <- solve(data, ...)
        x$setinverse(xi)
        xi
}
