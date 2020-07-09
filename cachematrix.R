## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMatrix creates a special "matrix", which is really a list containing
## 4 separate function to:
#       set the value of the matrix
#       get the value of the matrix
#       set the inverse matrix
#       get the inverse matrix

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
