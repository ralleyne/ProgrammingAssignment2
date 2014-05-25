## this file contains methods for calculating and caching inverse of a matrix

## this function creates a special matrix that can be cached. it returns a
## vector that can be used to set and get the matrix and also set and get the
## inverted matrix
##set(matrix) to get matrix
##get() to get matrix
##setinverse(inverse) to set matrix' inverse
##getinverse() to retrieve matrix
makeCacheMatrix <- function(x = matrix()) {
            i <- NULL
            set <- function(y) {
                x<<- y
                i<<-NULL
            }

            get <-function() x
            setinverse<-function(inverse) i<<-inverse
            getinverse<-function() i
            list(set = set, get = get, 
                    setinverse=setinverse,
                    getinverse=getinverse)
}


## this function solves the inversion of the matrix. it pulls from cache first.
##on cache miss, it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <-solve(data, ...)
        x$setinverse(i)
        i
}
