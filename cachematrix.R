## The intention of these functions is to avoid an unnecessary calculation of 
## the inverse of a matrix if it has been already inversed. Therefore being a
## cost-effective solution.

## This function creates our special matrix, which will be a list containing
## the following functions:
## set the values of the matrix
## get the values of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## inv will be our inversed matrix, also a flag if the 
        ## calculations were not performed yet
        inv <- NULL
        ## this function will set the values for our matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## this function will return the matrix
        get <- function() {
                x
        }
        ## this will set the cache with the inverted matrix
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        ## this function will return the inversed matrix from memory
        getinverse <- function() {
                inv
        }
        list(set = set, get = get , 
             setinverse = setinverse,
             getinverse = getinverse)
          
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}
