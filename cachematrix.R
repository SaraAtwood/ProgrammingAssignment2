## These functions work in conjunction to solve for the inverse of an invertible matrix,
## and save the result in the parent directory so that the result can be quickly retrieved 
## instead of calculating it each time (the result is "cached").

## makeCacheMatrix must be run first, or nested within the call of cacheSolve
## Example: cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4), 2,2)))

## These functions were modified from makeVector and cacheMean forked from: 
## https://github.com/rdpeng/ProgrammingAssignment2



## makeCacheMatrix(x) takes as input an invertible matrix x, and returns a list of four functions
## that can operate on the matrix x: set(x) - manually sets the matrix, get() - returns the matrix, 
## setinverse(solve) - sets the inverse of the matrix to value "solve" calculuated outside this function,
## and getinverse() - returns the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve(x,...) takes as input the result of makeCacheMatrix, which is a list of 4 functions, 
## and returns a matrix that is the inverse of the matrix x passed into makeCacheMatrix. The function
## checks if the inverse of x has already been calculuated and if so, returns the cached value, otherwise
## calculates the inverse using solve() and stores it in the parent directory.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {    
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
