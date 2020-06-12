## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object that can 
## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse <- function() inv 
        list(set = set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- inverse(mat, ...)
        x$set_inverse(inv)
        inv
}
