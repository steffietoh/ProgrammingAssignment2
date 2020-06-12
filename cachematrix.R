## makeCacheMatrix is a function that creates a list of 4 functions 
# (set,get,setsolve,getsolve) that will be used to input a
# matrix and store the inverse of that matrix in the parent environment.

## Two objects are created: x, the argument matrix passed into the function 
# and inv, the inverted matrix if it exists. 
# Both objects are in the same parent environment as the other functions.

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

## cacheSolve takes the list and objects created in makeCacheMatrix and either 
# 1) calculates the inverse of the matrix x and saves it in the object inv or
# 2) If the inverse already is saved as inv, then it will print the message
# "getting cached data" and return the stored value

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
