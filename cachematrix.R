## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Takes a matrix x and returns a list 
# containing functions to set the value of the matrix, get the value 
# of the matrix, set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        solve_matrix <- function(solve(x)) m <<- solve(x)
        get_matrix <- function() m
        list(set = set, get = get, solve_matrix = solve_matrix, get_matrix = get_matrix)
}


## Write a short comment describing this function
# The following function calculates the inverse of the matrix 'x'.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(x, ...)
        x$solve_matrix(m)
        m
}
