## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solution) inv <<- solution
        get_inv <- function() inv
        
        list(set = set,
             get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## cacheSolve computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated, it will be 
# retrieved from the "cache" and a relevant message will be displayed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}

# m <- matrix(c(3,3.2,3.5,3.6), nrow = 2)

