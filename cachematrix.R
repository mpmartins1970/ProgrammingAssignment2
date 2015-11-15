## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## These pair of functions cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL
        
        #Assign the value of the matrix.
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        #Retrieve the value of the matrix.
        get <- function() { 
                x
        }
        
        #Assign the inverse value of the matrix. 
        setSolve <- function(solve) {
                s <<- solve
        }
        
        #Retrieve the inverse value of the matrix.      
        getSolve <- function() {
                s
        }
        
        #Return a list that wraps the matrix and provide functions to set/get 
        # value and the inverse matrix.
        list(set = set, 
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        
        #Retrieve the inverse value of the matrix.      
        s <- x$getSolve()
        
        #Verify if the inverse matrix is cached.      
        if(is.null(s)) {
                # if not, calculate inverse matrix and cache it
                data <- x$get()
                s <- solve(data, ...)
                x$setSolve(s)
                message("no cached data")
        }else {
                message("getting cached data")
        }
        
        # Return a matrix that is the inverse of 'x'
        s
        
}
