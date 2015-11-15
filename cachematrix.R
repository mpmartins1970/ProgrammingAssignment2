## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## These pair of functions cache the inverse of a matrix.

# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() { 
                x
        }
        
        setSolve <- function(solve) {
                s <<- solve
        }
        
        getSolve <- function() {
                s
        }
        
        list(set = set, 
             get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        
        s <- x$getSolve()
        
        if(is.null(s)) {
                data <- x$get()
                s <- solve(data, ...)
                x$setSolve(s)
                message("no cached data")
        }else {
                message("getting cached data")
        }
        
        ## Return a matrix that is the inverse of 'x'
        s
        
}
