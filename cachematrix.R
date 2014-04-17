## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {    
    inv <- NULL                                                       
    set <- function(y) {                                                # set function defintion
        x <<- y                                                         # variable x to y
        inv <- NULL                                                     # if change, flush cache
    }
    get <- function() x                                                 # get function definiton
    getSolve <- function() inv                                          # receive the inversed cached value 
    setSolve <- function(computed_inv) inv <<- computed_inv             # transfer values into the cache  
    list(get=get, set=set, getSolve=getSolve, setSolve=setSolve)        # list is returned
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getSolve() 
    if(!is.null(inv)) {                                                 # entry available in cache
        message("getting cached data")
        return(inv)
    }
    new_inv <- solve(x$get(), ...)                                      # inverse calculation   
    x$setSolve(new_inv)                                                 # cache load with inverse      
    return(new_inv)
}
