##The two functions defined below are used to calculate matrix inverse and cache the calculated value.
##If the matrix changed, the inverse value will be recalculated. Otherwise, it will be retrieved from cache.

##Base function to get & set matrix and its inverse. 
##The <<- operator is used to assign a variable in different environment than current one. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(matrixinv) inv <<- matrixinv
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##Function to get the cached inverse. If the inverse is not calculated or matrix changed, inverse to be calculated.
cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("Get the inverse from cache")
                return(inv)
        }
        
        inv <- solve(x$get())
        x$setinv(inv)
        inv
}