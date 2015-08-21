## The functions defined below are used to calculate matrix inverse and store the calculated value
##If the matrix in changed the inverse value will be recalculated, otherwise it will be etrived from cache

## Base function to get & set values for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(matrixinv){
                inv <<- matrixinv  
        }
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to get the cached inverse otherwise calculate it
cacheSolve <- function(x, ...) {
        invtest <- x$getinv()
        if(is.null(invtest)) {
                inv <- solve(x$get())  
        }
        else{
                print("Get the inverse from cache")
                inv <- invtest
        }
        inv
}