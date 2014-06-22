## This function creates a special "matrix" object that can cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    ##get the value of the matrix
    get <- function() x  

    ##set the value of the inverse matrix
    setInverse <- function(solve) {
        inverse <-- solve
    }
    
    #get the value of the inverse matrix
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
##assume that the matrix supplied is always invertible

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse()
        ##getting cached version when one exists
        if(!is.null(inverse)) {
            message("getting the cached inverse matrix")
            return(inverse)
        }
        
        ##computing the non-cached part
        currData <- x$get()
        inverse <- solve(currData, ...)
        x$setInverse(inverse)
        inverse
}
