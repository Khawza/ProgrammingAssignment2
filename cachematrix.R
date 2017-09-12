## makeCacheMatrix create a special matrix that contains elements
## of the matrix and four functions namely setters and getters. 

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y){
                x <<- y
                inv_x <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(invm_x) inv_x <<- invm_x
        getInverse <- function() inv_x
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
        
}



## cacheSolve inverse a special matrix when the inverse of the
## matrix is not availabe in the cache. Otherwise, it just gets
## the value from the cache

cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                inv_x <- x$getInverse()
                if(!is.null(inv_x)){
                        message("getting cached data")
                        return(inv_x)
                }
                data <- x$get()
                inv_x <- solve(data)
                x$setInverse(inv_x) 
                inv_x
}
