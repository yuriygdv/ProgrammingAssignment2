## The functions can take a matrix as an argument, and calculate and store its inverse for reuse


## makeCacheMatrix takes a matrix as an argument and creates a list that stores it and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) i <<- inv
    
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The function works with objects created by makeCacheMatrix.
## It reads and returns the cached inverse matrix in the object passed as an argument. 
## If the inversed is not available, it calculates and caches it 



cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}


######################################################
### Functions were Tested with the Following Code: ###

# m1 <- matrix(1:4, nrow = 2, ncol =2)
# m2 <- matrix(3:6, nrow = 2, ncol =2)

# m <- makeCacheMatrix(m1)
# cacheSolve(m)
# cacheSolve(m)

# m <- makeCacheMatrix(m2)
# cacheSolve(m)
# cacheSolve(m)










