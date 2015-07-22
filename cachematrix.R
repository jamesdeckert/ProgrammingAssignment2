## Create a Matrix object which has accessor functions

makeCacheMatrix <- function(matX = matrix()) {
    inv <- NULL
    set <- function(y) {
        matX <<- y
        inv <<- NULL
    }
    
    get <- function() matX
    
    setInv <- function(InverseMatrix) inv <<- InverseMatrix
    
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Retrieves the inverse of a matrix. If the inverse was cached, then return the cached matrix instead of calculating again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatX <- x$getInv()
    if(!is.null(invMatX)) {
        message("getting cached data")
        return(invMatX)
    }
    data <- x$get()
    invMatX <-  solve(data) 
    x$setInv(invMatX)
    invMatX
}
