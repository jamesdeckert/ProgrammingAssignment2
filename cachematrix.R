## Create a Matrix object which has accessor functions
####

makeCacheMatrix <- function(matX = matrix()) {
    inv <- NULL
    set <- function(y) {
        matX <<- y
        inv <<- NULL
    }
    
    ## Returns original matrix
    get <- function() matX
    
    ## Sets inverse matrix
    setInv <- function(InverseMatrix) inv <<- InverseMatrix
    
    ## Gets inverse matrix
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Retrieves the inverse of a matrix. If the inverse was cached, then return the cached matrix instead of calculating again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatX <- x$getInv()
    ## checks for an inverse matrix in cache
    if(!is.null(invMatX)) {
        message("getting cached data")
        return(invMatX)
    }
    ## no cached inverse so get original and calculate inverse to put into cache
    data <- x$get()
    invMatX <-  solve(data) 
    x$setInv(invMatX)
    invMatX
}
