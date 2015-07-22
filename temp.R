makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cachemean <- function(z, ...) {
    q <- z$getmean()
    if(!is.null(q)) {
        message("getting cached data")
        return(q)
    }
    data <- z$get()
    q <- mean(data, ...)
    z$setmean(q)
    q
}