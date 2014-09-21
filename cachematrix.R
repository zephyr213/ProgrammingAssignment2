## These two functions are used to set a matrix, and calcute and cache 
## its inverse

## Function use to create a matrix, inquire its inverse

makeCacheMatrix <- function(x = matrix()) {
    invmx <- NULL  ## inverse matrix
    set <- function(y) {
        x <<- y
        invmx <<- NULL
    }
    get <- function() x
    setinvmx <- function(tt) invmx <<- tt
    getinvmx <- function() invmx
    list(set = set, get = get,
         setinvmx = setinvmx,
         getinvmx = getinvmx)
    

}


## Calculate the inverse of a matrix if it's not done already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    ## first check if the inverse matrix is already there
    invmx <- x$getinvmx()
    if(!is.null(invmx)) {
        message("getting cached data")
        return(invmx)
    }
    data <- x$get()
    invmx <- solve(data, ...)
    x$setinvmx(invmx)
    invmx
}
