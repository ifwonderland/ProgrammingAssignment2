## This function overall provides the capability to make matrix with caching capabilities for its inverse and then the actual function to get cached inversion

## Make a cached matrix, that caches its own inversion, assuming the matrix passed in are inversible

makeCacheMatrix <- function(x = matrix()) {
    ##initialization
    i <- NULL
    ##set the value of x which is the matrix data to be used to calculate inversion
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    ##get value of matrix x
    get <- function() x
    ##set cached inversed matrix 
    setInversed <- function(inversed) i<<- inversed
    ##get cached inversed matrix
    getInversed <- function() i
    ##return list of all functions 
    list(set = set, get = get, setInversed = setInversed, getInversed = getInversed)
}


## Solve inversion of matrix leveraging cached inversed matrix, if cached, returned cached inversed matrix, if not, calculate the inversed matrix then cached it and return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ##First check if cached inversion is available, if so, return it
    i <- x$getInversed()
    if(!is.null(i)){
        message("getting cached inverse")
        return(i)
    }
    ##otherwise solve the matrix inversion
    data <- x$get()
    i <- solve(data, ...)
    x$setInversed(i)
    i
}
