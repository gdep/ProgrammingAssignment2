
## The function makeCacheMatrix creates a special matrix that allows for caching of its inverse.
## The special matrix is actually a list of 4 functions used set/get the actual matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Creates a special matrix that allows for caching of its inverse.
   
    ## Initializing the inverse as NULL. 
    inverseX <- NULL
    ## Function to set the matrix and the inverse in the parent environment.
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    ## Function to get the matrix.
    get <- function() x
    ## Function to set the matrix inverse in the parent environment.
    setinverse <- function(inverse) inverseX <<- inverse
    ## Function to get the matrix inverse.
    getinverse <- function() inverseX
    ## Returns the special matrix (a list with the 4 get/set functions)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## The cacheSolve function receives a special matrix that can cache its inverse, verifies if said inverse
## is already cached, if so it returns the cached inverse and informs the user a cached version was used,
## otherwise it calculates the inverse, stores it in the special matrix and returns the result.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   
    ## Veryfing if the inverse has already been calculated ...
    inverseX <- x$getinverse()
    ## if it has, returns it.
    if(!is.null(inverseX)) {
        message("getting cached data")
        return(inverseX)
    }
    ## Otherwise, calculates the inverse.
    data <- x$get()
    inverseX <- solve(data, ...)
    ## Caching the inverse on the special matrix.
    x$setinverse(inverseX)
    ## Returns the inverse.
    inverseX
    
}
