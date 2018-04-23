## The purpose of these functions calculate and store the inverse of a certain
## matrix, so as to avoid the need of calculating it multiple times.

## This function set the fuctions that will permit the storing and recovering 
## (setting and getting) of the values and the cache.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function checks if the inverse of the matrix has been calculated, 
## retrieve and print that value, or calculate it, if necessary.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setsolve(im)
        im
}
