## Put comments here that give an overall description of what your
## functions do
## These 2 functions store a matrix and provide a list object
## that enables the matrix to be retrieved, and an inverse 
## of the matrix to be stored and retrieved - all in another 
## environment to enable 'caching'.

## Write a short comment describing this function
## Function creates a object that consists of a list of
## functions to store and retreive a matrix,and cache 
## and retreive it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
	  ## Create the functions
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
	  ## Return a list of the functions
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)


}


## Write a short comment describing this function
## Function attempts to get the 'matrix inverse' list object
## from the cache. If not found, it creates a new inverse and
## stores it using the setmatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## Try and get list from cache
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  ## If list not in cache, create/cache it
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
