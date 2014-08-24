## The following functions can be used to create a matrix (with 
## makeCacheMatrix), and then calculate its inverse (with cacheSolve)
## and the inverse will be cached.

## This function is a list containing functions that will:
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the invers

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
        		x <<- y							## assign matrix to x
        		m <<- NULL						## create variable to hold inverse of x
        }
        get <- function() x						## return matrix
        setinv <- function(solve) m <<- solve	## assign inverse to m
        getinv <- function() m					## return value of m
        list(set = set, get = get,				
             setinv = setinv,
             getinv = getinv)
}


## The following function checks to see if the inverse of the 
## matrix parameter has already been calculated. If so, it returns
## the cached inverse. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the 
## setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        ## if the inverse is cached
        if(!is.null(m)) {  					
                message("getting cached data")
                ## exit the function & display inverse
                return(m)					
        }
        ## if the inverse is not cached then get the matrix,
        ## calculate its inverse, and cache its value
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
