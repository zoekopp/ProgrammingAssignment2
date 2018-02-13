## These are two functions that cache the inverse of a matrix to
## decrease future computing time

## makeCacheMatrix takes any invertible matrix as an arg, and
## defines the functions to set and get the matrix and its inverse
## from memory.

makeCacheMatrix <- function(x = matrix()) {
	z <- NULL  		##empty variable to store inverse
	set <- function(y) {
		x <<- y
		z <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) z <<- inv
	getinverse <- function() z
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix by getting the cached
## data from makeCacheMatrix and then calling solve()

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	z <- x$getinverse()
	if (!is.null(z)) {			## if z has been computed already, return it
		message("getting cached data")
		return(z)
	}
	data <- x$get()				## get the oririnal cached matrix
	z <- solve(data, ...)		## compute inverse
	x$setinverse(z)				## cache the inverse
	z   
}
