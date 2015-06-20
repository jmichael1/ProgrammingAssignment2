## The cachematrix library consists of two functions, makeCacheMatrix and 
## cacheSolve. makeCacheMatrix creates a "matrix" object that can cache
## its inverse.  cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has been previously determined
## and the matrix is unchanged, then cacheSolve retrieves the cached inverse.

## makeCacheMatrix: Given an matrix argument x, create a "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
	}
}


## cacheSolve: compute the inverse of the special "matrix" returned by 
## makeCacheMatrix using solve. If the inverse has been previously calculated
## for this matrix then return the cached inverse.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	## If we have a cached version return it
	if (!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	## Otherwise calculate the 
	## inverse and return it
											data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}


