## These functions will calculate the inverse of a matrix and cache it so
## that it does not have to be computed each time, and call the inverse matrix
## by pulling from the cache, or computing the inverse if it is a new matrix

## This function contains a list of four functions to: set the matrix, get the matrix, set the
## inverse, and get the inverse. This is used as the input to the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setmatrix <- function(y) {
			x <<- y			## cache the inputted matrix so cacheSolve can check for changes
			inv <<- NULL 
	}
	getmatrix <- function() x
	setinverse <- function(inverse) inv <<- inverse		##cache the matrix inverse
	getinverse <- function() inv
	## creates a list of the four functions described above
	list(setmatrix = setmatrix, getmatrix = getmatrix,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x' by either pulling from
## the cache or computing the inverse of a new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from the cache
        inverse = x$getinverse()
        if(!is.null(inverse)) {
        		message("getting cached data")
        		return(inverse)
        }
        ## If matrix inverse has not been solved, solve for inverse and return
        mat.data <- x$getmatrix()
        inverse <- solve(mat.data, ...)
        x$setinverse(inverse)
        return(inverse)
}