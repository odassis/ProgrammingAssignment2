## Coursera R Programming Course - Programming Assignment 2
## Script that caches matrix inverse operations to avoid
## expensive computational recalculations.

## Stores matrix and inverse matrix data using R global environment
## 
## Argument : Input from class "matrix", default value: empty matrix.
## Output: List containing handles to functions that store or retrieve
## cahed matrix and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	## Initializes inverse:
	inv <- NULL

	## Assigns new argument to stored value and resets inverse
	set <- function(y){
		x <<- y
		inv <<- NULL
	}

	## Returns internal object
	get <- function() x

	## Getter and Setter for the inverse, same as above
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv

	## Returns internal functions as a list:
	list(	set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Solves inverse of given matrix, caching the result or
## using cached result when available.
##
## Argument : matrix created by "makeCacheMatrix()"
## Output: Inverse of the matrix

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	## Checks for cached result
	if (!is.null(inv)){
		## Returns cached result
		message("Getting cached data")
		return(inv)
	}
	## Retrieves cached input matrix:
	mtx <- x$get()
	## Calculates inverse (assuming squared, inversible matrix)
	inv < solve(mtx)
	## Caches inverse
	x$setInverse(inv)
	inv
}
