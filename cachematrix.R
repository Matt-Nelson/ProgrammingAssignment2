## cachematrix.R contains methods to allow a user to create a special
## matrix which is able to use lazy loading to return the inverse of the
## matrix when requested. In other words, the inverse of the matrix is 
## calculated the first time it is requested and then cached to be returned 
## from memory instead of being recalculated. All of this is done without 
## requiring the user to know whether or not the cache has been populated 
## with the inverse yet.

## makeCacheMatrix works as a wrapper for getter and setter methods for the
## matrix itself and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
	solution <- NULL
	set <- function(y) {
		x <<- y
		## empty the cache if a new matrix is set so that a new solution is calculated
		solution <<- NULL
	}
	get <- function() {
		x
	}
	setsolution <- function(calculatedSolution) {
		solution <<- calculatedSolution
	}
	getsolution <- function() {
		solution
	}
	list(set = set, get = get,
		 setsolution = setsolution,
		 getsolution = getsolution)
}


## cacheSolve returns the inverse for a matrix x. It accomplishes this
## efficiently by checking whether a cached solution is available for the
## matrix first and returning the cached solution if it is available. If it
## is not available in the cache, it calculates the solution and places it
## in the cache for future use.

cacheSolve <- function(x, ...) {
	
    ## Return the cached solution if it exists
	solution <- x$getsolution()
	if(!is.null(solution)) {
		message("getting cached solution")
		return(solution)
	}
	
	## Otherwise, calculate the solution, set the cache, and return the solution
	data <- x$get()
	solution <- solve(data, ...)
	x$setsolution(solution)
	solution
}
