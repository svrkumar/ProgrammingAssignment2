## Create a matrix object and calculating it's inverse.
## If the matrix inverse is already calculated, then it will search in the cache instead of calculating it again.

## makeCacheMatrix  creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


## cacheSolve checks cache for inverse, if not found in cache it will calcualte inverse

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- zapsmall(ginv(data, ...))
	x$setinverse(i)
	i
}
