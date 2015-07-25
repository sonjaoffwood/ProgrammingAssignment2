## Cache the inverse of a matrix to save computation time

# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
	# set the matrix
	# get the matrix
	# set the value of the inverse of the matrix
	# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: Computes the inverse of the matrix returned by makeCacheMatrix. 
	# If the inverse has already been calculated (and the matrix has not changed), then retrieve the inverse from the cache,
	# otherwise calculate inverse from scratch.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {		
                message("getting cached data")	
                return(inv)
        }
        data <- x$get()				
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}