## These functions take in a square matrix and returns the inverse of that matrix
## The result is cached to avoid computing the inverse if it has already been calculated

## makeCacheMatrix returns a list of functions to store the inverse of a square matrix

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


## cacheSolve takes the list from makeCacheMatrix() and returns the inverse of the matrix by
## firstly checking whether the inverse matrix exists and otherwise calculating and setting it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
