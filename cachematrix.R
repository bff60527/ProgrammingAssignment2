## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
	set <- function(y){
		x <<- y
		invMatrix <<- NULL
	}
	
	get <- function()x
	setInverse <- function(inverse) invMatrix <<- inverse
	getInverse <- function() invMatrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        
	if(!is.null(invMatrix)){
		message("getting cached data")
		return(invMatrix)
	}
	
	data <- x$get()
	invMatrix <- solve(data, ...)
	x$setInverse(invMatrix)
	invMatrix

}
