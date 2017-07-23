## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function sets value of matrix, gets value of matrix, sets value of inverse of matrix and then gets teh value of the inverse of teh amtrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y) {
        	x <<- y
        	inv <<- NULL
    	}
    	get <- function() x
    	setinverse <- function(inverse) inv <<- inverse
    	getinverse <- function() inv
    		list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function returns inverse of matrix. It checks if it has already been computed; if it has then it skips computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    	if(!is.null(inv)) {
       	 message("getting cached data.")
        	return(inv)
    	}
    	data <- x$get()
    	inv <- solve(data)
    	x$setinverse(inv)
    	inv
}
