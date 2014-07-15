## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## The following function makeCacheMatrix creates a special "Matrix", 
## which is really a list containing a function:
## 		set the value of the Matrix
## 		get the value of the Matrix
## 		set the value of the Inverted Matrix
## 		get the value of the Inverted Matrix

makeCacheMatrix <- function(x = matrix()) {

		## the <<- operator is used to assign a value to an object 
		## in an environment that is different from the current environment.
		
		ix <- NULL
		
		set <- function(y) {
			x <<- y
			ix <<- NULL
		}
		
		get <- function() { x }
		
		setInverse <- function(inverted) { ix <<- inverted }
		
		getInverse <- function() { ix }
		
		list(set=set, get=get, 
			 setInverse=setInverse, getInverse=getInverse)

}


## The following function calculates the inverted matrix of 
## the special "Matrix" created with the function "makeCacheMatrix". 
##
## It first checks to see if the inverse has already been calculated
## and the matrix has not changed. 
## If so, it gets the inverted-matrix from the cache and skips the computation. 
##
## Otherwise, it calculates the inverse of the Matrix and 
## sets the value of the inverted-matrix in the cache 
## via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ix <- x$getInverse()
        if (is.null(ix)) {
			## not in cache
			
			## retrieve matrix object
			theM <- x$get()
			
			## compute its inverse
			## we assume that the matrix is always invertible
			ix <- solve(theM)
			
			## put in cache
			x$setInverse(ix)
        } else {
			message("getting cached data")
        }
        
        # return inverted matrix
        ix
}
