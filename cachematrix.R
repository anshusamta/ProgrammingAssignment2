## Function to return inverse of a matrix and also considering if its already in cache

##  creates a special matrix and cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' but first check if its in cache 

cacheSolve <- function(x, ...) {
        
         inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## Calculate inverse of matrix
        inv <- inverse(data, ...)
        x$setinverse(inv)
        inv
}

## calculating inverse of the matrix 'x' and checking required condition to exist inverse
inverse <- function(x) {
		if(nrow(x)==ncol(x)) {
  				if(det(x)!=0)
					solve(x)
				else
					message("inverse doesn't exist")
		}
		else {
				message("inverse doesn't exist")
		}
}