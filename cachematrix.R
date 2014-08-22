## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x ) {
	  ##set m to null
	  m <- matrix()

	  ##sets the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }

	  ##get the matrix
        get <- x  

	  ##sets the inverse an of the matrix
        setinverse <- function(solve) m <<- solve  

	  ##gets the inverse of the matrix
        getinverse <- solve(x)

	  ##creates a list containig all the values 
	  ##This is the result printed when tihs function is called
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	  ##retrieves the inverse matriz stored in cache
        m <- x$getinverse

	  ##if a value was retrieved let me know with a message and print it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	  
	  ##retrievers the matrix that was cached
        data <- x$get

	  ##calculate its inverse
        m <- solve(data)

	  ##cache the inverse
        x$setinverse(m)

	  ##print the inverse
        m
}
