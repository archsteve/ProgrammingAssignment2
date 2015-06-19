## The set of functions are fed matrices, which are then inversed and cached.
## The cache stores the result, so it can be used later without having
## to be constantly recalculated. 

##The initial lines of code indicate that the inverse must first 
##be calculated in the cacheSolve function (below)
##Essentially, the cache is cleared (NULL called to clear m). 
##The results are included in the list.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the data is already
##cached. If not, the inverse must be calculated in solve(data, ...)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}



