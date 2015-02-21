## makeCacheMatrix function accepts a matrix as input argument.
## makeCacheMatrix function returns a list with 4 items. These items are actually 4 functions themselves:  set, get, setInverse, getInverse
## The set function uses the "super-assignment "<<-"  operator to invalidate the value of the inverse in the "parent" environment when a new value of the matrix is "set"

## inv = inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                x <<- y
                inv <<- NULL
                }
                get <- function() x
                setInverse <- function(solve) inv <<- solve
                getInverse <- function() inv
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
}

## The cacheSolve function accepts a matrix as an argument
## If the inverse of the matrix is found in the cache, then it is returned. If the inverse value in the cache has been invalidated (made null) by the set function above, then inverse is calculated afresh.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()
            if(!is.null(inv)) {
              message("getting the inverse of the matrix from cached data - thus saving computing resources")
              return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setInverse(inv)
            inv
}


