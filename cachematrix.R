## The first function enables the second function to get and set the matrix and its inverse. 
## The second function first tries to access the cached inverse matrix. 
## If it's cached then it is returned and otherwise it is first calculated and then returned.


## This function returns a list of functions that gets or sets the matrix as well as getting and setting its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function tries to get the inverse matrix of x. If it is cached it simply returns it. 
## If it is not cached the function first calculates the inverse, caches the solution and returns it.
cacheSolve <- function(x, ...) { 
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}