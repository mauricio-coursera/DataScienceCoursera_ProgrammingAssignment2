## The functions makeCacheMatrix and cacheSolve can be used
# to cache the inverse of a matrix, so it does not need to be
# calculated every time.


## makeCacheMatrix
# This function takes a square matrix and returns a list of
# functions to set and get a variable defined in a "protected"
# scope, holding a copy of the original matrix, and also functions 
# to set and get the cached value of the inverse of this matrix.
#
# For example, assuming you have a matrix "m" defined as 
# matrix(c(1,2,3,0,1,4,5,6,0),3,3), you can create a version 
# able to cache its own inverse:
# mCache <- makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
    
    # Lets define our cache variable
    cachedInv <- NULL
    
    # And our functions to manipulate this cache
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedInv <<- inverse
    getinverse <- function() cachedInv
    
    # Now lets create another variable to hold these functions.
    # Calling "set" here makes sure we will be able to use 
    # the original matrix even if it is deleted before we
    # call "cacheSolve".
    cacheMatrix <- list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)    
    cacheMatrix$set(x)
    return(cacheMatrix)
}


## cacheSolve
# this function takes the list created by the function makeCacheMatrix
# as argument, and returns the inverse of the matrix stored in that
# list.
# However, it first checks if the inverse is not cached.
# If it is, the cached value is returned. If it is not, the inverse 
# is calculated, stored in the cache, and returned.
# A message is shown if the value was taken from the cache.

cacheSolve <- function(x, ...) {    
    
    # validate the cached value
    if(is.null(x$getinverse())) {
        
        # If there isn't a cached value, calculate the inverse
        # and store it in the cache
        cachedInv <- solve(x$get())
        x$setinverse(cachedInv)
		
    } else {
        
        # If there is something in the cache, let the user
        # know we are retrieving a cached value
        message("getting cached data")
    }
    
    # returns the cached value
    return(x$getinverse())
}