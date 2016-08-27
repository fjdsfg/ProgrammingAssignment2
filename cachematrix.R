#########################################################
# Coursera - R Programming Course
# Week 3 - Peer Review Assignment
#
# Objective: 
#   Cache the result of a matrix inversion into
#   an appropriate object, since a matrix inversion
#   is a costly operation.
#
#   The presented source code will be subjected
#   to a peer-review.
#########################################################


#########################################################
# makeCacheMatrix(x):
#  - Arguments:
#    * x: An invertible matrix. Otherwise, an empty 
#       matrix will be assumed.
#
#  - Usage:
#       returns an object that contains an invertible
#       matrix, x, and the the respective cached
#       result from the inversion operation.
#
#########################################################
makeCacheMatrix <- function(x = matrix()) {
    #Sets the initial cached value
    cache <- NULL
    
    #Definition of the set function
    #i.e. when a new matrix value is stored in
    #the returned object
    set <- function(y) {
        #substitutes the value
        x <<- y
        #cleans the cached result
        cache <<- NULL
    }
    #Function that retrieves the invertible matrix
    get <- function() x
    
    #Function that sets the matrix inversion result
    #in cache
    setinverse <- function(inverted) cache <<- inverted
    
    #Method that retrieves the cache
    getinverse <- function() cache
    
    #Returns the object that contains the 
    #appropriate function calls in a list.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#########################################################
# cacheSolve(x, ...) :
#   - Arguments
#       * x: an object containing an invertible matrix
#         and the respective cache from the matrix 
#         inversion (see makeCacheMatrix).
#       * ...: additional parameters provided to the
#         matrix inversion function (see solve).
#   
#   - Usage:
#       Given an object, x, from  makeCacheMatrix(x),
#       computes the matrix inversion when x does not
#       have the result in cache.
#       Otherwise, it returns the cached result.
#########################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverted <- x$getinverse()
    
    #If the value is not null, then we can retrieved
    #the cached result
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    
    #otherwise, it retrieves the invertible matrix
    data <- x$get()
    #computes the matrix inversion
    inverted <- solve(data, ...)
    #sets the value in the cache
    x$setinverse(inverted)
    #and returns the matrix inversion result
    inverted
}
