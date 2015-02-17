# Functions for caching the calculation of the inverse of a matrix.

makeCacheMatrix <- function(m=matrix()){

    # Creates a "cached" matrix, this matrix stores its inverse to avoid recalculation.
    # The returned object is a list with 4 function elements:
    # get, set: get and set the internal matrix. Setting a new matrix resets the cache.
    # get_inverse, set_inverse: get and set the calculated inverse. These functions have
    # no effect in the cache
    

    # Inverse starts as NULL because it was not calculated
    i <- NULL

    # get and set functions for the internal matrix
    set <- function(new_matrix) {
        m <<- new_matrix
        i <<- NULL
    }

    get <- function() { m }

    # get and set for the inverse
    set_inverse <- function(new_inverse) { i <<- new_inverse }
    get_inverse <- function() { i }

    # return value
    list(
        set=set, get=get,
        set_inverse=set_inverse, get_inverse=get_inverse
    )    
}


cacheSolve <- function(m) {
    i <- m$get_inverse()

    # Was the inverse calculated?
    if (!is.null(i)) {
        return(i)
    }

    # Inverse not cached. Calculate and store it.
    calculated_inverse <- solve(m$get())
    m$set_inverse(calculated_inverse)

    # Return the calculated cache.
    calculated_inverse
}
