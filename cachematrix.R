# This file contains functions allowing a cached approach of getting the
# inverse of a matrix. The functions comprise a specialized cache matrix
# constructor function and a cache-enabled version of the solve function
# ('cacheSolve') that computes the inverse of a cache matrix

# This function creates so called 'cache matrices' that allow caching of the
# matrices' inverse matrices. It optionally gets an R matrix as input and
# returns a list representing the corresponding cache-enabled matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # getter function returning the actual matrix
    get <- function() x
    
    # setter function setting the matrix to the provided matrix values
    set <- function(mtrx) {
        x <<- mtrx
        
        # reset inverse (which may not be appropriate for the newly set matrix)
        inv <<- NULL
    }
    
    # getter function returning the inverse of the set matrix (or NULL if not
    # set, yet)
    get_inv <- function() inv
    
    # setter function setting the inverse
    set_inv <- function(inv_mtrx) {
        inv <<- inv_mtrx
    }
    
    list(get=get, set=set, get_inv=get_inv, set_inv=set_inv)
}


# This function computes the inverse of an input cache matrix x. If x's inverse
# was already computed, this cached value is returned. If x has no set inverse,
# yet, the inverse is computed and stored in x.
cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    
    if(!is.null(inv)) {
        message('inverted matrix already exists; nothing to do')
        # ...inverted matrix already computed --> return inv
        return(inv)

    } else {
        message('inverted matrix not computed, yet; going to compute it')
        # ...inverted matrix not computed, yet --> compute, set and return inv
        inv <- solve(x$get())
        x$set_inv(inv)
        return(inv)
    }
}
