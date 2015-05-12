## function to calculate inverse of a matrix
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 inv_cache_matrix <- NULL
    set <- function(x) {
        cache_matrix <<- x;
        inv_cache_matrix <<- NULL;
    }
    get <- function() return(cache_matrix);
    setinv <- function(_inv) inv_cache_matrix <<- _inv;
    getinv <- function() return(inv_cache_matrix);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## get inverse of matrix
	inv_cache_matrix <- cache_matrix$getinv()
	
	##if inverse of cache matrix is not null then return cached version otherwise return form solve function
    if(!is.null(inv_cache_matrix)) {
        message("Getting cached data...")
        return(inv_cache_matrix)
    }
    data <- cache_matrix$get()
	
	##solve step
    inv_cache_matrix <- solve(data, ...)
	
	##set inverse
    cache_matrix$setinv(inv_cache_matrix)
    return(inv_cache_matrix)
}
