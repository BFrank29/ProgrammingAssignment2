## These function create a special matrix that has a
## cached inverse. It can then be checked to see if
## the inverse has already been cached.

## Creates matrix and cached inverse

makeCacheMatrix <- function(x = matrix()) {
    invcache <- NULL
    set <- function(y) {
        x <<- y
        invcashe <<- NULL
    }
    get <- function() x
    setcache <- function(inverse1) invcache <<- inverse1
    getcache <- function() invcache
    list(set = set, get = get, 
         setcache = setcache, 
         getcache = getcache)
}


## This function looks for a cached inverse.
## If the inverse is not cached, it is created and cached

cacheSolve <- function(x, ...) {
        inverse <- x$getcache()
        if(!is.null(inverse)) {
            return(inverse)
        }
        cachematrix <- x$get()
        inverse <- solve(cachematrix)
        x$setcache(inverse)
        inverse
}
