## The 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_DO <- NULL
        set <- function(y){
                x <<- y
                inv_DO <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv_DO <<- solveMatrix
        getInverse <- function() inv_DO
        # The below section of code assigns functions set(), get(), setInverse(), and getInverse() to the list so that it can 
        # be returned to the parent environment.
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The 'cacheSolve' function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 'cachesolve' function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_DO <- x$getInverse()
        if(!is.null(inv_DO)){
                message("getting cached data")
                return(inv_DO)
        }
        data <- x$get()
        inv_DO <- solve(data)
        x$setInverse(inv_DO)
        inv_DO      
}

