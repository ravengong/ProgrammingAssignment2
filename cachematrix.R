## Our goal is to write a pair of functions, mainly,
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve is a function that computes inverse of the special "matrix"
## that is returned by makeCacheMatrix above. If the inverse has already
## been calculated (and matrix hasn't changed), then cacheSolve retrieves
## the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cache result")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
