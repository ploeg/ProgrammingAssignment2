## Purpose: Functions to cache the inverse of a matrix
## Reason: Matrix inversion is usually a costly computation 
## Functions: makeCacheMatrix and cacheSolve
## Date: 20140921

## Function makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeCacheMatrix<- function(x = matrix()) {
        xi <- matrix()
        set <- function(m) {
                x <<- m
                xi <<- matrix()
        }
        get <- function() x
        setinverse <- function(solve) xi <<- solve
        getinverse <- function() xi
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}



## Function cacheSolve computes the inverse of the special "matrix" returned by function makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinverse()
        if (!isnull(xi)){
                message("getting cached matrix")
                return (xi)
        }
        m <- x$get()
        xi <- solve(m)
        x$setinverse(xi)
        xi
}
