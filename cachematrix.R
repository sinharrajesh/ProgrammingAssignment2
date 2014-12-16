##  cachematrix.R
##  

##  The two functions - makeCacheMatrix and cacheSolve work in tandem to allow cached copy of inverse to be used 
##     instead of costly recomputations
##
##  Sample Usage 
##  > source("cachematrix.R")
##  > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##  > amatrix$get()         # Returns original matrix
##  > cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
##  > amatrix$getinverse()  # Returns matrix inverse
##  > cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse
##  > amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
##  > cacheSolve(amatrix)   # Computes, caches, and returns new matrix inverse
##  > amatrix$get()         # Returns matrix
##  > amatrix$getinverse()  # Returns matrix inverse
##  


## This function creates a special "matrix" object that can cache its inverse.
## Args:
##       x: Matrix whose inverse has to be cached (or can be explicitly set by using x$set )
## Returns:
##       a List with functions usable for setting and getting the matrix and its inverse in a cached manner
##        
makeCacheMatrix <- function(x = matrix()) {
        cached.inverse <- NULL
        set <- function (y) {
                x <<- y
                cached.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) cached.inverse <<- inv
        getinverse <- function() cached.inverse
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## 
## Args:
##       x: List created from makeCacheMatrix (an invertible Matrix)
## Returns:
##       Cached Inserve Matrix or a newly computed one. If cached console prints "getting cached Inverse Matrix"
##        
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached Inverse Matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m                  ## Return a matrix that is the inverse of 'x'
} 
