## cachematrix.R
## This is a pair of functions used to invert matrices with the 
## extra feature of cachig previously inverted matrices to improve performance.
## This is straight rewite of makeVector and cachemean provided in github
## https://github.com/rdpeng/ProgrammingAssignment2
## by Prof. Peng of Johns Hopkins University
## 
## Usage:
## Load the two functions.
## Create an invertable matrix and cache it with makeCacheMatrix
## this essentially turs the matrix into an object with getters and setters
## that can be later reused.
## Call cacheSolve to invert the matrix.
## The first time the matrix will be inverted, the second and subsequent times
## The cached inverted matrix wll be used
##
## Example
## > c <- matrix(1:4, 2, 2)
## > c
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > z <- makeCacheMatrix(c)
## > cacheSolve(z)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(z)
## getting cached inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## makeCacheMatrix creates a list of functions used to persist an inverted matrix
## It is esentially making an object of the inverted matrix with getter and setters

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve is passed a matrix to invert 
## It invertets the matrix and uses makeCacheMatrix methods to persist 
## the inverted matrix.  If the passed in matrix has already been persisted,
## the persisted copy is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
            message("getting cached inverted matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
