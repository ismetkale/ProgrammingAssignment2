# Matrix inversion is usually a costly computation. After computing it one time, 
# we can cache the result in a different environment and reuse it directly 
# rather than computing it repeatedly.

# makeCacheMatrix() is a function that takes a square invertible matrix as its input and
# creates a special "matrix" object that can be used as input in other functions 


makeCacheMatrix <- function(x = matrix()) {
        ## input: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ## this list is used as the input to cacheSolve() described below
        
        inv = NULL
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)     
}



# cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
# If the inverse has already been calculated and the matrix has not changed, 
# it’ll retrieves the inverse from the cache directly.


cacheSolve <- function(x, ...) {
        ## input: output of makeCacheMatrix(x)
        ## return: inverse of the original matrix input to makeCacheMatrix(), 
        ## which is the inverse of 'x'
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}




##Testing My Functions

## > source("ProgrammingAssignment2/cachematrix.R")
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

## > my_matrix$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

# Note that there is nothing in the cache in the first run, computes the inverse
## > cacheSolve(my_matrix)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

# Note that its retrieving from the cache in the second run
## > cacheSolve(my_matrix)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > my_matrix$getInverse
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

## > my_matrix$get()
##       [,1] [,2]
## [1,]    2    1
## [2,]    2    4

## > cacheSolve(my_matrix)
##         [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333

## > cacheSolve(my_matrix)
## getting cached data
##         [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333

## > my_matrix$getInverse()
##         [,1]       [,2]
## [1,]  0.6666667 -0.1666667
## [2,] -0.3333333  0.3333333
