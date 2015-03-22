## Matrix inversion may be time-consuming if it has to be computed repeatedly. There may be some benefit to caching the value 
## of the inverse of a matrix. So, when we need it again, it can be looked up in the cache rather than recomputed.
## The following pair of functions creates a special object that stores a matrix and cache's its inverse.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. The special "matrix" 
## object is a list containing the following functions : 
## 1. function to set the value of the matrix
## 2. function to get the value of the matrix
## 3. function to set the value of the inverse of the matrix
## 4. function to get the value of the inverse of the matrix
 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                           ## 1. function to set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                            ## 2. function to get the value of the matrix
        setinverse <- function(inverse) m <<- inverse  ## 3. function to set the value of the inverse of the matrix
        getinverse <- function() m                     ## 4. function to get the value of the inverse of the matrix
        list(set = set,                                ## Returns a list containing 1., 2., 3. and 4.
             get = get,                     
             setinverse = setinverse,      
             getinverse = getinverse)      
}

## The function "cacheSolve" calculates the inverse of the special matrix created with makeCacheMatrix. 
## It first checks if the value of the inverse of the matrix has already been calculated. 
## If so, it gets this value from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets its value in the cache using the "setinverse" function.
## The function "cacheSolve" assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {                        ## If the old cache value DOES exist
                message("getting cached data")   ## Prints the message "getting cached data"
                return(m)                        ## Returns the value of the inverse from the cache
        }
        data <- x$get()                          ## If the old cache value DOES NOT exist, it gets the value of the matrix
        m <- solve(data,...)                     ## Calculates the inverse of the matrix using the "solve" function
        x$setinverse(m)                          ## Uses the "setinverse" function to set the inverse of the matrix in the cache 
        m                                        ## Returns the value of the inverse of the matrix
}

## Example:
## > setwd("~/ProgrammingAssignment2")
## > source("cachematrix.R")
## > x<-matrix(1:4, 2, 2)
## > m<-makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## First run with the "cacheSolve" function
## > cacheSolve(m) 
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## No old cache value is retrieved in the first run  

## Second run with the "cacheSolve" function 
## > cacheSolve(m)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Retrieves & assigns the old cache value in the second run

