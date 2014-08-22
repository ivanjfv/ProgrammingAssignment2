## Put comments here that give an overall description of what your
## functions do

# These two functions create a special matrix that can keep and calculating its inverse, the inverse calculation 
# is saved and is not recalculated unless you change the values of the matrix
# The special matrix is a list that contains 4 elements:
# 1. set, function to save in a different environment (to cahe) values of the matrix
# 2. get, function to get the elements of the matrix
# 3. setinverse, function to save (to cahe) the inverse of the matrix
# 4. getinverse, function to get the inverse of the matrix

# it is assumed that the matrix supplied is always invertible

#exmple
# > m<-makeCacheMatrix( matrix(c(1,2,-1, 2,1,1, -1,0,2), nrow = 3, ncol = 3))
# > cacheSolve(m)
# [,1]       [,2]       [,3]
# [1,] -0.2222222  0.5555556 -0.1111111
# [2,]  0.4444444 -0.1111111  0.2222222
# [3,] -0.3333333  0.3333333  0.3333333
# > 



## Write a short comment describing this function
## This function creates a special matrix, which is actually a list


makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL ##inv variable to cache inverse of the matrix 
        set<-function(y){ ## function to save in a different environment (to cahe) values of the matrix
                x <<- y ## save the matrix (to cahce)
                inv <<- NULL ## to cahe inv
        }
        get <- function() x ## function to get the elements of the matrix
        setinverse<- function(invr) inv <<- invr ## function to save the inverse of the matrix
        getinverse <- function() inv  ## function to get the inverse of the matrix
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)  ## creates the list
}


## Write a short comment describing this function

##This function calculates the inverse of the special matrix.
##It is only calculated if the inverse was not calculated before


cacheSolve <- function(x) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

