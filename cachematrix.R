## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### R programming assignment 2 in week 3

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

# The function 'cacheSolve' returns the inverse of a matrix - given that all
# matrices are invertible. After checking if the inverse has been computed,
# it gets the already existing result and skips the computation. If the inverse
# does not exist, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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



## sample data to try above function
#   x = rbind(c(1, 2), c(1, 3))
#   m = makeCacheMatrix(x)
#   m$get()  
#   m$getinverse()    ## will be NULL in the first run
#   cacheSolve(m)
#   cacheSolve(m)


### Output should look like this:
# > m$get()
#       [,1] [,2]
# [1,]    1    2
# [2,]    1    3
# > cacheSolve(m)
#       [,1] [,2]
# [1,]    3   -2
# [2,]   -1    1
# > cacheSolve(m)
# getting cached matrix
#       [,1] [,2]
# [1,]    3   -2
# [2,]   -1    1
