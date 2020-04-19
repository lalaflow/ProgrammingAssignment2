## Pair of functions that cache the inverse of a matrix:

## `makeCacheMatrix`: Creates a special "matrix" object that can cache its inverse:
makeCacheMatrix <- function(x = matrix()) {
    ## x is a square invertible matrix
    ## This function, `makeVector` creates a list containing a function to
    ##    1.  set the value of the vector
    ##    2.  get the value of the vector
    ##    3.  set the value of the mean
    ##    4.  get the value of the mean
inv <- NULL
set <- function(y){
    ## `<<-` operator used to assign a value to an object in an environment that is 
    ## different from the current environment
    x <<- y
    inv <<- NULL
}
get <- function()x
setinverse <- function(solve) inv <<- solve
getinverse <- function() inv
list(set = set, get = get, 
     setinverse = setinverse, 
     getinverse = getinverse)
}

## `cacheSolve`: Computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated, 
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Get the inverse function from `makeCacheMatrix`
    inv <- x$getinverse()
    ## Check to see if the inverse matrix has already been calculated
    if(!is.null(inv)) {
        message("getting cached data")
        ## If the inverse matrix is calculated (!null), get the inverse from the cache
        return(inv)
    }
    ## Otherwise, calculate the inverse and sets the value of the inverse in the cache
    ## via the `setinverse` function
    mat_data <- x$get()
    inv <- solve(mat_data, ...)
    x$setinverse(inv)
    ## Return a matrix that is the inverse of 'x'
    return(inv)
}

## First, to test this function I create a test1() function
test1 <- function(m){
    inverse_matrix <- makeCacheMatrix(m)
    cacheSolve(inverse_matrix)
}

## Example with a simple square invertible matrix (2x2) 'my_matrixA'
my_matrixA <- matrix(c(2,4,-3,-7),2,2)
my_matrixA
##       [,1] [,2]
## [1,]    2   -3
## [2,]    4   -7
my_matrixB <- test1(my_matrix)
my_matrixB
##       [,1] [,2]
## [1,]  3.5 -1.5
## [2,]  2.0 -1.0
## Check if the condition AB=BA=I for invertible matrix is fullfilled
my_matrixA%*%my_matrixB
##        [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## I got the Identity matrix :)

## Second, to test the time that it takes for the normal calculation and the extracting 
## calculation I used the test() function
test <- function(mat) {
    temp<- makeCacheMatrix(mat)
    
    start_time <- Sys.time()
    cacheSolve(temp)
    time_inverse <- Sys.time()-start_time
    print(time_inverse)
    
    start_time <- Sys.time()
    cacheSolve(temp)
    time_inverse <- Sys.time()-start_time
    print(time_inverse)
}

## Example of time test with test() in a bigger matrix of 1000000 random normalized elements
matrix1<-matrix(rnorm(1000000),nrow = 1000, ncol = 1000)
test(matrix1)
## Time difference of 2.738456 secs
## getting cached data
## Time difference of 0.004146814 secs