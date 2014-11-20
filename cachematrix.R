## This function attempts to cache the inverse of a given matrix.
## For the purpose of this function, the input matrix is assumed to be invertible


makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       setmatrix <- function(y) {
              x <<- y
              m <<- NULL
       }
       getmatrix <- function() { x }
       setinv <- function(solve) { m <<- solve }
       getinv <- function() { m }
       list(setmatrix = setmatrix, getmatrix = getmatrix,
            setinv = setinv,
            getinv = getinv)
              
}

## The input for cacheSolve function is the makeCacheMatrix(x) function above
## This function takes the cached matrix from the makeCacheMatrix function and checks if an inverse is already
## calculated. If it is, then it takes the already available inverse of the input matrix and returns that.
## If previously calculated inverse is not available, it calculates the inverse and returns that.

cacheSolve <- function(x=matrix(), ...) {
       m <- x$getinv()
       if(!is.null(m)){
              message("Getting cached data")
              return(m)
       }
       
       matrix <- x$getmatrix()
       m <- solve(matrix)
       x$setinv(m)
       m
}

## The code can be executed like this:
## Run both functions.
## Case 1: 
##      Call makeCacheMatrix(your_matrix)
##      Then call cacheSolve(makeCacheMatrix(your_matrix))
##      The output is the inverse of your_matrix

## Case 2:
##      Call makeCacheMatrix(your_matrix)
##      Then call cacheSolve(makeCacheMatrix(different_matrix))
##      The output is the inverse of different_matrix

## Case 3:
##      Do not call makeCacheMatrix this time.
##      Then call cacheSolve(makeCacheMatrix(a_completely_different_matrix))
##      The output should be the inverse of a_completely_different_matrix

## Sample matrices I used:
## your_matrix <- matrix (c(4,3,3,2),nrow=2,ncol=2)
## different_matrix <- matrix (c(7,6,6,5),nrow=2,ncol=2)
## your_matrix <- matrix (c(10,7,7,4),nrow=2,ncol=2)





