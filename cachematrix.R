## Functions to create a special matrix object 
##  1) one that allows saving of the inverse matrix
##  2) another one that finds inverse of a matrix and save it, 
##      in case it is not available already
## 
## The first function issues warnings in case the user wants to save something 
## else than square matrix, or if the inverse matrix has different dimensions than
## the original matrix.
## 
## The second functions issues errors in case user wants to invert something 
## which is not invertible (i.e. is not square matrix with non-zero determinant).
## In general the error messages in 'cacheSolve' should be more helpful than the 
## default ones and should help to identify where the problem is right away
## 
## Example usage:
## > load("cachematrix.R")
## > # make a special matrix object containing invertible matrix
## > # 1 1 1 
## > # 2 3 2
## > # 3 4 5
## > mySpecMatrixObject <- makeCacheMatrix(x=matrix(c(1,1,1,2,3,2,3,4,5),3,3))
## > mySpecMatrixObject$getinverse()
## NULL  # because nothing is cached yet
## > cacheSolve(mySpecMatrixObject) # returns inverse matrix
##      [,1] [,2] [,3]
## [1,]  3.5   -2 -0.5
## [2,] -0.5    1 -0.5
## [3,] -0.5    0  0.5
## > mySpecMatrixObject$getinverse() # now it returns inverse matrix



## Function 'makeCacheMatrix' creates a special "matrix" object with 
## "member values" (inverse matrix) and
## accessor and settings "methods" (cf. classes in C++)

makeCacheMatrix <- function(x = matrix()) {
      inverse <-NULL # initialization of a variable that holds the inverse matrix
      set <- function(y) { 
            ## function to set a matrix from input y to variable x
            
            # checking if the input is a square matrix. If not issue a warning
            if(!is.matrix(y)) warning("input object is not a matrix")
            else if(nrow(y)!=ncol(y)) warning("input matrix is not square matrix")
            # saving the input into the variables of the special "matrix" object
            x <<- y
            inverse <<- NULL
      }
      get <- function() x # returns matrix object stored in 'x'
      setinverse <- function(inv){
            ## function to set inverse matrix to 'x' to a variable 'inverse'
            
            # check if inverse is a matrix and has the same dimensions as 'x'
            if(!is.matrix(inv)) warning("inverse matrix is not a matrix")
            if(ncol(inv)!=ncol(x)||nrow(inv)!=nrow(x)) 
                  warning("input inverse matrix has different dimensions than x")
            
            inverse <<-inv    
      }
      getinverse <- function() inverse #returns stored inverse matrix
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## Function to return an inverse of an input matrix. There are 2 cases:
## a) inverse matrix is stored in 'matrix' object from previous function 
##          =>  it is read from there
## b) inverse matrix is NOT stored in 'matrix' object from previous function 
##          => it is calculated on-the-fly and stored in the 'matrix' object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inverse <- x$getinverse()
       if(!is.null(inverse)){
             # the inverse was available in the 'x' object
             message("successfully retrieved cached data")
             return(inverse)
       }
       # if we got here, it means that the 'inverse' was null. 
       # It will be calculated on-the-fly
       
       data <- x$get() # retrieving the matrix from input object
       # checking if data is invertible matrix
       if(!is.matrix(data)) stop ("Input object is not a matrix.")
       if(ncol(data)!=nrow(data)) stop("Input matrix is not square.")
       if(det(data)==0) stop("Determinant of input matrix is O. Not invertible")
       inv <- solve(data,...) # calculating and saving the inverse matrix
       
       x$setinverse(inv) # caching the hard-obtained inverse
       inv
}
