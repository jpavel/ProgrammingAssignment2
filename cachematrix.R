## Functions to create a special matrix object \
##  1) one that allows saving of the inverse matrix
##  2) another one that finds inverse of a matrix and save it, 
##      in case it is not available already
## 

## Creates a special "matrix" object with "member values" (inverse matrix) and
## accessor and settings "methods" (cf. classes in C++)

makeCacheMatrix <- function(x = matrix()) {
      inverse <-NULL # initialization of a variable that holds the inverse matrix
      set <- function(y) { 
            # function to set a matrix from input y
            
            # checking if the input is a square matrix. If not issue a warning
            if(!is.matrix(y)) warning("input object is not a matrix")
            else if(nrow(y)!=ncol(y)) warning("input matrix is not square matrix")
            # saving the input into the variables of the special "matrix" object
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv){
            # check if inverse is a matrix and has the same dimensions than 'x'
            if(!is.matrix(inv)) warning("inverse matrix is not a matrix")
            if(ncol(inv)!=ncol(x)||nrow(inv)!=nrow(x)) 
                  warning("inverse has different dimensions than x")
            
            inverse <<-inv    
      }
      getinverse <- function() inverse
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
