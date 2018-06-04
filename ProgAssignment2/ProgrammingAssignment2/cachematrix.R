## This is a pair of functions that will cache the inverse of a matrix
## and, when a matrix inverse is called, first check to see if the 
## inverse has already been created and cached.

## makeCacheMatrix creates a matrix object and creates several functions
## that can be called on any matrix object of that type. These are get()
## to retrieve the matrix, set() to set the value of the matrix, 
## setinverse() to calculate and save the inverse of the matrix, and 
## getinverse() to retrieve the inverse of the matrix. It saves these 
## functions in a list by name, so that the functions can be called
## vis mymatrixname$getinverse() etc ...

makeCacheMatrix <- function(x = matrix()) { ## initializes an empty matrix if matrix isn't given in argument
  inv <- NULL  ## initializes the matrix inverse variable as NULL until it is calculated
  set <- function(y){  ## resets the matrix to a new value 
    x <<- y            
    inv <<- NULL       ## resets the inverse variable to NULL in case prior matrix inverse is cached
  }
  get <- function() x  ## retrieves the matrix
  setinverse <- function() {  ## sets the variable inv equal to (and stored as) the inverse of x using solve()
    inv <<- solve(x)
  }
  getinverse <- function() inv  ## retrieves the variable which holds the inverse matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## creates a list of function names
}                                                                        ## so functions can be called via $ sign


## Checks to see if an inverse of a given makeCacheMatrix object has been cached.
## If so, it returns that inverse matrix. If not, it calculates and caches that inverse matrix using
## the mymatrix$setinverse() function defined in makeCacheMatrix

cacheSolve <- function(x, ...) {  ## takes as an argument a makeCacheMatrix object
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  ## retrieves the inverse matrix of x using the $getinverse() function defined for makeCacheMatrix objects
  if(!is.null(inv)){     ## if the inverse for the given matrix is not NULL (if it exists), returns inverse matrix
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()      ## if the inverse matrix was NULL, saves the matrix object from the argument in a variable
  inv <- solve(data, ...) ## and then computes the inverse of the matrix object from the argument
  x$setinverse()          ## and then saves the inverse of the matrix to the cache using the $setinverse() function
  inv                     ## returns the inverse matrix
}
