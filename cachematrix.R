## The makeCacheMatrix() function forms a list that of functions that: 
## 1. set the value of a matrix and assign it to the global environment
## 2. get the value of this matrix 
## 3. set the value of the inverse of this matrix and assign it to the global environment
## 4. get the value of the inverse of this matrix 


## Initialize the makeCacheMatrix() function, which only takes one matrix for an argument:
makeCacheMatrix <- function(x = matrix()) { 
      ## First, initialize the invrs variable:
      invrs <- NULL 
      ## Then the series of functions described above:
      ## First, the set() function:
      set <- function(y){ 
          ## The purpose of the set() function is to assign the matrix passed in makeCacheMatrix()
          ## to a variable y in the global environment using the '<<-' assignment operator:
          x <<- y
          ## The set() function also initializes the invrs variable in the global environment:
          invrs <<- NULL
      }
      ## Next, the get() function, which returns the matrix passed in makeCacheMatrix():
      get <- function() x
      ## Then the setMatrixInverse() function, which returns invrs and assigns it to the variable inverse,
      ## stored in the global environment. At this point, invrs will now be cached.
      setMatrixInverse <- function(inverse) invrs <<- inverse
      ## The getMatrixInverse() function returns the cached value of invrs.
      getMatrixInverse <- function() invrs
      ## Finally, a list of the above functions and their respective values:
      list(set = set, get = get, setMatrixInverse = setMatrixInverse, 
            getMatrixInverse = getMatrixInverse)
}


## The cacheSolve() function calculates the inverse of a matrix that you you'd passed in to makeCacheMatrix()
## earlier; however, before it performs the calculation, it searches through the cache (global environment) 
## to see if the inverse of your matrix has already been calculated and stored. If it has been calculated 
## and stored previously, it does not go through the calculation at all. Rather, it returns the cached inverse. 
## If it has not been previously calculated and stored, it calculates it and then stores it.

## Initialize cacheSolve() and pass a matrix you'd passed in to makeCacheMatrix() earlier:
cacheSolve <- function(x) {
        ## Look through the cache to see if the inverse of your matrix has been stored:
        invrs <- x$getMatrixInverse()
        ## If it has been stored, return the cached invrs (function terminates, end of story):
        if(!is.null(invrs)){
            message("Getting cached data...")
            return(invrs)
        }
        else {
        ## If it has NOT been stored, the value of your matrix is assigned to variable newMatrix: 
        newMatrix <- x$get()
        ## Its inverse is then calculated:
        invrs <- solve(newMatrix)
        ## Then stored (so that if you want to get it again, no new expensive calculation will be made!):
        x$setMatrixInverse(invrs)
        ## Finally, it is returned to you:
        invrs
        }
}
