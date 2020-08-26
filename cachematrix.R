## The below 2 functions will inverse the matrix
## Example: 
## input <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(input)

## makeCacheMatrix function takes matrix as an input
## It then creates constructor functions for set, get, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
      flip <- NULL
      set <- function(inputMatrix){
              x <<- inputMatrix
            flip <<- NULL
      }
      get <- function() x
      setInverse <- function(inverseMatrix) flip <<- inverseMatrix
      getInverse <- function() flip
      list(set = set, get = get, getInverse = getInverse,
           setInverse = setInverse)
}


## cacheSolve function will inverse the matrix which is set by the above function
## First time execution the inverse will be stored in flip variable
## Second time execution, if the inverse is available in flip, then it uses that(cached data)
## the function returns the variable flip

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      flip <- x$getInverse()
      if(!is.null(flip)){
              message("Already inversed : getting cached data")
              return(flip)
      }
      cachedmatrix <- x$get()
      flip = solve(cachedmatrix,...)
      x$setInverse(flip)
      flip
}
