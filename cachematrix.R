## This function creates a cacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # Set the data of the object and clear cached results
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # Return current object-data
        get <- function() x
        # Set the inverse of the matrix
        setInverse <- function(inv) i <<- inv
        # Return the inverted matrix
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of a matrix
## The header of the function was modified since I got no good answer on the forums about what the indented meaning of the additional parameters are.
## Thus, the function is "only" capable of computing the inverse of matrix
##  instead of having the possibility of bypassing some additional parameters

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # retrive last stored value
  matrix <- x$get()
  # compute inverse
  i <- solve(matrix)
  # store inverse
  x$setInverse(i)
  # return result
  i
}
