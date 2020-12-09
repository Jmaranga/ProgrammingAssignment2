makeCacheMatrix <- function(x = matrix()){ ## I assume the matrix to supply here will be invertible
  bn <- NULL ## creating a null variable to the inverse
  set <- function(y){ ## Setting the value of the Matrix using a different/another function
    x <<- y ## Creating a function returned by another function using <<-
    bn <<- NULL
  }
  get <- function() {x}    ## Getting the value of the Matrix
  setInverse <- function(inverse) {bn <<- inverse} ## Setting the value of the Inverse and assign it to setInverse
  getInverse <- function() {bn} ## Getting the value of the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## Creating a list that will hold the inverse
}

cacheSolve <- function(x, ...){ ## cacheSolve is a function which computes the inverse of the special "matrix"
  bn <- x$getInverse() ## Returning a matrix that is inverse of x and assigns it to bn
  if(!is.null(bn)){ ## Checking if the inverse has been generated and if so, avoid further computation
    message("getting cached data") ## if the inverse will be retrieved from the Cache, this will be displayed
    return(bn) ## The inverse will be returned
  } ## Otherwise, Computing the values of the Matric
  mat <- x$get() 
  bn <- solve(mat, ...) ## Computing the inverse of the matrix
  x$setInverse(bn) ## Set the value of the inverse in Cache by sung setInverse function
  bn ## Return the inverse
}
