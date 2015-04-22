## Make a matrix object that are able to cache its inverse
## and a function to calculate and store the value into the object


makeCacheMatrix <- function(x = matrix()) {
  ## Returns a list of functions, to set and get the matrix, and
  ## to set and get the inverse of the matrix
  
  inv <- NULL
  
  # Function to store a matrix in the object
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  # Function to get the matrix from the object
  get <- function () {
    x
  }
  
  # Function to set the inverse of the matrix
  set_inverse <- function (i) {
    inv <<- i
  }
  
  # Function to get the inverse of the matrix
  get_inverse <- function () {
    inv
  }
  list (set = set, get = get, 
        set_inverse = set_inverse, get_inverse = get_inverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. Checks first
  ## if the inverse is already cached.
  
  inv <- x$get_inverse ()
  if (is.null (inv)) {
    inv <- solve (x$get ())
    x$set_inverse (inv)
  }
  inv
}
