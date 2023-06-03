
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # Assigns the value y to x and the value NULL to inv
    assign(x, y, inherits = TRUE)
    assign(inv, NULL, inherits = TRUE)
  }     
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # Returns a list with the specified rows and columns 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # Checks if the inverse of the matrix is null
  if(!is.null(inv)) {
    return(inv)
  }
  else {
    print("no inverse")      
  }
  orig_matrix <- x$get()
  # Calculates the inverse of the matrix 
  inv <- solve(orig_matrix, ...)
  x$setinverse(inv)
  # Returns the inverse
  inv
}
