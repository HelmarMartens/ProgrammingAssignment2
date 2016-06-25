### Given a matrix as an argument, this function returns the inverse of that matrix


### The makeCacheMatrix function creates a special "matrix" that contains a function to set and get the value of the matrix as well as to set and get its inverse value. 
makeCacheMatrix <- function(x = matrix()) {
    inverseValue <- NULL
    set <- function(y) {
      x <<- y
      inverseValue  <<- NULL
    }
    get <- function() x
    setInverseValue <- function(res)  inverseValue  <<- res
    getInverseValue <- function() inverseValue
    list(set = set, get = get, setInverseValue = setInverseValue , getInverseValue = getInverseValue )
}


### The cacheSolve function calculates the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  	inverseValue = x$getInverseValue()
  	if(!is.null(inverseValue)) {
    		message("getting cached data")
    		return(inverseValue)
  	}
  	data = x$get()
  	inverseValue = solve(data,...)
  	x$setInverseValue(inverseValue)
  	inverseValue
}
