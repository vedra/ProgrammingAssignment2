## Below are two functions used to create a special object that stores a matrix 
# and cache's its inverse.

##  Function makeCacheMatrix creates a list of 4 functions that: 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      # Create list of 4 fcns to deal with caching
      
      # 1st: content of the input variable is taken from the input, 
      # but s (the output which is a soulution of the inverse ) is still NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      
      # 2st: get returns back the value set with fcn set, so doesn't need an input
      get <- function() x
      
      # 3rd: content of the solution is given as an input and saved into s
      setsolve <- function(solve) s <<- solve
      
      # 4th: getsolve returns back the value set with fcn setsolve, so doesn't need an input
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}



## Function cacheSolve returns an inverse of the matrix in the following way:
# if the inverse was calculated before, it's taken from the cached memory, 
# otherwise it's calculated now for the 1st time and it's put in the cached memory


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      # call getsolve to get cached solution 
      # if cached solution exists, return it
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      
      # otherwise do the following:
      # take the content of the variable x
      data <- x$get()
      
      # run the solve fcn for the 1st time
      s <- solve(data, ...)
      
      # and put the solution in the cached memory
      # so that it could be reached later with $getsolve
      x$setsolve(s)
      s
}
