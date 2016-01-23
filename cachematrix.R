## makeCacheMatrix is a function that returns functions to get/set a special matrix
##in cache as well as to set/get the inverse of the matrix from cache.  The
##cacheSolve function takes in a matrix.  It then utilizies the makeCacheMtrix
##function to determine if the matrix and inverse are already set.  If so, it returns
##the value from cache.  Otherwise, it utilizes the solve() function in R to calculate
##the inverse of the square matrix


##function to create a special "matrix" object that can cache it's inverse.  
##The functoin takes in a square matrix as an argument and returns a list of 
##functions to set/get the matrix and set/get the inverse
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     seti <- function(inverse) i <<- inverse 
     geti <- function() i
     list(set=set, get=get, seti=seti, geti=geti)

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.
cacheSolve <- function(x, ...) {
     i = x$geti()
     
     # if the inverse has already been calculated
     if (!is.null(i)){
          # retrieve the inverse from cache 
          message("getting cached data")
          return(i)
     }
     
     # else, calculate and return the inverse 
     data = x$get()
     i = solve(data, ...)
     
     # sets the value of the inverse in the cache via the setinv function.
     x$seti(i)
     
     return(i)
}

