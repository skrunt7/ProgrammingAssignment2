## makeCacheMatrix is a function that returns functions to get/set a special matrix
##in cache as well as to get/set the inverse of the matrix in cache.  The
##cacheSolve function takes in a special matrix.  It then determines if the inverse is already set in cache.
##.  If so, it gets the inverse from cache using makeCacheMatrix and returns it.  
##Otherwise, it utilizes the solve() function in R to calculate the inverse of the square matrix.
##The function then sets that inverse in cache using makeCacheMatrix and returns it.


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

##This function computes the inverse of a special matrix
##If the inverse has already been calculated then the cachesolve should retrieve 
##the inverse from the cache and return it rather than re-calculating it.
cacheSolve <- function(x, ...) {
     i = x$geti()
     
     # if the inverse has already been calculated
     if (!is.null(i)){
          # retrieve the inverse from cache 
          message("getting cached data")
          return(i)
     }
     
     # else, calculate the inverse 
     data = x$get()
     i = solve(data, ...)
     
     # sets the value of the inverse in the cache via the setinv function.
     x$seti(i)
     
     return(i)
}

