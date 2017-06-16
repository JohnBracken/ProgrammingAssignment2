#The following functions are used to calculate the inverse 
#of a matrix and store it in the cache.  If a matrix inverse 
#has already been calculated, the inverse matrix will be pulled
#from the cache instead of being recalculated when the same input
#matrix is used again to calculate the inverse.  This approach
#eliminates unneccesary recalculation to free up memory.


## This function returns a list of four functions to set
#a matrix and its inverse in the cache and also to get
#these matrices.
makeCacheMatrix <- function(x = matrix()) {
  
  #Matrix inverse initially assigned a null value.
  inv_mat <- NULL
  
  #Inner function to set the initial matrix in the cache.
  #Set the default inverse matrix to a null value.
  set_mat <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  
  #Inner function to get the initial matrix.
  get_mat <- function() x
  
  #Inner function to set the inverse matrix in the cache.   
  setinverse <- function(inverse) inv_mat <<- inverse
  
  #Inner function to get the matrix inverse.
  getinverse <- function() inv_mat
  
  #Return function list
  list(set_mat = set_mat, get_mat = get_mat,
       setinverse = setinverse,
       getinverse = getinverse)
  }


#This function calculates the inverse of a matrix if it already
#hasn't been calculated and cached.  If it has been, then the inverse
#is pulled from the cache instead of being recalculated.
cacheSolve <- function(x) {
    
    #Try to get the inverse matrix if it has already been calculated and cached.    
    inv_mat <- x$getinverse()
    
    #If the inverse matrix is already in the cache, get it.
    if(!is.null(inv_mat)){
          message("getting cached data")  
          return(inv_mat)
    }
    
    #If not, get the original matrix, calculate its inverse, and send the result
    #back to the cache.
    data <-x$get_mat()
    inv_mat <- solve(data)
    x$setinverse(inv_mat)
    
    #Return the matrix inverse.
    inv_mat
}
