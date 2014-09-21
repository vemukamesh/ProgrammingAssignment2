##MakeCacheMatrix returns list of functions
## Function          Purpose
##________________________________
## setM             Sets value of the matrix
## getM             Gets value of the matrix
## cacheInv         Gets cached value of matrix
## getInv           Gets cached value of matrix  

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL                               #Initially cached value is NULL
  
  setM <- function(y) {                       #store matrix
    x <<- y    
    cache <<- NULL                           #flush cache since new value "y"
  }
  
  getM <- function() x                        #return stored matrix

  cacheInv <- function(solve) {               #cache the inverse
    cache <<- solve
  }
   
  getInv <- function() cache                  #getcached value
    
  list(setM = setM, getM = getM, cacheInv = cacheInv, getInv = getInv)
}


## Function gets the inverse of a matrix
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(y=matrix(), ...) {
       
        inverse <- y$getInv()
        
        if(!is.null(inverse)) {
        message("have cached data")
        return(inverse)
        } else {
        message("need to calculate inverse")
        data <- y$getM()
        inverse <- solve(data)
        y$cacheInv(inverse)
        return(inverse)
        }
}
