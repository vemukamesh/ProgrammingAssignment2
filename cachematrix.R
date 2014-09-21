##MakeCacheMatrix returns list of functions
##This function has four functions with purpose defined below
##________________________________
## Function          Purpose
##________________________________
## setM             Sets value of the matrix
## getM             Gets value of the matrix
## cacheInv         Gets cached value of matrix
## getInv           Gets cached value of matrix  

makeCacheMatrix <- function(x = matrix()) {   #changed numeric in original function to matrix
  
  m <- NULL                                  #Initialize cached values with NULL  
  set <- function(y) {                       #store matrix
    x <<- y    
    m <<- NULL                               #flush cache, i.e. set it to NULL since new value "y"
  } 
  get <- function() x                        #return stored matrix
  setInv <- function(solve) {                #cache the inverse
    m <<- solve
  }
  getInv <- function() m                     #getcached value
  
  #return list of functions
  list(set = set, 
       get = get, 
       setInv = setInv, 
       getInv = getInv) 
}


## Function gets the inverse of a matrix
## First checks if inverse has already been computed
## If so it skips the computation and retrieves the cached value
## Else it computes the inverse of the matrix and sets the value of inverse in the cache using the 
## setInv function
## Return a matrix that is the inverse of 'y'
cacheSolve <- function(y=matrix(), ...) {
       
        m <- y$getInv()
        if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
        } else {
                  message("need to calculate inverse")
        data <- y$get()
        m <- solve(data)
        y$setInv(m)
        return(m)
        }
}
