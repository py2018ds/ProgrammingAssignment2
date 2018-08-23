makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    #Matrix changed, set inverse to null
    x <<- y
    inverse <<- NULL  
  }
  #retrieve matrix
  get <- function() x  
  #store inverse
  setinverse <- function(inv) inverse <<- inv 
  #retrieve inverse
  getinverse <- function() inverse            
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  #retrieve cached inverse
  inverse <- x$getinverse()  
  if(!is.null(inverse)) {  
    #cached inverse is not null, return it
    message("getting cached data")
    return(inverse)  
  }
  #get the matrix
  data <- x$get()    
  #calculate its inverse
  inverse <- solve(data, ...)  
  #store the inverse to cache
  x$setinverse(inverse)      
  #return calculated inverse
  inverse		               
}

m <- matrix(data = 1:4, nrow = 2, ncol = 2, byrow = FALSE,
            dimnames = NULL)

#test functions
x <- makeCacheMatrix(m)
#first time, calculating mean and saving to cache
cacheSolve(x)


#second time, getting cached data
minv <- cacheSolve(x)
print(minv)


m <- matrix(data = 2:5, nrow = 2, ncol = 2, byrow = FALSE,
            dimnames = NULL)
#change the matrix, clear cache
x$set(m)

#first time, calculating mean and saving to cache
cacheSolve(x)

#second time, getting cached data
cacheSolve(x)





