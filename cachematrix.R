##Overall, these two functions "makeCacheMatrix and "cacheSolve" will cache the inverse of a matrix.

#makeCacheMatrix is a function whose return value is a list  of four functions (set,get,setsolve,getsolve)
#this function will make a special matrix object that can cache its inverse.
#This should be assigned to a variable such as "v", for example. 


makeCacheMatrix <- function(x = matrix()) {
  
  #"m" starts as NULL in the Global environment
  
  m <- NULL 
  
  #set is a function that takes y as an argument.
  #This will be used to set the original matrix that we want to inverse
  #Example:v$set([insert matrix here])
  
  set <- function(y) {
    
    #Assign "y" to "x" in a parent environment
    
    x <<- y 
    
    #Assign "m" as NULL in a parent environment
    
    m <<- NULL
  }
  
  #"get" is a function without any arguments which returns the expression "x". 
  #calling this function will "get" or return the matrix which you have previously "set"
  
  get <- function() x
  
  #"setsolve" is a function who takes recalcsolve as an argument.
  
  setsolve <- function(recalcsolve){
    
    #"m" is assign recalcesolve in a parent environment 
    
    m <<- recalcsolve
  }
  
  #getsolve is a function without any arguments 
  #which takes returns the expression "m" (previously assigned) 
  
  getsolve <- function() m
  
  #when called (e.g makeVector(v))this will return a list of set,get, setsolve, and getsolve's functions 
  #and in what environment they are in
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##"cacheSolve" is a function that computes the inverse of the special "matrix"
##returned by makeCacheMatrix found above. 
##If the matrix has not changed and the inverse has already been calculated
##then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #m is assigned to the value of  x's getsolve()
  
  m <- x$getsolve()
  
  #if "m" does not have a NULL value, then a message will be shown stating "getting cached data"
  #and the value of "m" will be returned which was retrieved from the cache
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##if "m" is NULL then x$get() will be named data
  ##x$get retrieves our special matrix
  
  data <- x$get()
  
  ##then m will be assigned as the function which calculates the inverse of a matrix
  
  m <- solve(data, ...)
  
  #The inverse of our special matrix is calculated, 
  #but the inverse is also cached for when it is called upon again
  
  x$setsolve(m)
  
  #the vakue "m" is returned with the inverse of the special matrix
  m
}
