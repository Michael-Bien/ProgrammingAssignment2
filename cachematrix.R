## These functions, combined, will calculate the inverse of a matrix, if it is not already calculated; 
## however, if the inverted matrix has already been calculated, it returns the cached matrix, saving time

## makeCacheMatrix returns a list of functions that can set, get, set an inverted matrix, and get an inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  Inverted_Matrix <- NULL     
  set <- function(y) {  
    x <<- y
    Inverted_Matrix <<- NULL
  }
  get <- function() x     #get returns x
  set_Inverted_Matrix <- function(User_Sets_Inverted_Matrix) Inverted_Matrix <<- User_Sets_Inverted_Matrix
  get_Inverted_Matrix <- function() Inverted_Matrix  
  list(set = set, get = get,  #listing out the object
       set_Inverted_Matrix = set_Inverted_Matrix,
       get_Inverted_Matrix = get_Inverted_Matrix)
  
}


#cacheSolve checks to see whether an inverted matrix is stored, and otherwise calculates it 

cacheSolve <- function(x, ...) {
  
  
  Inverted_Matrix <- x$get_Inverted_Matrix()  
  if(!is.null(Inverted_Matrix)) {  
    message("getting cached data")
    return(Inverted_Matrix)
  }
  data <- x$get()  
  Inverted_Matrix <- solve(data, ...)  
  x$set_Inverted_Matrix(Inverted_Matrix)    
  Inverted_Matrix    
  
  
}
