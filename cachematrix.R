## This file contains 2 functions: 
    ## one that creates a matrix object, 
    ## and one that operates on the matrix object to return an inverse matrix.


## makeCacheMatrix()creates the special matrix object that may cache the inverse matrix.

makeCacheMatrix <- function(loaded_matrix = matrix()) { #mCM refers to this function.
 
  #This line creates a null variable which may later be filled when the inverse is calculated 
  inverse_matrix <- NULL
  
  #This function can take in a new matrix and write it over our original.
    #It can do this without re-running the entire mCM function.
  setMatrix <- function(new_matrix) { 
    loaded_matrix <<- new_matrix #Sets the matrix value inside mCM equal to new_matrix
    inverse_matrix <<- NULL #If we reset the matrix, we want to reset the inverse matrix to NULL
  }
  
  getMatrix <- function() { #This function takes no arguments, because loaded_matrix  is in it's parent frame.
    return(loaded_matrix)#It returns the matrix to the parent frame, so we know the current value.
    
  }
  
  #This function returns the inverse matrix.
    #This will be NULL unless the inverse has been calculated.
  getInverseMatrix <- function(){
    return(inverse_matrix)
  }
  
  #this function takes original matrix and find its inverse. 
  calculateInverseMatrix <- function(){ 
    inverse_matrix <<- solve(loaded_matrix) 
    #It returns the solved matrix to the scope where inverse_matrix was first defined.
  }
  list(loaded_matrix=loaded_matrix,
       setMatrix=setMatrix,
       getMatrix=getMatrix,
       getInverseMatrix=getInverseMatrix,
       calculateInverseMatrix=calculateInverseMatrix)
      #This creates a list of the functions within mCM.
      #this list doesn't exist outside of the scope of the function, but will print to console.
      #we can access members of this list without running the entire mCM function.
}

#How does it work?
  #for square matrix c, we can run c_2<-makeCacheMatrix(c)
      #then: 
        # c_2$getMatrix() will return c,
        # c-2$getInverseMatrix() will return NULL

      # if we enter c2_calculateInverseMatrix(), the calculation will occur.
        # after that, c_2$getInverseMatrix() returns the calculated inverse matrix.

      # if we have another square matrix d, and we enter c_2$setMatrix(d)
        # d writes over c and is now the loaded matrix
        # inverse_matrix is reset as NULL, and will hold that value unless/until it is calculated.
      



## cacheSolve() will determine if the inverse matrix of matrix x is cached in object x_2
    ##if the inverse matrix is cached, cacheSolve() will return it.
    ##if the inverse matrix is not cached, cacheSolve() will calculate it and return it.

cacheSolve <- function(x, x_2) {
    #assume that we already did this: 
      # x_2<-makeCacheMatrix(x)
  
#First, let's verify that x is currently loaded as the matrix inside of x_2; 
  #it might have been reset through x_2$setMatrix()
  y<-x_2$getMatrix()

  if(isTRUE(all.equal(x,y))){ #x is the vector loaded in x_2
    
    #Then we check if the inverse matrix is currently cached.
    m <- x_2$getInverseMatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }else{
      message("Inverse matrix was not cached. Now calculating inverse matrix.")
      m<-x_2$calculateInverseMatrix()
      return(m)
    }
   
  }else{  #if x was not the matrix loaded in x_2, we must reset x as the matrix in x_2
    message("Matrix was not loaded. Now loading matrix and calculating inverse matrix.")
    x_2$setMatrix(x)
    m<-x_2$calculateInverseMatrix()
    return(m)}
}
