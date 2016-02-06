## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# Функция производит объект, который создает матрицу
makeCacheMatrix <- function(x = matrix()) {
    # Inversion is not yet evaluate, the object still creating
    invers<-NULL
   
    # Inversion is not yet evaluate, the object still creating 
    set<-function(y) {
        x<<-y
        invers<<-NULL
    }
    # Return the original matrix
    get<-function() x
    
    # Set the calculated inversion 
    setInversion<-function (solve1) {
      invers<<-solve1
    }
    
    # Return inversion 
    getInversion<-function(){
         invers
    }    
    # Return created oblect
    list(set = set, get = get, 
         setInversion=setInversion, getInversion = getInversion)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #Get the current value of the inversion of the input variable
    invers<-x$getInversion()
    
    #Check the value of the inversion, if it is not NULL and it is not necessary to calculate it, just return...
    if (!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    # ...but if the inversion is NULL, it means that we need to calculate it
     
    data<-x$get()
    # Inversion itself
    invers<-solve(data,...)
    # Set inversion at the input object x, during the second call of this object will return as the cache
    x$setInversion(invers)
    
    ## Return a matrix that is the inverse of 'x'
    invers
}