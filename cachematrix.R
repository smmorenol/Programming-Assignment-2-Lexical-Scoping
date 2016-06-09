
##Funcion inicial que describe los elementos que se usaran para calcular la inversa
##de una matriz

##Initial function to describe the elements that i will use for calculate the matrix inverse

makeCacheMatrix<-function(x = matrix()) {
    inv<-NULL
    set<-function(y) {
      x<<-y
      inv<<-NULL
    }
    get<-function() x
    setinverse<-function(inverse) 
    inv<<-inverse
    getinverse<-function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##La siguiente funcion confirma que se haya computado la inversa, 
##en caso contrario se muestra el valor guardado en cache

##The following function confirms that the inverse procedure is complete
##otherwise the stored value in cache is displayed

# This function assumes that the matrix is always invertible.
cacheSolve<-function(x, ...) {
    inv<-x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
}

x<-rbind(c(1, 0.2), c(0.5, 1))
matrix<-makeCacheMatrix(x)
matrix$get()
cacheSolve(matrix)
matrix$getinverse()




