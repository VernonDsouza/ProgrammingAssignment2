# R function to cache inverse of a Matrix

#This function accepts a matrix and creates inverse of it.

#Part of assignment for programming week 3 by Johns Hopkins

makeCacheMatrix <- function(x = matrix()) {
        makeCacheMatrix <- function(x= matrix()){
                inv <- NULL #hold value of inv matrix
                
                set<- function(y){ # assign valueof matrix to parent environment
                        x<<-y
                        inv<<- NULL #inv to NULL if new matrix
                }
                
                get<- function() x # returns matrix argument
                
                setInverse <- function(inverse) inv <<- inverse #inv is sent to parent environment
                
                getInverse<- function() inv #get inv value
                
                list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
        }
        
        # Returns inverse of a matrix
        cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
                
                inv <- x$getInverse()
                if(!is.null(inv)) {
                        message("caching!!....")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
                inv
        }
}
