

##This function takes the inverse of the matrix created by makeCacheMatrix.
##If the Inverse has been calculated with no changes, then, 
##it would retrieve from the inverted version.

makeCacheMatrix <- function(x = matrix()) {
          inver <- NULL
          setting <- function(y){
            x <<- y
            inver <<- NULL
          }
          getting <- function() x
          settinginverse <- function(inverse) inver <<- inverse 
          gettinginverse <- function() inver
          list(setting = setting,
               getting = getting,
               settinginverse = settinginverse,
               gettinginverse = gettinginverse)
}

cacheSolve <- function(x, ...) {
          inver <- x$gettinginverse()
          if(!is.null(inver)){
            message("getting cached data")
            return(inver)
          }
          matrx <- x$get()
          inver <- solve(matrx, ...)
          x$settinginverse(inver)
          inver
}
