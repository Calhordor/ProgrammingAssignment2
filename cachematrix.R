 makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL
      }

      get <- function() x 
      setInv <- function(inv) xinv <<- inv 
      getInv <- function() xinv 
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }


  cacheSolve <- function(x, ...) {
      m <- x$getInv()
      if(!is.null(m)) { 
	  message("getting cached data")
	  return(m) 
      }
      data <- x$get() 
      m <- solve(data) 
      x$setInv(m)
      m 
  }
  test <- matrix(runif(9,1,100),3,3)
  testCached <- makeCacheMatrix(test)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)
  
  #sample output
  > x = rbind(c(1992,420),c(420,1992))
  > 
  > m=makeCacheMatrix(x)
  > m$get()
       [,1] [,2]
  [1,] 1992  420
  [2,]  420 1992
  > cacheSolve(m)
                [,1]          [,2]
  [1,]  0.0005253630 -0.0001107693
  [2,] -0.0001107693  0.0005253630
