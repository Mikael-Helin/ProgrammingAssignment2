## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# First we need to create matrix m as inverse to matrix x
# We initialize m to be empty
# We give matrix x a function as "copy contructor"
# We give matrix x functions to; get x, set m, get m.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # initialize m
  set<-function(y){ # copy constructor, set y as x and "forget" m
    x<<-y
    m<<-NULL
  }
  get<-function() x # get x
  setinverse<-function(solved) m<<-solved # set m
  getinverse<-function() m # get m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) # print functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m<-x$getinverse() # get cached m from x
  if(!is.null(m)){ # if m is cahced return it
    message("getting cashed data")
    return(m)
  }
  data<-x$get() # m was not cached so copy x into data
  m<-solve(data,...) # compute the inverse m to data
  x$setinverse(m) # save the inverse m
  m
}
