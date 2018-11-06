## Function to create a matrix with information about known latent
## state z 
known.state.cjs <- function(ch) {
  state <- ch
  for(i in 1:dim(ch)[1]){
    n1 <- min(which(ch[i,]==1))
    n2 <- max(which(ch[i,]==1))
    state[i,n1:n2] <- 1
    state[i,n1] <- NA
  }
  state[state==0] <- NA
  state
}

## Function to create a matrix of initial values for latent state z
cjs.init.z <- function(ch, f) {
  for(i in 1:dim(ch)[1]){
    if(sum(ch[i,])==1) next
    n2 <- max(which(ch[i,]==1))
    ch[i,f[i]:n2] <- NA
  }
  for (i in 1:dim(ch)[1]){
    ch[i,1:f[i]] <- NA
  }
  ch
}

## return sorted unique values
id <- function(x) unique(sort(as.vector(unlist(x))))

## standardize a vector
standardize <- function(x)
  (x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)

## expit and logit functions
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

## function to clean up white-space in a column of data (replaces all
## instances of white-space with " " and empty cells with ""
fix.white.space <- function(d) {
  d <- as.character(d)

  ## returns string w/o leading whitespace
  trim.leading <- function(x)  sub('^\\s+', '', x)
  ## returns string w/o trailing whitespace
  trim.trailing <- function(x) sub('\\s+$', '', x)
  ## trim middle white-space
  trim.middle <- function(x) gsub('\\s+', ' ', x)
  ## returns string w/o leading or trailing whitespace
  trim <- function (x)gsub('^\\s+|\\s+$', '', x)

  d <- trim(d)
  d <- trim.middle(d)
  d
}

## load and return loaded object
load.local <- function(file) {
 v <- load(file)
 stopifnot(length(v) == 1)
 get(v)
}

