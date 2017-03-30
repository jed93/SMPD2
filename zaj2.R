library("MCDA")
library("OutrankingTools")
library("topsis")
library("MCDM")

VIKOR <- function(decision, #matrix with all the alternatives
                  weights,  #vector with the numeric values of the weights
                  cb,       #vector with the "type" of the criteria (benefit = "max", cost = "min")
                  v         #value with the real number of the 'v' parameter to calculate Q
)
{
  #Checking parameters
  if(! is.matrix(decision))
    stop("'decision' must be a matrix with the values of the alternatives")
  if(missing(weights))
    stop("a vector containing n weigths, adding up to 1, should be provided")
  if(sum(weights) != 1)
    stop("The sum of 'weights' is not equal to 1")
  if(! is.character(cb))
    stop("'cb' must be a character vector with the type of the criteria")
  if(! all(cb == "max" | cb == "min"))
    stop("'cb' should contain only 'max' or 'min'")
  if(length(weights) != ncol(decision))
    stop("length of 'weights' does not match the number of the criteria")
  if(length(cb) != ncol(decision))
    stop("length of 'cb' does not match the number of the criteria")
  if(missing(v))
    stop("a value for 'v' in [0,1] should be provided")
  
  #1. Ideal solutions
  posI <- as.integer(cb == "max") * apply(decision, 2, max) +
    as.integer(cb == "min") * apply(decision, 2, min)
  negI <- as.integer(cb == "min") * apply(decision, 2, max) +
    as.integer(cb == "max") * apply(decision, 2, min)
  
  #2. S and R index
  norm =function(x,w,p,n){
    w*((p-x)/(p-n))
  }
  SAux <- apply(decision, 1, norm, weights, posI, negI)
  S <- apply(SAux, 2, sum)
  R <- apply(SAux, 2, max)
  
  
  #3. Q index
  #If v=0
  if (v==0)
    Q <- (R-min(R))/(max(R)-min(R))
  #If v=1
  else if (v==1)
    Q <- (S-min(S))/(max(S)-min(S))
  #Another case
  else
    Q <- v*(S-min(S))/(max(S)-min(S))+(1-v)*(R-min(R))/(max(R)-min(R))
  
  #4. Checking if Q is valid
  if( (Q == "NaN") || (Q == "Inf")){
    RankingQ <- rep("-",nrow(decision))
  }else{
    RankingQ <- rank(Q, ties.method= "first") 
  }
  #5. Ranking the alternatives
  return(data.frame(Alternatives = 1:nrow(decision), S = S, R = R, Q = Q, Ranking = RankingQ))

}
d <- matrix(c(30, 4.8, 249, 29, 4.1, 349, 24, 3.9, 319, 27, 4.4, 299, 26, 5.5, 319, 22, 3.9, 149, 7, 7.1, 499, 22, 3.7, 249, 42, 19.2, 2699, 10, 11.5, 419, 7, 2.4, 269, 41, 7.3, 489, 1, 2.9, 189, 20, 4.1, 369, 30, 4.7, 319, 32, 2.9, 399, 24, 5.5, 159, 24, 5, 199, 20, 4.3, 269, 4, 2.3, 159),nrow = 20,ncol = 3)
w <- c(0.3, 0.3, 0.4)
cb <- c('max','min','min')
v <- 0.5
VIKOR(d,w,cb,v)
