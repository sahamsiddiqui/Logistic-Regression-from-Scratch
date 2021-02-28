### Implemenrtaion of Logistic Regression  ###

#https://towardsdatascience.com/logistic-regression-from-scratch-in-r-b5b122fd8e83

####### 3 Helper function #######
# sigmoid function# core log function which returns probability value
my_sigmoid_function = function(z)
{
  1/1+exp(-z) # z = x . theta
}

# cost function 
my_cost_function = function(theta,X,y)
{
  m = length(y)
  h = my_sigmoid_function( X %.% theta)
  jcost = ( t(-y) %.% log(h) - t(1-y) %.% log(1-h) )/m
  jcost
}

# gradient functin= defines how to optimize the cost function
my_grad = function(theta, X, y)
{
  m = length(y)
  h = my_sigmoid_function( X %.% theta)
  grad = (t(X) %.% h - y )/m
  grad
}


## Log Regression TRAINING MODEL function

my_logregression = function(X,y)
{
  X = na.omit(X)
  y = na.omit(y)
  
  # add bias column to X matric (predictor matrix)
  X = mutate(X, bias =1)
  #bias col should be at first place
  
#X = X[ncol(X) , 1: ncol(X)-1]
  
  # subset    X= X[ allrows , c(1,2)]
  X = X[ , c(ncol(X), 1: ncol(X) -1) ]
  
  X = as.matrix(X)
  y = as.matrix(y)
  
  #initialize theta
  theta = matrix (rep(0,ncol(X)) ,nrow = ncol(X) )
  
  optimized_cost = optim( theta , fn = my_cost_function() , gr = my_grad(), X=X , y=y)
  return(optimized_cost$par)
}
  
# Now model is trained = we got theta (parameter)
# predict using theta(parameter) 

#prediction function

priction_prob_fuct = function(theta , X)
{
  X = na.omit(X)
  y = na.omit(y)
  
  X = mutate(X, bias =1)
  X = as.matrix(X[, c(ncol(X) , 1:ncol(X)-1)])
  prob = my_sigmoid_function( X %.% theta)
  return(prob)
}

predict_class = function(prob){
  return( round(prob , 0))
}
  

