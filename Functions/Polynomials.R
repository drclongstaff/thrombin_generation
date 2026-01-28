#Collection of functions dealing with polynomial fitting and other manipulations

#Function to fit an N order polynomial
fun_POLYN <- function(X, Y){
  Fitpoly<-lm(Y~poly(X, 4, raw = TRUE)) #select the order of polynomial
}

#Function to zero columns
#Zerod_fun <- function(n){n-n[1]}

#Function to generate coefficients
fun_coeffs <- function(Ti, Fl){
  fitPoly <- fun_POLYN(Ti, Fl)
  fitPoly$coefficients
}

#Function to generate response from fitted coefficients
fun_eval <- function(x, A, B, C, D, E){
  g <- expression(A+B*x+C*x^2+D*x^3+E*x^4)
  genF <- eval(g)
}