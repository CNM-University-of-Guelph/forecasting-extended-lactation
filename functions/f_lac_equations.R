# Lactation curve fitting equations
# David Innes

# Changed Wilmink to fit 4th parameter, instead of fixing at 0.05
# Modified Wilmink to swap signs of b and c to match interpretation with others

# Persistency equation
persistency_eq <- function(MY_tmax, t_max, PY, PT) {
  P = (MY_tmax - PY) / (t_max - PT)
  return(P)
}


# Dijkstra model:
# Mo = a = theoritical intitial milk production (kg/day)
# uT = b = specific rate of mammary cell profileration
# k =  b0 = decay parameter
# L =  c = specific rate of mammary cell death
# t = Time (DIM)


# Eq 11 from Dijkstra et al 1997
dijkstra_eq <- function(a,b,b0,c,t) {
  M = a*exp((b*(1-exp(-b0*t))/b0)-c*t)
  return(M)
}


# Time to peak yield
# ln(b/c)/b0
dijkstra_time_PY_eq <- function(b, c, b0){
  Tp = log(b/c)/b0
  return(Tp)
}


# Wood Lactation model:
# a, b and c = parameters of function
# t = Time (DIM)

# Eq 1 from Wood 1967
wood_eq <- function(a,b,c,t) {
  M = a * t^b * exp(-c*t)
  return(M)
}


# Time to peak yield
# b/c
wood_time_PY_eq <- function(b,c){
  Tp = b/c
  return(Tp)
}

# Wilmink:
# Equation 11 in wilmink thesis
# a + be-0.05t + ct
# Updated so that 0.05 is b0 parameter and + are now -

#OLD:
#wilmink_eq <- function(a,b,c,t) {
#   M = a + (b * exp(-0.05*t)) + c*t
#   return(M)
# }

#
wilmink_eq <- function(a,b,b0,c,t) {
  M = a - (b * exp(-b0*t)) - c*t
  return(M)
}

# Time to peak yield
# ln[c/(0.05b)]/-0.05
#updated to ln(b*b0/c)/b0

wilmink_time_PY_eq <- function(b,b0,c){
  Tp = log(b*b0/c) / b0
  return(Tp)
}


