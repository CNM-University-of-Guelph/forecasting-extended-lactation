# Lactation curve fitting equations
# David Innes


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
dijkstra_time_PY_eq <- function(b, c, b0){
  Tp = log(b/c)/b0
  return(Tp)
}


# Wood Lactation model:
# Eq 1 from Wood 1967
wood_eq <- function(a,b,c,t) {
  M = a * t^b * exp(-c*t)
  return(M)
}


# Time to peak yield
wood_time_PY_eq <- function(b,c){
  Tp = b/c
  return(Tp)
}

# Wilmink:
# Equation 11 in wilmink thesis
# a + be-0.05t + ct
# Updated so that 0.05 is b0 parameter and + are now -
wilmink_eq <- function(a,b,b0,c,t) {
  M = a - (b * exp(-b0*t)) - c*t
  return(M)
}

# Time to peak yield
#updated to ln(b*b0/c)/b0

wilmink_time_PY_eq <- function(b,b0,c){
  Tp = log(b*b0/c) / b0
  return(Tp)
}


