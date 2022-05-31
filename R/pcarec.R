pcarec <- function(X,m,s,P){
  Xcs <- sweep(sweep(X,2,m,FUN="-"),2,s,"/")
  # Xcs <- sweep(X,2,m,FUN="-")
  Tscores <- Xcs%*%P
  # Xrec <- sweep(Tscores%*%t(P), 2, m, "+")
  Xrec <- sweep(sweep(Tscores%*%t(P), 2, s, "*"), 2, m, "+")
  return(Xrec)
  }