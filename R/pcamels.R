pcamels <- function(X,pcamodel){
  Xcs <- sweep(sweep(X,2,pcamodel$m,FUN="-"),2,pcamodel$s,"/")
  Tscores <- Xcs%*%pcamodel$P
  Xrec <- Tscores%*%t(pcamodel$P)
  E <- Xcs - Xrec
  Xrec <- sweep(sweep(Xrec,2,pcamodel$s,"*"),2,pcamodel$m,"+")
  SPE <- rowSums(E^2)
  T2 <- rowSums(sweep(Tscores^2, 2, pcamodel$lambdaT,"/"))
  return(list(Tscores = Tscores, Xrec = Xrec, E = E, SPE = SPE, T2 = T2))
}