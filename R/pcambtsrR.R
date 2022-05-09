pcambtsrR <- function(X,A,xscale = FALSE, maxiter = 5000, tol = 0.005, alpha = 0.01){
  if (is.numeric(X)){
    X <- as.matrix(X)
  }
  Xini <- X
  n <- nrow(X)
  p <- ncol(X)
  mis <- is.na(X)
  
  # Preprocessing
  if (xscale){
    sX <- matrix(apply(X,2,sd,na.rm=TRUE),nrow=1,ncol=p)
    X <- sweep(X,2,sX,FUN="/")
  } else {
    sX <- matrix(1,nrow=1,ncol=p)
  }
  mis.ind <- which(mis,arr.ind = TRUE)
  r <- mis.ind[,1]
  c <- mis.ind[,2]
  X[mis] <- 0
  meanc <- colSums(X)/(n-colSums(mis))
  # Initial observed mean imputation
  X[mis] <- matrix(rep(meanc,each = n), nrow = n)[mis]
  
  # TSR loop
  mX <- colMeans(X)
  Xc <- sweep(X,2,mX,FUN="-")
  S <- cov(Xc)
  vx <- sum(apply(Xc,2,var))
  
  if (n<p){
    svd.0 <-  svd(t(Xc))
    if (A==0){A <- min(c(sum((cumsum((svd.0$d^2)/(n-1))/vx) < 0.95) + 1,10))}
    V <- svd.0$u[,1:A,drop=F]
  } else {
    svd.0 <-  svd(Xc)
    if (A==0){A <- min(c(sum((cumsum((svd.0$d^2)/(n-1))/vx) < 0.95) + 1,10))}
    V <- svd.0$v[,1:A,drop=F]
  }
  
  diff <- 100
  diffvec <- matrix(NA,maxiter,1)
  It <- 0
  while (It<maxiter & diff>tol){
    It <- It +1
    Xmis <- X[mis]
    Pold <- V
    for (i in c(1:n)){
      if (any(mis[i,])){
        L <- V[!mis[i,],1:min(c(A,sum(!mis[i,]))),drop=F]
        S11 <- S[!mis[i,], !mis[i,], drop=F]
        S21 <- S[mis[i,], !mis[i,], drop =F]
        z1 <- Xc[i,!mis[i,]]
        z2 <- S21%*%L%*%pracma::pinv(t(L)%*%S11%*%L)%*%t(L)%*%z1
        Xc[i,mis[i,]] <- z2
      }
    }
    X <- sweep(Xc,2,mX,FUN="+")
    mX <- colMeans(X)
    Xc <- sweep(X,2,mX,FUN="-")
    S <- cov(Xc)
    if (n<p){
      V <-  svd(t(Xc), nu=A)$u
    } else {
      V <-  svd((Xc),nv=A)$v
    }
    Pnew <- V
    diff <- acos(round(min(diag(t(Pnew)%*%Pold)^2), digits=4))
    diffvec[It] < diff
  }
  # Final PCA model
  X <- sweep(sweep(Xc,2,mX,"+"),2,sX,"*")
  m <- colMeans(X)
  if(xscale){
    sX <- matrix(apply(X,2,sd),nrow = 1,ncol = p)
  }
  S <- cov(Xc)
  tryCatch({
    svd.s <- svd(S,nv = A)}, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")
      svd.s <<- svd::propack.svd(S,neig = A)
      })
  Xcs <- sweep(sweep(X,2,m,FUN="-"),2,sX,"/")
  P <- svd.s$v[,1:A,drop=F]
  Tscores <- Xcs%*%P
  Xrec <- Tscores%*%t(P)
  E <- Xcs - Xrec
  Xrec <- sweep(sweep(Xrec,2,sX,"*"),2,mX,"+")
  if(xscale){
    pcaprep <- "autosc"
  } else {
    pcaprep <- "cent"
  }
  # Projection
  SPE <- rowSums(E^2)
  T2 <- rowSums(sweep(Tscores^2, 2, apply(Tscores,2,var),"/"))
  pcamodel <- list(P = P, m = m, s = apply(X,2,sd), lambda = apply(Tscores,2,var),
                   ncomp = A, prepro = pcaprep)
  return(list(X = X, m = m, S = S, It = It, diffvec = diffvec, Xrec = Xrec,
              pcamodel = pcamodel))
  
}