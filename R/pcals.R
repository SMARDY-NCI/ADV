pcals <- function(X, A=0, alpha = 0.05){
  
  mX <- colMeans(X)
  sX <- apply(X,2,sd)
  n <- nrow(X)
  p <- ncol(X)
  Xc <- sweep(sweep(X, 2, mX, FUN="-"),2,sX,FUN="/")
  if(A==0){
    if (n<p){
      svd.out <-  svd(t(Xc), nu = p, nv = n)
    } else {
      svd.out <-  svd(Xc, nu = n, nv = p)
    }
    eig.vec <- data.frame(1:length(svd.out$d), svd.out$d, 
                          svd.out$d/sum(svd.out$d)*100, 
                          cumsum(svd.out$d/sum(svd.out$d)*100))
    names(eig.vec) <- c("PC", "Eig", "VarExp", "CumVar")
    ggplot(eig.vec, aes(x = PC)) + 
      geom_col(aes(y = VarExp)) + geom_line(aes(y = CumVar)) +
      ylim(0,100) + ylab("Variance (%)") + xlab("PCs")
    print(t(eig.vec[,c("PC", "CumVar")]))
    A <- as.integer(readline(prompt="Enter number of PCs: "))
  }
  if (n<p){svd.out <-  svd(t(Xc), nu = A, nv = A)
  P <- svd.out$u
  D <- diag(svd.out$d[1:A], nrow = A, ncol = A)
  U <- svd.out$v} else {
    svd.out <-  svd(Xc, nu = A, nv = A)
    P <- svd.out$v
    D <- diag(svd.out$d[1:A], nrow = A, ncol = A)
    U <- svd.out$u
  }
  Tscores <- U%*%D
  Tscores <- Tscores[,1:A,drop = F]
  P <- P[,1:A,drop=F]
  Xrec <- Tscores%*%t(P)
  Xrec <- sweep(sweep(Xrec,2,sX,FUN="*"), 2, mX, FUN="+")
  E <- as.matrix(X) - Xrec
  # Control limits
  ev <- eigen(stats::cov(E))
  LambdaE <- rev(ev$values)
  # SPE
  theta1 <- sum(LambdaE[-(1:A)])
  theta2 <- sum(LambdaE[-(1:A)]^2)
  theta3 <- sum(LambdaE[-(1:A)]^3)
  h0 <- 1 - 2 * theta1 * theta3/(3 * theta2^2)
  z_alpha <- stats::qnorm(1 - alpha)
  spe1 <- z_alpha * sqrt(2 * theta2 * h0^2)/theta1
  spe2 <- theta2 * h0 * (h0 - 1)/theta1^2
  cl_spe <- theta1 * (spe1 + 1 + spe2)^(1/h0)
  # T^2
  F_alpha <- stats::qf(1 - alpha, A, n - A)
  cl_t2 <- (n^2 - 1) * A/(n * (n - A)) * F_alpha
  # Scores
  z <- ((n - 1) * (n - 1)/n) * stats::qbeta(1 - alpha, 1, 
                                            (n - 3)/2)
  limits_t <- list()
  for (i in 1:A) {
    limits_t[[paste0("pc", i)]] = c(-sqrt(stats::var(Tscores[,i]) * z), 
                                    sqrt(stats::var(Tscores[, i]) * z))
  }
  
  c.lim = list(limspe = cl_spe,
               limt2 = cl_t2,
               limtscores = limits_t)

  return(list(sdev = apply(Tscores,2,sd), rotation = P, center = mX,
              scale = sX, x = Tscores, limspe = cl_spe, limt2 = cl_t2, 
              limits_t = limits_t, alpha = alpha))
}
