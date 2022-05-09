distplot <- function (X, pca, obstag = rep(0,nrow(X)), alpha = 0.05, plottitle = "Distance plot\n"){
  # Calculate distances
  A <- length(pca$sdev)
  Xc <- sweep(sweep(X, 2, pca$center, FUN="-"),2, pca$scale, FUN="/")
  T2 <- rowSums(pca$x^2/matrix(pca$sdev^2, nrow=nrow(X), ncol=A))
  SPE <- rowSums((Xc - pca$x%*%t(pca$rotation))^2)
  # Assemble data set
  set <- replace(obstag,obstag==0,"Obs.ref")
  set <- replace(set,set!="Obs.ref","Obs.new")
  df.plot <- as.data.frame(cbind(T2, SPE, set))
  colnames(df.plot) <- c("T2", "SPE", "set")
  # Graphic fabrication
  dplot <- ggplot(data = df.plot, aes(x = T2, y = SPE)) + 
    geom_vline(xintercept = pca$limt2, colour = "red", linetype = "dashed", 
                        size = 0.75, show.legend = TRUE) + 
    geom_hline(yintercept = pca$limspe, colour = "red", 
                        linetype = "dashed", size = 0.75) + 
    geom_point(data = df.plot, mapping = aes(x = T2, y = SPE, colour = set, shape = set), 
                        size = 3, alpha = 0.5) + 
    scale_colour_manual(name = "",
                                 values = c(Obs.ref = "blue", Obs.new = "red"),
                                 breaks = c("Obs.ref", "Obs.new")) +
    scale_shape_manual(name = "", values = c(16, 17),
                                breaks = c("Obs.ref", "Obs.new")) +
    theme_bw() + 
    coord_cartesian(ylim = c(0, 1.1 * max(c(SPE, pca$limspe))), 
                             xlim = c(0, 1.1 * max(c(T2, pca$limt2)))) + 
    labs(x = bquote(italic(~T^2)), y = "SPE", title = plottitle, 
                  subtitle = bquote(UCL[.(paste0((1 - pca$alpha) * 100, "%"))])) + 
    guides(colour = guide_legend("", override.aes = list(alpha = 1)), 
                    shape = guide_legend("")) + 
    theme(legend.position = "bottom", 
                   plot.title = element_text(hjust = 0.5, size = 14), 
                   axis.title.x = element_text(face = "italic", size = 12), 
                   axis.title.y = element_text(face = "italic", size = 12), 
                   legend.direction = "vertical", legend.text = element_text(size = 10), 
                   plot.subtitle = element_text(size = 10))
  return(dplot)
}
