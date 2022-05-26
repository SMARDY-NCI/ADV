lsdfig <- function(data,vy.name,vx.name,vg.name,yw,col=rgb(0,1,0,0.5),
                   graph.out = "errorbar",ytext="",xtext="",tittext=""){
  if (graph.out == "errorbar"){
    df.lines <- aggregate(data[,vy.name] ~ data[,vx.name], data=data, mean)
    colnames(df.lines) <- c(vx.name, vy.name)
    g.name <- names(data[,vg.name])
    g.levels <- levels(data[,vg.name])
    # colnames(df.lines) <- c(vx.name, vy.name)
    
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    
    lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name], group = vg.name)) +
      geom_errorbar(aes(ymin=ylo, ymax=yup, color=vg.name), width=.1) +
      theme_minimal(base_size = 12) + ylab(ytext) + 
      scale_fill_manual(g.name,values=g.levels) +
      xlab(xtext) + ggtitle(tittext)
    
  } else if(graph.out == "curve") {
    df.lines <- aggregate(data[,vy.name] ~ Method + data[,vx.name], data=data, mean)
    colnames(df.lines) <- c("Method",vx.name, vy.name)
    g.name <- names(data[,vg.name])
    g.levels <- levels(data[,vg.name])
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    
    lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name], group = vg.name)) +
      geom_line(aes(y = (df.lines[,vy.name]), color = vg.name)) +
      geom_ribbon(aes(ymin = (ylo),ymax = (yup), fill = vg.name), alpha=0.5) +
      theme_minimal(base_size = 12) + ylab(ytext) + 
      scale_fill_manual(g.name,values=g.levels) +
      xlab(xtext) + ggtitle(tittext)
    
  }
  
  # df.lines$Method <- as.factor(df.lines$Method)
  
  return(lsd_plot)
}