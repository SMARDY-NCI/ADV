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
    if (grepl("p*.corr",vy.name)){
    	lsd_plot <- lsd_plot + expand_limits(y = c(-1, 1))
    } 
  } else if(graph.out == "curve") {
    df.lines <- aggregate(data[,vy.name] ~ Method + data[,vx.name], data=data, mean)
    colnames(df.lines) <- c("Method",vx.name, vy.name)
    # g.name <- names(df.lines[,vg.name])
    g.levels <- levels(data[,vg.name])
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    
    lsd_plot <- ggplot(data=df.lines, aes(x=df.lines[,vx.name], y=df.lines[,vy.name], 
    																			group = df.lines[,vg.name],
    																			fill = df.lines[,vg.name], 
    																			color = df.lines[,vg.name])) + 
    	geom_line() + 
    	geom_ribbon(aes(ymin=ylo, ymax=yup),alpha=0.5, linetype=0) + 
      theme_minimal(base_size = 12) + ylab(ytext) + 
      labs(fill = "Method") +
      xlab(xtext) + ggtitle(tittext)
   if (grepl("p*.corr",vy.name)){
   	lsd_plot <- lsd_plot + expand_limits(y = c(-1, 1)) + 
   		ylab(bquote("p"[.(as.numeric(gsub("\\D", "", vy.name)))]~"corr"))
   } 
  }
  
  # df.lines$Method <- as.factor(df.lines$Method)
  
  return(lsd_plot)
}