lsdfig <- function(data,vy.name,vx.name,yw,col=rgb(0,1,0,0.5),ytext="",xtext="",tittext=""){
  if (vx.name == "Method"){
    df.lines <- aggregate(data[,vy.name] ~ data[,vx.name], data=data, mean)
    colnames(df.lines) <- c("Method", vy.name)
    # colnames(df.lines) <- c(vx.name, vy.name)
    
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    
    lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name], group = Method))+
      geom_errorbar(aes(ymin=ylo, ymax=yup, color=Method), width=.1) +
      theme_minimal(base_size = 12) + ylab(ytext) + 
      xlab(xtext) + ggtitle(tittext)
    
  } else {
    df.lines <- aggregate(data[,vy.name] ~ Method + data[,vx.name], data=data, mean)
    colnames(df.lines) <- c("Method",vx.name, vy.name)
    
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    
    lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name], group = Method))+
      geom_line(aes(y = (df.lines[,vy.name]), color = Method))+
      geom_ribbon(aes(ymin = (ylo),ymax = (yup), fill=Method), alpha=0.5)+
      theme_minimal(base_size = 12) + ylab(ytext) + 
      xlab(xtext) + ggtitle(tittext)
    
  }
  
  # df.lines$Method <- as.factor(df.lines$Method)
  
  return(lsd_plot)
}