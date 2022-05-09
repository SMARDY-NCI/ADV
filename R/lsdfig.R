lsdfig <- function(data,vy.name,vx.name,yw,col=rgb(0,1,0,0.5),ytext="",xtext="",tittext=""){

  df.lines <- aggregate(data[,vy.name] ~ Method + data[,vx.name], data=data, mean)
    colnames(df.lines) <- c("Method", vx.name, vy.name)
    df.lines$yup <- df.lines[,vy.name] + yw
    df.lines$ylo <- df.lines[,vy.name] - yw
    df.lines$Method <- as.factor(df.lines$Method)
    df.lines %>% 
      ggplot(aes(x = data[,vx.name], group = Method))+
      geom_line(aes(y = (data[,vy.name]), color = Method))+
      geom_ribbon(aes(ymin = (ylo),ymax = (yup), fill=Method), alpha=0.5)+
      theme_minimal() + ylab(ytext) + xlab(xtext) + ggtitle(tittext)
  
  
}