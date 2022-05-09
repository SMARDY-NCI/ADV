lsdfig <- function(data,yw,col=rgb(0,1,0,0.5),ytext="",xtext="",tittext=""){

    df.lines <- aggregate(MSE ~ Method + NLVs, data = data, mean)
    df.lines$yup <- df.lines$MSE + yw
    df.lines$ylo <- df.lines$MSE - yw
    df.lines %>% 
      ggplot(aes(x = NLVs, group = Method))+
      geom_line(aes(y = (MSE), color = Method))+
      geom_ribbon(aes(ymin = (ylo),ymax = (yup), fill=Method), alpha=0.5)+
      theme_minimal() + ylab(ytext) + xlab(xtext) + ggtitle(tittext)
  
  
}