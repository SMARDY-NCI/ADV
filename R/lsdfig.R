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
		if(is.null(vg.name)){
			lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name])) + 
				geom_errorbar(aes(ymin=ylo, ymax=yup), width=0.3, size = 1) +
				guides(linetype = "none", fill = "none") +
				theme_minimal(base_size = 12) + ylab(ytext) + 
				# scale_fill_manual(g.name,values=g.levels) +
				xlab(xtext) + ggtitle(tittext)
		} else {
			lsd_plot <- ggplot(df.lines, aes(x = df.lines[,vx.name], 
																			 group = df.lines[,vg.name],
																			 color = df.lines[,vg.name])) + 
				geom_errorbar(aes(ymin=ylo, ymax=yup), width=0.3, size = 1) +
				guides(color = "none", linetype = "none", fill = "none") +
				theme_minimal(base_size = 12) + ylab(ytext) + 
				scale_fill_manual(g.name,values=g.levels) +
				xlab(xtext) + ggtitle(tittext)
		}
		if (grepl("p*.corr",vy.name)){
			lsd_plot <- lsd_plot + coord_cartesian(ylim = c(0, 1.1)) + 
				scale_y_continuous(breaks=seq(0,1,.2)) + 
				ylab(bquote("s"[.(as.numeric(gsub("\\D", "", vy.name)))]))
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
			geom_line(aes(linetype = df.lines[,vg.name]), width = 2) + 
			geom_ribbon(aes(ymin=ylo, ymax=yup),alpha=0.5, linetype=0) + 
			theme_minimal(base_size = 12) + ylab(ytext) + 
			labs(fill = "Method", linetype = "Method", color = "Method") +
			# guides(color = FALSE, linetype = FALSE) + 
			xlab(xtext) + ggtitle(tittext)
		if (grepl("p*.corr",vy.name)){
			lsd_plot <- lsd_plot + coord_cartesian(ylim = c(0, 1.1)) + 
				scale_y_continuous(breaks=seq(0,1,.2)) + 
				ylab(bquote("s"[.(as.numeric(gsub("\\D", "", vy.name)))]))
		} 
	}
	
	# df.lines$Method <- as.factor(df.lines$Method)
	
	return(lsd_plot)
}