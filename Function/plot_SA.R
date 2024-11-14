plot_SA <- function(  figurePath,
                      prefix,
                      SA_min,
                      SA_hour){

  # plotting
  png(file.path(figurePath,paste0(prefix,'_SA_timeSeries.png')), 
      width = 12*1, height = 12*1, units = "cm", res = 300, pointsize = 10)
  
  df.plot <- subset(SA_hour,depth != 0)
  sts <- boxplot.stats(df.plot$SA)$stats
  
  p <- ggplot(df.plot, aes(x=time_hour, y=log10(SA),col=depth))+
        theme_classic()+
        geom_point()+
        facet_grid(freq~stationName)
  
  print(p)
  dev.off()
  
  png(file.path(figurePath,paste0(prefix,'_SA_hours_boxplot.png')),
      width = 12*1, height = 12*1, units = "cm", res = 300, pointsize = 10)
  
  df.plot <- subset(SA_min,depth == 0 & SA != 0)
  sts <- boxplot.stats(df.plot$SA)$stats

  p <- ggplot(df.plot, aes(x=as.factor(hour), y=SA,fill=as.factor(hour))) + 
        theme_classic()+
        geom_boxplot(outlier.colour=NA)+
        coord_cartesian(ylim = c(sts[2]/2,max(sts)*1.05))+
        theme(axis.text.x = element_text(angle = 90),legend.position = "none")+
        labs(x='Hour of day',y='SA')+
        facet_grid(freq~stationName)
  
  print(p)
  dev.off()

  png(file.path(figurePath,paste0(prefix,'_SA_depth_70khz.png')), 
      width = 12*1, height = 12*1, units = "cm", res = 300, pointsize = 10)

  p <-   ggplot(subset(SA_hour,depth != 0 & freq==70), aes(x=time_hour, y=depth, fill=log10(SA))) + 
          geom_tile()+
          scale_fill_viridis()+
          facet_wrap(~stationName)+
          labs(x='Time',y='range')
  
  print(p)
  dev.off()
  
  if(any(unique(SA_hour$freq) %in% '200')){
    png(file.path(figurePath,paste0(prefix,'_SA_depth_200khz.png')), 
        width = 12*1, height = 12*1, units = "cm", res = 300, pointsize = 10)
    
    p <-   ggplot(subset(SA_hour,depth != 0 & freq==200), aes(x=time_hour, y=depth, fill=log10(SA))) + 
            geom_tile()+
            scale_fill_viridis()+
            facet_wrap(~stationName)+
            labs(x='Time',y='range')
    
    print(p)
    dev.off()
  }
}
