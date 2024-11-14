plot_schoolParameters <- function(figurePath,
                                  prefix,
                                  school_posDay,
                                  school_posHours,
                                  school_parameters){
  # plotting
  scalor <- 1.5
  png(file.path(figurePath,paste0(prefix,'_fish_count_timeSeries.png')), 
      width = 12*scalor, height = 8*scalor, units = "cm", res = 300, pointsize = 10)
  
  p <- ggplot(data=school_posDay) +
    theme_classic()+
    geom_bar(aes(x=time_day, y=pos_hours,fill=as.factor(freq)),stat="identity",position = "dodge")+
    labs(x='Day',y='Fish positive hours')+
    facet_wrap(~stationName)
  
  print(p)
  dev.off()
  
  scalor <- 1.5
  png(file.path(figurePath,paste0(prefix,'_fish_count_hours.png')),
      width = 12*scalor, height = 12*scalor, units = "cm", res = 300, pointsize = 10)
  
  p <- ggplot(data=school_posHours) +
    theme_classic()+
    geom_tile(aes(x=time_day, y=hour, fill=count),size=0.1)+
    geom_line(aes(x=time_day,y=hourSunset),col='red')+
    geom_line(aes(x=time_day,y=hourSunrise),col='red')+
    scale_fill_viridis_c()+
    labs(x='Day',y='Hour of day',fill='Number of schools')+
    theme(axis.text.x = element_text(angle=90))+
    facet_grid(freq~stationName)
  
  print(p)
  dev.off()
  
  scalor <- 1.5
  png(file.path(figurePath,paste0(prefix,'_fish_count_hours_boxplot.png')), 
      width = 12*scalor, height = 8*scalor, units = "cm", res = 300, pointsize = 10)
  
  p <- ggplot(school_posHours[school_posHours$count !=0,], aes(x=as.factor(hour), y=count,fill=as.factor(stationName))) + 
    theme_classic()+
    geom_boxplot(position = "dodge")+
    #geom_jitter(width = 0.1,alpha=0.5)+
    theme(axis.text.x = element_text(angle = 90))+
    labs(x='Hour of day',y='Number of schools')+
    ylim(0,20)+
    facet_wrap(~freq)#,scale='free'
  
  print(p)
  dev.off()
  
  scalor <- 1.5
  png(file.path(figurePath,paste0(prefix,'_fish_sv_hours.png')), 
      width = 12*scalor, height = 8*scalor, units = "cm", res = 300, pointsize = 10)
  
  p <- ggplot(school_parameters, aes(x=as.factor(hour), y=10*log10(svMeanTruncated25),fill=as.factor(stationName))) + 
    theme_classic()+
    geom_boxplot(position = "dodge")+
    #geom_jitter(width = 0.1,alpha=0.5)+
    theme(axis.text.x = element_text(angle = 90))+
    labs(x='Hour of day',y='Mean Sv (dB)')+
    ylim(-70,-40)+
    facet_wrap(~freq,scale='free')
  
  print(p)
  dev.off()
  
  scalor <- 1.5
  png(file.path(figurePath,paste0(prefix,'fish_depth_hours.png')), 
      width = 12*scalor, height = 8*scalor, units = "cm", res = 300, pointsize = 10)
  
  p <- ggplot(school_parameters, aes(x=as.factor(hour), y=depth,fill=as.factor(stationName))) + 
    theme_classic()+
    geom_boxplot()+
    #geom_jitter(width = 0.1,alpha=0.5)+
    theme(axis.text.x = element_text(angle = 90),legend.position = "none")+
    labs(x='Hour of day',y='Depth (m)')+
    facet_wrap(~freq,scale='free')
  
  print(p)
  dev.off()
}
