if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, progress, icesTAF, timetk)

rm(list=ls())

#setwd('D:/PAM_RABO/finless_neonate_HK')
# setwd('G:/git/APELAFICO_OWF_study/')
#setwd('G:/git/WBAT_APELAFICO')
getwd()

sourceDir(file.path('.','function'))

figurePath    <- file.path('.','figures')
dataPath      <- file.path('.','data')
resultPath    <- file.path('.','results')

WBAT.tab <- file.path(dataPath, 'survey_db.csv') %>% 
  read_csv() %>%
  select(1:5, 8, 9) %>%
  mutate(dataSet_station = str_c(dataSet, '_', station))

overview.tab <- file.path(dataPath, 'data_overview.csv') %>% 
  read_csv() %>% 
  mutate(stationSet = str_c(dataSet, '_', station),
         pairingName = str_c(dataSet, '_', pairing)) %>% 
  select(-c('year', 'dataSet', 'station'))

load(file = file.path(resultPath,'CPOD_WBAT_workspace.RData'))


# adjust CPOD data frames (should be done at data generation...)
CPOD.all.day <- CPOD.all.day %>%
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>%
  filter(pairing != -1)

CPOD.all.hour <- CPOD.all.hour %>%
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>%
  filter(pairing != -1)

CPOD.all.min <- CPOD.all.min %>%
  mutate(station = stationName,
         stationSet = str_c(dataSet, '_', station)) %>%
  mutate(stationSet = case_when(
    stationSet == "2023-BSW_274174" ~ "2023-BSW_274174 BSW1",
    stationSet == "2023-BSW_278093" ~ "2023-BSW_278093 BSW2",
    stationSet == "2023-BSW_267814" ~ "2023-BSW_267814 BSW3",
    stationSet == "2023-BSW_267838" ~ "2023-BSW_267838 BSW4",
    TRUE ~ stationSet)) %>%
  select(-lat, -lon) %>%
  left_join(overview.tab, by = "stationSet") %>%
  filter(pairing != -1)

WBAT.all.summary <- WBAT.all.summary %>% 
  group_by(frequency, station, phase, dataSet) %>% 
  mutate(depthMaxR = depthIntegration / max(depthIntegration))

# temp <- subset(WBAT.all.summary,dataSet == "2021-BE" & station == 'belwind' & frequency == 70 & treshold == -50 & n >= 8)
# 
# ggplot(temp,aes(x=datetime,y=depthMaxR))+
#   geom_line()+
#   geom_hline(yintercept = 1)

WBAT.pairwise <- subset(WBAT.all.summary, pairing != -1 & n >= 8 & depthMaxR > 0.25 & treshold == -50)#& frequency == 70 
WBAT.pairwise <- subset(WBAT.pairwise, !(pairing == 1 &  dataSet == "2021-BE" & datetime < as.POSIXct("2021-08-17 00:00:00 UTC",tz='UTC')))
WBAT.pairwise$type <- factor(WBAT.pairwise$type,levels = c("OWF","control"))
WBAT.pairwise$time_hour <- as.POSIXct(cut(WBAT.pairwise$datetime, breaks = "1 hour"),tz='UTC')

# CPOD.all.min$type <- factor(CPOD.all.min$type,levels = c("OWF","out","wreck"))
# CPOD.all.hour$type <- factor(CPOD.all.hour$type,levels = c("OWF","out","wreck"))
# CPOD.all.day$type <- factor(CPOD.all.day$type,levels = c("OWF","out","wreck"))

CPOD.all.min$type <- CPOD.all.min$type.y
CPOD.all.hour$type <- CPOD.all.hour$type.y
CPOD.all.day$type <- CPOD.all.day$type.y

CPOD.all.hour$time_hour <- as.POSIXct(cut(CPOD.all.hour$time_hour, breaks = "1 hour"),tz='UTC')

library(RColorBrewer)

############################################################################
# pairwise comparison
############################################################################
#unique(WBAT.pairwise$type)

# by type and pairing
taf.png(file.path(figurePath,paste0("WBAT_pairing_70khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 70 & treshold == -50),aes(x=as.factor(pairing),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('Pairing')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        facet_wrap(dayNight~dataSet)+
        ggtitle('SA - pairs per data set and day/night - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_pairing_200khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 200 & treshold == -50),aes(x=as.factor(pairing),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('Pairing')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        facet_wrap(dayNight~dataSet)+
        ggtitle('SA - pairs per data set and day/night - 200 kHz'))
dev.off()

# by type
taf.png(file.path(figurePath,paste0("WBAT_OWF vs out per data set_70khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 70 & treshold == -50),aes(x=as.factor(type),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('Site')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        facet_wrap(dayNight~dataSet)+
        ggtitle('SA - OWF vs out per data set and day/night - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_OWF vs out per data set_200khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 200 & treshold == -50),aes(x=as.factor(type),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('Site')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        facet_wrap(dayNight~dataSet)+
        ggtitle('SA - OWF vs out per data set and day/night - 200 kHz'))
dev.off()

# all
taf.png(file.path(figurePath,paste0("WBAT_all_OWF vs out all_dayNight_70khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 70 & treshold == -50),aes(x=as.factor(dayNight),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA - OWF vs out for day/night - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_all_OWF vs out all_70khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 70 & treshold == -50),aes(x=as.factor(type),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA - OWF vs out for day/night - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_all_OWF vs out all_dayNight_200khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 200 & treshold == -50),aes(x=as.factor(dayNight),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA - OWF vs out for day/night - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_all_OWF vs out all_200khz.png")))
print(ggplot(subset(WBAT.pairwise,frequency == 200 & treshold == -50),aes(x=as.factor(type),y=log10(SA),fill=as.factor(type)))+
        geom_boxplot(position="dodge",width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        scale_fill_brewer(palette="Dark2")+
        #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle('SA - OWF vs out for day/night - 200 kHz'))
dev.off()

# for(idxDataSet in unique(WBAT.pairwise$dataSet)){
#   # WBAT
#   df.plot <- subset(WBAT.pairwise,dataSet == idxDataSet & treshold == -50)
#   
#   ggplot(subset(df.plot,pairing == 1 & frequency == 70),aes(x=datetime,y=log10(SA),col=as.factor(station)))+
#     geom_line()+
#     facet_wrap(~dayNight)
#   
#   ggplot(subset(df.plot,pairing == 1 & frequency == 70),aes(x=datetime,y=depthMaxR,col=as.factor(station)))+
#     geom_line()+
#     facet_wrap(~dayNight)
#   
#   print(ggplot(subset(df.plot,frequency == 70),aes(as.factor(pairing),log10(SA),fill=as.factor(type)))+
#           geom_boxplot(position="dodge")+
#           theme_bw()+
#           ylim(0.5,2.5)+
#           xlab('Pairing')+
#           theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#           ggtitle(paste0(idxDataSet)))
#   
#   # CPOD
#   df.plot <- subset(CPOD.all.hour,dataSet == idxDataSet)
#   
#   ggplot(df.plot,aes(time_hour,pos_minutes,col=type))+
#     geom_line()
#   
#   print(ggplot(df.plot,aes(as.factor(pairing),pos_minutes,fill=as.factor(type)))+
#           geom_boxplot(position="dodge")+
#           theme_bw()+
#           ylim(0,20)+
#           xlab('Pairing')+
#           theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#           ggtitle(paste0(idxDataSet)))
#   
#   
#   for(idxPairing in unique(WBAT.pairwise$pairing)){
#     df.plot <- subset(WBAT.pairwise,dataSet == idxDataSet & pairing == idxPairing)
#     df.plot <- df.plot[order(df.plot$type),]
#     
#     print(df.plot %>%
#             group_by(type) %>%
#             plot_seasonal_diagnostics(datetime,
#                                       log10(SA),
#                                       .interactive = F,
#                                       .feature_set=c('year','hour','week'))+
#             theme_bw()+
#             ylim(0.5,2.5)+
#             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#             ggtitle(paste0('WBAT-',idxDataSet,' pairing-',idxPairing)))
#     
#     df.plot <- subset(CPOD.all.hour,dataSet == idxDataSet & pairing == idxPairing)
#     df.plot <- df.plot[order(df.plot$type),]
#     
#     print(df.plot %>%
#             group_by(type) %>%
#             plot_seasonal_diagnostics(time_hour,
#                                       pos_minutes,
#                                       .interactive = F,
#                                       .feature_set=c('year','hour','week'))+
#             theme_bw()+
#             ylim(0,10)+
#             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#             ggtitle(paste0('CPOD-',idxDataSet,' pairing-',idxPairing)))
#   }
# }

############################################################################
# CPOD/SA correlation
############################################################################
df.all <- left_join(WBAT.pairwise,CPOD.all.hour,by=c('stationSet',
                                                     'type',
                                                     'dataSet',
                                                     'pairing',
                                                     'time_hour',
                                                     'dayNight'))



df.all$HP.bool <- 1
df.all$HP.bool[is.na(df.all$all)] <- 0

df.all$HP <- 'HP present'
df.all$HP[is.na(df.all$all)] <- 'No HP'

h1 = hist(df.all$pos_minutes)

df.all <- df.all %>% mutate(bin = cut(pos_minutes, breaks = h1$breaks, labels = h1$mids),
                            bin_inter = cut(pos_minutes, breaks = h1$breaks))


CPOD.all.hour$time_day <- as.POSIXct(cut(CPOD.all.hour$time_hour, breaks = "1 day"),tz='UTC')

CPOD.temp <- CPOD.all.hour %>% 
  group_by(time_day,stationName,dataSet,type,pairing) %>% 
  summarize(pres.ratio=length(which(is.na(all)))/n())

# ggplot(CPOD.all.day,aes(x=as.factor(type),y=pos_minutes))+
#   geom_boxplot()

taf.png(file.path(figurePath,paste0("CPOD_HP presence_pairs.png")))
print(ggplot(CPOD.temp,aes(x=as.factor(pairing),y=(1-pres.ratio)*100,fill=as.factor(type)))+
        theme_bw()+
  geom_boxplot(position='dodge',width=0.5)+
    xlab('')+
    ylab('Daily presence (%)')+
    guides(fill=guide_legend(title=""))+
    scale_fill_brewer(palette="Dark2")+
    facet_wrap(~dataSet))
dev.off()

taf.png(file.path(figurePath,paste0("CPOD_HP PPM_pairs.png")))
print(ggplot(CPOD.all.hour,aes(x=as.factor(pairing),y=pos_minutes,fill=as.factor(type)))+
        theme_bw()+
        geom_boxplot(position='dodge',width=0.5)+
        xlab('')+
        #ylim(0,10)+
        ylab('Hour positive minutes')+
        scale_fill_brewer(palette="Dark2")+
        guides(fill=guide_legend(title=""))+
        facet_wrap(~dataSet))
dev.off()

t <- subset(CPOD.all.hour,dataSet == "2023-BE")

taf.png(file.path(figurePath,paste0("CPOD_HP presence_all.png")))
print(ggplot(CPOD.temp,aes(x=as.factor(type),y=(1-pres.ratio)*100,fill=as.factor(type)))+
        theme_bw()+
        geom_boxplot(position='dodge',width=0.5)+
        xlab('')+
        ylab('Daily presence (%)')+
        scale_fill_brewer(palette="Dark2")+
        guides(fill=guide_legend(title="")))
dev.off()

taf.png(file.path(figurePath,paste0("CPOD_HP PPM_all.png")))
print(ggplot(CPOD.all.hour,aes(x=as.factor(type),y=pos_minutes,fill=as.factor(type)))+
        theme_bw()+
        geom_boxplot(position='dodge',width=0.5)+
        xlab('')+
        ylim(0,10)+
        ylab('Hour positive minutes')+
        scale_fill_brewer(palette="Dark2")+
        guides(fill=guide_legend(title="")))
dev.off()


taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_stats_70khz.png")))
print(ggplot(subset(df.all,frequency == 70 & HP.bool == 1),aes(x=as.numeric(bin),y=log10(SA),col=type))+
        geom_jitter(alpha=0.1,col='black')+
        stat_summary(fun.data=mean_se, geom="pointrange", na.rm = TRUE)+
        theme_bw()+
        ylim(0.5,3)+
        xlab('HP positive minute per hour')+
        facet_wrap(~dayNight)+
        scale_color_brewer(palette="Dark2")+
        theme(axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_stats_200khz.png")))
print(ggplot(subset(df.all,frequency == 200 & HP.bool == 1),aes(x=as.numeric(bin),y=log10(SA),col=type))+
        geom_jitter(alpha=0.1,col='black')+
        stat_summary(fun.data=mean_se, geom="pointrange", na.rm = TRUE)+
        theme_bw()+
        ylim(0.5,3)+
        scale_color_brewer(palette="Dark2")+
        xlab('HP positive minute per hour')+
        facet_wrap(~dayNight)+
        theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_boxplot_200khz.png")))
print(ggplot(subset(df.all,frequency == 200 & HP.bool == 1),aes(x=bin_inter,y=log10(SA),fill=type))+
        geom_jitter(alpha=0.1,col='black')+
        geom_boxplot(position="dodge",alpha=0.5,width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        scale_fill_brewer(palette="Dark2")+
        xlab('HP positive minute per hour')+
        facet_wrap(~dayNight)+
        theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_boxplot_70khz.png")))
print(ggplot(subset(df.all,frequency == 70 & HP.bool == 1),aes(x=bin_inter,y=log10(SA),fill=type))+
        geom_jitter(alpha=0.1,col='black')+
        geom_boxplot(position="dodge",alpha=0.5,width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        scale_fill_brewer(palette="Dark2")+
        xlab('HP positive minute per hour')+
        facet_wrap(~dayNight)+
        theme(axis.text.x = element_text(
            angle = 90,
            hjust = 1,
            vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_boxplot_70khz_all.png")))
print(ggplot(subset(df.all,frequency == 70 & HP.bool == 1),aes(x=bin_inter,y=log10(SA),fill=type))+
        geom_jitter(alpha=0.1,col='black')+
        geom_boxplot(position="dodge",alpha=0.5,width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        scale_fill_brewer(palette="Dark2")+
        xlab('HP positive minute per hour')+
        theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP pos minutes_boxplot_200khz_all.png")))
print(ggplot(subset(df.all,frequency == 200 & HP.bool == 1),aes(x=bin_inter,y=log10(SA),fill=type))+
        geom_jitter(alpha=0.1,col='black')+
        geom_boxplot(position="dodge",alpha=0.5,width=0.5)+
        theme_bw()+
        ylim(0.5,3)+
        scale_fill_brewer(palette="Dark2")+
        xlab('HP positive minute per hour')+
        theme(axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5))+
        ggtitle('Hourly HP positive minutes vs WBAT - 200 kHz'))
dev.off()


taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_70khz_all.png")))
print(ggplot(subset(df.all,frequency == 70),aes(x=as.factor(type),y=log10(SA),fill=as.factor(HP)))+
  theme_bw()+
  geom_boxplot(position="dodge",width=0.5)+
  ylim(0.5,3)+
  scale_fill_brewer(palette="Dark2")+
  xlab('')+
  ggtitle('Hourly HP presence vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_70khz_all.png")))
print(ggplot(subset(df.all,frequency == 70),aes(x=as.factor(HP),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        scale_fill_brewer(palette="PiYG")+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_70khz_all_OWF out.png")))
print(ggplot(subset(df.all,frequency == 70),aes(x=as.factor(type),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        scale_fill_brewer(palette="PiYG")+
        xlab('')+
        guides(fill=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 70 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_200khz_all.png")))
print(ggplot(subset(df.all,frequency == 200),aes(x=as.factor(HP),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        xlab('')+
        scale_fill_brewer(palette="PiYG")+
        guides(fill=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_200khz_all_OWF out.png")))
print(ggplot(subset(df.all,frequency == 200),aes(x=as.factor(type),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        xlab('')+
        scale_fill_brewer(palette="PiYG")+
        guides(fill=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_200khz.png")))
print(ggplot(subset(df.all,frequency == 200),aes(x=as.factor(type),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        xlab('')+
        facet_wrap(dayNight~dataSet)+
        scale_fill_brewer(palette="PiYG")+
        guides(col=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 200 kHz'))
dev.off()

taf.png(file.path(figurePath,paste0("WBAT_HP presence_boxplot_70khz.png")))
print(ggplot(subset(df.all,frequency == 70),aes(x=as.factor(type),y=log10(SA),fill=as.factor(HP)))+
        theme_bw()+
        geom_boxplot(position="dodge",width=0.5)+
        ylim(0.5,3)+
        xlab('')+
        facet_wrap(dayNight~dataSet)+
        scale_fill_brewer(palette="PiYG")+
        guides(col=guide_legend(title=""))+
        ggtitle('Hourly HP presence vs WBAT - 70 kHz'))
dev.off()

############################################################################
# code dump
############################################################################

