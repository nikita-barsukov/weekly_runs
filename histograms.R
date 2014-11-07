library("ggplot2")
library("scales")

dist <- ggplot(ds, aes(x=distance)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 1.5) +
  geom_vline(aes(xintercept=median(distance)), color="red", linetype="dashed") +
  scale_x_continuous(name="Distance of a run, km") +
  scale_y_continuous(name="", labels=percent) +
  coord_cartesian(ylim=c(0,0.4))+
  annotate("text", 
           label = paste("Median: ", round(median(ds$distance), 2), "km"), 
           x = median(ds$distance) + 0.7,
           hjust=0,
           y = 0.2, 
           size=4) +  
  theme(
    plot.title = element_text(lineheight=1, face="bold",vjust=2),
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="#D8D8D8")    
  )   
print(dist)

sp <- ggplot(ds, aes(x=speed)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 1) +
  geom_vline(aes(xintercept=median(speed)), color="#A8EBA0", linetype="dashed") +
  scale_x_continuous(name="Running speed, km/h") +
  scale_y_continuous(name="", labels=percent, breaks = seq(0,0.4, by=0.1)) +
  coord_cartesian(ylim=c(0,0.4))+
  annotate("text", 
           label = paste("Median: ", round(median(ds$speed), 2), "km/h"), 
           x = median(ds$speed) + 0.7,
           hjust=0,
           y = 0.2,
           size=4) +  
  theme(
    plot.title = element_text(lineheight=1, face="bold",vjust=2),
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="#D8D8D8")    
  )   
print(sp)

date_fun <- function(secs, u="hours") {
  round(as.numeric(difftime(Sys.time() + secs, Sys.time(), units=u)))
}

dur <- ggplot(ds, aes(x=duration)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), binwidth = 1200) +
  geom_vline(aes(xintercept=median(duration)), color="#75DAE6", linetype="dashed") +
  coord_cartesian(xlim=c(0,18000), ylim=c(0,0.4)) +
  scale_x_continuous(name="Duration of a run, hours", labels=date_fun, breaks = seq(0,40000,by = 3600)) +
  scale_y_continuous(name="", labels=percent) +
  annotate("text", 
           label = paste("Median: ", date_fun(median(ds$duration), "min"), "min"), 
           x = median(ds$duration) + 600,
           hjust=0,
           y = 0.3,
           size=4) +  
  theme(
    plot.title = element_text(lineheight=1, face="bold",vjust=2),
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="#D8D8D8")    
  )   

print(dur)
