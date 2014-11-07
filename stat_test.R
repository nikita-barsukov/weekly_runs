source("patterns.R")
require("ggplot2")

stat_test <- by(cl, cl$user_id, function(x){
  speed <- tail(x[x$distance > 40,"speed"], n=1) 
  id <- as.character(x[1,"user_id"])
  x <- x[x$distance < 40,]
  x$week <- cut(x$date, breaks="weeks")
  avg_dist <- mean(tapply(x$distance, x$week, sum),na.rm=T)
  avg_duration <- mean(tapply(x$duration, x$week, sum), na.rm=T)
  avg_runs <- mean(table(x$week))
  return(c(id, speed, avg_dist,avg_duration, avg_runs))
})

stat_test <- do.call(rbind, stat_test)
stat_test <- as.data.frame(stat_test, stringsAsFactors = F)
colnames(stat_test) <- c('user_id','marathon_speed','distance','time','runs')
stat_test <- transform(stat_test, 
                       marathon_speed = as.numeric(marathon_speed),
                       distance = as.numeric(distance),
                       time = as.numeric(time),
                       runs = as.numeric(runs))
stat_test$group <- ">= 50 km/week"
stat_test[stat_test$distance < 50,"group"] <- "< 50 km/week"
stat_test$group <- as.factor(stat_test$group)

theme_nikita <- theme(
  plot.title = element_text(lineheight=1, face="bold",vjust=2),
  panel.border = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_line(color="gray"),
  axis.ticks = element_blank(),
  plot.background = element_rect(fill = "transparent",colour = NA),
  legend.position="none"
) 

#histograms
source('histograms.R')

#density plot
r <- ggplot(data=ds, aes(x=distance,y=speed)) + 
  stat_density2d(
    aes(fill=..level.. ),
    geom="polygon"
  ) +
  geom_smooth(method='lm',formula=y~x) +
  coord_cartesian(xlim = c(0, 14), ylim=c(0, 14)) +
  scale_x_continuous(name="Distance of a run, km") +
  scale_y_continuous(name="Running speed, km/h") +  
  ggtitle("Density of runs") +
  scale_fill_gradient(high="#660066",low="#F0E6F0") +
  theme_nikita

print(r)

#scatterplot
p <- ggplot(data=stat_test, aes(x=distance,y=marathon_speed)) + 
  geom_point(
    aes(color=group)
  ) +
  geom_smooth(method='lm',formula=y~x, se=FALSE) +
  scale_x_continuous(name="Weekly running distance, km") +
  scale_y_continuous(name="Running speed at marathon, km/h") +
  ggtitle("Marathon speed and weekly running distance") +
  theme(
    plot.title = element_text(lineheight=1, face="bold",vjust=2),
    legend.title=element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color="#D8D8D8")    
  ) 

print(p)

# boxplot
q <- ggplot(data=stat_test) + 
  geom_boxplot(
    aes(x=group,y=marathon_speed),
    lwd=1,
    varwidth=TRUE
  ) +
  ggtitle("Marathon speed and training intensity") +
  scale_y_continuous(name="Running speed at marathon, km/h") +
  scale_x_discrete(name="") +
  theme_nikita
print(q)

fast_runners <- stat_test[stat_test$group == ">= 50 km/week", ]$marathon_speed
slow_runners <- stat_test[stat_test$group == "< 50 km/week", ]$marathon_speed
t_test <- t.test(fast_runners, slow_runners, conf.level=0.99, alternative="greater")

print(t_test)
