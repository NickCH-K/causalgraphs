library(tidyverse)
library(gganimate)
library(ggthemes)


set.seed(32322)
df <- data.frame(Control = c(rep("Control",300),rep("Treatment",300)),
                 Time=rep(c(rep("Before",150),rep("After",150)),2),
                 Group = rep(c(rep("Group A",75),rep("Group B",75)),4),
                 GroupTime=rep(c(rep("Group A Before",75),rep("Group B Before",75),
                             rep("Group A After",75),rep("Group B After",75)),2)) %>%
  mutate(Y = 2+.5*(Control=="Treatment")+.5*(Time=="After") + 
           .2*(Control=="Treatment")*(Time=="After")+
           .4*(GroupTime=="Group B After" & Control == "Treatment")+
           2.6*(Group=="Group B")+
           rnorm(600)/4,state="1",
         xaxisTime = (Time == "Before") + 2*(Time == "After") + (runif(600)-.5)*.95) %>%
  group_by(Control,GroupTime) %>%
  mutate(mean_Y=mean(Y)) %>%
  ungroup() 

df$Time <- factor(df$Time,levels=c("Before","After"))

#Create segments
dfseg <- df %>%
  group_by(Control,GroupTime) %>%
  summarize(mean_Y = mean(mean_Y)) %>%
  ungroup()

diffA <- filter(dfseg,GroupTime=='Group A After',Control=='Control')$mean_Y[1] - filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1]
diffB <- filter(dfseg,GroupTime=='Group B After',Control=='Control')$mean_Y[1] - filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1]
DIDA <- filter(dfseg,GroupTime=='Group A After',Control=='Treatment')$mean_Y[1] - filter(dfseg,GroupTime=='Group A Before',Control=='Treatment')$mean_Y[1] - diffA

dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(state='1. Start with raw data.'),
  #Step 2: Add Y-lines
  df %>% mutate(state='2. See how Y is explained by Treatment, Time and/or Group.'),
  #Step 3: Collapse to means
  df %>% mutate(Y = mean_Y,state="3. Keep only what's explained by Treatment, Time, and/or Group."),
  #Step 4: Display time effect
  df %>% mutate(Y = mean_Y,state="4. See how Control changed over Time in each Group."),
  #Step 5: Shift to remove time effect
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*ifelse(Group=="Group A",diffA,diffB),
                state="5. Remove the Before/After difference in each groups."),
  #Step 6: Raw demeaned data only
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*ifelse(Group=="Group A",diffA,diffB),
                state='6. The remaining Before/After difference is the diff-in-diff in each Group.'),
  #Step 7: And get rid of the group A difference
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*ifelse(Group=="Group A",diffA,diffB) - (Time=='After' & Control == "Treatment")*DIDA,
                state='7. Remove the difference-in-difference effect from Group A.'),
  #Step 8: And rest
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*ifelse(Group=="Group A",diffA,diffB) - (Time=='After' & Control == "Treatment")*DIDA,
                state='8. The remaining Before/After in Group B is the diff-in-diff-in-diff.')
  )



p <- ggplot(dffull,aes(y=Y,x=xaxisTime,color=as.factor(Control)))+geom_point()+
  guides(color=guide_legend(title="Group"))+
  geom_vline(aes(xintercept=1.5),linetype='dashed')+
  geom_hline(aes(yintercept=4),col='black',size=1)+
  scale_color_colorblind()+
  scale_x_continuous(
    breaks = c(1, 2),
    label = c("Before Treatment", "After Treatment")
  )+xlab("Time")+
  scale_y_continuous(
      breaks = c(2.5, 5.5),
      label = c("Y for Group A", "Y for Group B")
    )+ylab("")+
  theme(axis.text.y=element_text(angle=90,hjust=.5,size=14),
        axis.text.x=element_text(size=14),
        legend.text=element_text(size=14))+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,GroupTime=='Group A Before',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group A Before',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,GroupTime=='Group A After',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group A After',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,GroupTime=='Group A After',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group A After',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,GroupTime=='Group B Before',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group B Before',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,GroupTime=='Group B After',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group B After',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. See how Y is explained by Treatment, Time and/or Group.',"3. Keep only what's explained by Treatment, Time, and/or Group."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,GroupTime=='Group B After',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,GroupTime=='Group B After',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  #And the vertical lines for effects
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=="4. See how Control changed over Time in each Group.",
                            filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1]+diffB,
                            ifelse(state=="5. Remove the Before/After difference in each groups.",
                                   filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1],NA)),
                   yend=filter(dfseg,GroupTime=='Group B Before',Control=='Control')$mean_Y[1]),size=1.5,color='blue')+  
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=="4. See how Control changed over Time in each Group.",
                            filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1]+diffA,
                            ifelse(state=="5. Remove the Before/After difference in each groups.",
                                   filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1],NA)),
                   yend=filter(dfseg,GroupTime=='Group A Before',Control=='Control')$mean_Y[1]),size=1.5,color='blue')+  
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=='6. The remaining Before/After difference is the diff-in-diff in each Group.',
                            filter(dfseg,GroupTime=='Group A After',Control=='Treatment')$mean_Y[1]-diffA,
                            ifelse(state=='7. Remove the difference-in-difference effect from Group A.',
                                   filter(dfseg,GroupTime=='Group A Before',Control=='Treatment')$mean_Y[1],NA)),
                   yend=filter(dfseg,GroupTime=='Group A Before',Control=='Treatment')$mean_Y[1]),size=1.5,color='blue')+  
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=='8. The remaining Before/After in Group B is the diff-in-diff-in-diff.',
                            filter(dfseg,GroupTime=='Group B After',Control=='Treatment')$mean_Y[1]-diffA-DIDA,NA),
                   yend=filter(dfseg,GroupTime=='Group B Before',Control=='Treatment')$mean_Y[1]),size=1.5,color='blue')+  
  labs(title = 'The Difference-in-Difference-in-Difference Effect of Treatment \n{next_state}')+
  transition_states(state,transition_length=c(6,16,6,16,6,16,16,6),state_length=c(50,22,12,22,30,50,50,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()


animate(p,nframes=250)

