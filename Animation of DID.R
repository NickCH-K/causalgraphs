library(tidyverse)
library(gganimate)
library(ggthemes)

df <- data.frame(Control = c(rep("Control",150),rep("Treatment",150)),
                 Time=rep(c(rep("Before",75),rep("After",75)),2)) %>%
  mutate(Y = 2+2*(Control=="Treatment")+1*(Time=="After") + 1.5*(Control=="Treatment")*(Time=="After")+rnorm(300),state="1",
         xaxisTime = (Time == "Before") + 2*(Time == "After") + (runif(300)-.5)*.95) %>%
  group_by(Control,Time) %>%
  mutate(mean_Y=mean(Y)) %>%
  ungroup()

df$Time <- factor(df$Time,levels=c("Before","After"))

#Create segments
dfseg <- df %>%
  group_by(Control,Time) %>%
  summarize(mean_Y = mean(mean_Y)) %>%
  ungroup()

diff <- filter(dfseg,Time=='After',Control=='Control')$mean_Y[1] - filter(dfseg,Time=='Before',Control=='Control')$mean_Y[1]

dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(state='1. Start with raw data.'),
  #Step 2: Add Y-lines
  df %>% mutate(state='2. Figure out what differences in Y are explained by Treatment and/or Time.'),
  #Step 3: Collapse to means
  df %>% mutate(Y = mean_Y,state="3. Keep only what's explained by Treatment and/or Time."),
  #Step 4: Display time effect
  df %>% mutate(Y = mean_Y,state="4. See how Control changed over Time."),
  #Step 5: Shift to remove time effect
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*diff,
                state="5. Remove the Before/After Control difference for both groups."),
  #Step 6: Raw demeaned data only
  df %>% mutate(Y = mean_Y 
                - (Time=='After')*diff,
                state='6. The remaining Before/After Treatment difference is the effect.'))



p <- ggplot(dffull,aes(y=Y,x=xaxisTime,color=as.factor(Control)))+geom_point()+
  guides(color=guide_legend(title="Group"))+
  geom_vline(aes(xintercept=1.5),linetype='dashed')+
  scale_color_colorblind()+
  scale_x_continuous(
    breaks = c(1, 2),
    label = c("Before Treatment", "After Treatment")
  )+xlab("Time")+
  #The four lines for the four means
  geom_segment(aes(x=ifelse(state %in% c('2. Figure out what differences in Y are explained by Treatment and/or Time.',"3. Keep only what's explained by Treatment and/or Time."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,Time=='Before',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,Time=='Before',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. Figure out what differences in Y are explained by Treatment and/or Time.',"3. Keep only what's explained by Treatment and/or Time."),
                            .5,NA),
                   xend=1.5,y=filter(dfseg,Time=='Before',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,Time=='Before',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  geom_segment(aes(x=ifelse(state %in% c('2. Figure out what differences in Y are explained by Treatment and/or Time.',"3. Keep only what's explained by Treatment and/or Time."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,Time=='After',Control=='Control')$mean_Y[1],
                   yend=filter(dfseg,Time=='After',Control=='Control')$mean_Y[1]),size=1,color='black')+
  geom_segment(aes(x=ifelse(state %in% c('2. Figure out what differences in Y are explained by Treatment and/or Time.',"3. Keep only what's explained by Treatment and/or Time."),
                            1.5,NA),
                   xend=2.5,y=filter(dfseg,Time=='After',Control=='Treatment')$mean_Y[1],
                   yend=filter(dfseg,Time=='After',Control=='Treatment')$mean_Y[1]),size=1,color="#E69F00")+
  #Line indicating treatment effect
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=='6. The remaining Before/After Treatment difference is the effect.',
                            filter(dfseg,Time=='After',Control=='Treatment')$mean_Y[1]-diff,NA),
                   yend=filter(dfseg,Time=='Before',Control=='Treatment')$mean_Y[1]),size=1.5,color='blue')+
  #Line indicating pre/post control difference
  geom_segment(aes(x=1.5,xend=1.5,
                   y=ifelse(state=="4. See how Control changed over Time.",
                            filter(dfseg,Time=='After',Control=='Control')$mean_Y[1],
                            ifelse(state=="5. Remove the Before/After Control difference for both groups.",
                                   filter(dfseg,Time=='Before',Control=='Control')$mean_Y[1],NA)),
                   yend=filter(dfseg,Time=='Before',Control=='Control')$mean_Y[1]),size=1.5,color='blue')+
  labs(title = 'The Difference-in-Difference Effect of Treatment \n{next_state}')+
  transition_states(state,transition_length=c(6,16,6,16,6,6),state_length=c(50,22,12,22,12,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=150)
