library(tidyverse)
library(gganimate)
library(ggthemes)

df <- data.frame(xaxisTime=runif(300)*20) %>%
  mutate(Y = .2*xaxisTime+3*(xaxisTime>10)-.1*xaxisTime*(xaxisTime>10)+rnorm(300),
         state="1",
         groupX=floor(xaxisTime)+.5,
         groupLine=floor(xaxisTime),
         cutLine=rep(c(9,11),150)) %>%
  group_by(groupX) %>%
  mutate(mean_Y=mean(Y)) %>%
  ungroup() %>%
  arrange(groupX)


dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(groupLine=NA,cutLine=NA,mean_Y=NA,state='1. Start with raw data.'),
  #Step 2: Add Y-lines
  df %>% mutate(cutLine=NA,state='2. Figure out what differences in Y are explained by the Running Variable.'),
  #Step 3: Collapse to means
  df %>% mutate(Y = mean_Y,state="3. Keep only what's explained by the Running Variable."),
  #Step 4: Zoom in on just the cutoff
  df %>% mutate(mean_Y = ifelse(xaxisTime > 9 & xaxisTime < 11,mean_Y,NA),Y=ifelse(xaxisTime > 9 & xaxisTime < 11,mean_Y,NA),groupLine=NA,state="4. Focus just on what happens around the cutoff."),
  #Step 5: Show the effect
  df %>% mutate(mean_Y = ifelse(xaxisTime > 9 & xaxisTime < 11,mean_Y,NA),Y=ifelse(xaxisTime > 9 & xaxisTime < 11,mean_Y,NA),groupLine=NA,state="5. The jump at the cutoff is the effect of treatment."))


p <- ggplot(dffull,aes(y=Y,x=xaxisTime))+geom_point()+
  geom_vline(aes(xintercept=10),linetype='dashed')+
  geom_point(aes(y=mean_Y,x=groupX),color="red",size=2)+
  geom_vline(aes(xintercept=groupLine))+
  geom_vline(aes(xintercept=cutLine))+
  geom_segment(aes(x=10,xend=10,
                   y=ifelse(state=='5. The jump at the cutoff is the effect of treatment.',
                            filter(df,groupLine==9)$mean_Y[1],NA),
                   yend=filter(df,groupLine==10)$mean_Y[1]),size=1.5,color='blue')+
  scale_color_colorblind()+
  scale_x_continuous(
    breaks = c(5, 15),
    label = c("Untreated", "Treated")
  )+xlab("Running Variable")+
  labs(title = 'The Effect of Treatment on Y using Regression Discontinuity \n{next_state}')+
  transition_states(state,transition_length=c(6,16,6,16,6),state_length=c(50,22,12,22,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=175)