library(tidyverse)
library(gganimate)
library(ggthemes)

df <- data.frame(Z = as.integer(1:200>100),
                 W = rnorm(200)) %>%
  mutate(X = .5+2*W +2*Z+ rnorm(200)) %>%
  mutate(Y = -X + 4*W + 1 + rnorm(200),time="1") %>%
  group_by(Z) %>%
  mutate(mean_X=mean(X),mean_Y=mean(Y),YL=NA,XL=NA) %>%
  ungroup()

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
afterlab <- '6. Draw a line between the points. The slope is the effect of X on Y.'

dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y=NA,time='2. Figure out what differences in X are explained by Z'),
  #Step 3: X de-meaned 
  df %>% mutate(X = mean_X,mean_Y=NA,time="3. Remove everything in X not explained by Z"),
  #Step 4: Remove X lines, add Y
  df %>% mutate(X = mean_X,mean_X=NA,time="4. Figure out what differences in Y are explained by Z"),
  #Step 5: Y de-meaned
  df %>% mutate(X = mean_X,Y = mean_Y,mean_X=NA,time="5. Remove everything in Y not explained by Z"),
  #Step 6: Raw demeaned data only
  df %>% mutate(X =  mean_X,Y =mean_Y,mean_X=NA,mean_Y=NA,YL=mean_Y,XL=mean_X,time=afterlab))

#Get line segments
endpts <- df %>%
  group_by(Z) %>%
  summarize(mean_X=mean(mean_X),mean_Y=mean(mean_Y))

p <- ggplot(dffull,aes(y=Y,x=X,color=as.factor(Z)))+geom_point()+
  geom_vline(aes(xintercept=mean_X,color=as.factor(Z)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(Z)))+
  guides(color=guide_legend(title="Z"))+
  geom_segment(aes(x=ifelse(time==afterlab,endpts$mean_X[1],NA),
                   y=endpts$mean_Y[1],xend=endpts$mean_X[2],
                   yend=endpts$mean_Y[2]),size=1,color='blue')+
  scale_color_colorblind()+
  labs(title = 'The Relationship between Y and X, With Binary Z as an Instrumental Variable \n{next_state}')+
  transition_states(time,transition_length=c(6,16,6,16,6,6),state_length=c(50,22,12,22,12,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=175)
