library(tidyverse)
library(gganimate)
library(ggthemes)

df <- data.frame(W = as.integer((1:200>100))) %>%
  mutate(X = .5+2*W + rnorm(200)) %>%
  mutate(Y = -.5*X + 4*W + 1 + rnorm(200),time="1") %>%
  group_by(W) %>%
  mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
  ungroup()

#Calculate correlations
before_cor <- paste("1. Start with raw data. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
after_cor <- paste("6. Analyze what's left! Correlation between X and Y controlling for W: ",round(cor(df$X-df$mean_X,df$Y-df$mean_Y),3),sep='')




#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,time=before_cor),
  #Step 2: Add x-lines
  df %>% mutate(mean_Y=NA,time='2. Figure out what differences in X are explained by W'),
  #Step 3: X de-meaned 
  df %>% mutate(X = X - mean_X,mean_X=0,mean_Y=NA,time="3. Remove differences in X explained by W"),
  #Step 4: Remove X lines, add Y
  df %>% mutate(X = X - mean_X,mean_X=NA,time="4. Figure out what differences in Y are explained by W"),
  #Step 5: Y de-meaned
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=0,time="5. Remove differences in Y explained by W"),
  #Step 6: Raw demeaned data only
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=NA,time=after_cor))

ggplot(dffull,aes(y=Y,x=X,color=as.factor(W)))+geom_point()+
  geom_vline(aes(xintercept=mean_X,color=as.factor(W)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(W)))+
  guides(color=guide_legend(title="W"))+
  scale_color_colorblind()+
  labs(title = 'The Relationship between Y and X, Controlling for a Binary Variable W \n{next_state}')+
  transition_states(time,transition_length=c(6,16,6,16,6,6),state_length=c(50,22,12,22,12,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()
