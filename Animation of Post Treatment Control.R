library(tidyverse)
library(gganimate)
library(ggthemes)

#Probably try a few times until the raw correlation looks nice and low
df <- data.frame(X = rnorm(200)+1,time="1") %>%
  mutate(C = X > 1) %>%
  mutate(Y = rnorm(200)+1+2.5*C) %>%
  group_by(C) %>%
  mutate(mean_X=mean(X),mean_Y=mean(Y)) %>%
  ungroup()

cor(df$X,df$Y)

#Calculate correlations
before_cor <- paste("1. Start with raw data, ignoring C. Correlation between X and Y: ",round(cor(df$X,df$Y),3),sep='')
after_cor <- paste("7. Analyze what's left! Correlation between X and Y controlling for C: ",round(cor(df$X-df$mean_X,df$Y-df$mean_Y),3),sep='')




#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,C=0,time=before_cor),
  #Step 2: Raw data only
  df %>% mutate(mean_X=NA,mean_Y=NA,time='2. Separate data by the values of C.'),
  #Step 3: Add x-lines
  df %>% mutate(mean_Y=NA,time='3. Figure out what differences in X are explained by C'),
  #Step 4: X de-meaned 
  df %>% mutate(X = X - mean_X,mean_X=0,mean_Y=NA,time="4. Remove differences in X explained by C"),
  #Step 5: Remove X lines, add Y
  df %>% mutate(X = X - mean_X,mean_X=NA,time="5. Figure out what differences in Y are explained by C"),
  #Step 6: Y de-meaned
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=0,time="6. Remove differences in Y explained by C"),
  #Step 7: Raw demeaned data only
  df %>% mutate(X = X - mean_X,Y = Y - mean_Y,mean_X=NA,mean_Y=NA,time=after_cor))

p <- ggplot(dffull,aes(y=Y,x=X,color=as.factor(C)))+geom_point()+
  geom_vline(aes(xintercept=mean_X,color=as.factor(C)))+
  geom_hline(aes(yintercept=mean_Y,color=as.factor(C)))+
  guides(color=guide_legend(title="C"))+
  scale_color_colorblind()+
  labs(title = 'Removing the Effect of X on Y by Controlling for Post-Treatment C \n{next_state}')+
  transition_states(time,transition_length=c(1,12,32,12,32,12,12),state_length=c(160,125,100,75,100,75,160),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=200)
