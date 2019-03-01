library(tidyverse)
library(gganimate)
library(ggthemes)


df <- data.frame(X = runif(1000)+1,Treatment=as.factor("Untreated"),time="1") %>%
  mutate(Y = X + rnorm(1000)/6) 

cutoff <- 1.75

#Add step 2 in which X is demeaned, and 3 in which both X and Y are, and 4 which just changes label
dffull <- rbind(
  #Step 1: Untreated only
  df %>% mutate(time="1. If NOBODY got treatment, looks smooth."),
  #Step 2: Treated only only
  df %>% mutate(Y= Y + .25,Treatment="Treated",time="2. If EVERYBODY got treatment, looks smooth."),
  #Step 3: And the jump
  df %>% mutate(Y=ifelse(X > cutoff,Y+.25,Y),Treatment = ifelse(X>cutoff,"Treated","Untreated"),time='3. In reality, we observe this! The jump is only BECAUSE of treatment.')
)

p <- ggplot(dffull,aes(y=Y,x=X,color=Treatment))+geom_point()+
  geom_smooth(aes(x=X,y=Y,group=Treatment),method='lm',col='red',se=FALSE)+
  geom_vline(aes(xintercept=cutoff),col='black',linetype='dashed',size=1.5)+
  scale_color_colorblind()+
  labs(title = 'Checking for Smoothness in Potential Outcomes at the Cutoff \n{next_state}',
       x="Test Score (X)",
       y="Outcome (Y)")+
  transition_states(time,transition_length=c(12, 12, 12),state_length=c(100,100,100),wrap=FALSE)+
  ease_aes('linear')+
  exit_fade()+enter_fade()

animate(p,nframes=160)
