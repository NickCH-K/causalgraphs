library(tidyverse)
library(gganimate)
library(ggthemes)

df <- data.frame(xaxisTime=runif(60),Treated=c(rep("Treated",5),rep("Control",55))) %>%
  mutate(Y = 3+.4*xaxisTime+1*(Treated=="Treated")+rnorm(60),
         state="1")

#Make sure the treated obs aren't too close together, that makes it confusing
df[df$Treated=="Treated",]$xaxisTime <- c(1:5/6)+(runif(5)-.5)*.1

caliper <- .02

df <- df %>%
  mutate(bins = c(rep(filter(df,Treated=="Treated")$xaxisTime-caliper,6),
                  rep(filter(df,Treated=="Treated")$xaxisTime+caliper,6))) %>%
  #There has to be a less clunky way to do this
  rowwise() %>%
  mutate(matchmeas = min(abs(xaxisTime-filter(df,Treated=="Treated")$xaxisTime))) %>%
  mutate(match = matchmeas < caliper) %>%
  group_by(Treated,match) %>%
  mutate(mean_Y = ifelse(match==1,mean(Y),NA)) %>%
  ungroup()


#Check how many matches we have before proceeding; regenerate randomized data
#until we have a decent number
table(filter(df,Treated=="Control")$match)

dffull <- rbind(
  #Step 1: Raw data only
  df %>% mutate(bins=NA,mean_Y=NA,state='1. Start with raw data.'),
  #Step 2: Add Y-lines
  df %>% mutate(mean_Y=NA,state='2. Look for Controls with similar X values to the Treatments.'),
  #Step 3: Drop unmatch obs
  df %>% mutate(Y = ifelse(match==1,Y,NA),mean_Y=NA,state="3. Keep Controls only if they're similar enough."),
  #Step 4: Take means
  df %>% mutate(Y = ifelse(match==1,Y,NA),bins=NA,state="4. Among what's kept, see what the treatment explains."),
  #Step 5: Eliminate everything but the means
  df %>% mutate(Y = ifelse(match==1,mean_Y,NA),bins=NA,state="5. Ignore everything not explained by treatment."),
  #Step 6: Get treatment effect
  df %>% mutate(Y = NA,bins=NA,state="6. The treatment effect is the remaining difference."))


p <- ggplot(dffull,aes(y=Y,x=xaxisTime,color=Treated,size=Treated))+geom_point()+
  geom_vline(aes(xintercept=bins))+
  geom_hline(aes(yintercept=mean_Y,color=Treated))+
  geom_segment(aes(x=.5,xend=.5,
                   y=ifelse(state=="6. The treatment effect is the remaining difference.",
                            filter(df,Treated=="Treated")$mean_Y[1],NA),
                   yend=filter(df,Treated=="Control",match==TRUE)$mean_Y[1]),size=1.5,color='blue')+
  scale_color_colorblind()+
  scale_size_manual(values=c(2,3))+xlab("X")+
  guides(fill=guide_legend(title="Group"))+
  labs(title = 'The Effect of Treatment on Y while Matching on X (with a caliper) \n{next_state}')+
  transition_states(state,transition_length=c(12,16,16,16,16,16),state_length=c(50,36,30,30,30,50),wrap=FALSE)+
  ease_aes('sine-in-out')+
  exit_fade()+enter_fade()

animate(p,nframes=200)
