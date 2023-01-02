install.packages('texreg')
library(texreg) 
library(dplyr)
install.packages("devtools")
library(devtools)
require(ggplot2)


#drop first 5 observations
data <-data %>% filter(round!=1,round!=2,round!=3,round!=4,round!=5,round!=16,round!=17,round!=18,round!=19,round!=20 )

##########################################################################################################################################
#### Create useful variables
##########################################################################################################################################


# Group: (factor) 1 if phi=1,alpha=0.7; 2 if phi=1.5,alpha=0.7, 3 if phi=1,alpha=0.2; 4 if phi=1.5,alpha=0.2
data <-data %>% mutate(treatment  = factor(ifelse(phi ==1 & alpha == 0.7,  1, ifelse(phi==1&alpha==0.2,3,ifelse(phi==1.5&alpha==0.7,2,4)))))

# Order: 1 if 0.7->0.2; 2 if 0.2->0.7  
data <-data %>% mutate(order = ifelse(round <= 15 & alpha == 0.7,  1, ifelse(round>15&alpha==0.2,1,2)))

# cost of the trial
k<-10

# optimal final position given initial position
data <- data %>% mutate(opt_final = ifelse( phi==1 & alpha*initial>25  ,alpha*initial,
                                            ifelse( phi==1 & initial<25, initial,
                                                    ifelse( phi==1 & alpha*initial<=25 & initial>=25 ,25,
                                                            ifelse( phi==1.5 & alpha*initial>30  ,alpha*initial,
                                                                    ifelse(phi==1.5 & initial<30, initial,
                                                                           ifelse(phi==1.5 & alpha*initial<=30 & initial>=30 ,30,
                                                                                  NA ))) )))) 


# expected payoff (considering the realized final position)
data <- data %>% mutate(exp_payoff = ifelse(role==2 & accept==0 ,phi*final*(1-(final/100)) -  final*(final/100)   ,
                                            ifelse(role==1 & accept==0, -final*(1-(final/100)),
                                                   NA)) )  %>% mutate(exp_payoff= round(exp_payoff, 2)-k)

# can get c^P with c_I?
data <- data %>% mutate(cP_feasible = ifelse(role==2 & phi==1 & alpha*initial<=25 & initial>=25 ,1,
                                             ifelse(role==2 & phi==1.5 & alpha*initial<=30 & initial>=30 , 1,
                                                    0)) )

# worst expected loss -- proposer maximizes responder's loss --- considering optimal final position
data <- data %>% mutate(exp_payoff_res = ifelse(role==1 & alpha*initial>50  ,-alpha*initial*(1-(alpha*initial)/100),
                                                ifelse(role==1  & initial<50 , -initial*(1-(initial)/100),
                                                       ifelse(role==1  & alpha*initial<=50 & initial>=50 ,-50*(1-(50)/100),
                                                              ifelse(role==2 & alpha*initial>50  ,phi*alpha*initial*(1-(alpha*initial)/100) - alpha*initial*((alpha*initial)/100),
                                                                     ifelse(role==2  & initial<50 , phi*initial*(1-(initial)/100)- initial*((initial)/100),
                                                                            phi*50*(1-(50)/100) -50*(1-(50)/100))   )) )) )  %>% mutate(exp_payoff_res = -k+round(exp_payoff_res , 2))



# expected loss if proposer maximizes her payoff -- considering optimal final position
data <- data %>% mutate(exp_payoff_pro = ifelse(role==1 & phi==1 & alpha*initial>25  ,-alpha*initial*(1-(alpha*initial)/100),
                                                ifelse(role==1  & phi==1 & initial<25, -initial*(1-(initial)/100),
                                                       ifelse(role==1  & phi==1 & alpha*initial<=25 & initial>=25 ,-25*(1-(25)/100),
                                                              ifelse(role==1 & phi==1.5 & alpha*initial>30  ,-alpha*initial*(1-(alpha*initial)/100),
                                                                     ifelse(role==1  & phi==1.5 & initial<30, -initial*(1-(initial)/100),
                                                                            ifelse(role==1  & phi==1.5 & alpha*initial<=30 & initial>=30 ,-30*(1-(30)/100),
                                                                                   ifelse(role==2 & phi==1 & alpha*initial>25  ,alpha*initial*(1-(alpha*initial)/100) - alpha*initial*((alpha*initial)/100),
                                                                                          ifelse(role==2  & phi==1 & initial<25, initial*(1-(initial)/100)- initial*((initial)/100),
                                                                                                 ifelse(role==2  & phi==1 & alpha*initial<=25 & initial>=25 ,25*(1-(25)/100)-25*((25)/100),
                                                                                                        ifelse(role==2 & phi==1.5 & alpha*initial>30  ,phi*alpha*initial*(1-(alpha*initial)/100)-alpha*initial*((alpha*initial)/100),
                                                                                                               ifelse(role==2  & phi==1.5 & initial<30, phi*initial*(1-(initial)/100)-initial*((initial)/100),
                                                                                                                      phi*30*(1-(30)/100)-30*((30)/100) ))) )))))))))  %>% mutate(exp_payoff_pro= -k+round(exp_payoff_pro, 2))




# Percentage scaling down in the interval [alpha*c_I,c_I]
data <- data %>% mutate(perc_scaling_down = (final-alpha*initial)/(0.000000001+initial*(1-alpha)))   %>% mutate(perc_scaling_down = round(perc_scaling_down, 2))

# Offer as a percentage of alpha*c_I
#data <- data %>% mutate(perc_offer = ( alpha*initial-offer)/(alpha*initial) )   %>% mutate(perc_offer= round(perc_offer, 2))

# Offer as a proportion of optimal offer
data <- data %>% mutate(perc_offer = -offer/exp_payoff_pro) %>% mutate(perc_offer= round(perc_offer, 2))

# Final as a percentage of alpha*c_I
#data <- data %>% mutate(perc_final = ( alpha*initial-final)/(alpha*initial) )   %>% mutate(perc_final= round(perc_final, 2))

data <- data %>% mutate(perc_final =final/opt_final )   %>% mutate(perc_final= round(perc_final, 2))





# correct
error<- 0

# correct accept
#data <- data %>% mutate(opt_accept = ifelse(accept==1& role==1 & -exp_payoff_pro+error>=offer  ,1,
#                                            ifelse(accept==1& role==1 & -exp_payoff_pro<offer  ,0, NA ) ))
data <- data %>% mutate(opt_accept = ifelse(accept==1& role==1 & (1+error)>=offer/(-exp_payoff_pro)  ,1,
                                            ifelse(accept==1& role==1 & (1+error)<offer/(-exp_payoff_pro)  ,0, NA ) ))

# correct reject
# <- data %>% mutate(opt_reject = ifelse(accept==0& role==1 & -exp_payoff_pro -error<offer  ,1,
#                                            ifelse(accept==0& role==1 & -exp_payoff_pro >=offer  ,0,NA ) ))
data <- data %>% mutate(opt_reject = ifelse(accept==0 & role==1 & (1+error)>(-exp_payoff_pro )/offer  ,1,
                                            ifelse(accept==0& role==1 &  (1+error)<=(-exp_payoff_pro)/offer  ,0,NA ) ))


# accepted to a error level
data <- data %>% mutate(opt_accept_unc = ifelse(accept==1& role==1 & (1+error)>=offer/(-exp_payoff_pro)  ,1,0 ))

# expected loss if proposer maximizes her payoff -- considering optimal final position -- entered in each row
data <- data %>% mutate(exp_payoff_res_both = ifelse( phi==1 & alpha*initial>25  ,-alpha*initial*(1-(alpha*initial)/100),
                                                      ifelse( phi==1 & initial<25, -initial*(1-(initial)/100),
                                                              ifelse( phi==1 & alpha*initial<=25 & initial>=25 ,-25*(1-(25)/100),
                                                                      ifelse(phi==1.5 & alpha*initial>30  ,-alpha*initial*(1-(alpha*initial)/100),
                                                                             ifelse(  phi==1.5 & initial<30, -initial*(1-(initial)/100), -30*(1-(30)/100) ))))) )%>% mutate(exp_payoff_res_both= -k+round(exp_payoff_res_both, 2))



# offer made is optimal?
data <- data %>% mutate(offer_opt = ifelse(role==2 & (1+error)>=offer/(-exp_payoff_res_both) ,1, 0))








# feasible
data <- data %>% mutate(feasible = ifelse(role==1 & -exp_payoff_pro -error>=offer  ,1,0 ))





## expected payoff and loss enter in each row
data <- data %>% mutate(exp_payoff_pro_both = ifelse( phi==1 & alpha*initial>25  ,alpha*initial*(1-(alpha*initial)/100) - alpha*initial*((alpha*initial)/100),
                                                      ifelse( phi==1 & initial<25, initial*(1-(initial)/100)- initial*((initial)/100),
                                                              ifelse( phi==1 & alpha*initial<=25 & initial>=25 ,25*(1-(25)/100)-25*((25)/100),
                                                                      ifelse(phi==1.5 & alpha*initial>30  ,phi*alpha*initial*(1-(alpha*initial)/100)-alpha*initial*((alpha*initial)/100),
                                                                             ifelse( phi==1.5 & initial<30, phi*initial*(1-(initial)/100)-initial*((initial)/100),
                                                                                     phi*30*(1-(30)/100)-30*((30)/100) ))) ))) %>% mutate(exp_payoff_pro_both = -k+round(exp_payoff_pro_both , 2))




# expected loss if proposer maximizes her payoff -- considering optimal final position
data <- data %>% mutate(exp_payoff_res_both = ifelse( phi==1 & alpha*initial>25  ,-alpha*initial*(1-(alpha*initial)/100),
                                                      ifelse( phi==1 & initial<25, -initial*(1-(initial)/100),
                                                              ifelse( phi==1 & alpha*initial<=25 & initial>=25 ,-25*(1-(25)/100),
                                                                      ifelse(phi==1.5 & alpha*initial>30  ,-alpha*initial*(1-(alpha*initial)/100),
                                                                             ifelse(  phi==1.5 & initial<30, -initial*(1-(initial)/100), -30*(1-(30)/100) ))))) )%>% mutate(exp_payoff_res_both= -k+round(exp_payoff_res_both, 2))




##########################################################################################################################################
#### Summary statistic
##########################################################################################################################################


# All
data %>% filter(role==2) %>%group_by(treatment)  %>% summarise(mean_in = mean(initial), mean_of= mean(offer), perc_accept= mean(accept), mean_fi = mean(final,na.rm=TRUE),mean_exp_payoff_realized= mean( exp_payoff,na.rm=TRUE) , mean_opt_payoff_pro_both= mean(exp_payoff_pro_both),  mean_opt_payoff_res_both= mean(exp_payoff_res_both))
#data1 %>% filter(role==2) %>%group_by(treatment)  %>% summarise(mean_in = mean(initial), mean_of= mean(offer), perc_accept= mean(accept), mean_fi = mean(final,na.rm=TRUE), mean_opt_payoff_pro_both= mean(exp_payoff_pro_both),  mean_opt_payoff_res_both= mean(exp_payoff_res_both))
data %>% filter(role==1) %>%group_by(treatment)  %>% summarise(mean_exp_payoff_realized_res = mean( exp_payoff,na.rm=TRUE) )
#data1 %>% filter(role==1) %>%group_by(treatment)  %>% summarise(mean_in = mean(initial), mean_of= mean(offer), perc_accept= mean(accept), mean_fi = mean(final,na.rm=TRUE), mean_opt_payoff_pro_both= mean(exp_payoff_pro_both),  mean_opt_payoff_res_both= mean(exp_payoff_res_both))


#data  %>%group_by(treatment, role)  %>% summarise(mean_exp_payoff = mean(exp_payoff_pro))


# All - by order
data %>% filter(role==2) %>%group_by(order,treatment,)  %>% summarise(mean_in = mean(initial), mean_of= mean(offer), perc_accept= mean(accept), mean_fi = mean(final,na.rm=TRUE))
data %>% filter(role==2) %>%group_by(order, treatment, role)  %>% summarise(mean_exp_payoff = mean(exp_payoff_pro))






##########################################################################################################################################
#### Initial Position as a Threat
##########################################################################################################################################

# Considering $\alpha=0.2$, the proposer chooses the same c_I for both $\phi$.
ggplot(data , aes(x = treatment, y = initial))  +   geom_boxplot()

# mean are the same?
anova <- aov(initial~ treatment, data = data)
summary(anova)

# test to confirm
initial_P1 <- data %>% filter(treatment==1,role==2)
initial_P2 <- data %>% filter(treatment==2 ,role==2)  
initial_B3 <- data %>% filter(treatment==3,role==2)
initial_B4 <- data %>% filter(treatment==4,role==2)  

wilcox.test(initial_P1$initial, initial_P2$initial, alternative = "two.sided") # 
wilcox.test(initial_B3$initial, initial_B4$initial, alternative = "two.sided") # same

wilcox.test(initial_P1$initial, initial_B3$initial, alternative = "two.sided")
wilcox.test(initial_P2$initial, initial_B4$initial, alternative = "two.sided")






# alpha is important
reg_initial <- data %>% filter(role==2) %>% lm(initial~ alpha + phi+ round+ order, data = .)
summary(reg_initial)


# histograms: many proposers choose c_I=100 if $\alpha=0.2$
data %>% filter(treatment==1, role==2) %>% ggplot(., aes(x = initial, y = after_stat(count / sum(count) )) ) + geom_histogram(bins = 30)
data %>% filter(treatment==2, role==2) %>% ggplot(., aes(x = initial, y = after_stat(count / sum(count) )) ) + geom_histogram(bins = 30)
data %>% filter(treatment==3, role==2) %>% ggplot(., aes(x = initial, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)
data %>% filter(treatment==4, role==2) %>% ggplot(., aes(x = initial, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)


# histograms: scaling down
data %>% filter(accept ==0,treatment==1, role==2) %>% ggplot(., aes(x = perc_scaling_down, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)
data %>% filter(accept ==0,treatment==2, role==2) %>% ggplot(., aes(x = perc_scaling_down, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)
data %>% filter(accept ==0,treatment==3, role==2) %>% ggplot(., aes(x = perc_scaling_down, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)
data %>% filter(accept ==0,treatment==4, role==2) %>% ggplot(., aes(x = perc_scaling_down, y = after_stat(count / sum(count) ) )) + geom_histogram(bins = 30)





##########################################################################################################################################
#### Offer after threatening
##########################################################################################################################################


#Summary

data  %>%filter(role==1) %>% group_by(treatment)  %>% summarise(mean_exp_payoff=mean(exp_payoff_res_both,na.rm=TRUE))
data  %>%filter(role==1) %>% group_by( accept, treatment)  %>% summarise(mean_of=mean(offer,na.rm=TRUE))

error<- 0
data <- data %>% mutate(offer_opt = ifelse(role==2 & (1+error)>=offer/(-exp_payoff_res_both) ,1, 0))
data  %>%filter(role==1, (1+error)>=offer/(-exp_payoff_pro) ) %>% group_by(treatment)  %>% summarise( perc_accept=mean(accept,na.rm=TRUE))
data  %>%filter(role==2 ) %>% group_by(treatment)  %>% summarise(perc_offer=mean(offer_opt,na.rm=TRUE))

error<- 0.25
data <- data %>% mutate(offer_opt = ifelse(role==2 & (1+error)>=offer/(-exp_payoff_res_both) ,1, 0))
data  %>%filter(role==1, (1+error)>=offer/(-exp_payoff_pro) ) %>% group_by(treatment)  %>% summarise( perc_accept=mean(accept,na.rm=TRUE))
data  %>%filter(role==2 ) %>% group_by(treatment)  %>% summarise(perc_offer=mean(offer_opt,na.rm=TRUE))

error<- 0.5
data <- data %>% mutate(offer_opt = ifelse(role==2 & (1+error)>=offer/(-exp_payoff_res_both) ,1, 0))
data  %>%filter(role==1, (1+error)>=offer/(-exp_payoff_pro) ) %>% group_by(treatment)  %>% summarise( perc_accept=mean(accept,na.rm=TRUE))
data  %>%filter(role==2 ) %>% group_by(treatment)  %>% summarise(perc_offer=mean(offer_opt,na.rm=TRUE))




# The results show that the offer the proposer makes is the same independent of the treatment.
ggplot(data , aes(x = treatment, y = offer))  +   geom_boxplot()
anova <- aov(offer~treatment, data = data)
summary(anova) # anova p-value= 0.00602 **


# test to confirm
offer_P1 <- data %>% filter(treatment==1,role==2)
offer_P2 <- data %>% filter(treatment==2 ,role==2)  
offer_B3 <- data %>% filter(treatment==3,role==2)
offer_B4 <- data %>% filter(treatment==4,role==2)  

wilcox.test(offer_P1$offer, offer_P2$offer, alternative = "two.sided") # differente
wilcox.test(offer_B3$offer, offer_B4$offer, alternative = "two.sided") # same

wilcox.test(offer_P1$offer, offer_B3$offer, alternative = "two.sided") # same
wilcox.test(offer_P2$offer, offer_B4$offer, alternative = "two.sided") # same





# alpha and initial are important
reg_offer <- data %>% filter(role==2) %>% lm(offer~initial + alpha + phi+round+factor(order)+ exp_payoff_res_both , data = .)
summary(reg_offer)

# histograms: many proposers choose c_I=100 if $\alpha=0.2$
data %>% filter(treatment==1, role==1) %>% ggplot(., aes(x = perc_offer, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 40, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==2, role==1) %>% ggplot(., aes(x = perc_offer, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 40, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==3, role==1) %>% ggplot(., aes(x = perc_offer, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 40, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==4, role==1) %>% ggplot(., aes(x = perc_offer, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 40, fill = "blue", alpha = 0.2 )


#################################################################################################################################################################
#### Accept?
#################################################################################################################################################################


#it seems they accept offers that are 'low' and below c_I
# Acceptance decision only depends on the offer
probit_accept <- data %>% filter(role==1, treatment ==3) %>% glm(accept ~ offer  + initial + round  + order + exp_payoff_res_both+ exp_payoff_pro_both, family = binomial(link = "probit"), data = .)
summary(probit_accept)

logit_accept_1 <- data %>% filter(role==1, treatment ==1) %>%  glm(accept ~ offer + phi + alpha + initial + round + order + exp_payoff_res_both+ exp_payoff_pro_both , family = "binomial", data = .)
summary(logit_accept_1 )
logit_accept_2 <- data %>% filter(role==1, treatment ==2) %>%  glm(accept ~ offer + initial + round + order+ exp_payoff_res_both+ exp_payoff_pro_both, family = "binomial", data = .)
summary(logit_accept_2 )
logit_accept_3 <- data %>% filter(role==1, treatment ==3) %>%  glm(accept ~ offer + initial + round + order+ exp_payoff_res_both+ exp_payoff_pro_both, family = "binomial", data = .)
summary(logit_accept_3 )
logit_accept_4 <- data %>% filter(role==1, treatment ==4) %>%  glm(accept ~ offer + initial + round + order+ exp_payoff_res_both+ exp_payoff_pro_both, family = "binomial", data = .)
summary(logit_accept_4 )
texreg(list(logit_accept_1,logit_accept_2, logit_accept_3, logit_accept_4), booktabs = TRUE, dcolumn = TRUE)



# Histogram, of accepted and rejected offer
#P1
ggplot(data,aes(x=offer)) +
  geom_histogram(data=subset(data, role==1& treatment ==1& accept==1) ,aes(y =..density..),fill = "red", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1&treatment ==1& accept==1),col="red") +
  geom_histogram(data=subset(data,role==1&treatment ==1& accept==0),aes(y =..density..),fill = "blue", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1&treatment ==1& accept==0),col="blue") +
  geom_vline(data=subset(data,role==1&treatment ==1),aes(xintercept=mean(offer)),color="black",  size=1)+
  geom_vline(data=subset(data,role==1&treatment ==1),aes(xintercept=mean(-exp_payoff_pro)),color=5,  size=1)

# Histogram, of accepted and rejected offer
#P2
ggplot(data,aes(x=offer)) +
  geom_histogram(data=subset(data, role==1& treatment ==2& accept==1) ,aes(y =..density..),fill = "red", alpha = 0.2, bins = 30) + 
  geom_density(data=subset(data,role==1& treatment ==2& accept==1),col="red") +
  geom_histogram(data=subset(data,role==1& treatment ==2& accept==0),aes(y =..density..),fill = "blue", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1& treatment ==2& accept==0),col="blue") +
  geom_vline(data=subset(data,role==1& treatment ==2),aes(xintercept=mean(offer)),color="black",  size=1)+
  geom_vline(data=subset(data,role==1& treatment ==2),aes(xintercept=mean(-exp_payoff_pro)),color=5,  size=1)

# Histogram, of accepted and rejected offer
#B3
ggplot(data,aes(x=offer)) +
  geom_histogram(data=subset(data, role==1&  treatment ==3& accept==1) ,aes(y =..density..),fill = "red", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1&treatment ==3& accept==1),col="red") +
  geom_histogram(data=subset(data,role==1&treatment ==3& accept==0),aes(y =..density..),fill = "blue", alpha = 0.2, bins = 30) + 
  geom_density(data=subset(data,role==1&treatment ==3& accept==0),col="blue") +
  geom_vline(data=subset(data,role==1&treatment ==3),aes(xintercept=mean(offer)),color="black",  size=1)+
  geom_vline(data=subset(data,role==1&treatment ==3),aes(xintercept=mean(-exp_payoff_pro)),color=5,  size=1)

# Histogram, of accepted and rejected offer
#B4
ggplot(data,aes(x=offer)) +
  geom_histogram(data=subset(data, role==1& treatment ==4& accept==1) ,aes(y =..density..),fill = "red", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1&treatment ==4& accept==1),col="red") +
  geom_histogram(data=subset(data,role==1&treatment ==4& accept==0),aes(y =..density..),fill = "blue", alpha = 0.2, bins = 30) +  
  geom_density(data=subset(data,role==1&treatment ==4& accept==0),col="blue") +
  geom_vline(data=subset(data,role==1&treatment ==4),aes(xintercept=mean(offer)),color="black",  size=1)+
  geom_vline(data=subset(data,role==1&treatment ==4),aes(xintercept=mean(-exp_payoff_pro)),color=5,  size=1)




# initial vs offer(accepted and rejcted): no idea what to say
data %>% filter(role==2, phi==1, alpha==0.7) %>% ggplot(., aes(x=initial, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==2, phi==1, alpha==0.2) %>% ggplot(., aes(x=initial, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==2, phi==1.5, alpha==0.7) %>% ggplot(., aes(x=initial, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==2, phi==1.5, alpha==0.2) %>% ggplot(., aes(x=initial, y=offer, color=factor(accept) )) +   geom_point()





# percentage of correctly accepted and rejected offers
data  %>%filter(role==1) %>% group_by(treatment)  %>% summarise(mean_exp_payoff=mean(exp_payoff_pro,na.rm=TRUE),mean_of=mean(offer,na.rm=TRUE),mean_opt_ac=mean(opt_accept,na.rm=TRUE) , mean_opt_re=mean(opt_reject,na.rm=TRUE), mean_feasible = mean(feasible,na.rm=TRUE))

# mean of ecceted and rejected offer
data  %>%filter(role==1, accept==1) %>% group_by(treatment)  %>% summarise(mean_of=mean(offer,na.rm=TRUE), mean_exp_payoff=mean(exp_payoff_pro,na.rm=TRUE))
data  %>%filter(role==1, accept==0) %>% group_by(treatment)  %>% summarise(mean_of=mean(offer,na.rm=TRUE), mean_exp_payoff=mean(exp_payoff_pro,na.rm=TRUE))





## regression shows that the round is significantly relevant
## For 15 first round:

#P1: alpha = 0.7 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==1&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#P2: alpha = 0.7 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==2&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#B3: alpha = 0.2 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==3&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#B4: alpha = 0.2 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==4&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")





## For 15 second round:

#P1: alpha = 0.7 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==1&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#P2: alpha = 0.7 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==2&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#B3: alpha = 0.2 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==3&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#B4: alpha = 0.2 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==4&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")




## Both

#P1: alpha = 0.7 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==1&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==1 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==1 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")


#P2: alpha = 0.7 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==2&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==2 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==2 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")




#B3: alpha = 0.2 and phi=1
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==3&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==3 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==3 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")



#B4: alpha = 0.2 and phi=1.5
ggplot() +
  geom_point() +
  stat_summary( data=subset(data, role==2 &  treatment==4&round<=15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round<=15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round<=15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round<=15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4&round>15) ,aes(x = round, y = initial, group=1), fun=mean, colour="red", geom="line",group=1) + 
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = initial),colour="red",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round>15) ,aes(x = round, y = offer, group=1), fun=mean, colour="blue", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = offer),colour="blue",se=FALSE,method=lm,linetype="dashed")+
  stat_summary( data=subset(data, role==2 &  treatment==4 &round>15) ,aes(x = round, y = final, group=1), fun=mean, colour="black", geom="line",group=1) +
  geom_smooth(data=subset(data, role==2 &  treatment==4 &round>15),aes(x = round, y = final),colour="black",se=FALSE,method=lm,linetype="dashed")












##########################################################################################################################################
#### Final position
##########################################################################################################################################


# for any $\phi$, the c_F are equal for $\alpha=0.2$ and $\alpha=0.7$, and different within $\phi$
ggplot(data , aes(x = treatment, y = final))  +   geom_boxplot()
anova <- aov(final~treatment, data = data)
summary(anova)




# test to confirm
rejected_P1 <- data %>% filter(accept==0, treatment==1,role==2)
rejected_P2 <- data %>% filter(accept==0, treatment==2 ,role==2)  
rejected_B3 <- data %>% filter(accept==0, treatment==3,role==2)
rejected_B4 <- data %>% filter(accept==0, treatment==4,role==2)  

wilcox.test(rejected_P1$final, rejected_P2$final, alternative = "two.sided")
wilcox.test(rejected_B3$final, rejected_B4$final, alternative = "two.sided") # same

wilcox.test(rejected_P1$final, rejected_B3$final, alternative = "two.sided")
wilcox.test(rejected_P2$final, rejected_B4$final, alternative = "two.sided")


# Histogram
#data %>% filter(role==2, phi==1, alpha==0.7, accept ==0) %>% ggplot(.,aes(x=final)) +  geom_histogram( bins = 30)

ggplot(data,aes(x=final)) +
  geom_histogram(data=subset(data, phi==1& alpha==0.7& role==2 & accept==0),fill = "red", alpha = 0.2, bins = 30) +
  geom_histogram(data=subset(data,phi==1& alpha==0.2& role==2&  accept==0),fill = "blue", alpha = 0.2, bins = 30)


# what affect? alpha and initial, as in the offer
reg_final <- data %>% filter(role==2, accept==0) %>% lm(final~ initial + alpha + phi+round+order + exp_payoff_pro_both + exp_payoff_res_both , data = .)
summary(reg_final)


# Optimal final vs final
data %>% filter(accept==0, role==2) %>%group_by(treatment)  %>% summarise( mean_final=mean(final), mean_opt_final=mean(opt_final) )

# Final vs Optimal final
data %>% filter(role==2,treatment==1, accept==0) %>% ggplot() +
  geom_histogram(aes(x=final),fill = "red", alpha = 0.2, bins = 50) +
  geom_histogram(aes(x=opt_final),fill = "blue", alpha = 0.2, bins = 50)

data %>% filter(role==2,treatment==2, accept==0) %>% ggplot() +
  geom_histogram(aes(x=final),fill = "red", alpha = 0.2, bins = 50) +
  geom_histogram(aes(x=opt_final),fill = "blue", alpha = 0.2, bins = 50)

data %>% filter(role==2,treatment==3, accept==0) %>% ggplot() +
  geom_histogram(aes(x=final),fill = "red", alpha = 0.2, bins = 20) +
  geom_histogram(aes(x=opt_final),fill = "blue", alpha = 0.2, bins = 20)

data %>% filter(role==2,treatment==4, accept==0) %>% ggplot() +
  geom_histogram(aes(x=final),fill = "red", alpha = 0.2, bins = 20) +
  geom_histogram(aes(x=opt_final),fill = "blue", alpha = 0.2, bins = 20)



# histograms: final as a function of optimal final
data %>% filter(treatment==1, role==2, accept==0) %>% ggplot(., aes(x = perc_final, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 30, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==2, role==2, accept==0) %>% ggplot(., aes(x =perc_final, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 30, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==3, role==2, accept==0) %>% ggplot(., aes(x = perc_final, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 30, fill = "blue", alpha = 0.2 )
data %>% filter(treatment==4, role==2, accept==0) %>% ggplot(., aes(x = perc_final, y = after_stat(count / sum(count) ))) + geom_histogram(bins = 30, fill = "blue", alpha = 0.2 )








########################## until here! what is below is not complete  ########################## 

##########################################################################################################################################
#### Payments (and optimality of the final position given the initial)
##########################################################################################################################################






# Summary: this is not very interesting,
# All
data %>%group_by(treatment, role)  %>% summarise(mean_payoff = mean(payoff) )
# Accepted
data %>% filter(accept==1) %>%group_by(treatment, role)  %>% summarise(mean_payoff = mean(payoff) )
# Rejected
data %>% filter(accept==0) %>%group_by(treatment, role)  %>% summarise(mean_payoff = mean(payoff) )






# Expected payoff
data %>%group_by(phi, alpha, role)  %>% summarise(mean_exp_payoff = mean(exp_payoff,na.rm=TRUE) )
# Accepted
data %>% filter(accept==1, role==1) %>%group_by(phi, alpha)  %>% summarise(mean_of = mean(offer)  ,mean_exp_payoff_pro = mean(exp_payoff_pro) ,mean_exp_payoff_res = mean(exp_payoff_res) )
# Rejected
data %>% filter(accept==0) %>%group_by(phi, alpha, role)  %>% summarise(mean_of=mean(offer) ,mean_exp_payoff = mean(exp_payoff) ,mean_exp_payoff_pro = mean(exp_payoff_pro), mean_final=mean(final), mean_opt_final=mean(opt_final) )


# optimal expected payoff vs offer (accepted and rejcted): no idea what to say
data %>% filter(role==1, phi==1, alpha==0.7) %>% ggplot(., aes(x=exp_payoff_pro, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==1, phi==1, alpha==0.2) %>% ggplot(., aes(x=exp_payoff_pro, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==1, phi==1.5, alpha==0.7) %>% ggplot(., aes(x=exp_payoff_pro, y=offer, color=factor(accept) )) +   geom_point()
data %>% filter(role==1, phi==1.5, alpha==0.2) %>% ggplot(., aes(x=exp_payoff_pro, y=offer, color=factor(accept) )) +   geom_point()


# The c_F elected for the proposer, generates a expected payoff to the responder different than is there is no threat.
# except for $\phi=1.5$ and $\alpha=0.2$
prueba <- data %>% filter(accept==0, alpha==0.7, phi==1,role==1)  
wilcox.test(prueba$exp_payoff,  mu = -28.75,  alternative = "two.sided")
#wilcox.test(prueba$exp_payoff_pro,  mu = -28.75,  alternative = "two.sided")

prueba <- data %>% filter(accept==0, alpha==0.7, phi==1.5,role==1)  
wilcox.test(prueba$exp_payoff,  mu = -31,  alternative = "two.sided")
#wilcox.test(prueba$exp_payoff_pro,  mu = -31,  alternative = "two.sided")

prueba <- data %>% filter(accept==0, alpha==0.2, phi==1,role==1)  
wilcox.test(prueba$exp_payoff,  mu = -28.75,  alternative = "two.sided")
#wilcox.test(prueba$exp_payoff_pro,  mu = -28.75,  alternative = "two.sided")

prueba <- data %>% filter(accept==0, alpha==0.2, phi==1.5,role==1)  
wilcox.test(prueba$exp_payoff,  mu = -31,  alternative = "two.sided")
#wilcox.test(prueba$exp_payoff_pro,  mu = -31,  alternative = "two.sided")


##################3
#check if there is some learning and stuff....

# initial
#alpha = 0.7 and phi=1
ggplot(data,aes(x = round, y = initial)) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1& order==1 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.7& phi==1& order==1 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1& order==2 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.7& phi==1& order==2 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)


#alpha = 0.7 and phi=1.5
ggplot(data,aes(x = round, y = initial)) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1.5& order==1 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.7& phi==1.5& order==1 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1.5& order==2 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.7& phi==1.5& order==2 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)


#alpha = 0.2 and phi=1
ggplot(data,aes(x = round, y = initial)) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1& order==2 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.2& phi==1& order==2 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1& order==1 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.2& phi==1& order==1 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)


#alpha = 0.2 and phi=1.5
ggplot(data,aes(x = round, y = initial)) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1.5& order==2 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.2& phi==1.5& order==2 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1.5& order==1 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.2& phi==1.5& order==1 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)




# offer
#alpha = 0.7 and phi=1
ggplot(data,aes(x = round, y = offer)) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1& order==1 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.7& phi==1& order==1 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1& order==2 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.7& phi==1& order==2 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)

#alpha = 0.7 and phi=1.5
ggplot(data,aes(x = round, y = offer)) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1.5& order==1 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.7& phi==1.5& order==1 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.7& phi==1.5& order==2 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.7& phi==1.5& order==2 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)


#alpha = 0.2 and phi=1
ggplot(data,aes(x = round, y = offer)) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1& order==2 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.2& phi==1& order==2 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1& order==1 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.2& phi==1& order==1 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)


#alpha = 0.2 and phi=1.5
ggplot(data,aes(x = round, y = offer)) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1.5& order==2 &round<=15) ,fill = "red", alpha = 0.2,) +
  stat_summary( data=subset(data, role==2 & alpha==0.2& phi==1.5& order==2 &round<=15) ,aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1) +
  geom_point(data=subset(data, role==2 & alpha==0.2& phi==1.5& order==1 &round>15) ,fill = "red", alpha = 0.2,)   +
  stat_summary(   data=subset(data, role==2 & alpha==0.2& phi==1.5& order==1 &round>15) ,     aes(y = initial, group=1), fun=mean, colour="red", geom="line",group=1)









####### appendix about the A/R ###############
#density of offer accepted
df <- data.frame(id=c(1:100),den_ahpl = c(1:100),den_ahph = c(1:100),den_alpl = c(1:100),den_alph = c(1:100) )

i <- 1
while (i <= 100) {
  df$den_ahpl[i] <- data %>% filter(offer<=i, phi==1, alpha==0.7) %>% summarise(mean_ac=mean(accept) )
  df$den_ahph[i] <- data %>% filter(offer<=i, phi==1.5, alpha==0.7) %>% summarise(mean_ac=mean(accept) )
  df$den_alpl[i] <- data %>% filter(offer<=i, phi==1, alpha==0.2) %>% summarise(mean_ac=mean(accept) )
  df$den_alph[i] <- data %>% filter(offer<=i, phi==1.5, alpha==0.2) %>% summarise(mean_ac=mean(accept) )
  i = i+1
}


ggplot(df, aes(x=id,y=as.numeric(den_ahpl))) +   geom_point()
ggplot(df, aes(x=id,y=as.numeric(den_ahph))) +   geom_point()
ggplot(df, aes(x=id,y=as.numeric(den_alpl))) +   geom_point()
ggplot(df, aes(x=id,y=as.numeric(den_alph))) +   geom_point() 
