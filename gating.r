# gating experiment

library(ggsignif)
library(lattice)
library(lme4)
library(lmerTest)
library(plyr)
library(tidyverse)
library(emmeans)
library(brms) # bayesian
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(bayestestR)
library(tidybayes)
library(ordinal)
library(rjags)
library(BayesFactor)


#source('U:/owncloud/Behavioural_results/functions.R')
source("/Users/wycarol/ownCloud/Behavioural_results/functions.R")

options(mc.cores=4)

#setwd("U:/owncloud/manuscripts/gating/results")
#filePathOut <-"U:/owncloud/manuscripts/gating/results"

setwd("/Users/wycarol/ownCloud/manuscripts/gating/results")
filePathOut <- "/Users/wycarol/ownCloud/manuscripts/gating/results"

# read in long format data
df <- read.csv("gating_data.csv", head = TRUE, stringsAsFactors = FALSE)
dim(df) 

# get included subjects/items
length(unique(df$subject)) # number of subjects

# subset responses
df_response <- df[df$word1Type == "target" | df$word1Type == "lure",]

## remove outliers 617, 684, 93, 87
df_response <- df_response[df_response$subject != 617 & 
                             df_response$subject != 684 & 
                             df_response$subject != 93 &
                             df_response$subject != 87,]

# response accuracy
df_response$correct <- 0 # incorrect
df_response$correct[df_response$word1Type == "target" & df_response$key_press == 49] <- 1 # correct
df_response$correct[df_response$word1Type == "target" & df_response$key_press == 50] <- 1 # correct
df_response$correct[df_response$word1Type == "target" & df_response$key_press == 51] <- 1 # correct
df_response$correct[df_response$word1Type == "lure" & df_response$key_press == 52] <- 1 # correct
df_response$correct[df_response$word1Type == "lure" & df_response$key_press == 53] <- 1 # correct
df_response$correct[df_response$word1Type == "lure" & df_response$key_press == 54] <- 1 # correct

table(df_response$correct) # proportion correct

# response score 
df_response$score <- NA
df_response$score[df_response$word1Type == "target" & df_response$key_press == 49] <- 1
df_response$score[df_response$word1Type == "target" & df_response$key_press == 50] <- 2
df_response$score[df_response$word1Type == "target" & df_response$key_press == 51] <- 3
df_response$score[df_response$word1Type == "target" & df_response$key_press == 52] <- 4
df_response$score[df_response$word1Type == "target" & df_response$key_press == 53] <- 5
df_response$score[df_response$word1Type == "target" & df_response$key_press == 54] <- 6

df_response$score[df_response$word1Type == "lure" & df_response$key_press == 49] <- 6
df_response$score[df_response$word1Type == "lure" & df_response$key_press == 50] <- 5
df_response$score[df_response$word1Type == "lure" & df_response$key_press == 51] <- 4
df_response$score[df_response$word1Type == "lure" & df_response$key_press == 52] <- 3
df_response$score[df_response$word1Type == "lure" & df_response$key_press == 53] <- 2
df_response$score[df_response$word1Type == "lure" & df_response$key_press == 54] <- 1

table(df_response$score) 

# add item column
df_response$item <- NA
df_response$item[df_response$word1Type == "target"] <- df_response$word1[df_response$word1Type == "target"]
df_response$item[df_response$word1Type == "lure"] <- df_response$word2[df_response$word1Type == "lure"]



## subset practice
df_practice <- df_response[df_response$word1 == "bungle" |
                             df_response$word1 == "proton" |  
                             df_response$word1 == "caper" |
                             df_response$word1 == "doughty" |
                             df_response$word1 == "Whitsun" |
                             df_response$word1 == "gable" |
                             df_response$word1 == "grumble" |
                             df_response$word1 == "sabre",]
table(df_practice$correct)
table(df_practice$score) 


## subset fillers
df_filler <- df_response[df_response$condition == 'filler',]
table(df_filler$correct)
table(df_filler$score) 
#check error rate by subject, should be very low
acc_filler <- df_filler %>% group_by(version, subject) %>% 
  summarise(numCorrect = sum(correct),
            numIncorrect = length(correct) - sum(correct),
            errorRate = numIncorrect/length(correct))
acc_filler
plot(acc_filler$errorRate)


mean_filler = mean(acc_filler$errorRate)
upper_filler = 2*sd(acc_filler$errorRate) + mean_filler
#outlier 617 


rating_filler <- df_filler %>% group_by(version, subject) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score)),
            n = n()
  )
rating_filler  
plot(rating_filler$meanScore)
#617, 641 most guesses


#check the number of guesses on fillers
rating_count <- df_filler %>% group_by(subject,score) %>%
  summarise(n=n())

guess_count <- rating_count[rating_count$score == 3 | rating_count$score == 4,]



## subset main trials
df_nonPractice <- df_response %>% anti_join(df_practice)
df_main <- df_nonPractice %>% anti_join(df_filler)
table(df_main$correct)
table(df_main$score) 

## subset full trials
df_full <- df_main[df_main$condition == "full",]
table(df_full$correct)
table(df_full$score) 

#check error rate by subject, should be very low
acc_full <- df_full %>% group_by(version, subject) %>% 
  summarise(numCorrect = sum(correct),
            numIncorrect = length(correct) - sum(correct),
            errorRate = numIncorrect/length(correct))
acc_full
plot(acc_full$errorRate)

mean_full = mean(acc_full$errorRate)
upper_full = 2*sd(acc_full$errorRate) + mean_full
#outlier 684, 87


rating_full <- df_full %>% group_by(version, subject) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score))
  )
rating_full 


#check the number of guesses on full
rating_count_full <- df_full %>% group_by(subject,score) %>%
  summarise(n=n())

guess_count_full <- rating_count_full[rating_count_full$score == 3 | rating_count_full$score == 4,]


#check rts
rt_sub <- df_nonPractice %>% group_by(version, subject, condition) %>% 
  summarise(meanRT = mean(rt),
            sd = sd(rt),
            n = n(),
            se = sd/sqrt(n))

#93 rt too fast


#subset each problematic item pair DP trials
DP_mor <- df_main[(df_main$item == "mortal" | df_main$item == "mortar") & df_main$condition == 'DP',]
DP_cyc <- df_main[(df_main$item == "cyclist" | df_main$item == "psychic") & df_main$condition == 'DP',]
DP_pew <- df_main[(df_main$item == "pewter" | df_main$item == "putrid") & df_main$condition == 'DP',]
DP_foy <- df_main[(df_main$item == "foyer" | df_main$item == "foible") & df_main$condition == 'DP',]
DP_loc <- df_main[(df_main$item == "local" | df_main$item == "locust") & df_main$condition == 'DP',]
DP_sher <- df_main[(df_main$item == "sherry" | df_main$item == "sheriff") & df_main$condition == 'DP',]
DP_tick <- df_main[(df_main$item == "ticket" | df_main$item == "tickle") & df_main$condition == 'DP',]
DP_gal <- df_main[(df_main$item == "gallery" | df_main$item == "galaxy") & df_main$condition == 'DP',]
DP_tee <- df_main[(df_main$item == "teeter" | df_main$item == "teeny") & df_main$condition == 'DP',]
DP_axi <- df_main[(df_main$item == "axiom" | df_main$item == "accent") & df_main$condition == 'DP',]
DP_cryst <- df_main[(df_main$item == "crystal" | df_main$item == "christen") & df_main$condition == 'DP',]
DP_del <- df_main[(df_main$item == "delicate" | df_main$item == "delegate") & df_main$condition == 'DP',]
DP_flu <- df_main[(df_main$item == "fluid" | df_main$item == "fluent") & df_main$condition == 'DP',]
DP_fran <- df_main[(df_main$item == "franchise" | df_main$item == "frantic") & df_main$condition == 'DP',]
DP_tri <- df_main[(df_main$item == "trifle" | df_main$item == "tripod") & df_main$condition == 'DP',]
DP_vol <- df_main[(df_main$item == "volume" | df_main$item == "volley") & df_main$condition == 'DP',]


#remove problematic items (16 items with inconsistent DP)
df_main <- df_main[df_main$item != "mortal" & df_main$item != "mortar" &
                     df_main$item != "cyclist" & df_main$item != "psychic" &
                     df_main$item != "pewter" & df_main$item != "putrid" &
                     df_main$item != "foyer" & df_main$item != "foible" &
                     df_main$item != "local" & df_main$item != "locust" &
                     df_main$item != "sherry" & df_main$item != "sheriff" &
                     df_main$item != "ticket" & df_main$item != "tickle" &
                     df_main$item != "gallery" & df_main$item != "galaxy" &
                     df_main$item != "teeter" & df_main$item != "teeny" &
                     df_main$item != "axiom" & df_main$item != "accent" &
                     df_main$item != "crystal" & df_main$item != "christen" &
                     df_main$item != "delicate" & df_main$item != "delegate" &
                     df_main$item != "fluid" & df_main$item != "fluent" &
                     df_main$item != "franchise" & df_main$item != "frantic" &
                     df_main$item != "trifle" & df_main$item != "tripod" &
                     df_main$item != "volume" & df_main$item != "volley",]


## subset DP
df_DP <- df_main[df_main$condition == "DP",]
table(df_DP$correct)
table(df_DP$score) 

#rt
rt_sub_DP <- df_DP %>% group_by(version, subject) %>% 
  summarise(meanRT = mean(rt),
            sd = sd(rt),
            n = n(),
            se = sd/sqrt(n))

plot(rt_sub_DP$meanRT)

summary(df_DP$rt)
boxplot(df_DP$rt ~ df_DP$subject)
hist(df_DP$rt, breaks = 100)


#rating
rating_count_DP <- df_DP %>% group_by(version, subject, score) %>%
  summarise(n = n() )
  
ex_DP <- df_DP[df_DP$key_press == 49 | df_DP$key_press == 54,]
ex_item <- ex_DP %>% group_by(item) %>%
  summarise(n = n())

#df_DP <- df_DP %>% anti_join(ex_DP)

rating_count_DP_item <- df_DP %>% group_by(item, version, score) %>%
  summarise(n = n())

acc_DP_item <- df_DP %>% group_by(item, version) %>%
  summarise(numCorrect = sum(correct),
            numIncorrect = length(correct) - sum(correct),
            errorRate = numIncorrect/length(correct))



##order conditions
df_main$condition <- factor(df_main$condition, c("DP","DP_75", "DP_150", "DP_225", "DP_300", "full"))
df_main$version <- factor(df_main$version, c(1, 2))

#accuracy by subject 
acc_sub <- df_main %>% group_by(condition, version, subject) %>% 
  summarise(numCorrect = sum(correct),
            numIncorrect = length(correct) - sum(correct),
            errorRate = numIncorrect/length(correct))
acc_sub

#check error rate by group
acc_group <- acc_sub %>% group_by(condition, version) %>%
  summarise(error_rate = mean(errorRate),
            N = length(unique(subject)),
            SD = sd(errorRate, na.rm = TRUE),
            SE = SD/sqrt(N),
            ci = qnorm(0.975)*SE
  )
acc_group

#plot
ggplot(acc_group, aes(x = condition, y = error_rate, group = version, fill = version)) +
  geom_bar(stat = "identity", width = 0.8, color= "black", position = position_dodge(0.9)) +
  scale_fill_brewer(palette="Greys") +
  geom_text(aes(label = round(error_rate, digits=3)), vjust=2.5, color="black", size=3.5, position=position_dodge(.9))+
  scale_x_discrete("Groups") +
  scale_y_continuous("Error rate", 
                     limits = c(0,0.6), breaks = seq(0,.6,.1)) + 
  #coord_cartesian(ylim = c(0,0.6)) + 
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position='none',
    axis.line.x = element_line(color="white", size = 1),
    axis.line.y = element_line(color="white", size = 1)
  )+
  geom_errorbar(aes(ymin = error_rate - ci, ymax = error_rate + ci), width = 0,  size=0.5,
                position=position_dodge(.9)) 



##rating by subject
rating_sub <- df_main %>% group_by(condition, version, subject) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score))
  )
rating_sub

#rating by group
rating_group <- rating_sub %>% group_by(condition, version) %>%
  summarise(N = length(unique(subject)),
            mean_rating = mean(meanScore),
            SD = sd(meanScore, na.rm = TRUE),
            SE = SD/sqrt(N),
            ci = qnorm(0.975)*SE
  )
rating_group

#plot
ggplot(rating_group, aes(x = condition, y = mean_rating, group=version, fill = version)) +
  geom_bar(stat = "identity", width = 0.8, color= "black", position = position_dodge(0.9))+
  scale_fill_brewer(palette="Greys") +
  geom_text(aes(label = round(mean_rating, digits=3)), vjust=2.5, color="black", size=3.5, position=position_dodge(.9))+
  scale_x_discrete("Groups") +
  scale_y_continuous("Mean Score", breaks = seq(0,6,0.5)) + 
  coord_cartesian(ylim = c(1,4.5)) + 
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position='none',
    axis.line.x = element_line(color="white", size = 1),
    axis.line.y = element_line(color="white", size = 1)
  )+
  geom_errorbar(aes(ymin = mean_rating - ci, ymax = mean_rating + ci), width = 0,  size=.5,   # Thinner lines
                position=position_dodge(.9)
                )


##get mean rating at each alignment point
#grand mean
rating <- df_main %>% group_by(condition) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score))
  )

rating <- filter(rating, condition != 'full')
rating

#item mean
rating_item <- df_main %>% group_by(item, condition) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score))
  )
rating_item <- filter(rating_item, condition != 'full')

#plot
ggplot(data=rating, aes(x=condition, y=meanScore, group=1)) +
  #geom_line(data=rating_item, aes(group=item), color='grey', size=.1)+
  geom_line(size=0.7)+
  #geom_point()+
  geom_hline(yintercept = 3.5, linetype='dashed')+
  annotate("text", label = "Guessing", x=1.5, y=3.6)+
  geom_hline(yintercept = 6, linetype='dotted')+
  annotate("text", label = "Incorrect and confident", x=1.5, y=5.9)+
  geom_hline(yintercept = 1, linetype='dotted')+
  annotate("text", label = "Correct and confident", x=1.5, y=1.1)+
  scale_x_discrete("Alignment Point") +
  scale_y_continuous("Mean Rating", breaks = seq(0,6,1)) + 
  coord_cartesian(ylim = c(1,6)) + 
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position='none'
  )+
  geom_errorbar(aes(ymin = meanScore - seScore, ymax = meanScore + seScore), width=0.1, size=0.5   # Thinner lines
  )
  

#check the difference rating between two neighboring alignment point
rating_item <- group_by(rating_item, item) %>% 
  mutate(lagScore = lag(meanScore),
         diff_rating = meanScore - lagScore) 
  

   

##function to get bayes factor for chance level accuracy at each alignment point
bayes_acc <- function(df,rscale){
  acc_sub <- df %>% group_by(condition, version, subject) %>% 
    summarise(N = length(correct),
              numCorrect = sum(correct),
              numIncorrect = N - sum(correct),
              errorRate = numIncorrect/length(correct))
  all_correct <- sum(acc_sub$numCorrect)
  all_incorrect <- sum(acc_sub$numIncorrect)
  cat(all_correct, all_incorrect, "\n")
  1/proportionBF(y = all_correct, N = length(df$item), p = 0.5, rscale = rscale)
}   

## subset for other alignment points
df_DP_75 <- df_main[df_main$condition == "DP_75",]
df_DP_150 <- df_main[df_main$condition == "DP_150",]
df_DP_225 <- df_main[df_main$condition == "DP_225",]
df_DP_300 <- df_main[df_main$condition == "DP_300",]

#get bayes factor
bf_DP <- bayes_acc(df_DP,1)
round(as.numeric(as.vector(bf_DP)))
# Bayes factor analysis
# --------------
#   [1] Null, p=0.5 : 28.71335 ±0.03%
# 
# Against denominator:
#   Alternative, p0 = 0.5, r = 1, p =/= p0 
# ---
#   Bayes factor type: BFproportion, logistic
# 1464 1416 
bf_DP_75 <- bayes_acc(df_DP_75, 1)
as.numeric(as.vector(bf_DP_75))
#[1] Null, p=0.5 : 6.072489e-451 ±0%
#2510 270 
bf_DP_150 <- bayes_acc(df_DP_150,1)
as.numeric(as.vector(bf_DP_150))
#[1] Null, p=0.5 : 1.428288e-564 ±0%
#2301 79 
bf_DP_225 <- bayes_acc(df_DP_225,1)
as.numeric(as.vector(bf_DP_225))
#[1] Null, p=0.5 : 5.012967e-353 ±0%
#1351 29
bf_DP_300 <- bayes_acc(df_DP_300,1)
as.numeric(as.vector(bf_DP_300))
#[1] Null, p=0.5 : 1.906217e-117 ±0%
#417 3 

#The above method is only appropriate when the assumption of independence of observation is met, hence not suitable for the repeated measures design.
#Instead, use mixed effect logistic regression model for accuracy
priors <- c(prior(normal(8, 4), class = b),          
            prior(normal(0, 2), class = sd))  

mod <- brm(correct ~ 0 + Intercept +
             (0 + Intercept | subject) +
             (0 + Intercept | item),
           data = df_DP_300,
           prior = priors,
           family = bernoulli(link = "logit"),
           #iter = 10000, warmup = 2000, chains = 4,
           cores = getOption("mc.cores"),
           sample_prior = "yes",
           save_pars = save_pars(all = TRUE),
           control = list(adapt_delta = 0.95),
           seed = 1
           #file="bayes_proportion_DP_300_100"
)
print(summary(mod), digits=3)
#DP b-normal(0,2), sd-normal(0,2)
# Population-Level Effects: 
#            Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    0.040     0.076   -0.109    0.195 1.001     3075     3083


#DP_75 b-normal(2,1), sd-normal(0,2)
# Population-Level Effects: 
#            Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    3.406     0.221    2.989    3.853 1.000     1982     2682


#DP_150 b-normal(4,2), sd-normal(0,2)
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    6.258     0.556    5.242    7.411 1.001     1331     2206


#DP_225 b-normal(6,3), sd-normal(0,2)
#Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    7.393     1.025    5.648    9.716 1.004     1603     2347


#DP_300 b-normal(8,4), sd-normal(0,2)
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept    8.043     1.883    4.992   12.323 1.001     1728     1936


## check bayes factor
h <- hypothesis(mod, 'Intercept=0')
#DP
# Hypothesis Tests for class b:
#        Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0     0.04      0.08    -0.11      0.2      23.04      0.96     
# ---
#   'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
# '*': For one-sided hypotheses, the posterior probability exceeds 95%;
# for two-sided hypotheses, the value tested against lies outside the 95%-CI.
#Posterior probabilities of point hypotheses assume equal prior probabilities.

#DP_75
# Hypothesis Tests for class b:
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0     3.41      0.22     2.99     3.85          0         0    *

h$hypothesis$Evid.Ratio
#[1] 5.98008e-17

#DP_150
#        Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0     6.26      0.56     5.24     7.41          0         0    *

#-5.245827e-23


#DP_225
# Hypothesis Tests for class b:
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0     7.39      1.02     5.65     9.72          0         0    *

#6.55332e-16



#DP_300
# Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0     8.04      1.88     4.99    12.32          0         0    *
#1.893054e-16


plot(h)
plot(mod)
pp_check(mod)





bf <- matrix(c(23.04,0.001,0.001,0.001,0.001),ncol=1,byrow=TRUE)
condition <- c("DP","DP_75","DP_150","DP_225","DP_300")
bf <- data.frame(condition,bf)
bf$condition <- factor(bf$condition, c("DP","DP_75", "DP_150", "DP_225", "DP_300"))
#plot
ggplot(data=bf, aes(x=condition, y=bf, group=1)) +
  geom_line(size=0.7)+
  geom_point()+
  geom_hline(yintercept = 1, linetype='dashed')+
  geom_hline(yintercept = 10, linetype='dotted')+
  annotate("text", label = "Strong Evidence for H0 (Chance-level Accuracy)", x=4, y=12)+
  geom_hline(yintercept = 1/10, linetype='dotted')+
  annotate("text", label = "Strong Evidence for H1", x=4, y=0.08)+
  scale_x_discrete("Alignment Point") +
  scale_y_continuous("Bayes Factor", 
                     trans='log10', 
                     breaks = c(0, 1/100, 1/30, 1/10, 1/3, 1, 3, 10, 30, 100),
                     limits = c(0.001,100),
                     labels = c("0", "1/100", "1/30", "1/10", "1/3", "1", "3", '10', '30', '100')) + 
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position='none'
  )




all_correct <- length(df_DP[df_DP$correct == 1,]$item)
binom.test(all_correct, length(df_DP$item), p = 0.5,
           alternative = c("greater"),
           conf.level = 0.95)

# Exact binomial test
# 
# data:  all_correct and length(df_DP$item)
# number of successes = 1464, number of trials = 2880, p-value = 0.1906
# alternative hypothesis: true probability of success is greater than 0.5
# 95 percent confidence interval:
#   0.4928356 1.0000000
# sample estimates:
# probability of success 
# 0.5083333 



##bayes factor calculation for problematic items
bayesFun <- function(df){
  bf <- 1/proportionBF(y = sum(df$correct), N = length(df$correct), p = 0.5, rscale = 1)
  return(bf)  
} 

dfs <- list(DP_axi, DP_cryst, DP_cyc, DP_del, DP_flu, DP_foy, DP_fran, DP_gal,
         DP_loc, DP_mor, DP_pew, DP_sher, DP_tee, DP_tick, DP_tri, DP_vol)

for(df in dfs){
  num <- round(as.numeric(as.vector(bayesFun(df))),3)
  cat(unique(df$item), num, "\n")
}

## rscale 1
# accent axiom 0.776 
# crystal christen 0.311 
# cyclist psychic 0 
# delicate delegate 2.523 
# fluid fluent 2.523 
# foyer foible 0.097 
# franchise frantic 1.553 
# gallery galaxy 1.553 
# local locust 0.023 
# mortar mortal 0.004 
# pewter putrid 0.004 
# sherry sheriff 0.023 
# teeny teeter 0.097 
# ticket tickle 0.097 
# trifle tripod 3.7 
# volume volley 3.364 



logit <- function(p){log(p/(1-p))}

##plot the distribution of alternative hypothesis with different rscales
x = seq(-5,5, length=1000)
plot(x, dlogis(x, 0, 1), ylab="density", type ="l", col=5)
lines(x, dlogis(x, 0, 2), type ="l", col=4)
lines(x, dlogis(x, 0, 3), col=3) 
lines(x, dlogis(x, 0, 5), col=2) 
lines(x, dlogis(x, 0, 10), col=1) 
legend(3, 0.25, c("logis(0,1)","logis(0,2)","logis(0,3)", "logis(0,5)", "logis(0,10)"),lty=c(1,1,1,1,1),col=c(5,4,3,2,1))




## rating
rating_sub_DP <- df_DP %>% group_by(version, subject) %>%
  summarise(meanScore = mean(score, na.rm = TRUE),
            sdScore = sd(score, na.rm = TRUE),
            seScore = sdScore / sqrt(length(score))
  )
rating_sub_DP


##rating at DP
df_DP$rating_diff = df_DP$score - 3.5
priors <- c(prior(normal(0, 1), class = b),          
                 prior(normal(0, 1), class = sd),
                 prior(normal(0, 1), class = sigma))  

mod <- brm(rating_diff ~ 0 + Intercept +
             (0 + Intercept | subject) +
             (0 + Intercept | item),
           data = df_DP,
          prior = priors,
          #iter = 10000, warmup = 2000, chains = 4,
          cores = getOption("mc.cores"),
          sample_prior = "yes",
          save_pars = save_pars(all = TRUE),
          control = list(adapt_delta = 0.95),
          seed = 1,
          file="bayes_rating_DP"
)
print(summary(mod), digits=3)

# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept   -0.019     0.041   -0.099    0.061 1.002     2156     2602 

pp_check(mod) + theme_classic(base_size=20)
h <- hypothesis(mod, 'Intercept=0')

# Hypothesis Tests for class b:
#        Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0    -0.02      0.04     -0.1     0.06      21.79      0.96     
# ---
#   'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
# '*': For one-sided hypotheses, the posterior probability exceeds 95%;
# for two-sided hypotheses, the value tested against lies outside the 95%-CI.
# Posterior probabilities of point hypotheses assume equal prior probabilities.

plot(h,theme=theme_bw(20)) 
  
p <- plot(h, plot = F, theme = theme_bw(20))[[1]]
p + scale_y_continuous(limits = c(0,10))



# priors <- c(prior(normal(3.5, 1.5), class = Intercept),  
#             prior(normal(3.5, 1.5), class = b),
#             prior(normal(0, 1), class = sd)
#            )  
# 
# mod <- brm(score ~ 1 + Intercept +
#              (1 | subject) +
#              (1 | item),
#            data = df_DP,
#            prior = priors,
#            family = cumulative("probit"),
#            #iter = 10000, warmup = 2000, chains = 4,
#            cores = getOption("mc.cores"),
#            sample_prior = "yes",
#            save_pars = save_pars(all = TRUE),
#            control = list(adapt_delta = 0.95),
#            seed = 1,
#            file="bayes_rating_DP_ordinal"
# )
# print(summary(mod), digits=3)
# 
# pp_check(mod) + theme_classic(base_size=20)
# h <- hypothesis(mod, 'Intercept=3.5')
# 
# p <- plot(h, plot = F, theme = theme_bw(20))[[1]]
# p + scale_y_continuous(limits = c(0,10))





#difference between ratings at DP_75 and 2
df_DP_75$rating_diff = df_DP_75$score - 2

priors <- c(prior(normal(0, 1), class = b),          
            prior(normal(0, 1), class = sd),
            prior(normal(0, 1), class = sigma)) 

mod_75 <- brm(rating_diff ~ 0 + Intercept +
             (0 + Intercept | subject) +
             (0 + Intercept | item),
           data = df_DP_75,
           prior = priors,
           #iter = 10000, warmup = 2000, chains = 4,
           cores = getOption("mc.cores"),
           sample_prior = "yes",
           save_pars = save_pars(all = TRUE),
           control = list(adapt_delta = 0.95),
           seed = 1,
           file="bayes_rating_DP_75"
)
print(summary(mod_75), digits=3)

# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept   -0.091     0.081   -0.252    0.073 1.004     1111     1986


h <- hypothesis(mod_75, 'Intercept=0')

# Hypothesis Tests for class b:
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0    -0.09      0.08    -0.25     0.07       6.07      0.86     
# ---
#   'CI': 90%-CI for one-sided and 95%-CI for two-sided hypotheses.
# '*': For one-sided hypotheses, the posterior probability exceeds 95%;
# for two-sided hypotheses, the value tested against lies outside the 95%-CI.
# Posterior probabilities of point hypotheses assume equal prior probabilities.

plot(h)




#difference between ratings at DP_150 and 2
df_DP_150$rating_diff = df_DP_150$score - 2

priors <- c(prior(normal(0, 1), class = b),          
            prior(normal(0, 1), class = sd),
            prior(normal(0, 1), class = sigma)) 

mod_150 <- brm(rating_diff ~ 0 + Intercept +
                (0 + Intercept | subject) +
                (0 + Intercept | item),
              data = df_DP_150,
              prior = priors,
              #iter = 10000, warmup = 2000, chains = 4,
              cores = getOption("mc.cores"),
              sample_prior = "yes",
              save_pars = save_pars(all = TRUE),
              control = list(adapt_delta = 0.95),
              seed = 1,
              file="bayes_rating_DP_150"
)
print(summary(mod_150), digits=3)

# Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI  Rhat Bulk_ESS Tail_ESS
# Intercept   -0.707     0.046   -0.795   -0.617 1.001     1060     1808

# 
h <- hypothesis(mod_150, 'Intercept=0')
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
# 1 (Intercept) = 0    -0.71      0.05     -0.79   -0.62          0         0    *

1/h$hypothesis$Evid.Ratio
#7.752823e+20




## get average rating score for each item at each alignment point
mean_rating <- df_main %>% group_by(item,condition) %>%
  summarise(mean_rating = mean(score))

#select item condition with the first under 2 rating
recognition_point <- mean_rating %>% 
  mutate(order_condition = case_when(condition == 'DP' ~ 1,
                                       condition == 'DP_75' ~ 2,
                                       condition == 'DP_150' ~ 3,
                                       condition == 'DP_225' ~ 4,
                                       condition == 'DP_300' ~ 5,
                                       condition == 'full' ~ 6)) %>%
  group_by(item) %>% 
  arrange(order_condition, .by_group = TRUE) %>%
  filter(mean_rating < 2) %>%
  mutate(r = row_number()) %>%
  filter(r == 1)
 
recognition_count <- recognition_point %>% group_by(condition) %>% count() 

#reorder
recognition_count$condition <- factor(recognition_count$condition, c('DP', 'DP_75', 'DP_150', 'DP_225', 'DP_300', 'full'))

#plot the recognition point distribution 
ggplot(recognition_count, aes(x = condition, y = n, fill = condition)) + 
  geom_bar(stat = "identity", width = 0.8, color= "black", position = position_dodge(0.9)) + 
  scale_fill_brewer(palette="Greys") +
  #geom_text(aes(label = round(errorRate, digits=3)), vjust=1.6, color="black", size=3.5)+
  scale_x_discrete("Recognition Point") +
  scale_y_continuous("Number of Items", 
                     limits = c(0,200), breaks = seq(0,200,50)) + 
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position='none')
