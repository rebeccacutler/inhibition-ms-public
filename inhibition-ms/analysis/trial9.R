
###########################################
# trial 9 
# (old trial 4)
###########################################

trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trials_data_148.csv')

trial9 = trials_data[trials_data$trial_type == 4 ,]

serial_pos9 = trial9 %>%
  group_by(correct_object) %>%
  summarise(mean = mean(correct), sem = std.error(correct))

trial9$sp[trial9$correct_object == 'Dv1'] = 1
trial9$sp[trial9$correct_object == 'Dv3'] = 2
trial9$sp[trial9$correct_object == 'Dv5'] = 3
trial9$sp[trial9$correct_object == 'Dv7'] = 4
trial9$sp[trial9$correct_object == 'Dv9'] = 5
trial9$sp[trial9$correct_object == 'Dv10'] = 6

trial9$answer = 0
trial9$answer[trial9$response_info == 'single'] = 1


trial9$sp = as.factor(trial9$sp)

############################################
# early v late 1+2 and 5+6
############################################

early9 = trial9[trial9$sp == 1 | trial9$sp == 2 ,]
late9 = trial9[trial9$sp == 5 | trial9$sp == 6 ,]

subj_early = early9 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late9 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

t.test(subj_early$mean, subj_late$mean)

#t.test(early9$correct[early9,late9$correct, paired = TRUE)

############################################
# early v late 1+2+3 and 4+5+6
############################################

early9 = trial9[trial9$sp == 1 | trial9$sp == 2 | trial9$sp == 3 ,]
late9 = trial9[trial9$sp == 4 | trial9$sp == 5 | trial9$sp == 6 ,]

subj_early = early9 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late9 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

subj_early$stimuli = 'single'
subj_late$stimuli = 'single'

subj_early = rbind(subj_early,subj_early)
subj_early$stimuli[seq(149,296)] = 'single-foil'
subj_early$mean[seq(149,296)] = 1 - subj_early$mean[seq(149,296)]

subj_late = rbind(subj_late,subj_late)
subj_late$stimuli[seq(149,296)] = 'single-foil'
subj_late$mean[seq(149,296)] = 1 - subj_late$mean[seq(149,296)]

t.test(subj_early$mean[subj_early$stimuli=='single'], subj_early$mean[subj_early$stimuli=='single-foil'], paired = TRUE)
t.test(subj_late$mean[subj_late$stimuli=='single'], subj_late$mean[subj_late$stimuli=='single-foil'], paired = TRUE)

t.test(subj_early$mean[subj_early$stimuli=='single'], subj_late$mean[subj_late$stimuli=='single'])

#################################################
# logistic regression, correct ~ serial_position
#################################################

# Fit the model
model <- glm( correct ~ sp, data = trial9, family = binomial)

# Summarize the model
summary(model)

library(aod)
wald.test(b = coef(model), Sigma = vcov(model), Terms = 2:6)
