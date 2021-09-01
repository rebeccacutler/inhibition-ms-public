

###########################################
# trial 10
# (old trial 6.1)
###########################################

trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trials_data_148.csv')

trial10 = trials_data[trials_data$trial_subset1 == 6.1 ,]
trial10$sp[trial10$object1 == 'Dv2' | trial10$object2 == 'Dv2'] = 1
trial10$sp[trial10$object1 == 'Dv4' | trial10$object2 == 'Dv4'] = 2
trial10$sp[trial10$object1 == 'Dv6' | trial10$object2 == 'Dv6'] = 3
trial10$sp[trial10$object1 == 'Dv8' | trial10$object2 == 'Dv8'] = 4

trial10$answer = 0
trial10$answer[trial10$response_object == 'Dv2'] = 1
trial10$answer[trial10$response_object == 'Dv4'] = 1
trial10$answer[trial10$response_object == 'Dv6'] = 1
trial10$answer[trial10$response_object == 'Dv8'] = 1

serial_pos10 = trial10 %>%
  group_by(sp) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

############################################
# early v late 1+2 and 3+4
############################################

early10 = trial10[trial10$sp == 1 | trial10$sp == 2 ,]
late10 = trial10[trial10$sp == 3 | trial10$sp == 4 ,]

subj_early = early10 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late10 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

subj_early$stimuli = 'single'
subj_late$stimuli = 'single'

subj_early = rbind(subj_early,subj_early)
subj_early$stimuli[seq(149,296)] = 'long'
subj_early$mean[seq(149,296)] = 1 - subj_early$mean[seq(149,296)]
subj_early$time = 'early'

subj_late = rbind(subj_late,subj_late)
subj_late$stimuli[seq(149,296)] = 'long'
subj_late$mean[seq(149,296)] = 1 - subj_late$mean[seq(149,296)]
subj_late$time = 'late'

early_late = rbind(subj_early,subj_late)

t.test(subj_early$mean[subj_early$stimuli=='single'], subj_early$mean[subj_early$stimuli=='long'], paired = TRUE)
t.test(subj_late$mean[subj_late$stimuli=='single'], subj_late$mean[subj_late$stimuli=='long'], paired = TRUE)

t.test(subj_early$mean[subj_early$stimuli=='single'], subj_late$mean[subj_late$stimuli=='single'])

res.10 <- aov(mean ~ stimuli * time + Error(subjID / stimuli * time),
              data = early_late)

summary(res.10)
DescTools::EtaSq(res.10, type=1, anova = TRUE)
