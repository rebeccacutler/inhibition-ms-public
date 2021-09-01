
###########################################
# trial 3
# (old trial 2.1)
###########################################

trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trials_data_148.csv')

trial3 = trials_data[trials_data$trial_subset1 == 2.1 | trials_data$trial_subset1 == 6.2 ,]
trial3$sp[trial3$object1 == 'B11' | trial3$object2 == 'B11'] = 1
trial3$sp[trial3$object1 == 'B114' | trial3$object2 == 'B114'] = 2

trial3$answer = 0
trial3$answer[trial3$response_object == 'B11'] = 1
trial3$answer[trial3$response_object == 'B114'] = 1

serial_pos3 = trial3 %>%
  group_by(sp) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

############################################
# early v late 1+2 and 3+4
############################################

early3 = trial4[trial3$sp == 1 ,]
late3 = trial4[trial3$sp == 2 ,]

subj_early = early3 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late3 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

subj_early$stimuli = 'long'
subj_late$stimuli = 'long'

subj_early = rbind(subj_early,subj_early)
subj_early$stimuli[seq(149,296)] = 'long-foil'
subj_early$mean[seq(149,296)] = 1 - subj_early$mean[seq(149,296)]

subj_late = rbind(subj_late,subj_late)
subj_late$stimuli[seq(149,296)] = 'long-foil'
subj_late$mean[seq(149,296)] = 1 - subj_late$mean[seq(149,296)]

t.test(subj_early$mean[subj_early$stimuli=='long'], subj_early$mean[subj_early$stimuli=='long-foil'])
t.test(subj_late$mean[subj_late$stimuli=='long'], subj_late$mean[subj_late$stimuli=='long-foil'])

t.test(subj_early$mean[subj_early$stimuli=='long'], subj_late$mean[subj_late$stimuli=='long'])

#######################################

trial_summary = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trial_summary_148.csv')

trial3 = trial_summary[trial_summary$trial == 2.1 | trial_summary$trial == 6.2 ,]
trial3$answer = 1
trial3$answer[str_detect(trial3$response_object,'BF')] = 0

long_mean = mean(trial3$proportion[trial3$stimulus=='long'])
long_foil_mean = mean(trial3$proportion[trial3$stimulus=='long-foil'])

t.test(trial3$proportion[trial3$stimulus=='long'],
       trial3$proportion[trial3$stimulus=='long-foil'], paired = TRUE)



