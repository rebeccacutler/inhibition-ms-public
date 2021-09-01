
###########################################
# trial 5
# (old trial 2.3)
###########################################

trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trials_data_148.csv')

trial5 = trials_data[trials_data$trial_subset1 == 2.3 ,]

serial_pos5 = trial5 %>%
  group_by(sp) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

############################################
# early v late 1+2 and 3+4
############################################

early5 = trial5[trial5$sp == 1 | trial5$sp == 2 ,]
late5 = trial5[trial5$sp == 3 | trial5$sp == 4 ,]

subj_early = early5 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late5 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

t.test(subj_early$mean[subj_early$stimuli=='short-list'], subj_early$mean[subj_early$stimuli=='long-list'])
t.test(subj_late$mean[subj_late$stimuli=='short-list'], subj_late$mean[subj_late$stimuli=='long-list'])

t.test(subj_early$mean[subj_early$stimuli=='short-list'], subj_late$mean[subj_late$stimuli=='short-list'])
