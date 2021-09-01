
###########################################
# trial 5
# (old trial 2.2)
###########################################

trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trials_data_148.csv')

trial4 = trials_data[trials_data$trial_subset1 == 2.2 ,]
trial4$sp[trial4$object1 == 'C11' | trial4$object2 == 'C11'] = 1
trial4$sp[trial4$object1 == 'C14' | trial4$object2 == 'C14'] = 3
trial4$sp[trial4$object1 == 'C21' | trial4$object2 == 'C21'] = 2
trial4$sp[trial4$object1 == 'C24' | trial4$object2 == 'C24'] = 4

trial4$answer = 0
trial4$answer[trial4$response_object == 'C11'] = 1
trial4$answer[trial4$response_object == 'C14'] = 1
trial4$answer[trial4$response_object == 'C21'] = 1
trial4$answer[trial4$response_object == 'C24'] = 1

serial_pos4 = trial4 %>%
  group_by(sp) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

############################################
# early v late 1+2 and 3+4
############################################

early4 = trial4[trial4$sp == 1 | trial4$sp == 2 ,]
late4 = trial4[trial4$sp == 3 | trial4$sp == 4 ,]

subj_early = early4 %>%
  group_by(subjID) %>%
  summarise(mean = mean(answer), sem = std.error(answer))

subj_late = late4 %>%
  group_by(subjID) %>%
  summarise(mean=mean(answer), sem=std.error(answer))

subj_early$stimuli = 'short'
subj_late$stimuli = 'short'

subj_early = rbind(subj_early,subj_early)
subj_early$stimuli[seq(149,296)] = 'short-foil'
subj_early$mean[seq(149,296)] = 1 - subj_early$mean[seq(149,296)]

subj_late = rbind(subj_late,subj_late)
subj_late$stimuli[seq(149,296)] = 'short-foil'
subj_late$mean[seq(149,296)] = 1 - subj_late$mean[seq(149,296)]

t.test(subj_early$mean[subj_early$stimuli=='short'], subj_early$mean[subj_early$stimuli=='short-foil'])
t.test(subj_late$mean[subj_late$stimuli=='short'], subj_late$mean[subj_late$stimuli=='short-foil'])

t.test(subj_early$mean[subj_early$stimuli=='short'], subj_late$mean[subj_late$stimuli=='short'])
