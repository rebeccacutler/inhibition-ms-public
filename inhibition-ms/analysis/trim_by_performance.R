
data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/')
trial_summary = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/trial_summary_212.csv')
trials_data = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/results/trials_data.csv')

test = data[data$sender == 'test_stimulus' ,]
test$duration = as.numeric(test$duration)

subj_n = numeric()
subj_total = numeric()
subj_perf = numeric()
avg_RT = numeric()

n_subj = 212
n_correct = 27

subj_vec = unique(test$subjID)
# performance in test
for (s in 1:n_subj) {
  
  subj_n[s] = subj_vec[s]
  subj_total[s] = sum(as.logical(test$correct[test$subjID == subj_vec[s]]))
  subj_perf[s] = subj_total[s] / n_correct
  
  avg_RT[s] = mean(test$duration[test$subjID == subj_vec[s]])
  
}

subj_summary = data.frame(subj_n = subj_n,
                             subj_total = subj_total,
                             subj_perf = subj_perf,
                             avg_RT = avg_RT)

to_keep = unique(subj_summary$subj_n[subj_summary$subj_perf >= 0.59])
trial_summary = trial_summary[trial_summary$subj %in% to_keep ,]
trials_data = trials_data[trials_data$subjID %in% to_keep ,]

write.csv(trial_summary, '/Users/beckycutler/projects/iu-maxcey/E2/results/subjects_148/trial_summary_148.csv')
