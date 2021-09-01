detach("package:plyr", unload=TRUE)

# prac v no-prac (early)
trial1.3 = trials_data[trials_data$trial_subset2 == 1.3 ,] %>%
  group_by(subjID, exp) %>%
  summarise(n = plyr::count(response_info))

trial1.3$proportion = 1
#trial1.3$proportion[trial1.3$n$x == 'no-prac'] = 0 



# prac v no-prac (late)
trial1.4 = trials_data[trials_data$trial_subset2 == 1.4 ,] %>%
  group_by(subjID, exp) %>%
  summarise(n = plyr::count(response_info))

trial1.4$proportion = 1
#trial1.4$proportion[trial1.3$n$x == 'no-prac'] = 0 


trial_subj = c(trial1.3$subjID,
               trial1.4$subjID)

trial_number = c(rep(1.3,nrow(trial1.3)),
                 rep(1.4,nrow(trial1.4)))

trial_exp = c(trial1.3$exp,
              trial1.4$exp)


trialsNs = c(trial1.3$n[,2],
             trial1.4$n[,2])

trial_info = c(trial1.3$n[,1],
               trial1.4$n[,1])

trial_prop = c(trial1.3$proportion,
               trial1.4$proportion)

trial_summary_subset = data.frame(subj = trial_subj,
                                  condition = trial_exp,
                                  trial = trial_number,
                                  stimulus = trial_info,
                                  count = trialsNs,
                                  proportion = trial_prop)



to_add = trial_summary_subset[trial_summary_subset$proportion == 1 ,]

to_add$count = 0
to_add$proportion = 0
to_add2 = trial_summary[trial_summary$proportion == 1 ,]

to_add2 = trial_summary_subset[trial_summary_subset$proportion == 1 ,]

to_add$stimulus[to_add2$trial == 1.3 & to_add2$stimulus == 'prac'] = 'related'
to_add$stimulus[to_add2$trial == 1.3 & to_add2$stimulus == 'related'] = 'prac'

to_add$stimulus[to_add2$trial == 1.4 & to_add2$stimulus == 'prac'] = 'related'
to_add$stimulus[to_add2$trial == 1.4 & to_add2$stimulus == 'related'] = 'prac'

trial_summary_subset = rbind(trial_summary_subset, to_add)

trial_summary_subset$condition[trial_summary_subset$condition == 0] = 'study'
trial_summary_subset$condition[trial_summary_subset$condition == 1] = 'monitor'
trial_summary_subset$condition[trial_summary_subset$condition == 2] = '3phase'

trial_summary_subset$condition = as.factor(trial_summary_subset$condition)

trial1_summary_subset = trial_summary_subset[
  with(trial_summary_subset, order(subj, trial)),
  ]

