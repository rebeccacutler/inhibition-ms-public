

library(rstatix)

# read in data
data2 = read.csv('/Users/beckycutler/projects/iu-maxcey/E2/final/trial_summary.csv')

# ANOVA of trials with correct answers:
# 1, 2, 3, 4, 10, 11
# n_trials = 3, 2, 12, 4, 
# new_data = data
# throw_out = which(new_data$ms_trial2 == 2)
# new_data = new_data[-throw_out ,]
# 
# throw_out = which(new_data$ms_trial2 == 3)
# new_data = new_data[-throw_out ,]
# 
# throw_out = which(new_data$ms_trial2 == 4)
# new_data = new_data[-throw_out ,]
# 
# throw_out = which(new_data$ms_trial2 == 5)
# new_data = new_data[-throw_out ,]
# 
# throw_out = which(new_data$ms_trial2 == 10)
# new_data = new_data[-throw_out ,]

# 3: long v long-foil
# 4: short v short-foil
# 9: single v single-foil

trial_idx = which(data$ms_trial2 == 3 | data$ms_trial2 == 4 | data$ms_trial2 == 9)
trials_w_correct = data[trial_idx ,]

stim_idx = which(trials_w_correct$stimulus == 'long' | trials_w_correct$stimulus == 'short' |
            trials_w_correct$stimulus == 'single')
trials_w_correct = trials_w_correct[stim_idx ,]

trials_w_correct = trials_w_correct %>%
  group_by(subj, condition, stimulus) %>%
  summarise(mean = mean(proportion))

trials_w_correct$condition = as.factor(trials_w_correct$condition)
trials_w_correct$stimulus = as.factor(trials_w_correct$stimulus)
trials_w_correct$subj = as.factor(trials_w_correct$subj)

res.mixed = lmer(mean ~ condition * stimulus + (1|subj), data = trials_w_correct)
summary(aov(mean ~ condition * stimulus, data = trials_w_correct))


ggboxplot(trials_w_correct, x = "stimulus", y = "mean", color = "condition",
          +           palette = c("#00AFBB", "#E7B800"))

res = aov(mean ~  condition * stimulus, data = trials_w_correct)

summary(res.mixed)

cond.aov <- with(trials_w_correct,
                   aov(mean ~ condition * stimulus)
)

summary(cond.aov)
report(cond.aov)

cond.aov <- with(trials_w_correct,
                 aov(mean ~ condition * stimulus + Error(subj/condition))
)
summary(cond.aov)

# Two-way mixed ANOVA test
res.aov <- anova_test(data = trials_w_correct, dv = mean, wid = subj,
  between = condition, within = stimulus
)
get_anova_table(res.aov)

barsum = trials_w_correct %>%
  group_by(condition, stimulus) %>%
  summarise(mean = mean(mean), sem = plotrix::std.error(mean))


barsum$sem = c(0.02165688, 0.02591666, 0.01901805, 0.02419905, 0.02544175, 0.02438322)
# long m = 0.02165688
# short m = 0.02591666
# single m = 0.01901805
# long s = 0.02419905
# short s = 0.02544175
# single s = 0.02438322




ggplot(barsum, aes(x=stimulus, y=mean, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(.9)) +
  ylim(0,1) +
  scale_fill_brewer(palette="Paired") + theme_minimal()


#############################################################
# 2.1, 2.2, 3.1, 3.2, 4.1, 4.2, 5.1, 5.2, 10.1, 10.2
#############################################################
data = data2
new_data = data[data$ms_trial2 %in% c(3.1, 3.2, 4.1, 4.2, 5.1, 5.2, 10.1, 10.2) ,]
new_data$position = 'early'
new_data$position[new_data$ms_trial2 %in% c(3.2,4.2,5.2,10.2)] = 'late'

new_data = new_data[new_data$stimulus %in% c('long', 'short', 'long-list', 'single')  ,]
new_data = new_data[!(new_data$ms_trial2 == 10.1 & new_data$stimulus == 'long') ,]
new_data = new_data[!(new_data$ms_trial2 == 10.2 & new_data$stimulus == 'long') ,]




data_summ = new_data %>%
  group_by(subj, condition, position) %>%
  summarise(mean = mean(proportion), sd = sd(proportion))

sp.aov <- with(data_summ,
                 aov(mean ~ condition * position +
                       Error(subj / condition))
)

summary(sp.aov)
report(sp.aov)

data_summ$condition = as.factor(data_summ$condition)
data_summ$position = as.factor(data_summ$position)

barsum = new_data %>%
  group_by(condition, position) %>%
  summarise(mean = mean(proportion), sem = plotrix::std.error(proportion))

ggplot(barsum, aes(x=position, y=mean, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(.9)) +
  ylim(0,1) +
  scale_fill_brewer(palette="Paired") + theme_minimal()

allsum = new_data %>%
  group_by(condition, position) %>%
  summarise(mean = mean(proportion), sem = plotrix::std.error(proportion))


######################################################
# related early v long and related late v long
######################################################

trial8 = data[data$ms_trial2 == 8.1 | data$ms_trial2 == 8.2 ,]
trial8$position = 'early'
trial8$position[trial8$ms_trial2 == 8.2] = 'late'
trial8 = trial8[trial8$stimulus == 'related' ,]

trial8_sum = trial8 %>%
  group_by(subj,condition, position) %>%
  summarise(mean = mean(proportion), sem = plotrix::std.error(proportion))

ggplot(trial8_sum, aes(x=position, y=mean, fill=condition)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(.9)) +
  ylim(0,1) +
  scale_fill_brewer(palette="Paired") + theme_minimal()               
                
sp.aov <- with(trial8_sum,
               aov(mean ~ condition * position +
                     Error(subj / condition))
)

summary(sp.aov)
report(sp.aov)               

# Two-way mixed ANOVA test
trial8_sum = ungroup(trial8_sum)
res.aov <- anova_test(data = trial8_sum, dv = mean, wid = subj,
                      between = condition, within = position
)
get_anova_table(res.aov)    


######################################################
# A-end v A-start
######################################################

trial1 = data[data$ms_trial2 == 1 ,]

t.test(trial1$proportion[trial1$stimulus == 'A-end' & trial1$condition == 'study'],
       trial1$proportion[trial1$stimulus == 'A-end' & trial1$condition == 'monitor'])


aend = trial1[trial1$stimulus == 'A-end' ,]
aend = aend %>%
  group_by(condition) %>%
  summarise(mean = mean(proportion), sem = plotrix::std.error(proportion))

ggplot(aend, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), width=.2,
                position=position_dodge(.9)) +
  ylim(0,1) +
  scale_fill_brewer(palette="Paired") + theme_minimal()  


######################################################
# related early v long and related late v long
######################################################
data2$cond[data2$subjID < 300] = 0      
data2$cond[data2$subjID > 299] = 1  

trial = data2[data2$ms_trial1 == 10 | data2$ms_trial1 == 11 ,]
trial$position = 'early'
trial$position[trial$ms_trial1 == 11] = 'late'
trial = trial[trial$response_info == 'related' ,]

trial_sum = trial %>%
  group_by(subjID,cond, ms_trial1, position) %>%
  summarise(mean = mean(prob))   

trial_sum$subjID = as.factor(trial_sum$subjID)

rel_base = trial_sum

sp.aov <- with(trial_sum,
               aov(mean ~ cond * position +
                     Error(subjID / position)))
               
sp.aov <- with(rel_base,
              aov(mean ~ cond * position)
)

summary(sp.aov)
report(sp.aov)  

trial_sum = trial %>%
  group_by(cond, ms_trial1, position) %>%
  summarise(mean = mean(prob), sem = std.error(prob))   

