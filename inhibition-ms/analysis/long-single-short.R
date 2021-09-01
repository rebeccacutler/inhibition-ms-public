################################################
#script to look at images based on list length:
# long (B), short (C), single (D)
################################################

# carry overfrom script 1
target_foil = data.frame(stimulus = c('long','short','single'),
                         n = c(14,4,1),
                         mean = c(0.71, 0.77, 0.82),
                         sem = c(0.02, 0.02 ,0.01))

ggplot(data = target_foil, aes(x=stimulus, y=mean, ymin=mean-sem, ymax=mean+sem, fill=stimulus)) + 
  geom_col(position = position_dodge(0.9), width = 0.8) +
  #ggtitle('trial type: 3.3') +
  geom_errorbar( position = position_dodge(0.9), width=0.3, colour="black", size=0.4) +
  theme_minimal() +
  geom_hline(yintercept=0.5, linetype="dashed", color = "grey32", size = 0.5) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank(),
        title = element_text(size = 10),
        legend.position = "none") +
  ylim(0,1)
  #geom_text(x = 1.5, y = 0.9, label = '*', size = 7, color = 'grey24')

# short v short-foil = 2.2
# long v long-foil = 6.2
# single v single-foil = 4

target_foil = data.frame(trial = as.factor(rep(c('6.2','2.2','4'), each = 148)),
                         target = as.factor(rep(c('long', 'short','single'), each = 148)),
                         subj = as.factor(rep(seq(1,148), 3)),
                         mean = c(trial_summary$proportion[trial_summary$trial == '6.2' & trial_summary$stimulus == 'long'],
                                  trial_summary$proportion[trial_summary$trial == '2.2' & trial_summary$stimulus == 'short'],
                                  trial_summary$proportion[trial_summary$trial == '4' & trial_summary$stimulus == 'single']))

target_foil %>%
  group_by(target) %>%
  get_summary_stats(mean, type = "mean_sd")

target_foil$subj = as.factor(target_foil$subj)
res = aov(mean ~ target + Error(subj/target), data=target_foil)
summary(res)

library("emmeans")
# set orthogonal contrasts
options(contrasts = c("contr.sum", "contr.poly"))
res <- aov(mean ~ target + Error(subj / target), data = target_foil)

target_pairs <- emmeans(res, ~ target)
pairs(target_pairs)

library(tidyverse)
library(ggpubr)
library(rstatix)

ggboxplot(target_foil, x = "target", y = "mean",
          color = "target", palette = "jco")+
  stat_compare_means(method = "anova")

compare_means(mean ~ target,  data = target_foil)
