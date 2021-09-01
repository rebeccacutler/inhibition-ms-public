
early_late = data.frame(trial = as.factor(c(3,4,5,9,10)),
                        early = c(0.79,0.75,0.55,0.81,0.57),
                        late = c(0.74,0.79,0.48,0.83,0.64))

ggplot(early_late, aes(x=early, y=late, color=trial)) +
  geom_point(size = 2.5) +
  xlim(0.4,1) +
  ylim(0.4,1) +
  geom_abline(aes(slope = 1, intercept = 0), size=0.2, linetype='dashed') +
  geom_text(x=0.58, y=0.9, label='r = 0.91', color='black', size=3) +
  theme_minimal()
