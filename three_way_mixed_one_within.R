library(tidyverse)
library(ggpubr)
library(rstatix)

performance <- read.csv('C:/Users/Administrator/Ai/R/inferential/MIXED_ANOVA/three_way_mixed_one_within.csv')
performance

############################### Summary statistics
performance %>%
  group_by(gender, stress, time ) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  performance, x = "gender", y = "score",
  color = "stress", palette = "jco",
  facet.by =  "time"
)
bxp
############################### Check assumptions

################################# Outliers
performance %>%
  group_by(gender, stress, time) %>%
  identify_outliers(score)

################################# Normality assumption
# Check normality assumption by analyzing the model residuals

performance %>%
  group_by(gender, stress, time ) %>%
  shapiro_test(score)

ggqqplot(performance, "score", ggtheme = theme_bw(),
         color = "time", palette = "jco") +
  facet_grid(time ~ stress, labeller = "label_both")

################################# Homogneity of variance assumption
performance %>%
  group_by(time) %>%
  levene_test(score ~ gender*stress)

################################# Assumption of sphericity
#the assumption of sphericity will be automatically checked during the computation of the ANOVA test using the R function anova_test()

################################# Computation
res.aov <- anova_test(
  data = performance, dv = score, wid = id,
  within = time, between = c(gender, stress)
)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction


############## Compute Two_way Interaction
# two-way interaction at each time levels
two.way <- performance %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, between = c(gender, stress))
two.way

############## Compute simple main effects
stress.effect <- performance %>%
  group_by(time, gender) %>%
  anova_test(dv = score, wid = id, between = stress)
stress.effect
############## Compute simple comparisons
# Fit pairwise comparisons
pwc <- performance %>%
  group_by(time, gender) %>%
  pairwise_t_test(score ~ stress, p.adjust.method = "bonferroni") %>%
  select(-p, -p.signif) # Remove details
# Focus on the results of "female" at t2
pwc %>% filter(time == "t2", gender == "female")


############################################## Report
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "gender") 
pwc.filtered <- pwc %>% filter(time == "t2", gender == "female")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
