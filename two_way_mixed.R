library(tidyverse)
library(ggpubr)
library(rstatix)

anxiety <- read.csv('C:/Users/Administrator/Ai/R/inferential/MIXED_ANOVA/two_way_mixed.csv')
anxiety

############################### Summary statistics
anxiety %>%
  group_by(time, group) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  anxiety, x = "time", y = "score",
  color = "group", palette = "jco"
)
bxp
############################### Check assumptions

################################# Outliers
anxiety %>%
  group_by(time, group) %>%
  identify_outliers(score)

################################# Normality assumption
# Check normality assumption by analyzing the model residuals

anxiety %>%
  group_by(time, group) %>%
  shapiro_test(score)

ggqqplot(anxiety, "score", ggtheme = theme_bw(),
         color = "time", palette = "jco") +
  facet_grid(time ~ group)

################################# Homogneity of variance assumption
anxiety %>%
  group_by(time) %>%
  levene_test(score ~ group)


################################# Homogneity of covariance assumption
box_m(anxiety[, "score", drop = FALSE], anxiety$group)

################################# Assumption of sphericity
#the assumption of sphericity will be automatically checked during the computation of the ANOVA test using the R function anova_test()

################################# Computation
# Two-way mixed ANOVA test
res.aov <- anova_test(
  data = anxiety, dv = score, wid = id,
  between = group, within = time
)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction

############## Compute simple main effects

# Simple main effect of group variable.
# Effect of group at each time point
one.way <- anxiety %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, between = group) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

############# Compute pairwise comparisons
# Pairwise comparisons between group levels
pwc <- anxiety %>%
  group_by(time) %>%
  pairwise_t_test(score ~ group, p.adjust.method = "bonferroni")
pwc

# Simple main effects of time variable
# Effect of time at each level of exercises group
one.way2 <- anxiety %>%
  group_by(group) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way2

# Pairwise comparisons between time points at each group levels
# Paired t-test is used because we have repeated measures by time
pwc2 <- anxiety %>%
  group_by(group) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
pwc2

############################# Procedure for Non-Significant two-way interaction
anxiety %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

anxiety %>%
  pairwise_t_test(
    score ~ group, 
    p.adjust.method = "bonferroni"
  )


############################################## Report
# Visualization: boxplots with p-values
pwc <- pwc %>% add_xy_position(x = "time")
pwc.filtered <- pwc %>% filter(time != "t1")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
