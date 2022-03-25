library(tidyverse)
library(ggpubr)
library(rstatix)

weightloss <- read.csv('C:/Users/Administrator/Ai/R/inferential/MIXED_ANOVA/three_way_mixed_one_between.csv')
weightloss

############################### Summary statistics
weightloss %>%
  group_by(exercises, diet, time) %>%
  get_summary_stats(score, type = "mean_sd")

############################### Visualization
bxp <- ggboxplot(
  weightloss, x = "exercises", y = "score",
  color = "time", palette = "jco",
  facet.by = "diet", short.panel.labs = FALSE
)
bxp
############################### Check assumptions

################################# Outliers
weightloss %>%
  group_by(diet, exercises, time) %>%
  identify_outliers(score)

################################# Normality assumption
# Check normality assumption by analyzing the model residuals

weightloss %>%
  group_by(diet, exercises, time) %>%
  shapiro_test(score)

ggqqplot(weightloss, "score", ggtheme = theme_bw(),
         color = "exercises", palette = "jco") +
  facet_grid(diet + exercises ~ time, labeller = "label_both")

################################# Homogneity of variance assumption
weightloss %>%
  group_by(diet, time) %>%
  levene_test(score ~ exercises)

################################# Assumption of sphericity
#the assumption of sphericity will be automatically checked during the computation of the ANOVA test using the R function anova_test()

################################# Computation
res.aov <- anova_test(
  data = weightloss, dv = score, wid = id,
  between = exercises, within = c(diet, time)
)
get_anova_table(res.aov)

################################# Post-hoc tests
############################# Procedure for significant two-way interaction


############## Compute Two_way Interaction
# Two-way ANOVA at each exercises group level
two.way <- weightloss %>%
  group_by(exercises) %>%
  anova_test(dv = score, wid = id, within = c(diet, time))
two.way

# Extract anova table
get_anova_table(two.way)

############## Compute simple main effects
time.effect <- weightloss %>%
  group_by(exercises, diet) %>%
  anova_test(dv = score, wid = id, within = time) %>%
  get_anova_table()
time.effect %>% filter(exercises == "yes")

############## Compute simple comparisons
# compute pairwise comparisons
pwc <- weightloss %>%
  group_by(exercises, diet) %>%
  pairwise_t_test(
    score ~ time, paired = TRUE, 
    p.adjust.method = "bonferroni"
  ) %>%
  select(-statistic, -df) # Remove details
# Focus on the results of exercises:yes group
pwc %>% filter(exercises == "yes") %>%
  select(-p)    # Remove p column


############################################## Report
# Visualization: box plots with p-values
# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "exercises")
pwc.filtered <- pwc %>% filter(exercises == "yes")
bxp + 
  stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

