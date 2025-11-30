# Phase 3
str(merged_data$EAL_VALT)
str(merged_data$SOVI_RATNG)

merged_data <- merged_data %>%
  mutate(
    EAL_VALT = as.numeric(EAL_VALT),
    SOVI_RATNG = as.factor(SOVI_RATNG)
  )

ggplot(merged_data, aes(x = SOVI_RATNG, y = EAL_VALT)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_bw() +
  labs(
    title = "EAL_VALT by SOVI Rating (Log Scale)",
    x = "SOVI Rating",
    y = "Total Expected Annual Loss (log10 scale)"
  )

anova_model <- lm(log(EAL_VALT) ~ SOVI_RATNG, data = merged_data)
plot(anova_model, which = 2) 
# Failed miserably 

library(car)
leveneTest(log(EAL_VALT) ~ SOVI_RATNG, data = merged_data)
# Also failed miserably

anova_fit <- aov(log(EAL_VALT) ~ SOVI_RATNG, data = merged_data)
summary(anova_fit)
# Since assumptions are violated, we cannot trust this

kruskal.test(EAL_VALT ~ SOVI_RATNG, data = merged_data)
# Highly significant

pairwise.wilcox.test(
  merged_data$EAL_VALT,
  merged_data$SOVI_RATNG,
  p.adjust.method = "bonferroni"
)
# The analysis shows that expected annual loss (EAL_VALT) differs clearly across the five SOVI vulnerability groups. 
# Because loss data is extremely skewed, the normality and equal-variance assumptions for ANOVA do not hold, so we rely 
# on the Kruskal–Wallis test instead. This test finds strong evidence that losses are not the same across groups. 
# Follow-up pairwise comparisons show that the most vulnerable counties (“Very High”) experience much higher and more 
# unpredictable losses than the least vulnerable counties (“Very Low”). Some of the middle groups overlap with each 
# other, but the extremes are dramatically different. Overall, higher social vulnerability is associated with larger 
# and more variable expected annual losses.