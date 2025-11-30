
##########################################################################################################################
# Phase 2
library(ggplot2)

# ============================================
# PLOT 1 — Linear Fit (Log Scale)
# ============================================
ggplot(merged_data, aes(x = SOVI_SCORE, y = EAL_VALT)) +
  geom_point(alpha = 0.4, color = "darkgrey") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_log10() +
  theme_bw() +
  labs(
    title = "EAL_VALT vs SOVI_SCORE (Linear Fit, Log Scale)",
    x = "SOVI Score",
    y = "Total Expected Annual Loss (log10 scale)"
  )
## Counties with higher social vulnerability tend to have higher losses.
# The trend is not super strong, but the general direction is upward: 
# as SOVI Score increases, expected annual loss also increases.

# ============================================
# PLOT 2 — LOESS Nonlinear Fit (Log Scale)
# ============================================
ggplot(merged_data, aes(x = SOVI_SCORE, y = EAL_VALT)) +
  geom_point(alpha = 0.4, color = "darkgrey") +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1) +
  scale_y_log10() +
  theme_bw() +
  labs(
    title = "Nonlinear Trend: EAL_VALT vs SOVI_SCORE (Log Scale)",
    x = "SOVI Score",
    y = "Total Expected Annual Loss (log10 scale)"
  )
## The relationship is slightly curved: losses rise a little
# at first, then increase more for the most vulnerable counties.
# This suggests that the highest-vulnerability communities 
# may be hit harder than others.

# ============================================
# PLOT 3 — By SOVI Rating (Log Scale)
# ============================================
ggplot(merged_data, aes(x = SOVI_SCORE, y = EAL_VALT, color = SOVI_RATNG)) +
  geom_point(alpha = 0.6) +
  scale_y_log10() +
  theme_bw() +
  labs(
    title = "EAL_VALT vs SOVI_SCORE by SOVI Rating (Log Scale)",
    x = "SOVI Score",
    y = "Total Expected Annual Loss (log10 scale)",
    color = "SOVI Rating"
  )
## The more vulnerable the group, the higher and more spread-out 
# the losses. “Very Low” vulnerability counties have consistently
# low losses, while “Very High” vulnerability counties show 
# both higher and more unpredictable losses.
