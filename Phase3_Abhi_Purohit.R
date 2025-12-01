library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(purrr)

nri <- read.csv("/Users/abhip/Downloads/nri_county_trimmed.csv")

#finding all the hazard-specific total loss columns
#these columns end with "_EALT" like AVLN_EALT, CWAV_EALT
#each one is the expected annual loss (total) for a specific hazard type.
hazard_cols <- grep("_EALT$", names(nri), value = TRUE)

hazard_cols
length(hazard_cols)  #num of hazards

# making a "long" dataset, one row = county × hazard
hazard_long <- nri %>%
  select(STCOFIPS, SOVI_RATNG, all_of(hazard_cols)) %>%
  filter(
    !is.na(SOVI_RATNG),
    SOVI_RATNG != "Data Unavailable"
  ) %>%
  mutate(
    SOVI_RATNG = factor(
      SOVI_RATNG,
      levels = c("Very Low",
                 "Relatively Low",
                 "Relatively Moderate",
                 "Relatively High",
                 "Very High")
    )
  ) %>%
  pivot_longer(
    cols = all_of(hazard_cols),
    names_to = "hazard_code",
    values_to = "EAL_total"
  ) %>%
  # keep only positive losses for the tests
  filter(!is.na(EAL_total), EAL_total > 0) %>%
  mutate(
    hazard_short = str_remove(hazard_code, "_EALT$")
  )

head(hazard_long)

## running a Kruskal–Wallis test for each hazard
# tryna see if the loss for a hazard look different across the SOVI groups
# got outputs like statistic - test value from Kruskal–Wallis and p_value - how strong the evidence is that groups differ
#smaller p-value = stronger evidence SOVI groups are different
kruskal_results <- hazard_long %>%
  group_by(hazard_short) %>%
  group_modify(~{
    test <- kruskal.test(EAL_total ~ SOVI_RATNG, data = .x)
    tibble(
      statistic = as.numeric(test$statistic),
      p_value   = test$p.value
    )
  }) %>%
  ungroup()

kruskal_results

#seeing much the median loss moves between the lowest and highest SOVI group for each hazard

hazard_medians <- hazard_long %>%
  group_by(hazard_short, SOVI_RATNG) %>%
  summarise(
    median_loss = median(EAL_total, na.rm = TRUE),
    .groups = "drop"
  )

hazard_spread <- hazard_medians %>%
  group_by(hazard_short) %>%
  summarise(
    min_median  = min(median_loss, na.rm = TRUE),
    max_median  = max(median_loss, na.rm = TRUE),
    diff_median = max_median - min_median,
    .groups = "drop"
  )

hazard_spread

#combining the test results with the median spreads and ranking hazards by the biggest change in median loss and by p value
hazard_rank <- kruskal_results %>%
  left_join(hazard_spread, by = "hazard_short") %>%
  arrange(desc(diff_median), p_value)   # biggest spread first

hazard_rank

# choosing the top hazards: like keeping only hazards with p_value < 0.05 so that we have strong evidence SOVI matters and then from those, take the 5 hazards with the biggest median gap
top_hazards_table <- hazard_rank %>%
  filter(p_value < 0.05) %>%           # only hazards with a real difference
  slice_max(diff_median, n = 5)        # top 5 with largest median gap

top_hazards_table

top_hazards <- top_hazards_table$hazard_short

top_hazards

# boxplots for the top hazards; lets us see how loss levels change across SOVI groups
hazard_long %>%
  filter(hazard_short %in% top_hazards) %>%
  ggplot(aes(x = SOVI_RATNG, y = EAL_total)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ hazard_short, scales = "free_y") +
  labs(
    title = "Hazard-Specific Expected Annual Loss by Social Vulnerability",
    x = "Social Vulnerability Rating",
    y = "Hazard EAL (log scale)"
  )

write.csv(hazard_rank, "hazard_kruskal_rankings.csv", row.names = FALSE)
