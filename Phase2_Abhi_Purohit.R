library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

nri <- read.csv("/Users/abhip/Downloads/nri_county_trimmed.csv")

# SOVI_RATNG is the social vulnerability category
# EAL_VALB is the expected annual loss to buildings
# EAL_VALPE is the expected annual loss to people
# EAL_VALA is the expected annual loss to agriculture
# dropped rows where SOVI_RATNG is missing
# dropped rows where SOVI_RATNG is "Data Unavailable"
# turned SOVI_RATNG into an ordered factor from lowest to highest vulnerability
loss_df <- nri %>%
  select(SOVI_RATNG, EAL_VALB, EAL_VALPE, EAL_VALA) %>%
  filter(
    !is.na(SOVI_RATNG),
    SOVI_RATNG != "Data Unavailable"
  ) %>%
  mutate(
    # put SOVI levels in order from low to high
    SOVI_RATNG = factor(
      SOVI_RATNG,
      levels = c("Very Low",
                 "Relatively Low",
                 "Relatively Moderate",
                 "Relatively High",
                 "Very High")
    )
  )

head(loss_df)

#median and mean loss to buildings
#median and mean loss to people
#median and mean loss to agriculture
#n is how many counties are in that SOVI group
loss_summary <- loss_df %>%
  group_by(SOVI_RATNG) %>%
  summarise(
    median_buildings   = median(EAL_VALB,  na.rm = TRUE),
    median_people      = median(EAL_VALPE, na.rm = TRUE),
    median_agriculture = median(EAL_VALA,  na.rm = TRUE),
    mean_buildings     = mean(EAL_VALB,    na.rm = TRUE),
    mean_people        = mean(EAL_VALPE,   na.rm = TRUE),
    mean_agriculture   = mean(EAL_VALA,    na.rm = TRUE),
    n = n()
  )

loss_summary




#loss to buildings vs SOVI
ggplot(loss_df, aes(x = SOVI_RATNG, y = EAL_VALB)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Expected Annual Loss to Buildings by Social Vulnerability",
    x = "Social Vulnerability Rating",
    y = "EAL to Buildings (log scale)"
  )

#loss to people vs SOVI
ggplot(loss_df, aes(x = SOVI_RATNG, y = EAL_VALPE)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Expected Annual Loss to People by Social Vulnerability",
    x = "Social Vulnerability Rating",
    y = "EAL to People (log scale)"
  )

#loss to agriculture vs SOVI
ggplot(loss_df, aes(x = SOVI_RATNG, y = EAL_VALA)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Expected Annual Loss to Agriculture by Social Vulnerability",
    x = "Social Vulnerability Rating",
    y = "EAL to Agriculture (log scale)"
  )


# Median loss by SOVI_RATNG and loss type
median_by_type <- loss_df %>%
  group_by(SOVI_RATNG) %>%
  summarise(
    EAL_VALB  = median(EAL_VALB,  na.rm = TRUE),
    EAL_VALPE = median(EAL_VALPE, na.rm = TRUE),
    EAL_VALA  = median(EAL_VALA,  na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(EAL_VALB, EAL_VALPE, EAL_VALA),
    names_to = "loss_type",
    values_to = "median_loss"
  )

median_by_type

# For each loss type, see how much the median moves across SOVI levels
variation_by_type <- median_by_type %>%
  group_by(loss_type) %>%
  summarise(
    min_median  = min(median_loss, na.rm = TRUE),
    max_median  = max(median_loss, na.rm = TRUE),
    diff_median = max_median - min_median
  )

variation_by_type


#made a long-format version where each row is county × loss_type, with a single loss_value.
loss_long <- loss_df %>%
  pivot_longer(
    cols = c(EAL_VALB, EAL_VALPE, EAL_VALA),
    names_to = "loss_type",
    values_to = "loss_value"
  ) %>%
  mutate(
    loss_type = dplyr::recode(
      loss_type,
      EAL_VALB  = "Buildings",
      EAL_VALPE = "People",
      EAL_VALA  = "Agriculture"
    )
  )

head(loss_long)

ggplot(loss_long, aes(x = SOVI_RATNG, y = loss_value)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ loss_type, nrow = 1) +
  labs(
    title = "Expected Annual Loss by Social Vulnerability and Loss Type",
    x = "Social Vulnerability Rating",
    y = "EAL (log scale)"
  )

# out of the total loss (EAL_VALT), what share goes to buildings, people, and agriculture
loss_comp <- nri %>%
  select(SOVI_RATNG, EAL_VALT, EAL_VALB, EAL_VALPE, EAL_VALA) %>%
  filter(
    !is.na(SOVI_RATNG),
    SOVI_RATNG != "Data Unavailable",
    EAL_VALT > 0
  ) %>%
  mutate(
    SOVI_RATNG = factor(
      SOVI_RATNG,
      levels = c("Very Low",
                 "Relatively Low",
                 "Relatively Moderate",
                 "Relatively High",
                 "Very High")
    ),
    share_buildings   = EAL_VALB  / EAL_VALT,
    share_people      = EAL_VALPE / EAL_VALT,
    share_agriculture = EAL_VALA  / EAL_VALT
  )

#for each SOVI group, get the median share of total loss, going to buildings, people, and agriculture
share_summary <- loss_comp %>%
  group_by(SOVI_RATNG) %>%
  summarise(
    median_share_buildings   = median(share_buildings,   na.rm = TRUE),
    median_share_people      = median(share_people,      na.rm = TRUE),
    median_share_agriculture = median(share_agriculture, na.rm = TRUE)
  )

share_summary


#built a smaller dataset that keeps:
#county ID (STCOFIPS), SOVI_RATNG, raw EAL values for buildings, people, agriculture
#and also adds: log10(EAL + 1) for each loss type (to shrink the scale and handle skewed, heavy-tailed dollar amounts)
loss_features <- nri %>%
  select(STCOFIPS, SOVI_RATNG, EAL_VALB, EAL_VALPE, EAL_VALA) %>%
  filter(
    !is.na(SOVI_RATNG),
    SOVI_RATNG != "Data Unavailable"
  ) %>%
  mutate(
    log_EAL_VALB  = log10(EAL_VALB  + 1),
    log_EAL_VALPE = log10(EAL_VALPE + 1),
    log_EAL_VALA  = log10(EAL_VALA  + 1)
  )


head(loss_features)

write.csv(loss_features, "LOSS_FEATURES_log_scaled.csv", row.names = FALSE)
