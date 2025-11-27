library(tidycensus)
library(tidyverse)
census_api_key("b60ee402dd013df41b05dd1fe39c07050ee7a9e7", install = TRUE)
housing_raw <- get_acs(
  geography = "county",
  table = "B25001",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>% select(-matches("M$"))

occupancy_raw <- get_acs(
  geography = "county",
  table = "B25002",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>% select(-matches("M$"))

tenure_raw <- get_acs(
  geography = "county",
  table = "B25003",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>% select(-matches("M$"))

HOUSING <- housing_raw %>%
  select(GEOID, NAME, total_housing_units = B25001_001E) %>%
  left_join(
    occupancy_raw %>%
      select(
        GEOID,
        occupied_units = B25002_002E,
        vacant_units   = B25002_003E
      ),
    by = "GEOID"
  ) %>%
  left_join(
    tenure_raw %>%
      select(
        GEOID,
        owner_occupied_units  = B25003_002E,
        renter_occupied_units = B25003_003E
      ),
    by = "GEOID"
  )

education_raw <- get_acs(
  geography = "county",
  table = "B15003",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  select(-matches("M$"))   # removes MOE columns

income_raw <- get_acs(
  geography = "county",
  variables = c(median_hh_income = "B19013_001"),
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  select(-matches("M$")) %>%
  rename(median_hh_income = median_hh_incomeE)


poverty_raw <- get_acs(
  geography = "county",
  table = "B17001",
  year = 2022,
  survey = "acs5",
  output = "wide"
) %>%
  select(-matches("M$"))

HOUSING <- HOUSING %>%
  mutate(
    pct_occupied        = occupied_units / total_housing_units * 100,
    pct_vacant          = vacant_units / total_housing_units * 100,
    pct_owner_occupied  = owner_occupied_units / occupied_units * 100,
    pct_renter_occupied = renter_occupied_units / occupied_units * 100
  )

EDUCATION <- education_raw %>%
  mutate(
    total_25plus = B15003_001E,
    
    # Sum of ACS columns 17 through 24 (HS graduate b doctorate)
    hs_or_higher = rowSums(across(B15003_017E:B15003_024E)),
    
    # Sum of ACS columns 21 through 24 (BA b doctorate)
    ba_or_higher = rowSums(across(B15003_021E:B15003_024E)),
    
    pct_hs_or_higher = (hs_or_higher / total_25plus) * 100,
    pct_ba_or_higher = (ba_or_higher / total_25plus) * 100
  ) %>%
  select(
    GEOID,
    pct_hs_or_higher,
    pct_ba_or_higher
  )


INCOME_POVERTY <- income_raw %>%
  select(GEOID, median_hh_income) %>%
  left_join(
    poverty_raw %>%
      transmute(
        GEOID,
        poverty_universe = B17001_001E,
        poverty_below    = B17001_002E,
        poverty_rate     = (poverty_below / poverty_universe) * 100
      ),
    by = "GEOID"
  )

ACS_FEATURES_1 <- HOUSING %>%
  select(
    GEOID,
    NAME,
    pct_occupied, pct_vacant,
    pct_owner_occupied, pct_renter_occupied
  ) %>%
  left_join(EDUCATION, by = "GEOID") %>%
  left_join(INCOME_POVERTY, by = "GEOID")

write.csv(ACS_FEATURES,"ACS_FEATURES.csv",row.names = FALSE)
