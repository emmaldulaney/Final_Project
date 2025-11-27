library(tidycensus)
library(dplyr)

# pulls employment status data for every U.S. county
# use ACS table B23025, which describes work status for people age 16+.
employment_raw <- get_acs(
  geography = "county",
  variables = paste0("B23025_00", 1:7),  # 001 - 007
  year = 2022,
  survey = "acs5",
  output = "wide"
)

head(employment_raw)


# cleaning the data -> keeping only the estimates (E), dropping margins of error (M)
employment_clean <- employment_raw %>%
  select(-matches("_00[1-7]M$"))

#renaming the estimate columns to something easier to understand
#these are all counts of people age 16+ in different work status groups.
names(employment_clean)[-(1:2)] <- c(
  "pop16plus_total",                # B23025_001E
  "pop16plus_in_labor_force",       # B23025_002E
  "pop16plus_civilian_labor_force", # B23025_003E
  "pop16plus_civilian_employed",    # B23025_004E
  "pop16plus_civilian_unemployed",  # B23025_005E
  "pop16plus_armed_forces",         # B23025_006E
  "pop16plus_not_in_labor_force"    # B23025_007E
)

head(employment_clean)

#creates the percentage variables that are easier to compare across counties
#pct_in_labor_force: share of adults (16+) who are in the labor force
#pct_unemployed: share of the civilian labor force that is unemployed
#pct_armed_forces: share of the total labor force that is in the armed forces
EMPLOYMENT <- employment_clean %>%
  transmute(
    GEOID,
    pct_in_labor_force = pop16plus_in_labor_force / pop16plus_total * 100,
    pct_unemployed     = pop16plus_civilian_unemployed /
      pop16plus_civilian_labor_force * 100,
    pct_armed_forces   = pop16plus_armed_forces /
      pop16plus_in_labor_force * 100
  )

head(EMPLOYMENT)
summary(EMPLOYMENT)

write.csv(EMPLOYMENT, "EMPLOYMENT_features.csv", row.names = FALSE)
