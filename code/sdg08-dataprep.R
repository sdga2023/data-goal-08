library(dplyr)
library(tidyr)
library(wbstats)

countries <- wbstats::wb_countries()

# Chart 1: GDP damage
gdp_loss <- read.csv('../input/chart1_GDP_damage.csv')
gdp_loss.ok <- select(gdp_loss, iso3c, year = Year, value = GDP.Growth) %>%
  filter(!is.na(iso3c), !is.na(value)) %>%
  mutate(iso3c = ifelse(iso3c == "WLD", "WLT", iso3c)) %>%
  filter((year > 2018 & iso3c != "WLT") | iso3c == "WLT" ) %>%
  mutate(value = round(value, 1))
write.csv(gdp_loss.ok, "../output/gdp_loss.csv", row.names = FALSE)

# Chart 2: GDP difference
gdp_diff <- read.csv('../input/chart2_GDP_COVID_region.csv')
gdp_diff_world <- read.csv('../input/chart2_GDP_COVID_world.csv')
gdp_diff_world$region_iso3c <- "WLD"

gdp_diff.ok <- rbind(gdp_diff, gdp_diff_world) %>% select(iso3c = region_iso3c, year = date, gdp, gdp_forc20, gap_dollars, gap_relative) %>%
  mutate(gap_dollars = gsub("\\$", "", gap_dollars)) %>%
  mutate(gap_dollars = gsub(",", "", gap_dollars)) %>%
  mutate(gap_relative = as.numeric(gsub("%", "", gap_relative)))
write.csv(gdp_diff.ok, "../output/gdp_diff.csv", row.names = FALSE)

# Chart 3: GDP differences for countries
gdp_diffc <- read.csv('../input/chart3_GDP_COVID_country.csv')
gdp_diffc.ok <- select(gdp_diffc, iso3c, year = date, gdp, gdp_forc20, gap_dollars, gap_relative) %>%
  filter(year > 2018) %>%
  filter(!is.na(gdp)) %>%
  mutate(gap_dollars = gsub("\\$", "", gap_dollars)) %>%
  mutate(gap_dollars = gsub(",", "", gap_dollars)) %>%
  mutate(gap_relative = as.numeric(gsub("%", "", gap_relative)))
write.csv(gdp_diffc.ok, "../output/gdp_diff_countries.csv", row.names = FALSE)

# Ukraine chart
ukrain_diff <- read.csv('../input/chart8_GDP_Ukraine.csv')
ukrain_gdp_19 <- filter(ukrain_diff, date == 2019)$gdp
ukrain_diff.ok <- select(ukrain_diff, iso3c, year = date, gdp, gdp_forc20, gdp_forc_jan2022) %>%
  mutate(gap_dollars = gdp - gdp_forc20) %>%
  mutate(gdp = round(gdp/ukrain_gdp_19*100, 1)) %>%
  mutate(gdp_forc20 = round(gdp_forc20/ukrain_gdp_19*100, 1)) %>%
  mutate(gdp_forc_jan2022 = round(gdp_forc_jan2022/ukrain_gdp_19*100, 1)) %>%
  mutate(gap_relative = gdp - gdp_forc20)
write.csv(ukrain_diff.ok, "../output/ukrain_diff.csv", row.names = FALSE)

# Chart 4: Tourism
tour <- read.csv('../input/chart4_tourism.csv')
tour.ok <- select(tour, iso3c, tourism, change_gdp, pop = SP.POP.TOTL_2021) %>%
  filter(!is.na(change_gdp))
write.csv(tour.ok, "../output/tourism.csv", row.names = FALSE)

# Chart 5: unemployment
unemp <- read.csv("../input/chart5_unemployment_region.csv")
unemp.ok <- rename(unemp, iso3c = region_iso3c, value = Unemployment.Rate, year = date) %>%
  mutate(value = round(value, 1))
write.csv(unemp.ok, "../output/unemployment.csv", row.names = FALSE)

# Chart 6: gender gap in unemployment
unemp.gender.raw <- read.csv('../input/chart6_male_female_unemployment.csv')
unemp.gender <-  rename(unemp.gender.raw, female = Female, male = Male, year = date, iso3c = region_iso3c) %>%
  mutate(female = round(female, 2), male = round(male, 2)) %>%
  mutate(gap = round(gap, 3)) %>%
  filter(year > 2014)
write.csv(unemp.gender, "../output/unemploymentgender.csv", row.names = FALSE)

# Chart 7: shared prosperity
shared <- read.csv("../input/chart7_shared_prosperity.csv")
shared.ok <- select(shared, iso3c = code, growthb40, growthtotal) %>%
  mutate(growthb40 = round(growthb40, 2), growthtotal = round(growthtotal, 2))
write.csv(shared.ok, "../output/sharedprosperity.csv", row.names = FALSE)