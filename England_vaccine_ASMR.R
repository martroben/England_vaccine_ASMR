
#######################################################################################################
##                                                                                                   ##
##  Script name: England_vaccine_ASMR.R                                                              ##
##  Purpose of script: Compare age standardised mortality rate (ASMR) between vaccinated             ##
##                     and unvaccinated groups.                                                      ##
##                                                                                                   ##
##  Notes: ASMR is generally not used for analysing vaccine efficiency, because there are many       ##
##         other things apart from covid and vaccination, that affect ASMR.                          ##
##                                                                                                   ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 31. Jan 2022                                                                       ##
##                                                                                                   ##
##  Copyright: MIT License                                                                           ##
##  https://github.com/martroben/UK_covid_lost_years                                                 ##
##                                                                                                   ##
##  Contact: fb.com/martroben                                                                        ##
##                                                                                                   ##
#######################################################################################################


#################
# Load packages #
#################

if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  magrittr,
  dplyr,
  rio,
  stringr,
  lubridate,
  ggplot2)



################
# Data sources #
################

# England 2021 Jan-Oct Covid deaths data
covid_data_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsbyvaccinationstatusengland%2fdeathsoccurringbetween1januaryand31october2021/referencetable31.xlsx"



##################
# Importing data #
##################

data_var_names <- c(
  "month",
  "age_group",
  "vaccination_status",
  "n_deaths",
  "person_years",
  "mortality_per_100k_person_years",
  "unreliable",
  "lower_confidence_limit",
  "upper_confidence_limit")

covid_data_raw <- rio::import(
  covid_data_url,
  format = "xlsx",
  which = "Table 7",
  skip = 3,
  .name_repair = ~ data_var_names)



##################
# Analyzing data #
##################

groups_to_keep <- c("Unvaccinated", "21 days or more after second dose")
months_to_keep <- month.name[4:10]
vars_to_keep <- c("month", "age_group", "vaccinated", "mortality")

covid_data <- covid_data_raw %>%
  dplyr::filter(
    vaccination_status %in% groups_to_keep,
    month %in% months_to_keep) %>%
  dplyr::mutate(
    month = stringr::str_c(month, 2021, sep = " ") %>% lubridate::my(),
    vaccinated = vaccination_status != "Unvaccinated",
    mortality = as.numeric(mortality_per_100k_person_years)) %>%
  dplyr::select(all_of(vars_to_keep))



####################
# Visualising data #
####################

plot_title <- "Vaccination status and all-cause mortality"
plot_subtitle <- "England, by age-group, by month of 2021"
plot_caption <- "data: UK Office for National Statistics data
                 www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsbyvaccinationstatusengland\n
                 analysis: github.com/martroben/England_vaccine_ASMR"
y_label <- "mortality (per 100k person-years)"

ggplot2::ggplot() +
  geom_col(
    data = covid_data,
    aes(
      x = month,
      y = mortality,
      fill = vaccinated),
    position = position_dodge()) +
  facet_wrap(~age_group, scales = "free") +
  scale_x_date(
    breaks = "1 month",
    labels = ~lubridate::month(.x, label = TRUE)) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = plot_caption,
    y = y_label,
    x = element_blank()) +
  theme(
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 6))
