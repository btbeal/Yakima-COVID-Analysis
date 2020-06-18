# --------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #
#
#
#     COVID-19 Data; Lincoln County
#
#
# --------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #

# ---- Clearing Environment, Setting Dir, Reading Data
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggthemes)
library(cowplot)
county_data <- read.csv2("us-counties.csv", sep = ",", header = TRUE)

# ---- Basic Data Cleaning for WA
yak_pop <- 250000
kc_pop <- 2100000
wa_data_adj <- county_data %>% 
  filter(county %in% c("Yakima", "King"),
         state == "Washington") %>% 
  group_by(county, date) %>% 
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  pivot_longer(cols = c("cases", "deaths"), names_to = "Type", values_to = "N") %>% 
  mutate(pop_adj_N = ifelse(county == "King", N/kc_pop, N/yak_pop))
  
wa_data_adj$date <- as.Date(wa_data_adj$date)

# ---- Define order timeline
shsh <- as.Date("2020-03-23")
KC_modPhase1 <- as.Date("2020-06-05")


# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
#
#                                   CASES
#
# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #
# -- adjusted
cases_adjusted <- ggplot(wa_data_adj %>% filter(Type == "cases")) +
  geom_point(aes(x = date, y = pop_adj_N, color = county)) +
  geom_vline(xintercept = shsh, color = "black", alpha = 1/2, linetype = "dashed") + 
  geom_vline(xintercept = KC_modPhase1, color = "black", alpha = 1/2, linetype = "dashed") + 
  scale_x_date() +
  theme_fivethirtyeight() +
  scale_color_wsj() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.03)) +
  labs(y = "Cumulative Case Count",
       x = "Date",
       title = "Cumulative COVID-19 Population-Adjusted Cases",
       subtitle = "For Yakima and King County, Washington") +
  # # annotations for stay at home order
  annotate(geom = "curve", x = (shsh - 10), y = 0.0025, xend = (shsh-1), yend = 0.001,
           curvature = -.1, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text",  x = (shsh - 20), y = 0.0035, size = 3.5,
           label = "Gov. Inslee's \n'Stay Home, Stay Healthy' \norder",
           hjust = "left",
           lineheight = 1.0,
           alpha = 6/10) +
  # # annotations for KC Phase 1.5
  annotate(geom = "curve", x = (KC_modPhase1 - 10), y = 0.025, xend = (KC_modPhase1-1), yend = 0.02,
           curvature = .25, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text",  x = (KC_modPhase1 - 24), y = 0.026, size = 3.25,
           label = "King County begins Phase 1.5, \nwhile Yakima remains in \nPhase 1",
           hjust = "left",
           lineheight = 1.0,
           alpha = 6/10) 




# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
#
#                                   DEATHS
#
# --------------------------------- # # --------------------------------- #
# ---------------------------------- # # --------------------------------- #
# --------------------------------- # # --------------------------------- #

# -- adjusted
death_adjusted <- ggplot(wa_data_adj %>% filter(Type == "deaths")) +
  geom_point(aes(x = date, y = pop_adj_N, color = county)) +
  geom_vline(xintercept = shsh, color = "black", alpha = 1/2, linetype = "dashed") + 
  geom_vline(xintercept = KC_modPhase1, color = "black", alpha = 1/2, linetype = "dashed") + 
  scale_x_date() +
  theme_fivethirtyeight() +
  scale_color_wsj() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.001)) +
  labs(y = "Cumulative Case Count",
       x = "Date",
       title = "Cumulative COVID-19 Population-Adjusted Deaths",
       subtitle = "For Yakima and King County, Washington") +
  # # annotations for stay at home order
  annotate(geom = "curve", x = (shsh - 10), y = 0.0001, xend = (shsh-1), yend = 0.00007,
           curvature = .2, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text",  x = (shsh - 20), y = 0.00018, size = 3.5,
           label = "Gov. Inslee's \n'Stay Home, Stay Healthy' \norder",
           hjust = "left",
           lineheight = 1.0,
           color = "grey50") +
  # # annotations for KC Phase 1.5
  annotate(geom = "curve", x = (KC_modPhase1 - 10), y = 0.00055, xend = (KC_modPhase1-1), yend = 0.00045,
           curvature = -.25, color = "grey60", arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text",  x = (KC_modPhase1 - 24), y = 0.0006, size = 3.25,
           label = "King County begins Phase 1.5, \nwhile Yakima remains in \nPhase 1",
           hjust = "left",
           lineheight = 1.0,
           color = "grey50") 




