################################################################################
# EJC TASK
################################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, here, readxl, janitor, ggthemes, ggtext, sysfonts, showtext, paletteer)
font_add_google("Montserrat", "montserrat")
showtext_auto()


# Data Import -------------------------------------------------------------

income <- read_excel(here("data.xlsx"), 
                     sheet = "2.4", 
                     skip = 4,
                     col_types = "text") %>% # import all columns as type character to prevent coercion
  clean_names()


# Cleaning ----------------------------------------------------------------

income_cleaned <- income %>% 
  filter(nuts_1 == 1 | nuts_1 == 0, gebietseinheit %in% c("Berlin", "Deutschland")) %>%
  select(land, gebietseinheit, x1995:last_col()) %>% 
  pivot_longer(3:last_col(), values_to = "einkommen", names_to = "jahr") %>% # long format for easier grouping and plotting
  mutate(jahr = as.integer(str_replace(jahr, "x", "")),
         einkommen = as.integer(einkommen))

# Plotting ----------------------------------------------------------------

ggplot(income_cleaned, 
       aes(x = jahr, y = einkommen, group = gebietseinheit, color = gebietseinheit)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1995, 2020, 5)) +
  scale_y_continuous(breaks = seq(10000, 26000, 2000)) +
  scale_color_manual(values = c("Berlin" = "#F28E2BFF", "Deutschland" = "#4E79A7FF")) +
  guides(color = FALSE) +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "montserrat"),
    plot.title.position = "plot",
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, face = "italic"),
    plot.margin = margin(10, 75, 10, 10)) +
  coord_cartesian(xlim = c(1995, 2021), ylim = c(9000, 27000), expand = FALSE, clip = "off") +
  expand_limits(y = c(10000, 26000), x = c(1995, 2020)) +
  labs(
    title = "Berliner verdienen weniger",
    subtitle = "In Berlin liegt das verfügbare Haushaltseinkommen* seit über 20 Jahren \nunter dem Bundesdurchschnitt",
    x = NULL,
    y = NULL,
    caption = "*Das verfügbare Haushaltseinkommen ist das Einkommen , \ndas den Menschen nach Abzug von Steuern und Sozialabgaben bleibt."
  ) +
  annotate("text", x = c(2020.5, 2020.5), y = c(21745, 23559), 
           label = c("Berlin", "Deutschland"), color = c("#F28E2BFF", "#4E79A7FF"),
           family = "montserrat", hjust = 0)
