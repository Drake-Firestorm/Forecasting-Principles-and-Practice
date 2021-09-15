#* 1 ----
# Write a function to compute the mean and standard deviation of a time series, and apply it to the PBS data.
# Plot the series with the highest mean, and the series with the lowest standard deviation.


# calculate mean and std
PBS2 <- PBS %>%
  group_by(Concession, Type, ATC1, ATC2) %>%
  mutate(Mean = mean(Cost), STD = sd(Cost)) %>%
  ungroup()

PBS2 %>%
  filter(Mean == max(Mean)) %>%
  mutate(Series = glue:glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Series with highest average Cost")

PBS2 %>%
  filter(STD == min(STD)) %>%
  mutate(Series = glue:glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Series with lowest standard deviation in Cost")


#** 2 ----
# Use GGally::ggpairs() to look at the relationships between the STL-based features for the holiday series in the tourism data.
# Change seasonal_peak_year and seasonal_trough_year to factors, as shown in Figure 4.3.
# Which is the peak quarter for holidays in each state?

tourism_features <- tourism %>%
  features(Trips, feature_set("feasts"))

tourism %>%
  features(Trips, feat_stl) %>%
  select(-Region, -State, -Region) %>%
  mutate(
    seasonal_peak_year = factor(seasonal_peal_year + 1, labels = c("Q1", "Q2", "Q3", "Q4")),
    seasonal_trough_year = factor(seasonal_trough_year + 1, labels = c("Q1", "Q2", "Q3", "Q4"))
  ) %>%
  ggpairs(mapping = aes(color = Purpose), upper = "black", legend = 1) +
  theme(legend.position = 'top')

tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) %>%
  features(Trips, feat_stl) %>%
  select(State, seasonal_peak_year)


#** 3 ----
# Use a feature-based approach to look for outlying series in the PBS data.
# What is unusual about the series you identify as “outliers.”

PBS %>%
  as_tibble() %>%
  group_by(Concession, Type, ATC1, ATC2) %>%
  summarise(Total_Scripts = sum(Scripts), Total_Costs = sum(Cost)) %>%
  filter(Total_Scripts == 0, Total_Costs == 0)

PBS_nozeros <- PBS %>%
  filter(!(Concession == "General" & Type == "Co-payments" & (ATC2 %in% c("R", "S"))))

PBS_features <- PBS_nozeros %>%
  features(Cost, feature_set(pkgs = "feasts"))

PBS_pcs <- PBS_features %>%
  select(-Concession, -Type, -ATC1, -ATC2)

PBS_pcs <- as_tibble(t(na.omit(t(PBS_pcs)))) %>%
  prcomp(scale = TRUE) %>%
  augment(PBS_features)

PBS_pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point()

PBS_outliers <- PBS_pcs %>%
  filter(.fittedPC1 > 5.5) %>%
  select(Concession, Type, ATC1, ATC2, .fittedPC1, .fittedPC2)

PBS_outliers %>%
  left_join(PBS, by = c("Concession", "Type", "ATC1", "ATC2")) %>%
  mutate(Series = glue::glue("{Concession}", "{Type}", "{ATC1}", "{ATC2}", .sep = "\n\n")) %>%
  ggplot(aes(x = Month, y = Cost)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  ggtitle("Outlying time series in PC space")
