setwd("D:/OneDrive/Postdoctor/Quantitative_course/Week5") # this is the path I store my code and data, you need to change
library(dplyr)

fish <- read.csv("Data/fish_long.csv")

head(fish)

#keep Walleye from Lake Erie and just a few columns
fish_1 <- fish %>% # fish_1 is not a good naming strategy in practice 
  filter(Species == "Walleye", Lake == "Erie") %>%
  select(Species, Lake, Year, Length_cm, Weight_g) 
head(fish_1) 

fish_2 <- fish %>%
  filter(Species == "Walleye", Lake == "Erie") %>%
  select(-Age_years) 
head(fish_2) 

identical(fish_1, fish_2)

# Mutate creating new variables
fish_3 <- fish %>%
  mutate(
    Weight_kg = Weight_g / 1000
    ) %>%
  select(Species, Lake, Length_cm, Weight_g, Weight_kg) 
head(fish_3)

# Mean length by Lake
fish_4 <- fish %>%
  group_by(Lake) %>%
  summarise(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  ) 
fish_4

# Mean length by Lake × Species
fish_5 <- fish %>%
  group_by(Lake, Species) %>%
  summarise(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  ) 
View(fish_5)

# Mean length by Lake × Species × year
fish_6 <- fish %>%
  group_by(Lake, Species, Year) %>%
  summarise(
    mean_len = mean(Length_cm, na.rm = TRUE),
    n = n()
  ) 
View(fish_6)

# Mean, median length, and total weight by Lake × Species, then create a new column and convert g to kg
fish_7 <- fish %>%
  group_by(Lake, Species) %>%
  summarise(
    mean_len = mean(Length_cm, na.rm = TRUE),
    median_len = median(Length_cm, na.rm = TRUE),
    total_w_g = sum(Weight_g, na.rm = TRUE),
    n = n()
  ) %>%
  mutate (total_w_kg = total_w_g/1000) 
View(fish_7)

# Scatter plot of mean vs median length
plot(fish_7$mean_len, fish_7$median_len,
     xlab = "Mean Length (cm)",
     ylab = "Median Length (cm)",
     main = "Mean vs Median Length by Group",
     pch = 19, col = "blue")

# Add a 1:1 diagonal line
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# What if we want to know: How does the average length of each species change over years?
# To answer that, I first group the data by Species and Year, and then calculate the mean length in each group. That gives us one number — the average length — for each species in each year.
# Then, I plot those averages over time. Each line shows one species, so we can see how their lengths rise or fall year by year.
# This approach — grouping by time and category, then plotting the trend — is one of the most common ways we explore ecological data in R

# 1. Summarise average length per species × year
fish_summary <- fish %>%
  group_by(Species, Year) %>%
  summarise(mean_length = mean(Length_cm, na.rm = TRUE),
            n = n(),
            .groups = "drop")

# 2. Plot: mean length over time for each species
library(ggplot2)
ggplot(fish_summary, aes(x = Year, y = mean_length, color = Species)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Change in Average Fish Length Over Years",
       x = "Year",
       y = "Average Length (cm)") +
  theme_minimal()

# Plot with facets (one panel per species)
ggplot(fish_summary, aes(x = Year, y = mean_length)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred") +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Change in Average Fish Length Over Years",
       x = "Year",
       y = "Average Length (cm)") +
  theme_minimal()

# How is length distributed within each species?
# To see that, I’ll first bin lengths into classes using cut(). 
# Then I’ll count how many fish fall into each bin for each species, and compute percent within species so the panels are comparable even if sample sizes differ.
# Finally, I’ll plot a faceted bar chart—one panel per species—so we can compare distributions at a glance. (This mirrors a histogram but makes the binning and summarising steps explicit in the code.)

# 1) Define bins & labels (adjust breaks for your data range)
bins   <- c(0, 20, 40, 60, Inf)
labels <- c("≤20", "20–40", "40–60", ">60")

# 2) Bin, count, and compute % within each Species
binned <- fish %>%
  mutate(len_bin = cut(Length_cm, breaks = bins, labels = labels)) %>%
  count(Species, len_bin, name = "n") %>%                 # counts per bin per species
  group_by(Species) %>%
  mutate(pct = 100 * n / sum(n))                      # percent within species
  

# 3) Faceted bar “histogram” by Species (using the binned summaries)
ggplot(binned, aes(x = len_bin, y = pct)) +
  geom_col() +
  #geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.3, size = 3) +
  facet_wrap(~ Species, scales = "free_y") +
  labs(title = "Length Distribution by Species",
       x = "Length class (cm)",
       y = "Percent of records") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 9))

# saving data 
# 1) Save as CSV
write.csv(binned, "Output/binned.csv", row.names = FALSE)

# 2) Save as Excel (need writexl package)
# install.packages("writexl")
library(writexl)
write_xlsx(binned, "Output/binned.xlsx")

# 3) Save as RDS 
saveRDS(binned, "Output/binned.rds")

# Compare sizes
file.info(c("Output/binned.csv", "Output/binned.xlsx", "Output/binned.rds"))$size
