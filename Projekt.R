install.packages("ggplot2")
install.packages("readr")
install.packages("summarytoolls")
install.packages("tidyverse")
install.packages(c("dplyr", "corrplot", "ggpubr", "reshape2"), dependencies = TRUE)

library(dplyr)
library(corrplot)
library(ggpubr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(readr)
library(summarytools)

data <- read_csv("Global_Cybersecurity_Threats_2015-2024.csv")

library(tidyverse)

data %>%
  group_by(Year) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Year, y = Count)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Liczba incydentów cyberbezpieczeństwa w latach 2015–2024",
       x = "Rok", y = "Liczba incydentów") +
  theme_minimal()

data %>%
  count(`Target Industry`, `Attack Type`) %>%
  ggplot(aes(x = `Target Industry`, y = `Attack Type`, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Częstość typów ataków w różnych branżach",
       x = "Branża", y = "Typ ataku", fill = "Liczba incydentów") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank())

data %>%
  ggplot(aes(x = `Attack Type`, y = `Financial Loss (in Million $)`)) +  # użyj dokładnych nazw z colnames()
  geom_boxplot(fill = "tomato", alpha = 0.6, outlier.color = "black") +
  labs(title = "Rozkład strat finansowych według typu ataku",
       x = "Typ ataku", y = "Straty finansowe (mln USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_blank())


library(ggplot2)
library(ggpubr)

# Funkcja pomocnicza: przeskalowanie gęstości do liczby przypadków
scale_density <- function(density_obj, hist_obj) {
  max_hist <- max(hist_obj$counts)
  max_density <- max(density_obj$y)
  hist_obj$density_scale <- density_obj$y * (max_hist / max_density)
  hist_obj
}

# Histogram i gęstość – straty finansowe
dens1 <- density(data$`Financial Loss (in Million $)`, na.rm = TRUE)
hist1 <- hist(data$`Financial Loss (in Million $)`, plot = FALSE, breaks = seq(0, 100, by = 10))
hist1 <- scale_density(dens1, hist1)

g1 <- ggplot(data, aes(x = `Financial Loss (in Million $)`)) +
  geom_histogram(binwidth = 10, fill = "#ff6666", color = "black") +
  geom_line(data = data.frame(x = dens1$x, y = hist1$density_scale), aes(x = x, y = y), color = "darkred", size = 1) +
  labs(title = "Rozkład strat finansowych", x = "Straty (mln USD)", y = "Liczba incydentów") +
  theme_minimal()
g1

# Histogram i gęstość – liczba użytkowników
dens2 <- density(data$`Number of Affected Users`, na.rm = TRUE)
hist2 <- hist(data$`Number of Affected Users`, plot = FALSE, breaks = seq(0, 1000000, by = 50000))
hist2 <- scale_density(dens2, hist2)

g2 <- ggplot(data, aes(x = `Number of Affected Users`)) +
  geom_histogram(binwidth = 50000, fill = "#66b3ff", color = "black") +
  geom_line(data = data.frame(x = dens2$x, y = hist2$density_scale), aes(x = x, y = y), color = "blue", size = 1) +
  labs(title = "Rozkład liczby poszkodowanych", x = "Liczba użytkowników", y = "Liczba incydentów") +
  theme_minimal()
g2

# Histogram i gęstość – czas
dens3 <- density(data$`Incident Resolution Time (in Hours)`, na.rm = TRUE)
hist3 <- hist(data$`Incident Resolution Time (in Hours)`, plot = FALSE, breaks = seq(0, 75, by = 5))
hist3 <- scale_density(dens3, hist3)

g3 <- ggplot(data, aes(x = `Incident Resolution Time (in Hours)`)) +
  geom_histogram(binwidth = 5, fill = "#99ff99", color = "black") +
  geom_line(data = data.frame(x = dens3$x, y = hist3$density_scale), aes(x = x, y = y), color = "darkgreen", size = 1) +
  labs(title = "Czas rozwiązania incydentu", x = "Czas (godziny)", y = "Liczba incydentów") +
  theme_minimal()
g3

