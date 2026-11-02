options(repos = c(CRAN = "https://cloud.r-project.org"))


install.packages("palmerpenguins")
install.packages("tidyverse")
install.packages("moments")
install.packages("car")
install.packages("effsize")
install.packages("agricolae")

library(palmerpenguins)
library(tidyverse)
library(moments)      
library(car)          
library(effsize)      
library(agricolae)    

data("penguins")
penguins_clean <- penguins %>% drop_na(body_mass_g, flipper_length_mm, sex, species)


# TASK 1: Descriptive Statistical Analysis

body_mass_stats <- penguins_clean %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )
print("Overall Body Mass Statistics:")
print(body_mass_stats)

species_body_mass_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Mean = mean(body_mass_g),
    Median = median(body_mass_g),
    Min = min(body_mass_g),
    Max = max(body_mass_g),
    Variance = var(body_mass_g),
    SD = sd(body_mass_g),
    Q1 = quantile(body_mass_g, 0.25),
    Q3 = quantile(body_mass_g, 0.75),
    IQR = IQR(body_mass_g),
    Skewness = skewness(body_mass_g),
    Kurtosis = kurtosis(body_mass_g)
  )
print("Species-wise Body Mass Statistics:")
print(species_body_mass_stats)

# 1. Histogram
ggplot(penguins_clean, aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 200, color = "black", alpha = 0.7) +
  labs(title = "Histogram of Penguin Body Mass", x = "Body Mass (g)", y = "Count") +
  theme_minimal()

# 2. Boxplot
ggplot(penguins_clean, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Species-wise Boxplot of Body Mass", x = "Species", y = "Body Mass (g)") +
  theme_minimal()

# 3. Density Plot
ggplot(penguins_clean, aes(x = body_mass_g, fill = species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Body Mass by Species", x = "Body Mass (g)", y = "Density") +
  theme_minimal()


# TASK 2: Hypothesis Testing (Male vs Female Body Mass)

male_mass <- penguins_clean %>% filter(sex == "male") %>% pull(body_mass_g)
female_mass <- penguins_clean %>% filter(sex == "female") %>% pull(body_mass_g)

shapiro.test(male_mass)
shapiro.test(female_mass)

qqnorm(male_mass, main = "QQ Plot - Male Body Mass")
qqline(male_mass, col = "red")

qqnorm(female_mass, main = "QQ Plot - Female Body Mass")
qqline(female_mass, col = "red")

t_test_result <- t.test(body_mass_g ~ sex, data = penguins_clean, var.equal = FALSE)
print(t_test_result)

cohen_d_result <- cohen.d(body_mass_g ~ sex, data = penguins_clean)
print(cohen_d_result)



# TASK 3: One-Way ANOVA (Body Mass across Species)

by(penguins_clean$body_mass_g, penguins_clean$species, shapiro.test)

qqnorm(penguins_clean$body_mass_g[penguins_clean$species == "Adelie"])
qqline(penguins_clean$body_mass_g[penguins_clean$species == "Adelie"])

leveneTest(body_mass_g ~ species, data = penguins_clean)

anova_model <- aov(body_mass_g ~ species, data = penguins_clean)
summary(anova_model)

tukey_result <- TukeyHSD(anova_model)
print(tukey_result)


# TASK 4: Non-Parametric Analysis (Kruskal-Wallis Test)


kw_result <- kruskal.test(body_mass_g ~ species, data = penguins_clean)
print(kw_result)

# TASK 5: Two-Way ANOVA (Species * Sex on Body Mass)

two_way_anova <- aov(body_mass_g ~ species * sex, data = penguins_clean)
summary(two_way_anova)


# ==========================================
# TASK 6: Additional Analysis (Flipper Length)
# ==========================================

species_flipper_stats <- penguins_clean %>%
  group_by(species) %>%
  summarise(
    Mean = mean(flipper_length_mm),
    SD = sd(flipper_length_mm),
    Median = median(flipper_length_mm),
    IQR = IQR(flipper_length_mm)
  )
print(species_flipper_stats)

anova_flipper <- aov(flipper_length_mm ~ species, data = penguins_clean)
summary(anova_flipper)

TukeyHSD(anova_flipper)


# ==========================================
# TASK 7: Additional Visualizations
# ==========================================

ggplot(penguins_clean, aes(x = sex, y = body_mass_g, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Sex-wise Boxplot of Body Mass", x = "Sex", y = "Body Mass (g)") +
  theme_minimal()

ggplot(penguins_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Species-wise Flipper Length Comparison", x = "Species", y = "Flipper Length (mm)") +
  theme_minimal()


