---
title: "sleep disorder analysis"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# 1. Project overview

This project analyzes the **Sleep Disorder Diagnosis Dataset** (`sleep_disorder_dataset.csv`) to understand the **occurrence of sleep disorders**—primarily **Insomnia** and **Sleep Apnea**—and how they relate to **demographic**, **lifestyle**, and **physiological** factors. The dataset contains **\~374 observations** and **14 (15 after cleaning) variables** describing participants’ sleep habits and outcomes (e.g., **Sleep.Duration**, **Quality.of.Sleep**), lifestyle indicators (e.g., **Physical.Activity.Level**, **Stress.Level**, **Daily.Steps**, **Caffeine.Intake**), and biometric measures (e.g., **BMI.Category**, **Blood.Pressure**, **Heart.Rate**).

The analysis is designed to provide both descriptive insight (how common each sleep disorder is in this sample) and inferential/statistical conclusions about which factors are associated with sleep disorders and sleep quality. Following the project guidelines, the work begins with **data validation** in RStudio to ensure the dataset is imported correctly and contains no suspicious or missing values that could bias results, then proceeds through exploratory visualization and hypothesis-driven statistical analysis.

Core research questions include: - Is there an association between **sleep disorder occurrence** and **BMI category**? - Do **women** sleep longer on average than **men**? - Are **stress levels** different between **doctors, teachers, and engineers**? - Can **sleep quality** be predicted from lifestyle habits and physiological indicators using a regression-based model?

Overall, the project aims to translate the dataset into actionable statistical findings that clarify which measurable factors most strongly relate to sleep disorders and overall sleep quality in the observed population.

# 2. Data Validation and Cleaning

Ensuring that the dataset is clean, consistent and reliable is a cruical step before analysis. Data cleaning involves checking each column for missing values, wrong entries or inconsistencies and correcting them. Numeric columns such as age, sleep duration, quality of sleep, psychical activity level, stress level, heart rate and daily sleeps and caffeine intake were summarized to confirm that all values fall within reasonable range and no impossible values or outliers exist. Categorical variables including gender, occupation, BMI category and sleep disorder, were checked for consistency and duplication. Blood pressure was special case, where format of it is "systolic/diastolic". It was carefully handled and split into 2 variables systolic and diastolic. Additionally, the first few rows of dataset were visually inspected, and unique values were reviewed to validate that no formatting errors were present. This detailed cleaning and validation process ensures that dataset is ready for accurate exploratory analysis and statistical testing.

# 3. Preliminary Data Analysis

After cleaning, we will begin exploratory data analysis to understand the dataset's characteristics.

We will first look at the distribution of individual variables to get a baseline.

## Target Variable: Sleep.Disorder

```{r}
library(tidyverse)
library(scales)

df <- read.csv("sleep_data_cleaned.csv")

df <- df %>%
  mutate(Sleep.Disorder = replace_na(Sleep.Disorder, "None"))

# Generate the Bar Chart
ggplot(df, aes(x = Sleep.Disorder)) +
  geom_bar(fill = "steelblue") +
  # Add text labels with Count and Percentage
  geom_text(stat = "count", 
            aes(label = paste0(after_stat(count), "\n(", 
                               percent(after_stat(count) / sum(after_stat(count)), accuracy = 0.1), ")")),
            vjust = -0.25, 
            size = 4) +
  # Apply formatting
  theme_minimal() +
  labs(title = "Distribution of Sleep Disorders (Target Variable)",
       x = "Sleep Disorder Category",
       y = "Count of Individuals") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

## Lifestyle Factors: Stress Level, Physical Activity Level, and Daily Steps

Based on the exploratory plots of the lifestyle variables, the sample appears **moderately active overall**. The **Daily.Steps** distribution (*Plot 1*) shows that most participants fall roughly between **4,500 and 8,500 steps per day**, with the highest concentration around **7,500–8,000 steps**, and a smaller group reaching close to **10,000 steps**. This suggests that a large portion of the dataset consists of individuals who achieve a moderate amount of daily movement.

We chose to display the data using a density plot rather than a histogram because the histogram produced sharp spikes, making the distribution harder to interpret.

<img src="https://github.com/user-attachments/assets/cf292cf2-2f54-4a87-bb18-7b99bcdd817a" alt="image" width="865" height="546"/>

*Plot 1: Daily steps distribution.*

The **Physical.Activity.Level** plot (*Plot 2*) indicates that activity is not spread smoothly across the scale; instead, participants cluster at a few specific values (visible as strong spikes). This implies the variable likely represents **predefined or rounded activity scores**, meaning the dataset contains several distinct activity groups rather than a continuous range. The central tendency (marked by the vertical reference line) lies around the middle of the scale, reinforcing the conclusion that the “typical” participant has a **moderate activity level**.

This time, we chose a histogram rather than a density plot because the density plot would make it harder to interpret and summarize the overall physical activity level.

<img src="https://github.com/user-attachments/assets/ebf8beed-72c2-40ce-adfd-b8fb76c34bd3" alt="image" width="865" height="546"/>

*Plot 2: Physical Activity Level Distribution.*

For **Stress.Level**, the distribution (*Plot 3*) is concentrated in the **mid-to-higher range (approximately 3–8)**. There is no dominance of very low stress values, and the presence of many observations toward the upper levels indicates that a meaningful share of participants experience **elevated stress**. Overall, the plots suggest a population that is generally **moderately active**, but with **moderate-to-high stress levels**, which may be relevant when later examining relationships with sleep quality and sleep disorders.

<img src="https://github.com/user-attachments/assets/76ac7c4f-a477-4a53-a31d-ca9c2efd92b4" alt="image" width="865" height="546"/>

*Plot 3: Stress Level Distribution.*

R code for computing Plots 1-3:

``` r
library(readr)
library(ggplot2)

df <- read_csv("sleep_data_cleaned.csv")

ggplot(df, aes(x = Daily.Steps)) +
  geom_density(fill = "#69b3a2", alpha = 0.6) +
  theme_classic() +
  labs(title = "Daily Steps Density", x = "Steps")

ggplot(df, aes(x = factor(Physical.Activity.Level))) +
  geom_vline(xintercept = 8, linetype = "dashed", color = "darkred", size = 1) +
  geom_bar(fill = "#69b3a2", color = "white") +
  labs(title = "Physical activity level distribution", x = "Physical activity level", y = "Count") +
  theme_minimal()

ggplot(df, aes(x = factor(Stress.Level, levels = 1:9))) + 
  geom_bar(fill = "#69b3a2", color = "white") + 
  scale_x_discrete(drop = FALSE) +  
  theme_minimal() + 
  labs(title = "Distribution of Stress Levels", x = "Level", y = "Count")
```

## Biometrics: Sleep Duration, Quality of Sleep, and Heart Rate

To describe the baseline sleep and physiological characteristics of the participants, we examined the distributions of **Sleep.Duration**, **Quality.of.Sleep**, and **Heart.Rate**. These plots provide an overview of what is “typical” in the dataset and help frame later comparisons across gender, occupation, and sleep disorder groups.

The **Sleep.Duration** distribution (*Plot 4*) shows that most participants sleep between roughly **6 and 8.5 hours**, with noticeable peaks around **\~6–6.5 hours** and **\~7.5–8 hours**. Very short (\<6 hours) and very long (\~9 hours) sleep durations are less common. Overall, the sample appears to cluster around a fairly typical sleep range, but with clear variation that may be linked to lifestyle factors or sleep disorder status.

<img src="https://github.com/user-attachments/assets/48f1a730-bccf-40ac-8704-892a5e94a358" alt="image" width="798" height="546"/>

*Plot 4: Sleep Duration Distribution.*

The **Quality.of.Sleep** distribution (*Plot 5*) is concentrated in the **mid-to-high range**, with the highest density around approximately **7–8** on the 1–10 scale. Lower scores (around 4–5) occur less frequently, suggesting that most participants report **moderate to good sleep quality**, while a smaller subset experiences poorer perceived sleep quality.

<img src="https://github.com/user-attachments/assets/9c6c65bb-5d59-4634-9891-477fced5a473" alt="image" width="798" height="546"/>

*Plot 5: Quality of Sleep Distribution.*

Finally, the **Heart.Rate** distribution (*Plot 6*) is centered around the **high 60s to low 70s bpm**, indicating that the majority of participants fall within a relatively typical resting heart rate range. The distribution is slightly right-skewed, with fewer individuals showing higher values (above \~80 bpm). These higher heart rates may reflect differences in fitness, stress, or health status and could be informative when examined alongside stress, activity, and sleep disorder categories.

<img src="https://github.com/user-attachments/assets/c5cd904a-d3de-416e-b747-4ea711082c79" alt="image" width="798" height="546"/>

*Plot 6: Heart Rate Distribution.*

R code for computing Plots 4-6:

``` r
library(readr)
library(ggplot2)

df <- read_csv("sleep_data_cleaned.csv")

ggplot(df, aes(x = Sleep.Duration)) +
  geom_density(fill = "#4E79A7", color = "#4E79A7", alpha = 0.6) +
  xlim(5, 9) +
  theme_classic() +
  labs(title = "Sleep Duration Distribution", x = "Hours", y = "Density")

ggplot(df, aes(x = Quality.of.Sleep)) +
  geom_density(fill = "#59A14F", color = "#59A14F", alpha = 0.6, adjust = 2) +
  theme_classic() +
  labs(title = "Quality of Sleep Ratings", x = "Quality Score (1-10)", y = "Density")

ggplot(df, aes(x = Heart.Rate)) +
  geom_histogram(binwidth = 1, fill = "#E15759", color = "white", alpha = 0.7) +
  xlim(60, 90) +
  theme_classic() +
  labs(title = "Heart Rate Distribution", x = "Heart Rate (bpm)", y = "Count")
```

# 4. Questions

## Is there an association between sleep disorders and BMI category?

Both variables are categorical: - **BMI.Category** (Underweight, Normal, Overweight, Obese) - **Sleep.Disorder** (None, Insomnia, Sleep Apnea)

Because we are analyzing the relationship between two categorical variables, we use a **Chi-Square Test of Independence**.

------------------------------------------------------------------------

### Hypotheses

-   **Null Hypothesis (H₀):**\
    BMI category and sleep disorder are independent.\
    Knowing a person’s BMI category gives no information about their sleep disorder.

-   **Alternative Hypothesis (H₁):**\
    BMI category and sleep disorder are associated.\
    Knowing a person’s BMI category gives information about their sleep disorder.

------------------------------------------------------------------------

### Statistical Method

The Chi-Square Test of Independence compares: - the **observed frequencies** of sleep disorders within each BMI category - the **expected frequencies** assuming the two variables are independent

Large differences between observed and expected counts indicate a possible association.

------------------------------------------------------------------------

### Contingency Table

```{r}
bmi_sleep_table <- table(df$BMI.Category, df$Sleep.Disorder)

bmi_sleep_table
```

### Chi-Square test

```{r}
chi_test <- chisq.test(bmi_sleep_table)

chi_test
```

### P-value

```{r}
chi_test$p.value
```

```{r}
if (chi_test$p.value < 0.05) {
  cat("Conclusion: The p-value is less than 0.05. We reject the null hypothesis and conclude that there is a statistically significant association between BMI category and sleep disorder.")
} else {
  cat("Conclusion: The p-value is greater than or equal to 0.05. We fail to reject the null hypothesis and conclude that there is no statistically significant association between BMI category and sleep disorder.")
}

```

## Do women sleep longer on average than men?

...
