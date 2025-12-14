---
title: "sleep disorder analysis"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Project overview

This project analyzes the **Sleep Disorder Diagnosis Dataset** (`sleep_disorder_dataset.csv`) to understand the **occurrence of sleep disorders**—primarily **Insomnia** and **Sleep Apnea**—and how they relate to **demographic**, **lifestyle**, and **physiological** factors. The dataset contains **~374 observations** and **14 (15 after cleaning) variables** describing participants’ sleep habits and outcomes (e.g., **Sleep.Duration**, **Quality.of.Sleep**), lifestyle indicators (e.g., **Physical.Activity.Level**, **Stress.Level**, **Daily.Steps**, **Caffeine.Intake**), and biometric measures (e.g., **BMI.Category**, **Blood.Pressure**, **Heart.Rate**).

The analysis is designed to provide both descriptive insight (how common each sleep disorder is in this sample) and inferential/statistical conclusions about which factors are associated with sleep disorders and sleep quality. Following the project guidelines, the work begins with **data validation** in RStudio to ensure the dataset is imported correctly and contains no suspicious or missing values that could bias results, then proceeds through exploratory visualization and hypothesis-driven statistical analysis.

Core research questions include:
- Is there an association between **sleep disorder occurrence** and **BMI category**?
- Do **women** sleep longer on average than **men**?
- Are **stress levels** different between **doctors, teachers, and engineers**?
- Can **sleep quality** be predicted from lifestyle habits and physiological indicators using a regression-based model?

Overall, the project aims to translate the dataset into actionable statistical findings that clarify which measurable factors most strongly relate to sleep disorders and overall sleep quality in the observed population.


# 2. Data Validation and Cleaning
...

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

# 4. Questions
## Is there an association between sleep disorders and BMI category?

Both variables are categorical:
- **BMI.Category** (Underweight, Normal, Overweight, Obese)
- **Sleep.Disorder** (None, Insomnia, Sleep Apnea)

Because we are analyzing the relationship between two categorical variables, we use a **Chi-Square Test of Independence**.

---

### Hypotheses

- **Null Hypothesis (H₀):**  
  BMI category and sleep disorder are independent.  
  Knowing a person’s BMI category gives no information about their sleep disorder.

- **Alternative Hypothesis (H₁):**  
  BMI category and sleep disorder are associated.  
  Knowing a person’s BMI category gives information about their sleep disorder.

---

### Statistical Method

The Chi-Square Test of Independence compares:
- the **observed frequencies** of sleep disorders within each BMI category
- the **expected frequencies** assuming the two variables are independent

Large differences between observed and expected counts indicate a possible association.

---

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
