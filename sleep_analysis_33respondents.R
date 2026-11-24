# ============================================================
# Sleep Habits & Academic Performance Survey - Analysis Script
# Module: IT1212 Probability and Statistics
# 33 Respondents - Updated Analysis
# ============================================================

# Read the CSV file
data <- read.csv("Sleep_Habits___Academic_Performance_Survey-2.csv", stringsAsFactors = FALSE)

# Display initial info
cat("Number of respondents:", nrow(data), "\n\n")
cat("Column names:\n")
print(names(data))
cat("\n")

# Rename columns to clean names for easier analysis
colnames(data) <- c("timestamp", "age_group", "gender", "year_of_study", "program", 
                     "sleep_hours", "sleep_quality", "consistent_schedule", "caffeine", 
                     "screen_time_before_bed", "gpa", "concentration_difficulty", 
                     "stress_level", "missed_class", "self_rated_performance")

# Convert numeric columns
data$sleep_hours <- as.numeric(data$sleep_hours)
data$screen_time_before_bed <- as.numeric(data$screen_time_before_bed)
data$gpa <- as.numeric(data$gpa)

# ============================================================
# Custom mode function
# ============================================================
get_mode <- function(v) {
  v <- v[!is.na(v) & v != ""]
  if (length(v) == 0) return(NA)
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
}

# ============================================================
# Descriptive statistics function
# ============================================================
describe_numeric <- function(v, name) {
  v <- as.numeric(v)
  v <- v[!is.na(v)]
  cat("---", name, "---\n")
  cat("n =", length(v), "\n")
  cat("Mean =", round(mean(v), 2), "\n")
  cat("Median =", round(median(v), 2), "\n")
  cat("Mode =", round(get_mode(as.character(v)), 2), "\n")
  cat("Range =", round(max(v) - min(v), 2), paste0("(", min(v), "-", max(v), ")"), "\n")
  cat("Variance =", round(var(v), 3), "\n")
  cat("Standard Deviation =", round(sd(v), 3), "\n\n")
}

# ============================================================
# SECTION 1: DEMOGRAPHIC DISTRIBUTIONS
# ============================================================
cat("============ DEMOGRAPHIC DISTRIBUTIONS ============\n\n")

cat("-- Age Group --\n")
print(table(data$age_group))
cat("\n")

cat("-- Gender --\n")
print(table(data$gender))
cat("\n")

cat("-- Year of Study --\n")
print(table(data$year_of_study))
cat("\n")

# ============================================================
# SECTION 2: NUMERIC DESCRIPTIVE STATISTICS
# ============================================================
cat("\n============ NUMERIC ATTRIBUTES ============\n")
describe_numeric(data$sleep_hours, "Sleep Hours per Night")
describe_numeric(data$screen_time_before_bed, "Screen Time Before Bed (hours)")
describe_numeric(data$gpa, "GPA (out of 4.0)")

# ============================================================
# SECTION 3: CATEGORICAL FREQUENCY TABLES
# ============================================================
cat("\n============ CATEGORICAL ATTRIBUTES ============\n")

cat("-- Sleep Quality --\n")
print(table(data$sleep_quality))
cat("\n")

cat("-- Consistent Schedule --\n")
print(table(data$consistent_schedule))
cat("\n")

cat("-- Caffeine Before Bed --\n")
print(table(data$caffeine))
cat("\n")

cat("-- Concentration Difficulty --\n")
print(table(data$concentration_difficulty))
cat("\n")

cat("-- Stress Level --\n")
print(table(data$stress_level))
cat("\n")

cat("-- Missed Class --\n")
print(table(data$missed_class))
cat("\n")

cat("-- Self-Rated Performance --\n")
print(table(data$self_rated_performance))
cat("\n")

# ============================================================
# SECTION 4: GENERATE CHARTS (saved to /plots)
# ============================================================
dir.create("plots", showWarnings = FALSE)

# Pie charts for demographics
png("plots/age_pie.png", width = 600, height = 450)
pie(table(data$age_group), main = "Age Group Distribution", col = rainbow(length(unique(data$age_group))),
    labels = paste0(names(table(data$age_group)), " (", as.vector(table(data$age_group)), ")"))
dev.off()
cat("✓ Created: age_pie.png\n")

png("plots/gender_pie.png", width = 600, height = 450)
pie(table(data$gender), main = "Gender Distribution", col = rainbow(length(unique(data$gender))),
    labels = paste0(names(table(data$gender)), " (", as.vector(table(data$gender)), ")"))
dev.off()
cat("✓ Created: gender_pie.png\n")

png("plots/year_pie.png", width = 600, height = 450)
pie(table(data$year_of_study), main = "Year of Study Distribution", col = rainbow(length(unique(data$year_of_study))),
    labels = paste0(names(table(data$year_of_study)), " (", as.vector(table(data$year_of_study)), ")"))
dev.off()
cat("✓ Created: year_pie.png\n")

# Histograms for numeric variables
png("plots/sleep_hours_hist.png", width = 600, height = 450)
hist(data$sleep_hours, col = "steelblue", breaks = 8, main = "Distribution of Nightly Sleep Hours", 
     xlab = "Hours of Sleep", ylab = "Frequency")
dev.off()
cat("✓ Created: sleep_hours_hist.png\n")

png("plots/screen_time_hist.png", width = 600, height = 450)
hist(data$screen_time_before_bed, col = "mediumpurple", breaks = 8, main = "Screen Time Before Bed", 
     xlab = "Hours", ylab = "Frequency")
dev.off()
cat("✓ Created: screen_time_hist.png\n")

png("plots/gpa_hist.png", width = 600, height = 450)
hist(data$gpa, col = "seagreen", breaks = 8, main = "Distribution of GPA", 
     xlab = "GPA (out of 4.0)", ylab = "Frequency")
dev.off()
cat("✓ Created: gpa_hist.png\n")

# Pie charts for categorical variables
png("plots/sleep_quality_pie.png", width = 600, height = 450)
pie(table(data$sleep_quality), main = "Perceived Sleep Quality", col = rainbow(length(unique(data$sleep_quality))),
    labels = paste0(names(table(data$sleep_quality)), " (", as.vector(table(data$sleep_quality)), ")"))
dev.off()
cat("✓ Created: sleep_quality_pie.png\n")

png("plots/schedule_pie.png", width = 600, height = 450)
pie(table(data$consistent_schedule), main = "Consistent Sleep Schedule", col = rainbow(length(unique(data$consistent_schedule))),
    labels = paste0(names(table(data$consistent_schedule)), " (", as.vector(table(data$consistent_schedule)), ")"))
dev.off()
cat("✓ Created: schedule_pie.png\n")

png("plots/missed_class_pie.png", width = 600, height = 450)
pie(table(data$missed_class), main = "Missed Class Due to Poor Sleep", col = rainbow(length(unique(data$missed_class))),
    labels = paste0(names(table(data$missed_class)), " (", as.vector(table(data$missed_class)), ")"))
dev.off()
cat("✓ Created: missed_class_pie.png\n")

# Bar charts for categorical variables with ordering
png("plots/caffeine_bar.png", width = 600, height = 450)
caffeine_levels <- c("Never", "Rarely", "Sometimes", "Often")
caffeine_table <- table(factor(data$caffeine, levels = caffeine_levels))
barplot(caffeine_table, col = "steelblue", main = "Caffeine Intake Before Bed", 
        ylab = "Number of Students", names.arg = caffeine_levels)
dev.off()
cat("✓ Created: caffeine_bar.png\n")

png("plots/concentration_bar.png", width = 600, height = 450)
conc_levels <- c("Never", "Rarely", "Sometimes", "Often")
conc_table <- table(factor(data$concentration_difficulty, levels = conc_levels))
barplot(conc_table, col = "darkorange", main = "Difficulty Concentrating Due to Tiredness", 
        ylab = "Number of Students", names.arg = conc_levels)
dev.off()
cat("✓ Created: concentration_bar.png\n")

png("plots/stress_level_bar.png", width = 600, height = 450)
stress_levels <- c("very low", "low", "moderate", "high", "very high")
stress_table <- table(factor(data$stress_level, levels = stress_levels))
barplot(stress_table, col = "salmon", main = "Perceived Stress Level", 
        ylab = "Number of Students", names.arg = stress_levels)
dev.off()
cat("✓ Created: stress_level_bar.png\n")

png("plots/performance_bar.png", width = 600, height = 450)
perf_levels <- c("poor", "average", "good", "excellent")
perf_table <- table(factor(data$self_rated_performance, levels = perf_levels))
barplot(perf_table, col = "lightblue", main = "Self-Rated Academic Performance", 
        ylab = "Number of Students", names.arg = perf_levels)
dev.off()
cat("✓ Created: performance_bar.png\n")

cat("\n✓ All charts created successfully in /plots folder\n\n")

# ============================================================
# SECTION 5: HYPOTHESIS TESTING (One-sample t-tests)
# ============================================================
cat("\n============ HYPOTHESIS TESTING ============\n")

cat("\n-- Test 1: Is average sleep duration below the recommended 7 hours? --\n")
cat("H0: mean sleep >= 7 hours | H1: mean sleep < 7 hours\n")
sleep_test <- t.test(data$sleep_hours, mu = 7, alternative = "less", conf.level = 0.95)
print(sleep_test)

cat("\n-- Test 2: Is average GPA above the 3.0 'satisfactory' benchmark? --\n")
cat("H0: mean GPA <= 3.0 | H1: mean GPA > 3.0\n")
gpa_vals <- data$gpa[!is.na(data$gpa)]
gpa_test <- t.test(gpa_vals, mu = 3.0, alternative = "greater", conf.level = 0.95)
print(gpa_test)

# ============================================================
# SECTION 6: CHI-SQUARED TESTS OF INDEPENDENCE
# ============================================================
cat("\n============ CHI-SQUARED TESTS ============\n")

cat("\n-- Test 1: Consistent sleep schedule vs. missed classes --\n")
cat("H0: No association | H1: There is an association\n")
tab1 <- table(data$consistent_schedule, data$missed_class)
print(tab1)
chi1 <- chisq.test(tab1, simulate.p.value = TRUE, B = 10000)
print(chi1)

cat("\n-- Test 2: Caffeine intake before bed vs. concentration difficulty --\n")
cat("H0: No association | H1: There is an association\n")
tab2 <- table(data$caffeine, data$concentration_difficulty)
print(tab2)
chi2 <- chisq.test(tab2, simulate.p.value = TRUE, B = 10000)
print(chi2)

cat("\n============ END OF ANALYSIS ============\n")
