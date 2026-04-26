# Skip the first 6 rows of BehaviorSpace metadata
df <- read.csv("Experiment 1 - Main", skip = 6, header = TRUE)

# Rename columns
colnames(df) <- c(
  "run_number", "seed_id", "ratio_param", "scenario", "step", "ticks",
  "wealthy_clinic_ratio", "dynamic_lifespace",
  "n_poor_patients", "n_wealthy_patients",
  "n_poor_patches", "n_wealthy_patches",
  "total_poor_no_access", "total_wealthy_no_access",
  "total_poor_fail", "total_wealthy_fail",
  "total_poor_deaths", "total_wealthy_deaths"
)

# Condition label
df$condition <- ifelse(df$dynamic_lifespace == "true", "spiral", "baseline")

# Per-capita rates
df$no_access_rate_poor    <- df$total_poor_no_access / df$n_poor_patients
df$no_access_rate_wealthy <- df$total_wealthy_no_access / df$n_wealthy_patients

df$death_rate_poor    <- df$total_poor_deaths / df$n_poor_patients
df$death_rate_wealthy <- df$total_wealthy_deaths / df$n_wealthy_patients

df$fail_rate_poor    <- df$total_poor_fail / df$n_poor_patients
df$fail_rate_wealthy <- df$total_wealthy_fail / df$n_wealthy_patients

# Gaps (poor rate - wealthy rate)
df$no_access_gap <- df$no_access_rate_poor - df$no_access_rate_wealthy
df$death_gap     <- df$death_rate_poor - df$death_rate_wealthy
df$fail_gap      <- df$fail_rate_poor - df$fail_rate_wealthy

# Parameter sweep: means of gaps per ratio x condition
aggregate(no_access_gap ~ ratio_param + condition, data = df, FUN = mean)

aggregate(death_gap ~ ratio_param + condition, data = df, FUN = mean)

aggregate(fail_gap ~ ratio_param + condition, data = df, FUN = mean)


for (gap in c("no_access_gap", "death_gap", "fail_gap")) {
  cat("\n---", gap, "---\n")
  
  for (r in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
    baseline <- df[df$ratio_param == r & df$condition == "baseline", ]
    spiral <- df[df$ratio_param == r & df$condition == "spiral", ]
    baseline <- baseline[order(baseline$seed_id), ]
    spiral <- spiral[order(spiral$seed_id), ]
    
    diffs <- spiral[[gap]] - baseline[[gap]]
    t_test  <- t.test(spiral[[gap]], baseline[[gap]], paired = TRUE)
    cohens_d     <- mean(diffs) / sd(diffs)
    
    print(paste("Ratio:", r, 
                "t:", round(t_test$statistic, 6), 
                "p:", round(t_test$p.value, 6), 
                "Cohen's d:", round(cohens_d, 6)))
  }
}

for (cond in c("baseline", "spiral")) {
  print(paste("---", cond, "---"))
  
  sub <- df[df$condition == cond,]
  
  for (gap in c("no_access_gap", "death_gap", "fail_gap")) {
    test <- cor.test(sub$ratio_param, sub[[gap]])
    print(paste(gap, "r:", round(test$estimate, 6), "p:", round(test$p.value, 6)))
  }
}

# --- No-Access Gap ---
means <- aggregate(no_access_gap ~ ratio_param + condition, data = df, FUN = mean)
baseline_means <- means[means$condition == "baseline", ]
spiral_means   <- means[means$condition == "spiral", ]

plot(baseline_means$ratio_param, baseline_means$no_access_gap, 
     type = "b", col = "blue", ylim = c(0, 90),
     xlab = "Wealthy Clinic Ratio", ylab = "No-Access Gap",
     main = "No-Access Gap: Baseline vs Spiral")
lines(spiral_means$ratio_param, spiral_means$no_access_gap, type = "b", col = "red")
legend("topleft", legend = c("Baseline", "Spiral"), col = c("blue", "red"), lty = 1)

# --- Death Gap ---
means <- aggregate(death_gap ~ ratio_param + condition, data = df, FUN = mean)
baseline_means <- means[means$condition == "baseline", ]
spiral_means   <- means[means$condition == "spiral", ]

plot(baseline_means$ratio_param, baseline_means$death_gap, 
     type = "b", col = "blue", ylim = c(0, 2.5),
     xlab = "Wealthy Clinic Ratio", ylab = "Death Gap",
     main = "Death Gap: Baseline vs Spiral")
lines(spiral_means$ratio_param, spiral_means$death_gap, type = "b", col = "red")
legend("topleft", legend = c("Baseline", "Spiral"), col = c("blue", "red"), lty = 1)

# --- Fail Gap ---
means <- aggregate(fail_gap ~ ratio_param + condition, data = df, FUN = mean)
baseline_means <- means[means$condition == "baseline", ]
spiral_means   <- means[means$condition == "spiral", ]

plot(baseline_means$ratio_param, baseline_means$fail_gap, 
     type = "b", col = "blue", ylim = c(0, 15),
     xlab = "Wealthy Clinic Ratio", ylab = "Fail Gap",
     main = "Fail Gap: Baseline vs Spiral")
lines(spiral_means$ratio_param, spiral_means$fail_gap, type = "b", col = "red")
legend("topleft", legend = c("Baseline", "Spiral"), col = c("blue", "red"), lty = 1)


# --- Load Experiment 2 ---
raw2 <- read.csv("Experiment 2 - Main", skip = 6, header = TRUE)

colnames(raw2) <- c(
  "run_number", "seed_id", "ratio_param", "scenario", "step", "ticks",
  "wealthy_clinic_ratio", "dynamic_lifespace",
  "n_poor_patients", "n_wealthy_patients",
  "n_poor_patches", "n_wealthy_patches",
  "total_poor_no_access", "total_wealthy_no_access",
  "total_poor_fail", "total_wealthy_fail",
  "total_poor_deaths", "total_wealthy_deaths"
)

# --- Condition label ---
df2 <- raw2
df2$condition <- ifelse(grepl("Mobile", df2$scenario), "mobile", "spiral")

# --- Per-capita rates ---
df2$no_access_rate_poor    <- df2$total_poor_no_access / df2$n_poor_patients
df2$no_access_rate_wealthy <- df2$total_wealthy_no_access / df2$n_wealthy_patients
df2$death_rate_poor        <- df2$total_poor_deaths / df2$n_poor_patients
df2$death_rate_wealthy     <- df2$total_wealthy_deaths / df2$n_wealthy_patients
df2$fail_rate_poor         <- df2$total_poor_fail / df2$n_poor_patients
df2$fail_rate_wealthy      <- df2$total_wealthy_fail / df2$n_wealthy_patients

# --- Gaps ---
df2$no_access_gap <- df2$no_access_rate_poor - df2$no_access_rate_wealthy
df2$death_gap     <- df2$death_rate_poor - df2$death_rate_wealthy
df2$fail_gap      <- df2$fail_rate_poor - df2$fail_rate_wealthy

# --- Paired t-tests ---
for (gap in c("no_access_gap", "death_gap", "fail_gap")) {
  spiral <- df2[df2$condition == "spiral", ]
  mobile <- df2[df2$condition == "mobile", ]
  spiral <- spiral[order(spiral$seed_id), ]
  mobile <- mobile[order(mobile$seed_id), ]
  
  diffs <- mobile[[gap]] - spiral[[gap]]
  test  <- t.test(mobile[[gap]], spiral[[gap]], paired = TRUE)
  cohens_d <- mean(diffs) / sd(diffs)
  
  print(paste("---", gap, "---"))
  print(paste("Spiral mean:", round(mean(spiral[[gap]]), 3),
              "Mobile mean:", round(mean(mobile[[gap]]), 3),
              "Diff:", round(mean(diffs), 3),
              "t:", round(test$statistic, 3),
              "p:", round(test$p.value, 6),
              "Cohen's d:", round(cohens_d, 3)))
}

# --- Load Experiment 1 Timeline ---
tl1 <- read.csv("Experiment 1 - Timeline", skip = 6, header = TRUE)

colnames(tl1) <- c(
  "run_number", "seed_id", "ratio_param", "scenario", "step", "ticks",
  "seed_id2", "dynamic_lifespace",
  "n_poor_patients", "n_wealthy_patients",
  "total_poor_no_access", "total_wealthy_no_access",
  "total_poor_fail", "total_wealthy_fail",
  "total_poor_deaths", "total_wealthy_deaths"
)

tl1$condition <- ifelse(tl1$dynamic_lifespace == "true", "spiral", "baseline")

tl1$no_access_rate_poor    <- tl1$total_poor_no_access / tl1$n_poor_patients
tl1$no_access_rate_wealthy <- tl1$total_wealthy_no_access / tl1$n_wealthy_patients
tl1$death_rate_poor        <- tl1$total_poor_deaths / tl1$n_poor_patients
tl1$death_rate_wealthy     <- tl1$total_wealthy_deaths / tl1$n_wealthy_patients

tl1$no_access_gap <- tl1$no_access_rate_poor - tl1$no_access_rate_wealthy
tl1$death_gap     <- tl1$death_rate_poor - tl1$death_rate_wealthy

# Mean gap per step per condition (averaged over 30 seeds)
tl1_no_access <- aggregate(no_access_gap ~ step + condition, data = tl1, FUN = mean)
tl1_death     <- aggregate(death_gap ~ step + condition, data = tl1, FUN = mean)

tl1_baseline_na <- tl1_no_access[tl1_no_access$condition == "baseline", ]
tl1_spiral_na   <- tl1_no_access[tl1_no_access$condition == "spiral", ]

plot(tl1_baseline_na$step, tl1_baseline_na$no_access_gap,
     type = "l", col = "blue", ylim = c(0, 120),
     xlab = "Tick", ylab = "No-Access Gap (poor rate - wealthy rate)",
     main = "No-Access Gap Over Time: Baseline vs Spiral")
lines(tl1_spiral_na$step, tl1_spiral_na$no_access_gap, col = "red")
legend("topleft", legend = c("Baseline", "Spiral"), col = c("blue", "red"), lty = 1)

tl1_baseline_d <- tl1_death[tl1_death$condition == "baseline", ]
tl1_spiral_d   <- tl1_death[tl1_death$condition == "spiral", ]

plot(tl1_baseline_d$step, tl1_baseline_d$death_gap,
     type = "l", col = "blue", ylim = c(0, 3),
     xlab = "Tick", ylab = "Death Gap (poor rate - wealthy rate)",
     main = "Death Gap Over Time: Baseline vs Spiral")
lines(tl1_spiral_d$step, tl1_spiral_d$death_gap, col = "red")
legend("topleft", legend = c("Baseline", "Spiral"), col = c("blue", "red"), lty = 1)

# --- Load Experiment 2 Timeline ---
tl2 <- read.csv("Experiment 2 - Timeline", skip = 6, header = TRUE)

colnames(tl2) <- c(
  "run_number", "seed_id", "ratio_param", "scenario", "step", "ticks",
  "seed_id2", "mobile_clinic",
  "n_poor_patients", "n_wealthy_patients",
  "total_poor_no_access", "total_wealthy_no_access",
  "total_poor_fail", "total_wealthy_fail",
  "total_poor_deaths", "total_wealthy_deaths"
)

tl2$condition <- ifelse(grepl("Mobile", tl2$scenario), "mobile", "spiral")

tl2$no_access_rate_poor    <- tl2$total_poor_no_access / tl2$n_poor_patients
tl2$no_access_rate_wealthy <- tl2$total_wealthy_no_access / tl2$n_wealthy_patients
tl2$death_rate_poor        <- tl2$total_poor_deaths / tl2$n_poor_patients
tl2$death_rate_wealthy     <- tl2$total_wealthy_deaths / tl2$n_wealthy_patients

tl2$no_access_gap <- tl2$no_access_rate_poor - tl2$no_access_rate_wealthy
tl2$death_gap     <- tl2$death_rate_poor - tl2$death_rate_wealthy

tl2_no_access <- aggregate(no_access_gap ~ step + condition, data = tl2, FUN = mean)
tl2_death     <- aggregate(death_gap ~ step + condition, data = tl2, FUN = mean)

tl2_spiral_na <- tl2_no_access[tl2_no_access$condition == "spiral", ]
tl2_mobile_na <- tl2_no_access[tl2_no_access$condition == "mobile", ]

plot(tl2_spiral_na$step, tl2_spiral_na$no_access_gap,
     type = "l", col = "red", ylim = c(0, 120),
     xlab = "Tick", ylab = "No-Access Gap (poor rate - wealthy rate)",
     main = "No-Access Gap Over Time: Spiral vs Mobile Clinic")
lines(tl2_mobile_na$step, tl2_mobile_na$no_access_gap, col = "blue")
legend("topleft", legend = c("Spiral", "Mobile Clinic"), col = c("red", "blue"), lty = 1)


tl2_spiral_d <- tl2_death[tl2_death$condition == "spiral", ]
tl2_mobile_d <- tl2_death[tl2_death$condition == "mobile", ]

plot(tl2_spiral_d$step, tl2_spiral_d$death_gap,
     type = "l", col = "red", ylim = c(0, 3),
     xlab = "Tick", ylab = "Death Gap (poor rate - wealthy rate)",
     main = "Death Gap Over Time: Spiral vs Mobile Clinic")
lines(tl2_mobile_d$step, tl2_mobile_d$death_gap, col = "blue")
legend("topleft", legend = c("Spiral", "Mobile Clinic"), col = c("red", "blue"), lty = 1)