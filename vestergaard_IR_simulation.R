# ============================================================
# VESTERGAARD PREP - Simulation 2
# Insecticide Resistance: Standard Net vs PermaNet Dual
# Author: Sally Karimi Kinyua
# Date: March 2026
#
# BACKGROUND:
# Pyrethroid insecticide resistance is widespread in 
# Anopheles gambiae across sub-Saharan Africa. Standard 
# pyrethroid-only nets (e.g. PermaNet 2.0) show reduced 
# efficacy in resistant populations. PermaNet Dual combines 
# pyrethroid + chlorfenapyr, maintaining efficacy against 
# resistant mosquitoes through a different mode of action.
#
# THIS SIMULATION COMPARES:
# 1. No intervention (baseline)
# 2. Standard pyrethroid net (reduced by resistance)
# 3. PermaNet Dual (maintained efficacy)
# ============================================================
# ---- LIBRARIES ------------------------------------------------
# malariasimulation: individual-based malaria transmission model
#                   built by MRC Centre for Global Infectious 
#                   Disease Analysis, Imperial College London
#
# ggplot2: publication-quality data visualisation
# ---------------------------------------------------------------

library(malariasimulation)
library(ggplot2)

# ---- SIMULATION SETTINGS --------------------------------------
# timesteps: how many days to simulate
#            3 years = 3 * 365 = 1095 days
#            long enough to see sustained intervention effects
#
# n_runs: number of iterations per scenario
#         5 runs smooths out stochastic variation
#         without taking too long to compute
#
# pop_size: simulated human population
#           1000 people is standard for exploratory analysis
#
# nets_day: the day bednets are distributed
#           day 30 gives the model time to stabilise
#           before intervention begins
# ---------------------------------------------------------------

timesteps <- 3 * 365   # 1095 days
n_runs    <- 5         # iterations per scenario
pop_size  <- 1000      # human population size
nets_day  <- 30        # day nets are deployed

# ---- SCENARIO 1: BASELINE - No intervention -------------------
# This is our control scenario - no bednets deployed
# Represents a population with natural malaria transmission
# We will compare all intervention scenarios against this
# ---------------------------------------------------------------

cat("Running Scenario 1: Baseline (no nets)...\n")

# Set up parameters for a population of 1000 people
params_baseline <- get_parameters(list(
  human_population = pop_size
))

# Create empty list to store each run
baseline_list <- list()

# Run the simulation n_runs times
for (i in seq_len(n_runs)) {
  cat("  Run", i, "of", n_runs, "\n")
  
  sim <- run_simulation(
    timesteps  = timesteps,
    parameters = params_baseline
  )
  
  
  # Extract only the columns we need
  baseline_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run      = i,
    scenario = "1. No nets (baseline)"
  )
}

# Combine all runs into one dataframe
baseline_all <- do.call(rbind, baseline_list)


cat("Baseline complete!\n")

# How many rows does our combined data have?
nrow(baseline_all)

# Look at the first few rows
head(baseline_all)

# Look at the last few rows
tail(baseline_all)

# Check the structure
str(baseline_all)

# ---- SCENARIO 2: STANDARD PYRETHROID NET ---------------------
# Represents a standard pyrethroid-only net e.g. PermaNet 2.0
# In areas with insecticide resistance, efficacy is REDUCED:
#
# Normal net kill rate (dn0) would be ~0.40
# Resistance REDUCES this to 0.10 (only 10% of mosquitoes killed)
# Repellency (rn) also reduced from 0.35 to 0.20
# This reflects real-world resistance data from
# Anopheles gambiae populations in sub-Saharan Africa
# ---------------------------------------------------------------

cat("Running Scenario 2: Standard pyrethroid net...\n")

params_pyret <- get_parameters(list(
  human_population = pop_size
))

params_pyret <- set_bednets(
  params_pyret,
  timesteps = nets_day,
  coverages = 0.8,
  retention = 5 * 365,
  dn0       = matrix(0.10),  # REDUCED kill rate due to resistance
  rn        = matrix(0.20),  # REDUCED repellency due to resistance
  rnm       = matrix(0.10),  # REDUCED minimum repellency
  gamman    = matrix(2.64)
)

pyret_list <- list()

for (i in seq_len(n_runs)) {
  cat("  Run", i, "of", n_runs, "\n")
  
  sim <- run_simulation(
    timesteps  = timesteps,
    parameters = params_pyret
  )
  
  pyret_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run      = i,
    scenario = "2. Standard pyrethroid net"
  )
}

pyret_all <- do.call(rbind, pyret_list)

cat("Standard pyrethroid net complete!\n")

# ---- SCENARIO 3: PERMANT DUAL --------------------------------
# PermaNet Dual combines TWO active ingredients:
#   1. Pyrethroid (same as standard net)
#   2. Chlorfenapyr (completely different mode of action)
#
# Even when mosquitoes are resistant to pyrethroids,
# chlorfenapyr kills them through a different mechanism.
# This MAINTAINS high efficacy in resistant populations.
#
# Parameters reflect maintained high efficacy:
#   dn0 = 0.40 (kill rate restored vs standard net's 0.10)
#   rn  = 0.35 (repellency restored)
#   rnm = 0.20 (minimum repellency maintained)
#
# This is why NMCPs are switching to PermaNet Dual in
# areas with confirmed pyrethroid resistance
# ---------------------------------------------------------------

cat("Running Scenario 3: PermaNet Dual...\n")

params_dual <- get_parameters(list(
  human_population = pop_size
))

params_dual <- set_bednets(
  params_dual,
  timesteps = nets_day,
  coverages = 0.8,
  retention = 5 * 365,
  dn0       = matrix(0.40),  # RESTORED kill rate
  rn        = matrix(0.35),  # RESTORED repellency
  rnm       = matrix(0.20),  # RESTORED minimum repellency
  gamman    = matrix(2.64)
)

dual_list <- list()

for (i in seq_len(n_runs)) {
  cat("  Run", i, "of", n_runs, "\n")
  
  sim <- run_simulation(
    timesteps  = timesteps,
    parameters = params_dual
  )
  
  dual_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run      = i,
    scenario = "3. PermaNet Dual"
  )
}

dual_all <- do.call(rbind, dual_list)

cat("PermaNet Dual complete!\n")

# ---- COMBINE ALL SCENARIOS -----------------------------------
# Stack all three scenario dataframes into one
all_runs <- rbind(baseline_all, pyret_all, dual_all)

# Average across runs for each scenario per timestep
avg_results <- aggregate(
  n_detect ~ timestep + scenario,
  data = all_runs,
  FUN  = mean
)

# ---- CALCULATE CASE REDUCTIONS -------------------------------
# Average cases in final 6 months (day 900 onwards)
final_baseline <- mean(avg_results$n_detect[
  avg_results$timestep > 900 &
    avg_results$scenario == "1. No nets (baseline)"])

final_pyret <- mean(avg_results$n_detect[
  avg_results$timestep > 900 &
    avg_results$scenario == "2. Standard pyrethroid net"])

final_dual <- mean(avg_results$n_detect[
  avg_results$timestep > 900 &
    avg_results$scenario == "3. PermaNet Dual"])

# Print results
cat("\n===== RESULTS =====\n")
cat("Baseline cases (Day 900+):        ", 
    round(final_baseline, 1), "\n")
cat("Standard pyrethroid net cases:    ", 
    round(final_pyret, 1), "\n")
cat("PermaNet Dual cases:              ", 
    round(final_dual, 1), "\n")
cat("Standard net reduction:           ", 
    round((1 - final_pyret/final_baseline)*100, 1), "%\n")
cat("PermaNet Dual reduction:          ", 
    round((1 - final_dual/final_baseline)*100, 1), "%\n")

# ---- PLOT ----------------------------------------------------
ggplot(avg_results, aes(x = timestep,
                        y = n_detect,
                        colour = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = nets_day, linetype = "dashed",
             colour = "black", alpha = 0.6) +
  annotate("text", x = nets_day + 10,
           y = max(avg_results$n_detect) * 0.97,
           label = "Nets deployed", 
           hjust = 0, size = 3.5) +
  scale_colour_manual(values = c(
    "1. No nets (baseline)"      = "#d7191c",
    "2. Standard pyrethroid net" = "#fdae61",
    "3. PermaNet Dual"           = "#2c7bb6"
  )) +
  labs(
    title   = "Insecticide Resistance: Impact on Bednet Effectiveness",
    subtitle = paste0("Average of ", n_runs, 
                      " runs | Population ", pop_size,
                      " | 80% coverage | Pyrethroid resistance modelled"),
    x       = "Day",
    y       = "Microscopy-detected cases (2-10 yr olds)",
    colour  = "Scenario",
    caption = "Model: malariasimulation v3.0.0 (MRC-IDE, Imperial College London)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey40"),
    plot.caption  = element_text(colour = "grey60", size = 8),
    legend.position = "bottom"
  )