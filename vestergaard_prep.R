# Set up the MRC-IDE repository
options(repos = c(
  mrcide = 'https://mrc-ide.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'
))

# Install the package
install.packages('malariasimulation')

library(malariasimulation)
packageVersion('malariasimulation')

# Get default parameters
params <- get_parameters()

# Run a basic simulation for 1 year (365 days)
output <- run_simulation(timesteps = 365, parameters = params)

# Look at the structure
head(output)
names(output)

# ============================================
# STEP 1: Load libraries
# ============================================
library(malariasimulation)
library(ggplot2)

# ============================================
# STEP 2: Baseline - no bednets
# ============================================
params_baseline <- get_parameters(list(
  human_population = 1000
))

output_baseline <- run_simulation(timesteps = 3 * 365, 
                                  parameters = params_baseline)
output_baseline$scenario <- "No nets"

# ============================================
# STEP 3: Intervention - with PermaNet bednets
# ============================================
params_nets <- get_parameters(list(
  human_population = 1000
))

params_nets <- set_bednets(
  params_nets,
  timesteps = 30,
  coverages = 0.8,
  retention = 5 * 365,
  dn0 = matrix(0.40),   # death probability
  rn =  matrix(0.35),   # repelling probability
  rnm = matrix(0.20),   # minimum repelling
  gamman = matrix(2.64)
)

output_nets <- run_simulation(timesteps = 3 * 365, 
                              parameters = params_nets)
output_nets$scenario <- "PermaNet (80% coverage)"

# ============================================
# STEP 4: Combine both scenarios
# ============================================
combined <- rbind(output_baseline, output_nets)

## Keep only common columns + scenario
common_cols <- c("timestep", "n_detect_lm_730_3650", "scenario")

baseline_slim <- output_baseline[, common_cols]
nets_slim <- output_nets[, common_cols]

# Combine
combined <- rbind(baseline_slim, nets_slim)

# Plot
ggplot(combined, aes(x = timestep, 
                     y = n_detect_lm_730_3650, 
                     colour = scenario)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 30, linetype = "dashed", 
             colour = "black", alpha = 0.5) +
  annotate("text", x = 35, 
           y = max(combined$n_detect_lm_730_3650),
           label = "Nets deployed", hjust = 0, size = 3.5) +
  scale_colour_manual(values = c(
    "No nets" = "#d7191c",
    "PermaNet (80% coverage)" = "#2c7bb6"
  )) +
  labs(
    title = "Impact of PermaNet Bednets on Malaria Prevalence",
    subtitle = "Simulated population of 1,000 | 80% net coverage",
    x = "Day", 
    y = "Microscopy-detected cases (2–10 yr olds)",
    colour = "Scenario"
  ) +
  theme_minimal()


# Check columns of each output
ncol(output_baseline)
ncol(output_nets)

# See which columns differ
names(output_baseline)
names(output_nets)

# ================================================
# VESTERGAARD PREP - Multiple Iterations
# ================================================
library(malariasimulation)
library(ggplot2)

timesteps <- 3 * 365
n_runs <- 5

# ================================================
# BASELINE - collect all runs
# ================================================
cat("Running baseline...\n")
params_baseline <- get_parameters(list(human_population = 1000))

baseline_list <- list()
for (i in seq_len(n_runs)) {
  cat("Baseline run", i, "of", n_runs, "\n")
  sim <- run_simulation(timesteps = timesteps, 
                        parameters = params_baseline)
  baseline_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run = i,
    scenario = "No nets"
  )
}
baseline_all <- do.call(rbind, baseline_list)

# ================================================
# INTERVENTION - collect all runs
# ================================================
cat("Running intervention...\n")
params_nets <- get_parameters(list(human_population = 1000))
params_nets <- set_bednets(
  params_nets,
  timesteps = 30,
  coverages = 0.8,
  retention = 5 * 365,
  dn0 = matrix(0.40),
  rn  = matrix(0.35),
  rnm = matrix(0.20),
  gamman = matrix(2.64)
)

nets_list <- list()
for (i in seq_len(n_runs)) {
  cat("Intervention run", i, "of", n_runs, "\n")
  sim <- run_simulation(timesteps = timesteps, 
                        parameters = params_nets)
  nets_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run = i,
    scenario = "PermaNet (80% coverage)"
  )
}
nets_all <- do.call(rbind, nets_list)

# ================================================
# COMBINE & AVERAGE
# ================================================
all_runs <- rbind(baseline_all, nets_all)

avg_results <- aggregate(
  n_detect ~ timestep + scenario,
  data = all_runs,
  FUN = mean
)

# ================================================
# PLOT
# ================================================
ggplot(avg_results, aes(x = timestep,
                        y = n_detect,
                        colour = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 30, linetype = "dashed",
             colour = "black", alpha = 0.6) +
  annotate("text", x = 40,
           y = max(avg_results$n_detect) * 0.95,
           label = "Nets deployed", hjust = 0, size = 3.5) +
  scale_colour_manual(values = c(
    "No nets" = "#d7191c",
    "PermaNet (80% coverage)" = "#2c7bb6"
  )) +
  labs(
    title = "Impact of PermaNet Bednets on Malaria Prevalence",
    subtitle = "Average of 5 runs | Population 1,000 | 80% coverage",
    x = "Day",
    y = "Microscopy-detected cases (2–10 yr olds)",
    colour = "Scenario"
  ) +
  theme_minimal()
