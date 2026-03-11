# ============================================================
# VESTERGAARD PREP - Simulation 3
# Net Durability + Insecticide Resistance:
# How does net ageing affect protection in resistant populations?
#
# Author: Sally Karimi Kinyua
# Date: March 2026
#
# BACKGROUND:
# Bednets lose insecticide potency over time through:
#   - Washing (removes insecticide from fabric)
#   - UV degradation (sunlight breaks down insecticide)
#   - Physical wear (holes reduce barrier protection)
#
# In a resistant mosquito population this matters MORE
# because the net starts with already reduced kill rate.
# As it ages, even that reduced protection disappears.
#
# PermaNet Dual's chlorfenapyr ingredient is more stable
# and maintains efficacy longer than pyrethroid-only nets.
#
# THIS SIMULATION MODELS OVER 5 YEARS:
#   1. No nets (baseline)
#   2. Standard pyrethroid net degrading with resistance
#   3. PermaNet Dual degrading more slowly
# ============================================================

# ---- LIBRARIES ------------------------------------------------
library(malariasimulation)
library(ggplot2)

# ---- SIMULATION SETTINGS -------------------------------------
# 5 years this time (longer to show durability decline clearly)
# nets degrade significantly over 5 years
# ---------------------------------------------------------------

timesteps <- 5 * 365   # 1825 days = 5 years
n_runs    <- 5         # iterations per scenario
pop_size  <- 1000      # human population
nets_day  <- 30        # day nets are deployed
```

### Why 5 years this time?
```
3 year simulation: shows initial impact
but nets are still fairly new

5 year simulation: shows the FULL durability story
- Years 1-2: nets working
- Years 3-4: nets degrading
- Year 5:    nets nearly useless
(standard net)
vs still working
(PermaNet Dual)

# ---- SCENARIO 1: BASELINE - No intervention ------------------
# Control scenario - natural malaria transmission over 5 years
# No bednets deployed
# ---------------------------------------------------------------

cat("Running Scenario 1: Baseline (no nets)...\n")

params_baseline <- get_parameters(list(
  human_population = pop_size
))

baseline_list <- list()

for (i in seq_len(n_runs)) {
  cat("  Run", i, "of", n_runs, "\n")
  
  sim <- run_simulation(
    timesteps  = timesteps,
    parameters = params_baseline
  )
  
  baseline_list[[i]] <- data.frame(
    timestep = sim$timestep,
    n_detect = sim$n_detect_lm_730_3650,
    run      = i,
    scenario = "1. No nets (baseline)"
  )
}

baseline_all <- do.call(rbind, baseline_list)
cat("Baseline complete!\n")

# ---- SCENARIO 2: STANDARD PYRETHROID NET + DEGRADATION -------
# Models a standard pyrethroid-only net in a resistant 
# mosquito population that degrades over 5 years.
#
# TWO problems happening simultaneously:
#
# Problem 1 - RESISTANCE:
#   Mosquitoes are already resistant to pyrethroids
#   so starting kill rate is LOW (dn0 = 0.10)
#
# Problem 2 - DEGRADATION:
#   gamman controls how fast insecticide wears off
#   Lower gamman = faster degradation
#   Standard nets degrade FASTER = gamman of 1.5
#   (vs PermaNet Dual's more stable formula)
#
# Combined effect: net starts weak AND gets weaker faster
# This reflects real field data from resistant populations
# in Kenya, Uganda, and Burkina Faso
# ---------------------------------------------------------------

cat("Running Scenario 2: Standard pyrethroid net (degrading)...\n")

params_pyret <- get_parameters(list(
  human_population = pop_size
))

params_pyret <- set_bednets(
  params_pyret,
  timesteps = nets_day,
  coverages = 0.8,
  retention = 3 * 365,   # standard net lasts ~3 years
  dn0       = matrix(0.10),  # LOW kill rate - resistance
  rn        = matrix(0.20),  # LOW repellency - resistance
  rnm       = matrix(0.05),  # LOW minimum repellency
  gamman    = matrix(1.50)   # FASTER degradation
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
```

### What's new here — `retention` and `gamman`:
```
retention = 3 * 365
→ nets are physically kept for 3 years
→ after 3 years nets are gone entirely
→ cases will RISE again after day 1095

gamman = 1.50
→ controls insecticide decay speed
→ lower number = faster decay
→ standard net: 1.50 (faster decay)
→ PermaNet Dual: 2.64 (slower decay)
→ difference shows up clearly over 5 years

# ---- SCENARIO 3: PERMNET DUAL + SLOWER DEGRADATION -----------
# Models PermaNet Dual in the same resistant population.
#
# KEY DIFFERENCES from standard net:
#
# 1. RESISTANCE OVERCOME:
#    Chlorfenapyr kills pyrethroid-resistant mosquitoes
#    so kill rate is HIGH despite resistance (dn0 = 0.40)
#
# 2. SLOWER DEGRADATION:
#    PermaNet Dual's formulation is more chemically stable
#    gamman = 2.64 (slower decay vs standard net's 1.50)
#    Insecticide remains active longer on the fabric
#
# 3. LONGER RETENTION:
#    retention = 5 * 365 (lasts full 5 year simulation)
#    vs standard net's 3 years
#
# Combined effect: starts strong AND stays strong longer
# This is Vestergaard's core evidence for PermaNet Dual
# ---------------------------------------------------------------

cat("Running Scenario 3: PermaNet Dual (slower degradation)...\n")

params_dual <- get_parameters(list(
  human_population = pop_size
))

params_dual <- set_bednets(
  params_dual,
  timesteps = nets_day,
  coverages = 0.8,
  retention = 5 * 365,   # lasts full 5 years
  dn0       = matrix(0.40),  # HIGH kill rate - resistance overcome
  rn        = matrix(0.35),  # HIGH repellency maintained
  rnm       = matrix(0.20),  # HIGH minimum repellency
  gamman    = matrix(2.64)   # SLOWER degradation
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
```

### The key comparison between scenarios:
```
Standard Net    PermaNet Dual
Kill rate:         0.10    vs      0.40
Repellency:        0.20    vs      0.35
Degradation:       1.50    vs      2.64  (higher = slower)
Retention:       3 years   vs    5 years

# ---- COMBINE ALL SCENARIOS -----------------------------------
all_runs <- rbind(baseline_all, pyret_all, dual_all)

# Average across runs per timestep per scenario
avg_results <- aggregate(
  n_detect ~ timestep + scenario,
  data = all_runs,
  FUN  = mean
)

# ---- CALCULATE CASE REDUCTIONS -------------------------------
# Compare final year (day 1460 onwards) - year 5
final_baseline <- mean(avg_results$n_detect[
  avg_results$timestep > 1460 &
    avg_results$scenario == "1. No nets (baseline)"])

final_pyret <- mean(avg_results$n_detect[
  avg_results$timestep > 1460 &
    avg_results$scenario == "2. Standard pyrethroid net"])

final_dual <- mean(avg_results$n_detect[
  avg_results$timestep > 1460 &
    avg_results$scenario == "3. PermaNet Dual"])

# Print results
cat("\n===== RESULTS (Year 5) =====\n")
cat("Baseline cases:                   ",
    round(final_baseline, 1), "\n")
cat("Standard pyrethroid net cases:    ",
    round(final_pyret, 1), "\n")
cat("PermaNet Dual cases:              ",
    round(final_dual, 1), "\n")
cat("Standard net reduction:           ",
    round((1 - final_pyret/final_baseline)*100, 1), "%\n")
cat("PermaNet Dual reduction:          ",
    round((1 - final_dual/final_baseline)*100, 1), "%\n")

# ---- ADD YEAR MARKERS ----------------------------------------
# Convert timesteps to years for cleaner x axis
avg_results$year <- avg_results$timestep / 365

# ---- PLOT ----------------------------------------------------
ggplot(avg_results, aes(x = year,
                        y = n_detect,
                        colour = scenario)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = nets_day/365, 
             linetype = "dashed",
             colour = "black", alpha = 0.6) +
  # Mark when standard net expires (year 3)
  geom_vline(xintercept = 3, 
             linetype = "dotted",
             colour = "#fdae61", alpha = 0.8) +
  annotate("text", x = nets_day/365 + 0.05,
           y = max(avg_results$n_detect) * 0.97,
           label = "Nets deployed",
           hjust = 0, size = 3.2) +
  annotate("text", x = 3.05,
           y = max(avg_results$n_detect) * 0.88,
           label = "Standard net\nexpires",
           hjust = 0, size = 3.2,
           colour = "#e08020") +
  scale_colour_manual(values = c(
    "1. No nets (baseline)"      = "#d7191c",
    "2. Standard pyrethroid net" = "#fdae61",
    "3. PermaNet Dual"           = "#2c7bb6"
  )) +
  scale_x_continuous(breaks = 0:5,
                     labels = paste0("Year ", 0:5)) +
  labs(
    title   = "Net Durability & Insecticide Resistance Over 5 Years",
    subtitle = paste0("Average of ", n_runs,
                      " runs | Population ", pop_size,
                      " | 80% coverage | Pyrethroid resistance modelled"),
    x       = "Year",
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

# ---- SAVE PLOT -----------------------------------------------
ggsave(
  filename = "durability_simulation_plot.png",
  path     = "C:/Users/Admin/OneDrive/Desktop/vestergaard_malaria_modelling",
  width    = 10,
  height   = 6,
  dpi      = 300
)
cat("Plot saved!\n")