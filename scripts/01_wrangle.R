# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Manuscript - Data wrangling
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: August 10 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------


# SCRIPT SETUP ----
#
# Required libraries
source("scripts/00_libraries.R")

# Seed for reproducible bootstrapped confidence intervals
set.seed(8693)

# Load data files
expt2_performance_data <- readr::read_csv("data/expt2_performance-data.csv")
expt2_qaire_data <- readr::read_csv("data/expt2_qaire-data.csv")


# DATA SETUP ----
#
# Create tibbles to work with
#
# EXPERIMENT 2
#
# PERFORMANCE DATA
#
# Add columns for blocks, and ce^2. For blocks, we will collapse across the
# two different pre-tests for now.
expt2_performance_tib <- expt2_performance_data %>%
  dplyr::mutate(block_id = rep(rep(1:9, each = 12), 76)) %>%
  dplyr::mutate(ce2_deg = dplyr::if_else(phase_id != 4, (xi_deg - 40)^2,
                                         (xi_deg - 60)^2)) %>%
  dplyr::mutate(ce2_ms = (xi_ms - 225)^2)

# Make factors
expt2_performance_tib <- expt2_performance_tib %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    phase_id = forcats::as_factor(phase_id),
    block_id = forcats::as_factor(block_id)
  )

# Tibble for pre-test, retention, and transfer data at the participant (p)
# and group (g) levels
expt2_performance_prt_p <- expt2_performance_tib %>%
  dplyr::filter(block_id %in% c(1, 8, 9)) %>%
  dplyr::group_by(id, group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt2_performance_prt_g <- expt2_performance_prt_p %>%
  dplyr::group_by(group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Tibble for acquisition at the participant (p) and group (g) levels
expt2_performance_acq_p <- expt2_performance_tib %>%
  dplyr::filter(block_id %in% c(2:7)) %>%
  dplyr::group_by(id, group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    p_mean_e_deg = sqrt(mean(ce2_deg, na.rm = TRUE)),
    p_mean_e_ms = sqrt(mean(ce2_ms, na.rm = TRUE)),
    .groups = "drop"
  )

expt2_performance_acq_g <- expt2_performance_acq_p %>%
  dplyr::group_by(group_id, block_id) %>%
  dplyr::summarize(
    n = n(),
    g_mean_e_deg = mean(p_mean_e_deg, na.rm = TRUE),
    ci_low_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymin,
    ci_upp_deg = ggplot2::mean_cl_boot(p_mean_e_deg)$ymax,
    g_mean_e_ms = mean(p_mean_e_ms, na.rm = TRUE),
    ci_low_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymin,
    ci_upp_ms = ggplot2::mean_cl_boot(p_mean_e_ms)$ymax,
    .groups = "drop"
  )

# Number of hits during acquisition at the participant (p) and group (g) levels
expt2_deg_hit_p <- expt2_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(
    xi_deg > 39.99 & xi_deg < 40.01, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt2_deg_hit_g <- expt2_deg_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )

expt2_ms_hit_p <- expt2_performance_tib %>%
  dplyr::filter(phase_id == 2) %>%
  dplyr::mutate(hit = dplyr::if_else(xi_ms == 225, 1, 0)) %>%
  dplyr::group_by(id, group_id) %>%
  dplyr::summarize(
    n = n(),
    sum_hit = sum(hit, na.rm = TRUE)
  )

expt2_ms_hit_g <- expt2_ms_hit_p %>%
  dplyr::group_by(group_id) %>%
  dplyr::summarize(
    n = n(),
    total = sum(sum_hit, na.rm = TRUE),
    min = min(sum_hit, na.rm = TRUE),
    max = max(sum_hit, na.rm = TRUE)
  )


# QUESTIONNAIRE DATA
#
# Make factors
expt2_qaire_tib <- expt2_qaire_data %>%
  dplyr::mutate(
    group_id = forcats::as_factor(group_id),
    time_id = forcats::as_factor(time_id),
    scale_id = forcats::as_factor(scale_id)
  )

# Tibbles for intrinsic motivation (im), perceived competence (pc), and
# perceived autonomy (pa) at participant (p) and group (g) levels
expt2_qaire_pa_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "pa")

expt2_qaire_im_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "im")

expt2_qaire_pc_p <- expt2_qaire_tib %>%
  dplyr::filter(scale_id == "pc")

expt2_qaire_pa_g <- expt2_qaire_pa_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt2_qaire_im_g <- expt2_qaire_im_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

expt2_qaire_pc_g <- expt2_qaire_pc_p %>%
  dplyr::group_by(group_id, time_id) %>%
  dplyr::summarize(
    n = n(),
    mean_rating = mean(score, na.rm = TRUE),
    ci_low = ggplot2::mean_cl_boot(score)$ymin,
    ci_upp = ggplot2::mean_cl_boot(score)$ymax,
    .groups = "drop"
  )

