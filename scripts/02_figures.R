# -------------------------------------------
# CHOICE AND FEEDBACK CHARACTERISTICS PROJECT
# -- St. Germain, McKay, Poskus, Williams, Leshchyshen, Feldman, Cashaback,
# and Carter
#
# Manuscript - Figures
#
# Authors:
#   Laura St. Germain
#   Brad McKay
#   Mike Carter
#
# Last update: March 29 2022
#
# Website: https://www.cartermaclab.org
# -------------------------------------------

# SCRIPT SETUP ----
#
# Required files and tibbles
source("scripts/01_wrangle.R")

# Color and theme setup
color_expt2 <- c("#5e81ac", "#d08770")

theme_set(
  theme_classic() +
    theme(
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12, face = "bold")
    )
)


# FIGURE 3 ----
#
# Experiment 2
#
# Bind tibbles together
expt2_9blocks_g <- dplyr::bind_rows(
  expt2_performance_acq_g,
  expt2_performance_prt_g
)

expt2_9blocks_g <- expt2_9blocks_g %>%
  dplyr::mutate(
    phase_id = dplyr::case_when(
      block_id == 1 ~ "pre",
      block_id %in% c(2:7) ~ "acq",
      block_id == 8 ~ "ret",
      block_id == 9 ~ "trn"
    )
  )

# Fig 3a - Spatial total error (E) for all phases
fig3a <- ggplot2::ggplot(
  expt2_9blocks_g,
  aes(x = block_id,
      y = g_mean_e_deg,
      group = interaction(group_id, phase_id),
      color = group_id)) +
  geom_line(aes(linetype = group_id,
                group = interaction(group_id, phase_id)),
            position = position_dodge(0.6),
            show.legend = FALSE) +
  geom_pointrange(aes(ymin = ci_low_deg,
                      ymax = ci_upp_deg,
                      color = group_id,
                      shape = group_id),
                  size = 0.5,
                  position = position_dodge(0.6)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Spatial E (deg)",
                     limits = c(0, 30),
                     breaks = seq(0, 30, 5)) +
  scale_linetype_manual(values = c(1, 3),
                        labels = c("Choice+Binary",
                                   "Yoked+Binary")) +
  scale_color_manual(values = color_expt2,
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  scale_fill_manual(values = color_expt2,
                    labels = c("Choice+Binary",
                               "Yoked+Binary")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  geom_vline(xintercept = c(1.5, 7.5, 8.5),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

# Fig 3b - Timing total error (E) for all phases
fig3b <- ggplot2::ggplot(
  expt2_9blocks_g,
  aes(x = block_id,
      y = g_mean_e_ms,
      group = interaction(group_id, phase_id),
      color = group_id)) +
  geom_line(aes(linetype = group_id,
                group = interaction(group_id, phase_id)),
            position = position_dodge(0.6),
            show.legend = FALSE) +
  geom_pointrange(aes(ymin = ci_low_ms,
                      ymax = ci_upp_ms,
                      color = group_id,
                      shape = group_id),
                  size = 0.5,
                  position = position_dodge(0.6)) +
  scale_x_discrete(name = "Blocks of 12 trials",
                   labels = c("1" = "1",
                              "2" = "1",
                              "3" = "2",
                              "4" = "3",
                              "5" = "4",
                              "6" = "5",
                              "7" = "6",
                              "8" = "1",
                              "9" = "1")) +
  scale_y_continuous(name = "Timing E (ms)",
                     limits = c(0, 305),
                     breaks = seq(0, 305, 50)) +
  scale_linetype_manual(values = c(1, 3),
                        labels = c("Choice+Binary",
                                   "Yoked+Binary")) +
  scale_color_manual(values = color_expt2,
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  scale_fill_manual(values = color_expt2,
                    labels = c("Choice+Binary",
                               "Yoked+Binary")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  geom_vline(xintercept = c(1.5, 7.5, 8.5),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none") +
  annotate(
    geom = "text", x = 1, y = 305, label = "Pre-test",
    size = 3, fontface = "bold"
  ) +
  annotate(
    geom = "text", x = 4.5, y = 305, label = "Acquisition",
    size = 3, fontface = "bold"
  ) +
  annotate(
    geom = "text", x = 8, y = 305, label = "Retention",
    size = 3, fontface = "bold"
  ) +
  annotate(
    geom = "text", x = 9, y = 305, label = "Transfer",
    size = 3, fontface = "bold"
  )

# Fig 3c - Perceived autonomy
fig3c <- ggplot2::ggplot(
  expt2_qaire_pa_g,
  aes(x = time_id,
      y = mean_rating,
      color = group_id,
      shape = group_id)) +
  geom_pointrange(aes(ymin = ci_low,
                      ymax = ci_upp,
                      color = group_id,
                      shape = group_id),
                  size = 0.5,
                  position = position_dodge(0.9)) +
  geom_point(data = expt2_qaire_pa_p,
             aes(x = time_id,
                 y = score,
                 color = group_id,
                 shape = group_id),
             show.legend = FALSE,
             alpha = 0.15,
             position = position_jitterdodge(dodge.width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Perceived autonomy",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = color_expt2,
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

# Fig 3d - Perceived competence
fig3d <- ggplot2::ggplot(
  expt2_qaire_pc_g,
  aes(x = time_id,
      y = mean_rating,
      color = group_id,
      shape = group_id)) +
  geom_pointrange(aes(ymin = ci_low,
                      ymax = ci_upp,
                      color = group_id,
                      shape = group_id),
                  size = 0.5,
                  position = position_dodge(0.9)) +
  geom_point(data = expt2_qaire_pc_p,
             aes(x = time_id,
                 y = score,
                 color = group_id,
                 shape = group_id),
             show.legend = FALSE,
             alpha = 0.15,
             position = position_jitterdodge(dodge.width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Perceived competence",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = color_expt2,
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none",
        axis.text.x = element_blank())

# Fig 3e - Intrinsic motivation
fig3e <- ggplot2::ggplot(
  expt2_qaire_im_g,
  aes(x = time_id,
      y = mean_rating,
      color = group_id,
      shape = group_id)) +
  geom_pointrange(aes(ymin = ci_low,
                      ymax = ci_upp,
                      color = group_id,
                      shape = group_id),
                  size = 0.5,
                  position = position_dodge(0.9)) +
  geom_point(data = expt2_qaire_im_p,
             aes(x = time_id,
                 y = score,
                 color = group_id,
                 shape = group_id),
             show.legend = FALSE,
             alpha = 0.15,
             position = position_jitterdodge(dodge.width = 0.9)) +
  scale_x_discrete(name = "Time",
                   labels = c("1" = "After Pre-test",
                              "2" = "After Block 1",
                              "3" = "After Block 6",
                              "4" = "Before Retention")) +
  scale_y_continuous(name = "Intrinsic motivation",
                     limits = c(1, 7),
                     breaks = seq(1, 7, 1)) +
  scale_color_manual(values = color_expt2,
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  scale_shape_manual(values = c(16, 15),
                     labels = c("Choice+Binary",
                                "Yoked+Binary")) +
  geom_vline(xintercept = c(1.5, 2.5, 3.5),
             linetype = "dotted", alpha = 0.8) +
  theme(legend.position = "none")

# Create multipanel figure for Experiment 2
fig3 <- (fig3a / fig3b) | (fig3c / fig3d / fig3e)
fig3 +
  patchwork::plot_layout(guides = "collect") &
  patchwork::plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 16, face = "bold")) &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"))

