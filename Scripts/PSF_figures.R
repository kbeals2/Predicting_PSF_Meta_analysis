(.packages())
library(pacman)
p_load("fBasics", "rcompanion", "tidyverse", "ggplot2", "patchwork", "lemon")


########## Figure 1A: Overall main effects panel ##########
main.indiv.type <- c('PSF', 'Competition', 'Stress', 'Disturbance')
main.indiv.mean <- c(-0.0138, -0.1373, -0.0416, -0.0057)
main.indiv.lower_ci <- c(-0.0387, -0.1835, -0.1108, -0.1651)
main.indiv.upper_ci <- c(0.0111, -0.091, 0.0276, 0.1537)

(main.indiv.effects.data <- data.frame(main.indiv.type, main.indiv.mean, main.indiv.lower_ci, main.indiv.upper_ci))

# publication 
(main.individual.effects.panel <- ggplot(main.indiv.effects.data, aes(x = factor(main.indiv.type, level = c('PSF', 'Competition', 'Stress', 'Disturbance')), y = main.indiv.mean, group = main.indiv.type), axes = FALSE) +
  geom_errorbar(aes(ymin = main.indiv.lower_ci, ymax = main.indiv.upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(fill = main.indiv.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
  scale_fill_manual(values = c("darkviolet", "royalblue1", "springgreen2", "gold1"), breaks = c("PSF", "Competition", "Stress", "Disturbance")) +
  xlab("\nMain Effects") +
  ylab("RII\n") +
  scale_x_discrete(labels = c("psf" = "PSF", "competition"  = "Competition", "stress" = "Stress", "disturbance" = "Disturbance")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_symmetric(mid = 0) + 
  annotate("text", x = 4.2, y = 1.0, label = "A", size = 12, fontface = "bold") +
  annotate("text", x = c(2), y = c(0.3), label = "*", size = 14, fontface = "bold") +
  coord_capped_cart(left = "both") +
  theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
        axis.title.y = element_text(face = "bold", size = 40),
        axis.title.x = element_text(face = "bold", size = 36),
        axis.text.y = element_text(face = "plain", color = "black", size = 30),
        axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
        axis.ticks.length = unit(0.3, "cm"),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(size = 0.5),
        axis.line.x = element_blank(),
        legend.position = "none"))


(main.individual.effects.panel2 <- ggplot(main.indiv.effects.data, aes(x = factor(main.indiv.type, level = c('PSF', 'Competition', 'Stress', 'Disturbance')), y = main.indiv.mean, group = main.indiv.type), axes = FALSE) +
    geom_errorbar(aes(ymin = main.indiv.lower_ci, ymax = main.indiv.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = main.indiv.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("springgreen2", "darkviolet", "gold1", "royalblue1"), breaks = c("PSF", "Competition", "Stress", "Disturbance")) +
    xlab("\nMain Effects") +
    ylab("RII\n") +
    scale_x_discrete(labels = c("psf" = "PSF", "competition"  = "Competition", "stress" = "Stress", "disturbance" = "Disturbance")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_text(face = "bold", size = 40),
          axis.title.x = element_text(face = "bold", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 30),
          axis.text.x = element_text(face = "plain", color = "black", size = 16, vjust = 0.7),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))



########## Figure 1B: Overall Interactive effects panel ##########
main.interactive.type <- c('Competition', 'Stress', 'Disturbance')
main.interactive.mean <- c(-0.0215, 0.0358, 0.0988)
main.interactive.lower_ci <- c(-0.0551, 0.0031, -0.2185)
main.interactive.upper_ci <- c(0.0121, 0.0685, 0.1687)

(main.interactive.effects.data <- data.frame(main.interactive.type, main.interactive.mean, main.interactive.lower_ci, main.interactive.upper_ci))

# publication figure
(main.interactive.effects.panel <- ggplot(main.interactive.effects.data, aes(x = factor(main.interactive.type, level = c('Competition', 'Stress', 'Disturbance')), y = main.interactive.mean, group = main.interactive.type)) +
    geom_errorbar(aes(ymin = main.interactive.lower_ci, ymax = main.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = main.interactive.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("darkviolet", "royalblue1", "gold1"), breaks = c("Competition", "Stress", "Disturbance")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) + 
    annotate("text", x = 3.2, y = 1.0, label = "B", size = 12, fontface = "bold") +
    annotate("text", x = c(2), y = c(0.3), label = "*", size = 14, fontface = "bold") +
    coord_capped_cart(left = "both") +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = 36),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))


(main.interactive.effects.panel2 <- ggplot(main.interactive.effects.data, aes(x = factor(main.interactive.type, level = c('Competition', 'Stress', 'Disturbance')), y = main.interactive.mean, group = main.interactive.type)) +
    geom_errorbar(aes(ymin = main.interactive.lower_ci, ymax = main.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = main.interactive.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("darkviolet", "gold1", "royalblue1"), breaks = c("Competition", "Stress", "Disturbance")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 30),
          axis.text.x = element_text(face = "plain", color = "black", size = 16, vjust = 0.7),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))


## figures together side-by-side (publication)
overall_fig <- plot_grid(main.individual.effects.panel, main.interactive.effects.panel, nrow = 1, rel_widths = c(1, 1))
ggsave(overall_fig, filename = "overall_fig1_rev.eps", width = 50, height = 20, units = c("cm"), dpi = 600)


# Assembling figures using patchwork package
(patch1 <- main.individual.effects.panel2 +
    main.interactive.effects.panel2)


########## Fig 2A: Manipulation main effects panel ##########
manip.main.type <- c('PSF', 'PSF', 'PSF', 'Competition', 'Competition', 'Stress', 'Stress', 'Stress', 'Stress', 'Stress', 'Stress', 'Disturbance', 'Disturbance')
manip.type.manip.main <- c('fungicide_untreated', 'live_sterile', 'home_away', 'alone_together', 'con_hetero', 'drought', 'fertilization', 'grazing/herbivory', 'shade', 'mining', 'temperature', 'fire', 'tornado')
manip.main.mean <- c(-0.0015, -0.0784, 0.0154, -0.2952, -0.0451, -.2594, 0.1218, 0.0466, -0.3987, 0.2446, 0.0057, -0.2385, 0.0432)
manip.main.lower_ci <- c(-0.074, -0.1132, -0.0123, -0.3572, -0.096, -0.3918, 0.0561, -0.0764, -0.5009, -0.0648, -0.1169, -0.6135, -0.1088)
manip.main.upper_ci <- c(0.0709, -0.0437, 0.0432, -0.2333, 0.0057, -0.1271, 0.1875, 0.1697, -0.2965, 0.554, 0.1283, 0.1365, 0.1953)

(manip.main.effects.data <- data.frame(manip.main.type, manip.type.manip.main, manip.main.mean, manip.main.lower_ci, manip.main.upper_ci))

# publication figure
(manip.main.effects.panel <- ggplot(manip.main.effects.data, aes(x = factor(manip.type.manip.main, level = c('live_sterile', 'home_away', 'fungicide_untreated', 'alone_together', 'con_hetero', 'shade', 'drought',  'temperature',  'grazing/herbivory', 'mining', 'fertilization', 'fire', 'tornado')), y = manip.main.mean, group = manip.main.type)) +
  geom_errorbar(aes(ymin = manip.main.lower_ci, ymax = manip.main.upper_ci), width = 0.2, position = position_dodge(0.3)) +
  geom_point(aes(fill = manip.main.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
  scale_fill_manual(values = c("darkviolet", "royalblue1", "springgreen2", "gold1"), breaks = c("PSF", "Competition", "Stress", "Disturbance")) +
  xlab("\nMain Effects") +
  ylab("RII\n") +
  scale_x_discrete(labels = c("live_sterile" = "Live/\nSterile", "home_away"  = "Away/\nHome", "fungicide_untreated" = "Untreated/\nFungicide", "alone_together" = "Together/\nAlone", "con_hetero" = "Interspecific/\nIntraspecific", "drought" = "Drought", "fertilization" = "Fert", "grazing/herbivory" = "Graz\n& Herb", "mining" = "Mining", "shade" = "Shade", "temperature" = "Temperature", "fire" = "Fire", "tornado" = "Tornado")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_symmetric(mid = 0) + 
  annotate("text", x = 13.0, y = 1.0, label = "A", size = 16, fontface = "bold") +
  annotate("text", x = c(1, 4, 5, 6, 7, 11), y = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5), label = "*", size = 14, fontface = "bold") +
  guides(fill = guide_legend(title = "NA", title.theme = element_blank(), label.theme = element_text(size = 36, angle = 0))) +
  theme_classic(base_size = 10, base_family = "") +
  theme(panel.border = element_blank(),
        axis.title.y = element_text(face = "bold", size = 50),
        axis.title.x = element_text(face = "bold", size = 40),
        axis.text.y = element_text(face = "plain", color = "black", size = 40),
        axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(0.3, "cm"),
        axis.line.y = element_line(size = 0.5),
        axis.line.x = element_blank(),
        legend.position = c(0.10, 0.90)))

ggsave(manip.main.effects.panel, filename = "fig2a.eps", width = 70, height = 35, units = c("cm"), dpi = 300)


(manip.main.effects.panel2 <- ggplot(manip.main.effects.data, aes(x = factor(manip.type.manip.main, level = c('live_sterile', 'home_away', 'fungicide_untreated', 'alone_together', 'con_hetero', 'shade', 'drought',  'temperature',  'grazing/herbivory', 'mining', 'fertilization', 'fire', 'tornado')), y = manip.main.mean, group = manip.main.type)) +
    geom_errorbar(aes(ymin = manip.main.lower_ci, ymax = manip.main.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = manip.main.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("springgreen2", "darkviolet", "gold1", "royalblue1"), breaks = c("PSF", "Competition", "Stress", "Disturbance")) +
    xlab("\nMain Effects") +
    ylab("RII\n") +
    scale_x_discrete(labels = c("live_sterile" = "Live/\nSterile", "home_away"  = "Away/\nHome", "fungicide_untreated" = "Untreated/\nFungicide", "alone_together" = "Together/\nAlone", "con_hetero" = "Inter/\nIntra", "drought" = "Drought", "fertilization" = "Fert", "grazing/herbivory" = "Graz\n& Herb", "mining" = "Mining", "shade" = "Shade", "temperature" = "Temp", "fire" = "Fire", "tornado" = "Tornado")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_text(face = "bold", size = 30),
          axis.title.x = element_text(face = "bold", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 25),
          axis.text.x = element_text(face = "plain", color = "black", size = 13, vjust = 0.7, angle = 45),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0.3, "cm"),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))

  
########## Figure 2B: Manipulation interactive effects panel ##########
manip.interactive.type <- c('competition', 'competition', 'stress', 'stress', 'stress', 'stress', 'stress','stress', 'disturbance', 'disturbance')
manip.interactive.type.manip <- c('alone_together', 'con_hetero', 'drought', 'fertilization', 'grazing/herbivory', 'shade', 'mining', 'temperature', 'fire', 'tornado')
manip.interactive.mean <- c(-0.004, -0.0292, 0.1639, 0.0259, 0.0611, -0.0176, 0.1265, 0.0013, 0.1467, -0.0474)
manip.interactive.lower_ci <- c(-0.0623, -0.0688, 0.0772, -0.0069, -0.0282, -0.0884, -0.1322,  -0.1191, -0.4195, -0.2571)
manip.interactive.upper_ci <- c(0.0543, 0.0104, 0.2507, 0.0588, 0.1503, 0.0532, 0.3852, 0.1218, 0.7129, 0.1623)  

manip.interactive.effects.data <- data.frame(manip.interactive.type, manip.interactive.type.manip, manip.interactive.mean, manip.interactive.lower_ci, manip.interactive.upper_ci)

# publication figure
(manip.interactive.effects.panel <- ggplot(manip.interactive.effects.data, aes(x = factor(manip.interactive.type.manip, level = c('alone_together', 'con_hetero', 'shade', 'drought', 'temperature', 'grazing/herbivory', 'mining',  'fertilization', 'fire', 'tornado')), y = manip.interactive.mean, group = manip.interactive.type)) +
    geom_errorbar(aes(ymin = manip.interactive.lower_ci, ymax = manip.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = manip.interactive.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("darkviolet", "royalblue1", "gold1"), breaks = c("competition", "stress", "disturbance")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    scale_x_discrete(labels = c("alone_together" = "Together/\nAlone", "con_hetero" = "Interspecific/\nIntraspecific", "drought" = "Drought", "fertilization" = "Fert", "grazing/herbivory" = "Graz\n& Herb", "mining" = "Mining", "shade" = "Shade", "temperature" = "Temperature", "fire" = "Fire", "tornado" = "Tornado")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) + 
    guides(fill = guide_legend(title = "NA", title.theme = element_blank(), label.theme = element_text(size = 30, angle = 0))) +
    annotate("text", x = 10.0, y = 1.0, label = "B", size = 16, fontface = "bold") +
    annotate("text", x = c(4), y = c(0.5), label = "*", size = 14, fontface = "bold") +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_text(face = "bold", color = "black", size = 50),
          axis.title.x = element_text(face = "bold", color = "black", size = 40),
          axis.text.y = element_text(face = "plain", color = "black", size = 40),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0.3, "cm"),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))

ggsave(manip.interactive.effects.panel, filename = "fig2b.eps", width = 60, height = 35, units = c("cm"), dpi = 300)


(manip.interactive.effects.panel2 <- ggplot(manip.interactive.effects.data, aes(x = factor(manip.interactive.type.manip, level = c('alone_together', 'con_hetero', 'shade', 'drought', 'temperature', 'grazing/herbivory', 'mining',  'fertilization', 'fire', 'tornado')), y = manip.interactive.mean, group = manip.interactive.type)) +
    geom_errorbar(aes(ymin = manip.interactive.lower_ci, ymax = manip.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = manip.interactive.type), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("darkviolet", "gold1", "royalblue1"), breaks = c("competition", "stress", "disturbance")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    scale_x_discrete(labels = c("alone_together" = "Together/\nAlone", "con_hetero" = "Inter/\nIntra", "drought" = "Drought", "fertilization" = "Fert", "grazing/herbivory" = "Graz\n& Herb", "mining" = "Mining", "shade" = "Shade", "temperature" = "Temp", "fire" = "Fire", "tornado" = "Tornado")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_text(face = "bold", color = "black", size = 30),
          axis.title.x = element_text(face = "bold", color = "black", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 25),
          axis.text.x = element_text(face = "plain", color = "black", size = 14, vjust = 0.7, angle = 30),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0.3, "cm"),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))

## figures together side-by-side
(manipulation_fig <- plot_grid(manip.main.effects.panel, manip.interactive.effects.panel, align = "v", ncol = 1, rel_widths = c(1, 1)))
# exporting dimensions: 4200 height x 1200 width

ggsave(manipulation_fig, filename = "manipulation_fig2_rev.eps", width = 75, height = 100, units = c("cm"), dpi = 600)




########## Figure 3A: Experiment location: Field vs greenhouse main effects ##########
exp.local <- c('Field Exps', 'GH Exps', 'Field Exps', 'GH Exps', 'Field Exps', 'GH Exps')
exp.local.main.indiv.type <- c('psf', 'psf', 'competition', 'competition', 'stress', 'stress')
exp.local.main.indiv.mean <- c(0.0493, -0.0174, -0.5028, -0.0919, -0.0448, 0.1942) 
exp.local.main.indiv.lower_ci <- c(-0.0266, -0.0426, -0.6156, -0.1389, -0.1693, 0.1317)
exp.local.main.indiv.upper_ci <- c(0.1251, 0.0077, -0.3901, -0.0450, 0.0797, 0.2568)

(exp.local.main.indiv.effects.data <- data.frame(exp.local, exp.local.main.indiv.type, exp.local.main.indiv.mean, exp.local.main.indiv.lower_ci, exp.local.main.indiv.upper_ci))

(exp.local.main.individual.effects.panel <- ggplot(exp.local.main.indiv.effects.data, aes(x = factor(exp.local.main.indiv.type, level = c('psf', 'competition', 'stress')), y = exp.local.main.indiv.mean, group = exp.local)) +
    geom_errorbar(aes(ymin = exp.local.main.indiv.lower_ci, ymax = exp.local.main.indiv.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = exp.local), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("#453781FF", "#DCE319FF"), breaks = c("Field Exps", "GH Exps")) +
    xlab("\nMain Effects") +
    ylab("RII\n") +
    scale_x_discrete(labels = c("psf" = "PSF", "competition"  = "Competition", "stress" = "Stress")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) + 
    coord_capped_cart(left = "both") +
    annotate("text", x = 3.2, y = 1.0, label = "A", size = 12, fontface = "bold") +
    annotate("text", x = c(1.95, 2.08, 3.08), y = c(0.4, 0.4, 0.4), label = "*", size = 14, fontface = "bold") +
    guides(fill = guide_legend(title = "Experiment Location", title.theme = element_text(face = "bold", size = 30), label.theme = element_text(size = 22, angle = 0))) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_text(face = "bold", size = 40),
          axis.title.x = element_text(face = "bold", size = 36),
          axis.text.y = element_text(face = "plain", color = "black", size = 30),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0.3, "cm"),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = c(0.20, 0.90)))



########## Figure 3B: Experiment location: Field vs greenhouse interactive effects ##########
exp.local.interactive <- c('Field Exps', 'GH Exps', 'Field Exps', 'GH Exps')
exp.local.main.interactive.type <- c('Competition', 'Competition', 'Stress', 'Stress')
exp.local.main.interactive.mean <- c(0.0353, -0.0259, 0.1671, 0.0154)
exp.local.main.interactive.lower_ci <- c(-0.0838, -0.0606, 0.0044, -0.0913)
exp.local.main.interactive.upper_ci <- c(0.1543, 0.0089, 0.3299, 0.1222)

(exp.local.main.interactive.effects.data <- data.frame(exp.local.interactive, exp.local.main.interactive.type, exp.local.main.interactive.mean, exp.local.main.interactive.lower_ci, exp.local.main.interactive.upper_ci))


(exp.local.main.interactive.effect.panel <- ggplot(exp.local.main.interactive.effects.data, aes(x = factor(exp.local.main.interactive.type, level = c('Competition', 'Stress')), y = exp.local.main.interactive.mean, group = exp.local.interactive)) +
    geom_errorbar(aes(ymin = exp.local.main.interactive.lower_ci, ymax = exp.local.main.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = exp.local.interactive), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("#453781FF", "#DCE319FF"), breaks = c("Field Exps", "GH Exps")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) + 
    annotate("text", x = 2.2, y = 1.0, label = "B", size = 12, fontface = "bold") +
    annotate("text", x = c(1.93), y = c(0.5), label = "*", size = 14, fontface = "bold") +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = 36),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))


## figures together side-by-side
exp_locat_fig <- plot_grid(exp.local.main.individual.effects.panel, exp.local.main.interactive.effect.panel, nrow = 1, rel_widths = c(1.5, 1))

ggsave(exp_locat_fig, filename = "exp_locat_fig3_rev.eps", width = 60, height = 20, units = c("cm"), dpi = 600)


########## Figure 4A: Soil inoculum conditioning source: Field vs greenhouse main effects ##########
inoc.source.main.indiv <- c('Field-conditioned', 'GH-conditioned', 'Field-conditioned', 'GH-conditioned', 'Field-conditioned', 'GH-conditioned') 
inoc.source.main.indiv.type <- c('PSF', 'PSF', 'Competition', 'Competition', 'Stress', 'Stress')
inoc.source.main.indiv.mean <- c(-0.0639, 0.0444, -0.1144, -0.1674, -0.0294, -0.0622) 
inoc.source.main.indiv.lower_ci <- c(-0.0950, 0.0119, -0.1666, -0.2228, -0.1160, -0.1743)
inoc.source.main.indiv.upper_ci <- c(-0.0328, 0.0769, -0.0622, -0.1120, 0.0572, 0.0499)

(inoc.source.main.indiv.effects.data <- data.frame(inoc.source.main.indiv, inoc.source.main.indiv.type, inoc.source.main.indiv.mean, inoc.source.main.indiv.lower_ci, inoc.source.main.indiv.upper_ci))

(inoc.source.main.indiv.effects.panel <- ggplot(inoc.source.main.indiv.effects.data, aes(x = factor(inoc.source.main.indiv.type, level = c('PSF', 'Competition', 'Stress')), y = inoc.source.main.indiv.mean, group = inoc.source.main.indiv)) +
    geom_errorbar(aes(ymin = inoc.source.main.indiv.lower_ci, ymax = inoc.source.main.indiv.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = inoc.source.main.indiv), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("#453781FF", "#DCE319FF"), breaks = c("Field-conditioned", "GH-conditioned")) +
    xlab("\nMain Effects") +
    ylab("RII\n") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) +
    coord_capped_cart(left = "both") +
    annotate("text", x = 3.2, y = 1.0, label = "A", size = 12, fontface = "bold") +
    annotate("text", x = c(0.95, 1.08, 1.95, 2.08), y = c(0.3, 0.3, 0.3, 0.3), label = "*", size = 14, fontface = "bold") +
    guides(fill = guide_legend(title = "Soil inoculum conditioning source", title.theme = element_text(face = "bold", size = 30), label.theme = element_text(size = 22, angle = 0))) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_text(face = "bold", size = 40),
          axis.title.x = element_text(face = "bold", size = 36),
          axis.text.y = element_text(face = "plain", color = "black", size = 30),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0.3, "cm"),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = c(0.32, 0.90)))


########## Figure 4B: Soil inoculum conditioning source: Field vs greenhouse interactive effects ##########
inoc.source.main.interactive <- c('Field-conditioned', 'GH-conditioned', 'Field-conditioned', 'GH-conditioned') 
inoc.source.main.interactive.type <- c('Competition', 'Competition', 'Stress', 'Stress')
inoc.source.main.interactive.mean <- c(-0.0250, -0.0188, 0.0442, 0.0180)
inoc.source.main.interactive.lower_ci <- c(-0.0719, -0.0617, 0.0036, -0.0410)
inoc.source.main.interactive.upper_ci <- c(0.0219, 0.0241, 0.0848, 0.0769) 

(inoc.source.main.interactive.effects.data <- data.frame(inoc.source.main.interactive, inoc.source.main.interactive.type, inoc.source.main.interactive.mean, inoc.source.main.interactive.lower_ci, inoc.source.main.interactive.upper_ci))

(inoc.source.main.interactive.effects.panel <- ggplot(inoc.source.main.interactive.effects.data, aes(x = factor(inoc.source.main.interactive.type, level = c('Competition', 'Stress')), y = inoc.source.main.interactive.mean, group = inoc.source.main.interactive)) +
    geom_errorbar(aes(ymin = inoc.source.main.interactive.lower_ci, ymax = inoc.source.main.interactive.upper_ci), width = 0.2, position = position_dodge(0.3)) +
    geom_point(aes(fill = inoc.source.main.interactive), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("#453781FF", "#DCE319FF"), breaks = c("Field-conditioned", "GH-conditioned")) +
    xlab("\nInteraction Effects") +
    ylab("RII\n") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_symmetric(mid = 0) + 
    annotate("text", x = 2.2, y = 1.0, label = "B", size = 12, fontface = "bold") +
    annotate("text", x = c(1.93), y = c(0.3), label = "*", size = 14, fontface = "bold") +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(face = "bold", size = 36),
          axis.text.y = element_blank(),
          axis.text.x = element_text(face = "plain", color = "black", size = 26, vjust = 0.7),
          axis.ticks = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))

## figures together side-by-side
conditioning_source_fig <- plot_grid(inoc.source.main.indiv.effects.panel, inoc.source.main.interactive.effects.panel, nrow = 1, rel_widths = c(1.5, 1))

ggsave(conditioning_source_fig, filename = "conditioning_source_fig4_rev.eps", width = 60, height = 25, units = c("cm"), dpi = 600)






######### New figs ##########
inter.by.intra.by.psf.name <- c('Live_sterile', 'Live_sterile', 'Away_home', 'Away_home', 'Untreated/Fungicide', 'Untreated/Fungicide')
inter.by.intra.by.psf.color <- c('no_comp', 'with_comp', 'no_comp', 'with_comp', 'no_comp', 'with_comp')
inter.by.intra.by.psf.mean <- c(-0.0784, 0.0315, 0.0154, -0.0469, -0.0015, 0.0037)
inter.by.intra.by.psf.lower_ci <- c(-0.1132, -0.0446, -0.0123, -0.0908, -0.074, -0.1432)
inter.by.intra.by.psf.upper_ci <- c(-0.0437, 0.1076, 0.0432, -0.0031, 0.0709, 0.1506)

inter.by.intra.by.psf.data <- data.frame(inter.by.intra.by.psf.name, inter.by.intra.by.psf.color, inter.by.intra.by.psf.mean, inter.by.intra.by.psf.lower_ci, inter.by.intra.by.psf.upper_ci)


(inter.by.intra.by.psf.fig <- ggplot(inter.by.intra.by.psf.data, aes(x = factor(inter.by.intra.by.psf.name, level = c('Live_sterile', 'Away_home', 'Untreated/Fungicide')), y = inter.by.intra.by.psf.mean, group = inter.by.intra.by.psf.color)) +
    geom_errorbar(aes(ymin = inter.by.intra.by.psf.lower_ci, ymax = inter.by.intra.by.psf.upper_ci), width = 0.1, position = position_dodge(0.3)) +
    geom_point(aes(fill = inter.by.intra.by.psf.color), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("white", "grey50")) +
    xlab("PSF type") +
    ylab("RII") +
    scale_x_discrete(labels = c("Live_sterile" = "Live/\nsterile", "Away_home" = "Away/\nhome", "Untreated/Fungicide" = "Untreat/\nFungicide")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-0.20, 0.20), breaks = c(-0.20, 0, 0.20)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_text(face = "bold", size = 30),
          axis.title.x = element_text(face = "bold", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 26),
          axis.text.x = element_text(face = "plain", color = "black", size = 20, vjust = 0.7),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))



alone.togeth.by.psf.name <- c('Live_sterile', 'Live_sterile', 'Away_home', 'Away_home', 'Untreated/Fungicide', 'Untreated/Fungicide')
alone.togeth.by.psf.color <- c('no_comp', 'with_comp', 'no_comp', 'with_comp', 'no_comp', 'with_comp')
alone.togeth.by.psf.mean <- c(-0.0784,    -0.0518,     0.0154,     0.0094,      -0.0015, 0.0603)
alone.togeth.by.psf.lower_ci <- c(-0.1132, -0.1408,   -0.0123,    -0.0751,      -0.074, -0.0685)
alone.togeth.by.psf.upper_ci <- c(-0.0437, 0.0372,     0.0432,     0.0938,      0.0709, 0.1891)

alone.togeth.by.psf.data <- data.frame(alone.togeth.by.psf.name, alone.togeth.by.psf.color, alone.togeth.by.psf.mean, alone.togeth.by.psf.lower_ci, alone.togeth.by.psf.upper_ci)


(alone.togeth.by.psf.fig <- ggplot(alone.togeth.by.psf.data, aes(x = factor(alone.togeth.by.psf.name, level = c('Live_sterile', 'Away_home', 'Untreated/Fungicide')), y = alone.togeth.by.psf.mean, group = alone.togeth.by.psf.color)) +
    geom_errorbar(aes(ymin = alone.togeth.by.psf.lower_ci, ymax = alone.togeth.by.psf.upper_ci), width = 0.1, position = position_dodge(0.3)) +
    geom_point(aes(fill = alone.togeth.by.psf.color), size = 9, shape = 21, stroke = 1.5, position = position_dodge(0.3)) +
    scale_fill_manual(values = c("white", "grey50")) +
    xlab("PSF type") +
    ylab("RII") +
    scale_x_discrete(labels = c("Live_sterile" = "Live/\nsterile", "Away_home" = "Away/\nhome", "Untreated/Fungicide" = "Untreat/\nFungicide")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(limits = c(-0.20, 0.20), breaks = c(-0.20, 0, 0.20)) +
    theme_classic(base_size = 10, base_family = "") +
    theme(panel.border = element_rect(linetype = "solid", fill = NA, size = 2),
          axis.title.y = element_text(face = "bold", size = 30),
          axis.title.x = element_text(face = "bold", size = 30),
          axis.text.y = element_text(face = "plain", color = "black", size = 26),
          axis.text.x = element_text(face = "plain", color = "black", size = 20, vjust = 0.7),
          axis.ticks.length = unit(0.3, "cm"),
          axis.ticks.x = element_blank(),
          axis.line.y = element_line(size = 0.5),
          axis.line.x = element_blank(),
          legend.position = "none"))

(patch <- inter.by.intra.by.psf.fig + alone.togeth.by.psf.fig)

