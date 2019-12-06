for_bites <- sum_id %>%
  select(id, for_bites, site) %>%
  mutate(for_bites_binned = cut(for_bites, breaks = c(0,10,30,65), labels = c("low","med","high")),
         for_bites_rounded = round(for_bites, 0)
         )

ggplot(for_bites, aes(x = for_bites_rounded, fill = factor(site))) +
  geom_histogram() +
  geom_density(alpha=0.6) +
  theme_bw() +
  labs(x = "Grazing intensity (bites/foray)")

ggsave(here("figs/intensity_hist.png"), width = 3, height = 3)

ggplot(sum_ssp_sub, aes(x = for_bites_median, y = fr_median)) +
  geom_point() +
  labs(x = "Grazing intensity (bites/foray)", y = "Feeding rate (bites/min)")
ggsave(here("figs/intensity_vs_feedingrate.png"), width = 3, height = 3)

ggplot(sum_id, aes(x = for_bites, y = fr)) +
  geom_point(aes(colour = factor(site)))

## Loess plots of various drivers for grazing intensity

gi_q1 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = scar_bm/1000, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "",
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 1, 0, 0, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_q2 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = consp_den, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "", 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_q3 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = rugosity, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "", 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_q4 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = ta_canopy, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "", 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_q5 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = ta_cover, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "",
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_q7 <- ggplot(filter(sum_ssp_sub, species_code == "qup"), aes(x = ma_cover, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 25) +
  labs(y = "", 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
  )

gi_s1 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = scar_bm/1000, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "",
       x = expression(atop(NA, atop(textstyle('Scarid biomass'),
                                    textstyle('(kg/100m'^2*')')))),
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 1, 0, 0, unit = "pt"),
        axis.title.x = element_text(size = 9)
  )

gi_s2 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = consp_den, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "", 
       x = expression(atop(NA, atop(textstyle('Conspec. density'),
                                    textstyle('(indv/100m'^2*')')))),
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 9),
        plot.margin = margin(0, 1, 0, 1, unit = "pt")
  )

gi_s3 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = rugosity, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "", 
       x = expression(atop(NA, atop(textstyle('Rugosity'),
                                    textstyle('  ')))), 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 9),
        plot.margin = margin(0, 1, 0, 1, unit = "pt")
  )

gi_s4 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = ta_canopy, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "", 
       x = expression(atop(NA, atop(textstyle('EAM canopy'),
                                    textstyle('height (mm)')))), 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 9),
        plot.margin = margin(0, 1, 0, 1, unit = "pt")
  )

gi_s5 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = ta_cover, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "", 
       x = expression(atop(NA, atop(textstyle('EAM cover'),
                                    textstyle('(%)')))), 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 9),
        plot.margin = margin(0, 1, 0, 1, unit = "pt")
  )

gi_s7 <- ggplot(filter(sum_ssp_sub, species_code == "stop"), aes(x = ma_cover, y = for_bites_median)) +
  geom_smooth(method="loess", span=1) +
  geom_point(shape=18) +
  ylim(-5, 15) +
  labs(y = "", 
       x = expression(atop(NA, atop(textstyle('Macroalgal cover'),
                                    textstyle('(%)')))), 
       title = "") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 9),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0, 1, 0, 1, unit = "pt")
  )

label_vet = textGrob("S. vetula", gp = gpar(fontface="italic"), rot=270)
label_vir = textGrob("S. viride", gp = gpar(fontface="italic"), rot=270)

quartz()
g <- grid.arrange(left = "Grazing intensity (bites/foray)",
                  arrangeGrob(gi_q1, gi_q3, gi_q4, gi_q5, gi_q7, ncol=5, right = label_vet),
                  arrangeGrob(gi_s1, gi_s3, gi_s4, gi_s5, gi_s7, ncol=5, right = label_vir), 
                  nrow=2, heights = c(1,1.2)) 

ggsave(here("figs/biplot_panel_intensity.png"),g)


  