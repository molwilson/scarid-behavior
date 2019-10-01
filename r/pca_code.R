# PCA options!

### loading any data file with variables as columns/observations as rows
site <- read.csv(here("data", "site.csv"), strip.white = T, stringsAsFactors = F)

### set up dataframe with only variables that you want to include in the PCA 
pca_data <- site %>%
  # so that row/column names are ready to use as labels, if so desired:
  remove_rownames %>% column_to_rownames(var = "site") %>%
  select(rugosity, ma_cover, ma_canopy, ta_cover, ta_canopy, lc_cover, scar_bm, scar_den, carn30_bm) %>%
  na.omit() %>%
  # note: fviz_pca_biplot can handle names with spaces (e.g., "macroagal cover"), but I've had issues with spaces and autoplot() 
  setNames(c("rugosity", "macroalgal_cover", "macroalgal_canopy", "turf_cover", "turf_canopy", "coral_cover", "scarid_biomass", "scarid_density", "predator_biomass"))

### this does actual PCA
pca_pred <- prcomp(pca_data, center = T, scale = T)

### option A for visualization (this one has variables labeled but not sites)
library(factoextra)
fviz_pca_biplot(pca_pred,
                col.ind = site$island, 
                mean.point = F,
                label = "var",
                repel = T) +
  scale_shape_manual(values=c(15,16,17)) + # changing shape of points by group
  scale_color_manual(values = c("black","black","black")) +
  labs(x = "PC1 (55.6%)", y = "PC2 (24.5%)", title = "") +
  theme_bw() +
  theme(legend.title = element_blank())

### option B for visualization (this one has sites labeled)
library(ggfortify)
autoplot(pca_pred, data = site, shape = 'island', colour = 'island',
         label = T,
         loadings = T, loadings.colour = 'gray',
         loadings.label = T, loadings.label.size = 3, loadings.label.colour = 'black',
         loadings.label.repel = T) +
  scale_color_manual(values = c("cadetblue3","cadetblue3","cadetblue3")) +
  theme_bw() +
  theme(legend.title = element_blank())
