library(haven)      
library(dplyr)     
library(ggplot2)   
library(patchwork)  

data <- fread("C:\\Users\\Dell\\replication_package\\data\\input_primary\\rct1_watermeasure.csv") %>%
  filter(!is.na(waterunit))

data_copy <- data

data_copy <- data_copy %>%
  mutate(dat_bin = as.numeric(as.character(
    cut(dat, 
        breaks = seq(0, 180, by = 10), 
        right = FALSE, 
        labels = seq(5, 175, by = 10))
  )))

plot_list <- list()

for (j in c(1, 0)) {
  dat_sub <- data_copy %>% filter(anymarginal == j)
  titre <- if (j == 1) "A: Volumetric Price" else "B: Fixed Price"
  binned <- dat_sub %>%
    filter(dat < 120) %>%
    group_by(dat_bin, treatment) %>%
    summarise(m_waterlevel = mean(waterlevel, na.rm = TRUE),
              m_dryfield  = mean(dryfield, na.rm = TRUE),
              .groups = "drop")
  
  grid_dat <- data.frame(dat = seq(min(dat_sub$dat, na.rm = TRUE), 120, length.out = 200))
  
  # Non-parametric reg for waterlevel
  model_treat <- loess(waterlevel ~ dat, data = dat_sub %>% filter(treatment == 1))
  pred_treat  <- predict(model_treat, newdata = grid_dat)
  model_ctrl  <- loess(waterlevel ~ dat, data = dat_sub %>% filter(treatment == 0))
  pred_ctrl   <- predict(model_ctrl, newdata = grid_dat)
  
  # Non-parametric reg for dryfield
  model_treat_df <- loess(dryfield ~ dat, data = dat_sub %>% filter(treatment == 1))
  pred_treat_df  <- predict(model_treat_df, newdata = grid_dat)
  model_ctrl_df  <- loess(dryfield ~ dat, data = dat_sub %>% filter(treatment == 0))
  pred_ctrl_df   <- predict(model_ctrl_df, newdata = grid_dat)
  
  ## Graph 1 : waterlevel evolution -------------------------------
  p1 <- ggplot() +

    geom_line(data = data.frame(dat = grid_dat$dat, fit = pred_treat),
              aes(x = dat, y = fit), color = "blue", linewidth = 1, na.rm = TRUE) +
    geom_line(data = data.frame(dat = grid_dat$dat, fit = pred_ctrl),
              aes(x = dat, y = fit), color = "black", linewidth = 1, na.rm = TRUE) +
    geom_point(data = binned %>% filter(treatment == 1),
               aes(x = dat_bin, y = m_waterlevel), color = "blue", size = 2) +
    geom_point(data = binned %>% filter(treatment == 0),
               aes(x = dat_bin, y = m_waterlevel), color = "black", size = 2) +
    scale_x_continuous(breaks = seq(0, 120, by = 20), limits = c(0, 120)) +
    scale_y_continuous(breaks = 0:5, limits = c(0, 5)) +
    labs(x = "", y = "Water level CM", title = titre) +
    annotate("text", x = 80, y = 4, label = "Treatment", color = "blue", size = 5) +
    annotate("text", x = 80, y = 3.66, label = "Control", color = "black", size = 5) +
    theme_minimal()
  
  ## Graph 2 : dryfield evolution -------------------------------
  p2 <- ggplot() +
    geom_line(data = data.frame(dat = grid_dat$dat, fit = pred_treat_df),
              aes(x = dat, y = fit), color = "blue", linewidth = 1, na.rm = TRUE) +
    geom_line(data = data.frame(dat = grid_dat$dat, fit = pred_ctrl_df),
              aes(x = dat, y = fit), color = "black", linewidth = 1, na.rm = TRUE) +
    geom_point(data = binned %>% filter(treatment == 1),
               aes(x = dat_bin, y = m_dryfield), color = "blue", size = 2) +
    geom_point(data = binned %>% filter(treatment == 0),
               aes(x = dat_bin, y = m_dryfield), color = "black", size = 2) +
    scale_x_continuous(breaks = seq(0, 120, by = 20), limits = c(0, 120)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
    labs(x = "", y = "Field is dry") +
    theme_minimal()
  
  ## Graph 3 :  -------------------------------
p3 <- ggplot(dat_sub %>% filter(dat <= 120), aes(x = dat)) +

geom_histogram(aes(y = after_stat(density)), 
                binwidth = 5, 
                color = "gray40", 
                fill = "gray80", 
                boundary = 0) +
scale_x_continuous(breaks = seq(0, 120, by = 20), limits = c(0, 120)) +
labs(x = "Days after transplanting", y = "Density") +
theme_minimal()

  
  plot_list[[paste0("p1_", j)]] <- p1
  plot_list[[paste0("p2_", j)]] <- p2
  plot_list[[paste0("p3_", j)]] <- p3
}

# 3. Combine graphs: 

combined_plot <- (plot_list[["p1_1"]] + plot_list[["p1_0"]]) /
  (plot_list[["p2_1"]] + plot_list[["p2_0"]]) /
  (plot_list[["p3_1"]] + plot_list[["p3_0"]])

# save graph
ggsave("C:\\Users\\Dell\\replication_package\\figure3_waterlevel.pdf", plot = combined_plot, width = 9, height = 12, scale = 0.9)

