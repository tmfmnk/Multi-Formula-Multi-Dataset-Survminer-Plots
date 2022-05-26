#Libraries

library(tidyverse)
library(survminer)
library(cowplot)

#Generate mock datasets

set.seed(123)

df1 <- data.frame(event1 = sample(c(0, 1), 500, replace = TRUE),
                  time_to_event1 = sample(500),
                  exposure = sample(c(0, 1), 500, replace = TRUE))

df2 <- data.frame(event2 = sample(c(0, 1), 500, replace = TRUE),
                  time_to_event2 = sample(500),
                  exposure = sample(c(0, 1), 500, replace = TRUE))

#Combine dfs into a list

df_list <- mget(ls(pattern = "^df"))

#Extract event names

events <- map_chr(.x = df_list, 
                  ~ .x %>%
                   select(starts_with("event")) %>%
                   names())

#Survival probability plots
#Create formulas 

formulas <- map(.x = events, 
                ~ as.formula(paste("Surv(", 
                                   paste0("time_to_", .x),
                                   ", ",
                                   .x,
                                   ")",
                                   "~ exposure")))

#Define custom plot layout

custom_theme <- function() {
 theme_survminer() %+replace%
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10, angle = 90),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        legend.title = element_blank())
}

#Programmatic plotting and saving of plots
#The match.fd paramater must be set to TRUE to apply a specific formula to each dataset

map2(surv_fit(formula = formulas,
              data = df_list,
              match.fd = TRUE),
     events,
     
     function(x, y) {
      
      p <- ggsurvplot(x, 
                      conf.int = TRUE, 
                      conf.int.alpha = 0.3,
                      risk.table = TRUE,
                      cumcensor = TRUE,
                      legend.labs = c("Unexposed", "Exposed"),
                      title = paste0(y, " probability by exposure"),
                      xlab = "Time (days) since exposure",
                      ylab = "Survival probability (95% CI)",
                      palette = c("blue", "red"),
                      legend.title = "",
                      ggtheme = custom_theme())
      
      p1 <- p$plot
      p2 <- p$table
      p3 <- p$ncensor.plot
      plots <- plot_grid(p1, p2, p3, align = "v", ncol = 1, rel_heights = c(4, 1, 1))
      
      ggsave(plot = plots,
             filename = paste0("survival_probability_plot_", y, ".png"),
             path = "/path/",
             device = "png",
             width = 10, 
             height = 7, 
             dpi = 300)
     }
)
