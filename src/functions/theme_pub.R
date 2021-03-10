# GGPLOT Theme ------------------------------------------------------------
theme_pub <- function(base_size=12, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 14),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=12),
            legend.position = "right",
            legend.text = element_text(size=12),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),

            axis.title = element_text(size = 14),
            axis.text.x = element_text(size = 12, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 12)))
}
