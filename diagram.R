library(dataRetrieval)
library(ggplot2)
library(dplyr)
library(lubridate)
library(extrafont)

site_number <- "08167000"
parameter_code <- "00065"
start_date <- "2025-07-01"
end_date <- "2025-07-06"

gage_data <- readNWISuv(siteNumbers = site_number,
                        parameterCd = parameter_code,
                        startDate = start_date,
                        endDate = end_date)

gage_data_cleaned <- gage_data %>%
  rename(gage_height_ft = X_00065_00000) %>%
  mutate(dateTime = as_datetime(dateTime, tz = "America/Chicago"))

flood_stages <- data.frame(
  level = c(10, 15, 25, 30),
  name = c("Action Stage", "Minor Flood", "Moderate Flood", "Major Flood")
)

stage_colors <- c(
  "Action Stage" = "#FFA500",
  "Minor Flood" = "#FF0000",
  "Moderate Flood" = "#8B0000",
  "Major Flood" = "#800080"
)

time_range <- range(gage_data_cleaned$dateTime)


date_breaks <- seq(as.POSIXct("2025-07-01 00:00:00", tz = "America/Chicago"),
                   as.POSIXct("2025-07-07 00:00:00", tz = "America/Chicago"),
                   by = "1 day")

result <- ggplot(gage_data_cleaned) +

  annotate("rect",
           xmin = time_range[1], xmax = time_range[2],
           ymin = c(0, 10, 15, 25, 30),
           ymax = c(10, 15, 25, 30, max(gage_data_cleaned$gage_height_ft, na.rm = TRUE) * 1.2),
           fill = c("white", stage_colors),
           alpha = 0.2) +
  

  geom_line(aes(x = dateTime, y = gage_height_ft), 
            color = "navy", 
            size = 1.3,
            lineend = "round",
            linejoin = "round") +
  
  
  geom_hline(data = flood_stages, 
             aes(yintercept = level, color = name),
             linetype = "dashed", size = 1) +
  
  annotate("segment",
           x = max_point$dateTime - hours(12), 
           xend = max_point$dateTime,
           y = max_point$gage_height_ft + 5, 
           yend = max_point$gage_height_ft + 0.5,
           arrow = arrow(length = unit(0.3, "cm"))) +
  
  annotate("label",
           x = max_point$dateTime - hours(12),
           y = max_point$gage_height_ft + 5,
           label = paste("2025-07-04 (35.64 ft)"),
           family = "Times New Roman",
           size = 4,
           color = "black",
           fill = "white",
           label.padding = unit(0.4, "lines")) +
  scale_color_manual(
    name = "Flood Stages",
    values = stage_colors,
    labels = c("Action Stage (10 ft)", "Minor Flood (15 ft)",
               "Moderate Flood (25 ft)", "Major Flood (30 ft)")
  ) +
  labs(
    title = paste("Guadalupe River at Comfort, Texas (Site:", site_number, ")"),
    x = "Date ",
    y = "Gage Height (feet)",
    caption = "Data Source: USGS"
  ) +
  scale_x_datetime(
    breaks = date_breaks,
    date_labels = "%b %d", 
    limits = c(as.POSIXct("2025-07-01 00:00:00", tz = "America/Chicago"),
               as.POSIXct("2025-07-07 00:00:00", tz = "America/Chicago"))
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.title = element_text(face = "bold", size = 13),
    legend.text = element_text(size = 11),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = "grey", size = 1),
    legend.key = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray90", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  
  coord_cartesian(ylim = c(0, max(gage_data_cleaned$gage_height_ft, na.rm = TRUE) * 1.2))

print(result)
ggsave("result.png", width = 12, height = 6, dpi = 300)
