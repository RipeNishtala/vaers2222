library(ggplot2)
library(shadowtext)
library(grid)
library(tidyverse)

vaers_total_adrs$adr <- as.factor(vaers_total_adrs$adr)
plt <- vaers_total_adrs %>%
  filter(!adr=="") %>%
  filter(n>5000) %>%
ggplot+
  geom_col(aes(x=n, y=adr), width = 0.9)

plt <- plt +
  scale_x_continuous(
    limits = c(0, 50000),
    breaks = seq(5000, 50000, by = 10000),
    expand = c(0, 0), # The horizontal axis does not extend to either side
    position = "top"  # Labels are located on the top
  ) +
  # The vertical axis only extends upwards
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Set the color and the width of the grid lines for the horizontal axis
    panel.grid.major.x = element_line(color = "#A8BAC4", size = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length = unit(0, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only left line of the vertical axis is painted in black
    axis.line.y.left = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = "sans", size = 12)
  )

plt

plt2 <- plt +
  geom_shadowtext(
    #data = subset(data, n < 5000),
    aes(n, y = adr, label = adr),
    hjust = 0,
    nudge_x = 0.3,
    bg.colour = "white",
    bg.r = 0.05,
    family = "sans",
    size = 3
  ) +
  geom_text(
    #data = subset(data, n >= 5000),
    aes(0, y = adr, label = adr),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = "sans",
    size = 3
  )

plt2

plt3 <- plt2 +
  labs(
    title = "Moderna",
    subtitle = "Adverse drug events > 5000 from case reports, 2021"
  ) +
  theme(
    plot.title = element_text(
      family = "sans",
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "sand",
      size = 20
    )
  )

# Make room for annotations
plt4 <- plt3 +
  theme(
    plot.margin = margin(0.1, 0, 0.1, 0.09, "npc")
  )

# Print the ggplot2 plot
plt4

# Add horizontal line on top
# It goes from x = 0 (left) to x = 1 (right) on the very top of the chart (y = 1)
# You can think of 'gp' and 'gpar' as 'graphical parameters'.
# There we indicate the line color and width
grid.lines(
  x = c(0, 1),
  y = 1,
  gp = gpar(col = "#e5001c", lwd = 4)
)

# Add rectangle on top-left
# lwd = 0 means the rectangle does not have an outer line
# 'just' gives the horizontal and vertical justification
grid.rect(
  x = 0,
  y = 1,
  width = 0.05,
  height = 0.025,
  just = c("left", "top"),
  gp = gpar(fill = "#e5001c", lwd = 0)
)

print(plt4)



