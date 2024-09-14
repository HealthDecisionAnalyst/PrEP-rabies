library(plotly)

# Function to create a contour plot using Plotly
# data: 2D matrix or data frame for contour plot values
# y_labels: Vector of labels for the Y-axis
# x_labels: Vector of labels for the X-axis
create_contour_plot <- function(data, y_labels, x_labels) {
  # Predefined contour levels
  contour_values <- c(100, 200, 400, 800, 1600, 3200, 6400, 12800)

  # Initialize contour plot with custom settings
  p <- plot_ly(
    y = y_labels,          # Y-axis labels
    x = x_labels,          # X-axis labels
    z = data,              # Contour data
    type = 'contour',
    autocontour = FALSE,   # Disable automatic contour levels
    ncontours = 2,         # Number of contour levels
    colors = 'grey0',      # Set color to grey
    contours = list(
      start = 100,         # Starting contour value
      end = 100,           # Ending contour value (single-level)
      showlabels = TRUE,   # Show labels on the contour lines
      showlines = TRUE,    # Display contour lines
      coloring = "lines"   # Color only the contour lines
    )
  ) %>%
    hide_guides() %>%      # Hide guides for better visualization
    layout(xaxis = list(showgrid = FALSE, showline = TRUE, type = "log"),   # X-axis log scale
           yaxis = list(showgrid = FALSE, showline = TRUE)) %>%             # Hide Y-axis grid lines
    layout(
      xaxis = list(tickvals = list(0.6, 1.6, 5.7, 11, 19)),                 # Set custom tick values for X-axis
      yaxis = list(tickvals = list(0.1, 0.3, 0.5, 0.7, 0.9))                # Set custom tick values for Y-axis
    )
  
# Add multiple contour levels using a loop
for (value in contour_values) {
    p <- p %>%
      add_contour(
        ncontours = 2,
        colors = 'grey0',
        contours = list(
          start = value,
          end = value,
          showlabels = TRUE,
          showlines = FALSE,
          coloring = "lines"
        )
      ) %>%
      hide_guides()
  }
  
  return(p)                                                            # Return the final plot object
}

# Create individual contour plots for each dataset
# R1C1 to R5C5 are matrices or data frames representing different data sets
fig_R1C1 <- create_contour_plot(R1C1, rownames(R1C1), colnames(R1C1))  # Contour plot for fig_R1C1
fig_R1C2 <- create_contour_plot(R1C2, rownames(R1C2), colnames(R1C2))  # Contour plot for fig_R1C2
fig_R1C3 <- create_contour_plot(R1C3, rownames(R1C3), colnames(R1C3))  # Contour plot for fig_R1C3
fig_R1C4 <- create_contour_plot(R1C4, rownames(R1C4), colnames(R1C4))  # Contour plot for fig_R1C4
fig_R1C5 <- create_contour_plot(R1C5, rownames(R1C5), colnames(R1C5))  # Contour plot for fig_R1C5

fig_R2C1 <- create_contour_plot(R2C1, rownames(R2C1), colnames(R2C1))  # Contour plot for fig_R2C1
fig_R2C2 <- create_contour_plot(R2C2, rownames(R2C2), colnames(R2C2))  # Contour plot for fig_R2C2
fig_R2C3 <- create_contour_plot(R2C3, rownames(R2C3), colnames(R2C3))  # Contour plot for fig_R2C3
fig_R2C4 <- create_contour_plot(R2C4, rownames(R2C4), colnames(R2C4))  # Contour plot for fig_R2C4
fig_R2C5 <- create_contour_plot(R2C5, rownames(R2C5), colnames(R2C5))  # Contour plot for fig_R2C5

fig_R3C1 <- create_contour_plot(R3C1, rownames(R3C1), colnames(R3C1))  # Contour plot for fig_R3C1
fig_R3C2 <- create_contour_plot(R3C2, rownames(R3C2), colnames(R3C2))  # Contour plot for fig_R3C2
fig_R3C3 <- create_contour_plot(R3C3, rownames(R3C3), colnames(R3C3))  # Contour plot for fig_R3C3
fig_R3C4 <- create_contour_plot(R3C4, rownames(R3C4), colnames(R3C4))  # Contour plot for fig_R3C4
fig_R3C5 <- create_contour_plot(R3C5, rownames(R3C5), colnames(R3C5))  # Contour plot for fig_R3C5

fig_R4C1 <- create_contour_plot(R4C1, rownames(R4C1), colnames(R4C1))  # Contour plot for fig_R4C1
fig_R4C2 <- create_contour_plot(R4C2, rownames(R4C2), colnames(R4C2))  # Contour plot for fig_R4C2
fig_R4C3 <- create_contour_plot(R4C3, rownames(R4C3), colnames(R4C3))  # Contour plot for fig_R4C3
fig_R4C4 <- create_contour_plot(R4C4, rownames(R4C4), colnames(R4C4))  # Contour plot for fig_R4C4
fig_R4C5 <- create_contour_plot(R4C5, rownames(R4C5), colnames(R4C5))  # Contour plot for fig_R4C5

fig_R5C1 <- create_contour_plot(R5C1, rownames(R5C1), colnames(R5C1))  # Contour plot for fig_R5C1
fig_R5C2 <- create_contour_plot(R5C2, rownames(R5C2), colnames(R5C2))  # Contour plot for fig_R5C2
fig_R5C3 <- create_contour_plot(R5C3, rownames(R5C3), colnames(R5C3))  # Contour plot for fig_R5C3
fig_R5C4 <- create_contour_plot(R5C4, rownames(R5C4), colnames(R5C4))  # Contour plot for fig_R5C4
fig_R5C5 <- create_contour_plot(R5C5, rownames(R5C5), colnames(R5C5))  # Contour plot for fig_R5C5


# Arrange the contour plots into a 5x5 grid layout using subplot function
# Each row (R1, R2, etc.) contains five contour plots
subplot(list(fig_R1C1,fig_R1C2, fig_R1C3, fig_R1C4, fig_R1C5,          # First row plots
             fig_R2C1,fig_R2C2, fig_R2C3, fig_R2C4, fig_R2C5,          # Second row plots
             fig_R3C1,fig_R3C2, fig_R3C3, fig_R3C4, fig_R3C5,          # Third row plots
             fig_R4C1,fig_R4C2, fig_R4C3, fig_R4C4, fig_R4C5,          # Fourth row plots
             fig_R5C1,fig_R5C2, fig_R5C3, fig_R5C4, fig_R5C5),         # Fifth row plots
        nrows=5,                                                       # Number of rows in the grid (5x5)
        widths=c(.2,.224,.224,.224,.2)/1.072,                          # Adjust width ratios for each column
        heights=c(.2,.224,.224,.224,.2)/1.072,                         # Adjust height ratios for each row
        shareX=T,                                                      # Share the X-axis across all plots
        shareY=T,                                                      # Share the Y-axis across all plots
        titleX=T,                                                      # Show X-axis titles
        titleY=T,                                                      # Show Y-axis titles
        which_layout = "merge"                                         # Merge the layouts for a unified appearance
       ) %>% layout(showlegend=F)                                      # Hide the legend for a cleaner display



