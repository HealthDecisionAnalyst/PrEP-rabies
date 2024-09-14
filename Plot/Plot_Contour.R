library(plotly)

# Function to create contour plot
create_contour_plot <- function(data, y_labels, x_labels) {
  contour_values <- c(100, 200, 400, 800, 1600, 3200, 6400, 12800)
  
  p <- plot_ly(
    y = y_labels,
    x = x_labels,
    z = data,
    type = 'contour',
    autocontour = FALSE,
    ncontours = 2,
    colors = 'grey0',
    contours = list(
      start = 100,
      end = 100,
      showlabels = TRUE,
      showlines = TRUE,
      coloring = "lines"
    )
  ) %>%
    hide_guides() %>%
    layout(xaxis = list(showgrid = FALSE, showline = TRUE, type = "log"),
           yaxis = list(showgrid = FALSE, showline = TRUE)) %>%
    layout(
      xaxis = list(tickvals = list(0.6, 1.6, 5.7, 11, 19)),
      yaxis = list(tickvals = list(0.1, 0.3, 0.5, 0.7, 0.9))
    )
  
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
  
  p
}


fig_R1C1 <- create_contour_plot(R1C1, rownames(R1C1), colnames(R1C1))  # fig_R1C1
fig_R1C2 <- create_contour_plot(R1C2, rownames(R1C2), colnames(R1C2))  # fig_R1C2
fig_R1C3 <- create_contour_plot(R1C3, rownames(R1C3), colnames(R1C3))  # fig_R1C3
fig_R1C4 <- create_contour_plot(R1C4, rownames(R1C4), colnames(R1C4))  # fig_R1C4
fig_R1C5 <- create_contour_plot(R1C5, rownames(R1C5), colnames(R1C5))  # fig_R1C5

fig_R2C1 <- create_contour_plot(R2C1, rownames(R2C1), colnames(R2C1))  # fig_R2C1
fig_R2C2 <- create_contour_plot(R2C2, rownames(R2C2), colnames(R2C2))  # fig_R2C2
fig_R2C3 <- create_contour_plot(R2C3, rownames(R2C3), colnames(R2C3))  # fig_R2C3
fig_R2C4 <- create_contour_plot(R2C4, rownames(R2C4), colnames(R2C4))  # fig_R2C4
fig_R2C5 <- create_contour_plot(R2C5, rownames(R2C5), colnames(R2C5))  # fig_R2C5

fig_R3C1 <- create_contour_plot(R3C1, rownames(R3C1), colnames(R3C1))  # fig_R3C1
fig_R3C2 <- create_contour_plot(R3C2, rownames(R3C2), colnames(R3C2))  # fig_R3C2
fig_R3C3 <- create_contour_plot(R3C3, rownames(R3C3), colnames(R3C3))  # fig_R3C3
fig_R3C4 <- create_contour_plot(R3C4, rownames(R3C4), colnames(R3C4))  # fig_R3C4
fig_R3C5 <- create_contour_plot(R3C5, rownames(R3C5), colnames(R3C5))  # fig_R3C5

fig_R4C1 <- create_contour_plot(R4C1, rownames(R4C1), colnames(R4C1))  # fig_R4C1
fig_R4C2 <- create_contour_plot(R4C2, rownames(R4C2), colnames(R4C2))  # fig_R4C2
fig_R4C3 <- create_contour_plot(R4C3, rownames(R4C3), colnames(R4C3))  # fig_R4C3
fig_R4C4 <- create_contour_plot(R4C4, rownames(R4C4), colnames(R4C4))  # fig_R4C4
fig_R4C5 <- create_contour_plot(R4C5, rownames(R4C5), colnames(R4C5))  # fig_R4C5

fig_R5C1 <- create_contour_plot(R5C1, rownames(R5C1), colnames(R5C1))  # fig_R5C1
fig_R5C2 <- create_contour_plot(R5C2, rownames(R5C2), colnames(R5C2))  # fig_R5C2
fig_R5C3 <- create_contour_plot(R5C3, rownames(R5C3), colnames(R5C3))  # fig_R5C3
fig_R5C4 <- create_contour_plot(R5C4, rownames(R5C4), colnames(R5C4))  # fig_R5C4
fig_R5C5 <- create_contour_plot(R5C5, rownames(R5C5), colnames(R5C5))  # fig_R5C5



subplot(list(fig_R1C1,fig_R1C2, fig_R1C3, fig_R1C4, fig_R1C5,
             fig_R2C1,fig_R2C2, fig_R2C3, fig_R2C4, fig_R2C5,
             fig_R3C1,fig_R3C2, fig_R3C3, fig_R3C4, fig_R3C5,
             fig_R4C1,fig_R4C2, fig_R4C3, fig_R4C4, fig_R4C5,
             fig_R5C1,fig_R5C2, fig_R5C3, fig_R5C4, fig_R5C5),
        nrows=5,widths=c(.2,.224,.224,.224,.2)/1.072 ,heights=c(.2,.224,.224,.224,.2)/1.072,
        shareX=T,shareY=T,titleX=T,titleY=T,which_layout = "merge") %>% layout(showlegend=F)



