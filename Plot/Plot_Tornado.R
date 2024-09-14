###### plot Tornado ######
# set ICER base to the middle of Tornado plot
# to plot Tornado for (no-discount strategy) or (1.5% discount strategy) or (3% discount strategy) 
# one need to change re-run code Tornafo_ICER.R using discount_rate, which can be {0,0.015,0.03}
# and then use the scale_y_continuus() below that correspond to the right discount strategy 

# Set up scale for ICER values, starting from the base case (no-discount strategy, discount_rate <- 0.0)
# The base ICER value is taken from the first element of the data.Tornado$ICER.base vector
scale_ICER.base0 <- round(data.Tornado$ICER.base[1])
scale_ICER.base1 <- scale_ICER.base+1000   # ICER base + 1000
scale_ICER.base2 <- scale_ICER.base+2000   # ICER base + 2000
scale_ICER.base3 <- scale_ICER.base+3000   # ICER base + 3000
scale_ICER.base4 <- scale_ICER.base+4000   # ICER base + 4000
scale_ICER.base5 <- scale_ICER.base+5000   # ICER base + 5000
scale_ICER.base6 <- scale_ICER.base+6000   # ICER base + 6000
scale_ICER.base7 <- scale_ICER.base+7000   # ICER base + 7000

# Create Tornado plot for Incremental Cost-Effectiveness Ratio (ICER)
p <- ggplot(dataForTornado, aes(x = reorder(variable,+abs(ICER.dif)), y=ICER.dif, fill = Level)) +
  geom_bar(stat="identity", width = 0.5) +                       # Create horizontal bars for each variable
  scale_fill_manual(values=c("#FF6666", "darkblue")) +           # Set custom colors for the bars
  geom_abline(slope=0, intercept = 0, col = "black") +           # Add a horizontal line at ICER = 0 (base case)

# Customize Y-axis scale to display ICER values with manual labels
scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3)) +

# Optionally, uncomment the lines below to extend the Y-axis scale further
#scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3, "4000" = scale_ICER.base4,"5000" = scale_ICER.base5)) +
#scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3, "4000" = scale_ICER.base4,"5000" = scale_ICER.base5,"6000" = scale_ICER.base6 ,"7000" = scale_ICER.base7)) +
  coord_flip() +                                                                   # Flip the coordinates to make the bars horizontal
  labs(title = "Tornado Plot for Incremental Cost-Effectiveness Ratio (ICER)",     # Set plot title
       #caption = "The black line represents the ICER at the base case, which is 354, (no-discount strategy)", 
       #caption = "The black line represents the ICER at the base case, which is 506, (1.5% discount strategy)", 
       #caption = "The black line represents the ICER at the base case, which is 688, (3% discount strategy)", 
       x = "Parameters",                                                           # X-axis label (parameters)
       y = "ICER") +                                                               # Y-axis label (ICER differences)
  theme_minimal()                                                                  # Apply a minimal theme for clean visuals
# Render the plot
p

###### END Tornado Plot ######
