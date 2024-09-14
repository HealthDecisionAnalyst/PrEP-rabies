###### plot Tornado ######
# set ICER base to the middle of Tornado plot
# to plot Tornado for (no-discount strategy) or (1.5% discount strategy) or (3% discount strategy) 
# one need to change re-run code Tornafo_ICER.R using discount_rate, which can be {0,0.015,0.03}
# and then use the scale_y_continuus() below that correspond to the right discount strategy 

# scale ICER base to the middle of Tornado plot for (no-discount strategy: discount_rate <-0.0)
scale_ICER.base0 <- round(data.Tornado$ICER.base[1])
scale_ICER.base1 <- scale_ICER.base+1000
scale_ICER.base2 <- scale_ICER.base+2000
scale_ICER.base3 <- scale_ICER.base+3000
scale_ICER.base4 <- scale_ICER.base+4000
scale_ICER.base5 <- scale_ICER.base+5000
scale_ICER.base6 <- scale_ICER.base+6000
scale_ICER.base7 <- scale_ICER.base+7000

p <- ggplot(dataForTornado, aes(x = reorder(variable,+abs(ICER.dif)), y=ICER.dif, fill = Level)) +
  geom_bar(stat="identity", width = 0.5) +
  scale_fill_manual(values=c("#FF6666", "darkblue")) +
  geom_abline(slope=0, intercept = 0, col = "black") +
  scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3)) +
  #scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3, "4000" = scale_ICER.base4,"5000" = scale_ICER.base5)) +
  #scale_y_continuous(labels=c("0" = scale_ICER.base0, "1000" = scale_ICER.base1, "2000" = scale_ICER.base2, "3000" = scale_ICER.base3, "4000" = scale_ICER.base4,"5000" = scale_ICER.base5,"6000" = scale_ICER.base6 ,"7000" = scale_ICER.base7)) +
  coord_flip() +
  labs(title = "Tornado Plot for Incremental Cost-Effectiveness Ratio (ICER)",
       #caption = "The black line represents the ICER at the base case, which is 354, (no-discount strategy)", 
       #caption = "The black line represents the ICER at the base case, which is 506, (1.5% discount strategy)", 
       #caption = "The black line represents the ICER at the base case, which is 688, (3% discount strategy)", 
       x = "Parameters", 
       y = "ICER") +
  theme_minimal()

p

###### END plot Tornado ######
