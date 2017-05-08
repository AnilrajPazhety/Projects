library("RColorBrewer")

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))

ggplot(df_plot, aes(x = channel_to, y = channel_from, group = channel_from, color = transition_probability)) +
  #theme_minimal() +
  scale_size(range = c(10, 30),guide = F) +
  #  scale_y_continuous(limits = c(0, 1)) +
  geom_line(size = 2, alpha = 0.3) +
  geom_point(aes(size = transition_probability),alpha = 0.8)+   scale_colour_gradientn(colours = terrain.colors(10))+
  
  #scale_colour_gradientn(colours = myPalette(100)) +
  geom_text(aes(label = paste0(" ", percent(round(transition_probability, 2)))),
            color = 'black', size = 3, hjust = 0.5, vjust = 0.5, fontface = "plain")+
  theme(
        plot.title = element_text(size=15, face="bold", vjust=2,hjust = 0.5),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=16),
        axis.text.x = element_text(size=10, angle=90, hjust=.5, vjust=.5, face="bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        # panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.border = element_rect(colour = "blue", fill=NA, size=1)) +
  labs(x = 'Channels to', y = 'Channels from') +
  ggtitle("Transition Probabilities among channels")