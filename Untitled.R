
ggplot(data=ol$intersect_data, aes(x=intersect_id, y=freq)) +
  geom_bar(stat='identity', width = .4) +
  theme(plot.margin = unit(c(0,0,0,0), "cm"),
        panel.spacing = unit(c(0,0,0,0), "cm"),
        plot.background =  element_rect(colour = 'red'),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        rect = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank()
        ) + scale_y_continuous(expand = c(0,0))
  # theme(aspect.ratio = 2/1) +
  # theme_void()
