## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(settleR)
library(grid)


## ---- fig.height=10------------------------------------------------------
# Loads an example setlist 
gene_setlist <- 
  system.file('extdata','ex_gene_setlist.rds', package = 'settleR') %>% 
  readRDS(.)

# Creating the SettleR object
setobj <- SettleR(gene_setlist)

# Plotting to the device
settleR_plot(setobj)

## ------------------------------------------------------------------------
# Recommended way to save the plot
settleR_save(setobj, 'example_settleR_plot.pdf')

## ------------------------------------------------------------------------
col_map <- 
  system.file('extdata','ex_col_map.rds', package = 'settleR') %>% 
  readRDS(.)


# Creating the SettleR object w/ colors
setobj <- SettleR(gene_setlist, colMap = col_map)

# Changning the colors on a created object
colMap(setobj) <- col_map

# Plotting to the device
settleR_plot(setobj)


## ------------------------------------------------------------------------

set_levels <- 
  system.file('extdata','ex_set_levels.rds', package = 'settleR') %>% 
  readRDS(.)

# Creating the SettleR object ordered groups
setobj <- SettleR(gene_setlist, setLevels = set_levels)

# Changning group order on a created object
setLevels(setobj) <- set_levels


# Plotting to the device
settleR_plot(setobj)


## ------------------------------------------------------------------------
setobj <- SettleR(gene_setlist, setLevels = set_levels)
setobj <- reorder_by_singletons(setobj)
settleR_plot(setobj)


## ------------------------------------------------------------------------
# Getting the grid plot
tmp_plt <- gridPlot(setobj)
# Making modifications to the text using "theme" from ggplot2 
tmp_plt <- tmp_plt + 
  theme(axis.text.y = element_text(colour = 'blue', face="bold", size = rel(.875)), 
        axis.text.x = element_text(colour='red', face='italic', size = rel(1.25)))
# Updating the SettleR object with the modified plot
gridPlot(setobj) <- tmp_plt
settleR_plot(setobj)



## ---- eval=FALSE---------------------------------------------------------
#  
#  # Extracting the grid plot and intersect levels
#  tmp_plt <-  gridPlot(setobj)
#  intersect_lvls <- intersectLevels(setobj)
#  
#  # Getting the dimensions for the box
#  bound_boxes <- intersect_lvls[c(10:13)] %>%
#    box_intercepts_dims(tmp_plt, .)
#  
#  # Using geom_rect to draw red boxes
#  tmp_plt <- tmp_plt +
#    geom_rect(data=bound_boxes,aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax),
#              inherit.aes = FALSE,
#              color='red',
#              fill=NA,
#              size=.75
#    )
#  # Replacing the modified gridPlot
#  gridPlot(setobj) <- tmp_plt
#  settleR_plot(setobj)
#  

## ---- eval=FALSE---------------------------------------------------------
#  library(VennDiagram)
#  library(sp)
#  library(lefutils)
#  library(rgeos)
#  
#  
#  vp <- venn.diagram(d, alpha = 1, filename = NULL,
#                     category.names=c("A", "B", "C"), scaled=FALSE, euler.d=FALSE)
#  
#  ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
#  labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
#  
#  # Tries to convert str to numbers. Those which
#  # Are NA are letters
#  lab_positions <- labs[is.na(as.numeric(labs$label)),]
#  lab_positions$label <- c("1", "2", "3")
#  
#  
#  circles <- vp[4:6]
#  venn_polygons <- lapply(seq_along(circles), function(i){
#    cur_circ <- circles[[i]]
#    x <- cur_circ[['x']]
#    y <- cur_circ[['y']]
#    # I have no idea what the below line does.
#    p <- SpatialPolygons(list(Polygons(list(Polygon(cbind(x, y))), ID = i)))
#  }) %>% setNames(., c("1", "2", "3"))
#  
#  abc_overlap <- reduce(venn_polygons, gIntersection)
#  ab_overlap <- gIntersection(venn_polygons[[1]], venn_polygons[[2]]) %>% gDifference(., venn_polygons[[3]])
#  a_overlap <- gDifference(venn_polygons[[1]], venn_polygons[[2]]) %>% gDifference(., venn_polygons[[3]])
#  
#  
#  
#  create_custom_annotation <- function(text, x=.25, y=1.05, size=18){
#    text_annot <-
#      grobTree(textGrob(text,
#                        x=x,
#                        y=y,
#                        hjust=0,
#                        gp=gpar(col="black",
#                                fontsize=size,
#                                fontface="bold")))
#  
#    return(text_annot)
#  }
#  create_gg_venn <- function(venn_polygons, ip, lab_positions){
#  
#    intersect_area <-
#      ip@polygons[[1]]@Polygons[[1]]@coords %>%
#      as_tibble(.) %>%
#      setNames(., c('x', 'y'))
#  
#    circles_area <-
#      lapply(venn_polygons, function(x){
#        x@polygons[[1]]@Polygons[[1]]@coords %>%
#          as_tibble(.) %>%
#          setNames(., c('x', 'y'))
#      }) %>%
#      setNames(., c("1", "2", "3"))
#  
#    plot_data <- rbind_named_df_list(circles_area, 'group')
#  
#  
#    plt <-  ggplot() +
#      geom_path(data=plot_data, aes(x=x, y=y, group=group)) +
#      geom_polygon(data=intersect_area, aes(x=x, y=y)) +
#      geom_text(data=lab_positions,
#                aes(x=x, y=y, label=label),
#                size=8,
#                fontface='bold',
#                nudge_y = -.02) +
#      theme_void()
#  
#  
#    # plt <- plt +theme(plot.margin=margin(0, 0, 0, 0), aspect.ratio = 1)
#    plt <- plt +theme(plot.margin=margin(0, 0, 0, 0))
#  
#  
#  
#  
#    return(plt)
#  
#  }
#  
#  
#  venn_plots <-
#    list(a_overlap, ab_overlap, abc_overlap) %>%
#    lapply(., function(x){
#      create_gg_venn(venn_polygons, x, lab_positions)
#    })
#  
#  
#  annot_text_list <- c('A only', 'A & B overlap', 'A, B, & C overlap')
#  
#  
#  
#  
#  
#  
#  

## ---- eval=FALSE---------------------------------------------------------
#  
#  ## Making the grids
#  intersect_id <- factor(rep(paste0("intersect_", 1:3), 3))
#  set_names <- factor(rep(LETTERS[1:3], 3))
#  observed <- as.logical(c(1,1,1, 1,1,0, 1,0,0))
#  
#  
#  
#  grid_data <-
#    data.frame(intersect_id,set_names,observed ) %>%
#    arrange(set_names, intersect_id)
#  grid_data$intersect_id <- factor(intersect_id)
#  
#  
#  
#  grid_plts <-
#      rev(split(grid_data,grid_data$intersect_id)) %>%
#      lapply(., function(x){
#        tmp_plt <- grid_dot_plot(x, dot_size = 10)
#        tmp_plt <- tmp_plt +
#          theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
#  
#        tmp_plt$layers[[4]] <- NULL
#        # tmp_plt <- get_panel(tmp_plt)
#        return(tmp_plt)
#  
#      })
#  
#  
#  
#  
#  adjust <- .05
#  line_coords <- c(.5 - adjust, 1.5 + adjust)
#  grid_plts$intersect_2 <- grid_plts$intersect_2 + geom_vline(xintercept = line_coords)
#  
#  grid_plts <- lapply(grid_plts, get_panel)
#  
#  comb_plts <- lapply(seq_along(grid_plts), function(i){
#    pl <- list(as_grob(venn_plots[[i]]), grid_plts[[i]])
#    tmp_plt <-
#      plot_grid(plotlist = pl,
#                align = 'v',
#                axis ='tb',
#                ncol = 1,
#                rel_heights = c(6, 4), rel_widths = c(1,1))
#  })
#  
#  
#  
#  ggsave2('grid_venn_diagram.pdf', plot = plot_grid(plotlist = comb_plts, nrow = 1, labels = "AUTO", label_y = .5, label_x=0, label_size=24 ), height = 4.5, width = 9)

