
library(tidyverse)
library(circlize)
library(tweenr)

# Based on animated chord diagrams of global migration flow by Gut Abel 
# (estimateshttps://guyabel.com/post/animated-directional-chord-diagrams/)

# Data preparation 

f <- read_csv('cir-02.csv')

names(f)[2] <- 'IS'

f$IS <- str_split(f$IS, "-")

f$year <- lapply(f$IS, `[[`, 1)
f$year <- as.numeric(f$year)

f$name <- lapply(f$IS, '[[', 2)

names(f)[4] <- 'Czech Rep.'

f <- f %>% 
  select(- one_of('IS'))

f <- f[, c(1, 25, 24, 2:23)]

f[is.na(f)] <- 0

# generate colurs for the links
colours <- matrix(rand_color(22*22, transparency = 0.5), 22)

# make the matrix of colours symmetrics, so that the links are not directionals (same colour for A>B as for B>A).
colours[lower.tri(colours)] <- colours[upper.tri(colours)]

# make the self-links white
diag(colours) = "#FFFFFF00"

# generate colours for the nodes
node_col <- rep(c('#fbcba6', '#b8b6b6'), 11)

# 2014 ###
y_1 <- f  %>% 
  filter(year == 2014)

y_1 <- y_1[, 4:25]
y_1 <- as.matrix(y_1)
y_1 <- crossprod(y_1)
diag(y_1) <- 1

# 2015 ###
y_2 <- f  %>% 
  filter(year == 2014 | year == 2015)

y_2 <- y_2[, 4:25]
y_2 <- as.matrix(y_2)
y_2 <- crossprod(y_2)
diag(y_2) <- 2


# 2016 ###
y_3 <- f  %>% 
  filter(year == 2014 | year == 2015 | year == 2016)

y_3 <- y_3[, 4:25]
y_3 <- as.matrix(y_3)
y_3 <- crossprod(y_3)
diag(y_3) <- 2

# 2017 ###
y_4 <- f  %>% 
  filter(year == 2014 | year == 2015 | year == 2016 | year == 2017)

y_4 <- y_4[, 4:25]
y_4 <- as.matrix(y_4)
y_4 <- crossprod(y_4)
diag(y_4) <- 2

# 2018 ##
y_5 <- f
y_5 <- y_5[, 4:25]
y_5 <- as.matrix(y_5)
y_5 <- crossprod(y_5)
diag(y_5) <- 2

# Make Israel, Ireland and Hungary a bit bigger so they ar enot so squeezed
diag(y_5[10:12, 10:12]) <- 4

# Make the final result stay longer - add additional 100 frames to the animation
y_6 <- y_5



# Let the chording begin !! - separate chords year by year ##########

# 2014
chordDiagramFromMatrix(y_1, transparency = 0.5, annotationTrack = c('grid'), 
                       symmetric = TRUE, grid.col = node_col,
                       col = colours, keep.diagonal = TRUE, preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
              niceFacing = TRUE, adj = c(0, 0.5), cex = 1.6)}, bg.border = NA)
circos.clear()



# 2015
chordDiagramFromMatrix(y_2, transparency = 0.5, annotationTrack = c('grid'), 
                       symmetric = FALSE, grid.col = node_col,
                       col = colours, keep.diagonal = TRUE, preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
              niceFacing = TRUE, adj = c(0, 0.5), cex = 1.6)}, bg.border = NA)
circos.clear()

# 2016
chordDiagramFromMatrix(y_3, transparency = 0.5, annotationTrack = c('grid'), 
                       symmetric = TRUE, grid.col = node_col,
                       col = colours, keep.diagonal = TRUE, preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
              niceFacing = TRUE, adj = c(0, 0.5), cex = 1.6)}, bg.border = NA)
circos.clear()

# 2017
chordDiagramFromMatrix(y_4, transparency = 0.5, annotationTrack = c('grid'), 
                       symmetric = TRUE, grid.col = node_col,
                       col = colours, keep.diagonal = TRUE, preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
              niceFacing = TRUE, adj = c(0, 0.5), cex = 1.6)}, bg.border = NA)
circos.clear()


# 2018
chordDiagramFromMatrix(y_5, transparency = 0.5, annotationTrack = c('grid'), 
                       symmetric = TRUE, grid.col = node_col,
                       col = colours, keep.diagonal = TRUE, preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
              niceFacing = TRUE, adj = c(0, 0.5), cex = 1.6)}, bg.border = NA)
circos.clear()

# Make the final chart interactive
library(chorddiag)

chorddiag(y_5_int, groupColors = node_col, groupnamePadding = 20, showTicks = FALSE, 
          showTooltips = FALSE, chordedgeColor = node_col)



# Animation - data preparation #########

library(reshape2)

# delete the lower triangular part of the matrices
y_1[lower.tri(y_1)] <- 0

y1a <- y_1 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2014)

y_2[lower.tri(y_2)] <- 0

y2a <- y_2 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2015)

y_3[lower.tri(y_3)] <- 0
y3a <- y_3 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2016)

y_4[lower.tri(y_4)] <- 0
y4a <- y_4 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2017)

y_5[lower.tri(y_5)] <- 0
y5a <- y_5 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2018)

# add additional data to make the final state of the animation last longer
y_6[lower.tri(y_6)] <- 0
y6a <- y_5 %>%
  melt %>%
  rename(n1 = Var1, n2 = Var2) %>%
  mutate(year = 2019)

# bind it together
y <- bind_rows(y1a, y2a, y3a, y4a, y5a,  y6a)
  
# interpolate the data into 100 frames
ya <- y %>%
    mutate(link = paste(n1, n2, sep = " -> ")) %>%
    select(link, year, value) %>%
    mutate(ease = "linear") %>%
    tween_elements(time = "year", group = "link", ease = "ease", nframes = 100) %>%
    tbl_df()
  
ya <- ya %>%
  separate(col = .group, into = c("n1", "n2"), sep = " -> ") %>%
  select(n1, n2, value, everything())



# Create individual plots ########

library(circlize)

for(frm in unique(ya$.frame)){
  
  # open a PNG plotting device
  png(file = paste0("./plot/globalchord", frm, ".png"), height = 7.4, width = 7.4, 
      units = "in", res = 500)
  
  # intialise the circos plot
  circos.clear()
  par(mar = rep(0, 4), cex = 1)
  circos.par(start.degree = 90, track.margin = c(-0.1, 0.1), 
             points.overflow.warning = FALSE)
  
  # select the data by frame
  x <- ya %>%
    filter(.frame == frm) %>%
    select(n1, n2, value)
  
  # plot the chord diagram
  chordDiagram(x, directional = 2, 
               order = names(f)[4:25], col = as.vector(colours),
               grid.col = node_col, annotationTrack = "grid",
               transparency = 0.5,  annotationTrackHeight = c(0.05, 0.1),
               link.largest.ontop = TRUE, preAllocateTracks = 1)
  
  # add labels and axis
  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", 
                niceFacing = TRUE, adj = c(0, 0.5), cex = 1.4)}, bg.border = NA)
  
  # close plotting device
  dev.off()
}

# Put the individual plots together into an animated gif ####

library(magick)

img <- image_read(path = "./plot/globalchord0.png")

for(frm in unique(ya$.frame)[-1]){
  img0 <- image_read(path = paste0("./plot/globalchord", frm, ".png"))
  img <- c(img, img0)
  message(frm)
}

img1 <- image_scale(image = img, geometry = "820x820")

ani0 <- image_animate(image = img1, fps = 10)
image_write(image = ani0, path = "./plot/globalchord.gif")




