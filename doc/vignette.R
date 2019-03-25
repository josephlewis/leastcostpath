## ----libraries, echo = TRUE, message= FALSE, warning= FALSE--------------
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gdistance)
library(leastcostpath)

## ----slope_cs, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))

r <- crop(r, extent(2667600, 2667700, 6479100, 6479200))
        
slope_cs <- create_slope_cs(r, cost_function = 'tobler')

plot(raster(slope_cs))
text(raster(slope_cs), digits = 2)

## ----slope_eg, echo = TRUE,  fig.height = 6, fig.width = 6---------------
plot(r, main = "Elevation (metres)")
text(r)

plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")

## ----slope, echo = TRUE,  fig.height = 6, fig.width = 6, warning= FALSE----
altDiff_slope <- function(x) {
    x[2] - x[1]
}
    
hd <- gdistance::transition(r, altDiff_slope, 8, symm = FALSE)

class(hd)

plot(raster(hd), main = "Altitudinal differences between neighbouring cells")
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(hd), digits = 2)

## ----slope_2, echo = TRUE,  fig.height = 6, fig.width = 6----------------
# This shows TransitionLayer values for column 45. 
transitionMatrix(hd)[,45]

# This shows the TransitionLayer value for row 44, column 45 - this will be the altitudinal difference between cell 45 (145 value) and cell 44 (142 value)
transitionMatrix(hd)[44,45]

cell_sum <- sum(transitionMatrix(hd)[,45])

cell_neighbours <- sum(transitionMatrix(hd)[,45] != 0)

round((cell_sum / cell_neighbours), digits = 2)

## ----slope_3, echo = TRUE,  fig.height = 6, fig.width = 6----------------
slope <- gdistance::geoCorrection(hd)

plot(raster(slope))
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(slope), digits = 2)

## ----slope_4, echo = TRUE,  fig.height = 6, fig.width = 6----------------
# This shows the TransitionLayer non-distance-corrected values for all neighbours of cell 45
transitionMatrix(hd)[,45][transitionMatrix(hd)[45,] != 0]

# This shows the TransitionLayer distance-corrected values for all neighbours of cell 45
transitionMatrix(slope)[,45][transitionMatrix(slope)[45,] != 0]

# TransitionLayer non-distance-corrected altitudinal difference value between cell 45 (center of red outline) and cell 34 (top left of red outline)
transitionMatrix(hd)[34,45]

# TransitionLayer distance-corrected altitudinal difference value between cell 45 (center of red outline) and cell 34 (top left of red outline)
transitionMatrix(slope)[34,45]

# TransitionLayer distance-corrected altitudinal difference values are calculated by square rooting the non-distance-corrected altitudinal difference value 
(transitionMatrix(hd)[34,45]) * (1 / res(r)[1]) / sqrt(2)

cell_sum <- sum(transitionMatrix(slope)[,45])

cell_neighbours <- sum(transitionMatrix(slope)[,45] != 0)

round((cell_sum / cell_neighbours), digits = 2)

## ----slope_5, echo = TRUE,  fig.height = 6, fig.width = 6----------------
adj <- raster::adjacent(r, cells = 1:raster::ncell(r), pairs = TRUE, directions = 16)

# cells that are adjacent to cell 45 (center of red outline)
sort(unique(adj[adj[,1] == 45]))

# transitionLayer values for adjacent cells to cell 45
transitionMatrix(slope)[sort(unique(adj[adj[,1] == 45])),45]

## ----tob, echo = TRUE,  fig.height = 6, fig.width = 6--------------------
sequence <- seq(-1, 1, 0.0001)

cs <- function(x) {  
  6 * exp(-3.5 * abs(x + 0.05))
}

df <- data.frame(sequence, cs(sequence))

plot(df$sequence, df$cs.sequence., main = "Tobler's Hiking Function", xlab = "Slope (m)", ylab = "Speed (km/h)")

## ----tob_1, echo = TRUE--------------------------------------------------
# Slope pre-cost function transitionLayer values for adjacent cells to cell 45
transitionMatrix(slope)[sort(unique(adj[adj[,1] == 45])),45]

slope[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))

# Slope post-cost function transitionLayer values for adjacent cells to cell 45
transitionMatrix(slope)[sort(unique(adj[adj[,1] == 45])),45]

## ----slope_6, echo = TRUE,  fig.height = 6, fig.width = 6----------------
Conductance <- geoCorrection(slope)

plot(raster(Conductance))
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(Conductance), digits = 2)

## ----mod_tob, echo = FALSE,  fig.height = 6, fig.width = 6---------------
sequence <- seq(-1, 1, 0.0001)

cs <- function(x) {  
  4.8 * exp(-5.3 * abs((x * 0.7) + 0.03))
}

df <- data.frame(sequence, cs(sequence))

plot(df$sequence, df$cs.sequence., main = "Modified  Hiking Function", xlab = "Slope (m)", ylab = "Speed (km/h)")

## ----wheeled, echo = FALSE,  fig.height = 6, fig.width = 6---------------
sequence <- seq(-1, 1, 0.0001)

cs <- function(x) {  
  1/(1 + (x * 100/12)^2)
}

df <- data.frame(sequence, cs(sequence))

plot(df$sequence, df$cs.sequence., main = "Wheeled Transport", xlab = "Slope (m)", ylab = "Speed (km/h)")

## ----polynomial, echo = FALSE,  fig.height = 6, fig.width = 6------------
sequence <- seq(-1, 1, 0.0001)

cs <- function(x) {  
  ((1337.8 * x^6) + (278.19 * x^5) - (517.39 * x^4) - (78.199 * x^3) + (93.419 * x^2) + (19.825 * x) + 1.64)
}

df <- data.frame(sequence, cs(sequence))

plot(df$sequence, df$cs.sequence., main = "Sixth-Degree Polynomial Function", xlab = "Slope (m)", ylab = "kJ / kg*m")

## ----tra, echo = TRUE,  fig.height = 6, fig.width = 6--------------------
aspect_dem <- raster::terrain(r, opt = "aspect", unit = "degrees", neighbors = 8)

## ----tra_1, echo = TRUE,  fig.height = 6, fig.width = 6------------------
plot(aspect_dem)
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(aspect_dem)

## ----traverse2, echo = TRUE,  fig.height = 6, fig.width = 6--------------
aspect_dem <- raster::calc(aspect_dem, function(x) {
  ifelse(x >= 180, x - 180, x)
  })

plot(aspect_dem)
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(aspect_dem)

## ----traverse3, echo = TRUE,  fig.height = 6, fig.width = 6--------------
aspect_dem <- raster::calc(aspect_dem, function(x) {
  ifelse(x >= 0 & x <= 90, x + 90, x - 90)
  })

plot(aspect_dem)
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(aspect_dem)

## ----traverse4, echo = TRUE,  fig.height = 6, fig.width = 6--------------
altDiff_traversal <- function(x) {
            if (abs(x[2] - x[1]) == 0) {
                1
            } else if (x[2] > x[1]) {
                if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
                  hrma <- abs(x[2] - x[1])
                  1 + (0.5/45) * hrma
                } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
                  hrma <- abs(x[2] - x[1])
                  2 - (0.5/45) * hrma
                } else {
                  1
                }
            } else if (x[2] < x[1]) {
                if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
                  hrma <- abs(x[2] - x[1])
                  1 - (0.5/45) * hrma
                } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
                  hrma <- abs(x[2] - x[1])
                  (0.5/45) * hrma
                  
                } else {
                  1
                }
            }
}

trans <- gdistance::transition(aspect_dem, altDiff_traversal, 16, symm = FALSE)

plot(aspect_dem)
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(aspect_dem)

plot(raster(trans))
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(trans), digits = 2)

## ----traverse5, echo = TRUE----------------------------------------------
# conductivity value when moving from cell 45 to cell 44 - i.e. the direction perpendicular to the slope is 3 degrees downhill. 
transitionMatrix(trans)[44,45]

# conductivity value when moving from cell 45 to cell 46 - i.e. the direction perpendicular to the slope is 4 degrees uphill 
transitionMatrix(trans)[46,45]

## ----open_raw, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
plot(r)
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(r)

## ----open, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
open_Diff <- function(x) {
    x[2] - x[1]
  }

kernel <- 7

kernel_m <- matrix(c(rep(1, kernel^2)), ncol = kernel, nrow = kernel, byrow = TRUE)

kernel_m
    
center <- ((kernel + 1) / 2)

kernel_m[center, center] <- 0

kernel_m

open_diff_ts <- gdistance::transition(r, open_Diff, kernel_m, symm = FALSE)
    
open_diff_ts <- gdistance::geoCorrection(open_diff_ts)

plot(raster(open_diff_ts))
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(open_diff_ts), digits = 2)

## ----open_1, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
# adjacencyFromTransition ensures that the calculations are restricted to adjacent cells only.
open_adj <- gdistance::adjacencyFromTransition(open_diff_ts)

transitionMatrix(open_diff_ts)[,45]

# Index of adjacent cells to cell 45
open_adj[,1][open_adj[,2] == 45]

# Elevation values difference from cell 45 to each neighbouring cell. 
transitionMatrix(open_diff_ts)[open_adj[,1][open_adj[,2] == 45], 45]

## ----open_2, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
open_diff_ts[open_adj] <- 90 - atan(open_diff_ts[open_adj])

# Openness values from cell 45 to each neighbouring cell. 
transitionMatrix(open_diff_ts)[open_adj[,1][open_adj[,2] == 45], 45]

## ----open_3, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
plot(raster(open_diff_ts))
plot(extent(rasterFromCells(r, c(34,35,36,44,45,46,54,55,56), values= FALSE)), add = T,  col = "red")
text(raster(open_diff_ts), digits = 1)

## ----combine, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
plot(raster(slope_cs * trans))
text(raster(slope_cs * trans), digits = 2)

plot(raster(slope_cs * trans + open_diff_ts))
text(raster(slope_cs * trans + open_diff_ts), digits = 1)

plot(raster(open_diff_ts - slope_cs))
text(raster(open_diff_ts - slope_cs), digits = 1)

final_cost <- slope_cs * trans * open_diff_ts

## ----lcp, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
loc1 = cbind(2667624, 6479176)
loc1 = sp::SpatialPoints(loc1)

loc2 = cbind(2667634, 6479175)
loc2 = sp::SpatialPoints(loc2)

sPath <- gdistance::shortestPath(final_cost, loc1, loc2, output = "SpatialLines")

plot(raster(final_cost))
plot(sPath, add = T, col = "red")
text(coordinates(loc1)[,1] - 1, coordinates(loc1)[,2] - 1, "A")
text(coordinates(loc2)[,1] + 1, coordinates(loc2)[,2]  -4, "B")

# costDistance is calculated by taking the reciprocal of the values in the transition matrix.
costDistance(final_cost, loc1, loc2)

transitionMatrix(final_cost)[,23]

1 / transitionMatrix(final_cost)[23, 24]

## ----lcp_net, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
sp_random <- spsample(as(extent(raster(final_cost)), "SpatialPolygons"), 5, "random")

plot(raster(final_cost))
plot(sp_random, add = T)

# In order to compute a network the SpatialPoints to join need to be determined
network <- expand.grid(seq_along(sp_random), seq_along(sp_random))

# Since SpatialPoints cannot be joined to itself, identical point indexes need to be removed
network <- network[network[,1] != network[,2], ]

head(network)

# The least cost path is generated for all connections, using the point indexes
lcp_network <- apply(network, MARGIN = 1, function(x) {
        gdistance::shortestPath(final_cost, sp_random[x[1], ], sp_random[x[2], ], output = "SpatialLines")
    })

lcp_network <- do.call(rbind, lcp_network)

plot(lcp_network, add = T, col = "red")

