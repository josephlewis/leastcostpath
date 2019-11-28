## ----libraries, echo = TRUE, message= FALSE, warning= FALSE--------------
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gdistance)
library(leastcostpath)

## ----slope_cs, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))

plot(r)

cs <- create_slope_cs(dem = r, cost_function = 'tobler', neighbours = 16)

## ----lcp, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----

loc1 = cbind(2667876, 6479424)
loc1 = sp::SpatialPoints(loc1)

loc2 = cbind(2667677, 6478737)
loc2 = sp::SpatialPoints(loc2)

lcp <- create_lcp(cost_surface = cs, origin = loc1, destination = loc2, directional = FALSE)

plot(raster(cs))
plot(lcp[[1]], add = T, col = "red")
plot(lcp[[2]], add = T, col = "blue")

## ----libraries_2, echo = TRUE, message= FALSE, warning= FALSE------------
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gdistance)
library(leastcostpath)

## ----slope_traverse_cs, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))

plot(r)

cs <- create_slope_cs(dem = r, cost_function = 'tobler', neighbours = 16) %>%
  "*" (create_traversal_cs(dem = r, neighbours = 16))

## ----lcp_2, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----

loc1 = cbind(2667876, 6479424)
loc1 = sp::SpatialPoints(loc1)

loc2 = cbind(2667677, 6478737)
loc2 = sp::SpatialPoints(loc2)

lcp <- create_lcp(cost_surface = cs, origin = loc1, destination = loc2, directional = FALSE)

plot(raster(cs))
plot(lcp[[1]], add = T, col = "red")
plot(lcp[[2]], add = T, col = "blue")

## ----libraries_3, echo = TRUE, message= FALSE, warning= FALSE------------
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gdistance)
library(leastcostpath)

## ----slope_traverse_feature_cs, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))

plot(r)

feature_loc = cbind(2667652, 6478997)
feature_loc = sp::SpatialPoints(feature_loc)

cs <- create_slope_cs(dem = r, cost_function = 'tobler', neighbours = 16) %>%
  "*" (create_traversal_cs(dem = r, neighbours = 16)) %>%
  "*" (create_feature_attraction(dem = r, locs = feature_loc, max_attraction = 5000, distance = 200))

## ----lcp_3, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----

loc1 = cbind(2667876, 6479424)
loc1 = sp::SpatialPoints(loc1)

loc2 = cbind(2667677, 6478737)
loc2 = sp::SpatialPoints(loc2)

lcp <- create_lcp(cost_surface = cs, origin = loc1, destination = loc2, directional = FALSE)

plot(raster(cs))
plot(feature_loc, add = T, col = "black")
plot(lcp[[1]], add = T, col = "red")
plot(lcp[[2]], add = T, col = "blue")

## ----libraries_4, echo = TRUE, message= FALSE, warning= FALSE------------
library(rgdal)
library(rgeos)
library(sp)
library(raster)
library(gdistance)
library(leastcostpath)

## ----slope_traverse_feature_cc, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----
r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))

plot(r)

feature_loc = cbind(2667652, 6478997)
feature_loc = sp::SpatialPoints(feature_loc)

cs <- create_slope_cs(dem = r, cost_function = 'tobler', neighbours = 16) %>%
  "*" (create_traversal_cs(dem = r, neighbours = 16)) %>%
  "*" (create_feature_attraction(dem = r, locs = feature_loc, max_attraction = 100, distance = 50))

## ----cc, echo = TRUE,  fig.height = 6, fig.width = 6, warning = FALSE----

loc1 = cbind(2667876, 6479424)
loc1 = sp::SpatialPoints(loc1)

loc2 = cbind(2667677, 6478737)
loc2 = sp::SpatialPoints(loc2)

cc <- create_cost_corridor(cs, loc1, loc2)

plot(cc)

