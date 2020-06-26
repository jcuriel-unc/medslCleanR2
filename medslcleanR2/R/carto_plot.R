#' Create a dot plot choropleth cartogram 
#' 
#' This function takes a spatial object with a dataframe and overlays a weighted dot plot on top of the base map. For the choropleth color, it
#' reads in the values specified in the "color" column, which is produced in the map_breaks_calc function.  Additionally exports the transformed
#' spatial coordinates dataframe, which consists of the centroids from the base map. 
#' @param spat_obj The spatial object with a dataframe that will be transformed into a dot plot cartogram. 
#' @param pop_vec The dataframe column/vector that will provide the weights for the dot size. 
#' @param color_vec The color scheme dataframe column/vector that will be used for the choropleth map. Can be manually entered or easily 
#' computed using the map_breaks_calc function
#' @param weight_mod The modifier for the weight to make the dots appear uniformly larger or smaller. In the event of fewer/larger polygons,
#' this should be increased for aesthetic purposes. In the event of numerous small polygons, this should be decreased in size. Defaults to 5.
#' @param title The title for the map. Defaults to "". 
#' @param size_correct A TRUE or FALSE option to correct the size such that the discrepancies between dot sizes are smaller. 
#' Otherwise, the min-max normalized dot sizes are used. Defaults to FALSE such that very populous areas might dominate the map. 
#' @return The plotted dot cartogram map, in addition to the point spatial dataframe with the fields necessary to reproduce the map 
#' independently of the function.  
#' }
#' 
#' @export
#' @examples 
#' state_obj <- readOGR(wd_shp, "counties") # reading in the state shpfile 
#' state_obj <- subset(state_obj, STATEFP=="39") #subset by fip code, which you should have memorized for beer trivia purposes 
#' ##read in the pop data from the acs 
#' acs_counties <- read.csv("county_acs_demos.csv")
#' acs_counties$Geo_FIPS <- str_pad(acs_counties$Geo_FIPS, width = 5,pad="0",side="left")
#' state_obj <- merge(state_obj, acs_counties, by.x="GEOID",by.y="Geo_FIPS")
#' state_obj <- map_breaks_calc(state_obj, state_obj$white_pct,"purples")
#' round(getJenksBreaks(state_obj2$white_pct,5),2) # grabbing the values for the breaks 
#' carto_plot(state_obj2,state_obj2$total_pop,state_obj2$color,weight_mod = 7, size_correct = TRUE)
#' #add legend after as appropriate 
#' legend("bottomleft", fill=medsl_purples,legend = c("< 59.22%", "59.22 -< 71.05%", "71.05 -< 84.67%", "84.67 -< 91.59%", "91.59% +"), 
#'       title=" ",
#'       bty="n", horiz=FALSE, cex=0.8, ncol=1)
#' 
carto_plot <- function(spat_obj, pop_vec, color_vec, weight_mod = 5, title="",size_correct=FALSE){
  spat_obj <- spTransform(spat_obj, CRS=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  spat_coords <- as.data.frame(coordinates(spat_obj))
  spat_coords <- cbind(spat_coords, pop_vec)
  colnames(spat_coords)[3] <- "pop_field"
  norm1 <- as.data.frame(apply(as.data.frame(spat_coords$pop_field), 
                               2, function(x) (x - min(x))/(max(x) - min(x))))
  norm1 <- norm1*weight_mod
  spat_coords <- cbind(spat_coords,norm1)
  colnames(spat_coords)[4] <- "weight"
  spat_coords$weight[spat_coords$weight==0] <- weight_mod*1e-04
  spat_coords <- cbind(spat_coords, color_vec)
  colnames(spat_coords)[5] <- "color"
  quant_weights <- quantile(spat_coords$weight, seq(0,1,by=0.05))
  spat_coords$weight2 <- 0.05
  spat_coords$weight2[spat_coords$weight >= quant_weights[2] & spat_coords$weight < quant_weights[5] ] <- 0.1
  spat_coords$weight2[spat_coords$weight >= quant_weights[5] & spat_coords$weight < quant_weights[7] ] <- 0.15
  spat_coords$weight2[spat_coords$weight >= quant_weights[7] & spat_coords$weight < quant_weights[9] ] <- 0.2
  spat_coords$weight2[spat_coords$weight >= quant_weights[9] & spat_coords$weight < quant_weights[11] ] <- 0.25
  spat_coords$weight2[spat_coords$weight >= quant_weights[11] & spat_coords$weight < quant_weights[13] ] <- 0.3
  spat_coords$weight2[spat_coords$weight >= quant_weights[13] & spat_coords$weight < quant_weights[15] ] <- 0.35
  spat_coords$weight2[spat_coords$weight >= quant_weights[15] & spat_coords$weight < quant_weights[17] ] <- 0.4
  spat_coords$weight2[spat_coords$weight >= quant_weights[17] & spat_coords$weight < quant_weights[19] ] <- 0.45
  spat_coords$weight2[spat_coords$weight >= quant_weights[19]] <- 0.5
  spat_coords$weight2 <- spat_coords$weight2*weight_mod
  
  spat_coords1 <- subset(spat_coords, select=c(V1,V2))
  cents_coords <- SpatialPointsDataFrame(coords = spat_coords1, data = spat_coords,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  plot(spat_obj)
  if(size_correct==TRUE){
    points(cents_coords$V1,cents_coords$V2, add=TRUE,pch=21,bg=cents_coords$color,cex=cents_coords$weight2)
  }else{
    points(cents_coords$V1,cents_coords$V2, add=TRUE,pch=21,bg=cents_coords$color,cex=cents_coords$weight)
  }
  return(spat_coords)
}