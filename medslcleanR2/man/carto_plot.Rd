% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carto_plot.R
\name{carto_plot}
\alias{carto_plot}
\title{Create a dot plot choropleth cartogram}
\usage{
carto_plot(
  spat_obj,
  pop_vec,
  color_vec,
  weight_mod = 5,
  title = "",
  size_correct = FALSE
)
}
\arguments{
\item{spat_obj}{The spatial object with a dataframe that will be transformed into a dot plot cartogram.}

\item{pop_vec}{The dataframe column/vector that will provide the weights for the dot size.}

\item{color_vec}{The color scheme dataframe column/vector that will be used for the choropleth map. Can be manually entered or easily 
computed using the map_breaks_calc function}

\item{weight_mod}{The modifier for the weight to make the dots appear uniformly larger or smaller. In the event of fewer/larger polygons,
this should be increased for aesthetic purposes. In the event of numerous small polygons, this should be decreased in size. Defaults to 5.}

\item{title}{The title for the map. Defaults to "".}

\item{size_correct}{A TRUE or FALSE option to correct the size such that the discrepancies between dot sizes are smaller. 
Otherwise, the min-max normalized dot sizes are used. Defaults to FALSE such that very populous areas might dominate the map.}
}
\value{

}
\description{
This function takes a spatial object with a dataframe and overlays a weighted dot plot on top of the base map. For the choropleth color, it
reads in the values specified in the "color" column, which is produced in the map_breaks_calc function.  Additionally exports the transformed
spatial coordinates dataframe, which consists of the centroids from the base map.
}
\examples{
state_obj <- readOGR(wd_shp, "counties") # reading in the state shpfile 
state_obj <- subset(state_obj, STATEFP=="39") #subset by fip code, which you should have memorized for beer trivia purposes 
##read in the pop data from the acs 
acs_counties <- read.csv("county_acs_demos.csv")
acs_counties$Geo_FIPS <- str_pad(acs_counties$Geo_FIPS, width = 5,pad="0",side="left")
state_obj <- merge(state_obj, acs_counties, by.x="GEOID",by.y="Geo_FIPS")
state_obj <- map_breaks_calc(state_obj, state_obj$white_pct,"purples")
round(getJenksBreaks(state_obj2$white_pct,5),2) # grabbing the values for the breaks 
carto_plot(state_obj2,state_obj2$total_pop,state_obj2$color,weight_mod = 7, size_correct = TRUE)
#add legend after as appropriate 
legend("bottomleft", fill=medsl_purples,legend = c("< 59.22\%", "59.22 -< 71.05\%", "71.05 -< 84.67\%", "84.67 -< 91.59\%", "91.59\% +"), 
      title=" ",
      bty="n", horiz=FALSE, cex=0.8, ncol=1)

}
