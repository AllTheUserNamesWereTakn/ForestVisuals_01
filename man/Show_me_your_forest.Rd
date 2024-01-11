\name{ForestVisuals}
\alias{ForestVisuals}
\title{ForestVisuals, World!}
\usage{
Show_me_your_forest()
}
\description{
Produces rgl 3d object with the simplified reconstruction of your stand.
}
\examples{
require(ForestVisuals)
data("Sample_data")
Show_me_your_forest(
Show_me_your_forest(Sample_data$X, Sample_data$Y, Sample_data$Z, Sample_data$shape, Sample_data$Crown_height_m, Sample_data$DBH_mm, Sample_data$Total_tree_height, Sample_data$Crown_projection_m2, Sample_data$solid, Sample_data$colour, Sample_data$colour,  "3dscatter")
)

}
