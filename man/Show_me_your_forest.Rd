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
Show_me_your_forest(X = Sample_data$X, Y = Sample_data$Y, Z= Sample_data$Z,
                                shape = Sample_data$shape, solid = Sample_data$solid,
                                colour_trunk = Sample_data$colour, colour_crown = Sample_data$colour,
                                DBH_mm = Sample_data$DBH_mm, Total_tree_height = Sample_data$Total_tree_height,
                                Crown_height_m = Sample_data$Crown_height_m,  Crown_projection_m2 = Sample_data$Crown_projection_m2,
                                filename = "3dscatter")

}
