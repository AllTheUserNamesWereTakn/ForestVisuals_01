\name{ForestVisuals}
\alias{ForestVisuals}
\title{ForestVisuals}
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

\arguments{
  \item{X, Y, Z}{Numeric vectors representing the x, y, and z coordinates in the local coordinate system for the tree positions.}
  \item{shape}{A vector indicating the shape of the trees. Full list of options: "columnar" "pyramidal1" "pyramidal2" "oval" "palm" "rounded" "spreading1" "spreading2" "vase" "weeping". For more information, see the tree3d package: \href{https://cran.r-project.org/web/packages/tree3d/tree3d.pdf}{tree3d package documentation}}
  \item{solid}{A logical vector (TRUE/FALSE) indicating whether the tree crown should be solid.For more information, see the tree3d package: \href{https://cran.r-project.org/web/packages/tree3d/tree3d.pdf}{tree3d package documentation}}
  \item{colour_trunk}{A vector of colors specifying the color of each tree trunk.}
  \item{colour_crown}{A vector of colors specifying the color of each tree crown.}
  \item{DBH_mm}{A numeric vector representing the diameter at breast height (DBH) in millimeters for each tree.}
  \item{Total_tree_height}{A numeric vector specifying the total height in meters of each tree.}
  \item{Crown_height_m}{A numeric vector specifying the height of the tree crown base in meters for each tree.}
  \item{Crown_projection_m2}{A numeric vector specifying the area of the crown projection (in square meters).}
  \item{filename}{A character string for the name of the output file (without extension) for saving the 3D plot.}
}

}
