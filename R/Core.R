Show_me_your_forest <- function(X, Y, Z, shape, Crown_height_m, DBH_mm, Total_tree_height, Crown_projection_m2, solid, colour_trunk, colour_crown, filename) {

require(rgl)
require(akima)
require(tree3d)

  # Creating the terrain surface
  rgl::open3d()
  a_mat <- akima::interp(X, Y, Z)
  rgl::surface3d(x = a_mat$x, y = a_mat$z, z = a_mat$y, col = "#D5CA8B", alpha = 1)
  rgl::view3d(theta = 90)

  # Looingp through and displaying each tree
  for (i in 1:length(X)) {
    my_tree_trunk <- tree_mesh(
      shape[i],
      position = c(X[i], Z[i], Y[i]),
      trunk_height = Crown_height_m[i],
      trunk_width = DBH_mm[i] / 1000,
      crown_height = 0,
      crown_width = 0,
      solid = solid[i],
      ambient_intensity = 0.3,
      crown_color = colour_trunk[i]
    )

    write_tree_to_obj(my_tree_trunk, "my_tree_trunk", materials = TRUE, fileext = ".obj")
    my_tree_obj <-suppressWarnings(readOBJ("my_tree_trunk.obj"))
    shade3d(my_tree_obj, col = colour_trunk[i])

    my_tree_crown <- tree_mesh(
      shape[i],
      position = c(X[i], Z[i], Y[i]),
      trunk_height  = Crown_height_m[i],
      trunk_width = 0,
      crown_height = Total_tree_height[i] - Crown_height_m[i],
      crown_width = 2 * sqrt(Crown_projection_m2[i] / pi),
      solid = solid[i],
      ambient_intensity = 0.3,
      crown_color = colour_crown[i]
    )

    write_tree_to_obj(my_tree_crown, "my_tree_crown", materials = TRUE, fileext = ".obj")
    my_tree_obj2 <-suppressWarnings(readOBJ("my_tree_crown.obj"))
    shade3d(my_tree_obj2, col = colour_crown[i])
    unlink("my_tree_crown.obj")
    unlink("my_tree_crown.mtl")
    unlink("my_tree_trunk.obj")
    unlink("my_tree_trunk.mtl")
  }
  htmlwidgets::saveWidget(rgl::rglwidget(width = 520, height = 520),
                          file = paste0(filename, ".html"),
                          libdir = "libs",
                          selfcontained = FALSE)
}




