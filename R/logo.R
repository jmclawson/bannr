# img <- png::readPNG("~/Downloads/bannr3.png")
# g <- grid::rasterGrob(img, interpolate=TRUE)
# 
# p <- ggplot2::qplot(1:10, 1:10, geom="blank") +
#   ggplot2::annotation_custom(
#     g,
#     xmin=-Inf, xmax=Inf,
#     ymin=-Inf, ymax=Inf) +
#   ggplot2::theme_void() +
#   hexSticker::theme_transparent()
# 
# hexSticker::sticker(
#   p,
#   s_width = .7,
#   s_height = .7,
#   s_x = 1,
#   s_y = 0.69,
#   package = "bannr",
#   p_color = "black",
#   p_y = 1.35,
#   p_size = 38,
#   h_fill = "white",
#   h_color = "black",
#   filename = "man/figures/logo.png")
# 
# usethis::use_logo("man/figures/logo.png")
