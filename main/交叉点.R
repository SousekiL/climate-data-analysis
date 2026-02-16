# Create a polygon as an sf object
polygon <- st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))

# Create a line as an sf object
line <- st_linestring(matrix(c(-0.5, 1.5, 0.5, 2), ncol = 2))

intersection <- st_intersection(line, polygon)
intersection2 <- st_cast(intersection, "MULTIPOINT")
#intersection3 <- st_line_sample(intersection, sample = c(0,1))

coordinates(intersection)

plot(polygon, col = "white")
plot(line, col = "red",  add = TRUE)
plot(intersection, col = "blue",  add = TRUE)
plot(intersection2, col = "green",  add = TRUE)
#plot(intersection3, col = "yellow",  add = TRUE)
