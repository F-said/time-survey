minimum <- 1
maximum <- 100
y_const <- 0

shape_width <- 4
shape <- 16

x_vals <- c(seq(from=minimum, to=maximum, by=1))
y_vals <- c(rep(y_const, times=100))

color_vec <- dict()
color_vec$set("vec1", c("blue", "red", "green", "pink"))$
  set("vec2", c("brown", "orange", "gray", "green"))$
  set("vec3", c("red", "blue", "brown", "green"))$
  set("vec4", c("blue", "red", "gray", "pink"))

line <- function(time) {
  handle <- plot(x_vals, y_vals, axes=FALSE, type="l", lwd=shape_width, xlab="", ylab="")
  if (time=="yesterday") {
    handle <- handle + 
      points(35, y_const, cex=shape_width/2, pch=16)
  }
  if (time=="tomorrow") {
    handle <- handle + 
      points(35, y_const, cex=shape_width/2, pch=16) +
      points(65, y_const, cex=shape_width/2, pch=shape)
  }
  if (time=="today") {
    handle <- handle + 
      points(35, y_const, cex=shape_width/2, pch=16) +
      points(65, y_const, cex=shape_width/2, pch=shape) +
      points(50, y_const, cex=shape_width/2, pch=shape)
  }
  handle
}

interact_line <- function(clicks, vec_label) {
  handle <- line("today")
  i <- 1
  for (click in clicks) {
    handle <- handle + points(clicks[i], y_const, col=color_vec$get(vec_label)[i], 
                              cex=shape_width/2, pch=shape)
    i <- (i + 1) %% 5
    # R, this wouldn't be necessary if your arrays started with 0
    if (i == 0) {
      i <- 1
    }
  }
  handle
}
