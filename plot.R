minimum <- 1
maximum <- 100
shape_width <- 6

# function to create and update selection axis
select_line <- function(yesterday, tomorrow) {
  values <- c(seq(from=minimum, to=maximum, by=1))
  
  line <- plot(0, xlim=c(minimum, maximum), axes=FALSE, type="n", xlab="", ylab="") + points(50, -1)
  axis(1, at=values, lwd=shape_width, lwd.tick=0, labels=FALSE)
  if (yesterday) {
    line <- line + points(40, -1)
  } 
  if (tomorrow) {
    line <- line + points(60, -1)
  }
  
  line
}
