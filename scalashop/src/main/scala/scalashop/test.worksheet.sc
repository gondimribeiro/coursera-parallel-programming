import scalashop._

val img = new Img(30, 30)
val radius = 2

img.update(0, 1, rgba(1, 1, 3, 4))
img.update(1, 0, rgba(2, 1, 3, 5))
img.update(1, 1, rgba(3, 1, 3, 6))

for (dx <- -radius to radius)
    for (dy <- -radius to radius) 
        println((dx, dy))
  