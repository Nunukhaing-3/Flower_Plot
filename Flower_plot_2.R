library(plotrix)

flower_plot <- function(sample, value, start, a, b,  
                        ellipse_col = rgb(135, 206, 235, 150, max = 255), 
                        circle_col = rgb(0, 162, 214, max = 255),
                        circle_text_cex = 1, labels=labels) {
  par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type="n")
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    plotrix::draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                          y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                          col = ellipse_col,
                          border = ellipse_col,
                          a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         value[t]
    )
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = circle_text_cex
      )
      
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = circle_text_cex
      )
    }           
  })
  plotrix::draw.circle(x = 5, y = 5, r = 1.5, col = circle_col, border = circle_col)
  text(x = 4.7, y = 5, labels=labels)
}

#===================================================
flower_plot(c("WSM419", "A321", "M1", "M2", "M22", "M58", 
              "M102", "M161", "KH36b", "KH36c", "KH36d", "KH53a", "KH53b"),
            c(519, 556, 83, 62, 415, 425, 357, 441, 22, 41, 33, 44, 43), 90, 0.5, 2, labels="core")

#======================================================

flower_plot2 <- function(sample, value, start, a, b,  
                         ellipse_col = rgb(135, 206, 235, 150, max = 255), 
                         circle_col = rgb(0, 162, 214, max = 255),
                         circle_text_cex = 1, labels=labels) {
  par( bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1,1,1,1))
  plot(c(0,10),c(0,10),type="n")
  n   <- length(sample)
  deg <- 360 / n
  res <- lapply(1:n, function(t){
    ellipse_col <- ellipse_col[t]
    plotrix::draw.ellipse(x = 5 + cos((start + deg * (t - 1)) * pi / 180), 
                          y = 5 + sin((start + deg * (t - 1)) * pi / 180), 
                          col = ellipse_col,
                          border = ellipse_col,
                          a = a, b = b, angle = deg * (t - 1))
    text(x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
         y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
         value[t]
    )
    
    if (deg * (t - 1) < 180 && deg * (t - 1) > 0 ) {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) - start,
           adj = 1,
           cex = circle_text_cex
      )
      
    } else {
      text(x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
           y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
           sample[t],
           srt = deg * (t - 1) + start,
           adj = 0,
           cex = circle_text_cex
      )
    }           
  })
  plotrix::draw.circle(x = 5, y = 5, r = 1.5, col = circle_col, border = circle_col )
  
  # tune location by x and y.
  text(x = 4.7, y = 5, labels=labels)
}


flower_plot2 (c("WSM419\nAAA", "A321", "M1", "M2", "M22", "M58", 
                "M102", "M161", "KH36b", "KH36c", "KH36d", "KH53a", "KH53b"),
              c(519, 556, 83, 62, 415, 425, 357, 441, 22, 41, 33, 44, 43), 90, 0.5, 2, labels="core",
              ellipse_col = topo.colors(13, alpha = 0.3), 
              circle_col = topo.colors(1, alpha = 0.7) )

#=========================================================
#References: : https://www.biostars.org/p/144778/#145268
#             https://roweyerboat.github.io/drawing_flowers_with_r_and_ggplot2
