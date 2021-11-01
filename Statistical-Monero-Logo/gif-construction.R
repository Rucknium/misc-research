# MIT License

# Copyright (c) 2021 Rucknium

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


# install.packages("animation")
# install.packages("plotrix")
# install.packages("Cairo")
# NOTE: Cairo is a general software package that cannot be installed by R alone.
# Search for installation instructions according to your operating system.

library(animation)
library(plotrix)
library(Cairo)

load("logo-data.Rdata", verbose = TRUE)
# max(M.outline$y) == 1.58044863212111
# xmr.orange == "#FF6600FF"
# xmr.grey == "#4C4C4CFF"

load("generated-logo-observations.Rdata", verbose = TRUE)
# generated.logo.observations

# If Cairo in installed, use:
# ani.options(interval = 0.2, ani.dev = "CairoPNG")
# Otherwise, use:
ani.options(interval = 0.2)

saveGIF({
  
  y <- generated.logo.observations[1:10]

  for ( i in 3:73) {
    
    y <- c(y, generated.logo.observations[length(y) + (1:floor(2^(i/3-1))) ] )
    
    par(mar = c(5, 0, 0, 0))
    
    plot(0, 0, xlim = c(0 - 0.1, 1 + 0.1), ylim = c(0 , max(M.outline$y) * 2 ),
      axes = FALSE,
      ylab = "", xlab = "", main = "", col = "transparent",
      asp = (1.2)/(max(M.outline$y) * 2))
    
    mtext("Secure. Private. Untraceable.", side = 1, line = 0.5, cex = 2)
    mtext("Resistant to statistical attack.", side = 1, line = 2.5, cex = 2)
    mtext(paste0("N = ", prettyNum(length(y), big.mark = ",")), side = 1, line = 3.9, cex = 1)
    
    center_x = mean(c(0, 1))
    center_y = max(M.outline$y)
    
    plotrix::draw.ellipse(center_x, center_y,
      0.6, max(M.outline$y), 
      col = xmr.orange, border = NA)
    
    M.density <- density(y, bw = "SJ-dpi")
    
    polygon((c(M.density$x, M.density$x) * 1.5 - .5/2), 
      c(M.density$y, M.density$y * 1.5), border = NA, col = "white")
    
    hist(y, breaks = 50, col = xmr.grey, 
      axes = FALSE, border = NA,
      xlim = c(0, 1), ylim = c(0, max(M.outline$y) * 2),
      freq = FALSE,
      ylab = "", xlab = "", main = "", add = TRUE)
    
    
    # Pieced together from source of plotrix::draw.ellipse
    draw1ellipse <- function(x, y, a = 1, b = 1, angle = 0, segment = NULL, 
      arc.only = TRUE, nv = 100, deg = TRUE, border = NULL, 
      col = NA, lty = 1, lwd = 1, ...) {
      if (is.null(segment)) {
        if (deg) 
          segment <- c(0, 360)
        else segment <- c(0, 2 * pi)
      }
      if (deg) {
        angle <- angle * pi/180
        segment <- segment * pi/180
      }
      xyangle <- function(x, y, directed = FALSE, deg = TRUE) {
        if (missing(y)) {
          y <- x[, 2]
          x <- x[, 1]
        }
        out <- atan2(y, x)
        if (!directed) 
          out <- out%%pi
        if (deg) 
          out <- out * 180/pi
        out
      }
      z <- seq(segment[1], segment[2], length = nv + 1)
      xx <- a * cos(z)
      yy <- b * sin(z)
      alpha <- xyangle(xx, yy, directed = TRUE, deg = FALSE)
      rad <- sqrt(xx^2 + yy^2)
      xp <- rad * cos(alpha + angle) + x
      yp <- rad * sin(alpha + angle) + y
      if (!arc.only) {
        xp <- c(x, xp, x)
        yp <- c(y, yp, y)
      }
      data.frame(x = xp, y = yp)
    }
    
    
    eclipse <- draw1ellipse(center_x, center_y, 
      0.6, max(M.outline$y),
      segment = c(0, 180), angle = 180)
    
    eclipse <- rbind(data.frame(x = min(eclipse$x), y = -0.05),
      eclipse,
      data.frame(x = max(eclipse$x), y = 0))
    
    polygon(eclipse, col = "white", border = NA)
    
    print(paste(i, length(y)))
    
  }
  
}, movie.name = "Statistical-Monero-Logo.gif")
