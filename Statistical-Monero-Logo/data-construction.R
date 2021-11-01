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


# install.packages("rsvg")
# install.packages("grImport2")
# install.packages("GoFKernel")

library(rsvg)
library(grImport2)
library(GoFKernel)

M.svg  <- grImport2::readPicture(rawToChar(rsvg::rsvg_svg("monero file.svg")))
# From https://raw.githubusercontent.com/fluffypony/monero-logo-artefacts/master/Logo Subsequent%20Tweaks/monero%20file.svg
# https://github.com/fluffypony/monero-logo-artefacts/tree/master/Logo%20Subsequent%20Tweaks

M.svg.grob <- grImport2::pictureGrob(M.svg)

M.outline <- data.frame(
  x = M.svg.grob$children[[1]]$children[[2]]$children[[2]]$x,
  y = M.svg.grob$children[[1]]$children[[2]]$children[[2]]$y)

M.outline$y <- (-1) * M.outline$y + max(abs(M.outline$y))

y.min <- min(M.outline$y)

M.outline <- M.outline[(-1) * (which.min(M.outline$x) + 1):(which.max(M.outline$x) - 1), ]

M.outline <- M.outline[(-1) * c(1, 9), ]

M.outline <- M.outline

M.outline$x <- M.outline$x - min(M.outline$x)
M.outline$x <- M.outline$x / max(M.outline$x)

M.outline$x[2] <- M.outline$x[2] - 1e-10
M.outline$x[5] <- M.outline$x[5] + 1e-10
# Must add very small offset so that the non-unique x's are not dropped

M.outline$y <- M.outline$y / integrate(approxfun(M.outline), 0, 1)$value

M.pdf <- approxfun(M.outline)

M.cdf  <- function(x) {
  f.vectorized <- Vectorize(function(y) {integrate(approxfun(M.outline), 0, y)$value})
  f.vectorized(x)
}

M.inverse.cdf <- Vectorize(GoFKernel::inverse(M.cdf, lower = 0, upper  = 1))

xmr.orange <- M.svg.grob$children[[1]]$children[[1]]$gp$fill
# "#FF6600FF"
xmr.grey <- M.svg.grob$children[[1]]$children[[2]]$gp$fill
# "#4C4C4CFF"

save(M.outline, xmr.orange, xmr.grey, file = "logo-data.Rdata")


set.seed(314)

generated.logo.observations <- M.inverse.cdf(runif(10))

# Running this loop may take days since numerical integration is slow.
for ( i in 3:48) {
  
  generated.logo.observations <- 
    c(generated.logo.observations, M.inverse.cdf(runif(floor(2^(i/2)))) )
  
  save(generated.logo.observations, file = "generated-logo-observations.Rdata")
  # When i = 48, y will be length 57280991
  
  print(paste(i, length(generated.logo.observations) )
}


