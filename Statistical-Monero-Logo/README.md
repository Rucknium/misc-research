# Statistical Monero Logo
These files create an animated gif that visually builds a Monero logo from random draws of a probability density function.

The probability density function was constructed from the coordinates embedded in this SVG file:

https://raw.githubusercontent.com/fluffypony/monero-logo-artefacts/master/Logo Subsequent%20Tweaks/monero%20file.svg

To create the gif, first run data-construction.R . This may take days to complete since over 50 million observations have to be drawn from a cumulative distribution function that is created through numerical integration. Then run gif-construction.R . For best results, it is recommended to install Cairo, which is software separate from R. Read gif-construction.R for more details.

The code itself is available under the MIT open source license. However, since the resultant gif is based on an SVG that is released under the CC BY-SA 4.0 license (the [Creative Commons Attribution-ShareAlike 4.0 International license](https://creativecommons.org/licenses/by-sa/4.0/)), it is probably the case that the gif inherits the specified CC license. This means that you can copy and redistribute the material in any medium or format, and remix, transform, and build upon the material for any purpose, even commercially. However, when doing so you must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any reasonable manner, but not in any way that suggests The Monero Project endorses you or your use.

