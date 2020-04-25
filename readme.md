# Tools for evaluating isotope mixing models

 CJ Brown 2018-04-16

I am no longer maintaining this package. So it is not up to date with developments in simmr and mixsiar. However, my package [`BayeSens`](https://github.com/cbrown5/BayeSens) can be used for the same purpose and a vignette is included with BayeSens explaining how to calculate information criteria. 



 This is supplementary material prepared in support of the paper:
 ["Quantifying learning in biotracer studies"
*Christopher J. Brown, Michael Brett, Fernanda Adame, Ben Stewart-Koster, Stuart Bunn* Oecologia. In press. ](https://www.researchgate.net/publication/324491011_Quantifying_learning_in_biotracer_studies)

Please contact chris.brown@griffith.edu.au or pull a request on this github project if you identify any issues.

To install this beta version package, open R and type:
`install.packages("devtools")`
Then you need my BayeSens package:
`devtools::install_github("cbrown5/BayeSens")`
Finally, you can install remixsiar:
`devtools::install_github("cbrown5/remixsiar")`

If `install_github` is causing you strife (like me, maybe its a windows 10 thing?), then try this:
- click clone or download (scroll up and you will see the green button)
- download the zip archive and unzip it.
- Open R
- Run ` install.packages("C:/Documents/remixsiar", repos = NULL, type = "source")` using whatever local path is appropriate.

To get help on using the package from within R:
`library("remixsiar")`
`vignette("remixsiar")`
