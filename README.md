# robmedShiny: Shiny apps for (robust) mediation analysis

This companion package to [`robmed`](https://github.com/aalfons/robmed) contains a shiny app to compare various bootstrap procedures for mediation analysis on simulated data.  Future plans include to provide a graphical user interface for the robust bootstrap test ROBMED ([Alfons, Ates & Groenen, 2021](https://doi.org/10.1177/1094428121999096)) to make the method more accessible to less proficient `R` users.


## About ROBMED

The robust bootstrap test ROBMED for mediation analysis is less sensitive to deviations from model assumptions (such as outliers or heavily tailed distributions) than the standard bootstrap test of Preacher & Hayes ([2004](http://dx.doi.org/10.3758/BF03206553), [2008](http://dx.doi.org/10.3758/BRM.40.3.879)).  ROBMED utilizes the robust MM-regression estimator ([Yohai, 1987](https://projecteuclid.org/euclid.aos/1176350366)) instead of the OLS estimator for regression, and runs bootstrap tests with the fast and robust bootstrap methodology ([Salibián-Barrera & Zamar, 2002](https://projecteuclid.org/euclid.aos/1021379865); [Salibián-Barrera & Van Aelst, 2008](https://doi.org/10.1016/j.csda.2008.05.007)).

More information can be found in our article:

Alfons, A., Ates, N.Y., & Groenen, P.J.F. (2021). A Robust Bootstrap Test for Mediation Analysis. *Organizational Research Methods*. DOI [10.1177/1094428121999096](https://doi.org/10.1177/1094428121999096).


## Installation

The latest (possibly unstable) development version in this repository can be installed from the `R` command line via

```
install.packages("devtools")
devtools::install_github("aalfons/robmedShiny")
```

If you already have package `devtools` installed, you can skip the first line.


## Report issues and request features

If you experience any bugs or issues or if you have any suggestions for additional features, please submit an issue via the [*Issues*](https://github.com/aalfons/robmedShiny/issues) tab of this repository.  Please have a look at existing issues first to see if your problem or feature request has already been discussed.


## Contribute to the package

If you want to contribute to the package, you can fork this repository and create a pull request after implementing the desired functionality.


## Ask for help

If you need help using the package, or if you are interested in collaborations related to this project, please get in touch with the [package maintainer](https://personal.eur.nl/alfons/).
