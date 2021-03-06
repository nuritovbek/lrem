
<!-- README.md is generated from README.Rmd. Please edit that file -->
Solving and simulating a class of LRE models
============================================

This package solves for the recursive representation of the stable solution to a system of linear difference equations. The package contains formulas for dealing with both the autonomous LRE and LRE with an AR element and the associated exogenous shocks.

Specifically, included are: 1) The formulas that give the decision and motion rules for a dynamic system as per Blanchard and Kahn, Klein. With formulas lre\_auto for the autonomous case and lre\_ar for the AR case.

For the autonomous LRE, Inputs are two square matrices E and A and a natural number n where E and A are the coefficient matrices of the difference equation *E**x*<sub>*t* + 1</sub> = *A**x*<sub>*t*</sub>

For the AR LRE, inputs are the previously defined matrices E and A in addition to a matrix B and *Φ* that satisfy the following system

$Ex\_{t+1} = Ax\_t + Bu\_t \\\\ u\_{t+1} = \\Phi u\_t + e\_t$

1.  Simulation formulas that should also provide the impulse response of the system to an exogenous shock to examine the evolution of the dynamic system.

To install run:

``` r
devtools::install_github("nuritovbek/lrem")
```

To use the package, call the required lre function with the coefficient matrices of your model and a number of predetermined variables in n: e.g.

``` r
library(lrem)
sim <- lre_auto(A, E, n)
```

And then iterate the decision and motion rules by running

``` r
out <- simulate_no_shock(sim[[1]], sim[[2]], x0, t)
```
