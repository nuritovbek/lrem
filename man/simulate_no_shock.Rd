% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{simulate_no_shock}
\alias{simulate_no_shock}
\title{Simulation for an LRE without exogenous shocks. Takes either the autonomous case or the AR case specified g and h.}
\usage{
simulate_no_shock(g, h, x0, t)
}
\arguments{
\item{g}{Decision rule}

\item{h}{Motion rule}

\item{x0}{vector of predetermined variables}

\item{t}{length of the simulation}
}
\value{
A matrix showing the dynamic paths of the variables in the specification
}
\description{
Simulation for an LRE without exogenous shocks. Takes either the autonomous case or the AR case specified g and h.
}
