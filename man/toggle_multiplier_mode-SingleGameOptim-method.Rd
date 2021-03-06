% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods-optimizer.R
\name{toggle_multiplier_mode,SingleGameOptim-method}
\alias{toggle_multiplier_mode,SingleGameOptim-method}
\title{Toggle Multiplier Mode}
\usage{
\S4method{toggle_multiplier_mode}{SingleGameOptim}(object)
}
\arguments{
\item{object}{An object of class Optimizer}
}
\description{
Toggle Multiplier Mode
}
\details{
Toggles the Multiplier mode (where a player's salary and fpts are multiplied by a given value, usually 1.5)
    for single-game / showdown contest types.  For Classic contest_types, this function has no effect.
}
