% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constraints.R
\name{constr_team_stack}
\alias{constr_team_stack}
\title{Adds Same-Team stacks}
\usage{
constr_team_stack(
  model,
  players,
  positions,
  opt_positions = NULL,
  nstacks = 1,
  within_lines = FALSE
)
}
\arguments{
\item{model}{Model object}

\item{players}{List of player objects}

\item{positions}{Positions for that should be stacked within a single team}

\item{opt_positions}{Optional positions -- used to add OR-level positions (e.g., stack of QB, WR & (TE | RB))}

\item{nstacks}{Number of stacks to try to include (Default is 1)}

\item{within_lines}{Logical.  Whether to generate stacks within lines (works for hockey and nothing else)}
}
\description{
Adds Same-Team stacks
}
\keyword{internal}
