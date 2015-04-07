\name{DUEstart}
\alias{DUEstart}
\alias{DUErestart}
\alias{DUEstop}
\alias{DUEenv}
\title{
DUE starting and stopping
}
\description{
Functions to start, stop, and restart the Dose Utility Explorer window.
}
\usage{
DUEstart(inits, config)
DUErestart()
DUEstop()
}
\arguments{
  \item{inits}{
Initialization object if necessary.  By default, DUEenv.
}
  \item{config}{
Configuration object if necessary.  By default, DUEenv\$DUEconfig.
}
}
\details{
DUEstart() will start the interactive window. The values related to doses, the joint threshold distribution, utilities are read from DUEenv.  If DUEenv does not yet exist, then one is created by copying .DUEenvironment from the package. Having found an interesting set of values, one can copy DUEenv to another object name, for further study at a later time.

The configuration of the widgets (locations, dimensions, spacings etc) is read from DUEenv$DUEconfig. If the configuration is unsatisfactory, values can be changed, saved and restored as with the rest of DUEenv.

DUEstop() closes the DUE window, and DUErestart() is the same as DUEstop() followed by DUEstart().
}
\value{
No return value.  These functions are run for their graphical-window side effects.
}

\references{
This work was initially included in the doctoral dissertation of Meihua Wang at the University of Pittsburgh, 2009.
}
\author{
Roger Day (\email{day01@pitt.edu})
Meihua Wang (\email{meihua.wang@merck.com})
}

\seealso{
\code{\link{loadDUEconfig}}
}
\keyword{ Clinical decision analysis }
\keyword{ Bayesian expected utility }
