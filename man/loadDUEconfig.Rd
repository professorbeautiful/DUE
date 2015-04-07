\name{loadDUEconfig}
\alias{loadDUEconfig}

\title{
Re-apply the configuration directives in DUEenv$DUEconfig.
}
\description{
If the user wants to rearrange the window's appearance, some control is available by changing values in the data frame DUEenv\$DUEconfig.  The purpose of this function is to change the values, then make the changes appear in the window.
}
\usage{
loadDUEconfig(objnames, fields = words("x y width height"), values = NULL)
}
\arguments{
  \item{objnames}{
Names of the objects whose values are to be changed. Obtained from rownames(DUEenv\$DUEconfig).
}
  \item{fields}{
Obtained from colnames(DUEenv\$DUEconfig).
}
  \item{values}{
A list or vector of the desired new values. 
}
}
\details{
The three arguments must have the same length.
}
\value{
No useful value is returned. DUEconfig is changed and reassigned inside DUEenv.
}

\author{
Roger Day (\email{day01@pitt.edu})
Meihua Wang (\email{meihua.wang@merck.com})
}
\note{
This is a clunky way to handle configuration.

The location and dimension parameters, which should be numbers, are actually strings, in deference to the way the tcltk package handles things.
Therefore the function add() is used to increase or decrease the number, as seen in the examples.
}

\examples{
data(DUEenvironmentDefault)
if(!exists("DUEenv")) DUEenv = DUEenvironmentDefault

colnames(DUEenv$DUEconfig)

rownames(DUEenv$DUEconfig)

\dontrun{
loadDUEconfig("label.utilitychoice", "x", add(DUEenv$DUEconfig["label.utilitychoice", "x"],  -5))
}
}
