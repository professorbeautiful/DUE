\name{DUEenvironmentDefault}
\alias{DUEenvironmentDefault}
\docType{data}
\title{
Default DUEenv
}
\description{
DUEenvironmentDefault is the default environment from which
configuration values are obtained.
}
\usage{data(DUEenvironmentDefault)}
\format{
  The format is:
<environment: 0x7fece64ece00> 
}
\details{
You will be working with settings in DUEenv within .GlobalEnv,
which is initially set equal to DUEenvironmentDefault.
As you work with the program,
values will be saved in DUEenv.
Next time you load the library, DUEenv will be
detected and not overwritten.
You can, of course, save multiple versions of DUEenv,
but get("DUEenv") will be the one used.
}

\seealso{\link{loadDUEconfig}}

\examples{
data(DUEenvironmentDefault)
ls(env=DUEenvironmentDefault) 
### Lots of them!!
}
\keyword{datasets}
