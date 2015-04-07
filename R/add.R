# @title "Adding strings as numeric"
# \description{
#  Used for moving visual components, when customization of the screen is needed due to unusual window dimensions.
# }
# \arguments{
# \item{n1}{A string representing a number.}
# \item{n2}{A string representing a number.}
# }
# \value{
# @character n1+n2 represented as a string.
# }
# \seealso{loadDUEconfig}
#
# \author{Roger Day}
#*/###########################################################################
add <-
function(n1, n2)
	as.character(as.numeric(n1) + as.numeric(n2))

