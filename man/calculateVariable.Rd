% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_modifyDatasets.r
\name{calculateVariable}
\alias{calculateVariable}
\title{Calculate Variable}
\usage{
calculateVariable(dataset, cexpr = expression(""), name = NULL, type = "c")
}
\arguments{
\item{dataset}{The standard dataset as produced by \code{\link{gfd}}.}

\item{cexpr}{An arbitrary R expression describing how to calculate the 
values for the new variable. The expression will be evaluated using 'with', 
with the local environment being a single row of the provided dataset.}

\item{name}{Character length one. The name of the new variable.}

\item{type}{Character length one. The type of the new variable. Possible values 
are 'c' for class-variables and 'n' for numeric variables. Defaults to '"c"'.}
}
\value{
An object of class 'aquap_data' with a new variable in the header.
}
\description{
Calculate values for a new variable.
}
\details{
Similarly as \code{\link{combineVariable}} is simply combining , i.e. 
pasting together all the values of the selected variables from a single 
observation (within one row), \code{calculateVariable} is using variables of 
one observation (within one row) to calculate a new value according to the 
expression provided in the argument 'cexpr'. The expression can contain logical 
clauses etc, and it pertains exclusively to the values within a single row. 
Typically, the expression could contain an if clause followed by two values,
See examples.
New variables of type 'numeric' can obviously only be calculated from numerical 
variables, but the numeric result of such a calculation can be used as class 
variable by setting the type of the new variable to 'class' via providing 'c' 
to the argument 'type'.
The name of the new variable and the expression used to calculate it is stored 
in a list in the slot named 'calcVarInfo'. (\code{object@calcVarInfo})
}
\examples{
\dontrun{
fd <- gfd() # load a dataset
cexpr <- expression(Y_Foo + Y_Bar)
newFd <- calculateVariable(fd, cexpr, name="Addition")
# more examples of how to formulate the expression:
cexpr <- expression((Y_Foo * Y_Bar)/Y_Fuba)
cexpr <- expression(if (Y_Foo < 5 & C_Bar == "blabla") "Outcome1" else "Outcome2")
cexpr <- expression(if (C_Bar == "blabla" | C_Foo == "blibli") "Outcome1" else "Outcome2")
cexpr <- expression(as.numeric(paste0(Y_Foo, C_Bar))/Y_Foobar) # of course crazy, but
# it demonstrates that everything is possible in the expression. 
#
# use a numerical outcome as class variable:
newFd <- calculateVariable(fd, expression(Y_Foo - Y_Bar), name="Subtraction", type="c") 
newFd <- calculateVariable(fd, expression(Y_Foo - Y_Bar), name="Subtraction", type="n") 
# in that way you can color by the result of the calculation, and you 
# can also add the same variable as numerical variable
}
}
\seealso{
combineVariable

Other dataset modification functions: 
\code{\link{[,aquap_data-method}},
\code{\link{combineVariable}()},
\code{\link{generateMergeLabels,aquap_data,aquap_data,character,character-method}},
\code{\link{mergeDatasets,aquap_data,aquap_data,missing-method}}
}
\concept{dataset modification functions}
