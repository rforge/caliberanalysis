\name{summaryTable}
\alias{summaryTable}

\title{
Create a publication summary table from a template
}

\description{
Frequently one wants to generate a summary table from a data.table (e.g. a cohort of patients). We may want different statistics reported for each variable, and we might want to include additional text and explanation. This may be easier to lay out in a spreadsheet rather than coding it directly in R. This function processes such as 'template' spreadsheet (saved as a .csv file)
}

\usage{
summaryTable(template, output = NULL, datatable = NULL,
     latex = FALSE, sep = NULL, ...)
}


\arguments{
  \item{template}{
Template, either a character string for the filepath of a CSV (comma separated values) file or a data.frame.
}
  \item{output}{
Output file. If it has the extension .tex, it is output as LaTeX.
}
  \item{datatable}{
The data.table from which to construct the summary statistics. If not supplied as an argument to the function, the name of the data.table can be the first row, 
}
  \item{latex}{
TRUE or FALSE, whether to produce output in LaTeX format.
}
  \item{sep}{
separator if output is a text file.
}
  \item{\dots}{
other arguments.
}
}

\details{
The first column states whether there is a horizontal line below each row.

The \%&\% operator can be used to concatenate text.
The condition '<text>' means that the column contains entirely text, which should be interpreted as is.

In the body of the template, the following convention is used:

A period (.) means that the function above is repeated.

A function with an opening bracket but no closing bracket means that the variable name in the second column of the row should be used as the argument.

Anything else is interpreted as an R expression. For text to be interpreted as text, enclose it in single quotes (double quotes will be discarded when importing the CSV file).

\preformatted{
1,mydatatable,condition1,condition2 ...
1,,Group 1,Group 2 ...
0,Var1,Expr1,Expr2 ...
1,Var2,Expr1a,Expr2a ...
}

}

\value{
If output = NULL, the summary table is returned as a character matrix.
}

\author{
Anoop Shah
}

\seealso{



}

\examples{
DT <- data.table(a=1:5, b=c(1,1,1,2,2))

mytemplate <- rbind(c('1', 'DT', '<text>', 'b==1',    'b==2',    ''),
                    c('1', '',     '',       '"Grp 1"', '"Grp 2"', '"All"'),
                    c('0', '',     'N obs',  '.N',      '.N',      '.N'),
                    c('1', 'a',    'Mean a', 'mean(',   'mean(',   'mean('))

summaryTable(mytemplate)

}

