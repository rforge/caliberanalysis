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
template, either a character string for the filepath of a CSV (comma
separated values) file or a data.frame. The format is described in
'details' below.
}
  \item{output}{
Output file. If it has the extension .tex, it is output as LaTeX.
}
  \item{datatable}{
the data.table from which to construct the summary statistics. If not
supplied as an argument to the function, the name of the data.table is
taken from the second column of the first row of \code{template}.
}
  \item{latex}{
TRUE or FALSE, whether to produce output in LaTeX format.
}
  \item{sep}{
separator if output is a text file.
}
  \item{\dots}{
other arguments to pass to exportTable, e.g. \code{align}
}
}

\details{
The template must be in the following format:

\preformatted{
1,mydatatable,condition1,condition2 ...
1,,Group 1,Group 2 ...
0,Var1,Expr1,Expr2 ...
1,Var2,Expr1a,Expr2a ...
}

ROW 1, COLUMN 2: The name of the data.table from which the
summary statistics should be calculated. This is ignored if the
\code{datatable} argument is supplied.

ROW 1, COLUMNS 3+: Condition for defining a subset of 
rows of the data.table, from which the statistics for that column will be
derived. If blank, the entire data.table is used. If '<text>',
the column contains entirely text and is interpreted as is.

COLUMN 1, ROW 1: 1 if there is to be a horizontal line above the
table, 0 otherwise.

COLUMN 1, ROWS 2+: Each cell must contain 1 or 0, stating whether there is a
horizontal line below each row.

COLUMN 2, ROWS 2+: Either blank, or an expression for a variable on which
calculations can be performed in the body of the table. This may be a
variable name in the data.table or an expression evaluated within the
data.table. 

ROWS 2+, COLUMNS 3+: Each cell is interpreted as an R expression,
using the variable in column 2 evaluated within the data.table
for the subset of rows defined in row 1. 

For text to be interpreted as text, enclose it in single quotes
(double quotes will be discarded when importing the CSV file).

The \%&\% operator can be used to concatenate text.

A period (.) means that the function above is repeated.

The text 'MULTIXy contents', where X is a number betweeen 1 and 9 and y
is one of 'c' (centre), 'l' (left) or 'r' (right) puts the contents into
a multi-column cell spanning X columns aligned according to y.
This cell will span the relevant number of additional columns to the right,
which must be blank.

A function with an opening bracket but no closing bracket means that the
variable name in column 2 should be used as the argument.
}

\value{
If output = NULL, the summary table is returned as a character matrix.
}

\author{
Anoop Shah
}

\seealso{
\code{\link{exportTable}}
}

\examples{

DT <- data.table(a=1:5, b=c(1,1,1,2,2))
mytemplate <- rbind(c('1', 'DT', '<text>',  'b==1', 'b==2',  ''),
                    c('0', '',     '',  '"MULTI2c Group"','',''),
                    c('1', '',     '',      '"One"','"Two"', '"\\\\textbf{All}"'),
                    c('1', '',     'N obs', '.N',    '.N',   '.N'),
                    c('0', 'a',    'Mean a','mean(', 'mean(','mean('),
                    c('1', 'b',    'Mean b','.',     '.',    '.'))

# 'Group' will span two columns and will be centred

print(summaryTable(mytemplate))
#      [,1]     [,2]            [,3]  [,4]           
# [1,] ""       "MULTI2c Group" ""    ""             
# [2,] ""       "One"           "Two" "\\textbf{All}"
# [3,] "N obs"  "3"             "2"   "5"            
# [4,] "Mean a" "2"             "4.5" "3"            
# [5,] "Mean b" "1"             "2"   "1.4"          

cat(summaryTable(mytemplate, latex = TRUE, booktabs = TRUE, align = 'l'))
# % latex table generated in R 3.0.2 by xtable 1.7-1 package
# % Wed Nov 20 18:43:02 2013
# \begin{tabular}{llll}
#    \toprule
#  & \multicolumn{2}{c}{ Group }  &  \\ 
#  & One & Two & \textbf{All} \\ 
#    \midrule
# N obs & 3 & 2 & 5 \\ 
#    \midrule
# Mean a & 2 & 4.5 & 3 \\ 
# Mean b & 1 & 2 & 1.4 \\ 
#    \bottomrule
# \end{tabular}

}

