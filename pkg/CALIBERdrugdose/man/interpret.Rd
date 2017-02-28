\name{interpret}
\alias{interpret}

\title{
Convert a Single Dosage Text to Structured Dosage Information
}

\description{
Extracts structured information such as quantity, frequency and units
from a dosage text. This function is intended to be called directly by advanced users. For 
general use, the function \code{\link{doseconvert}} is provided to
automatically load the dictionaries and analyse a single text or multiple texts.
The function \code{\link{testdoseconvert}} is provided to assist in testing the
algorithm, by analysing a list of texts and comparing the output with a gold
standard interpretation.
}

\usage{
interpret(instring, singlewords, multiwords, patterns,
    noisy = FALSE, simplify = FALSE, maxifchoice = TRUE, id = NULL)
}

\arguments{
  \item{instring}{
Dosage string to interpret.
}
  \item{singlewords}{
A \code{\link{drugdose_singlewords}} object.
}
  \item{multiwords}{
A \code{\link{drugdose_multiwords}} object.
}
  \item{patterns}{
A \code{\link{drugdose_patterns}} object.
}
  \item{noisy}{
Whether to print debug information to the console.
}
  \item{simplify}{
Whether multiple periods with different doses should be returned as separate
doses or combined into a single dose.
}
  \item{maxifchoice}{
Whether to return the maximum dose if there is a choice of dose (e.g.
1 or 2 tablets daily). If FALSE, the average is returned. 
}
  \item{id}{
Unique identifier for the text being interpreted.
}
}

\value{
Data frame with the following columns:

\item{qty}{dose quantity (numeric)}
\item{units}{dose units (character)}
\item{freq}{dose frequency per time interval (numeric)}
\item{tot}{total dose per time period (numeric)}
\item{max}{factor with 3 levels: max, average, exact}
\item{time}{time period in days (numeric)}
\item{change}{factor with 4 levels: first, second, nochange, combined.
If doses for different time periods are combined using \code{simplify} = TRUE,
\code{change} states which dose contributes to the output.}
\item{choice}{factor with 3 levels: nochoice, choice, asneeded}
\item{daily_dose}{interpreted daily dose (numeric; 0 = missing)}

If \code{simplify} = TRUE, the row.names are the \code{ids} if supplied, or equal to the row numbers otherwise.
If \code{simplify} = FALSE, the row.names are \code{ids}.X where X (=1, 2, 3 etc.) is the order of the partial
dose if \code{ids} are supplied, or equal to the row numbers otherwise.
}

\details{

The algorithm works as follows:

\enumerate{
\item Any text occurring after "Notes for patient" or "repeat details" is ignored, as the text after these words usually contains long instructions to the patient. Although there may be some useful dosage information in this section, there is so much additional superfluous information that it is more likely that errors will be introduced if an attempt is made to interpret this part of the text.
\item The remaining text is split into separate words
\item Each word is checked against the "singlewords" dictionary. If it is not in the dictionary, an attempt is made to split it into 2 or more words which are in the dictionary (e.g. "everyday" changes to "every day"). The dictionary corrects some common spelling mistakes (e.g. "wekkly" is converted to "weekly")
\item All words which are not in the singlewords dictionary are deleted. In addition, any words which are between two words of 2 or more letters which are not in the dictionary are deleted. This is to remove superfluous words from the end of long dosage instructions.
\item An attempt is made to calculate the average of numbers wherever there is a choice of dose, using the function "numbers_replace". For example, "1 times or 2 times" is replaced by "1.5 times" and the choice flag is set to "average"
\item Phrases are checked against the "first" dictionary, which is derived from the sheets "1abbrev", "2numbers", "3units", "4times" and "5uncertainty". This changes certain phrases to make more standardised phrases. For example "for a week" is converted to "for 7 days"
\item The function "numbers_replace" is applied again.
\item The function "analyse_dose" is called. This splits the dosage text into up to 10 parts at certain link words, such as "changeto" or "and". Each partial dose is then analysed using the "second" dictionary to derive the dosage information.
\item The function "combine_parts" then combines the parts of the dose depending on the link words. For example, if there were two dosages separated by "and", such as "1every morning and 2 at night" then the function would combine them by adding the doses, giving a total daily dose of 3. If two parts represent different dosage regimens then an algorithm is applied to choose one of them, or combine them if they both have a duration (e.g. contraceptive pills with instructions such as 1 daily for 21 days then 7 days break is converted to 0.75  per (1 days) for 28 days)
}

}

\references{
Shah AD, Martinez C. An algorithm to derive a numerical daily dose
from unstructured text dosage instructions.
Pharmacoepidemiol Drug Saf 2006; 15(3): 161-166. doi: 10.1002/pds.1151
\url{http://onlinelibrary.wiley.com/doi/10.1002/pds.1151/}
}

\author{
Anoop Shah
}

\seealso{
\code{\link{doseconvert}}, \code{\link{testdoseconvert}}
}

\examples{
data(singlewords)
data(multiwords)
data(patterns)
interpret('one daily for 1 week then two daily', id = 2, 
    singlewords = as.drugdose_singlewords(singlewords),
    multiwords = as.drugdose_multiwords(multiwords),
    patterns = as.drugdose_patterns(patterns),
    noisy = TRUE)
}
