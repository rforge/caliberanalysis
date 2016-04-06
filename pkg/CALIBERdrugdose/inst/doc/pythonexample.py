# Python example using R
#
# (tested on Ubuntu 14.04)
# The packages r-cran-rserve and python-pip are required
# The python packages os and pyRserve are required

# Start R in daemon mode:
import os
os.system('R CMD Rserve')

# Now type the following commands in python:
import pyRserve
conn = pyRserve.connect()
conn.voidEval('library(CALIBERdrugdose)')

# Load package dictionaries into global environment
conn.voidEval('data(singlewords)')
conn.voidEval('singlewords <- as.drugdose_singlewords(singlewords)')
conn.voidEval('data(multiwords)')
conn.voidEval('multiwords <- as.drugdose_multiwords(multiwords)')
conn.voidEval('data(patterns)')
conn.voidEval('patterns <- as.drugdose_patterns(patterns)')

# Write a fast conversion function using preloaded dictionaries
conn.voidEval('doseconvert_fast <- function(text, textid)' +
    'doseconvert(text, textid, simplify = TRUE, ' +
    'singlewords = singlewords,' +
    'multiwords = multiwords, patterns = patterns)')

# Test analysis of a text
conn.r.doseconvert_fast(text = '2 tablets daily', textid = 3)

# Shut down R session
conn.shutdown()
