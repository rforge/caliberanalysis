
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<h1>CALIBER health records research toolkit</h1>

<p>This project comprises a set of R packages to assist in epidemiological
studies using electronic health records databases.</p>

<p>CALIBER (<a href="http://caliberresearch.org/">http://caliberresearch.org/</a>) is
led from the <a href="http://www.farrinstitute.org/centre/London/">
Farr Institute &#64; London</a>.
CALIBER investigators represent a collaboration between epidemiologists, clinicians, statisticians,
health informaticians and computer scientists with initial funding from the Wellcome Trust and the
National Institute for Health Research.</p>

<p>The goal of CALIBER is to provide evidence across different stages of translation, from discovery,
through evaluation to implementation where electronic health records provide new scientific opportunities.</p>

<p>Download R packages here:
<a href="http://r-forge.r-project.org/R/?group_id=1598">
http://r-forge.r-project.org/R/?group_id=1598</a></p>

<hr/>


<h2>CALIBERcodelists</h2>

<p>Identifying patients with particular medical diagnoses in electronic health record data requires an algorithm to select the appropriate diagnostic codes. Research groups may accumulate a large number of code lists for different medical conditions, and the CALIBERcodelists package contains functions for creating codelists and storing them in a standardised format with metadata such as the authors and version number.</p>

<p>CALIBER investigators can use this package in conjunction with the CALIBERlookups package which contains the source dictionaries; other researchers can use the scripts provided to create lookup tables from the official sources of the Read, ICD-10, OPCS and CPRD dictionaries.</p>


<h2>CALIBERdatamanage</h2>

<p>The CALIBER data management package includes functions to:</p>

<ol>
<li><strong>Import data</strong> - Import single or multiple files to data.table or ffdf objects in R, with automatic unzipping of compressed files and conversion of dates, and decoding using lookup tables in the CALIBERlookups package.</li>

<li><strong>Build cohorts</strong> - A 'cohort' S3 class to store information about a cohort, and functions for generating analysis variables from multiple row per patient data.</li>

<li><strong>Create presentation tables</strong> - Produce summary tables in LaTeX or plain text, and format numbers and percentages.</li>

<li><strong>Make forest plots</strong> - Produce forest plots using a spreadsheet template, with the facility to include several plots side by side, and specify the formatting of text.</li>
</ol>

<h2>CALIBERdrugdose</h2>

<p>The CALIBER drug dose algorithm converts unstructured text dosage instructions into a structured format.</p>

<h3>Reference</h3>

<ol>
<li>
Shah AD, Martinez C. Pharmacoepidemiology and Drug Safety 2006; 15: 161-166. doi: <a href="http://dx.doi.org/10.1002/pds.1151">10.1002/pds.1151</a>
</li>
</ol>

<h2>CALIBERfma</h2>

<p>The Freetext Matching Algorithm is a natural language processing system for analysing clinical free text,
and is available from the <a href="https://github.com/anoopshah/freetext-matching-algorithm">freetext-matching-algorithm GitHub repository</a>.
It uses lookup tables from the <a href="https://github.com/anoopshah/freetext-matching-algorithm-lookups">freetext-matching-algorithm-lookups GitHub repository</a>.

This R package provides an interface to the program, as well as tools to help manipulate the lookup tables.
It is currently only availble for Linux systems as it requires <a href="http://www.winehq.org/">wine</a> and
<a href="http://en.wikipedia.org/wiki/Git_%28software%29">git</a>.</p>

<h2>CALIBERrfimpute</h2>

<p>Missing data are frequently handled by multiple imputation, but parametric imputation methods
may lead to biased results if the imputation method is incorrectly specified. Random Forest is a
non-parametric prediction method which can handle non-linearities and interactions in a flexible way.</p>

<p>The CALIBERrfimpute package contains novel imputation functions using Random Forest within the
framework of Multivariate Imputation by Chained Equations.</p>

<p>An alternative Random Forest imputation algorithm was developed by Doove et al. and is available in the MICE package; our vignette in CALIBERrfimpute compares the two methods.</p>

<h3>References</h3>

<ol>
<li>
Shah AD, Bartlett JW, Carpenter J, Nicholas O, Hemingway H. Comparison of Random Forest and parametric imputation models for imputing missing data using MICE: a CALIBER study. American Journal of Epidemiology 2014. doi: <a href="http://dx.doi.org/10.1093/aje/kwt312">10.1093/aje/kwt312</a>
</li>

<li>
Doove LL, van Buuren S, Dusseldorp E. Recursive partitioning for missing data imputation in the presence of interaction effects. Computational Statistics and Data Analysis 2014;72:92--104. doi: <a href = "http://dx.doi.org/10.1016/j.csda.2013.10.025">10.1016/j.csda.2013.10.025</a>
</li>
</ol>


<hr/>

<p>
Link to project summary page:
<a href="http://r-forge.r-project.org/projects/caliberanalysis/">
http://r-forge.r-project.org/projects/caliberanalysis/</a>
</p>

<p>
Download R packages here:
<a href="http://r-forge.r-project.org/R/?group_id=1598">
http://r-forge.r-project.org/R/?group_id=1598</a>
</p>

</body>
</html>
