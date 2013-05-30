
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

<p>This project under development will comprise a set of R packages to assist in epidemiological
studies using electronic health records databases.</p>

<p>CALIBER (<a href="http://caliberresearch.org/">http://caliberresearch.org/</a>) is
led from the Centre for Health Service and Academic Partnership in Translational Electronic
health record Research (CHAPTER) incorporating the National Institute for Cardiovascular Outcomes
Research (NICOR). CHAPTER is led from University College London (UCL) and Partners, which include
UCL, the London School of Hygiene and Tropical Medicine and Queen Mary University of London.
CALIBER investigators represent a collaboration between epidemiologists, clinicians, statisticians,
health informaticians and computer scientists with initial funding from the Wellcome Trust and the
National Institute for Health Research.</p>

<p>The goal of CALIBER is to provide evidence across different stages of translation, from discovery,
through evaluation to implementation where electronic health records provide new scientific opportunities.</p>

<h2>CALIBERrfimpute</h2>

<p>Missing data are frequently handled by multiple imputation, but parametric imputation methods
may lead to biased results if the imputation method is incorrectly specified. Random Forest is a
non-parametric prediction method which can handle non-linearities and interactions in a flexible way.</p>

<p>The CALIBERrfimpute package contains novel imputation functions using Random Forest within the
framework of Multivariate Imputation by Chained Equations.</p>

<h2>CALIBERcodelists</h2>

<p>Identifying patients with particular medical diagnoses in electronic health record data requires an algorithm to select the appropriate diagnostic codes. Research groups may accumulate a large number of code lists for different medical conditions, and the CALIBERcodelists package contains functions for creating codelists and storing them in a standardised format with metadata such as the authors and version number.</p>

<p>CALIBER investigators can use this package in conjunction with the CALIBERlookups package which contains the source dictionaries; other researchers can use the scripts provided to create lookup tables from the official sources of the Read, ICD-10, OPCS and CPRD dictionaries.</p>

<h2>CALIBERfma</h2>

<p>The Freetext Matching Algorithm is a natural language processing system for analysing clinical free text, and is available from the <a href="https://github.com/anoopshah/freetext-matching-algorithm">freetext-matching-algorithm GitHub repository</a>. It uses lookup tables from the <a href="https://github.com/anoopshah/freetext-matching-algorithm-lookups">freetext-matching-algorithm-lookups GitHub repository</a>.

This R package provides an interface to the program, as well as tools to help manipulate the lookup tables. It is currently only availble for Linux systems as it requires <a href="http://www.winehq.org/">wine</a> and <a href="http://en.wikipedia.org/wiki/Git_%28software%29">git</a>.</p>

<hr/>

Link to project summary page:
<a href="http://r-forge.r-project.org/projects/caliberanalysis/">
http://r-forge.r-project.org/projects/caliberanalysis/</a>

</body>
</html>
