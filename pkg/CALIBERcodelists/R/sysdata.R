# This file creates objects for the CALIBERcodelists internal environment.

# All diagnosis dictionaries
ALLDICTS <- c("icd10", "opcs", "read", "icd9")

# Order of columns in exported codelists
CODELIST_COLORDER <- c("category", "code", "term")

META <- fread('
item|value
Author|"" 
Date|""
Name|""
Version|""
category|""
description|""
icd10|"HES"
icd9|"FALSE"
message|""
opcs|"OPCS"
read|"GPRD"
shortname|""
spreadsheet|""
')
setkey(META, item)

# Each source can only use one dictionary, but a dictionary may be
# used for more than one data source. 
SOURCEDICTS <- fread('
dict|Source
icd9|ONSICD9
icd10|HES
icd10|ONS
opcs|OPCS
read|GPRD
product|GPRDPROD
')

# This does not work with fread because the text is too long,
# so need to use a textConnection (it should however work
# the most recent version of data.table
SAMPLE_DICT <- textConnection('
code|term|dict|events|medcode|termlc
"A155"|"Tuberculosis of larynx, trachea and bronchus, confirmed"|"icd10"|NA|NA|"tuberculosis of larynx, trachea and bronchus, confirmed"
"E282"|"Polycystic ovarian syndrome"|"icd10"|NA|NA|"polycystic ovarian syndrome"
"I200"|"Unstable angina"|"icd10"|NA|NA|"unstable angina"
"I201"|"Angina pectoris with documented spasm"|"icd10"|NA|NA|"angina pectoris with documented spasm"
"I208"|"Other forms of angina pectoris"|"icd10"|NA|NA|"other forms of angina pectoris"
"I209"|"Angina pectoris, unspecified"|"icd10"|NA|NA|"angina pectoris, unspecified"
"I210"|"Acute transmural myocardial infarction of anterior wall"|"icd10"|NA|NA|"acute transmural myocardial infarction of anterior wall"
"I213"|"Acute transmural myocardial infarction of unspecified site"|"icd10"|NA|NA|"acute transmural myocardial infarction of unspecified site"
"I221"|"Subsequent myocardial infarction of inferior wall"|"icd10"|NA|NA|"subsequent myocardial infarction of inferior wall"
"I228"|"Subsequent myocardial infarction of other sites"|"icd10"|NA|NA|"subsequent myocardial infarction of other sites"
"I421"|"Obstructive hypertrophic cardiomyopathy"|"icd10"|NA|NA|"obstructive hypertrophic cardiomyopathy"
"I422"|"Other hypertrophic cardiomyopathy"|"icd10"|NA|NA|"other hypertrophic cardiomyopathy"
"I48X"|"Atrial fibrillation and flutter"|"icd10"|NA|NA|"atrial fibrillation and flutter"
"X050"|"Exposure to ignition or melting of nightwear ;  Home"|"icd10"|NA|NA|"exposure to ignition or melting of nightwear ;  home"
"X051"|"Exposure to ignition or melting of nightwear ;  Residential institution"|"icd10"|NA|NA|"exposure to ignition or melting of nightwear ;  residential institution"
"413"|"Angina pectoris"|"icd9"|NA|NA|"angina pectoris"
"4130"|"Angina pectoris ; Angina decubitus"|"icd9"|NA|NA|"angina pectoris ; angina decubitus"
"4131"|"Angina pectoris ; Prinzmetal angina"|"icd9"|NA|NA|"angina pectoris ; prinzmetal angina"
"4139"|"Angina pectoris ; Other and unspecified angina pectoris"|"icd9"|NA|NA|"angina pectoris ; other and unspecified angina pectoris"
"413"|"Angina pectoris"|"icd9head"|NA|NA|"angina pectoris"
"A15"|"Respiratory tuberculosis"|"icdhead"|NA|NA|"respiratory tuberculosis"
"E28"|"Ovarian dysfunction"|"icdhead"|NA|NA|"ovarian dysfunction"
"I20"|"Angina pectoris"|"icdhead"|NA|NA|"angina pectoris"
"I21"|"Acute myocardial infarction"|"icdhead"|NA|NA|"acute myocardial infarction"
"I22"|"Subsequent myocardial infarction"|"icdhead"|NA|NA|"subsequent myocardial infarction"
"I42"|"Cardiomyopathy"|"icdhead"|NA|NA|"cardiomyopathy"
"I48"|"Atrial fibrillation and flutter"|"icdhead"|NA|NA|"atrial fibrillation and flutter"
"X05"|"Exposure to ignition or melting of nightwear"|"icdhead"|NA|NA|"exposure to ignition or melting of nightwear"
"K621"|"Percutaneous transluminal ablation of pulmonary vein to left atrium conducting system"|"opcs"|NA|NA|"percutaneous transluminal ablation of pulmonary vein to left atrium conducting system"
"K622"|"Percutaneous transluminal ablation of atrial wall for atrial flutter"|"opcs"|NA|NA|"percutaneous transluminal ablation of atrial wall for atrial flutter"
"K623"|"Percutaneous transluminal ablation of conducting system of heart for atrial flutter NEC"|"opcs"|NA|NA|"percutaneous transluminal ablation of conducting system of heart for atrial flutter nec"
"Q494"|"Endoscopic drilling of ovary"|"opcs"|NA|NA|"endoscopic drilling of ovary"
"1226.11"|"No FH: Angina"|"read"|1409|9528|"no fh: angina"
"12C..14"|"FH: Angina"|"read"|2046|13222|"fh: angina"
"12CF.00"|"FH angina male first degree age unknown"|"read"|5041|40865|"fh angina male first degree age unknown"
"12CH.00"|"FH angina female first degree age unknown"|"read"|7353|42996|"fh angina female first degree age unknown"
"12CJ.00"|"FH: Cardiomyopathy"|"read"|396|13274|"fh: cardiomyopathy"
"12CM.00"|"FH: Angina in 1st degree male relative <55 years"|"read"|0|26653|"fh: angina in 1st degree male relative <55 years"
"12CR.00"|"FH: Hypertrophic obstructive cardiomyopathy"|"read"|1408|42999|"fh: hypertrophic obstructive cardiomyopathy"
"12FA.00"|"FH: Polycystic ovaries"|"read"|5650|32779|"fh: polycystic ovaries"
"3234.00"|"ECG:posterior/inferior infarct"|"read"|7181|26972|"ecg:posterior/inferior infarct"
"3615100"|"Bronchoscopy abnormal"|"read"|1612|5654|"bronchoscopy abnormal"
"3889.00"|"Euroscore for angina"|"read"|5071|39584|"euroscore for angina"
"662K.00"|"Angina control"|"read"|1|13185|"angina control"
"662K200"|"Angina control - improving"|"read"|4040|14782|"angina control - improving"
"662K300"|"Angina control - worsening"|"read"|6221|29300|"angina control - worsening"
"7445400"|"Rigid bronchoscopic laser destruction of lesion of trachea"|"read"|1|66478|"rigid bronchoscopic laser destruction of lesion of trachea"
"7445500"|"Endoscopic aspiration of trachea using rigid bronchoscope"|"read"|1163|50415|"endoscopic aspiration of trachea using rigid bronchoscope"
"7445y00"|"Therapeutic rigid bronchoscopic operation on trachea OS"|"read"|20|69727|"therapeutic rigid bronchoscopic operation on trachea os"
"7445z00"|"Therapeutic rigid bronchoscopic operation on trachea NOS"|"read"|3916|63881|"therapeutic rigid bronchoscopic operation on trachea nos"
"7446y00"|"Diagnostic endoscop exam trachea using rigid bronchoscope OS"|"read"|4619|59894|"diagnostic endoscop exam trachea using rigid bronchoscope os"
"7446z00"|"Diagnostic endosc exam trachea using rigid bronchoscope NOS"|"read"|20|34577|"diagnostic endosc exam trachea using rigid bronchoscope nos"
"744A500"|"Therapeutic rigid bronchoscopic operation on lung"|"read"|5|50232|"therapeutic rigid bronchoscopic operation on lung"
"744By00"|"Other specified rigid diagnostic bronchoscopy"|"read"|7993|55769|"other specified rigid diagnostic bronchoscopy"
"744Bz11"|"Bronchoscopy NEC"|"read"|87|2870|"bronchoscopy nec"
"744F.11"|"Diagnostic fibreoptic bronchoscopic examination of trachea"|"read"|12|8642|"diagnostic fibreoptic bronchoscopic examination of trachea"
"744H.00"|"Diagnostic fibreoptic bronchoscopy"|"read"|7940|3902|"diagnostic fibreoptic bronchoscopy"
"744Hz00"|"Diagnostic fibreoptic bronchoscopy NOS"|"read"|12|14200|"diagnostic fibreoptic bronchoscopy nos"
"7E25300"|"Endoscopic drilling of ovary"|"read"|2750|38133|"endoscopic drilling of ovary"
"A340000"|"Streptococcal angina"|"read"|5560|24176|"streptococcal angina"
"BBK3.00"|"[M]Rhabdomyomatous neoplasms"|"read"|7312|28382|"[m]rhabdomyomatous neoplasms"
"BBK3000"|"[M]Rhabdomyoma NOS"|"read"|141|43773|"[m]rhabdomyoma nos"
"BBK3100"|"[M]Rhabdomyosarcoma NOS"|"read"|23|31421|"[m]rhabdomyosarcoma nos"
"BBLH.00"|"[M]Rhabdoid sarcoma"|"read"|4891|17212|"[m]rhabdoid sarcoma"
"C164.00"|"Polycystic ovaries"|"read"|439|1466|"polycystic ovaries"
"C165.00"|"Polycystic ovarian syndrome"|"read"|0|11347|"polycystic ovarian syndrome"
"G30..11"|"Attack - heart"|"read"|1596|13566|"attack - heart"
"G30y.00"|"Other acute myocardial infarction"|"read"|4874|34803|"other acute myocardial infarction"
"G30y000"|"Acute atrial infarction"|"read"|7316|28736|"acute atrial infarction"
"G30y200"|"Acute septal infarction"|"read"|5026|41221|"acute septal infarction"
"G311.00"|"Preinfarction syndrome"|"read"|3171|36523|"preinfarction syndrome"
"G311.11"|"Crescendo angina"|"read"|102|4656|"crescendo angina"
"G311.13"|"Unstable angina"|"read"|9|1431|"unstable angina"
"G311.14"|"Angina at rest"|"read"|2210|19655|"angina at rest"
"G311100"|"Unstable angina"|"read"|25|7347|"unstable angina"
"G311200"|"Angina at rest"|"read"|3283|17307|"angina at rest"
"G311300"|"Refractory angina"|"read"|5452|34328|"refractory angina"
"G311400"|"Worsening angina"|"read"|11|18118|"worsening angina"
"G311z00"|"Preinfarction syndrome NOS"|"read"|148|54251|"preinfarction syndrome nos"
"G33..00"|"Angina pectoris"|"read"|684|1430|"angina pectoris"
"G330.00"|"Angina decubitus"|"read"|3300|20095|"angina decubitus"
"G330000"|"Nocturnal angina"|"read"|14|18125|"nocturnal angina"
"G330z00"|"Angina decubitus NOS"|"read"|563|29902|"angina decubitus nos"
"G331.00"|"Prinzmetal angina"|"read"|11|12986|"prinzmetal angina"
"G331.11"|"Variant angina pectoris"|"read"|23|11048|"variant angina pectoris"
"G33z.00"|"Angina pectoris NOS"|"read"|7374|25842|"angina pectoris nos"
"G33z300"|"Angina on effort"|"read"|0|1414|"angina on effort"
"G33z500"|"Post infarct angina"|"read"|10|9555|"post infarct angina"
"G33z700"|"Stable angina"|"read"|1401|12804|"stable angina"
"G33zz00"|"Angina pectoris NOS"|"read"|5111|28554|"angina pectoris nos"
"G35..00"|"Subsequent myocardial infarction"|"read"|5913|18842|"subsequent myocardial infarction"
"G35X.00"|"Subsequent myocardial infarction of unspecified site"|"read"|879|46166|"subsequent myocardial infarction of unspecified site"
"G551.00"|"Hypertrophic obstructive cardiomyopathy"|"read"|5961|8010|"hypertrophic obstructive cardiomyopathy"
"G554300"|"Hypertrophic non-obstructive cardiomyopathy"|"read"|636|3499|"hypertrophic non-obstructive cardiomyopathy"
"G573.00"|"Atrial fibrillation and flutter"|"read"|1865|2212|"atrial fibrillation and flutter"
"G573000"|"Atrial fibrillation"|"read"|2682|1664|"atrial fibrillation"
"G573100"|"Atrial flutter"|"read"|145|1757|"atrial flutter"
"G573z00"|"Atrial fibrillation and flutter NOS"|"read"|3387|23437|"atrial fibrillation and flutter nos"
"Gyu5M00"|"[X]Other hypertrophic cardiomyopathy"|"read"|1277|70648|"other hypertrophic cardiomyopathy"
"N233300"|"Rhabdomyolysis"|"read"|102|95831|"rhabdomyolysis"
"SK08.00"|"Acute renal failure due to rhabdomyolysis"|"read"|1792|24676|"acute renal failure due to rhabdomyolysis"
"SK09.00"|"Traumatic rhabdomyolysis"|"read"|170|98532|"traumatic rhabdomyolysis"
"ZR37.00"|"Canadian Cardiovascular Society classification of angina"|"read"|440|59350|"canadian cardiovascular society classification of angina"
"ZR3P.11"|"CLASP angina score"|"read"|4762|57910|"clasp angina score"
')

SAMPLE_DICT <- as.data.table(read.delim(SAMPLE_DICT, as.is = TRUE, sep = '|'))
SAMPLE_DICT[, category := NA_integer_]

SAMPLE_DICTMAPS <- fread('
code|map_stat|ref_flag|add_flag|elem_num|block_num|dict|medcode
"E282"|"G"|"C"|"C"|0|0|"icd10"|1466
"E282"|"T"|""|""|0|0|"icd10"|11347
"I200"|"G"|"C"|"C"|0|0|"icd10"|1431
"I200"|"T"|""|""|0|0|"icd10"|1431
"I200"|"G"|"C"|"C"|0|0|"icd10"|4656
"I200"|"G"|"C"|"C"|0|0|"icd10"|7347
"I200"|"T"|""|""|0|0|"icd10"|7347
"I200"|"G"|"C"|"C"|0|0|"icd10"|17307
"I200"|"G"|"C"|"C"|0|0|"icd10"|19655
"I200"|"G"|"C"|"C"|0|0|"icd10"|20095
"I200"|"G"|"C"|"C"|0|0|"icd10"|29902
"I200"|"G"|"C"|"C"|0|0|"icd10"|36523
"I200"|"D"|"C"|"C"|0|0|"icd10"|54251
"I201"|"G"|"C"|"C"|0|0|"icd10"|11048
"I201"|"G"|"C"|"C"|0|0|"icd10"|12986
"I208"|"G"|"C"|"C"|0|0|"icd10"|18125
"I209"|"D"|"C"|"C"|0|0|"icd10"|1430
"I209"|"D"|"C"|"C"|0|0|"icd10"|25842
"I209"|"D"|"C"|"C"|0|0|"icd10"|28554
"I221"|"R"|"C"|"C"|0|0|"icd10"|26972
"I421"|"D"|"C"|"C"|0|0|"icd10"|8010
"I422"|"G"|"C"|"C"|0|0|"icd10"|3499
"I422"|"E"|"C"|"C"|0|0|"icd10"|70648
"I422"|"T"|""|""|0|0|"icd10"|70648
"I48X"|"G"|"C"|"C"|0|0|"icd10"|1664
"I48X"|"G"|"C"|"C"|0|0|"icd10"|1757
"I48X"|"D"|"C"|"C"|0|0|"icd10"|2212
"I48X"|"T"|""|""|0|0|"icd10"|2212
"I48X"|"D"|"C"|"C"|0|0|"icd10"|23437
"I20"|"A"|"M"|"C"|0|0|"icdhead"|1430
"I20"|"T"|""|""|0|0|"icdhead"|1430
"I22"|"A"|"M"|"C"|0|0|"icdhead"|18842
"I22"|"T"|""|""|0|0|"icdhead"|18842
"I48"|"T"|""|""|0|0|"icdhead"|2212
"Q494"|"T"|""|""|0|0|"opcs"|38133
')

SAMPLE_GEM <- fread('
icd9cm|icd10cm|approximate|no_map|combination|scenario|choice_list|icd9|icd10|use|from9to10
"4130"|"I208"|TRUE|FALSE|FALSE|0|0|"4130"|"I208"|TRUE|TRUE
"4131"|"I201"|FALSE|FALSE|FALSE|0|0|"4131"|"I201"|TRUE|TRUE
"4139"|"I208"|TRUE|FALSE|FALSE|0|0|"4139"|"I208"|TRUE|TRUE
"4139"|"I209"|TRUE|FALSE|FALSE|0|0|"4139"|"I209"|TRUE|TRUE
"4131"|"I201"|FALSE|FALSE|FALSE|0|0|"4131"|"I201"|TRUE|FALSE
"4139"|"I208"|TRUE|FALSE|FALSE|0|0|"4139"|"I208"|TRUE|FALSE
"4139"|"I209"|TRUE|FALSE|FALSE|0|0|"4139"|"I209"|TRUE|FALSE
"4139"|"I25111"|TRUE|FALSE|TRUE|1|2|"4139"|"I251"|FALSE|FALSE
"4139"|"I25118"|TRUE|FALSE|TRUE|1|2|"4139"|"I251"|FALSE|FALSE
"4139"|"I25119"|TRUE|FALSE|TRUE|1|2|"4139"|"I251"|FALSE|FALSE
"4139"|"I25701"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25708"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25709"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25711"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25718"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25719"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25721"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25728"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25729"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25731"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25738"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25739"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25751"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25758"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25759"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25761"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25768"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25769"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25791"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25798"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
"4139"|"I25799"|TRUE|FALSE|TRUE|1|2|"4139"|"I257"|FALSE|FALSE
')

SAMPLE_GEM[, approximate := as.logical(approximate)]
SAMPLE_GEM[, no_map := as.logical(no_map)]
SAMPLE_GEM[, combination := as.logical(combination)]
SAMPLE_GEM[, use := as.logical(use)]
SAMPLE_GEM[, from9to10 := as.logical(from9to10)]

