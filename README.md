# Replication-material-for-Ludwig-Bruederl-ASR2018
Replication files, Data and Stata Do files for replication of research article: Ludwig, V., &amp; Brüderl, J. (2018). Is There a Male Marital Wage Premium? New Evidence from the United States. American Sociological Review, 83(4), 744–770. https://doi.org/10.1177/0003122418784909


The empirical analysis is conducted using Stata 14.
Replication material provided in this package:
1) Tag sets for download of NLSY79 data via the NLSY Investigator
2) Stata Do-files for a) extracting, b) preparing and c) analyzing the data 
3) Stata Do-files for simulation results shown in Online Supplement (Part A and B)
4) Stata Do-file for Figure 1 of the article (Hypotheses)
5) Final NLSY79 data set used for empirical analysis

# 1) Tag sets for download of NLSY79 data via the NLS Investigator

The analysis uses data of the National Longitudinal Survey of Youth 1979 (NLSY79).
Data access is granted by the Bureau of Labor Statistics.
* NLSY1979, version 1979-2012, released Feb 3rd 2015
* Download from www.nlsinfo.org on Sep 20th 2015

NLSY79_tag_sets.zip
-------------------
To access the data online via the NLS Investigator use tag sets provided in the replication package.
List of tag sets contained in NLSY79_tag_sets.zip :

mwp_us_intweek.NLSY79                  
mwp_us_intmonth.NLSY79                 
mwp_us_wage.NLSY79           
mwp_us_earnings.NLSY79              
mwp_us_workhours.NLSY79		
mwp_us_marstat.NLSY79         
mwp_us_mdauer.NLSY79           
mwp_us_childbirth.NLSY79            
mwp_us_cohabitation2.NLSY79        
mwp_us_enroll.NLSY79     
mwp_us_educ.NLSY79            
mwp_us_weeks.NLSY79              
mwp_us_weeks_miss.NLSY79             
mwp_us_anhours.NLSY79        
mwp_us_cps.NLSY79                
mwp_us_ten.NLSY79                    
mwp_us_lfs.NLSY79                    
mwp_us_class.NLSY79                    
mwp_us_controls.NLSY79               	
mwp_us_wifeempl.NLSY79

To get the data, upload each tagset to the NLS Investigator;
request download of the variables in Stata format;
save each file on your machine (use name of the tag set as file name).
The downloaded files are .zip files. 
In each .zip file, there is a .dct file containing the data.


# 2) Stata Do-files for extracting, preparing and analyzing the data 

For replication of the empirical analyses, 3 Stata Do-files are provided.

a) mwp_US_extract.do
--------------------
- extracts the .zip files downloaded in step 1);
- reads each .dct file into Stata;
- renames and creates variables;
- converts files to long-format panel data sets:
- saves the data sets as .dta files (Stata 14); 
- merges the .dta files into one long-format panel data set
- saves the data set as Stata 14 file named "mwp_US.dta"

b) mwp_US_dataprep.do
---------------------
- reads the data file "mwp_US.dta"
- renames and creates variables as used in the analyses;
- defines the main estimation sample (variable sample1),
	and a less restricted estimation sample (variable sample2)
	(used in robustness checks on sample selection)
- selects subset of variables needed for the analyses
- saves the final Stata 14 file named "mwp_US_analysis.dta"

c) mwp_US_analysis.do
---------------------
- uses the final Stata 14 file "mwp_US_analysis.dta"
- produces all empirical analyses (Tables and Figures) 
  published in the article and in the Online Supplement


# 3) Stata Do-files for simulation results shown in Online Supplement

For replication of the Monte Carlo simulation results shown in Online Supplement
(Tables S1 and S2), 2 Stata Do-files are provided.

a) sim1_FEGS.do 
---------------
- defines new Stata program panelsim
- executes panelsim to compute simulation results for different settings (Table S1)


b) sim2_tv_treatment.do 
-----------------------
- defines new Stata program panelsim
- executes panelsim to compute simulation results for different settings (Table S2)



# 4) Stata Do-file for Figure 1 of the article (Hypotheses)

Figure1.do
----------
- generates artificial data for idealtypical wage profiles
- plots the wage profiles


# 5) Final NLSY79 data set used for empirical analysis

mwp_us_analysis.dta
-------------------

- Stata 14 panel data set of processed and finally edited NLSY79 data
- raw NLSY79 data prepared with Do-files mwp_us_extract.do and mwp_us_dataprep.do
- input for analysis conducted with Do-file mwp_us_analysis.do

