# hot-chicks
R code to recreate 'Shy albatross (_Thalassarche cauta_) chick mortality and heat stress in temperate climates'.

# Chapter 4 of Claire Mason's PhD (June 2022)
# Submitted to MEPS Heatwave Impacts Special Issue 2022

# AUTHOR: Claire Mason <claire.mason@utas.edu.au>

# The scripts in this project are organised as follows: 
#1# load and clean datasets 
#2# analysis 
#3# create figures 
#4# supplementary material 


# These scripts use the CRAGS monitoring system for shy albatross on Albatross Island
# Methods paper: Lynch, T.P., Alderman, R. and Hobday, A.J. (2015), A high-resolution panorama camera system for monitoring colony-wide seabird nesting behaviour. Methods Ecol Evol, 6: 491-499. https://doi.org/10.1111/2041-210X.12339
# Commercial website: https://www.csiro.au/en/research/natural-environment/oceans/crags


# The most helpful tutorial ever was this series of four papers. I worked through these and now have a really good solid understanding of survival data and analysis 
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2394262/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2394368/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2376927/
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2394469/

# helpful resources for survival analysis in R 
#https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
#https://www.datacamp.com/community/tutorials/survival-analysis-R
#https://hal.archives-ouvertes.fr/hal-03007043/document
