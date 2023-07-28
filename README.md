# Tapir-Research-2023
 
This is the organized conglomeration of the projects I worked on during Mooring's summer research 2023: scripts and plots for circadian activity, lunar phase activity, relative abundance index, occupancy model plots, and any extra files. 

## Circadian Activity
Scripts and plots for circadian activity of the 4 tapir species.

### circ scripts
Scripts used to create circadian activity plots.

*SingleCircPlots.R*: this script creates circadian activity plots for each tapir species using their solar data. Each plot that is created is separate and is only for one single species.

*SingleCombinedCircPlots.R*: this script is like SingleCircPlots but it creates a circadian activity plot lattice of the four tapir species using their solar data (a lattice of the four single-species plots).

*OverlapCircPlots.R*: this script creates circadian activity overlap plots, which compare each species to each other. The coefficient of overlap is calculated. The output are individual plots.

*OverlapCombinedCircPlots.R*: this script is like OverlapCircPlots but creates a lattice of the overlap plots.

### circ plots 
Plots of tapir circadian activity that were created using my circ scripts.
## Lunar Activity
Scripts and plots for lunar phase activity of the 4 tapir species.

### lunar scripts 
Scripts used to create lunar activity plots.

*Lunar.R*: script that takes raw datasets for each species, determines lunar phase data using getMoonIllumination, and produces plots for each species. Plots include: nocturnal or diurnal for each species (single-line), nocturnal+diurnal overlap for each species, nocturnal overlap between species, and combined/latticed plots.

*Plotsw/Lines.R*: script that incorporates vertical lines and percent values to divide the plot into sections based on lunar phase.

### lunar plots

Plots of tapir lunar phase activity that were created using my lunar scripts.
## Relative Abundance Index

Scripts and plots for the relative abundance index of the 4 tapir species.

### RAI scripts
Script used to create a bar plot of RAI values using ggplot.

*RAIggplot.R*: script that takes the relative abundance index values of each tapir species and combines them into one bar plot using the package ggplot.

### RAI plots

Plots of RAI that were created using the RAIggplot script.

*regularRAIbarplot.png*: this barplot is sized regularly (smaller axis text, larger plot).

*smallRAIbarplot.png*: this barplot was created to adjust its size for publication purposes (larger axis text, smaller plot).
## Occupancy Figures

Scripts and plots for unicovariate occupancy model figures.

### occupancy scripts

Scripts used to create unicovariate occupancy plots.

*Cate_UniOccuPlots.R*: main script adapted from the other scripts in this folder. Contains the info needed to create unicovariate occupancy plots for each tapir species.

*BairdUnicovariateOccupancy.R*, *LowlandUnicovariateOccupancy.R*, *MalayanUnicovariateOccupancy.R*, *MountainUnicovariateOccupancy.R*: these scripts were developed by Christian and have the respective unicovariate occupancy models for each species.

*Graphics_All.R*: script from Sarah that is useful for making occupancy plots. Cate_UniOccuPlots.R is basically an updated version of this script, for our purposes.

### occupancy plots

Plots created using the Cate_UniOccuPlots script. These are for the top unicovariate models for each tapir species. These include single plots, as well as plots combined/latticed together.
## Extras

Extra files and scripts that I was trying out, these are incomplete and may not work

*Corrgram Script.R*: script to determine correlation between covariates and create correlogram plot

*MultiSpOccu(incomplete).R*: script where I was trying out multispecies occupancy, this was unsuccessful

*MyMalayanOccupancyModeling(incomplete).R*: trying out some occupancy modeling

*Single Species Graphs Individual and Latticed.R*: trying out density plots
