# Stratigraphy Paper

The code for "Identification of the mode of evolution in incomplete carbonate successions"

## Authors

__Jan Roel Koelewijn__  
Utrecht University  
Orcid: [0000-0002-4668-3797](https://orcid.org/0000-0002-4668-3797)

__Emilia Jarochowska__  
Utrecht University  
email: e.b.jarochowska [at] uu.nl  
Web page: [uu.nl/staff/EBJarochowska](https://www.uu.nl/staff/EBJarochowska)  
Orcid: [0000-0001-8937-9405](https://orcid.org/0000-0001-8937-9405)

__Niklas Hohmann__
Utrecht University  
email: n.hohmann@uu.nl  
Web page: [uu.nl/staff/NHohmann](uu.nl/staff/NHohmann)  
Orcid: [0000-0003-1559-1838](https://orcid.org/0000-0003-1559-1838)

## License

Apache 2.0, see LICENSE file for full license text

## Software requirements

Base R
Packages _paleoTS_, _renv_, _DAIME_, _ggplot2_ and their dependencies

## Usage

First, install the _renv_ package by running the following code in R:

``` R
if (!require("renv", quietly = TRUE)) install.packages("renv")
```

Then, open the Rproject file "stratigraphy_paper.Rproj". Next, run

``` R
renv::restore()
```

This will install all required packages and their dependencies needed in the project.

## Repository Structure

* _README.md_ : Readme file
* _LICENSE_ : Apache 2.0 license text
* _.gitignore_ : Files not tracked by git
* _Stratigraphy_paper.Rproj_ : Rstudio project file
* _renv_ : Folder generated by _renv_ package
* _data_ : Folder for data
  * _matlab_outputs_ : Folder with raw data generated by CarboCAT in Matlab
    * _scenarioA_matlab_outputs.mat_
    * _scenarioB_matlab_outputs.mat_
    * _scenarioA_and_B_matlab_to_R.mat_
    * _get_adm_from_carbocat_outputs.m_
  * _R_outputs_ : Folder with data generated by R
    * _age_depth_models.Rdata_
    * _results_modes_of_evolution.Rdata_
* _code_ : Folder with main code
  * _analysis_tests_and_simulations.R_
  * _simulate_and_test_modes_of_evolution.R_
  * _import_adm_to_R.R_
  * _multiplot.R_
* _figs_ : Folder with figures
* _tables_ : Folder with tables

## Old README File

This document describes how to the used in the publication INSERT TITLE HERE

REPEATABILITY OF DATA GENERATION
Below is a description of the pipeline uses to generate the data used in the publication.
Note that  all data generated by the following steps, including test results, are already provided.
The aim of the following description is reproducability

1. Run basin simulations:
Go into the folder scenarioX (where X=A or B), open the file“carboCATGUI.m” in Matlab and run it. In the window that appears, first click “initialize”.
Once the window shows the initial platform parameters, click “run CA model”. The run itself can take a while. Once it is done, a file “scenarioXMatlabOutputs.mat” will appear in
the folder. It contains all model outputs.
2. Extract age-depth models from model outputs:
Copy the files “scenarioXMatlabOutputs.mat” into the main folder. Open the file
“extractADMFromCarboCAT.m” in Matlab and run it. It extracts age-depth models from the
model outputs, and stores them in a file named “scenariosAandBMatlabToR.mat”.
3. Import age-depth models into R:
Open the file “importADMToR.R” in R, and run it.
It will import the age-depth models into R and save them as a workspace named “scenariosAandB.Rdata”.
This workspace contains all age-depth models used.
4. Test modes of evolution
Open the file "testModesOfEvolution.R" in R and run it.
It will test how well the different modes of evolution are recognized in the different scenarios, and save the results as a workspace named "resultsTestModesOfEvo.Rdata".
