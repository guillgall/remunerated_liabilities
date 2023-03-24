This repository contains the code and data of the paper "[Exchange Rate Dynamics and the Central Bank’s Balance Sheet](https://guillgall.github.io/files/conversion_er.pdf)" by Guillermo Gallacher, Camilo Granados and Janelle Mann.

## Code organization

There are two main folders `Code` and `Data`. The code inside `Code` transforms data from `Data/Input` into `Data/Output`.

## Software requirements
The code is written in [R](https://www.r-project.org/), Matlab, and RATS.

## Replication instructions for VAR models

### Data, variable constructiona and tables/plots
1. Download the [IMF-IFS](https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b) full dataset and the relevant variables for Argentina available in [UCEMA-CEA](https://ucema.edu.ar/cea).
2. Save this data in your desktop in  `Desktop/Data` subfolder. 
3. Follow instructions in the [master code](Code/00_master_run.R).

### Replication instructions for Unit Root Tests (Table A4)
1. Download and unzip 'Unit Root Tests'
2. Run 20230221 Unit Root Results.prg in Rats.

### Replication instructions for Cointegration Analysis (Tables 3 and A5)
1. Download and unzip 'Cointegration Analysis'.
2. Set the Current Folder in Matlab to 'Cointegration Analysis'. Add folders 'toolbox' and 'Sephton and Mann' to path.
3. Run driver.m in Matlab.

### Replication instructions for VAR models (Figures 3, A1, A2, and Tables 2, A1, A2, A3)
0. You can Download VARs.zip and run 'er_paper_VARexercise.R' in a local folder. Alternatively, you can use the files from `Code` and `Data` as explained below.
1. From `Data` download 'arg_cer.csv' and 'cer.csv'.
2. From `Code` download and run 'er_paper_VARexercise.R' in R. The code generates the plots and saves it in a designated folder path, the folder should be adjusted (each plot is still prompted by R and can be saved manually).
3. For tables 2 and A2, 'er_paper_VARexercise.R' shold be run until line 558. Then in line 563 the model should be changed for that of each country (names are shown in the file and follow the pattern var'ABC', with ABC being the ISO3 abreviation of each country). Then run the rest of the code to generate the results used for table 2, A2.
