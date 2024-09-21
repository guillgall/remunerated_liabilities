This repository contains the code and data of the paper "[Exchange Rate Dynamics and the Central Bank’s Balance Sheet](https://guillgall.github.io/files/conversion_er.pdf)" by Guillermo Gallacher, Camilo Granados and Janelle Mann (Journal of International Money and Finance, October 2024). Published version [here](https://www.sciencedirect.com/science/article/abs/pii/S0261560624001438)

## Code organization

There are two main folders `Code` and `Data`. 

## Software requirements
The code is written in [R](https://www.r-project.org/), Matlab, and RATS.

## Replication instructions for VAR models

### Data, variable construction and tables/plots
1. You need to download the [IMF-IFS](https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b) full dataset (raw data not included here due to size), as well as Argentina's informal exchange rate from [UCEMA-CEA](https://ucema.edu.ar/cea) (or use the raw data included in Data/Input folder). 
2. Our scripts clean and merge such databases, and then create relevant variables. The output of such scripts is the main data used in the paper and is available in [here ](Data/Output/cer_complete.csv).
3. Run the plot scripts in the `Code/data_imf_ifs` subfolder to generate Figures 1, 2 and A3; and Table 1 and A6. 

### Replication instructions for Unit Root Tests (Table A4)
1. Download and unzip 'Unit Root Tests'
2. Run 20230221 Unit Root Results.prg in Rats.

### Replication instructions for Cointegration Analysis (Tables 3 and A5)
1. Download and unzip 'Cointegration Analysis'.
2. Install the Econometrics Toolbox for Matlab.
3. Set the Current Folder in Matlab to 'Cointegration Analysis'. Add folder 'Sephton and Mann' to path.
4. Run driver.m in Matlab.

### Replication instructions for VAR models (Figures 3, A1, A2, and Tables 2, A1, A2, A3)
0. You can Download VARs.zip and run 'er_paper_VARexercise.R' in a local folder. Alternatively, you can use the files from `Code` and `Data` as explained below.
1. From `Data` download 'arg_cer.csv' and 'cer.csv'.
2. From `Code` download and run 'er_paper_VARexercise.R' in R. The code generates the plots and saves it in a designated folder path, the folder should be adjusted (each plot is still prompted by R and can be saved manually).
3. For tables 2 and A2, 'er_paper_VARexercise.R' shold be run until line 558. Then in line 563 the model should be changed for that of each country (names are shown in the file and follow the pattern var'ABC', with ABC being the ISO3 abreviation of each country). Then run the rest of the code to generate the results used for table 2, A2.
