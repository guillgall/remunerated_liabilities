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
1. From `VARs` in `Code` download and run `cer_VARs_replic.R` in R. The code generates the following plots: Figure 3, Figure A2. It also generates the tables A4 and A6. Additionally, it generates objects named `varCOUNTRYCODE` containing all the models' results, including the cointegration results used for generating Table 3 and Table A5. To query for a particular country call the object for the country of interest by changing the COUNTRYCODE part of the object with the ISO3 corresponding code (e.g., `varCOL` for the VAR with Colombian data).
2. For tables A3, A4: run `cer_VARs_withExternalLiabilities_replic.R`. The plots will be saved in the local working folder. The file also generates other set of results for these additional models that are not shown on the paper.
