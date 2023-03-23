TBC

This repository contains the code and data of the paper "[Exchange Rate Dynamics and the Central Bank’s Balance Sheet](https://guillgall.github.io/files/conversion_er.pdf)" by Guillermo Gallacher, Camilo Granados and Janelle Mann..

## Code organization

There are two main folders `Code` and `Data`. The code inside `Code` transforms data from `Data/Input` into `Data/Output`.

## Software requirements
The code is written in [R](https://www.r-project.org/), Matlab, and RATS.

## Replication instructions for VAR models

### Download and run code
1. Download the [IMF-IFS](https://data.imf.org/?sk=4c514d48-b6ba-49ed-8ab9-52b0c1a0179b) full dataset and the relevant variables for Argentina available in [UCEMA-CEA](https://ucema.edu.ar/cea).
2. Save this data in your desktop in  `Desktop/Data` subfolder. 
3. Follow instructions in the [master code](Code/00_master_run.R).

### Replication instructions for Unit Root Tests (Table A4)
1. Download files 20230221 Unit Root Results.prg, uradf.src, mackinnoncv.src, and Cointegration_Data (Rats).xlsx. 
2. Run 20230221 Unit Root Results.prg in Rats.

### Replication instructions for Cointegration Analysis (Tables 3 and A5)
1. Download files in folder Cointegration Analysis.
2. Run driver.m in Matlab.
