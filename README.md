# Investigating contagion of conflict through weapons trade

This repo is for the analysis of the relationships between weapons and conflict in a global scale.

## Folder structure

```
weapon_network
│   README.md
└───data
│   └─── raw (raw data for cleaning and merging)
│       │   ...
│   └─── intermediate (data after running code/data/clean.R)
│       │   ...
│   └─── final (data for regression analysis)
│       │   main.csv
│       │   ss_df.csv
│       │   ...
│   └─── paper (documentation of retrieved data)
│       │   UCDP/PRIOD data set
│       │   Brandon Kineer 2016 data set
│   └─── network (output of network analysis)
│       │   ...
│
└───code
│   └─── data (cleaning and constructing data)
│       │   clean.R (cleans data to get main.csv)
│       │   create_ss.R (creates the shift-share variable)
│       │   comtrade.R (uses the comtrade API to get trade data)
│       │   ...
│   └─── reg (regression analysis)
│       │   reg_01.R
│       │   reg_02.R
│       │   ...
│   └─── vis (code for visualising data)
│       │   dynamic_network.R (dynamic network animation of trade volumes)
│       │   plots.R (descriptive plots)
│       │   ...
│   └─── sim (simulation of the equilibrium)
│       │   simulation.R (equilibrium trade volumen depending on parameters)
│       │   ...
│   └─── notebook (testing codes)
│       │   ...
│ 
└───output
│   └─── images (plots and animations)
│       │   ...
│   └─── regression (results from regressions in tex files)
│       │   ...
```

## Running the code

To output all results simply run code/main.R

## Output

In presentation and paper folder there are slides and papers from this repo
