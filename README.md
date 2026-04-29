# digital-payments-financial-inclusion
Data cleaning, quality investigation and analysis of digital payment adoption across Indian states using R

## Project Overview
Analysis of digital payment adoption across Indian states using 
6 raw data sheets from RBI sources. Cleaned, analysed, and 
visualised transaction data to identify high-population, 
low-adoption districts for targeted intervention.
Data from ai agents - scratch

## Tools Used
- R (dplyr, ggplot2, readxl, writexl)
- Excel (data quality documentation)
- Power BI (final dashboard)

## Key Findings
- Identified X districts with population density > 500 
  but digital adoption < 20%
- Districts like Sheohar (Bihar) and Bahraich (UP) flagged 
  as priority targets for digital inclusion programmes

## Files
- data/ — raw and cleaned datasets
  Digital_Payments_Raw_Data_6Sheets (1) - Main raw data excel file
  -Has 6 sheets and 1 extra Data quality log sheet 
  -Sheet1,Sheet2,Sheet3,Sheet4,Sheet5,Sheet6 - 6 sheets cleaned
  -df_combined - All 6 sheets combined
  -df_clean -All 6 sheets combined and cleaned  
- scripts/ — R cleaning and analysis code
- visuals/ — exported charts
  -adoption_by_state.png
  -high_pop_low_adopt.png

## How to Run
1. Open Analysis_cleaning.R in RStudio
2. Set working directory to project folder
3. Run the script top to bottom
