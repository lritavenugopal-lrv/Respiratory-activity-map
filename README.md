# Respiratory-activity-map
How to update the respiratory activity level map on the website
# Respiratory-activity-map

## How to Update the Respiratory Activity Level Map on the Website

### Respiratory Activity Level Mapping

This repository contains R code and supporting datasets used to generate weekly respiratory activity level maps. The data sources include:

### Download

Download 'Respiratory activity map' folder into your working R directory 

### ESSENCE Data *(All based on percent of ED visits)* for:
- Influenza-like Illness (ILI) - ILI CCDD Neg Coronavirus DD v1
- COVID-like Illness (CLI) - CLI CC with CLI DD and Coronavirus DD v2
- Respiratory Syncytial Virus (RSV)  -  CDC Respiratory Syncytial Virus v1


### NBS Data   *(Case counts)* for:
- Pertussis  

---

### Data Preparation Instructions

#### ESSENCE Data (ILI, CLI, RSV)
- Download weekly data from ESSENCE (based on percent of CC and DD category). The data need to be downloaded as a data table, statified by counties.
- Include atleast an year of data for effective calculation of EWMA. 
- Open the Excel file and **delete the first two rows** (descriptive metadata).
- Rename the column containing week information to **`date`**.
- Save the cleaned file in your working R project directory as **`covidlevels, flulevels and rsvlevels`**.

#### Pertussis Data (NBS)
- Manually download the pertussis data from NBS.
- Rename the column **`Report Date`** to **`date`**.
- Save the file as **`data.xlsx`** in your working R project directory.
- Run the script **`Pertussis Data.R`** to process the file.
- The output will be saved as **`pertussislevels.xlsx`**, which is used in the mapping workflow.

