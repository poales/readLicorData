# readLicorData
R code to read li-6800 gas exchange and fluorescence

# Features
- Convert either excel (.XLSX) or text (extensionless TSV) into tidy data for further analysis.
- Optionally move your comments into rows
- Track changes in leaf area or oxygen constants and convert them into a column.
- Functions to read in fluorescence traces, both single flash and long duration traces (RCRDG files)

# Installation
devtools::install_github("poales/readLicorData")
