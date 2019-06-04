# gc2019underc
## Processing GC data for the 2019 field season 
### Written by Brittni Bertolet

#### First, batch process all the current data using the ChemStation software
Open terminal, and navigate to the batchProcessed_YYMMDD folder. 
Run the following command to execute the for loop below:

bash ../../code/convertToUTF16.sh

#### Open the Rstudio Project
Run the calculatingPPM_UNDERC2019.R script. Don't forget to change the working directory!!!

#### Concatonate all the new run files to the current DB
for file in 19*
do
cat $file | sed 1d >> ../../currentDB/GCdatabase2019.csv
done 
