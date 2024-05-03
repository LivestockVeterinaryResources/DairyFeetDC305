# Farm_Data_Analysis

Code to do data analysis from DairyComp Lameness Data

### Steps to run the code

1.  Download data from DairyComp using the commands below

    1.  This works best if the farm is using the lameness manager module in Dairycomp.

    2.  It will still work if they are not using lameness manager it will just require more work checking lesion classification

    3.  in file name replace farm with the name of the farm.

2.  Run the 01_single_import.R file.

    1.  Change the variables in the setup section

        1.  Where it ask for DC item names do not capitalize them.

        2.  where it says filename1 type the name you used for farm above

    2.  known issues include the need to change the column number on the lame and dry file with empty data

    3.  The first time you run a farm you will need to check the lameness remarks and how they are classified in lines 170 (recode section) of the code in the lame dataframe.
        The code is written to account for many ways people write things in remarks but it is not fool proof and requires checking.

    4.  If multiple site location in farms and not using something like PSTRG to separate out farms can use code in line 269 (site section ) to assign sites to where lameness occurred

        1.  Not used in template code other than for leg table but it is possible to facet things by site

3.  Open the 02_setup_for_analysis_single.R file and update the set up data section with herd specific info.

    1.  This is done in this file and repeated from 01 as for some herds with large files running the 01_import file takes too much time.

4.  Then open the QMD (Template Lameness Report) file and run the report.
    This should all run automatically

    1.  Adjust dairy name

    2.  know issues include needing to delete certain lesions from the 05_lesion repeat file if not enough repeats of a certain lesion occur

    3.  Adjust text and captions as required in the report.
        Currently the report contains explanations of the report in the Intro and for some of the sections that I adjust to summarize findings.

    4.  For the majority of reports like DIM, season and culling it is possible to create reports for each specific lesion.
        To do so just copy the specific file and in the first few lines of code their will be notes about what lesion is being used.
        Adjust as needed and insert into the report.

## Inputs:

the following DC commands to get the 5(s1826) years of data.

When exporting data in the filename replace farm with an abbreviation for farm.

filename: farm_lame command: EVENTS ID PEN BDAT FDAT ARDAT LACT CBRD FOOTRIM LAME\\2S1826

filename: farm_dry_cull command:EVENTS ID PEN CBRD BDAT FDAT LACT FTRIM SOLD DIED DRY\\2S1825

In the farm_dry_cull file the FTRIM item might also be FTDAT.
It is the date of the lame event.
If lameness manager is not set up this will not give accurate data.
Currently the FTRIM/FTDAT variable is not really used so it is ok to leave it out.

For farms that use multiple sites in the same DC file you will need to add the site identifier to each command and export each file individually for each site.
for example:

ADD FOR PSTRG=1 to all commands or site="TWIN"

For some other farms the PEN item allows the identification of location but this does not work to get denominator data

For some herds in MN the above is EVENTS ID BIRTH FRSH ARDAT LACT BREED XLAME LXLAM FOOTRIM LAME\\2S1825 EVENTS ID BREED BIRTH FRSH LACT FTRIM XLAME LXLAM LMREM FTREM SOLD DIED DRY\\2S1826

## To get denominators for rates

farm_all command:EGRAPH LAME FOOTRIM'\\'W0D1826z

filename: farm_lact1 command: EGRAPH LAME FOOTRIM'\\'W0D1826z for lact=1

filename: farm_lact2 EGRAPH LAME FOOTRIM'\\'W0D1826z for lact=2

filename: farm_lact3 EGRAPH LAME FOOTRIM'\\'W0D1826z for lact=3

filename: farm_lact4p EGRAPH LAME FOOTRIM'\\'W0D1826z for lact\>3

## Outputs a summary report contain the following Headings

-   Introduction

-   Timing of Trimming

-   Time to Lesion after Routine Trimming

-   Lesion Comparison

    -   What it is the success rate of lesion

-   DIM Distribution of Infectious lesions across lactations

-   DIM Distribution of Non-Infectious lesions across lactations

-   Impact of Season on Infectious Lesions

-   Impact of Season on Non Infectious Lesions

-   Time to next lesion after 1st Non-infectious lesion

-   Survival of cows after treatment for lesions

    -   Infectious Graphs and Tables

    -   Non Infectious Graphs and Tables

**Note:**

For systems it is possible to create a similar report to compare systems and this code is found in a the multiple sites repository

*Code was written by Dr. Gerard Cramer from the University of Minnesota and Dairy Cow Foot Doc. Please contact him at gerard\@dairycowfootdoc.com or make a request in GitHub if you notices errors*

### Future plans include

-   Create a pre render file so can use name of the dairy set in 02 as subtitle

-   Add n's to lesion graph to get a sense of total \# of cases

-   Add code to graph breakdown lesions by lactation

-   more automation of some of the repetitive code

-   create if statements for the lesion repeat file so no need to manually delete lesions that don't repeat 3-4 times.

-   the development of a Shiny App

-   multivariable modeling of some of the survival data
