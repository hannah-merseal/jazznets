# jazznets
Data cleaning &amp; analysis pipeline for jazznets project

## Directory

### Task
Semantic choice task for node pairs extracted from the Weimar Jazz Database. Also hosted on Gitlab for deployment to Pavlovia.

### Functions
1. JNfilerename - mass convert participant files to participant IDs. DO NOT use unless at very beginning of project - will delete all prior files in directory.

2. JNtidy - rearranging data from messy PsychoPy output to tidy format.

3. JNstats - filtering trials and participants with poor performance.

4. cleanup - cleanup pipeline + calculating means and creating a master file of all trials

5. plotmeans - plotting means using ggplot2

6. JNanovas - scripts for repeated measures ANOVA and post-hoc analysis

### Pilot Data
*Participant data files are not stored in this repository; you will need to save the raw data locally!*

### Pilot Results
Results document, plots

###Results
Main analysis results for Experiment 1, plots
