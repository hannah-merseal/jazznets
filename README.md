# jazznets
Data cleaning &amp; analysis pipeline for jazznets project

## Directory
### Functions
1. JNfilerename - mass convert participant files to participant IDs. DO NOT use unless at very beginning of project - will delete all prior files in directory.

2. JNtidy - rearranging data from messy PsychoPy output to tidy format.

3. JNstats - filtering trials and participants with poor performance.

4. cleanup - cleanup pipeline + calculating means and creating a master file of all trials

5. plotmeans - plotting means using ggplot2