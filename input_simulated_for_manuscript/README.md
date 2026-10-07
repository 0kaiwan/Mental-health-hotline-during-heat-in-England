Simulated data generation

This folder contains simulated daily hotline counts and meteorological data for demonstrating the analysis workflow. 
These data should not be interpreted as the original observations or used to reproduce the study’s substantive findings.

The datasets were generated using seasonal block resampling with a fixed random seed (20261007). 
Observations were sampled in blocks of seven days, with source blocks beginning within 28 calendar days of the target date’s position in the year and on the same weekday.
Source blocks could be selected from different years. 
The same source dates were used across meteorological variables and NHS regions to retain their observed relationships within each sampled block.

Hotline counts were resampled using the same source dates where available. 
When a selected source count was missing, an observed count from the same trust and a similar season was sampled, preferentially matching the weekday. 
If no observed count was available within the seasonal window, a count from the closest available calendar season was sampled. 
All original dates, column names, and positions of missing values were retained.

This procedure resamples observed values rather than generating values uniformly from their original ranges. 
It does not explicitly model long term trends or guarantee preservation of the original distributions, correlations, or analytical results. 
Because it reuses observed values and sequences, it does not by itself guarantee anonymisation.
