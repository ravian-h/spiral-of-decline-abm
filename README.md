# spiral-of-decline-abm

Agent-Based Model simulating feedback loops in healthcare access inequality.
Built for a thesis for the BSc Artifical Intelligence at Utrecht University.

## Requirements
- NetLogo 7.0.3
- R 4.4.3 with the `effsize` package (`install.packages("effsize")`)

# Running the model
1. Open `spiral-of-decline-abm.nlogox` in Netlogo
2. Select a scenario from the chooser
3. Click Setup, then Go

## Running experiments
1. Open Tools -> BehaviorSpace
2. Select an experiment and click Run
3. Save output as CSV to the same folder as the R script
4. Run the R script to reproduce the analysis

## Data
The CSV data files are not included in this repository.
To reproduce the analysis:
1. Open the model in NetLogo
2. Go to Tools → BehaviorSpace
3. Run each of the four experiments and save the output as CSV:
   - `Experiment 1 - Main`
   - `Experiment 1 - Timeline`
   - `Experiment 2 - Main`
   - `Experiment 2 - Timeline`
4. Place the CSV files in the same folder as `model_analysis.R`
5. Run `model_analysis.R` in R
