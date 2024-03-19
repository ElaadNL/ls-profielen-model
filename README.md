# ls-profielen-model
The model used to generate the low-voltage-profiles for EVs or Charging Stations.
Example usage of the model is given in `example.rmd`.

## `model.R`
The main simulation code is defined in `model.R`.
The entry point is the `simulate` function, which runs the simulation.

## `capacity_fractions.R`
Code for the generation of capacity fractions is given in `capacity_fractions.R`.
A capacity fraction is related to the charging point capacity as follows:

$$\text{capacity fraction} = \frac{\text{allowed capacity} - \text{base capacity}}{\text{max capacity} - \text{base capacity}}$$

Capacity fractions are used to determine the charging point capacity when using smart charging.
