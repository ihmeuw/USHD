# Child Growth Failure Modeling Pipeline

##### Ryan Fitzgerald is contact person

### Overview

In GBD, we model continuous curves of stunting (HAZ), wasting (WHZ), and underweight (WAZ). To accomplish this, we use 3 main modeling steps.

### STGPR Component

In the first step, we use STGPR models to estimate point prevalences of mild (< -1SD), moderate (< -2SD), severe (< -3SD), and extreme (< -4SD) CGF. We also have an STGPR model for each of the mean Z-scores for HAZ, WHZ, and WAZ. At the completion of this step we have estimates for each age/sex/year/location for the area under the curve below -1, -2, -3, and -4 standard deviations as well as estimates of what the mean Z score of the curve is.

### Weight Fitting Component

In the second step, we define the functional shapes of the stunting, wasting, and underweight curves. Here we use all microdata to assess what shapes stunting, wasting, and underweight distributions normally take. We use an optimization algorithm that tries different weighted averages of distribution families to best match the true distribution shapes of HAZ, WAZ, and WHZ microdata. These weighted averages make a curve shape that we use in step 3.

### Curve Fitting Component
Once we know the mean Z score as well as the area under the curve below -1, -2, -3, and -4 SD thresholds from step 1, we can combine this information with the curve shape from step 2. Here, we run an optimization algorithm that tries to find the optimal standard deviation for each age/sex/year/location to make a curve of that shape align best with the STGPR estimates. Integrating area below the curve yields final CGF results.