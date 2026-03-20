##### CHILD GROWTH FAILURE OTHER FUNCTIONS

###### Ryan Fitzgerald is the contact person about these functions

# CGF Filesystem Organization Functions

- ***create_plot_output_folder*** - You'll run this function before launching any parallel scripts that create CGF pipeline diagnostics. It looks to see if there's an existing folder with today's date already. If not, it creates one so that diagnostics can be saved there. If so (meaning you likely already ran a set of diagnostics today), it creates a new folder with "_2" and so forth so that plots can be written here. This avoids overwriting diagnostic plots and keeps things organized.

- ***prep_tracking_folder*** - This maintains the STGPR tracking folder so that you can see if STGPR models failed during registration or if they failed when the model actually ran. It deletes any existing folders so that there is only ever one tracking directory.


# Curve Creation Functions

- ***create_CDF***, ***create_pdf***, ***integrate_PDF***, ***create_pdf_points*** - These functions are all essentially helper functions for the main functions in the script. This means you shouldn't actually ever be interacting straight with these, and instead they just help the other functions accomplish their goal.
- ***data_to_curves*** - This takes microdata sources and outputs both a CDF and a PDF. This won't be a true continuous curve, but it will be 1,000 points along that line, essentially making it continuous "enough".
- ***data_to_curves_with_underlying*** - This does the exact same thing as the script above, but it also does an extra step. It calculates the underlying curves for each of the 10 distribution families that went into the ensemble, as if they were weighted 100%. 
- ***calculate_curve_know_mean_sd*** - This function is different from the previous two. In this one, you're not supplying microdata, you're suppling a mean and a standard deviation. It also produces a PDF and a CDF in the same format though.
- THINGS TO KNOW ABOUT THESE FUNCTIONS:
	+ The Data argument should be a vector (the column of the dataframe that you want to calculate the curve for, the Z-scores themselves)
	+ The weights argument should be a 1 column dataframe with 10 rows. They'll need to be in the same order as the names in distlist
	+ The nid_loc_yr_i argument is just a way for you to supply an index to the output dataframe for each curve you calculate, so that when you run this in a loop you can differentiate which curves are for which input.
