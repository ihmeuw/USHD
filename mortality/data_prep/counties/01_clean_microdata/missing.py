##-----------------------------------------------------------------------------------------------------------
## Description: After the raw data has been prepped, there should be consistent codes for missingness across
##				      datasets. This script tabulates that missingness for any given year, across most relevant
##			      	columns in the dataframe.
## Input:  --df: a pandas dataframe output by the codeath.py script.
## Output: --final_missingness: a small pandas dataframe tabulating percent missingness in df for every 
##			     column of df
##-----------------------------------------------------------------------------------------------------------


from __future__ import division
import pandas as pd 
import numpy as np


def calc_missingness(df):
	df = df.replace('9', np.nan)
	df = df.replace('99', np.nan)
	df = df.replace('999', np.nan)
	try:
		df['industry'] = df['industry'].replace('51', np.nan)
		df['occupation'] = df['occupation'].replace('59', np.nan)
	except:
		pass

	number_entries = len(df)

	not_missing = df.count()
	name_missing = df.astype(object).fillna('Missing')

	miss_index = name_missing.keys()
	miss_columns = ['number_entries', 'percent_missing']

	final_missingness = pd.DataFrame(index = miss_index, columns = miss_columns)

	final_missingness['number_entries'] = number_entries

	for idx in miss_index:
		misslist = name_missing[idx].value_counts()

		try:
			final_missingness['percent_missing'][idx] = misslist['Missing'] / number_entries
		except:
			final_missingness['percent_missing'][idx] = 0

	return final_missingness

