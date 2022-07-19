# script for retrieving views from IUCN species object and random page object

import requests
import time
import pandas as pd
import numpy as np
from pandas.io.json import json_normalize
import datetime
from datetime import date
from subprocess import Popen, PIPE

if __name__ == "__main__":
	###
	# Set up and data prep

	# set parameters for random pages and sleep
	no_pages = 11000
	sleep_period = 0.15

	# languages for views
	languages = ['en', 'zh', 'fr', 'de', 'es', 'ru', 'pt', 'it', 'ar', 'ja']

	# taxa of interest (including random)
	taxa_ls = ['ACTINOPTERYGII', 'AMPHIBIA', 'AVES', 'INSECTA', 'MAMMALIA', 'REPTILIA', 'RANDOM']

	# Load and prepare wikipedia page info
	pages = pd.read_csv("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/data/all_iucn_titles.csv")
	random_pages = pd.read_csv("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/data/random_pages_11000.csv")

	# Subset spp pages to languages of interest
	wiki_langs = [lang + "wiki" for lang in languages] 
	pages = pages.loc[pages.site.isin(wiki_langs) , ]
	pages = pages.drop_duplicates(subset = ["q_wikidata", "site"]).reset_index(drop = True)
	pages["wiki_lang"] = pages.site.str.replace("wiki", "")

	# Important pages columns: class_name, wiki_lang, title, q_wikidata
	# Prepare column names of random pages to match spp pages
	random_pages = random_pages.loc[random_pages.language.isin(languages) , ]
	random_pages = random_pages.rename(columns = {'wiki_title':'title', 'wiki_id':'q_wikidata'})
	random_pages = random_pages.drop_duplicates(subset = ["q_wikidata", "language"]).reset_index(drop = True)
	random_pages["class_name"] = "RANDOM"
	random_pages["wiki_lang"] = random_pages.language

	# subset the random pages for the number required - currently just selects all the random pages
	random_pages = random_pages.groupby(["language"]).head(no_pages)

	## Combine spp pages with random pages
	all_pages = pd.concat([pages[["class_name", "wiki_lang", "title", "q_wikidata"]], random_pages[["class_name", "wiki_lang", "title", "q_wikidata"]]]).reset_index(drop=True)
	all_pages[["class_name", "wiki_lang", "title", "q_wikidata"]]

	# Get dates of views needed 
	today = date.today()
	today_f = today.strftime("%Y%m%d")

	yday = today - datetime.timedelta(days=1)
	yday_f = yday.strftime("%Y%m%d")

	# Get end and start date of previous month
	last_month_end = today.replace(day=1) - datetime.timedelta(days=1)
	last_month_strt = last_month_end.replace(day=1)

	last_month_end_f = last_month_end.strftime("%Y%m%d")
	last_month_strt_f = last_month_strt.strftime("%Y%m%d")

	## Load current sai from the aws
	overall_SAI = pd.read_csv("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/outputs/overall_2.csv")
	overall_SAI["Year_alt"] = overall_SAI.Year.replace("-", "", regex=True).astype(int)     

	# Get last month of SAI monitoring data
	dt_last = datetime.datetime.strptime(str(max(overall_SAI.Year_alt)), "%Y%m%d")

	# Get first day of next month (comared to SAI end) 
	dt_next = (dt_last.replace(day=1) + datetime.timedelta(days=32)).replace(day=1)
	dt_next_f = dt_next.strftime("%Y%m%d")

	# if last month in SAI data download does not match last month based on current data, 
	# get all data since last sai download
	if last_month_strt != dt_next:
		last_month_strt_f = dt_next_f
		
	### Main Code
	# Get session for url query
	S = requests.Session()

	headers = {"User-Agent": "species_awareness_index/0.0 (https://joemillard.github.io/; joseph.millard@nhm.ac.uk) generic-library/0.0"}

	# For each language and taxa
	for lang in languages:
		print(lang)
		for taxa in taxa_ls:
			print(taxa)
			# Subset to focal language and taxa
			pages_tmp = all_pages.loc[(all_pages.wiki_lang == lang) & (all_pages.class_name == taxa ),].reset_index(drop = True)
			taxa_string = taxa.lower()

			# create result object
			result = []
			
			# for each page
			for i in range(0, pages_tmp.shape[0]):
				if i% 100 == 0:
					 print(i)
				try:
					title = pages_tmp.title[i]
					title = title.replace(" ", "_")
					tmp_q_wiki = pages_tmp.q_wikidata[i] 
					
					# Retrieve page views for URL
					URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/%s.wikipedia.org/all-access/user/%s/daily/%s/%s" % (lang, title, last_month_strt_f, last_month_end_f)

					R = S.get(url = URL, headers = headers)

					# clean up json file
					DATA = R.json()
					DATA = DATA['items']
					DATA = json_normalize(DATA)
					DATA['q_wikidata'] = tmp_q_wiki 

					## summarise by month and q_wikidata
					# first, extract month and year from timestamp
					DATA["dt_obj"] = pd.to_datetime(arg = DATA.timestamp, format = "%Y%m%d%H")
					DATA["year"] = DATA.dt_obj.dt.year
					DATA["month"] = DATA.dt_obj.dt.month

					DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'])['views'].agg(['mean']).rename(columns={'mean': 'av_views'}).reset_index()

					# Add summarised view data to results object
					result.append(DATA_summ)

					time.sleep(sleep_period)

				# in event that request is made, but no data returned e.g. name of page has changed
				except AttributeError:

					df = pd.DataFrame({'article': [title],'year': [''], 'month': [''], 'q_wikidata': [tmp_q_wiki], 'av_views': [np.nan]})

					result.append(df)
					print(title, "attributeerror")

				# in event of error retrieving views, insert row with article title e.g. odd characters in request
				except KeyError:

					df = pd.DataFrame({'article': [title],'year': [''], 'month': [''], 'q_wikidata': [tmp_q_wiki], 'av_views': [np.nan]})
					
					result.append(df)
					print(title, "keyerror")

				# in event that blocked from api, wait for 5 minutes before rretrying
				except IOError:
					time.sleep(300)

					# then retry
					try:
						# Retrieve page views for URL
						R = S.get(url = URL, headers = headers)
						
						# clean up json file
						DATA = R.json()
						DATA = DATA['items']
						DATA = json_normalize(DATA)
						DATA['q_wikidata'] = tmp_q_wiki 

						## summarise by month and q_wikidata
						DATA["dt_obj"] = pd.to_datetime(arg = DATA.timestamp, format = "%Y%m%d%H")
						DATA["year"] = DATA.dt_obj.dt.year
						DATA["month"] = DATA.dt_obj.dt.month

						DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'])['views'].agg(['mean']).rename(columns={'mean': 'av_views'}).reset_index()
						
						# Add summarised view data to results object
						result.append(DATA_summ)

						time.sleep(sleep_period)

					# in event that request is made, but no data returned e.g. name of page has changed
					except AttributeError:

						df = pd.DataFrame({'article': [title],'year': [''], 'month': [''], 'q_wikidata': [tmp_q_wiki], 'av_views': [np.nan]})

						result.append(df)
						print(title, "keyerror")

					# in event of error retrieving views, insert row with article title
					except KeyError:

						df = pd.DataFrame({'article': [title], 'year': [''], 'month': [''], 'q_wikidata': [tmp_q_wiki],'av_views': [np.nan]})

						result.append(df)
						print(title, "keyerror")

					# in event that blocked from api, insert error row 
					# only if 2 connection errors do we skip row...
					except IOError:
						
						df = pd.DataFrame({'article' : [title], 'year': [''], 'month': [''], 'q_wikidata': [tmp_q_wiki],'av_views': [np.nan]})
						
						result.append(df)
						print(title, "connect_fail")
						time.sleep(300)

			# if the taxa groups is not random, needs to be saved in the species_view folder
			if taxa_string != "random":

				# concatenate all appended results to dataframe and write to csv for each subset
				final = pd.concat(result).reset_index(drop = True)
				save_loc = 'C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/data/real_time_views/species_views/%s_%s_user_trends_%s_%s.csv' % (lang, taxa_string, last_month_strt_f, last_month_end_f) # Home PC
				final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')

			# if the taxa is the random group, needs to be saved in the random_views folder
			else:
				# concatenate all appended results to dataframe and write to csv for each subset
				final = pd.concat(result).reset_index(drop = True)
				save_loc = 'C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/data/real_time_views/random_views/%s_%s_user_trends_%s_%s.csv' % (lang, taxa_string, last_month_strt_f, last_month_end_f) # Home PC
				final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')  

# run command for calling R from Python, with error capture -- 03_derive_species_raw_trends.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/03_derive_species_raw_trends.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))

# run command for calling R from Python, with error capture -- 04_derive_random_raw_trends.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/04_derive_random_raw_trends.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))

# run command for calling R from Python, with error capture -- 04a_bootstrap_overall_random.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/04a_bootstrap_overall_random.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))

# run command for calling R from Python, with error capture -- 05_class_language_SAI.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/05_class_language_SAI.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))

# run command for calling R from Python, with error capture -- 06_taxa_language_SAI_model.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/06_taxa_language_SAI_model.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error)) 

# run command for calling R from Python, with error capture -- 07_class_SAI.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/07_class_SAI.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))

# run command for calling R from Python, with error capture -- 08_overall_SAI.R
cmd = ["C:/Program Files/R/R-4.1.2/bin/Rscript", "C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/R/08_overall_SAI.R"]
p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
output, error = p.communicate()
		  
print('R OUTPUT:\n {0}'.format(output))                            
print('R ERROR:\n {0}'.format(error))
