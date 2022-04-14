# script for retrieving views from IUCN species object and random page object

import requests
import time
import pandas as pd
import numpy as np
from pandas.io.json import json_normalize
import datetime
from datetime import date
from subprocess import Popen, PIPE

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

if __name__ == "__main__":
	###
	# SET UP

	# set parameters for random pages and sleep
	no_pages = 11000
	sleep_period = 0.15

	# languages for views
	languages = ['en', 'zh', 'fr', 'de', 'es', 'ru', 'pt', 'it', 'ar', 'ja']

	#taxa of interest
	taxa_ls = ['ACTINOPTERYGII', 'AMPHIBIA', 'AVES', 'INSECTA', 'MAMMALIA', 'REPTILIA']

	# Load and prepare page info
	### EDIT FILEPATH
	pages = pd.read_csv("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/all_iucn_titles.csv")

	# only if site in languages + 'wiki'
	wiki_langs = [lang + "wiki" for lang in languages] 

	pages = pages.loc[pages.site.isin(wiki_langs) , ]
	pages = pages.drop_duplicates(subset = ["q_wikidata", "site"]).reset_index(drop = True)

	pages["wiki_lang"] = pages.site.str.replace("wiki", "")

	# Get dates of views needed 
	today = date.today()
	today_f = today.strftime("%Y%m%d")

	yday = today - datetime.timedelta(days=1)
	yday_f = yday.strftime("%Y%m%d")

	last_month_end = today.replace(day=1) - datetime.timedelta(days=1)
	last_month_strt = last_month_end.replace(day=1)

	last_month_end_f = last_month_end.strftime("%Y%m%d")
	last_month_strt_f = last_month_strt.strftime("%Y%m%d")

	# # Load current sai
	### EDIT FILEPATH
	overall_SAI = pd.read_csv("C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/real_time_SAI/overall_SAI_2022_04_11.csv")
	overall_SAI.Year[1]
	overall_SAI["Year_alt"] = overall_SAI.Year.replace("-", "", regex=True).astype(int)     

	# Get last month of data
	dt_last = datetime.datetime.strptime(str(max(overall_SAI.Year_alt)), "%Y%m%d")

	# Get first day of next month...
	dt_next = (dt_last.replace(day=1) + datetime.timedelta(days=32)).replace(day=1)

	dt_next_f = dt_next.strftime("%Y%m%d")

	## verify last month in sai data download matches last month based on current data
	##
	# if last_month_strt == dt_next:
		# last_month_strt = last_month_strt

	# if not a match, get all data ince last sai download
	if last_month_strt != dt_next:
		last_month_strt_f = dt_next_f

	###
		
	### Main Code

	# Get session for url query
	S = requests.Session()

	headers = {"User-Agent": "species_awareness_index/0.0 (https://joemillard.github.io/; joseph.millard@nhm.ac.uk) generic-library/0.0"}

	for lang in languages:
		print(lang)
		for taxa in taxa_ls:
			print(taxa)
			pages_tmp = pages.loc[(pages.wiki_lang == lang) &
										(pages.class_name == taxa ),].reset_index(drop = True)
			taxa_string = taxa.lower()

			# write to result object
			result = []
			# actual code...
			for i in range(0, pages_tmp.shape[0]):
			#     print(i)
			# testing
			# for i in range(0, 1000):
				# print(i)
				# add counter for multiples of 100
				if i% 100 == 0:
				     print(i)
				try:
					# assign title at iteration to object, replace spaces, and retrieve views for URL
					title = pages_tmp.title[i]#taxa[j]['title'][i]
					title = title.replace(" ", "_")
					tmp_q_wiki = pages_tmp.q_wikidata[i] 
					
					# all needed data to end of previous month
					URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/%s.wikipedia.org/all-access/user/%s/daily/%s/%s" % (lang, title, last_month_strt_f, last_month_end_f)

					R = S.get(url = URL, headers = headers)
					# Check R status code
					# R.status_code
					# R.ok (ie., is status_code < 400)

					# clean up json file, append column for taxa object, and append to results[]
					DATA = R.json()
					DATA = DATA['items']
					DATA = json_normalize(DATA)
					DATA['q_wikidata'] = tmp_q_wiki #taxa[j]['q_wikidata'][i]

					## summarise by month and q_wikidata
					# first, extract month and year from timestamp
					DATA["dt_obj"] = pd.to_datetime(arg = DATA.timestamp, format = "%Y%m%d%H")
					DATA["year"] = DATA.dt_obj.dt.year
					DATA["month"] = DATA.dt_obj.dt.month

					# DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'], as_index=False).agg({"views" : "mean"})
					DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'])['views'].agg(['mean']).rename(columns={'mean': 'av_views'}).reset_index()

					# test with NA...
					# DATA_na = DATA.copy()
					# DATA_na.views[0] = np.nan
					# DATA_na.groupby(['article', 'year', 'month', 'q_wikidata'])['views'].agg(['mean']).rename(columns={'mean': 'av_views'}).reset_index()
					# NA are removed by default...

					# DATA.group
					result.append(DATA_summ)

					time.sleep(sleep_period)

				# in event of error retrieving views, insert row with article title
				except KeyError:

					df = pd.DataFrame({'article'     : [title], 
										'year'       : [''], 
										'month'      : [''], 
										'q_wikidata' : [tmp_q_wiki],
										'av_views'   : [np.nan]})
					# df = pd.DataFrame({'':[''],'access':[''], 'agent':[''], 'title':[title], 'granularity':[''], 'project':[''],'timestamp':[''], 'views':[np.nan], 'q_wikidata':[tmp_q_wiki]})
					result.append(df)
					# write error row to file and don't append
					print(title, "error")

				# in event that blocked from api, insert error row and wait for 5 minutes before reconnecting
				except IOError:
					# hmmm should retry here!!
					# wait 5 minutes...
					time.sleep(300)

					# then retry
					try:
						# assign title at iteration to object, replace spaces, and retrieve views for URL
						# title = pages_tmp.title[i]#taxa[j]['title'][i]
						# title = title.replace(" ", "_")
						# tmp_q_wiki = pages_tmp.q_wikidata[i] 
						
						# # all needed data to end of previous month
						# URL = "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/%s.wikipedia.org/all-access/user/%s/daily/%s/%s" % (lang, title, last_month_strt_f, last_month_end_f)

						R = S.get(url = URL, headers = headers)
						# Check R status code
						# R.status_code
						# R.ok (ie., is status_code < 400)

						# clean up json file, append column for taxa object, and append to results[]
						DATA = R.json()
						DATA = DATA['items']
						DATA = json_normalize(DATA)
						DATA['q_wikidata'] = tmp_q_wiki #taxa[j]['q_wikidata'][i]

						## summarise by month and q_wikidata
						DATA["dt_obj"] = pd.to_datetime(arg = DATA.timestamp, format = "%Y%m%d%H")
						DATA["year"] = DATA.dt_obj.dt.year
						DATA["month"] = DATA.dt_obj.dt.month

						# DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'], as_index=False).agg({"views" : "mean"})
						DATA_summ = DATA.groupby(['article', 'year', 'month', 'q_wikidata'])['views'].agg(['mean']).rename(columns={'mean': 'av_views'}).reset_index()
						
						result.append(DATA_summ)

						time.sleep(sleep_period)

					# in event of error retrieving views, insert row with article title
					except KeyError:

						df = pd.DataFrame({'article'     : [title], 
											'year'       : [''], 
											'month'      : [''], 
											'q_wikidata' : [tmp_q_wiki],
											'av_views'   : [np.nan]})
						# df = pd.DataFrame({'':[''],'access':[''], 'agent':[''], 'title':[title], 'granularity':[''], 'project':[''],'timestamp':[''], 'views':[np.nan], 'q_wikidata':[tmp_q_wiki]})
						result.append(df)
						# write error row to file and don't append
						print(title, "error")

					# in event that blocked from api, insert error row and wait for 5 minutes before reconnecting
					except IOError:
						# only if 2 connection errors dow we skip row...
						df = pd.DataFrame({'article'     : [title], 
											'year'       : [''], 
											'month'      : [''], 
											'q_wikidata' : [tmp_q_wiki],
											'av_views'   : [np.nan]})
						# df = pd.DataFrame({'':[''],'access':[''], 'agent':[''], 'title':[title], 'granularity':[''], 'project':[''],'timestamp':[''], 'views':['connect_fail'], 'q_wikidata':[tmp_q_wiki]})
						result.append(df)
						# write error row to file and don't append
						print(title, "connect_fail")
						time.sleep(300)

					
			# concatenate all appended results to dataframe and write to csv for each subset
			final = pd.concat(result).reset_index(drop = True)
			# taxa_level = taxa_string #s[j]
			### EDIT FILEPATH
			save_loc = 'D:/wikipedia_views/%s_%s_user_trends_%s_%s.csv' % (lang, taxa_string, last_month_strt_f, last_month_end_f) # Home PC
			# print(save_loc)
			# save_loc = ('C:/Users/Joseph Millard/Documents/PhD/Aims/Aim 3 - quantifying pollinator cultural value/wikipedia_target-1-metric/data/class_wiki_indices/submission_2/user_trends/%s%s_user_trends.csv') % ((languages[l] + '_'), (taxa_level + '_')) # CBER PC
			final.to_csv(save_loc, sep = ',', encoding = 'utf-8-sig')


