# Achieving real-time online monitoring for conservation culturomics

### Introduction

This repo contains a set of scripts for a near real-time version of the Species Awareness Index (SAI), currently hosted online here (https://joemillard.shinyapps.io/Real_time_SAI/). For the full papers on which the SAI is based please see Millard et al. (2021) and Johnson et al. (2023)

------------

### Structure

This repo contains three main code directories: /python contains the script that runs each month (currently on the 4th of each month) via the .bat script at the root of the project directory. This Python script downloads all the views from the previous month, and then executes a set of R scripts that derive the SAI; scripts at the base of /R contain the SAI pipeline that derives the SAI each month; /R/app contains all the code and files used to build the Shiny app, with the exception of any AWS keys.

------------

### References

Johnson, T.F., Cornford, R., Dove, S., Freeman, R. and Millard, J., 2023. Achieving real-time online monitoring for conservation culturomics. Conservation Biology. DOI: https://doi.org/10.1111/cobi.14096

Millard, J., Gregory, R.D., Jones, K.E. and Freeman, R., 2021. The species awareness index as a conservation culturomics metric for public biodiversity awareness. Conservation Biology. DOI: https://doi.org/10.1111/cobi.13701

------------

### Acknowledgements

We are very grateful to the following individuals and organisations who have worked with us and/or kindly shared their data: The Wikimedia Foundation for making Wikipedia view data available via APIs; Yan Wong & James Rosindell (Onezoom) for supplying the taxonomic reference data that underpins the linking of pages among Wikipedia languages. Thanks also to the RPSB and NERC who funded the initial development of the SAI.

------------

### License

With the exception of our publication with Conservation Biology, all code and data are subject to copyright. Please see our terms of data reuse online here (https://joemillard.shinyapps.io/Real_time_SAI/_w_f103eae4/SAI_data_agreement_2022.pdf)