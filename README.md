# third-sample-analysis
Third sample repository with excerpts to highlight some of my quantitative/ coding skills

Background: I worked as part of a team to complete the midterm evaluation for an international client. The client project was about education. Amongst others, the client was interested in whether or not students at project schools exhibited higher levels of self-reported school engagement compared to students control at control schools. The example project presented here contains the latent factor modelling to answer this question. To do so, the lavaan package in R was used. At the time of writing, a major caveat of the package has been that the output tables could not be exported into csv automatically using tools such as stargazer. Thus, some of the statistics produced had to be manually included into the report prepared for the client. 

The entire project consists of the following folders:
01 raw data, which contains raw data pulled into the project. Note, some relevant data may be pulled from other sources using API's.
02 processed data, which contains data manipulated during project work. 
03 scripts, which contains all R scripts to load, clean, and analyse data. The sequence in which to run the files are clearly indicated within the script names.
04 results, which contains all analysis output files such as tables and graphs produced during the analysis. 
05 documentation, which contains the reports that present the analysis results and findings.

The entire project is managed in RStudio. To review and edit the project, open the Rproj-file 'third-sample-analysis', which is located at the top-level of the repository. It opens up the two R-scripts, which are placed in the folder '03 scripts'.

Note: the main bulk of the data is pulled from Kobo toolbox through a live API. Th credentials have been removed from the script '01_data loading & cleaning_20230221_V01'. Thus, running the script will NOT result in pulling in the data.  