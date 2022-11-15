# Projeto de Data Mining I 

##Forest Fires in Portugal
###Description
Forest fires are a critical issue that negatively affects climate change. The causes of forest fires are those oversights, accidents and negligence committed by individuals, intentional acts and natural causes. The latter is the root cause for only a minority of the fires.

Their harmful impacts and effects on ecosystems can be major ones. Among them, we can mention the disappearance of native species,  the increased levels of carbon dioxide in the atmosphere, the earth’s nutrients destroyed by the ashes, and the massive loss of wildlife. 

Data mining techniques can help predict the cause of the fire and, thus, better support the decision to take preventive measures to avoid tragedy. This can significantly affect resource allocation, mitigation and recovery efforts. 

The ICFN - Nature and Forest Conservation Institute has a record of the forest fires that occurred in Portugal for several years. For each fire, there is information such as the site, the alert date/hour, the extinction date/hour, the affected area and the cause type (intentional, natural, negligent, rekindling or unknown).

In the file fires_train.csv, you have data on reported forest fires during 2014 and 2015, for which the cause is known. The attributes have information regarding the forest fire’s alarm point and the total affected area:

This practical assignment aims to build a machine learning model to predict the cause type of a forest fire: intentional or non-intentional.

As additional information, you can choose to use weather data. The R package climate provides a tool for scrapping meteorological data from the Ogimet website regarding a period and a specific station. You can choose the get the meteorological data reported at the nearest station for the fire. In getMeteoData.R, you can find an example for retrieving such information. However, you should be aware that new stations appeared during the period. You can also choose to fetch meteorological information from other sources.

###Tasks
Using the above data set, you have a set of main tasks to accomplish as described next. Still, you are free to include other tasks to increase the value of your assignment.  

####Task 1: Data Understanding and Preparation
This task involves summarizing and visualizing the data to provide valuable insights. Consider questions that could be interesting to check with the available data and provide answers using textual summaries or data visualization. Based on this analysis, you should also check if it is necessary to carry out any data clean-up and pre-processing steps.

####Task 2: Predictive Modelling
From the available data, you should define the data set used for the classification task at hand. Different models should be considered, and the choice of the final model should be justified. 

####Task 3: Kaggle Competition
Additionally, you should submit your solution for the fires_test.csv data set to the Kaggle Competition (to be open soon). Your rank will be accounted for in the final grade. 

####Extra Task: Descriptive Modelling
This task aims to apply a clustering algorithm on a set of variables that you find helpful to provide some description of the forest fires that occurred. 


###Tools
You can use R or Python. You can find material for dynamic reporting in R with markdown if you choose to use R. You can use the Colab Research Notebooks if you decide to use Python. 


###Deliverables
The practical assignment is mandatory and should be performed by groups of, preferably, three students. You should constitute your group until next November 18th, 2022. After this date, no more groups are accepted.

Your assignment should be submitted on moodle with a compressed file containing the following items:

slides for presentation (PDF format) focusing on the main issues of the project for a 10 minutes presentation; any additional information that cannot be presented in the 10 min slot can be included as annexes to the presentation;
the source of a ready-to-execute dynamic report or notebook with all the code necessary to run to obtain the results you present, including any complementary files needed to execute your report (e.g. data files, data objects).

###Suggested organization:

problem definition (1 slide)
data understanding:  concise summary, focusing on the main findings (2 slides)
data preparation: outline, focusing on the main operations  (2 slides)
descriptive modelling (1 slide) - if performed
predictive modelling with experimental results (2 slides)
conclusions, limitations and future work (1 slide)
annexes
TOTAL: max 40 slides

###Grades
The grading of the practical assignment is distributed as follows: 
- Task 1 (30%)
- Task 2 (30%)
- Task 3 (15%)
- Presentation (25%) - mandatory 
- Extra Task  (15%)


