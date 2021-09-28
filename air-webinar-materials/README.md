**Sentiment Analysis in R - Webinar Materials**

This directory contains the slides, data, and code for webinar: Sentiment Analysis in R, presented for the Association for Institutional Research by Jenn Schilling on October 20 & 22 2021.  


**Webinar Details** 

This webinar will teach participants how to complete a text analysis in R, including data processing and cleaning, visualization of word frequencies, and sentiment analysis. Sentiment analysis is useful for finding patterns in text data from open response questions on surveys and course evaluations as well as evaluating social media posts. This series is ideal for higher education professionals who have some experience in R and want to add text analysis to their R skills.

As a result of this webinar, participants will be able to: 
- Prepare data for a text analysis in R. 
- Conduct text mining in R.
- Complete sentiment analysis in R. 


**Folder Structure**

- code: Contains an R Markdown document that loads the data, processes the data, and completes the text analysis.

- data: Contains the CSV data file of tweets about the University of Arizona.



**How to Use These Files**

1. Save all three folders and the *.here* file to the same directory on your computer.

2. Open the R Markdown document in the *code* folder in RStudio.

3. Run the setup code chunks to load the libraries and read the data. Then run the rest of the code chunks in sequence to process the data and complete the text analysis. 

Note: The three libraries used are `here`, `tidyverse`, and `tidytextr`. If you do not have these packages installed, be sure to complete the installation prior to running the code by running `install.packages(c('here', 'tidyverse', 'tidytext'))` in the Console of RStudio. 


