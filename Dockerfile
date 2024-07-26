#start with out siny-tidy image
FROM rocker/r-ver:latest

#install system dependencies
RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev
   
#install R packages required
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('caret')"
RUN R -e "install.packages('plumber')"

#copy the app to the image
COPY API.R API.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv

#select port
EXPOSE 8000

#run app
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8000)"]