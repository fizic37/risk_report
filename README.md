
# RiskReport

<!-- badges: start -->
<!-- badges: end -->

The goal of RiskReport is to produce an end of month report (in the form of a word file) which contains complex processed data for risk management. The basic workflow is comprised of uploading data (excel or csv), processing data, output the processed data to the user and saving data. The user will receive proper messages when processing uploaded data is not possible due to other missing files. The saved data is available afterwards for downloading or deletion. User rights are to be developed - a user will only see data and actions that he is entitled to. Since the app is developed with golem framework it is completely modularized and thus future development will be pretty straightforward. Currently the code lacks proper testing which is done internally.

## Installation

You can install the released version of RiskReport from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("RiskReport")
```
## IMPORTANT on installation
Installing the app will not work since the data used is private (stored within R/reactivedata folder) and not available on github. 

## Deplyment to production
Deployment to production is handled with docker image (built with Dockerfile inside the repository) and docker volumes. The docker is ran with the following command: docker run \-d \-it \-p 0.0.0.0:9000:80 \--name RiskReport -v risk_report:/build_zone/R/reactivedata/ -v baza_provizioane_plati:/build_zone/R/reactivedata/plati/external_volume/ -v portof_database:/build_zone/R/reactivedata/solduri/external_volume/ -v portofoliu_ifrs:/build_zone/R/reactivedata/solduri/external_volume_ifrs/ -v cereri_plata:/build_zone/R/reactivedata/plati/external_volume_cereri_plata/  \risk_report:latest
where risk_report is the docker image built with docker build -t risk_report -f Dockerfile RiskReport/Dockerfile

The docker volumes created and used by the app are: risk_report and cereri_plata. These volumes are fed with data by the app and cereri_plata volume is used also by the RiskManagement app ( also available within my repository).
Docker volumes baza_provizioane_plati, portof_database, portofoliu_ifrs are used by the app but are fed with new data by the RiskManagement app.



