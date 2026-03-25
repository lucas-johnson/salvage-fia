# salvage-fia

A FIESTA-backed shiny app used to estimate forest resources affected by natural disasters. 

## Installation

### Download the App code to your machine

#### Option 1 (Rstudio)

Open Rstudio and select `New Project` -> `Version Control` -> `Git`. Enter the URL for this repo, and choose a working directory on your machine in the window as shown below

<img width="582" height="412" alt="image" src="https://github.com/user-attachments/assets/ebb20b81-db03-48c9-ac10-c00ff001b213" />

<br>

---

#### Option 2 (manual)

Clone the repo (via git CLI or other) to your local machine and open a new Rstudio project within the repo directory. 

---

### Install the required R packages

1. If you chose **Option 2** above you will need to install the `renv` package using `install.packages('renv')` in your R console. If you chose **Option 2**, skip this step. 

2. Run `renv::restore()` in the console to install the required R packages. 

---

## Run the app

Within your Rstudio console run `shiny::runApp("app")`. A window will open with the app interface. You may choose to _Open in Browser_  at the top of the app window if you prefer. When you are done either close the app window or quit Rstudio to finish. 

## Estimation engine

All estimates are made with the [FIESTA R package Greenbook Module](https://usdaforestservice.github.io/FIESTA/articles/FIESTA_tutorial_GB.html). The key difference between a standard Greenbook estimate and estimates made via this app is that we are using an area of interest (polygon boundary) as a spatial filter for plots included in the estimate. Notably, only the plots within the boundaries are used to estimate stratum means, however, the stratum weights remain unchanged and will correspond to that plots original estimation unit (e.g. a county). This will likely introduce a bias into the estimate that scales with the size of the difference between the area of interest forest conditions and the original stratum's conidtions. 

The data powering these estimates comes directly from the Datamart, and only includes inventories from the most recently published evaluation period. Given that the data comes from the datamart, it is worth noting that the our spatial filter uses the publicly available plot coordinates which are fuzzed and swapped at random. 
