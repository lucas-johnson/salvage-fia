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

FIESTA...
