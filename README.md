# Southern Ocean regional marine ecosystem model ensemble (SOMEME)
This repository contains all material necessary to reproduce the figures and results presented in the publication *"Addressing Key Uncertainties To Develop a Climate-Ready Southern Ocean Marine Ecosystem Model Ensemble"* led by Kieran Murphy. This manuscript will be published in the journal *Earth's Future*.  
  
## FishMIP model outputs
The Fisheries and Marine Ecosystem Model Intercomparison Project (FishMIP) group has outputs from [nine global ecosystem models](https://fishmip.org/modellingteams.html) available in the [German Climate Computing Center (DKRZ)](https://docs.dkrz.de/doc/getting_started/index.html) and via the [Inter-Sectoral Impact Model Intercomparison Project (ISIMIP) Data Portal](https://data.isimip.org/).  
  
Instructions on how to access data directly from the DKRZ server are available in the [ISIMIP website](https://www.isimip.org/dashboard/accessing-isimip-data-dkrz-server/).  
  
However, the FishMIP outputs extracted for the Southern Ocean from the DKRZ server using the [01_Extracting_tcb_fishmip.py](https://github.com/Fish-MIP/SOMEME/blob/main/scripts/01_Extracting_tcb_fishmip.py) script are publicly available in the [SOMEME folder](http://portal.sf.utas.edu.au/thredds/catalog/gem/fishmip/SOMEME/catalog.html) located in our THREDDS server. You can use all csv files in this folder to all scripts with a name that starts with `Fig`.  
  
In the same [SOMEME folder](http://portal.sf.utas.edu.au/thredds/catalog/gem/fishmip/SOMEME/catalog.html), you will also find a compressed folder called `outputs.zip`. The `outputs.zip` folder contains intermediary files and plots produced by all scripts in this repository. If you prefer, you can download this folder only (there is no need for you to download the csv files), and run the scripts. However, make sure you replace the `outputs` folder in this repository with the contents of `outputs.zip`.  
