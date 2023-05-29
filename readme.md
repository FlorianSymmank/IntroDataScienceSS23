install miniconda / anaconda
install R, add R to path "C:\Program Files\R\R-4.3.0\bin\x64\"

create new env (named DataScience)

``` shell
conda config --add channels conda-forge   
conda config --set channel_priority strict      
conda search r-base   
conda create -n your_name_here python=3.X     
conda activate your_name_here   
conda install -c conda-forge r-base=4.X.X     
conda install r r-essentials --channel conda-forge
conda install jupyter
```

In console type "R", R Console opens, install jupyter notebook
```shell
install.packages('IRkernel')       
install.packages(c("tidyverse", "sf", "rnaturalearth", "rnaturalearthdata"))
IRkernel::installspec()
``` 


create requirments.txt
`pip list --format=freeze > requirements.txt` in conda env

# Datenaufbereitung
Führe `iso3166_to_countryName.py` aus damit aus dem Ländercode der Ländername erzeugt werden kann, wird im JupyterNotebook benötigt.