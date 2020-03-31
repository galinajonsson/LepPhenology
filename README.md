# LepPhenology

Author(s): [Galina M. Jönsson](https://github.com/galinajonsson)

This repository contains all the code for:

>Jönsson, G. M. (2020). Phenology change of British Lepioptera since 1900.  Unpublished manucript. 


## Data
I used specimen records from the Natural History Museum, London (NHM). The georeferenced NHM dataset is available via the [NHM's iCollections](https://data.nhm.ac.uk/dataset/collection-specimens/resource/05ff2255-c38a-40c9-b657-4ccb55ab2feb?filters=project%3AiCollections). If you use it, please cite as folows: 
> Gordon L J Paterson; Sara Albuquerque; Vladimir Blagoderov; Steve Brooks et al. (2016). Dataset: iCollections. Natural History Museum Data Portal (data.nhm.ac.uk). https://doi.org/10.5519/0038559

I used the Met Office's HadUK-Grid dataset at 12 km resolution is derived from the associated 1km x 1km resolution, which spans the period from 1862 to 2017, and is avialable via [CEDA archives](https://catalogue.ceda.ac.uk/uuid/dc2ef1e4f10144f29591c21051d99d39) (full citation below). I used the following climatic varibales:
- Mean monthly temperature (tas)
- Maximum monthly temperature (tasmax)
- Minimum monthly temperature (tasmin)
- Mean monthly precipitation (rainfall)

> Met Office; Hollis, D.; McCarthy, M.; Kendon, M.; Legg, T.; Simpson, I. (2019): HadUK-Grid Gridded Climate Observations on a 12km grid over the UK, v1.0.0.0 (1862-2017). Centre for Environmental Data Analysis, 14 November 2019. doi:10.5285/dc2ef1e4f10144f29591c21051d99d39.

For reproducibility purposes, download all data and place it into the relevant `data/`-subfolder to rerun our analyses. 

## Analyses
The analyses code is divided into .Rmd files that run the analyses for each section of the manuscript/supplementary materials, more detailed scripts for some functions used in analyses and called by the .Rmd files, and scripts for the figures found in the manuscript.

Note that throughout I've commented out `write.csv` and `saveRDS` commands in order to not clog up your machine. For code chunks that run the models, I've set `eval` to FALSE, again, to not clog up your machine as the analyses are computationally expensive and were run on high performance machines.

* __01-SpecimenData.Rdm__ summarises the NHM specimen data for eleven univoltine British butterfly species. For each speies used in analyses, I summarise the annual 10th and 19th percentile of julian record days, the range of annual records days and the first annual record day Julian day.
* __02-ClimateData.Rdm__ summarises the HadUK-Grid dataset's relevant climatic variables and **WILL SUMMARISE RELEVANT CLIMATIC VARIABLES PER RECORD FOR ALL ELEVEN SPECIES**
* __03-Analyses.Rdm__ **WILL ANALYSE PHENOLOGY CHNAGE AND EFFECTS OF CLIMATIC VARIABLES ON PHENOLOGY AMONG THE ELEVEN SPECIES**



##### Code for figures



##### Code for functions

* __function-summarise_nc_tas.R__
* __function-summarise_nc_mintas.R__


## Other folders

* `/figs` contains the figures with file names matching those of the manuscript *NONE YET**
* `/output` contains the empty subfolders `/output/climate`. For reproducibility purposes, format the raw HadUK-Grid datasets according to the code in `/analyses/02-ClimateData.Rdm` and place into `/output/climate`. 


## Session Info
For reproducibility purposes, here is the output of devtools::session_info() used to perform the analyses in the publication.
```
─ Session info ───────────────────────────────────────────────────────────────────────────────
 setting  value                       
 version  R version 3.5.2 (2018-12-20)
 os       macOS Mojave 10.14.3        
 system   x86_64, darwin15.6.0        
 ui       RStudio                     
 language (EN)                        
 collate  en_GB.UTF-8                 
 ctype    en_GB.UTF-8                 
 tz       Europe/London               
 date     2020-01-27                  

─ Packages ───────────────────────────────────────────────────────────────────────────────────
 package     * version  date       lib source                                         
 abind         1.4-5    2016-07-21 [1] CRAN (R 3.5.0)                                 
 AICcmodavg  * 2.2-2    2019-05-29 [1] CRAN (R 3.5.2)                                 
 assertthat    0.2.1    2019-03-21 [1] CRAN (R 3.5.2)                                 
 backports     1.1.5    2019-10-02 [1] CRAN (R 3.5.2)                                 
 boot        * 1.3-23   2019-07-05 [1] CRAN (R 3.5.2)                                 
 BRCmap      * 0.10.3.3 2017-11-13 [1] local                                          
 callr         3.3.2    2019-09-22 [1] CRAN (R 3.5.2)                                 
 cli           1.1.0    2019-03-19 [1] CRAN (R 3.5.2)                                 
 coda          0.19-3   2019-07-05 [1] CRAN (R 3.5.2)                                 
 codetools     0.2-16   2018-12-24 [1] CRAN (R 3.5.2)                                 
 colorspace    1.4-1    2019-03-18 [1] CRAN (R 3.5.2)                                 
 cowplot     * 1.0.0    2019-07-11 [1] CRAN (R 3.5.2)                                 
 crayon        1.3.4    2017-09-16 [1] CRAN (R 3.5.0)                                 
 desc          1.2.0    2018-05-01 [1] CRAN (R 3.5.0)                                 
 devtools    * 2.2.1    2019-09-24 [1] CRAN (R 3.5.2)                                 
 digest        0.6.22   2019-10-21 [1] CRAN (R 3.5.2)                                 
 dplyr       * 0.8.3    2019-07-04 [1] CRAN (R 3.5.2)                                 
 ellipsis      0.3.0    2019-09-20 [1] CRAN (R 3.5.2)                                 
 evaluate      0.14     2019-05-28 [1] CRAN (R 3.5.2)                                 
 foreign       0.8-72   2019-08-02 [1] CRAN (R 3.5.2)                                 
 fs            1.3.1    2019-05-06 [1] CRAN (R 3.5.2)                                 
 gdata         2.18.0   2017-06-06 [1] CRAN (R 3.5.0)                                 
 ggplot2     * 3.2.1    2019-08-10 [1] CRAN (R 3.5.2)                                 
 ggthemes    * 4.2.0    2019-05-13 [1] CRAN (R 3.5.2)                                 
 glue          1.3.1    2019-03-12 [1] CRAN (R 3.5.2)                                 
 gridExtra   * 2.3      2017-09-09 [1] CRAN (R 3.5.0)                                 
 gtable        0.3.0    2019-03-25 [1] CRAN (R 3.5.2)                                 
 gtools        3.8.1    2018-06-26 [1] CRAN (R 3.5.0)                                 
 hms           0.5.2    2019-10-30 [1] CRAN (R 3.5.2)                                 
 htmltools     0.4.0    2019-10-04 [1] CRAN (R 3.5.2)                                 
 httr          1.4.1    2019-08-05 [1] CRAN (R 3.5.2)                                 
 kableExtra  * 1.1.0    2019-03-16 [1] CRAN (R 3.5.2)                                 
 knitr       * 1.26     2019-11-12 [1] CRAN (R 3.5.2)                                 
 lattice       0.20-38  2018-11-04 [1] CRAN (R 3.5.2)                                 
 lazyeval      0.2.2    2019-03-15 [1] CRAN (R 3.5.2)                                 
 LearnBayes    2.15.1   2018-03-18 [1] CRAN (R 3.5.0)                                 
 lifecycle     0.1.0    2019-08-01 [1] CRAN (R 3.5.2)                                 
 lme4        * 1.1-21   2019-03-05 [1] CRAN (R 3.5.2)                                 
 magrittr      1.5      2014-11-22 [1] CRAN (R 3.5.0)                                 
 maptools    * 0.9-8    2019-10-05 [1] CRAN (R 3.5.2)                                 
 MASS          7.3-51.4 2019-03-31 [1] CRAN (R 3.5.2)                                 
 Matrix      * 1.2-17   2019-03-22 [1] CRAN (R 3.5.2)                                 
 memoise       1.1.0    2017-04-21 [1] CRAN (R 3.5.0)                                 
 minqa         1.2.4    2014-10-09 [1] CRAN (R 3.5.0)                                 
 munsell       0.5.0    2018-06-12 [1] CRAN (R 3.5.0)                                 
 nlme          3.1-142  2019-11-07 [1] CRAN (R 3.5.2)                                 
 nloptr        1.2.1    2018-10-03 [1] CRAN (R 3.5.0)                                 
 pillar        1.4.2    2019-06-29 [1] CRAN (R 3.5.2)                                 
 pkgbuild      1.0.6    2019-10-09 [1] CRAN (R 3.5.2)                                 
 pkgconfig     2.0.3    2019-09-22 [1] CRAN (R 3.5.2)                                 
 pkgload       1.0.2    2018-10-29 [1] CRAN (R 3.5.0)                                 
 plyr        * 1.8.4    2016-06-08 [1] CRAN (R 3.5.0)                                 
 prettyunits   1.0.2    2015-07-13 [1] CRAN (R 3.5.0)                                 
 processx      3.4.1    2019-07-18 [1] CRAN (R 3.5.2)                                 
 ps            1.3.0    2018-12-21 [1] CRAN (R 3.5.0)                                 
 purrr         0.3.3    2019-10-18 [1] CRAN (R 3.5.2)                                 
 R2jags        0.5-7    2015-08-23 [1] CRAN (R 3.5.0)                                 
 R2WinBUGS     2.1-21   2015-07-30 [1] CRAN (R 3.5.0)                                 
 R6            2.4.1    2019-11-12 [1] CRAN (R 3.5.2)                                 
 raster        3.0-7    2019-09-24 [1] CRAN (R 3.5.2)                                 
 Rcpp          1.0.3    2019-11-08 [1] CRAN (R 3.5.2)                                 
 readr         1.3.1    2018-12-21 [1] CRAN (R 3.5.0)                                 
 remotes       2.1.0    2019-06-24 [1] CRAN (R 3.5.2)                                 
 reshape2    * 1.4.3    2017-12-11 [1] CRAN (R 3.5.0)                                 
 rgdal       * 1.4-7    2019-10-28 [1] CRAN (R 3.5.2)                                 
 rgeos       * 0.5-2    2019-10-03 [1] CRAN (R 3.5.2)                                 
 rjags         4-10     2019-11-06 [1] CRAN (R 3.5.2)                                 
 rlang         0.4.1    2019-10-24 [1] CRAN (R 3.5.2)                                 
 rmarkdown     1.17     2019-11-13 [1] CRAN (R 3.5.2)                                 
 rprojroot     1.3-2    2018-01-03 [1] CRAN (R 3.5.0)                                 
 rstudioapi    0.10     2019-03-19 [1] CRAN (R 3.5.2)                                 
 rvest         0.3.5    2019-11-08 [1] CRAN (R 3.5.2)                                 
 scales        1.1.0    2019-11-18 [1] CRAN (R 3.5.2)                                 
 sessioninfo   1.1.1    2018-11-05 [1] CRAN (R 3.5.0)                                 
 sp          * 1.3-2    2019-11-07 [1] CRAN (R 3.5.2)                                 
 sparta      * 0.2.07   2019-12-19 [1] Github (BiologicalRecordsCentre/sparta@9c4312f)
 stringi       1.4.3    2019-03-12 [1] CRAN (R 3.5.2)                                 
 stringr       1.4.0    2019-02-10 [1] CRAN (R 3.5.2)                                 
 survival      3.1-7    2019-11-09 [1] CRAN (R 3.5.2)                                 
 testthat      2.3.0    2019-11-05 [1] CRAN (R 3.5.2)                                 
 tibble        2.1.3    2019-06-06 [1] CRAN (R 3.5.2)                                 
 tidyselect    0.2.5    2018-10-11 [1] CRAN (R 3.5.0)                                 
 unmarked      0.13-0   2019-11-12 [1] CRAN (R 3.5.2)                                 
 usethis     * 1.5.1    2019-07-04 [1] CRAN (R 3.5.2)                                 
 vctrs         0.2.0    2019-07-05 [1] CRAN (R 3.5.2)                                 
 VGAM          1.1-1    2019-02-18 [1] CRAN (R 3.5.2)                                 
 viridis     * 0.5.1    2018-03-29 [1] CRAN (R 3.5.0)                                 
 viridisLite * 0.3.0    2018-02-01 [1] CRAN (R 3.5.0)                                 
 webshot       0.5.1    2018-09-28 [1] CRAN (R 3.5.0)                                 
 withr         2.1.2    2018-03-15 [1] CRAN (R 3.5.0)                                 
 xfun          0.11     2019-11-12 [1] CRAN (R 3.5.2)                                 
 xml2          1.2.2    2019-08-09 [1] CRAN (R 3.5.2)                                 
 xtable        1.8-4    2019-04-21 [1] CRAN (R 3.5.2)                                 
 yaml          2.2.0    2018-07-25 [1] CRAN (R 3.5.0)                                 
 zeallot       0.1.0    2018-01-28 [1] CRAN (R 3.5.0) 
 ```
