
This repository is associated with the paper *Multivariate hierarchical
analysis of car crashes data considering a spatial network lattice* by
Andrea Gilardi, Jorge Mateu, Riccardo Borgoni and Robin Lovelace.

The R code used to estimate the statistical models detailed in the paper
and in the supplementary material is summarised in a series of scripts
which are stored in the folder called `R`. The corresponding data
objects are saved in the folder named `data`.

The models were trained using the following packages:

``` r
sessionInfo()
```

    ## R version 3.6.3 (2020-02-29)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 18.04.5 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
    ## LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
    ##  [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8   
    ##  [7] LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] conflicted_1.0.4 dodgr_0.2.8.011  igraph_1.2.6     INLAMSM_0.2-2   
    ##  [5] spdep_1.1-8      spData_0.3.8     MCMCpack_1.5-0   MASS_7.3-53.1   
    ##  [9] coda_0.19-4      INLA_20.03.17    foreach_1.5.1    sp_1.4-5        
    ## [13] Matrix_1.3-2     sf_0.9-8        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] httr_1.4.2         jsonlite_1.7.2     splines_3.6.3      gtools_3.8.2      
    ##  [5] RcppParallel_5.1.2 assertthat_0.2.1   expm_0.999-6       yaml_2.2.1        
    ##  [9] LearnBayes_2.15.1  pillar_1.6.0       lattice_0.20-41    quantreg_5.85     
    ## [13] glue_1.4.2         digest_0.6.27      rvest_1.0.0        htmltools_0.5.1.1 
    ## [17] conquer_1.0.2      pkgconfig_2.0.3    raster_3.4-10      SparseM_1.81      
    ## [21] gmodels_2.18.1     purrr_0.3.4        osmdata_0.1.5      gdata_2.18.0      
    ## [25] MatrixModels_0.5-0 tibble_3.1.1       proxy_0.4-25       generics_0.1.0    
    ## [29] ellipsis_0.3.2     cachem_1.0.4       magrittr_2.0.1     crayon_1.4.1      
    ## [33] deldir_0.2-10      mcmc_0.9-7         evaluate_0.14      fansi_0.4.2       
    ## [37] nlme_3.1-152       xml2_1.3.2         class_7.3-18       tools_3.6.3       
    ## [41] lifecycle_1.0.0    matrixStats_0.59.0 stringr_1.4.0      compiler_3.6.3    
    ## [45] e1071_1.7-7        rlang_0.4.11       classInt_0.4-3     units_0.7-1       
    ## [49] grid_3.6.3         iterators_1.0.13   rmarkdown_2.7      boot_1.3-27       
    ## [53] codetools_0.2-18   DBI_1.1.1          curl_4.3.1         R6_2.5.0          
    ## [57] lubridate_1.7.10   knitr_1.33         dplyr_1.0.5        fastmap_1.1.0     
    ## [61] utf8_1.2.1         KernSmooth_2.23-18 stringi_1.5.3      Rcpp_1.0.6        
    ## [65] vctrs_0.3.8        tidyselect_1.1.1   xfun_0.22
