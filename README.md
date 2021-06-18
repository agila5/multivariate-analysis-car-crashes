
This repository is associated with the paper *Multivariate hierarchical
analysis of car crashes data considering a spatial network lattice* by
Andrea Gilardi, Jorge Mateu, Riccardo Borgoni and Robin Lovelace.

The R code used to estimate the statistical models detailed in the paper
and the supplementary material is summarised in a series of scripts
saved in the repository called `R`. The required data objects are saved
in the repositoyr named `data`.

The models were trained using the following packages and version:

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
    ##  [1] conflicted_1.0.4 igraph_1.2.6     INLAMSM_0.2-2    spdep_1.1-8     
    ##  [5] spData_0.3.8     MCMCpack_1.5-0   MASS_7.3-53.1    coda_0.19-4     
    ##  [9] INLA_20.03.17    foreach_1.5.1    sp_1.4-5         Matrix_1.3-2    
    ## [13] sf_0.9-8        
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.6         lattice_0.20-41    deldir_0.2-10      class_7.3-18      
    ##  [5] gtools_3.8.2       assertthat_0.2.1   digest_0.6.27      utf8_1.2.1        
    ##  [9] R6_2.5.0           MatrixModels_0.5-0 evaluate_0.14      e1071_1.7-7       
    ## [13] pillar_1.6.0       rlang_0.4.11       gdata_2.18.0       SparseM_1.81      
    ## [17] raster_3.4-10      gmodels_2.18.1     rmarkdown_2.7      splines_3.6.3     
    ## [21] stringr_1.4.0      proxy_0.4-25       compiler_3.6.3     xfun_0.22         
    ## [25] pkgconfig_2.0.3    mcmc_0.9-7         htmltools_0.5.1.1  tidyselect_1.1.1  
    ## [29] tibble_3.1.1       expm_0.999-6       codetools_0.2-18   matrixStats_0.59.0
    ## [33] fansi_0.4.2        crayon_1.4.1       dplyr_1.0.5        conquer_1.0.2     
    ## [37] grid_3.6.3         nlme_3.1-152       lifecycle_1.0.0    DBI_1.1.1         
    ## [41] magrittr_2.0.1     units_0.7-1        KernSmooth_2.23-18 cachem_1.0.4      
    ## [45] stringi_1.5.3      LearnBayes_2.15.1  ellipsis_0.3.2     generics_0.1.0    
    ## [49] vctrs_0.3.8        boot_1.3-27        iterators_1.0.13   tools_3.6.3       
    ## [53] glue_1.4.2         purrr_0.3.4        fastmap_1.1.0      yaml_2.2.1        
    ## [57] classInt_0.4-3     knitr_1.33         quantreg_5.85
