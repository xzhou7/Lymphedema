# Lymphedema Project

## This depository contains code for the analysis of one lymphedema cohort 

### The analysis enviroment is listed as below

> sessionInfo()
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: OS X  11.4

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
 [1] grid      parallel  stats4    stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tableHTML_2.1.0             htmlwidgets_1.5.3           factoextra_1.0.7            readr_1.4.0                 htmlTable_2.1.0            
 [6] table1_1.3                  ggrepel_0.9.1               patchwork_1.1.1             ggpubr_0.4.0                lubridate_1.7.10           
[11] devtools_2.4.0              usethis_2.0.1               ggbiplot_0.55               scales_1.1.1                plyr_1.8.6                 
[16] picante_1.8.2               nlme_3.1-152                vegan_2.5-7                 lattice_0.20-41             permute_0.9-5              
[21] ape_5.4-1                   ggsci_2.9                   cowplot_1.1.1               ggplot2_3.3.5               dplyr_1.0.7                
[26] phyloseq_1.32.0             SeuratObject_4.0.2          Seurat_4.0.3                SingleCellExperiment_1.10.1 SummarizedExperiment_1.18.2
[31] DelayedArray_0.14.1         matrixStats_0.58.0          Biobase_2.48.0              GenomicRanges_1.40.0        GenomeInfoDb_1.24.2        
[36] IRanges_2.22.2              S4Vectors_0.26.1            BiocGenerics_0.34.0        

loaded via a namespace (and not attached):
  [1] utf8_1.2.1             reticulate_1.18        tidyselect_1.1.1       Rtsne_0.15             munsell_0.5.0          codetools_0.2-18      
  [7] ica_1.0-2              future_1.21.0          miniUI_0.1.1.1         withr_2.4.2            colorspace_2.0-2       knitr_1.32            
 [13] rstudioapi_0.13        ROCR_1.0-11            ggsignif_0.6.1         tensor_1.5             listenv_0.8.0          labeling_0.4.2        
 [19] GenomeInfoDbData_1.2.3 polyclip_1.10-0        farver_2.1.0           rhdf5_2.32.4           rprojroot_2.0.2        parallelly_1.24.0     
 [25] vctrs_0.3.8            generics_0.1.0         xfun_0.24              R6_2.5.0               bitops_1.0-6           spatstat.utils_2.1-0  
 [31] cachem_1.0.5           assertthat_0.2.1       promises_1.2.0.1       gtable_0.3.0           globals_0.14.0         processx_3.5.2        
 [37] goftest_1.2-2          rlang_0.4.11           splines_4.0.2          rstatix_0.7.0          lazyeval_0.2.2         checkmate_2.0.0       
 [43] spatstat.geom_2.0-1    broom_0.7.6            reshape2_1.4.4         abind_1.4-5            backports_1.2.1        httpuv_1.6.1          
 [49] tools_4.0.2            ellipsis_0.3.2         spatstat.core_2.0-0    biomformat_1.16.0      RColorBrewer_1.1-2     sessioninfo_1.1.1     
 [55] ggridges_0.5.3         Rcpp_1.0.6             progress_1.2.2         zlibbioc_1.34.0        purrr_0.3.4            RCurl_1.98-1.3        
 [61] ps_1.6.0               prettyunits_1.1.1      rpart_4.1-15           deldir_0.2-10          pbapply_1.4-3          zoo_1.8-9             
 [67] haven_2.3.1            cluster_2.1.1          fs_1.5.0               magrittr_2.0.1         data.table_1.14.0      RSpectra_0.16-0       
 [73] scattermore_0.7        openxlsx_4.2.3         lmtest_0.9-38          RANN_2.6.1             fitdistrplus_1.1-3     pkgload_1.2.1         
 [79] hms_1.0.0              mime_0.11              xtable_1.8-4           rio_0.5.26             readxl_1.3.1           gridExtra_2.3         
 [85] testthat_3.0.2         compiler_4.0.2         tibble_3.1.2           KernSmooth_2.23-18     crayon_1.4.1           htmltools_0.5.1.1     
 [91] mgcv_1.8-34            later_1.2.0            Formula_1.2-4          tidyr_1.1.3            DBI_1.1.1              MASS_7.3-53.1         
 [97] Matrix_1.3-4           ade4_1.7-16            car_3.0-10             cli_3.0.0              igraph_1.2.6           forcats_0.5.1         
[103] pkgconfig_2.0.3        foreign_0.8-81         plotly_4.9.4.1         spatstat.sparse_2.0-0  foreach_1.5.1          multtest_2.44.0       
[109] XVector_0.28.0         stringr_1.4.0          callr_3.7.0            digest_0.6.27          sctransform_0.3.2      RcppAnnoy_0.0.18      
[115] spatstat.data_2.1-0    Biostrings_2.56.0      cellranger_1.1.0       leiden_0.3.7           uwot_0.1.10            curl_4.3.2            
[121] shiny_1.6.0            lifecycle_1.0.0        jsonlite_1.7.2         Rhdf5lib_1.10.1        carData_3.0-4          desc_1.3.0            
[127] viridisLite_0.4.0      fansi_0.5.0            pillar_1.6.1           fastmap_1.1.0          httr_1.4.2             pkgbuild_1.2.0        
[133] survival_3.2-10        glue_1.4.2             remotes_2.3.0          zip_2.1.1              png_0.1-7              iterators_1.0.13      
[139] stringi_1.6.2          memoise_2.0.0          irlba_2.3.3            future.apply_1.7.0    

