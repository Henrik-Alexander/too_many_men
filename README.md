February 2024

# Too many men
This folder contains the code to understand and control thes results of the paper **Too many men. Subnational Population Imbalances and Male Childlessness in Finland**. All code on which this analysis is based was written in the [**R**](https://www.r-project.org/) statistical programming language.


## Software and hardware
The analysis were executed in [**R**](https://www.r-project.org/) version 4.2.1 (2022-06-23 ucrt). The computing unit was platform x86_64-w64-mingw32/x64 (64-bit).
The program was running under Windows Server x64 (build 17763)

### Packages
This work would not have been possible with the scientific and programming contributions of people who developed packages and made them available free of use on [**R-Cran**](https://cran.r-project.org/). I list the packages used in this project to acknowledge the contribution of the authors and to ensure that people can download the required packages in order to fully reproduce the results. Furthermore, the interested reader can follow the link on the package name to read the vignettes.

- [`stargazer`](https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf) by Marek Hlavac
- [`feisr`](https://cran.r-project.org/web/packages/feisr/index.html) by Tobias Rüttenauer
- [`tidyverse`](https://cran.r-project.org/web/packages/tidyverse/index.html) by Hadley Wickham
- [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) by Matt Dowle et al.
- [`zoo`](https://cran.r-project.org/web/packages/zoo/index.html) by Achim Zeileis et al.
- [`reshape2`](https://cran.r-project.org/web/packages/reshape2/index.html) by Hadley Wickham
- [`usdata`](https://cran.rstudio.com/web/packages/usdata/index.html>) by Mine  Çetinkaya-Rundel et al.
- [`plm`](https://cran.r-project.org/web/packages/plm/plm.pdf) by Yves Croissant et al.
- [`clusterSEs`](https://cran.r-project.org/web/packages/clusterSEs/index.html) by Justin Esarey
- [`lmtest`](https://cran.r-project.org/web/packages/lmtest/index.html) by Torsten Hothorn et al.
- [`starpolisher`](https://github.com/ChandlerLutz/starpolishr) by Chandler Lutz
- [`aTSA`](https://cran.r-project.org/web/packages/aTSA/aTSA.pdf) by Debin Qiu
- [`readxl`](https://cran.r-project.org/web/packages/readxl/index.html) by Jennifer Bryan
- [`quantreg`](https://cran.r-project.org/web/packages/quantreg/index.html) by Roger Koenker
- [`SparseM`](https://cran.r-project.org/web/packages/SparseM/index.html) by Roger Koenker et al.
- [`rqpd`](https://r-forge.r-project.org/projects/rqpd/) by Roger Koenker and Stefan Holst Bache
- [`patchwork`](https://cran.r-project.org/web/packages/patchwork/index.html) by Thomas Lin Pedersen
- [`ggrepel`](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html) by Kamil Slowikowski
- [`bea.R`](https://cran.r-project.org/web/packages/bea.R/bea.R.pdf) by Andrea Batch

### Code:
*  "01_preperation.R" 
*  "02_wrangling.R"                       
*  "03_spatial_aggregation.R"     
*  "4.1_sex_ratio.R"                   
*  "4.2_preference_ratio.R"                 
*  "4.3_availability_ratio.R"                      
*  "05_contextual_variables.R"            
*  "06_covariable_dataset.R"                          
*  "07_descriptivess.R"                           
*  "08_analysis.R"                         
*  "09_discrete_time.R"         
*  "09_graphs.R"     
*  "10_regional_childlessness.R"                           
*  "11_robustness.R"           
*  "functions.R" 
*  "models.R"

### Raw_data:
The Finnish register data was analysed under the permission TK-53-1119-17. Unfortunately, the raw data is not freely accessible. 

