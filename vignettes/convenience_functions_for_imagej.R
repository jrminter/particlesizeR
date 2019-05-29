## ----setup, cache = F, echo = F, message = F, warning = F, tidy = F, comment=NA----
library(knitr)
options(width = 72)

## ----compute_area, message=FALSE--------------------------------------
library(particlesizeR)

## ----comput_area, comment=NA------------------------------------------
print(ecd_nm_to_area_um(5.0))


## ----compute_diameter, comment=NA-------------------------------------
print(area_um_to_ecd_nm(7.854e-5))

