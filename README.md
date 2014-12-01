YplantQMC
========================
*Remko Duursma* and *Mik Cieslak*

Welcome to the homepage for the YplantQMC model, a new implementation of the Yplant model (Pearcy and Yang 1996), combined with the QuasiMC raytracer.

QuasiMC is developed by Cieslak et al. (2008), which is part of the Virtual Laboratory plant modelling software created at the University of Calgary (http://www.algorithmicbotany.org).

To install this R package,
```
library(devtools)
install_bitbucket("remkoduursma/yplantqmc")
```

After loading YplantQMC (`library(YplantQMC)`), view the detailed manual, which [can be downloaded from here](http://www.remkoduursma.com/YplantQMC).

YplantQMC is also available on CRAN (but the newest version is installed with the above command).

**References**

Cieslak, M., C. Lemieux, J. Hanan and P. Prusinkiewicz. 2008. Quasi-Monte Carlo simulation of the light environment of plants. Functional Plant Biology. 35:837-849.

Pearcy, R.W. and W. Yang. 1996. A three-dimensional crown architecture model for assessment of light capture and carbon gain by understory plants. Oecologia. 108:1-12.