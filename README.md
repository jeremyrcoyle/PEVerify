PEVerify
========

Persistent Efficiency Auto M&amp;V model

========


Install from R:

1) Install devtools.

```
install.packages(“devtools”)
```

Note that you may need to install libcurl if you don’t already have it installed.

On Ubuntu, you can run the following from the command line:

```
sudo apt-get install libcurl4-openssl-dev
```

2) Install origami Package
```
library(devtools)
devtools::install_github("jeremyrcoyle/origami”)
```
3) Install PEVerify Package

```
devtools::install_github(“jeremyrcoyle/PEVerify”)
```

Run from the command line:

4) Use PEVerify script as follows:

```
R --no-save < PEVerifyCL.R --args trainingFile predictionFile holidayFile outputFile
```

Note that the combination algorithm can be slow, but is easy to parallelize for a substantial speedup. Contact jeremyrcoyle@gmail.com for details.
