# DASH
R Package Companion to the Drone Assisted Stream Habitat (DASH) Protocol. `DASH` is an R package to summarize habitat metrics from data generated using the DASH protocol (Carmichael et al. 2019). Initially, the DASH R package is being created to import, perform QA/QC, and summarize habitat data collected by on-the-ground personnel. Summarized habitat information can then be paired with fish abundance and density information for various fish-habitat models including quantile random forest (QRF) capacity models (See et al. 2020). The eventual goal of the `DASH` R package is also to join fish and habitat data to stream centerlines and also generate habitat metrics from drone collected orthomosaics. Habitat metrics from DASH include data describing characteristics such as large woody debris, undercut banks, channel unit size, undercut banks, etc.

## Getting Started
To install the current working version of this package to your computer, you can use Hadley Wickham's `devtools` package. To install and load `devtools` (or any R package on CRAN for that matter) use:

```
install.packages("devtools")
library(devtools)
```

Once `devtools` is successfully installed, use the following to install `DASH`:

```
devtools::install_github("BiomarkABS/DASH", build_vignettes = TRUE)
```

Be sure to include the `build_vignettes = TRUE` argument, as this will ensure that vignettes include with the package will be built during install and made available.

## Vignettes
Because the R packages `rmarkdown` and `knitr` are used to build this vignette to HTML output, you are required to have these installed to view vignettes available from this package. Both of these packages can be installed and loaded using the directions above for `devtools`. Vignettes can then be accessed using:

```
browseVignettes("DASH")
```

Alternatively, vignettes can be viewed in the Help menu using, for example:

```
vignette("otg-import-qc", package = "DASH")
```

`DASH` currently includes the following vignettes:

**otg-import-qc**: Described how to import and QC on-the-ground (OTG) DASH habitat data collected using the DASH protocol and [ArcGIS Survey123](https://www.esri.com/en-us/arcgis/products/arcgis-survey123/overview) data collection forms. Also describes how to resolve some errors identified during the QC process and some "data cleaning" to generate a summary of channel-unit scale data that can be joined to stream centerlines.

## Developers Note

To use `devtools` you may also have to download and install Rtools. The latest version of Rtools can be found [here](https://cran.r-project.org/bin/windows/Rtools/).

## Making Contributions

If you are interested in making contributions to `DASH`, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send us a pull request. The authors can then review any changes and merge.

## Executive Summary from DASH Protocol
This Drone Assisted Stream Habitat (DASH) protocol outlines procedures to collect accurate habitat data in an efficient and cost-effective manner that can be implemented across large spatial scales. Habitat attributes are collected primarily at the channel-unit (i.e., pool, riffle, run, rapid +, side channel) scale and secondarily at the reach (e.g., 100m - 1km) scale. Channel-unit scale habitat data can then later be summarized at larger scales if desired. By integrating high-resolution drone imagery, and when available, bathymetric light detection and ranging (LiDAR) data with minimal ground crew data collection, this protocol provides robust and accurate habitat data to inform habitat status and trends as well as fish-habitat modeling efforts. Ground crews delineate channel units, collect habitat attributes that cannot be obtained from remote sensing data, and collect high-resolution GPS information so that on-the-ground data is spatially explicit and easily compatible with remote sensing (e.g., drone, LiDAR) data. Data collected by ground crews can also be used to cross-validate remotely sensed data, when desired.

This protocol builds on previously developed methods for habitat sampling, and improves upon them by leveraging: 1) sub-meter global navigation satellite system (GNSS) receivers; 2) cost-effective drone imagery collection, image stitching, and photogrammetry; and 3) semi-automated data post-processing. Many of the ground crew methods used here have been adapted and simplified from the Columbia Habitat Monitoring Program (CHaMP) in an effort to increase survey repeatability and to remove potential human error. All data collection efforts are georeferenced and topologically compatible to increase repeatability of methods and data collection locations; a primary criticism of previous CHaMP survey efforts.

Another concern from previous habitat monitoring programs was the inability to extrapolate site-level data to larger (e.g., tributary, watershed) scales. With the DASH protocol, the intent is to circumvent the need to extrapolate data by collecting data for individual channel units in a rapid manner and using remote sensing technologies. During initial efforts, channel unit data will be collected at the reach scale (e.g., 3 km reaches); however, this protocol can easily be applied to larger (e.g., tributary, watershed) scales because of the speed and cost of drone imagery data collection and minimized minimal use of ground crew data collection. Habitat data acquired using this protocol can be paired with channel unit scale or larger scale fish abundance and density estimates to better elicit fish-habitat relationships. For example, estimates of capacity could be generated at any desired scale using available models (e.g., quantile regression forest [QRF] capacity models). The DASH protocol can be used for status and trends estimates of watershed health because of the ability to repeat measurements efficiently and effectively across large spatial scales. In addition, by enabling the use of drone and remote-sensing data, this protocol reduces labor; providing a cost-effective tool for habitat data collection supporting status and trend evaluation and model products to better inform habitat restoration prioritization and planning.

## Literature Cited
Carmichael, R.A., M.W. Ackerman, K. See, B. Lott, T. Mackay, and C. Beasley. 2019. Drone Assisted Stream Habitat (DASH) Protocol, DRAFT.

See, K.E., M.W. Ackerman, R.A. Carmichael, S.L. Hoffmann, and C. Beasley. **In Review**. Estimating Carrying Capacity for Juvenile Salmon Habitat using Quantile Random Forest Models. Ecosphere.

## Questions?

Please feel free to post an issue to this repository for requested features, bug fixes, errors in documentation, etc.

### Licenses

**Text and figures :**  [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC-0](http://creativecommons.org/publicdomain/zero/1.0/) attribution requested in reuse

### Cheers!
