# EMLaide <a href='https://CVPIA-OSC.github.io/EMLaide'><img src='man/figures/hex_logo.png' align ="right" height="200" /></a> 

### Installation
This package can be installed using the following commands: 
```{r}
# install.packages("remotes")
remotes::install_github("CVPIA-OSC/EMLaide")
```
### Overview

This package provides tools to create EML files that are compliant
with the [Environmental Data Initiative](https://portal.edirepository.org/nis/home.jsp) (EDI) standards. The package was designed to meet the specific needs of [CVPIA's](http://cvpia.scienceintegrationteam.com/) data guidance, but can be utilized by any persons wishing to create a working EML document that can be submitted to EDI. Documentation for each function can be found in the Reference tab. The full EML schema can be found on their [website](https://eml.ecoinformatics.org/schema/index.html).

### Usage 
A working template and additional guides can be found under the 'Articles' tab. These articles give step by step instructions for generating EML documents using EMLaide. 


Articles: 

* **[Metadata Template Documentation](https://cvpia-osc.github.io/EMLaide/articles/template-doc.html)** - The metadata template documentation article provides instructions for inputting your own metadata inputs into our [template materials](https://cvpia-data-stewardship.s3-us-west-1.amazonaws.com/metadata+template.zip) to generate a unique EML file. 
* **[EML Template Helper](https://cvpia-osc.github.io/EMLaide/articles/creating-EML.html)** - This article gives a step by step instructions of how to generate a full EML document for an [example dataset](https://cvpia-data-stewardship.s3-us-west-1.amazonaws.com/hannon-example.zip). 
* **[EML Dataset Section](https://cvpia-osc.github.io/EMLaide/articles/Dataset-Element.html)** - This document explains how to add multiple datasets to one EML document and how to add non tabular data to an EML document. 
* **[EML Custom Inputs to Initial Sections](https://cvpia-osc.github.io/EMLaide/articles/Inital-Metadata-Information.html)** - This document gives more in depth examples on how to use the `add_access`, `add_pub_date`, `add_keywords`, and `add_license` functions. 
* **[EML Custon Units](https://cvpia-osc.github.io/EMLaide/articles/custom-units.html)** - This document explains how to add custom units to a EML document. 





