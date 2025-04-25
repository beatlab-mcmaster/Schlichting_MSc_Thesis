[![DOI](https://zenodo.org/badge/971732218.svg)](https://doi.org/10.5281/zenodo.15282949)

# Schlichting_MSc_Thesis
Code and data to reproduce my master's thesis.  

## Overview
Title: "How Music Advocacy Influences Audiences' Prosociality" (April 2025).  

By Joshua L. Schlichting, B.Sc.  

A Thesis Submitted to the School of Graduate Studies in Partial Fulfilment of the Requirements for the Degree Master of Science  

McMaster University © Copyright by Joshua L. Schlichting, April 2025  


## Abstract
When advocating for social justice, music is commonly used to rally support for justice-seeking groups. Shared music experiences have been shown to increase prosocial behaviour like cooperation and helping, but previous research is mostly restricted to the individuals involved in the experience. It is thus unclear how music can foster prosociality towards external groups. Music can strengthen social bonding or empathic concern, which could facilitate prosociality, but might require shared co-presence. Alternatively, prosociality can emerge from problem awareness, but we lack empirical evidence as to whether music can effectively raise awareness for a specific issue. Here, we investigate the impact of co-presence and media type on bonding, empathy, and awareness in social justice advocacy through music. 
  
We organized a charity event featuring a documentary film and a percussion performance about wrongful imprisonment, shown in counterbalanced order. 94 audience members attending in-person or in a livestream reported their awareness of wrongful imprisonment, bonding and empathy with those affected, and intentions to support a relevant charity, before and after each presentation. At the end of the event, we assessed charitable behaviour. 
  
Our analyses revealed that the film afforded moderate to large increases in bonding and small to moderate increases in empathy, whereas the performance only afforded small to negligible increases. Both media had similar small effects on problem awareness. There were no significant differences between in-person and livestream audiences for any of the outcomes. Two thirds of participants supported the charity through one or more behaviours. Charitable behaviour was indirectly related to empathy through behavioural intentions. 
  
In conclusion, live and livestreamed charity events can increase empathy, which may result in prosocial outcomes towards external groups, though the music investigated here was less effective than the documentary film. We derive informed recommendations for designing musical experiences and psychological studies contributing to social justice.

## Repository Structure
This thesis is written in RMarkdown using papaja. Reproducing the manuscript requires an installation of R (https://cran.r-project.org/), a TeX distribution (e.g., TinyTeX: https://yihui.org/tinytex/), and papaja (https://github.com/crsh/papaja). To reproduce the manuscript, install the requirements and knit the file `Schlichting_MSc_Thesis.Rmd`.  

All analyses are based on the data in `data/innocents_df.csv`. The code books and printable surveys in `surveys/` explain how each of the variables was collected and which responses their values correspond to.  

Custom functions are in `src/LMMfunctions.R`.  

External images that are not created in R are stored in `images/`.  

Upon knitting the RMarkdown, all figures that are created in R are also saved in `Schlichting_MSc_Thesis_files/figure-latex/`.  

The references cited in the text are collected in `innocentsref.bib` and the R packages used are collected in `r-references.bib`.  
