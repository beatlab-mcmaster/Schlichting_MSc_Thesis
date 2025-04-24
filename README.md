# Schlichting_MSc_Thesis
Code and data to reproduce my master's thesis.  

## Overview
Title: "How Music Advocacy Influences Audiences' Prosociality" (April 2025).  

By Joshua L. Schlichting, B.Sc.  

A Thesis Submitted to the School of Graduate Studies in Partial Fulfilment of the Requirements for the Degree Master of Science  

McMaster University © Copyright by Joshua L. Schlichting, April 2025  


## Abstract
When advocating for social justice, music is commonly used to rally support for justice-seeking groups. Music has been shown to increase prosocial behaviour like cooperation and helping, but this effect has mostly been investigated among the individuals involved in the music experience. It is thus unclear how music can foster prosociality towards external groups. Music can strengthen social bonding or empathic concern, which could facilitate prosociality, but might require shared co-presence. Alternatively, prosociality could emerge from problem awareness, but we lack empirical evidence as to whether music can effectively raise awareness for a specific issue. Here, we investigate the role of bonding, empathy, and awareness in social justice advocacy through music. 

We organized a charity event featuring a documentary film and a percussion performance about wrongful imprisonment, shown in counterbalanced order. 94 audience members attending in-person or in a livestream reported their awareness of wrongful imprisonment, bonding and empathy with those affected, and intentions to support a relevant charity, before and after each presentation. At the end of the event, we assessed charitable behaviour. 

The event increased bonding, empathy, and awareness of in-person and livestream audiences. Comparing performance and film, both had a similar effect on empathy and awareness, while the film afforded steeper increases in bonding. Notably, seeing both media in succession had an additive effect on all three variables. Two-thirds of participants performed one or more charitable behaviours, predicted by behavioural intentions. Intentions did not change significantly throughout the event but were predicted by empathy. 

In conclusion, live and livestreamed charity concerts can foster empathy for external justice-seeking groups to rally support for their fight for justice. The results of this study contribute to the interdisciplinary scholarship on prosociality and directly inform the design of musical experiences that advocate for social justice.  

## Repository Structure
This thesis is written in RMarkdown using papaja. Reproducing the manuscript requires an installation of R (https://cran.r-project.org/), a TeX distribution (e.g., TinyTeX: https://yihui.org/tinytex/), and papaja (https://github.com/crsh/papaja). To reproduce the manuscript, install the requirements and knit the file `Schlichting_MSc_Thesis.Rmd`.  

All analyses are based on the data in `data/innocents_df.csv`. The code books and printable surveys in `surveys/` explain how each of the variables was collected and which responses their values correspond to.  

Custom functions are in `src/LMMfunctions.R`.  

External images that are not created in R are stored in `images/`.  

Upon knitting the RMarkdown, all figures that are created in R are also saved in `Schlichting_MSc_Thesis_files/figure-latex/`.  

The references cited in the text are collected in `innocentsref.bib` and the R packages used are collected in `r-references.bib`.  