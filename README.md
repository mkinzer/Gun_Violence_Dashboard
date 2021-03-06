# The GV Dashboard 

Welcome to the **Gun Violence (GV) Dashboard**, I developed this using the [R Statistical programming language](https://www.r-project.org/) as a personal project to investigate gun violence incidents in my home city of Philadelphia and the United States at large. 
The *User Guide* tab will describe many of the same details, techniques, and assumptions as below.

### Data Sources

The Dashboard takes in data from a few main sources:

- [Gun Violence Archive](https://www.gunviolencearchive.org/)
  - The GVA is one of the most reputable and reliable data sources for GV to date. Data is updated, scraped, and refreshed live from this site.
- Mass Shooting Tracker 
  - The MST database was discontinued in October 2019 to the best of my knowledge. It was an open source, crowd-supported dataset with a slightly larger pool than GVA. The dataset was saved and frozen shortly before it was discontinued.
- [Open Data Philly](https://www.opendataphilly.org/dataset/shooting-victims)
  - Open Data Philly is self-branded as “the official open data repository for the City.” Data scraped from here does not look into “mass shooting” events and instead looks at all reported shooting incidents.

### Definitions & Assumptions

**United States Gun Violence:**
- The District of Columbia (D.C.) as a standalone district was excluded due to the high incidence per capita. In the normalization graphs D.C. had a density of over 5 which heavily skewed the graph.
- A “Mass Shooting” is being defined as “a single outburst in which four or more people are shot.” For reference, the FBI defines a “mass murder” as “four or more murdered during an event with no”cooling-off period" between the murders." The definitions have changed over the years and the U.S. acknowledges that there is no agreed upon definition at present.
- Only data taken from 2013 to the present is accounted for due to a lack of resources from before this year and varying definitions for what constitutes a mass shooting as time has gone on since.

**Philadelphia, Pennsylvania Gun Violence:** 
- The Philadelphia map takes different considerations into account and does not restrict to only “mass shooting” events, specifically “city-wide shooting victims, including Police Officer-involved shootings.”

### **Disclaimer**
**This is in no way meant to reflect any political preferences, personal biases, or advocacy. What is presented here is the facts only, what viewers and users take away from them is their responsibility solely.**
