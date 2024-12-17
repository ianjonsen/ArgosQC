An R package to conduct unsupervised location quality-control of IMOS-deployed SMRU SRDL-CTD tag data in near real-time. {ArgosQC} automatically does the following:
1. accesses SMRU tag data from the SMRU server
2. organizes the multi-file data structures
3. organizes associated deployment metadata
4. collates the tag data with deployment metadata
5. fits SSM's to species-specific subsets of the data
6. appends SSM-estimated locations to every tag-measured event record (CTD, dive, haulout, raw Argos location, raw GPS location, etc)
7. writes appended tag files to .csv
8. pushes to an incoming server

The QC process is detailed in the IMOS [Best Practice Manual](https://repository.oceanbestpractices.org/handle/11329/2571)


# Acknowledgements:

### Financial support for the development of this software is provided by:

### [LOCEAN, France](https://www.locean.ipsl.fr) via Laurent MORTIER & Jean-Benoît CHARRASSIN

### [IMOS, Australia](https://imos.org.au) & [SIMS, Australia](https://sims.org.au) via Robert HARCOURT & Clive MCMAHON
