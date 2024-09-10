An R package to conduct location quality-control of IMOS-deployed SMRU SRDL-CTD tag data. The ArgosQC automatically does the following:
1. accesses SMRU tag data from the SMRU server
2. organizes the multi-file data structures
3. organizes associated deployment metadata
4. collates the tag data with deployment metadata
5. fits SSM's to species-specific subsets of the data
6. appends SSM-estimated locations to every tag-measured event record (CTD, dive, haulout, raw Argos location, raw GPS location, etc)
7. writes appended tag files to .csv
8. pushes to an incoming server
