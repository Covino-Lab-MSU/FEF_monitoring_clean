## Data cleaning for the Fraser Experimental Forest project monitoring

This repository uses data downloaded from field instrumentation maintained within streams and shallow groundwater wells within the Fraser Experimental Forest

### Goals:
    
   * Aggregate all downloaded files for a given stream reach and instrument type
   * Look for anomalies within the data and remove data not representative of environmental parameters
   * Compare instrumenation readings with manual readings (if available) and make adjustments as necessary 
   * Visualize clean data
   * Output clean data to 'clean' data file for future use in analysis
    
### Setup:
    
   Required packages are found in the repository .yml. To set up this repository, you will need to:
   1. Create the conda envrironment included in the repository
   2. Install datatools package with 'pip install -e?
   3. run the main bash script
   
#### Data Collection  and analysis 

Notebooks containing Python script required to collect, review, and analyze data are contained in the "scripts" directory and numbered according to workflow beginning with "01_download_clip_merge".  All datasets can be imported via url and clipped to an area of interest, though script to save large, clipped datasets locally may be used to avoid repeated downloads. Scripts after "01_download_clip_merge" may call these stored clipped and merged datasets for visualization and analysis. Updated project progress may be found in the 'blog_post.ipynb' notebook.

### Maintainers:
Lauren Kremer, 
Research Associate, 
Watershed Research Group at Colorado State University
![Lauren Kremer](https://avatars.githubusercontent.com/u/70210769?v=4)
     

### License   




