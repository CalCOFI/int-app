The CalCOFI Integrated Application is using the 
[integrated database](https://calcofi.io/docs/db.html#integrated-database-ingestion-strategy) 
constructed from the following datasets:

- [Bottle Database – CalCOFI](https://calcofi.org/data/oceanographic-data/bottle-database/) ([dataset page ↗](https://calcofi.io/datasets/calcofi_bottle/))
- [Fish Eggs & Larvae – CalCOFI](https://calcofi.org/data/marine-ecosystem-data/fish-eggs-larvae/) ([dataset page ↗](https://calcofi.io/datasets/swfsc_ichthyo/))

This app is open-source with code found here:

- [github.com/CalCOFI/db-viz-hex](https://github.com/CalCOFI/db-viz-hex)

The following key technologies enable this app:

- [R](https://www.r-project.org/): programming language for statistical computing
- [Shiny](https://shiny.rstudio.com/): web application framework for R
- [DuckDB](https://duckdb.org/): in-process SQL OLAP database management system
- [H3](https://www.uber.com/blog/h3/): hexagonal hierarchical geospatial indexing system
- [mapgl](https://walker-data.com/mapgl/): R interface to MapLibre GL JS for interactive maps

This app was developed by:

- [Ivy Li](https://www.linkedin.com/in/ivy-li-268ab330a/): CalCOFI intern from UCSB (BS in Physics and Statistics & Data Science)
- [Ben Best](https://ecoquants.com/about/#ben-best-phd): CalCOFI contractor, marine data scientist at EcoQuants LLC
- [Erin Satterthwaite](https://calcofi.org/about/staff/erin-satterthwaite/): CalCOFI Program Coordinator 
