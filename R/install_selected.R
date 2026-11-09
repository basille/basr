#' Install a list of selected packages by theme
#'
#' Install a list of selected packages (from CRAN and GitHub) by theme (spatial,
#' moveco, tidyverse, tidymodels, plotting, data, stats, utils).
#'
#' Here is the full list of packages:
#'
#' @section Spatial:
#'
#' - autoimage: Multiple Heat Maps for Projected Coordinates
#' - concaveman: A Very Fast 2D Concave Hull Algorithm
#' - elevatr: Access Elevation Data from Various APIs
#' - geodata: Download Geographic Data
#' - ggspatial: Spatial Data Framework for ggplot2
#' - greenR (GitHub): Green Index Quantification, Analysis and Visualization
#' - gstat: Spatial and Spatio-Temporal Geostatistical Modelling, Prediction and Simulation
#' - landscapemetrics: Landscape Metrics for Categorical Map Patterns
#' - leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library
#' - lwgeom: Bindings to Selected 'liblwgeom' Functions for Simple Features
#' - mapsf: Thematic Cartography
#' - mapview: Interactive Viewing of Spatial Data in R
#' - raster: Geographic Data Analysis and Modeling
#' - rasterVis: Visualization Methods for Raster Data
#' - RColorBrewer: ColorBrewer Palettes
#' - rayshader: Create Maps and Visualize Data in 2D and 3D
#' - rnaturalearth: World Map Data from Natural Earth
#' - rnaturalearthdata: World Vector Map Data from Natural Earth Used in 'rnaturalearth'
#' - sf: Simple Features for R
#' - sp: Classes and Methods for Spatial Data
#' - spData: Datasets for spatial analysis
#' - spacetime: Classes and Methods for Spatio-Temporal Data
#' - spatstat: Spatial Point Pattern Analysis, Model-Fitting, Simulation, Tests
#' - spdep: Spatial Dependence: Weighting Schemes, Statistics and Models
#' - splancs: Spatial and Space-Time Point Pattern Analysis
#' - stars: Spatiotemporal Arrays, Raster and Vector Data Cubes
#' - terra: Spatial Data Analysis
#' - tidyterra: tidyverse' Methods and 'ggplot2' Utils for 'terra' Objects
#' - tmap: Thematic Maps
#'
#' @section Moveco:
#'
#' - adehabitatHS: Analysis of Habitat Selection by Animals
#' - amt: Animal Movement Tools
#' - biomod2: Ensemble Platform for Species Distribution Modeling
#' - ctmm: Continuous-Time Movement Modeling
#' - dismo: Species Distribution Modeling
#' - ecospat: Spatial Ecology Miscellaneous Methods
#' - basille/hab: Habitat and movement functions
#' - move: Visualizing and Analyzing Animal Track Data
#' - picardis/nestR: Estimation of Bird Nesting from Tracking Data
#' - rpostgis: R Interface to a 'PostGIS' Database
#' - sftrack: Modern Classes for Tracking and Movement Data
#'
#' @section Tidyverse:
#'
#' - dplyr: A Grammar of Data Manipulation
#' - forcats: Tools for Working with Categorical Variables (Factors)
#' - ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics
#' - lubridate: Make Dealing with Dates a Little Easier
#' - purrr: Functional Programming Tools
#' - readr: Read Tabular Data
#' - stringr: Simple, Consistent Wrappers for Common String Operations
#' - tibble: Simple Data Frames
#' - tidyr: Easily Tidy Data with 'spread()' and 'gather()' Functions
#' - DBI: R Database Interface
#' - googledrive: An Interface to Google Drive
#' - googlesheets4: Access Google Sheets using the Sheets API V4
#' - haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files
#' - httr: Tools for Working with URLs and HTTP
#' - jsonlite: A Robust, High Performance JSON Parser and Generator for R
#' - readxl: Read Excel Files
#' - rvest: Easily Harvest (Scrape) Web Pages
#' - xml2: Parse XML
#' - blob: A Simple S3 Class for Representing Vectors of Binary Data ('BLOBS')
#' - hms: Pretty Time of Day
#' - dbplyr: A 'dplyr' Back End for Databases
#' - dtplyr: Data Table Back-End for 'dplyr'
#' - duckplyr: A drop-in replacement for dplyr, powered by DuckDB for speed.
#' - glue: Interpreted String Literals
#' - magrittr: A Forward-Pipe Operator for R
#'
#' @section Tidymodels:
#'
#' See \url{https://www.tidymodels.org/packages/}
#'
#' @section Plotting extra:
#'
#' - adegraphics: An S4 Lattice-Based Package for the Representation of Multivariate Data
#' - Cairo: R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and Win32) output
#' - colorBlindness: Safe Color Set for Color Blindness
#' - colorRamps: Builds color tables
#' - cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'
#' - explor: Interactive Interfaces for Results Exploration
#' - extrafont: Tools for using fonts
#' - geomtextpath: Curved Text in 'ggplot2'
#' - ggalluvial: Alluvial Diagrams in 'ggplot2'
#' - ggalt (GitHub): Extra Coordinate Systems, Geoms, Statistical Transformations, Scales & Fonts for 'ggplot2' \[requires proj4\]
#' - gganimate: A Grammar of Animated Graphics
#' - ggbeeswarm: Categorical Scatter (Violin Point) Plots
#' - ggdark (GitHub): Dark Mode for 'ggplot2' Themes
#' - ggExtra: Add Marginal Histograms to 'ggplot2', and More 'ggplot2' Enhancements
#' - ggforce: Accelerating 'ggplot2'
#' - ggfortify: Data Visualization Tools for Statistical Analysis Results
#' - gghighlight: Highlight Lines and Points in 'ggplot2'
#' - ggpomological (GitHub): Pomological plot themes for ggplot2
#' - ggraph: An Implementation of Grammar of Graphics for Graphs and Networks
#' - ggrepel: Repulsive Text and Label Geoms for 'ggplot2'
#' - ggrough (GitHub): Converts ggplot2 plots to sketchy charts using rough.js
#' - ggshadow: Shadow and Glow Geoms for 'ggplot2'
#' - ggsunburst (GitHub): Sunburst diagrams in ggplot2
#' - ggtext: Improved text rendering support for ggplot2
#' - ggthemes: Extra Themes, Scales and Geoms for 'ggplot2'
#' - ggthemr (GitHub): ggplot2 themes (for posters/presentations)
#' - ggwordcloud: A Word Cloud Geom for 'ggplot2'
#' - Giftmap (GitHub): Create Decorative Town Maps (map posters)
#' - gridExtra: Miscellaneous Functions for "Grid" Graphics
#' - hexbin: Hexagonal Binning Routines
#' - hrbrthemes: Additional Themes, Theme Components and Utilities for 'ggplot2'
#' - factoextra: Extract and Visualize the Results of Multivariate Data Analyses
#' - patchwork: The Composer of Plots
#' - plotly: Create Interactive Web Graphics via 'plotly.js'
#' - rphylopic: Get Silhouettes of Organisms from 'Phylopic'
#' - scales: Scale Functions for Visualization
#' - scatterpie: Scatter Pie Plot
#' - tweenr: Interpolate Data for Smooth Animations
#' - viridis: Default Color Maps from 'matplotlib'
#' - wordcloud: Word Clouds
#' - xkcd: Plotting ggplot2 Graphics in an XKCD Style
#'
#' @section Data management:
#'
#' - anytime: Anything to 'POSIXct' or 'Date' Converter
#' - constants: Reference on Constants, Units and Uncertainty
#' - errors: Error Propagation for R Vectors
#' - fancycut: A Fancy Version of 'base::cut'
#' - foreign: Read Data Stored by Minitab, S, SAS, SPSS, Stata, Systat, Weka, dBase, ...
#' - janitor: Simple Tools for Examining and Cleaning Dirty Data
#' - padr: Quickly Get Datetime Data Ready for Analysis
#' - pointblank: Data Validation and Organization of Metadata for Local and Remote Tables
#' - summarytools: Tools to Quickly and Neatly Summarize Data
#' - tibbletime: Time Aware Tibbles
#' - units: Measurement Units for R Vectors
#'
#' @section Stats:
#'
#' - ade4: Analysis of Ecological Data: Exploratory and Euclidean Methods in Environmental Sciences
#' - broom: Convert Statistical Analysis Objects into Tidy Data Frames
#' - CircStats: Circular Statistics, from "Topics in circular Statistics" (2001)
#' - coxme: Mixed Effects Cox Models
#' - lme4: Linear Mixed-Effects Models using 'Eigen' and S4
#' - lsmeans: Least-Squares Means
#' - MuMIn: Multi-Model Inference
#' - popbio: Construction and Analysis of Matrix Population Models
#' - randomForest: Breiman and Cutler's Random Forests for Classification and Regression
#' - RMark: R Code for Mark Analysis
#' - visreg: Visualization of Regression Models
#'
#' @section Utils:
#'
#' - blogdown: Create Blogs and Websites with R Markdown
#' - bookdown: Authoring Books and Technical Documents with R Markdown
#' - calendR: Ready to Print Monthly and Yearly Calendars Made with 'ggplot2'
#' - charlatan: Make Fake Data
#' - conflicted: An Alternative Conflict Resolution Strategy
#' - cowsay: Messages, Warnings, Strings with Ascii Animals
#' - ctv: CRAN Task Views
#' - default: Change the Default Arguments in R Functions
#' - devtools: Tools to Make Developing R Packages Easier
#' - diffr: Display Differences Between Two Files using Codediff Library
#' - distill: R Markdown' Format for Scientific and Technical Writing
#' - equatiomatic: Transform Models into 'LaTeX' Equations
#' - formatR: Format R Code Automatically
#' - formattable: Create 'Formattable' Data Structures
#' - fortunes: R Fortunes
#' - Hmisc: Harrell Miscellaneous
#' - httpgd: A 'HTTP' Server Graphics Device
#' - ids: Generate Random Identifiers
#' - kableExtra: Construct Complex Table with 'kable' and Pipe Syntax
#' - knitr: A General-Purpose Package for Dynamic Report Generation in R
#' - latex2exp: Use LaTeX Expressions in Plots
#' - liftr: Containerize R Markdown Documents for Continuous Reproducibility
#' - limer (GitHub): A LimeSurvey R client
#' - lintr: Static R Code Analysis
#' - magick: Advanced Graphics and Image-Processing in R
#' - mailR: A Utility to Send Emails from R
#' - memer (GitHub): Creating memes with R and magick
#' - nomnoml: Sassy 'UML' Diagrams
#' - packagefinder (GitHub): Comfortable Search for R Packages on CRAN
#' - palmerpenguins: Palmer Archipelago (Antarctica) Penguin Data
#' - pkgdown: Make Static HTML Documentation for a Package
#' - printr: Automatically Print R Objects to Appropriate Formats According to the 'knitr' Output Format
#' - processx: Execute and Control System Processes
#' - remotes: R Package Installation from Remote Repositories, Including 'GitHub'
#' - renv: Project Environments
#' - rmarkdown: Dynamic Documents for R
#' - rmote (GitHub): Running R on a remote server
#' - rorcid: Interface to the 'Orcid.org' 'API'
#' - roxygen2: In-Source Documentation for R
#' - rticles: Article Formats for R Markdown
#' - scholar: Analyse Citation Data from Google Scholar
#' - sessioninfo: R Session Information
#' - shiny: Web Application Framework for R
#' - styler: Non-Invasive Pretty Printing of R Code
#' - sudokuAlt: Tools for Making and Spoiling Sudoku Games
#' - tigris: Download TIGER/Line shapefiles from the United States Census Bureau and load into R as 'SpatialDataFrame' or 'sf' objects
#' - tint: tint' is not 'Tufte'
#' - tkrplot: TK Rplot
#' - vitae: Curriculum Vitae for R Markdown
#' - waldo: Find Differences Between R Objects
#'
#' @param spatial Logical, whether to install `spatial` packages (defaults to
#'     \code{TRUE}).
#' @param moveco Logical, whether to install `moveco` packages (defaults to
#'     \code{TRUE}).
#' @param tidyverse Logical, whether to install `tidyverse` packages (defaults
#'     to \code{TRUE}).
#' @param tidymodels Logical, whether to install `tidymodels` packages (defaults
#'     to \code{FALSE}).
#' @param plotting Logical, whether to install `plotting` packages (defaults to
#'     \code{TRUE}).
#' @param data Logical, whether to install `data` packages (defaults to
#'     \code{TRUE}).
#' @param stats Logical, whether to install `stats` packages (defaults to
#'     \code{TRUE}).
#' @param utils Logical, whether to install `utils` packages (defaults to
#'     \code{TRUE}).
#' @note Requires package \pkg{pak}.
#' @author Mathieu Basille <\email{mathieu@@basille.org}
#' @export
#' @returns (Invisibly) A data frame with information about the installed
#'     package(s).
#' @examples
##' \dontrun{
#' ## Install all packages (except 'tidymodels'):
#' install_selected()
#'
#' ## Only install spatial packages:
#' install_selected(spatial = TRUE, moveco = FALSE, tidyverse = FALSE, tidymodels = FALSE,
#'     plotting = FALSE, data = FALSE, stats = FALSE, utils = FALSE)}
#' @importFrom utils installed.packages
install_selected <- function(spatial, moveco, tidyverse, tidymodels,
  plotting, data, stats, utils) {
    ## `pak` is required
    if (!requireNamespace("pak", quietly = TRUE)) {
        stop(
            "Package \"pak\" must be installed to use this function.",
            call. = FALSE
        )
    }
    ## Defaults
    if (missing(spatial)) spatial <- TRUE
    if (missing(moveco)) moveco <- TRUE
    if (missing(tidyverse)) tidyverse <- TRUE
    if (missing(tidymodels)) tidymodels <- FALSE
    if (missing(plotting)) plotting <- TRUE
    if (missing(data)) data <- TRUE
    if (missing(stats)) stats <- TRUE
    if (missing(utils)) utils <- TRUE
    ## Spatial packages https://cran.r-project.org/web/views/Spatial.html
    spatial.list <- c(
        "autoimage",        # Multiple Heat Maps for Projected Coordinates
        "concaveman",       # A Very Fast 2D Concave Hull Algorithm
        "elevatr",          # Access Elevation Data from Various APIs
        "geodata",          # Download Geographic Data
        "ggspatial",        # Spatial Data Framework for ggplot2
        "sachit27/greenR",  # Green Index Quantification, Analysis and Visualization
        "gstat",            # Spatial and Spatio-Temporal Geostatistical Modelling, Prediction and Simulation
        "landscapemetrics", # Landscape Metrics for Categorical Map Patterns
        "leaflet",          # Create Interactive Web Maps with the JavaScript 'Leaflet' Library
        "lwgeom",           # Bindings to Selected 'liblwgeom' Functions for Simple Features
        "mapsf",            # Thematic Cartography
        "mapview",          # Interactive Viewing of Spatial Data in R
        "raster",           # Geographic Data Analysis and Modeling
        "rasterVis",        # Visualization Methods for Raster Data
        "RColorBrewer",     # ColorBrewer Palettes
        "rayshader",        # Create Maps and Visualize Data in 2D and 3D
        "rnaturalearth",    # World Map Data from Natural Earth
        "rnaturalearthdata", # World Vector Map Data from Natural Earth Used in 'rnaturalearth'
        "sf",               # Simple Features for R
        "sp",               # Classes and Methods for Spatial Data
        "spData",           # Datasets for spatial analysis
        "spacetime",        # Classes and Methods for Spatio-Temporal Data
        "spatstat",         # Spatial Point Pattern Analysis, Model-Fitting, Simulation, Tests
        "spdep",            # Spatial Dependence: Weighting Schemes, Statistics and Models
        "splancs",          # Spatial and Space-Time Point Pattern Analysis
        "stars",            # Spatiotemporal Arrays, Raster and Vector Data Cubes
        "terra",            # Spatial Data Analysis
        "tidyterra",        # 'tidyverse' Methods and 'ggplot2' Utils for 'terra' Objects
        "tmap"              # Thematic Maps
    )
    ## adehabitat, rpostgis and friends
    moveco.list <- c(
        "adehabitatHS",     # Analysis of Habitat Selection by Animals
        "amt",              # Animal Movement Tools
        "biomod2",          # Ensemble Platform for Species Distribution Modeling
        "ctmm",             # Continuous-Time Movement Modeling
        "dismo",            # Species Distribution Modeling
        "ecospat",          # Spatial Ecology Miscellaneous Methods
        "basille/hab",      # Habitat and movement functions
        "move",             # Visualizing and Analyzing Animal Track Data
        "picardis/nestR",   # Estimation of Bird Nesting from Tracking Data
        "rpostgis",         # R Interface to a 'PostGIS' Database
        "sftrack"           # Modern Classes for Tracking and Movement Data
        ## "rpostgisLT"        # Managing Animal Movement Data with 'PostGIS' and R
    )
    ## Tidyverse & Tidymodels
    tidyverse.list <- c(
        "tidyverse"          # Easily Install and Load 'Tidyverse' Packages
    )
    tidymodels.list <- c(
        "tidymodels"          # Easily Install and Load 'Tidymodels' Packages
    )
    ## Plotting extra
    plotting.list <- c(
        "adegraphics",       # An S4 Lattice-Based Package for the Representation of Multivariate Data
        "Cairo",             # R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF), vector (PDF, SVG, PostScript) and display (X11 and Win32) output
        "colorBlindness",    # Safe Color Set for Color Blindness
        "colorRamps",        # Builds color tables
        "cowplot",           # Streamlined Plot Theme and Plot Annotations for 'ggplot2'
        "explor",            # Interactive Interfaces for Results Exploration
        "extrafont",         # Tools for using fonts
        "geomtextpath",      # Curved Text in 'ggplot2'
        "ggalluvial",        # Alluvial Diagrams in 'ggplot2'
        "hrbrmstr/ggalt",    # Extra Coordinate Systems, Geoms, Statistical Transformations, Scales & Fonts for 'ggplot2' [requires proj4]
        "gganimate",         # A Grammar of Animated Graphics
        "ggbeeswarm",        # Categorical Scatter (Violin Point) Plots
        "nsgrantham/ggdark", # Dark Mode for 'ggplot2' Themes
        "ggExtra",           # Add Marginal Histograms to 'ggplot2', and More 'ggplot2' Enhancements
        "ggforce",           # Accelerating 'ggplot2'
        "ggfortify",         # Data Visualization Tools for Statistical Analysis Results
        "gghighlight",       # Highlight Lines and Points in 'ggplot2'
        "gadenbuie/ggpomological", # Pomological plot themes for ggplot2
        "ggraph",            # An Implementation of Grammar of Graphics for Graphs and Networks
        "ggrepel",           # Repulsive Text and Label Geoms for 'ggplot2'
        "xvrdm/ggrough",     # Converts ggplot2 plots to sketchy charts using rough.js
        "ggshadow",          # Shadow and Glow Geoms for 'ggplot2'
        "didacs/ggsunburst", # Sunburst diagrams in ggplot2
        "ggtext",            # Improved text rendering support for ggplot2
        "ggthemes",          # Extra Themes, Scales and Geoms for 'ggplot2'
        "cttobin/ggthemr",   # ggplot2 themes (for posters/presentations)
        "ggwordcloud",       # A Word Cloud Geom for 'ggplot2'
        "lina2497/Giftmap",  # Create Decorative Town Maps (map posters)
        "gridExtra",         # Miscellaneous Functions for "Grid" Graphics
        "hexbin",            # Hexagonal Binning Routines
        "hrbrthemes",        # Additional Themes, Theme Components and Utilities for 'ggplot2'
        "factoextra",        # Extract and Visualize the Results of Multivariate Data Analyses
        "patchwork",         # The Composer of Plots
        "plotly",            # Create Interactive Web Graphics via 'plotly.js'
        "rphylopic",         # Get Silhouettes of Organisms from 'Phylopic'
        "scales",            # Scale Functions for Visualization
        "scatterpie",        # Scatter Pie Plot
        "tweenr",            # Interpolate Data for Smooth Animations
        "viridis",           # Default Color Maps from 'matplotlib'
        "wordcloud",         # Word Clouds
        "xkcd"               # Plotting ggplot2 Graphics in an XKCD Style
    )
    ## Data management
    data.list <- c(
        "anytime",           # Anything to 'POSIXct' or 'Date' Converter
        "constants",         # Reference on Constants, Units and Uncertainty
        "errors",            # Error Propagation for R Vectors
        "fancycut",          # A Fancy Version of 'base::cut'
        "foreign",           # Read Data Stored by Minitab, S, SAS, SPSS, Stata, Systat, Weka, dBase, ...
        "janitor",           # Simple Tools for Examining and Cleaning Dirty Data
        "padr",              # Quickly Get Datetime Data Ready for Analysis
        "pointblank",        # Data Validation and Organization of Metadata for Local and Remote Tables
        "summarytools",      # Tools to Quickly and Neatly Summarize Data
        "tibbletime",        # Time Aware Tibbles
        "units"              # Measurement Units for R Vectors
    )
    ## Modeling
    stats.list <- c(
        "ade4",              # Analysis of Ecological Data: Exploratory and Euclidean Methods in Environmental Sciences
        "broom",             # Convert Statistical Analysis Objects into Tidy Data Frames
        "CircStats",         # Circular Statistics, from "Topics in circular Statistics" (2001)
        "coxme",             # Mixed Effects Cox Models
        "lme4",              # Linear Mixed-Effects Models using 'Eigen' and S4
        "lsmeans",           # Least-Squares Means
        "MuMIn",             # Multi-Model Inference
        "popbio",            # Construction and Analysis of Matrix Population Models
        "randomForest",      # Breiman and Cutler's Random Forests for Classification and Regression
        "RMark",             # R Code for Mark Analysis
        "visreg"             # Visualization of Regression Models
    )
    ## Other packages
    utils.list <- c(
        "blogdown",          # Create Blogs and Websites with R Markdown
        "bookdown",          # Authoring Books and Technical Documents with R Markdown
        "calendR",           # Ready to Print Monthly and Yearly Calendars Made with 'ggplot2'
        "charlatan",         # Make Fake Data
        "conflicted",        # An Alternative Conflict Resolution Strategy
        "cowsay",            # Messages, Warnings, Strings with Ascii Animals
        "ctv",               # CRAN Task Views
        "default",           # Change the Default Arguments in R Functions
        "devtools",          # Tools to Make Developing R Packages Easier
        "diffr",             # Display Differences Between Two Files using Codediff Library
        "distill",           # 'R Markdown' Format for Scientific and Technical Writing
        "equatiomatic",      # Transform Models into 'LaTeX' Equations
        "formatR",           # Format R Code Automatically
        "formattable",       # Create 'Formattable' Data Structures
        "fortunes",          # R Fortunes
        "Hmisc",             # Harrell Miscellaneous
        "httpgd",            # A 'HTTP' Server Graphics Device
        "ids",               # Generate Random Identifiers
        "kableExtra",        # Construct Complex Table with 'kable' and Pipe Syntax
        "knitr",             # A General-Purpose Package for Dynamic Report Generation in R
        "latex2exp",         # Use LaTeX Expressions in Plots
        "liftr",             # Containerize R Markdown Documents for Continuous Reproducibility
        "cloudyr/limer",     # A LimeSurvey R client
        "lintr",             # Static R Code Analysis
        "magick",            # Advanced Graphics and Image-Processing in R
        "mailR",             # A Utility to Send Emails from R
        "sctyner/memer",     # Creating memes with R and magick
        "nomnoml",           # Sassy 'UML' Diagrams
        "jsugarelli/packagefinder", # Comfortable Search for R Packages on CRAN
        "palmerpenguins",    # Palmer Archipelago (Antarctica) Penguin Data
        "pkgdown",           # Make Static HTML Documentation for a Package
        "printr",            # Automatically Print R Objects to Appropriate Formats According to the 'knitr' Output Format
        "processx",          # Execute and Control System Processes
        "remotes",           # R Package Installation from Remote Repositories, Including 'GitHub'
        "renv",              # Project Environments
        "rmarkdown",         # Dynamic Documents for R
        "cloudyr/rmote",     # Running R on a remote server
        "rorcid",            # Interface to the 'Orcid.org' 'API'
        "roxygen2",          # In-Source Documentation for R
        "rticles",           # Article Formats for R Markdown
        "scholar",           # Analyse Citation Data from Google Scholar
        "sessioninfo",       # R Session Information
        "shiny",             # Web Application Framework for R
        "styler",            # Non-Invasive Pretty Printing of R Code
        "sudokuAlt",         # Tools for Making and Spoiling Sudoku Games
        "tigris", # Download TIGER/Line shapefiles from the United States Census Bureau and load into R as 'SpatialDataFrame' or 'sf' objects
        "tint",              # 'tint' is not 'Tufte'
        "tkrplot",           # TK Rplot
        "vitae",             # Curriculum Vitae for R Markdown
        "waldo"              # Find Differences Between R Objects
    )
    ## Full list of packages
    packages.list <- data.frame(
        full_name = c(
            if (spatial) spatial.list,
            if (moveco) moveco.list,
            if (tidyverse) tidyverse.list,
            if (tidymodels) tidymodels.list,
            if (plotting) plotting.list,
            if (data) data.list,
            if (stats) stats.list,
            if (utils) utils.list))
    ## Extract exact name of package
    packages.list$package <- ifelse(grepl("/", packages.list$full_name),
        sapply(packages.list$full_name, function(ll) {
            strsplit(ll, "/")[[1]][2]
        }), packages.list$full_name)
    ## Installation of new packages (i.e. not previously installed):
    packages.list$installed <- packages.list$package %in%
        installed.packages()[, "Package"]
    if (sum(packages.list$installed) != nrow(packages.list)) {
        message(paste0("Installing packages (with dependencies):
  ",
            paste(packages.list$package[!packages.list$installed],
                collapse = "
  ")))
        pak::pkg_install(packages.list$full_name[!packages.list$installed])
    } else message("No new package to install.")
}
