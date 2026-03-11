# SDMAT: Species Distribution Mapping and Analysis Tool

**SDMAT** is an R-based Shiny application designed to automate the generation of species distribution maps and calculate key IUCN Red List range metrics. It provides a user-friendly graphical interface to process batch data.


## 🌟 Key Features
* **Interactive GUI:** Built with `shinydashboard` for easy folder selection and real-time processing feedback.
* **IUCN Compliant Metrics:**
* **Area of Occupancy (AOO):** Calculated using the standard $2 \times 2$ km grid cells.
* **Extent of Occurrence (EOO):** Calculated via the Minimum Convex Polygon (MCP) method.
* **Batch Processing:** Processes entire folders of species shapefiles automatically.

## 🚀 Installation & Setup

### Prerequisites
You must have [R and RStudio](https://posit.co/download/rstudio-desktop/) installed.

### Running the Tool
1. Clone this repository or download the `SDMAT.R` file.
2. Open `SDMAT.R` in RStudio.
3. The app will automatically check for and install missing packages (`sf`, `redlistr`, `shiny`, etc.).
4. Click the **"Run App"** button or type `shiny::runApp()` in the console.

## 📋 How to Use

1. **Select Input Folder:** Choose the directory containing your `.shp` (shapefiles).
2. **Select Output Folder (optional)**: Choose where results and maps are saved. If not set, the input folder is used.
3. **Bathymetry (optional)**: Enable the checkbox and select a depth raster (.tif, .asc, or .nc) to add bathymetry to maps.
4. **Map Format**: Choose "Static Maps (PNG)" or "Interactive Maps + Static Maps". Interactive maps are shown in the app but not saved to disk.
5. **Process**: Click "Process Species Data" and wait for the progress bar to finish.
6. **Results**: Use the Results tab to view the table and the Maps tab to browse species maps.
7. **Download**: After processing, use "Download Results (Excel)" to get Species_Distribution_Results.xlsx.
8. **Output files**:
    - Species_Distribution_Results.xlsx – AOO and EOO metrics
    - Species_Plots/ – PNG distribution maps (one per species)
    - Both are written to the output folder if set, otherwise to the input folder.

## 📝 Citation
If you use SDMAT in published Red List assessments or research papers, please cite it as:

> Alperen Kantarci, Marielle Dumestre, Julia Sigwart, Visvanathan Ramesh. (2026). SDMAT: Species Distribution Mapping and Analysis Tool (Version 1.0.0). GitHub repository: [https://github.com/Alpkant/SDMAT](https://github.com/Alpkant/SDMAT)

## ⚖️ License & Disclaimer
**Disclaimer:** This software is an experimental research tool. Results are generated automatically and should be independently verified before use in formal conservation assessments or publications.

This project is licensed under the **MIT License** permitting free use with no liability or warranty.
