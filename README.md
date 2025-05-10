# Global Aid Dashboard

## Project Overview
This project is an interactive Shiny Dashboard that analyzes international foreign aid commitments using the AidData Level 1 dataset.  
The dashboard visualizes aid flows between donor and recipient countries across different sectors, time periods, and regions.

The goal is to provide dynamic, multi-faceted insights into who gives, who receives, how much aid flows, and how aid trends evolved before and after 2008.

---

## Datasets Used
- **AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.0.csv**  
  (Public research dataset on global development aid)
- **World_Countries_(Generalized)_9029012925078512962.geojson**  
  (World map shape data for country-level visualization)

---

## Dashboard Structure

The application is organized into **5 major tabs**:

### 1. Mohd's Dashboard
- Focus: Top donor-recipient relationships.
- Visualizations:
  - Bubble plot of largest aid flows.
  - Sankey diagram of aid between countries.
  - Treemap of donor-recipient spending.
  - Interactive data table.

### 2. Rajat's Dashboard
- Focus: Patterns across all donor-recipient pairs.
- Visualizations:
  - Bar chart of top donor-recipient pairs.
  - Heatmap of donor vs recipient totals.
  - Treemap visualization.
  - Network graph.
  - Chord diagram.

### 3. Shreya's Dashboard
- Focus: Trends in total aid over time and by top donors.
- Visualizations:
  - Global total aid over years.
  - Bar charts and bubble plots for top donors.
  - Donor trends grid.
  - Heatmap over time.
  - Static treemap of cumulative donor aid.

### 4. Premann's Dashboard
- Focus: Sector-wise distribution of aid.
- Visualizations:
  - Interactive bar chart of top broad categories receiving aid.
  - Adjustable year range and number of sectors to show.

### 5. Marissa's Dashboard
- Focus: Changes in global aid dynamics around the 2008 financial crisis.
- Visualizations:
  - World maps showing donor commitments before (2005–2007) and after (2008–2010).
  - Select country for individual donor/recipient analysis.
  - Line charts showing before/after trends by country.



