# EEB_Dissertation: Investigating Density-Dependent Life-History Traits in Red Deer on the Isle of Rum

Welcome to the repository for my dissertation, which explores the density-dependent effects on life-history traits of red deer (*Cervus elaphus*) on the Isle of Rum, Scotland. This research uses five decades of ecological data and advanced statistical modeling to uncover how population density influences traits like birth weight, first winter survival, yearling spike length, and fecundity.

## Table of Contents
- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Data Description](#data-description)
- [Analysis Workflow](#analysis-workflow)
- [Results](#results)
- [Usage](#usage)
- [Citing this Work](#citing-this-work)
- [Acknowledgments](#acknowledgments)

## Overview
This project aims to refine our understanding of how population density impacts various life-history traits in red deer by leveraging an extended dataset and advanced modeling techniques. The Isle of Rum provides a unique environment for studying these dynamics in a setting with minimal human interference, offering valuable insights for broader ecological management.

## Repository Structure
| **File/Folder**                            | **Description**                                                                                                                                                                                                                                      |
|--------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `README.md`                                | Main documentation file providing an overview of the project, instructions for usage, and citation details.                                                                                                                                           |
| `data/`                                    | Contains all raw and processed data files used in the analysis. This includes population data, life-history traits data, and environmental variables related to the red deer population on the Isle of Rum.                                           |
| `r_scripts/`                               | Contains R scripts for various stages of the analysis, including data cleaning, model fitting, and graph creation.                                                                                                                   |                                                     |                                                           |
| `References/` | list of all papers referenced and PDF documents of some papers referenced in the study.                                                                           |
| `Weekly updates`  | Documents of weekly progress and discussion.                                                                         |
| `B141395.pdf`             | PDF document of the final dissertation.                   |

## Data Description
The data used in this research spans from 1973 to 2022 and includes detailed records of red deer population metrics, life-history traits, and environmental variables from the Isle of Rum. Key datasets include:
- **Population Data**: Records of population sizes for hinds, stags, and calves, as well as total livestock units.
- **Life-History Traits**: Data on birth weight, first winter survival, yearling spike length, and fecundity.
- **Environmental Variables**: Yearly environmental data that may influence life-history traits.

## Analysis Workflow
1. **Data Cleaning**: Initial preprocessing of raw data to prepare it for analysis. Scripts for this step are located in the `Scripts/01_Data_Cleaning/` directory.
2. **Exploratory Data Analysis**: Descriptive statistics and visualizations to explore trends and relationships in the data (`Scripts/02_EDA/`).
3. **Model Fitting**: Application of generalized linear mixed models (GLMMs) to assess density-dependent effects on life-history traits (`Scripts/03_Model_Fitting/`).
4. **Results Interpretation**: Generation of figures and tables summarizing the model outputs, with an emphasis on identifying significant patterns and temporal trends (`Results/`).

## Results
The main findings of the research indicate that:
- **Birth Weight**: Largely unaffected by population density, highlighting its stability across environmental conditions.
- **First Winter Survival**: Shows a significant negative correlation with hind density, suggesting that resource competition is a key driver of juvenile mortality.
- **Yearling Spike Length**: Exhibits complex temporal trends, with a gradual increase over time and reduced sensitivity to density in later years.
- **Fecundity**: Demonstrates intricate interactions with density, particularly when accounting for female reproductive status.

## Usage
To replicate the analysis or explore the data:
1. Clone the repository: `git clone https://github.com/Hayward-Wong/EEB_Dissertation.git`
2. Ensure you have R (version 4.3.1 or later) installed, along with the necessary packages listed in `Scripts/requirements.txt`.
3. Run the scripts in the order specified under [Analysis Workflow](#analysis-workflow).
