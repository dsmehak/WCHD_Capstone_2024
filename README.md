# **Interactive Dashboard for Traffic Accident Visualization and Prediction in Washtenaw County**  

### **Shiny App Access**  
▶ **[Launch the Shiny App](https://mehakds.shinyapps.io/capstone_WCHD_2024/)**  

### **Running Locally**  
To run the app locally, execute `server.R`.  

---

## **Project Overview**  
This repository contains the files for my final Capstone project, presented as part of my **UChicago MPH Data Science Concentration Capstone** in partnership with the **Washtenaw County Health Department (WCHD).** The goal of this project was to develop an interactive **Shiny application** to visualize and predict traffic accident data in Washtenaw County, analyzing factors relevant to public health.  

### **Key Findings & Model Performance**  
- The dataset contained **10,037 total accidents (2023), with 20 fatal accidents**.  
- Among fatal accidents:  
  - **7 involved a drunk driver**  
  - **5 involved drug use**  
- Initial models attempted to predict **accident fatality or severity**, but severe class imbalance (very few fatal accidents) led to **overfitting**, even with oversampling.  
- The focus shifted to predicting **Unsafe Driver involvement**, defined as:  
  - **Drinking**  
  - **Drug Use**  
  - **Driver under 16 or over 75**  

### **Model Performance**  
- **Baseline Model:** Logit GLM (AUC = **0.607**)  
  - Features: **Day of week, time of day, 6-mile grid location, worst injury in crash, urban/rural status, weather, road conditions**  
- **Other Models Tested:**  
  - **Random Forest (AUC = 0.5599)**
  - **XGBoost (performed worse than GLM)**  
- The **GLM model** was ultimately used to predict the probability of an accident involving an unsafe driver.  

---

## **Repository Structure**  

📂 **Files & Descriptions:**  

| File | Description |
|------|------------|
| `server.R` | Contains UI code, interactive maps, and server logic. |
| `model.R` | Finalized **GLM predictive model**, used for making predictions. |
| `dataprocessing.R` | Reads, cleans, and processes data, preparing predictive variables. |
| `model_testing.R` | Trains, tests, and evaluates **GLM, Random Forest, and XGBoost** models. |

---

## **Notes**  
- **Data Limitations:** The dataset was highly imbalanced, making severity and fatality predictions difficult.  
- **Computer Limitations:** Grid-Based accident location is a computationally intensive but important outcome to consider predicting.  
