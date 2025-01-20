###############################################################################
##                              *TABLES AND CHARTS*                          ##
###############################################################################
# Author:     Becky Foster
# Script Description:   Outputs for the KSP
# -----------------------------------------------------------------------------

financial_years <- c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24", "2024/25")
principal_authorities <- c("SC", "UA", "MD", "LB", "SD")

## CHARTS (2015 ref) ----
    ## 1)	Bar chart of real terms ASC NCE: ASC (RO), ASC (ASC-FR) and ASC (ASC-FR + NHS) ----
    ## 2)	Indexed real terms ASC NCE: ASC (RO), ASC (ASC-FR), ASC (ASC-FR + NHS) and ASC-FR per capita ----
    ## 3)	Bar chart of real terms ASC NCE: CSC (RO) and CSC (S251) ----
    ## 4)	Indexed real terms CSC NCE: CSC (RO), CSC (S251) and CSC (S251) per capita ----
    ## 5)	Compound bar chart of real terms Total Social Care NCE (RO) ----
    ## 6)	Indexed real terms Total Social Care NCE (RO) ----
    ## 7)	Indexed real terms total social care expenditure, GNSS and TSE: TSC, ASC, CSC, TSE and GNSS ----
    ## 8)	Indexed real terms social care expenditure and CSP: CSC, TSC, ASC and CSP ----
    ## 9)	Compound bar chart of Social care NCE as a proportion of GNSS: ASC and CSC ----
    ## 10)	Compound bar chart of Social care NCE as a proportion of TSE: ASC and CSC ----
    ## 11)	Compound bar chart of Social care NCE as a proportion of CSP: ASC and CSC ----


library(ggplot2)
library(dplyr)
library(ggpattern)

# Create per capita versions of variables
Eng_data$ASC_ASCFR_pc <- Eng_data$ASC_ASCFR / Eng_data$adult_population

# Define colours for variables
variable_colours <- c(
    "ASC_RO" = chart_palette[1],
    "ASC_ASCFR" = chart_palette[1],
    "ASC_ASCFR_NHSspend" = chart_palette[15],
    "ASC_ASCFR_pc" = chart_palette[2],
    "CSC_RO" = chart_palette[2],
    "CSC_S251" = chart_palette[2],
    "CSC_S251_pc" = chart_palette[1],
    "SC_RO" = chart_palette[15],
    "TSE_RO" = chart_palette[3],
    "GNSS_RO" = chart_palette[3],
    "CSP" = chart_palette[3],
    "ASC_perc_GNSS" = chart_palette[1],
    "CSC_perc_GNSS" = chart_palette[2],
    "ASC_perc_TSE" = chart_palette[1],
    "CSC_perc_TSE" = chart_palette[2],
    "ASC_perc_CSP" = chart_palette[1],
    "CSC_perc_CSP" = chart_palette[2]
)

variable_names <- c(
    "ASC_RO" = "ASC (RO)",
    "ASC_ASCFR" = "ASC (ASC-FR)",
    "ASC_ASCFR_NHSspend" = "ASC (ASC-FR + NHS funded spend)",
    "ASC_ASCFR_pc" = "ASC (ASC-FR) per capita",
    "CSC_RO" = "CSC (RO)",
    "CSC_S251" = "CSC (S251)",
    "CSC_S251_pc" = "CSC (S251) per capita",
    "SC_RO" = "Total social care (RO)",
    "TSE_RO" = "TSE",
    "GNSS_RO" = "GNSS",
    "CSP" = "CSP",
    "ASC_perc_TSE" = "ASC as a % of TSE",
    "CSC_perc_TSE" = "CSC as a % of TSE",
    "ASC_perc_GNSS" = "ASC as a % of GNSS",
    "CSC_perc_GNSS" = "CSC as a % of GNSS",
    "ASC_perc_CSP" = "ASC as a % of CSP",
    "CSC_perc_CSP" = "CSC as a % of CSP"
)

ASC_chart_1 <- ggplot(Eng_data %>% 
           filter(year >= '2015/16' & year <= '2023/24') %>%
           mutate(
               ASC_RO = ASC_RO * deflator_ratio_2023_24,
               ASC_ASCFR = ASC_ASCFR * deflator_ratio_2023_24,
               ASC_ASCFR_NHSspend = ASC_ASCFR_NHSspend * deflator_ratio_2023_24
           ) %>%
           pivot_longer(cols = c(ASC_RO, ASC_ASCFR, ASC_ASCFR_NHSspend),
                        names_to = "variable", values_to = "value") %>%
           mutate(variable = factor(variable, 
                                    levels = c("ASC_RO", 
                                               "ASC_ASCFR", 
                                               "ASC_ASCFR_NHSspend"))),
       aes(x = year, y = value, fill = variable, pattern = variable)) +
    geom_bar_pattern(stat = "identity", position = "dodge", 
                     pattern_fill = "white", pattern_density = 0.3, 
                     pattern_spacing = 0.02, pattern_angle = 45, 
                     colour = "black") +  
    scale_fill_manual(values = variable_colours,  
                      labels = variable_names) +  
    scale_pattern_manual(values = c("ASC_RO" = "none", 
                                    "ASC_ASCFR" = "stripe", 
                                    "ASC_ASCFR_NHSspend" = "none"),  
                         labels = variable_names) +
    labs(title = "Real terms ASC Net Current Expenditure (NCE): RO & ASC-FR, 
All principal LAs, 2015-16 to 2023-24, £bns (2023-24 prices)",
         x = NULL,
         y = "£bns (2023-24 prices)",
         fill = NULL,  
         pattern = NULL) +
    theme_minimal()


ASC_chart_2 <- ggplot(Eng_data %>% 
           filter(year >= '2015/16' & year <= '2023/24') %>%
           mutate(
               ASC_RO = ASC_RO * deflator_ratio_2023_24,
               ASC_ASCFR = ASC_ASCFR * deflator_ratio_2023_24,
               ASC_ASCFR_NHSspend = ASC_ASCFR_NHSspend * deflator_ratio_2023_24,
               ASC_ASCFR_pc = ASC_ASCFR_pc * deflator_ratio_2023_24
           ) %>%
           pivot_longer(cols = c(ASC_RO, ASC_ASCFR, ASC_ASCFR_NHSspend, ASC_ASCFR_pc),
                        names_to = "variable", values_to = "value") %>%
           group_by(variable) %>%
           mutate(indexed_value = value / value[year == '2015/16'] * 100,  
       percentage_increase = (indexed_value[year == '2023/24'] - 100)),
       aes(x = year, 
           y = indexed_value, 
           color = variable, 
           linetype = variable,  
           group = variable)) +
    geom_line(linewidth = 1) +  
    geom_text(data = . %>% filter(year == '2023/24'),
              aes(label = paste0(round(percentage_increase, 1), "%")),
              hjust = -0.1, vjust = 0.5, size = 3) +
    scale_color_manual(values = variable_colours,  
                       labels = variable_names) +
    scale_linetype_manual(values = c("ASC_RO" = "solid", 
                                     "ASC_ASCFR" = "dashed", 
                                     "ASC_ASCFR_NHSspend" = "solid", 
                                     "ASC_ASCFR_pc" = "dotted"),  
                          labels = variable_names) +
    labs(title = "Indexed real terms ASC Net Current Expenditure (NCE): RO & ASC-FR, 
All principal LAs, 2015/16 to 2023/24, (2015/16 = 100)",
         x = NULL,
         y = "2015/16 = 100",
         color = NULL,
         linetype = NULL) +
    theme_minimal()





## TABLES (2010 ref) ----

# Initialise a blank excel workbook
wb <- createWorkbook()

    ## 1)	ASC NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 1")
       writeData(wb, "Table 1", df1)
    ## 2)	ASC NCE (ASC-FR): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 2")
    ## 3)	ASC NCE including NHS funding and BCF: nominal, real and annual real % change ----
       addWorksheet(wb, "Table 3")
    ## 4)	CSC NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 4")
    ## 5)	CSC S251: nominal, real and annual real % change ----
       addWorksheet(wb, "Table 5")
    ## 6)	Total Social Care NCE (RO): nominal, real and annual real % change ----
       addWorksheet(wb, "Table 6")
 
 # Finalise the workbook
 saveWorkbook(wb, "KSP_tables.xlsx", overwrite = TRUE)
