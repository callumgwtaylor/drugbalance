#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggthemes)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    

    output$barchart <- renderPlot({
        
        # Create a dataframe 
        drugs <- tibble(drug = NA,
                        dose = NA,
                        ovsb = NA, #Is it an Opiate O or a Benzo B
                        filler = NA) #What colour should it be?
        
        drugs <- drugs %>%
            add_row(drug = "codeine_oral", ovsb = "Opiate", dose = input$codeine_oral / 10) %>%
            add_row(drug = "tramadol_oral", ovsb = "Opiate", dose = input$tramadol_oral / 10) %>%
            add_row(drug = "morphine_oral", ovsb = "Opiate", dose = input$morphine_oral * 1) %>%
            add_row(drug = "morphine_subcut", ovsb = "Opiate", dose = input$morphine_subcut * 2) %>%
            add_row(drug = "diamorphine_subcut", ovsb = "Opiate", dose = input$diamorphine_subcut * 3) %>%
            add_row(drug = "oxycodone_oral", ovsb = "Opiate", dose = input$oxycodone_oral * 2) %>%
            add_row(drug = "oxycodone_subcut", ovsb = "Opiate", dose = input$oxycodone_subcut * 4) %>%
            add_row(drug = "alfentanil_subcut", ovsb = "Opiate", dose = input$alfentanil_subcut * 30) %>%
            add_row(drug = "hydromorphone_oral", ovsb = "Opiate", dose = input$hydromorphone_oral * 7) %>%
            add_row(drug = "hydromorphone_subcut", ovsb = "Opiate", dose = input$hydromorphone_subcut * 15) %>%
            add_row(drug = "diazepam_oral", ovsb = "Benzo", dose = input$diazepam_oral * 1) %>%
            add_row(drug = "clobazam_oral", ovsb = "Benzo", dose = input$clobazam_oral /2) %>%
            add_row(drug = "clonazepam_oral", ovsb = "Benzo", dose = input$clonazepam_oral / 10) %>%
            add_row(drug = "lorazepam_oral", ovsb = "Benzo", dose = input$lorazepam_oral / 10) %>%
            add_row(drug = "midazolam_subcut", ovsb = "Benzo", dose = input$midazolam_subcut * 2) %>%
            add_row(drug = "nitrazepam_oral", ovsb = "Benzo", dose = input$nitrazepam_oral * 1) %>%
            add_row(drug = "temazepam_oral", ovsb = "Benzo", dose = input$temazepam_oral / 2) %>%
            add_row(drug = "fentanyl_patch", ovsb = "Opiate", dose = as.numeric(input$fentanyl_patch) * 3.75) %>%         #3.75 from https://www.palliativecareguidelines.scot.nhs.uk/guidelines/pain/choosing-and-changing-opioids.aspx 12micro patch ~ 45mg O Morphine
            add_row(drug = "buprenorphine_patch", ovsb = "Opiate", dose = as.numeric(input$buprenorphine_patch) * 2.4) #https://www.palliativecareguidelines.scot.nhs.uk/guidelines/medicine-information-sheets/buprenorphine-patches.aspx
        
        
        total_opiate <- drugs %>%
            filter(ovsb == "Opiate") %>%
            tally(dose)
        
        total_benzo <- drugs %>%
            filter(ovsb == "Benzo") %>%
            tally(dose)
        
        drugs <- drugs %>%
            add_row(drug = "Total_Opiate", ovsb = "Total_Opiate", dose = total_opiate) %>%
            add_row(drug = "Total_Benzo", ovsb = "Total_Benzo", dose = total_benzo) %>%
            mutate(drug = fct_relevel(drug,
                                      "codeine_oral",
                                      "tramadol_oral",
                                      "morphine_oral",
                                      "morphine_subcut",
                                      "diamorphine_subcut",
                                      "oxycodone_oral",
                                      "oxycodone_subcut",
                                      "alfentanil_subcut",
                                      "hydromorphone_oral",
                                      "hydromorphone_subcut",
                                      "fentanyl_patch",
                                      "buprenorphine_patch",
                                      "Total_Opiate",
                                      "Total_Benzo",
                                      "diazepam_oral",
                                      "clobazam_oral",
                                      "clonazepam_oral",
                                      "lorazepam_oral",
                                      "midazolam_subcut",
                                      "nitrazepam_oral",
                                      "temazepam_oral")) %>%
            mutate(drug = recode(drug,
                                      "codeine_oral" = "Codeine (Oral)",
                                      "tramadol_oral" = "Tramadol (Oral)",
                                      "morphine_oral" = "Morphine (Oral)",
                                      "morphine_subcut" = "Morphine (Subcut)",
                                      "diamorphine_subcut" = "Diamorphine (Subcut)",
                                      "oxycodone_oral" = "Oxycodone (Oral)",
                                      "oxycodone_subcut" = "Oxycodone (Subcut)",
                                      "alfentanil_subcut" = "Alfentanil (Subcut)",
                                      "hydromorphone_oral" = "Hydromorphone (Oral)",
                                      "hydromorphone_subcut" = "Hydromorphone (Subcut)",
                                      "fentanyl_patch" = "Fentanyl (Patch)",
                                      "buprenorphine_patch" = "Buprenorphine (Patch)",
                                      "Total_Opiate" = "Opiate Total Dose",
                                      "Total_Benzo" = "Benzo Total Dose",
                                      "diazepam_oral" = "Diazepam (Oral)",
                                      "clobazam_oral" = "Clobazam (Oral)",
                                      "clonazepam_oral" = "Clonazepam (Oral)",
                                      "lorazepam_oral" = "Lorazpema (Oral)",
                                      "midazolam_subcut" = "Midazolam (Subcut)",
                                      "nitrazepam_oral" = "Nitrazepam (Oral)",
                                      "temazepam_oral" = "Temazepam (Oral)")) %>%
            filter(!is.na(dose),
               dose !=0)
        
        graph_fills <- c("Opiate" = "#fee6ce", "Total_Opiate" = "#e6550d", "Benzo" = "#deebf7", "Total_Benzo" = "#3182bd")

        drugs %>%
            ggplot() +
            aes(drug,dose, fill = ovsb) +
            geom_bar(stat = "identity") +
            geom_label(aes(label = dose), position = position_stack(vjust = 0.5)) +
            scale_fill_manual(values = graph_fills) + 
            labs(y = "Equivalance of mgs of Oral Diazepam or Morphine / 24 hours",
                 x = "Drug") +
<<<<<<< HEAD
            theme_minimal() +
            theme(legend.position = "None")
=======
            theme_minimal()
>>>>>>> 1d05489aafcdc97bc7876b73ea4783d4fa6c8a0e
    })

})
