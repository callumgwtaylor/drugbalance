#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
<<<<<<< HEAD
    titlePanel("Comparing Drug Doses In Palliative Care - Opiates and Benzodiazepines"),
=======
    titlePanel("Comparing Opiates and Benzo Doses"),
>>>>>>> 1d05489aafcdc97bc7876b73ea4783d4fa6c8a0e
    
    head("This tool wants to help you do two things:"),
    br(),
    br(),
<<<<<<< HEAD
    head("First, to visualise the difference between different doses of opiates and benzos. How big a jump is 10mg of Codeine to 10mg of Alfentanil? What do the doses on the patches really mean?"),
    br(),
    br(),
    head("Second, to compare the ratio between opiate and benzodiazepine use. What's the balance of opiates and benzos in that pump?"),
    br(),
    br(),
    head("All comparison data is sourced from the Online Scottish Palliative Care Guidelines. All comparisons will be inexact. Please DO NOT use this tool to make clinical decisions, it's just showing data from Scottish Palliative Care Guidelines in a different format, it's not a decision aid. This app is not trying to equate 1mg of morphine and 1mg of diazepam in terms of clinical effect, it just makes for a simpler graph!"),
    br(),
    head("All code is available on GitHub. This tool does not store any patient data."),
    br(),
    a(href="https://www.palliativecareguidelines.scot.nhs.uk/", "link to Scottish Palliative Care Guidelines - this project is in no way linked to them, and doesn't make any copyright claims etc"),
    br(),
    a(href= "https://github.com/callumgwtaylor/drugbalance", "link to source code for this project"),
    br(),
    a(href = "https://twitter.com/CallumGWT", "message me on twitter if you notice any mistakes!"),
    br(),
    br(),
    head("To make comparisons between drugs, drag the individual drug's slider to it's dose (or click the option if a patch), and the graph will automatically form. A 'Total Opiate' and 'Total Benzo' column will appear when these drugs are being used, to show you the cumulative dose of multiple different agents. To reset it, just reload the webpage"),
=======
    head("First, to visualise the difference between different doses of opiates and benzodiazepines. How big a jump is 10mg of Codeine to 10mg of Alfentanil? What do the doses on the patches really mean?"),
    br(),
    br(),
    head("Second, to see in another way, the ratio between opiate and benzodiazepine use for your patients in palliative care. What's the balance of opiates and benzos in that pump?"),
    br(),
    br(),
    head("All comparison data is sourced from the Online Scottish Palliative Care Guidelines"),
    br(),
    head("All code is available on GitHub. This tool does not store any patient data."),
>>>>>>> 1d05489aafcdc97bc7876b73ea4783d4fa6c8a0e
         
         
# https://www.palliativecareguidelines.scot.nhs.uk/guidelines/pain/choosing-and-changing-opioids.aspx"),
# https://www.palliativecareguidelines.scot.nhs.uk/guidelines/medicine-information-sheets/midazolam-in-palliative-care.aspx,    
 
   hr(),
    
    plotOutput("barchart"),
    
    hr(),
    
    fluidRow(
        column(3,
               h3("Opiates - mg over 24 hours"),
               sliderInput("codeine_oral",
                           "Oral Codeine:",
                           min = 0,
                           max = 500,
                           value = 0),
               sliderInput("tramadol_oral",
                           "Oral Tramadol:",
                           min = 0,
                           max = 500,
                           value = 0),
               sliderInput("morphine_oral",
                           "Oral Morphine:",
                           min = 0,
                           max = 500,
                           value = 0),
               sliderInput("morphine_subcut",
                           "Subcut Morphine:",
                           min = 0,
                           max = 500,
                           value = 0),
               sliderInput("diamorphine_subcut",
                           "Subcut Diamorphine:",
                           min = 0,
                           max = 500,
                           value = 00),
               sliderInput("oxycodone_subcut",
                           "Subcut Oxycodone:",
                           min = 0,
                           max = 500,
                           value = 00),
               sliderInput("oxycodone_oral",
                           "Oral Oxycodone:",
                           min = 0,
                           max = 500,
                           value = 00),
               sliderInput("alfentanil_subcut",
                           "Subcut Alfentanil:",
                           min = 0,
                           max = 500,
                           value = 00),
               sliderInput("hydromorphone_oral",
                           "Oral Hydromorphone:",
                           min = 0,
                           max = 500,
                           value = 00),
               sliderInput("hydromorphone_subcut",
                           "Subcut Hydromorphone:",
                           min = 0,
                           max = 500,
                           value = 00)
               ),
        column(3, offset = 1,
               radioButtons("fentanyl_patch",
                            label = h3("Fentanyl Patch"),
                            choices = list("No Patch" = 0,
                                           "12 micrograms/hr" = 12,
                                           "25 micrograms/hr" = 25,
                                           "37.5 micrograms/hr" = 37.5,
                                           "50 micrograms/hr" = 50,
                                           "75 micrograms/hr" = 75,
                                           "100 micrograms/hr" = 100), 
                            selected = 0),
               radioButtons("buprenorphine_patch", label = h3("Buprenorphine Patch"),
                            choices = list("No Patch" = 0,
                                           "5 micrograms/hr" = 5,
                                           "10 micrograms/hr" = 10,
                                           "15 micrograms/hr" = 15,
                                           "20 micrograms/hr" = 20,
                                           "35 micrograms/hr" = 35,
                                           "52.5 micrograms/hr" = 52.5,
                                           "70 micrograms/hr" = 70,
                                           "140 micrograms/hr" = 140), 
                            selected = 0)
               ),
        column(3, offset = 1,
<<<<<<< HEAD
               h3("Benzodiazepines - mg over 24 hours (unless stated as micrograms)"),
=======
               h3("Benzos - mg over 24 hours (unless stated as micrograms)"),
>>>>>>> 1d05489aafcdc97bc7876b73ea4783d4fa6c8a0e
                    sliderInput("diazepam_oral",
                                "Oral Diazepam:",
                                min = 0,
                                max = 500,
                                value = 0),
                    sliderInput("clobazam_oral",
                                "Oral Clobazam:",
                                min = 0,
                                max = 500,
                                value = 0),
                    sliderInput("clonazepam_oral",
                                "Oral Clonazepam (MICROGRAMS):",
                                min = 0,
                                max = 5000,
                                value = 0),
                    sliderInput("lorazepam_oral",
                                "Oral Lorazepam (MICROGRAMS):",
                                min = 0,
                                max = 5000,
                                value = 0),
                    sliderInput("midazolam_subcut",
                                "Subcut Midazolam:",
                                min = 0,
                                max = 500,
                                value = 0),
                    sliderInput("nitrazepam_oral",
                                "Oral Nitrazepam:",
                                min = 0,
                                max = 500,
                                value = 0),
                    sliderInput("temazepam_oral",
                                "Oral Temazepam:",
                                min = 0,
                                max = 500,
                                value = 0))
        )
))