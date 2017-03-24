library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Welcome to the breast cancer early detection and treatment impact model"),
  
  navlistPanel(
    tabPanel("Introduction",
             h4('Overview'),
             p('This interface allows you to model the survival benefit of a 
                screening and/or treatment intervention in a virtual population of your choosing. 
                The model posits a simple stage-shift mechanism of screening benefit. 
                Specify inputs using the navigation sidebar.'),
             br(),
             h4('Default Model: Breast Cancer in East Africa'),
             p('The input defaults reflect an breast cancer example in East Africa:'),
             h5('Standard of Care: "Control" Scenario'),
             p('Because adjuvant treatment is logistically and financially difficult
                in East Africa, women are often not treated after surgery. Accordingly, in
                the Control scenario, there is no adjuvant treatment.'),
             h5('Standard of Care: "Intervention" Scenario'),
             p('The intervention modeled is ER screening, which leads to endocrine therapy being
               administered only to the 50% of women who are ER+. All of these women
               receive a survival benefit from the endocrine therapy.')
    ),
    "Natural History",
    tabPanel("Cancer Incidence",
             h4('Select incidence and all-cause mortality databases and an age range, 
                and specify the percent of cancers that are ER positive.'),
             p('Annual incidence in East Africa is patterned after Uganda, 
                where there are about 60 cases per year among 100,000 women ages 30-49. 
                The literature suggests that about 41% of East African breast 
                cancers are ER positive.'),
             br(),
             # Incidence
             uiOutput('chooseInc'),
             em('Source: CI5-X Incidence Database'),
             a('http://ci5.iarc.fr/CI5-X/'),
             br(), br(),
             uiOutput('chooseMort'),
             em('Source: IHME 2013 Lifetable Estimates'), 
             a('http://ghdx.healthdata.org/global-burden-disease-study-2013-gbd-2013-data-downloads'),
             br(), br(),
             sliderInput('agerange', label='Age range of the population',
                          value=c(30, 49), min=0, max=100, step=1, 
                          width = NULL),
             br(),
             # Percent ER+
             sliderInput("prop_ERpos", label = "Percent ER positive",
                         min=0, max=100, step=1, value=41)
             ),
    tabPanel("Stage-Specific Survival",
              h4('Select a year, k, by which you will specify the percent of cases 
                surviving at k years after diagnosis, or "baseline survival"'),
              p('Baseline survival should typically be survival in the absence of 
                systemic treatment, but it can be treated survival if the 
                intervention does not impact treatment. You must specify k-year 
                survival for advanced- and early-stage cases separately.'),
              p('Data from Uganda suggest that advanced-stage cases may 
                have a 5-year survival rate of 35%. Given the low incidence of 
                early-stage cancer in East Africa, survival data 
                are sparse for early stage cases. We approximate early-stage survival 
                using historical data from the US in the 1940s, which puts 5-year survival 
                of localized cases at 80%.'),
             br(),
             selectInput('year.surv', label='Year of survival statistic, k', 
                         choices=c(5,10), selected=5),
             
             sliderInput('surv.adv', label='Advanced cases: baseline survival at k years', 
                         35, min = 0, max = 100, step = 1),
             
             sliderInput('surv.early', label='Early cases: baseline survival at k years', 
                         80, min = 0, max = 100, step = 1)
             ),
    "Impact of Early Detection",
    tabPanel("Stage Distributions",
              h4('Enter the percent of advanced-stage cases in the control and intervention scenarios.'),
              p('In the control scenario, this is the typical percent of cases who are 
                advanced-stage at the time of clinical diagnosis. In the 
                intervention scenario, the percent presenting in advanced stage may
                decrease due to early detection efforts.'),
              p('The default intervention involves no formal early detection strategy,
                only changes to the treatments available (see next two panels).'),
             br(),
             # Percent advanced
              h5('CONTROL SCENARIO'),
             sliderInput("prop_a0", label = "Percent advanced",
                         min=0, max=100, step=1, value=78),
             # Percent advanced
              h5('INTERVENTION SCENARIO'),
             sliderInput("prop_a1", label = "Percent advanced",
                         min=0, max=100, step=1, value=78),
             h5('SUMMARY OF SELECTED VALUES'),
             tableOutput('edsummary')
             ),
    "Treatments Available",
    tabPanel("Control Scenario",
              h4('Specify who is eligible for each treatment, 
                and what percent of eligible cases receive it.'),
              p('Adjuvant treatment is rare in East Africa, so 
                we model no treatment for the Control Scenario.'),
              br(),
              h5('ENDOCRINE THERAPY'),
              radioButtons("tam.elig.control", "Who is eligible for endocrine therapy?",
                           c("All" = 'All',
                             "ER+ only" = 'ERpos')),
              sliderInput('tam.prop.control', label='What percent of eligible women receive endocrine therapy?', 
                          0, min = 0, max = 100, step = 1),
              br(),
              h5('CHEMOTHERAPY'),
              radioButtons("chemo.elig.control", "Who is eligible for chemotherapy?",
                          c("All" = 'All',
                            "ER- only" = 'ERneg',
                            "ER- and advanced-stage ER+" = 'ERnegERposAdv'
                            )),
              sliderInput('chemo.prop.control', label='What percent of eligible women receive chemotherapy?', 
                          0, min = 0, max = 100, step = 1)
              ),
    tabPanel("Intervention Scenario",
             h4('Specify who is eligible for each treatment, 
                and what percent of eligible cases receive it.'),
             p('In the default intervention, all ER+ women receive endocrine therapy.'),
             br(),
             h5('ENDOCRINE THERAPY'),
             radioButtons("tam.elig.interv", "Who is eligible for endocrine therapy?",
                          c("All" = 'All',
                            "ER+ only" = 'ERpos'),
                          selected='ERpos'),
             sliderInput('tam.prop.interv', label='What percent of eligible women receive endocrine therapy?', 
                         100, min = 0, max = 100, step = 1),
             br(),
             h5('CHEMOTHERAPY'),
             radioButtons("chemo.elig.interv", "Who is eligible for chemotherapy?",
                          c("All" = 'All',
                            "ER- only" = 'ERneg',
                            "ER- and advanced-stage ER+" = 'ERnegERposAdv'
                          ),
                          selected='All'),
             sliderInput('chemo.prop.interv', label='What percent of eligible women receive chemotherapy?', 
                         0, min = 0, max = 100, step = 1)
    ),
    "Parameter Summary",
    tabPanel("User-Defined Parameters",
              h4('Review the selected parameter values'),
             p('The parameters specified on the previous pages are summarized below.
               To make changes, revisit the previous pages.'),
             br(),
             h5('INCIDENCE FROM:'),
             textOutput('inccountry'),
             h5('ALL-CAUSE MORTALITY FROM:'),
             textOutput('mortcountry'),
             br(),
             h5('TUMOR CHARACTERISTICS'),
             tableOutput('paramsum1'),
             br(),
             h5('ADVANCED-STAGE TREATMENTS'),
             em('Values represent percents falling into each group. Columns
                should sum to 100% within each ER type'),
             tableOutput('paramsum2'),
             br(),
             h5('EARLY-STAGE TREATMENTS'),
             em('Values represent percents falling into each group. Columns
                should sum to 100% within each ER type'),
             tableOutput('paramsum3')
             ),
    tabPanel("Fixed Parameters",
             h4('Virtual population size and simulations'),
             p('The virtual population contains 100,000 women. Results reported are the average over 
               10 simulations.'),
             h4('The benefits of treatments are sourced from the literature'),
             p('Meta-analyses from the Early Breast Cancer Trialists Collaborative Group
               have summarized the benefits of endocrine therapy and chemotherapy across
               regimens. The benefits are quantified as "hazard ratios" on survival, which
               indicate the percent improvement in survival due to treatment'),
             br(),
             h5('SURVIVAL BENEFIT OF TREATMENT'),
             em('All treatments not specified in the table have a hazard ratio of 1,
                i.e. there is no impact on survival. So for example, endocrine therapy
                for ER- tumors has a hazard ratio of 1 and thus is not in the table.'),
             tableOutput('hazards'),
             p('References:'),
             em('1.
                Early Breast Cancer Trialists’ Collaborative Group (EBCTCG), Davies C, Godwin J, Gray R, Clarke M, Cutter D, et al. Relevance of breast cancer hormone receptors and other factors to the efficacy of adjuvant tamoxifen: patient-level meta-analysis of randomised trials. Lancet. 2011 Aug 27;378(9793):771–84. 
                '),
                br(),
             em('2.
                Early Breast Cancer Trialists’ Collaborative Group (EBCTCG), Peto R, Davies C, Godwin J, Gray R, Pan HC, et al. Comparisons between different polychemotherapy regimens for early breast cancer: meta-analyses of long-term outcome among 100,000 women in 123 randomised trials. Lancet. 2012 Feb 4;379(9814):432–44. 
                ')
             ),
    "Results",
    tabPanel("Tables",
             # This accesses the stylesheet, which just sets a 
             # location for the progress bar. Thanks to:
             # https://groups.google.com/forum/#!topic/shiny-discuss/VzGkfPqLWkY 
             # and https://github.com/johndharrison/Seed
             tags$head(
                tags$link(rel='stylesheet', type='text/css', href='styles.css'),
                tags$script(type="text/javascript", src="busy.js")
             ),
             h5('Results will appear when simulations are complete. 
                Expected wait time is approximately 2 minutes.'),
             em('Results are reported as statistics per 100,000 women'),

                    div(class = "busy",
                             p("Calculation in progress..."),
                                  img(src="ajax-loader.gif")
                                 ),

#            verbatimTextOutput('debug'),
#            tableOutput('debug'),
             h4('Results after 5 years'),
             textOutput('caption5'),
             tableOutput('resultsTable5'),
             br(),
             h4('Results after 10 years'),
             textOutput('caption10'),
             tableOutput('resultsTable10'),
             br(),
             h4('Results after 20 years'),
             textOutput('caption20'),
             tableOutput('resultsTable20')
             ),
    tabPanel("Plots",
             h4('Among incident cases, percent surviving'),
             p('x-axis shows the year of follow-up. Orange indicates gains from the intervention.'),
             plotOutput('resultsGraph')
             ),
    tabPanel("Uncertainty",
             h5('Empirical 95% uncertainty intervals'),
             h4('Results after 5 years'),
             tableOutput('uncertaintyTable5'),
             br(),
             h4('Results after 10 years'),
             tableOutput('uncertaintyTable10'),
             br(),
             h4('Results after 20 years'),
             tableOutput('uncertaintyTable20')
             )
  
  ) # end navlistPanel
)) # end fluidPage and shinyUI


