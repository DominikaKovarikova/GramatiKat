############################################
#                                          #
#       GramatiKat - Dominika Kovarikova   #
#                                          #
#                  GLOBAL                  #
#                                          #
############################################
library(shinyCNC)
library(shiny.i18n)
library(shinyjs)
library(shinyBS)

#i18n <- Translator$new(translation_json_path = "translation.json")
lang <- "cs"
#i18n$set_translation_language(lang)

defaultLemma <- "kočka"
defaultLemma2 <- "skromný"
defaultLemma3 <- "zkrotit"


singulars=c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7')
plurals=c('P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7')

singulars3=c('S1', 'S2', 'S3')
plurals3=c('P1', 'P2', 'P3')

gender=c('M', 'I', 'F', 'N')

degree=c('1', '2', '3')

negation=c('A', 'N')

printDebug <-TRUE

nazevProjektu <- "GramatiKat"

noseltext <- "Vyber řádek pro další možnosti"
noseltext2 <- ""
noseltext3 <- ""

jsku <- '
$(document).on("keyup", function(e) {
  if(e.keyCode == 13){
    $("#changeLemma").click();
    $("#changeLemma2").click();
    $("#changeLemma3").click();
  }
});
'

# only try to run this code if the shiny.i18n package is installed
#if (require(shiny.i18n, quietly = TRUE)) {


############################################
########           UI               ########
############################################

localized_UI <- function(i18n, lang) {
  # Standard case description
  fnCaseDescription <- function() {
    htmltools::withTags(table(
      class = 'display',
      #class = 'style="width:75%"',
      thead(
        tr(
          th("S1"),
          th("S2"),
          th("S3"),
          th("S4"),
          th("S5"),
          th("S6"),
          th("S7"),
          th("P1"),
          th("P2"),
          th("P3"),
          th("P4"),
          th("P5"),
          th("P6"),
          th("P7")
        ),
        tr(
          td("nominative singular"),
          td("genitive singular"),
          td("dative singular"),
          td("accusative singular"),
          td("vocative singular"),
          td("locative singular"),
          td("instrumental singular"),

          td("nominative plural"),
          td("genitive plural"),
          td("dative plural"),
          td("accusative plural"),
          td("vocative plural"),
          td("locative plural"),
          td("instrumental plural")
        )
      )
    )
    )
  }
  
  fluidPage(
    useShinyjs(),
    tags$head(tags$script(src = "message-handler.js"),
              tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed; top: 350px; left: 0px; width: 100%; padding: 5px 0px 5px 0px;
               text-align: center; font-weight: bold; font-size: 400%;
               color: #000000; background-color: #ea670c;
               z-index: 105;
             }

             #loadmessagebg {
               position: fixed; top: 0px; left: 0px; padding: 5px 0px 5px 0px;
               height: 100%; width: 100%;
               color: #000000; background-color: #999999;
               z-index: 104;
               opacity: 0.3;
             }

             
             #aboutPanels {
               position: fixed; 0px; 0px 0px 0px 0px; padding: px 0px px 0px;
               z-index: 100;
             }
             
             #more_info_div {
               z-index: 104;
             }
             
             #goButton1:passive {
              background-color: #ffffff;
             }

             #goButton1:hover {
              background-color: #ffffff;
             }
             
             #goButton1:active {
              background-color: #337ab7;
             }
             
             #goButton1 {
              background-color: #ffffff;
              color: #337ab7;
              border-style: none;
             }

             #goButton2:passive {
              background-color: #ffffff;
             }

             #goButton2:hover {
              background-color: #ffffff;
             }
             
             #goButton2:active {
              background-color: #337ab7;
             }
             
             #goButton2 {
              background-color: #ffffff;
              color: #337ab7;
              border-style: none;
             }

             #goButton3:passive {
              background-color: #ffffff;
             }

             #goButton3:hover {
              background-color: #ffffff;
             }
             
             #goButton3:active {
              background-color: #337ab7;
             }
             
             #goButton3 {
              background-color: #ffffff;
              color: #337ab7;
              border-style: none;
             }

             #goButtonAbout {
              background-color: #337ab7;
              color: #ffffff;
              border-style: none;
             }

                                             
          "))
    ),

    titlePanel(windowTitle="GramatiKat", title=""),

    #tags$style(HTML("#big-heading{color: #e2007a; display:inline;}")),
    tags$style("#textTitle {font-size:36px; color:#e2007a; display:inline; margin-left: 0px; margin-top: 2px; margin-right: 10px;}"),
    tags$style("#textKorpus {font-size:14px; color:Black; display:inline; margin-left: 0px; margin-top: 8px; margin-right: 10px;}"),

    div(id="newnew", style="display: flex; justify-content: space-between; border: 0px solid black;",
        #h1(id="big-heading", " ČísloPád2"),
        #div(id="textTitle", class="shiny-text-output shiny-bound-output", i18n$t("GramatiKatx")),
        div(id="iconTitle",
            img(src="gramatikat_logo.png", style="padding-top: 1px; padding-bottom: 1px; width: 200px;"),
            br()
        ),
        
        div(id="buttonsTitle",
            div(id="prepinani_tlacitek", verbatimTextOutput("goButton1Text"), verbatimTextOutput("goButton2Text"), verbatimTextOutput("goButton3Text"), verbatimTextOutput("goButtonAboutText")),
            bsButton("goButton1", label=i18n$t("substantivaTab"), block = FALSE, style="default"),
            bsButton("goButton2", label=i18n$t("adjektivaTab"), block = FALSE, style="default"),
            bsButton("goButton3", label=i18n$t("verbaTab"), block = FALSE, style="default"),
            bsButton("goButtonAbout", label=i18n$t("aboutTab"), block = FALSE, style="default")
        ),
        
        div(id="selectCorpora", style="display: none; margin-bottom: 0px; margin-top: 0px;",
            div(id="textKorpus", class="shiny-text-output shiny-bound-output", "Korpus:"),
            selectizeInput(inputId = "inSelectCorpus", label = NULL, choices = NULL, selected = 1),
            actionButton("changeCorpus", i18n$t("Načíst"), style = "height: 35px; margin-left: 10px;")
        ),
        
        div(id="selectCorporaAdj", style="display: none; margin-bottom: 0px; margin-top: 0px;",
            div(id="textKorpusAdj", class="shiny-text-output shiny-bound-output", "Korpus Adj:"),
            selectizeInput(inputId = "inSelectCorpusAdj", label = NULL, choices = NULL, selected = 1),
            actionButton("changeCorpusAdj", i18n$t("Načíst"), style = "height: 35px; margin-left: 10px;")
        ),
        
        div(id="selectCorporaVerb", style="display: none; margin-bottom: 0px; margin-top: 0px;",
            div(id="textKorpusVerb", class="shiny-text-output shiny-bound-output", "Korpus Verb:"),
            selectizeInput(inputId = "inSelectCorpusVerb", label = NULL, choices = NULL, selected = 1),
            actionButton("changeCorpusVerb", i18n$t("Načíst"), style = "height: 35px; margin-left: 10px;")
        ),

        div(id="selectCorporaNone", style="display: middle; margin-bottom: 0px; margin-top: 0px;",
            div(id="textKorpus", class="shiny-text-output shiny-bound-output", "Korpus:"),
            selectizeInput(inputId = "inSelectCorpusNone", label = NULL, choices = NULL, selected = 1),
            actionButton("changeCorpusNone", i18n$t("Načíst"), style = "height: 35px; margin-left: 10px;")
        ),
        
    ),

    tags$style(HTML(".tabbable > .nav > li > a[data-value='Substantiva'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Nouns'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Adjektiva'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Adjectives'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Verba'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='Verbs'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='O aplikaci'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),
    tags$style(HTML(".tabbable > .nav > li > a[data-value='About'] {background-color: transparent;  color:black; height: 0PX; width: 0PX;}")),

#    tags$style(HTML(".tabbable > .nav > li > a:hover[data-value='Substantiva'] {background-color: red;  color:black; height: 0PX; width: 0PX;}")),
#    tags$style(HTML(".tabbable > .nav > li > a:hover[data-value='Nouns'] {background-color: red;  color:black; height: 0PX; width: 0PX;}")),
#    tags$style(HTML(".tabbable > .nav > li > a:hover[data-value='O aplikaci'] {background-color: red;  color:black; height: 0PX; width: 0PX;}")),
#    tags$style(HTML(".tabbable > .nav > li > a:hover[data-value='About'] {background-color: red;  color:black; height: 0PX; width: 0PX;}")),

    tabsetPanel(id = "aboutPanels", type="pills", selected=i18n$t("aboutTab"),
          tabPanel(title ="", value = i18n$t("substantivaTab"),

                   div(id="divintro", style="width: 1000px; display: flex; justify-content: space-between; border: 0px solid black;",
                       tags$b(i18n$t("GramatiKat_head"))),
                   
                   #p(),
                   #br(),
                   #div(id="more_info_div",
                   #   div(
                   #       actionButton("more_info", i18n$t("more_info"))
                   #     ),
                   #     div(
                   #       textOutput("more_info"),
                   #       style = "align: left; width: 1000px;"
                   #      )
                   
                   #),
                   
                   tags$div( p(""), br() ),
                   #tags$div( p("") ),
                   
        
    
    wellPanel(style= "display: inline-block; padding: 0px 0px 0px 0px;",
              
              tags$div(style= "display: block; padding: 0px 0px 0px 0px;",
                       tags$style("#x1_current_week {font-size:20px; color:red; display:block; }"),
                       div(style="padding: 0px 15px 5px 15px; display: flex; justify-content: start",
                           tags$style("#textLemma {font-size:20px; color:Black; display:inline-block; margin-left: 0px; margin-top: 22px; margin-right: 10px;}"),
                           div(id="textLemma", class="shiny-text-output shiny-bound-output", "Lemma:"),

                           textInput(inputId="searchLemma", label="", value=defaultLemma, placeholder = "Search term"),
                           actionButton("changeLemma", i18n$t("Načíst"), style = "height: 34px; margin-left: 10px; margin-top: 20px;"),
                           tags$style("#textErrorLemma {font-size:20px; color: #ea670c; display:block; margin-left: 10px; margin-top: 20px;}"),
                           textOutput("textErrorLemma")
                       ),

                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$div(i18n$t("please_wait"),id="loadmessage") # busy message
                       ),
                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                           tags$div(id="loadmessagebg") # busy message background to avoid clicking
                       )
              )

    ),

    tags$p(
      tags$table(
        tags$tr(
          tags$td(tags$h3("Grafy pro lemma \"")),
          tags$td(tags$h3(uiOutput("lemmaref"))),
          tags$td(tags$h3("\":"))
        )
      )
    ),

    mainPanel(id="mainPanel", width = "100%",
              tabsetPanel(id = "topPanels", type="pills", selected=i18n$t("Číslopád"),

                          # -----------------------------------------------------------------------------------
                          tabPanel(i18n$t("Číslo"),
                                   tags$h1(),

                                   div(tags$b(i18n$t("cislo_head"))),
                                   br(),
                                   div(i18n$t("cislo_body"), style = "width: 1000px;"),

                                   div(tags$p("")),
                                   div(i18n$t("boxplot_description"), style = "width: 1000px;"),
                                   div(tags$p("")),
                                   tags$p(
                                     tags$table(
                                       tags$tr(
                                         tags$td(plotOutput("plotBoxplotCisla"), width="400px"),
                                         tags$td(plotOutput("plotBoxplotCislaLegend"), width="400px")
                                       )
                                     )
                                   ),

                                   div(tags$p("")),
                                   div(i18n$t("cislo_histogram"), style = "width: 1000px;"),
                                   div(tags$p("")),

                                   tags$p(
                                     tags$table(
                                       tags$tr(
                                         tags$td(plotOutput("plotHistogramCislaS"), width="400px"),
                                         tags$td(plotOutput("plotHistogramCislaP"), width="400px")
                                       )
                                     )
                                   ),

                                   tags$hr(),


                                   tags$h3(i18n$t("cislo_tabulky_header")),
                                   tags$h4(radioButtons(inputId="rbCislo", "",
                                                        choices = c('singulars', 'plurals'),
                                                        selected = "singulars",
                                                        inline=T)),
                                   tags$p(""),
                                   div(i18n$t("cislo_tabulky_head")),
                                   tags$p(""),
                                   div(tags$b(i18n$t("cislo_tabulky_top")), style = "font-size:15px;"),
                                   tags$p(""),
                                   DT::dataTableOutput('tblCisloTop'), hr(), div(id="cisloTopsel", noseltext2),
                                   tags$p(""),
                                   div(tags$b(i18n$t("cislo_tabulky_bottom")), style = "font-size:15px;"),
                                   tags$p(""),
                                   DT::dataTableOutput('tblCisloBottom'), hr(), div(id="cisloBottomsel", noseltext2),

                                   h1(),
                                   br(), br(), br(), br(), br(), br(), br()

                          ),

                          # -----------------------------------------------------------------------------------
                          tabPanel(i18n$t("Číslopád"),
                                   tags$h1(),
                                   div(tags$b(i18n$t("cislopad_head"))),
                                   br(),
                                   div(i18n$t("cislopad_body"), style = "width: 1000px;"),
                                   div(tags$p("")),
                                   
                                   if (lang != 'cs') {
                                      div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                   },
                                   tabsetPanel(id = "chPanels2", type="pills",
                                               tabPanel(i18n$t("Boxplots"),
                                                        tags$p(),

                                                        div(tags$h1("")),
                                                        div(i18n$t("boxplot_description"), style = "width: 1000px;"),
                                                        div(tags$p("")),

                                                        tags$p(
                                                          tags$table(
                                                            tags$tr(
                                                              tags$td(plotOutput("plotBoxplotCisloPad"), width="800px"),
                                                              tags$td(plotOutput("plotBoxplotCislopadLegend"), width="400px")
                                                            )
                                                          )
                                                        ),


                                                        tags$hr(),

                                                        tags$b(i18n$t("cislo_tabulky_header")),
                                                        tags$b(radioButtons(inputId="rbCisloPad", "",
                                                                             choices = append(singulars, plurals),
                                                                             selected = "S1",
                                                                             inline=T)),
                                                        tags$p(""),
                                                        div(tags$b(i18n$t("cislo_tabulky_top")), style = "font-size:15px;"),
                                                        div(i18n$t("cislo_tabulky_head")),
                                                        tags$p(""),
                                                        tags$p(""),
                                                        DT::dataTableOutput('tblcislopadTop'), #hr(), div(id="cisloTopsel", noseltext2),
                                                        tags$p(""),
                                                        div(tags$b(i18n$t("cislo_tabulky_bottom")), style = "font-size:15px;"),
                                                        tags$p(""),
                                                        DT::dataTableOutput('tblcislopadBottom'), #hr(), div(id="cisloBottomsel", noseltext2)#,
                                                        h1(),
                                                        br(), br(), br(), br(), br(), br(), br()

                                               ),
                                               tabPanel(i18n$t("Histograms"),
                                                        div(
                                                          i18n$t("histograms_body"),
                                                          style = "width: 1000px;"
                                                        ),
                                                        tags$p(),
                                                        radioButtons(inputId="rbCisloPadHist", "",
                                                                     choices = append(singulars, plurals),
                                                                     selected = "S1",
                                                                     inline=T),
                                                        plotOutput("plotHistogramCisloPad", width="600px")
                                               )
                                   )
                          ),

                          # -----------------------------------------------------------------------------------
                          tabPanel(i18n$t("Rodočíslopád"),
                                   tags$h1(),
                                   div(tags$b(i18n$t("rodocislopad_head"))),
                                   br(),
                                   div(i18n$t("rodocislopad_body"), style = "width: 1000px;"),
                                   div(tags$p("")),
                                   if (lang != 'cs') {
                                     div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                   },
                                   tabsetPanel(id = "chPanels2", type="pills",
                                               tabPanel(i18n$t("rodocislopad_chart1"),
                                                        div(tags$h1("")),
                                                        div(i18n$t("boxplot_description"), style = "width: 1000px;"),
                                                        div(tags$p("")),
                                                        plotOutput("plotBoxplotRodocislopad1Lemma", height = "600")
                                               ),
                                               tabPanel(i18n$t("rodocislopad_chart2"),
                                                        tags$p(),
                                                        column(12, plotOutput("plotBoxplotRodocislopad2", height=600)),
                                                        tags$table(style = "width: 80%")
                                               )
                                   )
                          ),                          
                          
                          # -----------------------------------------------------------------------------------
                          tabPanel(i18n$t("Tabulky"),
                                   tags$h1(),
                                   div(i18n$t("tables_body1"), br(),
                                       i18n$t("tables_body2"), br(),
                                       i18n$t("tables_body3"), br(),
                                       i18n$t("tables_body4"), br(),
                                       i18n$t("tables_body5"), br(),
                                       i18n$t("tables_body6"),
                                       style = "width: 1000px;"
                                   ),
                                   if (lang != 'cs') {
                                     div(tags$p(""), fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                   },
                                   
                                   tags$h1(),
                                   div(id="data1sel", noseltext),
                                   hr(),
                                   DT::dataTableOutput('data1')
                          )
                          



              ) # /tabsetPanel topPanels
    ) # /mainPanel
    
      
    ), # /substantivaTab
    

    
    
    
    
    
    
    
    
    
 ################################################       ADJEKTIVA      ##############################################################    
    
    tabPanel(title ="", value = i18n$t("adjektivaTab"),
             
             #div(id="divintroxzc", style="width: 1000px; display: flex; justify-content: space-between; border: 10px solid black;"),
                                   
             #tags$h1("sadasd sd sd sa dsa dsad sad sad a"),

             div(id="divintro", style="width: 1000px; display: flex; justify-content: space-between; border: 0px solid black;",
                 tags$b(i18n$t("GramatiKat_head"))),
             
             tags$div( p(""), br() ),
             
             wellPanel(style= "display: inline-block; padding: 0px 0px 0px 0px;",
                       tags$div(style= "display: block; padding: 0px 0px 0px 0px;",
                                tags$style("#x1_current_week {font-size:20px; color:red; display:block; }"),
                                div(style="padding: 0px 15px 5px 15px; display: flex; justify-content: start",
                                    tags$style("#textLemma2 {font-size:20px; color:Black; display:inline-block; margin-left: 0px; margin-top: 22px; margin-right: 10px;}"),
                                    div(id="textLemma2", class="shiny-text-output shiny-bound-output", "Lemma:"),
                                    
                                    textInput(inputId="searchLemma2", label="", value=defaultLemma2, placeholder = "Search term"),
                                    actionButton("changeLemma2", i18n$t("Načíst"), style = "height: 34px; margin-left: 10px; margin-top: 20px;"),
                                    tags$style("#textErrorLemma2 {font-size:20px; color: #ea670c; display:block; margin-left: 10px; margin-top: 20px;}"),
                                    textOutput("textErrorLemma2")
                                )
                       ),
                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                        tags$div(i18n$t("please_wait"),id="loadmessage") # busy message
                       ),
                       conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                        tags$div(id="loadmessagebg") # busy message background to avoid clicking
                       )
                       
             ),
             
             
             tags$p(
               tags$table(
                 tags$tr(
                   tags$td(tags$h3("Grafy pro lemma \"")),
                   tags$td(tags$h3(uiOutput("lemmaref2"))),
                   tags$td(tags$h3("\":")),
                   tags$td(div(" " , style = "width: 30px;")),
                   tags$td(tags$h1(uiOutput("pokus")))
                 )
               )
             ),
             
             mainPanel(id="mainPanel", width = "100%",
                       tabsetPanel(id = "topPanels", type="pills", selected=i18n$t("Číslopád"),
                                   
                                   # -----------------------------------------------------------------------------------
                                   tabPanel(i18n$t("Číslo"),
                                            tags$h1(),
                                            
                                            div(tags$b(i18n$t("cislo_head2"))),
                                            br(),
                                            div(i18n$t("cislo_body2"), style = "width: 1000px;"),
                                            
                                            div(tags$p("")),
                                            div(i18n$t("boxplot_description"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            tags$p(
                                              tags$table(
                                                tags$tr(
                                                  tags$td(plotOutput("plotBoxplotCisla2"), width="400px"),
                                                  tags$td(plotOutput("plotBoxplotCislaLegend2"), width="400px")
                                                )
                                              )
                                            ),
                                            
                                            div(tags$p("")),
                                            div(i18n$t("cislo_histogram"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            
                                            tags$p(
                                              tags$table(
                                                tags$tr(
                                                  tags$td(plotOutput("plotHistogramCislaS2"), width="400px"),
                                                  tags$td(plotOutput("plotHistogramCislaP2"), width="400px")
                                                )
                                              )
                                            ),
                                            
                                            tags$hr(),
                                            
                                            tags$h3(i18n$t("cislo_tabulky_header")),
                                            tags$h4(radioButtons(inputId="rbCislo2", "",
                                                                 choices = c('singulars', 'plurals'),
                                                                 selected = "singulars",
                                                                 inline=T)),
                                            
                                            tags$p(""),
                                            div(i18n$t("cislo_tabulky_head")),
                                            tags$p(""),
                                            div(tags$b(i18n$t("cislo_tabulky_top")), style = "font-size:15px;"),
                                            tags$p(""),
                                            DT::dataTableOutput('tblCisloTop2'), hr(), div(id="cisloTopsel2", noseltext2),
                                            tags$p(""),
                                            div(tags$b(i18n$t("cislo_tabulky_bottom")), style = "font-size:15px;"),
                                            tags$p(""),
                                            DT::dataTableOutput('tblCisloBottom2'), hr(), div(id="cisloBottomsel", noseltext2),
                                            
                                            h1(),
                                            br(), br(), br(), br(), br(), br(), br()
                                   ),
                                   
                                   # -----------------------------------------------------------------------------------
                                   
                                   tabPanel(i18n$t("Číslopád"),
                                            tags$h1(),
                                            div(tags$b(i18n$t("cislopad_head2"))),
                                            br(),
                                            div(i18n$t("cislopad_body2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            
                                            if (lang != 'cs') {
                                              div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                            },
                                            tabsetPanel(id = "chPanels2", type="pills",
                                                        tabPanel(i18n$t("Boxplots"),
                                                                 tags$p(),
                                                                 
                                                                 div(tags$h1("")),
                                                                 div(i18n$t("boxplot_description2"), style = "width: 1000px;"),
                                                                 div(tags$p("")),
                                                                 
                                                                 tags$p(
                                                                   tags$table(
                                                                     tags$tr(
                                                                       tags$td(plotOutput("plotBoxplotCisloPad2"), width="800px"),
                                                                       tags$td(plotOutput("plotBoxplotCislopadLegend2"), width="400px")
                                                                     )
                                                                   )
                                                                 ),
                                                                 
                                                                 
                                                                 tags$hr(),
                                                                 
                                                                 tags$b(i18n$t("cislo_tabulky_header2")),
                                                                 tags$b(radioButtons(inputId="rbCisloPad2", "",
                                                                                     choices = append(singulars, plurals),
                                                                                     selected = "S1",
                                                                                     inline=T)),
                                                                 tags$p(""),
                                                                 div(tags$b(i18n$t("cislo_tabulky_top2")), style = "font-size:15px;"),
                                                                 div(i18n$t("cislo_tabulky_head2")),
                                                                 tags$p(""),
                                                                 tags$p(""),
                                                                 DT::dataTableOutput('tblcislopadTop2'),
                                                                 tags$p(""),
                                                                 div(tags$b(i18n$t("cislo_tabulky_bottom2")), style = "font-size:15px;"),
                                                                 tags$p(""),
                                                                 DT::dataTableOutput('tblcislopadBottom2'),
                                                                 h1(),
                                                                 br(), br(), br(), br(), br(), br(), br()
                                                                 
                                                        ),
                                                        
                                                        tabPanel(i18n$t("Histograms"),
                                                                 div(
                                                                   i18n$t("histograms_body2"),
                                                                   style = "width: 1000px;"
                                                                 ),
                                                                 tags$p(),
                                                                 radioButtons(inputId="rbCisloPadHist2", "",
                                                                              choices = append(singulars, plurals),
                                                                              selected = "S1",
                                                                              inline=T),
                                                                 plotOutput("plotHistogramCisloPad2", width="600px")
                                                        )
                                            )
                                   ),
                                   
                                   # -----------------------------------------------------------------------------------
                                   tabPanel(i18n$t("Rod2"),
                                            tags$h1(),
                                            div(tags$b(i18n$t("rod_head2"))),
                                            br(),
                                            div(i18n$t("rod_body2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            if (lang != 'cs') {
                                              div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                            },
                                            #tabsetPanel(id = "chPanels2-rod-2", type="pills",
                                            #            tabPanel(i18n$t("rod_chart1-2"),
                                            div(tags$h1("")),
                                            div(i18n$t("boxplot_description2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            plotOutput("plotBoxplotRodLemma2", height = "600"),
                                            #            ),
                                            #)
                                            
                                            
                                            tags$hr(),
                                            
                                            
                                            tags$b(i18n$t("rod_tabulky_header2")),
                                            tags$b(radioButtons(inputId="rbRod2", "",
                                                                choices = gender, 
                                                                selected = 'M',
                                                                inline=T)),
                                            tags$p(""),
                                            div(tags$b(i18n$t("rod_tabulky_top2")), style = "font-size:15px;"),
                                            div(i18n$t("rod_tabulky_head2")),
                                            tags$p(""),
                                            tags$p(""),
                                            DT::dataTableOutput('tblRodTop2'),
                                            tags$p(""),
                                            div(tags$b(i18n$t("rod_tabulky_bottom2")), style = "font-size:15px;"),
                                            tags$p(""),
                                            DT::dataTableOutput('tblRodBottom2'),
                                            h1(),
                                            br(), br(), br(), br(), br(), br(), br()
                                   ),
                                   # -----------------------------------------------------------------------------------
                                   tabPanel(i18n$t("Degree2"),
                                            tags$h1(),
                                            div(tags$b(i18n$t("degree_head2"))),
                                            br(),
                                            div(i18n$t("degree_body2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            if (lang != 'cs') {
                                              div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                            },
                                            div(tags$h1("")),
                                            div(i18n$t("boxplot_description2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            plotOutput("plotBoxplotDegreeLemma2", height = "600"),

                                            tags$hr(),
                                            
                                            tags$b(i18n$t("degree_tabulky_header2")),
                                            tags$b(radioButtons(inputId="rbDegree2", "",
                                                                choices = degree, 
                                                                selected = '1',
                                                                inline=T)),
                                            tags$p(""),
                                            div(tags$b(i18n$t("degree_tabulky_top2")), style = "font-size:15px;"),
                                            div(i18n$t("degree_tabulky_head2")),
                                            tags$p(""),
                                            tags$p(""),
                                            DT::dataTableOutput('tblDegreeTop2'),
                                            tags$p(""),
                                            div(tags$b(i18n$t("degree_tabulky_bottom2")), style = "font-size:15px;"),
                                            tags$p(""),
                                            DT::dataTableOutput('tblDegreeBottom2'),
                                            h1(),
                                            br(), br(), br(), br(), br(), br(), br()
                                            
                                   ),
                                   # -----------------------------------------------------------------------------------
                                   tabPanel(i18n$t("Negation2"),
                                            tags$h1(),
                                            div(tags$b(i18n$t("negation_head2"))),
                                            br(),
                                            div(i18n$t("negation_body2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            if (lang != 'cs') {
                                              div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                            },
                                            div(tags$h1("")),
                                            div(i18n$t("boxplot_description2"), style = "width: 1000px;"),
                                            div(tags$p("")),
                                            plotOutput("plotBoxplotNegationLemma2", height = "600"),
                                            
                                            tags$hr(),
                                            
                                            tags$b(i18n$t("negation_tabulky_header2")),
                                            tags$b(radioButtons(inputId="rbNegation2", "",
                                                                choices = negation, 
                                                                selected = 'A',
                                                                inline=T)),
                                            tags$p(""),
                                            div(tags$b(i18n$t("negation_tabulky_top2")), style = "font-size:15px;"),
                                            div(i18n$t("negation_tabulky_head2")),
                                            tags$p(""),
                                            tags$p(""),
                                            DT::dataTableOutput('tblNegationTop2'),
                                            tags$p(""),
                                            div(tags$b(i18n$t("negation_tabulky_bottom2")), style = "font-size:15px;"),
                                            tags$p(""),
                                            DT::dataTableOutput('tblNegationBottom2'),
                                            h1(),
                                            br(), br(), br(), br(), br(), br(), br()
                                   ),
                                   # -----------------------------------------------------------------------------------
                                   # tabPanel(i18n$t("Rodočíslopád"),
                                   #          tags$h1(),
                                   #          div(tags$b(i18n$t("rodocislopad_head2"))),
                                   #          br(),
                                   #          div(i18n$t("rodocislopad_body2"), style = "width: 1000px;"),
                                   #          div(tags$p("")),
                                   #          if (lang != 'cs') {
                                   #            div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                   #          },
                                   #          tabsetPanel(id = "chPanels2-2", type="pills",
                                   #                      tabPanel(i18n$t("rodocislopad_chart1-2"),
                                   #                               div(tags$h1("")),
                                   #                               div(i18n$t("boxplot_description2"), style = "width: 1000px;"),
                                   #                               div(tags$p("")),
                                   #                               plotOutput("plotBoxplotRodocislopad1Lemma2", height = "600")
                                   #                      ),
                                   #                      tabPanel(i18n$t("rodocislopad_chart2-2"),
                                   #                               tags$p(),
                                   #                               column(12, plotOutput("plotBoxplotRodocislopad22", height=600)),
                                   #                               tags$table(style = "width: 80%")
                                   #                      )
                                   #          )
                                   # ),
                                   # -----------------------------------------------------------------------------------
                                   tabPanel(i18n$t("Tabulky"),
                                            tags$h1(),
                                            div(i18n$t("tables_body1_2"), br(),
                                                i18n$t("tables_body2_2"), br(),
                                                i18n$t("tables_body3_2"), br(),
                                                i18n$t("tables_body4_2"), br(),
                                                i18n$t("tables_body5_2"), br(),
                                                i18n$t("tables_body6_2"),
                                                style = "width: 1000px;"
                                            ),
                                            if (lang != 'cs') {
                                              div(tags$p(""), fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                            },
                                            
                                            hr(),
                                            div(id="data12sel", noseltext),
                                            hr(),
                                            
                                            hr(), hr(), hr(), h1('TestNahore'), hr(), hr(), hr(),
                                            
                                            tabsetPanel(id = "chPanels22222", type="pills",
                                                        tabPanel(i18n$t("fullTabRodCisloPad2"),
                                                                 h1('TestTestTest fullTabRodCisloPad2'),
                                                                 div(id="data12sel", noseltext),
                                                                 
                                                                 tags$p(),
                                                                 tabPanel(i18n$t("Tabulky"),
                                                                          tags$h1(),
                                                                          DT::dataTableOutput('data12'),
                                                                          h1(), br(), br(), br(), br(), br(), br(), br()
                                                                 )
                                                        ),
                                                        tabPanel(i18n$t("fullTabRod2"),
                                                                 h1('TestTestTest fullTabRod2'),
                                                                 div(id="data12sel", noseltext),
                                                                 
                                                                 tags$p(),
                                                                 tabPanel(i18n$t("Tabulky"), tags$h1(),
                                                                          DT::dataTableOutput('dataAdjGender'),
                                                                          h1(), br(), br(), br(), br(), br(), br(), br()
                                                                 )
                                                        ),
                                                        tabPanel(i18n$t("fullTabCase2"),
                                                                 tags$p(),
                                                                 tabPanel(i18n$t("Tabulky"), tags$h1(),
                                                                          DT::dataTableOutput('dataAdjCase'),
                                                                          h1(), br(), br(), br(), br(), br(), br(), br()
                                                                 )
                                                        ),
                                                        tabPanel(i18n$t("fullTabDegree2"),
                                                                 tags$p(),
                                                                 tabPanel(i18n$t("Tabulky"), tags$h1(),
                                                                          DT::dataTableOutput('dataAdjDegree'),
                                                                          h1(), br(), br(), br(), br(), br(), br(), br()
                                                                 )
                                                        ),
                                                        tabPanel(i18n$t("fullTabNegation2"),
                                                                 tags$p(),
                                                                 tabPanel(i18n$t("Tabulky"), tags$h1(),
                                                                          DT::dataTableOutput('dataAdjNegation'),
                                                                          h1(), br(), br(), br(), br(), br(), br(), br()
                                                                 )
                                                        )
                                            )
                                   )

                                   
                       ) # /tabsetPanel topPanels
             ) # /mainPanel
             
             
             
    ), # /adjektivaTab
    
    
    
    
 ################################################       /ADJEKTIVA      ##############################################################    
 
    
    
    
    
 ################################################       VERBA      ##############################################################    
 
 tabPanel(title ="", value = i18n$t("verbaTab"),
          div(id="divintro", style="width: 1000px; display: flex; justify-content: space-between; border: 0px solid black;", tags$b(i18n$t("GramatiKat_head"))),
          tags$div( p(""), br() ),
          wellPanel(style= "display: inline-block; padding: 0px 0px 0px 0px;",
                    tags$div(style= "display: block; padding: 0px 0px 0px 0px;",
                             tags$style("#x1_current_week {font-size:20px; color:red; display:block; }"),
                             div(style="padding: 0px 15px 5px 15px; display: flex; justify-content: start",
                                 tags$style("#textLemma2 {font-size:20px; color:Black; display:inline-block; margin-left: 0px; margin-top: 22px; margin-right: 10px;}"),
                                 div(id="textLemma2", class="shiny-text-output shiny-bound-output", "Lemma:"),

                                 textInput(inputId="searchLemma3", label="", value=defaultLemma3, placeholder = "Search term"),
                                 actionButton("changeLemma3", i18n$t("Načíst"), style = "height: 34px; margin-left: 10px; margin-top: 20px;"),
                                 tags$style("#textErrorLemma3 {font-size:20px; color: #ea670c; display:block; margin-left: 10px; margin-top: 20px;}"),
                                 textOutput("textErrorLemma3")
                             )
                    ),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(i18n$t("please_wait"),id="loadmessage") # busy message
                    ),
                    conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div(id="loadmessagebg") # busy message background to avoid clicking
                    )
          ),
          

          tags$p(
            tags$table(
              tags$tr(
                tags$td(tags$h3("Grafy pro lemma \"")),
                tags$td(tags$h3(uiOutput("lemmaref3"))),
                tags$td(tags$h3("\":")),
                tags$td(div(" " , style = "width: 30px;")),
                tags$td(tags$h1(uiOutput("pokus 3")))
              )
            )
          ),
 

          mainPanel(id="mainPanel", width = "100%",
                    tabsetPanel(id = "topPanels", type="pills", selected=i18n$t("Čísloosoba"),
                       tabPanel(i18n$t("Čísloosoba"),
                                tags$h1(""),
                                div(tags$b(i18n$t("cisloosoba_head3"))),
                                br(),
                                div(i18n$t("cisloosoba_body3"), style = "width: 1000px;"),
                                div(tags$p("")),
                                           
                                if (lang != 'cs') {
                                    div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
                                },
                                           
                                tabsetPanel(id = "chPanels3", type="pills",
                                            tabPanel(i18n$t("Boxplots"),
                                                     tags$p(),

                                                     div(tags$h1("")),
                                                     div(i18n$t("boxplot_description3"), style = "width: 1000px;"),
                                                     div(tags$p("")),

                                                              tags$p(
                                                                tags$table(
                                                                  tags$tr(
                                                                    tags$td(plotOutput("plotBoxplotCisloOsoba3"), width="800px"),
                                                                    tags$td(plotOutput("plotBoxplotCisloosobaLegend3"), width="400px")
                                                                  )
                                                                )
                                                              ),

                                                              tags$hr(),

                                                              tags$b(i18n$t("cislo_tabulky_header3")),
                                                              tags$b(radioButtons(inputId="rbCisloOsoba3", "",
                                                                                  choices = append(singulars3, plurals3),
                                                                                  selected = "S1",
                                                                                  inline=T)),
                                                              tags$p(""),
                                                              div(tags$b(i18n$t("cislo_tabulky_top3")), style = "font-size:15px;"),
                                                              div(i18n$t("cislo_tabulky_head3")),
                                                              tags$p(""),
                                                              tags$p(""),
                                                              DT::dataTableOutput('tblcisloosobaTop3'),
                                                              tags$p(""),
                                                              div(tags$b(i18n$t("cislo_tabulky_bottom3")), style = "font-size:15px;"),
                                                              tags$p(""),
                                                              DT::dataTableOutput('tblcisloosobaBottom3'),
                                                              h1(),
                                                              br(), br(), br(), br(), br(), br(), br()

                                                     ),

                                            tabPanel(i18n$t("Histograms"),
                                                     div(
                                                         i18n$t("histograms_body3"),
                                                         style = "width: 1000px;"
                                                        ),
                                                     tags$p(),
                                                              radioButtons(inputId="rbCisloOsobaHist3", "",
                                                                           choices = append(singulars, plurals),
                                                                           selected = "S1",
                                                                           inline=T),
                                                              plotOutput("plotHistogramCisloOsoba3", width="600px")
                                                     )
                                            )
                                ),


                                # -----------------------------------------------------------------------------------
                                tabPanel(i18n$t("Číslo"),
                                         tags$h1(),

                                         div(tags$b(i18n$t("cislo_head3"))),
                                         br(),
                                         div(i18n$t("cislo_body3"), style = "width: 1000px;"),

                                         div(tags$p("")),
                                         div(i18n$t("boxplot_description"), style = "width: 1000px;"),
                                         div(tags$p("")),
                                         tags$p(
                                           tags$table(
                                             tags$tr(
                                               tags$td(plotOutput("plotBoxplotCisla3"), width="400px"),
                                               tags$td(plotOutput("plotBoxplotCislaLegend3"), width="400px")
                                             )
                                           )
                                         ),

                                         div(tags$p("")),
                                         div(i18n$t("cislo_histogram"), style = "width: 1000px;"),
                                         div(tags$p("")),

                                         tags$p(
                                           tags$table(
                                             tags$tr(
                                               tags$td(plotOutput("plotHistogramCislaS3"), width="400px"),
                                               tags$td(plotOutput("plotHistogramCislaP3"), width="400px")
                                             )
                                           )
                                         ),

                                         tags$hr(),

                                         tags$h3(i18n$t("cislo_tabulky_header")),
                                         tags$h4(radioButtons(inputId="rbCislo3", "",
                                                              choices = c('singulars', 'plurals'),
                                                              selected = "singulars",
                                                              inline=T)),

                                         tags$p(""),
                                         div(i18n$t("cislo_tabulky_head")),
                                         tags$p(""),
                                         div(tags$b(i18n$t("cislo_tabulky_top3")), style = "font-size:15px;"),
                                         tags$p(""),
                                         DT::dataTableOutput('tblCisloTop3'), hr(), div(id="cisloTopsel3", noseltext3),
                                         tags$p(""),
                                         div(tags$b(i18n$t("cislo_tabulky_bottom3")), style = "font-size:15px;"),
                                         tags$p(""),
                                         DT::dataTableOutput('tblCisloBottom3'), hr(), div(id="cisloBottomsel3", noseltext3),

                                         h1(),
                                         br(), br(), br(), br(), br(), br(), br()
                                ),

 #                                # -----------------------------------------------------------------------------------
 #                                # tabPanel(i18n$t("Způsobčas3"),
 #                                #          tags$h1(),
 #                                #          div(tags$b(i18n$t("zpusobcas_head3"))),
 #                                #          br(),
 #                                #          div(i18n$t("zpusobcas_body3"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          if (lang != 'cs') {
 #                                #            div(fnCaseDescription(), tags$p(""), tags$p(i18n$translation_language), style = "width: 1000px;")
 #                                #          },
 #                                #          div(tags$h1("")),
 #                                #          div(i18n$t("boxplot_description3"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          plotOutput("plotBoxplotZpusobcasLemma3", height = "600"),
 #                                # 
 #                                #          
 #                                #          tags$hr(),
 #                                #          
 #                                #          h1("Nejake kecy o zpusobcas"),
 #                                #          br(), br(), br(), br(), br(), br(), br()
 #                                # ),
 #                                
 #                                # tabPanel(i18n$t("Rod jmenný"),
 #                                #          tags$h1(),
 #                                #          
 #                                #          div(tags$b(i18n$t("rodjmenny_head3"))),
 #                                #          br(),
 #                                #          div(i18n$t("rodjmenny_body3"), style = "width: 1000px;"),
 #                                #          
 #                                #          div(tags$p("")),
 #                                #          div(i18n$t("boxplot_description"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          tags$p(
 #                                #            tags$table(
 #                                #              tags$tr(
 #                                #                tags$td(plotOutput("plotBoxplotRodjmenny3"), width="400px"),
 #                                #                tags$td(plotOutput("plotBoxplotRodjmennyLegend3"), width="400px")
 #                                #              )
 #                                #            )
 #                                #          ),
 #                                #          
 #                                #          div(tags$p("")),
 #                                #          #div(i18n$t("rodjmenny_histogram"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          h1("Nejake kecy o rod jmenny")
 #                                #          
 #                                # ),
 # 
 #                                # tabPanel(i18n$t("Rod slovesný"),
 #                                #          tags$h1(),
 #                                #          
 #                                #          div(tags$b(i18n$t("rodslovesny_head3"))),
 #                                #          br(),
 #                                #          div(i18n$t("rodslovesny_body3"), style = "width: 1000px;"),
 #                                #          
 #                                #          div(tags$p("")),
 #                                #          div(i18n$t("boxplot_description"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          tags$p(
 #                                #            tags$table(
 #                                #              tags$tr(
 #                                #                tags$td(plotOutput("plotBoxplotRodslovesny3"), width="400px"),
 #                                #                tags$td(plotOutput("plotBoxplotRodslovesnyLegend3"), width="400px")
 #                                #              )
 #                                #            )
 #                                #          ),
 #                                #          
 #                                #          div(tags$p("")),
 #                                #          #div(i18n$t("rodslovesny_histogram"), style = "width: 1000px;"),
 #                                #          div(tags$p("")),
 #                                #          h1("Nejake kecy o rod slovesny")
 #                                # )
 #                                
                     ) # /tabsetPanel topPanels
          ), # /mainPanel
 ), # /verbaTab

 
 
 
 ################################################       /VERBA      ##############################################################     
    
    
    
    
    
    
    
        
    tabPanel(title ="", value = i18n$t("aboutTab"), 
             #tags$h1(),
             img(src=i18n$t("logolink_OP_VVV_hor_barva"), style="padding-top: 10px; padding-bottom: 10px; width: 800px;"),
             tags$p(""),
             
             div(
               
               
               tags$b(i18n$t("GramatiKat_body_head")),
               tags$p(""),
               tags$div(i18n$t("GramatiKat_body")),
               tags$p(""),
               
               
               tags$b(i18n$t("about_citace_head")),
               tags$p(""),
               tags$div(i18n$t("about_citace_body1"), br(),
                        i18n$t("about_citace_body2")),
               tags$p(""),
               
               
               tags$b(i18n$t("about_dedikace_head")),
               tags$p(""),
               tags$div(i18n$t("about_dedikace_body")),
               tags$p(""),
               tags$p(""),
               tags$div(i18n$t("Český národní korpus"), ":", tags$a(href="https://www.korpus.cz/", "www.korpus.cz")),
               tags$div("Kreas", ":", tags$a(href="https://kreas.ff.cuni.cz/", "kreas.ff.cuni.cz")),
               tags$div("Feast and Famine", ":",  tags$a(href="https://www.sheffield.ac.uk/feastandfamine", "https://www.sheffield.ac.uk/feastandfamine")),
               tags$p(""),
               
               tags$b(i18n$t("about_data_head")),
               tags$p(""),
               tags$div(i18n$t("about_data_body1"),
                        tags$a(href="https://www.korpus.cz/", "https://www.korpus.cz"),
                        br(),
                        i18n$t("about_data_body2"),
                        tags$a(href="https://www.korpus.cz/", "https://www.korpus.cz"),
                        br(),
                        i18n$t("about_data_body3"),
                        tags$a(href="https://www.korpus.cz/", "https://www.korpus.cz")),
               tags$p(""),
               
               tags$b(i18n$t("about_literature_head")),
               tags$p(""),
               tags$div(style="white-space: pre-wrap;", i18n$t("about_literature_body")),
               tags$p(""),
               
               
               tags$b(i18n$t("about_spoluprace_head")),
               tags$p(""),
               tags$div(i18n$t("about_spoluprace_body")),
               tags$p(""),

               
               tags$b(i18n$t("about_authors")),
               tags$p(""),
               tags$div(
                 style= "display: block;",
                 div(style="padding: 15px 15px 15px 15px; display: flex; justify-content: start",
                     img(src="dominika_kovarikova.jpg", style="padding-right: 10px; padding-bottom: 0px; width: 80px; height: 93px;"),
                     div(
                       tags$b("Dominika Kováříková, Ph.D."),
                       tags$div("lingvistka, Ústav Českého národního korpusu FF UK"),
                       tags$div("dominika.kovarikova@ff.cuni.cz"),
                       tags$p("")
                     )
                 )
               ),
               tags$p(""),
               
               tags$div(
                 style= "display: block;",
                 div(style="padding: 15px 15px 15px 15px; display: flex; justify-content: start",
                     img(src="oleg_kovarik3.jpg", style="padding-right: 10px; padding-bottom: 0px; width: 80px; height: 87px;"),
                     div(
                       tags$b("Ing. Oleg Kovářík"),
                       tags$div("programátor, Datamole s.r.o."),
                       tags$div("oleg.kovarik@gmail.com"),
                       tags$p("")
                     )
                 )
               ),
               
               
               tags$br(),
               tags$br(),
               tags$p(""),
               tags$p("")
               
               , style = "width: 70%")             
    ) # /aboutTab    
    ) # /aboutPanels
    
   
    
    
    
    
    
  )

  #)
}
