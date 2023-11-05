############################################
#                                          #
#       GramatiKat - Dominika Kovarikova   #
#                                          #
#                  SERVER                  #
#                                          #
############################################

library(ggplot2)
# library(shinyBS)
library(DT)
library(reshape2)
library(tidyverse)

source("boxplot_legend.R") # boxplot principle explanation on front page

histbw = 0.05 # bin width for histograms

server <- function(input, output, session) {
  print("neco2")
  #if (!init_shiny_cnc("gramatikat")) return()
  print("neco3")
  
  # set up the translator object
  lang <- get_lang(session)
  print("Language (server): ")
  print(lang)

  i18n <- Translator$new(translation_json_path = "translation.json")
  i18n$set_translation_language(lang)

  # dynamically render the UI using the translator object
  output$`localized-ui` <- renderUI(localized_UI(i18n, lang))

  # --------------------------------
  SearchLemma <- reactiveVal()
#  moreInfo <- reactiveValues(showinfo = FALSE)

#  observeEvent(input$more_info, {
#    moreInfo$showinfo <- !moreInfo$showinfo
#  })

#  output$more_info <- renderText({
#    if (!moreInfo$showinfo) return()
#    i18n$t("GramatiKat_body")
#  })

  # observeEvent(input$hideTab, {
  #   showTab(inputId = "tabs", target = "Bar")
  #   hideTab(inputId = "tabs", target = "Foo")
  # })
  # 
  # observeEvent(input$showTab, {
  #   showTab(inputId = "tabs", target = "Foo")
  #   hideTab(inputId = "tabs", target = "Bar")
  #   print("show tab")
  # })
  observeEvent(input[["keyPressed"]], {
    print("keyPressed")
    print(defaultLemma)
    print(defaultLemma2)
    print(input$searchLemma)
    print(sellemmar2$lemma)
    
    SearchLemma(input[["searchLemma"]])
    selectedData <- dfall[ which(dfall$lemma == input$searchLemma), ]

    # Try to change Lemma
    if (nrow(selectedData) > 0) {
      if (selectedData['FQ'] > 100) {
        sellemma <- input$searchLemma
        sellemmar$lemma <- sellemma
        sellemmar$data <- dfall[ which(dfall$lemma == sellemma), ]
        sellemmar$datarod <- dfallrod[ which(dfallrod$lemma == sellemma), ]
        output$textErrorLemma <- renderText('')
      } else {
        output$textErrorLemma <- renderText('Chyba: lemma s malou frekvencí (FQ < 100)')
      }

    } else {
      output$textErrorLemma <- renderText('Chyba: lemma nenalezeno')
    }
  })


    ######################################################################
  # INIT - PART 1

  # define variables
  output$`port-number` <- renderText(session$request$SERVER_PORT)
  output$`cnc-toolbar-lang` <- renderText(get_lang(session))
  output$textErrorLemma <- renderText('')
  #cdatasrc <- c('syn2015', 'oral_v4', 'intercorp_v13_sk', 'intercorp_v13_cs_intersect', 'intercorp_v13_sk_intersect')
  #cdatasrc <- c('syn2015', 'oral_v4', 'intercorp_v13_sk', 'intercorp_v13_cs_intersect', 'intercorp_v13_sk_intersect', 'syn2015_syn2020_fic', 'syn2015_syn2020_nfc', 'syn2015_syn2020_nmg', 'syn2020', 'syn_v9_2015_2020', 'syn_v9_all')
  cdatasrc <- c('syn2015', 'oral_v4', 'intercorp_v13_cs_intersect', 'intercorp_v13_sk_intersect', 'syn2015_syn2020_fic', 'syn2015_syn2020_nfc', 'syn2015_syn2020_nmg')
  cdatasrc2 <- c('syn_v11') # syn_v11_15_20 == syn_v11
  selecteddatasrc = 'syn2015'
  selecteddatasrc2 = 'syn_v11'
  urlkontext <- "<a href=\"https://kontext.korpus.cz/first?corpname=syn2015&queryselector=cqlrow&default_attr=word&cql="

  # /INIT - PART1
  ######################################################################


  fnRemoveNames <- function(data) {
    print("fnRemoveNames")
    # remove Lemmas starting with big letter
    d <- filter(data, !str_detect(lemma, "^[:upper:]"))
    return(d)
  }

  fnReadData <- function(prefix) {
    #  read data from prefix

    # dfall - all data: FQ > 100, percentual for each lemma
    #   clemmas - list of lemmas
    # dfsub - random subsanmple 10000???
    print("fnReadData ")

    dfallrod <- read.table(paste0("data/", prefix, "-subst-sg-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    drops <- c("rod")
    dfall <- dfallrod[ , !(names(dfallrod) %in% drops)]
    dfall <<- dfall[order(dfall$lemma),]
    clemmas <<- as.character(dfall$lemma)

    # tochange: import Singular/Plural file previously extracted in python
    dfallSP <- read.table(paste0("data/", prefix, "-subst-sg-sp-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    dfallSP <<- dfallSP[order(dfallSP$lemma),]

    dfallrod <<- dfallrod[order(dfallrod$lemma),]
    clemmasrod <<- as.character(dfallrod$lemma)

    dfsub <<- read.table(paste0("data/", prefix, "-subst-sg-100p-Rsub.csv"), header = TRUE, sep = ",")
    dfsub$cislopad <<- factor(dfsub$cislopad, levels = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7'))

    dfx <<- read.table(paste0("data/", prefix, "-subst-sg-100p-R.csv"), header = TRUE, sep = ",")
    dfx$cislopad <<- factor(dfx$cislopad, levels = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7'))
    #dfx$cislopad <<- factor(dfx$cislopad, levels = c('NomS', 'GenS', 'DatS', 'AccS', 'VocS', 'LocS', 'InsS', 'NomP', 'GenP', 'DatP', 'AccP', 'VocP', 'LocP', 'InsP'))
    #dfx$value <<- dfx$value * 100
    #print(fnGetOutliers(dfall))

    #updateSelectizeInput(session = session, inputId = 'inSelectLemma', choices = clemmas, server = TRUE, selected=defaultLemma)
    updateSelectizeInput(session = session, inputId = 'inSelectLemma', choices = clemmas, server = TRUE, selected=prefix)
    
    return()
  }

  fnReadData2 <- function(prefix_adjectives) {
    #  read data from prefix - adjectives
    print("fnReadData2 ")
    print(prefix_adjectives)
    if (prefix_adjectives == 'syn_v11') {prefix_adjectives = paste0(prefix_adjectives, '_15_20')}
      
    dfallrod2 <- read.table(paste0("data/", prefix_adjectives, "_adjective-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    drops2 <- c("rod")
    dfall2 <- dfallrod2[ , !(names(dfallrod2) %in% drops2)]
    dfall2 <<- dfall2[order(dfall2$lemma),]
    clemmas2 <<- as.character(dfall2$lemma)
    
    # tochange: import Singular/Plural file previously extracted in python
    dfallSP2 <- read.table(paste0("data/", prefix_adjectives, "_adjective-sp-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    dfallSP2 <<- dfallSP2[order(dfallSP2$lemma),]
    
    dfallrod2 <<- dfallrod2[order(dfallrod2$lemma),]
    clemmasrod2 <<- as.character(dfallrod2$lemma)
    
    dfsub2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-Rsub.csv"), header = TRUE, sep = ",")
    dfsub2$cislopad2 <<- factor(dfsub2$cislopad, levels = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7'))
    
    dfx2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-R.csv"), header = TRUE, sep = ",")
    dfx2$cislopad2 <<- factor(dfx2$cislopad, levels = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7'))

    dfgender100p2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-gender.csv"), header = TRUE, sep = "\t")
    #dfgender100p2$gender <<- factor(dfgender100p2$gender, levels = c('GM', 'GI', 'GF', 'GN'))

    dfcase100p2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-case.csv"), header = TRUE, sep = "\t")
    
    dfdegree100p2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-degree.csv"), header = TRUE, sep = "\t")
    
    dfnegation100p2 <<- read.table(paste0("data/", prefix_adjectives, "_adjective-100p-negation.csv"), header = TRUE, sep = "\t")
    
    updateSelectizeInput(session = session, inputId = 'inSelectLemmaAdj', choices = clemmas2, server = TRUE, selected=prefix_adjectives)
    
    return()
  }

  fnReadData3 <- function(prefix_verbs) {
    #  read data from prefix - verbs
    print("fnReadData3 ")
    print(prefix_verbs)
    if (prefix_verbs == 'syn_v11') {prefix_verbs = paste0(prefix_verbs, '_15_20')}
    
    dfallrod3 <- read.table(paste0("data/", prefix_verbs, "_verb-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    drops3 <- c("rod", "O")
    dfall3 <- dfallrod3[ , !(names(dfallrod3) %in% drops3)]
    dfall3 <<- dfall3[order(dfall3$lemma),]
    clemmas3 <<- as.character(dfall3$lemma)
    
    dfallrod3 <- read.table(paste0("data/", prefix_verbs, "_verb-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    drops3 <- c("rod", "O")
    dfall3 <- dfallrod3[ , !(names(dfallrod3) %in% drops3)]

    dfallSP3 <- read.table(paste0("data/", prefix_verbs, "_verb-sp-100p.csv"), header = TRUE, sep = "\t", quote="\"")
    dfallSP3 <<- dfallSP3[order(dfallSP3$lemma),]
    
    dfallrod3 <<- dfallrod3[order(dfallrod3$lemma),]
    clemmasrod3 <<- as.character(dfallrod3$lemma)
    
    # dfsub3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-Rsub.csv"), header = TRUE, sep = ",")
    # dfsub3$cislopad2 <<- factor(dfsub3$cislopad, levels = c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7'))
    
    dfx3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-cisloosoba-R.csv"), header = TRUE, sep = ",")
    dfx3$cisloosoba3 <<- factor(dfx3$cisloosoba, levels = c('S1', 'S2', 'S3', 'P1', 'P2', 'P3'))
    
    # dfgender100p3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-gender.csv"), header = TRUE, sep = "\t")

    # dfcase100p3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-case.csv"), header = TRUE, sep = "\t")
    
    # dfdegree100p3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-degree.csv"), header = TRUE, sep = "\t")
    
    # dfnegation100p3 <<- read.table(paste0("data/", prefix_verbs, "_verb-100p-negation.csv"), header = TRUE, sep = "\t")
    
    updateSelectizeInput(session = session, inputId = 'inSelectLemmaVerb', choices = clemmas2, server = TRUE, selected=prefix_verbs)
    
    return()
  }
  
    
  # change selected lemma
  fnChangeLemma <- function(newLemma) {
    print("fnChangeLemma1")
    sellemma <- newLemma
    sellemmar$lemma <- sellemma
    sellemmar$data <- dfall[ which(dfall$lemma == sellemma), ]
    sellemmar$datarod <- dfallrod[ which(dfallrod$lemma == sellemma), ]

    output$textErrorLemma <- renderText('')
    updateTextInput(session, "searchLemma", value = newLemma)
  }
  
  # change selected lemma
  fnChangeLemma2 <- function(newLemma2) {
    print("fnChangeLemma2")
    print(newLemma2)
    sellemma2 <- newLemma2
    sellemmar2$lemma <- sellemma2
    sellemmar2$data <- dfall2[ which(dfall2$lemma == sellemma2), ]
    sellemmar2$datarod <- dfallrod2[ which(dfallrod2$lemma == sellemma2), ]
    
    
    selectedData2 <- dfall2[ which(dfall2$lemma == sellemma2), ]
    
    if (selectedData2['FQ'] > 1000000) {
      output$pokus <- renderText({ paste("   ****") })
    } else if (selectedData2['FQ'] > 100000) {
      output$pokus <- renderText({ paste("   ****") })
    } else if (selectedData2['FQ'] > 10000) {
      output$pokus <- renderText({ paste("   ***") })
    } else if (selectedData2['FQ'] > 1000) {
      output$pokus <- renderText({ paste("   **") })
    } else if (selectedData2['FQ'] > 100) {
      output$pokus <- renderText({ paste("   *") })
    } else {
      output$pokus <- renderText({ paste("   ") })
    }
    print(selectedData2['FQ'])
    
    output$textErrorLemma <- renderText('')
    updateTextInput(session, "searchLemma2", value = newLemma2)
  }

  # change selected lemma for Verbs
  fnChangeLemma3 <- function(newLemma3) {
    print("fnChangeLemma3")
    print(newLemma3)
    sellemma3 <- newLemma3
    sellemmar3$lemma <- sellemma3
    sellemmar3$data <- dfall3[ which(dfall3$lemma == sellemma3), ]
    sellemmar3$datarod <- dfallrod3[ which(dfallrod3$lemma == sellemma3), ]
    
    selectedData3 <- dfall3[ which(dfall3$lemma == sellemma3), ]
    
    print(selectedData3)
    if (selectedData3['FQ'] > 1000000) {
      output$pokus <- renderText({ paste("   ****") })
    } else if (selectedData3['FQ'] > 100000) {
      output$pokus <- renderText({ paste("   ****") })
    } else if (selectedData3['FQ'] > 10000) {
      output$pokus <- renderText({ paste("   ***") })
    } else if (selectedData3['FQ'] > 1000) {
      output$pokus <- renderText({ paste("   **") })
    } else if (selectedData3['FQ'] > 100) {
      output$pokus <- renderText({ paste("   *") })
    } else {
      output$pokus <- renderText({ paste("   ") })
    }

    print(selectedData3['FQ'])
    
    output$textErrorLemma <- renderText('')
    updateTextInput(session, "searchLemma3", value = newLemma3)
  }
  
  # reset data and filters for all tables
  fnResetTblFilters <- function() {
    print("fnResetTblFilters")
    # Cislo
    dtproxygg <- DT::dataTableProxy('tblCisloTop')
    dfoutliersCisloTop <<- fnFilterOutliersTop(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::replaceData(dtproxygg, as.data.frame(dfoutliersCisloTop))
    clearSearch(dtproxygg)

    dtproxyhh <- DT::dataTableProxy('tblCisloBottom')
    dfoutliersCisloBottom <<- fnFilterOutliersBottom(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::replaceData(dtproxyhh, as.data.frame(dfoutliersCisloBottom))
    clearSearch(dtproxyhh)

    # CisloPad
    dtproxyii <- DT::dataTableProxy('tblcislopadTop')
    dfoutliersCisloPadTop <- fnFilterOutliersTop(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::replaceData(dtproxyii, as.data.frame(dfoutliersCisloPadTop))
    clearSearch(dtproxyii)

    # table cislopadBottom
    dtproxyjj <- DT::dataTableProxy('tblcislopadBottom')
    dfoutliersCisloPadBottom <- fnFilterOutliersBottom(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::replaceData(dtproxyjj, as.data.frame(dfoutliersCisloPadBottom))
    clearSearch(dtproxyjj)


    # Tabulky
    proxy1 <- DT::dataTableProxy('data1')
    DT::replaceData(proxy1, as.data.frame(dfallrod))
    clearSearch(proxy1)

    sellemma <- sellemmar$lemma
    selrod <- sellemmar$datarod['rod'][, ]

    cql <- paste0(URLencode("[lemma=\""), URLencode(toString(sellemma)), URLencode("\" "),
                  "%26", URLencode(" tag=\".."), URLencode(toString(selrod)), URLencode(".*\"]"))
    href <- paste0(urlkontext, cql, "\" target=\"_blank\">Zobrazit výskyty</a>")
    cqlhtml <- paste0("[lemma=\"", toString(sellemma), "\"", " & ", "tag=\"..", toString(selrod), ".*\"]")
    html(id = "data1sel", paste0(href, " v korpusu výsledky CQL dotazu ", cqlhtml))
  }
  
  # reset data and filters for all tables
  fnResetTblFilters2 <- function() {
    print("fnResetTblFilters 2")
    # Cislo
    dtproxygg <- DT::dataTableProxy('tblCisloTop')
    dfoutliersCisloTop <<- fnFilterOutliersTop(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::replaceData(dtproxygg, as.data.frame(dfoutliersCisloTop))
    clearSearch(dtproxygg)
    
    dtproxyhh <- DT::dataTableProxy('tblCisloBottom')
    dfoutliersCisloBottom <<- fnFilterOutliersBottom(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::replaceData(dtproxyhh, as.data.frame(dfoutliersCisloBottom))
    clearSearch(dtproxyhh)
    
    # CisloPad
    dtproxyii <- DT::dataTableProxy('tblcislopadTop')
    dfoutliersCisloPadTop <- fnFilterOutliersTop(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::replaceData(dtproxyii, as.data.frame(dfoutliersCisloPadTop))
    clearSearch(dtproxyii)
    
    # table cislopadBottom
    dtproxyjj <- DT::dataTableProxy('tblcislopadBottom')
    dfoutliersCisloPadBottom <- fnFilterOutliersBottom(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::replaceData(dtproxyjj, as.data.frame(dfoutliersCisloPadBottom))
    clearSearch(dtproxyjj)
    
    
    # Tabulky
    proxy1 <- DT::dataTableProxy('data1')
    DT::replaceData(proxy1, as.data.frame(dfallrod))
    clearSearch(proxy1)
    
    sellemma2 <- sellemmar2$lemma
    selrod2 <- sellemmar2$datarod['rod'][, ]
    
    cql <- paste0(URLencode("[lemma=\""), URLencode(toString(sellemma2)), URLencode("\" "),
                  "%26", URLencode(" tag=\".."), URLencode(toString(selrod2)), URLencode(".*\"]"))
    href <- paste0(urlkontext, cql, "\" target=\"_blank\">Zobrazit výskyty</a>")
    cqlhtml <- paste0("[lemma=\"", toString(sellemma2), "\"", " & ", "tag=\"..", toString(selrod2), ".*\"]")
    html(id = "data1sel", paste0(href, " v korpusu výsledky CQL dotazu ", cqlhtml))
  }
  
  ################################################################################
  # Buttons

  observeEvent(input$goButton1, ({
    shinyjs::show(id = "selectCorpora")
    shinyjs::hide(id = "selectCorporaAdj")
    shinyjs::hide(id = "selectCorporaVerb")
    shinyjs::hide(id = "selectCorporaNone")
    
      updateSelectizeInput(session = session, inputId = 'inSelectCorpus', choices = cdatasrc, server = TRUE, selected=selecteddatasrc)
  }))
  
  observeEvent(input$goButton2, ({
    #updateSelectizeInput(session = session, inputId = 'inSelectCorpus', choices = cdatasrc2, server = TRUE, selected=selecteddatasrc2)
    #fnReadData2(selecteddatasrc2)
    
    shinyjs::show(id = "selectCorporaAdj")
    shinyjs::hide(id = "selectCorporaNone")
    shinyjs::hide(id = "selectCorporaVerb")
    shinyjs::hide(id = "selectCorpora")

    updateSelectizeInput(session = session, inputId = 'inSelectCorpusAdj', choices = cdatasrc2, server = TRUE, selected=selecteddatasrc2)
    print("prepnuti na Adjektivita")
    fnChangeLemma2("skromný")
    #fnReadData2(selecteddatasrc2)
  }))

  observeEvent(input$goButton3, ({
    shinyjs::show(id = "selectCorporaVerb")
    shinyjs::hide(id = "selectCorporaNone")
    shinyjs::hide(id = "selectCorporaAdj")
    shinyjs::hide(id = "selectCorpora")
    
    updateSelectizeInput(session = session, inputId = 'inSelectCorpusVerb', choices = cdatasrc2, server = TRUE, selected=selecteddatasrc2)
    print("prepnuti na Verba")
    fnChangeLemma3("zkrotit")
  }))
  
  observeEvent(input$goButtonAbout, ({
    shinyjs::show(id = "selectCorporaNone")
    shinyjs::hide(id = "selectCorpora")
    shinyjs::hide(id = "selectCorporaAdj")
    shinyjs::hide(id = "selectCorporaVerb")
  }))
  
  # Reaction to "change corpus" sourcebutton
  observeEvent(input$changeCorpus, {
    validate( need(input$inSelectCorpus, 'Vyberte korpus!') )
    selecteddatasrc <- input$inSelectCorpus

    fnReadData(selecteddatasrc)
    if (input$searchLemma %in% clemmas) { fnChangeLemma(input$searchLemma) }
    else {
      #if (lang == "cs") { fnChangeLemma(defaultLemma) }
      if (selecteddatasrc == 'intercorp_v13_sk_intersect') { fnChangeLemma("mačka") }
      else { fnChangeLemma(defaultLemma) }
    }
    #else { fnChangeLemma(defaultLemma) }
    
    urlkontext <<- paste0("<a href=\"https://kontext.korpus.cz/first?corpname=", selecteddatasrc, "&queryselector=cqlrow&default_attr=word&cql=")

    # reload table
    fnResetTblFilters()
  })

  # Reaction to "change corpus" sourcebutton
  observeEvent(input$changeCorpusAdj, {
    print("input$changeCorpusAdj")
    validate( need(input$inSelectCorpusAdj, 'Vyberte korpus adjektiv!') )
    selecteddatasrc2 <- input$inSelectCorpusAdj
    
    fnReadData2(selecteddatasrc2)
    if (input$searchLemma2 %in% clemmas2) { fnChangeLemma2(input$searchLemma2) }
    else {
      fnChangeLemma2(defaultLemma2)
    }
    urlkontext <<- paste0("<a href=\"https://kontext.korpus.cz/first?corpname=", selecteddatasrc2, "&queryselector=cqlrow&default_attr=word&cql=")
    # reload table
    fnResetTblFilters2()
  })
  
  # Reaction to changed lemma button  (lemma in pure text box)
  observeEvent(input$changeLemma, {
    print("observeEvent input$changeLemma")
    selectedData <- dfall[ which(dfall$lemma == input$searchLemma), ]

    # Try to change Lemma
    if (nrow(selectedData) > 0) {
      if (selectedData['FQ'] > 100) {
        sellemma <- input$searchLemma
        sellemmar$lemma <- sellemma
        sellemmar$data <- dfall[ which(dfall$lemma == sellemma), ]
        sellemmar$datarod <- dfallrod[ which(dfallrod$lemma == sellemma), ]
        output$textErrorLemma <- renderText('')
      } else {
        output$textErrorLemma <- renderText('Chyba: lemma s malou frekvencí (FQ < 100)')
      }

    } else {
      output$textErrorLemma <- renderText('Chyba:  lemma nenalezeno')
    }
    return()
  })
  
  observeEvent(input$changeLemma2, {
    print("observeEvent input$changeLemma 2")
    selectedData2 <- dfall2[ which(dfall2$lemma == input$searchLemma2), ]
    
    # Try to change Lemma
    if (nrow(selectedData2) > 0) {
      if (selectedData2['FQ'] >= 100) {
        sellemma2 <- input$searchLemma2
        sellemmar2$lemma <- sellemma2
        sellemmar2$data <- dfall2[ which(dfall2$lemma == sellemma2), ]
        sellemmar2$datarod <- dfallrod2[ which(dfallrod2$lemma == sellemma2), ]
        output$textErrorLemma <- renderText('')
        
        selectedData2 <- dfall2[ which(dfall2$lemma == sellemma2), ]
        if (selectedData2['FQ'] >= 100000) {
          output$pokus <- renderText({ "   ****" })
        } else if (selectedData2['FQ'] >= 10000) {
          output$pokus <- renderText({ "   ****" })
        } else if (selectedData2['FQ'] >= 1000) {
          output$pokus <- renderText({ "   ***" })
        } else if (selectedData2['FQ'] >= 100) {
          output$pokus <- renderText({ "   **" })
        } else if (selectedData2['FQ'] >= 10) {
          output$pokus <- renderText({ "   *" })
        } else {
          output$pokus <- renderText({ "   " })
        }

      } else {
        output$textErrorLemma <- renderText('Chyba: lemma2 s malou frekvencí (FQ < 100)')
      }
      
    } else {
      output$textErrorLemma <- renderText('Chyba:  lemma2 nenalezeno')
    }
    return()
  })
  
  observeEvent(input$changeLemma3, {
    print("observeEvent input$changeLemma3 - Verb")
    selectedData3 <- dfall3[ which(dfall3$lemma == input$searchLemma3), ]
    
    # Try to change Lemma
    if (nrow(selectedData3) > 0) {
      if (selectedData3['FQ'] >= 100) {
        sellemma3 <- input$searchLemma3
        sellemmar3$lemma <- sellemma3
        sellemmar3$data <- dfall3[ which(dfall3$lemma == sellemma3), ]
        sellemmar3$datarod <- dfallrod3[ which(dfallrod3$lemma == sellemma3), ]
        output$textErrorLemma <- renderText('')
        
        selectedData3 <- dfall3[ which(dfall3$lemma == sellemma3), ]
        if (selectedData3['FQ'] >= 100000) {
          output$pokus <- renderText({ "   ****" })
        } else if (selectedData3['FQ'] >= 10000) {
          output$pokus <- renderText({ "   ****" })
        } else if (selectedData3['FQ'] >= 1000) {
          output$pokus <- renderText({ "   ***" })
        } else if (selectedData3['FQ'] >= 100) {
          output$pokus <- renderText({ "   **" })
        } else if (selectedData3['FQ'] >= 10) {
          output$pokus <- renderText({ "   *" })
        } else {
          output$pokus <- renderText({ "   " })
        }
        
      } else {
        output$textErrorLemma <- renderText('Chyba: lemma3 s malou frekvencí (FQ < 100)')
      }
      
    } else {
      output$textErrorLemma <- renderText('Chyba:  lemma3 nenalezeno')
    }
    return()
  })

    # Reaction to dev resetTblFilters button - not used
  # observeEvent(input$resetTblFilters, {
  #   proxy1 <- DT::dataTableProxy('data1')
  #   DT::replaceData(proxy1, as.data.frame(dfallrod))
  #   clearSearch(proxy1)
  # 
  #   sellemma <- sellemmar$lemma
  #   selrod <- sellemmar$datarod['rod'][, ]
  # 
  #   cql <- paste0(URLencode("[lemma=\""), URLencode(toString(sellemma)), URLencode("\" "),
  #                 "%26", URLencode(" tag=\".."), URLencode(toString(selrod)), URLencode(".*\"]"))
  #   href <- paste0(urlkontext, cql, "\" target=\"_blank\">Zobrazit výskyty</a>")
  #   cqlhtml <- paste0("[lemma=\"", toString(sellemma), "\"", " & ", "tag=\"..", toString(selrod), ".*\"]")
  #   html(id = "data1sel", paste0(href, " v korpusu výsledky CQL dotazu ", cqlhtml))
  # 
  #   return()
  # })

  # / Buttons
  ################################################################################

  # # Extract outliers from dataframe - not used
  # fnGetOutliers1 <- function(dataframe) {
  #   dataframe %>%
  #     select_if(is.numeric) %>%
  #     map(~ boxplot.stats(.x)$out)
  # }
  # 
  
  # Extract outliers from dataframe
  fnGetOutliers <- function(dataframe) {
    collen = ncol(dataframe) -1
    #print(dataframe[2:collen])
    #print(collen)
    #print(class(dataframe$lemma))
    #print(class(dataframe$singulars))
    #print(class(dataframe$FQ))
    result <- do.call("cbind",lapply(dataframe[2:collen],function(x) boxplot.stats(x)$stats))
    row.names(result) <- c("lower whisker", "lower hinge", "median", "upper hinge", "upper whisker")
    result
  }


  ######################################################################
  # INIT - PART 2

  fnReadData(selecteddatasrc)
  fnReadData2(selecteddatasrc2)
  fnReadData3(selecteddatasrc2)
  
  dfcisla = dfall[ ,c('lemma', 'FQ')]
  dfcisla$singulars<-rowSums(dfall[,singulars])
  dfcisla$plurals<-rowSums(dfall[,plurals])
  dfcislamelt <- melt(data = dfcisla, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

  dfcisla2 = dfall2[ ,c('lemma', 'FQ')]
  dfcisla2$singulars<-rowSums(dfall2[,singulars])
  dfcisla2$plurals<-rowSums(dfall2[,plurals])
  dfcislamelt2 <- melt(data = dfcisla2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

  dfcisla3 = dfall3[ ,c('lemma', 'FQ')]
  dfcisla3$singulars<-rowSums(dfall3[,singulars3])
  dfcisla3$plurals<-rowSums(dfall3[,plurals3])
  dfcislamelt3 <- melt(data = dfcisla3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

  # Definitions of lemma - related variables
  sellemma <- defaultLemma
  sellemmar <- reactiveValues(
    lemma = defaultLemma,
    data = dfall[ which(dfall$lemma == sellemma), ],
    datarod = dfallrod[ which(dfallrod$lemma == sellemma), ]
  )
  dfsel <- dfall[ which(dfall$lemma == sellemma), ]

  sellemma2 <- defaultLemma2
  sellemmar2 <- reactiveValues(
    lemma2 = defaultLemma2,
    data2 = dfall2[ which(dfall2$lemma == sellemma2), ],
    datarod2 = dfallrod2[ which(dfallrod2$lemma == sellemma2), ]
  )
  dfsel2 <- dfall2[ which(dfall2$lemma == sellemma2), ]
  
  sellemma3 <- defaultLemma3
  sellemmar3 <- reactiveValues(
    lemma3 = defaultLemma3,
    data3 = dfall3[ which(dfall3$lemma == sellemma3), ],
    datarod3 = dfallrod3[ which(dfallrod3$lemma == sellemma3), ]
  )
  dfsel3 <- dfall3[ which(dfall3$lemma == sellemma3), ]

    # Load lists
  updateSelectizeInput(session = session, inputId = 'inSelectLemma', choices = clemmas, server = TRUE, selected=defaultLemma)
  updateSelectizeInput(session = session, inputId = 'inSelectCorpus', choices = cdatasrc, server = TRUE, selected='syn2015')

  updateSelectizeInput(session = session, inputId = 'inSelectLemmaAdj', choices = clemmas2, server = TRUE, selected=defaultLemma2)
  updateSelectizeInput(session = session, inputId = 'inSelectCorpusAdj', choices = cdatasrc2, server = TRUE, selected='syn_v11_15_20')

  updateSelectizeInput(session = session, inputId = 'inSelectLemmaVerb', choices = clemmas3, server = TRUE, selected=defaultLemma3)
  updateSelectizeInput(session = session, inputId = 'inSelectCorpusVerb', choices = cdatasrc2, server = TRUE, selected='syn_v11_15_20')
  
  # /INIT - PART 2
  ######################################################################


  ###################################
  #             CHARTS              #
  ###################################

  #------------------------------------
  # Plot boxplot chart cisla - Číslo
  output$plotBoxplotCisla<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisla --") }
    validate( need(sellemmar$lemma, 'Vyberte lemma!') )

    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    #dfcislalmelt vs. dfcislamelt
    dfLemma <- dfall[ which(dfall$lemma == sellemmar$lemma), ]
    dfcislal = dfLemma[ ,c('lemma', 'FQ')]
    dfcislal$singulars<-rowSums(dfLemma[,singulars])
    dfcislal$plurals<-rowSums(dfLemma[,plurals])
    dfcislalmelt <- melt(data = dfcislal, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

    # Lemma
    dfLemmaSP = dfallSP[ which(dfallSP$lemma == sellemmar$lemma), ]
    dfcislaSPl = dfLemmaSP[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt <- melt(data = dfcislaSPl, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

    # TODO: data
    dfcislaSPX = dfallSP[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPXmelt <- melt(data = dfcislaSPX, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

    dfcislaSPlmelt$variable <- factor(dfcislaSPlmelt$variable,
                                      levels=c("singulars", "plurals"),
                                      labels=c(i18n$t("singular"), i18n$t("plural")))
    dfcislaSPXmelt$variable <- factor(dfcislaSPXmelt$variable,
                                      levels=c("singulars", "plurals"),
                                      labels=c(i18n$t("singular"), i18n$t("plural")))

    ggplot(dfcislaSPXmelt, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      geom_point(data = dfcislaSPlmelt, aes(x = variable, y  = value, color = lemma), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = dfcislaSPlmelt, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
      theme(aspect.ratio=2/1) + #ggtitle("Boxplot") + xlab("") + ylab("% lemmat / 100") +
      labs(x = "", y = i18n$t("cislo_boxplot_y"), title = i18n$t("cislo_boxplot_title"))
  })

  output$plotBoxplotCislaLegend<-renderPlot({
    if (c(lang) %in% c("en")) ggplot_box_legend_en()
    else ggplot_box_legend_cz()
  })
  
  output$plotBoxplotCisla2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisla 2 --") }
    validate( need(sellemmar2$lemma, 'Vyberte lemma!') )
    
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    #dfcislalmelt vs. dfcislamelt
    dfLemma2 <- dfall2[ which(dfall2$lemma == sellemmar2$lemma), ]
    dfcislal2 = dfLemma2[ ,c('lemma', 'FQ')]
    dfcislal2$singulars<-rowSums(dfLemma2[,singulars])
    dfcislal2$plurals<-rowSums(dfLemma2[,plurals])
    dfcislalmelt2 <- melt(data = dfcislal2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    # Lemma
    dfLemmaSP2 = dfallSP2[ which(dfallSP2$lemma == sellemmar2$lemma), ]
    dfcislaSPl2 = dfLemmaSP2[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt2 <- melt(data = dfcislaSPl2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    # TODO: data
    dfcislaSPX2 = dfallSP2[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPXmelt2 <- melt(data = dfcislaSPX2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    dfcislaSPlmelt2$variable <- factor(dfcislaSPlmelt2$variable,
                                      levels=c("singulars", "plurals"),
                                      labels=c(i18n$t("singular"), i18n$t("plural")))
    dfcislaSPXmelt2$variable <- factor(dfcislaSPXmelt2$variable,
                                      levels=c("singulars", "plurals"),
                                      labels=c(i18n$t("singular"), i18n$t("plural")))
    
    ggplot(dfcislaSPXmelt2, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      geom_point(data = dfcislaSPlmelt2, aes(x = variable, y  = value, color = lemma2), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = dfcislaSPlmelt2, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
      theme(aspect.ratio=2/1) + #ggtitle("Boxplot") + xlab("") + ylab("% lemmat / 100") +
      labs(x = "", y = i18n$t("cislo_boxplot_y"), title = i18n$t("cislo_boxplot_title2"))
  })

  output$plotBoxplotCisla3<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisla 3 Verb --") }
    validate( need(sellemmar3$lemma, 'Vyberte lemma!') )
    
    tmp <- input$changeLemma3; tmp <- input$changeCorpus3 # <- make chart reactive to buttons
    
    #dfcislalmelt vs. dfcislamelt
    dfLemma3 <- dfall3[ which(dfall3$lemma == sellemmar3$lemma), ]
    dfcislal3 = dfLemma3[ ,c('lemma', 'FQ')]
    dfcislal3$singulars<-rowSums(dfLemma3[,singulars3])
    dfcislal3$plurals<-rowSums(dfLemma3[,plurals3])
    dfcislalmelt3 <- melt(data = dfcislal3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    # Lemma
    dfLemmaSP3 = dfallSP3[ which(dfallSP3$lemma == sellemmar3$lemma), ]
    dfcislaSPl3 = dfLemmaSP3[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt3 <- melt(data = dfcislaSPl3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    # TODO: data
    dfcislaSPX3 = dfallSP3[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPXmelt3 <- melt(data = dfcislaSPX3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    
    dfcislaSPlmelt3$variable <- factor(dfcislaSPlmelt3$variable,
                                       levels=c("singulars", "plurals"),
                                       labels=c(i18n$t("singular"), i18n$t("plural")))
    dfcislaSPXmelt3$variable <- factor(dfcislaSPXmelt3$variable,
                                       levels=c("singulars", "plurals"),
                                       labels=c(i18n$t("singular"), i18n$t("plural")))
    
    ggplot(dfcislaSPXmelt3, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      geom_point(data = dfcislaSPlmelt3, aes(x = variable, y  = value, color = lemma3), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = dfcislaSPlmelt3, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
      theme(aspect.ratio=2/1) + #ggtitle("Boxplot") + xlab("") + ylab("% lemmat / 100") +
      labs(x = "", y = i18n$t("cislo_boxplot_y3"), title = i18n$t("cislo_boxplot_title3"))
  })
  
  output$plotBoxplotCislaLegend<-renderPlot({
    if (c(lang) %in% c("en")) ggplot_box_legend_en()
    else ggplot_box_legend_cz()
  })

  output$plotBoxplotCislopadLegend<-renderPlot({
    if (c(lang) %in% c("en")) ggplot_box_legend_en()
    else ggplot_box_legend_cz()
  })

  #------------------------------------
  # Plot boxplot chart - ČísloPád

  output$plotBoxplotCisloPad<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisloPad --") }

    validate( need(sellemmar$lemma, 'Vyberte lemma!') )
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    dfl <- sellemmar$data
    drops <- c("lemma","FQ")
    dfdrops <- dfl[ , !(names(dfl) %in% drops)]
    final_df <- as.data.frame(t(dfdrops))
    names(final_df)[1] <- "value"
    final_df <- cbind(cislopad = rownames(final_df), final_df)
    rownames(final_df) <- 1:nrow(final_df)

    #print(str(dfx))
    #dfxs1 <- dfx[dfx$cislopad == "S1", ]
    #print(str(dfxs1))
    
    #tmp2 <- quantile(dfx[dfx$cislopad == "S1", ]$value, probs = c(0.00, .25, .5, .75, 1.00))
    #print(tmp2)
    
    #tmp2 <- quantile(dfx[dfx$cislopad == "S2", ]$value, probs = c(0.00, .25, .5, .75, 1.00))
    #print(tmp2)
    
    #mytitle <- expression(paste(italic("okraj ") ,  "‘edge, margin’"))
    #mytitle <- expression(paste(italic("tvorstvo ") ,  "‘creatures, creation’"))
    #mytitle <- expression(paste(italic("rámec ") ,  "‘framework, context’"))
    
    
    if (c(lang) %in% c("en")) mylabelsx <- c(' Nom ', ' Gen ', ' Dat ', ' Acc ', ' Voc ', ' Loc ', ' Ins ', 'Nom', 'Gen', 'Dat', 'Acc', 'Voc', 'Loc', 'Ins')
    else mylabelsx <- c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7')
    
    
    #dfsub %>% ggplot(aes(x=cislopad, y=value)) +
    dfx %>% ggplot(aes(x=cislopad, y=value)) +
      scale_x_discrete(labels= mylabelsx) + 
      stat_boxplot(geom = "errorbar", width = 0.3) +
      geom_boxplot() +
      geom_point(data = final_df, aes(x = cislopad, y  = value, color = lemma), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = final_df, aes(x = cislopad, y  = value), shape = 1, size = 4, alpha = 1.0, color='black')+
      #geom_point(data = dfx, aes(x = cislopad, y  = value * 100, color = lemma), size = 4, alpha = 1.0, color='cornflowerblue')+
      #geom_point(data = dfx, aes(x = cislopad, y  = value * 100), shape = 1, size = 4, alpha = 1.0, color='black')+
      theme(aspect.ratio=1/1.8, plot.title = element_text(hjust = 0.5)) +
      #theme(aspect.ratio=1/1.8)  + 
      labs(x = i18n$t("cislopad_boxplot_x"), y = i18n$t("cislopad_boxplot_y"), title = i18n$t("cislopad_boxplot_title"))
      #labs(x = "< SG | PL >", y = "%", title = "okraj" + italic("bacteria X") +  "‘edge, margin’") +
      #labs(x = "< SG | PL >", y = "%", title = mytitle) +
  })


  output$plotBoxplotCisloPad2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisloPad 2 --") }
    
    validate( need(sellemmar2$lemma, 'Vyberte lemma!') )
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfl2 <- sellemmar2$data
    drops2 <- c("lemma","FQ")
    dfdrops2 <- dfl2[ , !(names(dfl2) %in% drops2)]
    final_df2 <- as.data.frame(t(dfdrops2))
    names(final_df2)[1] <- "value"
    final_df2 <- cbind(cislopad2 = rownames(final_df2), final_df2)
    rownames(final_df2) <- 1:nrow(final_df2)
    
    if (c(lang) %in% c("en")) mylabelsx2 <- c(' Nom ', ' Gen ', ' Dat ', ' Acc ', ' Voc ', ' Loc ', ' Ins ', 'Nom', 'Gen', 'Dat', 'Acc', 'Voc', 'Loc', 'Ins')
    else mylabelsx2 <- c('S1', 'S2', 'S3', 'S4', 'S5', 'S6', 'S7', 'P1', 'P2', 'P3', 'P4', 'P5', 'P6', 'P7')
    
    dfx2 %>% ggplot(aes(x=cislopad2, y=value)) +
      scale_x_discrete(labels= mylabelsx2) + 
      stat_boxplot(geom = "errorbar", width = 0.3) +
      geom_boxplot() +
      geom_point(data = final_df2, aes(x = cislopad2, y  = value, color = lemma2), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = final_df2, aes(x = cislopad2, y  = value), shape = 1, size = 4, alpha = 1.0, color='black')+
      theme(aspect.ratio=1/1.8, plot.title = element_text(hjust = 0.5)) +
      labs(x = i18n$t("cislopad_boxplot_x2"), y = i18n$t("cislopad_boxplot_y2"), title = i18n$t("cislopad_boxplot_title2"))
  })

  output$plotBoxplotCisloOsoba3<-renderPlot({
    if (printDebug) { print("-- plotBoxplotCisloOsoba 3 --") }
    print("-- plotBoxplotCisloOsoba 3 --")
    
    validate( need(sellemmar3$lemma, 'Vyberte lemma!') )
    tmp <- input$changeLemma3; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfl3 <- sellemmar3$data
    drops3 <- c("lemma","O", "FQ")
    dfdrops3 <- dfl3[ , !(names(dfl3) %in% drops3)]
    final_df3 <- as.data.frame(t(dfdrops3))
    names(final_df3)[1] <- "value"
    final_df3 <- cbind(cisloosoba3 = rownames(final_df3), final_df3)
    rownames(final_df3) <- 1:nrow(final_df3)

    if (c(lang) %in% c("en")) mylabelsx3 <- c(' Nom ', ' Gen ', ' Dat ', 'Nom', 'Gen', 'Dat')
    else mylabelsx3 <- c('S1', 'S2', 'S3', 'P1', 'P2', 'P3')
    
    dfx3 %>% ggplot(aes(x=cisloosoba3, y=value)) +
      scale_x_discrete(labels= mylabelsx3) + 
      stat_boxplot(geom = "errorbar", width = 0.3) +
      geom_boxplot() +
      geom_point(data = final_df3, aes(x = cisloosoba3, y  = value, color = lemma3), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = final_df3, aes(x = cisloosoba3, y  = value), shape = 1, size = 4, alpha = 1.0, color='black')+
      theme(aspect.ratio=1/1.8, plot.title = element_text(hjust = 0.5)) +
      labs(x = i18n$t("cisloosoba_boxplot_x3"), y = i18n$t("cisloosoba_boxplot_y3"), title = i18n$t("cisloosoba_boxplot_title3"))
  })
  
  # -------------------------------------------------
  # Plot boxplotrod0 chart  -  RodoČísloPád / Spojené

  output$plotBoxplotRodocislopad1Lemma<-renderPlot({
    if (printDebug) { print("-- plotBoxplotRodocislopad1Lemma --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    # grouped boxplot data
    drops <- c("lemma", 'FQ')
    dfdrops <- dfallrod[ , !(names(dfallrod) %in% drops)]
    dfbpl <- cbind(ID=rownames(dfdrops),  dfdrops)
    dfbpl <- reshape2::melt(dfbpl)

    drops <- c("ID")
    dfbpl <- dfbpl[ , !(names(dfbpl) %in% drops)]
    names(dfbpl)[2] <- "cislopad"

    # lemma data
    dfl <- sellemmar$datarod
    dfdrops <- dfl[ , !(names(dfl) %in% c("lemma","FQ"))]
    dfbpllem <- reshape2::melt(dfdrops, id.vars = c('rod'))
    names(dfbpllem)[2] <- "cislopad"
    drops <- c("variable")
    dfbpllem <- dfbpllem[ , !(names(dfbpllem) %in% drops)]

    lrod = c()
    lcislopad = c()
    lvalue = c()
    addrod <- c("M", "I", "F", "N")
    addvalue <- c(NA, NA, NA, NA)
    for (x in singulars) {
      addcislopad <- c(x, x, x, x)
      lrod <- c(lrod, addrod)
      lcislopad <- c(lcislopad, addcislopad)
      lvalue <- c(lvalue, addvalue)
    }
    for (x in plurals) {
      addcislopad <- c(x, x, x, x)
      lrod <- c(lrod, addrod)
      lcislopad <- c(lcislopad, addcislopad)
      lvalue <- c(lvalue, addvalue)
    }
    zeroList <- list(rod = lrod, cislopad = lcislopad, value = lvalue)

    de <- as.data.frame(zeroList)
    names(de)<-c("rod","cislopad", "value")
    dfbpllem2<- rbind(dfbpllem, de)

    lemma <- sellemmar$lemma

    dfbpl$rod <- factor(dfbpl$rod,levels = c('M','I','F','N'), ordered = TRUE)
    dfbpllem2$rod <- factor(dfbpllem2$rod,levels = c('M','I','F','N'), ordered = TRUE)

    # chart1
    pd = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.0)

    dfbpl %>%
      ggplot (aes(x=cislopad, y=value, fill=rod)) +
      #scale_fill_brewer(palette="Dark2") +
      scale_fill_manual(values=c("#57aB27", "#ea670C", "#e2007a", "#009ee0")) +
      #scale_fill_manual(breaks = c("2", "1", "0.5"), values=c("red", "blue", "green"))
      #stat_boxplot(geom = "errorbar", width = 0.3) +
      geom_boxplot(alpha=0.75, outlier.size = 0.5) +
      geom_point(data = dfbpllem2, aes(group=rod), position = pd, shape=21, size=3, stroke=1.5) + #stroke=1.0
      theme(aspect.ratio=1/2.8, axis.text.x =element_text(size=14, face="bold")) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold", size=20),
        legend.text=element_text(size=16),
        legend.key.size = unit(3,"line"),
        #plot.background = element_rect(fill="red")
        panel.background = element_rect(fill="lightgray"),
        #panel.background = element_rect(fill = "lightgray",
        #                                colour = "lightgray",
        #                                size = 1.0, linetype = "solid") #,
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "gray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'blank', colour = "blue"),
        panel.grid.major.x = element_line(colour = "green", linetype = 'blank')
      ) +
      geom_vline(xintercept = as.numeric(dfbpl$cislopad) - 0.5, size = 3, colour = "white") +
      labs(x = i18n$t("cislopad_boxplot_x"), y = i18n$t("cislopad_boxplot_y"), title = "")
    #coord_flip()
  })

  
  output$plotBoxplotRodocislopad1Lemma2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotRodocislopad1Lemma2 --") }
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    # grouped boxplot data
    drops2 <- c("lemma", 'FQ')
    dfdrops2 <- dfallrod2[ , !(names(dfallrod2) %in% drops2)]
    dfbpl2 <- cbind(ID=rownames(dfdrops2),  dfdrops2)
    dfbpl2 <- reshape2::melt(dfbpl2)
    
    drops2 <- c("ID")
    dfbpl2 <- dfbpl2[ , !(names(dfbpl2) %in% drops2)]
    names(dfbpl2)[2] <- "cislopad2"
    
    # lemma data
    dfl2 <- sellemmar2$datarod
    dfdrops2 <- dfl2[ , !(names(dfl2) %in% c("lemma","FQ"))]
    dfbpllem2 <- reshape2::melt(dfdrops2, id.vars = c('rod'))
    names(dfbpllem2)[2] <- "cislopad2"
    drops2 <- c("variable")
    dfbpllem2 <- dfbpllem2[ , !(names(dfbpllem2) %in% drops2)]
    
    #print(dfl2)
    
    lrod2 = c()
    lcislopad2 = c()
    lvalue2 = c()
    addrod2 <- c("M", "I", "F", "N")
    addvalue2 <- c(NA, NA, NA, NA)
    for (x in singulars) {
      addcislopad2 <- c(x, x, x, x)
      lrod <- c(lrod2, addrod2)
      lcislopad2 <- c(lcislopad2, addcislopad2)
      lvalue2 <- c(lvalue2, addvalue2)
    }
    for (x in plurals) {
      addcislopad2 <- c(x, x, x, x)
      lrod2 <- c(lrod2, addrod2)
      lcislopad2 <- c(lcislopad2, addcislopad2)
      lvalue2 <- c(lvalue2, addvalue2)
    }
    zeroList2 <- list(rod2 = lrod2, cislopad2 = lcislopad2, value2 = lvalue2)
    
    de2 <- as.data.frame(zeroList2)
    names(de2)<-c("rod","cislopad2", "value")
    dfbpllem2<- rbind(dfbpllem2, de2)
    
    lemma2 <- sellemmar2$lemma
    
    dfbpl2$rod <- factor(dfbpl2$rod,levels = c('M','I','F','N'), ordered = TRUE)
    dfbpllem2$rod <- factor(dfbpllem2$rod,levels = c('M','I','F','N'), ordered = TRUE)
    
    # chart1
    pd2 = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.0)
    
    dfbpl2 %>%
      ggplot (aes(x=cislopad2, y=value, fill=rod)) +
      scale_fill_manual(values=c("#57aB27", "#ea670C", "#e2007a", "#009ee0")) +
      geom_boxplot(alpha=0.75, outlier.size = 0.5) +
      geom_point(data = dfbpllem2, aes(group=rod), position = pd2, shape=21, size=3, stroke=1.5) + #stroke=1.0
      theme(aspect.ratio=1/2.8, axis.text.x =element_text(size=14, face="bold")) +
      theme(
        legend.position = "top",
        legend.title = element_text(face = "bold", size=20),
        legend.text=element_text(size=16),
        legend.key.size = unit(3,"line"),
        panel.background = element_rect(fill="lightgray"),
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "gray"),
        panel.grid.minor.y = element_line(size = 0.25, linetype = 'blank', colour = "blue"),
        panel.grid.major.x = element_line(colour = "green", linetype = 'blank')
      ) +
      geom_vline(xintercept = as.numeric(dfbpl2$cislopad2) - 0.5, size = 3, colour = "white") +
      labs(x = i18n$t("cislopad_boxplot_x2"), y = i18n$t("cislopad_boxplot_y2"), title = "")
  })
  
  # ---------------------------------------------------
  # Plot boxplotrod2 chart  -  RodoČísloPád / Rozdělené
  output$plotBoxplotRodocislopad2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotRodocislopad2 --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons
    # grouped boxplot data
    drops <- c("lemma", 'FQ')
    dfdrops <- dfallrod[ , !(names(dfallrod) %in% drops)]
    dfbpl <- cbind(ID=rownames(dfdrops),  dfdrops)
    dfbpl <- reshape2::melt(dfbpl)
    drops <- c("ID")
    dfbpl <- dfbpl[ , !(names(dfbpl) %in% drops)]
    names(dfbpl)[2] <- "cislopad"

    #dfbpl$rod <- factor(dfbpl$rod, levels = c("-", "F", "I", "M", "N"))
    dfbpl$rod <- factor(dfbpl$rod, levels = c("-", "M", "I", "F", "N"))

    # lemma data
    dfl <- sellemmar$datarod
    dfdrops <- dfl[ , !(names(dfl) %in% c("lemma","FQ"))]
    lemma <- sellemmar$lemma

    # chart2
    dfbpllem <- reshape2::melt(dfdrops, id.vars = c('rod'))
    names(dfbpllem)[2] <- "cislopad"

    #dfbpllem$rod <- factor(dfbpllem$rod, levels = c("-", "F", "I", "M", "N"))
    dfbpllem$rod <- factor(dfbpllem$rod, levels = c("-", "M", "I", "F", "N"))

    p <- ggplot(data = dfbpl, aes(x = cislopad, y = value, fill=rod)) +
         scale_fill_manual(values=c("#57aB27", "#ea670C", "#e2007a", "#009ee0")) +
         stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +

         geom_point(data = dfbpllem, aes(x = cislopad, y  = value, color = lemma), size = 4, alpha = 1.0) +
         geom_point(data = dfbpllem, aes(x = cislopad, y  = value), shape = 1, size = 4, color='black')
    p + facet_wrap(~interaction(rod), scales='free', ncol=2) +
        theme(strip.text = element_text(size=20)) +
    labs(x = "", y = "", title = "")

  })

  output$plotBoxplotRodocislopad22<-renderPlot({
    if (printDebug) { print("-- plotBoxplotRodocislopad22 --") }
    tmp2 <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    # grouped boxplot data
    drops2 <- c("lemma", 'FQ')
    dfdrops2 <- dfallrod2[ , !(names(dfallrod2) %in% drops2)]
    dfbpl2 <- cbind(ID=rownames(dfdrops2),  dfdrops2)
    dfbpl2 <- reshape2::melt(dfbpl2)
    drops2 <- c("ID")
    dfbpl2 <- dfbpl2[ , !(names(dfbpl2) %in% drops2)]
    names(dfbpl2)[2] <- "cislopad2"
    
    #dfbpl$rod <- factor(dfbpl$rod, levels = c("-", "F", "I", "M", "N"))
    dfbpl2$rod <- factor(dfbpl2$rod, levels = c("-", "M", "I", "F", "N"))
    
    # lemma data
    dfl2 <- sellemmar2$datarod
    dfdrops2 <- dfl2[ , !(names(dfl2) %in% c("lemma","FQ"))]
    lemma2 <- sellemmar2$lemma
    
    # chart2
    dfbpllem2 <- reshape2::melt(dfdrops2, id.vars = c('rod'))
    names(dfbpllem2)[2] <- "cislopad2"
    
    #dfbpllem$rod <- factor(dfbpllem$rod, levels = c("-", "F", "I", "M", "N"))
    dfbpllem2$rod <- factor(dfbpllem2$rod, levels = c("-", "M", "I", "F", "N"))
    
    p <- ggplot(data = dfbpl2, aes(x = cislopad2, y = value, fill=rod)) +
      scale_fill_manual(values=c("#57aB27", "#ea670C", "#e2007a", "#009ee0")) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      
      geom_point(data = dfbpllem2, aes(x = cislopad2, y  = value, color = lemma2), size = 4, alpha = 1.0) +
      geom_point(data = dfbpllem2, aes(x = cislopad2, y  = value), shape = 1, size = 4, color='black')
    p + facet_wrap(~interaction(rod), scales='free', ncol=2) +
      theme(strip.text = element_text(size=20)) +
      labs(x = "", y = "", title = "")
    
  })

  # ------------------------------------              ROD          -----------------------------------------------
  output$plotBoxplotRodLemma2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotRodLemma2 predelat z singular - plural na rody --") }
    validate( need(sellemmar2$lemma, 'Vyberte lemma!') )
    
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfLemma2 <- dfgender100p2[ which(dfgender100p2$lemma == sellemmar2$lemma), ]
    dfcislal2 = dfgender100p2[ ,c('lemma', 'FQ')]
    dfcislal2$gender<-rowSums(dfLemma2[,c('M', 'I', 'F','N')])
    # Lemma
    dfLemmaG2 = dfgender100p2[ which(dfgender100p2$lemma == sellemmar2$lemma), ]
    dfcislaGl2 = dfLemmaG2[ ,c('lemma', 'M', 'I', 'F', 'N')]
    dfcislaGlmelt2 <- melt(data = dfcislaGl2, id.vars = c('lemma'),  measure.vars = c('M', 'I', 'F', 'N'))
    
    # TODO: data
    dfcislaGX2 = dfgender100p2[ ,c('lemma', 'M', 'I', 'F', 'N')]
    dfcislaGXmelt2 <- melt(data = dfcislaGX2, id.vars = c('lemma'),  measure.vars = c('M', 'I', 'F', 'N'))
    
    dfcislaGlmelt2$variable <- factor(dfcislaGlmelt2$variable,
                                       levels=c('M', 'I', 'F', 'N'),
                                       labels=c(i18n$t("M"), i18n$t("I"), i18n$t("F"), i18n$t("N")))
    dfcislaGXmelt2$variable <- factor(dfcislaGXmelt2$variable,
                                       levels=c('M', 'I', 'F', 'N'),
                                       labels=c(i18n$t("M"), i18n$t("I"), i18n$t("F"), i18n$t("N")))
    ggplot(dfcislaGXmelt2, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      scale_x_discrete(labels=c('M', 'I', 'F', 'N')) +
      geom_point(data = dfcislaGlmelt2, aes(x = variable, y  = value, color = lemma2), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = dfcislaGlmelt2, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
      theme(aspect.ratio=2/1) + #ggtitle("Boxplot") + xlab("") + ylab("% lemmat / 100") +
      labs(x = "", y = i18n$t("cislo_boxplot_y2"), title = i18n$t("cislo_boxplot_title2"))
  })
  
  # ------------------------------------              DEGREE          -----------------------------------------------
  output$plotBoxplotDegreeLemma2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotDegreeLemma2 --") }
    validate( need(sellemmar2$lemma, 'Vyberte lemma!') )
    
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfLemma2 <- dfdegree100p2[ which(dfdegree100p2$lemma == sellemmar2$lemma), ]
    dfcislal2 = dfdegree100p2[ ,c('lemma', 'FQ')]
    dfcislal2$deg<-rowSums(dfLemma2[,c('D1', 'D2','D3')])
    # Lemma
    dfLemmaD2 = dfdegree100p2[ which(dfdegree100p2$lemma == sellemmar2$lemma), ]
    dfcislaDl2 = dfLemmaD2[ ,c('lemma', 'D1', 'D2', 'D3')]
    dfcislaDlmelt2 <- melt(data = dfcislaDl2, id.vars = c('lemma'),  measure.vars = c('D1', 'D2', 'D3'))
    
    # TODO: data
    dfcislaDX2 = dfdegree100p2[ ,c('lemma', 'D1', 'D2', 'D3')]
    dfcislaDXmelt2 <- melt(data = dfcislaDX2, id.vars = c('lemma'),  measure.vars = c('D1', 'D2', 'D3'))
    
    dfcislaDlmelt2$variable <- factor(dfcislaDlmelt2$variable,
                                      levels=c('D1', 'D2', 'D3'),
                                      labels=c(i18n$t("D1"), i18n$t("D2"), i18n$t("D3")))
    dfcislaDXmelt2$variable <- factor(dfcislaDXmelt2$variable,
                                      levels=c('D1', 'D2', 'D3'),
                                      labels=c(i18n$t("D1"), i18n$t("D2"), i18n$t("D3")))
    
    ggplot(dfcislaDXmelt2, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      scale_x_discrete(labels=c('1', '2', '3')) +
      geom_point(data = dfcislaDlmelt2, aes(x = variable, y  = value, color = lemma2), size = 4, alpha = 1.0, color='cornflowerblue')+
      geom_point(data = dfcislaDlmelt2, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
      theme(aspect.ratio=2/1) +
      labs(x = "", y = i18n$t("cislo_boxplot_y2"), title = i18n$t("cislo_boxplot_title2"))
  })
  
  # ------------------------------------              NEGATION          -----------------------------------------------
  output$plotBoxplotNegationLemma2<-renderPlot({
    if (printDebug) { print("-- plotBoxplotNegationLemma2 --") }
    validate( need(sellemmar2$lemma, 'Vyberte lemma!') )
    
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfLemma2 <- dfnegation100p2[ which(dfnegation100p2$lemma == sellemmar2$lemma), ]
    dfcislal2 = dfnegation100p2[ ,c('lemma', 'FQ')]
    dfcislal2$neg<-rowSums(dfLemma2[,c('A', 'N')])
    # Lemma
    dfLemmaN2 = dfnegation100p2[ which(dfnegation100p2$lemma == sellemmar2$lemma), ]
    dfcislaNl2 = dfLemmaN2[ ,c('lemma', 'A', 'N')]
    dfcislaNlmelt2 <- melt(data = dfcislaNl2, id.vars = c('lemma'),  measure.vars = c('A', 'N'))
    
    # TODO: data
    dfcislaNX2 = dfnegation100p2[ ,c('lemma', 'A', 'N')]
    dfcislaNXmelt2 <- melt(data = dfcislaNX2, id.vars = c('lemma'),  measure.vars = c('A', 'N'))
    
    dfcislaNlmelt2$variable <- factor(dfcislaNlmelt2$variable,
                                      levels=c('A', 'N'),
                                      labels=c(i18n$t("A"), i18n$t("N")))
    dfcislaNXmelt2$variable <- factor(dfcislaNXmelt2$variable,
                                      levels=c('A', 'N'),
                                      labels=c(i18n$t("A"), i18n$t("N")))
    print(dfLemmaN2$A)
    print(dfLemmaN2$N)

    sA1 = dfLemmaN2$A == 1
    sA0 = dfLemmaN2$A == 0
    sN1 = dfLemmaN2$N == 1
    sN0 = dfLemmaN2$N == 0
    
    # ggplot(dfcislaNXmelt2, aes(x=variable, y=value)) +
    #   stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
    #   scale_x_discrete(labels=c('A', 'N')) +
    #   geom_point(data = dfcislaNlmelt2, aes(x = variable, y  = value, color = lemma2), size = 4, alpha = 1.0, color='cornflowerblue')+
    #   geom_point(data = dfcislaNlmelt2, aes(x = variable, y  = value), shape = 1, size = 4, alpha = 1.0, color='black') +
    #   geom_point(aes(x=2,y=0), shape = 21, color="black", fill="red", size = 4) +
    #   theme(aspect.ratio=2/1) + #ggtitle("Boxplot") + xlab("") + ylab("% lemmat / 100") +
    #   labs(x = "", y = i18n$t("cislo_boxplot_y2"), title = i18n$t("cislo_boxplot_title2"))
    Switch=T
    ggplot(dfcislaNXmelt2, aes(x=variable, y=value)) +
      stat_boxplot(geom = "errorbar", width = 0.3) + geom_boxplot() +
      scale_x_discrete(labels=c('A', 'N')) +
      geom_point(data = dfcislaNlmelt2, aes(x = variable, y  = value), shape = 21, size = 4, alpha = 1.0, color='black', fill="cornflowerblue") +
      geom_point(aes(x=1,y=1), shape = 21, size = 4, alpha = case_when(sA1 ~ 1.0, TRUE ~ 0.0), color="black", fill=case_when(sA1 ~ "red")) +
      geom_point(aes(x=1,y=0), shape = 21, size = 4, alpha = case_when(sA0 ~ 1.0, TRUE ~ 0.0), color="black", fill=case_when(sA0 ~ "red")) +
      geom_point(aes(x=2,y=1), shape = 21, size = 4, alpha = case_when(sN1 ~ 1.0, TRUE ~ 0.0), color="black", fill=case_when(sN1 ~ "red")) +
      geom_point(aes(x=2,y=0), shape = 21, size = 4, alpha = case_when(sN0 ~ 1.0, TRUE ~ 0.0), color="black", fill=case_when(sN0 ~ "red")) +
      theme(aspect.ratio=2/1) +
      labs(x = "", y = i18n$t("cislo_boxplot_y2"), title = i18n$t("cislo_boxplot_title2"))
      #if (Switch) print("nulaaaaa")
  })
  
  # -----------------------------------------------------------------------------------------------
  # Plot Histogram
  output$plotHistogram<-renderPlot({
    if (printDebug) { print("-- plotHistogram --") }
    validate( need(sellemmar$lemma, 'Vyberte lemma!') )
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    df2 <- dfx[ which(dfx$cislopad==input$prasopad), ]

    hp <- qplot(x=value, data = df2, fill=..count.., geom="histogram", binwidth=0.05)
    ggp_build <- ggplot_build(hp)
    maxy <- max(ggp_build[["data"]][[1]][["count"]])

    columnname <- input$prasopad
    posx = as.numeric(sellemmar$data[columnname])

    # plot arrow with lemma
    hp + scale_fill_gradient(low="blue", high="red") +
      annotate("segment", x=posx, xend=posx, y=posx - maxy/10, yend=posx, arrow=arrow(), color = "black") +
      annotate("text", label = sellemmar$lemma, x = posx, y = posx - maxy/10 - maxy/20, size = 5, colour = "black")

  })

  # Plot Histogram for simple Cisla
  output$plotHistogramCisla<-renderPlot({
    if (printDebug) { print("-- plotHistogramCisla --") }

    validate( need(sellemmar$lemma, 'Vyberte lemma!') )
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    dfLemmaSP = dfallSP[ which(dfallSP$lemma == sellemmar$lemma), ]
    dfcislaSPl = dfLemmaSP[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt <- melt(data = dfcislaSPl, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

    dfcislalmelt <- melt(data = dfcisla, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))

    hp <- qplot(x=value, data = dfcislamelt, fill=..count.., geom="histogram", binwidth=histbw)

    ggp_build <- ggplot_build(hp)

    if (sellemmar$lemma %in% clemmas) {
      print("plotHistogramCisla lemma je v seznamu")
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt[ which(dfcislaSPlmelt$lemma == sellemmar$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])

      print(sellemmar$lemma)
      print(zzz$value[1])
      print(zzz$value[2])

      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxS, xend=posxS, y=posxS - maxy/10, yend=posxS, arrow=arrow(), color = "black") +
        annotate("text", label = paste(sellemmar$lemma, " S"), x = posxS, y = posxS - maxy/10 - maxy/20, size = 5, colour = "black") +
        annotate("segment", x=posxP, xend=posxP, y=posxP - maxy/10, yend=posxP, arrow=arrow(), color = "black") +
        annotate("text", label = paste(sellemmar$lemma, " P"), x = posxP, y = posxP - maxy/10 - maxy/20, size = 5, colour = "black") +
        ggtitle("Histogram - Singulars / Plurals")
    } else {
      print("plotHistogramCisla lemma neni v seznamu")
      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        ggtitle("Histogram - Singulars / Plurals")
    }
  })

  # Plot Histogram for simple Cisla singular
  output$plotHistogramCislaS<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaS --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    dfLemmaSP = dfallSP[ which(dfallSP$lemma == sellemmar$lemma), ]
    dfcislaSPl = dfLemmaSP[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt <- melt(data = dfcislaSPl, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalsmelt <- melt(data = dfcisla, id.vars = c('lemma'),  measure.vars = c('singulars'))

    hp <- qplot(x=value, data = dfcislalsmelt, fill=..count.., geom="histogram", binwidth=histbw)
    #hp + scale_fill_gradient(low="blue", high="red") + ggtitle("Histogram - Singulars")
    #hp2 <- hp + geom_segment(aes(x = as.numeric(.75), y = as.numeric(0), xend = as.numeric(.75), yend = as.numeric(100)), linetype="dashed",  color="green")
    ggp_build <- ggplot_build(hp)

    if (sellemmar$lemma %in% clemmas) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt[ which(dfcislaSPlmelt$lemma == sellemmar$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])

      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxS, xend=posxS, y=posxS - maxy/10, yend=posxS, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar$lemma, " S"), x = posxS, y = posxS - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS_title")) + xlab(i18n$t("cislo_histogram_x")) +
        annotate("segment", x=posxS, xend=posxS, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))

    } else {
      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS_title")) + xlab(i18n$t("cislo_histogram_x"))
    }

  })

  # Plot Histogram for simple Cisla plural
  output$plotHistogramCislaP<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaP --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    dfLemmaSP = dfallSP[ which(dfallSP$lemma == sellemmar$lemma), ]
    dfcislaSPl = dfLemmaSP[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt <- melt(data = dfcislaSPl, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalpmelt <- melt(data = dfcisla, id.vars = c('lemma'),  measure.vars = c('plurals'))

    hp <- qplot(x=value, data = dfcislalpmelt, fill=..count.., geom="histogram", binwidth=histbw)

    ggp_build <- ggplot_build(hp)

    if (sellemmar$lemma %in% clemmas) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt[ which(dfcislaSPlmelt$lemma == sellemmar$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])

      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxP, xend=posxP, y=posxP - maxy/10, yend=posxP, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar$lemma, " P"), x = posxP, y = posxP - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP_title")) + xlab(i18n$t("cislo_histogram_x")) +
        annotate("segment", x=posxP, xend=posxP, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))
    } else {
      # plot arrow with lemma
      hp + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP_title")) + xlab(i18n$t("cislo_histogram_x"))
    }

  })

  # Plot Histogram for simple Cisla singular adjectives
  output$plotHistogramCislaS2<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaS2 --") }
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    print("sellemmar$lemma")
    print(sellemmar$lemma)
    print("sellemmar2$lemma")
    print(sellemmar2$lemma)
    
    dfLemmaSP2 = dfallSP2[ which(dfallSP2$lemma == sellemmar2$lemma), ]
    dfcislaSPl2 = dfLemmaSP2[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt2 <- melt(data = dfcislaSPl2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalsmelt2 <- melt(data = dfcisla2, id.vars = c('lemma'),  measure.vars = c('singulars'))
    
    hp2 <- qplot(x=value, data = dfcislalsmelt2, fill=..count.., geom="histogram", binwidth=histbw)
    #hp + scale_fill_gradient(low="blue", high="red") + ggtitle("Histogram - Singulars")
    #hp2 <- hp + geom_segment(aes(x = as.numeric(.75), y = as.numeric(0), xend = as.numeric(.75), yend = as.numeric(100)), linetype="dashed",  color="green")
    ggp_build <- ggplot_build(hp2)
    
    if (sellemmar2$lemma %in% clemmas2) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt2[ which(dfcislaSPlmelt2$lemma == sellemmar2$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])
      
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxS, xend=posxS, y=posxS - maxy/10, yend=posxS, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar2$lemma, " S"), x = posxS, y = posxS - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS2_title")) + xlab(i18n$t("cislo_histogram_x2")) +
        annotate("segment", x=posxS, xend=posxS, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))
      
    } else {
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS2_title")) + xlab(i18n$t("cislo_histogram_x2"))
    }
    
  })
  
  # Plot Histogram for simple Cisla plural adjectives
  output$plotHistogramCislaP2<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaP2 --") }
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfLemmaSP2 = dfallSP2[ which(dfallSP2$lemma == sellemmar2$lemma), ]
    dfcislaSPl2 = dfLemmaSP2[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt2 <- melt(data = dfcislaSPl2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalpmelt2 <- melt(data = dfcisla2, id.vars = c('lemma'),  measure.vars = c('plurals'))
    
    hp2 <- qplot(x=value, data = dfcislalpmelt2, fill=..count.., geom="histogram", binwidth=histbw)
    
    ggp_build <- ggplot_build(hp2)
    
    if (sellemmar2$lemma %in% clemmas2) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt2[ which(dfcislaSPlmelt2$lemma == sellemmar2$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])
      
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxP, xend=posxP, y=posxP - maxy/10, yend=posxP, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar2$lemma, " P"), x = posxP, y = posxP - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP2_title")) + xlab(i18n$t("cislo_histogram_x2")) +
        annotate("segment", x=posxP, xend=posxP, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))
    } else {
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP2_title")) + xlab(i18n$t("cislo_histogram_x2"))
    }
    
  })

  
  
  # Plot Histogram for simple Cisla singular verbs
  output$plotHistogramCislaS3<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaS3 --") }
    tmp <- input$changeLemma3; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    print("sellemmar$lemma")
    print(sellemmar$lemma)
    print("sellemmar2$lemma")
    print(sellemmar3$lemma)
    
    dfLemmaSP3 = dfallSP3[ which(dfallSP3$lemma == sellemmar3$lemma), ]
    dfcislaSPl3 = dfLemmaSP3[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt3 <- melt(data = dfcislaSPl3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalsmelt3 <- melt(data = dfcisla3, id.vars = c('lemma'),  measure.vars = c('singulars'))
    
    hp3 <- qplot(x=value, data = dfcislalsmelt3, fill=..count.., geom="histogram", binwidth=histbw)
    ggp_build <- ggplot_build(hp3)
    
    if (sellemmar3$lemma %in% clemmas3) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt3[ which(dfcislaSPlmelt3$lemma == sellemmar3$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])
      
      # plot arrow with lemma
      hp3 + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxS, xend=posxS, y=posxS - maxy/10, yend=posxS, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar3$lemma, " S"), x = posxS, y = posxS - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS3_title")) + xlab(i18n$t("cislo_histogram_x3")) +
        annotate("segment", x=posxS, xend=posxS, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))
      
    } else {
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramS3_title")) + xlab(i18n$t("cislo_histogram_x3"))
    }
    
  })
  
  # Plot Histogram for simple Cisla plural verbs
  output$plotHistogramCislaP3<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaP3 --") }
    tmp <- input$changeLemma3; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfLemmaSP3 = dfallSP3[ which(dfallSP3$lemma == sellemmar3$lemma), ]
    dfcislaSPl3 = dfLemmaSP3[ ,c('lemma', 'singulars', 'plurals')]
    dfcislaSPlmelt3 <- melt(data = dfcislaSPl3, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    dfcislalpmelt3 <- melt(data = dfcisla3, id.vars = c('lemma'),  measure.vars = c('plurals'))
    
    hp3 <- qplot(x=value, data = dfcislalpmelt3, fill=..count.., geom="histogram", binwidth=histbw)
    
    ggp_build <- ggplot_build(hp3)
    
    if (sellemmar3$lemma %in% clemmas3) {
      maxy <- max(ggp_build[["data"]][[1]][["count"]])
      zzz <- dfcislaSPlmelt3[ which(dfcislaSPlmelt3$lemma == sellemmar3$lemma), ]
      posxS = as.numeric(zzz$value[1])
      posxP = as.numeric(zzz$value[2])
      
      # plot arrow with lemma
      hp3 + scale_fill_gradient(low="blue", high="red") +
        annotate("segment", x=posxP, xend=posxP, y=posxP - maxy/10, yend=posxP, arrow=arrow(), color = "black", size=1.0) +
        annotate("text", label = paste(sellemmar3$lemma, " P"), x = posxP, y = posxP - maxy/10 - maxy/20, size = 5, colour = "black") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP3_title")) + xlab(i18n$t("cislo_histogram_x3")) +
        annotate("segment", x=posxP, xend=posxP, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
        coord_cartesian(ylim = c(0.0 - maxy/7, maxy))
    } else {
      # plot arrow with lemma
      hp2 + scale_fill_gradient(low="blue", high="red") +
        theme(legend.position = "none") + ggtitle(i18n$t("cislo_histogramP3_title")) + xlab(i18n$t("cislo_histogram_x3"))
    }
    
  })
  
  
  
  
    # Plot Histogram for Cisla singular+plural
  output$plotHistogramCislaSP<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaSP --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    dfcislalspmelt <- melt(data = dfcisla, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    # print(dfcisla[which(dfcisla$plurals == max(dfcisla$plurals)), ])
  })

  output$plotHistogramCislaSP2<-renderPlot({
    if (printDebug) { print("-- plotHistogramCislaSP2 --") }
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    dfcislalspmelt2 <- melt(data = dfcisla2, id.vars = c('lemma'),  measure.vars = c('singulars', 'plurals'))
    # print(dfcisla[which(dfcisla$plurals == max(dfcisla$plurals)), ])
  })
  
  # ----- Plot Histogram for CisloPad - pro vsechny pady
  output$plotHistogramCisloPad<-renderPlot({
    if (printDebug) { print("-- plotHistogramCisloPad --") }
    tmp <- input$changeLemma; tmp <- input$changeCorpus # <- make chart reactive to buttons

    CP <- input$rbCisloPadHist
    lemma <- sellemmar$lemma
    dfLemma = dfall[ which(dfall$lemma == sellemmar$lemma), ]
    lemmaStat <- sum(dfLemma[, c(CP)])
    dfHCPmelt <- melt(data = dfx, id.vars = c('cislopad'),  measure.vars = c('value'))
    df = dfHCPmelt[ which(dfHCPmelt$cislopad == CP), ]

    hp <- qplot(x=value, data = df, fill=..count.., geom="histogram", binwidth=histbw/5)

    ggp_build <- ggplot_build(hp)
    maxy <- max(ggp_build[["data"]][[1]][["count"]])

    posx = as.numeric(lemmaStat)

    # plot arrow with lemma
    hp + scale_fill_gradient(low="blue", high="red") +
      annotate("segment", x=posx, xend=posx, y=posx - maxy/10, yend=posx, arrow=arrow(), color = "black", size=1.0) +
      annotate("text", label = lemma, x = posx + posx/10+0.01, y = 0.0 - maxy/10, size = 5, colour = "black") +
      ggtitle(paste(i18n$t("cislopad_histogram_title"), CP)) +  xlab(i18n$t("cislopad_histogram_x")) +
      annotate("segment", x=posx, xend=posx, y=0.0, yend=maxy+maxy/20, color = "black", size=0.5) +
      coord_cartesian(ylim = c(0.0 - maxy/10, maxy))
  })

  output$plotHistogramCisloPad2<-renderPlot({
    if (printDebug) { print("-- plotHistogramCisloPad 2 --") }
    tmp <- input$changeLemma2; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    CP2 <- input$rbCisloPadHist2
    lemma2 <- sellemmar2$lemma
    dfLemma2 = dfall2[ which(dfall2$lemma == sellemmar2$lemma), ]
    lemmaStat2 <- sum(dfLemma2[, c(CP2)])
    dfHCPmelt2 <- melt(data = dfx2, id.vars = c('cislopad2'),  measure.vars = c('value'))
    df2 = dfHCPmelt2[ which(dfHCPmelt2$cislopad == CP2), ]
    
    hp2 <- qplot(x=value, data = df2, fill=..count.., geom="histogram", binwidth=histbw/5)
    
    ggp_build2 <- ggplot_build(hp2)
    maxy2 <- max(ggp_build2[["data"]][[1]][["count"]])
    
    posx2 = as.numeric(lemmaStat2)
    
    # plot arrow with lemma
    hp2 + scale_fill_gradient(low="blue", high="red") +
      annotate("segment", x=posx2, xend=posx2, y=posx2 - maxy2/10, yend=posx2, arrow=arrow(), color = "black", size=1.0) +
      annotate("text", label = lemma2, x = posx2 + posx2/10+0.01, y = 0.0 - maxy2/10, size = 5, colour = "black") +
      ggtitle(paste(i18n$t("cislopad_histogram_title2"), CP2)) +  xlab(i18n$t("cislopad_histogram_x2")) +
      annotate("segment", x=posx2, xend=posx2, y=0.0, yend=maxy2+maxy2/20, color = "black", size=0.5) +
      coord_cartesian(ylim = c(0.0 - maxy2/10, maxy2))
  })
  
  output$plotHistogramCisloOsoba3<-renderPlot({
    if (printDebug) { print("-- plotHistogramCisloOsoba 3 --") }
    tmp <- input$changeLemma3; tmp <- input$changeCorpus2 # <- make chart reactive to buttons
    
    CP3 <- input$rbCisloOsobaHist3
    lemma3 <- sellemmar3$lemma
    dfLemma3 = dfall3[ which(dfall3$lemma == sellemmar3$lemma), ]
    lemmaStat3 <- sum(dfLemma3[, c(CP3)])
    dfHCPmelt3 <- melt(data = dfx3, id.vars = c('cisloosoba3'),  measure.vars = c('value'))
    df3 = dfHCPmelt3[ which(dfHCPmelt3$cisloosoba == CP3), ]
    
    hp3 <- qplot(x=value, data = df3, fill=..count.., geom="histogram", binwidth=histbw/5)
    
    ggp_build3 <- ggplot_build(hp3)
    maxy3 <- max(ggp_build3[["data"]][[1]][["count"]])
    
    posx3 = as.numeric(lemmaStat3)
    
    # plot arrow with lemma
    hp3 + scale_fill_gradient(low="blue", high="red") +
      annotate("segment", x=posx3, xend=posx3, y=posx3 - maxy3/10, yend=posx3, arrow=arrow(), color = "black", size=1.0) +
      annotate("text", label = lemma3, x = posx3 + posx3/10+0.01, y = 0.0 - maxy3/10, size = 5, colour = "black") +
      ggtitle(paste(i18n$t("cisloosoba_histogram_title3"), CP3)) +  xlab(i18n$t("cisloosoba_histogram_x3")) +
      annotate("segment", x=posx3, xend=posx3, y=0.0, yend=maxy3+maxy3/20, color = "black", size=0.5) +
      coord_cartesian(ylim = c(0.0 - maxy3/10, maxy3))
  })
  ###################################
  #             OUTPUTS             #
  ###################################
  output$prasopad <- renderText({ paste(input$prasopad) })
  output$lemma <- renderText({ paste(sellemmar$lemma) })
  output$lemmaref <- renderUI({
    a(sellemmar$lemma, href = paste0("https://www.korpus.cz/slovo-v-kostce/search/cs/", sellemmar$lemma, "?lemma=&pos="), style="color:#e2007a")
  })

  output$lemma2 <- renderText({ paste(sellemmar2$lemma) })
  output$lemmaref2 <- renderUI({
    a(sellemmar2$lemma, href = paste0("https://www.korpus.cz/slovo-v-kostce/search/cs/", sellemmar2$lemma, "?lemma=&pos="), style="color:#e2007a")
  })
  
  output$lemma3 <- renderText({ paste(sellemmar3$lemma) })
  output$lemmaref3 <- renderUI({
    a(sellemmar3$lemma, href = paste0("https://www.korpus.cz/slovo-v-kostce/search/cs/", sellemmar3$lemma, "?lemma=&pos="), style="color:#e2007a")
  })

  #output$pokus <- renderText({ paste("   *****") })
  
  ###################################
  #             TABLES              #
  ###################################

  output$searchText <- renderText(HTML(
    if (nchar(input$search))
      str_replace_all(searchText, sprintf("(%s)", input$search), "<mark>\\1</mark>") else
        searchText
  ))

  # Standard header for lemma table
  fnCreateHeader <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "ROD"),
          th(icon("info-circle"), " ", "S1"),
          th(icon("info-circle"), " ", "S2"),
          th(icon("info-circle"), " ", "S3"),
          th(icon("info-circle"), " ", "S4"),
          th(icon("info-circle"), " ", "S5"),
          th(icon("info-circle"), " ", "S6"),
          th(icon("info-circle"), " ", "S7"),
          th(icon("info-circle"), " ", "P1"),
          th(icon("info-circle"), " ", "P2"),
          th(icon("info-circle"), " ", "P3"),
          th(icon("info-circle"), " ", "P4"),
          th(icon("info-circle"), " ", "P5"),
          th(icon("info-circle"), " ", "P6"),
          th(icon("info-circle"), " ", "P7"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }

  # Standard simple header for lemma table
  fnCreateHeader1 <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "S1"),
          th(icon("info-circle"), " ", "S2"),
          th(icon("info-circle"), " ", "S3"),
          th(icon("info-circle"), " ", "S4"),
          th(icon("info-circle"), " ", "S5"),
          th(icon("info-circle"), " ", "S6"),
          th(icon("info-circle"), " ", "S7"),
          th(icon("info-circle"), " ", "P1"),
          th(icon("info-circle"), " ", "P2"),
          th(icon("info-circle"), " ", "P3"),
          th(icon("info-circle"), " ", "P4"),
          th(icon("info-circle"), " ", "P5"),
          th(icon("info-circle"), " ", "P6"),
          th(icon("info-circle"), " ", "P7"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }

  fnCreateHeader3 <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "S1"),
          th(icon("info-circle"), " ", "S2"),
          th(icon("info-circle"), " ", "S3"),
          th(icon("info-circle"), " ", "P1"),
          th(icon("info-circle"), " ", "P2"),
          th(icon("info-circle"), " ", "P3"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }
  
  stdCallback = JS("
var tips = ['',
            'Slovníkový tvar slova',
            'Rod',
            'Singulár - 1. pád',
            'Singulár - 2. pád',
            'Singulár - 3. pád',
            'Singulár - 4. pád',
            'Singulár - 5. pád',
            'Singulár - 6. pád',
            'Singulár - 7. pád',
            'Plurál - 1. pád',
            'Plurál - 2. pád',
            'Plurál - 3. pád',
            'Plurál - 4. pád',
            'Plurál - 5. pád',
            'Plurál - 6. pád',
            'Plurál - 7. pád',
            'Frekvence'
           ],
header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}

table.on('click.dt', 'td', function() {
            var row_=table.cell(this).index().row;
            var col=table.cell(this).index().column;
            var rnd= Math.random();
            var data = [row_, col, rnd];
            Shiny.onInputChange('rowsdata1', data);
    });
")

  stdCallbackAdj1 = JS("
var tips = ['',
            'Slovníkový tvar slova', 'Rod', 'Singulár - 1. pád', 'Singulár - 2. pád', 'Singulár - 3. pád', 'Singulár - 4. pád', 'Singulár - 5. pád', 'Singulár - 6. pád', 'Singulár - 7. pád',
            'Plurál - 1. pád', 'Plurál - 2. pád', 'Plurál - 3. pád', 'Plurál - 4. pád', 'Plurál - 5. pád', 'Plurál - 6. pád', 'Plurál - 7. pád',
            'Frekvence'
           ],
header = table.columns().header();
for (var i = 0; i < tips.length; i++) { $(header[i]).attr('title', tips[i]); }

table.on('click.dt', 'td', function() {
            var row_=table.cell(this).index().row;
            var col=table.cell(this).index().column;
            var rnd= Math.random();
            var data = [row_, col, rnd];
            Shiny.onInputChange('rowsdata12', data);
    });
")
  
  stdCallbackVerb1 = JS("
var tips = ['',
            'Slovníkový tvar slova', 'Rod', 'Singulár - 1. osoba', 'Singulár - 2. osoba', 'Singulár - 3. osoba', 
            'Plurál - 1. osoba', 'Plurál - 2. osoba', 'Plurál - 3. osoba', 
            'Frekvence'
           ],
header = table.columns().header();
for (var i = 0; i < tips.length; i++) { $(header[i]).attr('title', tips[i]); }

table.on('click.dt', 'td', function() {
            var row_=table.cell(this).index().row;
            var col=table.cell(this).index().column;
            var rnd= Math.random();
            var data = [row_, col, rnd];
            Shiny.onInputChange('rowsdata12', data);
    });
")
  
    
  tblCisloTopCallback = JS("
var tips = ['',
            'Slovníkový tvar slova',
            'Singulár',
            'Plurál',
            'Frekvence'
           ],
header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")

  tblCisloBottomCallback = JS("
var tips = ['',
            'Slovníkový tvar slova',
            'Singulár',
            'Plurál',
            'Frekvence'
           ],
header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")

  # Standard simple header for lemma table in Číslo
  fnCreateHeaderCislo <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "Singular"),
          th(icon("info-circle"), " ", "Plural"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }
  
  # Standard simple header for lemma table in Rod
  fnCreateHeaderRod <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "M"),
          th(icon("info-circle"), " ", "I"),
          th(icon("info-circle"), " ", "F"),
          th(icon("info-circle"), " ", "N"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }
  
  # Standard simple header for lemma table in Degree
  fnCreateHeaderDegree <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "1"),
          th(icon("info-circle"), " ", "2"),
          th(icon("info-circle"), " ", "3"),
          th(icon("info-circle"), " ", "FQ")
        )
      )
    )
    )
  }

  # Standard simple header for lemma table in Negation
  fnCreateHeaderNegation <- function() {
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(""),
          th(icon("info-circle"), " ", "LEMMA"),
          th(icon("info-circle"), " ", "A"),
          th(icon("info-circle"), " ", "N"),
          th(icon("info-circle"), " ", "FQ")
          #th(icon("info-circle"), " ", "")
        )
      )
    )
    )
  }
  
  # click on table data1
  observeEvent(input$rowsdata1, {
    print("observeEvent(input$rowsdata1")
    rowindex <- input$data1_rows_current[input$rowsdata1[1]+1]
    sellemma <- dfallrod[rowindex,1]
    selrod <- dfallrod[rowindex,2]
    cql <- paste0(URLencode("[lemma=\""), URLencode(toString(sellemma)), URLencode("\" "),
                  "%26", URLencode(" tag=\".."), URLencode(toString(selrod)), URLencode(".*\"]"))
    href <- paste0(urlkontext, cql, "\" target=\"_blank\">Zobrazit výskyty</a>")
    cqlhtml <- paste0("[lemma=\"", toString(sellemma), "\"", " & ", "tag=\"..", toString(selrod), ".*\"]")
    html(id = "data1sel", paste0(href, " v korpusu výsledky CQL dotazu ", cqlhtml))

    #updateSelectizeInput(session = session, inputId = 'inSelectLemma', choices = clemmas, server = TRUE, selected=sellemma)
    if (sellemma %in% clemmas) { fnChangeLemma(sellemma) }
  })
  
  # click on table data1_2
  observeEvent(input$rowsdata12, {
    print("observeEvent(input$rowsdata12")
    rowindex2 <- input$data12_rows_current[input$rowsdata12[1]+1]
    sellemma2 <- dfallrod2[rowindex2,1]
    selrod2 <- dfallrod2[rowindex2,2]
    cql <- paste0(URLencode("[lemma=\""), URLencode(toString(sellemma2)), URLencode("\" "),
                  "%26", URLencode(" tag=\".."), URLencode(toString(selrod2)), URLencode(".*\"]"))
    href <- paste0(urlkontext, cql, "\" target=\"_blank\">Zobrazit výskyty</a>")
    cqlhtml <- paste0("[lemma=\"", toString(sellemma2), "\"", " & ", "tag=\"..", toString(selrod2), ".*\"]")
    html(id = "data12sel", paste0(href, " v korpusu výsledky CQL dotazu ", cqlhtml))
    
    #updateSelectizeInput(session = session, inputId = 'inSelectLemma', choices = clemmas, server = TRUE, selected=sellemma)
    if (sellemma2 %in% clemmas2) { fnChangeLemma2(sellemma2) }
  })

  # table Data 1 (Tabulky)
  output$data1 <- DT::renderDataTable({
    header <- fnCreateHeader()
    dfallrodDF <- as.data.frame(dfallrod)
    row.names(dfallrodDF) <- NULL

    DT::datatable(as.data.frame(dfallrod), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallback) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })

  # table Data 1_2 (Tabulky)
  output$data12 <- DT::renderDataTable({
    header <- fnCreateHeader()
    dfallrodDF2 <- as.data.frame(dfallrod2)
    row.names(dfallrodDF2) <- NULL
    
    DT::datatable(as.data.frame(dfallrod2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallbackAdj1) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })
  
  # table dataAjdGender (Tabulky)
  output$dataAdjGender <- DT::renderDataTable({
    header <- fnCreateHeaderRod()
    dfgender100pDF2 <- as.data.frame(dfgender100p2)
    row.names(dfgender100pDF2) <- NULL

    DT::datatable(as.data.frame(dfgender100p2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallbackAdj1) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })

  # table dataAdjCase (Tabulky)
  output$dataAdjCase <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfcase100pDF2 <- as.data.frame(dfcase100p2)
    row.names(dfcase100pDF2) <- NULL
    
    DT::datatable(as.data.frame(dfcase100p2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallbackAdj1) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })
  
  # table dataAdjDegree (Tabulky)
  output$dataAdjDegree <- DT::renderDataTable({
    header <- fnCreateHeaderDegree()
    dfdegree100pDF2 <- as.data.frame(dfdegree100p2)
    row.names(dfdegree100pDF2) <- NULL
    
    DT::datatable(as.data.frame(dfdegree100p2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallbackAdj1) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })

  # table dataAdjNegation (Tabulky)
  output$dataAdjNegation <- DT::renderDataTable({
    header <- fnCreateHeaderNegation()
    dfnegation100pDF2 <- as.data.frame(dfnegation100p2)

    #d <- c()
    #if (FQ >= 1000){
    #  d = "***"
    #} else if (FQ >= 100){
    #  d = "**"
    #} else {
    #  d = "*"
    #}
    #print(dfnegation100pDF2)
    #dfnegation100pDF2$stars <- d
    #dfnegation100pDF2$stars <- dfnegation100pDF2$lemma
    #print("sdsd")
    #dfnegation100pDF2$stars[df$FQ==100] <- "kuk"
    #dfnegation100pDF2$d[df$FQ>=0, df] <- df$b[df$a==6]
    #print(dfnegation100pDF2)
    row.names(dfnegation100pDF2) <- NULL
    
    DT::datatable(as.data.frame(dfnegation100p2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) ),
                  callback = stdCallbackAdj1) %>% formatStyle('lemma',  color = '#009ee0', fontWeight = 'bold')
  })
  
  fnFilterOutliersTop <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    upper_whisker = dfout[5, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] > upper_whisker), ]
    dfoutliers
  }

  fnFilterOutliersTop2 <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    upper_whisker = dfout[5, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] > upper_whisker), ]
    dfoutliers
  }
  
  fnFilterOutliersTop3 <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    upper_whisker = dfout[5, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] > upper_whisker), ]
    dfoutliers
  }
  
  fnFilterOutliersBottom <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    lower_whisker = dfout[1, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] <= lower_whisker), ]
    dfoutliers
  }
  
  fnFilterOutliersBottom2 <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    lower_whisker = dfout[1, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] <= lower_whisker), ]
    dfoutliers
  }

  fnFilterOutliersBottom3 <- function(df, colstring) {
    dfout <- fnGetOutliers(df)
    mycol <- grep(colstring, colnames(dfout))
    lower_whisker = dfout[1, mycol]
    dfoutliers <- df[which(df[ ,mycol+1] <= lower_whisker), ]
    dfoutliers
  }
  
  # table tblCisloTop
  output$tblCisloTop <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    #print("cisloTop")
    print(dfallSP)
    print(input$rbCislo)
    dfoutliersCisloTop <<- fnFilterOutliersTop(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloTop), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  #options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Czech.json', pageLength = 25)), callback = tblCisloTopCallback
                  #rownames = FALSE,
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
                  )
  })


  # table tblCisloTop2
  output$tblCisloTop2 <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfoutliersCisloTop2 <<- fnFilterOutliersTop2(dfallSP2, paste0("^", input$rbCislo2, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloTop2), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  # table tblCisloTop3
  output$tblCisloTop3 <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfoutliersCisloTop3 <<- fnFilterOutliersTop3(dfallSP3, paste0("^", input$rbCislo3, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloTop3), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  # table tblRodTop2
  output$tblRodTop2 <- DT::renderDataTable({
    print("tblRodTop2")
    header <- fnCreateHeaderRod()
    dfoutliersRodTop2 <<- fnFilterOutliersTop2(dfgender100p2, paste0("^", input$rbRod2, "$"))
    DT::datatable(as.data.frame(dfoutliersRodTop2), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
    
  })

  # table tblDegreeTop2
  output$tblDegreeTop2 <- DT::renderDataTable({
    print("tblDegreeTop2")
    header <- fnCreateHeaderDegree()
    dfoutliersDegreeTop2 <<- fnFilterOutliersTop2(dfdegree100p2, paste0("^D", input$rbDegree2, "$"))
    DT::datatable(as.data.frame(dfoutliersDegreeTop2), container=header, rownames=TRUE, colnames=c('Lepsi' = 2),
                                            selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  
  fnAddStars <- function(data, type) ({
    df <- data
    if (type == "top") {
      df <- fnFilterOutliersTop2(dfnegation100p2, paste0("^", input$rbNegation2, "$"))
    } else if (type == "bottom") {
      df <- fnFilterOutliersBottom2(dfnegation100p2, paste0("^", input$rbNegation2, "$"))
    }
      
    ##df$stars <- df$lemma
    
    ##df$stars[df$FQ>=0] <- ""
    ##df$stars[df$FQ>=10] <- "*"
    ##df$stars[df$FQ>=100] <- "**"
    ##df$stars[df$FQ>=1000] <- "***"
    ##df$stars[df$FQ>=10000] <- "****"
    ##df$stars[df$FQ>=100000] <- "*****"
    return(df)
  })
    
    
  # table tblNegationTop2
  output$tblNegationTop2 <- DT::renderDataTable({
    print("tblNegationTop2")
    header <- fnCreateHeaderNegation()
    df <- fnAddStars(dfoutliersNegationTop2, "top")
    DT::datatable(as.data.frame(df), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  # output$tblCisloTop <- renderText({fnCreateHeaderCislo()})
  #output$tblCisloTop <- renderText({"blablabla"})
  
  # table tblCisloBottom
  output$tblCisloBottom <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfoutliersCisloBottom <<- fnFilterOutliersBottom(dfallSP, paste0("^", input$rbCislo, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloBottom), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  #options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Czech.json', pageLength = 25)), callback = tblCisloBottomCallback
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
                  )
  })


  # table tblCisloBottom2
  output$tblCisloBottom2 <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfoutliersCisloBottom2 <<- fnFilterOutliersBottom2(dfallSP2, paste0("^", input$rbCislo2, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloBottom2), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  # table tblCisloBottom3
  output$tblCisloBottom3 <- DT::renderDataTable({
    header <- fnCreateHeaderCislo()
    dfoutliersCisloBottom3 <<- fnFilterOutliersBottom3(dfallSP3, paste0("^", input$rbCislo3, "$"))
    DT::datatable(as.data.frame(dfoutliersCisloBottom3), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  # # table tblRodBottom2
  output$tblRodBottom2 <- DT::renderDataTable({
    header <- fnCreateHeaderRod()
    dfoutliersRodBottom2 <<- fnFilterOutliersBottom2(dfgender100p2, paste0("^", input$rbRod2, "$"))
    DT::datatable(as.data.frame(dfoutliersRodBottom2), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  # # table tblDegreeBottom2
  output$tblDegreeBottom2 <- DT::renderDataTable({
    header <- fnCreateHeaderDegree()
    dfoutliersDegreeBottom2 <<- fnFilterOutliersBottom2(dfdegree100p2, paste0("^D", input$rbDegree2, "$"))
    DT::datatable(as.data.frame(dfoutliersDegreeBottom2), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  # # table tblNegationBottom2
  output$tblNegationBottom2 <- DT::renderDataTable({
    header <- fnCreateHeaderNegation()
    df <- fnAddStars(dfoutliersNegationBottom2, "bottom")
    DT::datatable(as.data.frame(df), container=header, rownames=TRUE,
                  selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  #output$tblCisloBottom <- renderText({typeof(dfoutliersCisloBottom)})
    
#    DT::renderDataTable({
#    header <- fnCreateHeaderCislo()
    
#    DT::datatable(as.data.frame(dfoutliersCisloBottom), container=header, rownames=TRUE,
#                  selection='single', escape=FALSE, filter = 'top',
#                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
#                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
#    )
#  })
  
  
  #output$pokus <- renderText({dfoutliersCisloBottom})
  
  # table cislopadTop
  output$tblcislopadTop <- DT::renderDataTable({
    header <- fnCreateHeader1()
    #print(dfall)
    #print(input$rbCisloPad)
    dfoutliers <- fnFilterOutliersTop(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::datatable(as.data.frame(dfoutliers), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  #options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Czech.json', pageLength = 25)), callback = stdCallback)
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 #autoWidth = FALSE, columnDefs = list(list(width = '300px', targets = c(1 ,2)))
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
                  )
  })

  output$tblcislopadTop2 <- DT::renderDataTable({
    header <- fnCreateHeader1()
    #print(dfall)
    #print(input$rbCisloPad)
    dfoutliers2 <- fnFilterOutliersTop2(dfall2, paste0("^", input$rbCisloPad2, "$"))
    DT::datatable(as.data.frame(dfoutliers2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  output$tblcisloosobaTop3 <- DT::renderDataTable({
    header <- fnCreateHeader3()
    dfoutliers3 <- fnFilterOutliersTop2(dfall3, paste0("^", input$rbCisloOsoba3, "$"))
    DT::datatable(as.data.frame(dfoutliers3), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  # table cislopadBottom
  output$tblcislopadBottom <- DT::renderDataTable({
    header <- fnCreateHeader1()
    dfoutliers <- fnFilterOutliersBottom(dfall, paste0("^", input$rbCisloPad, "$"))
    DT::datatable(as.data.frame(dfoutliers), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  #options = list(language = list(url = 'https://cdn.datatables.net/plug-ins/1.10.11/i18n/Czech.json', pageLength = 25)), callback = stdCallback)
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
                  )
  })

  output$tblcislopadBottom2 <- DT::renderDataTable({
    header <- fnCreateHeader1()
    dfoutliers2 <- fnFilterOutliersBottom2(dfall2, paste0("^", input$rbCisloPad2, "$"))
    DT::datatable(as.data.frame(dfoutliers2), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })

  output$tblcisloosobaBottom3 <- DT::renderDataTable({
    header <- fnCreateHeader3()
    dfoutliers3 <- fnFilterOutliersBottom2(dfall3, paste0("^", input$rbCisloOsoba3, "$"))
    DT::datatable(as.data.frame(dfoutliers3), container=header, rownames=TRUE, selection='single', escape=FALSE, filter = 'top',
                  options = list(language = list(url = i18n$t("tbl_lang")), pageLength = 25,
                                 columnDefs = list(list(targets = 0, visible = FALSE)) )
    )
  })
  
  # print outliers
  # output$outliers <- renderText(HTML(
  #   ooo <- outliers(dfcisla),
  #   print(ooo),
  #   "out"
  # ))

  
  # menu ~ buttons
  gobutton1text <- eventReactive(input$goButton1, {
    updateTabsetPanel(session, "aboutPanels", selected = paste0(i18n$t("substantivaTab"), input$controller))
    runjs(paste0('$("#goButton1").css("background-color","#337ab7")'))
    runjs(paste0('$("#goButton1").css("color","#ffffff")'))

    runjs(paste0('$("#goButton2").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton2").css("color","#337ab7")'))

    runjs(paste0('$("#goButton3").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton3").css("color","#337ab7")'))

        runjs(paste0('$("#goButtonAbout").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButtonAbout").css("color","#337ab7")'))

    input$n
  })

  output$goButton1Text <- renderText({gobutton1text()})


  gobutton2text <- eventReactive(input$goButton2, {
    print("akce adj")
    
    
    updateTabsetPanel(session, "aboutPanels", selected = paste0(i18n$t("adjektivaTab"), input$controller))
    runjs(paste0('$("#goButton1").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton1").css("color","#337ab7")'))
    
    runjs(paste0('$("#goButton2").css("background-color","#337ab7")'))
    runjs(paste0('$("#goButton2").css("color","#ffffff")'))

    runjs(paste0('$("#goButton3").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton3").css("color","#337ab7")'))

    runjs(paste0('$("#goButtonAbout").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButtonAbout").css("color","#337ab7")'))
    
    input$n
  })
  
  output$goButton2Text <- renderText({gobutton2text()})

  goButton3text <- eventReactive(input$goButton3, {
    print("akce 3 verba")

    updateTabsetPanel(session, "aboutPanels", selected = paste0(i18n$t("verbaTab"), input$controller))
    runjs(paste0('$("#goButton1").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton1").css("color","#337ab7")'))
    
    runjs(paste0('$("#goButton2").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton2").css("color","#337ab7")'))
    
    runjs(paste0('$("#goButton3").css("background-color","#337ab7")'))
    runjs(paste0('$("#goButton3").css("color","#ffffff")'))
    
    runjs(paste0('$("#goButtonAbout").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButtonAbout").css("color","#337ab7")'))
    
    input$n
  })
  
  output$goButton3Text <- renderText({goButton3text()})
  
  gobuttonAbouttext <- eventReactive(input$goButtonAbout, {
    updateTabsetPanel(session, "aboutPanels", selected = paste0(i18n$t("aboutTab"), input$controller))
    runjs(paste0('$("#goButton1").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton1").css("color","#337ab7")'))
    
    runjs(paste0('$("#goButton2").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton2").css("color","#337ab7")'))

    runjs(paste0('$("#goButton3").css("background-color","#ffffff")'))
    runjs(paste0('$("#goButton3").css("color","#337ab7")'))

    runjs(paste0('$("#goButtonAbout").css("background-color","#337ab7")'))
    runjs(paste0('$("#goButtonAbout").css("color","#ffffff")'))
    input$n
  })
  
  output$goButtonAboutText <- renderText({gobuttonAbouttext()})
  # /menu ~ buttons
  
  print("test")
}
