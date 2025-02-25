## install libraries
library(shiny)
library(bslib)
library(DT)

## list of buoys owned by NOS
nos_buoys = c('agxc1', 'baxc1', 'bdrn4', 'bdsp1', 'bgnn6', 'bkbf1', 'blif1', 'bltm3', 'bmtw1', 'brnd1', 'chcm2', 'chyv2', 'covm2', 'cpmw1', 'cpnw1', 'cptr1', 'cpvm2', 'cryv2', 'deld1', 'dmsf1', 'domv2', 'dpxc1', 'ebef1', 'efla1', 'einl1', 'fmoa1', 'frma1', 'frvm3', 'frxm3', 'fskm2', 'fsnm2', 'gctf1', 'hivt2', 'klmw1', 'lndc1', 'lqat2', 'ltjf1', 'mbpa1', 'mcga1', 'mhbt2', 'mhrn6', 'mnpv2', 'mrcp1', 'mtbf1', 'mzxc1', 'nbgm3', 'nblp1', 'nfdf1', 'nwhc3', 'obxc1', 'okxc1', 'omhc1', 'optf1', 'pdvr1', 'pegf1', 'pfdc1', 'pfxc1', 'pill1', 'pmaf1', 'pptm2', 'ppxc1', 'prhh1', 'prjc1', 'prur1', 'psbc1', 'psxc1', 'ptbm6', 'ptcr1', 'ptoa1', 'pvdr1', 'pxac1', 'pxoc1', 'pxsc1', 'qptr1', 'rcmc1', 'robn4', 'rplv2', 'rtyc1', 'seim1', 'sjsn4', 'skcf1', 'swpm4', 'tcbm2', 'tcmw1', 'tcnw1', 'tlvt2', 'tpaf1', 'tshf1', 'txvt2', 'upbc1', 'utvt2', 'vtbt2', 'wdsv2', 'ykrv2')

## ui
ui = page_sidebar(

  ## tags
  window_title = 'NOAA Buoy Data',

  ## app title
  title = 'NOS Stations',

  ## sidebar
  sidebar = sidebar(

    ## selector for buoy id
    selectInput(inputId = 'buoy', label = 'Buoy ID:', choices = nos_buoys),

    ## action button
    actionButton(inputId = 'query', label = 'Run')

  ),

  ## output table
  card(
    full_screen = TRUE,
    card_header('Data'),
    dataTableOutput('table')
  )

)

## server
server = function(input, output, session) {

  ## buoy data
  buoyData = reactiveValues(data = NULL)

  ## query data
  observeEvent(input$query, {

    ## url
    url = paste0('https://www.ndbc.noaa.gov/data/realtime2/', toupper(input$buoy), '.txt')

    ## test
    test_url = try(read.table(url), silent = TRUE)

    ## query
    if (class(test_url) != "try-error") {

      ## data
      buoyData$data = test_url

      ## fix column names
      # units: yr mo dy hr mn degT m/s m/s m sec sec degT hPa degC degC degC nmi hPa ft
      colnames(buoyData$data) = c('YY', 'MM', 'DD', 'hh', 'mm', 'WDIR', 'WSPD', 'GST', 'WVHT', 'DPD', 'APD', 'MWD', 'PRES', 'ATMP', 'WTMP', 'DEWP', 'VIS', 'PTDY', 'TIDE')

    } else {

      buoyData$data = data.frame()

    }

  })

  ## display table
  output$table = renderDataTable(buoyData$data, options = list(pageLength = 25))

}

## Shiny app
shinyApp(ui, server)
