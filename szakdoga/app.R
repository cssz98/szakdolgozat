library(shiny)
library(plotly)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)



ui <- fluidPage(
  titlePanel(
    title =
      tags$link(rel = "icon", type = "image", href = "favicon.png"),
    "Szakdolgozat"
  ),
  
  tags$style(
    HTML(
      ".navbar-static-top .container-fluid {background:   	#00a65a}
      .navbar-static-top .container-fluid {
  border: 3px solid #0f7864}

      .navbar-static-top div.container-fluid ul.nav.navbar-nav li.active a {background-image: linear-gradient(#00a65a, #18bc9c)}
     .navbar-static-top div.container-fluid ul.nav.navbar-nav a {color: #FFFFFF}"
    )
  ),
  navbarPage(
    title = div(img(
      src = "favicon.png",
      style = "margin-top: -14px;",
      height = 50
    )),
    
    tabPanel(
      "Nevezetes diszkrét eloszlások",
      uiOutput("diszkret"),
      icon = icon("chart-bar")
    ),
    tabPanel(
      "Nevezetes folytonos eloszlások",
      uiOutput("folytonos"),
      icon = icon("chart-line")
    ),
    tabPanel(
      title = HTML(
        "</a></li><li><a href='https://github.com/cssz98/szakdolgozat' target='_blank'> GitHub "
      )
    )
  ),
  
)


server <- function(input, output) {
  
  output$diszkret <- renderUI({
    dashboardPage(
      skin = "green",
      dashboardHeader(title = "Eloszlások"),
      dashboardSidebar(sidebarMenu(
        menuItem(
          "Binomiális eloszlás",
          tabName = "d_elso",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Poisson eloszlás",
          tabName = "d_masodik",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Hipergeometriai eloszlás",
          tabName = "d_harmadik",
          icon = icon("chart-bar")
        )
      )),
      
      
      dashboardBody(
        tabItems(
          #Binomiális eloszlás
          {
            tabItem(
              tabName = "d_elso",
              fluidRow(
                column(
                  9,
                  #CSS
                  
                  
                  tags$style(
                    HTML(
                      "

                      #binom_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
                      #binom_box1 {
width: 20%
}
                      #binom_box2 {
width: 25%
}
                      #binom_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


                      #binom_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

                      #binom_box3 {width: 70%}
.logo {color: #FFFFFF}
                      .logo {background-color: #0f7864}

                                                  #diszkret .navbar-static-top {background: linear-gradient(130deg,#18bc9c 91%, #0f7864 9%)}
                                                   .sidebar-toggle:hover{
                                                  background-image: linear-gradient(-90deg, #18bc9c, #0f7864)}
                                                  a.sidebar-toggle {
                                                  color: #FFFFFF}
                                      aside#sidebarCollapsed.main-sidebar {background-image: linear-gradient(#233, #000000)}
                                      #sidebarItemExpanded a {
                                                  color: #FFFFFF}
                                                  #sidebarItemExpanded a:hover {
                                                  border-style: dashed; #FFFFFF}

                      .irs-bar, .irs-bar-edge {background:   	#18bc9c}
                     .irs-single,.irs-from,.irs-to {background:   	 	#008080}
                    .nav-tabs-custom .nav-tabs li.active {border-top-color:#00CCCC}

                   .border-radius-none , .bg-teal-gradient .box-header {background: #0f7864}


                 .selectize-input {height: 42px; width: 133px}
                 .col-sm-6 .box-body
                   {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}


            .col-sm-6
                   {width: 55%}


 div.col-sm-9 div.col-sm-6 div.box {
                      border-top-color:#28b463;
                  }
                    #binom_box3 {
                      border-left: 3px dashed #28b463;
                      border-bottom: 3px dashed #28b463;
                    }

                    .control-label
                    {font-family: Times New Roman}
.box-title
 {font-family: Times New Roman}
                    #binom_valsz_input .control-label
                    {
                      color: #ffffff
                  }")
                  ),
                  
                  
                  
                  #ValueBox
                  valueBoxOutput("binom_box1"),
                  valueBoxOutput("binom_box2"),
                  box(
                    valueBoxOutput("binom_box3"),
                    uiOutput("binom_valsz_input")
                  )
                ),
                column(
                  3,
                  #Képletek
                  tabBox(
                    width = "110%",
                    side = "right",
                    tabPanel("Várható érték", uiOutput('binom_ex2_dynamic')),
                    tabPanel("Szórás", uiOutput('binom_ex3_dynamic')),
                    tabPanel(uiOutput("binom_tab1"), uiOutput('binom_ex1_dynamic'))
                  )
                )
              ),
              
              fluidRow(column(
                2,
                #Adattábla
                box(
                  title = "Adattábla",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "success",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  DT::dataTableOutput("binom_tabla")
                ),
                
                #Paraméterek
                gradientBox(
                  width = 12,
                  title = "Paraméterek",
                  icon = "fa fa-sliders",
                  gradientColor = "teal",
                  collapsible = FALSE,
                  footer = list(
                    uiOutput("binom_n_slider", inline = T),
                    uiOutput("binom_p_slider", inline = T)
                  )
                )
              ),
              
              column(8,
                     #Plot
                     box(
                       width = NULL,
                       status = "success",
                       plotlyOutput("binom_plot")
                     )),
              column(
                2,
                #Paraméterek
                gradientBox(
                  width = 12,
                  title = "Paraméterek",
                  icon = "fa fa-sliders",
                  gradientColor = "teal",
                  collapsible = FALSE,
                  footer = list(
                    uiOutput("binom_x_slider", inline = T),
                    uiOutput("binom_x_intervallum_slider", inline = T)
                  )
                ),
              ))
            )
          },
#Poisson eloszlás
{
  tabItem(
    tabName = "d_masodik",
    fluidRow(
      column(
        #CSS
        9,
        tags$style(
          HTML(
            " #poisson_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
                      #poisson_box1 {
width: 22%
}
                      #poisson_box2 {
width: 22%
}
                      #poisson_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


                      #poisson_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

                      #poisson_box3 {width: 70%}
 #poisson_box3 {
                      border-left: 3px dashed #28b463;
                      border-bottom: 3px dashed #28b463;
                    }


 #poisson_valsz_input .control-label
                    {
                      color: #ffffff
                  }"
          )
        ),
        
        #ValueBox
        valueBoxOutput("poisson_box1"),
        valueBoxOutput("poisson_box2"),
        box(
          valueBoxOutput("poisson_box3"),
          uiOutput("poisson_valsz_input")
        )
      ),
      column(
        3,
        #Képletek
        tabBox(
          width = "110%",
          side = "right",
          tabPanel("Várható érték", uiOutput('poisson_ex2_dynamic')),
          tabPanel("Szórás", uiOutput('poisson_ex3_dynamic')),
          tabPanel(uiOutput('poisson_tab1'), uiOutput('poisson_ex1_dynamic'))
        )
      )
    ),
    
    fluidRow(
      column(
        2,
        #Adattábla
        box(
          title = "Adattábla",
          width = NULL,
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          collapsed = TRUE,
          DT::dataTableOutput("poisson_tabla")
        ),
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = list(uiOutput("poisson_lambda_slider", inline = T))
        )
      ),
      
      column(8,
             #Plot
             box(
               width = NULL,
               status = "success",
               plotlyOutput("poisson_plot")
             )),
      
      column(
        2,
        
        
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = list(
            uiOutput("poisson_x_slider", inline = T),
            uiOutput("poisson_x_intervallum_slider", inline = T)
          )
        )
      )
    )
  )
}, 
#Hipergeometriai eloszlás
{
  tabItem(
    tabName = "d_harmadik",
    fluidRow(
      column(
        9,
        #CSS
        tags$style(
          HTML(
            " #hipergeo_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
                      #hipergeo_box1 {
width: 17%
}
                      #hipergeo_box2 {
width: 27%
}
                     #hipergeo_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


                      #hipergeo_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

                      #hipergeo_box3 {width: 70%}
 #hipergeo_box3 {
                      border-left: 3px dashed #28b463;
                      border-bottom: 3px dashed #28b463;
                    }


 #hipergeo_valsz_input .control-label
                    {
                      color: #ffffff
                  }"
          )
        ),
        
        #ValueBox
        valueBoxOutput("hipergeo_box1"),
        valueBoxOutput("hipergeo_box2"),
        box(
          valueBoxOutput("hipergeo_box3"),
          uiOutput("hipergeo_valsz_input")
        )
      ),
      column(
        3,
        #Képletek
        tabBox(
          width = "110%",
          side = "right",
          tabPanel("Várható érték", uiOutput('hipergeo_ex2_dynamic')),
          tabPanel("Szórás", uiOutput('hipergeo_ex3_dynamic')),
          tabPanel(
            uiOutput("hipergeo_tab1"),
            uiOutput('hipergeo_ex1_dynamic')
          )
        )
      )
    ),
    
    fluidRow(
      column(
        2,
        #Adattábla
        box(
          title = "Adattábla",
          width = NULL,
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          collapsed = TRUE,
          DT::dataTableOutput("hipergeo_tabla")
        ),
        
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = list(
            uiOutput("hipergeo_m_slider", inline = T),
            uiOutput("hipergeo_n_slider", inline = T),
            uiOutput("hipergeo_k_slider", inline = T)
          )
        )
      ),
      #Plot
      column(8,
             box(
               width = NULL,
               status = "success",
               plotlyOutput("hipergeo_plot")
             )),
      
      column(
        2,
        
        
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = list(
            uiOutput("hipergeo_x_slider", inline = T),
            uiOutput("hipergeo_x_intervallum_slider", inline = T)
            
            
            
            
          )
        )
      )
    )
  )
}))
    )
  })
  
  #Binomiális eloszlás
  {
    #Változók
    {
      binom_x <- reactive({
        input$binom_x
      })
      binom_n <- reactive({
        input$binom_n
      })
      binom_p <- reactive({
        input$binom_p
      })
      binom_xn_range <- reactive({
        binom_x():binom_n()
      })
      binom_0x_range <- reactive({
        0:binom_x()
      })
      binom_x_range <- reactive({
        input$binom_x_intervallum[1]:input$binom_x_intervallum[2]
      })
      binom_n_range <- reactive({
        0:binom_n()
      })
      
    }
    #Input-ok
    {
      output$binom_p_slider <- renderUI({
        sliderInput(
          "binom_p",
          "$$(\\color{blue}{\\pi})$$
                             Adja meg a siker valószínűségét:",
          0.5,
          min = 0,
          max = 1,
          step = 0.01
        )
      })
      
      output$binom_x_slider <- renderUI({
        sliderInput(
          "binom_x",
          "$$(\\color{green}{x})$$
                            Adja meg az x értékét:",
          min = 0,
          max(binom_n()),
          value = 5,
          step = 1
        )
      })
      
      output$binom_x_intervallum_slider <- renderUI({
        sliderInput(
          "binom_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
                            Adja meg az intervallumot:",
          min = 0,
          max(binom_n()),
          value = c(0, 10),
          step = 1
        )
      })
      
      output$binom_n_slider <- renderUI({
        sliderInput(
          "binom_n",
          "$$(\\color{red}{n})$$
                            Adja meg a minta elemszámát:",
          15,
          min = 1,
          max = 50,
          step = 1
        )
      })
      
      output$binom_valsz_input <- renderUI({
        selectInput(
          "binom_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X=x)$$" = "binom_egyenlo",
            "$$\\mathbf{P}(X \\leq x)$$" = "binom_ekisebb",
            "$$\\mathbf{P}(X \\geq x)$$" = "binom_enagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)}$$" =
              "binom_intervallum"
          )
        )
      })
    }
    #Eredmények
    {
      binom_eredmeny <- reactive({
        if (input$binom_tipus == "binom_egyenlo")
          dbinom(binom_x(), binom_n(), binom_p())
        
        else if (input$binom_tipus == "binom_ekisebb")
        {
          sum(dbinom(binom_0x_range(), binom_n(), binom_p()))
        }
        
        else if (input$binom_tipus == "binom_enagyobb")
        {
          sum(dbinom(binom_xn_range(), binom_n(), binom_p()))
        }
        else
        {
          sum(dbinom(binom_x_range(), binom_n(), binom_p()))
        }
      })
      
      binom_varhato_ertek_react <- reactive ({
        round(binom_n() * binom_p(), digits = 5)
      })
      
      binom_szoras_react <- reactive ({
        round(binom_n() * binom_p() * (1 - binom_p()), digits = 5)
      })
    }
    #Plot
    {
      output$binom_plot <- renderPlotly({
        sulyfvg <-
          plot_ly(type = "bar")
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dbinom(binom_0x_range(), binom_n(), binom_p()),
            x = binom_0x_range(),
            name = "P(X\u2264 x)",
            marker = list(color = '#00CCCC')
          )
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dbinom(binom_xn_range(), binom_n(), binom_p()),
            x = binom_xn_range(),
            name = "P(X\u2265 x)",
            marker = list(color = '#0e6655')
          )
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dbinom(binom_x(), binom_n(), binom_p()),
            x = binom_x(),
            name = "P(X= x)",
            marker = list(color = '#28b463')
            
          )
        if (input$binom_tipus == "binom_intervallum")
          sulyfvg <-
          sulyfvg %>% add_lines(
            y = dbinom(binom_x_range(), binom_n(), binom_p()),
            x = binom_x_range(),
            name = "P(x\u2081 \u2264 X \u2264 x\u2082)",
            line = list(shape = "spline", color = '#FF0000'),
            marker = list(color = '#FF0000'),
            fill = "tozeroy",
            fillcolor = 'rgba(255, 0, 0, 0.4)'
          )
        
        
        sulyfvg <-
          sulyfvg %>% layout(
            xaxis = list(
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            barmode = 'overlay'
          )
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = pbinom(binom_n_range(), binom_n(), binom_p()),
            x = binom_n_range(),
            name = "Kumulatív valószínűség",
            line = list(shape = "hvh", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
    }
    #DT
    {
      binom_eredmeny_dt <- reactive({
        if (input$binom_tipus == "binom_egyenlo")
          dbinom(binom_x(), binom_n(), binom_p())
        
        else if (input$binom_tipus == "binom_ekisebb")
        {
          dbinom(binom_0x_range(), binom_n(), binom_p())
        }
        else if (input$binom_tipus == "binom_enagyobb")
        {
          dbinom(binom_xn_range(), binom_n(), binom_p())
        }
        else
        {
          dbinom(binom_x_range(), binom_n(), binom_p())
        }
      })
      
      binom_x_dt <- reactive({
        if (input$binom_tipus == "binom_egyenlo")
          binom_x()
        
        else if (input$binom_tipus == "binom_enagyobb")
        {
          binom_xn_range()
        }
        else if (input$binom_tipus == "binom_ekisebb") {
          binom_0x_range()
        }
        else
        {
          binom_x_range()
        }
      })
      
      binom_dt <- reactive({
        if (input$binom_tipus == "binom_egyenlo")
          "P(X= x)"
        else if (input$binom_tipus == "binom_enagyobb")
          "P(X\u2265 x)"
        else if (input$binom_tipus == "binom_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      
      
      
      binom_df <- reactive({
        data.frame(x = binom_x_dt(),
                   y = round(binom_eredmeny_dt(), digits = 5))
      })
      
      output$binom_tabla <- renderDataTable({
        bdt <-
          datatable(
            binom_df(),
            caption = binom_dt(),
            colnames = c("X", "Valószínűség"),
            rownames = FALSE,
            options = list(
              lengthChange = FALSE,
              searching = FALSE,
              info = FALSE,
              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Hungarian.json'),
              pageLength = 2
            )
          )
        
        bdt2 <-
          formatStyle(
            bdt,
            "x",
            color = '#FFFFFF',
            backgroundColor = "#006666",
            fontWeight = 'bold'
          )
        bdt3 <-
          formatStyle(
            bdt2,
            "y",
            color = '#FFFFFF',
            backgroundColor = "#009999",
            fontWeight = 'bold'
          )
      })
    }
    #ValueBox
    {
      output$binom_box1 <- renderValueBox({
        valueBox(
          binom_varhato_ertek_react(),
          icon = icon(" fa-calculator"),
          uiOutput('binom_ex2'),
          color = "aqua",
          href = NULL
        )
      })
      
      output$binom_box2 <- renderValueBox({
        valueBox(
          binom_szoras_react(),
          uiOutput('binom_ex3'),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$binom_box3 <- renderValueBox({
        valueBox(
          round(binom_eredmeny(), digits = 5),
          uiOutput("binom_ex1"),
          
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
    }
    #Képletek
    {
      output$binom_tab1 <- renderUI({
        if (input$binom_tipus == "binom_egyenlo")
          "P(X= x)"
        else if (input$binom_tipus == "binom_enagyobb")
          "P(X\u2265 x)"
        else if (input$binom_tipus == "binom_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      
      output$binom_ex1 <- renderUI({
        if (input$binom_tipus == "binom_intervallum")
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)=\\sum_{k= \\ x_1}^{\\ x_2}{n\\choose k}\\pi^k(1-\\pi)^{n-k}}$$'
            )
          )
        else if (input$binom_tipus == "binom_egyenlo")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X=x)={n\\choose x}\\pi^x (1-\\pi)^{n-x}}$$'
            )
          )
        }
        else if (input$binom_tipus == "binom_enagyobb")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\geq x)=\\sum_{k=x}^{n}{n\\choose k}\\pi^k(1-\\pi)^{n-k}}$$'
            )
          )
        }
        else if (input$binom_tipus == "binom_ekisebb")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\leq x)=\\sum_{k=0}^{x}{n\\choose k}\\pi^k(1-\\pi)^{n-k}}$$'
            )
          )
        }
      })
      
      output$binom_ex1_dynamic <- renderUI({
        if (input$binom_tipus == "binom_intervallum")
          withMathJax(
            sprintf(
              "$$\ X\\sim\\text{Bin}(n,\\pi),\\pi = \\frac{K}{N}$$
                    $$\\small{
                    \\mathbf{P}(\\color{green}{\\ %g} \\leq X  \\leq \\color{green}{%g})=\\sum_{k=\\color{green}{%g}}^{\\color{green}{%g}}
                    {\\color{red}{%g}\\choose k}
                    \\color{blue}{%g}^k(1-\\color{blue}{%g})^{\\color{red}{%g}-k}}$$",
              input$binom_x_intervallum[1],
              input$binom_x_intervallum[2],
              input$binom_x_intervallum[1],
              input$binom_x_intervallum[2],
              binom_n(),
              binom_p(),
              binom_p(),
              binom_n()
            )
          )
        else if (input$binom_tipus == "binom_egyenlo")
        {
          withMathJax(
            sprintf(
              "$$\ X\\sim\\text{Bin}(n,\\pi),\\pi = \\frac{K}{N}$$
                                        $$\\small
                    {
                        \\mathbf{P}(X=\\color{green}{%g})={\\color{red}{%g}\\choose \\color{green}{%g}}\\color{blue}{%g}^\\color{green}{%g}
                        (1-\\color{blue}{%g})^{\\color{red}{%g}-\\color{green}{%g}}}$$",
              binom_x(),
              binom_n(),
              binom_x(),
              binom_p(),
              binom_x(),
              binom_p(),
              binom_n(),
              binom_x()
            )
          )
        }
        else if (input$binom_tipus == "binom_enagyobb")
        {
          withMathJax(
            sprintf(
              '$$\ X\\sim\\text{Bin}(n,\\pi),\\pi = \\frac{K}{N}$$
                    $$\\small{\\mathbf{P}(X \\geq \\color{green}{%g})=\\sum_{k=
                    \\color{green}{%g}}^{\\color{red}{%g}}{\\color{red}{%g}\\choose k}\\color{blue}{%g}^k(1-\\color{blue}{%g})^{\\color{red}{%g}-k}}$$',
              binom_x(),
              binom_x(),
              binom_n(),
              binom_n(),
              binom_p(),
              binom_p(),
              binom_n()
            )
          )
        }
        
        else if (input$binom_tipus == "binom_ekisebb")
        {
          withMathJax(
            sprintf(
              '$$\ X\\sim\\text{Bin}(n,\\pi),\\pi = \\frac{K}{N}$$
                    $$\\small{\\mathbf{P}(X \\leq \\color{green}{%g})=\\sum_{k=0}^{\\color{green}{%g}}{\\color{red}{%g}
                        \\choose k
                    }\\color{blue}{%g}^k(1-\\color{blue}{%g})^{\\color{red}{%g}-k}}$$',
              binom_x(),
              binom_x(),
              binom_n(),
              binom_p(),
              binom_p(),
              binom_n()
            )
          )
        }
        
      })
      
      output$binom_ex2 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{E}(X)=n\\pi}$$'))
      })
      
      output$binom_ex2_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{E}(X)=\\color{red}{%g}*\\color{blue}{%g} $$',
            binom_n(),
            binom_p()
          )
        )
      })
      
      output$binom_ex3 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{D}^2(X)=n\\pi(1-\\pi)}$$'))
      })
      
      output$binom_ex3_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{D}^2(X)=\\color{red}{%g}*\\color{blue}{%g}(1-\\color{blue}{%g})$$',
            binom_n(),
            binom_p(),
            binom_p()
          )
        )
      })
    }
  }
  #Poisson eloszlás
  {
    #Változók
    {
      poisson_lambda <- reactive({
        input$poisson_lambda
      })
      poisson_x_range <- reactive({
        input$poisson_x_intervallum[1]:input$poisson_x_intervallum[2]
      })
      poisson_x <- reactive({
        input$poisson_x
      })
      poisson_0x_range <- reactive({
        0:poisson_x()
      })
      poisson_xlambda <- reactive({
        poisson_x() + poisson_lambda()
      })
      poisson_x_xlambda_range <- reactive({
        poisson_x():poisson_xlambda()
      })
      poisson_0_xlambda_range <- reactive({
        0:poisson_xlambda()
      })
      
    }
    #Input-ok
    {
      output$poisson_x_slider <- renderUI({
        sliderInput(
          "poisson_x",
          
          "$$(\\color{green}{x})$$
            Adja meg az x értékét:",
          min = 0,
          max = 50,
          value = 5,
          step = 1
        )
      })
      
      output$poisson_x_intervallum_slider <- renderUI({
        sliderInput(
          "poisson_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
        Adja meg az intervallumot:",
          min = 0,
          max = 50,
          value = c(0, 10),
          step = 1
        )
      })
      
      output$poisson_lambda_slider <- renderUI({
        sliderInput(
          "poisson_lambda",
          "$$(\\color{red}{\\lambda})$$
        Adja meg a lambda értékét:",
          10,
          min = 1,
          max = 50,
          step = 0.5
        )
      })
      
      output$poisson_valsz_input <- renderUI({
        selectInput(
          "poisson_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X=x)$$" = "poisson_egyenlo",
            "$$\\mathbf{P}(X \\leq x)$$" = "poisson_ekisebb",
            "$$\\mathbf{P}(X \\geq x)$$" = "poisson_enagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)}$$" = "poisson_intervallum"
          )
        )
      })
    }
    #Eredmények
    {
      poisson_eredmeny <- reactive({
        if (input$poisson_tipus == "poisson_egyenlo")
          dpois(poisson_x(), poisson_lambda())
        
        else if (input$poisson_tipus == "poisson_ekisebb")
        {
          sum(dpois(poisson_0x_range(), poisson_lambda()))
        }
        else if (input$poisson_tipus == "poisson_enagyobb")
        {
          ppois(poisson_x(), poisson_lambda(), lower.tail = F) + dpois(poisson_x(), poisson_lambda())
        }
        else
        {
          sum(dpois(poisson_x_range(), poisson_lambda()))
        }
      })
      
    }
    #Plot
    {
      plot_x <- reactive({
        if (input$poisson_tipus == "poisson_intervallum")
          poisson_x_range()
        else{
          poisson_0_xlambda_range()
        }
        
      })
      
      plot_y <- reactive({
        if (input$poisson_tipus == "poisson_intervallum")
          ppois(poisson_x_range(), poisson_lambda())
        else{
          ppois(poisson_0_xlambda_range(), poisson_lambda())
        }
        
      })
      
      output$poisson_plot <- renderPlotly({
        sulyfvg <-
          plot_ly(type = "bar")
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dpois(poisson_0x_range(), poisson_lambda()),
            x = poisson_0x_range(),
            name = "P(X\u2264 x)",
            marker = list(color = '#00CCCC')
          )
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dpois(poisson_x_xlambda_range(), poisson_lambda()),
            x = poisson_x_xlambda_range(),
            name = "P(X\u2265 x)",
            marker = list(color = '#0e6655')
          )
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dpois(poisson_x(), poisson_lambda()),
            x = poisson_x(),
            name = "P(X= x)",
            marker = list(color = '#28b463')
            
          )
        if (input$poisson_tipus == "poisson_intervallum")
          sulyfvg <-
          sulyfvg %>% add_lines(
            y = dpois(poisson_x_range(), poisson_lambda()),
            x = poisson_x_range(),
            name = "P(x\u2081 \u2264 X \u2264 x\u2082)",
            line = list(shape = "spline", color = '#FF0000'),
            marker = list(color = '#FF0000'),
            fill = "tozeroy",
            fillcolor = 'rgba(255, 0, 0, 0.4)'
          )
        
        
        sulyfvg <-
          sulyfvg %>% layout(
            xaxis = list(
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            barmode = 'overlay'
          )
        
        
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = plot_y(),
            x = plot_x(),
            name = "Kumulatív valószínűség",
            line = list(shape = "hvh", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
    }
    #DT
    {
      poisson_eredmeny_dt <- reactive({
        if (input$poisson_tipus == "poisson_egyenlo")
          dpois(poisson_x(), poisson_lambda())
        
        else if (input$poisson_tipus == "poisson_ekisebb")
        {
          dpois(poisson_0x_range(), poisson_lambda())
        }
        
        else if (input$poisson_tipus == "poisson_enagyobb")
        {
          dpois(poisson_x_xlambda_range(), poisson_lambda())
        }
        else
        {
          dpois(poisson_x_range(), poisson_lambda())
        }
      })
      
      
      poisson_x_dt <- reactive({
        if (input$poisson_tipus == "poisson_egyenlo")
          poisson_x()
        
        else if (input$poisson_tipus == "poisson_ekisebb")
        {
          poisson_0x_range()
        }
        
        else if (input$poisson_tipus == "poisson_enagyobb")
        {
          poisson_x_xlambda_range()
        }
        else
        {
          poisson_x_range()
        }
        
      })
      
      poisson_dt <- reactive({
        if (input$poisson_tipus == "poisson_egyenlo")
          "P(X= x)"
        else if (input$poisson_tipus == "poisson_enagyobb")
          "P(X\u2265 x)"
        else if (input$poisson_tipus == "poisson_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      
      poisson_df <- reactive({
        data.frame(x = poisson_x_dt(),
                   y = round(poisson_eredmeny_dt(), digits = 5))
      })
      
      output$poisson_tabla <- renderDataTable({
        pdt <-
          datatable(
            poisson_df(),
            caption = poisson_dt(),
            colnames = c("X", "Valószínűség"),
            rownames = FALSE,
            options = list(
              lengthChange = FALSE,
              searching = FALSE,
              info = FALSE,
              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Hungarian.json'),
              pageLength = 2
            )
          )
        
        pdt2 <-
          formatStyle(
            pdt,
            "x",
            color = '#FFFFFF',
            backgroundColor = "#006666",
            fontWeight = 'bold'
          )
        pdt3 <-
          formatStyle(
            pdt2,
            "y",
            color = '#FFFFFF',
            backgroundColor = "#009999",
            fontWeight = 'bold'
          )
      })
    }
    #ValueBox
    {
      output$poisson_box1 <- renderValueBox({
        valueBox(
          poisson_lambda(),
          icon = icon(" fa-calculator"),
          uiOutput("poisson_ex2"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$poisson_box2 <- renderValueBox({
        valueBox(
          poisson_lambda(),
          uiOutput("poisson_ex3"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$poisson_box3 <- renderValueBox({
        valueBox(
          round(poisson_eredmeny(), digits = 5),
          uiOutput('poisson_ex1'),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
    }
    #Képletek
    {
      output$poisson_tab1 <- renderUI({
        if (input$poisson_tipus == "poisson_egyenlo")
          "P(X= x)"
        else if (input$poisson_tipus == "poisson_enagyobb")
          "P(X\u2265 x)"
        else if (input$poisson_tipus == "poisson_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      output$poisson_ex1 <- renderUI({
        if (input$poisson_tipus == "poisson_intervallum")
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)=
                \\sum_{k= \\ x_1}^{\\ x_2}\\frac{\\lambda^k\\mathrm{e}^{-\\lambda}}{k!}}$$'
            )
          )
        else if (input$poisson_tipus == "poisson_egyenlo")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X=x)=\\frac{\\lambda^x\\mathrm{e}^{-\\lambda}}{x!}}$$'
            )
          )
        }
        else if (input$poisson_tipus == "poisson_enagyobb")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\geq x)=\\sum_{k=x}^{\\infty}\\frac{\\lambda^k\\mathrm{e}^{-\\lambda}}{k!}}$$'
            )
          )
        }
        else
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\leq x)=\\sum_{k=0}^{x}\\frac{\\lambda^k\\mathrm{e}^{-\\lambda}}{k!}}$$'
            )
          )
        }
      })
      
      output$poisson_ex1_dynamic <- renderUI({
        if (input$poisson_tipus == "poisson_intervallum")
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Poi}(\\lambda)$$
            $$\\small{
            \\mathbf{P}(\\color{green}{%g} \\leq X  \\leq \\color{green}{%g})=
            \\sum_{k=\\color{green}{%g}}^{\\color{green}{%g}}\\frac{\\color{red}{%g}^k\\mathrm{e}^{\\color{red}{- %g}}}{k!}}$$",
              input$poisson_x_intervallum[1],
              input$poisson_x_intervallum[2],
              input$poisson_x_intervallum[1],
              input$poisson_x_intervallum[2],
              poisson_lambda(),
              poisson_lambda()
            )
          )
        else if (input$poisson_tipus == "poisson_egyenlo")
        {
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Poi}(\\lambda)$$
            $$\\small{\\mathbf{P}(X=\\color{green}{%g})=\\frac{\\color{red}{%g}^\\color{green}{%g}\\mathrm{e}^{-\\color{red}{%g}}}{\\color{green}{%g}!}}$$",
              poisson_x(),
              poisson_lambda(),
              poisson_x(),
              poisson_lambda(),
              poisson_x()
            )
          )
        }
        else if (input$poisson_tipus == "poisson_enagyobb")
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\text{Poi}(\\lambda)$$
                                $$\\small{\\mathbf{P}(X \\geq
            \\color{green}{%g})=\\sum_{k=\\color{green}{%g}}^{\\infty}\\frac{\\color{red}{%g}^k\\mathrm{e}^{-\\color{red}{%g}}}{k!}}$$',
              poisson_x(),
              poisson_x(),
              poisson_lambda(),
              poisson_lambda()
            )
          )
        }
        
        else
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\text{Poi}(\\lambda)$$
            $$\\small{\\mathbf{P}(X \\leq \\color{green}{%g})=
            \\sum_{k=0}^{\\color{green}{%g}}\\frac{\\color{red}{%g}^k\\mathrm{e}^{-\\color{red}{%g}}}{k!}}$$',
              poisson_x(),
              poisson_x(),
              poisson_lambda(),
              poisson_lambda()
            )
          )
        }
      })
      
      output$poisson_ex2 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{E}(X)=\\lambda}$$'))
      })
      
      output$poisson_ex2_dynamic <- renderUI({
        p(withMathJax(
          sprintf(
            '$$\\mathbf{E}(X)=\\color{red}{%g}$$',
            poisson_lambda()
          )
        ))
      })
      
      output$poisson_ex3 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{D}^2(X)=\\lambda}$$'))
      })
      
      output$poisson_ex3_dynamic <- renderUI({
        p(withMathJax(
          sprintf(
            '$$\\mathbf{D}^2(X)=\\color{red}{%g}$$',
            poisson_lambda()
          )
        ))
      })
    }
  }
  #Hipergeometriai eloszlás
  {
    #Változók
    {
      hipergeo_n <- reactive({
        input$hipergeo_n
      })
      hipergeo_m <- reactive({
        input$hipergeo_m
      })
      hipergeo_k <- reactive({
        input$hipergeo_k
      })
      hipergeo_x <- reactive({
        input$hipergeo_x
      })
      
      hipergeo_0x_range <- reactive({
        0:input$hipergeo_x
      })
      hipergeo_k_range <- reactive({
        0:input$hipergeo_k
      })
      
      hipergeo_x_range <- reactive({
        input$hipergeo_x_intervallum[1]:input$hipergeo_x_intervallum[2]
      })
      
      hipergeo_xk_range <- reactive({
        input$hipergeo_x:input$hipergeo_k
      })
      
    }
    #Input-ok
    {
      output$hipergeo_k_slider <- renderUI({
        sliderInput(
          "hipergeo_k",
          "$$(\\color{brown}{k})$$
          Halmazból választott minta elemszáma:",
          min = 0,
          max(hipergeo_n() + hipergeo_m()),
          value = 10,
          step = 1
        )
      })
      output$hipergeo_m_slider <- renderUI({
        sliderInput(
          "hipergeo_m",
          "$$(\\color{blue}{m})$$
          Adott tulajdonsággal rendelkező elemek száma a halmazban:",
          min = 0,
          max = 50,
          value = 45,
          step = 1
        )
      })
      output$hipergeo_n_slider <- renderUI({
        sliderInput(
          "hipergeo_n",
          "$$(\\color{red}{n})$$
          Halmazban a további elemek száma:",
          min = 0,
          max = 50,
          value = 30,
          step = 1
        )
      })
      output$hipergeo_x_intervallum_slider <- renderUI({
        sliderInput(
          "hipergeo_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
          Adja meg az intervallumot:",
          min = 0,
          max = hipergeo_k(),
          value = c(0, 10),
          step = 1
        )
      })
      output$hipergeo_x_slider <- renderUI({
        sliderInput(
          "hipergeo_x",
          "$$(\\color{green}{x})$$
          Adja meg az x értékét:",
          5,
          min = 0,
          max = hipergeo_k(),
          step = 1
        )
      })
      output$hipergeo_valsz_input <- renderUI({
        selectInput(
          "hipergeo_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X=x)$$" = "hipergeo_egyenlo",
            "$$\\mathbf{P}(X \\leq x)$$" = "hipergeo_ekisebb",
            "$$\\mathbf{P}(X \\geq x)$$" = "hipergeo_enagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)}$$" =
              "hipergeo_intervallum"
          )
        )
      })
    }
    #Eredmények
    {
      hipergeo_eredmeny <- reactive({
        if (input$hipergeo_tipus == "hipergeo_egyenlo")
          dhyper(hipergeo_x(),
                 hipergeo_m(),
                 hipergeo_n(),
                 hipergeo_k())
        
        else if (input$hipergeo_tipus == "hipergeo_ekisebb")
        {
          sum(dhyper(
            hipergeo_0x_range(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_k()
          ))
        }
        
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
        {
          sum(dhyper(
            hipergeo_xk_range(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_k()
          ))
        }
        else{
          sum(dhyper(
            hipergeo_x_range(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_k()
          ))
          
        }
      })
      
      
      
      
      hipergeo_varhato_ertek_react <-
        reactive ({
          round((hipergeo_k() * hipergeo_m()) / (hipergeo_m() + hipergeo_n()),
                digits = 5)
        })
      
      hipergeo_szoras_react <-
        reactive ({
          round((
            hipergeo_k() * hipergeo_m() * hipergeo_n() * (hipergeo_m() + hipergeo_n() - hipergeo_k())
          ) / ((hipergeo_m() + hipergeo_n()) ^ 2 * (hipergeo_m() + hipergeo_n() - 1)
          ),
          digits = 5)
        })
    }
    #Plot
    {
      output$hipergeo_plot <- renderPlotly({
        sulyfvg <-
          plot_ly(type = "bar")
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dhyper(
              hipergeo_0x_range(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
            ),
            x =  hipergeo_0x_range(),
            name = "P(X\u2264 x)",
            marker = list(color = '#00CCCC')
          )
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dhyper(
              hipergeo_xk_range(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
            ),
            x = hipergeo_xk_range(),
            name = "P(X\u2265 x)",
            marker = list(color = '#0e6655')
          )
        
        sulyfvg <-
          sulyfvg %>% add_trace(
            y = dhyper(hipergeo_x(),
                       hipergeo_m(),
                       hipergeo_n(),
                       hipergeo_k()),
            x = hipergeo_x(),
            name = "P(X= x)",
            marker = list(color = '#28b463')
            
          )
        if (input$hipergeo_tipus == "hipergeo_intervallum")
          sulyfvg <-
          sulyfvg %>% add_lines(
            y = dhyper(
              hipergeo_x_range(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
            ),
            x = hipergeo_x_range(),
            name = "P(x\u2081 \u2264 X \u2264 x\u2082)",
            line = list(shape = "spline", color = '#FF0000'),
            marker = list(color = '#FF0000'),
            fill = "tozeroy",
            fillcolor = 'rgba(255, 0, 0, 0.4)'
          )
        
        
        sulyfvg <-
          sulyfvg %>% layout(
            xaxis = list(
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            barmode = 'overlay'
          )
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = phyper(
              hipergeo_k_range(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
            ),
            x = hipergeo_k_range(),
            name = "Kumulatív valószínűség",
            line = list(shape = "hvh", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 3,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
    }
    #DT
    {
      hipergeo_dt <- reactive({
        if (input$hipergeo_tipus == "hipergeo_egyenlo")
          "P(X= x)"
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
          "P(X\u2265 x)"
        else if (input$hipergeo_tipus == "hipergeo_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      
      hipergeo_eredmeny_dt <- reactive({
        if (input$hipergeo_tipus == "hipergeo_egyenlo")
          dhyper(hipergeo_x(),
                 hipergeo_m(),
                 hipergeo_n(),
                 hipergeo_k())
        
        else if (input$hipergeo_tipus == "hipergeo_ekisebb")
        {
          dhyper(hipergeo_0x_range(),
                 hipergeo_m(),
                 hipergeo_n(),
                 hipergeo_k())
        }
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
        {
          dhyper(hipergeo_xk_range(),
                 hipergeo_m(),
                 hipergeo_n(),
                 hipergeo_k())
        }
        else{
          dhyper(hipergeo_x_range(),
                 hipergeo_m(),
                 hipergeo_n(),
                 hipergeo_k())
          
        }
      })
      
      
      hipergeo_x_dt <- reactive({
        if (input$hipergeo_tipus == "hipergeo_egyenlo")
          hipergeo_x()
        
        else if (input$hipergeo_tipus == "hipergeo_ekisebb")
        {
          hipergeo_0x_range()
        }
        
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
        {
          hipergeo_xk_range()
        }
        else{
          hipergeo_x_range()
          
        }
      })
      
      
      hipergeo_df <- reactive({
        data.frame(x = hipergeo_x_dt(),
                   y = round(hipergeo_eredmeny_dt(), digits = 5))
        
      })
      
      output$hipergeo_tabla <- renderDataTable({
        hdt <-
          datatable(
            hipergeo_df(),
            caption = hipergeo_dt(),
            colnames = c("X", "Valószínűség"),
            rownames = FALSE,
            options = list(
              lengthChange = FALSE,
              searching = FALSE,
              info = FALSE,
              language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Hungarian.json'),
              pageLength = 2
            )
          )
        
        hdt2 <-
          formatStyle(
            hdt,
            "x",
            color = '#FFFFFF',
            backgroundColor = "#006666",
            fontWeight = 'bold'
          )
        hdt3 <-
          formatStyle(
            hdt2,
            "y",
            color = '#FFFFFF',
            backgroundColor = "#009999",
            fontWeight = 'bold'
          )
      })
      
      
      
    }
    #ValueBox
    {
      output$hipergeo_box1 <- renderValueBox({
        valueBox(
          hipergeo_varhato_ertek_react(),
          uiOutput("hipergeo_ex2"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$hipergeo_box2 <- renderValueBox({
        valueBox(
          hipergeo_szoras_react(),
          uiOutput("hipergeo_ex3"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$hipergeo_box3 <- renderValueBox({
        valueBox(
          round(hipergeo_eredmeny(), digits = 5),
          uiOutput("hipergeo_ex1"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
    }
    #Képletek
    {
      output$hipergeo_tab1 <- renderUI({
        if (input$hipergeo_tipus == "hipergeo_egyenlo")
          "P(X= x)"
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
          "P(X\u2265 x)"
        else if (input$hipergeo_tipus == "hipergeo_ekisebb")
          "P(X\u2264 x)"
        else {
          "P(x\u2081 \u2264 X \u2264 x\u2082)"
        }
      })
      output$hipergeo_ex1 <- renderUI({
        if (input$hipergeo_tipus == "hipergeo_intervallum")
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)=
                \\sum_{x= \\ x_1}^{\\ x_2}\\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}}$$'
            )
          )
        else if (input$hipergeo_tipus == "hipergeo_egyenlo")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X=x) = \\frac{\\binom{m}{x}\\binom{n}{k-x}}{\\binom{m+n}{k}}}$$'
            )
          )
        }
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\geq x)=\\sum_{t=x}^{k}\\frac{\\binom{m}{t}\\binom{n}{k-t}}{\\binom{m+n}{k}}}$$'
            )
          )
        }
        else
        {
          withMathJax(
            helpText(
              '$$\\color{white}{\\mathbf{P}(X \\leq x)=\\sum_{t=0}^{x}\\frac{\\binom{m}{t}\\binom{n}{k-t}}{\\binom{m+n}{k}}}$$'
            )
          )
        }
        
      })
      
      output$hipergeo_ex1_dynamic <- renderUI({
        if (input$hipergeo_tipus == "hipergeo_intervallum")
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Hip}(n,k,m)$$
                    $$\\small{
                    \\mathbf{P}(\\color{green}{%g} \\leq X  \\leq \\color{green}{%g})=
                \\sum_{x= \\color{green}{%g}}^{\\color{green}{%g}}\\frac{\\binom{\\color{blue}{%g}}{x}\\binom{\\color{red}{%g}}
                {\\color{brown}{%g}-x}}{\\binom{\\color{blue}{%g}+\\color{red}{%g}}{\\color{brown}{%g}}}
              }$$",
              input$hipergeo_x_intervallum[1],
              input$hipergeo_x_intervallum[2],
              input$hipergeo_x_intervallum[1],
              input$hipergeo_x_intervallum[2],
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
              
            )
          )
        else if (input$hipergeo_tipus == "hipergeo_egyenlo")
        {
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Hip}(n,k,m)$$
                                        $$\\small
                    {\\mathbf{P}(X= \\color{green}{%g}) = \\frac{\\binom{\\color{blue}{%g}}{\\color{green}{%g}}\\binom{\\color{red}{%g}
              }{\\color{brown}{%g}-\\color{green}{%g}}}{\\binom{\\color{blue}{%g}+\\color{red}{%g}}{\\color{brown}{%g}}}}$$",
              hipergeo_x(),
              hipergeo_m(),
              hipergeo_x(),
              hipergeo_n(),
              hipergeo_k(),
              hipergeo_x(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
            )
          )
        }
        else if (input$hipergeo_tipus == "hipergeo_enagyobb")
        {
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Hip}(n,k,m)$$
                    $$\\small{
                    \\mathbf{P}(X \\geq \\color{green}{%g})=\\sum_{t=\\color{green}{%g}}^{\\color{brown}{%g}}\\frac{\\binom{\\color{blue}{%g}}{t}\\binom{\\color{red}{%g}}
                {\\color{brown}{%g}-t}}{\\binom{\\color{blue}{%g}+\\color{red}{%g}}{\\color{brown}{%g}}}
              }$$",
              hipergeo_x(),
              hipergeo_x(),
              hipergeo_k(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
              
            )
          )
        }
        
        else
        {
          withMathJax(
            sprintf(
              "$$X\\sim\\text{Hip}(n,k,m)$$
                    $$\\small{
                    \\mathbf{P}(X \\leq \\color{green}{%g})=\\sum_{t=0}^{\\color{green}{%g}}\\frac{\\binom{\\color{blue}{%g}}{t}\\binom{\\color{red}{%g}}
                {\\color{brown}{%g}-t}}{\\binom{\\color{blue}{%g}+\\color{red}{%g}}{\\color{brown}{%g}}}
              }$$",
              hipergeo_x(),
              hipergeo_x(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k(),
              hipergeo_m(),
              hipergeo_n(),
              hipergeo_k()
              
            )
          )
        }
        
      })
      
      
      output$hipergeo_ex2 <- renderUI({
        withMathJax(
          helpText(
            '$$\\scriptsize{\\color{white}{\\mathbf{E}(X)=\\frac{km}{m+n}}}$$'
          )
        )
      })
      
      output$hipergeo_ex2_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{E}(X)=\\frac{\\color{brown}{%g} * \\color{blue}{%g}}{\\color{blue}{%g} + \\color{red}{%g}}$$',
            hipergeo_k(),
            hipergeo_m(),
            hipergeo_m(),
            hipergeo_n()
          )
        )
      })
      
      
      output$hipergeo_ex3 <- renderUI({
        withMathJax(
          helpText(
            ' $$\\scriptsize{\\color{white}{\\mathbf{D}^2(X)=\\frac{kmn(m+n-k)}{(m+n)^2 (m+n-1)}}}$$'
          )
        )
      })
      
      output$hipergeo_ex3_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{D}^2(X)=\\frac{\\color{brown}{%g} * \\color{blue}{%g} * \\color{red}{%g}
    (\\color{blue}{%g} + \\color{red}{%g} - \\color{brown}{%g})} {(\\color{blue}{%g} + \\color{red}{%g})^2 (\\color{blue}{%g} + \\color{red}{%g} -1)}$$',
            hipergeo_k(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_k(),
            hipergeo_m(),
            hipergeo_n(),
            hipergeo_m(),
            hipergeo_n()
          )
        )
      })
      
    }
  }
  
  output$folytonos <- renderUI({
    dashboardPage(
      skin = "green",
      dashboardHeader(title = "Eloszlások"),
      dashboardSidebar(sidebarMenu(
        menuItem(
          "Egyenletes eloszlás" ,
          tabName = "f_elso",
          icon = icon("chart-line")
        ),
        menuItem(
          "Exponenciális eloszlás" ,
          tabName = "f_masodik",
          icon = icon("chart-line")
        ),
        menuItem(
          "Normális eloszlás" ,
          tabName = "f_harmadik",
          icon = icon("chart-line")
        )
      )),
      
      dashboardBody(tabItems(
        #Egyenletes eloszlás
        {
          tabItem(
            tabName = "f_elso" ,
            fluidRow(
              column(
                9,
                #CSS
                tags$style(
                  HTML(
                    "

                      #egyenletes_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
                      #egyenletes_box1 {
width: 22%
}
                      #egyenletes_box2 {
width: 23%
}
                      #egyenletes_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


                      #egyenletes_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

                      #egyenletes_box3 {width: 70%}
#folytonos .navbar-static-top {background: linear-gradient(130deg,#18bc9c 91%, #0f7864 9%)}
                      .irs-bar, .irs-bar-edge {background:   	#18bc9c}
                     .irs-single,.irs-from,.irs-to {background:   	 	#008080}
                    .nav-tabs-custom .nav-tabs li.active {border-top-color:#00CCCC}

                   .border-radius-none , .bg-teal-gradient .box-header {background: #0f7864}


                 .selectize-input {height: 42px; width: 133px}
                 .col-sm-6 .box-body
                   {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}


            .col-sm-6
                   {width: 55%}


 div.col-sm-9 div.col-sm-6 div.box {
                      border-top-color:#28b463;
                  }
                    #egyenletes_box3 {
                      border-left: 3px dashed #28b463;
                      border-bottom: 3px dashed #28b463;
                    }

                    .control-label
                    {font-family: Times New Roman}

.box-title
 {font-family: Times New Roman}
                  #egyenletes_valsz_input    .control-label
                    {
                      color: #ffffff
                  }")
                ),
                #ValueBox
                valueBoxOutput("egyenletes_box1"),
                valueBoxOutput("egyenletes_box2"),
                box(
                  valueBoxOutput("egyenletes_box3"),
                  uiOutput("egyenletes_valsz_input")
                )
              ),
              column(
                3,
                #Képletek
                tabBox(
                  width = "110%",
                  side = "right",
                  tabPanel("Várható érték", uiOutput('egyenletes_ex2_dynamic')),
                  tabPanel("Szórás", uiOutput('egyenletes_ex3_dynamic')),
                  tabPanel(
                    uiOutput("egyenletes_tab1"),
                    uiOutput('egyenletes_ex1_dynamic')
                  )
                )
              )
            ),
            
            fluidRow(
              column(
                2,
                #Sűrűségfüggvény
                box(
                  title = "Sűrűségfüggvény",
                  width = NULL,
                  solidHeader = TRUE,
                  status = "success",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  uiOutput('egyenletes_ex4_dynamic')
                ),
                
                #Paraméterek
                gradientBox(
                  width = 12,
                  title = "Paraméterek",
                  icon = "fa fa-sliders",
                  gradientColor = "teal",
                  collapsible = FALSE,
                  footer = list(
                    uiOutput("egyenletes_a_slider", inline = T),
                    uiOutput("egyenletes_b_slider", inline = T)
                    
                  )
                )
              ),
              
              column(8,
                     #Plot
                     box(
                       width = NULL,
                       status = "success",
                       plotlyOutput("egyenletes_plot")
                     )),
              
              column(
                2,
                
                
                #Paraméterek
                gradientBox(
                  width = 12,
                  title = "Paraméterek",
                  icon = "fa fa-sliders",
                  gradientColor = "teal",
                  collapsible = FALSE,
                  footer = list(
                    uiOutput("egyenletes_x_slider", inline = T),
                    uiOutput("egyenletes_x_intervallum_slider", inline = T)
                    
                    
                  )
                )
                
              )
            )
          )
        },
#Exponenciális eloszlás
{
  tabItem(
    tabName = "f_masodik",
    fluidRow(
      column(
        9,
        #CSS
        tags$style(
          HTML(
            "

                      #expo_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
                      #expo_box1 {
width: 22%
}
                      #expo_box2 {
width: 22%
}
                      #expo_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


                      #expo_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

                      #expo_box3 {width: 70%}

                       #expo_box3 {
                      border-left: 3px dashed #28b463;
                      border-bottom: 3px dashed #28b463;
                    }

                       #expo_valsz_input    .control-label
                    {
                      color: #ffffff
                  }

                  "
          )
        ),
        
        #ValueBox
        valueBoxOutput("expo_box1"),
        valueBoxOutput("expo_box2"),
        box(valueBoxOutput("expo_box3"), uiOutput("expo_valsz_input"))
      ),
      column(
        3,
        #Képletek
        tabBox(
          width = "110%",
          side = "right",
          tabPanel("Várható érték", uiOutput('expo_ex2_dynamic')),
          tabPanel("Szórás", uiOutput('expo_ex3_dynamic')),
          tabPanel(uiOutput("expo_tab1"),
                   uiOutput('expo_ex1_dynamic'))
        )
      )
    ),
    
    fluidRow(
      column(
        2,
        #Sűrűségfüggvény
        box(
          title = "Sűrűségfüggvény",
          width = NULL,
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          collapsed = TRUE,
          uiOutput('expo_ex4_dynamic')
        ),
        
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = uiOutput("expo_lambda_slider", inline = T)
          
        )
      ),
      
      column(8,
             #Plot
             box(
               width = NULL,
               status = "success",
               plotlyOutput("expo_plot"))),
      
      column(
        2,
        
        
        #Paraméterek
        gradientBox(
          width = 12,
          title = "Paraméterek",
          icon = "fa fa-sliders",
          gradientColor = "teal",
          collapsible = FALSE,
          footer = list(
            uiOutput("expo_x_slider", inline = T),
            uiOutput("expo_x_intervallum_slider", inline = T)
            
            
          )
        )
        
      )
    )
  )
},
#Normális eloszlás
{
  tabItem(tabName = "f_harmadik",
          fluidRow(
            column(
              9,
              #CSS
              tags$style(
                HTML(
                  "

	                      #norm_box1 .inner {background-image: linear-gradient(to right, #006666 , 	#009999)}
	                      #norm_box1 {
	width: 22%
	}
	                      #norm_box2 {
	width: 22%
	}
	                      #norm_box2 .inner {background-image: linear-gradient(to right, #009999 , 	#00e6e6)}


	                      #norm_box3 .inner {background-image: linear-gradient(to right, #00e6e6 , #00c0ef	)}

	                      #norm_box3 {width: 70%}

	                       #norm_box3 {
                      border - left:3px dashed #28b463;
                      border - bottom:3px dashed #28b463;
                    }

	                       #norm_valsz_input    .control-label
	                    {
color:#ffffff
	                  }

	                  "
                )
              ),
              
              
              #ValueBox
              valueBoxOutput("norm_box1"),
              valueBoxOutput("norm_box2"),
              box(valueBoxOutput("norm_box3"), uiOutput("norm_valsz_input"))
            ),
            column(
              3,
              #Képletek
              tabBox(
                width = "110%",
                side = "right",
                tabPanel("Várható érték", uiOutput('norm_ex2_dynamic')),
                tabPanel("Szórás", uiOutput('norm_ex3_dynamic')),
                tabPanel(uiOutput("norm_tab1"),
                         uiOutput('norm_ex1_dynamic'))
              )
            )
          ),
          
          fluidRow(
            column(
              2,
              #Sűrűségfüggvény
              box(
                title = "Sűrűségfüggvény",
                width = NULL,
                solidHeader = TRUE,
                status = "success",
                collapsible = TRUE,
                collapsed = TRUE,
                uiOutput('norm_ex4_dynamic')
              ),
              
              #Paraméterek
              gradientBox(
                width = 12,
                title = "Paraméterek",
                icon = "fa fa-sliders",
                gradientColor = "teal",
                collapsible = FALSE,
                footer = list(
                  uiOutput("norm_mu_slider", inline = T),
                  uiOutput("norm_szor_slider", inline = T)
                )
              )
            ),
            
            column(8,
                   #Plot
                   box(
                     width = NULL,
                     status = "success",
                     plotlyOutput("norm_plot")
                   )),
            
            column(
              2,
              
              
              #Paraméterek
              gradientBox(
                width = 12,
                title = "Paraméterek",
                icon = "fa fa-sliders",
                gradientColor = "teal",
                collapsible = FALSE,
                footer = list(
                  uiOutput("norm_x_slider", inline = T),
                  uiOutput("norm_x_intervallum_slider", inline = T)
                  
                  
                )
              )
              
            )
          ))
}))
    )
    
  })
  
  #Egyenletes eloszlás
  {
    #Változók
    {
      egyenletes_plot_range <- reactive ({
        dunif(seq(egyenletes_a(), egyenletes_b(), 0.01),
              egyenletes_a(),
              egyenletes_b())
      })
      
      
      egyenletes_x1_intervallum <- reactive({
        input$egyenletes_x_intervallum[1]
      })
      
      
      
      egyenletes_x2_intervallum <- reactive({
        input$egyenletes_x_intervallum[2]
      })
      
      
      egyenletes_a <-
        reactive({
          input$egyenletes_a
        })
      egyenletes_b <-
        reactive({
          input$egyenletes_b
        })
      egyenletes_x <-
        reactive({
          input$egyenletes_x
        })
      
      egyenletes_ab_range <- reactive({
        input$egyenletes_a:input$egyenletes_b
      })
      
    }
    #Input-ok
    {
      output$egyenletes_a_slider <- renderUI({
        sliderInput(
          "egyenletes_a",
          "$$(\\color{red}{a})$$
        Adja meg a minimumot:",
          min = 0,
          max = 50,
          value = 1,
          step = 1
        )
        
      })
      output$egyenletes_b_slider <- renderUI({
        sliderInput(
          "egyenletes_b",
          "$$(\\color{blue}{b})$$
        Adja meg a maximumot:",
          min = 0,
          max = 50,
          value = 15,
          step = 1
        )
      })
      
      output$egyenletes_x_slider <- renderUI({
        sliderInput(
          "egyenletes_x",
          "$$(\\color{green}{x})$$
      Adja meg az x értékét:",
          min = egyenletes_a(),
          max = egyenletes_b(),
          value = 5,
          step = 1
        )
      })
      
      output$egyenletes_x_intervallum_slider <- renderUI({
        sliderInput(
          "egyenletes_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
          Adja meg az intervallumot:",
          min = egyenletes_a(),
          max = egyenletes_b(),
          value = c(5, 10),
          step = 1
        )
        
        
      })
      output$egyenletes_valsz_input <- renderUI({
        selectInput(
          "egyenletes_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X<x)$$" = "egyenletes_kisebb",
            "$$\\mathbf{P}(X>x)$$" = "egyenletes_nagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 < X < \\ x_2)}$$" = "egyenletes_intervallum"
            
          )
        )
        
        
      })
      
    }
    #Eredmények
    {
      egyenletes_eredmeny <- reactive({
        if (input$egyenletes_tipus == "egyenletes_kisebb") {
          (egyenletes_x() - egyenletes_a()) / (egyenletes_b() - egyenletes_a())
        }
        
        else if (input$egyenletes_tipus == "egyenletes_nagyobb") {
          (egyenletes_b() - egyenletes_x()) / (egyenletes_b() - egyenletes_a())
        }
        else{
          (egyenletes_x2_intervallum() - egyenletes_x1_intervallum()) / (egyenletes_b() -
                                                                           egyenletes_a())
        }
        
        
      })
      
      
      egyenletes_varhato_ertek_react <-
        
        reactive ({
          if (egyenletes_a() < egyenletes_b())
            round((input$egyenletes_a + input$egyenletes_b) / 2, digits = 2)
          else{
            0
          }
        })
      
      
      egyenletes_szoras_react <-
        reactive ({
          if (egyenletes_a() < egyenletes_b())
            round((input$egyenletes_a - input$egyenletes_b) ^ 2 / 12,
                  digits = 2)
          else{
            0
          }
        })
      
      egyenletes_fx_react <-
        reactive ({
          if (egyenletes_a() < egyenletes_b())
            round(dunif(egyenletes_x(), egyenletes_a(), egyenletes_b()),
                  digits = 2)
          else{
            0
          }
        })
      
    }
    #Plot
    {
      #P(X>x)
      {
        egyenletes_1 <-
          reactive ({
            c(egyenletes_a(),
              seq(egyenletes_a(), egyenletes_x(), 0.01),
              egyenletes_x())
          })
        egyenletes_2 <-
          reactive ({
            c(0, dunif(
              seq(egyenletes_a(), egyenletes_x(), 0.01),
              egyenletes_a(),
              egyenletes_b()
            ), 0)
          })
      }
      #P(X<x)
      {
        egyenletes_3 <-
          reactive ({
            c(egyenletes_x(),
              seq(egyenletes_x(), egyenletes_b(), 0.01),
              egyenletes_b())
          })
        egyenletes_4 <-
          reactive ({
            c(0, dunif(
              seq(egyenletes_x(), egyenletes_b(), 0.01),
              egyenletes_a(),
              egyenletes_b()
            ), 0)
          })
      }
      #P(x1<X<x2)
      {
        egyenletes_5 <-
          reactive ({
            c(
              input$egyenletes_x_intervallum[1],
              seq(
                input$egyenletes_x_intervallum[1],
                input$egyenletes_x_intervallum[2],
                0.01
              ),
              input$egyenletes_x_intervallum[2]
            )
          })
        egyenletes_6 <-
          reactive ({
            c(0, dunif(
              seq(
                input$egyenletes_x_intervallum[1],
                input$egyenletes_x_intervallum[2],
                0.01
              ),
              egyenletes_a(),
              egyenletes_b()
            ), 0)
          })
        
      }
      
      
      
      
      
      
      
      output$egyenletes_plot <- renderPlotly({
        sulyfvg <- plot_ly()
        
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            egyenletes_1(),
            egyenletes_2(),
            fillcolor = 'rgba(14, 102, 85, 0.75',
            line = list(width = 4, color = 'rgb(14, 102, 85)'),
            name = "P(X> x)"
          )
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            egyenletes_3(),
            egyenletes_4(),
            fillcolor = 'rgba(0, 204, 204, 0.75',
            line = list(width = 4, color = 'rgb(0, 204, 204)'),
            name = "P(X< x)"
          )
        
        if (input$egyenletes_tipus == "egyenletes_intervallum")
          sulyfvg <-
          sulyfvg %>% add_polygons(
            egyenletes_5(),
            egyenletes_6(),
            fillcolor = 'rgba(245, 66, 69, 0.75)',
            line = list(
              width = 1,
              color = 'rgb(255,0, 0)',
              dash = "dash"
            ),
            name = "P(x\u2081 < X < x\u2082)"
          )
        
        sulyfvg <-
          sulyfvg %>% layout(
            yaxis = list(
              range = c(0, max(egyenletes_plot_range()) + 0.03),
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            
            xaxis = list(
              range = c(egyenletes_a() - 0.5, egyenletes_b() + 0.5),
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5
            )
          )
        
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = punif(egyenletes_ab_range(),
                      egyenletes_a(),
                      egyenletes_b()),
            x = egyenletes_ab_range(),
            name = "Kumulatív valószínűség",
            line = list(shape = "spline", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              range = c(0, egyenletes_b()),
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
      
    }
    #ValueBox
    {
      output$egyenletes_box1 <- renderValueBox({
        valueBox(
          egyenletes_varhato_ertek_react(),
          icon = icon(" fa-calculator"),
          uiOutput("egyenletes_ex2"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$egyenletes_box2 <- renderValueBox({
        valueBox(
          egyenletes_szoras_react(),
          uiOutput("egyenletes_ex3"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$egyenletes_box3 <- renderValueBox({
        valueBox(
          round(egyenletes_eredmeny(), digits = 5),
          uiOutput("egyenletes_ex1"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      
    }
    #Képletek
    {
      output$egyenletes_tab1 <- renderUI({
        if (input$egyenletes_tipus == "egyenletes_nagyobb")
          "P(X> x)"
        else if (input$egyenletes_tipus == "egyenletes_kisebb")
          "P(X< x)"
        else {
          "P(x\u2081 < X < x\u2082)"
        }
      })
      
      output$egyenletes_ex1 <- renderUI({
        if (input$egyenletes_tipus == "egyenletes_kisebb" &&
            egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X<x)={\\int_{\\small a}^{\\small x}}f(x)dt={\\frac{x-a}{b-a}}}}$$'
            )
          )
        }
        else if (input$egyenletes_tipus == "egyenletes_nagyobb" &&
                 egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X>x)={\\int_{\\small x}^{\\small b}}f(x)dt={\\frac{b-x}{b-a}}}}$$'
            )
          )
        }
        else if (input$egyenletes_tipus == "egyenletes_intervallum" &&
                 egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
  \\mathbf{P}(a \\leq \\ X_1 < x  < \\ X_2 \\leq  b)=\\frac{\\ X_2 - \\ X_1}{b-a}}}$$'
            )
          )
        }
        
        else {
          p("Hiba", style = "color:red")
        }
        
      })
      
      output$egyenletes_ex1_dynamic <- renderUI({
        if (input$egyenletes_tipus == "egyenletes_nagyobb" &&
            egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\mathcal{U}(a,b)$$
                    $$\\small{\\mathbf{P}(X>\\color{green}{%g})={\\int_{\\small \\color{green}{%g}}^{
              \\small \\color{blue}{%g}}}f(\\color{green}{%g})dt={\\frac{\\color{blue}{%g}-\\color{green}{%g}}{\\color{blue}{%g}-\\color{red}{%g}}}}$$',
              egyenletes_x(),
              egyenletes_x(),
              egyenletes_b(),
              egyenletes_x(),
              egyenletes_b(),
              egyenletes_x(),
              egyenletes_b(),
              egyenletes_a()
            )
          )
        }
        else if (input$egyenletes_tipus == "egyenletes_kisebb" &&
                 egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\mathcal{U}(a,b)$$
                    $$\\small{\\mathbf{P}(X<\\color{green}{%g})={\\int_{\\small \\color{red}{%g}}^{\\small \\color{green}{%g}}}f(\\color{green}{%g})dt={\\frac{\\color{green}{%g}-\\color{red}{%g}}{\\color{blue}{%g}-\\color{red}{%g}}}}$$',
              egyenletes_x(),
              egyenletes_a(),
              egyenletes_x(),
              egyenletes_x(),
              egyenletes_x(),
              egyenletes_a(),
              egyenletes_b(),
              egyenletes_a()
            )
          )
        }
        
        else if (input$egyenletes_tipus == "egyenletes_intervallum" &&
                 egyenletes_a() < egyenletes_b())
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\mathcal{U}(a,b)$$
                    $$\\small{\\mathbf{P}(\\color{red}{%g} \\leq \\color{green}{%g} < x <
              \\color{green}{%g} \\leq \\color{blue}{%g})=\\frac{\\color{green}{%g} - \\ \\color{green}{%g}}{\\color{blue}{%g}-\\color{red}{%g}}}$$',
              egyenletes_a(),
              input$egyenletes_x_intervallum[1],
              input$egyenletes_x_intervallum[2],
              egyenletes_b(),
              input$egyenletes_x_intervallum[2],
              input$egyenletes_x_intervallum[1],
              egyenletes_b(),
              egyenletes_a()
            )
          )
        }
        else {
          p("A minimum és a maximum értéke nem megfelelő!", style = "color:red")
        }
        
      })
      
      
      output$egyenletes_ex2 <- renderUI({
        if (egyenletes_a() < egyenletes_b())
          withMathJax(helpText('$$\\color{white}{\\mathbf{E}(X)=\\dfrac{a+b}{2}}$$'))
        else{
          p("Hiba", style = "color:red")
        }
      })
      
      output$egyenletes_ex2_dynamic <- renderUI({
        if (egyenletes_a() < egyenletes_b())
          withMathJax(
            sprintf(
              '$$\\mathbf{E}(X)=\\dfrac{\\color{red}{%g}+\\color{blue}{%g}}{2}$$',
              egyenletes_a(),
              egyenletes_b()
            )
          )
        else{
          p("A minimum és a maximum értéke nem megfelelő!", style = "color:red")
        }
      })
      
      output$egyenletes_ex3 <- renderUI({
        if (egyenletes_a() < egyenletes_b())
          withMathJax(helpText(
            '$$\\color{white}{\\mathbf{D}^2(X)=\\dfrac{(b-a)^2}{12}}$$'
          ))
        else{
          p("Hiba", style = "color:red")
        }
      })
      
      output$egyenletes_ex3_dynamic <- renderUI({
        if (egyenletes_a() < egyenletes_b())
          withMathJax(
            sprintf(
              '$$\\mathbf{D}^2(X)=\\dfrac{(\\color{blue}{%g}-\\color{red}{%g})^2}{12}$$',
              egyenletes_b(),
              egyenletes_a()
            )
          )
        else{
          p("A minimum és a maximum értéke nem megfelelő!", style = "color:red")
        }
      })
      
      
      
      
      output$egyenletes_ex4_dynamic <- renderUI({
        if (input$egyenletes_a >= input$egyenletes_b)
          
          
          p("A minimum és a maximum értéke nem megfelelő!", style = "color:red")
        
        else{
          withMathJax(
            helpText(
              '$$\\scriptsize{f(x)=\\begin{cases}
  \\frac{1}{b - a} &\\text{ha}\\ a \\le x \\le b, \\\\
  0 & \\text{egyébként}
  \\end{cases}}$$'
              
            ),
            sprintf(
              '$$X\\sim\\mathcal{U}(a,b)$$
            $$x= %g $$
          $$\\scriptsize{f(\\color{green}{%g})=\\begin{cases}
  \\frac{1}{\\color{blue}{%g} - \\color{red}{%g}} &\\text{ha}\\ \\color{red}{%g} \\le \\color{green}{%g} \\le \\color{blue}{%g}, \\\\
  0 & \\text{egyébként}
  \\end{cases}}$$
            $$\\small{\\frac{1}{\\color{blue}{%g} - \\color{red}{%g}}=\\underline{%g}}$$',
              egyenletes_x(),
              egyenletes_x(),
              egyenletes_b(),
              egyenletes_a(),
              egyenletes_a(),
              egyenletes_x(),
              egyenletes_b(),
              egyenletes_b(),
              egyenletes_a(),
              egyenletes_fx_react()
            )
            
          )
        }
      })
    }
  }
  #Exponenciális eloszlás
  {
    #Változók
    {
      expo_x <- reactive({
        input$expo_x
      })
      expo_lambda <- reactive({
        input$expo_lambda
      })
      
      expo_range1 <-
        reactive ({
          seq(0,
              expo_varhato_ertek_react() + 5 * expo_plot_szoras(),
              0.01)
        })
      
      expo_plot_szoras <-
        reactive ({
          sqrt(1 / (input$expo_lambda) ^ 2)
        })
      
    }
    #Input-ok
    {
      output$expo_x_slider <- renderUI({
        sliderInput(
          "expo_x",
          "$$(\\color{green}{x})$$
      Adja meg az x értékét:",
          min = 0,
          max = max(expo_range1()),
          value = 5,
          step = 0.1
        )
      })
      
      output$expo_x_intervallum_slider <- renderUI({
        sliderInput(
          "expo_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
          Adja meg az intervallumot:",
          min = 0,
          max = max(expo_range1()),
          value = c(5, 10),
          step = 0.1
        )
        
        
      })
      
      output$expo_lambda_slider <- renderUI({
        sliderInput(
          "expo_lambda",
          "$$(\\color{red}{\\lambda})$$
      Adja meg a lambda értékét:",
          min = 0.1,
          max = 5,
          value = 0.5,
          step = 0.1
        )
      })
      output$expo_valsz_input <- renderUI({
        selectInput(
          "expo_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X<x)$$" = "expo_kisebb",
            "$$\\mathbf{P}(X>x)$$" = "expo_nagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 < X < \\ x_2)}$$" = "expo_intervallum"
            
          )
        )
        
      })
    }
    #Eredmények
    {
      expo_eredmeny <- reactive({
        if (input$expo_tipus == "expo_kisebb") {
          pexp(expo_x(), expo_lambda())
        }
        
        else if (input$expo_tipus == "expo_nagyobb") {
          1 - pexp(expo_x(), expo_lambda())
        }
        else{
          pexp(input$expo_x_intervallum[2], expo_lambda()) - pexp(input$expo_x_intervallum[1], expo_lambda())
        }
      })
      expo_fx_eredmeny  <- reactive({
        dexp(expo_x(), expo_lambda())
        
        
      })
      
      expo_varhato_ertek_react <-
        reactive ({
          1 / input$expo_lambda
        })
      
      expo_szoras_react <-
        reactive ({
          1 / (input$expo_lambda) ^ 2
        })
      
    }
    #Plot
    {
      #P(X<x)
      {
        expo_1 <-
          reactive ({
            c(0, seq(min(expo_range1()), expo_x(), 0.01), expo_x())
          })
        expo_2 <-
          reactive ({
            c(0, dexp(seq(
              min(expo_range1()), expo_x(), 0.01
            ),
            expo_lambda()), 0)
          })
      }
      #P(X>x)
      {
        expo_3 <-
          reactive ({
            c(expo_x(),
              seq(expo_x(), max(expo_range1()), 0.01),
              max(expo_range1()))
          })
        expo_4 <-
          reactive ({
            c(0, dexp(seq(
              expo_x(), max(expo_range1()), 0.01
            ),
            expo_lambda()), 0)
          })
      }
      #P(x1<X<x2)
      {
        expo_5 <-
          reactive ({
            c(
              input$expo_x_intervallum[1],
              seq(
                input$expo_x_intervallum[1],
                input$expo_x_intervallum[2],
                0.01
              ),
              input$expo_x_intervallum[2]
            )
          })
        expo_6 <-
          reactive ({
            c(0, dexp(
              seq(
                input$expo_x_intervallum[1],
                input$expo_x_intervallum[2],
                0.01
              ),
              expo_lambda()
            ), 0)
          })
        
      }
      
      
      
      output$expo_plot <- renderPlotly({
        sulyfvg <- plot_ly()
        
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            expo_1(),
            expo_2(),
            fillcolor = 'rgba(14, 102, 85, 0.75',
            line = list(width = 4, color = 'rgb(14, 102, 85)'),
            name = "P(X< x)"
          )
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            expo_3(),
            expo_4(),
            fillcolor = 'rgba(0, 204, 204, 0.75',
            line = list(width = 4, color = 'rgb(0, 204, 204)'),
            name = "P(X> x)"
          )
        
        if (input$expo_tipus == "expo_intervallum")
          sulyfvg <-
          sulyfvg %>% add_polygons(
            expo_5(),
            expo_6(),
            fillcolor = 'rgba(245, 66, 69, 0.75)',
            line = list(
              width = 1,
              color = 'rgb(255,0, 0)',
              dash = "dash"
            ),
            name = "P(x\u2081 < X < x\u2082)"
          )
        
        sulyfvg <-
          sulyfvg %>% layout(
            yaxis = list(
              range = c(0, max(dexp(
                expo_range1(), expo_lambda()
              )) + 0.1),
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            
            xaxis = list(
              range = expo_range1(),
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5
            )
          )
        
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = pexp(0:max(expo_range1()), input$expo_lambda),
            x = 0:max(expo_range1()),
            name = "Kumulatív valószínűség",
            line = list(shape = "spline", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              range = c(0:max(expo_range1())),
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
      
      
    }
    #ValueBox
    {
      output$expo_box1 <- renderValueBox({
        valueBox(
          round(expo_varhato_ertek_react(), digits = 2),
          icon = icon(" fa-calculator"),
          uiOutput("expo_ex2"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$expo_box2 <- renderValueBox({
        valueBox(
          round(expo_szoras_react(), digits = 2),
          uiOutput("expo_ex3"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$expo_box3 <- renderValueBox({
        valueBox(
          round(expo_eredmeny(), digits = 5),
          uiOutput("expo_ex1"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      
      
    }
    #Képletek
    {
      output$expo_tab1 <- renderUI({
        if (input$expo_tipus == "expo_nagyobb")
          "P(X> x)"
        else if (input$expo_tipus == "expo_kisebb")
          "P(X< x)"
        else {
          "P(x\u2081 < X < x\u2082)"
        }
      })
      
      output$expo_ex1 <- renderUI({
        if (input$expo_tipus == "expo_kisebb")
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X<x)={\\int_{\\small 0}^{\\small x}}f(x,\\lambda)dt={\\large e^{-\\lambda x}}}}$$'
            )
          )
        }
        else if (input$expo_tipus == "expo_nagyobb")
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X>x)={\\int_{\\small x}^{\\small \\infty}}f(x,\\lambda)dt={\\large 1-e^{-\\lambda x}}}}$$'
            )
          )
        }
        else
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(\\ x_1 < X < \\ x_2)=e^{-\\lambda \\ x_2}-e^{-\\lambda \\ x_1}}}$$'
            )
          )
        }
        
      })
      
      output$expo_ex1_dynamic <- renderUI({
        if (input$expo_tipus == "expo_kisebb")
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\text{Exp}(\\lambda)$$
   $$\\small{\\mathbf{P}(X<\\color{green}{%g})={\\int_{\\small 0}^{\\small \\color{green}{%g} }}f(\\color{green}{%g},\\color{red}{%g})dt={\\large e^{-\\color{red}{%g} * \\color{green}{%g}}}}$$',
              expo_x(),
              expo_x(),
              expo_x(),
              input$expo_lambda,
              input$expo_lambda,
              expo_x()
              
            )
          )
        }
        else if (input$expo_tipus == "expo_nagyobb")
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\text{Exp}(\\lambda)$$
   $$\\small{\\mathbf{P}(X<\\color{green}{%g})={\\int_{\\small 0}^{\\small \\color{green}{%g} }}f(\\color{green}{%g},\\color{red}{%g})dt={\\large 1-e^{-\\color{red}{%g} * \\color{green}{%g}}}}$$',
              expo_x(),
              expo_x(),
              expo_x(),
              input$expo_lambda,
              input$expo_lambda,
              expo_x()
              
            )
          )
        }
        else
        {
          withMathJax(
            sprintf(
              '$$X\\sim\\text{Exp}(\\lambda)$$
   $$\\small{\\mathbf{P}(\\color{green}{%g} < X < \\ \\color{green}{%g})=e^{-\\color{red}{%g}*\\color{green}{%g}}-e^{-\\color{red}{%g}*\\color{green}{%g}}}$$',
              input$expo_x_intervallum[1],
              input$expo_x_intervallum[2],
              input$expo_lambda,
              input$expo_x_intervallum[2],
              input$expo_lambda,
              input$expo_x_intervallum[1]
              
            )
          )
        }
        
      })
      
      
      output$expo_ex2 <- renderUI({
        withMathJax(helpText(
          ' $$\\color{white}{\\mathbf{E}(X)=\\dfrac{1}{\\lambda}}$$'
        ))
      })
      output$expo_ex2_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{E}(X)=\\dfrac{1}{\\color{red}{%g}}$$',
            input$expo_lambda
          )
        )
      })
      
      
      output$expo_ex3 <- renderUI({
        withMathJax(helpText(
          '$$\\color{white}{\\mathbf{D}^2(X)=\\dfrac{1}{\\lambda^2}}$$'
        ))
      })
      output$expo_ex3_dynamic <- renderUI({
        withMathJax(
          sprintf(
            '$$\\mathbf{D}^2(X)=\\dfrac{1}{\\color{red}{%g}^2}$$',
            input$expo_lambda
          )
        )
      })
      
      
      
      
      
      output$expo_ex4_dynamic <- renderUI({
        withMathJax(
          helpText(
            '$$\\scriptsize{f(x)=\\begin{cases}
  \\lambda \\mathrm{e}^{-\\lambda x}, &\\text{ha } x \\ge 0\\\\
  0 & \\text{egyébként}
  \\end{cases}}$$'
            
          ),
          sprintf(
            '$$X\\sim\\text{Exp}(\\lambda)$$
            $$x= %g $$
          $$\\scriptsize{f(\\color{green}{%g})=\\begin{cases}
  \\color{red}{%g} * \\mathrm{e}^{-\\color{red}{%g} * \\color{green}{%g}}, &\\text{ha } \\color{green}{%g} \\ge 0\\\\
  0 & \\text{egyébként}
  \\end{cases}}$$
            $$\\small{\\color{red}{%g} * \\mathrm{e}^{-\\color{red}{%g} * \\color{green}{%g}}=\\underline{%g}}$$',
            expo_x(),
            expo_x(),
            input$expo_lambda,
            input$expo_lambda,
            expo_x(),
            expo_x(),
            input$expo_lambda,
            input$expo_lambda,
            expo_x(),
            expo_fx_eredmeny()
          )
          
        )
        
      })
      
      
    }
    
  }
  #Normális eloszlás
  {
    #Változók
    {
      norm_mu <- reactive({
        input$norm_mu
      })
      norm_szor <- reactive({
        input$norm_szor
      })
      norm_x <- reactive({
        input$norm_x
      })
    }
    #Input-ok
    {
      output$norm_mu_slider <- renderUI({
        sliderInput(
          "norm_mu",
          "$$(\\color{blue}{\\mu})$$
        Adja meg a sokasági átlag értékét:",
          min = -10,
          max = 10,
          value = 0,
          step = 0.5
        )
      })
      
      output$norm_szor_slider <- renderUI({
        sliderInput(
          "norm_szor",
          "$$(\\color{red}{\\sigma})$$
        Adja meg a sokasági szórás értékét:",
          min = 0.1,
          max = 10,
          value = 0.5,
          step = 0.1
        )
        
      })
      
      
      output$norm_x_slider <- renderUI({
        sliderInput(
          "norm_x",
          "$$(\\color{green}{x})$$
      Adja meg az x értékét:",
          min =
            norm_plot_range3(),
          max = norm_plot_range2(),
          value = 0,
          step = 1
        )
      })
      
      output$norm_x_intervallum_slider <- renderUI({
        sliderInput(
          "norm_x_intervallum",
          "$$(\\color{green}{\\ x_1,\\ x_2})$$
          Adja meg az intervallumot:",
          min =
            norm_plot_range3(),
          max = norm_plot_range2(),
          value = c(-1, 1),
          step = 1
        )
      })
      
      output$norm_valsz_input <- renderUI({
        selectInput(
          "norm_tipus",
          "Valószínűségi típus:",
          c(
            "$$\\mathbf{P}(X<x)$$" = "norm_kisebb",
            "$$\\mathbf{P}(X>x)$$" = "norm_nagyobb",
            "$$\\small{\\mathbf{P}(\\ x_1 < X < \\ x_2)}$$" = "norm_intervallum"
            
          )
        )
        
      })
      
      
    }
    #Eredmények
    {
      norm_fx_eredmeny  <- reactive({
        dnorm(norm_x(), norm_mu(), norm_szor())
        
        
      })
      
      norm_varhato_ertek_react <- reactive ({
        norm_mu()
      })
      
      norm_szoras_react <-
        reactive ({
          norm_szor() ^ 2
        })
      
      norm_eredmeny <- reactive({
        if (input$norm_tipus == "norm_nagyobb") {
          1 - pnorm(norm_x(),
                    norm_mu(),
                    norm_szor())
          
          
        }
        
        else if (input$norm_tipus == "norm_kisebb") {
          pnorm(norm_x(),
                norm_mu(),
                norm_szor())
          
        }
        else{
          pnorm(input$norm_x_intervallum[2], norm_mu(), norm_szor()) -
            pnorm(input$norm_x_intervallum[1], norm_mu(), norm_szor())
          
        }
        
      })
      
    }
    #Plot
    {
      norm_plot_range <- reactive({
        seq(norm_plot_range3(), norm_plot_range2(), 0.01)
      })
      norm_plot_range2 <- reactive({
        norm_mu() + 4 * norm_szor()
      })
      norm_plot_range3 <- reactive({
        norm_mu() - 4 * norm_szor()
      })
      #P(X<x)
      {
        norm_1 <-
          reactive ({
            c(0, seq(min(norm_plot_range()), norm_x(), 0.01), norm_x())
          })
        norm_2 <-
          reactive ({
            c(0, dnorm(seq(
              min(norm_plot_range()), norm_x(), 0.01
            ),
            norm_mu(), norm_szor()), 0)
          })
      }
      #P(X>x)
      {
        norm_3 <-
          reactive ({
            c(norm_x(),
              seq(norm_x(), max(norm_plot_range()), 0.01),
              norm_plot_range2())
          })
        norm_4 <-
          reactive ({
            c(0, dnorm(seq(
              norm_x(), max(norm_plot_range()), 0.01
            ),
            norm_mu(), norm_szor()), 0)
          })
      }
      
      #P(x1<X<x2)
      {
        norm_5 <-
          reactive ({
            c(
              input$norm_x_intervallum[1],
              seq(
                input$norm_x_intervallum[1],
                input$norm_x_intervallum[2],
                0.01
              ),
              input$norm_x_intervallum[2]
            )
          })
        norm_6 <-
          reactive ({
            c(0, dnorm(
              seq(
                input$norm_x_intervallum[1],
                input$norm_x_intervallum[2],
                0.01
              ),
              norm_mu(),
              norm_szor()
            ), 0)
          })
        
      }
      
      
      
      
      output$norm_plot <- renderPlotly({
        sulyfvg <- plot_ly()
        
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            norm_1(),
            norm_2(),
            fillcolor = 'rgba(14, 102, 85, 0.75',
            line = list(width = 4, color = 'rgb(14, 102, 85)'),
            name = "P(X< x)"
          )
        
        sulyfvg <-
          sulyfvg %>% add_polygons(
            norm_3(),
            norm_4(),
            fillcolor = 'rgba(0, 204, 204, 0.75',
            line = list(width = 4, color = 'rgb(0, 204, 204)'),
            name = "P(X> x)"
          )
        
        if (input$norm_tipus == "norm_intervallum")
          sulyfvg <-
          sulyfvg %>% add_polygons(
            norm_5(),
            norm_6(),
            fillcolor = 'rgba(245, 66, 69, 0.75)',
            line = list(
              width = 1,
              color = 'rgb(255,0, 0)',
              dash = "dash"
            ),
            name = "P(x\u2081 < X < x\u2082)"
          )
        
        
        sulyfvg <-
          sulyfvg %>% layout(
            yaxis = list(
              range = c(0, max(
                dnorm(norm_plot_range(), norm_mu(), norm_szor())
              ) + 0.1),
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              showline = TRUE,
              gridwidth = 10,
              linewidth = 2.5
            ),
            
            xaxis = list(
              range = norm_plot_range(),
              ticks = "outside",
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5
            )
          )
        
        eloszlasfvg <-
          
          plot_ly()
        
        eloszlasfvg <-
          eloszlasfvg %>% add_lines(
            y = pnorm(norm_plot_range(), norm_mu(), norm_szor()),
            x = norm_plot_range(),
            name = "Kumulatív valószínűség",
            line = list(shape = "spline", color = '#FF0000'),
            showlegend = FALSE
          )
        
        
        eloszlasfvg <-
          eloszlasfvg %>% layout(
            xaxis = list(
              range = norm_plot_range(),
              tickwidth = 2,
              tickcolor = "#000000",
              dtick = 2,
              linewidth = 2.5,
              showspikes = T,
              ticks = "outside",
              showgrid = FALSE
            ),
            yaxis = list(
              ticks = "outside",
              tickwidth = 1,
              tickcolor = "#000000",
              gridwidth = 10,
              linewidth = 2.5,
              showspikes = T,
              zeroline = F,
              rangemode = "tozero"
            )
          )
        
        sp <-
          subplot(sulyfvg,
                  eloszlasfvg)
        
        sp %>% layout(annotations = list(
          list(
            x = 0.2,
            y = 1.05,
            text = "Súlyfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          ),
          list(
            x = 0.8,
            y = 1.05,
            text = "Eloszlásfüggvény",
            showarrow = F,
            xref = 'paper',
            yref = 'paper'
          )
        ))
      })
      
      
      
      
    }
    #ValueBox
    {
      output$norm_box1 <- renderValueBox({
        valueBox(
          round(norm_varhato_ertek_react(), digits = 2),
          icon = icon(" fa-calculator"),
          uiOutput("norm_ex2"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$norm_box2 <- renderValueBox({
        valueBox(
          round(norm_szoras_react(), digits = 2),
          uiOutput("norm_ex3"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
      output$norm_box3 <- renderValueBox({
        valueBox(
          round(norm_eredmeny(), digits = 5),
          uiOutput("norm_ex1"),
          icon = icon(" fa-calculator"),
          color = "aqua",
          href = NULL
        )
      })
      
    }
    #Képletek
    {
      output$norm_tab1 <- renderUI({
        if (input$norm_tipus == "norm_nagyobb")
          "P(X> x)"
        else if (input$norm_tipus == "norm_kisebb")
          "P(X< x)"
        else {
          "P(x\u2081 < X < x\u2082)"
        }
      })
      
      output$norm_ex1 <- renderUI({
        if (input$norm_tipus == "norm_kisebb")
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X<x)={\\large\\int_{\\small-\\infty}^{\\small x}}f(x,\\mu,\\sigma)dt}}$$'
            )
          )
        }
        else if (input$norm_tipus == "norm_nagyobb")
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(X>x)={\\large\\int_{\\small x}^{\\small\\infty}}f(x,\\mu,\\sigma)dt}}$$'
            )
          )
        }
        else
        {
          withMathJax(
            helpText(
              '$$\\small{\\color{white}{
   \\mathbf{P}(\\ x_1 < X < \\ x_2)=\\mathbf{P}(X< \\ x_2)-\\mathbf{P}(X< \\ x_1)}}$$'
            )
          )
        }
        
      })
      
      output$norm_ex1_dynamic <- renderUI({
        if (input$norm_tipus == "norm_kisebb")
        {
          withMathJax(
            sprintf(
              '$$X\\sim \\mathcal{N}(\\mu,\\sigma)$$
   $$\\small{ \\mathbf{P}(X<\\color{green}{%g})={\\large\\int_{\\small-\\infty}^{\\small \\color{green}{%g}}}f(\\color{green}{%g},\\color{blue}{%g},\\color{red}{%g})dt}$$',
              norm_x(),
              norm_x(),
              norm_x(),
              norm_mu(),
              norm_szor()
            )
          )
        }
        else if (input$norm_tipus == "norm_nagyobb")
        {
          withMathJax(
            sprintf(
              '$$X\\sim \\mathcal{N}(\\mu,\\sigma)$$
   $$\\small{\\mathbf{P}(X>\\color{green}{%g})={\\large\\int_{\\small \\color{green}{%g}}^{\\small\\infty}}f(\\color{green}{%g},\\color{blue}{%g},\\color{red}{%g})dt}$$',
              norm_x(),
              norm_x(),
              norm_x(),
              norm_mu(),
              norm_szor()
            )
          )
        }
        else
        {
          withMathJax(
            sprintf(
              '$$X\\sim \\mathcal{N}(\\mu,\\sigma)$$
   $$\\small{\\mathbf{P}(\\color{green}{%g} < X < \\color{green}{%g})=\\mathbf{P}(X< \\color{green}{%g})-\\mathbf{P}(X< \\color{green}{%g})}$$',
              input$norm_x_intervallum[1],
              input$norm_x_intervallum[2],
              input$norm_x_intervallum[2],
              input$norm_x_intervallum[1]
            )
          )
        }
        
      })
      
      output$norm_ex2 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{E}(X)= \\mu }$$'))
      })
      
      output$norm_ex2_dynamic <- renderUI({
        withMathJax(sprintf('$$\\mathbf{E}(X)= \\color{blue}{%g} $$',
                            norm_mu()))
      })
      
      output$norm_ex3 <- renderUI({
        withMathJax(helpText('$$\\color{white}{\\mathbf{D}^2(X)=\\sigma^2}$$'))
      })
      
      output$norm_ex3_dynamic <- renderUI({
        withMathJax(sprintf(
          '$$\\mathbf{D}^2(X)=\\color{red}{%g}^2$$',
          norm_szor()
        ))
      })
      
      output$norm_ex4_dynamic <- renderUI({
        withMathJax(
          helpText(
            '$$\\scriptsize{f(x)=\\dfrac{1}{\\sqrt{2\\pi}\\sigma}\\mathrm{e}^{-\\dfrac{1}{2}\\left(\\dfrac{x-\\mu}{\\sigma}\\right)^2}}$$'
            
          ),
          sprintf(
            '$$X\\sim \\mathcal{N}(\\mu,\\sigma)$$
            $$x= %g $$
          $$\\scriptsize{f(\\color{green}{%g})=\\dfrac{1}{\\sqrt{2\\pi}*\\color{red}{%g}}\\mathrm{e}^{-\\dfrac{1}{2}\\left(\\dfrac{\\color{green}{%g}-(\\color{blue}{%g})}{\\color{red}{%g}}\\right)^2}}$$
            $$\\small{=\\underline{%g}}$$',
            norm_x(),
            norm_x(),
            norm_szor(),
            norm_x(),
            norm_mu(),
            norm_szor(),
            norm_fx_eredmeny()
          )
          
        )
      })
      
    }
    
  }
  
}

shinyApp(ui = ui, server = server)
