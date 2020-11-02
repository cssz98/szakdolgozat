valsz_input <- function(valsz_tipus) {
  selectInput(
    valsz_tipus,
    "Valószínűségi típus:",
    c(
      "$$\\mathbf{P}(X=x)$$" = "egyenlo",
      "$$\\mathbf{P}(X \\leq x)$$" = "ekisebb",
      "$$\\mathbf{P}(X \\geq x)$$" = "enagyobb",
      "$$\\small{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)}$$" =
        "intervallum"
    )
  )
}

teszt_input <- function(valasz_tipus, text, x) {
  numericInput(
    valasz_tipus,
    text,
    teszt_na(x),
    min = -Inf,
    max = Inf,
    step = 0.001
  )
}

teszt_na <- function(y) {
  if_else(y == F, NA, NA)
}

valoszinuseg <-
  function(valsz_input,
           egyenlo_valoszinuseg,
           ekisebb_valoszinuseg,
           enagyobb_valoszinuseg,
           intervallum_valoszinuseg) {
    case_when(
      valsz_input == "egyenlo" ~ round(egyenlo_valoszinuseg, digits = 3),
      valsz_input == "ekisebb" ~ round(sum(ekisebb_valoszinuseg), digits = 3),
      valsz_input == "enagyobb" ~ round(sum(enagyobb_valoszinuseg), digits = 3),
      valsz_input == "intervallum" ~ round(sum(intervallum_valoszinuseg), digits = 3)
    )
  }

fuggvenyek <-
  function(sulyfvg_ekisebb_x,
           sulyfvg_ekisebb_y,
           sulyfvg_enagyobb_x,
           sulyfvg_enagyobb_y,
           sulyfvg_egyenlo_x,
           sulyfvg_egyenlo_y,
           valsz_input,
           sulyfvg_intervallum_x,
           sulyfvg_intervallum_y,
           eloszlasfvg_x,
           eloszlasfvg_y) {
    sulyfvg <-
      plot_ly(type = "bar")
    
    sulyfvg <-
      sulyfvg %>% add_trace(
        y = sulyfvg_ekisebb_y,
        x = sulyfvg_ekisebb_x,
        name = "P(X\u2264 x)",
        marker = list(color = "#00CCCC")
      )
    sulyfvg <-
      sulyfvg %>% add_trace(
        y = sulyfvg_enagyobb_y,
        x = sulyfvg_enagyobb_x,
        name = "P(X\u2265 x)",
        marker = list(color = "#0e6655")
      )
    
    sulyfvg <-
      sulyfvg %>% add_trace(
        y = sulyfvg_egyenlo_y,
        x = sulyfvg_egyenlo_x,
        name = "P(X= x)",
        marker = list(color = "#28b463")
      )
    if (valsz_input == "intervallum") {
      sulyfvg <-
        sulyfvg %>% add_lines(
          y = sulyfvg_intervallum_y,
          x = sulyfvg_intervallum_x,
          name = "P(x\u2081 \u2264 X \u2264 x\u2082)",
          line = list(shape = "spline", color = "#FF0000"),
          marker = list(color = "#FF0000"),
          fill = "tozeroy",
          fillcolor = "rgba(255, 0, 0, 0.4)"
        )
    }
    
    
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
        barmode = "overlay"
      )
    
    eloszlasfvg <-
      
      plot_ly()
    
    eloszlasfvg <-
      eloszlasfvg %>% add_lines(
        y = eloszlasfvg_y,
        x = eloszlasfvg_x,
        name = "Kumulatív valószínűség",
        line = list(shape = "hvh", color = "#FF0000"),
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
        xref = "paper",
        yref = "paper"
      ),
      list(
        x = 0.8,
        y = 1.05,
        text = "Eloszlásfüggvény",
        showarrow = F,
        xref = "paper",
        yref = "paper"
      )
    ))
  }

adattabla <- function(valsz_tipus,
                      egyenlo_x,
                      egyenlo_y,
                      ekisebb_x,
                      ekisebb_y,
                      enagyobb_x,
                      enagyobb_y,
                      intervallum_x,
                      intervallum_y) {
  eredmeny_dt <-
    if (valsz_tipus == "egyenlo") {
      egyenlo_y
    } else if (valsz_tipus == "ekisebb") {
      ekisebb_y
    }
  else if (valsz_tipus == "enagyobb") {
    enagyobb_y
  }
  else {
    intervallum_y
  }
  
  
  x_dt <-
    if (valsz_tipus == "egyenlo") {
      egyenlo_x
    } else if (valsz_tipus == "enagyobb") {
      enagyobb_x
    }
  else if (valsz_tipus == "ekisebb") {
    ekisebb_x
  }
  else {
    intervallum_x
  }
  
  
  dt <-
    
    case_when(
      valsz_tipus == "egyenlo" ~ "P(X= x)",
      valsz_tipus == "ekisebb" ~ "P(X\u2264 x)",
      valsz_tipus == "enagyobb" ~ "P(X\u2265 x)",
      valsz_tipus == "intervallum" ~ "P(x\u2081 \u2264 X \u2264 x\u2082)"
    )
  
  
  
  
  
  df <-
    data.frame(x = x_dt,
               y = round(eredmeny_dt, digits = 5))
  
  
  
  dt <-
    datatable(
      df,
      caption = dt,
      colnames = c("X", "Valószínűség"),
      rownames = FALSE,
      options = list(
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Hungarian.json"),
        pageLength = 2
      )
    )
  
  dt <-
    formatStyle(
      dt,
      "x",
      color = "#FFFFFF",
      backgroundColor = "#006666",
      fontWeight = "bold"
    )
  dt <-
    formatStyle(
      dt,
      "y",
      color = "#FFFFFF",
      backgroundColor = "#009999",
      fontWeight = "bold"
    )
}

vb <- function(eredmeny, keplet) {
  valueBox(
    eredmeny,
    icon = icon(" fa-calculator"),
    uiOutput(keplet),
    color = "aqua",
    href = NULL
  )
}

teszt_valsz <- function(tipus_ujra) {
  if_else(tipus_ujra == F, sample(c(
    "P(X\u2265 x)?", "P(X\u2264 x)?", "P(X= x)?"
  ), 1), sample(c(
    "P(X\u2265 x)?", "P(X\u2264 x)?", "P(X= x)?"
  ), 1))
}

teszt_eredmeny <-
  function(teszt_valsz,
           teszt_egyenlo_eredmeny,
           teszt_ekisebb_eredmeny,
           teszt_enagyobb_eredmeny) {
    case_when(
      teszt_valsz == "P(X= x)?" ~ teszt_egyenlo_eredmeny,
      teszt_valsz == "P(X\u2264 x)?" ~ teszt_ekisebb_eredmeny,
      teszt_valsz == "P(X\u2265 x)?" ~ teszt_enagyobb_eredmeny
    )
  }

feedback <- function(x, y) {
  if (is.na(x)) {
    
  }
  else if (x == y) {
    HTML(
      "<p  style='color:green'>Helyes válasz! <i class='fas fa-check-circle' style='font-size:15px;'></i>"
    )
  }
  
  else {
    HTML(
      "<p  style='color:red'>Helytelen válasz! <i class='fas fa-times-circle' style='font-size:15px;'></i>"
    )
  }
}

tab <- function(valsz_tipus) {
  case_when(
    valsz_tipus == "egyenlo" ~ "P(X= x)",
    valsz_tipus == "ekisebb" ~ "P(X\u2264 x)",
    valsz_tipus == "enagyobb" ~ "P(X\u2265 x)",
    valsz_tipus == "intervallum" ~ "P(x\u2081 \u2264 X \u2264 x\u2082)"
  )
}

keplet <-
  function(valsz_tipus,
           intervallum_keplet,
           egyenlo_keplet,
           ekisebb_keplet,
           enagyobb_keplet) {
    if (valsz_tipus == "intervallum") {
      withMathJax(helpText(intervallum_keplet))
    } else if (valsz_tipus == "egyenlo") {
      withMathJax(helpText(egyenlo_keplet))
    }
    else if (valsz_tipus == "enagyobb") {
      withMathJax(helpText(enagyobb_keplet))
    }
    else if (valsz_tipus == "ekisebb") {
      withMathJax(helpText(ekisebb_keplet))
    }
  }

interaktiv_keplet <-
  function(valsz_tipus,
           intervallum_keplet_interaktiv,
           egyenlo_keplet_interaktiv,
           ekisebb_keplet_interaktiv,
           enagyobb_keplet_interaktiv) {
    if (valsz_tipus == "intervallum") {
      withMathJax(intervallum_keplet_interaktiv)
    } else if (valsz_tipus == "egyenlo") {
      withMathJax(egyenlo_keplet_interaktiv)
    }
    else if (valsz_tipus == "enagyobb") {
      withMathJax(enagyobb_keplet_interaktiv)
    }
    
    else if (valsz_tipus == "ekisebb") {
      withMathJax(ekisebb_keplet_interaktiv)
    }
  }


# folytonos
{
  tab_folytonos <- function(valsz_tipus) {
    case_when(
      valsz_tipus == "kisebb" ~ "P(X< x)",
      valsz_tipus == "nagyobb" ~ "P(X> x)",
      valsz_tipus == "intervallum" ~ "P(x\u2081 \u2264 X \u2264 x\u2082)"
    )
  }
  
  keplet_folytonos <-
    function(valsz_tipus,
             intervallum_keplet,
             kisebb_keplet,
             nagyobb_keplet) {
      if (valsz_tipus == "intervallum") {
        withMathJax(helpText(intervallum_keplet))
      } else if (valsz_tipus == "nagyobb") {
        withMathJax(helpText(nagyobb_keplet))
      }
      else if (valsz_tipus == "kisebb") {
        withMathJax(helpText(kisebb_keplet))
      }
    }
  
  interaktiv_keplet_folytonos <-
    function(valsz_tipus,
             intervallum_keplet_interaktiv,
             kisebb_keplet_interaktiv,
             nagyobb_keplet_interaktiv) {
      if (valsz_tipus == "intervallum") {
        withMathJax(intervallum_keplet_interaktiv)
      } else if (valsz_tipus == "nagyobb") {
        withMathJax(nagyobb_keplet_interaktiv)
      }
      
      else if (valsz_tipus == "kisebb") {
        withMathJax(kisebb_keplet_interaktiv)
      }
    }
  
  valsz_input_folytonos <- function(valsz_tipus) {
    selectInput(
      valsz_tipus,
      "Valószínűségi típus:",
      c(
        "$$\\mathbf{P}(X < x)$$" = "kisebb",
        "$$\\mathbf{P}(X > x)$$" = "nagyobb",
        "$$\\small{\\mathbf{P}(\\ x_1 \\leq X  \\leq \\ x_2)}$$" =
          "intervallum"
      )
    )
  }
  
  valoszinuseg_folytonos <-
    function(valsz_input,
             kisebb_valoszinuseg,
             nagyobb_valoszinuseg,
             intervallum_valoszinuseg) {
      case_when(
        valsz_input == "kisebb" ~ round(kisebb_valoszinuseg, digits = 3),
        valsz_input == "nagyobb" ~ round(nagyobb_valoszinuseg, digits = 3),
        valsz_input == "intervallum" ~ round(intervallum_valoszinuseg, digits = 3)
      )
    }
  
  teszt_valsz_folytonos <- function(tipus_ujra) {
    if_else(tipus_ujra == F, sample(c("P(X> x)?", "P(X< x)?"), 1), sample(c("P(X> x)?", "P(X< x)?"), 1))
  }
  
  teszt_eredmeny_folytonos <-
    function(teszt_valsz,
             teszt_kisebb_eredmeny,
             teszt_nagyobb_eredmeny) {
      case_when(
        teszt_valsz == "P(X< x)?" ~ teszt_kisebb_eredmeny,
        teszt_valsz == "P(X> x)?" ~ teszt_nagyobb_eredmeny
      )
    }
  
  fuggveny_folytonos <-
    function(sulyfvg_kisebb_x,
             sulyfvg_kisebb_y,
             sulyfvg_nagyobb_x,
             sulyfvg_nagyobb_y,
             valsz_input,
             sulyfvg_intervallum_x,
             sulyfvg_intervallum_y,
             sulyfvg_range_x,
             sulyfvg_range_y,
             eloszlasfvg_range_x,
             eloszlasfvg_range_y,
             eloszlasfvg_range) {
      sulyfvg <- plot_ly()
      
      sulyfvg <-
        sulyfvg %>% add_polygons(
          sulyfvg_kisebb_x,
          sulyfvg_kisebb_y,
          fillcolor = "rgba(14, 102, 85, 0.75",
          line = list(width = 4, color = "rgb(14, 102, 85)"),
          name = "P(X< x)"
        )
      
      sulyfvg <-
        sulyfvg %>% add_polygons(
          sulyfvg_nagyobb_x,
          sulyfvg_nagyobb_y,
          fillcolor = "rgba(0, 204, 204, 0.75",
          line = list(width = 4, color = "rgb(0, 204, 204)"),
          name = "P(X> x)"
        )
      
      if (valsz_input == "intervallum") {
        sulyfvg <-
          sulyfvg %>% add_polygons(
            sulyfvg_intervallum_x,
            sulyfvg_intervallum_y,
            fillcolor = "rgba(245, 66, 69, 0.75)",
            line = list(
              width = 1,
              color = "rgb(255,0, 0)",
              dash = "dash"
            ),
            name = "P(x\u2081 < X < x\u2082)"
          )
      }
      
      sulyfvg <-
        sulyfvg %>% layout(
          yaxis = list(
            range = sulyfvg_range_y,
            ticks = "outside",
            tickwidth = 1,
            tickcolor = "#000000",
            showline = TRUE,
            gridwidth = 10,
            linewidth = 2.5
          ),
          
          xaxis = list(
            range = sulyfvg_range_x,
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
          y = eloszlasfvg_range_y,
          x = eloszlasfvg_range_x,
          name = "Kumulatív valószínűség",
          line = list(shape = "spline", color = "#FF0000"),
          showlegend = FALSE
        )
      
      
      eloszlasfvg <-
        eloszlasfvg %>% layout(
          xaxis = list(
            range = eloszlasfvg_range,
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
          xref = "paper",
          yref = "paper"
        ),
        list(
          x = 0.8,
          y = 1.05,
          text = "Eloszlásfüggvény",
          showarrow = F,
          xref = "paper",
          yref = "paper"
        )
      ))
    }
}