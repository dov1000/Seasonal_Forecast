---
title: "Interfaz Pronostico Estacional"
output: 
  flexdashboard::flex_dashboard:
    #orientation: scroll
    orientation: rows
    vertical_layout: scroll
    theme: bootstrap
    runtime: shiny
---





Column {.sidebar column-width=200}
=========================================================================


```r
selectInput('estacion', 'Cuenca', colnames(redn))
```

<!--html_preserve--><div class="form-group shiny-input-container">
<label class="control-label" for="estacion">Cuenca</label>
<div>
<select id="estacion"><option value="Angostura" selected>Angostura</option>
<option value="Arenal">Arenal</option>
<option value="Reventazon">Reventazon</option>
<option value="Ventanas">Ventanas</option>
<option value="Cachi">Cachi</option>
<option value="Cariblanco">Cariblanco</option>
<option value="Penas">Penas</option>
<option value="Pirris">Pirris</option>
<option value="RioMacho">RioMacho</option>
<option value="Toro1">Toro1</option></select>
<script type="application/json" data-for="estacion" data-nonempty="">{}</script>
</div>
</div><!--/html_preserve-->

```r
tex <- reactive(round(promedios[[input$estacion]][month2,]))

useShinyjs(rmd = TRUE)
```

<!--html_preserve--><!--SHINY.SINGLETON[7e84d2035cadfaea966f7fbc9f50e59f75024418]-->
<script>Shiny.addCustomMessageHandler('shinyjs-show', function(params) { shinyjs.debugMessage('shinyjs: calling function "show" with parameters:'); shinyjs.debugMessage(params); shinyjs.show(params);});
Shiny.addCustomMessageHandler('shinyjs-hide', function(params) { shinyjs.debugMessage('shinyjs: calling function "hide" with parameters:'); shinyjs.debugMessage(params); shinyjs.hide(params);});
Shiny.addCustomMessageHandler('shinyjs-toggle', function(params) { shinyjs.debugMessage('shinyjs: calling function "toggle" with parameters:'); shinyjs.debugMessage(params); shinyjs.toggle(params);});
Shiny.addCustomMessageHandler('shinyjs-enable', function(params) { shinyjs.debugMessage('shinyjs: calling function "enable" with parameters:'); shinyjs.debugMessage(params); shinyjs.enable(params);});
Shiny.addCustomMessageHandler('shinyjs-disable', function(params) { shinyjs.debugMessage('shinyjs: calling function "disable" with parameters:'); shinyjs.debugMessage(params); shinyjs.disable(params);});
Shiny.addCustomMessageHandler('shinyjs-toggleState', function(params) { shinyjs.debugMessage('shinyjs: calling function "toggleState" with parameters:'); shinyjs.debugMessage(params); shinyjs.toggleState(params);});
Shiny.addCustomMessageHandler('shinyjs-addClass', function(params) { shinyjs.debugMessage('shinyjs: calling function "addClass" with parameters:'); shinyjs.debugMessage(params); shinyjs.addClass(params);});
Shiny.addCustomMessageHandler('shinyjs-removeClass', function(params) { shinyjs.debugMessage('shinyjs: calling function "removeClass" with parameters:'); shinyjs.debugMessage(params); shinyjs.removeClass(params);});
Shiny.addCustomMessageHandler('shinyjs-toggleClass', function(params) { shinyjs.debugMessage('shinyjs: calling function "toggleClass" with parameters:'); shinyjs.debugMessage(params); shinyjs.toggleClass(params);});
Shiny.addCustomMessageHandler('shinyjs-html', function(params) { shinyjs.debugMessage('shinyjs: calling function "html" with parameters:'); shinyjs.debugMessage(params); shinyjs.html(params);});
Shiny.addCustomMessageHandler('shinyjs-onevent', function(params) { shinyjs.debugMessage('shinyjs: calling function "onevent" with parameters:'); shinyjs.debugMessage(params); shinyjs.onevent(params);});
Shiny.addCustomMessageHandler('shinyjs-alert', function(params) { shinyjs.debugMessage('shinyjs: calling function "alert" with parameters:'); shinyjs.debugMessage(params); shinyjs.alert(params);});
Shiny.addCustomMessageHandler('shinyjs-logjs', function(params) { shinyjs.debugMessage('shinyjs: calling function "logjs" with parameters:'); shinyjs.debugMessage(params); shinyjs.logjs(params);});
Shiny.addCustomMessageHandler('shinyjs-runjs', function(params) { shinyjs.debugMessage('shinyjs: calling function "runjs" with parameters:'); shinyjs.debugMessage(params); shinyjs.runjs(params);});
Shiny.addCustomMessageHandler('shinyjs-reset', function(params) { shinyjs.debugMessage('shinyjs: calling function "reset" with parameters:'); shinyjs.debugMessage(params); shinyjs.reset(params);});
Shiny.addCustomMessageHandler('shinyjs-delay', function(params) { shinyjs.debugMessage('shinyjs: calling function "delay" with parameters:'); shinyjs.debugMessage(params); shinyjs.delay(params);});
Shiny.addCustomMessageHandler('shinyjs-click', function(params) { shinyjs.debugMessage('shinyjs: calling function "click" with parameters:'); shinyjs.debugMessage(params); shinyjs.click(params);});</script>
<script src="shinyjs/shinyjs-default-funcs.js"></script>
<script>shinyjs.debug = false;</script>
<style>.shinyjs-hide { display: none !important; }</style>
<!--/SHINY.SINGLETON[7e84d2035cadfaea966f7fbc9f50e59f75024418]--><!--/html_preserve-->

```r
actionButton("update", "Actualizar Datos")
```

<!--html_preserve--><button id="update" type="button" class="btn btn-default action-button">Actualizar Datos</button><!--/html_preserve-->

```r
onclick("update",
        source(here("Data.R")))
```

```
## Error: shinyjs: could not find the Shiny session object. This usually happens when a shinyjs function is called from a context that wasn't set up by a Shiny session.
```

Ensamble
=========================================================================

Row {data-height=650}
-------------------------------------

### 


```r
renderPlotly(plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,input$estacion] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,input$estacion] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Viridis',
             color = 'Jet') %>%
  layout(title = 'Resultados de Ensamble',
         yaxis = list(title = 'Probabilidad'),
         xaxis = list(title = 'Decil')))
```

<!--html_preserve--><div id="out0132fb3438971fe3" style="width:100%; height:400px; " class="plotly html-widget html-widget-output shiny-report-size"></div><!--/html_preserve-->


Row {data-height=150}
-------------------------------------
### Ultimo Mes Medido


```r
valueBox(month, icon = "fa-calendar-check", color ="light-blue")
```

<!--html_preserve--><span class="value-output" data-icon="fa-calendar-check" data-color="light-blue">10</span><!--/html_preserve-->

### Promedio Lluvia Mensual

```r
renderValueBox(valueBox(tex(), icon = "fa-balance-scale-right", color = "light-blue"))
```

<!--html_preserve--><div class="shiny-html-output shiny-html-output shiny-valuebox-output" height="160px" id="out9e97a8010d353124" width="100%"></div><!--/html_preserve-->

### Mes Pronosticado

```r
valueBox(month2, icon = "fa-arrow-alt-circle-right",color = "light-blue")
```

<!--html_preserve--><span class="value-output" data-icon="fa-arrow-alt-circle-right" data-color="light-blue">11</span><!--/html_preserve-->



Modelos
=========================================================================

Row {data-height=550}
------------------------------------------------

###


```r
renderPlotly(ggplotly(ggplot(data = data_modelos[[input$estacion]] %>% mutate(obs = 1:10) %>%  gather('modelo', 'value', -c(obs)), aes(x= obs,y = value, group = modelo))+
   geom_line(aes(linetype=modelo,color=modelo))+
   geom_point(aes(shape=modelo,color = modelo))+
   scale_x_continuous(breaks = 1:10,labels=deciles_nombre)+
   xlab('Decil')+
   ylab('Probabilidad') ))
```

<!--html_preserve--><div id="out6949eec66a9f7c5d" style="width:100%; height:400px; " class="plotly html-widget html-widget-output shiny-report-size"></div><!--/html_preserve-->

Row {data-height=650}
--------------------------------------------------------------------------
### Ultimo Mes Medido


```r
valueBox(month, icon = "fa-calendar-check", color = "light-blue")
```

<!--html_preserve--><span class="value-output" data-icon="fa-calendar-check" data-color="light-blue">10</span><!--/html_preserve-->

### Promedio Lluvia Mensual

```r
#tex <- reactive(round(promedios[[input$estacion]][month,]))
renderValueBox(valueBox(tex(), icon = "fa-balance-scale-right", color = "light-blue"))
```

<!--html_preserve--><div class="shiny-html-output shiny-html-output shiny-valuebox-output" height="160px" id="out978b7c4f7cbfd8b0" width="100%"></div><!--/html_preserve-->

### Mes Pronosticado

```r
valueBox(month2, icon = "fa-arrow-alt-circle-right", color = "light-blue")
```

<!--html_preserve--><span class="value-output" data-icon="fa-arrow-alt-circle-right" data-color="light-blue">11</span><!--/html_preserve-->

Conjunto
=========================================================================

Row {data-height=650}
-------------------------------------

### 


```r
p1<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Reventazon'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Reventazon'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Reventazon',
             marker_color = 'Jet'
            )
  

  p2<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Penas'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Penas'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Penas',
             marker_color = 'black') 
  
  
  p3<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Arenal'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Arenal'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Arenal',
             marker_color = 'azure')
  
  
  p4<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Angostura'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Angostura'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Angostura',
             marker_color = 'cornsilk') 
  
  
  p5<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Pirris'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Pirris'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Pirris',
             marker_color = 'lavender') 
  
  
  p6<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Cariblanco'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Cariblanco'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Cariblanco',
             marker_color = 'gainsboro') 
  
  p7<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Cachi'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Cachi'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Cachi',
             marker_color = 'burlywood') 
  
  
  p8<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Ventanas'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Ventanas'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Ventanas',
             marker_color = 'aqua') 
  
  
  p9<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'Toro1'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'Toro1'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'Toro1',
             marker_color = 'springgreen') 
  
  
  p10<-plot_ly(data.frame(deciles = deciles_nombre,valor = redn[,'RioMacho'] %>% unlist() %>% unname()),
             x = ~deciles_nombre,
             y = ~redn[,'RioMacho'] %>% unlist() %>% unname(),
             type = 'bar',
             name = 'RioMacho',
             marker_color = 'thistle') 
  
  
  renderPlotly(subplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,nrows = 5,margin = 0.04))
```

<!--html_preserve--><div id="outb85a5969b1e41d1f" style="width:100%; height:400px; " class="plotly html-widget html-widget-output shiny-report-size"></div><!--/html_preserve-->




