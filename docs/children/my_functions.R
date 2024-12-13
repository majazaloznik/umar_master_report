# http://svgicons.sparkk.fr/
icon_svg_path = "M15.608,6.262h-2.338v0.935h2.338c0.516,0,0.934,0.418,0.934,0.935v8.879c0,0.517-0.418,0.935-0.934,0.935H4.392c-0.516,0-0.935-0.418-0.935-0.935V8.131c0-0.516,0.419-0.935,0.935-0.935h2.336V6.262H4.392c-1.032,0-1.869,0.837-1.869,1.869v8.879c0,1.031,0.837,1.869,1.869,1.869h11.216c1.031,0,1.869-0.838,1.869-1.869V8.131C17.478,7.099,16.64,6.262,15.608,6.262z M9.513,11.973c0.017,0.082,0.047,0.162,0.109,0.226c0.104,0.106,0.243,0.143,0.378,0.126c0.135,0.017,0.274-0.02,0.377-0.126c0.064-0.065,0.097-0.147,0.115-0.231l1.708-1.751c0.178-0.183,0.178-0.479,0-0.662c-0.178-0.182-0.467-0.182-0.645,0l-1.101,1.129V1.588c0-0.258-0.204-0.467-0.456-0.467c-0.252,0-0.456,0.209-0.456,0.467v9.094L8.443,9.553c-0.178-0.182-0.467-0.182-0.645,0c-0.178,0.184-0.178,0.479,0,0.662L9.513,11.973z"
dl_button <- list(
  name = "Download data",
  icon = list(
    path = icon_svg_path,
    transform = "scale(0.84) translate(-1, -1)"
  ),
  click = htmlwidgets::JS("
          function(gd) {
            var text = '\ufeff'; // UTF-8 BOM

            var validDataIndices = [];
            // Write the headers for the csv file
            for(var j = 0; j < gd.data.length; j++){
              if (gd.data[j].name !== ' ') { // Check if the name is not empty
                text += gd.data[j].name + ' - Datum';
                text += ';' + gd.data[j].name + ' - ' + gd.layout.yaxis.title.text;
                validDataIndices.push(j); // Store valid data indices
                if (j < gd.data.length - 1) {
                  text += ';';
                }
              }
            }
            text += '\\n';

            // Find the maximum length of the data arrays
            var maxLength = 0;
            for(var i = 0; i < validDataIndices.length; i++){
              var dataIndex = validDataIndices[i];
              if(gd.data[dataIndex].x.length > maxLength) {
                maxLength = gd.data[dataIndex].x.length;
              }
            }

            // Write the data for each observation
            for(var i = 0; i < maxLength; i++){
              for(var j = 0; j < validDataIndices.length; j++){
                var dataIndex = validDataIndices[j];
                text += (gd.data[dataIndex].x[i] || '').toString().replace('.', ',') + ';' + (gd.data[dataIndex].y[i] || '').toString().replace('.', ',');
                if (j < validDataIndices.length - 1) {
                  text += ';';
                }
              }
              text += '\\n';
            }

            var blob = new Blob([text], {type: 'text/plain;charset=utf-8'});
            var a = document.createElement('a');
            const object_URL = URL.createObjectURL(blob);
            a.href = object_URL;
            a.download = 'data.csv';
            document.body.appendChild(a);
            a.click();
            URL.revokeObjectURL(object_URL);
          }
   ")
)



# incremental automation
umar_layout <- function(plot, slider_w, m, ...) {
  plot <- layout(plot, ...)
 # add rangeslider thinckness and other stuff that is the same everywhere
  plot <- layout(plot, xaxis = list(rangeslider = list(thickness = slider_w)),
                 showlegend = TRUE,
                 autosize = F, margin = m,
                 font=list(family = "Myriad Pro")) |>
    config(modeBarButtonsToAdd = list(dl_button))


  return(plot)
}

# add lines with hovertemplate Q %
add_lines_qp <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|Q%q %Y} %{y:.2f}%", ...)
  return(plot)
}
# add lines with hovertemplate Q
add_lines_q <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|Q%q %Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate M %
add_lines_mp <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|%b %Y} %{y:.2f}%", ...)
  return(plot)
}
# add lines with hovertemplate M
add_lines_m <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|%b %Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate D %
add_lines_dp <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|%e.%b %y} %{y:.2f}%", ...)
  return(plot)
}


# add lines with hovertemplate A %
add_lines_ap <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|%Y} %{y:.2f}%", ...)
  return(plot)
}
# add lines with hovertemplate A
add_lines_a <- function(plot, y,  ...) {
  plot <- add_lines(plot, y = y, hovertemplate="%{x|%Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate Q
add_bars_q <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|Q%q-%Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate Q
add_bars_qp <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|Q%q-%Y} %{y:.2f}%", ...)
  return(plot)
}


# add lines with hovertemplate Q
add_bars_m <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|%b %Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate Q
add_bars_mp <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|%b %Y} %{y:.2f}%", ...)
  return(plot)
}

# add lines with hovertemplate A
add_bars_a <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|%Y} %{y:.2f}", ...)
  return(plot)
}

# add lines with hovertemplate A
add_bars_ap <- function(plot, y,  ...) {
  plot <- add_bars(plot, y = y, hovertemplate="%{x|%Y} %{y:.2f}%", ...)
  return(plot)
}

# y axis
umar_yaxis <- function(title_text, ...) {
  list(fixedrange = FALSE,
       title = list(text = title_text, font = list(size = 12)),
       ...)
}

emph_line <- function(y = 100, period){
  list(
    list(
      type = "line",
      x0 = min(period), x1 = max(period),
      y0 = y, y1 = y,
      line = list(color = umar_cols("emph"), width = 1)
    ))
}

umar_xaxis <- function(interval){
if(interval == "M") out <- "%b %Y"
if(interval == "Q") out <- "Q%q-%Y"
if(interval == "A") out <- "%Y"
  list(title = "",
       tickformatstops = list(
         list(dtickrange = list("M1", "M6"),
              value = out),
         list(dtickrange = list("M6", NULL),
              value = "%Y")))
}


#' Get a subtitle with updates, soruces and transformations
#'
#' Options are:
#' the default is SURS as the source, but add must be null and surs true
#' add = "UMAR" adds "preracuni UMAR" to surs if surs is true
#' add = "FURS" and surs = ture or something else just adds a second source to surs
#' alt = "FURS" and surs = flase has just alternative source
#' add = "UMAR", alt = "FURS" abd surs = false gives alternative & preracun umar
#' Those should be all five options. my_panel_subtitle works the same way.
#'
#' @param updated
#' @param add
#' @param transformation
#' @param surs
#' @param alt
#'
#' @return
#' @export
#'
#' @examples
umar_subtitle <- function(updated, add = NULL, transformation = NULL, surs = TRUE, alt = NULL) {
  if(surs){
    if(!is.null(add) && add == "UMAR") {add <- " & prera\u010dun UMAR"} else {
      if(!is.null(add)) add <- paste ( " &", add)}
    text <- paste0("Posodobljeno: ", updated, " ",
                   ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                   "(Vir: SURS", add, ")")} else {
                     if(!is.null(add) && add == "UMAR") {add <- " & prera\u010dun UMAR"
                     text <- paste0("Posodobljeno: ", updated, " ",
                                    ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                                    "(Vir: ", alt, add, ")")
                     }else {
                       text <- paste0("Posodobljeno: ", updated, " ",
                                      ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                                      "(Vir: ", alt, ")")
                     }
                   }


  list(text = text,
       font = list(size = 12),
       x = 0)
}


initials <- function(initials) {
  list(
  x = 1, y = 1.02, text = initials, showarrow = FALSE,
  xref='paper', yref='paper', xanchor='right', yanchor='top',
  font=list(size=10, color = umar_cols()[3]))
}



add_empty_lines <- function(figure, no_lines) {
  for(i in 1:no_lines) {
    figure <- figure |>
      add_lines(y = 0.01,  name = " ",  color = I('rgba(0,0,0,0)'),
                hoverinfo = "none")
  }
  return(figure)
}


my_panel_title <- function(fig, add = NULL, transformation = NULL, surs = TRUE, alt = NULL, updated = NULL) {
  if(surs){
    if(!is.null(add) && add == "UMAR") {add <- " & prera\u010dun UMAR"} else {
      if(!is.null(add)) add <- paste ( " &", add)}
    text <- paste0("Posodobljeno: ", updated, " ",
                   ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                   "(Vir: SURS", add, ")")} else {
                     if(!is.null(add) && add == "UMAR") {add <- " & prera\u010dun UMAR"
                     text <- paste0("Posodobljeno: ", updated, " ",
                                    ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                                    "(Vir: ", alt, add, ")")
                     }else {
                       text <- paste0("Posodobljeno: ", updated, " ",
                                      ifelse(!is.null(transformation), paste0(transformation, " "), ""),
                                      "(Vir: ", alt, ")")
                     }
         }
  fig <- fig %>%
    layout(annotations = list(
      x = -0.08,
      y = 1.13,
      text = text,
      showarrow = F,
      xref='paper',
      yref='paper',
      xanchor='middle',
      yanchor='top',
      font=list(size=12)
    ))
  fig
}

my_panel_subtitle <- function(fig, text) {
  fig <- fig %>%
    layout(annotations = list(
      x = 0,
      y = 1,
      text = text,
      showarrow = F,
      xref='paper',
      yref='paper',
      xanchor='middle',
      yanchor='top',
      font=list(size=12)
    ))
  return(fig)
}

my_panel_note <- function(fig, text) {
  fig <- fig %>%
    layout(
      annotations = list(
        x = 0,
        y = -0.25,  # Adjust this value to move the note up or down
        xref = 'paper',
        yref = 'paper',
        text = text,
        showarrow = FALSE,
        font=list(size=12)
      ))
  return(fig)
}
