#' Launch the interactive CineGrade Shiny dashboard
#'
#' Opens a browser-based Shiny application for cinematic colour grading.
#' Upload a target photo, choose a reference colour style (built-in film
#' presets or your own upload), adjust clusters k, and download the result.
#'
#' @details
#' \strong{Features:} target upload, film preset selection, k slider,
#' before/after split-screen, \eqn{\Delta E} info bar, download handler.
#' Internally calls \code{\link{apply_grade}} in LAB space.
#'
#' @return Launches a Shiny app (invisible). Blocks the R session.
#'
#' @examples
#' if (interactive()) run_cinegrader()
#'
#' @importFrom shiny shinyApp fluidPage tags fileInput radioButtons
#'   conditionalPanel selectInput sliderInput actionButton downloadButton
#'   renderPlot renderText plotOutput verbatimTextOutput downloadHandler
#'   eventReactive reactive req withProgress HTML h4 hr
#' @export
run_cinegrader <- function() {

  preset_paths <- list(
    "Wes Anderson" = system.file("extdata", "preset_wes.jpg",     package = "CineGrade"),
    "Noir"         = system.file("extdata", "preset_noir.jpg",    package = "CineGrade"),
    "Vintage"      = system.file("extdata", "preset_vintage.jpg", package = "CineGrade"),
    "Anime"        = system.file("extdata", "preset_anime.jpg",   package = "CineGrade")
  )
  missing <- names(Filter(function(p) nchar(p) == 0L, preset_paths))
  if (length(missing) > 0L)
    warning("Missing presets: ", paste(missing, collapse = ", "), call. = FALSE)

  # ---------------------------------------------------------------------------
  # CSS — Film Noir Laboratory
  # ---------------------------------------------------------------------------
  css <- "
@import url('https://fonts.googleapis.com/css2?family=Bebas+Neue&family=DM+Sans:wght@300;400;500;600&family=JetBrains+Mono:wght@400;500;700&display=swap');

:root {
  --ink:    #090806;
  --coal:   #100f0b;
  --ash:    #181610;
  --lift:   #201d14;
  --cu:     #c4622d;
  --ember:  #e07a3a;
  --glow:   rgba(196,98,45,.3);
  --wire:   rgba(196,98,45,.18);
  --text:   #e8e2d4;
  --dim:    #7a7060;
  --faint:  #3a3526;
  --fn:     'JetBrains Mono', monospace;
  --dn:     'Bebas Neue', sans-serif;
  --bd:     'DM Sans', sans-serif;
}
* { box-sizing: border-box; margin: 0; padding: 0; }
html, body {
  background: var(--ink) !important;
  color: var(--text);
  font-family: var(--bd);
  font-size: 14px;
  min-height: 100vh;
  -webkit-font-smoothing: antialiased;
}
.container-fluid { padding: 0 !important; }
.row { margin: 0 !important; }
::-webkit-scrollbar { width: 4px; }
::-webkit-scrollbar-track { background: var(--coal); }
::-webkit-scrollbar-thumb { background: var(--faint); border-radius: 2px; }

/* HEADER */
.cg-hdr {
  display: flex; align-items: center;
  justify-content: space-between;
  height: 62px; padding: 0 28px;
  background: var(--coal);
  border-bottom: 1px solid var(--wire);
  position: relative;
}
.cg-hdr::after {
  content: ''; position: absolute; bottom: 0; left: 0; right: 0; height: 1px;
  background: linear-gradient(90deg, transparent, var(--cu), transparent);
}
.cg-wordmark { font-family: var(--dn); font-size: 2rem; letter-spacing: .05em; line-height: 1; }
.cg-wordmark b { color: var(--cu); font-weight: 400; }
.cg-sub { font-family: var(--fn); font-size: .68rem; color: var(--dim); margin-left: 14px; letter-spacing: .05em; }
.cg-badge {
  font-family: var(--fn); font-size: .62rem; font-weight: 700;
  letter-spacing: .1em; text-transform: uppercase;
  padding: 4px 10px; border: 1px solid var(--wire);
  color: var(--cu); background: rgba(196,98,45,.07); border-radius: 2px;
}

/* LAYOUT */
.cg-root { display: flex; height: calc(100vh - 62px); overflow: hidden; }

/* SIDEBAR */
.cg-sidebar {
  width: 306px; min-width: 306px;
  background: var(--coal);
  border-right: 1px solid var(--wire);
  display: flex; flex-direction: column;
  overflow-y: auto; overflow-x: hidden;
}
.cg-sec { padding: 18px 18px 14px; border-bottom: 1px solid rgba(255,255,255,.04); }
.cg-sec:last-child { border-bottom: none; }
.cg-sec-title {
  font-family: var(--dn); font-size: .82rem;
  letter-spacing: .16em; color: var(--cu);
  margin-bottom: 12px;
  display: flex; align-items: center; gap: 8px;
}
.cg-sec-title::before {
  content: ''; display: block; width: 14px; height: 1px;
  background: var(--cu); opacity: .6;
}

/* INPUTS */
.form-group { margin-bottom: 0 !important; }
.control-label { display: none !important; }
.btn-file {
  background: rgba(196,98,45,.1) !important; color: var(--cu) !important;
  border: 1px solid rgba(196,98,45,.35) !important; border-radius: 3px !important;
  font-family: var(--bd) !important; font-size: .8rem !important;
  font-weight: 500 !important; padding: 8px 14px !important;
  width: 100% !important; transition: all .18s !important;
}
.btn-file:hover { background: rgba(196,98,45,.2) !important; border-color: var(--cu) !important; }
.input-group-btn { width: 100%; }
#target_file_progress .progress-bar,
#ref_file_progress   .progress-bar { background: var(--cu) !important; }
.progress { background: var(--faint) !important; border-radius: 2px !important; height: 3px !important; margin-top: 6px !important; }
input[type=text].form-control {
  background: transparent !important; border: none !important;
  border-bottom: 1px solid var(--faint) !important; border-radius: 0 !important;
  color: var(--dim) !important; font-family: var(--fn) !important;
  font-size: .7rem !important; padding: 4px 2px !important;
  margin-top: 5px !important; box-shadow: none !important;
}
.radio { display: inline-block; margin: 0 0 5px 0 !important; }
.radio label {
  color: var(--dim) !important; font-size: .8rem !important;
  padding: 4px 10px 4px 26px !important;
  cursor: pointer !important; transition: color .15s !important;
}
.radio label:hover { color: var(--text) !important; }
.radio input[type=radio] { accent-color: var(--cu) !important; }
.selectize-control { margin-top: 4px; }
.selectize-input {
  background: var(--ash) !important; color: var(--text) !important;
  border: 1px solid var(--faint) !important; border-radius: 3px !important;
  font-family: var(--bd) !important; font-size: .82rem !important;
  padding: 8px 10px !important; box-shadow: none !important; cursor: pointer !important;
}
.selectize-input.focus { border-color: var(--cu) !important; box-shadow: 0 0 0 2px rgba(196,98,45,.15) !important; outline: none !important; }
.selectize-dropdown {
  background: #161410 !important; border: 1px solid var(--wire) !important;
  border-radius: 3px !important; box-shadow: 0 8px 36px rgba(0,0,0,.8) !important;
}
.selectize-dropdown-content .option {
  color: var(--dim) !important; font-family: var(--bd) !important;
  font-size: .82rem !important; padding: 9px 12px !important;
  border-bottom: 1px solid rgba(255,255,255,.03) !important;
}
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .active { background: rgba(196,98,45,.12) !important; color: var(--ember) !important; }

/* SLIDER */
.irs { font-family: var(--fn) !important; }
.irs--shiny .irs-bar { background: var(--cu) !important; height: 3px !important; border: none !important; }
.irs--shiny .irs-bar--single { background: var(--cu) !important; }
.irs--shiny .irs-line { background: var(--faint) !important; height: 3px !important; border: none !important; }
.irs--shiny .irs-handle {
  background: var(--ink) !important; border: 2px solid var(--cu) !important;
  box-shadow: 0 0 0 3px var(--glow) !important;
  width: 16px !important; height: 16px !important;
  border-radius: 50% !important; top: 22px !important; cursor: ew-resize !important;
}
.irs--shiny .irs-handle:hover { box-shadow: 0 0 0 5px rgba(196,98,45,.35) !important; }
.irs--shiny .irs-single {
  background: var(--cu) !important; color: var(--ink) !important;
  font-family: var(--fn) !important; font-weight: 700 !important;
  font-size: .72rem !important; border-radius: 2px !important; padding: 2px 6px !important;
}
.irs--shiny .irs-min, .irs--shiny .irs-max {
  color: var(--dim) !important; font-family: var(--fn) !important;
  font-size: .65rem !important; background: transparent !important;
}
.cg-hint { font-family: var(--fn); font-size: .64rem; color: var(--faint); margin-top: 8px; letter-spacing: .04em; }

/* BUTTONS */
.btn-apply {
  width: 100%; border: none !important; border-radius: 3px !important;
  background: linear-gradient(135deg, #a84d23, var(--cu), var(--ember)) !important;
  color: var(--ink) !important; font-family: var(--dn) !important;
  font-size: 1.15rem !important; letter-spacing: .14em !important;
  padding: 13px 20px !important; cursor: pointer !important;
  position: relative !important; overflow: hidden !important;
  transition: box-shadow .2s !important;
}
.btn-apply::after {
  content: ''; position: absolute; top: -60%; left: -80%;
  width: 60%; height: 220%; background: linear-gradient(90deg, transparent, rgba(255,255,255,.18), transparent);
  transform: skewX(-20deg); transition: left .55s;
}
.btn-apply:hover { box-shadow: 0 0 28px rgba(196,98,45,.55) !important; color: var(--ink) !important; }
.btn-apply:hover::after { left: 130%; }
.btn-apply:active { transform: scale(.98) !important; }
.btn-download {
  width: 100%; background: transparent !important; color: var(--dim) !important;
  border: 1px solid var(--faint) !important; border-radius: 3px !important;
  padding: 9px 16px !important; font-family: var(--fn) !important;
  font-size: .72rem !important; letter-spacing: .06em !important;
  cursor: pointer !important; transition: all .18s !important; margin-top: 8px !important;
}
.btn-download:hover { border-color: var(--wire) !important; color: var(--text) !important; background: rgba(255,255,255,.03) !important; }

/* CANVAS */
.cg-canvas {
  flex: 1; display: flex; flex-direction: column; overflow: hidden;
  background: var(--ink);
  background-image:
    linear-gradient(rgba(196,98,45,.04) 1px, transparent 1px),
    linear-gradient(90deg, rgba(196,98,45,.04) 1px, transparent 1px);
  background-size: 36px 36px;
}
.cg-panels { flex: 1; display: flex; overflow: hidden; }
.cg-panel { flex: 1; display: flex; flex-direction: column; position: relative; overflow: hidden; }
.cg-panel + .cg-panel { border-left: 1px solid var(--wire); }

/* Film-frame corner marks */
.cg-panel::before, .cg-panel::after {
  content: ''; position: absolute; width: 22px; height: 22px;
  border-color: rgba(196,98,45,.45); border-style: solid; z-index: 20; pointer-events: none;
}
.cg-panel::before { top: 14px; left: 14px; border-width: 1px 0 0 1px; }
.cg-panel::after  { bottom: 14px; right: 14px; border-width: 0 1px 1px 0; }

.cg-lbl {
  position: absolute; top: 14px; left: 50%; transform: translateX(-50%);
  z-index: 30; display: flex; align-items: center; gap: 6px;
  padding: 5px 14px; backdrop-filter: blur(8px);
  pointer-events: none; white-space: nowrap;
}
.cg-lbl-b { background: rgba(9,8,6,.75); border: 1px solid rgba(255,255,255,.1); color: var(--dim); }
.cg-lbl-a { background: rgba(196,98,45,.15); border: 1px solid rgba(196,98,45,.42); color: var(--ember); }
.cg-lbl-dot { width: 5px; height: 5px; border-radius: 50%; background: currentColor; opacity: .8; }
.cg-lbl-txt { font-family: var(--dn); font-size: .8rem; letter-spacing: .14em; }
.shiny-plot-output { background: var(--ink) !important; }

/* INFO BAR */
.cg-bar {
  height: 46px; background: var(--coal); border-top: 1px solid var(--wire);
  display: flex; align-items: center; padding: 0 22px; gap: 0; overflow: hidden;
}
.cg-bar-tag {
  font-family: var(--dn); font-size: .65rem; letter-spacing: .2em;
  color: var(--cu); margin-right: 18px; flex-shrink: 0;
}
pre.shiny-text-output {
  background: transparent !important; border: none !important;
  color: var(--dim) !important; font-family: var(--fn) !important;
  font-size: .72rem !important; letter-spacing: .04em !important;
  margin: 0 !important; padding: 0 !important;
  white-space: nowrap !important; overflow: hidden !important;
}
"

  # ---------------------------------------------------------------------------
  # UI
  # ---------------------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(css)),
      shiny::tags$title("CineGrade -- Cinematic Colour Grading")
    ),

    # Header
    shiny::tags$div(class = "cg-hdr",
      shiny::tags$div(style = "display:flex;align-items:baseline;",
        shiny::tags$span(class = "cg-wordmark",
          "CINE", shiny::tags$b("GRADE")),
        shiny::tags$span(class = "cg-sub",
          "k-means \u00b7 LAB space \u00b7 MTH 209")
      ),
      shiny::tags$div(class = "cg-badge", "v0.1.0")
    ),

    # Root
    shiny::tags$div(class = "cg-root",

      # === SIDEBAR ===
      shiny::tags$div(class = "cg-sidebar",

        shiny::tags$div(class = "cg-sec",
          shiny::tags$div(class = "cg-sec-title", "Target Image"),
          shiny::fileInput("target_file", NULL,
            accept = c("image/jpeg", "image/png"),
            buttonLabel = "\u2191  Upload Image",
            placeholder = "No file selected"
          )
        ),

        shiny::tags$div(class = "cg-sec",
          shiny::tags$div(class = "cg-sec-title", "Reference Grade"),
          shiny::radioButtons("ref_type", NULL,
            choices  = c("Built-in preset" = "preset", "Upload your own" = "upload"),
            selected = "preset"
          ),
          shiny::conditionalPanel("input.ref_type == 'preset'",
            shiny::selectInput("preset_name", NULL,
              choices = names(preset_paths), selected = "Wes Anderson")
          ),
          shiny::conditionalPanel("input.ref_type == 'upload'",
            shiny::fileInput("ref_file", NULL,
              accept = c("image/jpeg", "image/png"),
              buttonLabel = "\u2191  Upload Reference",
              placeholder = "No reference selected"
            )
          )
        ),

        shiny::tags$div(class = "cg-sec",
          shiny::tags$div(class = "cg-sec-title", "Colour Clusters (k)"),
          shiny::sliderInput("k", NULL,
            min = 3L, max = 20L, value = 8L, step = 1L, ticks = FALSE),
          shiny::tags$div(class = "cg-hint",
            "LOW k \u2014 stylized   \u00b7   HIGH k \u2014 detailed")
        ),

        shiny::tags$div(class = "cg-sec",
          shiny::actionButton("apply",
            shiny::HTML("\u25b6&nbsp; APPLY GRADE"),
            class = "btn-apply"),
          shiny::downloadButton("download_result",
            "\u2193  export result",
            class = "btn-download")
        )
      ),

      # === CANVAS ===
      shiny::tags$div(class = "cg-canvas",

        shiny::tags$div(class = "cg-panels",

          shiny::tags$div(class = "cg-panel",
            shiny::tags$div(class = "cg-lbl cg-lbl-b",
              shiny::tags$div(class = "cg-lbl-dot"),
              shiny::tags$div(class = "cg-lbl-txt", "BEFORE")
            ),
            shiny::plotOutput("before_plot",
              height = "calc(100vh - 108px)", width = "100%")
          ),

          shiny::tags$div(class = "cg-panel",
            shiny::tags$div(class = "cg-lbl cg-lbl-a",
              shiny::tags$div(class = "cg-lbl-dot"),
              shiny::tags$div(class = "cg-lbl-txt", "AFTER")
            ),
            shiny::plotOutput("after_plot",
              height = "calc(100vh - 108px)", width = "100%")
          )
        ),

        shiny::tags$div(class = "cg-bar",
          shiny::tags$div(class = "cg-bar-tag", "OUTPUT"),
          shiny::verbatimTextOutput("grade_info")
        )
      )
    )
  )

  # ---------------------------------------------------------------------------
  # Server  (logic unchanged)
  # ---------------------------------------------------------------------------
  server <- function(input, output, session) {

    target_img <- shiny::reactive({
      shiny::req(input$target_file)
      load_image(input$target_file$datapath)
    })

    ref_img_path <- shiny::reactive({
      if (input$ref_type == "preset") {
        preset_paths[[input$preset_name]]
      } else {
        shiny::req(input$ref_file)
        input$ref_file$datapath
      }
    })

    graded_img <- shiny::eventReactive(input$apply, {
      shiny::req(target_img(), ref_img_path())
      shiny::withProgress(message = "Applying colour grade\u2026", value = 0.5, {
        result <- apply_grade(
          target    = target_img(),
          reference = ref_img_path(),
          k         = input$k
        )
      })
      result
    })

    grade_dist <- shiny::eventReactive(input$apply, {
      shiny::req(target_img(), ref_img_path())
      compare_grades(target_img(), ref_img_path(), k = input$k)
    })

    output$before_plot <- shiny::renderPlot({
      shiny::req(target_img())
      graphics::par(bg = "#090806", mar = c(0, 0, 0, 0))
      plot(target_img(), axes = FALSE, rescale = FALSE)
    }, bg = "#09080600")

    output$after_plot <- shiny::renderPlot({
      shiny::req(graded_img())
      graphics::par(bg = "#090806", mar = c(0, 0, 0, 0))
      plot(graded_img(), axes = FALSE, rescale = FALSE)
    }, bg = "#09080600")

    output$grade_info <- shiny::renderText({
      if (input$apply == 0L)
        return("Upload an image and press  APPLY GRADE  to begin.")
      shiny::req(grade_dist())
      d   <- grade_dist()
      tag <- if (d < 5) "subtle" else if (d < 15) "similar" else
             if (d < 30) "moderate shift" else "strong transformation"
      ref_label <- if (input$ref_type == "preset") input$preset_name else "custom"
      paste0("Grade applied  \u00b7  ref: ", ref_label,
             "  \u00b7  k = ", input$k,
             "  \u00b7  \u0394E = ", round(as.numeric(d), 2),
             "  (", tag, ")")
    })

    output$download_result <- shiny::downloadHandler(
      filename = function()
        paste0("cinegraded_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg"),
      content = function(file) {
        shiny::req(graded_img())
        imager::save.image(graded_img(), file)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
