## RHANDSONTABLE DATA EDITOR ---------------------------------------------------

#' Interactive data editor
#'
#' @param x object of class matrix or data.frame with colnames specified.
#' @param title title to include in above the table.
#' @param type can be either "editor", "menu" or "selector", set to "editor" by
#'   default. The editor option allows complete editing of the data_matrix with
#'   text input. The menu option converts the entries in the last column to
#'   checkboxes for logical inputs. The selector option converts entries in the
#'   last column to dropdown menus that contain choices supplied to the
#'   \code{options} argument.
#' @param options vector of options to use in dropdown menus in the last column.
#' @param save_as name of a csv file to which the edited table should be saved.
#' @param viewer logical indicating whether the table editor should be launched
#'   in the RStudio viewer pane, set to TRUE by default. Make sure to use the
#'   "Save & Close" to close the data editor or else the changes will not be
#'   saved.
#' @param logo path to image to include a logo in data editor title panel, set
#'   to CytoExploreR by default.
#'
#' @importFrom shiny shinyApp fluidPage titlePanel mainPanel runApp onStop img
#'   div paneViewer observeEvent
#' @importFrom bslib bs_theme
#' @importFrom rhandsontable rHandsontableOutput renderRHandsontable
#'   rhandsontable hot_col hot_to_r
#' @importFrom utils read.csv write.csv
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @export
data_editor <- function(x,
                        title = "Data Editor",
                        options = NULL,
                        save_as = NULL,
                        viewer = TRUE,
                        logo = "CytoExploreR") {
  
  
  # PULL DOWN ROW NAMES
  row_names <- rownames(x)
  
  # CONVERT TO CHRACTER MATRIX
  if(is(x, "data.frame")){
    x <- as.matrix(x)
  }
  

    # MOVE COLNAMES INTO MATRIX
    if(is.null(colnames)){
      stop("colnames must be assigned!")
    }else{
      x <- rbind(colnames(x), x)
    }
  # MENU DATA
  
  
  # CONVERT TO DATA FRAME
  x <- data.frame(x, stringsAsFactors = FALSE)
  
  # CytoExploreR logo from GitHub man folder
  if(logo == "CytoExploreR"){
    logo <- paste0(
      "https://raw.githubusercontent.com/DillonHammill/CytoExploreR",
      "/master/man/figures/logo.png"
    )
  }
  
  # TEMP_FILE
  temp_file <- NULL

  # RUN DATA EDITOR - INTERACTIVE MODE ONLY

  # EDITOR DATA
    # UPDATE COLUMN NAMES
    colnames(x) <- x[1, ]
    # REMOVE COLNAMES FROM MATRIX
    x <- x[-1, , drop = FALSE]
    # CONVERT NUMERIC CHARACTERS
    lapply(seq_len(ncol(x)), function(z){
      # NUMBERS TO NUMERIC
      if(all(grepl("^[0-9 ]+$", x[, z]))){
        x[, z] <<- as.numeric(x[, z])
     }
    })
    # CONVERT EMPTY CHARACTERS TO NA
    lapply(seq_len(ncol(x)), function(z){
      if(any(LAPPLY(x[, z], ".empty"))){
        x[,z][which(LAPPLY(x[, z], ".empty"))] <<- NA
      }
    })
    # ADD BACK ROW NAMES
    rownames(x) <- row_names

  # WRITE TO FILE
  if(!is.null(save_as)){
    write.csv(x,
              save_as,
              row.names = FALSE)
  }
  
  # RETURN UPDATED DATA MATRIX
  return(x)
  
}