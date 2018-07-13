has_source_attribute <- function(x) {
  if (is.data.frame(x)) x <- list(x)
  all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}

whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", shiny::tags$div(style = "overflow-x: auto;",...))
}

remove_enclosing_curly_braces <- function(x) {
  if (!is.character(x)) stop("x needs to be a character")
  
  if (length(x) == 0) {
    x
  } else {
    txt <- unlist(strsplit(x, "\n", fixed = TRUE))
    if (grepl("^\\{[[:space:]]*", txt[1]) && grepl("^\\}[[:space:]]*", tail(txt, 1))) {
      txt2 <- txt[-c(1, length(txt))]
      
      n_spaces_indent <- vapply(txt2, function(txt_i) {
        if (grepl("^[[:space:]]*$", txt_i)) {
          NA_integer_
        } else {
          txt_i <- "    a <-   "
          i <- which(diff(which(unlist(strsplit(txt_i, " ", fixed = TRUE)) == "")) != 1)[1] 
          
          if (length(i) == 1 && i > 0) {
            i
          } else {
            NA_integer_
          }
        }
      }, numeric(1), USE.NAMES = FALSE)
      
      if (sum(!is.na(n_spaces_indent))>0) {
        n_rm <-  min(n_spaces_indent, na.rm = TRUE) 
        gsub(paste0("^[[:space:]]{", n_rm, "}"), "", txt2)
      } else {
        txt2
      }
      
      
      
    } else {
      txt
    }
  }
}

validate_standard_inputs <- function(ASL = NULL, aslvars = character(0),
                                     ANL = NULL, anlvars = character(0),
                                     arm_var = NULL,
                                     ref_arm,
                                     comp_arm,
                                     min_n_levels_armvar = 2,
                                     min_nrow = 15) {
  
  validate_has_data(ASL, min_nrow = min_nrow)
  validate_has_data(ANL, min_nrow = min_nrow)    
  
  if (length(aslvars) > 0) validate_has_variable(ASL, c(aslvars, arm_var))  
  if (length(anlvars) > 0) validate_has_variable(ANL, anlvars) 
  
  
  if (!is.null(arm_var) && !is.null(min_n_levels_armvar)) {
    validate_n_levels(ASL[[arm_var]], more_than = 1, less_than = 15,
                      msg = "arm variable needs more than 2 levels and less than 15 levels")
    
    validate(need(!("" %in% ASL[[arm_var]]), "arm values can not contain empty strings ''"))
    
  }
  
  if (!missing(comp_arm)) validate_has_elements(comp_arm, "need comparison arms")
  if (!missing(ref_arm)) validate_has_elements(ref_arm, "need reference arms")
  
  if (!missing(comp_arm) && !missing(ref_arm))  {
    
    validate_no_intersection(comp_arm, ref_arm, "reference and comparison arms cannot overlap")
    
    if (!is.null(arm_var)) {
      validate_in(c(comp_arm, ref_arm), ASL[[arm_var]],
                  "current ASL data does not have observations from the reference and comparison arms")
    }
  }
  
}


validate_has_data <- function(x, min_nrow = NULL) {
  validate(need(!is.null(x) && is.data.frame(x), "no data left"))
  
  if (!is.null(min_nrow)) {
    validate(need(nrow(x) >= min_nrow , paste("need more than", min_nrow, "observations")))
  }
  
}

# Also check if USUBJID, STUDYID matches
validate_one_row_per_id <- function(x, key = c("USUBJID", "STUDYID")) {
  validate(need(!any(duplicated(x[key])) , paste("more then one row per id")))
}

validate_in <- function(x, choices, msg) {
  validate(need(length(x) > 0 && length(choices) > 0  && all(x %in% choices), msg))  
}

validate_has_elements <- function(x, msg) {
  validate(need(length(x) > 0, msg))
}

validate_no_intersection <- function(x, y, msg) {
  validate(need(length(intersect(x, y)) == 0, msg))
}

validate_has_variable <- function(data, varname, msg) {
  
  if (length(varname) != 0) {
    has_vars <- all(varname %in% names(data))
    has_all <- all(has_vars)
    
    if (!has_all) {
      if (missing(msg)) {
        dataname <- deparse(substitute(data))
        msg <- paste(dataname, "does not not have the required variables:",
                     paste(varname[!has_vars], collapse = ", "))
      } 
      validate(need(FALSE, msg))    
    }
  }
  
}

validate_n_levels <- function(x, more_than = 2, less_than = 12, msg) {
  
  x_levels <- if (is.factor(x)) {
    levels(x)
  } else {
    unique(x)
  }
  
  validate(need(length(x_levels) > more_than &&  length(x_levels) < less_than, msg))
  
}