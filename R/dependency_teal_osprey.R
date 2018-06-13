check_arm_ref_comp <- function(x, ASL, module) {
  
  msg <- paste("module", module, "argument arm_ref_comp ")
  
  if (!is.null(x)) {
    
    if (!is.list(x)) stop(msg, "needs to be a list or NULL")
    
    vars <- names(x)
    if (is.null(vars) || any(vars == "")) stop(msg, "is not named")
    
    if (!all(vars %in% names(ASL))) stop(msg, "refers to variables that are not in ASL")
    
    Map(function(xi, var) {
      if (!is.list(xi)) stop(msg, "definition for arm variable ", var, " list element needs to be lists with ref and comp elements")
      
      rc <- names(xi)
      if (is.null(rc) || !identical(sort(rc), c('comp', 'ref'))) stop(msg, "definition for arm variable ", var , " nested list needs to have the elements ref and comp")
      
      arm_levels <- unlist(xi)
      
      if (!all(arm_levels %in% ASL[[var]])) stop(msg, "definition for arm variable ", var , " refers to arm levels that do not exist in ASL[[",var,"]]")
    }, x, vars)
  }
  
  invisible(TRUE)
  
}

get_rcode_header <- function(title, datanames, datasets, code_data_processing = NULL) {
  
  datanames <- c("ASL", setdiff(datanames, "ASL"))
  
  datanames_import <- if (is.null(code_data_processing)) {
    datanames
  } else {
    datasets$datanames() # because the code_data_processing might have references to all datasets
  }
  
  data <- lapply(datanames_import, function(x)datasets$get_data(x, reactive = FALSE, filtered=FALSE))
  names(data) <- datanames_import
  
  comment <- function(txt) {
    paste0("# ", gsub("\n", "\n# ", txt, fixed = TRUE))  
  }
  
  if (!has_source_attribute(data)) {
    "# not all datasets have the 'source' attribute, please contact the app maintainer"
  } else {
    
    info <- Sys.info()
    
    txt_location <- if (info['nodename'] == 'rkaub00459.kau.roche.com') {
      sub("/opt/bee_tools/shiny/",  "https://shiny.roche.com/", getwd(), fixed = TRUE)
    } else {
      getwd()
    }
    
    
    
    
    
    
    
    txt_data <- paste(
      unlist(Map(function(x, name) {
        txt <- paste(name, "<-", attr(x, "source"))
        md5 <- attr(x, "md5sum")
        if (is.null(md5)) txt else paste(txt, "# md5sum:", md5)
      }, data, names(data))),
      collapse = "\n"
    )
    
    txt_data_processing <- if (is.null(code_data_processing)) {
      ""
    } else {
      paste(c(code_data_processing, ""), collapse = "\n")
    }
    
    txt_filter <- teal::get_filter_txt(datanames, datasets)    
    
    
    ## header
    txt_inst_pkgs <- c(
      paste0('devtools::install_github("Roche/rtables", ref="v',
             packageDescription("rtables")$Version,'")'),
      paste0('devtools::install_github("Rpackages/tern", ref="v',
             packageDescription("tern")$Version,'", host="https://github.roche.com/api/v3")')
    )
    
    needs_rcd <- any(grepl("radam\\(", txt_data))
    if (needs_rcd) {
      txt_inst_pkgs <- c(
        paste0('devtools::install_github("Rpackages/random.cdisc.data", ref="v',
               packageDescription("random.cdisc.data")$Version,'", host="https://github.roche.com/api/v3")'),
        txt_inst_pkgs
      )
    }  
    
    
    txt_comment <- paste(c(
      title,
      "",
      paste("Output Created on", format(Sys.time(), "%b %d, %Y"), "by", info['user']),
      paste("with teal app under", txt_location), 
      "",
      paste(R.version$version.string, "on", info['nodename']), 
      "",
      txt_inst_pkgs
    ), collapse = "\n")
    
    
    paste(
      comment(txt_comment),
      "",
      "library(tern)",
      if (needs_rcd) "library(random.cdisc.data)" else NULL,
      "",
      txt_data,
      "",
      txt_data_processing,
      txt_filter,
      "",
      sep = "\n"
    )
  }
  
  
  
}


has_source_attribute <- function(x) {
  if (is.data.frame(x)) x <- list(x)
  all(vapply(x, function(dat) !is.null(attr(dat, "source")), logical(1)))
}

#' Check if list or data.frame has elements/variables
#' 
#' Checks if list names exist and throws an error otherwise
#' 
#' @param data a \code{data.frame} or named list
#' @param names vector with names
#' 
#' @return \code{TRUE} if all variables exist and an appropriate error if not.
#' 
#' @noRd
#' 
#' @author Adrian Waddell (waddella), \email{adrian.waddell@roche.com}
#' 
`%needs%` <- function(data, names) {
  
  i <- is.na(match(names, names(data)))
  
  if (any(i)) {
    msg <- if (sum(i) == 1) {
      paste("variable ", names[i], " does not exist")
    } else {
      paste("variables", paste(names[i], collapse = ", "), "do not exist")
    }
    stop(msg)
  }
  
  invisible(TRUE)
}




whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", shiny::tags$div(style = "overflow-x: auto;",...))
}


as.global <- function(...) {
  
  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)
  
  args <- list(...)
  
  ge <- globalenv()
  
  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)
  
}



# x <- c("{", "     ", "    a <- 3", "}    ")
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