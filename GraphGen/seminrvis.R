# this file contains functions to generate DOT code from models from seminr



#class(model) consider tweaking according to class

# gets structural nodes description
getSMnodes <- function(model) {
  model$constructs %>% str_replace("\\*", "_x_") %>% paste0(collapse = "; ")
}


# gets measurement nodes description
getMMnodes <- function(model) {
  model$mmVariables %>%
    as_tibble() %>%
    filter(!str_detect(value, "\\*")) %>%
    pull(value) %>%
    #str_replace("\\*", "_x_") %>%
    paste0(collapse = "; ")
}



# get edges for structural model
getSMedges <- function(model, weights = 1, rounding = 3) {
  if ("boot_seminr_model" %in% class(model)) {
     cat("Using a bootstrapped PLS model")
      model$paths_descriptives
  } else {
    cat("Using an estimated PLS model")
  }

  nr <- nrow(model$smMatrix)
  nc <- ncol(model$smMatrix)
  sm <- model$smMatrix
  model$path_coef
  sm_edges <- ""

  for (i in 1:nrow(sm)) {
    #print(sm[i,1])
    coef <- round(model$path_coef[sm[i, 1], sm[i,2]], rounding)
    sm_edges <- paste0(sm_edges, sm[i, 1], " -> {", sm[i,2], "} [weight = ", weights, ", label = '𝞫=", coef, "' ]\n")
  }
  sm_edges %>% str_replace_all("\\*", "_x_")
}


# get edges for measurement model
# weights for dot (high value recommended for mm)
getMMedges <- function(model, weights = 1000, rounding = 3, use_outer_weights = FALSE) {
  mm <- model$mmMatrix
  #%>% as_tibble()
  #mm <- mm %>% filter(!str_detect(measurement, "\\*")) %>%
  mm_edges <- ""

 # use_outer_weights <- FALSE
  for (i in 1:nrow(mm)) {
    # i <- 1
    if (use_outer_weights) {
      loading = round(model$outer_weights[mm[i,2], mm[i, 1]], rounding)
    } else {
      loading = round(model$outer_loadings[mm[i,2], mm[i, 1]], rounding)
    }
    if (str_detect(mm[i, 2], "\\*")) {
      # show interaction indicators?
    } else {
      mm_edges <- paste0(mm_edges, mm[i, 2], " -> {", mm[i,1], "} [weight = ",weights, ", label = ", loading ," ]\n")
    }
  }
 mm_edges %>% str_replace_all("\\*", "_x_")
}


if (FALSE) {

  model <- bs_model
  getSMedges(pls_model)
  getSMedges(bs_model)

  getSMedges(pls_model) %>% cat()
  getSMedges(bs_model) %>% cat()
  getMMedges(bs_model, rounding = 3) %>% cat()
}





createEdgeStyle <- function(color = "black", fontsize = 7, fontname = "helvetica", forward = TRUE, minlen = NA){

  arrowdir <-
    "dir = both,
     arrowhead = none,
     arrowtail = normal"

  if (forward) {
    arrowdir <-
      "dir = both,
     arrowhead = normal,
     arrowtail = none"
  }

  minlen_str <- ""
  if (!is.na(minlen)) {
    minlen_str <- glue::glue("minlen = {minlen},")
  }

  glue::glue("
             color = {color},
             fontsize = {fontsize},
             fontname = {fontname},
              {minlen_str}
              {arrowdir}
             ")
}

createOuterWeightStyle <- function(color = "dimgray", fontsize = 7, forward = FALSE, minlen = 1){
  createEdgeStyle(color = color,
                  fontsize = fontsize,
                  forward = forward, minlen = minlen)
}

createInnerWeightStyle <- function(color = "black", fontsize = 9, forward = TRUE){
  createEdgeStyle(color = color, fontsize = fontsize, forward = forward)
}



createItemStyle <- function(color = "dimgray", fill = "white", fontsize = 8, height = 0.2, width = 0.4){
  createStyle(shape = "box",
              color = color,
              fontsize = fontsize,
              fill = fill,
              width = width,
              height = height)
}

createConstructStyle <- function(color = "black", fill = "white", fontsize = 12, height = 0.5, width = 1){
  createStyle(shape = "ellipse",
              color = color,
              fill = fill,
              fontsize = fontsize,
              width = width,
              height = height)
}

createStyle <- function(shape = "box", color = "dimgray", fill = "white", fontsize = 8, height = 0.2, width = 0.4, fontname = "helvetica"){
  style <- glue::glue("
              shape = {shape},
              color = {color},
              fillcolor = {fill},
              style = filled,
              fontsize = {fontsize},
              height = {height},
              width = {width},
              fontname = {fontname},
              fixedsize = true
        ")
  style

}





plot_model <- function(model, use_outer_weights = FALSE,
                       title = "",
                       title_font = "helvetica",
                       title_size = 24,
                       item_style = createItemStyle(),
                       construct_style = createConstructStyle(),
                       outer_weight_style = createOuterWeightStyle(),
                       inner_weight_style = createInnerWeightStyle()
                       ){

  sm_nodes <- getSMnodes(model)
  mm_nodes <- getMMnodes(model)
  sm_edges <- getSMedges(model)
  mm_edges <- getMMedges(model, use_outer_weights = use_outer_weights)


  # TODO
  cat(model$rSquared)


  # use rank=same; A; B; C; to force same level on items of construct.

  glue(
    "digraph G {{
      // general graph settings
      graph [
        charset = 'UTF-8',
        layout = dot,
        label = '{title}',
        fontsize = {title_size},
        fontname = {title_font},
        rankdir = LR,
        labelloc = t,
        //splines = false
      ]

      // The structural model
      subgraph sm {{
        rankdir = LR;
        node [
          {construct_style}
        ]
        {sm_nodes}

        // How constructs are connected
        edge [
        {inner_weight_style}
        ]
        {sm_edges}
      }

      // ---------------------
      // The measurement model
      // ---------------------
      subgraph mm {{
        node [
          {item_style}
        ]
        {mm_nodes}

        // How items are connected with nodes
        edge [
        {outer_weight_style}
        ]
        {mm_edges}
      }
  }"
  )
}






