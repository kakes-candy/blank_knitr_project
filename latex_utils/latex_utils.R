# Maakt van een numerieke vector een character vector, met procent teken
latex_percent <- function (x, digits = 0, na_fill = "") {
  
  # Checks
  if(!is.numeric(x)) {stop("x must be numeric")}
  
  #afronden
  x <- round(x, digits = digits)
  
  # omzetten naar nederlands formaat
  x <- format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
  
  # procent teken erbij, NA vervangen door NA fill
  x <- ifelse(x %in% c("NA", "NaN"), na_fill, paste(x, "\\%"))
  
  return(x)
}


latex_decimal <- function (x, digits = 0, na_fill = "") {
  
  # Checks
  if(!is.numeric(x)) {return(as.character(x))}
  
  #afronden
  x <- round(x, digits = digits)
  
  # omzetten naar nederlands formaat
  x <- format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
  
  # procent teken erbij, NA vervangen door NA fill
  x <- ifelse(x %in% c("NA", "NaN"), na_fill, x)
  
  return(x)
}


latex_euro <- function (x, digits = 0, na_fill = "") {
  
  # Checks
  if(!is.numeric(x)) {stop("x must be numeric")}
  
  #afronden
  x <- round(x, digits = digits)
  
  # omzetten naar nederlands formaat
  x <- format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
  
  # procent teken erbij, NA vervangen door NA fill
  x <- ifelse(x %in% c("NA", "NaN"), na_fill, paste0("\\EUR{", x, "}"))
  
  return(x)
}


latex_italic <- function(x) {paste0("\\emph{", x, "}")}

latex_bold <- function(x) {paste0("\\textbf{", x, "}")}

latex_bold_italic <- function(x) {latex_bold(latex_italic(x))}



# Functie voor een simpele tabel
latex_table <- function(x, ...) {
  
  
  # tabbelen met gegevens over aantal unieke clienten
  out <- gsub("\\\\toprule|\\\\bottomrule", "", capture.output( 
    Hmisc::latex(x, file="", 
                 # booktabs =  TRUE, 
                 first.hline.double = FALSE,
                 size="scriptsize", 
                 here=TRUE, 
                 colnamesTexCmd = "bfseries", 
                 center = "none", 
                 na.blank = TRUE, 
                 col.just = c("l", rep("r", times = ncol(x) -1 )), 
                 rowname=NULL,
                 ...)
  ))
  
  return (out)
  
}

# Functie voor een simpele tabel
latex_kable <- function(x, ...) {
  
  
  # tabbelen met gegevens over aantal unieke clienten
  bold_names <- sapply(names(x), FUN = latex_bold, USE.NAMES = FALSE)
  out <- kable(x, format = 'latex', 
               col.names = bold_names
               ,escape = FALSE,
              ...)
  
  return (out)
  
}





#vector maken die kleuren van rijen aangeeft (afwisselen wit en een andere kleur)
rijkleuren <- function (x) { 
  v.out <- character()
  
  if(length(x) > 2){
    
    for(rij in 1:(length(x)-1)) {
      if(rij %% 2 == 0)
        v.out <- append(v.out, paste("\\rowcolor{bluerow}",  x[rij]))
      else {
        v.out <- append(v.out, paste("",  x[rij]))
      }
    }
    v.out <- append(v.out, paste("\\rowcolor{bluetotal}",  x[length(x)]))
  }  
  
  else if(length(x) == 2) {
    
    if(grepl("Totaal" , x[2])) {
      v.out <- append(v.out, paste("",  x[1]))
      v.out <- append(v.out, paste("\\rowcolor{bluetotal}",  x[2]))
    }
    
    else {
      v.out <- append(v.out, paste("",  x[1]))
      v.out <- append(v.out, paste("\\rowcolor{bluerow}",  x[2]))
    }
  }
  
  else if(length(x) == 1)
    v.out <- append(v.out, paste("",  x[1]))
  
}  