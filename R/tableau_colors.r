# Purpose: Create XML code to generate custom color palettes for Tableau
# Date: 2017_12_08
# Author: Tim Essam, GeoCenter
# Notes: Materials prepared for Tableau training 

#FROM : https://github.com/dKvale/tableau
# http://dkvale.github.io/tableau/
# Creaate color palettes for tableau 
##-- Create a Tableau palette function
##-- Reference: http://kb.tableau.com/articles/knowledgebase/creating-custom-color-palettes

tableau_palette <- function(color_list = c("#FBFBFB", "#DFDFDF"), 
                            name = "New Palette") {
  
out <- paste0(c("<workbook> \n\t <preferences>
                <color-palette name=\"", name, "\" type=\"regular\"> \n",
                paste("\t\t\t <color>", color_list, "</color> \n"),
                "\t\t </color-palette> \n\t </preferences> \n </workbook>"), 
              collapse="")

cat(out)
}

tableau_palette(mpca, "MPCA Logo")

tableau_palette(force_awakens, "Force Awakens")

tableau_palette(wes_palettes[["FantasticFox"]], "Fox")
#**************************************************************************************



# Load required libraries -------------------------------------------------
library(XML)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(datapasta)


# Color palettes ----------------------------------------------------------

# Color Brewer
Reds    = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D")
Oranges = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704")
OrRd    = c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000")
YlOrRd  = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
YlOrBr  = c("#FFFFE5", "#FFF7BC", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#993404", "#662506")
YlGn    = c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529")
Greens  = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B")
BuGn    = c("#F7FCFD", "#E5F5F9", "#CCECE6", "#99D8C9", "#66C2A4", "#41AE76", "#238B45", "#006D2C", "#00441B")
YlGnBu  = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
GnBu    = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081")
PuBuGn  = c("#FFF7FB", "#ECE2F0", "#D0D1E6", "#A6BDDB", "#67A9CF", "#3690C0", "#02818A", "#016C59", "#014636")
Blues   = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
PuBu    = c("#FFF7FB", "#ECE7F2", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#045A8D", "#023858")
Purples = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
BuPu    = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B")
RdPu    = c("#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A")
PuRd    = c("#F7F4F9", "#E7E1EF", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F")
Greys   = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000")

# Color Brewer diverging palettes 
PiYG    = c("#8E0152", "#C51B7D", "#DE77AE", "#F1B6DA", "#FDE0EF", "#F7F7F7", "#E6F5D0", "#B8E186", "#7FBC41", "#4D9221", "#276419")
PRGn    = c("#40004B", "#762A83", "#9970AB", "#C2A5CF", "#E7D4E8", "#F7F7F7", "#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441B")
PuOr    = c("#7F3B08", "#B35806", "#E08214", "#FDB863", "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", "#2D004B")
BrBG    = c("#543005", "#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", "#F5F5F5", "#C7EAE5", "#80CDC1", "#35978F", "#01665E", "#003C30")
RdYlGn  = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
RdYlBu  = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE090", "#FFFFBF", "#E0F3F8", "#ABD9E9", "#74ADD1", "#4575B4", "#313695")
RdBu    = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")
RdGy    = c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#E0E0E0", "#BABABA", "#878787", "#4D4D4D", "#1A1A1A")
Spectral = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

# Accent Categoricals
Accent  = c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#BF5B17", "#666666")
Dark2   = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
Paired  = c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
Pastel1 = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4", "#FED9A6", "#FFFFCC", "#E5D8BD", "#FDDAEC", "#F2F2F2")
Pastel2 = c("#B3E2CD", "#FDCDAC", "#CBD5E8", "#F4CAE4", "#E6F5C9", "#FFF2AE", "#F1E2CC", "#CCCCCC")
Set1    = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")
Set2    = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
Set3    = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F")

# carto DB colors
lipstick1 = c("#ffc6c4", "#f4a3a8", "#e38191", "#cc607d", "#ad466c", "#8b3058", "#672044")
lipstick2 = c("#fbe6c5", "#f5ba98", "#ee8a82", "#dc7176", "#c8586c", "#9c3f5d", "#70284a")
lipstick3 = c("#f6d2a9", "#f5b78e", "#f19c7c", "#ea8171", "#dd686c", "#ca5268", "#b13f64")
reds1_carto = c("#ecda9a", "#efc47e", "#f3ad6a", "#f7945d", "#f97b57", "#f66356", "#ee4d5a")
reds2_carto =c("#fde0c5", "#facba6", "#f8b58b", "#f59e72", "#f2855d", "#ef6a4c", "#eb4a40")
reds3_carto = c("#fef6b5", "#ffdd9a", "#ffc285", "#ffa679", "#fa8a76", "#f16d7a", "#e15383")

seagreen = c("#e4f1e1", "#b4d9cc", "#89c0b6", "#63a6a0", "#448c8a", "#287274", "#0d585f")
seagreen1 = c("#c4e6c3", "#96d2a4", "#6dbc90", "#4da284", "#36877a", "#266b6e", "#1d4f60")
seagreen2 = c("#d2fbd4", "#a5dbc2", "#7bbcb0", "#559c9e", "#3a7c89", "#235d72", "#123f5a")
seagreen3 = c("#d3f2a3", "#97e196", "#6cc08b", "#4c9b82", "#217a79", "#105965", "#074050")
seagreen4 = c("#f7feae", "#b7e6a5", "#7ccba2", "#46aea0", "#089099", "#00718b", "#045275")
seagreen5 = c("#d1eeea", "#a8dbd9", "#85c4c9", "#68abb8", "#4f90a6", "#3b738f", "#2a5674")
seagreen6 = c("#b0f2bc", "#89e8ac", "#67dba5", "#4cc8a3", "#38b2a3", "#2c98a0", "#257d98")

purp1_carto = c("#f3e0f7", "#e4c7f1", "#d1afe8", "#b998dd", "#9f82ce", "#826dba", "#63589f")
purp2_carto = c("#f9ddda", "#f2b9c4", "#e597b9", "#ce78b3", "#ad5fad", "#834ba0", "#573b88")
purp3_carto = c("#f3e79b", "#fac484", "#f8a07e", "#eb7f86", "#ce6693", "#a059a0", "#5c53a5")
purp4_carto = c("#f3cbd3", "#eaa9bd", "#dd88ac", "#ca699d", "#b14d8e", "#91357d", "#6c2167")
purp5_carto = c("#fcde9c", "#faa476", "#f0746e", "#e34f6f", "#dc3977", "#b9257a", "#7c1d6f")
purp6_carto = c("#ede5cf", "#e0c2a2", "#d39c83", "#c1766f", "#a65461", "#813753", "#541f3f")

# Divergent schemes
grn_pnk = c("#798234", "#a3ad62", "#d0d3a2", "#fdfbe4", "#f0c6c3", "#df91a3", "#d46780")
grn_org = c("#3d5941", "#778868", "#b5b991", "#f6edbd", "#edbb8a", "#de8a5a", "#ca562c")
trq_org = c("#008080", "#70a494", "#b4c8a8", "#f6edbd", "#edbb8a", "#de8a5a", "#ca562c")
trq_pnk = c("#009392", "#39b185", "#9ccb86", "#e9e29c", "#eeb479", "#e88471", "#cf597e")
trq_pnk2 = c("#009392", "#72aaa1", "#b1c7b3", "#f1eac8", "#e5b9ad", "#d98994", "#d0587e")
trq_prp = c("#009B9E", "#42B7B9", "#A7D3D4", "#F1F1F1", "#E4C1D9", "#D691C1", "#C75DAB")
brn_blu = c("#A16928", "#bd925a", "#d6bd8d", "#edeac2", "#b5c8b8", "#79a7ac", "#2887a1")

# Qualitative schemes
qual1 = c("#855C75", "#D9AF6B", "#AF6458", "#736F4C", "#526A83", "#625377", "#68855C", "#9C9C5E", "#A06177", "#8C785D", "#467378", "#7C7C7C")
qual2 = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A", "#E68310", "#008695", "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99")
qual3 = c("#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2", "#87C55F", "#9EB9F3", "#FE88B1", "#C9DB74", "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3")
qual4 = c("#5F4690", "#1D6996", "#38A6A5", "#0F8554", "#73AF48", "#EDAD08", "#E17C05", "#CC503E", "#94346E", "#6F4070", "#994E95", "#666666")
qual5 = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
qual6 = c("#E58606", "#5D69B1", "#52BCA3", "#99C945", "#CC61B0", "#24796C", "#DAA51B", "#2F8AC4", "#764E9F", "#ED645A", "#CC3A8E", "#A5AA99")



# Function to generate XML output -----------------------------------------


# Function to create XML code for Tableau custom colors.
# Requires
# @data = character vector of hex codes for colors
# @name = name of the color palette (string)
# @type = regular, ordered-sequential, ordered-diverging 

xml_colors = function(data, name = "NewColors", type ="ordered-sequential") {
  
  # Default values are name = NewColors and type = ordered-sequential
  top = newXMLNode('color-palette', attrs = c(name = name, type = type))
  lapply(data, function(x) {newXMLNode('color', newXMLTextNode(x), parent = top)})
  
  return(top)
}

# Test function
xml_colors(Accent, "BrownBlue", "ordered-diverging")


# TODO: Create a list of color palette names, map the list to the palette description,
# and to the palette type so the function call can be vectorized and will write the
# XML code to a file\

# Use purrr:map values call to accomplish this?







palettes <- as.list(ls())
tmp <- newcolors <- tibble::tribble(
  ~palette,              ~name,                 ~type,
  "brn_blu",        "BrownBlue",  "ordered-diverging",
  "grn_org",      "GreenOrange",   "ordered-diverging"
)

# Ordered Diverging
xml_colors(brn_blu, "BrownBlue", "ordered-diverging")
xml_colors(grn_org, "GreenOrange", "ordered-diverging")
xml_colors(grn_pnk, "GreenPink", "ordered-diverging")
xml_colors(trq_org, "TurquoiseOrange", "ordered-diverging")
xml_colors(trq_pnk, "TurquoisePink", "ordered-diverging")
xml_colors(trq_pnk2, "TurquoisePink2", "ordered-diverging")
xml_colors(trq_prp, "TurquoisePurple", "ordered-diverging")
xml_colors(PiYG, "PiYG CB", "ordered-diverging")
xml_colors(PRGn, "PRGn CB", "ordered-diverging")
xml_colors(PuOr, "PuOr CB", "ordered-diverging")
xml_colors(BrBG, "BrBG CB", "ordered-diverging")
xml_colors(RdYlGn, "RdYlGn CB", "ordered-diverging")
xml_colors(RdYlBu, "RdYlBu CB", "ordered-diverging")
xml_colors(RdBu, "RdBu CB", "ordered-diverging")
xml_colors(RdGy, "RdGy CB", "ordered-diverging")
xml_colors(Spectral, "Spectral CB", "ordered-diverging")

# Ordered sequential
xml_colors(lipstick1, "Lipstick1", "ordered-sequential")
xml_colors(lipstick2, "Lipstick2", "ordered-sequential")
xml_colors(lipstick3, "Lipstick3", "ordered-sequential")
xml_colors(purp1_carto, "Purples1", "ordered-sequential")
xml_colors(purp2_carto, "Purples2", "ordered-sequential")
xml_colors(purp3_carto, "Purples3", "ordered-sequential")
xml_colors(purp4_carto, "Purples4", "ordered-sequential")
xml_colors(purp5_carto, "Purples5", "ordered-sequential")
xml_colors(purp6_carto, "Purples6", "ordered-sequential")
xml_colors(reds1_carto, "Reds1", "ordered-sequential")
xml_colors(reds2_carto, "Reds2", "ordered-sequential")
xml_colors(reds3_carto, "Reds3", "ordered-sequential")
xml_colors(seagreen, "Seagreen", "ordered-sequential")
xml_colors(seagreen1, "Seagreen 1", "ordered-sequential")
xml_colors(seagreen2, "Seagreen 2", "ordered-sequential")
xml_colors(seagreen3, "Seagreen 3", "ordered-sequential")
xml_colors(seagreen4, "Seagreen 4", "ordered-sequential")
xml_colors(seagreen5, "Seagreen 5", "ordered-sequential")
xml_colors(seagreen6, "Seagreen 6", "ordered-sequential")
xml_colors(Reds, "Reds CB", "ordered-sequential")
xml_colors(Oranges, "Oranges CB", "ordered-sequential")
xml_colors(OrRd, "OrRd CB", "ordered-sequential")
xml_colors(YlOrRd, "YlOrRd CB", "ordered-sequential")
xml_colors(YlOrBr, "YlOrBr CB", "ordered-sequential")
xml_colors(YlGn, "YlGn CB", "ordered-sequential")
xml_colors(Greens, "Greens CB", "ordered-sequential")
xml_colors(BuGn, "BuGn CB", "ordered-sequential")
xml_colors(YlGnBu, "YlGnBu CB", "ordered-sequential")
xml_colors(GnBu, "GnBu CB", "ordered-sequential")
xml_colors(PuBuGn, "PuBuGn CB", "ordered-sequential")
xml_colors(Blues, "Blues CB", "ordered-sequential")
xml_colors(PuBu, "PuBu CB", "ordered-sequential")
xml_colors(Purples, "Purples CB", "ordered-sequential")
xml_colors(BuPu, "BuPu CB", "ordered-sequential")
xml_colors(RdPu, "RdPu CB", "ordered-sequential")
xml_colors(PuRd, "PuRd CB", "ordered-sequential")
xml_colors(Greys, "Greys CB", "ordered-sequential")

# Qualitative colors
xml_colors(qual1, "Qualitative1", "regular")
xml_colors(qual2, "Qualitative2", "regular")
xml_colors(qual3, "Qualitative3", "regular")
xml_colors(qual4, "Qualitative4", "regular")
xml_colors(qual5, "Qualitative5", "regular")
xml_colors(qual6, "Qualitative6", "regular")
xml_colors(Accent, "Accent CB", "regular")
xml_colors(Dark2, "Dark2 CB", "regular")
xml_colors(Paired, "Paired CB", "regular")
xml_colors(Pastel1, "Pastel1 CB", "regular")
xml_colors(Pastel2, "Pastel2 CB", "regular")
xml_colors(Set1, "Set1 CB", "regular")
xml_colors(Set2, "Set2 CB", "regular")
xml_colors(Set3, "Set3 CB", "regular")




# Data of color palette details -------------------------------------------



# Save data as a tibble for later calling in a function
color_palettes <- tibble::tribble(
           ~name,           ~palette,                 ~type,
       "brn_blu",        "BrownBlue",   "ordered-diverging",
       "grn_org",      "GreenOrange",   "ordered-diverging",
       "grn_pnk",        "GreenPink",   "ordered-diverging",
       "trq_org",  "TurquoiseOrange",   "ordered-diverging",
       "trq_pnk",    "TurquoisePink",   "ordered-diverging",
      "trq_pnk2",   "TurquoisePink2",   "ordered-diverging",
       "trq_prp",  "TurquoisePurple",   "ordered-diverging",
          "PiYG",          "PiYG CB",   "ordered-diverging",
          "PRGn",          "PRGn CB",   "ordered-diverging",
          "PuOr",          "PuOr CB",   "ordered-diverging",
          "BrBG",          "BrBG CB",   "ordered-diverging",
        "RdYlGn",        "RdYlGn CB",   "ordered-diverging",
        "RdYlBu",        "RdYlBu CB",   "ordered-diverging",
          "RdBu",          "RdBu CB",   "ordered-diverging",
          "RdGy",          "RdGy CB",   "ordered-diverging",
      "Spectral",      "Spectral CB",   "ordered-diverging",
     "lipstick1",        "Lipstick1",  "ordered-sequential",
     "lipstick2",        "Lipstick2",  "ordered-sequential",
     "lipstick3",        "Lipstick3",  "ordered-sequential",
   "purp1_carto",         "Purples1",  "ordered-sequential",
   "purp2_carto",         "Purples2",  "ordered-sequential",
   "purp3_carto",         "Purples3",  "ordered-sequential",
   "purp4_carto",         "Purples4",  "ordered-sequential",
   "purp5_carto",         "Purples5",  "ordered-sequential",
   "purp6_carto",         "Purples6",  "ordered-sequential",
   "reds1_carto",            "Reds1",  "ordered-sequential",
   "reds2_carto",            "Reds2",  "ordered-sequential",
   "reds3_carto",            "Reds3",  "ordered-sequential",
      "seagreen",         "Seagreen",  "ordered-sequential",
     "seagreen1",       "Seagreen 1",  "ordered-sequential",
     "seagreen2",       "Seagreen 2",  "ordered-sequential",
     "seagreen3",       "Seagreen 3",  "ordered-sequential",
     "seagreen4",       "Seagreen 4",  "ordered-sequential",
     "seagreen5",       "Seagreen 5",  "ordered-sequential",
     "seagreen6",       "Seagreen 6",  "ordered-sequential",
          "Reds",          "Reds CB",  "ordered-sequential",
       "Oranges",       "Oranges CB",  "ordered-sequential",
          "OrRd",          "OrRd CB",  "ordered-sequential",
        "YlOrRd",        "YlOrRd CB",  "ordered-sequential",
        "YlOrBr",        "YlOrBr CB",  "ordered-sequential",
          "YlGn",          "YlGn CB",  "ordered-sequential",
        "Greens",        "Greens CB",  "ordered-sequential",
          "BuGn",          "BuGn CB",  "ordered-sequential",
        "YlGnBu",        "YlGnBu CB",  "ordered-sequential",
          "GnBu",          "GnBu CB",  "ordered-sequential",
        "PuBuGn",        "PuBuGn CB",  "ordered-sequential",
         "Blues",         "Blues CB",  "ordered-sequential",
          "PuBu",          "PuBu CB",  "ordered-sequential",
       "Purples",       "Purples CB",  "ordered-sequential",
          "BuPu",          "BuPu CB",  "ordered-sequential",
          "RdPu",          "RdPu CB",  "ordered-sequential",
          "PuRd",          "PuRd CB",  "ordered-sequential",
         "Greys",         "Greys CB",  "ordered-sequential",
         "qual1",     "Qualitative1",             "regular",
         "qual2",     "Qualitative2",             "regular",
         "qual3",     "Qualitative3",             "regular",
         "qual4",     "Qualitative4",             "regular",
         "qual5",     "Qualitative5",             "regular",
         "qual6",     "Qualitative6",             "regular",
        "Accent",        "Accent CB",             "regular",
         "Dark2",         "Dark2 CB",             "regular",
        "Paired",        "Paired CB",             "regular",
       "Pastel1",       "Pastel1 CB",             "regular",
       "Pastel2",       "Pastel2 CB",             "regular",
          "Set1",          "Set1 CB",             "regular",
          "Set2",          "Set2 CB",             "regular",
          "Set3",          "Set3 CB",             "regular"
  )

