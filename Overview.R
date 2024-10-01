#' ########################################################################### #
#' PROJECT: [BioDT] 
#' CONTENTS: 
#'  - Network Analysis and Visualisation of European Biodiversity Research Infrastructures
#'  DEPENDENCIES:
#'  - ECA countries (v2023-04) - ECA Countries.csv [https://docs.google.com/spreadsheets/d/1LDDrj4sZiLyjH2s9nWhVbRf2A2RLUbA6tD3jBYvje4o/edit#gid=1141004453]
#' AUTHOR: [Erik Kusch]
#' ########################################################################### #

# PREAMBLE =====================================================================
rm(list=ls())

## Directories ------
### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Contacts <- file.path(Dir.Base, "RI Contacts")
Dir.Logos <- file.path(Dir.Base, "RI Logos")
Dir.Exports <- file.path(Dir.Base, "Exports")
### Create directories which aren't present yet
Dirs <- c(Dir.Exports)
CreateDir <- sapply(Dirs, function(x) if(!dir.exists(x)) dir.create(x))

## Packages ------
install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos='http://cran.us.r-project.org', dependencies = TRUE)
  require(x, character.only = TRUE)
}
# names of the packages required placed here as character objects
package_vec <- c(
 "ggplot2",
 "ggspatial",
 "igraph",
 "ggraph",
 "sf",
 "rnaturalearth",
 "rnaturalearthdata",
 "dplyr",
 "raster",
 # "chorddiag", #devtools::install_github("mattflor/chorddiag")
 "png",
 "grid",
 "cowplot"
 )
sapply(package_vec, install.load.package)

## Functionality ------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA =========================================================================
## Loading Data ------
# reading RI overview table
RI_df <- read.csv(file.path(Dir.Contacts, "ECA countries (v2023-04) - ECA Countries.csv"))

## Manipulating Data ------
# fixing country names
RI_df$Country[RI_df$Country == "Czech Republic"] <- "Czechia"
RI_df[nrow(RI_df)+1,] <- NA
RI_df$Country[nrow(RI_df)] <- "Kosovo"

# subsetting for relevant columns
RI_df <- RI_df[, c("Country", "GBIF.Member", "ELIXIR", "eLTER", "LifeWatch", "CETAF", "DiSSCo")]
# Reformatting character columns
RI_df$GBIF.Member[grep(x = RI_df$GBIF.Member, pattern = "?", fixed = TRUE)] <- ""
RI_df$GBIF.Member <- RI_df$GBIF.Member != ""
RI_df$ELIXIR <- RI_df$ELIXIR != ""
RI_df$eLTER <- RI_df$eLTER != ""
RI_df$LifeWatch <- RI_df$LifeWatch != ""
RI_df$DiSSCo <- RI_df$DiSSCo != ""
RI_df$CETAF <- as.numeric(gsub("\\D", "", RI_df$CETAF))
RI_df$CETAFBin <- !is.na(RI_df$CETAF)
RI_df[RI_df$Country == "Kosovo", colnames(RI_df)[-1]] <- FALSE

# Remove CETAFBin
RI_df <- RI_df[,-which(colnames(RI_df) %in% c("CETAF", "CETAFBin", "ELIXIR"))]

# Renaming Columns
colnames(RI_df) <- c("Country", "GBIF", "eLTER", "LifeWatch", "DiSSCo")

# summing up RI coverage
RI_df$RIs <- apply(X = RI_df[,c("GBIF", "eLTER", "DiSSCo", "LifeWatch")],
                   MARGIN = 1, FUN = sum)

## Making Shapefiles ------
map <- ne_countries(scale = "large", type = "countries")
map <- map[match(c(RI_df$Country), map$name), "name"]
map <- cbind(map, RI_df[,-1])
map <- crop(as(map, "Spatial"), extent(-25, 110, 20, 82))
map <- st_as_sf(map)

## Making Network Object ------
RI_net <- igraph::make_empty_graph()
attr_ls <- lapply(colnames(RI_df), function(i){
  RI_df[,i]
})
names(attr_ls) <- colnames(RI_df)
RI_net <- igraph::add.vertices(RI_net, nv = nrow(RI_df), attr = attr_ls)
 # take inspiration from: https://www.dissco.eu/dissco/ri-landscape/

## Combinations of RIs
Combins_df <- count(RI_df,GBIF,eLTER,LifeWatch, DiSSCo)

# PLOTTING =====================================================================
RI_colours <- c("#509D49", "#EC9A40", "#8EC3E1", "#28A2A5")
names(RI_colours) <- c("GBIF", "eLTER", "DiSSCo", "LifeWatch")

## Maps -----
# loading relevant RI logo
img <- readPNG(file.path(Dir.Logos, "biodt-logo.png"))
g <- rasterGrob(img, interpolate=TRUE)

# plotting object
p <- ggplot(map) +
  geom_sf(data = map, aes(fill = as.factor(RIs)), colour = "black") +
  annotation_custom(g, xmin=40, xmax=100, ymin=40, ymax=80) +
  theme_void() +
  coord_sf() + #crs = st_crs("EPSG:3035-1149")
  scale_fill_viridis_d(option = "G")+ 
  # theme(legend.position = "bottom") + guides(fill = guide_legend(nrow = 1)) +
  # theme(legend.direction = "horizontal") + 
  labs(fill = "RIs")
p

# saving plot to disk
# ggsave(plot = p, filename = file.path(Dir.Exports, paste0("RIs", ".png")), 
#        width = 12, height = 9, units = "cm")

indivplots <- lapply(sort(names(RI_colours)), FUN = function(i){
	# extracting relevant map data
	plot_map <- map[,i]
	colnames(plot_map)[1] <- "PLOT"
	
	# loading relevant RI logo
	img <- readPNG(file.path(Dir.Logos, paste0(tolower(i), "-logo.png")))
	g <- rasterGrob(img, interpolate=TRUE)
	
	# plotting object
	p <- ggplot(plot_map) +
		geom_sf(data = plot_map, aes(fill = PLOT), colour = "black") +
		annotation_custom(g, xmin=40, xmax=100, ymin=40, ymax=80) +
		theme_void() +
		coord_sf() + #crs = st_crs("EPSG:3035-1149")
		scale_fill_manual(values=c("grey", as.character(RI_colours[names(RI_colours) == i]))) + 
		theme(legend.position = "none")
	
	# saving plot to disk
	ggsave(plot = p, filename = file.path(Dir.Exports, paste0(i, ".png")), 
				 width = 12, height = 8, units = "cm")
	p
})

ggsave(plot = plot_grid(p, plot_grid(plotlist = indivplots, ncol = 2), ncol = 1, 
												rel_heights = c(1.5, 2), labels = "AUTO"),
			 filename = file.path(Dir.Exports, paste0("RIs", ".png")), 
			 width = 16, height = 20, units = "cm")




# # CASE EXAMPLES ================================================================
# 
# ## Top-Down -----
# edg_ls <- data.frame(from =
#                        c(
#                          # "Research Projects",
#                          # "Research Projects",
#                          # "Research Projects",
#                          # "Research Projects",
#                          # "Data Centres",
#                          "Swedigarch",
#                          "NBS",
#                          "SITES",
#                          "EMBRC",
#                          # "Authorities",
#                          # "Authorities",
#                          "SLU",
#                          "CMS",
#                          "CMS"
#                          ),
#                       to = 
#                        c(
#                          # "Swedigarch",
#                          # "NBS",
#                          # "SITES",
#                          # "EMBRC",
#                          # "SBDI",
#                          "SBDI",
#                          "SBDI",
#                          "SBDI",
#                          "SBDI",
#                          # "CMS",
#                          # "SLU",
#                          "CMS",
#                          "DiSSCo",
#                          "SBDI"
#                        )
#                        )
# 
# SE_g <- igraph::graph_from_edgelist(as.matrix(edg_ls))
# SE_Members <- cluster_walktrap(SE_g)
# SE_Mod <- round(modularity(SE_Members), 2)
# SE_degree <- degree(SE_g)
# 
# set.seed(42)
# ggraph(graph = SE_g, layout = 'graphopt') +
#   # geom_edge_link(edge_colour = "grey") + 
#   geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
#                  arrow = arrow(type = "closed", length = unit(3, 'mm'))) + 
#   geom_node_point(aes(size = SE_degree, col = as.factor(SE_Members$membership))) + 
#   # geom_node_text(aes(label = name), repel=TRUE) +
#   theme_void() + theme(legend.position = "none") +
#   geom_label(aes(x = -25, y = -25,
#                  label = paste("Modularity = ", SE_Mod)),
#              stat = "unique")
# 
# # +
# #   theme_bw()
# 
# ggsave(filename = file.path(getwd(), "RI_Logos", "RI_SWEDEN.png"), 
#        width = 12, height = 8, units = "cm")
# 
# ## Bottom-Up ----
# edg_ls <- data.frame(from =
#                        c(
#                          # "GBIF",
#                          "DiSSCo",
#                          # "DiSSCo",
#                          "DiSSCo",
#                          "arise",
#                          "DiSSCo",
#                          "BGE",
#                          "arise",
#                          "BGE",
#                          "arise",
#                          "BGE",
#                          "BGE",
#                          "ELIXIR",
#                          "Observation",
#                          "MAMBO",
#                          "Observation",
#                          "arise",
#                          "MAMBO",
#                          "arise",
#                          "Observation",
#                          "GBIF",
#                          "elTER",
#                          "GBIF"
#                        ),
#                      to = 
#                        c(
#                          # "BioDT",
#                          "GBIF",
#                          # "BioDT",
#                          "arise",
#                          "DiSSCo",
#                          "BGE",
#                          "DiSSCo",
#                          "BGE",
#                          "arise",
#                          "GBIF",
#                          "GBIF",
#                          "ELIXIR",
#                          "BGE",
#                          "GBIF",
#                          "Observation",
#                          "MAMBO",
#                          "MAMBO",
#                          "arise",
#                          "Observation",
#                          "arise",
#                          "LifeWatch",
#                          "GBIF",
#                          "elTER"
#                        )
# )
# 
# NL_g <- igraph::graph_from_edgelist(as.matrix(edg_ls))
# NL_Members <- cluster_walktrap(NL_g)
# NL_Mod <- round(modularity(NL_Members), 2)
# NL_degree <- degree(NL_g)
# 
# set.seed(42)
# ggraph(graph = NL_g, layout = 'graphopt') +
#   # geom_edge_link(edge_colour = "grey") + 
#   geom_edge_link(aes(start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)), 
#                  arrow = arrow(type = "closed", length = unit(3, 'mm'))) + 
#   geom_node_point(aes(size = NL_degree, col = as.factor(NL_Members$membership))) + 
#   # geom_node_text(aes(label = name), repel=TRUE) +
#   theme_void() + theme(legend.position = "none") +
#   geom_label(aes(x = 10, y = 30,
#                  label = paste("Modularity = ", NL_Mod)),
#              stat = "unique") 
# # +
# #   theme_bw()
# 
# ggsave(filename = file.path(getwd(), "RI_Logos", "RI_NETHERLANDS.png"), 
#        width = 12, height = 8, units = "cm")