#' ####################################################################### #
#' PROJECT: [BioDT RI - Publication Analysis] 
#' CONTENTS: 
#'  - 
#'  DEPENDENCIES:
#'  - data files in same directory
#' AUTHOR: [Erik Kusch]
#' ####################################################################### #

# PREAMBLE =====================================================================
rm(list=ls())
set.seed(42) # making things reproducibly random

## Directories ------
### Define directories in relation to project directory
Dir.Base <- getwd()
Dir.Contacts <- file.path(Dir.Base, "RI Contacts")
Dir.Logos <- file.path(Dir.Base, "RI Logos")
Dir.Publications <- file.path(Dir.Base, "RI Publications")
Dir.Exports <- file.path(Dir.Base, "Exports")
### Create directories which aren't present yet
Dirs <- c(Dir.Exports)
CreateDir <- sapply(Dirs, function(x) if(!dir.exists(x)) dir.create(x))

## Packages ---------------------------------------------------------------
install.load.package <- function(x) {
	if (!require(x, character.only = TRUE))
		install.packages(x, repos='http://cran.us.r-project.org')
	require(x, character.only = TRUE)
}
### CRAN PACKAGES ----
package_vec <- c(
	"readxl", # reading .xlsx
	"tidyverse",
	"ggplot2",
	"networkD3",
	"png",
	"cowplot",
	"grid"
)
sapply(package_vec, install.load.package)

### NON-CRAN ----
if("ggsankey" %in% rownames(installed.packages()) == FALSE){
	remotes::install_github("davidsjoberg/ggsankey")
}
library(ggsankey)

## Functionality ----------------------------------------------------------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA ====================================================================
eLTER_df <- readRDS(file.path(Dir.Publications, "DATA_eurobioricc_elter-publications-authors-references-and-citations_20240305_V0.1.0.rds"))
eLTER_df <- eLTER_df[!duplicated(eLTER_df$doi), ]
eLTER_df$peer_review <- ifelse(eLTER_df$is_peer_reviewed == "yes", TRUE, FALSE)
eLTER_df$open_access <- ifelse(eLTER_df$oa_status == "gold", TRUE, FALSE)
eLTER_df$open_access[is.na(eLTER_df$open_access)] <- FALSE
elterKey <- readRDS(file.path(Dir.Publications, "DATA_eurobioricc_elter-publications-keywords_20240823_V0.1.0.rds"))
eLTER_df <- eLTER_df[eLTER_df$documentType == "journal-article", ]

if(!file.exists(file.path(Dir.Publications, "GBIF-publications_2024-06-25_gbif-literature-with-relevance.rds"))){
	GBIF_df <- read_csv("https://raw.githubusercontent.com/ymgan/gojo-bibliographic-information-file/main/files/2024-06-25_gbif-literature-with-relevance.csv")
	saveRDS(GBIF_df, file = file.path(Dir.Publications, "GBIF-publications_2024-06-25_gbif-literature-with-relevance.rds"))	
}
GBIF_df <- readRDS(file.path(Dir.Publications, "GBIF-publications_2024-06-25_gbif-literature-with-relevance.rds"))
GBIF_df <- GBIF_df[!duplicated(GBIF_df[, c("title")]), ]
GBIF_df <- GBIF_df[GBIF_df$literature_type == "JOURNAL",]

pubs_ls <- list(GBIF = GBIF_df,
								eLTER = eLTER_df)

# PLOTTING ================================================================
## Doughnut Charts --------------------------------------------------------
Pubs_iter <- names(pubs_ls)[1]
WheelPlots_ls <- lapply(names(pubs_ls), FUN = function(Pubs_iter){
	data_iter <- pubs_ls[[Pubs_iter]]
	
	img <- readPNG(file.path(Dir.Logos, paste0(tolower(Pubs_iter), "-logo.png")))
	g <- rasterGrob(img, interpolate=TRUE)
	
	### Peer-Review -----
	plot_df <- data.frame(
		round(
			table(data_iter$peer_review)/nrow(data_iter)*100,
			2)
	)
	plot_df$ymax <- cumsum(plot_df$Freq)/100
	plot_df$ymin <- c(0, head(plot_df$ymax, n=-1))
	plot_df$labelPosition <- (plot_df$ymax + plot_df$ymin) / 2
	plot_df$label <- paste0(plot_df$Var1, "\n", plot_df$Freq , "%")
	
	print(plot_df)
	Dough <- ggplot(data = plot_df, aes(ymax = ymax, ymin = ymin, xmax = 7, xmin = 4, fill=Var1)) +
		geom_rect() +
		geom_text(x = 8.5, aes(y = labelPosition, label=label, color=Var1), size = 4) + # x here controls label position (inner / outer)
		coord_polar(theta = "y") +
		scale_color_manual(
			values = na.omit(rev(c("forestgreen", ifelse(all(as.logical(plot_df$Var1)), NA, "darkred"))))) +
		scale_fill_manual(
			values = na.omit(rev(c("forestgreen", ifelse(all(as.logical(plot_df$Var1)), NA, "darkred"))))
		) +
		# annotation_custom(g, xmin=-1, xmax=1, ymin=-1, ymax=1) +
		xlim(c(-1, 8)) +
		# theme_bw() + 
		theme_void() +
		theme(legend.position = "none")
	
	peer_review_gg <- ggplot(data.frame(x = -10:10, y = -10:10), aes(x = x, y = y), geom="blank") +
		annotation_custom(ggplotGrob(Dough), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
		annotation_custom(g, xmin=-4, xmax=4, ymin=-1, ymax=1) +
		geom_label(x = 0, y = 3, label = "Peer Review", size = 6) + 
		geom_label(x = 0, y = -3, label = paste("n =", nrow(data_iter)), size = 6) + 
		geom_point(fill = NA, col = NA) + 
		# labs(title = "Peer-Review Status") + 
		# theme_bw()
		theme_void()
	
	### Open Access -----
	plot_df <- data.frame(
		round(
			table(data_iter$open_access)/nrow(data_iter)*100,
			2)
	)
	plot_df$ymax <- cumsum(plot_df$Freq)/100
	plot_df$ymin <- c(0, head(plot_df$ymax, n=-1))
	plot_df$labelPosition <- (plot_df$ymax + plot_df$ymin) / 2
	plot_df$label <- paste0(plot_df$Var1, "\n", plot_df$Freq , "%")
	
	print(plot_df)
	Dough <- ggplot(data = plot_df, aes(ymax = ymax, ymin = ymin, xmax = 7, xmin = 4, fill=Var1)) +
		geom_rect() +
		geom_text(x = 8.5, aes(y = labelPosition, label=label, color=Var1), size = 4) + # x here controls label position (inner / outer)
		coord_polar(theta = "y") +
		scale_color_manual(
			values = na.omit(rev(c("forestgreen", ifelse(all(as.logical(plot_df$Var1)), NA, "darkred"))))
		) +
		scale_fill_manual(
			values = na.omit(rev(c("forestgreen", ifelse(all(as.logical(plot_df$Var1)), NA, "darkred"))))
		) +
		# annotation_custom(g, xmin=-1, xmax=1, ymin=-1, ymax=1) +
		xlim(c(-1, 8)) +
		# theme_bw() +
		theme_void() +
		theme(legend.position = "none")
	
	open_access_gg <- ggplot(data.frame(x = -10:10, y = -10:10), aes(x = x, y = y), geom="blank") +
		annotation_custom(ggplotGrob(Dough), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
		annotation_custom(g, xmin=-4, xmax=4, ymin=-1, ymax=1) +
		geom_label(x = 0, y = 3, label = "Open Access", size = 6) + 
		geom_label(x = 0, y = -3, label = paste("n =", nrow(data_iter)), size = 6) + 
		geom_point(fill = NA, col = NA) + 
		# labs(title = "Peer-Review Status") + 
		# theme_bw()
		theme_void()
	
	### Plot-Fusing -----
	cowplot::plot_grid(
		peer_review_gg, #+ theme(plot.margin = margin(-2, -2, -2, -2, "cm"))
		open_access_gg, 
		ncol = 1)
	})
ggsave(plot = plot_grid(plotlist = WheelPlots_ls, ncol = 2), 
			 filename = file.path(Dir.Exports, paste0("Publications_Wheel", ".png")), 
			 width = 12*2.5, height = 12*2.5, units = "cm")


## Sankey Daigrams -------------------------------------------------------------
# Expand the topics column
GBIF_df_expanded <- GBIF_df %>%
	separate_rows(topics, sep = "\\|")
GBIF_df_expanded <- GBIF_df_expanded[!is.na(GBIF_df_expanded$topics), ]

# Create nodes
nodes <- data.frame(name = c("Peer-reviewed", "Not Peer-reviewed", "Open Access", "Not Open Access",
														 unique(GBIF_df_expanded$topics)),
										stringsAsFactors = FALSE)

## adding counts
add_count <- table(GBIF_df_expanded$peer_review)/length(GBIF_df_expanded$peer_review)
names(add_count) <- c("Not Peer-reviewed", "Peer-reviewed")
add <- table(GBIF_df_expanded$open_access[GBIF_df_expanded$peer_review])/length(GBIF_df_expanded$open_access[GBIF_df_expanded$peer_review])
names(add) <- c("Not Open Access", "Open Access")
add_count <- c(add_count, add)
add <- table(GBIF_df_expanded$topics[GBIF_df_expanded$open_access & GBIF_df_expanded$peer_review])/length(GBIF_df_expanded$topics[GBIF_df_expanded$open_access & GBIF_df_expanded$peer_review])
add_count <- c(add_count, add)

nodes <- left_join(nodes, data.frame(name = names(add_count), count = add_count))
# nodes <- na.rm(nodes)

# Create links
# Create links for peer-reviewed status to open access status
peer_review_links <- GBIF_df_expanded %>%
	mutate(source = ifelse(peer_review, 0, 1)) %>%
	mutate(target = ifelse(open_access, 2, 3)) %>%
	group_by(source, target) %>%
	summarise(value = n()) %>%
	ungroup()

# Filter for only peer-reviewed and open access entries for creating links to topics
topic_links <- GBIF_df_expanded %>%
	filter(peer_review == TRUE, open_access == TRUE) %>%
	mutate(source = 2) %>%
	mutate(target = match(topics, nodes$name) - 1) %>%
	group_by(source, target) %>%
	summarise(value = n()) %>%
	ungroup()

# Combine the links
links <- bind_rows(peer_review_links, topic_links)

# Create Sankey diagram
sankeyNetwork(Links = links, Nodes = nodes,
							Source = "source", Target = "target",
							Value = "value", NodeID = "name",
							fontSize = 15,
							sinksRight = FALSE)
message("Export sankey by hand")
