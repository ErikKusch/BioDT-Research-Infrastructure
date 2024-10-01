#' ####################################################################### #
#' PROJECT: [BioDT RI - Collaboration Network] 
#' CONTENTS: 
#'  - Network analysis and visualisation of self-reported collaborations of European Biodiversity Research Infrastructure Nodes (DiSSCo, eLTER, GBIF, LifeWatch Eric)
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
Dir.Survey <- file.path(Dir.Base, "RI Survey")
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
	"readr", # reading .xlsx
	"readxl",
	"dplyr",
	"zoo",
	"data.table", # reshaping data wide to long
	'cowplot', # grid plotting
	'ggplot2', # ggplot machinery
	'ggpmisc', # table plotting in ggplot environment
	'ggpubr', # t-test comparison in ggplot
	"stringr",
	## network specific packages
	"igraph", 
	"network",
	"ggraph",
	"networkD3",
	"htmlwidgets",
	"visNetwork"
)
sapply(package_vec, install.load.package)

## Functionality ----------------------------------------------------------
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# DATA ====================================================================
## Colours ----------------------------------------------------------------
RI_colours <- c("#509D49", "#EC9A40", "#8EC3E1", "#28A2A5", "#544F4C", "#1C467A")
names(RI_colours) <- c("GBIF", "eLTER", "DiSSCo", "LifeWatch", "ELIXIR", "CETAFBin")

## RI Contacts ------------------------------------------------------------
RIContacts_df <- read_excel(file.path(Dir.Contacts, "RIContacts.xlsx"))
CountryCodes <- read_delim(file.path(Dir.Contacts, "CountryCodes.csv"), delim = ";", escape_double = FALSE, trim_ws = TRUE)
RIContacts_df$COUNTRY <- CountryCodes$Country[match(RIContacts_df$COUNTRY, CountryCodes$`Alpha-2 Code`)]

## Main Survey Responses --------------------------------------------------
### Loading ----------
if(file.exists(file.path(Dir.Survey, "SurveyResponses_2024-09-12.csv"))){
  All_df <- read.csv(file.path(Dir.Survey, "SurveyResponses_2024-09-12.csv"))
  
  ## Manipulation ----------
  ### fix column names
  colnames(All_df)[1:4] <- c("Timestamp", "Consent", "RI", "Country")
  
  colnames(All_df) <- gsub("Please.rate.how.closely.you.work.with.the.following.", 
                           "", colnames(All_df))
  colnames(All_df) <- gsub(".nodes..Please.make.only.one.selection.per.row..", 
                           "", colnames(All_df))
  HeadOfficeLocs <- grep("Head.Office", colnames(All_df))
  colnames(All_df) <- gsub("Head.Office..", "", colnames(All_df))
  colnames(All_df) <- gsub(".Eric", "", colnames(All_df))
  colnames(All_df) <- gsub("\\.$", "", colnames(All_df))
  colnames(All_df) <- gsub("\\.", "_", colnames(All_df))
  HeadOffices <- colnames(All_df)[HeadOfficeLocs]
  
  ### Remove superfluous rows
  All_df <- All_df[, -which(colnames(All_df) == "DiSSCo_Switzerland")] # remove DiSSCo Switzerland because not actually a member yet
  
  ### remove non-consenting respondentens
  All_df <- All_df[startsWith(All_df$Consent, "Yes"), ]
  
  ### Check if there are issues with the self-reporting
  if(length(table(All_df$RI)) > 4){stop("More than 4 RIs in self-reported field")}
  
  ### fix weird country entries in the self-reporting
  # unique(All_df$Country)
  All_df$Country[All_df$Country == "distributed RI, site in Finland"] <- "Finland"
  
  ### make column entries numeric
  All_df <- data.frame(lapply(All_df, function(x) {
    gsub("No Interaction", 0, x)
  }))
  
  MultiEntryCheck <- apply(All_df, 1, function(x){
    sum(is.na(as.numeric(x[-1:-4]))) != 0
  })
  
  if(any(MultiEntryCheck)){
    Problems <- apply(All_df[MultiEntryCheck, c("RI", "Country")], 1, 
                      function(x){paste(x, collapse = "_")})
    message("Erroneous double entries per survey row present for ", 
            paste(Problems, collapse = ", "), ". Remvoing these from data.")
    All_df <- All_df[!MultiEntryCheck,]
  }
  All_df <- All_df[!duplicated(All_df[, c("RI", "Country")]), ]
  
  
  ### set self-collabs to NA
  for(y in 1:nrow(All_df)){
    x <- All_df[y,]
    pattern <- paste(x["RI"], x["Country"], sep = "_")
    x[grep(pattern, colnames(x))] <- NA
    All_df[y,] <- x
  }
  
  colnames(All_df)[which(endsWith(colnames(All_df), "_"))] <- substr(
    colnames(All_df)[which(endsWith(colnames(All_df), "_"))],
    1, nchar(colnames(All_df)[which(endsWith(colnames(All_df), "_"))])-1)
  source("DataAnonymisation.R")
}else{
  All_df <- readRDS(file.path(Dir.Survey, "SurveyResponses_2024-09-12_ANONYMISED.rds"))
}

### Reformatting data
Orig_df <- All_df
All_df <- melt(All_df[,-1:-(which(colnames(All_df) == "RI")-1)], id.vars = c("RI", "Country"), 
		 variable.name = "Partner", value.name = "Collaboration")
All_df$Partner_RI <- unlist(lapply(strsplit(as.character(All_df$Partner), split = "_"), "[[", 1))
All_df$Partner_Country <- str_replace(
	str_replace_all(All_df$Partner, 
									c(eLTER_ = "",
										DiSSCo_ = "",
										GBIF_ = "",
										LifeWatch_ = "")),
	"_", " "
)
All_df <- All_df[,-3]
All_df$Collaboration <- as.numeric(All_df$Collaboration)

# ANALYSES ================================================================
## Reporting Response Rates and Identify Missing Respondents --------------
if("Timestamp" %in% colnames(Orig_df)){
  Respondents <- unique(apply(All_df[, c("RI", "Country")], 1, function(x){paste(x,collapse = "_")}))
  Queried <- apply(RIContacts_df[, c("ASSOCIATION", "COUNTRY")], 1, function(x){paste(x,collapse = "_")})
  RIContacts_df$Responded <- FALSE
  if(any(is.na(match(Respondents, Queried)))){stop("One or some of the responses report different RIs or Countries as the list of RI contacts. Double check.")}
  RIContacts_df$Responded[match(Respondents, Queried)] <- TRUE
  
  AbsoluteResponses <- aggregate(Responded ~ ASSOCIATION, RIContacts_df, FUN = sum)
  RelativeResponses <- AbsoluteResponses
  RelativeResponses$Responded <- round(
    RelativeResponses$Responded/table(RIContacts_df$ASSOCIATION) * 100, 
    2)
  
  ## full response rates
  ggplot(RelativeResponses, aes(x = ASSOCIATION, y = Responded, fill = ASSOCIATION)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = RI_colours[1:4], name = "Research Infrastructure") + 
    labs(x = "Research Infrastructure", y = "Response Rate [%]") +
    lims(y = c(0, 100)) + 
    theme_bw() + theme(legend.position = "none")
  ggsave(filename = file.path(Dir.Exports, "Survey_ResponseRate.png"), width = 22, height = 12, units = "cm")
  
  ## responses over time
  Orig_ls <- split(Orig_df, Orig_df$RI)
  Cumsum_ls <- lapply(Orig_ls, FUN = function(df){
    df2 <- df %>%
      mutate(Date = as.Date(Timestamp, format = "%d/%m/%Y")) %>%
      group_by(Date, RI) %>%
      summarize(Entries = n()) 
    df2$CumulativeEntries <- cumsum(df2$Entries)
    df2 <- rbind(
      df2,
      data.frame(Date = c(as.Date("2024-04-10"), Sys.Date()),
                 RI = rep(unique(df$RI), 2),
                 Entries = c(0, NA),
                 CumulativeEntries = c(0, df2$CumulativeEntries[nrow(df2)])
      )
    )
    Fullseq <- seq(min(df2$Date), max(df2$Date), by = "day")
    df3 <- merge(data.frame(Date = Fullseq), df2, by = "Date", all.x = TRUE)									 
    df3 <- na.locf(df3)
    df3$CumulativeEntries <- df3$CumulativeEntries/table(RIContacts_df$ASSOCIATION)[unique(df$RI)] * 100
    df3
  })
  Cumsum_df <- do.call(rbind, Cumsum_ls)
  
  ggplot(Cumsum_df, aes(y = CumulativeEntries, x = Date)) + 
    geom_line(aes(col = RI), size = 1.2) + 
    scale_color_manual(values = RI_colours[1:4], name = "Research Infrastructure") + 
    geom_vline(xintercept = as.Date("2024-04-11"), linetype = "dashed") + 
    geom_label(x = as.Date("2024-04-11"), y = 100, label = "Initial E-Mail", hjust = 0, size = 3) + 
    lims(y = c(0, 100), x = c(min(Cumsum_df$Date), max(Cumsum_df$Date)+10)) + 
    geom_vline(xintercept = as.Date("2024-05-28"), linetype = "dashed") + 
    geom_label(x = as.Date("2024-05-28"), y = 100, label = "GBIF ECA Meeting", hjust = 0, size = 3) + 
    geom_vline(xintercept = as.Date("2024-06-25"), linetype = "dashed") + 
    geom_label(x = as.Date("2024-06-25"), y = 100, label = "Second E-Mail", hjust = 0, size = 3) + 
    geom_vline(xintercept = as.Date("2024-08-21"), linetype = "dashed") + 
    geom_label(x = as.Date("2024-08-21"), y = 100, label = "E-Mail to Boards", hjust = 0, size = 3) + 
    theme_bw() + theme(legend.position = "top") + labs(y = "Response Rate [%]")
  ggsave(filename = file.path(Dir.Exports, "Survey_ResponseRateTime.png"), width = 22, height = 12, units = "cm") 
}else{
  message("You are working with anonymised survey response data. As a result, response rates and time-to-response are not visualised.")
}

## Within vs. Without RI Collaborations -----------------------------------
All_df$Type <- ifelse(All_df$RI == All_df$Partner_RI, "Within", "Between")
dodge <- position_dodge(width = 1)

p1 <- ggplot(All_df, aes(x = RI, y = Collaboration)) + 
	geom_violin(aes(fill = Type), position = dodge) +
	geom_boxplot(aes(group=interaction(Type,RI)), 
							 width=0.1, fill="white", position=dodge) + 
	scale_fill_viridis_d(option = "G", begin = 0.2, end = 0.8, name = "Collaboration Network") + 
	# stat_compare_means(aes(group = Type), label = "p.format", 
	# 									 position = position_nudge(y = 0.3)) +
	scale_y_continuous(breaks = pretty(All_df$Collaboration, n = 5)) + 
	theme_bw() + theme(legend.position = "top") + labs(x = "Research Infrastructure", y = "Collaboration Intensity")

p2 <- ggplot(All_df, aes(x = RI, y = Collaboration)) + 
	geom_violin(aes(fill = RI), position = dodge) +
	geom_boxplot(aes(group=RI), 
							 width=0.1, fill="white", position=dodge) + 
	scale_fill_manual(values = RI_colours[1:4], name = "Research Infrastructure") +
	# stat_compare_means(comparisons = as.list(data.frame(combn(sort(unique(All_df$RI)), 2))), label = "p.format", 
	# 									 position = position_nudge(y = 0.5)) +
	facet_wrap(~Type) + 
	theme_bw() + theme(legend.position = "none") + labs(x = "Research Infrastructure", y = "Collaboration Intensity")

ggsave(plot_grid(p1, p2, ncol = 1, rel_heights = c(1, 1)), filename = file.path(Dir.Exports, "Survey_WithinBetween.png"), width = 22, height = 22, units = "cm")

# ## Network Visualisation --------------------------------------------------
# edg_ls <- data.frame(from = paste(All_df$RI, All_df$Country, sep = "_"),
# 										 to = paste(All_df$Partner_RI, All_df$Partner_Country, sep = "_"))
# graph_ig <- igraph::graph_from_edgelist(as.matrix(edg_ls))
# V(graph_ig)$Country <- unlist(lapply(strsplit(names(V(graph_ig)), split = "_"), "[[", 2))
# V(graph_ig)$RI <- unlist(lapply(strsplit(names(V(graph_ig)), split = "_"), "[[", 1))
# E(graph_ig)$Collaboration <- as.numeric(All_df$Collaboration)
# E(graph_ig)$Collaboration <- ifelse(E(graph_ig)$Collaboration == 0, NA, E(graph_ig)$Collaboration)
# 
# ## remove NA edges
# graph_ig <- igraph::delete.edges(
# 	graph_ig,
# 	E(graph_ig)[is.na(E(graph_ig)$Collaboration)]
# )
# 
# ## remove nodes without edges
# graph_ig <- igraph::delete.vertices(graph_ig, which(degree(graph_ig)==0))
# 
# ## summary metrics
# graph_Members <- cluster_walktrap(graph_ig)
# graph_Mod <- round(modularity(graph_Members), 2)
# graph_degree <- degree(graph_ig)
# 
# ## plotting
# ggraph(graph = graph_ig, layout = "kk") +
# 	geom_edge_link(aes(width = Collaboration, 
# 										 start_cap = label_rect(node1.name), end_cap = label_rect(node2.name)),
# 								 arrow = arrow(type = "closed", length = unit(2, 'mm'))
# 	) + 
# 	scale_edge_width(guide = "none", range = c(0.5, 2.5)) +
# 	geom_node_point(aes(size = graph_degree, col = as.factor(RI))) +
# 	scale_color_manual(values = RI_colours, name = "Research Infrastructure") +
# 	scale_size(guide = "none") +
# 	geom_node_label(aes(label = Country), repel = TRUE) +
# 	theme_void() + theme(legend.position = "bottom")
# # ggsave(filename = file.path(Dir.Exports, "Survey_Network.png"), width = 19*2.5, height = 9*2.5, units = "cm")
# 
# ## interactive graph
# nodes_df <- data.frame(id = as.numeric(factor(V(graph_ig)$name)),
# 											 label = V(graph_ig)$name,
# 											 title = V(graph_ig)$name,
# 											 group = unlist(lapply(strsplit(V(graph_ig)$name, "_"), "[[", 1))
# )
# nodes_df$color <- as.character(RI_colours[nodes_df$group])
# 
# edges_df <- as.data.frame(as_edgelist(graph_ig))
# colnames(edges_df) <- c("from", "to")
# edges_df$value <- E(graph_ig)$Collaboration
# edges_df <- edges_df[edges_df$from != edges_df$to, ]
# 
# edges_df$from <- nodes_df$id[match(edges_df$from, nodes_df$label)]
# edges_df$to <- nodes_df$id[match(edges_df$to, nodes_df$label)]
# nodes_df$label <- unlist(lapply(strsplit(V(graph_ig)$name, "_"), "[[", 2))
# 
# visgraph <- visNetwork(nodes_df, edges_df, width = "3840px", height = "2160px") %>%
# 	visIgraphLayout(layout = "layout.fruchterman.reingold") %>%
# 	visNodes(
# 		shape = "dot",
# 		shadow = list(enabled = TRUE, size = 10)
# 	) %>%
# 	visEdges(
# 		shadow = FALSE,
# 		color = list(color = "grey", highlight = "#C62F4B"),
# 		arrows = "middle"
# 	) %>%
# 	visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
# 						 selectedBy = "group") %>% 
# 	visLayout(randomSeed = 42)
# visSave(visgraph, file = file.path(Dir.Exports, "Survey_Network.html"), selfcontained = TRUE)
# 
# # FAILED TO RESPON =============================================================
# MissingResp_ls <- split.data.frame(RIContacts_df[!RIContacts_df$Responded, ], RIContacts_df[!RIContacts_df$Responded, "ASSOCIATION"])
# lapply(MissingResp_ls, FUN = function(x){
# 	paste(x$COUNTRY, collapse = ", ")
# 	})
