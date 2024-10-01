rm(list=ls())

`%nin%` <- Negate(`%in%`)

## GBIF ------------------------------------------------------------------------
library("rjson")
json_file <- "https://api.gbif.org/v1/node?limit=500"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

GBIF_ls <- lapply(json_data$results, FUN = function(x){ # 1:length(json_data$results)
	# print(x)
	# x <- json_data$results[[x]]
	HeadPos <- which(unlist(lapply(x$contacts, "[[", "type")) == "HEAD_OF_DELEGATION")
	data.frame(
		ASSOCIATION = "GBIF",
		COUNTRY = ifelse(is.null(x$country), NA, x$country),
		CONTACT = ifelse(length(HeadPos) == 0, NA, paste(x$contacts[[HeadPos]]$firstName, x$contacts[[HeadPos]]$lastName)),
		EMAIL = ifelse(length(HeadPos) == 0, NA, x$contacts[[HeadPos]]$email),
		TITLE = ifelse(is.null(x$participantTitle), NA, x$participantTitle),
		STATUS = x$participationStatus
	)
})
GBIF_df <- do.call(rbind, GBIF_ls)
GBIF_df <- GBIF_df[GBIF_df$STATUS %in% c("VOTING", "ASSOCIATE"), ]

CC_df <- read.csv("CountryCodes.csv", sep = ";")
colnames(CC_df)[1] = "Country"
GBIF_df <- GBIF_df[GBIF_df$COUNTRY %in% CC_df$Alpha.2.Code, ]

for(Dupl_iter in unique(GBIF_df$COUNTRY[duplicated(GBIF_df$COUNTRY)])){
	# print(Dupl_iter)
	rows <- rownames(GBIF_df[GBIF_df$COUNTRY == Dupl_iter, ])
	NAs <- apply(GBIF_df[GBIF_df$COUNTRY == Dupl_iter, ], MARGIN = 1, FUN = function(x){sum(is.na(x))})
	if(length(unique(NAs))!= 1){
		GBIF_df <- GBIF_df[-which(rownames(GBIF_df) %in% rows[rows %in% names(NAs[max(NAs) == NAs])]), ]
	}
	# print(dim(GBIF_df))
}
GBIF_df <- GBIF_df[order(GBIF_df$COUNTRY), ]
write.csv(GBIF_df, file = "RIContacs_GBIF.csv")

# write.csv(data.frame(CC_df$Country[match(GBIF_df$COUNTRY[GBIF_df$STATUS == "VOTING"], CC_df$Alpha.2.Code)]), file = "COPYPASTEDocs.csv")

## DiSSCo ----------------------------------------------------------------------
library("rvest")
library("xml2")

link <- "https://www.dissco.eu/contact/"
DiSSCo_page <- read_html(link)

COUNTRY <- DiSSCo_page %>%
	html_elements(css = ".dsm_card_title") %>%  # Country Names
	html_text()

CONTACT <- DiSSCo_page %>%
	html_elements(css = ".dsm_card_subtitle") %>%  # Contact Name
	html_text()

EMAIL <- DiSSCo_page %>%
	html_elements(css = ".dsm_card_description") %>%  # Email
	html_text() %>% 
	gsub(pattern = " ", replacement = "")

DiSSCo_df <- na.omit(data.frame(
	ASSOCIATION = "DiSSCo",
	COUNTRY = CC_df$Alpha.2.Code[match(COUNTRY, toupper(CC_df$Country))],
	CONTACT = CONTACT,
	EMAIL = EMAIL))
DiSSCo_df

write.csv(DiSSCo_df, file = "RIContacs_DiSSCo.csv")

## eLTER -----------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("ropensci/ReLTER")
library(ReLTER)
link <- "https://www.deims.org/networks/4742ffca-65ac-4aae-815f-83738500a1fc"
eLTER_page <- read_html(link)

eLTER_elements <- eLTER_page %>% 
	html_elements(xpath = '/html/body/div[1]/div/div/section/div/div/div[1]/div[1]/div[2]/div[5]') %>% 
	html_children() %>% 
	html_children()

COUNTRY <- eLTER_elements %>% 
	html_text2() %>% 
	gsub(., pattern = "LTER", replacement = "")
for(eL_iter in 1:nrow(CC_df)){
	COUNTRY[grep(CC_df$Country[eL_iter], COUNTRY)] <- CC_df$Alpha.2.Code[eL_iter]
	COUNTRY[grep(CC_df$Alpha.2.Code[eL_iter], COUNTRY)] <- CC_df$Alpha.2.Code[eL_iter]
}
COUNTRY[grep("UK", COUNTRY)] <- "GB"

LINK <- paste0(ReLTER::get_deims_base_url(), 
							 eLTER_elements %>% 
							 	html_elements("a") %>% 
							 	html_attr("href")
	)
	
eLTER_df <- data.frame(ASSOCIATION = "eLTER",
											 COUNTRY = COUNTRY,
											 LINK = LINK,
											 CONTACT = NA,
											 EMAIL = NA)
eLTER_df <- eLTER_df[sapply(eLTER_df$COUNTRY, nchar) == 2, ]


message("Where do we go from here?")

## LifeWatch -------------------------------------------------------------------
link <- "https://www.lifewatch.eu/contact-us/"
LifeWatch_page <- read_html(link)

LW_elements <- LifeWatch_page %>% 
	html_elements("p")
LW_text <- LW_elements %>% html_text2()
LW_nations <- LW_elements[grep(LW_text, pattern = 'National branch details are listed on their')] %>% 
	html_children()

COUNTRY <- CC_df$Alpha.2.Code[
	LW_nations[-1] %>% # the first element is just a line break
		html_text() %>% 
		# subset(. != "") %>% 
		gsub(., pattern = " ", replacement = "") %>% 
		match(., CC_df$Country)
	]
LINK <- c(
	# the first object is not of type strong
	LW_nations[2] %>% 
	html_attr("href"), 
	# all others can be subsetted for type a
	LW_nations %>% 
	html_elements("a") %>% 
	html_attr("href"))

LifeWatch_df <- data.frame(ASSOCIATION = "LifeWatch",
													 COUNTRY = COUNTRY,
													 LINK = LINK,
													 CONTACT = NA,
													 EMAIL = NA)
for(LW_iter in 1:nrow(LifeWatch_df)){
	print(LifeWatch_df$COUNTRY[LW_iter])
	LifeWatchNode_page <- read_html(LifeWatch_df$LINK[LW_iter])
	EMAIL <- LifeWatchNode_page %>% 
		html_elements(css = ".elementor-icon-list-text") %>% 
		html_text() %>% 
		gsub(., pattern = "\\[at\\]", replacement = "@") %>% 
		gsub(., pattern = "\\|\\]", replacement = "@") %>% 
		grep(., pattern = "@", value = TRUE)
	LifeWatch_df$EMAIL[LW_iter] <- EMAIL
	
	CONTACT <- LifeWatchNode_page %>% 
		html_elements(css = ".jet-team-member") %>% 
		html_text2() %>% 
		grep(., pattern = "National Coordinator", value = TRUE) %>% 
		strsplit("\n")
	if(length(CONTACT) == 0){ # no national coordinator mentioned (usually means just one member listed)
		CONTACT <- LifeWatchNode_page %>% 
			html_elements(css = ".jet-team-member") %>% 
			html_text2() %>% 
			strsplit("\n")
	}
	LifeWatch_df$CONTACT[LW_iter] <- CONTACT[[1]][1]
}
write.csv(LifeWatch_df, file = "RIContacs_LifeWatch.csv")

## Fusing ----------------------------------------------------------------------
library(data.table)

View(rbindlist(list(GBIF_df, DiSSCo_df, eLTER_df, LifeWatch_df), fill = TRUE))
