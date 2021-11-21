#########################
# BUSINESS CONFIDENCE
#########################

rm(list = ls())

# Libraries
library(magick)
library(rvest)
library(dplyr)

# India Business Expectation Index
URL0 <- "https://rbi.org.in/Scripts/QuarterlyPublications.aspx?head=Quarterly+Industrial+Outlook+Survey"

library(rvest)
Latest <- read_html(URL0) %>% 
  html_nodes(".tablebg td a") %>% 
  .[1] %>% 
  html_attr("href") %>% 
  paste0("https://rbi.org.in/Scripts/",. )

BEI <- read_html(Latest) %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  stringr::str_subset(".xlsx")

filename <- gsub(".*\\/", "", BEI)
path <- file.path(getwd(), filename)
download.file(trimws(BEI), path, quiet = TRUE, mode = "wb")
data <- readxl::read_excel(path, sheet = "Table 23", skip = 1) %>% 
  filter(grepl("^Q[0-9].*", Quarter))
unlink(path)

data2 <- data %>% 
  mutate(Quarter = paste0("Q",
                          ifelse(as.numeric(stringr::str_extract(Quarter, "(?<=Q).*(?=:)"))>=4,
                                 1,
                                 as.numeric(stringr::str_extract(Quarter, "(?<=Q).*(?=:)"))+1),
                          "'",
                          ifelse(as.numeric(stringr::str_extract(Quarter, "(?<=Q).*(?=:)"))>=4,
                          as.numeric(substr(stringr::str_extract(Quarter, "(?<=:).*(?=-)"),3,4))+1,
                          substr(stringr::str_extract(Quarter, "(?<=:).*(?=-)"),3,4))
                          )) %>% 
  .[1:(nrow(.)-1),] %>% 
   slice_tail(n=12)

data2$Quarter = factor(data2$Quarter, unique(data2$Quarter))

# Plot
library(ggplot2)
library(ggtext)
ggplot(data = data2, 
       aes(x=Quarter, y = `Business Assessment Index (BAI)`, group=1)) +
  geom_line(color = "#666666")+
  theme_light() +
  
  # Add labels at the beginning of the line
  geom_richtext(data = slice_head(data2, n=1),
                aes(label = scales::comma(`Business Assessment Index (BAI)`, 0.1)), 
                vjust = -0.5, hjust = 0.5) +
  
  # Add labels at the end of the line
  geom_richtext(data = slice_tail(data2, n=1),
                aes(label = scales::comma(`Business Assessment Index (BAI)`, 0.1)), 
                vjust = 1.5, hjust = 0.5)  +
  
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off') +
  
  scale_y_continuous(labels = scales::comma) +
  
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  ) +
  labs(title  = paste("Business Assessment Index"),
       caption = "Source: RBI")


# Save
path <- file.path(getwd(), "Blog", "Economic")
ggsave(paste0(path,"/BAI.png"), 
       width = 20, 
       height = 20, 
       units = "cm")

# =================
# Push to Github
# =================

library(gert)

# Global settings
git_config_global_set("user.name", "marksac")
git_config_global_set("user.email", "bhandari_ujjawal@yahoo.com")

# Repo
repoName <- "jsonfile"
repoLink <- "https://github.com/markwsac/"

# Set Password
# library(credentials)
# git_credential_update(repoLink)
# credentials::set_github_pat()
# ssh_key_info()

# Set Working Directory
path <- file.path("C:/Users/deepa/Documents", "Blog", "Economic", repoName)
setwd(path)

# Clone
repo <- git_clone(paste0(repoLink, repoName))

# Set Working Directory
path <- file.path("C:/Users/deepa/Documents", "Blog", "Economic", repoName)
setwd(path)


# Create a branch
git_branch_create("mybranch", checkout = TRUE)

# Add file
myfile <- "BusinessExpectationsIndex.R"
fileToAdd <- paste0(gsub(repoName, "", getwd()), myfile)
file.copy(from=fileToAdd, 
          to=getwd(), 
          overwrite = TRUE, 
          recursive = FALSE, 
          copy.mode = TRUE)

git_add(".")
git_commit("Adding a file")

# Merge it in master
git_branch_checkout("main")
git_merge("mybranch")
git_branch_delete("mybranch")

# Push
git_push(remote = "origin", 
         repo = ".")

# Remove the commit
git_reset_hard("HEAD^")

