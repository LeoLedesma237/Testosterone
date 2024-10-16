# This script will be looking at the screening decisions of the testosterone papers
Root.Folder <- "~/Testosterone/Abstract Screening"
setwd(Root.Folder)

# Load in packages
library(openxlsx)

# load in the data
decisions.org <- read.csv("customizationsCor_log.csv")
articles <- read.csv("articlesCor.csv")

# Quickly clean articles
articles$article_id <- as.numeric(gsub("\\D", "", articles$key))

# Choose variables of interest
articles <- select(articles, title, article_id)

# Keep only 'included' for key, this determines the decision
decisions <- decisions.org %>%
  filter(key %in% "included")

# Data cleaning
decisions <- decisions %>%
  mutate(tester = sub("@.*", "",user_email))

# Select the variables of interest
decisions <- select(decisions, date = created_at, tester, article_id, value)

# Arrange the dataset by article and by date
decisions.arranged <- arrange(decisions, article_id, date)

# Extract the unique article IDs into a vector
unique.article.ids <- unique(decisions.arranged$article_id)


# Identify the first two unique testers for each article
tester.scores <- list()

for(ii in 1:length(unique.article.ids)) {

  # Extract current artticle ID
  current.article <- filter(decisions.arranged, article_id %in% unique.article.ids[ii])
  
  # Extract the names of the first two testers
  first.two <- unique(current.article$tester)[1:2]

  # Create an empty vector
  row_numbers <- c()
  
  # Loop through each of the first two unique values and get the first occurrence
  for (i in 1:length(first.two)) {
    
    row_number <- tail(which(current.article$tester == first.two[i]),1)  # Get the first occurrence only
    row_numbers <- c(row_numbers, row_number)
  }
    
  # Extract the values
  values <- as.numeric(current.article$value[row_numbers])
  
  # Create a new dataframe
  two.tester.df <- data.frame(article_id = current.article$article_id[1:2],
                              response.num = c("Response1", "Response2"),
                              responses = values)
  
  # Convert to wide format
  two.tester.df.wide <- two.tester.df %>%
    pivot_wider(names_from = response.num, values_from = responses) %>%
    data.frame()
  
  # Add the testers
  two.tester.df.wide$testers <- paste(first.two, collapse = "; ")

  # Introduce a conflict variable
  two.tester.df.wide$Conflict <- ifelse(two.tester.df.wide$Response1 == two.tester.df.wide$Response2,
                                        "-",
                                        "Yes")
  
  # Save the dataset
  tester.scores[[ii]] <- two.tester.df.wide
      
}


# Save the dataset as one data frame
final.data <- do.call(rbind, tester.scores)

# Introduce the article name to final data
final.data <- final.data %>%
  full_join(articles, by = "article_id") %>%
  tibble()

# Descriptives
#table(final.data$Conflict)

# Testers involved in conflicts
conflicts <- filter(final.data, Conflict == "Yes")
conflict.testers <- do.call(c,str_split(conflicts$testers, "; "))
sort(table(conflict.testers))


# No conflicts
no.conflicts <- filter(final.data, Conflict == "-")

# Arrange the data by results
no.conflicts <- arrange(no.conflicts, desc(Response1))
conflicts <- arrange(conflicts, desc(Response1))

# Set as working directory
setwd("~/Testosterone/Abstract Screening/Abstract_Screening_Outcomes")

# Save the data
write.xlsx(conflicts, "Cortisol_Conflicts.xlsx")
write.xlsx(no.conflicts, "Cortisol_NoConflicts.xlsx")

