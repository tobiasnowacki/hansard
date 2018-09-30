# ==========================================================================
# Scraping Division Data from Hansard API
# ==========================================================================

# Require packages
install.packages("rvest")
install.packages("zoo")
require(rvest)
library(tidyr)
require(zoo)
require(ggplot2)

setwd("~/github/hansard")

# Define functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

scrape_letter <- function(x){
	path <- paste("https://api.parliament.uk/historic-hansard/divisions/", x, 
		collapse = "", sep = "")
	print(path)
	hansard <- read_html(path)

	divisions <- hansard %>% html_nodes("#divisions") %>% html_table(trim = T)

	divisions <- divisions[[1]]
	divisions[divisions == ""] <- NA
	divisions <- divisions %>% fill(X1, X2, X3, X4)
	names(divisions) <- c("name", "id", "date", "type")
	divisions$year <- as.numeric(substrRight(divisions$date, 4))

	# divisions$links <- hansard %>% html_nodes(".division_numbers")
	return(divisions)
}

# Scrape divisions across all letters
all_letters <- c("index.html", letters[c(2:23, 25, 26)])

results <- list()

for (i in 1:length(all_letters)){
	l <- all_letters[i]
	print(l)
	results[[i]] <- scrape_letter(l)
}
index <- do.call("rbind", results)

write.csv(index, "index.csv")

# Analysis

# Plot histogram of divisions by year (by Commons / Lords)
hist_plot <- ggplot(index, aes(x = year)) + 
	geom_histogram() +
	labs(x = "Year", y = "No. of Divisions", 
		title = "Number of Divisions in Parliamentary API, by year") +
	facet_wrap(. ~ type) +
	theme_classic()
ggsave("hansard_freq.pdf", hist_plot)

# Output no. of Commons divisions before 1850, by year.
table(index$year[index$year < 1850 & index$type == "Commons"])

# Conclusion: virtually no Commons divisions pre-1850 recorded by API...