## This code is run daily to ensure that number of cases in our working dataset is properly pulled from the 
## Georgia DPH website with number of confirmed cases in each county
library(rvest)
library(lubridate)

# Tell which website to look in / scrape HTML cod 
dailyCountWebsite <- "https://d20s4vd27d0hk0.cloudfront.net/?initialWidth=663&childId=covid19dashdph&parentTitle=COVID-19%20Daily%20Status%20Report%20%7C%20Georgia%20Department%20of%20Public%20Health&parentUrl=https%3A%2F%2Fdph.georgia.gov%2Fcovid-19-daily-status-report"
tables = read_html(dailyCountWebsite)

# Telling R to grab only the TABLES from the website
tbls <- html_nodes(tables, "table")


# head(tbls)

# From looking at the website, the table we want is the 3rd table on the site
tbls_ls <- tbls %>%
  html_nodes("table") %>%
  .[6] %>%                    ## Indicating we want the 3rd table
  html_table(fill = TRUE)

# Creating a dataframe from the picked table
countyCasesDaily = as.data.frame(tbls_ls)
names(countyCasesDaily) = c("County", "Cases", "Deaths")
countyCasesDaily = countyCasesDaily[c(-1, -nrow(countyCasesDaily)), -3]


# countyCasesDaily$date = ymd(Sys.Date())


# Reading in our working (full) dataset of all counties that we want to populate 
countsToBeUpdated = read.csv("Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv")
countsToBeUpdated$date = as.Date(countsToBeUpdated$date)
countsToBeUpdated[is.na(countsToBeUpdated)] <- 0
names(countsToBeUpdated)[1] = "date"







library(tidyr)
library(plyr)


# going through DPH website and populating the number of cases for each county in our working dataset
for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(countsToBeUpdated))){
    
    countsToBeUpdated[countsToBeUpdated$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}

# Overwriting our working (full) dataset with up to date case numbers for each county in GA
write.csv(countsToBeUpdated, "Data/ACC Healthcare Region Simulation  - Case Counts by County GA.csv", row.names = F)


detach("package:plyr", unload=TRUE)





## This bit of code just goes through and populates counties in the Athens area -- primary and secondary
## I guess we could just subset the full working dataset above and indicate which we want, but 
## to minimize that work, I just made a CSV file with only the primary / secondary counties and put an 
## indicator variable to tell which kind (include in both primary AND sec or just secondary)


primSecCounties = read.csv("Data/primary and secondary counties cases.csv")
primSecCounties$date = as.Date(countsToBeUpdated$date)
primSecCounties[is.na(primSecCounties)] <- 0
names(primSecCounties)[1] = "date"



library(tidyr)
library(plyr)

for (i in 1:nrow(countyCasesDaily)){
  
  if (countyCasesDaily$County[i] %in%(names(primSecCounties))){
    
    primSecCounties[primSecCounties$date == Sys.Date(), as.character(countyCasesDaily$County)[i]] = 
      countyCasesDaily[countyCasesDaily$County == as.character(countyCasesDaily$County)[i], 2]
  }
}


write.csv(primSecCounties, "Data/primary and secondary counties cases.csv", row.names = F)

newCasesDaily = data.frame(matrix(0L, nrow = nrow(primSecCounties), ncol = ncol(primSecCounties)))

for(i in 2:nrow(primSecCounties)) {
  
  newCasesDaily[,1] = primSecCounties[,1]
  
  newCasesDaily[i, 2:18] = as.numeric(primSecCounties[i, 2:18]) - as.numeric(primSecCounties[i-1, 2:18])
  
  names(newCasesDaily) = names(primSecCounties)
}


names(newCasesDaily) = names(primSecCounties)


write.csv(newCasesDaily[1:which(newCasesDaily$date == as.character(Sys.Date())), ], "Data/primSecNewCasesDaily.csv", row.names = F)

detach("package:plyr", unload=TRUE)

# This process will be defined in a function ----
# It is saved in the location: paste0(getwd(),"/","UpdateGitHub.R")
UpdateGitHub <- function(repo=getwd(), untracked=TRUE, stage=TRUE, commit=TRUE, pull=TRUE, push=TRUE) {
  
  # Input: ----
  # - 'repo' must be an atomic string value which is a valid directory that contains the files for this repository. It will be validated by the rprojroot package.
  # - 'untracked' must be an atomic logical value which is used for determining whether or not to process the untracked files.
  # - 'stage' must be an atomic logical value which is used for determining whether or not to stage the files.
  # - 'commit' must be an atomic logical value which is used for determining whether or not to commit the files.
  # - 'pull' must be an atomic logical value which is used for determining whether or not to pull from the repo.
  # - 'push' must be an atomic logical value which is used for determining whether or not to push to the repo.
  
  # Output: ----
  # - Will print updates from the different stages.
  
  # Validate parameters: ----
  stopifnot(is.character(repo))
  stopifnot(dir.exists(repo))
  stopifnot(is.atomic(repo))
  stopifnot(is.atomic(untracked))
  stopifnot(is.atomic(stage))
  stopifnot(is.atomic(commit))
  stopifnot(is.atomic(pull))
  stopifnot(is.atomic(push))
  stopifnot(is.logical(untracked))
  stopifnot(is.logical(stage))
  stopifnot(is.logical(commit))
  stopifnot(is.logical(pull))
  stopifnot(is.logical(push))
  
  # Loop through the required packages. If not installed, then install it. If not loaded, then load it. ----
  packages <- c("git2r","rprojroot")
  for (package in packages) {
    if (!package %in% installed.packages()) {
      install.packages( package
                        , quiet = TRUE
                        , verbose = FALSE
                        , dependencies = TRUE
      )
    }
    if (!package %in% .packages()) {
      suppressPackageStartupMessages(
        suppressWarnings(
          suppressMessages(
            library( package
                     , character.only = TRUE
                     , quietly = TRUE
                     , warn.conflicts = FALSE
                     , verbose = FALSE
            )
          )
        )
      )
    }
  }
  
  # Check the Project Root directory. This is to ensure that the entire repo is captured. ----
  if (getwd() == find_rstudio_root_file()) {
    repo <- getwd()
  } else {
    repo <- find_rstudio_root_file()
  }
  
  # Check if there is anything to do. ####
  if (is.null(unlist(status()))) {
    return (writeLines(paste0("There is nothing to do.")))
  }
  
  # Set credentials. This will require input in the Console. ----
  username <- readline(prompt = "Please enter your GitHub Username: ") #ALWAYS BE CAREFUL ABOUT STORING YOUR CREDENTIALS ON GITHUB!!
  password <- readline(prompt = "Please enter your GitHub Password: ") #ALWAYS BE CAREFUL ABOUT STORING YOUR CREDENTIALS ON GITHUB!!
  credentials <- cred_user_pass(username = username, password = password)
  
  # NOTE: values returned from the status() command are as follows ----
  # 1. "untracked" means new files which have not yet been added to GitHub.
  # 2. "unstaged" means existing files which have been modified but not yet ready to be committed to GitHub.
  # 3. "staged" means files that are staged and ready to be committed.
  
  # Check the Untracked items. The intention is to just push these straight through. So this will Add, Commit, then Push them. ----
  if (untracked == TRUE) {
    num <- length(unlist(status()["untracked"]))
    if (num > 0) {
      writeLines(paste0("There are ", num, " Untracked items to be processed."))
      for (i in 1:num) {
        writeLines(paste0("    ", i, ": ",unlist(status()["untracked"])[i]))
      }
      add(repo, unlist(status()["untracked"]))
      writeLines(paste0("Items have been Staged."))
      commit(message = paste(Sys.time(), "Initial commit", sep = " - "))
      writeLines(paste0("Items have been Committed."))
      push(credentials = credentials)
      writeLines(paste0("Items have been Pushed."))
    }
  }
  
  # Process the Unstaged items. Add them. ----
  if (stage == TRUE) {
    num <- length(unlist(status()["unstaged"]))
    if (num > 0) {
      writeLines(paste0("There are ", num, " Tracked items to be processed."))
      for (i in 1:num) {
        writeLines(paste0("    ", i, ": ", unlist(status()["unstaged"])[i]))
      }
    }
    if (!is.null(unlist(status()["unstaged"]))) {
      add(repo, unlist(status()["unstaged"]))
      num2 <- length(unlist(status()["unstaged"]))
      if (num2 == 0) {
        writeLines(paste0("Items have been Staged."))
      } else if (num == num2) {
        stop ("Something went wrong with the Staging.")
      }
    }
  }
  
  # Process the Staged items. Commit them. ----
  if (commit == TRUE) {
    if (!is.null(unlist(status()["staged"]))) {
      commit(message = paste(Sys.time(), "Update", sep = " - ")) # Generic message, including timestamp.
      num2 <- length(unlist(status()["staged"]))
      if (num2 == 0) {
        writeLines(paste0("Items have been Committed."))
      } else if (num == num2) {
        stop ("Something went wrong with Committing.")
      }
    }
  }
  
  # Do the Pull step. ----
  if (pull == TRUE) {
    pull <- tryCatch ( #tryCatch is utilised because the error message when executing pull() or push() is not very helpful: "too many redirects or authentication replays". The main issue is usually that the credentials are incorrect or missing.
      expr = {
        pull(credentials = credentials)
      },
      error = function (err) {
        message (paste0("Error when Pulling from GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
        stop (err)
      },
      warning = function (war) {
        message ("There was a Warning when Pulling from GitHub.")
        return (war)
      },
      finally = {
        # It was successful. Move on.
      }
    )
    if (unlist(pull["up_to_date"]) == TRUE) {
      writeLines(paste0("There are no discrepancies with the Master branch."))
    } else {
      stop ("Something went wrong with pulling the repo. Please manually check, merge the code, validate discrepancies, then re-try.")
    }
  }
  
  # Process the Committed items. Push them. ----
  if (push == TRUE) {
    if (num > 0) {
      tryCatch(
        expr = {
          push(credentials = credentials)
        },
        error = function(err) {
          message (paste0("Error when Pushing to GitHub. Try checking your credentials and try again.","\n","Message thrown: "))
          stop (err)
        },
        warning = function (war) {
          message ("There was a Warning when Pushing to GitHub.")
          return (war)
        },
        finally = {
          # It was successful. Move on.
        }
      )
      num2 <- length(unlist(status()))
      if (num2 == 0) {
        writeLines(paste0("Items have been Pushed."))
      } else if (num == num2) {
        stop ("Something went wrong with Pushing.")
      }
    }
  }
  
  return(writeLines(paste0("Successfully updated.")))
  
}

# Run the function.
UpdateGitHub()

