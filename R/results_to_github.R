require(here)

args = commandArgs(trailingOnly=TRUE)

source(here::here(args))

# Commit results and push to github
system("make commit_Results")