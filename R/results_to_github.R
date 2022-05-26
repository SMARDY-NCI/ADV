# Ensure changes to any code have been pulled to the server
system("git pull")

# Run file provided as argument
source(commandArgs(trailingOnly=TRUE)[1])

# Commit RData files and push to github
system("make commit_RData")