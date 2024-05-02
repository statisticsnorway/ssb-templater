
#' Fix files by replacement of holders
#'
#' @param destination Where the file should be stored
#' @param file Target file
#' @param find Term to find
#' @param replace Replacement term
#'
#' @return NULL
fix_file <- function(destination, file, find, replace){
  destination_path <- file.path(destination, file)

  content <- readLines(destination_path)
  content <- gsub(find, replace, content, fixed = TRUE)

  # Check if the last line is a newline; if not, add one
  if(length(content) > 0 && nchar(content[length(content)]) > 0){
    content <- c(content, "")  # Add an empty string as the new final line
  }

  writeLines(content, destination_path)
}


#' SSB package template
#'
#' Create an R package with standard SSB format
#'
#' @param path Where the package should be created.
#' @param description A short description of what the package does.
#' @param firstname First name of the author
#' @param surname Surname of the Author
#' @param github Boolean for whether to create a github repository for the package. Default = TRUE
#'
#' @return NULL
#' @export
ssb_rproject <- function(path, description,
                          firstname, surname, github){
  wd_dir <- getwd()
  base_dir <- dirname(path)
  project_name <- basename(path)
  original_path <- NULL

  if (substr(project_name, 1, 4) == "stat-"){
    prefixed_name <- project_name
    project_name <- substring(project_name, 5, nchar(project_name))
  } else {
    original_path <- path
    prefixed_name <- paste0("stat-", project_name)
    path <- file.path(base_dir, prefixed_name)
  }

  # Create the local directory with the prefixed name
  dir.create(path, recursive = TRUE)
  message("Project created at: ", path)

  # Specify other variables
  year <- substring(Sys.Date(), 1, 4)
  user <- Sys.info()['user']
  email <- paste0(user, '@ssb.no')

  # Get the list of files and directories inside the template_path
  template_path <- system.file("rstudio/templates/project/project", package = "templater")
  template_contents <- list.files(template_path, full.names = TRUE)
  template_contents <- template_contents[!grepl("create_ssb_project", template_contents)]

  # Copy each file and directory in template_contents to destination
  for (file in template_contents) {
    file.copy(file, path, recursive = TRUE)
  }
  Sys.sleep(2)

  # Download files from KVAKK and ssb-project-client
  gitignore_url <- "https://raw.githubusercontent.com/statisticsnorway/kvakk-git-tools/main/kvakk_git_tools/recommended/gitignore"
  gitattributes_url <- "https://raw.githubusercontent.com/statisticsnorway/kvakk-git-tools/main/kvakk_git_tools/recommended/gitattributes"
  security_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/SECURITY.md"
  conduct_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/CODE_OF_CONDUCT.md"
  licence_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/LICENSE"

  utils::download.file(gitignore_url, destfile = paste0(path,"/.gitignore"), method = "auto", quiet=T)
  utils::download.file(gitattributes_url, destfile = paste0(path,"/.gitattributes"), method = "auto", quiet=T)
  utils::download.file(security_url, destfile = paste0(path,"/SECURITY.md"), method = "auto", quiet=T)
  utils::download.file(conduct_url, destfile = paste0(path,"/CODE_OF_CONDUCT.md"), method = "auto", quiet=T)
  utils::download.file(licence_url, destfile = paste0(path,"/LICENSE"), method = "auto", quiet=T)

  # Fix Readme file
  fix_file(path, "README.md", find = "{{PROJECT_NAME}}", project_name)
  fix_file(path, "README.md", find = "{{PROJECT_DESCRIPTION}}", description)

  # Fix Licence files
  fix_file(path, "LICENSE", find = "2022", year)

  # Fix SECURITY
  fix_file(path, "SECURITY.md", find = "ssb-project-cli", prefixed_name)

  # Fix name of project file
  setwd(path)
  file.rename( "packagename.Rproj", paste0(prefixed_name, ".Rproj"))

  # Start git
  usethis::use_git_config(user.name = firstname, user.email = email)
  usethis::use_git()
  usethis::git_default_branch_configure(name = "main")

  # Set up tests
  usethis::use_testthat()
  usethis::use_test("hello_world.R", open = F)

  # Set up github
  if (github){

    print("Project setting up. Preparing to create a repo on github...")
    if (Sys.getenv("GITHUB_PAT") == ""){
      Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priviledges):"))
    }

    usethis::use_github(organisation = "statisticsnorway",
                        visibility = "internal", protocol = "https")

    # Set up test action
    usethis::use_github_action("check-standard", badge = TRUE)

    # Set up pkgdown
    #usethis::use_pkgdown_github_pages()

    # Set up other things
    #usethis::use_github_links()

    # Push all changes
    git2r::add(path=".")
    git2r::commit(message="Initial commit.")
    git2r::push(credentials=git2r::cred_token())

    # Rename repo
    url <- paste0("https://api.github.com/repos/statisticsnorway/", project_name)
    response <- httr::PATCH(url, body = list(name = prefixed_name),
                            httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                            encode = "json")
    httr::content(response)

    # Add branch protection
    response <- gh::gh(
      "PUT /repos/:owner/:repo/branches/:branch/protection",
      owner = "statisticsnorway",
      repo = prefixed_name,
      branch = "main",
      .token = Sys.getenv("GITHUB_PAT"),
      required_status_checks = NA,  # No specific status checks are mentioned
      enforce_admins = TRUE,  # Do not allow bypassing of the settings
      required_pull_request_reviews = list(
        dismiss_stale_reviews = TRUE,  # Dismiss stale pull request approvals when new commits are pushed
        require_code_owner_reviews = FALSE,  # No specific requirement for code owner reviews was mentioned
        required_approving_review_count = 1  # Require at least one approval
      ),
      restrictions = NA,  # No specific user or team restrictions mentioned
      required_pull_requests_reviews_enforcement_level = "everyone"  # Enforce rules on everyone, including admins
    )
  }

  # Open new project
  Sys.sleep(2)
  setwd(wd_dir)
  rstudioapi::openProject(path = path, newSession = FALSE)

  # Cancel further processes - this supresses Rstudio from creating a duplicate project (without suffix)
  # This is currently sending an error message which I haven't been able to suppress.
  stop("Finished setting up package! This looks like an error but isn't (Just click ok).")
}


