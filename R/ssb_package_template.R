
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
ssb_rtemplate <- function(path, description,
                          firstname, surname, github){
  wd_dir <- getwd()
  base_dir <- dirname(path)
  package_name <- basename(path)
  original_path <- NULL

  if (substr(package_name, 1, 4) == "ssb-"){
    prefixed_name <- package_name
    package_name <- substring(package_name, 5, nchar(package_name))
  } else {
    original_path <- path
    prefixed_name <- paste0("ssb-", package_name)
    path <- file.path(base_dir, prefixed_name)
  }

  # Create the local directory with the prefixed name
  dir.create(path, recursive = TRUE)
  message("Project created at: ", path)

  # Specify other variables
  year <- substring(Sys.Date(), 1, 4)
  user <- Sys.info()['user']
  email <- paste0(user, '@ssb.no')

  # Copy files to new project
  get_files(path)
  get_standard_files_offline(path)

  # Fix Readme file
  fix_file(path, "README.md", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(path, "README.md", find = "{{PACKAGE_NAME_CODE}}", prefixed_name)
  fix_file(path, "README.md", find = "{{PACKAGE_DESCRIPTION}}", description)

  # Fix description
  fix_file(path, "DESCRIPTION", find = "{{PACKAGE_NAME}}", package_name)
  fix_file(path, "DESCRIPTION", find = "{{PACKAGE_DESCRIPTION}}", description)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_NAME1}}", firstname)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_NAME2}}", surname)
  fix_file(path, "DESCRIPTION", find = "{{AUTHOR_EMAIL}}", email)

  # Fix Licence files
  fix_file(path, "LICENSE.md", find = "2022", year)
  fix_file(path, "LICENSE", find = "{{YEAR}}", year)

  # Fix SECURITY
  fix_file(path, "SECURITY.md", find = "ssb-project-cli", prefixed_name)

  # Add project file
  create_project_file(path, prefixed_name = prefixed_name,
                project_type = "package")
  setwd(path)

  # Add comments file
  usethis::use_cran_comments(open=F)

  # Add news file
  usethis::use_news_md(open = F)

  # Add buildignore
  usethis::use_build_ignore("cran-comments.md")

  # Add example data
  #' @export
  test_data <- data.frame(x = stats::runif(10), y=stats::runif(10))
  usethis::use_data(test_data, overwrite=TRUE)

  # Add NAMESPACE and documents
  roxygen2::roxygenise()

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
      Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priveldges):"))
    }

    usethis::use_github(organisation = "statisticsnorway",
                        visibility = "internal", protocol = "https")

    # Set up test action
    usethis::use_github_action("check-standard", badge = TRUE)

    # Set up pkgdown
    usethis::use_pkgdown_github_pages()

    # Set up other things
    usethis::use_github_links()

    # Push all changes
    git2r::add(path=".")
    git2r::commit(message="Initial commit.")
    git2r::push(credentials=git2r::cred_token())

    # Rename repo
    url <- paste0("https://api.github.com/repos/statisticsnorway/", package_name)
    response <- httr::PATCH(url, body = list(name = prefixed_name),
                            httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                            encode = "json")

    # Add branch protection
    add_branch_protect(prefixed_name)
  }

  # Open project
  Sys.sleep(2)
  setwd(wd_dir)
  rstudioapi::openProject(path = path, newSession = FALSE)

  # Cancel further processes - this supresses Rstudio from creating a duplicate project (without suffix)
  # This is currently sending an error message which I haven't been able to suppress.
  stop("Finished setting up package! This looks like an error but isn't (Just click ok).")
}

get_files <- function(path){
    # Get the list of files and directories inside the template_path
    template_path <- system.file("rstudio/templates/project/package", package = "templater")
    template_contents <- list.files(template_path, full.names = TRUE, all.files = TRUE)

    # Copy each file and directory in template_contents to destination
    for (file in template_contents) {
        file.copy(file, path, recursive = TRUE)
    }
    Sys.sleep(3)
}

#' Download standard files
get_standard_files <- function(path){
    # Set file paths from KVAKK and ssb-project-client
    gitignore_url <- "https://raw.githubusercontent.com/statisticsnorway/kvakk-git-tools/main/kvakk_git_tools/recommended/gitignore"
    gitattributes_url <- "https://raw.githubusercontent.com/statisticsnorway/kvakk-git-tools/main/kvakk_git_tools/recommended/gitattributes"
    security_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/SECURITY.md"
    conduct_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/CODE_OF_CONDUCT.md"
    licence_url <- "https://raw.githubusercontent.com/statisticsnorway/ssb-project-cli/main/LICENSE"

    # Download to destination path
    utils::download.file(gitignore_url, destfile = paste0(path,"/.gitignore"), method = "auto", quiet=T)
    utils::download.file(gitattributes_url, destfile = paste0(path,"/.gitattributes"), method = "auto", quiet=T)
    utils::download.file(security_url, destfile = paste0(path,"/SECURITY.md"), method = "auto", quiet=T)
    utils::download.file(conduct_url, destfile = paste0(path,"/CODE_OF_CONDUCT.md"), method = "auto", quiet=T)
    utils::download.file(licence_url, destfile = paste0(path,"/LICENSE.md"), method = "auto", quiet=T)
}

#' Copy project files
#' Copy standard file to new project without accessing github standard files
get_standard_files_offline <- function(path){
    template_path <- system.file("rstudio/templates/project/standardfiles", package = "templater")
    template_contents <- list.files(template_path, full.names = TRUE, all.files = TRUE)

    for (file in template_contents) {
        file.copy(file, path, recursive = TRUE)
    }
    Sys.sleep(3)

    # specifically copy .gitignore and .gitattributtes
    file.copy(file.path(template_path, ".gitignore"), path)
    file.copy(file.path(template_path, ".gitattributes"), path)
}


#' Create project file
#'
create_project_file <- function(path, prefixed_name, project_type = "package"){

    project_file <- file.path(path, paste0(prefixed_name, ".Rproj"))
    if (project_type == "package"){
        print("in package control")
        package_lines <- c(
            sprintf('BuildType: Package\n'),
            sprintf('PackageUseDevtools: Yes\n'),
            sprintf('PackageInstallArgs: --no-multiarch --with-keep.source\n'),
            sprintf('PackageRoxygenize: rd,collate,namespace\n')
        )}
    if (project_type == "project"){
        print("in project control")
        package_lines <- ""
    }
    writeLines(
        c(
            sprintf('Version: 1.0\n'),
            sprintf('RestoreWorkspace: Default\n'),
            sprintf('SaveWorkspace: Default\n'),
            sprintf('AlwaysSaveHistory: Default\n'),
            sprintf('EnableBookmarks: Yes\n'),
            sprintf('EnableCodeIndexing: Yes\n'),
            sprintf('NumSPacesForTab: 4\n'),
            sprintf('Encoding: UTF-8\n'),
            sprintf('RnwWeave: Sweave\n'),
            sprintf('LaTeX: pdfLaTeX\n'),
            sprintf('StripTrailingWhitespace: Yes\n'),
            package_lines
        ),
        con = project_file
    )
}

#'Add branch protections
#'
add_branch_protect <- function(prefixed_name, branch = "main"){
    if (Sys.getenv("GITHUB_PAT") == ""){
        Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow priveldges):"))
    }
    gh::gh(
        "PUT /repos/:owner/:repo/branches/:branch/protection",
        owner = "statisticsnorway",
        repo = prefixed_name,
        branch = branch,
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