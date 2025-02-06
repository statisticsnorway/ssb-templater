

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

  # Set usethis options
  options(usethis.overwrite = TRUE)

  # Create the local directory with the prefixed name
  dir.create(path, recursive = TRUE)
  message("Project created at: ", path)

  # Specify other variables
  year <- substring(Sys.Date(), 1, 4)
  user <- initialer_funk()
  email <- paste0(user, '@ssb.no')

  # Copy files to new project
  get_files(path, "package")
  get_standard_files(path)

  # Fix and swap out template holders on copied files
  fix_files(path, package_name, prefixed_name, description, firstname, surname, email)

  # Add project file
  create_project_file(path, prefixed_name = prefixed_name,
                project_type = "package")
  setwd(path)
  print(paste0("Project files copied to: ", path))

  # Add comments file
  usethis::use_cran_comments(open=F)

  # Add news file
  usethis::use_news_md(open = F)

  # Add buildignore
  usethis::use_build_ignore(c("cran-comments.md","CODE_OF_CONDUCT.md", "LICENSE.md", "SECURITY.md"))

  # Add example data
  #' Test data
  #'
  #' An example of how test data is documented. See https://r-pkgs.org/data.html#sec-documenting-data
  #'
  #' @format ## `test_data`
  #' A data frame with 10 rows and 2 columns:
  #' \describe{
  #'   \item{x}{Random variable}
  #'   \item{y}{Random variable}
  #' }
  #' @export
  test_data <- data.frame(x = stats::runif(10), y=stats::runif(10))
  Sys.sleep(3) # To ensure that the object is created before saving
  usethis::use_data(test_data, overwrite=TRUE)
  safe_data()

  # Add NAMESPACE and documents
  roxygen2::roxygenise()

  # Start git - still asking about commits and can't find solution to suppress
  usethis::use_git_config(user.name = firstname, user.email = email)
  git2r::init(branch="main")
  git2r::add(repo=".", path=".")
  git2r::commit(message="Initial commit")

  # Set up tests
  usethis::use_testthat()
  usethis::use_test("hello_world.R", open = F)

  # Set up github
  if (github){

    print("Project setting up. Preparing to create a repo on github...")
    if (Sys.getenv("GITHUB_PAT") == ""){
      Sys.setenv(GITHUB_PAT = getPass::getPass("Enter your github PAT (with workflow privileges):"))
    }
    # Check everything commited
    git2r::add(repo=".", path=".")
    git2r::commit(message="Initial commit")

    # Set up repo
    usethis::use_github(organisation = "statisticsnorway",
                        visibility = "internal", protocol = "https")

    # Set up actions
    print("Setting up GitHub actions...")
    add_github_actions(path, type = "package")

    # Push all changes
    git2r::add(repo = ".", path=".")
    git2r::commit(message="Initial commit")
    git2r::push(credentials=git2r::cred_token())

    # Rename repo
    url <- paste0("https://api.github.com/repos/statisticsnorway/", package_name)
    response <- httr::PATCH(url, body = list(name = prefixed_name),
                            httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                            encode = "json")
    # Update remote
    new_remote_url <- paste0("https://github.com/statisticsnorway/", prefixed_name, ".git")
    system(paste("git remote set-url origin", new_remote_url))

    # Add links with new name
    usethis::use_github_links(overwrite = TRUE)

    # Final push
    git2r::add(repo = ".", path=".")
    git2r::commit(message="Initial commit")
    git2r::push(credentials=git2r::cred_token())

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
