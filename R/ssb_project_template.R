
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

    # Establish directories
    wd_dir <- getwd()
    base_dir <- dirname(path)
    project_name <- basename(path)
    original_path <- NULL

    # Establish standard project name
    if (substr(project_name, 1, 5) == "stat-"){
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

    # Copy files to project
    get_files(path, "project")
    get_standard_files_offline(path)

    fix_files(path, prefixed_name, prefixed_name, description,
              firstname, surname, email,
              type = "project")

    # Fix name of project file
    create_project_file(path, prefixed_name = prefixed_name,
                        project_type = "project")
    setwd(path)

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
        add_github_actions(path, type = "project")

        # Push all changes
        git2r::add(path=".")
        git2r::commit(message="Initial commit.")
        git2r::push(credentials=git2r::cred_token())

        # Rename repo
        url <- paste0("https://api.github.com/repos/statisticsnorway/", project_name)
        httr::PATCH(url, body = list(name = prefixed_name),
                                httr::authenticate("", Sys.getenv("GITHUB_PAT")),
                                encode = "json")


        # Add branch protection
        add_branch_protect(prefixed_name)
    }

    # set up renv
    renv::init(restart = FALSE, force = TRUE,
               load = TRUE, bare = TRUE)
    renv::install("testthat")
    renv::snapshot()

    if (github){

        # Push renv changes
        git2r::add(path=".")
        git2r::commit(message="Added renv environment.")
        git2r::push(credentials=git2r::cred_token())

        # Add branch protection
        add_branch_protect(prefixed_name)
    }

    # Open new project
    Sys.sleep(2)
    setwd(wd_dir)
    rstudioapi::openProject(path = path, newSession = FALSE)

    # Cancel further processes - this supresses Rstudio from creating a duplicate project (without suffix)
    # This is currently sending an error message which I haven't been able to suppress.
    stop("Finished setting up package! This looks like an error but isn't (Just click ok).")
}


