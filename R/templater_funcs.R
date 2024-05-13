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


#' Copy files from standard template
#'
#' @param path Path to new project
#' @param project_type Type of project. Choose between "project" or "package".
get_files <- function(path, project_type){
    # Define the template path
    template_path <- system.file(file.path("rstudio/templates/project", project_type), package = "templater")

    # List all files and directories in the template path
    template_contents <- list.files(template_path, full.names = TRUE, all.files = TRUE, recursive = TRUE)

    # Copy each file and directory to the new project path, maintaining structure
    for (file in template_contents) {
        # Determine relative path from template path
        relative_path <- gsub(paste0(template_path, "/"), "", file)

        # Create the full destination path
        destination_file <- file.path(path, relative_path)

        # Ensure the destination directory exists
        if (!dir.exists(dirname(destination_file))) {
            dir.create(dirname(destination_file), recursive = TRUE)
        }

        # Copy the file if it's a file, or create the directory if it's a directory
        if (file.info(file)$isdir) {
            if (!dir.exists(destination_file)) {
                dir.create(destination_file, recursive = TRUE)
            }
        } else {
            file.copy(file, destination_file)
        }

        # Optionally print each file copied for verification
        print(paste("Copying", file, "to", destination_file))
    }

    # Delay to ensure all file operations are completed
    Sys.sleep(3)
}


#' Download standard files
#'
#' @param path Path to where the new project is being set up
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
#'
#' Copy standard file to new project without accessing github standard files
#'
#' @param path Path to where the new project is being set up
get_standard_files_offline <- function(path){
    template_path <- system.file("rstudio/templates/project/standardfiles", package = "templater")
    template_contents <- list.files(template_path, full.names = TRUE, all.files = TRUE)

    for (file in template_contents) {
        file.copy(file, path, recursive = FALSE)
    }
    Sys.sleep(3)

    # Rename gitignore and gitattributes
    # These are copied without . at start as they were being removed form package
    file.rename(file.path(path, "gitignore"), file.path(path, ".gitignore"))
    file.rename(file.path(path, "gitattributes"), file.path(path, ".gitattributes"))
}


#' Create project file
#'
#' @param path to where the new project is being set up
#' @param prefixed_name Name of the package including the ssb- or stat- prefix
#' @param project_type Whether the project is a package ("package") or project ("project").
create_project_file <- function(path, prefixed_name, project_type = "package"){

    project_file <- file.path(path, paste0(prefixed_name, ".Rproj"))
    if (project_type == "package"){
        package_lines <- c(
            sprintf('BuildType: Package\n'),
            sprintf('PackageUseDevtools: Yes\n'),
            sprintf('PackageInstallArgs: --no-multiarch --with-keep.source\n'),
            sprintf('PackageRoxygenize: rd,collate,namespace\n')
        )}
    if (project_type == "project"){
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
#' @param prefixed_name Name of the project including the prefix
#' @param branch Name of the github branch to protect
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

add_github_actions <- function(path, type = "package"){
    # Define the primary function attempt
    tryCatch({

        # Attempt to use a GitHub Action
        usethis::use_github_action("check-standard.yaml", badge = TRUE)

        if (type == "package"){
            usethis::use_pkgdown_github_pages()
            usethis::use_github_links()
        }

        print("GitHub Action setup was successful.")

    }, error = function(e) {
        # If an error occurs, print the error message
        print("No internet access found. Copying actions from template.")

        dir.create(file.path(path, ".github"))
        dir.create(file.path(path, ".github", "workflows"))
        template_path <- system.file("rstudio/templates/project/actionfiles", package = "templater")
        #template_contents <- list.files(template_path, full.names = TRUE, all.files = TRUE)
        action_path <- file.path(path, ".github", "workflows")
        file.copy(file.path(template_path, "R-CMD-check.yaml"), action_path, recursive = FALSE)
        if (type == "package"){
            file.copy(file.path(template_path, "pkgdown.yaml"), action_path, recursive = FALSE)
            file.copy(file.path(template_path, "_pkgdown.yml"), path)
        }
        Sys.sleep(3)

        # add docs to gitignore
        usethis::use_git_ignore("docs/")
    })
}

