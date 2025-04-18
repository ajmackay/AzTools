% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow.R
\name{new_project}
\alias{new_project}
\title{Create New R Project}
\usage{
new_project(path, open = TRUE)
}
\arguments{
\item{path}{Path where new project should be created}

\item{open}{Logical: Should tyhe new project be opened after creation?}
}
\value{
Invisibly returns the path to the new project
}
\description{
Creates and opens a new R project with the template files in the AzTools package.
Initialises a git repository and performs an initial commit.
}
