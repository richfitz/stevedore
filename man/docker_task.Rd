% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help.R
\name{docker_task}
\alias{docker_task}
\title{Management commands for working with a particular docker task}
\description{
Methods for working with a particular docker task.  Task objects
are returned by using \code{$task$get} to fetch an existing task
by name or id, or \code{$tasks} from a
\code{\link{docker_service}} object representing a docker service.
}
\details{
\Sexpr[results=rd,stage=render]{stevedore:::generate_help("docker_task")}
}
\seealso{
\code{\link{docker_task_collection}} for other
  task management methods.
}
