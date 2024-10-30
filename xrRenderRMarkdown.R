#!/usr/bin/env Rscript

ProjectDir <- getwd()

rags <- commandArgs(trailingOnly=TRUE)
rmdScript <- rags[1]

outHTML <- stringr::str_replace(rmdScript, ".Rmd", ".html")
outHTML <- file.path("docs", outHTML)

message("Script:", rmdScript)
message("Ouput:", outHTML)

outHTML <- file.path(ProjectDir, outHTML)

rmarkdown::render(rmdScript,
                  output_file=outHTML,
                  output_format = "html_document")
