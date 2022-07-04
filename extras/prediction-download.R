# @file prediction-download.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of OhdsiShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' The module viewer for downloading prediction model pacakges
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The user interface to the downloading prediction model pacakges module
#'
#' @export
predictionDownloadViewer <- function(id) {
  
  ns <- shiny::NS(id)
  
  shiny::div(
    
    shinydashboard::box(
      title = "Development R Package", 
      status = 'info', 
      solidHeader = T,
      shiny::p("Click here to download an R package that contains all the settings requires to replicate the model development using any OMOP CDM database."),
      shiny::actionButton(
        inputId = ns('downloadPackageDev'), 
        label = "Download Development"
      )
    ),
    
    shinydashboard::box(
      title = "Validation R Package", 
      status = 'info', 
      solidHeader = T,
      shiny::p("Click here to download an R package that contains all the settings requires to validate the existing model using any OMOP CDM database."),
      shiny::actionButton(
        inputId = ns('downloadPackageVal'), 
        label = "Download Validation"
      )
      
    )
  )
}

#' The module server for prediction model package downloading
#'
#' @details
#' The user specifies the id for the module
#'
#' @param id  the unique reference id for the module
#' 
#' @return
#' The server to the prediction download package module
#'
#' @export
predictionDownloadServer <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(
        input$downloadPackageDev, 
        {
          dir.create(file.path(tempdir(), 'devPackage'), recursive = T)
          #Hydra::hydrate(specifications = specifications, outputFolder = outputPackageLocation)
          createPackage <- tryCatch(
            {downLoadSkeleton(
              outputFolder = file.path(tempdir()),
              packageName = 'devPackage',
              skeletonType = 'SkeletonPredictionStudy'
            )#'SkeletonPredictionValidationStudy'
            }, error = function(e){return(NULL)}
          )
          
          if(!is.null(createPackage)){
            createPackage <- tryCatch(
              {replaceName(
                packageLocation = file.path(tempdir(), 'devPackage'),
                packageName = 'devPackage',
                skeletonType = 'SkeletonPredictionStudy')
              },
              error = function(e){return(NULL)}
            )
          }
          
          
        })
      
    }
  )
}

### DOWNLOAD

downLoadSkeleton <- function(
  outputFolder,
  packageName,
  skeletonType = 'SkeletonPredictionStudy'
){
  utils::download.file(
    url = paste0("https://github.com/ohdsi/",skeletonType,"/archive/main.zip"), 
    destfile = file.path(outputFolder, "package.zip")
  )
  # unzip the .zip file
  utils::unzip(
    zipfile = file.path(outputFolder, "package.zip"), 
    exdir = outputFolder
  )
  file.rename( 
    from = file.path(outputFolder, paste0(skeletonType, '-main')),
    to = file.path(outputFolder,  packageName)
  )
  unlink(file.path(outputFolder, "package.zip"))
  return(file.path(outputFolder, packageName))
}

# change name
replaceName <- function(
  packageLocation = getwd(),
  packageName = 'ValidateRCRI',
  skeletonType = 'SkeletonPredictionValidationStudy'
){
  
  filesToRename <- c(paste0(skeletonType,".Rproj"),paste0("R/",skeletonType,".R"))
  for(f in filesToRename){
    ParallelLogger::logInfo(paste0('Renaming ', f))
    fnew <- gsub(skeletonType, packageName, f)
    file.rename(from = file.path(packageLocation,f), to = file.path(packageLocation,fnew))
  }
  
  filesToEdit <- c(
    file.path(packageLocation,"DESCRIPTION"),
    file.path(packageLocation,"README.md"),
    file.path(packageLocation,"extras/CodeToRun.R"),
    dir(file.path(packageLocation,"R"), full.names = T)
  )
  for( f in filesToEdit ){
    ParallelLogger::logInfo(paste0('Editing ', f))
    x <- readLines(f)
    y <- gsub( skeletonType, packageName, x )
    cat(y, file=f, sep="\n")
    
  }
  
  return(packageName)
}
