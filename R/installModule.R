#' The user interface for installing ohdsi github packages
#' @param id   The module identifier
#' @param label The name of the ohdsi github package
#'
#' @examples
#' \dontrun{
#' installViewer("installPatientLevelPrediction", "PatientLevelPrediction")
#' }
#' @export
installViewer <- function(id, label = "Package") {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("placeholder"))
}

#' The server for installing ohdsi github packages
#' @param input   Standard for shiny modules
#' @param output  Standard for shiny modules
#' @param session Standard for shiny modules
#' @param package The name of the ohdsi github package
#'
#' @examples
#' \dontrun{
#' webApi <- callModule(webApiServer, 'webApiMain')
#' cohortReactive <- callModule(OhdsiShinyModules::installServer, 'installPatientLevelPrediction',
#'                             package = 'PatientLevelPrediction')
#' }
#' @export
installServer <- function(input, output, session, package) {
  
  info <- shiny::reactiveVal(installInfo(package = package))
  
  output$placeholder = shiny::renderUI({
    shinydashboard::box(title = package,
                        status = getStatus(info()$main),
                        solidHeader = T,
                        shiny::textOutput(session$ns('availableVersion')),
                        shiny::textOutput(session$ns('installedVersion')),
                        shiny::actionButton(inputId = session$ns('install_button'), label = 'Install'),
                        shiny::actionButton(inputId = session$ns('View'), label = 'View Details')
    )
  })
  
  output$availableVersion <- shiny::renderText(paste('Latest Version:', info()$main$versionAvailable))
  output$installedVersion <- shiny::renderText(paste('Installed Version:', info()$main$versionInsalled))
  
  output$installDeps <- shiny::renderDataTable(info()$dependencies)
  
  
  # install/update
  shiny::observeEvent(input$install_button, {
    shiny::showModal(installPackageModal(session$ns, package))
  })
  
  shiny::observeEvent(input$install, {
    # check devtools and install if needed
    install.packages(setdiff('devtools', rownames(installed.packages())))
    
    # install package
    shiny::showNotification(paste0('Starting to install ',package, ' - modal will close when package install is complete'), duration = 5)
    devtools::install_github(repo = paste0('ohdsi/',package), upgrade = "never")
    
    status <- ifelse(length(tryCatch({as.character(packageVersion(package))},error = function(e){return(NULL)}))==1, 'success','warning')
    
    if(status == 'success'){
      shiny::showNotification(paste0('Installed ',package), duration = 5)
    }else{
      shiny::showNotification(paste0('Installation Issue with ',package), duration = 5)
    }
    
    # update the info
    info(installInfo(package = package))
    
    shiny::removeModal()
    
  })
  
  
  # launch modal to view details:
  
  # install/update
  shiny::observeEvent(input$View, {
    shiny::showModal(viewPackageModal(session$ns, package))
  })
  
}



installPackageModal <- function(ns, pkg) {
  shiny::modalDialog(
    
    # select the type of covariate to add to the covarite Name
    shiny::h1(paste0('Do you want to install/update package: ',pkg, '?')),
    
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('install'), 'Install/Update')
    )
  )
}


viewPackageModal <- function(ns, pkg) {
  shiny::modalDialog(
    
    shinydashboard::box(title = paste0(pkg,' Dependencies:'), width = '60%',
                        shiny::dataTableOutput(ns('installDeps'))
    ),
    
    footer = shiny::tagList(
      shiny::modalButton("Cancel")
    )
  )
}


getStatus <- function(x){
  if(x$versionInsalled == 'Not installed'){
    return('danger')
  } else if(x$versionInsalled == x$versionAvailable){
    return('success')
  } else{
    return('warning')
  }
}



# HELPERS

getPlpPackageDeps <- function(package = 'PatientLevelPrediction',
                              branch = 'master'){
  descPlp <- read.table(curl::curl(paste0('https://raw.githubusercontent.com/OHDSI/',package,'/',branch,'/DESCRIPTION')),sep = '?')
  descPlp <- sapply(descPlp, FUN = as.character)
  
  if(length(grep('Depends:',descPlp))>0 && length(grep('Imports:',descPlp))>0 && length(grep('Suggests:',descPlp))>0){
    depPk <- descPlp[(grep('Depends:',descPlp) + 1): (grep('Imports:',descPlp)-1)]
    importPk <- descPlp[(grep('Imports:',descPlp)+1):(grep('Suggests:',descPlp)-1)]
    suggestPk <- descPlp[(grep('Suggests:',descPlp)+1):ifelse(length(grep("Remotes:",descPlp))>0, grep("Remotes:",descPlp)-1, max(grep('  ', descPlp)) )]
    
    plpPk <- rbind(data.frame(name = depPk, type = rep('Dependencies', length(depPk))),
                   data.frame(name = importPk, type = rep('Imports', length(importPk))),
                   data.frame(name = suggestPk, type = rep('Suggests', length(suggestPk)))
    )
  }
  
  if(length(grep('Depends:',descPlp))==0 && length(grep('Imports:',descPlp))>0 && length(grep('Suggests:',descPlp))>0){
    importPk <- descPlp[(grep('Imports:',descPlp)+1):(grep('Suggests:',descPlp)-1)]
    suggestPk <- descPlp[(grep('Suggests:',descPlp)+1):ifelse(length(grep("Remotes:",descPlp))>0, grep("Remotes:",descPlp)-1,  max(grep('  ', descPlp)) )]
    
    plpPk <- rbind(data.frame(name = importPk, type = rep('Imports', length(importPk))),
                   data.frame(name = suggestPk, type = rep('Suggests', length(suggestPk)))
    )
  }
  
  plpPk$name <- gsub('\t','', gsub(' ', '', gsub(',','', plpPk$name)))
  
  names <- sapply(plpPk$name, function(x) strsplit(x, '\\(')[[1]][1])
  version <- gsub('\\)','',sapply(plpPk$name, function(x) strsplit(x, '\\(')[[1]][2]))
  
  plpPk$name <- names
  plpPk$version <- version
  
  return(plpPk)
}

getInstalledPk <- function(plpPk){
  plpPk$versionInstalled <- unlist(lapply(1:length(plpPk$name), function(i){tryCatch({as.character(packageVersion(plpPk$name[i]))},
                                                                                     error = function(e){return('Not installed')}
  )}))
  
  rVersion <- R.Version()
  plpPk$versionInstalled[plpPk$name=='R'] <- paste0(rVersion$major, '.', rVersion$minor)
  
  return(plpPk)
}

versionCheck <- function(required, installed){
  if(is.na(required)){
    if(installed != 'Not installed'){
      return('Pass')
    }
  }
  
  if(!is.na(required)){
    
    if(length(grep('>=', required))>0){
      if(installed >= gsub('>=','', required)){
        return('Pass')
      } else{
        return('Fail')
      }
    }
    if(length(grep('>', required))>0){
      if(installed > gsub('>','', required)){
        return('Pass')
      } else{
        return('Fail')
      }
    }
    
  }
  return('Fail')
}


installInfo <- function(package = 'PatientLevelPrediction',
                        branch = 'master'){
  
  descPlp <- read.table(curl::curl(paste0('https://raw.githubusercontent.com/OHDSI/',package,'/',branch,'/DESCRIPTION')),sep = '?')
  descPlp <- sapply(descPlp, FUN = as.character)
  
  mainInfo <- data.frame(package = package,
                         versionAvailable = gsub(' ','', gsub('Version:','',descPlp[grep('Version:', descPlp )])),
                         versionInsalled = tryCatch({as.character(packageVersion(package))},
                                                    error = function(e){return('Not installed')}))
  
  # run to get deps for PLP master
  plpPk <- getPlpPackageDeps(package = package, branch = branch)
  plpPk <- getInstalledPk(plpPk)
  plpPk$check <- apply(plpPk, 1, function(x){versionCheck(x['version'], x['versionInstalled'])})
  
  return(list(main = mainInfo,
              dependencies = plpPk))
}
