#file operation
CreatePath <- function(p=kPath) {
  if (!file.exists(p))      
    dir.create(p)
}

WriteTable <- function(data,file.name) {
  path <- file.path(kPath, file.name) 
  write.table(data, path, sep="\t", row.names=FALSE,append = FALSE)  
}

ReadTable <- function(file.name) {
  path <- file.path(kPath, file.name) 
  table <- read.table(path, header=TRUE)
  return(table)
}

RenderPDF <- function(input.file, output.dir=kPath, output.filename=NULL) {
  rmarkdown::render(input.file, output_dir=output.dir, output_file=output.filename)
}
