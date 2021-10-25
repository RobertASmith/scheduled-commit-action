# Automated Rmarkdown creation & online publication...
rmarkdown::render(output_dir = "plots", 
                  output_format = "pdf_document",
                  input = './R/example.Rmd')

rmarkdown::render(output_dir = "plots", 
                  output_format = "html_document",
                  input = './R/example.Rmd')

markdown::rpubsUpload(id = "https://api.rpubs.com/api/v1/document/826791/13f0b53ed9244b559e2fd7902da114fd",
                      title='example', 
                      htmlFile="R/example.html",
                      method=getOption('rpubs.upload.method','auto'))
                      
