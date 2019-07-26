library(tesseract)

# Render pdf to png image
pdf.file <- "G:/R/99 PersonalRProjects/Four_2017_Nenana_Data.pdf"
img_file <- pdftools::pdf_convert(pdf.file, format = 'png', pages = 1, dpi = 400)

# Extract text from png image
text <- ocr(img_file)
unlink(img_file)
cat(text)

# https://stackoverflow.com/questions/42468845/ocr-tables-in-r-tesseract-and-pre-pocessing-images
# https://stackoverflow.com/questions/9480013/image-processing-to-improve-tesseract-ocr-accuracy?rq=1

