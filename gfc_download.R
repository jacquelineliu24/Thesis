neededPackages = c("rgdal","devtools","snow")


library(gfcanalysis)
library(rgdal)
library(devtools)

output_folder <- "download/input/gfc"

aoi <- readOGR('download/input/shapes', 'ZOI_NAK_2012_EEsimple')

tiles <- calc_gfc_tiles(aoi)
print(length(tiles))

plot(tiles)
plot(aoi, add=TRUE, lty=2, col="#00ff0050")

download_tiles(tiles, output_folder, images = c('treecover2000', 'gain', 'lossyear', 
                                                'datamask'), data_year = 2017)

gfc_extract <- extract_gfc(aoi, output_folder, filename="NAK_GFC_extract.tif")





gfc_thresholded <- threshold_gfc(gfc_extract, forest_threshold=25, 
                                 filename="NAK_GFC_extract_thresholded.tif")

gfc_annual_stack <- annual_stack(gfc_thresholded)


writeRaster(gfc_annual_stack, filename="NAK_GFC_extract_thresholded_annual.tif")

gfc_stats <- gfc_stats(aoi, gfc_extract)