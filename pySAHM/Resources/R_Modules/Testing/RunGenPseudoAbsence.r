
input.file="J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\FDAW_1.csv"

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95AdHocIsoplethKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=FALSE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95MCP.tif"
PseudoAbsGen(input.file,output.dir,method="MCP",bw.otim="adhoc",isopleth=95,bias=FALSE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\AdHocContinKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="adhoc",isopleth=95,bias=TRUE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95HpiKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="Hpi",isopleth=95,bias=FALSE)

output.dir<-"J:\\Projects\\Climate_RS_Comparison\\Cheatgrass_VisTrails\\BackgroundPtSurfaces\\95HpiContKDE.tif"
PseudoAbsGen(input.file,output.dir,method="KDE",bw.otim="Hpi",isopleth=95,bias=TRUE)
#PseudoAbsGen(input.file,output.file,response.col="ResponseBinary",method="Density",isopleth=95)

input.file="C:\\temp\\TestDataSets\\TestTrainingSplit_8.csv"

SetWeights(input.file,output.file,response.col="ResponseBinary",method="PresAbs")
I:\VisTrails\Central_VisTrailsInstall\Central_R\R-2.12.1\bin\i386\Rterm.exe --vanilla -f I:\VisTrails\Central_VisTrailsInstall_debug\vistrails\packages\sahm_MarianDev\pySAHM\Resources\R_Modules\SetWeights.r --args  i="C:/VisTrails/mtalbert_20110504T132851/readMaTests/Split.csv" o="C:\temp\SAHMDebugJunk\BRTOut1\out.csv" rc=responseBinary met="Density"