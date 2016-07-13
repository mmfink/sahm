#  -*- coding: latin-1 -*-
###############################################################################
# This file is part of the Software for Assisted Habitat Modeling (SAHM) package
# developed by the U.S. Geological Survey Fort Collins Science Center.
# It is intended to be used in the VisTrails Scientific
# VisTrails was developed by New York University (2014-2016), NYU-Poly (2011-2014),
# University of Utah (2006-2011).  VisTrails Contact: contact@vistrails.org
#
# SAHM Contact: talbertc@usgs.gov
#
# --------------------------------------------------------------------------------
# U.S. Geological Survey Disclaimers
# Any use of trade, product or firm names is for descriptive purposes only and does
# not imply endorsement by the U.S. Geological Survey.
#
# Although this information product, for the most part, is in the public domain,
# it also contains copyrighted material as noted in the text. Permission to reproduce
# copyrighted items for other than personal use must be secured from the copyright owner.
#
# Although these data have been processed successfully on a computer system at the
# U.S. Geological Survey, no warranty, expressed or implied is made regarding the
# display or utility of the data on any other system, or for general or scientific
# purposes, nor shall the act of distribution constitute any such warranty. The
# U.S. Geological Survey shall not be held liable for improper or incorrect use
# of the data described and/or contained herein.
#
# Although this program has been used by the U.S. Geological Survey (USGS), no
# warranty, expressed or implied, is made by the USGS or the U.S. Government as
# to the accuracy and functioning of the program and related program material nor
# shall the fact of distribution constitute any such warranty, and no responsibility
# is assumed by the USGS in connection therewith.
# --------------------------------------------------------------------------------
#
# This code is in the public domain and is licensed under Creative Commons CC0 1.0 Universal
#
###############################################################################

import sys
import csv
import os
import time
import random
import shutil

import Queue
import thread
import subprocess

from osgeo import gdalconst
from osgeo import gdal

from optparse import OptionParser

import utilities
from utilities import isMDSFile

class FormatConverter(object):
    def __init__(self):
        #instance level variables
        self.verbose = False
        self.MDSFile = ''
        self.inputDir = ''
        self.outputDir = ''
        self.format = 'asc'
        self.logger = None
        self.multicore = True
        self.driverExt = {'asc':'AAIGrid', 'bil':'EHdr', 'img':'HFA', 'jpg':'JPEG', 'rst':'RST'}
        
    def run(self):
        self.validateArgs()
        if self.MDSFile <> '':
            usedTifs = self.extractFileNames()
        else:
            usedTifs = self.fileNamesFromFolder()
        #self.writetolog('    Converting: ' + ','.join(usedTifs))
        if self.multicore:
            self.convertEnvironmentalLayers_mc(usedTifs, self.outputDir, self.format)
        else:
            self.convertEnvironmentalLayers(usedTifs, self.outputDir, self.format)
        
    def validateArgs(self):
        argProblem = ""
        
        if self.logger is None:
            self.logger = utilities.logger(self.outputDir, self.verbose)
        self.writetolog = self.logger.writetolog
        
        if os.path.isdir(self.inputDir):
            pass
        elif os.path.exists(self.MDSFile):
            if not isMDSFile(self.MDSFile):
                argProblem += "The supplied MDS file, " + self.MDSFile + ", does not appear to be in the appropriate format."
        else:
            argProblem += "Neither an input Directory or MDS File was supplied."       
        
        if not os.path.isdir(self.outputDir):
            try:
                os.mkdir(self.outputDir)
            except:
                argProblem += 'The supplied output directory, ' + self.outputDir + ", does not exist and could not be created."
        if not self.format.lower() in self.driverExt.keys():
            argProblem += "The supplied format must be one of " + ", ".join(self.driverExt.keys()) 

        if argProblem:
            raise utilities.TrappedError("There was a problem with one or more of the inputs to RasterFormatConverter")
        
    def extractFileNames(self):
        #Read through the MDS and pull the headers
        MDSreader = csv.reader(open(self.MDSFile, 'r'))
        header1 = MDSreader.next()
        header2 = MDSreader.next()
        header3 = MDSreader.next()
        
        usedTifs = []
        for i in range(3, len(header1)):
            if header2[i] == '1' and  header1[i] not in ['Weights', 'Split', 'EvalSplit']:
                usedTifs.append(header3[i])
        return usedTifs
    
    def fileNamesFromFolder(self):
        usedRasters = []
        items = os.listdir(self.inputDir)
        for item in items:
#            print item
            fullPath = os.path.join(self.inputDir, item)
            try:
                if os.path.isdir(fullPath) or \
                    os.path.splitext(item)[1].lower() in \
                    ['.bil', '.img', '.tif', '.jpg', '.bmp', '.asc']:
                    inds = None
                    inds = gdal.Open(fullPath, gdalconst.GA_ReadOnly)
                    if inds is not None:
                        usedRasters.append(fullPath)
            except:
                pass
        return usedRasters

    def convertEnvironmentalLayers(self, files, outputFolder, type):
        
        i = 1
        for f in files:
            f_name = os.path.splitext(os.path.split(f)[1])[0]
            self.writetolog('    Starting on ' + f_name)
            outputfile = os.path.join(outputFolder, f_name + '.' + type)
            self.convertFormat(f, outputfile, self.driverExt[type]) 
            if self.verbose:
                self.writetolog('   Finished converting ' + f_name + '    ' + str(i) + ' out of ' + str(len(files)) + ' finished.')
            i += 1
           
    def convertEnvironmentalLayers_mc(self, files, outputFolder, type):
        '''This function has the same functionality as convertEnvironmentalLayers
        with the addition of utilizing multiple cores to do the processing.
        '''
        results = Queue.Queue()
        process_count= 0
        
        i = 1
        for f in files:
            f_name = os.path.splitext(os.path.split(f)[1])[0]
            # args = '-i ' + '"' + f + '"'
            # args += ' -o '  + '"' + outputFolder + '"'
            # args += ' -f ' + type
            args = ['-i', f,
                    '-o', outputFolder,
                    '-f', type]
#            if self.verbose:
#                args += " -v"
            
            execDir = os.path.split(__file__)[0]
            executable = os.path.join(execDir, 'singleRasterFormatConverter.py')
            
            command_arr = [sys.executable, executable] + args
            self.logger.writetolog(' '.join(command_arr), False, False)
#            proc = subprocess.Popen( command_arr )
#            thread.start_new_thread(utilities.process_waiter,
#                    (proc, f_name, results))
            
            utilities.add_process_to_pool(utilities.launch_cmd, 
                                    [command_arr])
            
            
        utilities.wait_for_pool_to_finish()
#        while process_count > 0:
#            description, rc= results.get()
#            
#            if rc == 0:
#                if self.verbose:
#                    msg = "    " + description + " finished successfully:  " + \
#                        str(len(files) - process_count + 1)  + " done out of " \
#                        + str(len(files))
#                    self.logger.writetolog(msg, True, True)
#            else:
#                self.logger.writetolog("There was a problem with: " + description , True, True)
#            process_count-= 1
        
        
    def convertFormat(self, file, outfile, driver):
        inds = gdal.Open(file, gdalconst.GA_ReadOnly)
       
        #ascii = "AAIGrid"
        #bil = "EHdr"
        Driver = gdal.GetDriverByName(driver)
        Driver.CreateCopy(outfile, inds, 0)

def main(argv):
    usageStmt = "usage:  options: -m --MDSFile -o --outputDir -f --format -v --verbose"
    desc = "Converts all of the tif files specified in an MDS to ASCII format (or optionally other formats)"
    parser = OptionParser(usage=usageStmt, description=desc)
    
    parser.add_option("-v", 
                      dest="verbose", 
                      default=False, 
                      action="store_true", 
                      help="the verbose flag causes diagnostic output to print.")
    parser.add_option("-m", "--MDSFile", 
                      dest="MDSFile", 
                      help="The input MDS used to determine tif inputs.")           
    parser.add_option("-o", "--outputDir", 
                      dest="outputDir", 
                      help="Output directory to save files in.")
    parser.add_option("-f", "--format", 
                      dest="format",
                      default='asc', 
                      help="The format to convert into. 'bil', 'img', 'tif', 'jpg', 'bmp', 'asc'")
    parser.add_option("-s", dest="multicore", 
                      default=True, 
                      action="store_true", 
                      help="the use single-core flag causes each iteration to run on the same thread.") 
    
    (options, args) = parser.parse_args(argv)
    
    ourFC = FormatConverter()
    ourFC.verbose = options.verbose
    ourFC.MDSFile = options.MDSFile
    ourFC.outputDir = options.outputDir
    ourFC.format = options.format
    ourFC.multicore = options.multicore
    ourFC.run()

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))