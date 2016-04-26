# Written by MerseyViking (mersey dot viking at gmail dot com), 2011.
# Released into the public domain - May 8, 2011
# I accept no responsibility for any errors or loss of data, revenue, or life this script may cause. Use at your own risk.
#
# Edited by Tina Cormier 3/24/16 to generate just the tfw.
import osgeo.gdal as gdal
import osgeo.osr as osr
import os
import glob
import sys

def generate_tfw(path):
    for infile in glob.glob(os.path.join(path, '*.tif')):
        src = gdal.Open(infile)
        xform = src.GetGeoTransform()

        # if gen_prj == 'prj':
        #     src_srs = osr.SpatialReference()
        #     src_srs.ImportFromWkt(src.GetProjection())
        #     src_srs.MorphToESRI()
        #     src_wkt = src_srs.ExportToWkt()

        #     prj = open(os.path.splitext(infile)[0] + '.prj', 'wt')
        #     prj.write(src_wkt)
        #     prj.close()

        src = None
        edit1=xform[0]+xform[1]/2
        edit2=xform[3]+xform[5]/2

        tfw = open(os.path.splitext(infile)[0] + '.tfw', 'wt')
        tfw.write("%0.8f\n" % xform[1])
        tfw.write("%0.8f\n" % xform[2])
        tfw.write("%0.8f\n" % xform[4])
        tfw.write("%0.8f\n" % xform[5])
        tfw.write("%0.8f\n" % edit1)
        tfw.write("%0.8f\n" % edit2)
        tfw.close()

if __name__ == '__main__':
    generate_tfw(sys.argv[1])