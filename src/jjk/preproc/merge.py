#!/usr/cadc/misc/bin/python
"""Given a list of MOPfiles merge them based on x/y coordinates matching.."""

if __name__ == '__main__':
    from optparse import OptionParser
    parser=OptionParser()
    parser.add_option("--outfile",
                      "-o",action="store",
                      help="merged output file name", default='merge.out')
    parser.add_option("--tolerance",default=2,help="Tolerance for x/y matching (radius)")
    
    (opt, filenames)=parser.parse_args()

    xymatch(opt.outfile,filenames,tol=opt.tolerance)

def xymatch(outfile, filenames, tol=2):
    import math
    import MOPfiles
    import sys
    output={}
    files=[]
    for filename in filenames:
        this_file=MOPfiles.read(filename)
        ## match files based on the 'X' and 'Y' column.
        ## if those don't exist then skip this file
        if not this_file['data'].has_key('X') or not this_file['data'].has_key('Y'):
            continue
        if not output.has_key('data'):
            output=this_file
            continue
        delete_list=[]
        for keyword in this_file['header']:
            if not keyword in output['header']:
                output['header'][keyword]=this_file['header'][keyword]
        for col in this_file['data'].keys():
            if not output['data'].has_key(col):
                output['order'].append(col)
                output['data'][col]=[]
                output['order'].append(col)
                output['format'].append(this_file['format'][col])
                ### pad previously values with empties.. 
                for i in range(len(output['data']['X'])):
                    output['data'][col].append(None)
        for i in xrange(len(output['data']['X'])):
            x1=float(output['data']['X'][i])
            y1=float(output['data']['Y'][i])
            matched=False
            for j in xrange(len(this_file['data']['X'])):
                x2=float(this_file['data']['X'][j])
                y2=float(this_file['data']['Y'][j])
                if ( ((x1-x2)**2+(y1-y2)**2) < tol**2):
                    for col in this_file['data'].keys():
                        if output['data'][col][i] is None:
                            output['data'][col][i]=this_file['data'][col][j]
                    matched=True
                    break
            if not matched:
                delete_list.append(i)

        delete_list.sort()
        delete_list.reverse()
        for i in delete_list:
            for col in output['data'].keys():
                del output['data'][col][i]


    MOPfiles.write(outname,output)

