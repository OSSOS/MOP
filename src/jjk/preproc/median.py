# Provide the median value of a masked numpy array

def median(ma):
    """ do it row by row, to save memory...."""
    _median = 0*ma[0].filled(fill_value=0)
    for i in range(ma.shape[-1]):
	t=xmedian(ma[:,:,i])
    	_median[:,i]=t
	t=None
    return _median

def xmedian(ma):
    """ Given a masked numpy array (build using numpy.ma class) return
    the median value of the array."""
    import numpy
    _medianIndex = numpy.floor(ma.count(axis=0)/2.0)
    _sortIndex = ma.argsort(kind='heapsort',axis=0)
    _median = ma[0].filled(fill_value=0)*0
    for idx in range(len(_sortIndex)):
        _median = _median+_sortIndex[idx]*(_medianIndex==idx)
    _medianIndex=_median
    _median=0*_median
    for idx in range(len(ma)):
        _median = _median + ma[idx].filled(fill_value=0)*(_medianIndex==idx)
    _sortIndex=None
    _medianIndex=None
    return _median

if __name__ == '__main__':
    ## Do a unit test
    import numpy
    # build a 10x10x10 masked array
    nFrames = 10
    nx = 2400
    ny = 4096
    d = numpy.random.random(nFrames*nx*ny).reshape((nFrames,nx,ny))*10
    m = numpy.random.random(nFrames*nx*ny).reshape((nFrames,nx,ny))<0.3
    ma = numpy.ma.masked_array(d,m)
    print median(ma)
