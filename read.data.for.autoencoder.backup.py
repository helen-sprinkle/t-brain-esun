import scipy as sc
import numpy as np

x=sc.sparse('behavior/behavior_9453.csv')
np.sum(x.todense())
x.todense().nbytes
a = x.todense()
np.expand_dims (a, axis =1)
a.astype(np.int16).nbytes
