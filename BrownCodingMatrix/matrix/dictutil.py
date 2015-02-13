# Copyright 2013 Philip N. Klein
def dict2list(dct, keylist): return [dct[key] for key in keylist]

def list2dict(L, keylist): return {keylist[k]:L[k] for k in range(0,len(L))}
