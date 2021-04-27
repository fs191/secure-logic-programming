#!/usr/bin/env python
import struct
import os
import sys

from subprocess import call

class IncompleteInput(Exception):
    pass

def parseArguments(inFile=sys.stdin, sizeTypeFormat='Q', sizeTypeSize=8,
        encoding='utf-8'):
    def readBlock(size):
        buf = inFile.buffer.read(size) if hasattr(inFile, 'buffer') else inFile.read(size)
        if len(buf) != size:
            raise IncompleteInput()
        return buf

    def readString(size):
        return readBlock(size).decode(encoding)

    def parseData(typeName, data):
        if typeName == 'bool':
            return list(struct.unpack('%s?' % len(data), data))
        elif typeName == 'float32':
            return list(struct.unpack('%sf' % (len(data) // 4), data))
        elif typeName == 'float64':
            return list(struct.unpack('%sd' % (len(data) // 8), data))
        elif typeName == 'int8':
            return list(struct.unpack('%sb' % len(data), data))
        elif typeName == 'int16':
            return list(struct.unpack('%sh' % (len(data) // 2), data))
        elif typeName == 'int32':
            return list(struct.unpack('%si' % (len(data) // 4), data))
        elif typeName == 'int64':
            return list(struct.unpack('%sq' % (len(data) // 8), data))
        elif typeName == 'uint8':
            return list(struct.unpack('%sB' % len(data), data))
        elif typeName == 'uint16':
            return list(struct.unpack('%sH' % (len(data) // 2), data))
        elif typeName == 'uint32':
            return list(struct.unpack('%sI' % (len(data) // 4), data))
        elif typeName == 'uint64':
            return list(struct.unpack('%sQ' % (len(data) // 8), data))
        elif typeName == 'string':
            return data.decode(encoding)
        else:
            return data

    args = {}

    try:
        while True:
            argNameSize = struct.unpack(sizeTypeFormat, readBlock(sizeTypeSize))[0]
            argName = readString(argNameSize)
            pdNameSize = struct.unpack(sizeTypeFormat, readBlock(sizeTypeSize))[0]
            pdName = readString(pdNameSize)
            typeNameSize = struct.unpack(sizeTypeFormat, readBlock(sizeTypeSize))[0]
            typeName = readString(typeNameSize)
            argDataSize = struct.unpack(sizeTypeFormat, readBlock(sizeTypeSize))[0]
            argData = parseData(typeName, readBlock(argDataSize))
            args[argName] = argData
    except IncompleteInput:
        pass

    return args

controllercfg = '--conf=client.conf'
runscript     = './sharemind-runscript'
logfile = '_log.txt'
outfile = '_output.txt'

outfilearg = "--outFile=" + outfile

kw_yesno  = 'does'
kw_yesno_len = len(kw_yesno)

kw_var  = 'var'
kw_len  = 'len'
kw_val  = 'val'

# this can be used for testing
def testPrint(res):
    for solution in res:
        s = "["
        solution=sorted(solution, key=lambda x: x[0])
        for (i,var,val) in solution:
             s = s + val + ("" if i == solution[-1][0] else ",")
        print(s + ']')

# this can be shown to the user
def nicePrint(res):
    m = len(res)
    j = 0
    for solution in res:
        j = j + 1
        solution=sorted(solution, key=lambda x: x[0])
        for (i,var,val) in solution:
             print(var + '=' + val + ("" if i == solution[-1][0] else ", ")),
        print((";" if j < m else "."))

#formatting
def format(result):
    res = []
    aggr = []
    for argname in result:
         if len(argname) >= kw_yesno_len and argname[:kw_yesno_len] == kw_yesno:

             if result[argname][0]:
                 print("true")
             else:
                 print("false")
                 sys.exit(0)
         else:
             index = int(argname[3])
             vtype = argname[:3]
             vvar = kw_var + str(index)
             vlen = kw_len + str(index)
             vval = kw_val + str(index)
             name  = result[vvar]
             value = result[vval]
             if vtype == kw_var:
                 if (vlen) in result:
                     n = result[vlen][0]
                     #print(result[vlen])
                     #print(value)
                     #print("m * n: " + str(len(value)))
                     #print("n: " + str(n))
                     #print("m: " + str(len(value) // n))
                     value = np.reshape(list(value), (len(value) // n, n))
                     value = map(lambda x: "".join(map(lambda z : z if isinstance(z,str) else '' if z == 0 else chr(z), x)), value)

                 if len(value) > 1:
                     res.append(map(lambda x : (index,name,str(x)), value))
                 else:
                     aggr.append(map(lambda x : (index,name,str(x)), value))

    if not (res == []): res = np.transpose(res,(1,0,2))
    return list(aggr) + list(res)


# TODO the type and domain should come from the program, and this script generated automatically
# python run.py market True X1=alice:private:string X2=garlic:public:string
n = len(sys.argv)
filename = sys.argv[1]
if n <= 2:
    niceprint = True
    k = 2
elif sys.argv[2] == '1':
    niceprint = True
    k = 3
elif sys.argv[2] == '0':
    niceprint = False
    k = 3
else:
    niceprint = True
    k = 2

s = [runscript, outfilearg, controllercfg, filename + ".sb"]

args = {}
for i in range(k,n):
    [var,decl]   = sys.argv[i].split("=")
    [value,vdom,vtype] = decl.split(":")
    args[var] = value

    if (vdom=="private"):
        vdom="pd_shared3p"

    vsize="0"
    if (vtype[-1:]=="8"):  vsize = "1"
    if (vtype[-2:]=="16"): vsize = "2"
    if (vtype[-2:]=="32"): vsize = "4"
    if (vtype[-2:]=="64"): vsize = "8"

    if (vtype=="string"):
        if (vdom=="pd_shared3p"):

            #TODO use this code after updating sharemind-runscript
            #s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(value)), "--xstr=" + ("".join("{:02x}".format(ord(c)) for c in value))]

            #this is a workaround
            xstr = map(lambda x : (x / 15) * 16 + x % 15, (ord(c) for c in value))
            s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(xstr)), "--xstr=" + ("".join("{:02x}".format(c) for c in xstr))]

    elif (vtype=="bool"):

            #TODO use this code after updating sharemind-emulator
            #s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(value)), "--xstr=" + ("".join("{:02x}".format(int(c)) for c in value))]

            #this is a workaround
            xstr = map(lambda x : (x / 15) * 16 + x % 15, (int(c) for c in value))
            s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(xstr)), "--xstr=" + ("".join("{:02x}".format(c) for c in xstr))]

        else:
            s = s + ["--str=" + var, "--str=", "--str=" + vtype, "--size=" + str(len(value)), "--cstr=" + value]

    #currently, anything that is not a string is integer
    else:
        s = s + ["--str=" + var, "--str=" + vdom, "--str=" + vtype, "--size=" + vsize, "--" + vtype + "=" + value + ""]

    #TODO add float arguments, we can pass them as xstr componentwise

#there is a special argument "responses" which requires a dummy input if not specified otherwise
if not "responses" in args:
    s = s + ["--str=responses", "--str=pd_shared3p", "--str=xor_uint8", "--size=1", "--xstr=00"]

#print(s)
#print(' '.join(s))
if os.path.exists(outfile):
  os.remove(outfile)

fout = open(logfile, "w")
os.environ["LD_LIBRARY_PATH"] = "../lib"
result = call(s, stdout=fout)
fout.close()

fin = open(outfile, "r")
result = parseArguments(fin)
fin.close()

#print(result)

if niceprint:
    nicePrint(format(result))
else:
    testPrint(format(result))
