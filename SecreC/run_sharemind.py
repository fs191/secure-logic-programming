#!/usr/bin/env python
import struct
import os
import sys
import numpy as np
import argparse

from subprocess import call

scriptpath = './'
filepath   = './examples/'
emulator_path  = '/usr/local/sharemind/bin/sharemind-emulator'
runscript_path = '/usr/local/sharemind/bin/sharemind-runscript'

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
                     value = np.reshape(list(value), (len(value) // n, n))
                     value = map(lambda x: "".join(map(lambda z : z if isinstance(z,str) else '' if z == 0 else chr(z), x)), value)

                 if len(value) > 1:
                     res.append(map(lambda x : (index,name,str(x)), value))
                 else:
                     aggr.append(map(lambda x : (index,name,str(x)), value))

    if not (res == []): res = np.transpose(res,(1,0,2))
    return list(aggr) + list(res)

def interaction(qrfilename):
    f = open(qrfilename, 'r')
    responses = []
    for line in f:
        print(line)
        response = raw_input()
        responses.append('1' if response == 'y' or response =='Y' else '0')
    f.close()
    return responses

logfile = '_log.txt'
outfile = '_output.txt'

outfilearg = "--outFile=" + scriptpath + outfile

kw_yesno  = 'does'
kw_yesno_len = len(kw_yesno)

kw_var  = 'var'
kw_len  = 'len'
kw_val  = 'val'

# python run.py market 0 X1=alice:private:string X2=garlic:public:string
def run():

    parser = argparse.ArgumentParser(description='Run SecreC script')
    parser.add_argument('inputs', metavar='X', nargs='*',
                        help='user input of the form \'VARIABLE=VALUE:DOMAIN:TYPE\'')
    parser.add_argument('--sb',   dest='sbfilename')
    parser.add_argument('--qr',   dest='qrfilename')
    parser.add_argument('--nice', dest='niceprint', action='store_const',
                        const=True, default=False,
                        help='swipl-style output format')
    parser.add_argument('--real',   dest='runscript', action='store_const',
                        const=runscript_path, default=emulator_path,
                        help='run the program on real Sharemind servers (default: emulator)')

    sysargs = parser.parse_args()

    niceprint = sysargs.niceprint
    runscript = sysargs.runscript
    qrfilename = sysargs.qrfilename
    sbfilename = sysargs.sbfilename
    inputs = sysargs.inputs

    s = [runscript, outfilearg, filepath + sbfilename]

    if not qrfilename == None:
        responses = interaction(qrfilename)
        inputs.append("responses=" + "".join(responses) + ":private:bool")

    args = {}
    for i in range(0,len(inputs)):
        [var,decl]         = inputs[i].split("=")
        [value,vdom,vtype] = decl.split(":")
        args[var]          = value

        if (vdom=="private"):
            vdom="pd_shared3p"
        else:
            vdom=""

        vsize="0"
        if (vtype[-1:]=="8"):  vsize = "1"
        if (vtype[-2:]=="16"): vsize = "2"
        if (vtype[-2:]=="32"): vsize = "4"
        if (vtype[-2:]=="64"): vsize = "8"

        if (vtype=="string"):
            if (vdom=="pd_shared3p"):

                #TODO use this code after updating sharemind-emulator
                #s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(value)), "--xstr=" + ("".join("{:02x}".format(ord(c)) for c in value))]

                #this is a workaround
                xstr = map(lambda x : (x / 15) * 16 + x % 15, (ord(c) for c in value))
                s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(xstr)), "--xstr=" + ("".join("{:02x}".format(c) for c in xstr))]

            else:
                s = s + ["--str=" + var, "--str=", "--str=" + vtype, "--size=" + str(len(value)), "--cstr=" + value]


        elif (vtype=="bool"):

            #TODO use this code after updating sharemind-emulator
            #s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(value)), "--xstr=" + ("".join("{:02x}".format(int(c)) for c in value))]

            #this is a workaround
            xstr = map(lambda x : (x / 15) * 16 + x % 15, (int(c) for c in value))
            s = s + ["--str=" + var, "--str=pd_shared3p", "--str=xor_uint8", "--size=" + str(len(xstr)), "--xstr=" + ("".join("{:02x}".format(c) for c in xstr))]


        #currently, anything else is integer
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
    result = call(s, stdout=fout)
    fout.close()

    fin = open(outfile, "r")
    result = parseArguments(fin)
    fin.close()

    if niceprint:
        nicePrint(format(result))
    else:
        testPrint(format(result))

def main():
    run()

if __name__ == "__main__":
    main()

