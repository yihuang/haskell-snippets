import os, sys

bufSize = 4096

def main(input, outdir, size):
    fileidx = 0
    with open(input) as fi:
        while True:
            accum = 0
            filename = os.path.join(outdir, str(fileidx))
            fileend = False
            with open(filename, 'w') as fo:
                while accum < size:
                    c = fi.read(bufSize)
                    if not c:
                        fileend = True
                        break
                    fo.write(c)
                    accum += len(c)
            if fileend:
                break
            fileidx += 1

if __name__ == '__main__':
    [_, input, outdir, size] = sys.argv
    size = int(size)
    main(input, outdir, size)
