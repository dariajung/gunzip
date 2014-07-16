gunzip
======

An implementation of gunzip/gzip -d in Haskell. 

Notes 
----

Byte Flags:

```
bit 0 set: file probably ascii text
bit 1 set: continuation of multi-part gzip file
bit 2 set: extra field present
bit 3 set: original file name present
bit 4 set: file comment present
bit 5 set: file is encrypted
bit 6,7:   reserved
```

Operating System Codes:

```
0 - FAT file system (DOS, OS/2, NT) + PKZIPW 2.50 VFAT, NTFS
1 - Amiga
2 - VMS (VAX or Alpha AXP)
3 - Unix
4 - VM/CMS
5 - Atari
6 - HPFS file system (OS/2, NT 3.x)
7 - Macintosh
8 - Z-System
9 - CP/M
10 - TOPS-20
11 - NTFS file system (NT)
12 - SMS/QDOS
13 - Acorn RISC OS
14 - VFAT file system (Win95, NT)
15 - MVS (code also taken for PRIMOS)
16 - BeOS (BeBox or PowerMac)
17 - Tandem/NSK
18 - THEOS
```


Resources 
----

- [Algorithm specifications](http://www.gzip.org/algorithm.txt)
- [Format information](http://www.gzip.org/format.txt)
- [Gunzip in Julia by Julia Evans](https://github.com/jvns/gzip.jl/blob/master/gzip.jl)
- [Dissecting the GZip format](http://www.infinitepartitions.com/art001.html)
