      logical large,mlarg,longs,lsdain,bigids,bigidb
      common/bigprb/large,mlarg,longs,lsdain,bigids,bigidb
c     lsdain -replace structure input with lsda database
c     longs  -long structure input
c     bigids -use bigid format
c     bigidb -have big id > 2147483647
c
      character*12 fmt16i85,fmt10i85,fmt16i5,fmt10i8,fmt8i10,fmt8e10,
     . fmt5e16,fmti10e10,fmt10xe10,fmt2xi8a,fmt4xi10
      common/inpfmt/fmt16i85,fmt10i85,fmt16i5,fmt10i8,fmt8i10,fmt8e10,
     . fmt5e16,fmti10e10,fmt10xe10,fmt2xi8a,fmt4xi10
c
c                 longs         large        88 format
c     fmt16i85    (10i20)       (10i8)       (16i5)
c     fmt10i85    (10i20)       (10i8)       (10i5)
c     fmt16i5     (8i20/8i20)   (16i5)       (16i5)
c     fmt10i8     (8i20)        (10i8)       (10i8)
c     fmt8i10     (4i20)        (8i10)       (8i10)
c     fmt8e10     (4e20.0)      (8e10.0)     (8e10.0)
c     fmt5e16     (4e20.0)      (5e16.0)     (8e10.0)
c     fmti10e10   (i20,7e20.0)  (i10,7e10.0) (i10,7e10.0)
c     fmt10xe10   (20x,7e20.0)  (10x,7e10.0) (10x,7e10.0)
c     fmt2xi8a    (2x,i18,a)    (2x,i8,a)    (2x,i8,a)
c     fmt4xi10    (4x,i20)      (4x,i10)     (4x,i10)
c
c     output format for structural file
c
      character*12 sfmt16i85,sfmt10i85,sfmt16i5,sfmt10i8,sfmt8i10,
     .  sfmt8e10,sfmt5e16
      character*28 sfmti10e10,sfmta10e10
      common/outfmt/sfmt16i85,sfmt10i85,sfmt16i5,sfmt10i8,sfmt8i10,
     . sfmt8e10,sfmti10e10,sfmta10e10,sfmt5e16
c
c     output      mlarge        mlarge           88 format
c     sfmt16i85   (10i20)       (10i8)           (16i5)
c     sfmt10i85   (10i20)       (10i8)           (10i5)
c     sfmt16i5    (8i20/8i20)   (16i5)           (16i5)
c     sfmt10i8    (8e20.13)     (10i8)           (10i8)
c     sfmt8i10    (8e20.13)     (8i10)           (8i10)
c     sfmt8e10    (8e20.13)     (8e10.3)         (8e10.3)
c     sfmt5e16    (8e20.13)     (5e16.9)         (5e16.9)
c     sfmti10e10  (i20,7e20.13) (i10,7e10.3)     (i10,7e10.3)
c     sfmta10e10  (a20,7e20.13) (a10,7e10.3)     (a10,7e10.3)
c
      integer   nbigp,nbigp1
c     nbigp - number of integer words for big IDs
c     nbigp1- nbigp-1
      common/bigid8/nbigp,nbigp1
