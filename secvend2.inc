c
c     input/output format for keyword file
c
      character*12 kfmt10i8,kfmt8i10,kfmt8e10,kfmt5e16,
     . kfmti10e10,kfmt10xe10,kfmti10a
      character*12 ofmt10i8,ofmt8i10,ofmt8e10,ofmt5e16
      character*28 ofmti10e10,ofmte10i10,ofmta10i10,ofmta10e10
      character*12 ofmti10a
      common/secvend2/
     . kfmt10i8,kfmt8i10,kfmt8e10,kfmt5e16,kfmti10e10,kfmt10xe10,
     . kfmti10a,
     . ofmt10i8,ofmt8i10,ofmt8e10,ofmt5e16,
     . ofmti10e10,ofmte10i10,ofmta10i10,ofmta10e10,
     . ofmti10a
c
c     input       mlarg           longkey
c     kfmt10i8    (10i8)          (8i20)
c     kfmt8i10    (8i10)          (8i20)
c     kfmt8e10    (8e10.0)        (8e20.0)
c     kfmt5e16    (5e16.0)        (8e20.0)
c     kfmti10e10  (i10,7e10.0)    (i20,7e20.0)
c     kfmt10xe10  (10x,7e10.0)    (10x,7e20.0)
c     kfmti10a    (i10,a)         (i20,a)
c
c     output      mlarg           long   
c     ofmt10i8    (10i8)          (10i20)
c     ofmt8i10    (8i10)          (8i20)
c     ofmt8e10    (1p,8e10.3)     (1p,8e20.13)
c     ofmt5e16    (1p,5e16.9)     (1p,5e20.13)
c     ofmti10e10  (1p,i10,7e10.3) (1p,i20,7e20.13)
c     ofmte10i10  (1p,e10.3,7i10) (1p,e20.13,7i20)
c     ofmta10i10  (a10,7i10)      (a20,7e20.13)
c     ofmta10e10  (1p,a10,7e10.3) (1p,a20,7e20.13)
c     ofmti10a    (i10,a)         (i20,a)
c
c     output format for structural file
c
      character*12 sfmt10i5,sfmt10i8,sfmt8i10,sfmt8e10,sfmt5e16,
     . sfmti10a,sfmta2i8a
      character*28 sfmti10e10,sfmta10e10,sfmta10i10
      common/secvend3/sfmt10i5,sfmt10i8,sfmt8i10,sfmt8e10,sfmt5e16,
     . sfmti10e10,sfmta10e10,sfmta10i10,sfmti10a,sfmta2i8a
c
c     output      mlarg           long          
c     sfmt10i5    (10i5)          (10i20)     
c     sfmt10i8    (10i8)          (10i8)     
c     sfmt8i10    (8i10)          (8i20)     
c     sfmt8e10    (1p,8e10.3)     (1p,8e20.13)     
c     sfmt5e16    (1p,5e16.9)     (1p,8e20.13)     
c     sfmti10e10  (1p,i10,7e10.3) (1p,i20,7e20.13) 
c     sfmta10e10  (1p,a10,7e10.3) (1p,a20,7e20.13) 
c     sfmti10a    (i10,a)         (i20,a)
c     sfmta2i8a   (2x,i8,a)       (2x,i18,a)
c
