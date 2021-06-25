c
c     parameters for lsmsg messages
c
      integer MSG_KEY,MSG_STR,MSG_INI,MSG_SOL,MSG_MPP,MSG_IMP,MSG_OTH,
     . MSG_CESE,MSG_EM,MSG_ICFD,MSG_STOC,MSG_CHEM,MSG_ATMC,MSG_EOS,
     . MSG_MESH,MSG_MSC,MSG_TOT
c
      parameter (MSG_KEY=0)
      parameter (MSG_STR=2000)
      parameter (MSG_INI=4000)
      parameter (MSG_SOL=5000)
      parameter (MSG_MPP=7000)
      parameter (MSG_IMP=8000)
      parameter (MSG_OTH=9000)
      parameter (MSG_CESE=11000)
      parameter (MSG_EM=11500)
      parameter (MSG_ICFD=12000)
      parameter (MSG_MESH=12450)
      parameter (MSG_STOC=12500)
      parameter (MSG_CHEM=12600)
      parameter (MSG_ATMC=13000)
      parameter (MSG_EOS=13200)
      parameter (MSG_MSC=13500)
      parameter (MSG_TOT=16000)
c
c     This include file/common block is used to hold the currently open
c     unit numbers for commonly used files.
c
c     iohsp  = d3hsp file      (formerly just '13')
c     ionull = /dev/null       (currently 173)
c     iomsg  = messag          (currently 59)
c     iotty  = terminal output (currently 6)
c     ioinp  = terminal input  (currently 5)
c     ioall  = iohsp+iomsg+iotty (=-1)
c     ioinf  = iohsp+iomsg       (=-2)
c     ioecho = echo file       (currently 20)
c
      integer iohsp,ionull,iomsg,iotty,ioinp,ioall,ioinf,ioecho
      common/iounits/iohsp,ionull,iomsg,iotty,ioinp,ioall,ioinf,ioecho
c
      integer*8 ierdat
      real rerdat
      character*80 cerdat
      common/lsmsg1/ierdat(24),rerdat(24),cerdat(24)

