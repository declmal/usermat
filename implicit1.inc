c   ... implicit common ...
c
      character*80    sc_filename, moddyn_filename
      character*10    sc_mass, sc_damp, sc_stiff, sc_inert
      common/bki01ch/ sc_filename, sc_mass, sc_damp, sc_stiff, sc_inert,
     &                moddyn_filename
c
      integer imauto,iteopt,lauto,mthsol,ilimit,maxref,icnvrg,
     & igdiv,nwebuf,neql,neqt,imterm,imphas,nbfgs,
     & numupd,istif,itrlas,imerr,imdof,neqtgl,ilsmtd,lsdir,ptr_aspc_h,
     & impcntctl,ipmaxx,igpx,itermp,itermp1,h_lp,h_rd,ptr_rf_data_h,
     & moddyn_flag, moddyn_mdrange, moddyn_mdlen,
     & moddyn_mdfreq, moddyn_mdfrqlen, moddyn_aux
      common/bki01i/imauto,iteopt,lauto,mthsol,ilimit,maxref,icnvrg,
     & igdiv,nwebuf,neql,neqt,imterm,imphas,nbfgs,
     & numupd,istif,itrlas,imerr,imdof,neqtgl,ilsmtd,lsdir,ptr_aspc_h,
     & impcntctl,ipmaxx,igpx,itermp,itermp1,h_lp,h_rd,ptr_rf_data_h,
     & moddyn_flag, moddyn_mdrange, moddyn_mdlen,
     & moddyn_mdfreq, moddyn_mdfrqlen, moddyn_aux(13)
c
      real dtimp,dtimp0,timeim,dtmnim,dtmxim,cvtl,ectl,rctl,
     & tolls,dnorm2,dtprnt,dtplot,dtiter,dtrefm,ppmax,pmaxr,errord,umax,
     & sumx,xrte,moddyn_valdmp
      common/bki01r/dtimp,dtimp0,timeim,dtmnim,dtmxim,cvtl,ectl,rctl,
     & tolls,dnorm2,dtprnt(2),dtplot(2),dtiter(2),dtrefm(2),
     & ppmax,pmaxr,errord,umax,sumx,xrte,moddyn_valdmp
c
      real ascntl
      common/bki02r/ascntl(150)
c
      logical lsensw
      common/bki01l/lsensw(20)
c
      integer imip,isolvr,icwrb
      common/bki02i/imip(150),isolvr(250),icwrb(50)
