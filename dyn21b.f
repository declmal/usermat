c
c $Id: dyn21b.F 105368 2016-02-03 12:58:52Z thomask $
c
      subroutine usrshl(rule,ixp,x,rhs,rhr,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,ies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,ibqshl,iqtype,bkqs,gmi,ihgenf,hgener,
     3 lft,llt,rhssav,eig,eign,qextra,nmtcon,ithxpid,ietyp,cmusr,
     4 lenvec8,xipn,drlstr,rhsl,loceps,epsint,eosp,isdrill,rots)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     main subroutine for the user defined shell
c
      include 'nlqparm'
      include 'implicit1.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
      include 'memaia.inc'
      include 'nhisparm.inc'
      include 'shlioc.inc'
      common/aux00loc/
     & ds11(nlq),ds12(nlq),ds13(nlq),ds22(nlq),ds23(nlq),
     & ds33(nlq),sjunk(nlq,4),
     & str33(nlq),enginc(nlq)
      common/aux01loc/
     &ft11(nlq),ft12(nlq),ft13(nlq),ft21(nlq),ft22(nlq),ft23(nlq),
     &fm11(nlq),fm12(nlq),fm21(nlq),fm22(nlq),
     &fm31(nlq),fm32(nlq),fm41(nlq),fm42(nlq),
     &fmr11(nlq),fmr12(nlq),fmr21(nlq),fmr22(nlq),fmr31(nlq),
     &fmr32(nlq),fmr41(nlq),fmr42(nlq),sg5(nlq),sg6(nlq)
      common/aux2loc/dstrn(nlq,6),
     1 wzzdt(nlq),wyydt(nlq),wxxdt(nlq),einc(nlq)
      common/aux5loc/
     &b1vx(nlq),b1vy(nlq),b1vz(nlq),b2vx(nlq),b2vy(nlq),b2vz(nlq),
     &b1tx(nlq),b1ty(nlq),b2tx(nlq),b2ty(nlq),bxyv(nlq),bxyt(nlq),
     &epyz(nlq),epzx(nlq)
      common/aux7loc/
     1 vx1(nlq),vx2(nlq),vx3(nlq),vx4(nlq),
     2 vx5(nlq),vx6(nlq),vx7(nlq),vx8(nlq),
     3 vy1(nlq),vy2(nlq),vy3(nlq),vy4(nlq),
     4 vy5(nlq),vy6(nlq),vy7(nlq),vy8(nlq),
     5 vz1(nlq),vz2(nlq),vz3(nlq),vz4(nlq),
     6 vz5(nlq),vz6(nlq),vz7(nlq),vz8(nlq)
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      real mx1,my1,mz1,mx2,my2,mz2,mx3,my3,mz3,mx4,my4,mz4
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     &gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq),
     &fx1(nlq),fy1(nlq),fz1(nlq),fx2(nlq),fy2(nlq),fz2(nlq),
     &fx3(nlq),fy3(nlq),fz3(nlq),fx4(nlq),fy4(nlq),fz4(nlq),
     &mx1(nlq),my1(nlq),mz1(nlq),mx2(nlq),my2(nlq),mz2(nlq),
     &mx3(nlq),my3(nlq),mz3(nlq),mx4(nlq),my4(nlq),mz4(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/aux14loc/ax(nlq,7)
      common/aux33loc/
     1 ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ixs(nlq,4),mxt(nlq)
      common/aux35loc/rhoa(nlq),cxx(nlq),fcl(nlq),sidem(nlq)
      common/aux45loc/crap(nlq,124),
     1 dndx(nlq,4),dndy(nlq,4),hnhx(nlq,4),hnhy(nlq,4)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5),
     . bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/failuloc/sieu(nlq),fail(nlq),ifaili(nlq)
      common/maxsvloc/mxsave,ipt_type(300)
      common/prescloc/voltot(nlq)
      common/shloptloc/ibelyt
      common/hourgloc/ymod(nlq),gmod(nlq),ifsv(nlq)
      common/sidesloc/sidmn(nlq)
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/subtssloc/dt1siz(nlq)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk26/begtim,nintcy
      common/bk28/summss,xke,xpe,tt,xte0,erodeke,erodeie,selie,selke,
     1 erodehg
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1 ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/eigvc/eke,eme,rsav,dt2std,isavdt2,iegflg,iegflg0(10)
      common/hour11loc/ebar(nlq),ebarmn(nlq),eyld(nlq),etanmd(nlq)
      logical failur,failgl
      common/failcm/failur,failgl
      common/neicmm/neiph,neips,maxint,itrist,itetst,labatto,neifut(2),
     . ldarry,lmarry,lcdamp,valdmp,dsclfcs(6)
      common/numcpu/ncpu,ncpua,ncpub,lenvec(8)
      common/shel/xi(4),eta(4),zetq(6,6),qw(6,6),xinod(4),etand(4),nz(6)
      common/ssbsis/h(8,10,10),pr(8,10,10),ps(8,10,10),pt(8,10,10),
     1 wgts(10,10),zet(10,10),iptz(2)
      common/shlopt/istrn,istupd,ibelyts,miter,wrpang,ipstpd,intsts,
     1 nodsts,intstn,nodstn,jstrn
c
      real ies(1)
      real*8 x,rots
      dimension ixp(5,*),x(3,*),rhs(*),rhr(*),vt(3,*),vr(3,*),
     1 yhatn(12,*),rots(3,*),
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),nsubgv(*),mtnum(*),
     3 nfegp(*),ihgq(1),hgq(*),strain(12,*),isrn(2,10),ener(*),
     4 iqtype(*),bkqs(3,*),gmi(4,*),hgener(*),rhssav(27,*),eig(3,*),
     5 eign(3,*),qextra(*),failjw(nlq),drlstr(4,*),rhsl(24,*)
      dimension cmusr(48,*),xipn(4,100,*),loceps(*),epsint(*)
      dimension scr(nlq,12)
c
      data isrn/1,1,1,2,2,3,1,4,2,5,1,6,1,7,1,8,1,9,1,10/
      data zero/0.0/
c
c     mxe = internal part number
c
c     Inverse of density
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux00loc/)
c$omp threadprivate (/aux01loc/)
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/aux5loc/)
c$omp threadprivate (/aux7loc/)
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/aux45loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/failuloc/)
c$omp threadprivate (/hourgloc/)
c$omp threadprivate (/hour11loc/)
c$omp threadprivate (/maxsvloc/)
c$omp threadprivate (/prescloc/)
c$omp threadprivate (/shloptloc/)
c$omp threadprivate (/sidesloc/)
c$omp threadprivate (/soundloc/)
c$omp threadprivate (/subtssloc/)
c$omp threadprivate (/vect13loc/)
      rho=1./ro(mxe)
c
c     Number of integration points in the plane and
c     through the thickness
c
      nipp=nint(cmusr(1,mxe))
      nipt=0
      if (nipp.gt.0) nipt=nint(csprop(2,mxe))/nipp
c
c     User shell properties
c
      nxdof=nint(cmusr(2,mxe))
      ihgf=nint(cmusr(4,mxe))
      if (nipp.eq.0) ihgf=0
      iunf=nint(cmusr(3,mxe))
      itaj=nint(cmusr(5,mxe))
      lmc=nint(cmusr(6,mxe))
      nhsv=nint(cmusr(7,mxe))
      iloc=nint(cmusr(8,mxe))
c
c     Quadrature rule or integration rule ID
c
      if (nipt.gt.0) then
        irl=nint(csprop(4,mxe))
        nrl=iabs(irl)
c
c       Switch to trapezoidal rule
c
        if (nipt.gt.10.and.irl.eq.0) then
          irl=1
        endif
c
c       User defined integration rule is specified
c       check for failure option for mixed material types
c
         if (irl.lt.0) then
           ifl=rule(mpusr-1,1,nrl)
           if (ifl.eq.1) ifaili(lft)=1
         endif
      endif
c
c     Material type
c
      mte=mtype(mxe)
c
c     Sound speed initialization
c
      sndspd(lft)=1.e-16
c
c     Gather geometry and kinematical properties
c
c     xi,yi,zi       = Nodal coordinates at t_{n+1}
c     xhi,yhi,zhi    = Nodal coordinates at t_{n+1/2}
c     dxi,dyi,dzi    = Translational increments
c     wxxi,wyyi,wzzi = Rotational increments
c
      do i=lft,llt
        x1(i) =x(1,ix1(i))
        y1(i) =x(2,ix1(i))
        z1(i) =x(3,ix1(i))
        dx1(i)=vt(1,ix1(i))*dt1siz(i)
        dy1(i)=vt(2,ix1(i))*dt1siz(i)
        dz1(i)=vt(3,ix1(i))*dt1siz(i)
        wxx1(i)=vr(1,ix1(i))*dt1siz(i)
        wyy1(i)=vr(2,ix1(i))*dt1siz(i)
        wzz1(i)=vr(3,ix1(i))*dt1siz(i)
        x2(i) =x(1,ix2(i))
        y2(i) =x(2,ix2(i))
        z2(i) =x(3,ix2(i))
        dx2(i)=vt(1,ix2(i))*dt1siz(i)
        dy2(i)=vt(2,ix2(i))*dt1siz(i)
        dz2(i)=vt(3,ix2(i))*dt1siz(i)
        wxx2(i)=vr(1,ix2(i))*dt1siz(i)
        wyy2(i)=vr(2,ix2(i))*dt1siz(i)
        wzz2(i)=vr(3,ix2(i))*dt1siz(i)
        x3(i) =x(1,ix3(i))
        y3(i) =x(2,ix3(i))
        z3(i) =x(3,ix3(i))
        dx3(i)=vt(1,ix3(i))*dt1siz(i)
        dy3(i)=vt(2,ix3(i))*dt1siz(i)
        dz3(i)=vt(3,ix3(i))*dt1siz(i)
        wxx3(i)=vr(1,ix3(i))*dt1siz(i)
        wyy3(i)=vr(2,ix3(i))*dt1siz(i)
        wzz3(i)=vr(3,ix3(i))*dt1siz(i)
        x4(i) =x(1,ix4(i))
        y4(i) =x(2,ix4(i))
        z4(i) =x(3,ix4(i))
        dx4(i)=vt(1,ix4(i))*dt1siz(i)
        dy4(i)=vt(2,ix4(i))*dt1siz(i)
        dz4(i)=vt(3,ix4(i))*dt1siz(i)
        wxx4(i)=vr(1,ix4(i))*dt1siz(i)
        wyy4(i)=vr(2,ix4(i))*dt1siz(i)
        wzz4(i)=vr(3,ix4(i))*dt1siz(i)
      enddo
c
c     Coordinates and increments are expressed in global system
c
      if (nxdof.gt.0) then
        do k=1,4
          do j=1,nxdof
            nsnd=(j-1)/3+1
            nsdf=j-(nsnd-1)*3
            do i=lft,llt
              xdof(i,k,j)=x(nsdf,ixshl(i,k,nsnd))
              dxdof(i,k,j)=vt(nsdf,ixshl(i,k,nsnd))*dt1siz(i)
            enddo
          enddo
        enddo
      endif
c
c
c     Accuracy option is on, compute half step geometry
c
      if (lenvec8.ne.0) then
        do i=lft,llt
          xh1(i)=x1(i)-.5*dx1(i)
          yh1(i)=y1(i)-.5*dy1(i)
          zh1(i)=z1(i)-.5*dz1(i)
          xh2(i)=x2(i)-.5*dx2(i)
          yh2(i)=y2(i)-.5*dy2(i)
          zh2(i)=z2(i)-.5*dz2(i)
          xh3(i)=x3(i)-.5*dx3(i)
          yh3(i)=y3(i)-.5*dy3(i)
          zh3(i)=z3(i)-.5*dz3(i)
          xh4(i)=x4(i)-.5*dx4(i)
          yh4(i)=y4(i)-.5*dy4(i)
          zh4(i)=z4(i)-.5*dz4(i)
        enddo
c
        if (nxdof.gt.0) then
          do k=1,4
            do j=1,nxdof
              do i=lft,llt
                xhdof(i,k,j)=xdof(i,k,j)-.5*dxdof(i,k,j)
              enddo
            enddo
          enddo
        endif
c
      else
         do i=lft,llt
           xh1(i)=x1(i)
           yh1(i)=y1(i)
           zh1(i)=z1(i)
           xh2(i)=x2(i)
           yh2(i)=y2(i)
           zh2(i)=z2(i)
           xh3(i)=x3(i)
           yh3(i)=y3(i)
           zh3(i)=z3(i)
           xh4(i)=x4(i)
           yh4(i)=y4(i)
           zh4(i)=z4(i)
        enddo
c
        if (nxdof.gt.0) then
          do k=1,4
            do j=1,nxdof
              do i=lft,llt
                xhdof(i,k,j)=xdof(i,k,j)
              enddo
            enddo
          enddo
        endif
      endif
c
c     Coordinates for X-tra dofs are expressed in
c     global system (if for some reason directionally dependent)
c
      if (iunf.ne.0) then
c
c     Load nodal vectors from previous step
c
         do i=lft,llt
            yhtnx1(i)=yhatn(1,i+nnm1)
            yhtny1(i)=yhatn(2,i+nnm1)
            yhtnz1(i)=yhatn(3,i+nnm1)
            yhtnx2(i)=yhatn(4,i+nnm1)
            yhtny2(i)=yhatn(5,i+nnm1)
            yhtnz2(i)=yhatn(6,i+nnm1)
            yhtnx3(i)=yhatn(7,i+nnm1)
            yhtny3(i)=yhatn(8,i+nnm1)
            yhtnz3(i)=yhatn(9,i+nnm1)
            yhtnx4(i)=yhatn(10,i+nnm1)
            yhtny4(i)=yhatn(11,i+nnm1)
            yhtnz4(i)=yhatn(12,i+nnm1)
         enddo
c
c     Update nodal vectors
c
         call usrshl_nvu(lenvec8,lft,llt)
c
      endif
c
c     Initialize some parameters
c
      do i=lft,llt
c     failure
         fail(i)  =1.0
         failjw(i)=1.0
c     internal energy
         sieu(i)  =ies(nnm1+i)
c     volume at t_{n+1/2} and t_{n+1}
         voltot(i)=0.
         cvltot(i)=0.
      enddo
c
c     For shell thickness changes
c
      if (istupd.ne.0) then
         if (iunf.ne.0) then
            do i=lft,llt
               ds11(i)=0.
               ds22(i)=0.
               ds33(i)=0.
               ds12(i)=0.
               ds23(i)=0.
               ds13(i)=0.
            enddo
         else
            do i=lft,llt
               str33(i) =0.0
            enddo
         endif
      endif
c
c     Check jacobian and delete distorted elements
c
      if (ioshl(12).ge.1) then
         call crnjac    (lft,llt,xh1,yh1,zh1,xh2,yh2,zh2,
     .        xh3,yh3,zh3,xh4,yh4,zh4)
         call pscrnr(lft,llt,nnm1,failjw)
      if (rioshl(20).gt.0.0) then
      call crnjac_diag(lft,llt,xh1,yh1,zh1,xh2,yh2,zh2,
     .             xh3,yh3,zh3,xh4,yh4,zh4,ix1,ix2,ix3,ix4,rots,
     .             rioshl(20))
      endif
      endif
c
c     Set up lamina coordinate system at t_{n+1/2} (if requested)
c     for strain calculations and at t_{n+1} for force and
c     stiffness calculations
c
      call usrshl_ls(lenvec8,lft,llt)
c
c     Modify velocities for Nastran type offsets
c
      if (ioshl(36).eq.1) call mod_vel (a(ioshl(37)+nnm1),lft,llt)
c
c     Transform coordinates and increments to local system
c
      if (iloc.eq.0) call usrshl_g2l(lenvec8,lft,llt)
c
c     Gather length of fibers and transforms fiber directions
c
      call usrshl_fbl(fibl(1,nnm1+1),iunf,lenvec8,lft,llt,iloc)
c
c     Compute element properties for time step calculations
c
      do i=lft,llt
         x13=xh3(i)-xh1(i)
         x24=xh4(i)-xh2(i)
         y13=yh3(i)-yh1(i)
         y24=yh4(i)-yh2(i)
         z13=zh3(i)-zh1(i)
         z24=zh4(i)-zh2(i)
         fs1=x13-x24
         ft1=x13+x24
         fs2=y13-y24
         ft2=y13+y24
         fs3=z13-z24
         ft3=z13+z24
         e=fs1*fs1+fs2*fs2+fs3*fs3
         f=fs1*ft1+fs2*ft2+fs3*ft3
         g=ft1*ft1+ft2*ft2+ft3*ft3
         diag1   =x13**2+y13**2+z13**2
         diag2   =x24**2+y24**2+z24**2
         diagm(i)=  max(diag1,diag2)
         sidmn(i)=0.0
         sarea(i)=sqrt((e*g-f*f)/16.)
         area(i)=1./(sarea(i)+1.e-16)
         x21=xh2(i)-xh1(i)
         y21=yh2(i)-yh1(i)
         z21=zh2(i)-zh1(i)
         side1   =x21*x21+y21*y21+z21*z21
         x32=xh3(i)-xh2(i)
         y32=yh3(i)-yh2(i)
         z32=zh3(i)-zh2(i)
         side2   =x32*x32+y32*y32+z32*z32
         x43=xh4(i)-xh3(i)
         y43=yh4(i)-yh3(i)
         z43=zh4(i)-zh3(i)
         side3   =x43*x43+y43*y43+z43*z43-1.e-10
         x14=xh1(i)-xh4(i)
         y14=yh1(i)-yh4(i)
         z14=zh1(i)-zh4(i)
         side4   =x14*x14+y14*y14+z14*z14
         sida3   =side4*(.5-sign(.5,side3))+side3
         sidmn(i)=  min(side1,side2,sida3,side4)
         sidem(i)=  max(side1,side2,side3,side4)*(.625+sign(.375,side3))
      enddo
      if (isdo.eq.0.or.isdo.eq.2) then
         do i=lft,llt
            diagm(i)=  min(diagm(i),sidem(i))
         enddo
      endif
c
      if (nipt.gt.0) then
c
c     Initialize parameters
c
         mxsave       =0
         ipt_type(mte)=1
c
c     Process user defined integration rule
c
         if (irl.lt.0) then
            do m=1,nipt
               mtu=nint(rule(m,3,nrl))
               if (mtu.ne.0) then
                  ipt_type(mtype(mtu))=1
               endif
            enddo
         endif
c
      endif
c
c     Zero internal force vector
c
      ndtot=4*(6+nxdof)
      do j=1,ndtot
         do i=lft,llt
            frc(i,j)=0.
         enddo
      enddo
c
      if (nipp.gt.0) then
c
c     strain needed for user-defined failure like matusr_24
c     
      neps=6*nipp*nipt
      eps2df=0.
      if ((cm(5+48*(mxt(lft)-1)).lt.0..and.
     .    (mte.eq.24.or.mte.eq.114.or.mte.eq.123.or.mte.eq.124.or.
     .     mte.eq.155.or.mte.eq.182.or.mte.eq.238.or.mte.eq.255)).or.
     .    (cm(30+48*(mxt(lft)-1)).gt.0..and.(mte.eq.36.or.mte.eq.243))
     .    .or.(cm(9+48*(mxt(lft)-1)).lt.0..and.mte.eq.133))then
        inteps=1
      else
        inteps=0
      endif
c
c     Loop over integration points in plane
c
      do ipp=1,nipp
c
c     Data for quadrature point
c
         xiq=xipn(1,ipp,mxe)
         etaq=xipn(2,ipp,mxe)
         wgt=xipn(4,ipp,mxe)
c
c     Loop over integration points through thickness
c
         do m=1,nipt
c
            mtv=mxt(lft)
            mtf=mte
            sds=sndspd(lft)
c
c     Integration point number
c
            ipt=nipp*(m-1)+ipp
c
c     Get parental coordinate through the thickness
c
            if (irl.eq.0) then
c     Gauss
               ztaq=zet(m,nipt)
               zwgt=wgts(m,nipt)
            elseif (irl.gt.0) then
c     Trapezoidal
               ztaq=(-1.0+(m-1)*2./(nipt-1))
               zwgt=2./(nipt-1)
               if (m.eq.1.or.m.eq.nipt) zwgt=zwgt/2.
            else
c     User defined
               ztaq=rule(m,1,nrl)
               zwgt=rule(m,2,nrl)
               mtu=nint(rule(m,3,nrl))
               if (mtu.ne.0) then
                  mxt(lft)=mtu
                  mte     =mtype(mtu)
               endif
            endif
c
c     Call routine for computing b-matrix and g-matrix
c
            call usrshl_b(ietyp,xiq,etaq,ztaq,lenvec8,lft,llt,iloc)
c
c     Set volume of quadrature point and compute "real" b-matrix
c
            call usrshl_b2b(itaj,nxdof,zwgt*wgt,lenvec8,lft,llt)
            do i=lft,llt
               vol(i)=vol(i)*zwgt*wgt
            enddo
c
c     Compute strain and spin increments
c
            call usrshl_str(nxdof,lft,llt,iloc)
c
c     Zero energy increments
c
            do i=lft,llt
               einc(i)=0.
            enddo
c
c     If thermal expansion modify strain increment
c
            if (ithxpid.gt.0)
     .           call adthstr(lft,llt,0)
c
c     users need strain for user-defined failure like matusr_24
c     
            if (ioshl(557).ge.1) then
              leps=loceps(nnm1+lft)+6*(m-1)+6*nipt*(ipp-1)
              if (inteps.gt.0)
     .         call psseps4um(inteps,neps,epsint(leps),lft,llt,eps2df)
            endif
c
c     Constitutive model evaluation
c
            call usrshl_con(nmtcon,auxvec,cm,lav,mte,nipp*nipt,ipt,
     1           csprop(1,mxe),mxe,lft,llt,nnm1,
     2           xiq,etaq,ztaq,m,ihgf,vol,nhsv,iunf,eosp)
c
c     If thermal expansion, reset strain increment
c
            if (ithxpid.gt.0)
     .           call sbthstr(lft,llt,0)
c
c     Update internal energy, volume and plastic strain
c
            if (isolvr(18).eq.0) then
               do i=lft,llt
                  ies(nnm1+i)=ies(nnm1+i)+.5*vol(i)*einc(i)
               enddo
               if (ipi.ne.iph) then
                  if (mte.eq.224) then
                    iep=19
                  else
                    iep=7
                  endif
                  do i=lft,llt
                     a(ipi-1+nnm1+i)=a(ipi-1+nnm1+i)+vol(i)
                     a(iph-1+nnm1+i)=a(iph-1+nnm1+i)+vol(i)*ax(i,iep)
                  enddo
               endif
            endif
c
c     Get strains for post-processing
c
            if (istrn.ne.0) then
               if (irl.eq.0) then
                  if (m.eq.isrn(1,nipt)) then
                     call usrshl_istr(strain(1,nnm1+1),wgt,lft,llt)
                  endif
                  if (m.eq.isrn(2,nipt)) then
                     call usrshl_istr(strain(7,nnm1+1),wgt,lft,llt)
                  endif
               else
                  if (m.eq.1  ) then
                     call usrshl_istr(strain(1,nnm1+1),wgt,lft,llt)
                  endif
                  if (m.eq.nipt) then
                     call usrshl_istr(strain(7,nnm1+1),wgt,lft,llt)
                  endif
               endif
            endif
c
c     store all integration point strains if requested
c
            if (ioshl(557).ge.1) call psseps(neps,epsint(leps),lft,llt)
c
            if (istupd.ne.0) then
c
c     Shell thickness changes
c
               call usrshl_igs(.125*wgt*zwgt,iunf,lft,llt)
c
            endif
c
c     Compute force components in local/global system
c
            call usrshl_frc(nxdof,lft,llt)
c
            if (is17loc.eq.1) then
c
c     Assemble stiffness matrix
c
               if (iloc.eq.0) call usrshl_trb(nxdof,lft,llt)
               if (isolvr(23).eq.1)
     .              call usrshl_kgm(ske,nxdof,lft,llt)
               if (isolvr(91).ne.2)
     .              call usrshl_kmt(ske,nxdof,lft,llt)
c
            endif
c
            ipt_type(mte)=m+1
            mxt(lft)=mtv
            mte     =mtf
            sndspd(lft)=max(sndspd(lft),sds)
c
         enddo
c
      enddo
c
      else
c
c     Resultant element
c
         do i=lft,llt
            sndspd(i)=cm(48*(mxe-1)+1)/(1.-cm(48*(mxe-1)+6)*2)
         enddo
         call usrshl_h(ietyp,cmusr(9,mxe),lmc,nhsv,nxdof,lft,llt,iloc,
     .        ihgf)
c
      endif
c
c     Compute sound speed for time step calculations
c
      sndspd(lft)=sqrt(sndspd(lft)*rho)
c
      if (ihgf.eq.1) then
c
c     LS-DYNA hourglass force requested
c
c     Compute shape function derivatives
c
         call usrshl_sfd(lenvec8,lft,llt)
c
c     Assemble hourglass forces
c
         call usrshl_hgf(fibl(5,nnm1+1),ihgenf,hgener,
     1        mte,nxdof,lenvec8,lft,llt,iloc)
c
      elseif (ihgf.eq.2.or.ihgf.eq.3) then
c
c     User defined hourglass force requested
c
         call usrshl_h(ietyp,cmusr(9,mxe),lmc,nhsv,nxdof,lft,llt,iloc,
     .        ihgf)
c
      endif
c
      if (ioshl(12).ge.1) then
         if (failur) then
            do i=lft,llt
               fail(i)=min(fail(i),failjw(i))
            enddo
         endif
      endif
c
      if (lenvec8.ne.0) then
c
c     Recompute element properties for time step calculations
c
      do i=lft,llt
         x13=x3(i)-x1(i)
         x24=x4(i)-x2(i)
         y13=y3(i)-y1(i)
         y24=y4(i)-y2(i)
         z13=z3(i)-z1(i)
         z24=z4(i)-z2(i)
         fs1=x13-x24
         ft1=x13+x24
         fs2=y13-y24
         ft2=y13+y24
         fs3=z13-z24
         ft3=z13+z24
         e=fs1*fs1+fs2*fs2+fs3*fs3
         f=fs1*ft1+fs2*ft2+fs3*ft3
         g=ft1*ft1+ft2*ft2+ft3*ft3
         diag1   =x13**2+y13**2+z13**2
         diag2   =x24**2+y24**2+z24**2
         diagm(i)=  max(diag1,diag2)
         sidmn(i)=0.0
         sarea(i)=sqrt((e*g-f*f)/16.)
         area(i)=1./(sarea(i)+1.e-16)
         x21=x2(i)-x1(i)
         y21=y2(i)-y1(i)
         z21=z2(i)-z1(i)
         side1   =x21*x21+y21*y21+z21*z21
         x32=x3(i)-x2(i)
         y32=y3(i)-y2(i)
         z32=z3(i)-z2(i)
         side2   =x32*x32+y32*y32+z32*z32
         x43=x4(i)-x3(i)
         y43=y4(i)-y3(i)
         z43=z4(i)-z3(i)
         side3   =x43*x43+y43*y43+z43*z43-1.e-10
         x14=x1(i)-x4(i)
         y14=y1(i)-y4(i)
         z14=z1(i)-z4(i)
         side4   =x14*x14+y14*y14+z14*z14
         sida3   =side4*(.5-sign(.5,side3))+side3
         sidmn(i)=  min(side1,side2,sida3,side4)
         sidem(i)=  max(side1,side2,side3,side4)*(.625+sign(.375,side3))
      enddo
      if (isdo.eq.0.or.isdo.eq.2) then
         do i=lft,llt
            diagm(i)=  min(diagm(i),sidem(i))
         enddo
      endif
c
      endif
c
      if (ihgf.eq.1) then
c
c     LS-DYNA hourglass force requested
c
         if (is17loc.eq.1.and.isolvr(91).ne.2) then
c
c     Assemble hourglass stiffness
c
            call usrshl_hgs(ske,lft,llt)
c
         endif
c
      endif
c
c     Transform force to global system
c
      if (iloc.eq.0) call usrshl_trf(nxdof,lft,llt)
c
      if (istupd.ne.0) then
c
c     Handle shell thickness updates
c
         istpd=max(1,istupd)
         call usrshl_stc(fibl(1,nnm1+1),rhssav(1,nnm1+1),a(istpd),
     1        iunf,nipp,lft,llt,iloc)
      endif
c
      do i=lft,llt
         gl11(i)=hl11(i)
         gl21(i)=hl21(i)
         gl31(i)=hl31(i)
         gl12(i)=hl12(i)
         gl22(i)=hl22(i)
         gl32(i)=hl32(i)
         gl13(i)=hl13(i)
         gl23(i)=hl23(i)
         gl33(i)=hl33(i)
      enddo
c
      if (nipp.ne.0) then
      if (ioshl(36).eq.1) then
         do i=lft,llt
            fx1(i)=frc(i,1)
            fy1(i)=frc(i,2)
            fz1(i)=frc(i,3)
            mx1(i)=frc(i,4)
            my1(i)=frc(i,5)
            mz1(i)=frc(i,6)
            fx3(i)=frc(i,13+2*nxdof)
            fy3(i)=frc(i,14+2*nxdof)
            fz3(i)=frc(i,15+2*nxdof)
            mx3(i)=frc(i,16+2*nxdof)
            my3(i)=frc(i,17+2*nxdof)
            mz3(i)=frc(i,18+2*nxdof)
            fx2(i)=frc(i,7+nxdof)
            fy2(i)=frc(i,8+nxdof)
            fz2(i)=frc(i,9+nxdof)
            mx2(i)=frc(i,10+nxdof)
            my2(i)=frc(i,11+nxdof)
            mz2(i)=frc(i,12+nxdof)
            fx4(i)=frc(i,19+3*nxdof)
            fy4(i)=frc(i,20+3*nxdof)
            fz4(i)=frc(i,21+3*nxdof)
            mx4(i)=frc(i,22+3*nxdof)
            my4(i)=frc(i,23+3*nxdof)
            mz4(i)=frc(i,24+3*nxdof)
         enddo
         call nasofrc(a(ioshl(37)+nnm1),lft,llt)
         do i=lft,llt
            frc(i,4)=mx1(i)
            frc(i,5)=my1(i)
            frc(i,6)=mz1(i)
            frc(i,16+2*nxdof)=mx3(i)
            frc(i,17+2*nxdof)=my3(i)
            frc(i,18+2*nxdof)=mz3(i)
            frc(i,10+nxdof)=mx2(i)
            frc(i,11+nxdof)=my2(i)
            frc(i,12+nxdof)=mz2(i)
            frc(i,22+3*nxdof)=mx4(i)
            frc(i,23+3*nxdof)=my4(i)
            frc(i,24+3*nxdof)=mz4(i)
         enddo
      endif
      endif
c
      if (lenvec(1).ne.0) then
         call usrshl_fs(nxdof,rhssav(1,nnm1+1),mte,lft,llt,
     .    rhsl(1,nnm1+1))
      else
         call usrshl_ft (nxdof,rhs,rhr,mte,lft,llt)
      endif
c
      if (iunf.ne.0) then
c
c     Save nodal vectors for next step
c
         if (isolvr(18).eq.0) then
            do i=lft,llt
               yhatn(1,i+nnm1)=yhatx1(i)
               yhatn(2,i+nnm1)=yhaty1(i)
               yhatn(3,i+nnm1)=yhatz1(i)
               yhatn(4,i+nnm1)=yhatx2(i)
               yhatn(5,i+nnm1)=yhaty2(i)
               yhatn(6,i+nnm1)=yhatz2(i)
               yhatn(7,i+nnm1)=yhatx3(i)
               yhatn(8,i+nnm1)=yhaty3(i)
               yhatn(9,i+nnm1)=yhatz3(i)
               yhatn(10,i+nnm1)=yhatx4(i)
               yhatn(11,i+nnm1)=yhaty4(i)
               yhatn(12,i+nnm1)=yhatz4(i)
            enddo
         endif
c
      endif
c
c     Add drilling contribution
c
      if (nipp.ne.0) then
      if ((isolvr(1).eq.1.and.ascntl(40).gt.0.0.and.isolvr(76).eq.4)
     & .or.isdrill.eq.1) then
         if (iunf.ne.0) then
            do i=lft,llt
               thick(i)=.25*(thick(i)+fga(i)+fgb(i)+fgc(i))
            enddo
         endif
         call drlfrc(rhs,rhr,rhssav(1,nnm1+1),lenvec(1),
     .        thick,sarea,ymod(lft),
     .        ske,drlstr(1,nnm1+1),lft,llt,
     .        x,vt,vr,dt1siz,
     .        4)
      endif
      endif
c
      if (is17loc.eq.1) then
c
c     Take care of drilling DOFs
c
         if (isolvr(91).ne.2) then
c
            if (ascntl(40).gt.0.0) then
               if (isolvr(76).ne.4) then
               do i=lft,llt
                  dy2(i)=cvltot(i)/sarea(i)
               enddo
               call psszk(is17loc,lft,llt,ske,
     &              x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     &              hl11,hl12,hl13,hl21,hl22,hl23,hl31,hl32,hl33)
c$$$               call zlen(lft,llt,scr,hl13,hl23,hl33,
c$$$     &              x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
c$$$               call ttkt(lft,llt,ske,scr)
               endif
            else
               call bfixrt(ske,lft,llt)
            endif
         endif
c
c     Assemble tangent stiffness
c
         ndofpn=6
         if (nipp.ne.0) then
         if (ioshl(36).eq.1) call nasostf(a(ioshl(37)+nnm1),lft,llt)
         if (ascntl(40).gt.0.0.and.isolvr(76).eq.4) then
            if (iunf.ne.0) then
               do i=lft,llt
                  thick(i)=.25*(thick(i)+fga(i)+fgb(i)+fgc(i))
               enddo
            endif
            call drlstf(rhs,rhr,rhssav(1,nnm1+1),lenvec(1),
     .           thick,sarea,ymod(lft),
     .           ske,drlstr(1,nnm1+1),is17loc,lft,llt,
     .           x,vt,vr,dt1siz,
     .           4)
         endif
         endif
         nsnd=0
         if (nxdof.ne.0) nsnd=(nxdof-1)/3+1
         nnpke=4+4*nsnd
         imlft=lft
         imllt=llt
         melemt=llt-lft-1
         do i=lft,llt
            lnodim(i,1)=ix1(i)
            lnodim(i,2)=ix2(i)
            lnodim(i,3)=ix3(i)
            lnodim(i,4)=ix4(i)
         enddo
         do j=1,nsnd
           do i=lft,llt
             do k=1,4              
               ij=4+4*(j-1)+k
               lnodim(i,ij)=ixshl(i,k,j)
             enddo
           enddo
         enddo
c
         if (isolvr(77).ne.3.or.nipp.ne.0) then
         call imasem
         else
         call imasem_unsym
         endif
c
      endif
c
      return
      end
      subroutine usrshl_nvu(lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     Nodal vector update for user defined shell
c
      include 'nlqparm'
      include 'nhisparm.inc'
c
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq)
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     &gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
      common/vect18loc/
     1 rot1(nlq),rot2(nlq),rot3(nlq),rot4(nlq),rot5(nlq),
     2 rot6(nlq),rot7(nlq),rot8(nlq),rot9(nlq)
      common/rn/irnxx
c
      dimension wx(nlq),wy(nlq),wz(nlq)
c
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/vect13loc/)
c$omp threadprivate (/vect18loc/)
      if (irnxx.ne.-2) then
c
c     Recompute fiber directions in each cycle
c
         if (lenvec8.ne.0) then
c
         do i=lft,llt
            ux=xh2(i)-xh1(i)
            uy=yh2(i)-yh1(i)
            uz=zh2(i)-zh1(i)
            vx=xh4(i)-xh1(i)
            vy=yh4(i)-yh1(i)
            vz=zh4(i)-zh1(i)
            yhtmx1(i)=uy*vz-vy*uz
            yhtmy1(i)=uz*vx-vz*ux
            yhtmz1(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhtmx1(i)**2+yhtmy1(i)**2+yhtmz1(i)**2+1.e-16)
            yhtmx1(i)=yhtmx1(i)*ynrmi
            yhtmy1(i)=yhtmy1(i)*ynrmi
            yhtmz1(i)=yhtmz1(i)*ynrmi
         enddo
         do i=lft,llt
            ux=xh3(i)-xh2(i)
            uy=yh3(i)-yh2(i)
            uz=zh3(i)-zh2(i)
            vx=xh1(i)-xh2(i)
            vy=yh1(i)-yh2(i)
            vz=zh1(i)-zh2(i)
            yhtmx2(i)=uy*vz-vy*uz
            yhtmy2(i)=uz*vx-vz*ux
            yhtmz2(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhtmx2(i)**2+yhtmy2(i)**2+yhtmz2(i)**2+1.e-16)
            yhtmx2(i)=yhtmx2(i)*ynrmi
            yhtmy2(i)=yhtmy2(i)*ynrmi
            yhtmz2(i)=yhtmz2(i)*ynrmi
         enddo
         do i=lft,llt
            ux=xh4(i)-xh3(i)
            uy=yh4(i)-yh3(i)
            uz=zh4(i)-zh3(i)
            vx=xh2(i)-xh3(i)
            vy=yh2(i)-yh3(i)
            vz=zh2(i)-zh3(i)
            yhtmx3(i)=uy*vz-vy*uz
            yhtmy3(i)=uz*vx-vz*ux
            yhtmz3(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhtmx3(i)**2+yhtmy3(i)**2+yhtmz3(i)**2+1.e-16)
            yhtmx3(i)=yhtmx3(i)*ynrmi
            yhtmy3(i)=yhtmy3(i)*ynrmi
            yhtmz3(i)=yhtmz3(i)*ynrmi
         enddo
         do i=lft,llt
            ux=xh1(i)-xh4(i)
            uy=yh1(i)-yh4(i)
            uz=zh1(i)-zh4(i)
            vx=xh3(i)-xh4(i)
            vy=yh3(i)-yh4(i)
            vz=zh3(i)-zh4(i)
            yhtmx4(i)=uy*vz-vy*uz
            yhtmy4(i)=uz*vx-vz*ux
            yhtmz4(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhtmx4(i)**2+yhtmy4(i)**2+yhtmz4(i)**2+1.e-16)
            yhtmx4(i)=yhtmx4(i)*ynrmi
            yhtmy4(i)=yhtmy4(i)*ynrmi
            yhtmz4(i)=yhtmz4(i)*ynrmi
         enddo
c
         endif
c
         do i=lft,llt
            ux=x2(i)-x1(i)
            uy=y2(i)-y1(i)
            uz=z2(i)-z1(i)
            vx=x4(i)-x1(i)
            vy=y4(i)-y1(i)
            vz=z4(i)-z1(i)
            yhatx1(i)=uy*vz-vy*uz
            yhaty1(i)=uz*vx-vz*ux
            yhatz1(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhatx1(i)**2+yhaty1(i)**2+yhatz1(i)**2+1.e-16)
            yhatx1(i)=yhatx1(i)*ynrmi
            yhaty1(i)=yhaty1(i)*ynrmi
            yhatz1(i)=yhatz1(i)*ynrmi
         enddo
         do i=lft,llt
            ux=x3(i)-x2(i)
            uy=y3(i)-y2(i)
            uz=z3(i)-z2(i)
            vx=x1(i)-x2(i)
            vy=y1(i)-y2(i)
            vz=z1(i)-z2(i)
            yhatx2(i)=uy*vz-vy*uz
            yhaty2(i)=uz*vx-vz*ux
            yhatz2(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhatx2(i)**2+yhaty2(i)**2+yhatz2(i)**2+1.e-16)
            yhatx2(i)=yhatx2(i)*ynrmi
            yhaty2(i)=yhaty2(i)*ynrmi
            yhatz2(i)=yhatz2(i)*ynrmi
         enddo
         do i=lft,llt
            ux=x4(i)-x3(i)
            uy=y4(i)-y3(i)
            uz=z4(i)-z3(i)
            vx=x2(i)-x3(i)
            vy=y2(i)-y3(i)
            vz=z2(i)-z3(i)
            yhatx3(i)=uy*vz-vy*uz
            yhaty3(i)=uz*vx-vz*ux
            yhatz3(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhatx3(i)**2+yhaty3(i)**2+yhatz3(i)**2+1.e-16)
            yhatx3(i)=yhatx3(i)*ynrmi
            yhaty3(i)=yhaty3(i)*ynrmi
            yhatz3(i)=yhatz3(i)*ynrmi
         enddo
         do i=lft,llt
            ux=x1(i)-x4(i)
            uy=y1(i)-y4(i)
            uz=z1(i)-z4(i)
            vx=x3(i)-x4(i)
            vy=y3(i)-y4(i)
            vz=z3(i)-z4(i)
            yhatx4(i)=uy*vz-vy*uz
            yhaty4(i)=uz*vx-vz*ux
            yhatz4(i)=ux*vy-vx*uy
            ynrmi=1./sqrt(yhatx4(i)**2+yhaty4(i)**2+yhatz4(i)**2+1.e-16)
            yhatx4(i)=yhatx4(i)*ynrmi
            yhaty4(i)=yhaty4(i)*ynrmi
            yhatz4(i)=yhatz4(i)*ynrmi
         enddo
c
      else
c
         if (lenvec8.ne.0) then
c
         do i=lft,llt
            wx(i)=.5*wxx1(i)
            wy(i)=.5*wyy1(i)
            wz(i)=.5*wzz1(i)
         enddo
         call rwngfl(wx,wy,wz,lft,llt)
         do 10 i=lft,llt
            yhtmx1(i)=rot1(i)*yhtnx1(i)+
     1           rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
            yhtmy1(i)=rot2(i)*yhtnx1(i)+
     1           rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
            yhtmz1(i)=rot3(i)*yhtnx1(i)+
     1           rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
            yhatx1(i)=rot1(i)*yhtmx1(i)+
     1           rot4(i)*yhtmy1(i)+rot7(i)*yhtmz1(i)
            yhaty1(i)=rot2(i)*yhtmx1(i)+
     1           rot5(i)*yhtmy1(i)+rot8(i)*yhtmz1(i)
            yhatz1(i)=rot3(i)*yhtmx1(i)+
     1           rot6(i)*yhtmy1(i)+rot9(i)*yhtmz1(i)
 10      continue
c
         do i=lft,llt
            wx(i)=.5*wxx2(i)
            wy(i)=.5*wyy2(i)
            wz(i)=.5*wzz2(i)
         enddo
         call rwngfl(wx,wy,wz,lft,llt)
         do 20 i=lft,llt
            yhtmx2(i)=rot1(i)*yhtnx2(i)+
     1           rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
            yhtmy2(i)=rot2(i)*yhtnx2(i)+
     1           rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
            yhtmz2(i)=rot3(i)*yhtnx2(i)+
     1           rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
            yhatx2(i)=rot1(i)*yhtmx2(i)+
     1           rot4(i)*yhtmy2(i)+rot7(i)*yhtmz2(i)
            yhaty2(i)=rot2(i)*yhtmx2(i)+
     1           rot5(i)*yhtmy2(i)+rot8(i)*yhtmz2(i)
            yhatz2(i)=rot3(i)*yhtmx2(i)+
     1           rot6(i)*yhtmy2(i)+rot9(i)*yhtmz2(i)
 20      continue
c
         do i=lft,llt
            wx(i)=.5*wxx3(i)
            wy(i)=.5*wyy3(i)
            wz(i)=.5*wzz3(i)
         enddo
         call rwngfl(wx,wy,wz,lft,llt)
         do 30 i=lft,llt
            yhtmx3(i)=rot1(i)*yhtnx3(i)+
     1           rot4(i)*yhtny3(i)+rot7(i)*yhtnz3(i)
            yhtmy3(i)=rot2(i)*yhtnx3(i)+
     1           rot5(i)*yhtny3(i)+rot8(i)*yhtnz3(i)
            yhtmz3(i)=rot3(i)*yhtnx3(i)+
     1           rot6(i)*yhtny3(i)+rot9(i)*yhtnz3(i)
            yhatx3(i)=rot1(i)*yhtmx3(i)+
     1           rot4(i)*yhtmy3(i)+rot7(i)*yhtmz3(i)
            yhaty3(i)=rot2(i)*yhtmx3(i)+
     1           rot5(i)*yhtmy3(i)+rot8(i)*yhtmz3(i)
            yhatz3(i)=rot3(i)*yhtmx3(i)+
     1           rot6(i)*yhtmy3(i)+rot9(i)*yhtmz3(i)
 30      continue
c
         do i=lft,llt
            wx(i)=.5*wxx4(i)
            wy(i)=.5*wyy4(i)
            wz(i)=.5*wzz4(i)
         enddo
         call rwngfl(wx,wy,wz,lft,llt)
         do 40 i=lft,llt
            yhtmx4(i)=rot1(i)*yhtnx4(i)+
     1           rot4(i)*yhtny4(i)+rot7(i)*yhtnz4(i)
            yhtmy4(i)=rot2(i)*yhtnx4(i)+
     1           rot5(i)*yhtny4(i)+rot8(i)*yhtnz4(i)
            yhtmz4(i)=rot3(i)*yhtnx4(i)+
     1           rot6(i)*yhtny4(i)+rot9(i)*yhtnz4(i)
            yhatx4(i)=rot1(i)*yhtmx4(i)+
     1           rot4(i)*yhtmy4(i)+rot7(i)*yhtmz4(i)
            yhaty4(i)=rot2(i)*yhtmx4(i)+
     1           rot5(i)*yhtmy4(i)+rot8(i)*yhtmz4(i)
            yhatz4(i)=rot3(i)*yhtmx4(i)+
     1           rot6(i)*yhtmy4(i)+rot9(i)*yhtmz4(i)
 40      continue
c
         else
c
         call rwngfl(wxx1,wyy1,wzz1,lft,llt)
         do i=lft,llt
            yhatx1(i)=rot1(i)*yhtnx1(i)+
     1           rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
            yhaty1(i)=rot2(i)*yhtnx1(i)+
     1           rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
            yhatz1(i)=rot3(i)*yhtnx1(i)+
     1           rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
         enddo
c
         call rwngfl(wxx2,wyy2,wzz2,lft,llt)
         do i=lft,llt
            yhatx2(i)=rot1(i)*yhtnx2(i)+
     1           rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
            yhaty2(i)=rot2(i)*yhtnx2(i)+
     1           rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
            yhatz2(i)=rot3(i)*yhtnx2(i)+
     1           rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
         enddo
c
         call rwngfl(wxx3,wyy3,wzz3,lft,llt)
         do i=lft,llt
            yhatx3(i)=rot1(i)*yhtnx3(i)+
     1           rot4(i)*yhtny3(i)+rot7(i)*yhtnz3(i)
            yhaty3(i)=rot2(i)*yhtnx3(i)+
     1           rot5(i)*yhtny3(i)+rot8(i)*yhtnz3(i)
            yhatz3(i)=rot3(i)*yhtnx3(i)+
     1           rot6(i)*yhtny3(i)+rot9(i)*yhtnz3(i)
         enddo
c
         call rwngfl(wxx4,wyy4,wzz4,lft,llt)
         do i=lft,llt
            yhatx4(i)=rot1(i)*yhtnx4(i)+
     1           rot4(i)*yhtny4(i)+rot7(i)*yhtnz4(i)
            yhaty4(i)=rot2(i)*yhtnx4(i)+
     1           rot5(i)*yhtny4(i)+rot8(i)*yhtnz4(i)
            yhatz4(i)=rot3(i)*yhtnx4(i)+
     1           rot6(i)*yhtny4(i)+rot9(i)*yhtnz4(i)
         enddo
c
         endif
c
      endif
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            yhtmx1(i)=yhatx1(i)
            yhtmy1(i)=yhaty1(i)
            yhtmz1(i)=yhatz1(i)
            yhtmx2(i)=yhatx2(i)
            yhtmy2(i)=yhaty2(i)
            yhtmz2(i)=yhatz2(i)
            yhtmx3(i)=yhatx3(i)
            yhtmy3(i)=yhaty3(i)
            yhtmz3(i)=yhatz3(i)
            yhtmx4(i)=yhatx4(i)
            yhtmy4(i)=yhaty4(i)
            yhtmz4(i)=yhatz4(i)
         enddo
      endif
c
      return
      end
      subroutine usrshl_ls (lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     set up lamina coordinate system for user defined shell
c
      include 'nlqparm'
      include 'nhisparm.inc'
      include 'shlioc.inc'
      common/aux13loc/
     1 zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     2 gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     3 gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     4 x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     5 x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      real x21(nlq),y21(nlq),z21(nlq)
      real x31(nlq),y31(nlq),z31(nlq)
      real x42(nlq),y42(nlq),z42(nlq)
c
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
      if (ioshl(60).le.1) then
c
c     Default coordinate system
c
         if (lenvec8.ne.0) then
c
         do i=lft,llt
            x21(i)=xh2(i)-xh1(i)
            y21(i)=yh2(i)-yh1(i)
            z21(i)=zh2(i)-zh1(i)
            x31(i)=xh3(i)-xh1(i)
            y31(i)=yh3(i)-yh1(i)
            z31(i)=zh3(i)-zh1(i)
            x42(i)=xh4(i)-xh2(i)
            y42(i)=yh4(i)-yh2(i)
            z42(i)=zh4(i)-zh2(i)
         enddo
         do i=lft,llt
            c1=y31(i)*z42(i)-z31(i)*y42(i)
            c2=z31(i)*x42(i)-x31(i)*z42(i)
            c3=x31(i)*y42(i)-y31(i)*x42(i)
            xl=1./sqrt(c1*c1+c2*c2+c3*c3+1.e-16)
            gl13(i)=c1*xl
            gl23(i)=c2*xl
            gl33(i)=c3*xl
         enddo
         do i=lft,llt
            xort=x21(i)*gl13(i)+y21(i)*gl23(i)+z21(i)*gl33(i)
            c1=x21(i)-gl13(i)*xort
            c2=y21(i)-gl23(i)*xort
            c3=z21(i)-gl33(i)*xort
            xl=1./sqrt(c1*c1+c2*c2+c3*c3+1.e-16)
            gl11(i)=c1*xl
            gl21(i)=c2*xl
            gl31(i)=c3*xl
         enddo
         do i=lft,llt
            gl12(i)=gl23(i)*gl31(i)-gl33(i)*gl21(i)
            gl22(i)=gl33(i)*gl11(i)-gl13(i)*gl31(i)
            gl32(i)=gl13(i)*gl21(i)-gl23(i)*gl11(i)
         enddo
c
         endif
c
         do i=lft,llt
            x21(i)=x2(i)-x1(i)
            y21(i)=y2(i)-y1(i)
            z21(i)=z2(i)-z1(i)
            x31(i)=x3(i)-x1(i)
            y31(i)=y3(i)-y1(i)
            z31(i)=z3(i)-z1(i)
            x42(i)=x4(i)-x2(i)
            y42(i)=y4(i)-y2(i)
            z42(i)=z4(i)-z2(i)
         enddo
         do i=lft,llt
            c1=y31(i)*z42(i)-z31(i)*y42(i)
            c2=z31(i)*x42(i)-x31(i)*z42(i)
            c3=x31(i)*y42(i)-y31(i)*x42(i)
            xl=1./sqrt(c1*c1+c2*c2+c3*c3+1.e-16)
            hl13(i)=c1*xl
            hl23(i)=c2*xl
            hl33(i)=c3*xl
         enddo
         do i=lft,llt
            xort=x21(i)*hl13(i)+y21(i)*hl23(i)+z21(i)*hl33(i)
            c1=x21(i)-hl13(i)*xort
            c2=y21(i)-hl23(i)*xort
            c3=z21(i)-hl33(i)*xort
            xl=1./sqrt(c1*c1+c2*c2+c3*c3+1.e-16)
            hl11(i)=c1*xl
            hl21(i)=c2*xl
            hl31(i)=c3*xl
         enddo
         do i=lft,llt
            hl12(i)=hl23(i)*hl31(i)-hl33(i)*hl21(i)
            hl22(i)=hl33(i)*hl11(i)-hl13(i)*hl31(i)
            hl32(i)=hl13(i)*hl21(i)-hl23(i)*hl11(i)
         enddo
c
      else
c
c     Node invariant coordinate system
c
         if (lenvec8.ne.0) then
c
         do i=lft,llt
            x31(i)=xh3(i)-xh1(i)
            y31(i)=yh3(i)-yh1(i)
            z31(i)=zh3(i)-zh1(i)
            x42(i)=xh4(i)-xh2(i)
            y42(i)=yh4(i)-yh2(i)
            z42(i)=zh4(i)-zh2(i)
         enddo
         do i=lft,llt
            xl=1./sqrt(x31(i)**2+y31(i)**2+z31(i)**2+1.e-16)
            x31(i)=x31(i)*xl
            y31(i)=y31(i)*xl
            z31(i)=z31(i)*xl
            xl=1./sqrt(x42(i)**2+y42(i)**2+z42(i)**2+1.e-16)
            x42(i)=x42(i)*xl
            y42(i)=y42(i)*xl
            z42(i)=z42(i)*xl
         enddo
         do i=lft,llt
            gl11(i)=x31(i)-x42(i)
            gl21(i)=y31(i)-y42(i)
            gl31(i)=z31(i)-z42(i)
            gl12(i)=x31(i)+x42(i)
            gl22(i)=y31(i)+y42(i)
            gl32(i)=z31(i)+z42(i)
         enddo
         do i=lft,llt
            xl=1./sqrt(gl11(i)**2+gl21(i)**2+gl31(i)**2+1.e-16)
            gl11(i)=gl11(i)*xl
            gl21(i)=gl21(i)*xl
            gl31(i)=gl31(i)*xl
            xl=1./sqrt(gl12(i)**2+gl22(i)**2+gl32(i)**2+1.e-16)
            gl12(i)=gl12(i)*xl
            gl22(i)=gl22(i)*xl
            gl32(i)=gl32(i)*xl
         enddo
         do i=lft,llt
            gl13(i)=gl21(i)*gl32(i)-gl31(i)*gl22(i)
            gl23(i)=gl31(i)*gl12(i)-gl11(i)*gl32(i)
            gl33(i)=gl11(i)*gl22(i)-gl21(i)*gl12(i)
         enddo
c
         endif
c
         do i=lft,llt
            x31(i)=x3(i)-x1(i)
            y31(i)=y3(i)-y1(i)
            z31(i)=z3(i)-z1(i)
            x42(i)=x4(i)-x2(i)
            y42(i)=y4(i)-y2(i)
            z42(i)=z4(i)-z2(i)
         enddo
         do i=lft,llt
            xl=1./sqrt(x31(i)**2+y31(i)**2+z31(i)**2+1.e-16)
            x31(i)=x31(i)*xl
            y31(i)=y31(i)*xl
            z31(i)=z31(i)*xl
            xl=1./sqrt(x42(i)**2+y42(i)**2+z42(i)**2+1.e-16)
            x42(i)=x42(i)*xl
            y42(i)=y42(i)*xl
            z42(i)=z42(i)*xl
         enddo
         do i=lft,llt
            hl11(i)=x31(i)-x42(i)
            hl21(i)=y31(i)-y42(i)
            hl31(i)=z31(i)-z42(i)
            hl12(i)=x31(i)+x42(i)
            hl22(i)=y31(i)+y42(i)
            hl32(i)=z31(i)+z42(i)
         enddo
         do i=lft,llt
            xl=1./sqrt(hl11(i)**2+hl21(i)**2+hl31(i)**2+1.e-16)
            hl11(i)=hl11(i)*xl
            hl21(i)=hl21(i)*xl
            hl31(i)=hl31(i)*xl
            xl=1./sqrt(hl12(i)**2+hl22(i)**2+hl32(i)**2+1.e-16)
            hl12(i)=hl12(i)*xl
            hl22(i)=hl22(i)*xl
            hl32(i)=hl32(i)*xl
         enddo
         do i=lft,llt
            hl13(i)=hl21(i)*hl32(i)-hl31(i)*hl22(i)
            hl23(i)=hl31(i)*hl12(i)-hl11(i)*hl32(i)
            hl33(i)=hl11(i)*hl22(i)-hl21(i)*hl12(i)
         enddo
c
      endif
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            gl11(i)=hl11(i)
            gl21(i)=hl21(i)
            gl31(i)=hl31(i)
            gl12(i)=hl12(i)
            gl22(i)=hl22(i)
            gl32(i)=hl32(i)
            gl13(i)=hl13(i)
            gl23(i)=hl23(i)
            gl33(i)=hl33(i)
         enddo
      endif
c
      return
      end
      subroutine usrshl_g2l(lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform coordinates and increments to local system
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq)
      common/aux13loc/
     1 zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     2 gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     3 gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     4 x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     5 x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c     Coordinates at t_{n+1}
c
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
      do i=lft,llt
         xx1(i)  =hl11(i)*x1(i)+hl21(i)*y1(i)+hl31(i)*z1(i)
         yy1(i)  =hl12(i)*x1(i)+hl22(i)*y1(i)+hl32(i)*z1(i)
         zz1(i)  =hl13(i)*x1(i)+hl23(i)*y1(i)+hl33(i)*z1(i)
      enddo
      do i=lft,llt
         xx2(i)  =hl11(i)*x2(i)+hl21(i)*y2(i)+hl31(i)*z2(i)
         yy2(i)  =hl12(i)*x2(i)+hl22(i)*y2(i)+hl32(i)*z2(i)
         zz2(i)  =hl13(i)*x2(i)+hl23(i)*y2(i)+hl33(i)*z2(i)
      enddo
      do i=lft,llt
         xx3(i)  =hl11(i)*x3(i)+hl21(i)*y3(i)+hl31(i)*z3(i)
         yy3(i)  =hl12(i)*x3(i)+hl22(i)*y3(i)+hl32(i)*z3(i)
         zz3(i)  =hl13(i)*x3(i)+hl23(i)*y3(i)+hl33(i)*z3(i)
      enddo
      do i=lft,llt
         xx4(i)  =hl11(i)*x4(i)+hl21(i)*y4(i)+hl31(i)*z4(i)
         yy4(i)  =hl12(i)*x4(i)+hl22(i)*y4(i)+hl32(i)*z4(i)
         zz4(i)  =hl13(i)*x4(i)+hl23(i)*y4(i)+hl33(i)*z4(i)
      enddo
c
c     Coordinates at t_{n+1/2}, only if accuracy option is on
c
      if (lenvec8.ne.0) then
c
      do i=lft,llt
         px1(i)  =gl11(i)*xh1(i)+gl21(i)*yh1(i)+gl31(i)*zh1(i)
         py1(i)  =gl12(i)*xh1(i)+gl22(i)*yh1(i)+gl32(i)*zh1(i)
         pz1(i)  =gl13(i)*xh1(i)+gl23(i)*yh1(i)+gl33(i)*zh1(i)
      enddo
      do i=lft,llt
         px2(i)  =gl11(i)*xh2(i)+gl21(i)*yh2(i)+gl31(i)*zh2(i)
         py2(i)  =gl12(i)*xh2(i)+gl22(i)*yh2(i)+gl32(i)*zh2(i)
         pz2(i)  =gl13(i)*xh2(i)+gl23(i)*yh2(i)+gl33(i)*zh2(i)
      enddo
      do i=lft,llt
         px3(i)  =gl11(i)*xh3(i)+gl21(i)*yh3(i)+gl31(i)*zh3(i)
         py3(i)  =gl12(i)*xh3(i)+gl22(i)*yh3(i)+gl32(i)*zh3(i)
         pz3(i)  =gl13(i)*xh3(i)+gl23(i)*yh3(i)+gl33(i)*zh3(i)
      enddo
      do i=lft,llt
         px4(i)  =gl11(i)*xh4(i)+gl21(i)*yh4(i)+gl31(i)*zh4(i)
         py4(i)  =gl12(i)*xh4(i)+gl22(i)*yh4(i)+gl32(i)*zh4(i)
         pz4(i)  =gl13(i)*xh4(i)+gl23(i)*yh4(i)+gl33(i)*zh4(i)
      enddo
c
      else
c
      do i=lft,llt
         px1(i)  =xx1(i)
         py1(i)  =yy1(i)
         pz1(i)  =zz1(i)
         px2(i)  =xx2(i)
         py2(i)  =yy2(i)
         pz2(i)  =zz2(i)
         px3(i)  =xx3(i)
         py3(i)  =yy3(i)
         pz3(i)  =zz3(i)
         px4(i)  =xx4(i)
         py4(i)  =yy4(i)
         pz4(i)  =zz4(i)
      enddo
c
      endif
c
c     Nodal rotation increments
c
      do i=lft,llt
         px5(i)  =gl11(i)*wxx1(i)+gl21(i)*wyy1(i)+gl31(i)*wzz1(i)
         py5(i)  =gl12(i)*wxx1(i)+gl22(i)*wyy1(i)+gl32(i)*wzz1(i)
         pz5(i)  =gl13(i)*wxx1(i)+gl23(i)*wyy1(i)+gl33(i)*wzz1(i)
      enddo
      do i=lft,llt
         px6(i)  =gl11(i)*wxx2(i)+gl21(i)*wyy2(i)+gl31(i)*wzz2(i)
         py6(i)  =gl12(i)*wxx2(i)+gl22(i)*wyy2(i)+gl32(i)*wzz2(i)
         pz6(i)  =gl13(i)*wxx2(i)+gl23(i)*wyy2(i)+gl33(i)*wzz2(i)
      enddo
      do i=lft,llt
         px7(i)  =gl11(i)*wxx3(i)+gl21(i)*wyy3(i)+gl31(i)*wzz3(i)
         py7(i)  =gl12(i)*wxx3(i)+gl22(i)*wyy3(i)+gl32(i)*wzz3(i)
         pz7(i)  =gl13(i)*wxx3(i)+gl23(i)*wyy3(i)+gl33(i)*wzz3(i)
      enddo
      do i=lft,llt
         px8(i)  =gl11(i)*wxx4(i)+gl21(i)*wyy4(i)+gl31(i)*wzz4(i)
         py8(i)  =gl12(i)*wxx4(i)+gl22(i)*wyy4(i)+gl32(i)*wzz4(i)
         pz8(i)  =gl13(i)*wxx4(i)+gl23(i)*wyy4(i)+gl33(i)*wzz4(i)
      enddo
c
c     Nodal translation increments
c
      do i=lft,llt
         dx5(i)  =gl11(i)*dx1(i)+gl21(i)*dy1(i)+gl31(i)*dz1(i)
         dy5(i)  =gl12(i)*dx1(i)+gl22(i)*dy1(i)+gl32(i)*dz1(i)
         dz5(i)  =gl13(i)*dx1(i)+gl23(i)*dy1(i)+gl33(i)*dz1(i)
      enddo
      do i=lft,llt
         dx6(i)  =gl11(i)*dx2(i)+gl21(i)*dy2(i)+gl31(i)*dz2(i)
         dy6(i)  =gl12(i)*dx2(i)+gl22(i)*dy2(i)+gl32(i)*dz2(i)
         dz6(i)  =gl13(i)*dx2(i)+gl23(i)*dy2(i)+gl33(i)*dz2(i)
      enddo
      do i=lft,llt
         dx7(i)  =gl11(i)*dx3(i)+gl21(i)*dy3(i)+gl31(i)*dz3(i)
         dy7(i)  =gl12(i)*dx3(i)+gl22(i)*dy3(i)+gl32(i)*dz3(i)
         dz7(i)  =gl13(i)*dx3(i)+gl23(i)*dy3(i)+gl33(i)*dz3(i)
      enddo
      do i=lft,llt
         dx8(i)  =gl11(i)*dx4(i)+gl21(i)*dy4(i)+gl31(i)*dz4(i)
         dy8(i)  =gl12(i)*dx4(i)+gl22(i)*dy4(i)+gl32(i)*dz4(i)
         dz8(i)  =gl13(i)*dx4(i)+gl23(i)*dy4(i)+gl33(i)*dz4(i)
      enddo
c
c     Assumes that any X-tra nodes are directional independent
c     so these are not transformed at all
c
      return
      end
      subroutine usrshl_fbl(fibl,iunf,lenvec8,lft,llt,iloc)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     gather length of fibers
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     &gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
c
      dimension fibl(9,*)
c
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/vect13loc/)
      if (iunf.eq.0) then
         do i=lft,llt
            thick(i)=fibl(1,i)
         enddo
      else
         do i=lft,llt
            thick(i)=fibl(1,i)
            fga(i)=fibl(2,i)
            fgb(i)=fibl(3,i)
            fgc(i)=fibl(4,i)
         enddo
c
         if (iloc.eq.0) then
c
         if (lenvec8.ne.0) then
c
         do i=lft,llt
            yht1=gl11(i)*yhtmx1(i)+gl21(i)*yhtmy1(i)+gl31(i)*yhtmz1(i)
            yht2=gl12(i)*yhtmx1(i)+gl22(i)*yhtmy1(i)+gl32(i)*yhtmz1(i)
            yht3=gl13(i)*yhtmx1(i)+gl23(i)*yhtmy1(i)+gl33(i)*yhtmz1(i)
            yhtmx1(i)=yht1
            yhtmy1(i)=yht2
            yhtmz1(i)=yht3
         enddo
         do i=lft,llt
            yht1=gl11(i)*yhtmx2(i)+gl21(i)*yhtmy2(i)+gl31(i)*yhtmz2(i)
            yht2=gl12(i)*yhtmx2(i)+gl22(i)*yhtmy2(i)+gl32(i)*yhtmz2(i)
            yht3=gl13(i)*yhtmx2(i)+gl23(i)*yhtmy2(i)+gl33(i)*yhtmz2(i)
            yhtmx2(i)=yht1
            yhtmy2(i)=yht2
            yhtmz2(i)=yht3
         enddo
         do i=lft,llt
            yht1=gl11(i)*yhtmx3(i)+gl21(i)*yhtmy3(i)+gl31(i)*yhtmz3(i)
            yht2=gl12(i)*yhtmx3(i)+gl22(i)*yhtmy3(i)+gl32(i)*yhtmz3(i)
            yht3=gl13(i)*yhtmx3(i)+gl23(i)*yhtmy3(i)+gl33(i)*yhtmz3(i)
            yhtmx3(i)=yht1
            yhtmy3(i)=yht2
            yhtmz3(i)=yht3
         enddo
         do i=lft,llt
            yht1=gl11(i)*yhtmx4(i)+gl21(i)*yhtmy4(i)+gl31(i)*yhtmz4(i)
            yht2=gl12(i)*yhtmx4(i)+gl22(i)*yhtmy4(i)+gl32(i)*yhtmz4(i)
            yht3=gl13(i)*yhtmx4(i)+gl23(i)*yhtmy4(i)+gl33(i)*yhtmz4(i)
            yhtmx4(i)=yht1
            yhtmy4(i)=yht2
            yhtmz4(i)=yht3
         enddo
c
         endif
c
         do i=lft,llt
            yht1=hl11(i)*yhatx1(i)+hl21(i)*yhaty1(i)+hl31(i)*yhatz1(i)
            yht2=hl12(i)*yhatx1(i)+hl22(i)*yhaty1(i)+hl32(i)*yhatz1(i)
            yht3=hl13(i)*yhatx1(i)+hl23(i)*yhaty1(i)+hl33(i)*yhatz1(i)
            yhtnx1(i)=yht1
            yhtny1(i)=yht2
            yhtnz1(i)=yht3
         enddo
         do i=lft,llt
            yht1=hl11(i)*yhatx2(i)+hl21(i)*yhaty2(i)+hl31(i)*yhatz2(i)
            yht2=hl12(i)*yhatx2(i)+hl22(i)*yhaty2(i)+hl32(i)*yhatz2(i)
            yht3=hl13(i)*yhatx2(i)+hl23(i)*yhaty2(i)+hl33(i)*yhatz2(i)
            yhtnx2(i)=yht1
            yhtny2(i)=yht2
            yhtnz2(i)=yht3
         enddo
         do i=lft,llt
            yht1=hl11(i)*yhatx3(i)+hl21(i)*yhaty3(i)+hl31(i)*yhatz3(i)
            yht2=hl12(i)*yhatx3(i)+hl22(i)*yhaty3(i)+hl32(i)*yhatz3(i)
            yht3=hl13(i)*yhatx3(i)+hl23(i)*yhaty3(i)+hl33(i)*yhatz3(i)
            yhtnx3(i)=yht1
            yhtny3(i)=yht2
            yhtnz3(i)=yht3
         enddo
         do i=lft,llt
            yht1=hl11(i)*yhatx4(i)+hl21(i)*yhaty4(i)+hl31(i)*yhatz4(i)
            yht2=hl12(i)*yhatx4(i)+hl22(i)*yhaty4(i)+hl32(i)*yhatz4(i)
            yht3=hl13(i)*yhatx4(i)+hl23(i)*yhaty4(i)+hl33(i)*yhatz4(i)
            yhtnx4(i)=yht1
            yhtny4(i)=yht2
            yhtnz4(i)=yht3
         enddo
c
         else
c
            do i=lft,llt
               yhtnx1(i)=yhatx1(i)
               yhtnx2(i)=yhatx2(i)
               yhtnx3(i)=yhatx3(i)
               yhtnx4(i)=yhatx4(i)
               yhtny1(i)=yhaty1(i)
               yhtny2(i)=yhaty2(i)
               yhtny3(i)=yhaty3(i)
               yhtny4(i)=yhaty4(i)
               yhtnz1(i)=yhatz1(i)
               yhtnz2(i)=yhatz2(i)
               yhtnz3(i)=yhatz3(i)
               yhtnz4(i)=yhatz4(i)
            enddo
c
         endif
         if (lenvec8.eq.0) then
            do i=lft,llt
               yhtmx1(i)=yhtnx1(i)
               yhtmy1(i)=yhtny1(i)
               yhtmz1(i)=yhtnz1(i)
               yhtmx2(i)=yhtnx2(i)
               yhtmy2(i)=yhtny2(i)
               yhtmz2(i)=yhtnz2(i)
               yhtmx3(i)=yhtnx3(i)
               yhtmy3(i)=yhtny3(i)
               yhtmz3(i)=yhtnz3(i)
               yhtmx4(i)=yhtnx4(i)
               yhtmy4(i)=yhtny4(i)
               yhtmz4(i)=yhtnz4(i)
            enddo
         endif
c
      endif
c
      return
      end
      subroutine usrshl_b(ietyp,xi,eta,zeta,lenvec8,lft,llt,iloc)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux13loc/
     &zet(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     &gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
c
      real n1,n2,n3,n4
      real dn1dxi,dn2dxi,dn3dxi,dn4dxi
      real dn1deta,dn2deta,dn3deta,dn4deta
c
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/vect13loc/)
      n1     =.25*(1.-xi)*(1.-eta)
      n2     =.25*(1.+xi)*(1.-eta)
      n3     =.25*(1.+xi)*(1.+eta)
      n4     =.25*(1.-xi)*(1.+eta)
c
      dn1dxi =-.25*(1.-eta)
      dn2dxi = .25*(1.-eta)
      dn3dxi = .25*(1.+eta)
      dn4dxi =-.25*(1.+eta)
c
      dn1deta=-.25*(1.-xi)
      dn2deta=-.25*(1.+xi)
      dn3deta= .25*(1.+xi)
      dn4deta= .25*(1.-xi)
c
      if (lenvec8.ne.0) then
c
      do l=1,4*(6+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=0.
               enddo
            enddo
         enddo
      enddo
c
      if (iloc.eq.0) then
c
      if (ietyp.eq.101) then
         call ushl_b101(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        px1,px2,px3,px4,py1,py2,py3,py4,pz1,pz2,pz3,pz4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_b102(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        px1,px2,px3,px4,py1,py2,py3,py4,pz1,pz2,pz3,pz4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_b103(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        px1,px2,px3,px4,py1,py2,py3,py4,pz1,pz2,pz3,pz4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_b104(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        px1,px2,px3,px4,py1,py2,py3,py4,pz1,pz2,pz3,pz4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_b105(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        px1,px2,px3,px4,py1,py2,py3,py4,pz1,pz2,pz3,pz4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      endif
c
      else
c
      if (ietyp.eq.101) then
         call ushl_b101(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xh1,xh2,xh3,xh4,yh1,yh2,yh3,yh4,zh1,zh2,zh3,zh4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_b102(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xh1,xh2,xh3,xh4,yh1,yh2,yh3,yh4,zh1,zh2,zh3,zh4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_b103(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xh1,xh2,xh3,xh4,yh1,yh2,yh3,yh4,zh1,zh2,zh3,zh4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_b104(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xh1,xh2,xh3,xh4,yh1,yh2,yh3,yh4,zh1,zh2,zh3,zh4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_b105(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xh1,xh2,xh3,xh4,yh1,yh2,yh3,yh4,zh1,zh2,zh3,zh4,
     6        xhdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtmx1,yhtmx2,yhtmx3,yhtmx4,
     9        yhtmy1,yhtmy2,yhtmy3,yhtmy4,
     .        yhtmz1,yhtmz2,yhtmz3,yhtmz4,
     .        gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     .        lft,llt)
      endif
c
      endif
c
      endif
c
      do l=1,4*(6+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  cmtrx(i,j,k,l)=0.
               enddo
            enddo
         enddo
      enddo
c
      if (iloc.eq.0) then
c
      if (ietyp.eq.101) then
         call ushl_b101(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_b102(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_b103(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_b104(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_b105(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      endif
c
      else
c
      if (ietyp.eq.101) then
         call ushl_b101(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_b102(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_b103(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_b104(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_b105(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn1deta,dn2deta,dn3deta,dn4deta,
     5        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6        xdof,
     7        thick,thick,fga,fgb,fgc,
     8        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     9        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        lft,llt)
      endif
c
      endif
c
      if (lenvec8.eq.0) then
      do l=1,4*(6+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
               enddo
            enddo
         enddo
      enddo
      do k=1,3
         do j=1,3
            do i=lft,llt
               gmtrx(i,j,k)=hmtrx(i,j,k)
            enddo
         enddo
      enddo
      endif
c
      return
      end
      subroutine usrshl_b2b(itaj,nxdof,wgt,lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform b-matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/aux13loc/
     & zet(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     & gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     & gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     & x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     & x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/prescloc/voltot(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/prescloc/)
      if (itaj.eq.0) then
         call usrshl_b2b_old(nxdof,wgt,lenvec8,lft,llt)
         return
      endif
c
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
         vol(i)=gmtrx(i,1,1)
         voltot(i)=voltot(i)+vol(i)*wgt
c
      enddo
c
      endif
c
      do i=lft,llt
c
         vol(i)=hmtrx(i,1,1)
         cvltot(i)=cvltot(i)+vol(i)*wgt
c
      enddo
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            voltot(i)=cvltot(i)
            gmtrx(i,1,1)=hmtrx(i,1,1)
         enddo
      endif
c
c     Compute "real" b-matrix
c
      if (lenvec8.eq.0) then
         do l=1,4*(6+NXDOFUE)
            do k=1,3
               do j=1,3
                  do i=lft,llt
                     bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
                  enddo
               enddo
            enddo
         enddo
      endif
c
      return
      end
      subroutine usrshl_b2b_old(nxdof,wgt,lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform b-matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/aux13loc/
     & zet(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     & gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     & gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     & x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     & x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/prescloc/voltot(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/prescloc/)
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
         vol(i)=gmtrx(i,1,1)*(gmtrx(i,2,2)*gmtrx(i,3,3)-
     1        gmtrx(i,3,2)*gmtrx(i,2,3))+
     2        gmtrx(i,2,1)*(gmtrx(i,3,2)*gmtrx(i,1,3)-
     3        gmtrx(i,1,2)*gmtrx(i,3,3))+
     4        gmtrx(i,3,1)*(gmtrx(i,1,2)*gmtrx(i,2,3)-
     5        gmtrx(i,2,2)*gmtrx(i,1,3))
         voltot(i)=voltot(i)+vol(i)*wgt
         gjac=1./vol(i)
         g11i=gmtrx(i,2,2)*gmtrx(i,3,3)-gmtrx(i,2,3)*gmtrx(i,3,2)
         g12i=gmtrx(i,3,2)*gmtrx(i,1,3)-gmtrx(i,3,3)*gmtrx(i,1,2)
         g13i=gmtrx(i,1,2)*gmtrx(i,2,3)-gmtrx(i,1,3)*gmtrx(i,2,2)
         g21i=gmtrx(i,2,3)*gmtrx(i,3,1)-gmtrx(i,2,1)*gmtrx(i,3,3)
         g22i=gmtrx(i,3,3)*gmtrx(i,1,1)-gmtrx(i,3,1)*gmtrx(i,1,3)
         g23i=gmtrx(i,1,3)*gmtrx(i,2,1)-gmtrx(i,1,1)*gmtrx(i,2,3)
         g31i=gmtrx(i,2,1)*gmtrx(i,3,2)-gmtrx(i,2,2)*gmtrx(i,3,1)
         g32i=gmtrx(i,3,1)*gmtrx(i,1,2)-gmtrx(i,3,2)*gmtrx(i,1,1)
         g33i=gmtrx(i,1,1)*gmtrx(i,2,2)-gmtrx(i,1,2)*gmtrx(i,2,1)
         gmtrx(i,1,1)=gjac*g11i
         gmtrx(i,2,1)=gjac*g21i
         gmtrx(i,3,1)=gjac*g31i
         gmtrx(i,1,2)=gjac*g12i
         gmtrx(i,2,2)=gjac*g22i
         gmtrx(i,3,2)=gjac*g32i
         gmtrx(i,1,3)=gjac*g13i
         gmtrx(i,2,3)=gjac*g23i
         gmtrx(i,3,3)=gjac*g33i
c
      enddo
c
      endif
c
      do i=lft,llt
c
         vol(i)=hmtrx(i,1,1)*(hmtrx(i,2,2)*hmtrx(i,3,3)-
     1        hmtrx(i,3,2)*hmtrx(i,2,3))+
     2        hmtrx(i,2,1)*(hmtrx(i,3,2)*hmtrx(i,1,3)-
     3        hmtrx(i,1,2)*hmtrx(i,3,3))+
     4        hmtrx(i,3,1)*(hmtrx(i,1,2)*hmtrx(i,2,3)-
     5        hmtrx(i,2,2)*hmtrx(i,1,3))
         cvltot(i)=cvltot(i)+vol(i)*wgt
         hjac=1./vol(i)
         h11i=hmtrx(i,2,2)*hmtrx(i,3,3)-hmtrx(i,2,3)*hmtrx(i,3,2)
         h12i=hmtrx(i,3,2)*hmtrx(i,1,3)-hmtrx(i,3,3)*hmtrx(i,1,2)
         h13i=hmtrx(i,1,2)*hmtrx(i,2,3)-hmtrx(i,1,3)*hmtrx(i,2,2)
         h21i=hmtrx(i,2,3)*hmtrx(i,3,1)-hmtrx(i,2,1)*hmtrx(i,3,3)
         h22i=hmtrx(i,3,3)*hmtrx(i,1,1)-hmtrx(i,3,1)*hmtrx(i,1,3)
         h23i=hmtrx(i,1,3)*hmtrx(i,2,1)-hmtrx(i,1,1)*hmtrx(i,2,3)
         h31i=hmtrx(i,2,1)*hmtrx(i,3,2)-hmtrx(i,2,2)*hmtrx(i,3,1)
         h32i=hmtrx(i,3,1)*hmtrx(i,1,2)-hmtrx(i,3,2)*hmtrx(i,1,1)
         h33i=hmtrx(i,1,1)*hmtrx(i,2,2)-hmtrx(i,1,2)*hmtrx(i,2,1)
         hmtrx(i,1,1)=hjac*h11i
         hmtrx(i,2,1)=hjac*h21i
         hmtrx(i,3,1)=hjac*h31i
         hmtrx(i,1,2)=hjac*h12i
         hmtrx(i,2,2)=hjac*h22i
         hmtrx(i,3,2)=hjac*h32i
         hmtrx(i,1,3)=hjac*h13i
         hmtrx(i,2,3)=hjac*h23i
         hmtrx(i,3,3)=hjac*h33i
c
      enddo
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            voltot(i)=cvltot(i)
         enddo
         do k=1,3
            do j=1,3
               do i=lft,llt
                  gmtrx(i,j,k)=hmtrx(i,j,k)
               enddo
            enddo
         enddo
      endif
c
c     Compute "real" b-matrix
c
      if (lenvec8.ne.0) then
c
      ndtot=4*(6+nxdof)
      do l=1,ndtot
         do j=1,3
            do i=lft,llt
c
               b1=bmtrx(i,j,1,l)*gmtrx(i,1,1)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,1)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,1)
               b2=bmtrx(i,j,1,l)*gmtrx(i,1,2)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,2)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,2)
               b3=bmtrx(i,j,1,l)*gmtrx(i,1,3)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,3)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,3)
               bmtrx(i,j,1,l)=b1
               bmtrx(i,j,2,l)=b2
               bmtrx(i,j,3,l)=b3
c
            enddo
         enddo
      enddo
c
      endif
c
c     Compute "real" c-matrix
c
      ndtot=4*(6+nxdof)
      do l=1,ndtot
         do j=1,3
            do i=lft,llt
c
               c1=cmtrx(i,j,1,l)*hmtrx(i,1,1)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,1)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,1)
               c2=cmtrx(i,j,1,l)*hmtrx(i,1,2)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,2)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,2)
               c3=cmtrx(i,j,1,l)*hmtrx(i,1,3)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,3)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,3)
               cmtrx(i,j,1,l)=c1
               cmtrx(i,j,2,l)=c2
               cmtrx(i,j,3,l)=c3
c
            enddo
         enddo
      enddo
c
      if (lenvec8.eq.0) then
      do l=1,4*(6+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
               enddo
            enddo
         enddo
      enddo
      endif
c
      return
      end
      subroutine usrshl_str(nxdof,lft,llt,iloc)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute strain increments and spin
c     increments for user defined shell
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux2loc/dstrn(nlq,6),
     1 wdt(nlq,3),einc(nlq)
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/bel7loc/)
      ndtot=4*(6+nxdof)
c
      do j=1,6
c
         if (j.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=bmtrx(i,j,j,k)
               enddo
            enddo
         else
            j1=2
            j2=1
            if (j.eq.5) then
               j1=3
               j2=2
            elseif (j.eq.6) then
               j1=1
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=bmtrx(i,j1,j2,k)+bmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
         if (iloc.eq.0) then
c
         do i=lft,llt
c
            dstrn(i,j)=
     1           bvec(i,1)*dx5(i)+
     2           bvec(i,2)*dy5(i)+
     3           bvec(i,3)*dz5(i)+
     4           bvec(i,4)*px5(i)+
     5           bvec(i,5)*py5(i)+
     6           bvec(i,6)*pz5(i)+
     7           bvec(i,7+nxdof)*dx6(i)+
     8           bvec(i,8+nxdof)*dy6(i)+
     9           bvec(i,9+nxdof)*dz6(i)+
     .           bvec(i,10+nxdof)*px6(i)+
     1           bvec(i,11+nxdof)*py6(i)+
     2           bvec(i,12+nxdof)*pz6(i)+
     3           bvec(i,13+2*nxdof)*dx7(i)+
     4           bvec(i,14+2*nxdof)*dy7(i)+
     5           bvec(i,15+2*nxdof)*dz7(i)+
     6           bvec(i,16+2*nxdof)*px7(i)+
     7           bvec(i,17+2*nxdof)*py7(i)+
     8           bvec(i,18+2*nxdof)*pz7(i)+
     9           bvec(i,19+3*nxdof)*dx8(i)+
     .           bvec(i,20+3*nxdof)*dy8(i)+
     1           bvec(i,21+3*nxdof)*dz8(i)+
     2           bvec(i,22+3*nxdof)*px8(i)+
     3           bvec(i,23+3*nxdof)*py8(i)+
     4           bvec(i,24+3*nxdof)*pz8(i)
c
         enddo
c
         else
c
         do i=lft,llt
c
            dstrn(i,j)=
     1           bvec(i,1)*dx1(i)+
     2           bvec(i,2)*dy1(i)+
     3           bvec(i,3)*dz1(i)+
     4           bvec(i,4)*wxx1(i)+
     5           bvec(i,5)*wyy1(i)+
     6           bvec(i,6)*wzz1(i)+
     7           bvec(i,7+nxdof)*dx2(i)+
     8           bvec(i,8+nxdof)*dy2(i)+
     9           bvec(i,9+nxdof)*dz2(i)+
     .           bvec(i,10+nxdof)*wxx2(i)+
     1           bvec(i,11+nxdof)*wyy2(i)+
     2           bvec(i,12+nxdof)*wzz2(i)+
     3           bvec(i,13+2*nxdof)*dx3(i)+
     4           bvec(i,14+2*nxdof)*dy3(i)+
     5           bvec(i,15+2*nxdof)*dz3(i)+
     6           bvec(i,16+2*nxdof)*wxx3(i)+
     7           bvec(i,17+2*nxdof)*wyy3(i)+
     8           bvec(i,18+2*nxdof)*wzz3(i)+
     9           bvec(i,19+3*nxdof)*dx4(i)+
     .           bvec(i,20+3*nxdof)*dy4(i)+
     1           bvec(i,21+3*nxdof)*dz4(i)+
     2           bvec(i,22+3*nxdof)*wxx4(i)+
     3           bvec(i,23+3*nxdof)*wyy4(i)+
     4           bvec(i,24+3*nxdof)*wzz4(i)
c
         enddo
c
         endif
c
         if (nxdof.gt.0) then
            do k=1,nxdof
               k1=6+k
               k2=12+nxdof+k
               k3=18+2*nxdof+k
               k4=24+3*nxdof+k
               do i=lft,llt
c
                  dstrn(i,j)=dstrn(i,j)+
     1                 bvec(i,k1)*dxdof(i,1,k)+
     2                 bvec(i,k2)*dxdof(i,2,k)+
     3                 bvec(i,k3)*dxdof(i,3,k)+
     4                 bvec(i,k4)*dxdof(i,4,k)
c
               enddo
            enddo
         endif
c
         if (j.gt.3) then
c
            jj=1
            if (j.eq.5) jj=3
            if (j.eq.6) jj=2
c
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=.5*(bmtrx(i,j1,j2,k)-bmtrx(i,j2,j1,k))
               enddo
            enddo
c
            if (iloc.eq.0) then
c
            do i=lft,llt
c
               wdt(i,jj)=
     1              bvec(i,1)*dx5(i)+
     2              bvec(i,2)*dy5(i)+
     3              bvec(i,3)*dz5(i)+
     4              bvec(i,4)*px5(i)+
     5              bvec(i,5)*py5(i)+
     6              bvec(i,6)*pz5(i)+
     7              bvec(i,7+nxdof)*dx6(i)+
     8              bvec(i,8+nxdof)*dy6(i)+
     9              bvec(i,9+nxdof)*dz6(i)+
     .              bvec(i,10+nxdof)*px6(i)+
     1              bvec(i,11+nxdof)*py6(i)+
     2              bvec(i,12+nxdof)*pz6(i)+
     3              bvec(i,13+2*nxdof)*dx7(i)+
     4              bvec(i,14+2*nxdof)*dy7(i)+
     5              bvec(i,15+2*nxdof)*dz7(i)+
     6              bvec(i,16+2*nxdof)*px7(i)+
     7              bvec(i,17+2*nxdof)*py7(i)+
     8              bvec(i,18+2*nxdof)*pz7(i)+
     9              bvec(i,19+3*nxdof)*dx8(i)+
     .              bvec(i,20+3*nxdof)*dy8(i)+
     1              bvec(i,21+3*nxdof)*dz8(i)+
     2              bvec(i,22+3*nxdof)*px8(i)+
     3              bvec(i,23+3*nxdof)*py8(i)+
     4              bvec(i,24+3*nxdof)*pz8(i)
c
            enddo
c
            else
c
            do i=lft,llt
c
               wdt(i,jj)=
     1              bvec(i,1)*dx1(i)+
     2              bvec(i,2)*dy1(i)+
     3              bvec(i,3)*dz1(i)+
     4              bvec(i,4)*wxx1(i)+
     5              bvec(i,5)*wyy1(i)+
     6              bvec(i,6)*wzz1(i)+
     7              bvec(i,7+nxdof)*dx2(i)+
     8              bvec(i,8+nxdof)*dy2(i)+
     9              bvec(i,9+nxdof)*dz2(i)+
     .              bvec(i,10+nxdof)*wxx2(i)+
     1              bvec(i,11+nxdof)*wyy2(i)+
     2              bvec(i,12+nxdof)*wzz2(i)+
     3              bvec(i,13+2*nxdof)*dx3(i)+
     4              bvec(i,14+2*nxdof)*dy3(i)+
     5              bvec(i,15+2*nxdof)*dz3(i)+
     6              bvec(i,16+2*nxdof)*wxx3(i)+
     7              bvec(i,17+2*nxdof)*wyy3(i)+
     8              bvec(i,18+2*nxdof)*wzz3(i)+
     9              bvec(i,19+3*nxdof)*dx4(i)+
     .              bvec(i,20+3*nxdof)*dy4(i)+
     1              bvec(i,21+3*nxdof)*dz4(i)+
     2              bvec(i,22+3*nxdof)*wxx4(i)+
     3              bvec(i,23+3*nxdof)*wyy4(i)+
     4              bvec(i,24+3*nxdof)*wzz4(i)
c
            enddo
c
            endif
c
            if (nxdof.gt.0) then
               do k=1,nxdof
                  k1=6+k
                  k2=12+nxdof+k
                  k3=18+2*nxdof+k
                  k4=24+3*nxdof+k
                  do i=lft,llt
c
                     wdt(i,jj)=wdt(i,jj)+
     1                    bvec(i,k1)*dxdof(i,1,k)+
     2                    bvec(i,k2)*dxdof(i,2,k)+
     3                    bvec(i,k3)*dxdof(i,3,k)+
     4                    bvec(i,k4)*dxdof(i,4,k)
c
                  enddo
               enddo
            endif
c
         endif
c
      enddo
c
      return
      end
      subroutine usrshl_con(nmtcon,auxvec,cm,lav,mte,nip,ipt,
     . capa,mxe,lft,llt,nnm1,rcoor,scoor,tcoor,ipt_thk,
     . ihgf,vol,nhsv,iunf,eosp)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      integer
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10,
     5 nu01,nu02,nu03,nu04,nu05,nu06,nu07,nu08,nu09,nu10,
     6 nd01,nd02,nd03,nd04,nd05,nd06,nd07,nd08,nd09,nd10,
     7 ntbqs
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10,
     5 nu01,nu02,nu03,nu04,nu05,nu06,nu07,nu08,nu09,nu10,
     6 nd01,nd02,nd03,nd04,nd05,nd06,nd07,nd08,nd09,nd10,
     7 ntbqs
c
      integer*8 dm_hmtnum,dm_hnfegp
      integer*8 dm_bmtnum,dm_bnfegp
      integer*8 dm_smtnum,dm_snfegp
      integer*8 dm_tmtnum,dm_tnfegp
c
      common/dmbk05/
     1 dm_hmtnum,dm_hnfegp,
     2 dm_bmtnum,dm_bnfegp,
     3 dm_smtnum,dm_snfegp,
     4 dm_tmtnum,dm_tnfegp
      integer n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n44a,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,n81,n82,locend,iname,lendf
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n44a,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,n81,n82,locend,iname,lendf
      integer n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr,n4i,n4j,n4k,n4l
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr,n4i,n4j,n4k,n4l
      integer lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b,lc7c,lc7d,lc7e,lc7f,lc7g,lc7h,lc7i,lc7j,lc7k,lc7l
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b,lc7c,lc7d,lc7e,lc7f,lc7g,lc7h,lc7i,lc7j,lc7k,lc7l
c
c     dynamic memory allocation stuff.  This has to be a C style 
c     include because of the use of integer*8
c
      integer i_mem(0:1)
      real r_mem(0:1)
      integer*8 i8_mem(0:1)
      real*8 r8_mem(0:1)
      real*8 real8_mem
      integer*8 m_mem(0:1)
      common/dynmem/real8_mem(0:1)
      equivalence (i_mem,r_mem,i8_mem,r8_mem,real8_mem,m_mem)
      integer*8 mem_alloc, mems_alloc,mem_realloc, memh_ptr
      integer mem_length,memh_alloc,memh_realloc,memh_length
      integer memh_enum,memh_enums,memh_attach
c      integer mem_newmark,mem_getmark,memh_getmark
      integer*8 memh_detach,memh_ralloc
      integer*8 m_to_m8
      integer*8 m_to_mm
      external mem_alloc, mems_alloc, mem_realloc, mem_length
      external memh_alloc, memh_realloc, memh_length, memh_ptr
      external memh_enum,memh_attach,memh_detach,memh_ralloc
      external memh_enums
      external m_to_m8
      external m_to_mm
      integer*8 dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr
      integer*8 dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma
      integer*8 dm_xtpz,dm_disp
      integer*8 dm_me1,dm_me2,dm_me3
      integer*8 dm_masp,dm_masr
      integer*8 dm_xrb,dm_yrb,dm_zrb
      integer*8 dm_axrb,dm_ayrb,dm_azrb
      integer*8 dm_rbfx,dm_rbfy,dm_rbfz
      integer*8 dm_n45,dm_n46,dm_n47
      integer*8 dm_rwsx
      integer len_n45,len_n46,len_n47
      common/dynmem1/ 
     . dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr,
     . dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma,
     . dm_xtpz,dm_disp,
     . dm_me1,dm_me2,dm_me3,
     . dm_masp,dm_masr,
     . dm_xrb,dm_yrb,dm_zrb,
     . dm_axrb,dm_ayrb,dm_azrb,
     . dm_rbfx,dm_rbfy,dm_rbfz,
     . dm_n45,dm_n46,dm_n47,dm_rwsx
      common/dynmem1l/len_n45,len_n46,len_n47
c
      integer*8 mem_allocm,mems_allocm,mem_reallocm,mem_loc
      integer memh_allocm,memh_reallocm
      external mem_allocm,mems_allocm,mem_reallocm,mem_loc
      external memh_allocm,memh_reallocm
      integer*8 mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      integer*8 mem_reallocmchk,mem_reallocchk
      integer memh_allocmchk,memh_allocchk
      external mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      external mem_reallocmchk,mem_reallocchk
      external memh_allocmchk,memh_allocchk
      include 'implicit1.inc'
      include 'iounits.inc'
      include 'memaia.inc'
      include 'nhisparm.inc'
      include 'oasys_ceap.inc'
      include 'sbelt.inc'
      include 'shlioc.inc'
      include 'subcyc.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR znnc->nnc, zlczc->lczc
c
      real znnc,zlczc
      integer
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/znnc, zlczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
c
      common/aux13loc/
     1 zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq)
      common/aux14loc/ax(nlq,7+NHISVAR)
      common/bel8loc/uehisv(nlq,2*(NHISVUE+45))
      common/bel8aloc/iuehup(nlq,2*(NHISVUE+45))
      common/bk12loc/b12,b2,qhg,qhgm,qhgb,qhgw
      common/maxsvloc/mxsave,ipt_type(300)
      common/rayleiloc/betav
      common/shloptloc/ibelyt
      common/sorterloc/nnc,lczc
      common/vect8loc/dsave(nlq,6,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      common/bk01/itherm,itemp,ntmp0,ntmp1,itempan,itempdr,itmpe(44)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1 ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2 grvity,idirgv,nodspc,nspcor
      common/feedbck/ifdbck(41)
      common/numcpu/ncpu,ncpua,ncpub,lenvec(8)
      common/shlopt/istrn,istupd,ibelyts,miter,wrpang
      common/slcntr/islcnt(50)
c
      dimension cm(48,1),auxvec(1),eosp(48,1)
      dimension ss1(nlq),ss2(nlq),ss3(nlq),ss4(nlq),ss5(nlq),ss6(nlq)
      dimension vol(*),thck(nlq),bulkq(nlq)
c
      logical iamusrmat
      external iamusrmat
c
c     Gather history variables
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/bk12loc/)
c$omp threadprivate (/maxsvloc/)
c$omp threadprivate (/rayleiloc/)
c$omp threadprivate (/shloptloc/)
c$omp threadprivate (/sorterloc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/bel8loc/)
c$omp threadprivate (/bel8aloc/)
c$omp threadprivate (/vect8loc/)
c$omp threadprivate (/aux13loc/)
      lavloc=(ipt-1)*nmtcon+lav
      do j=1,nmtcon
         do i=lft,llt
            ax(i,j)=auxvec(lavloc+(i-lft)*nip*nmtcon+(j-1))
         enddo
      enddo
c
c     Special treatment for material 98
c
      iscl=sign(1,mte)
      mte =abs(mte)
c
      if (is17loc.eq.1.or.ihgf.eq.3) then
c
c     Tangent stiffness preparations
c
         call cmatsi (cm,capa,ipt,lft,llt,mte,a(n8),a(n9),a(ntmp0+1),
     .        a(n19))
      endif
c
c     Stiffness Rayleigh damping
c
      if (abs(betav).gt.0.000)
     .     call rydmp1 (lft,llt)
      if (abs(betav).gt.0.000) then
      call rydmp1 (lft,llt)
      if (lenvec(5).eq.1)
     .call rydmp2_mov
     .  (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),ax(1,nmtcon-3),
     .           ax(1,nmtcon-2),ax(1,nmtcon-1),ax(1,nmtcon  ),
     .           ss1,ss2,ss3,ss4,ss5,ss6)
      if (ioshl(67).ne.0) call bulkq_mov(lft,llt,ax(1,nmtcon-6),bulkq)
      ioffbq=6
      else
      ioffbq=0
      if (ioshl(67).ne.0) call bulkq_mov(lft,llt,ax(1,nmtcon  ),bulkq)
      endif
c
c     Staged construction
c
      nnm1_store = nnm1
      if (ceap_i_cnt(13).gt.0.or.ceap_i_cnt(12).gt.0)
     &      call ceap_stiffen_1 (lft,llt,'shell')
c
c     Special treatment for user defined integration
c
      npt=ipt_type(mte)
      if (npt.eq.0) npt=ipt
c
c     Material routines
c
      if (mte.eq.1) then
      call shl1s (cm,capa,lft,llt)
      elseif (mte.eq.24)  then
      if (miter.eq.0) then
      call sh24sc (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (miter.eq.1) then
      call shl24s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (miter.eq.2) then
      call sh24si (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      endif
      elseif (mte.eq.3)  then
      if (miter.le.1) then
      call shl3s (cm,capa,lft,llt,npt)
      elseif (miter.eq.2) then
      call sh3si (cm,capa,lft,llt,npt)
      endif
      elseif (mte.eq.2)  then
      call shl2s (cm,capa,lft,llt)
      elseif (mte.eq.37) then
      call shl37s(cm,capa,lft,llt,a(n8),a(n9))
      elseif (mte.eq.4)  then
      call shl4s (cm,a(ntmp0+1),a(n19),lft,llt,nnm1,rcoor,scoor,tcoor)
      elseif (mte.eq.6) then
      call shl6s (cm,capa,lft,llt,a(islcnt(16)),nnm1,rcoor,scoor,tcoor)
      elseif (mte.eq.7) then
      call shl7s (cm,capa,lft,llt)
      elseif (mte.eq.12)  then
      call shl12s (cm,capa,lft,llt)
      elseif (mte.eq.15)  then
      call shl15s (cm,capa,lft,llt,npt)
      elseif (mte.eq.98)  then
      if (nip.eq.1.and.ipt.eq.1.or.iscl.eq.-1) then
      call shl98r (cm,capa,lft,llt,ipt)
      else
      call shl98s (cm,capa,lft,llt,npt)
      endif
      elseif (mte.eq.99)  then
      call shl99s (cm,capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)))
      elseif (mte.eq.18)  then
      call shl18s (cm,capa,lft,llt,npt)
      elseif (mte.eq.19) then
      if (miter.eq.0) then
      call sh19sc (cm,a(n8),a(n9),capa,lft,llt,npt,a(islcnt(16)))
      elseif (miter.eq.1) then
      call shl19s (cm,a(n8),a(n9),capa,lft,llt,npt,a(islcnt(16)))
      elseif (miter.eq.2) then
      call sh19si (cm,a(n8),a(n9),capa,lft,llt,npt,a(islcnt(16)))
      endif
      elseif (mte.eq.21) then
      call shl21s(cm,capa,a(ntmp0+1),a(n19),lft,llt,
     1        rcoor,scoor,tcoor,nnm1)
      elseif (mte.eq.22) then
      call shl22s(cm,capa,lft,llt,npt)
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))+lc2
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),r_mem(dm_x),lft,llt,
     1        rcoor,scoor,tcoor,nnm1)
      elseif (mte.eq.27) then
      call shl27s (cm,capa,lft,llt)
      elseif (mte.eq.28)  then
      call shl28s (cm,capa,lft,llt)
      elseif (mte.eq.30)  then
      call shl30s (cm,capa,lft,llt)
      elseif (mte.eq.32)  then
      call shl32s (cm,capa,ipt_thk,lft,llt)
      elseif (mte.eq.33) then
      call shl33s(cm,capa,lft,llt,npt,a(islcnt(16)))
      elseif (mte.eq.34) then
      call shl34s(cm,capa,lft,llt,a(n19),a(lbe4),a(lbe2d))
      elseif (mte.eq.36) then
      call shl36s(cm,capa,lft,llt,npt,a(n8),a(n9),a(ifdbck(31)),
     . a(islcnt(16)),a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     . nnm1,rcoor,scoor,tcoor,ipt_thk,nip,a(ns13+numels+nnm1),
     . a(n4b))
      elseif (mte.eq.38) then
      call shl38s (cm,capa,lft,llt)
      elseif (mte.eq.39) then
      call shl39s(a(islcnt(16)),cm,capa,lft,llt,a(n8),a(n9))
      elseif (mte.eq.40) then
      call shl40s (cm,a(n8),a(n9),capa,lft,llt,npt)
      elseif (mte.eq.51) then
      call shl51s(cm,capa,lft,llt)
      elseif (mte.eq.52) then
      call shl52s(cm,capa,lft,llt,npt)
      elseif (mte.eq.54)  then
      call shl54s (cm,capa,nnm1,a(ns13+numels),a(ns07+numels),lft,llt,
     . a(ns21),npt,nip,ipt_thk,a(n4b),a(n8),a(n9),a(islcnt(16)))
      elseif (mte.eq.55)  then
      call shl55s (cm,capa,nnm1,a(ns13+numels),a(ns07+numels),lft,llt,
     . a(ns21),npt,nip)
      elseif (mte.eq.56)  then
      call shl56s (cm,capa,nnm1,a(ns13+numels),a(ns07+numels),lft,llt,
     . a(ns21),npt,nip)
      elseif (mte.eq.58)  then
      call shl58s (cm,capa,nnm1,a(ns13+numels),a(ns07+numels),lft,llt,
     . a(ns21),npt,nip,a(n4b),a(n8),a(n9),a(islcnt(16)),a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1))
      elseif (mte.eq.59)  then
      call shl59s (cm,capa,nnm1,a(ns13+numels),a(ns07+numels),lft,llt,
     . a(ns21),npt,nip)
      elseif (mte.eq.60) then
      call shl60s(cm,capa,a(n19),a(ntmp0+1),lft,llt)
      elseif (mte.eq.64) then
c     guo start spf
      call shl64s (cm,a(n8),a(n9),capa,lft,llt,a(islcnt(16)),
     .  a(ns13+numels+nnm1+1),npt,nip)
c     guo end
      elseif (mte.eq.65) then
      call shl65s (cm,capa,lft,llt,npt)
      elseif (mte.eq.76) then
      call shl76s(cm,capa,eosp,lft,llt)
      elseif (mte.eq.77) then
      call shl77s(cm,a(n4b),capa,lft,llt)
      elseif (mte.eq.81) then
      call shl81s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)))
      elseif (mte.eq.82) then
      if (a(lc12).ge.0.) then
      call shl82s_rcdc (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),
     . r_mem(dm_x),a(lc12+numnp),a(lc12+2*numnp),a(lc12))
      else
      call shl82s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)))
      endif
      elseif (mte.eq.86) then
      call shl86s (cm,cm(1+nmmat*48,1),capa,lft,llt)
      elseif (mte.eq.88) then
      call shl88s (cm,capa,lft,llt)
      elseif (mte.eq.89)  then
      call shl89s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)))
      elseif (mte.eq.91) then
      call shl91s (cm,capa,lft,llt,ipt)
      elseif (mte.eq.92) then
      call shl92s (cm,capa,lft,llt,ipt)
      elseif (mte.eq.101) then
      call shl101s(cm,capa,lft,llt,npt,a(islcnt(16)))
      elseif (mte.eq.103) then
      call shl103s(cm,a(n4b),capa,lft,llt,npt,a(n8),a(n9),a(islcnt(16)),
     .        mxe,a(n8+2*nlcur+1),a(n8+3*nlcur+1),nnm1)
      elseif (mte.eq.104) then
      call shl104s(cm,capa,lft,llt,npt,a(n8),a(n9),a(islcnt(16)))
      elseif (mte.eq.105) then
      call shl105s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)))
      elseif (mte.eq.106) then
      call shl106s (cm,capa,a(ntmp0+1),a(n19),lft,llt,a(islcnt(16)),
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),npt,a(n9),a(n8),nnm1,rcoor,
     . scoor,tcoor)
      elseif (mte.eq.107) then
      call shl107s (cm,capa,lft,llt,npt)
      elseif (mte.eq.108) then
      call shl108s (cm,capa,lft,llt,a(islcnt(16)))
      elseif (mte.eq.109) then
      call shl109s (cm,capa,lft,llt,npt,a(islcnt(16)))
      elseif (mte.eq.113) then
      call shl113s(cm,capa,lft,llt,nnm1,rcoor,scoor,tcoor,a(islcnt(16)))
      elseif (mte.eq.114) then
      call shl114s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
c---------Yun Huang 11/18/2006
      elseif (mte.eq.115) then
      call shl115s (cm,capa,lft,llt)
c---------------------------
      elseif (mte.eq.116)  then
      call shl117s (cm,capa,lft,llt)
      elseif (mte.eq.117)  then
      call shl117s (cm,capa,lft,llt)
      elseif (mte.eq.118)  then
      call shl118s (cm,capa,lft,llt)
      elseif (mte.eq.120) then
      call shl120s (cm,capa,lft,llt,npt,a(islcnt(16)),a(n8+2*nlcur+1),
     + a(n8+3*nlcur+1),r_mem(dm_x),a(n4b),ipt_thk)
      elseif (mte.eq.122) then
      call shl122s(cm,capa,lft,llt,npt,a(n8),a(n9),
     .     a(islcnt(16)),a(ifdbck(31)))
      elseif (mte.eq.123) then
      call shl123s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (mte.eq.124) then
      call shl124s (cm,a(n4b),a(n8),a(n9),capa,lft,llt,npt,
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),a(islcnt(16)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (mte.eq.224) then
      call shl224s (cm,a(n4b),a(n8),a(n9),capa,lft,llt,npt,
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),
     . ipt_thk,rcoor,scoor,tcoor)
      elseif (mte.eq.225) then
      call shl225s (cm,a(n8),a(n9),capa,lft,llt,npt,
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),
     . ipt_thk,nip,a(ns13+numels+nnm1))
      elseif (mte.eq.125) then
      call shl125s(cm,capa,lft,llt,a(n8),a(n9))
      elseif (mte.eq.133) then
      call shl133s (cm,capa,lft,llt,npt,a(islcnt(16)),
     .        a(n8+2*nlcur+1),a(n8+3*nlcur+1),a(n4b),
     .        nnm1,rcoor,scoor,tcoor,ipt_thk,nip,a(ns13+numels+nnm1))
      elseif (mte.eq.242) then
      call shl242s (cm,capa,lft,llt,npt,a(islcnt(16)),
     .        a(n8+2*nlcur+1),a(n8+3*nlcur+1))
      elseif (mte.eq.135) then
      call shl135s(cm,capa,lft,llt,npt,a(n8),a(n9),a(islcnt(16)))
      elseif (mte.eq.136) then
      call shl136s (cm,a(n4b),capa,lft,llt,a(n8),a(n9))
      elseif (mte.eq.137) then
      call shl137s (cm,capa,lft,llt,a(n8),a(n9))
      elseif (mte.eq.181) then
      call shl181s (cm,a(n8),a(n9),lft,llt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),capa)
      elseif (mte.eq.182) then
      call shl182s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),a(n4b),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (mte.eq.130)  then
      call shl130s (cm,capa,lft,llt)
      elseif (mte.eq.134) then
      call shl134s (cm,capa,lft,llt)
      elseif (mte.eq.253) then
      lthrpr=nint(cm(48,mxe))+lc2
      call shl253s (cm,capa,lft,llt,ipt,a(lthrpr))
      elseif (mte.eq.153)  then
      lthrpr=nint(cm(48,mxe))+lc2
      call shl153s (cm,capa,lft,llt,npt,a(n8),a(n9))
      elseif (mte.eq.157) then
      call shl157s (cm,capa,lft,llt,a(islcnt(16)),a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1))
      elseif (mte.eq.165)  then
      call shl165s (cm,capa,lft,llt,npt)
      elseif (mte.eq.158)  then
      call shl158s(cm,a(n4b),capa,nnm1,a(ns13+numels),a(ns07+numels),
     . lft,llt,a(ns21),npt,nip)
      elseif (mte.eq.162) then
      lcmaux=nint(cm(48,mxe))+lc2
      call shl162 (cm,capa,lft,llt,npt,nnm1,a(ns13+numels),a(lcmaux))
      elseif (mte.eq.170)  then
      call shl170s (cm,a(n8),a(n9),a(ifdbck(31)),capa,lft,llt)
      elseif (mte.eq.188) then
      call shl188s (cm,capa,a(ntmp0+1),a(n19),lft,llt,a(islcnt(16)),
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),nnm1,rcoor,scoor,tcoor)
      elseif (mte.eq.194)  then
      call shl194s (cm,capa,lft,llt)
      elseif (mte.eq.213) then
      call shl213s(cm,capa,lft,llt,a(n8),a(n9),a(islcnt(16)),
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),npt,nip,ipt_thk,a(n4b))
      elseif (mte.eq.214)  then
      call shl214s (cm,lft,llt)
      elseif (mte.eq.215) then
      call shl215s(cm,capa,lft,llt,npt)
      elseif (mte.eq.219)  then
      call shl219s (cm,eosp,capa,lft,llt,ipt,4)
      elseif (mte.eq.232) then
      call shl232s(cm,capa,lft,llt)
      elseif (mte.eq.233) then
      call shl233s(cm,capa,lft,llt,a(islcnt(16)),a(n8),a(n9),
     .             a(n8+2*nlcur+1),a(n8+3*nlcur+1),npt,ipt_thk)
      elseif (mte.eq.234) then
      call shl234s(cm,lft,llt,capa,npt,nip,nnm1)
      elseif (mte.eq.235) then
      call shl235s(cm,lft,llt,capa,npt,nip)
      elseif (mte.eq.236) then
      call shl236s (cm,capa,lft,llt)
      elseif (mte.eq.244) then
      call shl244s (cm,a(n4b),a(ntmp0+1),capa,lft,llt,a(islcnt(16)),
     $        a(n8),a(n9),a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     $       a(ioshl(488)),ipt,r_mem(dm_x),a(ns05),rcoor,scoor,tcoor)
      elseif (mte.eq.248) then
      call shl248s (cm,a(n4b),a(ntmp0+1),capa,lft,llt,a(islcnt(16)),
     $        a(n8),a(n9),a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     $       a(ioshl(488)),ipt,r_mem(dm_x),a(ns05),rcoor,scoor,tcoor)
      elseif (mte.eq.251) then
      call shl251s (cm,a(n8),a(n9),capa,lft,llt,npt,a(n8+2*nlcur+1),
     . a(n8+3*nlcur+1),a(islcnt(16)),a(ifdbck(31)),ipt_thk,nip,
     . a(ns13+numels+nnm1))
      elseif (mte.eq.255) then
      call shl255s(cm,lft,llt,capa,a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     $        a(islcnt(16)),a(n8),a(n9),npt,ipt_thk,nip,
     $        a(ns13+numels+nnm1),nnm1)
      elseif (mte.eq.276) then
      call shl276s(cm,capa,lft,llt)
      elseif (mte.eq.243) then
      call shl243s(cm,capa,lft,llt,npt,a(n8),a(n9),a(ifdbck(31)),
     . a(islcnt(16)),a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     . nnm1,rcoor,scoor,tcoor,ipt_thk,nip,a(ns13+numels+nnm1))
      elseif (mte.eq.261) then
      call shl261s(cm,capa,lft,llt,npt,nip,ipt_thk,a(islcnt(16)),
     .             a(ns21))
      elseif (mte.eq.262) then
      call shl262s(cm,capa,lft,llt,npt,nip,ipt_thk,a(islcnt(16)),
     .             a(ns21))
      elseif (mte.eq.270)  then
         call shl270s(cm,capa,lft,llt,a(islcnt(16)),npt,
     . nnm1,rcoor,scoor,tcoor)
      elseif (mte.eq.274)  then
         call shl274s(cm,capa,lft,llt,a(islcnt(16)),a(n4b),npt)
      elseif (mte.eq.249) then
      call shl249s(cm,capa,lft,llt,npt,nip,ipt_thk,a(islcnt(16)),
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),rcoor,scoor,tcoor)
      elseif (iamusrmat(mte)) then
      call usrmat (lft,llt,cm,ss1,capa,'shell',mte,npt,
     . a(n8),a(n9),a(islcnt(16)),rcoor,scoor,tcoor,nnm1,nip,ipt_thk)
      else
      write(iotty,10) mte
      write(iohsp,10) mte
      write(iomsg,10) mte
      call adios (2)
      endif
c
c     Staged construction
c
      if (ceap_i_cnt(13).gt.0) call ceap_stiffen_2 (lft,llt,'shell')
      if (ceap_i_cnt(12).gt.0) call ceap_part_removal (lft,llt)
      if (abs(betav).gt.0.000) then
c
c     Stiffness Rayleigh damping
c
      if (lenvec(5).eq.1)
     . call rydmp2_new (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),
     .  ax(1,nmtcon-3),ax(1,nmtcon-2),ax(1,nmtcon-1),ax(1,nmtcon),
     .  ss1,ss2,ss3,ss4,ss5,ss6)
      endif
c
c     Material tangent stiffness
c
      if (is17loc.eq.1.or.ihgf.eq.3) then
        call cmats (cm,capa,ipt,lft,llt,mte,a(n8),a(n9),mxe,nnm1,a(1),
     .   1,0,1.,1.,0.)
      endif
c
      if (ihgf.eq.3) then
         if (iunf.eq.0) then
            do i=lft,llt
               thck(i)=thick(i)
            enddo
         else
            do i=lft,llt
               thck(i)=.25*(thick(i)+fga(i)+fgb(i)+fgc(i))
            enddo
         endif
c
         do i=lft,llt
            c11=dsave(i,3,1)
            c12=dsave(i,3,2)
            c13=dsave(i,3,4)
            c14=dsave(i,3,5)
            c15=dsave(i,3,6)
            b11=dsave(i,3,3)
            b11i=1./b11
            bc11=b11i*c11
            bc12=b11i*c12
            bc13=b11i*c13
            bc14=b11i*c14
            bc15=b11i*c15
            dsave(i,1,1)=dsave(i,1,1)-c11*bc11
            dsave(i,2,1)=dsave(i,2,1)-c12*bc11
            dsave(i,3,1)=dsave(i,4,1)-c13*bc11
            dsave(i,4,1)=dsave(i,5,1)-c14*bc11
            dsave(i,5,1)=dsave(i,6,1)-c15*bc11
            dsave(i,1,2)=dsave(i,1,2)-c11*bc12
            dsave(i,2,2)=dsave(i,2,2)-c12*bc12
            dsave(i,3,2)=dsave(i,4,2)-c13*bc12
            dsave(i,4,2)=dsave(i,5,2)-c14*bc12
            dsave(i,5,2)=dsave(i,6,2)-c15*bc12
            dsave(i,1,3)=dsave(i,1,4)-c11*bc13
            dsave(i,2,3)=dsave(i,2,4)-c12*bc13
            dsave(i,3,3)=dsave(i,4,4)-c13*bc13
            dsave(i,4,3)=dsave(i,5,4)-c14*bc13
            dsave(i,5,3)=dsave(i,6,4)-c15*bc13
            dsave(i,1,4)=dsave(i,1,5)-c11*bc14
            dsave(i,2,4)=dsave(i,2,5)-c12*bc14
            dsave(i,3,4)=dsave(i,4,5)-c13*bc14
            dsave(i,4,4)=dsave(i,5,5)-c14*bc14
            dsave(i,5,4)=dsave(i,6,5)-c15*bc14
            dsave(i,1,5)=dsave(i,1,6)-c11*bc15
            dsave(i,2,5)=dsave(i,2,6)-c12*bc15
            dsave(i,3,5)=dsave(i,4,6)-c13*bc15
            dsave(i,4,5)=dsave(i,5,6)-c14*bc15
            dsave(i,5,5)=dsave(i,6,6)-c15*bc15
         enddo
c
         if (ipt.eq.1) then
            do j=1,45
               do i=lft,llt
                  uehisv(i,nhsv+j)=uehisv(i,nhsv+j-45)
               enddo
            enddo
            index=1
            do j=1,5
               do k=1,j
                  do i=lft,llt
                     uehisv(i,nhsv+index-45)=dsave(i,j,k)*vol(i)
                     uehisv(i,nhsv+index-30)=dsave(i,j,k)*vol(i)
     .                    *thck(i)*tcoor*.5
                     uehisv(i,nhsv+index-15)=dsave(i,j,k)*vol(i)
     .                    *(thck(i)*tcoor*.5)**2
                  enddo
                  index=index+1
               enddo
            enddo
         else
            index=1
            do j=1,5
               do k=1,j
                  do i=lft,llt
                     uehisv(i,nhsv+index-45)=uehisv(i,nhsv+index-45)+
     .                    dsave(i,j,k)*vol(i)
                     uehisv(i,nhsv+index-30)=uehisv(i,nhsv+index-30)+
     .                    dsave(i,j,k)*vol(i)*thck(i)*tcoor*.5
                     uehisv(i,nhsv+index-15)=uehisv(i,nhsv+index-15)+
     .                    dsave(i,j,k)*vol(i)*(thck(i)*tcoor*.5)**2
                  enddo
                  index=index+1
               enddo
            enddo
         endif
         if (ipt.eq.nip) then
            fact=.5
            dsum=0.
            do j=1,15
               dsum=dsum+abs(uehisv(lft,nhsv+j))
            enddo
            if (dsum.lt.1.e-20) fact=1.
            do j=1,45
               do i=lft,llt
                  uehisv(i,nhsv+j)=
     .                 fact*(uehisv(i,nhsv+j)+uehisv(i,nhsv+j-45))
               enddo
            enddo
         endif
c
         if (is17loc.eq.1) then
            do i=lft,llt
               dsave(i,6,6)=dsave(i,5,5)
               dsave(i,5,6)=dsave(i,4,5)
               dsave(i,4,6)=dsave(i,3,5)
               dsave(i,3,6)=0.
               dsave(i,2,6)=dsave(i,2,5)
               dsave(i,1,6)=dsave(i,1,5)
               dsave(i,6,5)=dsave(i,5,4)
               dsave(i,5,5)=dsave(i,4,4)
               dsave(i,4,5)=dsave(i,3,4)
               dsave(i,3,5)=0.
               dsave(i,2,5)=dsave(i,2,4)
               dsave(i,1,5)=dsave(i,1,4)
               dsave(i,6,4)=dsave(i,5,3)
               dsave(i,5,4)=dsave(i,4,3)
               dsave(i,4,4)=dsave(i,3,3)
               dsave(i,3,4)=0.
               dsave(i,2,4)=dsave(i,2,3)
               dsave(i,1,4)=dsave(i,1,3)
               dsave(i,6,3)=0.
               dsave(i,5,3)=0.
               dsave(i,4,3)=0.
               dsave(i,3,3)=1.e-4
               dsave(i,2,3)=0.
               dsave(i,1,3)=0.
               dsave(i,6,2)=dsave(i,5,2)
               dsave(i,5,2)=dsave(i,4,2)
               dsave(i,4,2)=dsave(i,3,2)
               dsave(i,3,2)=0.
               dsave(i,6,1)=dsave(i,5,1)
               dsave(i,5,1)=dsave(i,4,1)
               dsave(i,4,1)=dsave(i,3,1)
               dsave(i,3,1)=0.
            enddo
         endif
c
      endif
c
c     Compute bulk viscosity
c
      if (b12+b2.ne.0.0) call bulkq_shl(lft,llt,nmtcon-ioffbq,bulkq)
c
c     Scatter history variables
c
      if (isolvr(18).eq.0) then
         lavloc=(ipt-1)*nmtcon+lav
         do j=1,nmtcon
            do i=lft,llt
               auxvec(lavloc+(i-lft)*nip*nmtcon+(j-1))=ax(i,j)
            enddo
         enddo
      endif
c
      if (abs(betav).gt.0.000) then
c
c     Stiffness Rayleigh damping
c
         if (lenvec(5).eq.0) then
            call rydmp3 (lft,llt)
         else
            call rydmp4 (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),
     .           ax(1,nmtcon-3),ax(1,nmtcon-2),ax(1,nmtcon-1),
     .           ax(1,nmtcon))
         endif
      endif
c
      return
 10   format (/
     . ' *** Error material type',i4,' is unavailable for shells')
      end
      subroutine usrshl_istr(strain,wgt,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'implicit1.inc'
      common/aux2loc/d1(nlq),d2(nlq),d3(nlq),d4(nlq),d5(nlq),d6(nlq),
     1 wzzdt(nlq),wyydt(nlq),wxxdt(nlq),einc(nlq)
      dimension strain(12,1)
c
c$omp threadprivate (/aux2loc/)
      if (isolvr(18).eq.0) then
      do 10 i=lft,llt
      strain(1,i)=strain(1,i)+wgt*.25*d1(i)
      strain(2,i)=strain(2,i)+wgt*.25*d2(i)
      strain(3,i)=strain(3,i)+wgt*.25*d3(i)
      strain(4,i)=strain(4,i)+wgt*.125*d4(i)
      strain(5,i)=strain(5,i)+wgt*.125*d5(i)
   10 strain(6,i)=strain(6,i)+wgt*.125*d6(i)
      endif
      return
      end
      subroutine usrshl_igs(wgt,iunf,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
c
      common/aux00loc/ds11(nlq),ds12(nlq),ds13(nlq),ds22(nlq),
     1     ds23(nlq),ds33(nlq),sjunk(nlq,4),str33(nlq)
      common/aux2loc/d1(nlq),d2(nlq),d3(nlq),d4(nlq),d5(nlq),d6(nlq)
c
c     Integrates global strain for later
c     determination of fiber changes
c
c$omp threadprivate (/aux00loc/)
c$omp threadprivate (/aux2loc/)
      if (iunf.ne.0) then
c
c     Account for large nodal rotations
c
         do i=lft,llt
c
            d4i=.5*d4(i)
            d5i=.5*d5(i)
            d6i=.5*d6(i)
c
            ds11(i)=wgt*d1(i)+ds11(i)
            ds22(i)=wgt*d2(i)+ds22(i)
            ds33(i)=wgt*d3(i)+ds33(i)
            ds12(i)=wgt*d4i+ds12(i)
            ds23(i)=wgt*d5i+ds23(i)
            ds13(i)=wgt*d6i+ds13(i)
c
         enddo
c
      else
c
         do i=lft,llt
c
            str33(i)=str33(i)+wgt*d3(i)
c
         enddo
c
      endif
c
      return
      end
      subroutine usrshl_frc(nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     computes internal force for user defined shell
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/aux14loc/sig(nlq,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c     Number of degrees of freedom in the shell
c
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/bel7loc/)
      ndtot=4*(6+nxdof)
c
c     Assumes zero internal force vector on first entry
c
      do j=1,6
c
c     Stress times volume
c
         do i=lft,llt
            sigv(i)=sig(i,j)*vol(i)
         enddo
c
         if (j.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j,j,k)
               enddo
            enddo
         else
            j1=1
            j2=2
            if (j.eq.5) then
               j1=3
            elseif (j.eq.6) then
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
         do k=1,ndtot
            do i=lft,llt
               frc(i,k)=frc(i,k)+bvec(i,k)*sigv(i)
            enddo
         enddo
c
      enddo
c
      return
      end
      subroutine usrshl_trb(nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform b-matrix to global system for
c     tangent stiffness assembly
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/bel7loc/)
      do n=1,3
         do m=1,3
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,1)+hl12(i)*cmtrx(i,m,n,2)+
     1              hl13(i)*cmtrx(i,m,n,3)
               b2=hl21(i)*cmtrx(i,m,n,1)+hl22(i)*cmtrx(i,m,n,2)+
     1              hl23(i)*cmtrx(i,m,n,3)
               b3=hl31(i)*cmtrx(i,m,n,1)+hl32(i)*cmtrx(i,m,n,2)+
     1              hl33(i)*cmtrx(i,m,n,3)
               cmtrx(i,m,n,1)=b1
               cmtrx(i,m,n,2)=b2
               cmtrx(i,m,n,3)=b3
            enddo
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,4)+hl12(i)*cmtrx(i,m,n,5)+
     1              hl13(i)*cmtrx(i,m,n,6)
               b2=hl21(i)*cmtrx(i,m,n,4)+hl22(i)*cmtrx(i,m,n,5)+
     1              hl23(i)*cmtrx(i,m,n,6)
               b3=hl31(i)*cmtrx(i,m,n,4)+hl32(i)*cmtrx(i,m,n,5)+
     1              hl33(i)*cmtrx(i,m,n,6)
               cmtrx(i,m,n,4)=b1
               cmtrx(i,m,n,5)=b2
               cmtrx(i,m,n,6)=b3
            enddo
            i1=7+nxdof
            i2=8+nxdof
            i3=9+nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
            i1=10+nxdof
            i2=11+nxdof
            i3=12+nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
            i1=13+2*nxdof
            i2=14+2*nxdof
            i3=15+2*nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
            i1=16+2*nxdof
            i2=17+2*nxdof
            i3=18+2*nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
            i1=19+3*nxdof
            i2=20+3*nxdof
            i3=21+3*nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
            i1=22+3*nxdof
            i2=23+3*nxdof
            i3=24+3*nxdof
            do i=lft,llt
               b1=hl11(i)*cmtrx(i,m,n,i1)+hl12(i)*cmtrx(i,m,n,i2)+
     1              hl13(i)*cmtrx(i,m,n,i3)
               b2=hl21(i)*cmtrx(i,m,n,i1)+hl22(i)*cmtrx(i,m,n,i2)+
     1              hl23(i)*cmtrx(i,m,n,i3)
               b3=hl31(i)*cmtrx(i,m,n,i1)+hl32(i)*cmtrx(i,m,n,i2)+
     1              hl33(i)*cmtrx(i,m,n,i3)
               cmtrx(i,m,n,i1)=b1
               cmtrx(i,m,n,i2)=b2
               cmtrx(i,m,n,i3)=b3
            enddo
         enddo
      enddo
c
      return
      end
      subroutine usrshl_kgm(ske,nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     assembles geometric stiffness matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/aux14loc/sig(nlq,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      dimension ske(nlq,*)
c
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/bel7loc/)
      do j=1,6
c
c     Stress times volume
c
         do i=lft,llt
            sigv(i)=sig(i,j)*vol(i)
         enddo
c
         if (j.le.3) then
c
c     Loop over element in stiffness matrix
c
            do ml=1,6
               m=ml
               irow=m*(m-1)/2
               m=ml
               do mr=1,ml
                  n=mr
                  iske=irow+n
                  n=mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+sigv(i)*
     1                    (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                    cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                    cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                  enddo
               enddo
            enddo
            do nl=2,4
               do ml=1,6
                  m=(nl-1)*6+ml
                  irow=m*(m-1)/2
                  m=(nl-1)*(6+nxdof)+ml
                  do nr=1,nl-1
                     do mr=1,6
                        n=(nr-1)*6+mr
                        iske=irow+n
                        n=(nr-1)*(6+nxdof)+mr
                        do i=lft,llt
                           ske(i,iske)=ske(i,iske)+sigv(i)*
     1                          (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                          cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                          cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                        enddo
                     enddo
                  enddo
                  do mr=1,ml
                     n=(nl-1)*6+mr
                     iske=irow+n
                     n=(nl-1)*(6+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+sigv(i)*
     1                       (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                       cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                       cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                     enddo
                  enddo
               enddo
            enddo
            if (nxdof.gt.0) then
              nsnd=(nxdof-1)/3+1
              do isnd=1,nsnd
                nsdf=3
                if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
                do irnd=1,4
                  do isdf=1,nsdf
                    m=24+24*(isnd-1)+6*(irnd-1)+isdf
                    irow=m*(m-1)/2
                    mm=6+(6+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                    do nr=1,4
                      do mr=1,6
                        n=(nr-1)*6+mr
                        iske=irow+n
                        nn=(nr-1)*(6+nxdof)+mr
                        do i=lft,llt
                          ske(i,iske)=ske(i,iske)+sigv(i)*
     1                         (cmtrx(i,1,j,mm)*cmtrx(i,1,j,nn)+
     2                         cmtrx(i,2,j,mm)*cmtrx(i,2,j,nn)+
     3                         cmtrx(i,3,j,mm)*cmtrx(i,3,j,nn))
                        enddo
                      enddo
                    enddo
                    do jsnd=1,nsnd
                      msdf=3
                      if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                      do jrnd=1,4
                        do 1 jsdf=1,msdf
                          n=24+24*(jsnd-1)+6*(jrnd-1)+jsdf
                          if (n.gt.m) goto 1
                          iske=irow+n
                          nn=6+(6+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                          do i=lft,llt
                            ske(i,iske)=ske(i,iske)+sigv(i)*
     1                           (cmtrx(i,1,j,mm)*cmtrx(i,1,j,nn)+
     2                           cmtrx(i,2,j,mm)*cmtrx(i,2,j,nn)+
     3                           cmtrx(i,3,j,mm)*cmtrx(i,3,j,nn))
                          enddo
 1                      continue
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
c     
         else
c
            j1=1
            j2=2
            if (j.eq.5) then
               j1=3
            elseif (j.eq.6) then
               j2=3
            endif
c
c     Loop over element in stiffness matrix
c
            do ml=1,6
               m=ml
               irow=m*(m-1)/2
               m=ml
               do mr=1,ml
                  n=mr
                  iske=irow+n
                  n=mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+sigv(i)*
     1                    (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                    cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                    cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                    cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                    cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                    cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                  enddo
               enddo
            enddo
            do nl=2,4
               do ml=1,6
                  m=(nl-1)*6+ml
                  irow=m*(m-1)/2
                  m=(nl-1)*(6+nxdof)+ml
                  do nr=1,nl-1
                     do mr=1,6
                        n=(nr-1)*6+mr
                        iske=irow+n
                        n=(nr-1)*(6+nxdof)+mr
                        do i=lft,llt
                           ske(i,iske)=ske(i,iske)+sigv(i)*
     1                          (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                          cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                          cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                          cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                          cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                          cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                        enddo
                     enddo
                  enddo
                  do mr=1,ml
                     n=(nl-1)*6+mr
                     iske=irow+n
                     n=(nl-1)*(6+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+sigv(i)*
     1                       (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                       cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                       cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                       cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                       cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                       cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                     enddo
                  enddo
               enddo
            enddo
            if (nxdof.gt.0) then
              nsnd=(nxdof-1)/3+1
              do isnd=1,nsnd
                nsdf=3
                if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
                do irnd=1,4
                  do isdf=1,nsdf
                    m=24+24*(isnd-1)+6*(irnd-1)+isdf
                    irow=m*(m-1)/2
                    mm=6+(6+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                    do nr=1,4
                      do mr=1,6
                        n=(nr-1)*6+mr
                        iske=irow+n
                        nn=(nr-1)*(6+nxdof)+mr
                        do i=lft,llt
                          ske(i,iske)=ske(i,iske)+sigv(i)*
     1                         (cmtrx(i,1,j1,mm)*cmtrx(i,1,j2,nn)+
     2                         cmtrx(i,1,j2,mm)*cmtrx(i,1,j1,nn)+
     3                         cmtrx(i,2,j1,mm)*cmtrx(i,2,j2,nn)+
     4                         cmtrx(i,2,j2,mm)*cmtrx(i,2,j1,nn)+
     5                         cmtrx(i,3,j1,mm)*cmtrx(i,3,j2,nn)+
     6                         cmtrx(i,3,j2,mm)*cmtrx(i,3,j1,nn))
                        enddo
                      enddo
                    enddo
                    do jsnd=1,nsnd
                      msdf=3
                      if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                      do jrnd=1,4
                        do 2 jsdf=1,msdf
                          n=24+24*(jsnd-1)+6*(jrnd-1)+jsdf
                          if (n.gt.m) goto 2
                          iske=irow+n
                          nn=6+(6+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                          do i=lft,llt
                            ske(i,iske)=ske(i,iske)+sigv(i)*
     1                           (cmtrx(i,1,j1,mm)*cmtrx(i,1,j2,nn)+
     2                           cmtrx(i,1,j2,mm)*cmtrx(i,1,j1,nn)+
     3                           cmtrx(i,2,j1,mm)*cmtrx(i,2,j2,nn)+
     4                           cmtrx(i,2,j2,mm)*cmtrx(i,2,j1,nn)+
     5                           cmtrx(i,3,j1,mm)*cmtrx(i,3,j2,nn)+
     6                           cmtrx(i,3,j2,mm)*cmtrx(i,3,j1,nn))
                          enddo
 2                      continue
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
c
         endif
c
      enddo
c
      return
      end
      subroutine usrshl_kmt(ske,nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     assembles material stiffness matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux9loc/vlrho(nlq),vol(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/vect8loc/dsave(nlq,6,6)
c
      dimension ske(nlq,*)
c
c     Static condensation of material tangent modulus
c
c     C*d1+B*d2=0 -> d2=-inv(B)*C*d1 -> A*d1+C'*d2=(A-C'*inv(B)*C)*d1
c
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/vect8loc/)
      do i=lft,llt
c
c     C
c
         c11=dsave(i,3,1)
         c12=dsave(i,3,2)
         c13=dsave(i,3,4)
         c14=dsave(i,3,5)
         c15=dsave(i,3,6)
c
c     B
c
         b11=dsave(i,3,3)
c
c     inv(B)
c
         b11i=1./b11
c
c     inv(B)*C
c
         bc11=b11i*c11
         bc12=b11i*c12
         bc13=b11i*c13
         bc14=b11i*c14
         bc15=b11i*c15
c
c     A-C'*inv(B)*C
c
         dsave(i,1,1)=dsave(i,1,1)-c11*bc11
         dsave(i,2,1)=dsave(i,2,1)-c12*bc11
         dsave(i,4,1)=dsave(i,4,1)-c13*bc11
         dsave(i,5,1)=dsave(i,5,1)-c14*bc11
         dsave(i,6,1)=dsave(i,6,1)-c15*bc11
         dsave(i,1,2)=dsave(i,1,2)-c11*bc12
         dsave(i,2,2)=dsave(i,2,2)-c12*bc12
         dsave(i,4,2)=dsave(i,4,2)-c13*bc12
         dsave(i,5,2)=dsave(i,5,2)-c14*bc12
         dsave(i,6,2)=dsave(i,6,2)-c15*bc12
         dsave(i,1,4)=dsave(i,1,4)-c11*bc13
         dsave(i,2,4)=dsave(i,2,4)-c12*bc13
         dsave(i,4,4)=dsave(i,4,4)-c13*bc13
         dsave(i,5,4)=dsave(i,5,4)-c14*bc13
         dsave(i,6,4)=dsave(i,6,4)-c15*bc13
         dsave(i,1,5)=dsave(i,1,5)-c11*bc14
         dsave(i,2,5)=dsave(i,2,5)-c12*bc14
         dsave(i,4,5)=dsave(i,4,5)-c13*bc14
         dsave(i,5,5)=dsave(i,5,5)-c14*bc14
         dsave(i,6,5)=dsave(i,6,5)-c15*bc14
         dsave(i,1,6)=dsave(i,1,6)-c11*bc15
         dsave(i,2,6)=dsave(i,2,6)-c12*bc15
         dsave(i,4,6)=dsave(i,4,6)-c13*bc15
         dsave(i,5,6)=dsave(i,5,6)-c14*bc15
         dsave(i,6,6)=dsave(i,6,6)-c15*bc15
c
c     Reset row and column 3
c
         dsave(i,1,3)=0.
         dsave(i,2,3)=0.
         dsave(i,4,3)=0.
         dsave(i,5,3)=0.
         dsave(i,6,3)=0.
         dsave(i,3,1)=0.
         dsave(i,3,2)=0.
         dsave(i,3,4)=0.
         dsave(i,3,5)=0.
         dsave(i,3,6)=0.
c
         dsave(i,3,3)=1.e-4
c
      enddo
c
c     condensated tangent in position 1,2,4,5,6
c
c     Number of degrees of freedom in the shell
c
      ndtot=4*(6+nxdof)
c
      do jj=1,6
c
         do j=1,ndtot
            do i=lft,llt
               cvec(i,j)=0.
            enddo
         enddo
c
         do j=1,6
c
c     Stiffness times volume
c
            do i=lft,llt
               dsave(i,j,jj)=dsave(i,j,jj)*vol(i)
            enddo
c
            if (j.le.3) then
               do k=1,ndtot
                  do i=lft,llt
                     bvec(i,k)=cmtrx(i,j,j,k)
                  enddo
               enddo
            else
               j1=1
               j2=2
               if (j.eq.5) then
                  j1=3
               elseif (j.eq.6) then
                  j2=3
               endif
               do k=1,ndtot
                  do i=lft,llt
                     bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
                  enddo
               enddo
            endif
c
            do k=1,ndtot
               do i=lft,llt
                  cvec(i,k)=cvec(i,k)+bvec(i,k)*dsave(i,j,jj)
               enddo
            enddo
c
         enddo
c
         if (jj.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,jj,jj,k)
               enddo
            enddo
         else
            j1=1
            j2=2
            if (jj.eq.5) then
               j1=3
            elseif (jj.eq.6) then
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
c     Loop over element in stiffness matrix
c
         do ml=1,6
            m=ml
            irow=m*(m-1)/2
            m=ml
            do mr=1,ml
               n=mr
               iske=irow+n
               n=mr
               do i=lft,llt
                  ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
               enddo
            enddo
         enddo
         do nl=2,4
            do ml=1,6
               m=(nl-1)*6+ml
               irow=m*(m-1)/2
               m=(nl-1)*(6+nxdof)+ml
               do nr=1,nl-1
                  do mr=1,6
                     n=(nr-1)*6+mr
                     iske=irow+n
                     n=(nr-1)*(6+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
                     enddo
                  enddo
               enddo
               do mr=1,ml
                  n=(nl-1)*6+mr
                  iske=irow+n
                  n=(nl-1)*(6+nxdof)+mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
                  enddo
               enddo
            enddo
         enddo
         if (nxdof.gt.0) then
           nsnd=(nxdof-1)/3+1
           do isnd=1,nsnd
             nsdf=3
             if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
             do irnd=1,4
               do isdf=1,nsdf
                 m=24+24*(isnd-1)+6*(irnd-1)+isdf
                 irow=m*(m-1)/2
                 mm=6+(6+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                 do nr=1,4
                   do mr=1,6
                     n=(nr-1)*6+mr
                     iske=irow+n
                     nn=(nr-1)*(6+nxdof)+mr
                     do i=lft,llt
                       ske(i,iske)=ske(i,iske)+cvec(i,mm)*bvec(i,nn)
                     enddo
                   enddo
                 enddo
                 do jsnd=1,nsnd
                   msdf=3
                   if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                   do jrnd=1,4
                     do 1 jsdf=1,msdf
                       n=24+24*(jsnd-1)+6*(jrnd-1)+jsdf
                       if (n.gt.m) goto 1
                       iske=irow+n
                       nn=6+(6+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                       do i=lft,llt
                         ske(i,iske)=ske(i,iske)+cvec(i,mm)*bvec(i,nn)
                       enddo
 1                   continue
                   enddo
                 enddo
               enddo
             enddo
           enddo
         endif
c
      enddo
c
      return
      end
      subroutine usrshl_sfd(lenvec8,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     computes dN/dx and dN/dy (shape function derivatives)
c     in local systems to be used for computing hourglass forces
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux10loc/area(nlq),
     1     px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     &     px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2     py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     &     py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3     pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     &     pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4     dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5     dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6     dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7     dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8     dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9     dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux45loc/crap(nlq,124),
     1 dndx(nlq,4),dndy(nlq,4),hnhx(nlq,4),hnhy(nlq,4)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux45loc/)
c$omp threadprivate (/bel7loc/)
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
         dxdxi =.25*(-px1(i)+px2(i)+px3(i)-px4(i))
         dydxi =.25*(-py1(i)+py2(i)+py3(i)-py4(i))
         dxdeta=.25*(-px1(i)-px2(i)+px3(i)+px4(i))
         dydeta=.25*(-py1(i)-py2(i)+py3(i)+py4(i))
c
         deti   =1./(dxdxi*dydeta-dydxi*dxdeta)
         dxidx =dydeta*deti
         detadx=-dydxi*deti
         dxidy =-dxdeta*deti
         detady=dxdxi*deti
c
         dndx(i,1)=-.25*(dxidx+detadx)
         dndx(i,2)=.25*(dxidx-detadx)
         dndx(i,3)=-dndx(i,1)
         dndx(i,4)=-dndx(i,2)
         dndy(i,1)=-.25*(dxidy+detady)
         dndy(i,2)=.25*(dxidy-detady)
         dndy(i,3)=-dndy(i,1)
         dndy(i,4)=-dndy(i,2)
c
      enddo
c
      endif
c
      do i=lft,llt
c
         dxdxi =.25*(-xx1(i)+xx2(i)+xx3(i)-xx4(i))
         dydxi =.25*(-yy1(i)+yy2(i)+yy3(i)-yy4(i))
         dxdeta=.25*(-xx1(i)-xx2(i)+xx3(i)+xx4(i))
         dydeta=.25*(-yy1(i)-yy2(i)+yy3(i)+yy4(i))
c
         deti   =1./(dxdxi*dydeta-dydxi*dxdeta)
         dxidx =dydeta*deti
         detadx=-dydxi*deti
         dxidy =-dxdeta*deti
         detady=dxdxi*deti
c
         hnhx(i,1)=-.25*(dxidx+detadx)
         hnhx(i,2)=.25*(dxidx-detadx)
         hnhx(i,3)=-hnhx(i,1)
         hnhx(i,4)=-hnhx(i,2)
         hnhy(i,1)=-.25*(dxidy+detady)
         hnhy(i,2)=.25*(dxidy-detady)
         hnhy(i,3)=-hnhy(i,1)
         hnhy(i,4)=-hnhy(i,2)
c
      enddo
c
      if (lenvec8.eq.0) then
         do j=1,4
            do i=lft,llt
               dndx(i,j)=hnhx(i,j)
               dndy(i,j)=hnhy(i,j)
            enddo
         enddo
      endif
c
      return
      end
      subroutine usrshl_hgf(qs,ihgenf,hgener,mte,nxdof,lenvec8,lft,llt,
     . iloc)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     adds hourglass force to internal force in local system
c
      include 'nlqparm'
      include 'matflr.inc'
      include 'implicit1.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
      include 'nhisparm.inc'
c
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux11loc/
     &ft31(nlq),ft32(nlq),ft33(nlq),ft41(nlq),ft42(nlq),ft43(nlq),
     &htx(nlq),hty(nlq),gm(nlq,4),
     &bsum(nlq),qhx(nlq),qhy(nlq),qwz(nlq),qtx(nlq),qty(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/aux13loc/
     1 zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     2 gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     3 gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     4 x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     5 x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/aux45loc/crap(nlq,124),
     1 dndx(nlq,4),dndy(nlq,4),hnhx(nlq,4),hnhy(nlq,4)
      common/aux35loc/rhoa(nlq),cxx(nlq),fcl(nlq),fcq(nlq)
      common/bk12loc/b12,b2,qhg,qhgm,qhgb,qhgw
      common/failuloc/sieu(nlq),fail(nlq)
      common/hourgloc/ymod(nlq),gmod(nlq),ifsv(nlq)
      common/prescloc/voltot(nlq)
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/subtssloc/dt1siz(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      dimension qs1(nlq),qs2(nlq),qs3(nlq),qs4(nlq),qs5(nlq)
      dimension qs(9,*),hgener(*)
      real mmode,mcoef
c
c     Hourglass stiffnesses
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux11loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/aux45loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/bk12loc/)
c$omp threadprivate (/hourgloc/)
c$omp threadprivate (/failuloc/)
c$omp threadprivate (/prescloc/)
c$omp threadprivate (/soundloc/)
c$omp threadprivate (/subtssloc/)
      tmode=qhgb*ymod(lft)/1920.0
      wmode=qhgw*gmod(lft)/120.00
      mmode=qhgm*ymod(lft)/80.000
c
      if (ifsv(lft).eq.3) then
c
c     Stiffness based hourglass
c
         do i=lft,llt
c
c     Compute tau
c
            htxi  =px1(i)+px3(i)-px2(i)-px4(i)
            htyi  =py1(i)+py3(i)-py2(i)-py4(i)
            gm(i,1)= 1.-htxi*dndx(i,1)-htyi*dndy(i,1)
            gm(i,2)=-1.-htxi*dndx(i,2)-htyi*dndy(i,2)
            gm(i,3)= 1.-htxi*dndx(i,3)-htyi*dndy(i,3)
            gm(i,4)=-1.-htxi*dndx(i,4)-htyi*dndy(i,4)
c
c     Hourglass strain increments
c
            qhx(i)=
     1           gm(i,1)*dx5(i)+gm(i,2)*dx6(i)+
     2           gm(i,3)*dx7(i)+gm(i,4)*dx8(i)
            qhy(i)=
     1           gm(i,1)*dy5(i)+gm(i,2)*dy6(i)+
     2           gm(i,3)*dy7(i)+gm(i,4)*dy8(i)
            qwz(i)=
     1           gm(i,1)*dz5(i)+gm(i,2)*dz6(i)+
     2           gm(i,3)*dz7(i)+gm(i,4)*dz8(i)
            qtx(i)=
     1           gm(i,1)*px5(i)+gm(i,2)*px6(i)+
     2           gm(i,3)*px7(i)+gm(i,4)*px8(i)
            qty(i)=
     1           gm(i,1)*px5(i)+gm(i,2)*px6(i)+
     2           gm(i,3)*px7(i)+gm(i,4)*px8(i)
c
         enddo
c
         do i=lft,llt
c
c     Modify for orthogonality to rigid body motion
c
            b1=-.25*(px5(i)+px6(i)+px7(i)+px8(i))
            b2=-.25*(py5(i)+py6(i)+py7(i)+py8(i))
            b3=-.25*(pz5(i)+pz6(i)+pz7(i)+pz8(i))
            c1=gm(i,1)*px1(i)+gm(i,2)*px2(i)+
     1           gm(i,3)*px3(i)+gm(i,4)*px4(i)
            c2=gm(i,1)*py1(i)+gm(i,2)*py2(i)+
     1           gm(i,3)*py3(i)+gm(i,4)*py4(i)
            c3=gm(i,1)*pz1(i)+gm(i,2)*pz2(i)+
     1           gm(i,3)*pz3(i)+gm(i,4)*pz4(i)
            a1=b2*c3-b3*c2
            a2=b3*c1-b1*c3
            a3=b1*c2-b2*c1
            qhx(i)=qhx(i)+a1
            qhy(i)=qhy(i)+a2
            qwz(i)=qwz(i)+a3
c
         enddo
c
         do i=lft,llt
c
c     Hourglass stresses
c
            bsumi = dndx(i,1)**2+dndx(i,2)**2+dndx(i,3)**2+dndx(i,4)**2+
     1           dndy(i,1)**2+dndy(i,2)**2+dndy(i,3)**2+dndy(i,4)**2
c
            thck=voltot(i)*area(i)
            coef=bsumi*voltot(i)
            tcoef=tmode*coef*thck**2
            mcoef=mmode*coef
            wcoef=wmode*coef*thck**2*area(i)
c
            qs1(i)=qs(1,i)+mcoef*qhx(i)
            qs2(i)=qs(2,i)+mcoef*qhy(i)
            qs3(i)=qs(3,i)+wcoef*qwz(i)
            qs4(i)=qs(4,i)+tcoef*qtx(i)
            qs5(i)=qs(5,i)+tcoef*qty(i)
c
         enddo
c
      else
c
c     Viscous based hourglass
c
         hgfac=rhoa(lft)*sndspd(lft)
c
         do i=lft,llt
c
c     Compute tau
c
            htxi  =px1(i)+px3(i)-px2(i)-px4(i)
            htyi  =py1(i)+py3(i)-py2(i)-py4(i)
            gm(i,1)= 1.-htxi*dndx(i,1)-htyi*dndy(i,1)
            gm(i,2)=-1.-htxi*dndx(i,2)-htyi*dndy(i,2)
            gm(i,3)= 1.-htxi*dndx(i,3)-htyi*dndy(i,3)
            gm(i,4)=-1.-htxi*dndx(i,4)-htyi*dndy(i,4)
c
c     Hourglass strain increments
c
            qhx(i)=
     1           gm(i,1)*dx5(i)+gm(i,2)*dx6(i)+
     2           gm(i,3)*dx7(i)+gm(i,4)*dx8(i)
            qhy(i)=
     1           gm(i,1)*dy5(i)+gm(i,2)*dy6(i)+
     2           gm(i,3)*dy7(i)+gm(i,4)*dy8(i)
            qwz(i)=
     1           gm(i,1)*dz5(i)+gm(i,2)*dz6(i)+
     2           gm(i,3)*dz7(i)+gm(i,4)*dz8(i)
            qtx(i)=
     1           gm(i,1)*px5(i)+gm(i,2)*px6(i)+
     2           gm(i,3)*px7(i)+gm(i,4)*px8(i)
            qty(i)=
     1           gm(i,1)*py5(i)+gm(i,2)*py6(i)+
     2           gm(i,3)*py7(i)+gm(i,4)*py8(i)
c
         enddo
c
         do i=lft,llt
c
c     Modify for orthogonality to rigid body motion
c
            b1=-.25*(px5(i)+px6(i)+px7(i)+px8(i))
            b2=-.25*(py5(i)+py6(i)+py7(i)+py8(i))
            b3=-.25*(pz5(i)+pz6(i)+pz7(i)+pz8(i))
            c1=gm(i,1)*px1(i)+gm(i,2)*px2(i)+
     1           gm(i,3)*px3(i)+gm(i,4)*px4(i)
            c2=gm(i,1)*py1(i)+gm(i,2)*py2(i)+
     1           gm(i,3)*py3(i)+gm(i,4)*py4(i)
            c3=gm(i,1)*pz1(i)+gm(i,2)*pz2(i)+
     1           gm(i,3)*pz3(i)+gm(i,4)*pz4(i)
            a1=b2*c3-b3*c2
            a2=b3*c1-b1*c3
            a3=b1*c2-b2*c1
            qhx(i)=qhx(i)+a1
            qhy(i)=qhy(i)+a2
            qwz(i)=qwz(i)+a3
c
         enddo
c
         do i=lft,llt
c
c     Hourglass stresses
c
            thck=voltot(i)*area(i)
            coef=hgfac*sqrt(abs(sarea(i)))*thck/(dt1siz(i)+1.e-16)
            wcoef=qhgw*coef
            mcoef=qhgm*coef
            tcoef=qhgb*.01*coef*thck**2
c
            qs1(i)=mcoef*qhx(i)
            qs2(i)=mcoef*qhy(i)
            qs3(i)=wcoef*qwz(i)
            qs4(i)=tcoef*qtx(i)
            qs5(i)=tcoef*qty(i)
c
         enddo
      endif
c
      if (ihgenf.gt.1) then
c
c     Hourglass energy contribution
c
         if (mtfailx(mte).ne.1) then
            do i=lft,llt
               hgener(i)=.5*(qhx(i)*(qs(1,i)+qs1(i))+
     .              qhy(i)*(qs(2,i)+qs2(i))
     .              +qwz(i)*(qs(3,i)+qs3(i))+qtx(i)*(qs(4,i)+qs4(i))
     .              +qty(i)*(qs(5,i)+qs5(i)))
            enddo
         else
            do i=lft,llt
               hgener(i)=.5*(qhx(i)*(qs(1,i)+qs1(i))+
     .              qhy(i)*(qs(2,i)+qs2(i))
     .              +qwz(i)*(qs(3,i)+qs3(i))+qtx(i)*(qs(4,i)+qs4(i))
     .              +qty(i)*(qs(5,i)+qs5(i)))*fail(i)
            enddo
         endif
      endif
c
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
c     Recompute tau in current configuration
c
         htxi  =xx1(i)+xx3(i)-xx2(i)-xx4(i)
         htyi  =yy1(i)+yy3(i)-yy2(i)-yy4(i)
         gm(i,1)= 1.-htxi*hnhx(i,1)-htyi*hnhy(i,1)
         gm(i,2)=-1.-htxi*hnhx(i,2)-htyi*hnhy(i,2)
         gm(i,3)= 1.-htxi*hnhx(i,3)-htyi*hnhy(i,3)
         gm(i,4)=-1.-htxi*hnhx(i,4)-htyi*hnhy(i,4)
c
      enddo
c
      endif
c
c     Add hourglass force to internal force
c
      if (iloc.eq.0) then
      do l=1,4
         ll=(l-1)*(6+nxdof)
         do i=lft,llt
c
            frc(i,ll+1)=frc(i,ll+1)+gm(i,l)*qs1(i)
            frc(i,ll+2)=frc(i,ll+2)+gm(i,l)*qs2(i)
            frc(i,ll+3)=frc(i,ll+3)+gm(i,l)*qs3(i)
            frc(i,ll+4)=frc(i,ll+4)+gm(i,l)*qs4(i)
            frc(i,ll+5)=frc(i,ll+5)+gm(i,l)*qs5(i)
c
         enddo
      enddo
      else
      do l=1,4
         ll=(l-1)*(6+nxdof)
         do i=lft,llt
c
            fx=gm(i,l)*qs1(i)
            fy=gm(i,l)*qs2(i)
            fz=gm(i,l)*qs3(i)
            frc(i,ll+1)=frc(i,ll+1)+
     .           gl11(i)*fx+gl12(i)*fy+gl13(i)*fz
            frc(i,ll+2)=frc(i,ll+2)+
     .           gl21(i)*fx+gl22(i)*fy+gl23(i)*fz
            frc(i,ll+3)=frc(i,ll+3)+
     .           gl31(i)*fx+gl32(i)*fy+gl33(i)*fz
            fx=gm(i,l)*qs4(i)
            fy=gm(i,l)*qs5(i)
            frc(i,ll+4)=frc(i,ll+4)+
     .           gl11(i)*fx+gl12(i)*fy
            frc(i,ll+5)=frc(i,ll+5)+
     .           gl21(i)*fx+gl22(i)*fy
            frc(i,ll+6)=frc(i,ll+6)+
     .           gl31(i)*fx+gl32(i)*fy
c
         enddo
      enddo
      endif
c
c     Store hourglass strain
c
      if (isolvr(18).eq.0) then
c
         do i=lft,llt
c
            qs(1,i)=qs1(i)
            qs(2,i)=qs2(i)
            qs(3,i)=qs3(i)
            qs(4,i)=qs4(i)
            qs(5,i)=qs5(i)
c
         enddo
c
      endif
c
      return
      end
      subroutine usrshl_h(ietyp,cm,lmc,nhsv,nxdof,lft,llt,iloc,ihgf)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'implicit1.inc'
      include 'nhisparm.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
c
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      real mx1,my1,mz1,mx2,my2,mz2,mx3,my3,mz3,mx4,my4,mz4
      common/aux13loc/
     & zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     & gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     & gl31(nlq),gl32(nlq),gl33(nlq),
     & x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     & x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq),
     & fx1(nlq),fy1(nlq),fz1(nlq),fx2(nlq),fy2(nlq),fz2(nlq),
     & fx3(nlq),fy3(nlq),fz3(nlq),fx4(nlq),fy4(nlq),fz4(nlq),
     & mx1(nlq),my1(nlq),mz1(nlq),mx2(nlq),my2(nlq),mz2(nlq),
     & mx3(nlq),my3(nlq),mz3(nlq),mx4(nlq),my4(nlq),mz4(nlq)
      common/bel8loc/uehisv(nlq,2*(NHISVUE+45))
      common/bel8aloc/iuehup(nlq,2*(NHISVUE+45))
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
      dimension cm(lmc)
      dimension force(nlq,4*(6+NXDOFUE))
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/bel8loc/)
c$omp threadprivate (/bel8aloc/)
c$omp threadprivate (/vect13loc/)
c$omp threadprivate (/soundloc/)
      ndtot=4*(6+nxdof)
      do j=1,ndtot
         do i=lft,llt
            force(i,j)=0.
            stiff(i,j)=0.
         enddo
      enddo
      ndtot2=ndtot*ndtot
      do j=ndtot+1,ndtot2
         do i=lft,llt
            stiff(i,j)=0.
         enddo
      enddo
c
      nadd1=0
      if (ihgf.eq.3) nadd1=45
c
      if (iloc.eq.0) then
      if (ietyp.eq.101) then
         call ushl_e101(force,stiff,ndtot,is17loc,
     .        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx5,dx6,dx7,dx8,dy5,dy6,dy7,dy8,dz5,dz6,dz7,dz8,
     .        px5,px6,px7,px8,py5,py6,py7,py8,pz5,pz6,pz7,pz8,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_e102(force,stiff,ndtot,is17loc,
     .        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx5,dx6,dx7,dx8,dy5,dy6,dy7,dy8,dz5,dz6,dz7,dz8,
     .        px5,px6,px7,px8,py5,py6,py7,py8,pz5,pz6,pz7,pz8,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_e103(force,stiff,ndtot,is17loc,
     .        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx5,dx6,dx7,dx8,dy5,dy6,dy7,dy8,dz5,dz6,dz7,dz8,
     .        px5,px6,px7,px8,py5,py6,py7,py8,pz5,pz6,pz7,pz8,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_e104(force,stiff,ndtot,is17loc,
     .        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx5,dx6,dx7,dx8,dy5,dy6,dy7,dy8,dz5,dz6,dz7,dz8,
     .        px5,px6,px7,px8,py5,py6,py7,py8,pz5,pz6,pz7,pz8,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_e105(force,stiff,ndtot,is17loc,
     .        xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,zz1,zz2,zz3,zz4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx5,dx6,dx7,dx8,dy5,dy6,dy7,dy8,dz5,dz6,dz7,dz8,
     .        px5,px6,px7,px8,py5,py6,py7,py8,pz5,pz6,pz7,pz8,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      endif
c
      else
c
      if (ietyp.eq.101) then
         call ushl_e101(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     .        wxx1,wxx2,wxx3,wxx4,wyy1,wyy2,wyy3,wyy4,
     .        wzz1,wzz2,wzz3,wzz4,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.102) then
         call ushl_e102(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     .        wxx1,wxx2,wxx3,wxx4,wyy1,wyy2,wyy3,wyy4,
     .        wzz1,wzz2,wzz3,wzz4,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.103) then
         call ushl_e103(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     .        wxx1,wxx2,wxx3,wxx4,wyy1,wyy2,wyy3,wyy4,
     .        wzz1,wzz2,wzz3,wzz4,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.104) then
         call ushl_e104(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     .        wxx1,wxx2,wxx3,wxx4,wyy1,wyy2,wyy3,wyy4,
     .        wzz1,wzz2,wzz3,wzz4,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      elseif (ietyp.eq.105) then
         call ushl_e105(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     .        yhtnx1,yhtnx2,yhtnx3,yhtnx4,
     .        yhtny1,yhtny2,yhtny3,yhtny4,
     .        yhtnz1,yhtnz2,yhtnz3,yhtnz4,
     .        xdof,
     .        dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     .        wxx1,wxx2,wxx3,wxx4,wyy1,wyy2,wyy3,wyy4,
     .        wzz1,wzz2,wzz3,wzz4,
     .        dxdof,
     .        thick,thick,fga,fgb,fgc,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        hl11,hl21,hl31,hl12,hl22,hl32,hl13,hl23,hl33,
     .        uehisv(1,nhsv+1),lft,llt)
      endif
c
      endif
c
      do j=1,ndtot
         do i=lft,llt
            frc(i,j)=frc(i,j)+force(i,j)
         enddo
      enddo
c
      if (is17loc.eq.1) then
         if (iloc.eq.0) then
         do jnod=1,8
            jnd=(jnod-1)/2+1
            jn=3*(2*jnd-jnod)
            jj=(jnd-1)*(6+nxdof)+jn
            do inod=1,8
               ind=(inod-1)/2+1
               in=3*(2*ind-inod)
               ii=(ind-1)*(6+nxdof)+in
               do i=lft,llt
                  sx=hl11(i)*stiff(i,ndtot*jj+ii+1)+
     1                 hl12(i)*stiff(i,ndtot*jj+ii+2)+
     2                 hl13(i)*stiff(i,ndtot*jj+ii+3)
                  sy=hl21(i)*stiff(i,ndtot*jj+ii+1)+
     1                 hl22(i)*stiff(i,ndtot*jj+ii+2)+
     2                 hl23(i)*stiff(i,ndtot*jj+ii+3)
                  sz=hl31(i)*stiff(i,ndtot*jj+ii+1)+
     1                 hl32(i)*stiff(i,ndtot*jj+ii+2)+
     2                 hl33(i)*stiff(i,ndtot*jj+ii+3)
                  t11=sx*hl11(i)
                  t21=sy*hl11(i)
                  t31=sz*hl11(i)
                  t12=sx*hl21(i)
                  t22=sy*hl21(i)
                  t32=sz*hl21(i)
                  t13=sx*hl31(i)
                  t23=sy*hl31(i)
                  t33=sz*hl31(i)
                  sx=hl11(i)*stiff(i,ndtot*(jj+1)+ii+1)+
     1                 hl12(i)*stiff(i,ndtot*(jj+1)+ii+2)+
     2                 hl13(i)*stiff(i,ndtot*(jj+1)+ii+3)
                  sy=hl21(i)*stiff(i,ndtot*(jj+1)+ii+1)+
     1                 hl22(i)*stiff(i,ndtot*(jj+1)+ii+2)+
     2                 hl23(i)*stiff(i,ndtot*(jj+1)+ii+3)
                  sz=hl31(i)*stiff(i,ndtot*(jj+1)+ii+1)+
     1                 hl32(i)*stiff(i,ndtot*(jj+1)+ii+2)+
     2                 hl33(i)*stiff(i,ndtot*(jj+1)+ii+3)
                  t11=t11+sx*hl12(i)
                  t21=t21+sy*hl12(i)
                  t31=t31+sz*hl12(i)
                  t12=t12+sx*hl22(i)
                  t22=t22+sy*hl22(i)
                  t32=t32+sz*hl22(i)
                  t13=t13+sx*hl32(i)
                  t23=t23+sy*hl32(i)
                  t33=t33+sz*hl32(i)
                  sx=hl11(i)*stiff(i,ndtot*(jj+2)+ii+1)+
     1                 hl12(i)*stiff(i,ndtot*(jj+2)+ii+2)+
     2                 hl13(i)*stiff(i,ndtot*(jj+2)+ii+3)
                  sy=hl21(i)*stiff(i,ndtot*(jj+2)+ii+1)+
     1                 hl22(i)*stiff(i,ndtot*(jj+2)+ii+2)+
     2                 hl23(i)*stiff(i,ndtot*(jj+2)+ii+3)
                  sz=hl31(i)*stiff(i,ndtot*(jj+2)+ii+1)+
     1                 hl32(i)*stiff(i,ndtot*(jj+2)+ii+2)+
     2                 hl33(i)*stiff(i,ndtot*(jj+2)+ii+3)
                  t11=t11+sx*hl13(i)
                  t21=t21+sy*hl13(i)
                  t31=t31+sz*hl13(i)
                  t12=t12+sx*hl23(i)
                  t22=t22+sy*hl23(i)
                  t32=t32+sz*hl23(i)
                  t13=t13+sx*hl33(i)
                  t23=t23+sy*hl33(i)
                  t33=t33+sz*hl33(i)
                  stiff(i,ndtot*jj+ii+1)=
     .                 t11
                  stiff(i,ndtot*(jj+1)+ii+1)=
     .                 t12
                  stiff(i,ndtot*(jj+2)+ii+1)=
     .                 t13
                  stiff(i,ndtot*jj+ii+2)=
     .                 t21
                  stiff(i,ndtot*(jj+1)+ii+2)=
     .                 t22
                  stiff(i,ndtot*(jj+2)+ii+2)=
     .                 t23
                  stiff(i,ndtot*jj+ii+3)=
     .                 t31
                  stiff(i,ndtot*(jj+1)+ii+3)=
     .                 t32
                  stiff(i,ndtot*(jj+2)+ii+3)=
     .                 t33
               enddo
            enddo
         enddo
         if (nxdof.gt.0) then
            do jnod=1,4
               jj=(jnod-1)*(6+nxdof)+6
               do jdof=1,nxdof
                  do inod=1,8
                     ind=(inod-1)/2+1
                     in=3*(2*ind-inod)
                     ii=(ind-1)*(6+nxdof)+in
                     do i=lft,llt
                        sx=hl11(i)*stiff(i,ndtot*(jj+jdof-1)+ii+1)+
     1                       hl12(i)*stiff(i,ndtot*(jj+jdof-1)+ii+2)+
     2                       hl13(i)*stiff(i,ndtot*(jj+jdof-1)+ii+3)
                        sy=hl21(i)*stiff(i,ndtot*(jj+jdof-1)+ii+1)+
     1                       hl22(i)*stiff(i,ndtot*(jj+jdof-1)+ii+2)+
     2                       hl23(i)*stiff(i,ndtot*(jj+jdof-1)+ii+3)
                        sz=hl31(i)*stiff(i,ndtot*(jj+jdof-1)+ii+1)+
     1                       hl32(i)*stiff(i,ndtot*(jj+jdof-1)+ii+2)+
     2                       hl33(i)*stiff(i,ndtot*(jj+jdof-1)+ii+3)
                        stiff(i,ndtot*(jj+jdof-1)+ii+1)=sx
                        stiff(i,ndtot*(jj+jdof-1)+ii+2)=sy
                        stiff(i,ndtot*(jj+jdof-1)+ii+3)=sz
                        sx=hl11(i)*stiff(i,ndtot*ii+jj+jdof)+
     1                       hl12(i)*stiff(i,ndtot*(ii+1)+jj+jdof)+
     2                       hl13(i)*stiff(i,ndtot*(ii+2)+jj+jdof)
                        sy=hl21(i)*stiff(i,ndtot*ii+jj+jdof)+
     1                       hl22(i)*stiff(i,ndtot*(ii+1)+jj+jdof)+
     2                       hl23(i)*stiff(i,ndtot*(ii+2)+jj+jdof)
                        sz=hl31(i)*stiff(i,ndtot*ii+jj+jdof)+
     1                       hl32(i)*stiff(i,ndtot*(ii+1)+jj+jdof)+
     2                       hl33(i)*stiff(i,ndtot*(ii+2)+jj+jdof)
                        stiff(i,ndtot*ii+jj+jdof)=sx
                        stiff(i,ndtot*(ii+1)+jj+jdof)=sy
                        stiff(i,ndtot*(ii+2)+jj+jdof)=sz
                     enddo
                  enddo
               enddo
            enddo
         endif
         endif
         nsnd=0
         if (nxdof.gt.0) nsnd=(nxdof-1)/3+1
         do isnd=1,1+nsnd
           idof=0
           nsdf=6
           if (isnd.gt.1) then
             idof=6+3*(isnd-2)
             nsdf=3
             if (isnd.eq.1+nsnd) nsdf=nxdof-(nsnd-1)*3
           endif
           do irnd=1,4
             do isdf=1,nsdf
               m=24*(isnd-1)+6*(irnd-1)+isdf
               irow=m*(m-1)/2
               mm=(6+nxdof)*(irnd-1)+idof+isdf
               do jsnd=1,1+nsnd
                 jdof=0
                 msdf=6
                 if (jsnd.gt.1) then
                   jdof=6+3*(jsnd-2)
                   msdf=3
                   if (jsnd.eq.1+nsnd) msdf=nxdof-(nsnd-1)*3
                 endif
                 do jrnd=1,4
                   do 1 jsdf=1,msdf
                     n=24*(jsnd-1)+6*(jrnd-1)+jsdf
                     if (isolvr(77).ne.3.or.ihgf.ne.0) then
                     if (n.gt.m) goto 1
                     iske=irow+n
                     nn=(6+nxdof)*(jrnd-1)+jdof+jsdf
                     do i=lft,llt
                       ske(i,iske)=ske(i,iske)+
     1                      stiff(i,(mm-1)*ndtot+nn)
                     enddo
                     else
                     nn=(6+nxdof)*(jrnd-1)+jdof+jsdf
                     do i=lft,llt
                       ske_unsym(i,n,m)=ske_unsym(i,n,m)+
     1                      stiff(i,(mm-1)*ndtot+nn)
                     enddo
                     endif
 1                 continue
                 enddo
               enddo
             enddo
           enddo
         enddo
      endif
c
      return
      end
      subroutine usrshl_hgs(ske,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     adds hourglass stiffness to tangent stiffness matrix
c
      include 'nlqparm'
      include 'matflr.inc'
      include 'nhisparm.inc'
c
      common/prescloc/voltot(nlq)
      common/failuloc/sieu(nlq),fail(nlq)
      common/hourgloc/ymod(nlq),gmod(nlq),ifsv(nlq)
      common/bk12loc/b12,b2,qhg,qhgm,qhgb,qhgw
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux11loc/
     &ft31(nlq),ft32(nlq),ft33(nlq),ft41(nlq),ft42(nlq),ft43(nlq),
     &htx(nlq),hty(nlq),gm(nlq,4),
     &bsum(nlq),qhx(nlq),qhy(nlq),qwz(nlq),qtx(nlq),qty(nlq)
      common/aux12loc/
     1 wxx1(nlq),wxx2(nlq),wxx3(nlq),wxx4(nlq),
     2 wyy1(nlq),wyy2(nlq),wyy3(nlq),wyy4(nlq),
     3 wzz1(nlq),wzz2(nlq),wzz3(nlq),wzz4(nlq),
     4 a13(nlq),a23(nlq),a33(nlq)
      common/aux13loc/
     1 zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     2 gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     3 gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     4 x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     5 x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/aux35loc/rhoa(nlq),cxx(nlq),fcl(nlq),fcq(nlq)
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/aux45loc/crap(nlq,124),
     1 dndx(nlq,4),dndy(nlq,4),hnhx(nlq,4),hnhy(nlq,4)
      common/subtssloc/dt1siz(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      dimension qs1(nlq),qs2(nlq),qs3(nlq),qs4(nlq),qs5(nlq)
      real mmode,mcoef
      dimension xcarr(nlq,6),xmt1(nlq,3,3),xmt2(nlq,3,3)
      dimension ske(nlq,*)
c
c     Hourglass stiffnesses
c
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux11loc/)
c$omp threadprivate (/aux12loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/aux45loc/)
c$omp threadprivate (/hourgloc/)
c$omp threadprivate (/bk12loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/failuloc/)
c$omp threadprivate (/prescloc/)
c$omp threadprivate (/soundloc/)
c$omp threadprivate (/subtssloc/)
      tmode=qhgb*ymod(lft)/1920.0
      wmode=qhgw*gmod(lft)/120.00
      mmode=qhgm*ymod(lft)/80.000
c
      do i=lft,llt
c
c     Compute tau
c
         htxi  =xx1(i)+xx3(i)-xx2(i)-xx4(i)
         htyi  =yy1(i)+yy3(i)-yy2(i)-yy4(i)
         gm(i,1)= 1.-htxi*hnhx(i,1)-htyi*hnhy(i,1)
         gm(i,2)=-1.-htxi*hnhx(i,2)-htyi*hnhy(i,2)
         gm(i,3)= 1.-htxi*hnhx(i,3)-htyi*hnhy(i,3)
         gm(i,4)=-1.-htxi*hnhx(i,4)-htyi*hnhy(i,4)
c
      enddo
      if (ifsv(lft).eq.3) then
c
c     Stiffness based hourglass
c
         do i=lft,llt
c
c     Hourglass stresses
c
            bsumi = hnhx(i,1)**2+hnhx(i,2)**2+hnhx(i,3)**2+hnhx(i,4)**2+
     1           hnhy(i,1)**2+hnhy(i,2)**2+hnhy(i,3)**2+hnhy(i,4)**2
c
            thck=cvltot(i)*area(i)
            coef=bsumi*cvltot(i)
            tcoef=tmode*coef*thck**2
            mcoef=mmode*coef
            wcoef=wmode*coef*thck**2*area(i)
c
            xcarr(i,1)=mcoef
            xcarr(i,2)=mcoef
            xcarr(i,3)=wcoef
            xcarr(i,4)=tcoef
            xcarr(i,5)=tcoef
            xcarr(i,6)=0.
c
         enddo
c
      else
c
c     Viscous based hourglass
c
         hgfac=rhoa(lft)*sndspd(lft)
c
         do i=lft,llt
c
c     Hourglass stresses
c
            thck=cvltot(i)*area(i)
            coef=hgfac*sqrt(abs(sarea(i)))*thck/(dt1siz(i)+1.e-16)
            wcoef=qhgw*coef
            mcoef=qhgm*coef
            tcoef=qhgb*.01*coef*thck**2
c
            xcarr(i,1)=mcoef
            xcarr(i,2)=mcoef
            xcarr(i,3)=wcoef
            xcarr(i,4)=tcoef
            xcarr(i,5)=tcoef
            xcarr(i,6)=0.
c
         enddo
c
      endif
c
      do i=lft,llt
c
         xmt2(i,1,1)=xcarr(i,4)*hl11(i)
         xmt2(i,2,1)=xcarr(i,5)*hl12(i)
         xmt2(i,3,1)=xcarr(i,6)*hl13(i)
         xmt2(i,1,2)=xcarr(i,4)*hl21(i)
         xmt2(i,2,2)=xcarr(i,5)*hl22(i)
         xmt2(i,3,2)=xcarr(i,6)*hl23(i)
         xmt2(i,1,3)=xcarr(i,4)*hl31(i)
         xmt2(i,2,3)=xcarr(i,5)*hl32(i)
         xmt2(i,3,3)=xcarr(i,6)*hl33(i)
c
         tmp1=hl11(i)*xmt2(i,1,1)+hl12(i)*xmt2(i,2,1)+
     1        hl13(i)*xmt2(i,3,1)
         tmp2=hl21(i)*xmt2(i,1,1)+hl22(i)*xmt2(i,2,1)+
     1        hl23(i)*xmt2(i,3,1)
         xmt2(i,3,1)=hl31(i)*xmt2(i,1,1)+hl32(i)*xmt2(i,2,1)+
     1        hl33(i)*xmt2(i,3,1)
         xmt2(i,2,1)=tmp2
         xmt2(i,1,1)=tmp1
         tmp1=hl11(i)*xmt2(i,1,2)+hl12(i)*xmt2(i,2,2)+
     1        hl13(i)*xmt2(i,3,2)
         tmp2=hl21(i)*xmt2(i,1,2)+hl22(i)*xmt2(i,2,2)+
     1        hl23(i)*xmt2(i,3,2)
         xmt2(i,3,2)=hl31(i)*xmt2(i,1,2)+hl32(i)*xmt2(i,2,2)+
     1        hl33(i)*xmt2(i,3,2)
         xmt2(i,2,2)=tmp2
         xmt2(i,1,2)=tmp1
         tmp1=hl11(i)*xmt2(i,1,3)+hl12(i)*xmt2(i,2,3)+
     1        hl13(i)*xmt2(i,3,3)
         tmp2=hl21(i)*xmt2(i,1,3)+hl22(i)*xmt2(i,2,3)+
     1        hl23(i)*xmt2(i,3,3)
         xmt2(i,3,3)=hl31(i)*xmt2(i,1,3)+hl32(i)*xmt2(i,2,3)+
     1        hl33(i)*xmt2(i,3,3)
         xmt2(i,2,3)=tmp2
         xmt2(i,1,3)=tmp1
c
      enddo
c
      do i=lft,llt
c
         xmt1(i,1,1)=xcarr(i,1)*hl11(i)
         xmt1(i,2,1)=xcarr(i,2)*hl12(i)
         xmt1(i,3,1)=xcarr(i,3)*hl13(i)
         xmt1(i,1,2)=xcarr(i,1)*hl21(i)
         xmt1(i,2,2)=xcarr(i,2)*hl22(i)
         xmt1(i,3,2)=xcarr(i,3)*hl23(i)
         xmt1(i,1,3)=xcarr(i,1)*hl31(i)
         xmt1(i,2,3)=xcarr(i,2)*hl32(i)
         xmt1(i,3,3)=xcarr(i,3)*hl33(i)
c
         tmp1=hl11(i)*xmt1(i,1,1)+hl12(i)*xmt1(i,2,1)+
     1        hl13(i)*xmt1(i,3,1)
         tmp2=hl21(i)*xmt1(i,1,1)+hl22(i)*xmt1(i,2,1)+
     1        hl23(i)*xmt1(i,3,1)
         xmt1(i,3,1)=hl31(i)*xmt1(i,1,1)+hl32(i)*xmt1(i,2,1)+
     1        hl33(i)*xmt1(i,3,1)
         xmt1(i,2,1)=tmp2
         xmt1(i,1,1)=tmp1
         tmp1=hl11(i)*xmt1(i,1,2)+hl12(i)*xmt1(i,2,2)+
     1        hl13(i)*xmt1(i,3,2)
         tmp2=hl21(i)*xmt1(i,1,2)+hl22(i)*xmt1(i,2,2)+
     1        hl23(i)*xmt1(i,3,2)
         xmt1(i,3,2)=hl31(i)*xmt1(i,1,2)+hl32(i)*xmt1(i,2,2)+
     1        hl33(i)*xmt1(i,3,2)
         xmt1(i,2,2)=tmp2
         xmt1(i,1,2)=tmp1
         tmp1=hl11(i)*xmt1(i,1,3)+hl12(i)*xmt1(i,2,3)+
     1        hl13(i)*xmt1(i,3,3)
         tmp2=hl21(i)*xmt1(i,1,3)+hl22(i)*xmt1(i,2,3)+
     1        hl23(i)*xmt1(i,3,3)
         xmt1(i,3,3)=hl31(i)*xmt1(i,1,3)+hl32(i)*xmt1(i,2,3)+
     1        hl33(i)*xmt1(i,3,3)
         xmt1(i,2,3)=tmp2
         xmt1(i,1,3)=tmp1
c
      enddo
c
      do m=1,4
         do n=1,m
            do k=1,3
               irow1=6*(m-1)+k
               irow1=irow1*(irow1-1)/2
               irow2=6*(m-1)+k+3
               irow2=irow2*(irow2-1)/2
               if (m.eq.n) then
                  do l=1,k
                     iske1=irow1+6*(n-1)+l
                     iske2=irow2+6*(n-1)+l+3
                     do i=lft,llt
                        ske(i,iske1)=ske(i,iske1)+
     1                       gm(i,m)*gm(i,n)*xmt1(i,k,l)
                        ske(i,iske2)=ske(i,iske2)+
     1                       gm(i,m)*gm(i,n)*xmt2(i,k,l)
                     enddo
                  enddo
               else
                  do l=1,3
                     iske1=irow1+6*(n-1)+l
                     iske2=irow2+6*(n-1)+l+3
                     do i=lft,llt
                        ske(i,iske1)=ske(i,iske1)+
     1                       gm(i,m)*gm(i,n)*xmt1(i,k,l)
                        ske(i,iske2)=ske(i,iske2)+
     1                       gm(i,m)*gm(i,n)*xmt2(i,k,l)
                     enddo
                  enddo
               endif
            enddo
         enddo
      enddo
c
      return
      end
      subroutine usrshl_trf(nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transforms internal force to global system,
c     disregarding extra degrees of freedom
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/bel7loc/)
      do l=1,4
         ll=(l-1)*(6+nxdof)
         do i=lft,llt
c
            fx=hl11(i)*frc(i,ll+1)+hl12(i)*frc(i,ll+2)+
     1           hl13(i)*frc(i,ll+3)
            fy=hl21(i)*frc(i,ll+1)+hl22(i)*frc(i,ll+2)+
     1           hl23(i)*frc(i,ll+3)
            fz=hl31(i)*frc(i,ll+1)+hl32(i)*frc(i,ll+2)+
     1           hl33(i)*frc(i,ll+3)
            frc(i,ll+1)=fx
            frc(i,ll+2)=fy
            frc(i,ll+3)=fz
c
            fx=hl11(i)*frc(i,ll+4)+hl12(i)*frc(i,ll+5)+
     1           hl13(i)*frc(i,ll+6)
            fy=hl21(i)*frc(i,ll+4)+hl22(i)*frc(i,ll+5)+
     1           hl23(i)*frc(i,ll+6)
            fz=hl31(i)*frc(i,ll+4)+hl32(i)*frc(i,ll+5)+
     1           hl33(i)*frc(i,ll+6)
            frc(i,ll+4)=fx
            frc(i,ll+5)=fy
            frc(i,ll+6)=fz
c
         enddo
      enddo
c
      return
      end
      subroutine usrshl_stc(fibl,rhs,sthick,iunf,nipp,lft,llt,iloc)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'implicit1.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
      common/aux00loc/ds11(nlq),ds12(nlq),ds13(nlq),ds22(nlq),
     1     ds23(nlq),ds33(nlq),sjunk(nlq,4),str33(nlq)
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),
     &gl23(nlq),gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq)
      common/aux33loc/
     1 ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ixs(nlq,4),mxt(nlq)
      common/numcpu/ncpu,ncpua,ncpub,lenvec(8)
      common/vect13loc/
     1 yhtnx1(nlq),yhtny1(nlq),yhtnz1(nlq),
     2 yhtnx2(nlq),yhtny2(nlq),yhtnz2(nlq),
     3 yhtnx3(nlq),yhtny3(nlq),yhtnz3(nlq),
     4 yhtnx4(nlq),yhtny4(nlq),yhtnz4(nlq),
     5 yhatx1(nlq),yhaty1(nlq),yhatz1(nlq),
     6 yhatx2(nlq),yhaty2(nlq),yhatz2(nlq),
     7 yhatx3(nlq),yhaty3(nlq),yhatz3(nlq),
     8 yhatx4(nlq),yhaty4(nlq),yhatz4(nlq),
     9 yhtmx1(nlq),yhtmy1(nlq),yhtmz1(nlq),
     & yhtmx2(nlq),yhtmy2(nlq),yhtmz2(nlq),
     & yhtmx3(nlq),yhtmy3(nlq),yhtmz3(nlq),
     & yhtmx4(nlq),yhtmy4(nlq),yhtmz4(nlq)
      dimension fibl(9,*),rhs(27,*),sthick(*)
c
c     Account for thickness changes
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux00loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/vect13loc/)
      if (iunf.ne.0) then
         if (isolvr(18).eq.0) then
         if (nipp.gt.0) then
         if (iloc.ne.0) then
            do i=lft,llt
            t11=gl11(i)*ds11(i)+gl12(i)*ds12(i)+gl13(i)*ds13(i)
            t21=gl21(i)*ds11(i)+gl22(i)*ds12(i)+gl23(i)*ds13(i)
            t31=gl31(i)*ds11(i)+gl32(i)*ds12(i)+gl33(i)*ds13(i)
            t12=gl11(i)*ds12(i)+gl12(i)*ds22(i)+gl13(i)*ds23(i)
            t22=gl21(i)*ds12(i)+gl22(i)*ds22(i)+gl23(i)*ds23(i)
            t32=gl31(i)*ds12(i)+gl32(i)*ds22(i)+gl33(i)*ds23(i)
            t13=gl11(i)*ds13(i)+gl12(i)*ds23(i)+gl13(i)*ds33(i)
            t23=gl21(i)*ds13(i)+gl22(i)*ds23(i)+gl23(i)*ds33(i)
            t33=gl31(i)*ds13(i)+gl32(i)*ds23(i)+gl33(i)*ds33(i)
            ds11(i)=t11*gl11(i)+t12*gl12(i)+t13*gl13(i)
            ds12(i)=t21*gl11(i)+t22*gl12(i)+t23*gl13(i)
            ds13(i)=t31*gl11(i)+t32*gl12(i)+t33*gl13(i)
            ds22(i)=t21*gl21(i)+t22*gl22(i)+t23*gl23(i)
            ds23(i)=t31*gl21(i)+t32*gl22(i)+t33*gl23(i)
            ds23(i)=t31*gl31(i)+t32*gl32(i)+t33*gl33(i)
            enddo
         endif
         do i=lft,llt
            yht1=yhtmx1(i)
            yht2=yhtmy1(i)
            yht3=yhtmz1(i)
            c1=ds11(i)*yht1+ds12(i)*yht2+ds13(i)*yht3
            c2=ds12(i)*yht1+ds22(i)*yht2+ds23(i)*yht3
            c3=ds13(i)*yht1+ds23(i)*yht2+ds33(i)*yht3
            sn=  c1*yht1+  c2*yht2+  c3*yht3+1.
            fibl(1,i)=sn*fibl(1,i)
         enddo
         do i=lft,llt
            yht1=yhtmx2(i)
            yht2=yhtmy2(i)
            yht3=yhtmz2(i)
            c1=ds11(i)*yht1+ds12(i)*yht2+ds13(i)*yht3
            c2=ds12(i)*yht1+ds22(i)*yht2+ds23(i)*yht3
            c3=ds13(i)*yht1+ds23(i)*yht2+ds33(i)*yht3
            sn=  c1*yht1+  c2*yht2+  c3*yht3+1.
            fibl(2,i)=sn*fibl(2,i)
         enddo
         do i=lft,llt
            yht1=yhtmx3(i)
            yht2=yhtmy3(i)
            yht3=yhtmz3(i)
            c1=ds11(i)*yht1+ds12(i)*yht2+ds13(i)*yht3
            c2=ds12(i)*yht1+ds22(i)*yht2+ds23(i)*yht3
            c3=ds13(i)*yht1+ds23(i)*yht2+ds33(i)*yht3
            sn=  c1*yht1+  c2*yht2+  c3*yht3+1.
            fibl(3,i)=sn*fibl(3,i)
         enddo
         do i=lft,llt
            yht1=yhtmx4(i)
            yht2=yhtmy4(i)
            yht3=yhtmz4(i)
            c1=ds11(i)*yht1+ds12(i)*yht2+ds13(i)*yht3
            c2=ds12(i)*yht1+ds22(i)*yht2+ds23(i)*yht3
            c3=ds13(i)*yht1+ds23(i)*yht2+ds33(i)*yht3
            sn=  c1*yht1+  c2*yht2+  c3*yht3+1.
            fibl(4,i)=sn*fibl(4,i)
         enddo
         else
            do i=lft,llt
               fibl(1,i)=thick(i)
               fibl(2,i)=fga(i)
               fibl(3,i)=fgb(i)
               fibl(4,i)=fgc(i)
            enddo
         endif
         endif
c
         if (lenvec(1).ne.0) then
            do i=lft,llt
               rhs(25,i)=
     1              .25*(fibl(1,i)+fibl(2,i)+fibl(3,i)+fibl(4,i))
            enddo
         else
            do i=lft,llt
               avgthk=.25*(fibl(1,i)+fibl(2,i)+fibl(3,i)+fibl(4,i))
               sthick(ix1(i))=max(sthick(ix1(i)),avgthk)
               sthick(ix2(i))=max(sthick(ix2(i)),avgthk)
               sthick(ix3(i))=max(sthick(ix3(i)),avgthk)
               sthick(ix4(i))=max(sthick(ix4(i)),avgthk)
            enddo
         endif
c
      else
c
         if (isolvr(18).eq.0) then
         if (nipp.gt.0) then
         do i=lft,llt
            str33(i)=1.+str33(i)
            fibl(1,i)=str33(i)*fibl(1,i)
         enddo
         else
            do i=lft,llt
               fibl(1,i)=thick(i)
            enddo
         endif
         endif
c
         if (lenvec(1).ne.0) then
            do i=lft,llt
               rhs(25,i)=fibl(1,i)
            enddo
         else
            do i=lft,llt
               sthick(ix1(i))=max(sthick(ix1(i)),fibl(1,i))
               sthick(ix2(i))=max(sthick(ix2(i)),fibl(1,i))
               sthick(ix3(i))=max(sthick(ix3(i)),fibl(1,i))
               sthick(ix4(i))=max(sthick(ix4(i)),fibl(1,i))
            enddo
         endif
c
      endif
c
      return
      end
      subroutine usrshl_fs(nxdof,rhs,mte,lft,llt,rhsl)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'matflr.inc'
      include 'memaia.inc'
      include 'nhisparm.inc'
      include 'shlioc.inc'
      common/aux00loc/
     & sig1m(nlq),sig2m(nlq),sig4m(nlq),sig1n(nlq),sig2n(nlq),
     & sig4n(nlq),sig5n(nlq),sig6n(nlq),sig5l(nlq),sig6l(nlq),
     & str33(nlq),enginc(nlq)
      common/aux01loc/
     &ft11(nlq),ft12(nlq),ft13(nlq),ft21(nlq),ft22(nlq),ft23(nlq),
     &fm11(nlq),fm12(nlq),fm21(nlq),fm22(nlq),
     &fm31(nlq),fm32(nlq),fm41(nlq),fm42(nlq),
     &fmr11(nlq),fmr12(nlq),fmr21(nlq),fmr22(nlq),fmr31(nlq),
     &fmr32(nlq),fmr41(nlq),fmr42(nlq),sg5(nlq),sg6(nlq)
      common/aux7loc/
     1 vx1(nlq),vx2(nlq),vx3(nlq),vx4(nlq),
     2 vx5(nlq),vx6(nlq),vx7(nlq),vx8(nlq),
     3 vy1(nlq),vy2(nlq),vy3(nlq),vy4(nlq),
     4 vy5(nlq),vy6(nlq),vy7(nlq),vy8(nlq),
     5 vz1(nlq),vz2(nlq),vz3(nlq),vz4(nlq),
     6 vz5(nlq),vz6(nlq),vz7(nlq),vz8(nlq)
      common/aux10loc/area(nlq),
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/aux11loc/
     &ft31(nlq),ft32(nlq),ft33(nlq),ft41(nlq),ft42(nlq),ft43(nlq),
     &htx(nlq),hty(nlq),gm(nlq,4),
     &bsum(nlq),qhx(nlq),qhy(nlq),qwz(nlq),qtx(nlq),qty(nlq)
      real mx1,my1,mz1,mx2,my2,mz2,mx3,my3,mz3,mx4,my4,mz4
      common/aux13loc/
     &zeta(nlq),thick(nlq),fga(nlq),fgb(nlq),fgc(nlq),
     &gl11(nlq),gl12(nlq),gl13(nlq),gl21(nlq),gl22(nlq),gl23(nlq),
     &gl31(nlq),gl32(nlq),gl33(nlq),
     &x1(nlq),y1(nlq),z1(nlq),x2(nlq),y2(nlq),z2(nlq),
     &x3(nlq),y3(nlq),z3(nlq),x4(nlq),y4(nlq),z4(nlq),
     &fx1(nlq),fy1(nlq),fz1(nlq),fx2(nlq),fy2(nlq),fz2(nlq),
     &fx3(nlq),fy3(nlq),fz3(nlq),fx4(nlq),fy4(nlq),fz4(nlq),
     &mx1(nlq),my1(nlq),mz1(nlq),mx2(nlq),my2(nlq),mz2(nlq),
     &mx3(nlq),my3(nlq),mz3(nlq),mx4(nlq),my4(nlq),mz4(nlq)
      common/aux33loc/
     1 ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ixs(nlq,4),mxt(nlq)
      common/aux35loc/rhoa(nlq),cxx(nlq),fcl(nlq),fcq(nlq)
      common/berwcmloc/xll(nlq),rigx(nlq),rigy(nlq)
      common/bk12loc/b12,b2,qhg,qhgm,qhgb,qhgw
      common/failuloc/sieu(nlq),fail(nlq)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 ncs10,ncs11,ncs12,ncs13,ncs14,ncs15,
     1 numcsd,csdinc,csdout,output,slnew,future(10)
      common/csfsavloc/savfrc(nlq,24),svfail(nlq),ndof,ifail
      common/hourgloc/ymod(nlq),gmod(nlq),ifsv(nlq)
      common/sorterloc/nnc,lczc
      common/soundloc/sndspd(nlq),sndsp(nlq),diagm(nlq),sarea(nlq),
     . dxl(nlq)
      common/subtssloc/dt1siz(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR znnc->nnc, zlczc->lczc
c
      real znnc,zlczc
      integer
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/znnc, zlczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
      real mmode,ies
      real mcoef
      dimension e(3,1),f(3,1),qs(9,1)
      dimension qs1(nlq),qs2(nlq),qs3(nlq),qs4(nlq),qs5(nlq)
      dimension fibl(9,1),rhs(27,*),rhsl(24,*)
c
c$omp threadprivate (/aux00loc/)
c$omp threadprivate (/aux01loc/)
c$omp threadprivate (/aux7loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux11loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/bk12loc/)
c$omp threadprivate (/soundloc/)
c$omp threadprivate (/hourgloc/)
c$omp threadprivate (/failuloc/)
c$omp threadprivate (/subtssloc/)
c$omp threadprivate (/csfsavloc/)
c$omp threadprivate (/sorterloc/)
c$omp threadprivate (/berwcmloc/)
c$omp threadprivate (/bel7loc/)
      ifail=0
c
c     Store right hand side
c
      if (mtfailx(mte).ne.1) then
        do l=1,4
          ll=(l-1)*(6+nxdof)
          do i=lft,llt
            rhs((l-1)*3+1,i)=frc(i,ll+1)
            rhs((l-1)*3+2,i)=frc(i,ll+2)
            rhs((l-1)*3+3,i)=frc(i,ll+3)
            rhs((l-1)*3+13,i)=frc(i,ll+4)
            rhs((l-1)*3+14,i)=frc(i,ll+5)
            rhs((l-1)*3+15,i)=frc(i,ll+6)
          enddo
        enddo
c
        if (nxdof.gt.0) then
          do l=1,4
            ll=(l-1)*(6+nxdof)
            do k=1,nxdof
              do i=lft,llt
                rhsl((l-1)*3+k,i)=frc(i,ll+6+k)
              enddo
            enddo
          enddo
        endif
c
      else
        ifail=1
c
c       Store right hand side
c
        do l=1,4
          ll=(l-1)*(6+nxdof)
          do i=lft,llt
            rhs((l-1)*3+1,i)=fail(i)*frc(i,ll+1)
            rhs((l-1)*3+2,i)=fail(i)*frc(i,ll+2)
            rhs((l-1)*3+3,i)=fail(i)*frc(i,ll+3)
            rhs((l-1)*3+13,i)=fail(i)*frc(i,ll+4)
            rhs((l-1)*3+14,i)=fail(i)*frc(i,ll+5)
            rhs((l-1)*3+15,i)=fail(i)*frc(i,ll+6)
          enddo
        enddo
c
        if (nxdof.gt.0) then
          do l=1,4
            ll=(l-1)*(6+nxdof)
            do k=1,nxdof
              do i=lft,llt
                rhsl((l-1)*3+k,i)=frc(i,ll+6+k)*fail(i)
              enddo
            enddo
          enddo
        endif
      endif
c
c     For output
c
      if (output) then
        do l=1,4
          ll=(l-1)*(6+nxdof)
          do i=lft,llt
            savfrc((l-1)*3+1,i)=frc(i,ll+1)
             savfrc((l-1)*3+2,i)=frc(i,ll+2)
             savfrc((l-1)*3+3,i)=frc(i,ll+3)
             savfrc((l-1)*3+13,i)=frc(i,ll+4)
             savfrc((l-1)*3+14,i)=frc(i,ll+5)
             savfrc((l-1)*3+15,i)=frc(i,ll+6)
          enddo
        enddo
c
         ndof=4
         if (ifail.eq.1) call blkcpy (fail,svfail,nnlq)
      endif
c
      return
      end
      subroutine usrshl_ft(nxdof,e,f,mte,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
c
c     dynamic memory allocation stuff.  This has to be a C style 
c     include because of the use of integer*8
c
      integer i_mem(0:1)
      real r_mem(0:1)
      integer*8 i8_mem(0:1)
      real*8 r8_mem(0:1)
      real*8 real8_mem
      integer*8 m_mem(0:1)
      common/dynmem/real8_mem(0:1)
      equivalence (i_mem,r_mem,i8_mem,r8_mem,real8_mem,m_mem)
      integer*8 mem_alloc, mems_alloc,mem_realloc, memh_ptr
      integer mem_length,memh_alloc,memh_realloc,memh_length
      integer memh_enum,memh_enums,memh_attach
c      integer mem_newmark,mem_getmark,memh_getmark
      integer*8 memh_detach,memh_ralloc
      integer*8 m_to_m8
      integer*8 m_to_mm
      external mem_alloc, mems_alloc, mem_realloc, mem_length
      external memh_alloc, memh_realloc, memh_length, memh_ptr
      external memh_enum,memh_attach,memh_detach,memh_ralloc
      external memh_enums
      external m_to_m8
      external m_to_mm
      integer*8 dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr
      integer*8 dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma
      integer*8 dm_xtpz,dm_disp
      integer*8 dm_me1,dm_me2,dm_me3
      integer*8 dm_masp,dm_masr
      integer*8 dm_xrb,dm_yrb,dm_zrb
      integer*8 dm_axrb,dm_ayrb,dm_azrb
      integer*8 dm_rbfx,dm_rbfy,dm_rbfz
      integer*8 dm_n45,dm_n46,dm_n47
      integer*8 dm_rwsx
      integer len_n45,len_n46,len_n47
      common/dynmem1/ 
     . dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr,
     . dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma,
     . dm_xtpz,dm_disp,
     . dm_me1,dm_me2,dm_me3,
     . dm_masp,dm_masr,
     . dm_xrb,dm_yrb,dm_zrb,
     . dm_axrb,dm_ayrb,dm_azrb,
     . dm_rbfx,dm_rbfy,dm_rbfz,
     . dm_n45,dm_n46,dm_n47,dm_rwsx
      common/dynmem1l/len_n45,len_n46,len_n47
c
      integer*8 mem_allocm,mems_allocm,mem_reallocm,mem_loc
      integer memh_allocm,memh_reallocm
      external mem_allocm,mems_allocm,mem_reallocm,mem_loc
      external memh_allocm,memh_reallocm
      integer*8 mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      integer*8 mem_reallocmchk,mem_reallocchk
      integer memh_allocmchk,memh_allocchk
      external mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      external mem_reallocmchk,mem_reallocchk
      external memh_allocmchk,memh_allocchk
      include 'matflr.inc'
      include 'memaia.inc'
      include 'nhisparm.inc'
      dimension e(3,1),f(3,1)
      common/aux33loc/
     1 ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ixs(nlq,4),mxt(nlq)
      common/sorterloc/nnc,lczc
      common/aux45loc/crap(nlq,124),
     1 dndx(nlq,4),dndy(nlq,4),hnhx(nlq,4),hnhy(nlq,4)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 ncs10,ncs11,ncs12,ncs13,ncs14,ncs15,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsavloc/savfrc(nlq,24),svfail(nlq),ndof,ifail
      common/failuloc/sieu(nlq),fail(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR znnc->nnc, zlczc->lczc
c
      real znnc,zlczc
      integer
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/znnc, zlczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
c
c$omp threadprivate (/sorterloc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/aux45loc/)
c$omp threadprivate (/failuloc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/csfsavloc/)
      do 85 n=1,nnc
c        lcn=lczc+n+ns12-1
c        i0=ia(lcn)
c        i1=ia(lcn+1)-1
         i0=i_mem(dm_snfegc+lczc+n-1)
         i1=i_mem(dm_snfegc+lczc+n  )-1
         if (i1.lt.lft) go to 85
         if (i0.gt.llt) go to 90
         i0=max(i0,lft)
         i1=min(i1,llt)
         if (mtfailx(mte).ne.1) then
            do i=i0,i1
               e(1,ix1(i))=e(1,ix1(i))-frc(i,1)
               e(2,ix1(i))=e(2,ix1(i))-frc(i,2)
               e(3,ix1(i))=e(3,ix1(i))-frc(i,3)
               f(1,ix1(i))=f(1,ix1(i))-frc(i,4)
               f(2,ix1(i))=f(2,ix1(i))-frc(i,5)
               f(3,ix1(i))=f(3,ix1(i))-frc(i,6)
               e(1,ix3(i))=e(1,ix3(i))-frc(i,13+2*nxdof)
               e(2,ix3(i))=e(2,ix3(i))-frc(i,14+2*nxdof)
               e(3,ix3(i))=e(3,ix3(i))-frc(i,15+2*nxdof)
               f(1,ix3(i))=f(1,ix3(i))-frc(i,16+2*nxdof)
               f(2,ix3(i))=f(2,ix3(i))-frc(i,17+2*nxdof)
               f(3,ix3(i))=f(3,ix3(i))-frc(i,18+2*nxdof)
            enddo
            do i=i0,i1
               e(1,ix2(i))=e(1,ix2(i))-frc(i,7+nxdof)
               e(2,ix2(i))=e(2,ix2(i))-frc(i,8+nxdof)
               e(3,ix2(i))=e(3,ix2(i))-frc(i,9+nxdof)
               f(1,ix2(i))=f(1,ix2(i))-frc(i,10+nxdof)
               f(2,ix2(i))=f(2,ix2(i))-frc(i,11+nxdof)
               f(3,ix2(i))=f(3,ix2(i))-frc(i,12+nxdof)
               e(1,ix4(i))=e(1,ix4(i))-frc(i,19+3*nxdof)
               e(2,ix4(i))=e(2,ix4(i))-frc(i,20+3*nxdof)
               e(3,ix4(i))=e(3,ix4(i))-frc(i,21+3*nxdof)
               f(1,ix4(i))=f(1,ix4(i))-frc(i,22+3*nxdof)
               f(2,ix4(i))=f(2,ix4(i))-frc(i,23+3*nxdof)
               f(3,ix4(i))=f(3,ix4(i))-frc(i,24+3*nxdof)
            enddo
            if (nxdof.gt.0) then
               do j=1,nxdof
                  nsnd=(j-1)/3+1
                  nsdf=j-(nsnd-1)*3
                  do i=i0,i1
                     e(nsdf,ixshl(i,1,nsnd))=e(nsdf,ixshl(i,1,nsnd))-
     1                   frc(i,6+j)
                     e(nsdf,ixshl(i,3,nsnd))=e(nsdf,ixshl(i,3,nsnd))-
     1                    frc(i,18+2*nxdof+j)
                  enddo
                  do i=i0,i1
                     e(nsdf,ixshl(i,2,nsnd))=e(nsdf,ixshl(i,2,nsnd))-
     1                   frc(i,12+nxdof+j)
                     e(nsdf,ixshl(i,4,nsnd))=e(nsdf,ixshl(i,4,nsnd))-
     1                    frc(i,24+3*nxdof+j)
                  enddo
               enddo
            endif
c
         else
            do i=i0,i1
               e(1,ix1(i))=e(1,ix1(i))-fail(i)*frc(i,1)
               e(2,ix1(i))=e(2,ix1(i))-fail(i)*frc(i,2)
               e(3,ix1(i))=e(3,ix1(i))-fail(i)*frc(i,3)
               f(1,ix1(i))=f(1,ix1(i))-fail(i)*frc(i,4)
               f(2,ix1(i))=f(2,ix1(i))-fail(i)*frc(i,5)
               f(3,ix1(i))=f(3,ix1(i))-fail(i)*frc(i,6)
               e(1,ix3(i))=e(1,ix3(i))-fail(i)*frc(i,13+2*nxdof)
               e(2,ix3(i))=e(2,ix3(i))-fail(i)*frc(i,14+2*nxdof)
               e(3,ix3(i))=e(3,ix3(i))-fail(i)*frc(i,15+2*nxdof)
               f(1,ix3(i))=f(1,ix3(i))-fail(i)*frc(i,16+2*nxdof)
               f(2,ix3(i))=f(2,ix3(i))-fail(i)*frc(i,17+2*nxdof)
               f(3,ix3(i))=f(3,ix3(i))-fail(i)*frc(i,18+2*nxdof)
            enddo
            do i=i0,i1
               e(1,ix2(i))=e(1,ix2(i))-fail(i)*frc(i,7+nxdof)
               e(2,ix2(i))=e(2,ix2(i))-fail(i)*frc(i,8+nxdof)
               e(3,ix2(i))=e(3,ix2(i))-fail(i)*frc(i,9+nxdof)
               f(1,ix2(i))=f(1,ix2(i))-fail(i)*frc(i,10+nxdof)
               f(2,ix2(i))=f(2,ix2(i))-fail(i)*frc(i,11+nxdof)
               f(3,ix2(i))=f(3,ix2(i))-fail(i)*frc(i,12+nxdof)
               e(1,ix4(i))=e(1,ix4(i))-fail(i)*frc(i,19+3*nxdof)
               e(2,ix4(i))=e(2,ix4(i))-fail(i)*frc(i,20+3*nxdof)
               e(3,ix4(i))=e(3,ix4(i))-fail(i)*frc(i,21+3*nxdof)
               f(1,ix4(i))=f(1,ix4(i))-fail(i)*frc(i,22+3*nxdof)
               f(2,ix4(i))=f(2,ix4(i))-fail(i)*frc(i,23+3*nxdof)
               f(3,ix4(i))=f(3,ix4(i))-fail(i)*frc(i,24+3*nxdof)
            enddo
            if (nxdof.gt.0) then
               do j=1,nxdof
                  nsnd=(j-1)/3+1
                  nsdf=j-3*(nsnd-1)
                  do i=i0,i1
                     e(nsdf,ixshl(i,1,nsnd))=e(nsdf,ixshl(i,1,nsnd))-
     1                    fail(i)*frc(i,6+j)
                     e(nsdf,ixshl(i,3,nsnd))=e(nsdf,ixshl(i,3,nsnd))-
     1                    fail(i)*frc(i,18+2*nxdof+j)
                  enddo
                  do i=i0,i1
                     e(nsdf,ixshl(i,2,nsnd))=e(nsdf,ixshl(i,2,nsnd))-
     1                    fail(i)*frc(i,12+nxdof+j)
                     e(nsdf,ixshl(i,4,nsnd))=e(nsdf,ixshl(i,4,nsnd))-
     1                    fail(i)*frc(i,24+3*nxdof+j)
                  enddo
               enddo
            endif
         endif
   85 continue
   90 continue
      if (output) then
         do i=lft,llt
            savfrc(i, 1)= frc(i,1)
            savfrc(i, 2)= frc(i,2)
            savfrc(i, 3)= frc(i,3)
            savfrc(i, 4)= frc(i,4)
            savfrc(i, 5)= frc(i,5)
            savfrc(i, 6)= frc(i,6)
            savfrc(i, 7)= frc(i,7+nxdof)
            savfrc(i, 8)= frc(i,8+nxdof)
            savfrc(i, 9)= frc(i,9+nxdof)
            savfrc(i,10)= frc(i,10+nxdof)
            savfrc(i,11)= frc(i,11+nxdof)
            savfrc(i,12)= frc(i,12+nxdof)
            savfrc(i,13)= frc(i,13+2*nxdof)
            savfrc(i,14)= frc(i,14+2*nxdof)
            savfrc(i,15)= frc(i,15+2*nxdof)
            savfrc(i,16)= frc(i,16+2*nxdof)
            savfrc(i,17)= frc(i,17+2*nxdof)
            savfrc(i,18)= frc(i,18+2*nxdof)
            savfrc(i,19)= frc(i,19+3*nxdof)
            savfrc(i,20)= frc(i,20+3*nxdof)
            savfrc(i,21)= frc(i,21+3*nxdof)
            savfrc(i,22)= frc(i,22+3*nxdof)
            savfrc(i,23)= frc(i,23+3*nxdof)
            savfrc(i,24)= frc(i,24+3*nxdof)
         enddo
         ndof=4
         if (ifail.eq.1) call blkcpy (fail,svfail,nnlq)
      endif
c
      return
      end
      subroutine ushl_b101(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn1deta,dn2deta,dn3deta,dn4deta,
     5 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6 xdof,
     7 thick,thck1,thck2,thck3,thck4,
     8 fx1,fx2,fx3,fx4,
     9 fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined shell 101
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension thick(nlq)
      dimension thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension xdof(nlq,8,*)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     . gl12(nlq),gl22(nlq),gl32(nlq),
     . gl13(nlq),gl23(nlq),gl33(nlq)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4                     = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
      do i=lft,llt
c
         coef=.5*thick(i)*zeta
c
         bmtrx(i,1,1,1) =dn1dxi
         bmtrx(i,1,1,7) =dn2dxi
         bmtrx(i,1,1,13)=dn3dxi
         bmtrx(i,1,1,19)=dn4dxi
c
         bmtrx(i,1,1,5) =coef*dn1dxi
         bmtrx(i,1,1,11)=coef*dn2dxi
         bmtrx(i,1,1,17)=coef*dn3dxi
         bmtrx(i,1,1,23)=coef*dn4dxi
c
         bmtrx(i,1,2,1) =dn1deta
         bmtrx(i,1,2,7) =dn2deta
         bmtrx(i,1,2,13)=dn3deta
         bmtrx(i,1,2,19)=dn4deta
c
         bmtrx(i,1,2,5) =coef*dn1deta
         bmtrx(i,1,2,11)=coef*dn2deta
         bmtrx(i,1,2,17)=coef*dn3deta
         bmtrx(i,1,2,23)=coef*dn4deta
c
         bmtrx(i,2,1,2) =dn1dxi
         bmtrx(i,2,1,8) =dn2dxi
         bmtrx(i,2,1,14)=dn3dxi
         bmtrx(i,2,1,20)=dn4dxi
c
         bmtrx(i,2,1,4) =-coef*dn1dxi
         bmtrx(i,2,1,10)=-coef*dn2dxi
         bmtrx(i,2,1,16)=-coef*dn3dxi
         bmtrx(i,2,1,22)=-coef*dn4dxi
c
         bmtrx(i,1,3,5) =.5*thick(i)*n1
         bmtrx(i,1,3,11)=.5*thick(i)*n2
         bmtrx(i,1,3,17)=.5*thick(i)*n3
         bmtrx(i,1,3,23)=.5*thick(i)*n4
c
         bmtrx(i,3,1,3) =dn1dxi
         bmtrx(i,3,1,9) =dn2dxi
         bmtrx(i,3,1,15)=dn3dxi
         bmtrx(i,3,1,21)=dn4dxi
c
         bmtrx(i,2,2,2) =dn1deta
         bmtrx(i,2,2,8) =dn2deta
         bmtrx(i,2,2,14)=dn3deta
         bmtrx(i,2,2,20)=dn4deta
c
         bmtrx(i,2,2,4) =-coef*dn1deta
         bmtrx(i,2,2,10)=-coef*dn2deta
         bmtrx(i,2,2,16)=-coef*dn3deta
         bmtrx(i,2,2,22)=-coef*dn4deta
c
         bmtrx(i,2,3,4) =-.5*thick(i)*n1
         bmtrx(i,2,3,10)=-.5*thick(i)*n2
         bmtrx(i,2,3,16)=-.5*thick(i)*n3
         bmtrx(i,2,3,22)=-.5*thick(i)*n4
c
         bmtrx(i,3,2,3) =dn1deta
         bmtrx(i,3,2,9) =dn2deta
         bmtrx(i,3,2,15)=dn3deta
         bmtrx(i,3,2,21)=dn4deta
c
         gmtrx(i,1,1)=
     1        x1(i)*dn1dxi+x2(i)*dn2dxi+
     2        x3(i)*dn3dxi+x4(i)*dn4dxi
         gmtrx(i,2,1)=
     1        y1(i)*dn1dxi+y2(i)*dn2dxi+
     2        y3(i)*dn3dxi+y4(i)*dn4dxi
         gmtrx(i,3,1)=
     1        0.
         gmtrx(i,1,2)=
     1        x1(i)*dn1deta+x2(i)*dn2deta+
     2        x3(i)*dn3deta+x4(i)*dn4deta
         gmtrx(i,2,2)=
     1        y1(i)*dn1deta+y2(i)*dn2deta+
     2        y3(i)*dn3deta+y4(i)*dn4deta
         gmtrx(i,3,2)=
     1        0.
         gmtrx(i,1,3)=
     1        0.
         gmtrx(i,2,3)=
     1        0.
         gmtrx(i,3,3)=
     1        .5*thick(i)
c
      enddo
c
      return
      end
      subroutine ushl_b102(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn1deta,dn2deta,dn3deta,dn4deta,
     5 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6 xdof,
     7 thick,thck1,thck2,thck3,thck4,
     8 fx1,fx2,fx3,fx4,
     9 fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined shell 102
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension thick(nlq)
      dimension thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension xdof(nlq,8,*)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4                     = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c
      do i=lft,llt
c
         bmtrx(i,1,1,1) =dn1dxi
         bmtrx(i,1,1,7) =dn2dxi
         bmtrx(i,1,1,13)=dn3dxi
         bmtrx(i,1,1,19)=dn4dxi
c
         bmtrx(i,1,1,5) =.5*thck1(i)*zeta*fz1(i)*dn1dxi
         bmtrx(i,1,1,11)=.5*thck2(i)*zeta*fz2(i)*dn2dxi
         bmtrx(i,1,1,17)=.5*thck3(i)*zeta*fz3(i)*dn3dxi
         bmtrx(i,1,1,23)=.5*thck4(i)*zeta*fz4(i)*dn4dxi
c
         bmtrx(i,1,1,6) =-.5*thck1(i)*zeta*fy1(i)*dn1dxi
         bmtrx(i,1,1,12)=-.5*thck2(i)*zeta*fy2(i)*dn2dxi
         bmtrx(i,1,1,18)=-.5*thck3(i)*zeta*fy3(i)*dn3dxi
         bmtrx(i,1,1,24)=-.5*thck4(i)*zeta*fy4(i)*dn4dxi
c
         bmtrx(i,1,2,1) =dn1deta
         bmtrx(i,1,2,7) =dn2deta
         bmtrx(i,1,2,13)=dn3deta
         bmtrx(i,1,2,19)=dn4deta
c
         bmtrx(i,1,2,5) =.5*thck1(i)*zeta*fz1(i)*dn1deta
         bmtrx(i,1,2,11)=.5*thck2(i)*zeta*fz2(i)*dn2deta
         bmtrx(i,1,2,17)=.5*thck3(i)*zeta*fz3(i)*dn3deta
         bmtrx(i,1,2,23)=.5*thck4(i)*zeta*fz4(i)*dn4deta
c
         bmtrx(i,1,2,6) =-.5*thck1(i)*zeta*fy1(i)*dn1deta
         bmtrx(i,1,2,12)=-.5*thck2(i)*zeta*fy2(i)*dn2deta
         bmtrx(i,1,2,18)=-.5*thck3(i)*zeta*fy3(i)*dn3deta
         bmtrx(i,1,2,24)=-.5*thck4(i)*zeta*fy4(i)*dn4deta
c
         bmtrx(i,2,1,2) =dn1dxi
         bmtrx(i,2,1,8) =dn2dxi
         bmtrx(i,2,1,14)=dn3dxi
         bmtrx(i,2,1,20)=dn4dxi
c
         bmtrx(i,2,1,4) =-.5*thck1(i)*zeta*fz1(i)*dn1dxi
         bmtrx(i,2,1,10)=-.5*thck2(i)*zeta*fz2(i)*dn2dxi
         bmtrx(i,2,1,16)=-.5*thck3(i)*zeta*fz3(i)*dn3dxi
         bmtrx(i,2,1,22)=-.5*thck4(i)*zeta*fz4(i)*dn4dxi
c
         bmtrx(i,2,1,6) =.5*thck1(i)*zeta*fx1(i)*dn1dxi
         bmtrx(i,2,1,12)=.5*thck2(i)*zeta*fx2(i)*dn2dxi
         bmtrx(i,2,1,18)=.5*thck3(i)*zeta*fx3(i)*dn3dxi
         bmtrx(i,2,1,24)=.5*thck4(i)*zeta*fx4(i)*dn4dxi
c
         bmtrx(i,1,3,5) =.5*thck1(i)*fz1(i)*n1
         bmtrx(i,1,3,11)=.5*thck2(i)*fz2(i)*n2
         bmtrx(i,1,3,17)=.5*thck3(i)*fz3(i)*n3
         bmtrx(i,1,3,23)=.5*thck4(i)*fz4(i)*n4
c
         bmtrx(i,1,3,6) =-.5*thck1(i)*fy1(i)*n1
         bmtrx(i,1,3,12)=-.5*thck2(i)*fy2(i)*n2
         bmtrx(i,1,3,18)=-.5*thck3(i)*fy3(i)*n3
         bmtrx(i,1,3,24)=-.5*thck4(i)*fy4(i)*n4
c
         bmtrx(i,3,1,3) =dn1dxi
         bmtrx(i,3,1,9) =dn2dxi
         bmtrx(i,3,1,15)=dn3dxi
         bmtrx(i,3,1,21)=dn4dxi
c
         bmtrx(i,3,1,4) =.5*thck1(i)*zeta*fy1(i)*dn1dxi
         bmtrx(i,3,1,10)=.5*thck2(i)*zeta*fy2(i)*dn2dxi
         bmtrx(i,3,1,16)=.5*thck3(i)*zeta*fy3(i)*dn3dxi
         bmtrx(i,3,1,22)=.5*thck4(i)*zeta*fy4(i)*dn4dxi
c
         bmtrx(i,3,1,5) =-.5*thck1(i)*zeta*fx1(i)*dn1dxi
         bmtrx(i,3,1,11)=-.5*thck2(i)*zeta*fx2(i)*dn2dxi
         bmtrx(i,3,1,17)=-.5*thck3(i)*zeta*fx3(i)*dn3dxi
         bmtrx(i,3,1,23)=-.5*thck4(i)*zeta*fx4(i)*dn4dxi
c
         bmtrx(i,2,2,2) =dn1deta
         bmtrx(i,2,2,8) =dn2deta
         bmtrx(i,2,2,14)=dn3deta
         bmtrx(i,2,2,20)=dn4deta
c
         bmtrx(i,2,2,4) =-.5*thck1(i)*zeta*fz1(i)*dn1deta
         bmtrx(i,2,2,10)=-.5*thck2(i)*zeta*fz2(i)*dn2deta
         bmtrx(i,2,2,16)=-.5*thck3(i)*zeta*fz3(i)*dn3deta
         bmtrx(i,2,2,22)=-.5*thck4(i)*zeta*fz4(i)*dn4deta
c
         bmtrx(i,2,2,6) =.5*thck1(i)*zeta*fx1(i)*dn1deta
         bmtrx(i,2,2,12)=.5*thck2(i)*zeta*fx2(i)*dn2deta
         bmtrx(i,2,2,18)=.5*thck3(i)*zeta*fx3(i)*dn3deta
         bmtrx(i,2,2,24)=.5*thck4(i)*zeta*fx4(i)*dn4deta
c
         bmtrx(i,2,3,4) =-.5*thck1(i)*fz1(i)*n1
         bmtrx(i,2,3,10)=-.5*thck2(i)*fz2(i)*n2
         bmtrx(i,2,3,16)=-.5*thck3(i)*fz3(i)*n3
         bmtrx(i,2,3,22)=-.5*thck4(i)*fz4(i)*n4
c
         bmtrx(i,2,3,6) =.5*thck1(i)*fx1(i)*n1
         bmtrx(i,2,3,12)=.5*thck2(i)*fx2(i)*n2
         bmtrx(i,2,3,18)=.5*thck3(i)*fx3(i)*n3
         bmtrx(i,2,3,24)=.5*thck4(i)*fx4(i)*n4
c
         bmtrx(i,3,2,3) =dn1deta
         bmtrx(i,3,2,9) =dn2deta
         bmtrx(i,3,2,15)=dn3deta
         bmtrx(i,3,2,21)=dn4deta
c
         bmtrx(i,3,2,4) =.5*thck1(i)*zeta*fy1(i)*dn1deta
         bmtrx(i,3,2,10)=.5*thck2(i)*zeta*fy2(i)*dn2deta
         bmtrx(i,3,2,16)=.5*thck3(i)*zeta*fy3(i)*dn3deta
         bmtrx(i,3,2,22)=.5*thck4(i)*zeta*fy4(i)*dn4deta
c
         bmtrx(i,3,2,5) =-.5*thck1(i)*zeta*fx1(i)*dn1deta
         bmtrx(i,3,2,11)=-.5*thck2(i)*zeta*fx2(i)*dn2deta
         bmtrx(i,3,2,17)=-.5*thck3(i)*zeta*fx3(i)*dn3deta
         bmtrx(i,3,2,23)=-.5*thck4(i)*zeta*fx4(i)*dn4deta
c
         bmtrx(i,3,3,4) =.5*thck1(i)*fy1(i)*n1
         bmtrx(i,3,3,10)=.5*thck2(i)*fy2(i)*n2
         bmtrx(i,3,3,16)=.5*thck3(i)*fy3(i)*n3
         bmtrx(i,3,3,22)=.5*thck4(i)*fy4(i)*n4
c
         bmtrx(i,3,3,5) =-.5*thck1(i)*fx1(i)*n1
         bmtrx(i,3,3,11)=-.5*thck2(i)*fx2(i)*n2
         bmtrx(i,3,3,17)=-.5*thck3(i)*fx3(i)*n3
         bmtrx(i,3,3,23)=-.5*thck4(i)*fx4(i)*n4
c
         gmtrx(i,1,1)=
     1        (x1(i)+.5*zeta*thck1(i)*fx1(i))*dn1dxi+
     2        (x2(i)+.5*zeta*thck2(i)*fx2(i))*dn2dxi+
     3        (x3(i)+.5*zeta*thck3(i)*fx3(i))*dn3dxi+
     4        (x4(i)+.5*zeta*thck4(i)*fx4(i))*dn4dxi
         gmtrx(i,2,1)=
     1        (y1(i)+.5*zeta*thck1(i)*fy1(i))*dn1dxi+
     2        (y2(i)+.5*zeta*thck2(i)*fy2(i))*dn2dxi+
     3        (y3(i)+.5*zeta*thck3(i)*fy3(i))*dn3dxi+
     4        (y4(i)+.5*zeta*thck4(i)*fy4(i))*dn4dxi
         gmtrx(i,3,1)=
     1        (z1(i)+.5*zeta*thck1(i)*fz1(i))*dn1dxi+
     2        (z2(i)+.5*zeta*thck2(i)*fz2(i))*dn2dxi+
     3        (z3(i)+.5*zeta*thck3(i)*fz3(i))*dn3dxi+
     4        (z4(i)+.5*zeta*thck4(i)*fz4(i))*dn4dxi
         gmtrx(i,1,2)=
     1        (x1(i)+.5*zeta*thck1(i)*fx1(i))*dn1deta+
     2        (x2(i)+.5*zeta*thck2(i)*fx2(i))*dn2deta+
     3        (x3(i)+.5*zeta*thck3(i)*fx3(i))*dn3deta+
     4        (x4(i)+.5*zeta*thck4(i)*fx4(i))*dn4deta
         gmtrx(i,2,2)=
     1        (y1(i)+.5*zeta*thck1(i)*fy1(i))*dn1deta+
     2        (y2(i)+.5*zeta*thck2(i)*fy2(i))*dn2deta+
     3        (y3(i)+.5*zeta*thck3(i)*fy3(i))*dn3deta+
     4        (y4(i)+.5*zeta*thck4(i)*fy4(i))*dn4deta
         gmtrx(i,3,2)=
     1        (z1(i)+.5*zeta*thck1(i)*fz1(i))*dn1deta+
     2        (z2(i)+.5*zeta*thck2(i)*fz2(i))*dn2deta+
     3        (z3(i)+.5*zeta*thck3(i)*fz3(i))*dn3deta+
     4        (z4(i)+.5*zeta*thck4(i)*fz4(i))*dn4deta
         gmtrx(i,1,3)=
     1        .5*thck1(i)*fx1(i)*n1+
     2        .5*thck2(i)*fx2(i)*n2+
     3        .5*thck3(i)*fx3(i)*n3+
     4        .5*thck4(i)*fx4(i)*n4
         gmtrx(i,2,3)=
     1        .5*thck1(i)*fy1(i)*n1+
     2        .5*thck2(i)*fy2(i)*n2+
     3        .5*thck3(i)*fy3(i)*n3+
     4        .5*thck4(i)*fy4(i)*n4
         gmtrx(i,3,3)=
     1        .5*thck1(i)*fz1(i)*n1+
     2        .5*thck2(i)*fz2(i)*n2+
     3        .5*thck3(i)*fz3(i)*n3+
     4        .5*thck4(i)*fz4(i)*n4
      enddo
c
      return
      end
      subroutine ushl_b103(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn1deta,dn2deta,dn3deta,dn4deta,
     5 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6 xdof,
     7 thick,thck1,thck2,thck3,thck4,
     8 fx1,fx2,fx3,fx4,
     9 fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined shell 103
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension thick(nlq)
      dimension thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension xdof(nlq,8,*)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4                     = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers
c                                       in local (ILOC=0) or global (ILOC=1) system (IUNF=1)
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c
      do i=lft,llt
c
         coef=.5*thick(i)*zeta
c
         bmtrx(i,1,1,1) =dn1dxi
         bmtrx(i,1,1,7) =dn2dxi
         bmtrx(i,1,1,13)=dn3dxi
         bmtrx(i,1,1,19)=dn4dxi
c
         bmtrx(i,1,1,5) =coef*dn1dxi
         bmtrx(i,1,1,11)=coef*dn2dxi
         bmtrx(i,1,1,17)=coef*dn3dxi
         bmtrx(i,1,1,23)=coef*dn4dxi
c
         bmtrx(i,1,2,1) =dn1deta
         bmtrx(i,1,2,7) =dn2deta
         bmtrx(i,1,2,13)=dn3deta
         bmtrx(i,1,2,19)=dn4deta
c
         bmtrx(i,1,2,5) =coef*dn1deta
         bmtrx(i,1,2,11)=coef*dn2deta
         bmtrx(i,1,2,17)=coef*dn3deta
         bmtrx(i,1,2,23)=coef*dn4deta
c
         bmtrx(i,2,1,2) =dn1dxi
         bmtrx(i,2,1,8) =dn2dxi
         bmtrx(i,2,1,14)=dn3dxi
         bmtrx(i,2,1,20)=dn4dxi
c
         bmtrx(i,2,1,4) =-coef*dn1dxi
         bmtrx(i,2,1,10)=-coef*dn2dxi
         bmtrx(i,2,1,16)=-coef*dn3dxi
         bmtrx(i,2,1,22)=-coef*dn4dxi
c
         bmtrx(i,1,3,5) =.5*thick(i)*.25*(1.-eta)
         bmtrx(i,1,3,11)=.5*thick(i)*.25*(1.-eta)
         bmtrx(i,1,3,17)=.5*thick(i)*.25*(1.+eta)
         bmtrx(i,1,3,23)=.5*thick(i)*.25*(1.+eta)
c
         bmtrx(i,3,1,3) =dn1dxi
         bmtrx(i,3,1,9) =dn2dxi
         bmtrx(i,3,1,15)=dn3dxi
         bmtrx(i,3,1,21)=dn4dxi
c
         bmtrx(i,2,2,2) =dn1deta
         bmtrx(i,2,2,8) =dn2deta
         bmtrx(i,2,2,14)=dn3deta
         bmtrx(i,2,2,20)=dn4deta
c
         bmtrx(i,2,2,4) =-coef*dn1deta
         bmtrx(i,2,2,10)=-coef*dn2deta
         bmtrx(i,2,2,16)=-coef*dn3deta
         bmtrx(i,2,2,22)=-coef*dn4deta
c
         bmtrx(i,2,3,4) =-.5*thick(i)*.25*(1.-xi)
         bmtrx(i,2,3,10)=-.5*thick(i)*.25*(1.+xi)
         bmtrx(i,2,3,16)=-.5*thick(i)*.25*(1.+xi)
         bmtrx(i,2,3,22)=-.5*thick(i)*.25*(1.-xi)
c
         bmtrx(i,3,2,3) =dn1deta
         bmtrx(i,3,2,9) =dn2deta
         bmtrx(i,3,2,15)=dn3deta
         bmtrx(i,3,2,21)=dn4deta
c
         gmtrx(i,1,1)=
     1        x1(i)*dn1dxi+x2(i)*dn2dxi+
     2        x3(i)*dn3dxi+x4(i)*dn4dxi
         gmtrx(i,2,1)=
     1        y1(i)*dn1dxi+y2(i)*dn2dxi+
     2        y3(i)*dn3dxi+y4(i)*dn4dxi
         gmtrx(i,3,1)=
     1        0.
         gmtrx(i,1,2)=
     1        x1(i)*dn1deta+x2(i)*dn2deta+
     2        x3(i)*dn3deta+x4(i)*dn4deta
         gmtrx(i,2,2)=
     1        y1(i)*dn1deta+y2(i)*dn2deta+
     2        y3(i)*dn3deta+y4(i)*dn4deta
         gmtrx(i,3,2)=
     1        0.
         gmtrx(i,1,3)=
     1        0.
         gmtrx(i,2,3)=
     1        0.
         gmtrx(i,3,3)=
     1        .5*thick(i)
c
      enddo
c
      return
      end
      subroutine ushl_b104(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn1deta,dn2deta,dn3deta,dn4deta,
     5 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6 xdof,
     7 thick,thck1,thck2,thck3,thck4,
     8 fx1,fx2,fx3,fx4,
     9 fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined shell 104
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension thick(nlq)
      dimension thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension xdof(nlq,8,*)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                 = parental coordinates
c     n1,n2,n3,n4                 = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi = nodal shape function derivatives
c                                   w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta = nodal shape function derivatives
c                                   w.r.t. second parental coordinate
c     x1,x2,x3,x4                 = x-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     y1,y2,y3,y4                 = y-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     z1,z2,z3,z4                 = z-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     thick                       = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4     = nodal thicknesses of shell (IUNF=1)
c     xdof                        = values corresponding to
c                                   extra degrees of freedom (NXDOF.GT.0)
c     fx1,fx2,fx3,fx4             = x-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     fy1,fy2,fy3,fy4             = y-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     fz1,fz2,fz3,fz4             = z-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     gl11,gl21,...,gl33          = transformation matrix between
c                                   local and global system
c
      return
      end
      subroutine ushl_b105(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn1deta,dn2deta,dn3deta,dn4deta,
     5 x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     6 xdof,
     7 thick,thck1,thck2,thck3,thck4,
     8 fx1,fx2,fx3,fx4,
     9 fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined shell 105
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension thick(nlq)
      dimension thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension xdof(nlq,8,*)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                 = parental coordinates
c     n1,n2,n3,n4                 = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi = nodal shape function derivatives
c                                   w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta = nodal shape function derivatives
c                                   w.r.t. second parental coordinate
c     x1,x2,x3,x4                 = x-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     y1,y2,y3,y4                 = y-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     z1,z2,z3,z4                 = z-coordinates of nodes
c                                   in local (ILOC=0) or global (ILOC=1) system
c     thick                       = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4     = nodal thicknesses of shell (IUNF=1)
c     xdof                        = values corresponding to
c                                   extra degrees of freedom (NXDOF.GT.0)
c     fx1,fx2,fx3,fx4             = x-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     fy1,fy2,fy3,fy4             = y-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     fz1,fz2,fz3,fz4             = z-components of nodal fibers
c                                   in local (ILOC=0)
c                                   or global (ILOC=1) system (IUNF=1)
c     gl11,gl21,...,gl33          = transformation matrix between
c                                   local and global system
c
      return
      end
      subroutine ushl_e101(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     . fx1,fx2,fx3,fx4,
     . fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . xdof,
     . dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     . wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4,wz1,wz2,wz3,wz4,
     . dxdof,
     . thick,thck1,thck2,thck3,thck4,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . cmtrx,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined shell 101
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension wx1(nlq),wx2(nlq),wx3(nlq),wx4(nlq)
      dimension wy1(nlq),wy2(nlq),wy3(nlq),wy4(nlq)
      dimension wz1(nlq),wz2(nlq),wz3(nlq),wz4(nlq)
      dimension dxdof(nlq,8,*)
      dimension thick(nlq),thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
      dimension cmtrx(nlq,15,3)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     thick                           = thickness of shell (IUNF=0,NIPP=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1,NIPP=0)
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     xdof                            = values corresponding to
c                                       extra degrees of freedom, current configuration (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4                 = x-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dy1,dy2,dy3,dy4                 = y-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dz1,dz2,dz3,dz4                 = z-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wx1,wx2,wx3,wx4                 = x-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wy1,wy2,wy3,wy4                 = y-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wz1,wz2,wz3,wz4                 = z-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c     cmtrx                           = resultant tangent moduli
c
c     Silly useless element implemented for the purpose of
c     checking the resultant user element interface
c
      dimension q(3)
      dimension x(4),y(4),z(4),dx(4),dy(4),dz(4),wx(4),wy(4),wz(4)
      e=cm(1)
      do i=lft,llt
         x(1)=x1(i)
         x(2)=x2(i)
         x(3)=x3(i)
         x(4)=x4(i)
         y(1)=y1(i)
         y(2)=y2(i)
         y(3)=y3(i)
         y(4)=y4(i)
         z(1)=z1(i)
         z(2)=z2(i)
         z(3)=z3(i)
         z(4)=z4(i)
         dx(1)=dx1(i)
         dx(2)=dx2(i)
         dx(3)=dx3(i)
         dx(4)=dx4(i)
         dy(1)=dy1(i)
         dy(2)=dy2(i)
         dy(3)=dy3(i)
         dy(4)=dy4(i)
         dz(1)=dz1(i)
         dz(2)=dz2(i)
         dz(3)=dz3(i)
         dz(4)=dz4(i)
         wx(1)=wx1(i)
         wx(2)=wx2(i)
         wx(3)=wx3(i)
         wx(4)=wx4(i)
         wy(1)=wy1(i)
         wy(2)=wy2(i)
         wy(3)=wy3(i)
         wy(4)=wy4(i)
         wz(1)=wz1(i)
         wz(2)=wz2(i)
         wz(3)=wz3(i)
         wz(4)=wz4(i)
         do j=1,24
            force(i,j)=hsv(i,j)
         enddo
         do j=1,4
            force(i,(j-1)*6+4)=force(i,(j-1)*6+4)+e*wx(j)
            force(i,(j-1)*6+5)=force(i,(j-1)*6+5)+e*wy(j)
            force(i,(j-1)*6+6)=force(i,(j-1)*6+6)+e*wz(j)
         enddo
         if (istif.eq.1) then
            do j=1,4
               stiff(i,(j-1)*6+4,(j-1)*6+4)=e
               stiff(i,(j-1)*6+5,(j-1)*6+5)=e
               stiff(i,(j-1)*6+6,(j-1)*6+6)=e
            enddo
         endif
         do j=1,3
            do k=j+1,4
               eps=(dx(k)-dx(j))*(x(k)-x(j))+
     .              (dy(k)-dy(j))*(y(k)-y(j))+
     .              (dz(k)-dz(j))*(z(k)-z(j))
               enrm=sqrt((x(k)-x(j))**2+
     .              (y(k)-y(j))**2+
     .              (z(k)-z(j))**2)
               eeps=e*eps/enrm**2
               q(1)=(x(k)-x(j))/enrm
               q(2)=(y(k)-y(j))/enrm
               q(3)=(z(k)-z(j))/enrm
               force(i,(k-1)*6+1)=force(i,(k-1)*6+1)+eeps*q(1)
               force(i,(k-1)*6+2)=force(i,(k-1)*6+2)+eeps*q(2)
               force(i,(k-1)*6+3)=force(i,(k-1)*6+3)+eeps*q(3)
               force(i,(j-1)*6+1)=force(i,(j-1)*6+1)-eeps*q(1)
               force(i,(j-1)*6+2)=force(i,(j-1)*6+2)-eeps*q(2)
               force(i,(j-1)*6+3)=force(i,(j-1)*6+3)-eeps*q(3)
               if (istif.eq.1) then
                  do kk=1,3
                     do jj=1,3
                        stiff(i,(k-1)*6+kk,(j-1)*6+jj)=
     .                       stiff(i,(k-1)*6+kk,(j-1)*6+jj)-
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(j-1)*6+kk,(k-1)*6+jj)=
     .                       stiff(i,(j-1)*6+kk,(k-1)*6+jj)-
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(k-1)*6+kk,(k-1)*6+jj)=
     .                       stiff(i,(k-1)*6+kk,(k-1)*6+jj)+
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(j-1)*6+kk,(j-1)*6+jj)=
     .                       stiff(i,(j-1)*6+kk,(j-1)*6+jj)+
     .                       e*q(kk)*q(jj)/enrm
                     enddo
                  enddo
               endif
            enddo
         enddo
         do j=1,24
            hsv(i,j)=force(i,j)
         enddo
c
      enddo
      return
      end
      subroutine ushl_e102(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     . fx1,fx2,fx3,fx4,
     . fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . xdof,
     . dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     . wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4,wz1,wz2,wz3,wz4,
     . dxdof,
     . thick,thck1,thck2,thck3,thck4,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . cmtrx,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined shell 102
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension wx1(nlq),wx2(nlq),wx3(nlq),wx4(nlq)
      dimension wy1(nlq),wy2(nlq),wy3(nlq),wy4(nlq)
      dimension wz1(nlq),wz2(nlq),wz3(nlq),wz4(nlq)
      dimension dxdof(nlq,8,*)
      dimension thick(nlq),thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
      dimension cmtrx(nlq,15,3)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     thick                           = thickness of shell (IUNF=0,NIPP=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1,NIPP=0)
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     xdof                            = values corresponding to
c                                       extra degrees of freedom, current configuration (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4                 = x-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dy1,dy2,dy3,dy4                 = y-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dz1,dz2,dz3,dz4                 = z-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wx1,wx2,wx3,wx4                 = x-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wy1,wy2,wy3,wy4                 = y-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wz1,wz2,wz3,wz4                 = z-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c     cmtrx                           = resultant tangent moduli
c
c
      return
      end
      subroutine ushl_e103(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     . fx1,fx2,fx3,fx4,
     . fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . xdof,
     . dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     . wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4,wz1,wz2,wz3,wz4,
     . dxdof,
     . thick,thck1,thck2,thck3,thck4,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . cmtrx,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined shell 103
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension wx1(nlq),wx2(nlq),wx3(nlq),wx4(nlq)
      dimension wy1(nlq),wy2(nlq),wy3(nlq),wy4(nlq)
      dimension wz1(nlq),wz2(nlq),wz3(nlq),wz4(nlq)
      dimension dxdof(nlq,8,*)
      dimension thick(nlq),thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
      dimension cmtrx(nlq,15,3)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     thick                           = thickness of shell (IUNF=0,NIPP=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1,NIPP=0)
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     xdof                            = values corresponding to
c                                       extra degrees of freedom, current configuration (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4                 = x-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dy1,dy2,dy3,dy4                 = y-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dz1,dz2,dz3,dz4                 = z-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wx1,wx2,wx3,wx4                 = x-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wy1,wy2,wy3,wy4                 = y-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wz1,wz2,wz3,wz4                 = z-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c     cmtrx                           = resultant tangent moduli
c
c
      return
      end
      subroutine ushl_e104(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     . fx1,fx2,fx3,fx4,
     . fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . xdof,
     . dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     . wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4,wz1,wz2,wz3,wz4,
     . dxdof,
     . thick,thck1,thck2,thck3,thck4,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . cmtrx,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined shell 104
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension wx1(nlq),wx2(nlq),wx3(nlq),wx4(nlq)
      dimension wy1(nlq),wy2(nlq),wy3(nlq),wy4(nlq)
      dimension wz1(nlq),wz2(nlq),wz3(nlq),wz4(nlq)
      dimension dxdof(nlq,8,*)
      dimension thick(nlq),thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
      dimension cmtrx(nlq,15,3)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     thick                           = thickness of shell (IUNF=0,NIPP=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1,NIPP=0)
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     xdof                            = values corresponding to
c                                       extra degrees of freedom, current configuration (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4                 = x-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dy1,dy2,dy3,dy4                 = y-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dz1,dz2,dz3,dz4                 = z-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wx1,wx2,wx3,wx4                 = x-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wy1,wy2,wy3,wy4                 = y-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wz1,wz2,wz3,wz4                 = z-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c     cmtrx                           = resultant tangent moduli
c
c
      dimension xcur(14,4),xref(14,4)
      nxdof=3
      do i=lft,llt
        xcur(1,1)=x1(i)
        xcur(1,2)=x2(i)
        xcur(1,3)=x3(i)
        xcur(1,4)=x4(i)
        xcur(2,1)=y1(i)
        xcur(2,2)=y2(i)
        xcur(2,3)=y3(i)
        xcur(2,4)=y4(i)
        xcur(3,1)=z1(i)
        xcur(3,2)=z2(i)
        xcur(3,3)=z3(i)
        xcur(3,4)=z4(i)
        xcur(4,1)=0
        xcur(4,2)=0
        xcur(4,3)=0
        xcur(4,4)=0
        xcur(5,1)=0
        xcur(5,2)=0
        xcur(5,3)=0
        xcur(5,4)=0
        xcur(6,1)=0
        xcur(6,2)=0
        xcur(6,3)=0
        xcur(6,4)=0
        do j=1,nxdof
          do k=1,4
            xcur(6+j,k)=xdof(i,k,j)
          enddo
        enddo
        if (hsv(i,1).ne.1.) then
          hsv(i,1)=1.
          ijk=1
          do j=1,4
            do k=1,6+nxdof
              ijk=ijk+1
              hsv(i,ijk)=xcur(k,j)
            enddo
          enddo
        endif
        ijk=1
        do j=1,4
          do k=1,6+nxdof
            ijk=ijk+1
            xref(k,j)=hsv(i,ijk)
          enddo
        enddo
        do j=1,ndtot
          force(i,j)=0.
          do k=1,ndtot
            jk=iabs(j-k)
            stiff(i,j,k)=.5**jk
            if (jk.ne.0) stiff(i,j,k)=-stiff(i,j,k)*.5
            knod=(k-1)/(6+nxdof)+1
            kdof=k-(knod-1)*(6+nxdof)
            force(i,j)=force(i,j)+stiff(i,j,k)*
     1           (xcur(kdof,knod)-xref(kdof,knod))
          enddo
        enddo
      enddo
c
      return
      end
      subroutine ushl_e105(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,y1,y2,y3,y4,z1,z2,z3,z4,
     . fx1,fx2,fx3,fx4,
     . fy1,fy2,fy3,fy4,
     . fz1,fz2,fz3,fz4,
     . xdof,
     . dx1,dx2,dx3,dx4,dy1,dy2,dy3,dy4,dz1,dz2,dz3,dz4,
     . wx1,wx2,wx3,wx4,wy1,wy2,wy3,wy4,wz1,wz2,wz3,wz4,
     . dxdof,
     . thick,thck1,thck2,thck3,thck4,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . gl11,gl21,gl31,gl12,gl22,gl32,gl13,gl23,gl33,
     . cmtrx,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined shell 105
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension fx1(nlq),fx2(nlq),fx3(nlq),fx4(nlq)
      dimension fy1(nlq),fy2(nlq),fy3(nlq),fy4(nlq)
      dimension fz1(nlq),fz2(nlq),fz3(nlq),fz4(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension wx1(nlq),wx2(nlq),wx3(nlq),wx4(nlq)
      dimension wy1(nlq),wy2(nlq),wy3(nlq),wy4(nlq)
      dimension wz1(nlq),wz2(nlq),wz3(nlq),wz4(nlq)
      dimension dxdof(nlq,8,*)
      dimension thick(nlq),thck1(nlq),thck2(nlq),thck3(nlq),thck4(nlq)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension gl11(nlq),gl21(nlq),gl31(nlq),
     .     gl12(nlq),gl22(nlq),gl32(nlq),
     .     gl13(nlq),gl23(nlq),gl33(nlq)
      dimension cmtrx(nlq,15,3)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     thick                           = thickness of shell (IUNF=0,NIPP=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1,NIPP=0)
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4                     = x-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     y1,y2,y3,y4                     = y-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     z1,z2,z3,z4                     = z-coordinates of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fx1,fx2,fx3,fx4                 = x-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fy1,fy2,fy3,fy4                 = y-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     fz1,fz2,fz3,fz4                 = z-components of nodal fibers (IUNF=1)
c                                       in local (ILOC=0) or global (ILOC=1) system, current configuration
c     xdof                            = values corresponding to
c                                       extra degrees of freedom, current configuration (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4                 = x-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dy1,dy2,dy3,dy4                 = y-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dz1,dz2,dz3,dz4                 = z-displacements of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wx1,wx2,wx3,wx4                 = x-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wy1,wy2,wy3,wy4                 = y-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     wz1,wz2,wz3,wz4                 = z-rotational increment of nodes
c                                       in local (ILOC=0) or global (ILOC=1) system
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     thick                           = thickness of shell (IUNF=0)
c     thck1,thck2,thck3,thck4         = nodal thicknesses of shell (IUNF=1)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     gl11,gl21,...,gl33              = transformation matrix between
c                                       local and global system
c     cmtrx                           = resultant tangent moduli
c
c
      return
      end
      subroutine ushl_m101(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user shell 101
c
      dimension w(nxdof,4),x(3,4),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,4)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,4)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine ushl_m102(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user shell 102
c
      dimension w(nxdof,4),x(3,4),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,4)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,4)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine ushl_m103(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user shell 103
c
      dimension w(nxdof,4),x(3,4),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,4)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,4)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine ushl_m104(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user shell 104
c
      dimension w(nxdof,4),x(3,4),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,4)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,4)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      do i=1,4
        do j=1,nxdof
          w(j,i)=100.
        enddo
      enddo
c
      return
      end
c     
      subroutine ushl_m105(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user shell 105
c
      dimension w(nxdof,4),x(3,4),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,4)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,4)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
      subroutine usrsld(mtype,cm,u,v,fv,x,auxvec,eosp,tnew,fval,ener,
     . npc,pld,hgforc,ies,bqs,nhxbwp,hgener,mte,nmtcon,lav,ihg,
     . nnm2,lft,llt,ibq,nes,idmp,kp,nnm1,mxe,iehgfg,strains,rhssav,
     . volfrc,cmaux,eig,eign,idam,damag,lpwphv,rots,hges,lochvh,ithxpid,
     . ietyp,cmusr,xipn)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     main routine for the user defined solid
c
      include 'nlqparm'
      include 'adjcom.inc'
c     include 'alecom.inc'
      include 'alemisc.inc'
      include 'implicit1.inc'
      include 'memaia.inc'
      include 'nhisparm.inc'
      include 'shlioc.inc'
      include 'subcyc.inc'
      include 'umatss.inc'
      integer n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n44a,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,n81,n82,locend,iname,lendf
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n44a,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,n81,n82,locend,iname,lendf
      integer lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b,lc7c,lc7d,lc7e,lc7f,lc7g,lc7h,lc7i,lc7j,lc7k,lc7l
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b,lc7c,lc7d,lc7e,lc7f,lc7g,lc7h,lc7i,lc7j,lc7k,lc7l
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter_noloc.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR nnc->znnc, lczc->zlczc
c
      integer nnc,lczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/nnc, lczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
c
      common/aux2loc/
     & d1(nlq),d2(nlq),d3(nlq),d4(nlq),d5(nlq),d6(nlq),
     & wzzdt(nlq),wyydt(nlq),wxxdt(nlq),einc(nlq)
      common/aux8loc/
     & x1(nlq),x2(nlq),x3(nlq),x4(nlq),
     & x5(nlq),x6(nlq),x7(nlq),x8(nlq),
     & y1(nlq),y2(nlq),y3(nlq),y4(nlq),
     & y5(nlq),y6(nlq),y7(nlq),y8(nlq),
     & z1(nlq),z2(nlq),z3(nlq),z4(nlq),
     & z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      common/aux9loc/vlrho(nlq),voln(nlq)
      common/aux10loc/
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 vx1(nlq),vx2(nlq),vx3(nlq),vx4(nlq),
     5 vx5(nlq),vx6(nlq),vx7(nlq),vx8(nlq),
     6 vy1(nlq),vy2(nlq),vy3(nlq),vy4(nlq),
     7 vy5(nlq),vy6(nlq),vy7(nlq),vy8(nlq),
     8 vz1(nlq),vz2(nlq),vz3(nlq),vz4(nlq),
     9 vz5(nlq),vz6(nlq),vz7(nlq),vz8(nlq)
      common/aux13loc/
     1 p11(nlq),p12(nlq),p13(nlq),p14(nlq),
     2 p15(nlq),p16(nlq),p17(nlq),p18(nlq),
     3 p21(nlq),p22(nlq),p23(nlq),p24(nlq),
     4 p25(nlq),p26(nlq),p27(nlq),p28(nlq),
     5 p31(nlq),p32(nlq),p33(nlq),p34(nlq),
     6 p35(nlq),p36(nlq),p37(nlq),p38(nlq),
     . hgforq(12,nlq)
      common/aux14loc/ax(nlq,7+NHISVAR)
      common/aux15loc/qp(nlq),specen(nlq),dvol(nlq),volold(nlq)
      common/aux18loc/dd(nlq),dfe(nlq),ddq(nlq)
      common/aux33loc/ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ix5(nlq),
     1 ix6(nlq),ix7(nlq),ix8(nlq),mxt(nlq)
      common/aux35loc/rhoa(nlq),cb(nlq)
      common/aux37loc/ad(nlq),vol3rd(nlq),qx(nlq),dtx(nlq)
      common/aux43loc/xm(nlq),p(nlq),xmua(nlq)
      common/bel8loc/uehisv(nlq,2*(NHISVUE+45))
      common/bel8aloc/iuehup(nlq,2*(NHISVUE+45))
      common/frozloc/vfroz,tfroz,
     . vfrzn(nlq),tfrzn(nlq),dtold(nlq),
     1 x1f(nlq),x2f(nlq),x3f(nlq),x4f(nlq),
     1 x5f(nlq),x6f(nlq),x7f(nlq),x8f(nlq),
     1 y1f(nlq),y2f(nlq),y3f(nlq),y4f(nlq),
     2 y5f(nlq),y6f(nlq),y7f(nlq),y8f(nlq),
     2 z1f(nlq),z2f(nlq),z3f(nlq),z4f(nlq),
     3 z5f(nlq),z6f(nlq),z7f(nlq),z8f(nlq)
      common/numcpuloc/lenvec8,lenvech
      common/prescloc/voltot(nlq)
      common/rayleiloc/betav
      common/scr01loc/
     1 spx1(nlq),spx2(nlq),spx3(nlq),spx4(nlq),
     & spx5(nlq),spx6(nlq),spx7(nlq),spx8(nlq),
     2 spy1(nlq),spy2(nlq),spy3(nlq),spy4(nlq),
     & spy5(nlq),spy6(nlq),spy7(nlq),spy8(nlq),
     3 spz1(nlq),spz2(nlq),spz3(nlq),spz4(nlq),
     & spz5(nlq),spz6(nlq),spz7(nlq),spz8(nlq),
     & sx1(nlq),sx2(nlq),sx3(nlq),sx4(nlq),
     & sx5(nlq),sx6(nlq),sx7(nlq),sx8(nlq),
     & sy1(nlq),sy2(nlq),sy3(nlq),sy4(nlq),
     & sy5(nlq),sy6(nlq),sy7(nlq),sy8(nlq),
     & sz1(nlq),sz2(nlq),sz3(nlq),sz4(nlq),
     & sz5(nlq),sz6(nlq),sz7(nlq),sz8(nlq),volm(nlq)
      common/shloptloc/ibelyt
      common/subtssloc/dt1siz(nlq)
      common/vect2loc/vlinc(nlq),pmean(nlq)
      common/vect8loc/dsave(nlq,6,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1 ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2 grvity,idirgv,nodspc,nspcor
      common/bk26/begtim,nintcy
      common/bk28/summss,xke,xpe,tt,xte0,erodeke,erodeie,selie,selke,
     1 erodehg
      common/bttn/ntnwf,ixa(50)
c
c     common/comrbbr/ishrub(300),icompf(300),mhyper(300)
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     . ncs10,ncs11,ncs12,ncs13,ncs14,ncs15,
     . numcsd,csdinc,csdout,output,sldef,future(2),issfmx,istos,
     . ifls,iflm,nsntl,nmntl
      common/eigvc/eke,eme,rsav,dt2std,isavdt2,iegflg,iegflg0(10)
      common/feedbck/ifdbck(41)
      include 'nikcpl.inc'
      common/numcpu/ncpu,ncpua,ncpub,lenvec(8)
      common/shlopt/istrn,istupd,ibelyts,miter,wrpang,ipstpd,intsts,
     1 nodsts,intstn,nodstn,jstrn
      common/slcntr/islcnt(21)
c
      real ies
      real*8 x,rots
      dimension mtype(1),cm(48,1),u(1),v(3,1),fv(1),x(3,1),auxvec(1),
     . eosp(1),tnew(1),fval(1),ener(1),npc(1),pld(1),hgforc(12,1),
     . ies(*),bqs(*),nhxbwp(*),hgener(*),strains(6,*),idam(*),
     . rhssav(27,*),volfrc(*),cmaux(*),eig(3,*),eign(3,*),damag(119,*),
     . rots(*),hges(*),lochvh(*)
      dimension cmusr(48,*),xipn(4,100,*)
c
      logical iamusrmat
      external iamusrmat
c
      data o64th/0.0156250/
c
c     Number of integration points
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/aux8loc/)
c$omp threadprivate (/aux9loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/aux18loc/)
c$omp threadprivate (/aux15loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/aux37loc/)
c$omp threadprivate (/aux43loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/bel8loc/)
c$omp threadprivate (/bel8aloc/)
c$omp threadprivate (/frozloc/)
c$omp threadprivate (/numcpuloc/)
c$omp threadprivate (/prescloc/)
c$omp threadprivate (/rayleiloc/)
c$omp threadprivate (/scr01loc/)
c$omp threadprivate (/shloptloc/)
c$omp threadprivate (/subtssloc/)
c$omp threadprivate (/vect2loc/)
c$omp threadprivate (/vect8loc/)
      nip=nint(cmusr(1,mxe))
c
c     Number of extra degrees of freedom
c
      nxdof=nint(cmusr(2,mxe))
c
c     Flag for requesting hourglass stiffness
c
      ihgf=nint(cmusr(4,mxe))
      if (nip.eq.0) ihgf=0
c
c     Flag determining how to input data for integrated element
c
      itaj=nint(cmusr(5,mxe))
c
c     Number of property constants
c
      lmc=nint(cmusr(6,mxe))
c
c     Number of history variables
c
      nhsv=nint(cmusr(7,mxe))
c
c     Pointer to crv array
c
      islcnt16=islcnt(16)
c
c     Parameter used for hourglass
c
      nnm3=1
c
c     No support of vref and tref in material 126
c
c     For hourglass treatment
c
      if (nik3d(9).gt.0) nnm3=nnm2
c
c     Gather nodal coordinates and velocities
c
c     xi,yi,zi       = Nodal coordinates at t_{n+1}
c     xhi,yhi,zhi    = Nodal coordinates at t_{n+1/2}
c     vxi,vyi,vzi    = Translational velocities
c
      do i=lft,llt
c
         x1(i)=x(1,ix1(i))
         y1(i)=x(2,ix1(i))
         z1(i)=x(3,ix1(i))
         x2(i)=x(1,ix2(i))
         y2(i)=x(2,ix2(i))
         z2(i)=x(3,ix2(i))
         x3(i)=x(1,ix3(i))
         y3(i)=x(2,ix3(i))
         z3(i)=x(3,ix3(i))
         x4(i)=x(1,ix4(i))
         y4(i)=x(2,ix4(i))
         z4(i)=x(3,ix4(i))
         x5(i)=x(1,ix5(i))
         y5(i)=x(2,ix5(i))
         z5(i)=x(3,ix5(i))
         x6(i)=x(1,ix6(i))
         y6(i)=x(2,ix6(i))
         z6(i)=x(3,ix6(i))
         x7(i)=x(1,ix7(i))
         y7(i)=x(2,ix7(i))
         z7(i)=x(3,ix7(i))
         x8(i)=x(1,ix8(i))
         y8(i)=x(2,ix8(i))
         z8(i)=x(3,ix8(i))
c
         vx1(i)=v(1,ix1(i))
         vy1(i)=v(2,ix1(i))
         vz1(i)=v(3,ix1(i))
         vx2(i)=v(1,ix2(i))
         vy2(i)=v(2,ix2(i))
         vz2(i)=v(3,ix2(i))
         vx3(i)=v(1,ix3(i))
         vy3(i)=v(2,ix3(i))
         vz3(i)=v(3,ix3(i))
         vx4(i)=v(1,ix4(i))
         vy4(i)=v(2,ix4(i))
         vz4(i)=v(3,ix4(i))
         vx5(i)=v(1,ix5(i))
         vy5(i)=v(2,ix5(i))
         vz5(i)=v(3,ix5(i))
         vx6(i)=v(1,ix6(i))
         vy6(i)=v(2,ix6(i))
         vz6(i)=v(3,ix6(i))
         vx7(i)=v(1,ix7(i))
         vy7(i)=v(2,ix7(i))
         vz7(i)=v(3,ix7(i))
         vx8(i)=v(1,ix8(i))
         vy8(i)=v(2,ix8(i))
         vz8(i)=v(3,ix8(i))
c
      enddo
c
      if (nxdof.gt.0) then
         do k=1,8
            do j=1,nxdof
               nsnd=(j-1)/3+1
               nsdf=j-3*(nsnd-1)
               do i=lft,llt
                  xdof(i,k,j)=x(nsdf,ixsld(i,k,nsnd))
                  dxdof(i,k,j)=v(nsdf,ixsld(i,k,nsnd))
               enddo
            enddo
         enddo
      endif
c
      if (lenvec8.ne.0) then
c
c     Accuracy option is on, compute half step geometry
c
         do i=lft,llt
c
            sx1(i)=x1(i)-.5*vx1(i)*dt1siz(i)
            sy1(i)=y1(i)-.5*vy1(i)*dt1siz(i)
            sz1(i)=z1(i)-.5*vz1(i)*dt1siz(i)
            sx2(i)=x2(i)-.5*vx2(i)*dt1siz(i)
            sy2(i)=y2(i)-.5*vy2(i)*dt1siz(i)
            sz2(i)=z2(i)-.5*vz2(i)*dt1siz(i)
            sx3(i)=x3(i)-.5*vx3(i)*dt1siz(i)
            sy3(i)=y3(i)-.5*vy3(i)*dt1siz(i)
            sz3(i)=z3(i)-.5*vz3(i)*dt1siz(i)
            sx4(i)=x4(i)-.5*vx4(i)*dt1siz(i)
            sy4(i)=y4(i)-.5*vy4(i)*dt1siz(i)
            sz4(i)=z4(i)-.5*vz4(i)*dt1siz(i)
            sx5(i)=x5(i)-.5*vx5(i)*dt1siz(i)
            sy5(i)=y5(i)-.5*vy5(i)*dt1siz(i)
            sz5(i)=z5(i)-.5*vz5(i)*dt1siz(i)
            sx6(i)=x6(i)-.5*vx6(i)*dt1siz(i)
            sy6(i)=y6(i)-.5*vy6(i)*dt1siz(i)
            sz6(i)=z6(i)-.5*vz6(i)*dt1siz(i)
            sx7(i)=x7(i)-.5*vx7(i)*dt1siz(i)
            sy7(i)=y7(i)-.5*vy7(i)*dt1siz(i)
            sz7(i)=z7(i)-.5*vz7(i)*dt1siz(i)
            sx8(i)=x8(i)-.5*vx8(i)*dt1siz(i)
            sy8(i)=y8(i)-.5*vy8(i)*dt1siz(i)
            sz8(i)=z8(i)-.5*vz8(i)*dt1siz(i)
c
            xh1(i)=sx1(i)
            yh1(i)=sy1(i)
            zh1(i)=sz1(i)
            xh2(i)=sx2(i)
            yh2(i)=sy2(i)
            zh2(i)=sz2(i)
            xh3(i)=sx3(i)
            yh3(i)=sy3(i)
            zh3(i)=sz3(i)
            xh4(i)=sx4(i)
            yh4(i)=sy4(i)
            zh4(i)=sz4(i)
            xh5(i)=sx5(i)
            yh5(i)=sy5(i)
            zh5(i)=sz5(i)
            xh6(i)=sx6(i)
            yh6(i)=sy6(i)
            zh6(i)=sz6(i)
            xh7(i)=sx7(i)
            yh7(i)=sy7(i)
            zh7(i)=sz7(i)
            xh8(i)=sx8(i)
            yh8(i)=sy8(i)
            zh8(i)=sz8(i)
c
         enddo
c
         if (nxdof.gt.0) then
            do k=1,8
               do j=1,nxdof
                  do i=lft,llt
                     xhdof(i,k,j)=xdof(i,k,j)-
     1                    .5*dxdof(i,k,j)*dt1siz(i)
                  enddo
               enddo
            enddo
         endif
c
      endif
c
      if (nip.eq.1.and.ihgf.eq.1) then
c
c     LS-DYNA hourglass requested for single point integrated solid
c
         if (lenvec8.ne.0) then
c
            do i=lft,llt
c
               x1(i)=sx1(i)
               y1(i)=sy1(i)
               z1(i)=sz1(i)
               x2(i)=sx2(i)
               y2(i)=sy2(i)
               z2(i)=sz2(i)
               x3(i)=sx3(i)
               y3(i)=sy3(i)
               z3(i)=sz3(i)
               x4(i)=sx4(i)
               y4(i)=sy4(i)
               z4(i)=sz4(i)
               x5(i)=sx5(i)
               y5(i)=sy5(i)
               z5(i)=sz5(i)
               x6(i)=sx6(i)
               y6(i)=sy6(i)
               z6(i)=sz6(i)
               x7(i)=sx7(i)
               y7(i)=sy7(i)
               z7(i)=sz7(i)
               x8(i)=sx8(i)
               y8(i)=sy8(i)
               z8(i)=sz8(i)
c
            enddo
c
         endif
c
         if (ihg.ne.3.and.ihg.ne.5.and.ihg.ne.6) then
            call prtal(lft,llt)
         else
            call prtalf(lft,llt)
         endif
c
         if (lenvec8.ne.0) then
c
            do i=lft,llt
               spx1(i)=px1(i)
               spx2(i)=px2(i)
               spx3(i)=px3(i)
               spx4(i)=px4(i)
               spx5(i)=px5(i)
               spx6(i)=px6(i)
               spx7(i)=px7(i)
               spx8(i)=px8(i)
               spy1(i)=py1(i)
               spy2(i)=py2(i)
               spy3(i)=py3(i)
               spy4(i)=py4(i)
               spy5(i)=py5(i)
               spy6(i)=py6(i)
               spy7(i)=py7(i)
               spy8(i)=py8(i)
               spz1(i)=pz1(i)
               spz2(i)=pz2(i)
               spz3(i)=pz3(i)
               spz4(i)=pz4(i)
               spz5(i)=pz5(i)
               spz6(i)=pz6(i)
               spz7(i)=pz7(i)
               spz8(i)=pz8(i)
               volm(i)=voln(i)
               x1(i)=x(1,ix1(i))
               y1(i)=x(2,ix1(i))
               z1(i)=x(3,ix1(i))
               x2(i)=x(1,ix2(i))
               y2(i)=x(2,ix2(i))
               z2(i)=x(3,ix2(i))
               x3(i)=x(1,ix3(i))
               y3(i)=x(2,ix3(i))
               z3(i)=x(3,ix3(i))
               x4(i)=x(1,ix4(i))
               y4(i)=x(2,ix4(i))
               z4(i)=x(3,ix4(i))
               x5(i)=x(1,ix5(i))
               y5(i)=x(2,ix5(i))
               z5(i)=x(3,ix5(i))
               x6(i)=x(1,ix6(i))
               y6(i)=x(2,ix6(i))
               z6(i)=x(3,ix6(i))
               x7(i)=x(1,ix7(i))
               y7(i)=x(2,ix7(i))
               z7(i)=x(3,ix7(i))
               x8(i)=x(1,ix8(i))
               y8(i)=x(2,ix8(i))
               z8(i)=x(3,ix8(i))
            enddo
c
            if (ihg.ne.3.and.ihg.ne.5.and.ihg.ne.6) then
               call prtal(lft,llt)
            else
               call prtalf(lft,llt)
            endif
c
         endif
c
      else
c
c     Compute volume of element
c
         do i=lft,llt
            x17=x7(i)-x1(i)
            x28=x8(i)-x2(i)
            x35=x5(i)-x3(i)
            x46=x6(i)-x4(i)
            y17=y7(i)-y1(i)
            y28=y8(i)-y2(i)
            y35=y5(i)-y3(i)
            y46=y6(i)-y4(i)
            z17=z7(i)-z1(i)
            z28=z8(i)-z2(i)
            z35=z5(i)-z3(i)
            z46=z6(i)-z4(i)
            aj1=x17+x28-x35-x46
            aj2=y17+y28-y35-y46
            aj3=z17+z28-z35-z46
            a17=x17+x46
            a28=x28+x35
            b17=y17+y46
            b28=y28+y35
            c17=z17+z46
            c28=z28+z35
            aj4=a17+a28
            aj5=b17+b28
            aj6=c17+c28
            aj7=a17-a28
            aj8=b17-b28
            aj9=c17-c28
            aj5968=aj5*aj9-aj6*aj8
            aj6749=aj6*aj7-aj4*aj9
            aj4857=aj4*aj8-aj5*aj7
            voln(i)=o64th*(aj1*aj5968+aj2*aj6749+aj3*aj4857)
         enddo
c
      endif
c
c     Compute parameters used in material routines
c
      crho=.0625*rhoa(lft)
c
      do i=lft,llt
         voloi=u(nnm2+i-1)
         xm(i)=1./voloi
         vlrho(i)=crho*voloi
         dfe(i)=voln(i)*xm(i)
      enddo
c
c     Compute characteristic length used for
c     time step calculations
c
      call felen (lft,llt)
c
c     Initialize some parameters
c
      do i=lft,llt
c     volume at t_{n+1/2} and t_{n+1}
         voltot(i)=0.
         cvltot(i)=0.
         dd(i)=0.
         ddq(i)=0.
      enddo
c
c     Zero internal force vector
c
      ndtot=8*(3+nxdof)
      do j=1,ndtot
         do i=lft,llt
            frc(i,j)=0.
         enddo
      enddo
c
      if (nip.gt.0) then
c
c     Loop over integration points
c
      do ipt=1,nip
c
c     Gather parental coordinates and weights
c
         xiipt=xipn(1,ipt,mxe)
         etaipt=xipn(2,ipt,mxe)
         zetaipt=xipn(3,ipt,mxe)
         wgt=xipn(4,ipt,mxe)
c
c     Pointer to first item in history variables array
c
         lavloc=(ipt-1)*nmtcon+lav
         locstn=lavloc+nmtcon-6*jstrn
c
c     Compute b-matrix and g-matrix
c
         call usrsld_b(ietyp,xiipt,etaipt,zetaipt,lft,llt)
c
c     Compute volume of integration point and "real" b-matrix
c
         call usrsld_b2b(itaj,nxdof,wgt,lft,llt)
         do i=lft,llt
            vlm(i)=vlm(i)*wgt
         enddo
c
c     Compute strain and rotational increments
c
         call usrsld_str(nxdof,lft,llt)
c
c     Zero energy increments
c
         do i=lft,llt
            einc(i)=0.
         enddo
c
c     Compute deformation gradient if necessary
c
         call usrsld_grd(rots,nxdof,ietyp,mte,
     1        xiipt,etaipt,zetaipt,lft,llt)
c
         if (ihgf.eq.1.and.nip.eq.1) then
c
            if (ihg.gt.3.and.ihg.lt.6) then
c
c     Rotation of hourglass tensor related to the
c     Flanagan-Belytschko stiffness form
c
               call hgfrot (hgforc(1,nnm2),lft,llt)
c
            endif
c
         endif
c
c     icpf.ne.0 not supported
c
c     Gather history variables
c
         nc=nip*nmtcon
         if (mte.eq.126) nc=nc+27
c
         do j=1,nmtcon
            do i=lft,llt
               ax(i,j)=auxvec(lavloc+nc*(i-lft)+(j-1))
            enddo
         enddo
c
c     Rayleigh damping
c
         if (abs(betav).gt.0.000)  then
            call rydmp1 (lft,llt)
            if (lenvec(5).eq.1) then
               call rydmp5 (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),
     .              ax(1,nmtcon-3),ax(1,nmtcon-2),ax(1,nmtcon-1),
     .              ax(1,nmtcon),ies(nnm2),1)
            endif
         endif
c
c     Process thermal expansion
c
         if (ithxpid.gt.0) then
            call adthstr(lft,llt,1)
            if (icompf(mte).eq.mte)
     .           call mlthjc(lft,llt)
         endif
c
c     Constitutive matrix
c
         if (is17loc.eq.1.or.ihgf.eq.3) then
            call cmatsi3d (cm,lft,llt,mte,npc,pld,tnew,fval)
         endif
c
         if (lenvec8.ne.0) then
c
c     Accuracy option is on, set up for
c     half-step rotation of stresses
c
            do i=lft,llt
               wxxdt(i)=.50*wxxdt(i)
               wyydt(i)=.50*wyydt(i)
               wzzdt(i)=.50*wzzdt(i)
            enddo
            lenvech=1
c
         endif
c
         if (istrn.ne.0) then
            if (isolvr(18).eq.0) then
c
c     Rotate strains for post-processing
c
               call usrsld_rstr(strains(1,nnm2),lft,llt,ipt,nip,wgt)
               if (jstrn.eq.1) call qrttrs (auxvec(locstn),lft,llt,nc)
               if (cm(5,mxt(lft)).lt.0..and.
c                 need updated strain for usrmat_24
     .            (mte.eq.24.or.mte.eq.114.or.mte.eq.123.or.
     .             mte.eq.124.or.mte.eq.155.or.mte.eq.182.or.
     .             mte.eq.238.or.mte.eq.255))
     .         call sldeps4um (auxvec(locstn),lft,llt,nc)
c
            endif
         endif
c
c     For passing correct hourglass ID
c
         if (nip.eq.1.and.ihgf.eq.1) then
            ihgp=ihg
         else
            ihgp=5
         endif
c
      if (ioshl(68).gt.0) then
c
c     Stress initialization?
c
         call acmfch_00 (cm,mte,a(ncs1),ia(ncs2),ia(ncs11),ia(ncs14),
     .      numcsd,nnm1,lft,llt,auxvec,fval,ia(ioshl(69)),ia(ioshl(70)),
     .      ia(ncs1+20*numcsd),ia(ioshl(71)),ies(nnm2),nmmat)
      endif
c
      if (mte.gt.10) go to 120
      go to (20,30,40,50,60,70,80,90,100,110), mte
   20 call rstrss (lft,llt)
      call f3dm1 (cm,bqs(nnm2),lft,llt)
      go to 207
   30 call f3dm2 (cm,bqs(nnm2),ihgp,lft,llt,mxe,eosp,cmaux)
      go to 207
   40 call rstrss (lft,llt)
      call rstrn (lft,llt)
      call f3dm3 (cm,bqs(nnm2),lft,llt)
      go to 207
   50 call rstrss (lft,llt)
      call f3dm4 (cm,bqs(nnm2),tnew,fval,lft,llt)
      go to 207
   60 call rstrss (lft,llt)
      call f3dm5 (cm,bqs(nnm2),u(nnm2),0,lft,llt)
      go to 207
   70 call rstrss (lft,llt)
      call rstrnn (lft,llt,1)
      call f3dm6 (cm,bqs(nnm2),lft,llt,ia(islcnt16))
      go to 207
   80 call rstrss (lft,llt)
      call f3dm7 (cm,bqs(nnm2),ihgp,lft,llt)
      go to 207
   90 call f3dm8 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
  100 call rstrss (lft,llt)
      call f3dm9 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
  110 call rstrss (lft,llt)
      call f3dm10 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
  120 if (mte.lt.30) then
      mte10=mte-10
      go to (130,140,150,160,170,180,190,192,194,207,196,197,198,199,
     1 200,204,205),mte10
      else
      if (mte.eq.30) then
      call rstrss (lft,llt)
      call f3dm30(cm,bqs(nnm2),ia(islcnt16),tnew,fval,lft,llt)
      go to 207
      endif
      if (mte.eq.98) then
      call rstrss (lft,llt)
      call f3dm98 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.159) then
      call rstrss (lft,llt)
      call f3dm159 (cm,cmaux,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.31) then
      call rstrss (lft,llt)
      call f3dm31 (cm,bqs(nnm2),ihgp,lft,llt)
      go to 207
      endif
      if (mte.eq.33) then
      call rstrss (lft,llt)
      call f3dm33 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.35) then
      call rqtat1 (lft,llt)
      call gtvrtm (lft,llt)
      call f3dm35 (cm,bqs(nnm2),lft,llt)
      call rqtat2 (lft,llt)
      go to 207
      endif
      if (mte.eq.36) then
      call rqtat1 (lft,llt)
      call gtvrtm (lft,llt)
      call f3dm36 (cm,lft,llt)
      call rqtat2 (lft,llt)
      go to 207
      endif
      if (mte.eq.38) then
      call rstrss (lft,llt)
      call f3dm38 (cm,bqs(nnm2),ihgp,lft,llt)
      go to 207
      endif
      if (mte.eq.40) then
      call rstrss (lft,llt)
      call f3dm40 (cm,npc,pld,bqs(nnm2),ihgp,lft,llt,cmaux)
      go to 207
      endif
      if (mte.eq.141) then
      call rstrss (lft,llt)
      call rstrn  (lft,llt)
      call f3dm141(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.142) then
      call rstrss (lft,llt)
      call f3dm142(cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.143) then
      call rstrss (lft,llt)
      call f3dm143(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.51) then
      call rstrss (lft,llt)
      call rstrn (lft,llt)
      call f3dm51(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.52) then
      call rstrss (lft,llt)
      call rstrn (lft,llt)
      call f3dm52 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.151) then
      call rstrss (lft,llt)
      call rstrn (lft,llt)
      call f3dm151(cm,eosp,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.53) then
      call rstrss (lft,llt)
      call f3dm53 (cm,bqs(nnm2),lft,llt,ia(islcnt16))
      go to 207
      endif
      if (mte.eq.57) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      if (is17loc.eq.1) then
      call s57main (cm,npc,pld,lft,llt,ia(islcnt16),
     &             a(n8+2*nlcur+1),a(n8+3*nlcur+1))
      endif
      call f3dm57 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt16),
     &             a(n8+2*nlcur+1),a(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.261) then
      call rstrss (lft,llt)
      call f3dm261(cm,bqs(nnm2),lft,llt,ipt,nip,a(islcnt(16)),a(ns21))
      go to 207
      endif
      if (mte.eq.262) then
      call rstrss (lft,llt)
      call f3dm262(cm,bqs(nnm2),lft,llt,ipt,nip,a(islcnt(16)),a(ns21))
      go to 207
      endif
      if (mte.eq.213) then
      call rstrss (lft,llt)
      call f3dm213(cm,bqs(nnm2),lft,llt,npc,pld,a(islcnt(16)),
     . a(n8+2*nlcur+1),a(n8+3*nlcur+1),ipt,nip,eosp)
      go to 207
      endif
      if (mte.eq.215) then
      call rstrss (lft,llt)
      call f3dm215(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.177) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm177 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.178) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm178 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.179.or.mte.eq.180) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm179 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt16),mte,
     .     a(n8+2*nlcur+1),a(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.181) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm181 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),eosp)
      go to 207
      endif
c
      if (mte.eq.187) then
      call rstrss (lft,llt)
      call f3dm187 (cm,nnm1,bqs(nnm2),a(ioshl(332)),a(ioshl(333)),
     &              ipt,lft,llt)
      go to 207
      endif
c
      if (mte.eq.183) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm183 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.100) then
      call rstrss (lft,llt)
      call f3dm100 (cm,bqs(nnm2),nhxbwp(nnm2),lft,llt,npc,pld,
     , ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.73) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm73 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt16))
      go to 207
      endif
      if (mte.eq.83) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      lthrpr=nint(cm(48,mxe))+lc2
      call f3dm83 (cm,bqs(nnm2),npc,pld,lft,llt,ia(lthrpr),
     . ia(lthrpr+20),ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),
     . ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.77) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm77 (cm,eosp,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.127) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm127 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.129) then
      call rstrss (lft,llt)
      call gtvrtn (lft,llt)
      call f3dm129 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
      endif
c
c***  TNO failure models
      if (mte.eq.131) then
      call rstrss (lft,llt)
      call f3dm131(cm,bqs(nnm2),nnm1,lft,llt,nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.132) then
      call rstrss (lft,llt)
      call f3dm132(cm,bqs(nnm2),lft,llt,nhxbwp(nnm2),cmaux)
      go to 207
      endif
c
      if (mte.eq.87) then
      call rstrss (lft,llt)
      if (is17loc.eq.1) call bfgs_tan0(lft,llt)
      call gtvrtn (lft,llt)
      call f3dm87 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.147) then
      call rstrss (lft,llt)
      call f3dm147 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.91) then
        call rstrss (lft,llt)
        call f3dm91(cm,bqs(nnm2),lft,llt)
        go to 207
      endif
      if (mte.eq.92) then
        call rstrss (lft,llt)
        call f3dm92(cm,bqs(nnm2),lft,llt)
        go to 207
      endif
      if (mte.eq.96) then
      call rstrss (lft,llt)
      call rstrn3 (lft,llt)
      call rstrn4 (lft,llt)
      call f3dm96 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.59) then
      call rstrss (lft,llt)
      call f3dm59(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.80) then
      call rstrss (lft,llt)
      call f3dm80(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.60) then
      call rstrss (lft,llt)
      call f3dm60 (cm,bqs(nnm2),fval,tnew,lft,llt)
      go to 207
      endif
      if (mte.eq.89) then
      call rstrss (lft,llt)
      call rtensor(lft,llt,ax(1,20))
      call rtensor(lft,llt,ax(1,26))
      call f3dm89 (cm,bqs(nnm2),lft,llt,npc,pld,
     1             ipt,ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.61) then
      call rstrss (lft,llt)
      call rstrnn61 (lft,llt,2)
      call f3dm61 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.164) then
      call rstrss (lft,llt)
      call rstrnn61 (lft,llt,2)
      call f3dm164(cm,bqs(nnm2),lft,llt,ies(nnm2),hges(nnm2))
      go to 207
      endif
      if (mte.eq.165) then
      call rstrss (lft,llt)
      call rstrn (lft,llt)
      call f3dm165 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.62) then
      call rstr62 (lft,llt)
      call f3dm62 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.75) then
      call rstrss (lft,llt)
      call f3dm75 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),npc,pld)
      go to 207
      endif
      if (mte.eq.63) then
      call rstrss (lft,llt)
      if (is17loc.eq.1) then
      call s63main (cm,npc,pld,lft,llt,ia(islcnt(16)))
      endif
      call f3dm63 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.64) then
      call rstrss (lft,llt)
      call f3dm64 (cm,bqs(nnm2),npc,pld,lft,llt,nhxbwp(nnm2),1,1)
      go to 207
      endif
      if (mte.eq.70) then
      call rstrss (lft,llt)
      call f3dm70 (cm,bqs(nnm2),npc,pld,lft,llt)
      go to 207
      endif
      if (mte.eq.126) then
      call rstrss (lft,llt)
      call f3dm126(cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)),cmaux)
      go to 207
      endif
      if (mte.eq.76) then
      call rstrss (lft,llt)
      call f3dm76 (cm,eosp,bqs(nnm2),tnew,lft,llt)
      go to 207
      endif
      if (mte.eq.276) then
      call rstrss (lft,llt)
      call f3dm276 (cm,bqs(nnm2),tnew,lft,llt)
      go to 207
      endif
      if (mte.eq.277) then
      call rstrss (lft,llt)
      call f3dm277 (cm,bqs(nnm2),tnew,lft,llt)
      go to 207
      endif
      if (mte.eq.176) then
      call rstrss (lft,llt)
      call f3dm176 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.78) then
      call rstrss (lft,llt)
      call f3dm78 (cm,bqs(nnm2),0,ia(islcnt(16)),lft,llt)
      go to 207
      endif
      if (mte.eq.79) then
      call rstr79 (lft,llt)
      call f3dm79 (cm,bqs(nnm2),fval,npc,pld,lft,llt)
      go to 207
      endif
c     Winfrith concrete model not supported
      if (mte.eq.65) then
      call rstrss (lft,llt)
      call f3dm65 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
      endif
      if (mte.eq.72) then
      call rstrss (lft,llt)
      lcm2=lc2+nint(cm(7,mxe)-1.)
      call f3dm72 (cm,npc,pld,eosp,mte,nes,ibq,ies(nnm2),fval,
     .             nhxbwp(nnm2),a(lcm2),lft,llt,bqs(nnm2),ipt,nip)
      go to 207
      endif
      if (mte.eq.88) then
      call rstrss (lft,llt)
      call f3dm88 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
      endif
      if (iamusrmat(mte)) then
      call rstrss (lft,llt)
      call usrmat (lft,llt,cm,bqs(nnm2),capa,'solid',mte,ipt,
     . a(n8),a(n9),a(islcnt(16)),0.,0.,0.,nnm1,nip,ipt)
      if(nes.ne.0) then
         if (ipt.eq.nip) then
            call sueos (eosp,mte,nes,lft,llt)
            call bulkq (ibq,nhxbwp(nnm2),lft,llt)
         endif
         if (nip.ne.1) then
            do i=lft,llt
               vlinc(i)=vlm(i)
            enddo
            call heupd8 (lft,llt,ies(nnm2),ipt,ener)
         else
            call hieupd (lft,llt)
         endif
         if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
         goto 208
      else
         go to 207
      endif
      endif
      if (mte.eq.102) then
      call rstrss (lft,llt)
      call f3dm102 (cm,bqs(nnm2),lft,llt,tnew,fval,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.103) then
      call rstrss (lft,llt)
      call f3dm103 (cm,eosp,bqs(nnm2),lft,llt,npc,pld,ia(islcnt(16)),
     .     ia(n8+2*nlcur+1),ia(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.157) then
      call rstrss (lft,llt)
      call f3dm157 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),
     .              ia(n8+2*nlcur+1),ia(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.122) then
      call rstrss (lft,llt)
      call f3dm122 (cm,eosp,bqs(nnm2),lft,llt,npc,pld,ia(islcnt(16)),
     .     ia(n8+2*nlcur+1),ia(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.104) then
      call rstrss (lft,llt)
      call f3dm104 (cm,eosp,bqs(nnm2),lft,llt,npc,pld,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.105) then
      call rstrss (lft,llt)
      call f3dm105 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.106) then
      call rstrss (lft,llt)
      call f3dm106(cm,bqs(nnm2),tnew,fval,lft,llt,ia(islcnt(16)),
     . ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),a(n9),a(n8))
      go to 207
      endif
      if (mte.eq.188) then
      call rstrss (lft,llt)
      call f3dm188(cm,bqs(nnm2),tnew,fval,lft,llt,ia(islcnt(16)),
     . ia(n8+2*nlcur+1),ia(n8+3*nlcur+1))
      go to 207
      endif
      if (mte.eq.107) then
      call rstrss (lft,llt)
      call f3dm107 (cm,bqs(nnm2),lft,llt,tnew,fval)
      go to 207
      endif
      if (mte.eq.109) then
      call rstrss (lft,llt)
      call f3dm109 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.110) then
      call rstrss (lft,llt)
      call f3dm110 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.241) then
      call rstrss (lft,llt)
      call f3dm241 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.111) then
      call rstrss (lft,llt)
      call f3dm111 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.269) then
      call rstrss (lft,llt)
      call f3dm269 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.270) then
      call rstrss (lft,llt)
      call f3dm270 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.271) then
      call rstrss (lft,llt)
      call f3dm271 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),0,0,0)
      go to 207
      endif
      if (mte.eq.272) then
      call rstrss (lft,llt)
      call f3dm272 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.274) then
         call f3dm274 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),eosp)
         go to 207
      endif
      if (mte.eq.275) then
      call rstrss (lft,llt)
      call f3dm275 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.112) then
      call rstrss (lft,llt)
      call f3dm112 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),a(ifdbck(31)))
      go to 207
      endif
      if (mte.eq.115) then
      call rstrss (lft,llt)
      call f3dm115(cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.120) then
      call rstrss (lft,llt)
      call f3dm120(cm,bqs(nnm2),lft,llt,ia(islcnt(16)),ia(n8+2*nlcur+1),
     + ia(n8+3*nlcur+1),x,u(nnm2),ipt,eosp)
      go to 207
      endif
      if (mte.eq.123) then
      call rstrss (lft,llt)
      call f3dm123 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),ia(ifdbck(31)),ipt,nip,
     . nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.124) then
      call rstrss (lft,llt)
      call f3dm124 (cm,eosp,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),ipt,nip,nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.210) then
      call rstrss (lft,llt)
      call f3dm210 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.244) then
      call rstrss (lft,llt)
      call f3dm244 (cm,eosp,lft,llt,tnew,bqs(nnm2),ia(islcnt(16)),
     $     npc,pld,a(n8+2*nlcur+1),a(n8+3*nlcur+1),
     $     a(ioshl(488)),ipt,nnm1)
      go to 207
      endif
      if (mte.eq.255) then
      call rstrss (lft,llt)
      call f3dm255 (cm,tnew,bqs(nnm2),npc,pld,lft,llt,
     $     ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),ia(islcnt(16)),ipt,nip,
     $     nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.223) then
      call rstrss (lft,llt)
      lcmx=lc2+nint(cm(48,mxe))
      call f3dm223 (cm,a(lcmx),bqs(nnm2),lft,llt,1,1)
      go to 207
      endif
      if (mte.eq.229) then
      call rstrss (lft,llt)
      lcmx=lc2+nint(cm(48,mxe))
      call f3dm229 (cm,a(lcmx),bqs(nnm2),lft,llt,ipt,nip,nnm1)
      go to 207
      endif
      if (mte.eq.233) then
      call rstrss (lft,llt)
      call f3dm233 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),npc,pld,
     . ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),ipt)
      go to 207
      endif
      if (mte.eq.256) then
      call f3dm256 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.221) then
      call rstrss (lft,llt)
      call f3dm221 (cm,eosp,bqs(nnm2),lft,llt,1,1)
      go to 207
      endif
      if (mte.eq.224) then
      call rstrss (lft,llt)
      call f3dm224 (cm,eosp,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),ipt,nes)
      if (nes.eq.0) then
        go to 207
      else
        if (ipt.eq.nip) then
          call sueos (eosp,mte,nes,lft,llt)
          iffg=0
          if (nip.eq.1.and.ihgf.eq.1.and.ihg.eq.6)
     .      call hrgbbc (cm,ia(islcnt(16)),mte,lft,llt,iffg,x,auxvec,1)
          call bulkq (ibq,nhxbwp(nnm2),lft,llt)
        endif
        if (nip.ne.1) then
           do i=lft,llt
              vlinc(i)=vlm(i)
           enddo
           call heupd8 (lft,llt,ies(nnm2),ipt,ener)
        else
           call hieupd (lft,llt)
        endif
        if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
        go to 208
      endif
      endif
      if (mte.eq.225) then
      call rstrss (lft,llt)
      call rstrn225(lft,llt)
      call f3dm225 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.228) then
      call rstrss (lft,llt)
      call f3dm228 (cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.155) then
      call rstrss (lft,llt)
      call f3dm155(cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),ipt,nip,nhxbwp(nnm2))
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         iffg=0
         if (nip.eq.1.and.ihgf.eq.1.and.ihg.eq.6)
     .     call hrgbbc (cm,ia(islcnt(16)),mte,lft,llt,iffg,x,auxvec,1)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
      endif
      if (mte.eq.128) then
      call rstrss (lft,llt)
      call f3dm128(cm,bqs(nnm2),ihgp,lft,llt)
      go to 207
      endif
      if (mte.eq.144) then
      call rstrss (lft,llt)
      call f3dm144 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)))
      go to 207
      endif
      if (mte.eq.145) then
      call rstrss (lft,llt)
      call f3dm145 (cm,bqs(nnm2),lft,llt,nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.152) then
      call rstrss (lft,llt)
      call f3dm152 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),tnew,fval)
      go to 207
      endif
      if (mte.eq.153) then
      call rstrss (lft,llt)
      call rstrn153 (lft,llt)
      lthrpr=nint(cm(48,mxe))+lc2
      call f3dm153 (cm,bqs(nnm2),npc,pld,lft,llt)
      go to 207
      endif
      if (mte.eq.154) then
      call rstrss (lft,llt)
      call f3dm154 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.161) then
      call rstrss (lft,llt)
      call f3dm161 (mtype,cm,bqs(nnm2),lft,llt,nnm1,auxvec,a(nh14),
     . nhxbwp,a(lc1h),a(iptadj))
      go to 207
      endif
      if (mte.eq.162) then
      call rstrss (lft,llt)
      lcmaux=nint(cm(48,mxe))+lc2
      call f3dm162 (mtype,cm,bqs(nnm2),lft,llt,nnm1,auxvec,a(nh14),
     . nhxbwp,a(lc1h),a(iptadj),ia(lcmaux))
      go to 207
      endif
      if (mte.eq.163) then
      call rstrss (lft,llt)
      call f3dm163 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)),
     . ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),ihgp)
      go to 207
      endif
      if (mte.eq.167) then
      call rstrss (lft,llt)
      call f3dm167 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.168) then
      call rstrss (lft,llt)
      call f3dm168 (cm,bqs(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.169) then
      call rstrss (lft,llt)
      call f3dm169(cm,bqs(nnm2),lft,llt,a(ns21),nip,ipt,a(islcnt(16)),
     . nhxbwp(nnm2))
      go to 207
      endif
      if (mte.eq.173) then
      call rstrss (lft,llt)
      call f3dm173(cm,bqs(nnm2),lft,llt,x,fval,npc,pld,ipt,eosp,
     &              0,idummy,0)
      go to 207
      endif
      if (mte.eq.189) then
      call f3dm189(cm,bqs(nnm2),ihgp,lft,llt,
     .             a(islcnt(16)),tnew,fval)
      go to 207
      endif
      if (mte.eq.192) then
      call rstrss (lft,llt)
      call f3dm192(cm,bqs(nnm2),lft,llt,x,nhxbwp(nnm2))
      call m192_store_dtx (lft,llt,0)
      go to 207
      endif
      if (mte.eq.193) then
      call rstrss (lft,llt)
      call f3dm193(cm,bqs(nnm2),lft,llt,x)
      go to 207
      endif
      if (mte.eq.198) then
      call rstrss (lft,llt)
      call f3dm198(cm,bqs(nnm2),lft,llt,x,fval)
      go to 207
      endif
      if(mte.eq.200) then
      call rstrss (lft,llt)
      call f3dm200 (cm,npc,pld,eosp,mte,nes,ibq,ies(nnm2),fval,
     .             nhxbwp(nnm2),lft,llt)
      go to 207
      endif
      if (mte.eq.252) then
      call rstrss (lft,llt)
      call f3dm252 (cm,bqs(nnm2),lft,llt,ia(islcnt(16)),
     . ia(n8+2*nlcur+1),ia(n8+3*nlcur+1),nhxbwp(nnm2))
      go to 207
      endif
      endif
  130 call rstrss (lft,llt)
      call f3dm11 (cm,lft,llt)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
  140 call rstrss (lft,llt)
      call f3dm12 (cm,bqs(nnm2),0,lft,llt)
      go to 207
  150 call rstrss (lft,llt)
      call f3dm13 (cm,bqs(nnm2),ibq,nhxbwp(nnm2),lft,llt,1)
      go to 207
  160 call rstrss (lft,llt)
      call f3dm14 (cm,bqs(nnm2),lft,llt)
      go to 207
  170 call rstrss (lft,llt)
      call f3dm15 (cm,lft,llt,1.)
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      go to 208
  180 call rstrss (lft,llt)
      call f3dm16 (cm,npc,pld,eosp,mte,nes,ibq,ies(nnm2),fval,
     .             nhxbwp(nnm2),lft,llt,ipt,nip)
      go to 207
  190 call rstrss (lft,llt)
      call f3dm17 (cm,x,nnm1,lft,llt,nhxbwp(nnm2),a(ns21))
      if (ipt.eq.nip) then
         call sueos (eosp,mte,nes,lft,llt)
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      endif
      if (nip.ne.1) then
         do i=lft,llt
            vlinc(i)=vlm(i)
         enddo
         call heupd8 (lft,llt,ies(nnm2),ipt,ener)
      else
         call hieupd (lft,llt)
      endif
      if (ipt.eq.nip) call eqos (fval,ies(nnm2),mte,nes,lft,llt)
      call fzero (lft,llt)
      go to 208
  192 call rstrss (lft,llt)
      call f3dm18 (cm,bqs(nnm2),lft,llt)
      go to 207
  194 call rstrss (lft,llt)
      call f3dm19 (cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
  196 call f3dm21(cm,bqs(nnm2),tnew,fval,ihgp,lft,llt,1,1)
      go to 207
  197 call rstrss (lft,llt)
      call f3dm22(cm,bqs(nnm2),lft,llt)
      go to 207
  198 lthrpr=nint(cm(48,mxe))+lc2
      call f3dm23(cm,bqs(nnm2),tnew,fval,ia(lthrpr),ihgp,lft,llt)
      go to 207
  199 call rstrss (lft,llt)
      call f3dm24(cm,bqs(nnm2),npc,pld,lft,llt,ia(n8+2*nlcur+1),
     . ia(n8+3*nlcur+1),ia(islcnt(16)),ia(ifdbck(31)),ipt,nip,
     . nhxbwp(nnm2))
      go to 207
  200 call rstrss (lft,llt)
      call f3dm25(cm,lft,llt)
      go to 207
  204 call rstrss (lft,llt)
      if (is17loc.eq.1)
     .call s26maini(cm,        npc,pld,lft,llt,ia(islcnt(16)))
      call f3dm26(cm,bqs(nnm2),npc,pld,lft,llt,ia(islcnt(16)))
      go to 207
  205 call rstrss (lft,llt)
      call f3dm27(cm,bqs(nnm2),ihgp,lft,llt)
  207 continue
c
c     Compute bulk viscosity
c
      if (mte.ne.100) then
         if (ipt.eq.1) call bulkq (ibq,nhxbwp(nnm2),lft,llt)
      else
         if (ipt.eq.1) call bulkq_sw (ibq,nhxbwp(nnm2),lft,llt)
      endif
c
      if (mte.eq.192) call m192_store_dtx (lft,llt,1)
c
c     Update internal energy and bulk stress
c
      if (isolvr(18).eq.0) then
         do i=lft,llt
            ies(nnm2+i-1)=ies(nnm2+i-1)+
     1           .5*vlm(i)*(einc(i)+dt1siz(i)*dd(i)*qp(i))
            bqs(nnm2+i-1)=qp(i)
         enddo
      endif
 208  continue
c
c     borrvall, thermal expansion
c
      if (ithxpid.gt.0) then
         call sbthstr(lft,llt,1)
         if (icompf(mte).eq.mte)
     .        call dvthjc(lft,llt)
      endif
c
      if (lenvec8.ne.0) then
c
c     Accuracy option is on, rotate stress from n+1/2 to n+1
c
         if (mhyper(mte).ne.57) then
c
            call rstrss (lft,llt)
c
c     Rotation of back stress & similar tensors
c
            if (mte.eq.6) then
               call rstrnn   (lft,llt,1)
            elseif (mte.eq.3.or.mte.eq.51.or.
     .              mte.eq.52.or.mte.eq.141.or.mte.eq.165) then
               call rstrn    (lft,llt)
            elseif (mte.eq.225 ) then
               call rstrn225 (lft,llt)
            elseif (mte.eq.61) then
               call rstrnn61 (lft,llt,2)
            elseif (mte.eq.62) then
               call rstr62   (lft,llt)
            elseif (mte.eq.79) then
               call rstr79   (lft,llt)
            elseif (mte.eq.89) then
               call rtensor(lft,llt,ax(1,20))
               call rtensor(lft,llt,ax(1,26))
            elseif (mte.eq.96) then
               call rstrn3   (lft,llt)
               call rstrn4   (lft,llt)
            elseif (mte.eq.153 ) then
               call rstrn153 (lft,llt)
            endif
         endif
      endif
c
c     No interior contact supported
c
      if (ioshl(68).gt.0) then
c
c     Stress initialization?
c
         call acmfch_01 (cm,mte,a(ncs1),ia(ncs2),ia(ncs11),ia(ncs14),
     .      numcsd,nnm1,lft,llt,auxvec,fval,ia(ioshl(69)),ia(ioshl(70)),
     .      ia(ncs1+20*numcsd),ia(ioshl(71)),ies(nnm2),nmmat)
      endif
c
      if (is17loc.eq.1.or.ihgf.eq.3) then
c
c     Constitutive matrix
c
         call cmats3d (cm,lft,llt,mte,npc,pld,ia(islcnt(16)),mxe,nnm1,
     .    ia(1),1,0,1,0,1.,1.,0.)
c
c     ALE not supported
c
      endif
c
      if (ihgf.eq.3) then
         if (ipt.eq.1) then
            do j=1,21
               do i=lft,llt
                  uehisv(i,nhsv+j)=uehisv(i,nhsv+j-21)
               enddo
            enddo
            index=1
            do j=1,6
               do k=1,j
                  do i=lft,llt
                     uehisv(i,nhsv+index-21)=dsave(i,j,k)*vlm(i)
                  enddo
                  index=index+1
               enddo
            enddo
         else
            index=1
            do j=1,6
               do k=1,j
                  do i=lft,llt
                     uehisv(i,nhsv+index-21)=uehisv(i,nhsv+index-21)+
     .                    dsave(i,j,k)*vlm(i)
                  enddo
                  index=index+1
               enddo
            enddo
         endif
         if (ipt.eq.nip) then
            fact=.5
            dsum=0.
            do j=1,21
               dsum=dsum+abs(uehisv(lft,nhsv+j))
            enddo
            if (dsum.lt.1.e-20) fact=1.
            do j=1,21
               do i=lft,llt
                  uehisv(i,nhsv+j)=
     .                 fact*(uehisv(i,nhsv+j)+uehisv(i,nhsv+j-21))
               enddo
            enddo
         endif
      endif
c
c     Damage models not supported
c
c     Rayleigh damping
c
      if (abs(betav).gt.0.000) then
         if (lenvec(5).eq.1) then
            call rydmp5 (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),
     .           ax(1,nmtcon-3),ax(1,nmtcon-2),ax(1,nmtcon-1),
     .           ax(1,nmtcon),ies(nnm2),2)
         endif
      endif
c
c     Scatter history variables
c
      if (isolvr(18).eq.0) then
         do j=1,nmtcon
            do i=lft,llt
               auxvec(lavloc+nc*(i-lft)+(j-1))=ax(i,j)
            enddo
         enddo
      endif
c
c     icpf.ne.0 not supported
c
c     tied nodes with failure not supported
c
c     Rayleigh damping
c
      if (abs(betav).gt.0.000) then
         if (lenvec(5).eq.0) then
            call rydmp6 (lft,llt)
         else
            call rydmp4 (lft,llt,ax(1,nmtcon-5),ax(1,nmtcon-4),
     .           ax(1,nmtcon-3),ax(1,nmtcon-2),ax(1,nmtcon-1),
     .           ax(1,nmtcon))
         endif
      endif
c
      if (ihgf.eq.1.and.nip.eq.1) then
c
         if (ihg.ne.3.and.ihg.ne.5.and.ihg.ne.6) then
            if (ihg.eq. 2.or.ihg.eq.4) then
               if (lenvec8.eq.0.or.ihg.eq.2) then
                  call hrgfb (ihg,lft,llt,hgener,hgforc(1,nnm3),iehgfg)
               else
                  call hrgfbp(ihg,lft,llt,hgener,hgforc(1,nnm3),iehgfg)
               endif
            else
               call hrgmd (lft,llt,hgener,hgforc(1,nnm3),iehgfg)
            endif
         else
            if (ihg.eq.6) then
               iffg=0
               call hrgbbc (cm,ia(islcnt(16)),mte,lft,llt,iffg,x,
     .          auxvec,1)
               call hrgbb (lft,llt,hgener,hgforc(1,nnm3),iehgfg,mte,0)
            else
               if (lenvec8.eq.0.or.ihg.eq.3) then
                  call hrgffb (ihg,lft,llt,hgener,hgforc(1,nnm3),iehgfg)
               else
                  call hrgffbp(ihg,lft,llt,hgener,hgforc(1,nnm3),iehgfg)
               endif
            endif
         endif
c
      else
c
         do i=lft,llt
            p11(i)=0.0
            p12(i)=0.0
            p13(i)=0.0
            p14(i)=0.0
            p15(i)=0.0
            p16(i)=0.0
            p17(i)=0.0
            p18(i)=0.0
            p21(i)=0.0
            p22(i)=0.0
            p23(i)=0.0
            p24(i)=0.0
            p25(i)=0.0
            p26(i)=0.0
            p27(i)=0.0
            p28(i)=0.0
            p31(i)=0.0
            p32(i)=0.0
            p33(i)=0.0
            p34(i)=0.0
            p35(i)=0.0
            p36(i)=0.0
            p37(i)=0.0
            p38(i)=0.0
         enddo
c
      endif
c
c     Internal force assembly
c
      call usrsld_frc(nxdof,lft,llt)
c
      if (is17loc.eq.1) then
c
c     Compute tangent stiffness matrix
c
         if (nip.eq.1.and.ihgf.eq.1.and.isolvr(91).ne.2) then
c
c     Store hourglass forces or it will be overwritten
c
            do i=lft,llt
               x1f(i)=p11(i)
               x2f(i)=p12(i)
               x3f(i)=p13(i)
               x4f(i)=p14(i)
               x5f(i)=p15(i)
               x6f(i)=p16(i)
               x7f(i)=p17(i)
               x8f(i)=p18(i)
               y1f(i)=p21(i)
               y2f(i)=p22(i)
               y3f(i)=p23(i)
               y4f(i)=p24(i)
               y5f(i)=p25(i)
               y6f(i)=p26(i)
               y7f(i)=p27(i)
               y8f(i)=p28(i)
               z1f(i)=p31(i)
               z2f(i)=p32(i)
               z3f(i)=p33(i)
               z4f(i)=p34(i)
               z5f(i)=p35(i)
               z6f(i)=p36(i)
               z7f(i)=p37(i)
               z8f(i)=p38(i)
            enddo
            call hrgbbk(ske,lft,llt)
            do i=lft,llt
               p11(i)=x1f(i)
               p12(i)=x2f(i)
               p13(i)=x3f(i)
               p14(i)=x4f(i)
               p15(i)=x5f(i)
               p16(i)=x6f(i)
               p17(i)=x7f(i)
               p18(i)=x8f(i)
               p21(i)=y1f(i)
               p22(i)=y2f(i)
               p23(i)=y3f(i)
               p24(i)=y4f(i)
               p25(i)=y5f(i)
               p26(i)=y6f(i)
               p27(i)=y7f(i)
               p28(i)=y8f(i)
               p31(i)=z1f(i)
               p32(i)=z2f(i)
               p33(i)=z3f(i)
               p34(i)=z4f(i)
               p35(i)=z5f(i)
               p36(i)=z6f(i)
               p37(i)=z7f(i)
               p38(i)=z8f(i)
            enddo
         endif
         if (isolvr(91).ne.2) call usrsld_kmt(ske,nxdof,lft,llt)
         if (isolvr(21).eq.1) call usrsld_kgm(ske,nxdof,lft,llt)
c
      endif
c
      enddo
c
      else
c
c     Resultant element
c
         do i=lft,llt
            cb(i)=cm(1,mxe)
            p11(i)=0.0
            p12(i)=0.0
            p13(i)=0.0
            p14(i)=0.0
            p15(i)=0.0
            p16(i)=0.0
            p17(i)=0.0
            p18(i)=0.0
            p21(i)=0.0
            p22(i)=0.0
            p23(i)=0.0
            p24(i)=0.0
            p25(i)=0.0
            p26(i)=0.0
            p27(i)=0.0
            p28(i)=0.0
            p31(i)=0.0
            p32(i)=0.0
            p33(i)=0.0
            p34(i)=0.0
            p35(i)=0.0
            p36(i)=0.0
            p37(i)=0.0
            p38(i)=0.0
         enddo
         call bulkq (ibq,nhxbwp(nnm2),lft,llt)
         call usrsld_h(ietyp,cmusr(9,mxe),lmc,nhsv,nxdof,lft,llt,ihgf,
     1        ies(nnm2))
c
      endif
c
      if (ihgf.eq.2.or.ihgf.eq.3) then
c
c     User defined hourglass force requested
c
         do i=lft,llt
            hgener(i)=0.
         enddo
         call usrsld_h(ietyp,cmusr(9,mxe),lmc,nhsv,nxdof,lft,llt,ihgf,
     1        hgener)
c
      endif
c
      if (lenvec(1).ne.0) then
         call usrsld_fs(nxdof,rhssav(1,nnm1+1),lft,llt)
      else
         call usrsld_ft(nxdof,fv,lft,llt)
      endif
c
c     Spotweld failure not supported
c
      if (is17loc.eq.1) then
c
c     Compute tangent stiffness matrix
c
         ndofpn=3
         nsnd=0
         if (nxdof.ne.0) nsnd=(nxdof-1)/3+1
         nnpke=8+8*nsnd
         imlft=lft
         imllt=llt
         melemt=llt-lft+1
         do i=lft,llt
            lnodim(i,1)=ix1(i)
            lnodim(i,2)=ix2(i)
            lnodim(i,3)=ix3(i)
            lnodim(i,4)=ix4(i)
            lnodim(i,5)=ix5(i)
            lnodim(i,6)=ix6(i)
            lnodim(i,7)=ix7(i)
            lnodim(i,8)=ix8(i)
         enddo
         do j=1,nsnd
           do i=lft,llt
             do k=1,8
               ij=8+8*(j-1)+k
               lnodim(i,ij)=ixsld(i,k,j)
             enddo
           enddo
         enddo
c
         if (isolvr(77).ne.3.or.nip.ne.0) then
         call imasem
         else
         call imasem_unsym
         endif
c
      endif
c
      return
      end
      subroutine usrsld_b(ietyp,xi,eta,zeta,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/numcpuloc/lenvec8,lenvech
      common/aux8loc/
     & x1(nlq),x2(nlq),x3(nlq),x4(nlq),
     & x5(nlq),x6(nlq),x7(nlq),x8(nlq),
     & y1(nlq),y2(nlq),y3(nlq),y4(nlq),
     & y5(nlq),y6(nlq),y7(nlq),y8(nlq),
     & z1(nlq),z2(nlq),z3(nlq),z4(nlq),
     & z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      real n1,n2,n3,n4,n5,n6,n7,n8
      real dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     .     dn5dxi,dn6dxi,dn7dxi,dn8dxi
      real dn1deta,dn2deta,dn3deta,dn4deta,
     .     dn5deta,dn6deta,dn7deta,dn8deta
      real dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     .     dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta
c
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/aux8loc/)
c$omp threadprivate (/numcpuloc/)
      n1     =.125*(1.-xi)*(1.-eta)*(1.-zeta)
      n2     =.125*(1.+xi)*(1.-eta)*(1.-zeta)
      n3     =.125*(1.+xi)*(1.+eta)*(1.-zeta)
      n4     =.125*(1.-xi)*(1.+eta)*(1.-zeta)
      n5     =.125*(1.-xi)*(1.-eta)*(1.+zeta)
      n6     =.125*(1.+xi)*(1.-eta)*(1.+zeta)
      n7     =.125*(1.+xi)*(1.+eta)*(1.+zeta)
      n8     =.125*(1.-xi)*(1.+eta)*(1.+zeta)
c
      dn1dxi =-.125*(1.-eta)*(1.-zeta)
      dn2dxi = .125*(1.-eta)*(1.-zeta)
      dn3dxi = .125*(1.+eta)*(1.-zeta)
      dn4dxi =-.125*(1.+eta)*(1.-zeta)
      dn5dxi =-.125*(1.-eta)*(1.+zeta)
      dn6dxi = .125*(1.-eta)*(1.+zeta)
      dn7dxi = .125*(1.+eta)*(1.+zeta)
      dn8dxi =-.125*(1.+eta)*(1.+zeta)
c
      dn1deta=-.125*(1.-xi)*(1.-zeta)
      dn2deta=-.125*(1.+xi)*(1.-zeta)
      dn3deta= .125*(1.+xi)*(1.-zeta)
      dn4deta= .125*(1.-xi)*(1.-zeta)
      dn5deta=-.125*(1.-xi)*(1.+zeta)
      dn6deta=-.125*(1.+xi)*(1.+zeta)
      dn7deta= .125*(1.+xi)*(1.+zeta)
      dn8deta= .125*(1.-xi)*(1.+zeta)
c
      dn1dzeta=-.125*(1.-xi)*(1.-eta)
      dn2dzeta=-.125*(1.+xi)*(1.-eta)
      dn3dzeta=-.125*(1.+xi)*(1.+eta)
      dn4dzeta=-.125*(1.-xi)*(1.+eta)
      dn5dzeta= .125*(1.-xi)*(1.-eta)
      dn6dzeta= .125*(1.+xi)*(1.-eta)
      dn7dzeta= .125*(1.+xi)*(1.+eta)
      dn8dzeta= .125*(1.-xi)*(1.+eta)
c
      if (lenvec8.ne.0) then
c
      do l=1,8*(3+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=0.
               enddo
            enddo
         enddo
      enddo
c
      if (ietyp.eq.101) then
         call usld_b101(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
     .        yh1,yh2,yh3,yh4,yh5,yh6,yh7,yh8,
     .        zh1,zh2,zh3,zh4,zh5,zh6,zh7,zh8,
     .        xhdof,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call usld_b102(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
     .        yh1,yh2,yh3,yh4,yh5,yh6,yh7,yh8,
     .        zh1,zh2,zh3,zh4,zh5,zh6,zh7,zh8,
     .        xhdof,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call usld_b103(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
     .        yh1,yh2,yh3,yh4,yh5,yh6,yh7,yh8,
     .        zh1,zh2,zh3,zh4,zh5,zh6,zh7,zh8,
     .        xhdof,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call usld_b104(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
     .        yh1,yh2,yh3,yh4,yh5,yh6,yh7,yh8,
     .        zh1,zh2,zh3,zh4,zh5,zh6,zh7,zh8,
     .        xhdof,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call usld_b105(bmtrx,gmtrx,gmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        xh1,xh2,xh3,xh4,xh5,xh6,xh7,xh8,
     .        yh1,yh2,yh3,yh4,yh5,yh6,yh7,yh8,
     .        zh1,zh2,zh3,zh4,zh5,zh6,zh7,zh8,
     .        xhdof,
     .        lft,llt)
      endif
c
      endif
c
      do l=1,8*(3+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  cmtrx(i,j,k,l)=0.
               enddo
            enddo
         enddo
      enddo
c
      if (ietyp.eq.101) then
         call usld_b101(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        lft,llt)
      elseif (ietyp.eq.102) then
         call usld_b102(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        lft,llt)
      elseif (ietyp.eq.103) then
         call usld_b103(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        lft,llt)
      elseif (ietyp.eq.104) then
         call usld_b104(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        lft,llt)
      elseif (ietyp.eq.105) then
         call usld_b105(cmtrx,hmtrx,hmtrx,
     1        xi,eta,zeta,
     2        n1,n2,n3,n4,n5,n6,n7,n8,
     3        dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4        dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5        dn1deta,dn2deta,dn3deta,dn4deta,
     6        dn5deta,dn6deta,dn7deta,dn8deta,
     7        dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8        dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        lft,llt)
      endif
c
      if (lenvec8.eq.0) then
      do l=1,8*(3+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
               enddo
            enddo
         enddo
      enddo
      endif
c
      return
      end
      subroutine usrsld_b2b(itaj,nxdof,wgt,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform b-matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/numcpuloc/lenvec8,lenvech
      common/prescloc/voltot(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/prescloc/)
c$omp threadprivate (/numcpuloc/)
      if (itaj.eq.0) then
         call usrsld_b2b_old(nxdof,wgt,lft,llt)
         return
      endif
c
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
         vlm(i)=gmtrx(i,1,1)
         voltot(i)=voltot(i)+vlm(i)*wgt
c
      enddo
c
      endif
c
      do i=lft,llt
c
         vlm(i)=hmtrx(i,1,1)
         cvltot(i)=cvltot(i)+vlm(i)*wgt
c
      enddo
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            voltot(i)=cvltot(i)
            gmtrx(i,1,1)=hmtrx(i,1,1)
         enddo
      endif
c
c     Compute "real" b-matrix
c
      if (lenvec8.eq.0) then
         do l=1,8*(3+NXDOFUE)
            do k=1,3
               do j=1,3
                  do i=lft,llt
                     bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
                  enddo
               enddo
            enddo
         enddo
      endif
c
      return
      end
      subroutine usrsld_b2b_old(nxdof,wgt,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     transform b-matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/numcpuloc/lenvec8,lenvech
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/prescloc/voltot(nlq)
c
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/numcpuloc/)
c$omp threadprivate (/prescloc/)
      if (lenvec8.ne.0) then
c
      do i=lft,llt
c
         vlm(i)=gmtrx(i,1,1)*(gmtrx(i,2,2)*gmtrx(i,3,3)-
     1        gmtrx(i,3,2)*gmtrx(i,2,3))+
     2        gmtrx(i,2,1)*(gmtrx(i,3,2)*gmtrx(i,1,3)-
     3        gmtrx(i,1,2)*gmtrx(i,3,3))+
     4        gmtrx(i,3,1)*(gmtrx(i,1,2)*gmtrx(i,2,3)-
     5        gmtrx(i,2,2)*gmtrx(i,1,3))
         voltot(i)=voltot(i)+vlm(i)*wgt
         gjac=1./vlm(i)
         g11i=gmtrx(i,2,2)*gmtrx(i,3,3)-gmtrx(i,2,3)*gmtrx(i,3,2)
         g12i=gmtrx(i,3,2)*gmtrx(i,1,3)-gmtrx(i,3,3)*gmtrx(i,1,2)
         g13i=gmtrx(i,1,2)*gmtrx(i,2,3)-gmtrx(i,1,3)*gmtrx(i,2,2)
         g21i=gmtrx(i,2,3)*gmtrx(i,3,1)-gmtrx(i,2,1)*gmtrx(i,3,3)
         g22i=gmtrx(i,3,3)*gmtrx(i,1,1)-gmtrx(i,3,1)*gmtrx(i,1,3)
         g23i=gmtrx(i,1,3)*gmtrx(i,2,1)-gmtrx(i,1,1)*gmtrx(i,2,3)
         g31i=gmtrx(i,2,1)*gmtrx(i,3,2)-gmtrx(i,2,2)*gmtrx(i,3,1)
         g32i=gmtrx(i,3,1)*gmtrx(i,1,2)-gmtrx(i,3,2)*gmtrx(i,1,1)
         g33i=gmtrx(i,1,1)*gmtrx(i,2,2)-gmtrx(i,1,2)*gmtrx(i,2,1)
         gmtrx(i,1,1)=gjac*g11i
         gmtrx(i,2,1)=gjac*g21i
         gmtrx(i,3,1)=gjac*g31i
         gmtrx(i,1,2)=gjac*g12i
         gmtrx(i,2,2)=gjac*g22i
         gmtrx(i,3,2)=gjac*g32i
         gmtrx(i,1,3)=gjac*g13i
         gmtrx(i,2,3)=gjac*g23i
         gmtrx(i,3,3)=gjac*g33i
c
      enddo
c
      endif
c
      do i=lft,llt
c
         vlm(i)=hmtrx(i,1,1)*(hmtrx(i,2,2)*hmtrx(i,3,3)-
     1        hmtrx(i,3,2)*hmtrx(i,2,3))+
     2        hmtrx(i,2,1)*(hmtrx(i,3,2)*hmtrx(i,1,3)-
     3        hmtrx(i,1,2)*hmtrx(i,3,3))+
     4        hmtrx(i,3,1)*(hmtrx(i,1,2)*hmtrx(i,2,3)-
     5        hmtrx(i,2,2)*hmtrx(i,1,3))
         cvltot(i)=cvltot(i)+vlm(i)*wgt
         hjac=1./vlm(i)
         h11i=hmtrx(i,2,2)*hmtrx(i,3,3)-hmtrx(i,2,3)*hmtrx(i,3,2)
         h12i=hmtrx(i,3,2)*hmtrx(i,1,3)-hmtrx(i,3,3)*hmtrx(i,1,2)
         h13i=hmtrx(i,1,2)*hmtrx(i,2,3)-hmtrx(i,1,3)*hmtrx(i,2,2)
         h21i=hmtrx(i,2,3)*hmtrx(i,3,1)-hmtrx(i,2,1)*hmtrx(i,3,3)
         h22i=hmtrx(i,3,3)*hmtrx(i,1,1)-hmtrx(i,3,1)*hmtrx(i,1,3)
         h23i=hmtrx(i,1,3)*hmtrx(i,2,1)-hmtrx(i,1,1)*hmtrx(i,2,3)
         h31i=hmtrx(i,2,1)*hmtrx(i,3,2)-hmtrx(i,2,2)*hmtrx(i,3,1)
         h32i=hmtrx(i,3,1)*hmtrx(i,1,2)-hmtrx(i,3,2)*hmtrx(i,1,1)
         h33i=hmtrx(i,1,1)*hmtrx(i,2,2)-hmtrx(i,1,2)*hmtrx(i,2,1)
         hmtrx(i,1,1)=hjac*h11i
         hmtrx(i,2,1)=hjac*h21i
         hmtrx(i,3,1)=hjac*h31i
         hmtrx(i,1,2)=hjac*h12i
         hmtrx(i,2,2)=hjac*h22i
         hmtrx(i,3,2)=hjac*h32i
         hmtrx(i,1,3)=hjac*h13i
         hmtrx(i,2,3)=hjac*h23i
         hmtrx(i,3,3)=hjac*h33i
c
      enddo
c
      if (lenvec8.eq.0) then
         do i=lft,llt
            voltot(i)=cvltot(i)
         enddo
         do k=1,3
            do j=1,3
               do i=lft,llt
                  gmtrx(i,j,k)=hmtrx(i,j,k)
               enddo
            enddo
         enddo
      endif
c
c     Compute "real" b-matrix
c
      if (lenvec8.ne.0) then
c
      ndtot=8*(3+nxdof)
      do l=1,ndtot
         do j=1,3
            do i=lft,llt
c
               b1=bmtrx(i,j,1,l)*gmtrx(i,1,1)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,1)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,1)
               b2=bmtrx(i,j,1,l)*gmtrx(i,1,2)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,2)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,2)
               b3=bmtrx(i,j,1,l)*gmtrx(i,1,3)+
     1              bmtrx(i,j,2,l)*gmtrx(i,2,3)+
     2              bmtrx(i,j,3,l)*gmtrx(i,3,3)
               bmtrx(i,j,1,l)=b1
               bmtrx(i,j,2,l)=b2
               bmtrx(i,j,3,l)=b3
c
            enddo
         enddo
      enddo
c
      endif
c
c     Compute "real" c-matrix
c
      ndtot=8*(3+nxdof)
      do l=1,ndtot
         do j=1,3
            do i=lft,llt
c
               c1=cmtrx(i,j,1,l)*hmtrx(i,1,1)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,1)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,1)
               c2=cmtrx(i,j,1,l)*hmtrx(i,1,2)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,2)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,2)
               c3=cmtrx(i,j,1,l)*hmtrx(i,1,3)+
     1              cmtrx(i,j,2,l)*hmtrx(i,2,3)+
     2              cmtrx(i,j,3,l)*hmtrx(i,3,3)
               cmtrx(i,j,1,l)=c1
               cmtrx(i,j,2,l)=c2
               cmtrx(i,j,3,l)=c3
c
            enddo
         enddo
      enddo
c
      if (lenvec8.eq.0) then
      do l=1,8*(3+NXDOFUE)
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=cmtrx(i,j,k,l)
               enddo
            enddo
         enddo
      enddo
      endif
c
      return
      end
      subroutine usrsld_str(nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute strain increments and spin
c     increments for user defined solid
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux2loc/dstrn(nlq,6),wdt(nlq,3),einc(nlq)
      common/aux10loc/
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq),
     5 dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq),
     6 dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq),
     7 dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq),
     8 dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq),
     9 dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      common/subtssloc/dt1siz(nlq)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/subtssloc/)
      ndtot=8*(3+nxdof)
c
      do j=1,6
c
         if (j.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=bmtrx(i,j,j,k)
               enddo
            enddo
         else
            j1=2
            j2=1
            if (j.eq.5) then
               j1=3
               j2=2
            elseif (j.eq.6) then
               j1=1
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=bmtrx(i,j1,j2,k)+bmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
         do i=lft,llt
c
            dstrn(i,j)=
     1           bvec(i,1)*dx1(i)+
     2           bvec(i,2)*dy1(i)+
     3           bvec(i,3)*dz1(i)+
     4           bvec(i,4+nxdof)*dx2(i)+
     5           bvec(i,5+nxdof)*dy2(i)+
     6           bvec(i,6+nxdof)*dz2(i)+
     7           bvec(i,7+2*nxdof)*dx3(i)+
     8           bvec(i,8+2*nxdof)*dy3(i)+
     9           bvec(i,9+2*nxdof)*dz3(i)+
     .           bvec(i,10+3*nxdof)*dx4(i)+
     1           bvec(i,11+3*nxdof)*dy4(i)+
     2           bvec(i,12+3*nxdof)*dz4(i)+
     3           bvec(i,13+4*nxdof)*dx5(i)+
     4           bvec(i,14+4*nxdof)*dy5(i)+
     5           bvec(i,15+4*nxdof)*dz5(i)+
     6           bvec(i,16+5*nxdof)*dx6(i)+
     7           bvec(i,17+5*nxdof)*dy6(i)+
     8           bvec(i,18+5*nxdof)*dz6(i)+
     9           bvec(i,19+6*nxdof)*dx7(i)+
     .           bvec(i,20+6*nxdof)*dy7(i)+
     1           bvec(i,21+6*nxdof)*dz7(i)+
     2           bvec(i,22+7*nxdof)*dx8(i)+
     3           bvec(i,23+7*nxdof)*dy8(i)+
     4           bvec(i,24+7*nxdof)*dz8(i)
c
         enddo
c
         if (nxdof.gt.0) then
            do k=1,nxdof
               k1=3+k
               k2=6+nxdof+k
               k3=9+2*nxdof+k
               k4=12+3*nxdof+k
               k5=15+4*nxdof+k
               k6=18+5*nxdof+k
               k7=21+6*nxdof+k
               k8=24+7*nxdof+k
               do i=lft,llt
c
                  dstrn(i,j)=dstrn(i,j)+
     1                 bvec(i,k1)*dxdof(i,1,k)+
     2                 bvec(i,k2)*dxdof(i,2,k)+
     3                 bvec(i,k3)*dxdof(i,3,k)+
     4                 bvec(i,k4)*dxdof(i,4,k)+
     5                 bvec(i,k5)*dxdof(i,5,k)+
     6                 bvec(i,k6)*dxdof(i,6,k)+
     7                 bvec(i,k7)*dxdof(i,7,k)+
     8                 bvec(i,k8)*dxdof(i,8,k)
c
               enddo
            enddo
         endif
c
         if (j.gt.3) then
c
            jj=1
            if (j.eq.5) jj=3
            if (j.eq.6) jj=2
c
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=.5*(bmtrx(i,j1,j2,k)-bmtrx(i,j2,j1,k))*
     1                 dt1siz(i)
               enddo
            enddo
c
            do i=lft,llt
c
               wdt(i,jj)=
     1           bvec(i,1)*dx1(i)+
     2           bvec(i,2)*dy1(i)+
     3           bvec(i,3)*dz1(i)+
     4           bvec(i,4+nxdof)*dx2(i)+
     5           bvec(i,5+nxdof)*dy2(i)+
     6           bvec(i,6+nxdof)*dz2(i)+
     7           bvec(i,7+2*nxdof)*dx3(i)+
     8           bvec(i,8+2*nxdof)*dy3(i)+
     9           bvec(i,9+2*nxdof)*dz3(i)+
     .           bvec(i,10+3*nxdof)*dx4(i)+
     1           bvec(i,11+3*nxdof)*dy4(i)+
     2           bvec(i,12+3*nxdof)*dz4(i)+
     3           bvec(i,13+4*nxdof)*dx5(i)+
     4           bvec(i,14+4*nxdof)*dy5(i)+
     5           bvec(i,15+4*nxdof)*dz5(i)+
     6           bvec(i,16+5*nxdof)*dx6(i)+
     7           bvec(i,17+5*nxdof)*dy6(i)+
     8           bvec(i,18+5*nxdof)*dz6(i)+
     9           bvec(i,19+6*nxdof)*dx7(i)+
     .           bvec(i,20+6*nxdof)*dy7(i)+
     1           bvec(i,21+6*nxdof)*dz7(i)+
     2           bvec(i,22+7*nxdof)*dx8(i)+
     3           bvec(i,23+7*nxdof)*dy8(i)+
     4           bvec(i,24+7*nxdof)*dz8(i)
c
            enddo
c
            if (nxdof.gt.0) then
               do k=1,nxdof
                  k1=3+k
                  k2=6+nxdof+k
                  k3=9+2*nxdof+k
                  k4=12+3*nxdof+k
                  k5=15+4*nxdof+k
                  k6=18+5*nxdof+k
                  k7=21+6*nxdof+k
                  k8=24+7*nxdof+k
                  do i=lft,llt
c
                     wdt(i,jj)=wdt(i,jj)+
     1                 bvec(i,k1)*dxdof(i,1,k)+
     2                 bvec(i,k2)*dxdof(i,2,k)+
     3                 bvec(i,k3)*dxdof(i,3,k)+
     4                 bvec(i,k4)*dxdof(i,4,k)+
     5                 bvec(i,k5)*dxdof(i,5,k)+
     6                 bvec(i,k6)*dxdof(i,6,k)+
     7                 bvec(i,k7)*dxdof(i,7,k)+
     8                 bvec(i,k8)*dxdof(i,8,k)
c
                  enddo
               enddo
            endif
c
         endif
c
      enddo
c
      return
      end
      subroutine usrsld_grd(x,nxdof,ietyp,mte,xi,eta,zeta,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute deformation gradient
c
      include 'nlqparm'
      include 'nhisparm.inc'
      include 'umatss.inc'
      common/aux2loc/dstrn(nlq,6)
      common/aux33loc/ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ix5(nlq),
     1 ix6(nlq),ix7(nlq),ix8(nlq),mxt(nlq)
      common/aux18loc/dd(nlq),dfe(nlq),ddq(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c     common/comrbbr/ishrub(300),icompf(300),mhyper(300)
      common/vect32loc/g11(nlq),g21(nlq),g31(nlq),g12(nlq),g22(nlq),
     . g32(nlq),g13(nlq),g23(nlq),g33(nlq),ifg(nlq)
c
      real*8 x
      dimension x(3,1)
      dimension xx(nlq,8*(3+NXDOFUE))
c
c$omp threadprivate (/aux18loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/vect32loc/)
      do j=1,3
         do i=lft,llt
            xx(i,j)=x(j,ix1(i))
            xx(i,3+nxdof+j)=x(j,ix2(i))
            xx(i,6+2*nxdof+j)=x(j,ix3(i))
            xx(i,9+3*nxdof+j)=x(j,ix4(i))
            xx(i,12+4*nxdof+j)=x(j,ix5(i))
            xx(i,15+5*nxdof+j)=x(j,ix6(i))
            xx(i,18+6*nxdof+j)=x(j,ix7(i))
            xx(i,21+7*nxdof+j)=x(j,ix8(i))
         enddo
      enddo
      if (nxdof.gt.0) then
         do k=1,8
            do j=1,nxdof
               indx=3+(3+nxdof)*(k-1)+j
               nsnd=(j-1)/3+1
               nsdf=j-(nsnd-1)*3
               do i=lft,llt
                  xx(i,indx)=x(nsdf,ixsld(i,k,nsnd))
               enddo
            enddo
         enddo
      endif
c
      do i=lft,llt
         dd(i)=-dstrn(i,1)-dstrn(i,2)-dstrn(i,3)
         ddq(i)=dd(i)
         g11(i)=0.
         g21(i)=0.
         g31(i)=0.
         g12(i)=0.
         g22(i)=0.
         g32(i)=0.
         g13(i)=0.
         g23(i)=0.
         g33(i)=0.
      enddo
c
      ndtot=8*(3+nxdof)
c
      do j=1,ndtot
         do i=lft,llt
            g11(i)=g11(i)+cmtrx(i,1,1,j)*xx(i,j)
            g21(i)=g21(i)+cmtrx(i,2,1,j)*xx(i,j)
            g31(i)=g31(i)+cmtrx(i,3,1,j)*xx(i,j)
            g12(i)=g12(i)+cmtrx(i,1,2,j)*xx(i,j)
            g22(i)=g22(i)+cmtrx(i,2,2,j)*xx(i,j)
            g32(i)=g32(i)+cmtrx(i,3,2,j)*xx(i,j)
            g13(i)=g13(i)+cmtrx(i,1,3,j)*xx(i,j)
            g23(i)=g23(i)+cmtrx(i,2,3,j)*xx(i,j)
            g33(i)=g33(i)+cmtrx(i,3,3,j)*xx(i,j)
         enddo
      enddo
c
      do i=lft,llt
c
         dfe(i)=1./(g11(i)*(g22(i)*g33(i)-
     1        g32(i)*g23(i))+
     2        g21(i)*(g32(i)*g13(i)-
     3        g12(i)*g33(i))+
     4        g31(i)*(g12(i)*g23(i)-
     5        g22(i)*g13(i)))
         g11i=g22(i)*g33(i)-g23(i)*g32(i)
         g12i=g32(i)*g13(i)-g33(i)*g12(i)
         g13i=g12(i)*g23(i)-g13(i)*g22(i)
         g21i=g23(i)*g31(i)-g21(i)*g33(i)
         g22i=g33(i)*g11(i)-g31(i)*g13(i)
         g23i=g13(i)*g21(i)-g11(i)*g23(i)
         g31i=g21(i)*g32(i)-g22(i)*g31(i)
         g32i=g31(i)*g12(i)-g32(i)*g11(i)
         g33i=g11(i)*g22(i)-g12(i)*g21(i)
         g11(i)=dfe(i)*g11i
         g21(i)=dfe(i)*g21i
         g31(i)=dfe(i)*g31i
         g12(i)=dfe(i)*g12i
         g22(i)=dfe(i)*g22i
         g32(i)=dfe(i)*g32i
         g13(i)=dfe(i)*g13i
         g23(i)=dfe(i)*g23i
         g33(i)=dfe(i)*g33i
      enddo
c
      if (icompf(mte).eq.mte) ifg(lft)=1
c
      return
      end
      subroutine usrsld_rstr(strain,lft,llt,ipt,nip,wgt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     rotate strains for output
c
      include 'nlqparm'
      common/strsavloc/wzza(nlq),wyya(nlq),wxxa(nlq),d1av(nlq),
     . d2av(nlq),d3av(nlq),d4av(nlq),d5av(nlq),d6av(nlq)
      common/aux2loc/d1(nlq),d2(nlq),d3(nlq),d4(nlq),d5(nlq),d6(nlq),
     1 wzz(nlq),wyy(nlq),wxx(nlq),ener(nlq)
      common/numcpuloc/lenvec8,lenvech
      common/subtssloc/dt1siz(nlq)
      dimension strain(6,*)
c
c$omp threadprivate (/aux2loc/)
c$omp threadprivate (/numcpuloc/)
c$omp threadprivate (/subtssloc/)
c$omp threadprivate (/strsavloc/)
      if (ipt.eq.1) then
      do 10 i=lft,llt
      wzza(i)=wgt*wzz(i)
      wyya(i)=wgt*wyy(i)
      wxxa(i)=wgt*wxx(i)
      d1av(i)=wgt*d1(i)
      d2av(i)=wgt*d2(i)
      d3av(i)=wgt*d3(i)
      d4av(i)=wgt*d4(i)
      d5av(i)=wgt*d5(i)
      d6av(i)=wgt*d6(i)
   10 continue
      else
      do 20 i=lft,llt
      wzza(i)=wzza(i)+wgt*wzz(i)
      wyya(i)=wyya(i)+wgt*wyy(i)
      wxxa(i)=wxxa(i)+wgt*wxx(i)
      d1av(i)=d1av(i)+wgt*d1(i)
      d2av(i)=d2av(i)+wgt*d2(i)
      d3av(i)=d3av(i)+wgt*d3(i)
      d4av(i)=d4av(i)+wgt*d4(i)
      d5av(i)=d5av(i)+wgt*d5(i)
      d6av(i)=d6av(i)+wgt*d6(i)
   20 continue
      endif
      if (ipt.ne.nip) return
      fact=.125
      if (lenvech.eq.1) fact=0.250
      do 30 i=lft,llt
      wzzi=fact*wzza(i)
      wyyi=fact*wyya(i)
      wxxi=fact*wxxa(i)
      d1i =.125*d1av(i)
      d2i =.125*d2av(i)
      d3i =.125*d3av(i)
      d4i =.125*d4av(i)
      d5i =.125*d5av(i)
      d6i =.125*d6av(i)
      s1=strain(1,i)
      s2=strain(2,i)
      s3=strain(3,i)
      s4=strain(4,i)
      s5=strain(5,i)
      s6=strain(6,i)
      q1=2.*s4*wzzi
      q2=2.*s6*wyyi
      q3=2.*s5*wxxi
      dt1s=.5*dt1siz(i)
      strain(1,i)=s1-q1+q2+dt1siz(i)*d1i
      strain(2,i)=s2+q1-q3+dt1siz(i)*d2i
      strain(3,i)=s3-q2+q3+dt1siz(i)*d3i
      strain(4,i)=s4+wzzi*(s1-s2)+wyyi*s5-wxxi*s6+dt1s*d4i
      strain(5,i)=s5+wxxi*(s2-s3)+wzzi*s6-wyyi*s4+dt1s*d5i
   30 strain(6,i)=s6+wyyi*(s3-s1)+wxxi*s4-wzzi*s5+dt1s*d6i
      return
      end
      subroutine usld_b101(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,n5,n6,n7,n8,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5 dn1deta,dn2deta,dn3deta,dn4deta,
     6 dn5deta,dn6deta,dn7deta,dn8deta,
     7 dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8 dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9 x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined solid 101
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4,n5,n6,n7,n8
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4,n5,n6,n7,n8         = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi,
c     dn5dxi,dn6dxi,dn7dxi,dn8dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta,
c     dn5deta,dn6deta,dn7deta,dn8deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     dn1dzeta,dn2dzeta,
c     dn3dzeta,dn4dzeta,
c     dn5dzeta,dn6dzeta,
c     dn7dzeta,dn8dzeta               = nodal shape function derivatives
c                                       w.r.t. third parental coordinate
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c
      do i=lft,llt
c
         bmtrx(i,1,1,1) =dn1dxi
         bmtrx(i,1,1,4) =dn2dxi
         bmtrx(i,1,1,7) =dn3dxi
         bmtrx(i,1,1,10)=dn4dxi
         bmtrx(i,1,1,13)=dn5dxi
         bmtrx(i,1,1,16)=dn6dxi
         bmtrx(i,1,1,19)=dn7dxi
         bmtrx(i,1,1,22)=dn8dxi
c
         bmtrx(i,2,1,2) =dn1dxi
         bmtrx(i,2,1,5) =dn2dxi
         bmtrx(i,2,1,8) =dn3dxi
         bmtrx(i,2,1,11)=dn4dxi
         bmtrx(i,2,1,14)=dn5dxi
         bmtrx(i,2,1,17)=dn6dxi
         bmtrx(i,2,1,20)=dn7dxi
         bmtrx(i,2,1,23)=dn8dxi
c
         bmtrx(i,3,1,3) =dn1dxi
         bmtrx(i,3,1,6) =dn2dxi
         bmtrx(i,3,1,9) =dn3dxi
         bmtrx(i,3,1,12)=dn4dxi
         bmtrx(i,3,1,15)=dn5dxi
         bmtrx(i,3,1,18)=dn6dxi
         bmtrx(i,3,1,21)=dn7dxi
         bmtrx(i,3,1,24)=dn8dxi
c
         bmtrx(i,1,2,1) =dn1deta
         bmtrx(i,1,2,4) =dn2deta
         bmtrx(i,1,2,7) =dn3deta
         bmtrx(i,1,2,10)=dn4deta
         bmtrx(i,1,2,13)=dn5deta
         bmtrx(i,1,2,16)=dn6deta
         bmtrx(i,1,2,19)=dn7deta
         bmtrx(i,1,2,22)=dn8deta
c
         bmtrx(i,2,2,2) =dn1deta
         bmtrx(i,2,2,5) =dn2deta
         bmtrx(i,2,2,8) =dn3deta
         bmtrx(i,2,2,11)=dn4deta
         bmtrx(i,2,2,14)=dn5deta
         bmtrx(i,2,2,17)=dn6deta
         bmtrx(i,2,2,20)=dn7deta
         bmtrx(i,2,2,23)=dn8deta
c
         bmtrx(i,3,2,3) =dn1deta
         bmtrx(i,3,2,6) =dn2deta
         bmtrx(i,3,2,9) =dn3deta
         bmtrx(i,3,2,12)=dn4deta
         bmtrx(i,3,2,15)=dn5deta
         bmtrx(i,3,2,18)=dn6deta
         bmtrx(i,3,2,21)=dn7deta
         bmtrx(i,3,2,24)=dn8deta
c
         bmtrx(i,1,3,1) =dn1dzeta
         bmtrx(i,1,3,4) =dn2dzeta
         bmtrx(i,1,3,7) =dn3dzeta
         bmtrx(i,1,3,10)=dn4dzeta
         bmtrx(i,1,3,13)=dn5dzeta
         bmtrx(i,1,3,16)=dn6dzeta
         bmtrx(i,1,3,19)=dn7dzeta
         bmtrx(i,1,3,22)=dn8dzeta
c
         bmtrx(i,2,3,2) =dn1dzeta
         bmtrx(i,2,3,5) =dn2dzeta
         bmtrx(i,2,3,8) =dn3dzeta
         bmtrx(i,2,3,11)=dn4dzeta
         bmtrx(i,2,3,14)=dn5dzeta
         bmtrx(i,2,3,17)=dn6dzeta
         bmtrx(i,2,3,20)=dn7dzeta
         bmtrx(i,2,3,23)=dn8dzeta
c
         bmtrx(i,3,3,3) =dn1dzeta
         bmtrx(i,3,3,6) =dn2dzeta
         bmtrx(i,3,3,9) =dn3dzeta
         bmtrx(i,3,3,12)=dn4dzeta
         bmtrx(i,3,3,15)=dn5dzeta
         bmtrx(i,3,3,18)=dn6dzeta
         bmtrx(i,3,3,21)=dn7dzeta
         bmtrx(i,3,3,24)=dn8dzeta
c
         gmtrx(i,1,1)=x1(i)*dn1dxi+x2(i)*dn2dxi+
     1        x3(i)*dn3dxi+x4(i)*dn4dxi+
     2        x5(i)*dn5dxi+x6(i)*dn6dxi+
     3        x7(i)*dn7dxi+x8(i)*dn8dxi
         gmtrx(i,2,1)=y1(i)*dn1dxi+y2(i)*dn2dxi+
     1        y3(i)*dn3dxi+y4(i)*dn4dxi+
     2        y5(i)*dn5dxi+y6(i)*dn6dxi+
     3        y7(i)*dn7dxi+y8(i)*dn8dxi
         gmtrx(i,3,1)=z1(i)*dn1dxi+z2(i)*dn2dxi+
     1        z3(i)*dn3dxi+z4(i)*dn4dxi+
     2        z5(i)*dn5dxi+z6(i)*dn6dxi+
     3        z7(i)*dn7dxi+z8(i)*dn8dxi
         gmtrx(i,1,2)=x1(i)*dn1deta+x2(i)*dn2deta+
     1        x3(i)*dn3deta+x4(i)*dn4deta+
     2        x5(i)*dn5deta+x6(i)*dn6deta+
     3        x7(i)*dn7deta+x8(i)*dn8deta
         gmtrx(i,2,2)=y1(i)*dn1deta+y2(i)*dn2deta+
     1        y3(i)*dn3deta+y4(i)*dn4deta+
     2        y5(i)*dn5deta+y6(i)*dn6deta+
     3        y7(i)*dn7deta+y8(i)*dn8deta
         gmtrx(i,3,2)=z1(i)*dn1deta+z2(i)*dn2deta+
     1        z3(i)*dn3deta+z4(i)*dn4deta+
     2        z5(i)*dn5deta+z6(i)*dn6deta+
     3        z7(i)*dn7deta+z8(i)*dn8deta
         gmtrx(i,1,3)=x1(i)*dn1dzeta+x2(i)*dn2dzeta+
     1        x3(i)*dn3dzeta+x4(i)*dn4dzeta+
     2        x5(i)*dn5dzeta+x6(i)*dn6dzeta+
     3        x7(i)*dn7dzeta+x8(i)*dn8dzeta
         gmtrx(i,2,3)=y1(i)*dn1dzeta+y2(i)*dn2dzeta+
     1        y3(i)*dn3dzeta+y4(i)*dn4dzeta+
     2        y5(i)*dn5dzeta+y6(i)*dn6dzeta+
     3        y7(i)*dn7dzeta+y8(i)*dn8dzeta
         gmtrx(i,3,3)=z1(i)*dn1dzeta+z2(i)*dn2dzeta+
     1        z3(i)*dn3dzeta+z4(i)*dn4dzeta+
     2        z5(i)*dn5dzeta+z6(i)*dn6dzeta+
     3        z7(i)*dn7dzeta+z8(i)*dn8dzeta
c
      enddo
c
      return
      end
      subroutine usld_b102(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,n5,n6,n7,n8,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5 dn1deta,dn2deta,dn3deta,dn4deta,
     6 dn5deta,dn6deta,dn7deta,dn8deta,
     7 dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8 dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9 x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined solid 102
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4,n5,n6,n7,n8
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4,n5,n6,n7,n8         = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi,
c     dn5dxi,dn6dxi,dn7dxi,dn8dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta,
c     dn5deta,dn6deta,dn7deta,dn8deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     dn1dzeta,dn2dzeta,
c     dn3dzeta,dn4dzeta,
c     dn5dzeta,dn6dzeta,
c     dn7dzeta,dn8dzeta               = nodal shape function derivatives
c                                       w.r.t. third parental coordinate
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c
      dimension factor(nlq),gitrx(nlq,3,3),dvec(nlq,24)
c
      dm1dxi =-.125
      dm2dxi = .125
      dm3dxi = .125
      dm4dxi =-.125
      dm5dxi =-.125
      dm6dxi = .125
      dm7dxi = .125
      dm8dxi =-.125
c
      dm1deta=-.125
      dm2deta=-.125
      dm3deta= .125
      dm4deta= .125
      dm5deta=-.125
      dm6deta=-.125
      dm7deta= .125
      dm8deta= .125
c
      dm1dzeta=-.125
      dm2dzeta=-.125
      dm3dzeta=-.125
      dm4dzeta=-.125
      dm5dzeta= .125
      dm6dzeta= .125
      dm7dzeta= .125
      dm8dzeta= .125
c
      do i=lft,llt
c
         bmtrx(i,1,1,1) =dm1dxi
         bmtrx(i,1,1,4) =dm2dxi
         bmtrx(i,1,1,7) =dm3dxi
         bmtrx(i,1,1,10)=dm4dxi
         bmtrx(i,1,1,13)=dm5dxi
         bmtrx(i,1,1,16)=dm6dxi
         bmtrx(i,1,1,19)=dm7dxi
         bmtrx(i,1,1,22)=dm8dxi
c
         bmtrx(i,2,1,2) =dm1dxi
         bmtrx(i,2,1,5) =dm2dxi
         bmtrx(i,2,1,8) =dm3dxi
         bmtrx(i,2,1,11)=dm4dxi
         bmtrx(i,2,1,14)=dm5dxi
         bmtrx(i,2,1,17)=dm6dxi
         bmtrx(i,2,1,20)=dm7dxi
         bmtrx(i,2,1,23)=dm8dxi
c
         bmtrx(i,3,1,3) =dm1dxi
         bmtrx(i,3,1,6) =dm2dxi
         bmtrx(i,3,1,9) =dm3dxi
         bmtrx(i,3,1,12)=dm4dxi
         bmtrx(i,3,1,15)=dm5dxi
         bmtrx(i,3,1,18)=dm6dxi
         bmtrx(i,3,1,21)=dm7dxi
         bmtrx(i,3,1,24)=dm8dxi
c
         bmtrx(i,1,2,1) =dm1deta
         bmtrx(i,1,2,4) =dm2deta
         bmtrx(i,1,2,7) =dm3deta
         bmtrx(i,1,2,10)=dm4deta
         bmtrx(i,1,2,13)=dm5deta
         bmtrx(i,1,2,16)=dm6deta
         bmtrx(i,1,2,19)=dm7deta
         bmtrx(i,1,2,22)=dm8deta
c
         bmtrx(i,2,2,2) =dm1deta
         bmtrx(i,2,2,5) =dm2deta
         bmtrx(i,2,2,8) =dm3deta
         bmtrx(i,2,2,11)=dm4deta
         bmtrx(i,2,2,14)=dm5deta
         bmtrx(i,2,2,17)=dm6deta
         bmtrx(i,2,2,20)=dm7deta
         bmtrx(i,2,2,23)=dm8deta
c
         bmtrx(i,3,2,3) =dm1deta
         bmtrx(i,3,2,6) =dm2deta
         bmtrx(i,3,2,9) =dm3deta
         bmtrx(i,3,2,12)=dm4deta
         bmtrx(i,3,2,15)=dm5deta
         bmtrx(i,3,2,18)=dm6deta
         bmtrx(i,3,2,21)=dm7deta
         bmtrx(i,3,2,24)=dm8deta
c
         bmtrx(i,1,3,1) =dm1dzeta
         bmtrx(i,1,3,4) =dm2dzeta
         bmtrx(i,1,3,7) =dm3dzeta
         bmtrx(i,1,3,10)=dm4dzeta
         bmtrx(i,1,3,13)=dm5dzeta
         bmtrx(i,1,3,16)=dm6dzeta
         bmtrx(i,1,3,19)=dm7dzeta
         bmtrx(i,1,3,22)=dm8dzeta
c
         bmtrx(i,2,3,2) =dm1dzeta
         bmtrx(i,2,3,5) =dm2dzeta
         bmtrx(i,2,3,8) =dm3dzeta
         bmtrx(i,2,3,11)=dm4dzeta
         bmtrx(i,2,3,14)=dm5dzeta
         bmtrx(i,2,3,17)=dm6dzeta
         bmtrx(i,2,3,20)=dm7dzeta
         bmtrx(i,2,3,23)=dm8dzeta
c
         bmtrx(i,3,3,3) =dm1dzeta
         bmtrx(i,3,3,6) =dm2dzeta
         bmtrx(i,3,3,9) =dm3dzeta
         bmtrx(i,3,3,12)=dm4dzeta
         bmtrx(i,3,3,15)=dm5dzeta
         bmtrx(i,3,3,18)=dm6dzeta
         bmtrx(i,3,3,21)=dm7dzeta
         bmtrx(i,3,3,24)=dm8dzeta
c
      enddo
c
      do i=lft,llt
c
         gmtrx(i,1,1)=x1(i)*dm1dxi+x2(i)*dm2dxi+
     1        x3(i)*dm3dxi+x4(i)*dm4dxi+
     2        x5(i)*dm5dxi+x6(i)*dm6dxi+
     3        x7(i)*dm7dxi+x8(i)*dm8dxi
         gmtrx(i,2,1)=y1(i)*dm1dxi+y2(i)*dm2dxi+
     1        y3(i)*dm3dxi+y4(i)*dm4dxi+
     2        y5(i)*dm5dxi+y6(i)*dm6dxi+
     3        y7(i)*dm7dxi+y8(i)*dm8dxi
         gmtrx(i,3,1)=z1(i)*dm1dxi+z2(i)*dm2dxi+
     1        z3(i)*dm3dxi+z4(i)*dm4dxi+
     2        z5(i)*dm5dxi+z6(i)*dm6dxi+
     3        z7(i)*dm7dxi+z8(i)*dm8dxi
         gmtrx(i,1,2)=x1(i)*dm1deta+x2(i)*dm2deta+
     1        x3(i)*dm3deta+x4(i)*dm4deta+
     2        x5(i)*dm5deta+x6(i)*dm6deta+
     3        x7(i)*dm7deta+x8(i)*dm8deta
         gmtrx(i,2,2)=y1(i)*dm1deta+y2(i)*dm2deta+
     1        y3(i)*dm3deta+y4(i)*dm4deta+
     2        y5(i)*dm5deta+y6(i)*dm6deta+
     3        y7(i)*dm7deta+y8(i)*dm8deta
         gmtrx(i,3,2)=z1(i)*dm1deta+z2(i)*dm2deta+
     1        z3(i)*dm3deta+z4(i)*dm4deta+
     2        z5(i)*dm5deta+z6(i)*dm6deta+
     3        z7(i)*dm7deta+z8(i)*dm8deta
         gmtrx(i,1,3)=x1(i)*dm1dzeta+x2(i)*dm2dzeta+
     1        x3(i)*dm3dzeta+x4(i)*dm4dzeta+
     2        x5(i)*dm5dzeta+x6(i)*dm6dzeta+
     3        x7(i)*dm7dzeta+x8(i)*dm8dzeta
         gmtrx(i,2,3)=y1(i)*dm1dzeta+y2(i)*dm2dzeta+
     1        y3(i)*dm3dzeta+y4(i)*dm4dzeta+
     2        y5(i)*dm5dzeta+y6(i)*dm6dzeta+
     3        y7(i)*dm7dzeta+y8(i)*dm8dzeta
         gmtrx(i,3,3)=z1(i)*dm1dzeta+z2(i)*dm2dzeta+
     1        z3(i)*dm3dzeta+z4(i)*dm4dzeta+
     2        z5(i)*dm5dzeta+z6(i)*dm6dzeta+
     3        z7(i)*dm7dzeta+z8(i)*dm8dzeta
c
      enddo
c
      do i=lft,llt
c
         factor(i)=gmtrx(i,1,1)*(gmtrx(i,2,2)*gmtrx(i,3,3)-
     1        gmtrx(i,3,2)*gmtrx(i,2,3))+
     2        gmtrx(i,2,1)*(gmtrx(i,3,2)*gmtrx(i,1,3)-
     3        gmtrx(i,1,2)*gmtrx(i,3,3))+
     4        gmtrx(i,3,1)*(gmtrx(i,1,2)*gmtrx(i,2,3)-
     5        gmtrx(i,2,2)*gmtrx(i,1,3))
c
         gjc=1./factor(i)
         g11i=gmtrx(i,2,2)*gmtrx(i,3,3)-gmtrx(i,2,3)*gmtrx(i,3,2)
         g12i=gmtrx(i,3,2)*gmtrx(i,1,3)-gmtrx(i,3,3)*gmtrx(i,1,2)
         g13i=gmtrx(i,1,2)*gmtrx(i,2,3)-gmtrx(i,1,3)*gmtrx(i,2,2)
         g21i=gmtrx(i,2,3)*gmtrx(i,3,1)-gmtrx(i,2,1)*gmtrx(i,3,3)
         g22i=gmtrx(i,3,3)*gmtrx(i,1,1)-gmtrx(i,3,1)*gmtrx(i,1,3)
         g23i=gmtrx(i,1,3)*gmtrx(i,2,1)-gmtrx(i,1,1)*gmtrx(i,2,3)
         g31i=gmtrx(i,2,1)*gmtrx(i,3,2)-gmtrx(i,2,2)*gmtrx(i,3,1)
         g32i=gmtrx(i,3,1)*gmtrx(i,1,2)-gmtrx(i,3,2)*gmtrx(i,1,1)
         g33i=gmtrx(i,1,1)*gmtrx(i,2,2)-gmtrx(i,1,2)*gmtrx(i,2,1)
         gitrx(i,1,1)=gjc*g11i
         gitrx(i,2,1)=gjc*g21i
         gitrx(i,3,1)=gjc*g31i
         gitrx(i,1,2)=gjc*g12i
         gitrx(i,2,2)=gjc*g22i
         gitrx(i,3,2)=gjc*g32i
         gitrx(i,1,3)=gjc*g13i
         gitrx(i,2,3)=gjc*g23i
         gitrx(i,3,3)=gjc*g33i
c
      enddo
c
      do j=1,24
         do i=lft,llt
            dvec(i,j)=0.
         enddo
      enddo
c
      do l=1,24
         do k=1,3
            do j=1,3
               do i=lft,llt
                  dvec(i,l)=dvec(i,l)+bmtrx(i,j,k,l)*gitrx(i,k,j)
               enddo
            enddo
         enddo
      enddo
c
      do i=lft,llt
c
         bmtrx(i,1,1,1) =dn1dxi
         bmtrx(i,1,1,4) =dn2dxi
         bmtrx(i,1,1,7) =dn3dxi
         bmtrx(i,1,1,10)=dn4dxi
         bmtrx(i,1,1,13)=dn5dxi
         bmtrx(i,1,1,16)=dn6dxi
         bmtrx(i,1,1,19)=dn7dxi
         bmtrx(i,1,1,22)=dn8dxi
c
         bmtrx(i,2,1,2) =dn1dxi
         bmtrx(i,2,1,5) =dn2dxi
         bmtrx(i,2,1,8) =dn3dxi
         bmtrx(i,2,1,11)=dn4dxi
         bmtrx(i,2,1,14)=dn5dxi
         bmtrx(i,2,1,17)=dn6dxi
         bmtrx(i,2,1,20)=dn7dxi
         bmtrx(i,2,1,23)=dn8dxi
c
         bmtrx(i,3,1,3) =dn1dxi
         bmtrx(i,3,1,6) =dn2dxi
         bmtrx(i,3,1,9) =dn3dxi
         bmtrx(i,3,1,12)=dn4dxi
         bmtrx(i,3,1,15)=dn5dxi
         bmtrx(i,3,1,18)=dn6dxi
         bmtrx(i,3,1,21)=dn7dxi
         bmtrx(i,3,1,24)=dn8dxi
c
         bmtrx(i,1,2,1) =dn1deta
         bmtrx(i,1,2,4) =dn2deta
         bmtrx(i,1,2,7) =dn3deta
         bmtrx(i,1,2,10)=dn4deta
         bmtrx(i,1,2,13)=dn5deta
         bmtrx(i,1,2,16)=dn6deta
         bmtrx(i,1,2,19)=dn7deta
         bmtrx(i,1,2,22)=dn8deta
c
         bmtrx(i,2,2,2) =dn1deta
         bmtrx(i,2,2,5) =dn2deta
         bmtrx(i,2,2,8) =dn3deta
         bmtrx(i,2,2,11)=dn4deta
         bmtrx(i,2,2,14)=dn5deta
         bmtrx(i,2,2,17)=dn6deta
         bmtrx(i,2,2,20)=dn7deta
         bmtrx(i,2,2,23)=dn8deta
c
         bmtrx(i,3,2,3) =dn1deta
         bmtrx(i,3,2,6) =dn2deta
         bmtrx(i,3,2,9) =dn3deta
         bmtrx(i,3,2,12)=dn4deta
         bmtrx(i,3,2,15)=dn5deta
         bmtrx(i,3,2,18)=dn6deta
         bmtrx(i,3,2,21)=dn7deta
         bmtrx(i,3,2,24)=dn8deta
c
         bmtrx(i,1,3,1) =dn1dzeta
         bmtrx(i,1,3,4) =dn2dzeta
         bmtrx(i,1,3,7) =dn3dzeta
         bmtrx(i,1,3,10)=dn4dzeta
         bmtrx(i,1,3,13)=dn5dzeta
         bmtrx(i,1,3,16)=dn6dzeta
         bmtrx(i,1,3,19)=dn7dzeta
         bmtrx(i,1,3,22)=dn8dzeta
c
         bmtrx(i,2,3,2) =dn1dzeta
         bmtrx(i,2,3,5) =dn2dzeta
         bmtrx(i,2,3,8) =dn3dzeta
         bmtrx(i,2,3,11)=dn4dzeta
         bmtrx(i,2,3,14)=dn5dzeta
         bmtrx(i,2,3,17)=dn6dzeta
         bmtrx(i,2,3,20)=dn7dzeta
         bmtrx(i,2,3,23)=dn8dzeta
c
         bmtrx(i,3,3,3) =dn1dzeta
         bmtrx(i,3,3,6) =dn2dzeta
         bmtrx(i,3,3,9) =dn3dzeta
         bmtrx(i,3,3,12)=dn4dzeta
         bmtrx(i,3,3,15)=dn5dzeta
         bmtrx(i,3,3,18)=dn6dzeta
         bmtrx(i,3,3,21)=dn7dzeta
         bmtrx(i,3,3,24)=dn8dzeta
c
      enddo
c
      do i=lft,llt
c
         gmtrx(i,1,1)=x1(i)*dn1dxi+x2(i)*dn2dxi+
     1        x3(i)*dn3dxi+x4(i)*dn4dxi+
     2        x5(i)*dn5dxi+x6(i)*dn6dxi+
     3        x7(i)*dn7dxi+x8(i)*dn8dxi
         gmtrx(i,2,1)=y1(i)*dn1dxi+y2(i)*dn2dxi+
     1        y3(i)*dn3dxi+y4(i)*dn4dxi+
     2        y5(i)*dn5dxi+y6(i)*dn6dxi+
     3        y7(i)*dn7dxi+y8(i)*dn8dxi
         gmtrx(i,3,1)=z1(i)*dn1dxi+z2(i)*dn2dxi+
     1        z3(i)*dn3dxi+z4(i)*dn4dxi+
     2        z5(i)*dn5dxi+z6(i)*dn6dxi+
     3        z7(i)*dn7dxi+z8(i)*dn8dxi
         gmtrx(i,1,2)=x1(i)*dn1deta+x2(i)*dn2deta+
     1        x3(i)*dn3deta+x4(i)*dn4deta+
     2        x5(i)*dn5deta+x6(i)*dn6deta+
     3        x7(i)*dn7deta+x8(i)*dn8deta
         gmtrx(i,2,2)=y1(i)*dn1deta+y2(i)*dn2deta+
     1        y3(i)*dn3deta+y4(i)*dn4deta+
     2        y5(i)*dn5deta+y6(i)*dn6deta+
     3        y7(i)*dn7deta+y8(i)*dn8deta
         gmtrx(i,3,2)=z1(i)*dn1deta+z2(i)*dn2deta+
     1        z3(i)*dn3deta+z4(i)*dn4deta+
     2        z5(i)*dn5deta+z6(i)*dn6deta+
     3        z7(i)*dn7deta+z8(i)*dn8deta
         gmtrx(i,1,3)=x1(i)*dn1dzeta+x2(i)*dn2dzeta+
     1        x3(i)*dn3dzeta+x4(i)*dn4dzeta+
     2        x5(i)*dn5dzeta+x6(i)*dn6dzeta+
     3        x7(i)*dn7dzeta+x8(i)*dn8dzeta
         gmtrx(i,2,3)=y1(i)*dn1dzeta+y2(i)*dn2dzeta+
     1        y3(i)*dn3dzeta+y4(i)*dn4dzeta+
     2        y5(i)*dn5dzeta+y6(i)*dn6dzeta+
     3        y7(i)*dn7dzeta+y8(i)*dn8dzeta
         gmtrx(i,3,3)=z1(i)*dn1dzeta+z2(i)*dn2dzeta+
     1        z3(i)*dn3dzeta+z4(i)*dn4dzeta+
     2        z5(i)*dn5dzeta+z6(i)*dn6dzeta+
     3        z7(i)*dn7dzeta+z8(i)*dn8dzeta
c
      enddo
c
      do i=lft,llt
c
         xjn=gmtrx(i,1,1)*(gmtrx(i,2,2)*gmtrx(i,3,3)-
     1        gmtrx(i,3,2)*gmtrx(i,2,3))+
     2        gmtrx(i,2,1)*(gmtrx(i,3,2)*gmtrx(i,1,3)-
     3        gmtrx(i,1,2)*gmtrx(i,3,3))+
     4        gmtrx(i,3,1)*(gmtrx(i,1,2)*gmtrx(i,2,3)-
     5        gmtrx(i,2,2)*gmtrx(i,1,3))
c
         gjc=1./xjn
         g11i=gmtrx(i,2,2)*gmtrx(i,3,3)-gmtrx(i,2,3)*gmtrx(i,3,2)
         g12i=gmtrx(i,3,2)*gmtrx(i,1,3)-gmtrx(i,3,3)*gmtrx(i,1,2)
         g13i=gmtrx(i,1,2)*gmtrx(i,2,3)-gmtrx(i,1,3)*gmtrx(i,2,2)
         g21i=gmtrx(i,2,3)*gmtrx(i,3,1)-gmtrx(i,2,1)*gmtrx(i,3,3)
         g22i=gmtrx(i,3,3)*gmtrx(i,1,1)-gmtrx(i,3,1)*gmtrx(i,1,3)
         g23i=gmtrx(i,1,3)*gmtrx(i,2,1)-gmtrx(i,1,1)*gmtrx(i,2,3)
         g31i=gmtrx(i,2,1)*gmtrx(i,3,2)-gmtrx(i,2,2)*gmtrx(i,3,1)
         g32i=gmtrx(i,3,1)*gmtrx(i,1,2)-gmtrx(i,3,2)*gmtrx(i,1,1)
         g33i=gmtrx(i,1,1)*gmtrx(i,2,2)-gmtrx(i,1,2)*gmtrx(i,2,1)
         gitrx(i,1,1)=gjc*g11i
         gitrx(i,2,1)=gjc*g21i
         gitrx(i,3,1)=gjc*g31i
         gitrx(i,1,2)=gjc*g12i
         gitrx(i,2,2)=gjc*g22i
         gitrx(i,3,2)=gjc*g32i
         gitrx(i,1,3)=gjc*g13i
         gitrx(i,2,3)=gjc*g23i
         gitrx(i,3,3)=gjc*g33i
c
         factor(i)=(factor(i)/xjn)**(1./3.)
c
         gmtrx(i,1,1)=factor(i)*gmtrx(i,1,1)
         gmtrx(i,2,1)=factor(i)*gmtrx(i,2,1)
         gmtrx(i,3,1)=factor(i)*gmtrx(i,3,1)
         gmtrx(i,1,2)=factor(i)*gmtrx(i,1,2)
         gmtrx(i,2,2)=factor(i)*gmtrx(i,2,2)
         gmtrx(i,3,2)=factor(i)*gmtrx(i,3,2)
         gmtrx(i,1,3)=factor(i)*gmtrx(i,1,3)
         gmtrx(i,2,3)=factor(i)*gmtrx(i,2,3)
         gmtrx(i,3,3)=factor(i)*gmtrx(i,3,3)
c
      enddo
c
      do l=1,24
         do k=1,3
            do j=1,3
               do i=lft,llt
                  dvec(i,l)=dvec(i,l)-bmtrx(i,j,k,l)*gitrx(i,k,j)
               enddo
            enddo
         enddo
      enddo
c
      do j=1,24
         do i=lft,llt
            dvec(i,j)=dvec(i,j)/3.
         enddo
      enddo
c
      do l=1,24
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=factor(i)*bmtrx(i,j,k,l)
               enddo
            enddo
         enddo
      enddo
c
      do l=1,24
         do k=1,3
            do j=1,3
               do i=lft,llt
                  bmtrx(i,j,k,l)=bmtrx(i,j,k,l)+
     1                 dvec(i,l)*gmtrx(i,j,k)
               enddo
            enddo
         enddo
      enddo
c
      return
      end
      subroutine usld_b103(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,n5,n6,n7,n8,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5 dn1deta,dn2deta,dn3deta,dn4deta,
     6 dn5deta,dn6deta,dn7deta,dn8deta,
     7 dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8 dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9 x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined solid 103
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4,n5,n6,n7,n8
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4,n5,n6,n7,n8         = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi,
c     dn5dxi,dn6dxi,dn7dxi,dn8dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta,
c     dn5deta,dn6deta,dn7deta,dn8deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     dn1dzeta,dn2dzeta,
c     dn3dzeta,dn4dzeta,
c     dn5dzeta,dn6dzeta,
c     dn7dzeta,dn8dzeta               = nodal shape function derivatives
c                                       w.r.t. third parental coordinate
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c
      return
      end
      subroutine usld_b104(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,n5,n6,n7,n8,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5 dn1deta,dn2deta,dn3deta,dn4deta,
     6 dn5deta,dn6deta,dn7deta,dn8deta,
     7 dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8 dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9 x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined solid 104
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4,n5,n6,n7,n8
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4,n5,n6,n7,n8         = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi,
c     dn5dxi,dn6dxi,dn7dxi,dn8dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta,
c     dn5deta,dn6deta,dn7deta,dn8deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     dn1dzeta,dn2dzeta,
c     dn3dzeta,dn4dzeta,
c     dn5dzeta,dn6dzeta,
c     dn7dzeta,dn8dzeta               = nodal shape function derivatives
c                                       w.r.t. third parental coordinate
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c
      return
      end
      subroutine usld_b105(bmtrx,gmtrx,gjac,
     1 xi,eta,zeta,
     2 n1,n2,n3,n4,n5,n6,n7,n8,
     3 dn1dxi,dn2dxi,dn3dxi,dn4dxi,
     4 dn5dxi,dn6dxi,dn7dxi,dn8dxi,
     5 dn1deta,dn2deta,dn3deta,dn4deta,
     6 dn5deta,dn6deta,dn7deta,dn8deta,
     7 dn1dzeta,dn2dzeta,dn3dzeta,dn4dzeta,
     8 dn5dzeta,dn6dzeta,dn7dzeta,dn8dzeta,
     9 x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute b and g matrix for user defined solid 105
c
      include 'nlqparm'
      dimension bmtrx(nlq,3,3,*),gmtrx(nlq,3,3),gjac(nlq)
      real n1,n2,n3,n4,n5,n6,n7,n8
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
c
c     Variable description
c
c     outputs:
c
c     bmtrx = gradient-displacement matrix
c     gmtrx = jacobian matrix (ITAJ=0)
c     gjac  = jacobian determinant (ITAJ=1)
c
c     inputs:
c
c     xi,eta,zeta                     = parental coordinates
c     n1,n2,n3,n4,n5,n6,n7,n8         = nodal shape function values
c     dn1dxi,dn2dxi,dn3dxi,dn4dxi,
c     dn5dxi,dn6dxi,dn7dxi,dn8dxi     = nodal shape function derivatives
c                                       w.r.t. first parental coordinate
c     dn1deta,dn2deta,dn3deta,dn4deta,
c     dn5deta,dn6deta,dn7deta,dn8deta = nodal shape function derivatives
c                                       w.r.t. second parental coordinate
c     dn1dzeta,dn2dzeta,
c     dn3dzeta,dn4dzeta,
c     dn5dzeta,dn6dzeta,
c     dn7dzeta,dn8dzeta               = nodal shape function derivatives
c                                       w.r.t. third parental coordinate
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c
      return
      end
      subroutine usrsld_frc(nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     computes internal force for user defined solid
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux14loc/sig(nlq,6)
      common/aux15loc/qp(nlq),specen(nlq),dvol(nlq),volold(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
c     Number of degrees of freedom in the solid
c
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/aux15loc/)
c$omp threadprivate (/bel7loc/)
      ndtot=8*(3+nxdof)
c
c     Assumes zero internal force vector on first entry
c
      do j=1,6
c
c     Stress times volume
c
         if (j.le.3) then
            do i=lft,llt
               sigv(i)=(sig(i,j)-qp(i))*vlm(i)
            enddo
         else
            do i=lft,llt
               sigv(i)=sig(i,j)*vlm(i)
            enddo
         endif
c
         if (j.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j,j,k)
               enddo
            enddo
         else
            j1=1
            j2=2
            if (j.eq.5) then
               j1=3
            elseif (j.eq.6) then
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
         do k=1,ndtot
            do i=lft,llt
               frc(i,k)=frc(i,k)+bvec(i,k)*sigv(i)
            enddo
         enddo
c
      enddo
c
      return
      end
      subroutine usrsld_kgm(ske,nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     assembles geometric stiffness matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/aux14loc/sig(nlq,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      dimension ske(nlq,*)
c
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/bel7loc/)
      do j=1,6
c
c     Stress times volume
c
         do i=lft,llt
            sigv(i)=sig(i,j)*vlm(i)
         enddo
c
         if (j.le.3) then
c
c     Loop over element in stiffness matrix
c
            do ml=1,3
               m=ml
               irow=m*(m-1)/2
               m=ml
               do mr=1,ml
                  n=mr
                  iske=irow+n
                  n=mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+sigv(i)*
     1                    (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                    cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                    cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                  enddo
               enddo
            enddo
            do nl=2,8
               do ml=1,3
                  m=(nl-1)*3+ml
                  irow=m*(m-1)/2
                  m=(nl-1)*(3+nxdof)+ml
                  do nr=1,nl-1
                     do mr=1,3
                        n=(nr-1)*3+mr
                        iske=irow+n
                        n=(nr-1)*(3+nxdof)+mr
                        do i=lft,llt
                           ske(i,iske)=ske(i,iske)+sigv(i)*
     1                          (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                          cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                          cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                        enddo
                     enddo
                  enddo
                  do mr=1,ml
                     n=(nl-1)*3+mr
                     iske=irow+n
                     n=(nl-1)*(3+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+sigv(i)*
     1                       (cmtrx(i,1,j,m)*cmtrx(i,1,j,n)+
     2                       cmtrx(i,2,j,m)*cmtrx(i,2,j,n)+
     3                       cmtrx(i,3,j,m)*cmtrx(i,3,j,n))
                     enddo
                  enddo
               enddo
            enddo
            if (nxdof.gt.0) then
              nsnd=(nxdof-1)/3+1
              do isnd=1,nsnd
                nsdf=3
                if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
                do irnd=1,8
                  do isdf=1,nsdf
                    m=24+24*(isnd-1)+3*(irnd-1)+isdf
                    irow=m*(m-1)/2
                    mm=3+(3+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                    do nr=1,8
                      do mr=1,3
                        n=(nr-1)*3+mr
                        iske=irow+n
                        nn=(nr-1)*(3+nxdof)+mr
                        do i=lft,llt
                          ske(i,iske)=ske(i,iske)+sigv(i)*
     1                         (cmtrx(i,1,j,mm)*cmtrx(i,1,j,nn)+
     2                         cmtrx(i,2,j,mm)*cmtrx(i,2,j,nn)+
     3                         cmtrx(i,3,j,mm)*cmtrx(i,3,j,nn))
                        enddo
                      enddo
                    enddo
                    do jsnd=1,nsnd
                      msdf=3
                      if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                      do jrnd=1,8
                        do 1 jsdf=1,msdf
                          n=24+24*(jsnd-1)+3*(jrnd-1)+jsdf
                          if (n.gt.m) goto 1
                          iske=irow+n
                          nn=3+(3+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                          do i=lft,llt
                            ske(i,iske)=ske(i,iske)+sigv(i)*
     1                           (cmtrx(i,1,j,mm)*cmtrx(i,1,j,nn)+
     2                           cmtrx(i,2,j,mm)*cmtrx(i,2,j,nn)+
     3                           cmtrx(i,3,j,mm)*cmtrx(i,3,j,nn))
                          enddo
 1                      continue
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
c
         else
c
            j1=1
            j2=2
            if (j.eq.5) then
               j1=3
            elseif (j.eq.6) then
               j2=3
            endif
c
c     Loop over element in stiffness matrix
c
            do ml=1,3
               m=ml
               irow=m*(m-1)/2
               m=ml
               do mr=1,ml
                  n=mr
                  iske=irow+n
                  n=mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+sigv(i)*
     1                    (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                    cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                    cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                    cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                    cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                    cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                  enddo
               enddo
            enddo
            do nl=2,8
               do ml=1,3
                  m=(nl-1)*3+ml
                  irow=m*(m-1)/2
                  m=(nl-1)*(3+nxdof)+ml
                  do nr=1,nl-1
                     do mr=1,3
                        n=(nr-1)*3+mr
                        iske=irow+n
                        n=(nr-1)*(3+nxdof)+mr
                        do i=lft,llt
                           ske(i,iske)=ske(i,iske)+sigv(i)*
     1                          (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                          cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                          cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                          cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                          cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                          cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                        enddo
                     enddo
                  enddo
                  do mr=1,ml
                     n=(nl-1)*3+mr
                     iske=irow+n
                     n=(nl-1)*(3+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+sigv(i)*
     1                       (cmtrx(i,1,j1,m)*cmtrx(i,1,j2,n)+
     2                       cmtrx(i,1,j2,m)*cmtrx(i,1,j1,n)+
     3                       cmtrx(i,2,j1,m)*cmtrx(i,2,j2,n)+
     4                       cmtrx(i,2,j2,m)*cmtrx(i,2,j1,n)+
     5                       cmtrx(i,3,j1,m)*cmtrx(i,3,j2,n)+
     6                       cmtrx(i,3,j2,m)*cmtrx(i,3,j1,n))
                     enddo
                  enddo
               enddo
            enddo
            if (nxdof.gt.0) then
              nsnd=(nxdof-1)/3+1
              do isnd=1,nsnd
                nsdf=3
                if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
                do irnd=1,8
                  do isdf=1,nsdf
                    m=24+24*(isnd-1)+3*(irnd-1)+isdf
                    irow=m*(m-1)/2
                    mm=3+(3+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                    do nr=1,8
                      do mr=1,3
                        n=(nr-1)*3+mr
                        iske=irow+n
                        nn=(nr-1)*(3+nxdof)+mr
                        do i=lft,llt
                          ske(i,iske)=ske(i,iske)+sigv(i)*
     1                         (cmtrx(i,1,j1,mm)*cmtrx(i,1,j2,nn)+
     2                         cmtrx(i,1,j2,mm)*cmtrx(i,1,j1,nn)+
     3                         cmtrx(i,2,j1,mm)*cmtrx(i,2,j2,nn)+
     4                         cmtrx(i,2,j2,mm)*cmtrx(i,2,j1,nn)+
     5                         cmtrx(i,3,j1,mm)*cmtrx(i,3,j2,nn)+
     6                         cmtrx(i,3,j2,mm)*cmtrx(i,3,j1,nn))
                        enddo
                      enddo
                    enddo
                    do jsnd=1,nsnd
                      msdf=3
                      if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                      do jrnd=1,8
                        do 2 jsdf=1,msdf
                          n=24+24*(jsnd-1)+3*(jrnd-1)+jsdf
                          if (n.gt.m) goto 2
                          iske=irow+n
                          nn=3+(3+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                          do i=lft,llt
                            ske(i,iske)=ske(i,iske)+sigv(i)*
     1                           (cmtrx(i,1,j1,mm)*cmtrx(i,1,j2,nn)+
     2                           cmtrx(i,1,j2,mm)*cmtrx(i,1,j1,nn)+
     3                           cmtrx(i,2,j1,mm)*cmtrx(i,2,j2,nn)+
     4                           cmtrx(i,2,j2,mm)*cmtrx(i,2,j1,nn)+
     5                           cmtrx(i,3,j1,mm)*cmtrx(i,3,j2,nn)+
     6                           cmtrx(i,3,j2,mm)*cmtrx(i,3,j1,nn))
                          enddo
 2                      continue
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            endif
c
         endif
c
      enddo
c
      return
      end
      subroutine usrsld_kmt(ske,nxdof,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     assembles material stiffness matrix
c
      include 'nlqparm'
      include 'nhisparm.inc'
      common/vect8loc/dsave(nlq,6,6)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
c
      dimension ske(nlq,*)
c
c     Number of degrees of freedom in the solid
c
c$omp threadprivate (/vect8loc/)
c$omp threadprivate (/bel7loc/)
      ndtot=8*(3+nxdof)
c
      do jj=1,6
c
         do j=1,ndtot
            do i=lft,llt
               cvec(i,j)=0.
            enddo
         enddo
c
         do j=1,6
c
c     Stiffness times volume
c
            do i=lft,llt
               dsave(i,j,jj)=dsave(i,j,jj)*vlm(i)
            enddo
c
            if (j.le.3) then
               do k=1,ndtot
                  do i=lft,llt
                     bvec(i,k)=cmtrx(i,j,j,k)
                  enddo
               enddo
            else
               j1=1
               j2=2
               if (j.eq.5) then
                  j1=3
               elseif (j.eq.6) then
                  j2=3
               endif
               do k=1,ndtot
                  do i=lft,llt
                     bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
                  enddo
               enddo
            endif
c
            do k=1,ndtot
               do i=lft,llt
                  cvec(i,k)=cvec(i,k)+bvec(i,k)*dsave(i,j,jj)
               enddo
            enddo
c
         enddo
c
         if (jj.le.3) then
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,jj,jj,k)
               enddo
            enddo
         else
            j1=1
            j2=2
            if (jj.eq.5) then
               j1=3
            elseif (jj.eq.6) then
               j2=3
            endif
            do k=1,ndtot
               do i=lft,llt
                  bvec(i,k)=cmtrx(i,j1,j2,k)+cmtrx(i,j2,j1,k)
               enddo
            enddo
         endif
c
c     Loop over element in stiffness matrix
c
         do ml=1,3
            m=ml
            irow=m*(m-1)/2
            m=ml
            do mr=1,ml
               n=mr
               iske=irow+n
               n=mr
               do i=lft,llt
                  ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
               enddo
            enddo
         enddo
         do nl=2,8
            do ml=1,3
               m=(nl-1)*3+ml
               irow=m*(m-1)/2
               m=(nl-1)*(3+nxdof)+ml
               do nr=1,nl-1
                  do mr=1,3
                     n=(nr-1)*3+mr
                     iske=irow+n
                     n=(nr-1)*(3+nxdof)+mr
                     do i=lft,llt
                        ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
                     enddo
                  enddo
               enddo
               do mr=1,ml
                  n=(nl-1)*3+mr
                  iske=irow+n
                  n=(nl-1)*(3+nxdof)+mr
                  do i=lft,llt
                     ske(i,iske)=ske(i,iske)+cvec(i,m)*bvec(i,n)
                  enddo
               enddo
            enddo
         enddo
         if (nxdof.gt.0) then
           nsnd=(nxdof-1)/3+1
           do isnd=1,nsnd
             nsdf=3
             if (isnd.eq.nsnd) nsdf=nxdof-(nsnd-1)*3
             do irnd=1,8
               do isdf=1,nsdf
                 m=24+24*(isnd-1)+3*(irnd-1)+isdf
                 irow=m*(m-1)/2
                 mm=3+(3+nxdof)*(irnd-1)+3*(isnd-1)+isdf
                 do nr=1,8
                   do mr=1,3
                     n=(nr-1)*3+mr
                     iske=irow+n
                     nn=(nr-1)*(3+nxdof)+mr
                     do i=lft,llt
                       ske(i,iske)=ske(i,iske)+cvec(i,mm)*bvec(i,nn)
                     enddo
                   enddo
                 enddo
                 do jsnd=1,nsnd
                   msdf=3
                   if (jsnd.eq.nsnd) msdf=nxdof-(nsnd-1)*3
                   do jrnd=1,8
                     do 1 jsdf=1,msdf
                       n=24+24*(jsnd-1)+3*(jrnd-1)+jsdf
                       if (n.gt.m) goto 1
                       iske=irow+n
                       nn=3+(3+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf                          
                       do i=lft,llt
                         ske(i,iske)=ske(i,iske)+cvec(i,mm)*bvec(i,nn)
                       enddo
 1                   continue
                   enddo
                 enddo
               enddo
             enddo
           enddo
         endif
c
      enddo
c
      return
      end
      subroutine usrsld_h(ietyp,cm,lmc,nhsv,nxdof,lft,llt,ihgf,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'implicit1.inc'
      include 'nhisparm.inc'
c   ... implicit common ...
      integer lnodim,ndofpn,nnpke,melemt,imlft,imllt,is17loc,is18loc,
     &        imp_mxe
      common/bki03iloc/lnodim(nlq,48),ndofpn,nnpke,melemt,imlft,imllt,
     &                 is17loc,is18loc,imp_mxe
c
      real ske,sme,ske_unsym(nlq,100,100)
      equivalence ( ske, ske_unsym )
      common/bki03rloc/ske(nlq,10440),sme(nlq,10440)
c
      integer lmke
      common/bki04iloc/lmke(nlq,144)
c
      common/aux10loc/
     1 px1(nlq),px2(nlq),px3(nlq),px4(nlq),
     & px5(nlq),px6(nlq),px7(nlq),px8(nlq),
     2 py1(nlq),py2(nlq),py3(nlq),py4(nlq),
     & py5(nlq),py6(nlq),py7(nlq),py8(nlq),
     3 pz1(nlq),pz2(nlq),pz3(nlq),pz4(nlq),
     & pz5(nlq),pz6(nlq),pz7(nlq),pz8(nlq),
     4 vx1(nlq),vx2(nlq),vx3(nlq),vx4(nlq),
     5 vx5(nlq),vx6(nlq),vx7(nlq),vx8(nlq),
     6 vy1(nlq),vy2(nlq),vy3(nlq),vy4(nlq),
     7 vy5(nlq),vy6(nlq),vy7(nlq),vy8(nlq),
     8 vz1(nlq),vz2(nlq),vz3(nlq),vz4(nlq),
     9 vz5(nlq),vz6(nlq),vz7(nlq),vz8(nlq)
      common/aux35loc/rhoa(nlq),cb(nlq),davg(nlq),p(nlq)
      common/subtssloc/dt1siz(nlq)
      common/aux8loc/
     & x1(nlq),x2(nlq),x3(nlq),x4(nlq),
     & x5(nlq),x6(nlq),x7(nlq),x8(nlq),
     & y1(nlq),y2(nlq),y3(nlq),y4(nlq),
     & y5(nlq),y6(nlq),y7(nlq),y8(nlq),
     & z1(nlq),z2(nlq),z3(nlq),z4(nlq),
     & z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/bel8loc/uehisv(nlq,2*(NHISVUE+45))
      common/bel8aloc/iuehup(nlq,2*(NHISVUE+45))
c
      dimension dener(*)
      dimension cm(lmc)
      dimension force(nlq,8*(3+NXDOFUE))
c
c$omp threadprivate (/bki03iloc/)
c$omp threadprivate (/bki03rloc/)
c$omp threadprivate (/bki04iloc/)
c$omp threadprivate (/bel7loc/)
c$omp threadprivate (/bel8loc/)
c$omp threadprivate (/bel8aloc/)
c$omp threadprivate (/aux8loc/)
c$omp threadprivate (/aux10loc/)
c$omp threadprivate (/subtssloc/)
c$omp threadprivate (/aux35loc/)
      ndtot=8*(3+nxdof)
      do j=1,ndtot
         do i=lft,llt
            force(i,j)=0.
            stiff(i,j)=0.
         enddo
      enddo
      ndtot2=ndtot*ndtot
      do j=ndtot+1,ndtot2
         do i=lft,llt
            stiff(i,j)=0.
         enddo
      enddo
c
      nadd1=0
      if (ihgf.eq.3) nadd1=21
c
      do i=lft,llt
         vx1(i)=vx1(i)*dt1siz(i)
         vy1(i)=vy1(i)*dt1siz(i)
         vz1(i)=vz1(i)*dt1siz(i)
         vx2(i)=vx2(i)*dt1siz(i)
         vy2(i)=vy2(i)*dt1siz(i)
         vz2(i)=vz2(i)*dt1siz(i)
         vx3(i)=vx3(i)*dt1siz(i)
         vy3(i)=vy3(i)*dt1siz(i)
         vz3(i)=vz3(i)*dt1siz(i)
         vx4(i)=vx4(i)*dt1siz(i)
         vy4(i)=vy4(i)*dt1siz(i)
         vz4(i)=vz4(i)*dt1siz(i)
         vx5(i)=vx5(i)*dt1siz(i)
         vy5(i)=vy5(i)*dt1siz(i)
         vz5(i)=vz5(i)*dt1siz(i)
         vx6(i)=vx6(i)*dt1siz(i)
         vy6(i)=vy6(i)*dt1siz(i)
         vz6(i)=vz6(i)*dt1siz(i)
         vx7(i)=vx7(i)*dt1siz(i)
         vy7(i)=vy7(i)*dt1siz(i)
         vz7(i)=vz7(i)*dt1siz(i)
         vx8(i)=vx8(i)*dt1siz(i)
         vy8(i)=vy8(i)*dt1siz(i)
         vz8(i)=vz8(i)*dt1siz(i)
      enddo
c
      if (nxdof.gt.0) then
         do k=1,8
            do j=1,nxdof
               do i=lft,llt
                  dxdof(i,k,j)=dxdof(i,k,j)*dt1siz(i)
               enddo
            enddo
         enddo
      endif
c
      if (ietyp.eq.101) then
         call usld_e101(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,
     .        vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,
     .        vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8,
     .        dxdof,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        uehisv(1,nhsv+1),lft,llt,dener)
      elseif (ietyp.eq.102) then
         call usld_e102(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,
     .        vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,
     .        vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8,
     .        dxdof,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        uehisv(1,nhsv+1),lft,llt,dener)
      elseif (ietyp.eq.103) then
         call usld_e103(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,
     .        vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,
     .        vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8,
     .        dxdof,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        uehisv(1,nhsv+1),lft,llt,dener)
      elseif (ietyp.eq.104) then
         call usld_e104(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,
     .        vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,
     .        vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8,
     .        dxdof,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        uehisv(1,nhsv+1),lft,llt,dener)
      elseif (ietyp.eq.105) then
         call usld_e105(force,stiff,ndtot,is17loc,
     .        x1,x2,x3,x4,x5,x6,x7,x8,
     .        y1,y2,y3,y4,y5,y6,y7,y8,
     .        z1,z2,z3,z4,z5,z6,z7,z8,
     .        xdof,
     .        vx1,vx2,vx3,vx4,vx5,vx6,vx7,vx8,
     .        vy1,vy2,vy3,vy4,vy5,vy6,vy7,vy8,
     .        vz1,vz2,vz3,vz4,vz5,vz6,vz7,vz8,
     .        dxdof,
     .        uehisv,iuehup,nhsv-nadd1,
     .        cm,lmc,
     .        uehisv(1,nhsv+1),lft,llt,dener)
      endif
c
      do j=1,ndtot
         do i=lft,llt
            frc(i,j)=frc(i,j)+force(i,j)
         enddo
      enddo
c
      if (is17loc.eq.1) then
        nsnd=0
        if (nxdof.gt.0) nsnd=(nxdof-1)/3+1
        do isnd=1,1+nsnd
          nsdf=3
          if (isnd.gt.1) then
            if (isnd.eq.1+nsnd) nsdf=nxdof-(nsnd-1)*3
          endif
          do irnd=1,8
            do isdf=1,nsdf
              m=24*(isnd-1)+3*(irnd-1)+isdf
              irow=m*(m-1)/2
              mm=(3+nxdof)*(irnd-1)+3*(isnd-1)+isdf
              do jsnd=1,1+nsnd
                msdf=3
                if (jsnd.gt.1) then
                  if (jsnd.eq.1+nsnd) msdf=nxdof-(nsnd-1)*3
                endif
                do jrnd=1,8
                  do 1 jsdf=1,msdf
                    n=24*(jsnd-1)+3*(jrnd-1)+jsdf
                    if (isolvr(77).ne.3.or.ihgf.ne.0) then
                    if (n.gt.m) goto 1
                    iske=irow+n
                    nn=(3+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf
                    do i=lft,llt
                      ske(i,iske)=ske(i,iske)+stiff(i,(mm-1)*ndtot+nn)
                    enddo
                    else
                    nn=(3+nxdof)*(jrnd-1)+3*(jsnd-1)+jsdf
                    do i=lft,llt
                      ske_unsym(i,n,m)=ske_unsym(i,n,m)+
     1                      stiff(i,(mm-1)*ndtot+nn)
                    enddo
                    endif
 1                continue
                enddo
              enddo
            enddo
          enddo
        enddo
      endif
c
      return
      end
      subroutine usrsld_ft(nxdof,e,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
c
c     dynamic memory allocation stuff.  This has to be a C style 
c     include because of the use of integer*8
c
      integer i_mem(0:1)
      real r_mem(0:1)
      integer*8 i8_mem(0:1)
      real*8 r8_mem(0:1)
      real*8 real8_mem
      integer*8 m_mem(0:1)
      common/dynmem/real8_mem(0:1)
      equivalence (i_mem,r_mem,i8_mem,r8_mem,real8_mem,m_mem)
      integer*8 mem_alloc, mems_alloc,mem_realloc, memh_ptr
      integer mem_length,memh_alloc,memh_realloc,memh_length
      integer memh_enum,memh_enums,memh_attach
c      integer mem_newmark,mem_getmark,memh_getmark
      integer*8 memh_detach,memh_ralloc
      integer*8 m_to_m8
      integer*8 m_to_mm
      external mem_alloc, mems_alloc, mem_realloc, mem_length
      external memh_alloc, memh_realloc, memh_length, memh_ptr
      external memh_enum,memh_attach,memh_detach,memh_ralloc
      external memh_enums
      external m_to_m8
      external m_to_mm
      integer*8 dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr
      integer*8 dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma
      integer*8 dm_xtpz,dm_disp
      integer*8 dm_me1,dm_me2,dm_me3
      integer*8 dm_masp,dm_masr
      integer*8 dm_xrb,dm_yrb,dm_zrb
      integer*8 dm_axrb,dm_ayrb,dm_azrb
      integer*8 dm_rbfx,dm_rbfy,dm_rbfz
      integer*8 dm_n45,dm_n46,dm_n47
      integer*8 dm_rwsx
      integer len_n45,len_n46,len_n47
      common/dynmem1/ 
     . dm_x,dm_xr,dm_v,dm_vr,dm_a,dm_ar,dm_rots,dm_rotr,
     . dm_xms,dm_xmr,dm_xm2,dm_xmz,dm_xma,
     . dm_xtpz,dm_disp,
     . dm_me1,dm_me2,dm_me3,
     . dm_masp,dm_masr,
     . dm_xrb,dm_yrb,dm_zrb,
     . dm_axrb,dm_ayrb,dm_azrb,
     . dm_rbfx,dm_rbfy,dm_rbfz,
     . dm_n45,dm_n46,dm_n47,dm_rwsx
      common/dynmem1l/len_n45,len_n46,len_n47
c
      integer*8 mem_allocm,mems_allocm,mem_reallocm,mem_loc
      integer memh_allocm,memh_reallocm
      external mem_allocm,mems_allocm,mem_reallocm,mem_loc
      external memh_allocm,memh_reallocm
      integer*8 mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      integer*8 mem_reallocmchk,mem_reallocchk
      integer memh_allocmchk,memh_allocchk
      external mem_allocmchk,mem_allocchk,mems_allocmchk,mems_allocchk
      external mem_reallocmchk,mem_reallocchk
      external memh_allocmchk,memh_allocchk
      include 'memaia.inc'
      include 'nhisparm.inc'
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/aux13loc/
     1 p11(nlq),p12(nlq),p13(nlq),p14(nlq),
     2 p15(nlq),p16(nlq),p17(nlq),p18(nlq),
     3 p21(nlq),p22(nlq),p23(nlq),p24(nlq),
     4 p25(nlq),p26(nlq),p27(nlq),p28(nlq),
     5 p31(nlq),p32(nlq),p33(nlq),p34(nlq),
     6 p35(nlq),p36(nlq),p37(nlq),p38(nlq)
      common/aux33loc/ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ix5(nlq),
     1 ix6(nlq),ix7(nlq),ix8(nlq),mxt(nlq)
      common/sorterloc/nnc,lczc
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR znnc->nnc, zlczc->lczc
c
      real znnc,zlczc
      integer
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/znnc, zlczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
c
      dimension e(3,1)
c
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/sorterloc/)
c$omp threadprivate (/bel7loc/)
      do 60 n=1,nnc
c        lcn=lczc+n+nh12-1
c        i0=ia(lcn)
c        i1=ia(lcn+1)-1
         i0=i_mem(dm_hnfegc+lczc+n-1)
         i1=i_mem(dm_hnfegc+lczc+n  )-1
         if (i1.lt.lft) go to 60
         if (i0.gt.llt) go to 70
         i0=max(i0,lft)
         i1=min(i1,llt)
         do i=i0,i1
            e(1,ix1(i))=e(1,ix1(i))+p11(i)-frc(i,1)
            e(2,ix1(i))=e(2,ix1(i))+p21(i)-frc(i,2)
            e(3,ix1(i))=e(3,ix1(i))+p31(i)-frc(i,3)
            e(1,ix2(i))=e(1,ix2(i))+p12(i)-frc(i,4+nxdof)
            e(2,ix2(i))=e(2,ix2(i))+p22(i)-frc(i,5+nxdof)
            e(3,ix2(i))=e(3,ix2(i))+p32(i)-frc(i,6+nxdof)
            e(1,ix3(i))=e(1,ix3(i))+p13(i)-frc(i,7+2*nxdof)
            e(2,ix3(i))=e(2,ix3(i))+p23(i)-frc(i,8+2*nxdof)
            e(3,ix3(i))=e(3,ix3(i))+p33(i)-frc(i,9+2*nxdof)
            e(1,ix4(i))=e(1,ix4(i))+p14(i)-frc(i,10+3*nxdof)
            e(2,ix4(i))=e(2,ix4(i))+p24(i)-frc(i,11+3*nxdof)
            e(3,ix4(i))=e(3,ix4(i))+p34(i)-frc(i,12+3*nxdof)
         enddo
         do  i=i0,i1
            e(1,ix5(i))=e(1,ix5(i))+p15(i)-frc(i,13+4*nxdof)
            e(2,ix5(i))=e(2,ix5(i))+p25(i)-frc(i,14+4*nxdof)
            e(3,ix5(i))=e(3,ix5(i))+p35(i)-frc(i,15+4*nxdof)
         enddo
         do i=i0,i1
            e(1,ix6(i))=e(1,ix6(i))+p16(i)-frc(i,16+5*nxdof)
            e(2,ix6(i))=e(2,ix6(i))+p26(i)-frc(i,17+5*nxdof)
            e(3,ix6(i))=e(3,ix6(i))+p36(i)-frc(i,18+5*nxdof)
         enddo
         do i=i0,i1
            e(1,ix7(i))=e(1,ix7(i))+p17(i)-frc(i,19+6*nxdof)
            e(2,ix7(i))=e(2,ix7(i))+p27(i)-frc(i,20+6*nxdof)
            e(3,ix7(i))=e(3,ix7(i))+p37(i)-frc(i,21+6*nxdof)
         enddo
         do i=i0,i1
            e(1,ix8(i))=e(1,ix8(i))+p18(i)-frc(i,22+7*nxdof)
            e(2,ix8(i))=e(2,ix8(i))+p28(i)-frc(i,23+7*nxdof)
            e(3,ix8(i))=e(3,ix8(i))+p38(i)-frc(i,24+7*nxdof)
         enddo
         if (nxdof.gt.0) then
            do j=1,nxdof
               nsnd=(j-1)/3+1
               nsdf=j-(nsnd-1)*3
               do i=i0,i1
                  e(nsdf,ixsld(i,1,nsnd))=e(nsdf,ixsld(i,1,nsnd))-
     1                frc(i,3+j)
                  e(nsdf,ixsld(i,2,nsnd))=e(nsdf,ixsld(i,2,nsnd))-
     1                 frc(i,6+nxdof+j)
                  e(nsdf,ixsld(i,3,nsnd))=e(nsdf,ixsld(i,3,nsnd))-
     1                 frc(i,9+2*nxdof+j)
                  e(nsdf,ixsld(i,4,nsnd))=e(nsdf,ixsld(i,4,nsnd))-
     1                 frc(i,12+3*nxdof+j)
                  e(nsdf,ixsld(i,5,nsnd))=e(nsdf,ixsld(i,5,nsnd))-
     1                 frc(i,15+4*nxdof+j)
                  e(nsdf,ixsld(i,6,nsnd))=e(nsdf,ixsld(i,6,nsnd))-
     1                 frc(i,18+5*nxdof+j)
                  e(nsdf,ixsld(i,7,nsnd))=e(nsdf,ixsld(i,7,nsnd))-
     1                 frc(i,21+6*nxdof+j)
                  e(nsdf,ixsld(i,8,nsnd))=e(nsdf,ixsld(i,8,nsnd))-
     1                 frc(i,24+7*nxdof+j)
               enddo
            enddo
         endif
 60   continue
 70   continue
      return
      end
      subroutine usrsld_fs(nxdof,rhs,lft,llt)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'memaia.inc'
      include 'nhisparm.inc'
      common/bel7loc/ixsld(nlq,8,5),ixshl(nlq,4,5)
     .,bmtrx(nlq,3,3,8*(3+NXDOFUE)),
     . cmtrx(nlq,3,3,8*(3+NXDOFUE)),xdof(nlq,8,NXDOFUE),
     . dxdof(nlq,8,NXDOFUE),xhdof(nlq,8,NXDOFUE),
     . bvec(nlq,8*(3+NXDOFUE)),cvec(nlq,8*(3+NXDOFUE)),
     . frc(nlq,8*(3+NXDOFUE)),
     . stiff(nlq,64*(3+NXDOFUE)*(3+NXDOFUE)),
     . sigv(nlq),
     . gmtrx(nlq,3,3),hmtrx(nlq,3,3),cvltot(nlq),
     . xh1(nlq),xh2(nlq),xh3(nlq),xh4(nlq),
     . xh5(nlq),xh6(nlq),xh7(nlq),xh8(nlq),
     . yh1(nlq),yh2(nlq),yh3(nlq),yh4(nlq),
     . yh5(nlq),yh6(nlq),yh7(nlq),yh8(nlq),
     . zh1(nlq),zh2(nlq),zh3(nlq),zh4(nlq),
     . zh5(nlq),zh6(nlq),zh7(nlq),zh8(nlq),
     . hl11(nlq),hl12(nlq),hl13(nlq),hl21(nlq),hl22(nlq),
     . hl23(nlq),hl31(nlq),hl32(nlq),hl33(nlq),
     . xx1(nlq),xx2(nlq),xx3(nlq),xx4(nlq),
     . xx5(nlq),xx6(nlq),xx7(nlq),xx8(nlq),
     . yy1(nlq),yy2(nlq),yy3(nlq),yy4(nlq),
     . yy5(nlq),yy6(nlq),yy7(nlq),yy8(nlq),
     . zz1(nlq),zz2(nlq),zz3(nlq),zz4(nlq),
     . zz5(nlq),zz6(nlq),zz7(nlq),zz8(nlq),vlm(nlq)
      common/aux13loc/
     1 p11(nlq),p12(nlq),p13(nlq),p14(nlq),
     2 p15(nlq),p16(nlq),p17(nlq),p18(nlq),
     3 p21(nlq),p22(nlq),p23(nlq),p24(nlq),
     4 p25(nlq),p26(nlq),p27(nlq),p28(nlq),
     5 p31(nlq),p32(nlq),p33(nlq),p34(nlq),
     6 p35(nlq),p36(nlq),p37(nlq),p38(nlq)
      common/aux33loc/ix1(nlq),ix2(nlq),ix3(nlq),ix4(nlq),ix5(nlq),
     1 ix6(nlq),ix7(nlq),ix8(nlq),mxt(nlq)
      common/sorterloc/nnc,lczc
c
c NOTE: IF YOU CHANGE THIS FILE PLEASE ALSO CHANGE sorter.inc
c WHICH NEEDS TO BE IDENTICAL EXCEPT FOR znnc->nnc, zlczc->lczc
c
      real znnc,zlczc
      integer
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
      common/sorter/znnc, zlczc,
     &    ns11, ns12, ns13, ns14, ns15, ns16,
     &    nh11, nh12, nh13, nh14, nh15, nh16,
     &    nt11, nt12, nt13, nt14, nt15, nt16,
     &    nb11, nb12, nb13, nb14, nb15, nb16,
     &    nu11, nu12, nu13, nu14, nu15, nu16,
     &    nd11, nd12, nd13, nd14, nd15, nd16,
     &    nd17, nd18, nd19, nd20, nd21, nd22,
     &    ncf26, ncf27, ncf28,
     &    n_em_26, n_em_27, n_em_28,
     &    nscf08, nscf09, nscf10, nscf13, nscf14,
     &    nhcf11, nhcf12, nhcf13, nhcf14, nhcf15,
     &    nh_em_11, nh_em_12, nh_em_13, nh_em_14, nh_em_15,
     &    lccf1h, lccf1s, lc_em_1h
c
      integer*8 dm_hnsubgc,dm_hnfegc
      integer*8 dm_bnsubgc,dm_bnfegc
      integer*8 dm_snsubgc,dm_snfegc
      integer*8 dm_tnsubgc,dm_tnfegc
c
      common/dmsorter/
     1 dm_hnsubgc,dm_hnfegc,
     2 dm_bnsubgc,dm_bnfegc,
     3 dm_snsubgc,dm_snfegc,
     4 dm_tnsubgc,dm_tnfegc
c
c The size of the sorter common block.  Used in the restart and dump
c routines.
c
      integer ISORTERSIZE
      parameter (ISORTERSIZE = 68)
c
      dimension rhs(27,1)
c
c$omp threadprivate (/aux13loc/)
c$omp threadprivate (/aux33loc/)
c$omp threadprivate (/sorterloc/)
c$omp threadprivate (/bel7loc/)
      do k=1,3
         do j=1,8
            do i=lft,llt
               rhs((j-1)*3+k,i)=-frc(i,(j-1)*(3+nxdof)+k)
            enddo
         enddo
      enddo
c
      return
      end
      subroutine usld_e101(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined solid 101
c
      include 'nlqparm'
c     dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension force(nlq,*),stiff(nlq,ndtot,*)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      dimension dxdof(nlq,8,*)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension cmtrx(nlq,21)
      dimension dener(nlq)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c     dener                           = internal or hourglass energy, to be incremented
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8 = x-displacements of nodes
c     dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8 = y-displacements of nodes
c     dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8 = z-displacements of nodes
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     cmtrx                           = resultant tangent modulus
c
c     Silly useless element implemented for the purpose of
c     checking the resultant user element interface
c
      dimension q(3)
      dimension x(8),y(8),z(8),dx(8),dy(8),dz(8)
      e=cm(1)
      do i=lft,llt
         x(1)=x1(i)
         x(2)=x2(i)
         x(3)=x3(i)
         x(4)=x4(i)
         x(5)=x5(i)
         x(6)=x6(i)
         x(7)=x7(i)
         x(8)=x8(i)
         y(1)=y1(i)
         y(2)=y2(i)
         y(3)=y3(i)
         y(4)=y4(i)
         y(5)=y5(i)
         y(6)=y6(i)
         y(7)=y7(i)
         y(8)=y8(i)
         z(1)=z1(i)
         z(2)=z2(i)
         z(3)=z3(i)
         z(4)=z4(i)
         z(5)=z5(i)
         z(6)=z6(i)
         z(7)=z7(i)
         z(8)=z8(i)
         dx(1)=dx1(i)
         dx(2)=dx2(i)
         dx(3)=dx3(i)
         dx(4)=dx4(i)
         dx(5)=dx5(i)
         dx(6)=dx6(i)
         dx(7)=dx7(i)
         dx(8)=dx8(i)
         dy(1)=dy1(i)
         dy(2)=dy2(i)
         dy(3)=dy3(i)
         dy(4)=dy4(i)
         dy(5)=dy5(i)
         dy(6)=dy6(i)
         dy(7)=dy7(i)
         dy(8)=dy8(i)
         dz(1)=dz1(i)
         dz(2)=dz2(i)
         dz(3)=dz3(i)
         dz(4)=dz4(i)
         dz(5)=dz5(i)
         dz(6)=dz6(i)
         dz(7)=dz7(i)
         dz(8)=dz8(i)
         do j=1,24
            force(i,j)=hsv(i,j)
         enddo
         do j=1,7
            do k=j+1,8
               eps=(dx(k)-dx(j))*(x(k)-x(j))+
     .              (dy(k)-dy(j))*(y(k)-y(j))+
     .              (dz(k)-dz(j))*(z(k)-z(j))
               enrm=sqrt((x(k)-x(j))**2+
     .              (y(k)-y(j))**2+
     .              (z(k)-z(j))**2)
               eeps=e*eps/enrm**2
               q(1)=(x(k)-x(j))/enrm
               q(2)=(y(k)-y(j))/enrm
               q(3)=(z(k)-z(j))/enrm
               force(i,(k-1)*3+1)=force(i,(k-1)*3+1)+eeps*q(1)
               force(i,(k-1)*3+2)=force(i,(k-1)*3+2)+eeps*q(2)
               force(i,(k-1)*3+3)=force(i,(k-1)*3+3)+eeps*q(3)
               force(i,(j-1)*3+1)=force(i,(j-1)*3+1)-eeps*q(1)
               force(i,(j-1)*3+2)=force(i,(j-1)*3+2)-eeps*q(2)
               force(i,(j-1)*3+3)=force(i,(j-1)*3+3)-eeps*q(3)
               if (istif.eq.1) then
                  do kk=1,3
                     do jj=1,3
                        stiff(i,(k-1)*3+kk,(j-1)*3+jj)=
     .                       stiff(i,(k-1)*3+kk,(j-1)*3+jj)-
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(j-1)*3+kk,(k-1)*3+jj)=
     .                       stiff(i,(j-1)*3+kk,(k-1)*3+jj)-
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(k-1)*3+kk,(k-1)*3+jj)=
     .                       stiff(i,(k-1)*3+kk,(k-1)*3+jj)+
     .                       e*q(kk)*q(jj)/enrm
                        stiff(i,(j-1)*3+kk,(j-1)*3+jj)=
     .                       stiff(i,(j-1)*3+kk,(j-1)*3+jj)+
     .                       e*q(kk)*q(jj)/enrm
                     enddo
                  enddo
               endif
            enddo
         enddo
         do j=1,24
            hsv(i,j)=force(i,j)
         enddo
c
      enddo
      return
      end
      subroutine usld_e102(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined solid 102
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      dimension dxdof(nlq,8,*)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension cmtrx(nlq,21)
      dimension dener(nlq)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c     dener                           = internal or hourglass energy, to be incremented
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8 = x-displacements of nodes
c     dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8 = y-displacements of nodes
c     dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8 = z-displacements of nodes
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     cmtrx                           = resultant tangent modulus
c
c     Default solid element 102, replace this call with own code
c
      call usld_e102_d(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
      return
      end
      subroutine usld_e103(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined solid 103
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      dimension dxdof(nlq,8,*)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension cmtrx(nlq,21)
      dimension dener(nlq)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c     dener                           = internal or hourglass energy, to be incremented
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8 = x-displacements of nodes
c     dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8 = y-displacements of nodes
c     dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8 = z-displacements of nodes
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     cmtrx                           = resultant tangent modulus
c
      return
      end
      subroutine usld_e104(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined solid 104
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      dimension dxdof(nlq,8,*)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension cmtrx(nlq,21)
      dimension dener(nlq)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c     dener                           = internal or hourglass energy, to be incremented
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8 = x-displacements of nodes
c     dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8 = y-displacements of nodes
c     dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8 = z-displacements of nodes
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     cmtrx                           = resultant tangent modulus
c     
      dimension xcur(11,8),xref(11,8)
      nxdof=3
      do i=lft,llt
        xcur(1,1)=x1(i)
        xcur(1,2)=x2(i)
        xcur(1,3)=x3(i)
        xcur(1,4)=x4(i)
        xcur(1,5)=x5(i)
        xcur(1,6)=x6(i)
        xcur(1,7)=x7(i)
        xcur(1,8)=x8(i)
        xcur(2,1)=y1(i)
        xcur(2,2)=y2(i)
        xcur(2,3)=y3(i)
        xcur(2,4)=y4(i)
        xcur(2,5)=y5(i)
        xcur(2,6)=y6(i)
        xcur(2,7)=y7(i)
        xcur(2,8)=y8(i)
        xcur(3,1)=z1(i)
        xcur(3,2)=z2(i)
        xcur(3,3)=z3(i)
        xcur(3,4)=z4(i)
        xcur(3,5)=z5(i)
        xcur(3,6)=z6(i)
        xcur(3,7)=z7(i)
        xcur(3,8)=z8(i)
        do j=1,nxdof
          do k=1,8
            xcur(3+j,k)=xdof(i,k,j)
          enddo
        enddo
        if (hsv(i,1).ne.1.) then
          hsv(i,1)=1.
          ijk=1
          do j=1,8
            do k=1,3+nxdof
              ijk=ijk+1
              hsv(i,ijk)=xcur(k,j)
            enddo
          enddo
        endif
        ijk=1
        do j=1,8
          do k=1,3+nxdof
            ijk=ijk+1
            xref(k,j)=hsv(i,ijk)
          enddo
        enddo
        do j=1,ndtot
          force(i,j)=0.
          do k=1,ndtot
            jk=iabs(j-k)
            stiff(i,j,k)=.5**jk
            if (jk.ne.0) stiff(i,j,k)=-stiff(i,j,k)*.5
            knod=(k-1)/(3+nxdof)+1
            kdof=k-(knod-1)*(3+nxdof)
            force(i,j)=force(i,j)+stiff(i,j,k)*
     1           (xcur(kdof,knod)-xref(kdof,knod))
          enddo
        enddo
      enddo
c
      return
      end
      subroutine usld_e105(force,stiff,ndtot,istif,
     . x1,x2,x3,x4,x5,x6,x7,x8,
     . y1,y2,y3,y4,y5,y6,y7,y8,
     . z1,z2,z3,z4,z5,z6,z7,z8,
     . xdof,
     . dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8,
     . dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8,
     . dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8,
     . dxdof,
     . hsv,ihsv,nhsv,
     . cm,lmc,
     . cmtrx,lft,llt,dener)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     compute force (and stiffness) for user defined solid 105
c
      include 'nlqparm'
      dimension force(nlq,ndtot),stiff(nlq,ndtot,ndtot)
      dimension x1(nlq),x2(nlq),x3(nlq),x4(nlq)
      dimension x5(nlq),x6(nlq),x7(nlq),x8(nlq)
      dimension y1(nlq),y2(nlq),y3(nlq),y4(nlq)
      dimension y5(nlq),y6(nlq),y7(nlq),y8(nlq)
      dimension z1(nlq),z2(nlq),z3(nlq),z4(nlq)
      dimension z5(nlq),z6(nlq),z7(nlq),z8(nlq)
      dimension xdof(nlq,8,*)
      dimension dx1(nlq),dx2(nlq),dx3(nlq),dx4(nlq)
      dimension dx5(nlq),dx6(nlq),dx7(nlq),dx8(nlq)
      dimension dy1(nlq),dy2(nlq),dy3(nlq),dy4(nlq)
      dimension dy5(nlq),dy6(nlq),dy7(nlq),dy8(nlq)
      dimension dz1(nlq),dz2(nlq),dz3(nlq),dz4(nlq)
      dimension dz5(nlq),dz6(nlq),dz7(nlq),dz8(nlq)
      dimension dxdof(nlq,8,*)
      dimension hsv(nlq,nhsv),ihsv(nlq,nhsv),cm(lmc)
      dimension cmtrx(nlq,21)
      dimension dener(nlq)
c
c     Variable description
c
c     outputs:
c
c     force                           = force
c     stiff                           = stiffness
c     hsv                             = history variables
c     ihsv                            = put to 1 to force history variable update regardless of convergence
c     dener                           = internal or hourglass energy, to be incremented
c
c     inputs:
c
c     istif                           = flag for setting up
c                                       stiffness matrix (0 or 1)
c
c     x1,x2,x3,x4,x5,x6,x7,x8         = x-coordinates of nodes
c     y1,y2,y3,y4,y5,y6,y7,y8         = y-coordinates of nodes
c     z1,z2,z3,z4,z5,z6,z7,z8         = z-coordinates of nodes
c     xdof                            = values corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     dx1,dx2,dx3,dx4,dx5,dx6,dx7,dx8 = x-displacements of nodes
c     dy1,dy2,dy3,dy4,dy5,dy6,dy7,dy8 = y-displacements of nodes
c     dz1,dz2,dz3,dz4,dz5,dz6,dz7,dz8 = z-displacements of nodes
c     dxdof                           = displacements corresponding to
c                                       extra degrees of freedom (NXDOF.GT.0)
c     hsv                             = history variables
c     ihsv                            = 0 on input
c     cm                              = property parameters
c     cmtrx                           = resultant tangent modulus
c
      return
      end
      subroutine usld_m101(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user solid 101
c
      dimension w(nxdof,8),x(3,8),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,8)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,8)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine usld_m102(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user solid 102
c
      dimension w(nxdof,8),x(3,8),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,8)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,8)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine usld_m103(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user solid 103
c
      dimension w(nxdof,8),x(3,8),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,8)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,8)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine usld_m104(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user solid 104
c
      dimension w(nxdof,8),x(3,8),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,8)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,8)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      do i=1,8
        do j=1,nxdof
          w(j,i)=100.
        enddo
      enddo
c
      return
      end
c     
      subroutine usld_m105(w,nxdof,x,rho,cm,lmc)
c
c     Compute mass for extra dofs for user solid 105
c
      dimension w(nxdof,8),x(3,8),cm(lmc)
c
c     Variable description
c
c     outputs:
c
c     w(nxdof,8)           = mass vector for extra dofs, first index
c                            corresponds to dof and second to local node
c                            of element
c
c     inputs:
c
c     nxdof                = number of extra dofs for this element
c     x(3,8)               = nodal coordinates for element, first index
c                            corresponds to dof and second to local node
c                            of element
c     rho                  = density of material for this element
c     cm(lmc)              = property parameter array
c     lmc                  = length of cm
c
      return
      end
c     
      subroutine umat42c(idpart,params,lft,llt,fTraction,jump_u,dxdt,
     & aux,ek,ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     vectorized cohesive material user model example
c
c***  variables
c          idpart ---- part ID
c          params ---- material constants
c          lft,llt --- start and end of block
c          fTraction - components of the cohesive force
c          jump_u ---- components of the displacement
c          dxdt ------ components of the velocity
c          aux ------- history storage
c          ek -------- max. stiffness/area for time step calculation
c          ifail ----- =.false. not failed
c                      =.true. failed
c          dt1siz ---- time step size
c          crv ------- curve array
c          nhxbwp ---- internal element id array, lqfinv(nhxbwp(i),2)
c                      gives external element id
c
c***  jump_u, dxdt, and fTraction are in the local coordinate system:
c     components 1 and 2 are in the plane of the cohesive surface
c     component 3 is normal to the plane
c
c***  cm storage convention
c     (1) =0 density is per area
c         =1 density is per volume
c     (2) number of integration points for element deletion
c         =0 no deletion
c     (3:48) material model constants
c
c     Tvergaard-Hutchinson model based on:
c     tahoe/src/elements/cohesive_surface/cohesive_models/TvergHutch3DT.cpp
c     the declaration below is processed by the C preprocessor and
c     is real*4 or real*8 depending on whether LS-DYNA is single or double
c     precision
c
      include 'nlqparm'
      real L,jump_u
      logical ifail
      dimension params(*),fTraction(nlq,*),jump_u(nlq,*),
     &  dxdt(nlq,*),aux(nlq,*),ek(*),ifail(*),dt1siz(*),
     &  crv(lq1,2,*),nhxbwp(*),cma(*)
c
      fsigma_max=params(3)
      fd_c_n=params(4)
      fd_c_t=params(5)
      fL_1=params(6)
      fL_2=params(7)
      fdist=params(8)
      fpenalty=params(9)
      fL_1=fL_1/fdist
      fL_2=fL_2/fdist
      fK=fpenalty*fsigma_max/(fL_1*fd_c_n)
      fac=min(fd_c_n/fd_c_t**2,1./fd_c_n)
      do i=lft,llt
      u_t1 = jump_u(i,1)
      u_t2 = jump_u(i,2)
      u_n = jump_u(i,3)
      r_t1 = u_t1/fd_c_t
      r_t2 = u_t2/fd_c_t
      r_n = u_n/fd_c_n
      L = sqrt(r_t1*r_t1 + r_t2*r_t2 + r_n*r_n)
      if (L .lt. fL_1) then
         sigbyL=fsigma_max/fL_1
      else if (L .lt. fL_2) then
         sigbyL = fsigma_max/L
      else if (L .lt. 1.) then
         sigbyL = fsigma_max*(1. - L)/(1. - fL_2)/L
      else
         sigbyL = 0.0
         ifail(i)=.true.
      endif
      fTraction(i,1) = sigbyL*r_t1*(fd_c_n/fd_c_t)
      fTraction(i,2) = sigbyL*r_t2*(fd_c_n/fd_c_t)
      fTraction(i,3) = sigbyL*r_n
c     penetration
      if (u_n .lt. 0) fTraction(i,3)=fTraction(i,3)+fK*u_n
c     approximate stiffness for timestep
      if (u_n .lt. 0) then
        ek(i)=fac*sigbyL+fK
      else
        ek(i)=fac*sigbyL
      endif
      enddo
      return
      end
      subroutine umat43c(idpart,params,lft,llt,fTraction,jump_u,dxdt,
     & aux,ek,ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     scalar cohesive material user model example
c
c***  variables
c          params ---- material constants
c          lft,llt --- start and end of block
c                      will be set to 0 and 0 for scalar
c          fTraction - components of the cohesive force
c          jump_u ---- components of the displacement
c          dxdt ------ components of the velocity
c          aux ------- history storage
c          ek -------- max. stiffness/area for time step calculation
c          ifail ----- =.false. not failed
c                      =.true. failed
c          dt1siz ---- time step size
c          crv ------- curve array
c          nhxbwp ---- internal element id array, lqfinv(nhxbwp(i),2)
c                      gives external element id
c          idpart ---- part ID
c
c***  jump_u, dxdt, and fTraction are in the local coordinate system:
c     components 1 and 2 are in the plane of the cohesive surface
c     component 3 is normal to the plane
c
c***  cm storage convention
c     (1) =0 density is per area
c         =1 density is per volume
c     (2) number of integration points for element deletion
c         =0 no deletion
c     (3:48) material model constants
c
c     Tveergard-Hutchinson model based on:
c     tahoe/src/elements/cohesive_surface/cohesive_models/TvergHutch3DT.cpp
c     the declaration below is processed by the C preprocessor and
c     is real*4 or real*8 depending on whether LS-DYNA is single or double
c     precision
c
      include 'nlqparm'
      real L,jump_u
      logical ifail
      dimension params(*),fTraction(*),jump_u(*),
     &  dxdt(*),aux(*),crv(lq1,2,*),nhxbwp(*),cma(*)
c
      fsigma_max=params(3)
      fd_c_n=params(4)
      fd_c_t=params(5)
      fL_1=params(6)
      fL_2=params(7)
      fdist=params(8)
      fpenalty=params(9)
      fL_1=fL_1/fdist
      fL_2=fL_2/fdist
      fK=fpenalty*fsigma_max/(fL_1*fd_c_n)
      fac=min(fd_c_n/fd_c_t**2,1./fd_c_n)
      u_t1 = jump_u(1)
      u_t2 = jump_u(2)
      u_n = jump_u(3)
      r_t1 = u_t1/fd_c_t
      r_t2 = u_t2/fd_c_t
      r_n = u_n/fd_c_n
      L = sqrt(r_t1*r_t1 + r_t2*r_t2 + r_n*r_n)
      if (L .lt. fL_1) then
         sigbyL=fsigma_max/fL_1
      else if (L .lt. fL_2) then
         sigbyL = fsigma_max/L
      else if (L .lt. 1.) then
         sigbyL = fsigma_max*(1. - L)/(1. - fL_2)/L
      else
         sigbyL = 0.0
         ifail=.true.
      endif
      fTraction(1) = sigbyL*r_t1*(fd_c_n/fd_c_t)
      fTraction(2) = sigbyL*r_t2*(fd_c_n/fd_c_t)
      fTraction(3) = sigbyL*r_n
c     penetration
      if (u_n .lt. 0) fTraction(3)=fTraction(3)+fK*u_n
c     approximate stiffness for timestep
      if (u_n .lt. 0) then
        ek=fac*sigbyL+fK
      else
        ek=fac*sigbyL
      endif
      return
      end
      subroutine umat44c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     &  aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     &  nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' **** Error umat44c not implemented for part ',i10)
c
      return
      end
      subroutine umat45c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat45c not implemented for part ',i10)
c
      return
      end
      subroutine umat46c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     &  aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     &  nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat46c not implemented for part ',i10)
c
      return
      end
      subroutine umat47c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat47c not implemented for part ', i10)
c
      return
      end
      subroutine umat48c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat48c not implemented for part ',i10)
c
      return
      end
      subroutine umat49c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat49c not implemented for part ',i10)
c
      return
      end
      subroutine umat50c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      write(iotty,1000) idpart
      write(iohsp,1000) idpart
      write(iomsg,1000) idpart
      call adios(2)
c
 1000 format(/' *** Error umat50c not implemented for part ', i10)
c
      return
      end
      subroutine ueoslib(lft,llt,nes,mte,eosp,pnew,v0,dvol,
     & crv,ivect,ihistp,iflag,nh)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     user equation of state library
c
c     include 'alecom.inc'
      include 'nlqparm'
      include 'nhisparm.inc'
c
c
      common/aux14loc/
     & sign1(nlq),sign2(nlq),sign3(nlq),sign4(nlq),
     & sign5(nlq),sign6(nlq),sign7(nlq),aux(nlq,NHISVAR)
      common/aux18loc/dd(nlq),df(nlq),ddq(nlq)
      common/aux35loc/rho(nlq),cb(nlq),q1a(nlq),cxa(nlq)
      common/aux43loc/xm(nlq),p(nlq),xmua(nlq),specen(nlq),cc(nlq)
      common/eosdloc/pc(nlq),g(nlq)
      common/subtssloc/dt1siz(nlq)
c
      common/bk26/begtim,nintcy
      common/bk28/summss,xke,xpe,tt,xte0,erodeke,erodeie,selie,selke,
     1 erodehg
c
      logical first
c
      dimension eosp(*),pnew(*),v0(*),dvol(*),crv(lq1,2,*)
      dimension auxs(148)
c
c$omp threadprivate (/aux14loc/)
c$omp threadprivate (/aux18loc/)
c$omp threadprivate (/aux35loc/)
c$omp threadprivate (/aux43loc/)
c$omp threadprivate (/eosdloc/)
c$omp threadprivate (/subtssloc/)
      nes20=nes-20
c
c***  first time step flag
      first=tt.eq.begtim.and.nintcy.eq.0
c
c***  reference density
      rho0=rho(lft)
c
c***  vectorized implementation of equation of state
      if (ivect.eq.1) then
        go to (21,22,23,24,25,26,27,28,29,30),nes20
c
   21   call ueos21v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   22   call ueos22v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   23   call ueos23v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   24   call ueos24v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   25   call ueos25v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   26   call ueos26v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   27   call ueos27v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   28   call ueos28v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   29   call ueos29v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
   30   call ueos30v(lft,llt,iflag,cb,pnew,aux(1,ihistp),
     &               rho0,eosp,specen,
     &               df,dvol,v0,pc,dt1siz,tt,crv,first)
        return
c
c***  scalar
      else if (ivect.eq.0) then
        go to (121,122,123,124,125,126,127,128,129,130),nes20
c
  121   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos21s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  122   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos22s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  123   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos23s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  124   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos24s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  125   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos25s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  126   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos26s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  127   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos27s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  128   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos28s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  129   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos29s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
c
  130   do i=lft,llt
          do j=1,nh
            auxs(j)=aux(i,ihistp-1+j)
          enddo
          dt=dt1siz(i)
          call ueos30s(iflag,cb(i),pnew(i),auxs,
     &                 rho0,eosp,specen(i),
     &                 df(i),dvol(i),v0(i),pc(i),dt,tt,crv,first)
          do j=1,nh
            aux(i,ihistp-1+j)=auxs(j)
          enddo
        enddo
        return
      endif
c
      return
      end
      subroutine ueos21v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     example vectorized user implementation of the gruneisen EOS
c
c***  variables
c          lft,llt --- tt,crv,first and last indices into arrays
c          iflag ----- =0 calculate bulk modulus
c                      =1 update pressure and energy
c          cb -------- bulk modulus
c          pnew ------ new pressure
c          hist ------ history variables
c          rho0 ------ reference density
c          eosp ------ EOS constants
c          specen ---- energy/reference volume
c          df -------- volume ratio, v/v0 = rho0/rho
c          dvol ------ change in volume over time step
c          v0 -------- reference volume
c          pc -------- pressure cut-off
c          dt -------- time step size
c          tt -------- current time
c          crv ------- curve array
c          first ----- logical .true. for tt,crv,first time step
c                      (for initialization of the history variables)
c
      include 'nlqparm'
      logical first
c
      dimension cb(*),pnew(*),hist(nlq,*),eosp(*),
     &          specen(*),df(*),dvol(*),pc(*),v0(*)
c
      c  =eosp(1)
      s1 =eosp(2)
      s2 =eosp(3)
      s3 =eosp(4)
      g0 =eosp(5)
      sa =eosp(6)
      s11=s1-1.
      s22=2.*s2
      s33=3.*s3
      s32=2.*s3
      sad2=.5*sa
      g0d2=1.-.5*g0
      roc2=rho0*c**2
c
c***  calculate the bulk modulus for the EOS contribution to the sound speed
      if (iflag.eq.0) then
        do i=lft,llt
          xmu=1.0/df(i)-1.
          dfmu=df(i)*xmu
          facp=.5*(1.+sign(1.,xmu))
          facn=1.-facp
          xnum=1.+xmu*(+g0d2-sad2*xmu)
          xdem=1.-xmu*(s11+dfmu*(s2+s3*dfmu))
          tmp=facp/(xdem*xdem)
          a=roc2*xmu*(facn+tmp*xnum)
          b=g0+sa*xmu
          pnum=roc2*(facn+facp*(xnum+xmu*(g0d2-sa*xmu)))
          pden=2.*xdem*(-s11 +dfmu*(-s22+dfmu*(s2-s33+s32*dfmu)))
          cb(i)=pnum*(facn+tmp)-tmp*a*pden+sa*specen(i)+
     &          b*df(i)**2*max(pc(i),(a+b*specen(i)))
        enddo
c
c***  update the pressure and internal energy
      else
        do i=lft,llt
          xmu=1.0/df(i)-1.
          dfmu=df(i)*xmu
          facp=.5*(1.+sign(1.,xmu))
          facn=1.-facp
          xnum=1.+xmu*(+g0d2-sad2*xmu)
          xdem=1.-xmu*(s11+dfmu*(s2+s3*dfmu))
          tmp=facp/(xdem*xdem)
          a=roc2*xmu*(facn+tmp*xnum)
          b=g0+sa*xmu
          dvov0=0.5*dvol(i)/v0(i)
          denom=1.+b*dvov0
          pnew(i)=(a+specen(i)*b)/max(1.e-6,denom)
          pnew(i)=max(pnew(i),pc(i))
          specen(i)=specen(i)-pnew(i)*dvov0
        enddo
      endif
c
      return
      end
      subroutine ueos21s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     example scalar user implementation of the gruneisen EOS
c
c***  variables
c          iflag ----- =0 calculate bulk modulus
c                      =1 update pressure and energy
c          cb -------- bulk modulus
c          pnew ------ new pressure
c          hist ------ history variables
c          rho0 ------ reference density
c          eosp ------ EOS constants
c          specen ---- energy/reference volume
c          df -------- volume ratio, v/v0 = rho0/rho
c          dvol ------ change in volume over time step
c          v0 -------- reference volume
c          pc -------- pressure cut-off
c          dt -------- time step size
c          tt -------- current time
c          crv ------- curve array
c          first ----- logical .true. for tt,crv,first time step
c                      (for initialization of the history variables)
c
      include 'nlqparm'
      logical first
c
      dimension hist(*),eosp(*),crv(lq1,2,*)
c
      c  =eosp(1)
      s1 =eosp(2)
      s2 =eosp(3)
      s3 =eosp(4)
      g0 =eosp(5)
      sa =eosp(6)
      s11=s1-1.
      s22=2.*s2
      s33=3.*s3
      s32=2.*s3
      sad2=.5*sa
      g0d2=1.-.5*g0
      roc2=rho0*c**2
c
c***  calculate the bulk modulus for the EOS contribution to the sound speed
      if (iflag.eq.0) then
        xmu=1.0/df-1.
        dfmu=df*xmu
        facp=.5*(1.+sign(1.,xmu))
        facn=1.-facp
        xnum=1.+xmu*(+g0d2-sad2*xmu)
        xdem=1.-xmu*(s11+dfmu*(s2+s3*dfmu))
        tmp=facp/(xdem*xdem)
        a=roc2*xmu*(facn+tmp*xnum)
        b=g0+sa*xmu
        pnum=roc2*(facn+facp*(xnum+xmu*(g0d2-sa*xmu)))
        pden=2.*xdem*(-s11 +dfmu*(-s22+dfmu*(s2-s33+s32*dfmu)))
        cb=pnum*(facn+tmp)-tmp*a*pden+sa*specen+
     &        b*df**2*max(pc,(a+b*specen))
c
c***  update the pressure and internal energy
      else
        xmu=1.0/df-1.
        dfmu=df*xmu
        facp=.5*(1.+sign(1.,xmu))
        facn=1.-facp
        xnum=1.+xmu*(+g0d2-sad2*xmu)
        xdem=1.-xmu*(s11+dfmu*(s2+s3*dfmu))
        tmp=facp/(xdem*xdem)
        a=roc2*xmu*(facn+tmp*xnum)
        b=g0+sa*xmu
        dvov0=.5*dvol/v0
        denom=1.+b*dvov0
        pnew=(a+specen*b)/max(1.e-6,denom)
        pnew=max(pnew,pc)
        specen=specen-pnew*dvov0
      endif
c
      return
      end
      subroutine ueos22v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos22v not implemented')
      end
      subroutine ueos23v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos23v not implemented')
      end
      subroutine ueos24v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos24v not implemented')
      end
      subroutine ueos25v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos25v not implemented')
      end
      subroutine ueos26v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos26v not implemented')
      end
      subroutine ueos27v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos27v not implemented')
      end
      subroutine ueos28v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos28v not implemented')
      end
      subroutine ueos29v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos29v not implemented')
      end
      subroutine ueos30v(lft,llt,iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos30v not implemented')
      end
      subroutine ueos22s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos22s not implemented')
      end
      subroutine ueos23s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos23s not implemented')
      end
      subroutine ueos24s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos24s not implemented')
      end
      subroutine ueos25s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos25s not implemented')
      end
      subroutine ueos26s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos26s not implemented')
      end
      subroutine ueos27s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos27s not implemented')
      end
      subroutine ueos28s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos28s not implemented')
      end
      subroutine ueos29s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos29s not implemented')
      end
      subroutine ueos30s(iflag,cb,pnew,hist,rho0,eosp,specen,
     & df,dvol,v0,pc,dt,tt,crv,first)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'iounits.inc'
      logical first
      write(iotty,1000)
      write(iohsp,1000)
      write(iomsg,1000)
      call adios(2)
      return
 1000 format(/' *** Error ueos30s not implemented')
      end
      subroutine thusrmat(mt,c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,iphsv,r_matp,crv,npc,plc,nel,nep,iep,eltype,dt,atime,
     2 ihsrcl,hsvm,nmecon,temp,hsv2,hstored)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      integer iburn,isdo
      real dt1,dt2
      common/bk02/iburn,dt1,dt2,isdo
      common/bk26/begtim,nintcy
      real summss,xke,xpe,tt,xte0,erodeke,erodeie,selie,selke,
     . erodehg,eintcnt,engjnt
      common/bk28/summss,xke,xpe,tt,xte0,erodeke,erodeie,selie,selke,
     . erodehg,eintcnt,engjnt
      integer ishc_tran,ishc_step,ishc_iprnt,ishc_iplot,
     & ishc_newfil,ishc_newstp,ishc_dump,ishc_lcthdt
      real shc_alpha,shc_timin,shc_dtlast,shc_timend,
     & shc_dt,shc_dtmin,shc_dtmax,shc_tmpmax,shc_tscp,shc_dyntim
      common/blk05/shc_alpha,shc_timin,shc_dtlast,shc_timend,
     & shc_dt,shc_dtmin,shc_dtmax,shc_tmpmax,shc_tscp,shc_dyntim,
     & ishc_tran,ishc_step,ishc_iprnt,ishc_iplot,
     & ishc_newfil,ishc_newstp,ishc_dump,ishc_lcthdt
      common/blk15/time,idumy6(7)
      common/blk17/nrstrt,ifirst
      character*(*) eltype
      dimension hsv(*),r_matp(64),crv(lq1,2,*),iphsv(*)
      dimension npc(*),plc(*)
      dimension hsvm(100),hstored(*),hsv2(*)
      logical inithis
c
c     Thermal user material top routine. User's should place their
c     user material routines in thumat11-15 corresponding to:
c       - Keyword input: MT=11-15 in MAT_THERMAL_USER_DEFINED MT=11-15.
c       - Structured input: Thermal material type 11-15.
c
c     These thermal material routines are not vectorized.
c
c     Results ( Write only)
c     c1,c2,c3- IORTHO.EQ.1 Conductivity in local r,s,t direction.
c               IORTHO.EQ.0 then only set c1 to the isotropic heat
c               conductivity (c2 and c3 are not used).
c     cvl     - Specific heat capacity (Energy/Mass/Temperature)
c     dcvldt  - Temperature derivative of cvl
c     hsrcl   - Heat source (Energy/Time/Volume)
c     dhsrcdtl- Temperature derivative of hsrcl
c     ihsrcl  - Set to 1 if heat source is to be taken into account for this material
c
c     Read and Writeable data
c     hsv     - History variables
c               History variable i=1...NVH for current element and
c               evaluation point is hsvep(i)=hsv(iphsv(nel)+(iep-1)*nvh+i)
c
c     Read only parameters:
c     mt      - Material number 11-15.
c     iphsv   - History variables index pointer
c     temp    - Temperature
c     atime   - Current time
c     dt      - Length of current time step
c     r_matp - Material parameters from card input (real):
c              r_matp(1)=LMC
c              r_matp(2)=NVH
c              r_matp(3)=IORTHO
c              r_matp(4)=IHVE
c              r_matp(9)=P1
c              ...
c              r_matp(8+LMC)=P_LMC
c
c     plc    - load curve data pairs (abcissa,ordinate).
c     npc    - pointer into plc.  (plc(npc(lcidi)) points to the beginning
c              of load curve ID lc.  npoints=npc(lc+1)-npc(lc)=
c              number of points in the load curve.
c     crv    - Array of load curves interpolated to curves
c              with 101 equidistant points. Array crv(*,*,lcidi)
c              contains the data for curve number lcidi.
c     nel    - Element internal number.
c     eltype - Element type:
c                 eq.'shell'  = Shell element.
c                 eq.'tshell' = Thick shell element (TSHELL=1 on *CONTROL_SHELL).
c                 eq.'solid'  = 3D Solid element (hexahedral or tetrahedal)
c                 eq.'soliddt'= Called for time step calculation for solid element.
c                 eq.'shelldt'= Called for time step calculation for shell element.
c                 eq.'flux'   = Called for flux calculation for solid element.
c                 eq.'flux2d' = Called for flux calculation for shell element.
c                 eq.'na'     = Unknown.
c           Note: When called for time step or flux calculation only return
c                 current values for c1,c2,c3 and cvl and do not update
c                 history variables. Variable atime=0.0 and dt is not
c                 well defined in this situation.
c
c     nep    - Number of points at which the material model is evaluated
c              for this element. Not valid for eltype.eq.'na'.
c     iep    - Current evaluation point, not valid for eltype.eq.'na'.
c     hsvm   - If IHVE.eq.1 then hsvm(i) i=1...nmecon, contains history
c              variable data from mechanical user material routine
c              interpolated to current integration point using linear interpolation
c              or extrapolation.
c                 hsvm(i) i=1..6:            Stress components Sxx, Syy, Szz, Sxy,
c                                            Syz, Szx.
c                                            Stresse
c                 hsvm(6+1):                 Effective plastic strain.
c                                            Corresponds to variable epsp in
c                                            mechanical user defined material.
c                 hsvm(6+1+i) i=i..LMC:      History variables hisv(i) in
c                                            mechanical user defined material.
c                                            LMC refers to the card input for the
c                                            mechanical user defined material.
c                 hsvm(i) i=(LMC+8)..nmecon: Additional history variables
c                                            such as material directions.
c              Note: The following mechanical elements are supported:
c                -Shell: 2, 3, 4, 16
c                -Solid: 1, 2, 10, 13
c     
c     For treating mechanical history variables the following is good to know about
c     staggered step solutions since this can have consequences on this issue.
c     A thermal step is taken whenever the next mechanical time is greater than the
c     last converged thermal time, this since the mechanical side must always know
c     of the thermal state at a given time. Thermal steps are continuously taken until
c     the converged thermal time is greater than the next mechanical step. Whence,
c     at the time of solving the thermal problem we have (parameters in user thermal
c     material routines)
c
c     ttconv - last converged thermal time
c     atime  - time at which thermal properties are to be evaluated
c     ttnext - the time for which we are now solving the thermal problem
c     tmconv - last converged mechanical time
c     tmnext - PREDICTED time for which the next mechanical solution is obtained
c              (may change in implicit mechanics with variable time step)
c
c     We always have tmconv <= ttconv <= tmnext. For explicit mechanics we usually
c     have ttconv~=tmnext (roughly) and thus ttnext>tmnext. This means that only
c     one thermal step is needed before the next mechanical step is taken. In 
c     implicit mechanics we may have ttnext<tmnext or even ttnext<<tmnext, which means 
c     that several thermal steps are needed before the next mechanical step is taken.
c     The thermal solver does NOT know of the mechanical state at a given time, but
c     only the state at time tmconv. This is a consequence of staggered step approach,
c     one of the disciplines must be "ahead" of the other. If for instance the thermal 
c     user material is supposed to convert mechanical work into heat, then the user 
c     could (and should) use these parameters appropriately in order to distribute the 
c     heat evenly throughout the entire time frame until the next mechanical steps to
c     avoid heat spikes after each mechanical step. A pseudo coding is the following.
c
c     Let hsvm(1) be a quantity we want to compute the derivative of, reserve hsv(1) to
c     be the "previous" value of this quantity and hsv(2) the time at which the "previous"
c     value was computed. We want to compute dhsv1=time derivative of hsvm(1), these are
c     the principles, the details may have to be worked on.
c
c     if (ttnext.ge.tmnext) then
c     dhsv1=(hsvm(1)-hsv(1))/(tmconv-hsv(2))
c     hsv(1)=hsvm(1)
c     hsv(2)=tmconv
c     else
c     frac=(ttnext-ttconv)/(tmnext-ttconv)
c     hsv1=hsv(1)+frac*(hsvm(1)-hsv(1))
c     hsv2=hsv(2)+frac*(tmconv-hsv(1))
c     dhsv1=(hsv1-hsv(1))/(hsv2-hsv(2))
c     hsv(1)=hsv1
c     hsv(2)=hsv2
c     endif
c
c     The conditional if (inithis) can be used to determine
c     the first time the thermal user subroutine is entered, this
c     in order to initialize history variables.
c
c     Note: If atime.lt.0.0 then that parameter is not available
c     If dt.lt.0.0 then that parameter is not available
c
c
c     Note on load curves (*DEFINE_CURVE):
c     Use the crvval subroutine for extracting curve value and slope:
c
c         subroutine crvval(crv,eid,xval,yval,slope)
c
c         crv  = curve array                    input (real array)
c         eid   = external curve id             input (real)
c         xval = abcissa value (x-axis)         input (real)
c         yval = ordinate value (y-axis)        output (real)
c         slope= slope of curve (dy/dx)         output (real)
c
c
c     To convert an external load curve number lcid to the internal
c     load curve number lcidi use function lcids:
c        lcidi=lcids(lcid)
c
c
c     Implementation notes:
c      -A user defined subroutine must be able to
c       return results for the following eltype:
c       Material routine for solid: eltype.eq. 'solid', 'soliddt', and 'flux'
c       Material routine for 4-node thermal shell: eltype.eq. 'shell', 'shelldt'
c       Material routine for 12-node thermal shell: eltype.eq. 'tshell', 'shelldt'
c      -r_matp(i), i=41...64 are unused and may be used to store data for each
c       material.
c      -If nrstrt.gt.0 then this is a restart or remap.
c
      nvh=abs(nint(r_matp(2)))
c
      if(eltype.eq.'na') go to 1000
c
      inithis=tt.eq.begtim.and.nintcy.eq.0
c
      do i=1,nvh
        hstored(i)=hsv2(iphsv(nel)+(iep-1)*nvh+i)
      enddo
c
      gptime=atime
      if (gptime.eq.0.) gptime=time-dt+shc_alpha*dt
      goto (110,120,130,140,150) mt-10
 110  continue
      call thumat11(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1     hstored,hsvm,nmecon,r_matp,crv,
     2     nel,nep,iep,eltype,dt,gptime,ihsrcl,temp,
     3     time-dt,time,tt-dt1,tt,inithis)
      go to 200
 120  continue
      call thumat12(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1     hstored,hsvm,nmecon,r_matp,crv,
     2     nel,nep,iep,eltype,dt,gptime,ihsrcl,temp,
     3     time-dt,time,tt-dt1,tt,inithis)
      go to 200
 130  continue
      call thumat13(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1     hstored,hsvm,nmecon,r_matp,crv,
     2     nel,nep,iep,eltype,dt,gptime,ihsrcl,temp,
     3     time-dt,time,tt-dt1,tt,inithis)
      go to 200
 140  continue
      call thumat14(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1     hstored,hsvm,nmecon,r_matp,crv,
     2     nel,nep,iep,eltype,dt,gptime,ihsrcl,temp,
     3     time-dt,time,tt-dt1,tt,inithis)
      go to 200
 150  continue
      call thumat15(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1     hstored,hsvm,nmecon,r_matp,crv,
     2     nel,nep,iep,eltype,dt,gptime,ihsrcl,temp,
     3     time-dt,time,tt-dt1,tt,inithis)
      go to 200
c
 200  continue
      if (.not.(eltype.eq.'soliddt'.or.eltype.eq.'shelldt'.or.
     1     eltype.eq.'solid_ie'.or.eltype.eq.'shell_ie'.or.
     2     eltype.eq.'tshell_ie'.or.eltype.eq.'flux')) then
        do i=1,nvh
          hsv(iphsv(nel)+(iep-1)*nvh+i)=hstored(i)
        enddo
      endif
c
      return
c     Error termination
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1100 format(/' *** Error non-valid element type.')
      end
      subroutine thumat11(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,hsvm,nmecon,r_matp,crv,
     2 nel,nep,iep,eltype,dt,atime,ihsrcl,temp,ttconv,ttnext,
     3 tmconv,tmnext,inithis)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
c
      character*(*) eltype
      logical inithis
      dimension hsv(*),hsvm(*),r_matp(*),crv(lq1,2,*)
c
c     Thermal usermaterial number 11.
c
c     See comments at the begining of subroutine thusrmat
c     for instructions.
c
c     Orthotropic thermal material
      c1=r_matp(8+1)
      c2=r_matp(8+2)
      c3=r_matp(8+3)
      cvl=r_matp(8+4)
      return
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1100 format(/' *** Error thumat11 not defined')
      return
      end
      subroutine thumat12(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,hsvm,nmecon,r_matp,crv,
     2 nel,nep,iep,eltype,dt,atime,ihsrcl,temp,ttconv,ttnext,
     3 tmconv,tmnext,inithis)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
c
      character*(*) eltype
      logical inithis
      dimension hsv(*),hsvm(*),r_matp(*),crv(lq1,2,*)
c
c     Thermal usermaterial number 12.
c
c     See comments at the begining of subroutine thusrmat
c     for instructions.
c
c     Example: isotropic/orthotropic material with k1=P1 and
c     cvl=P2 for solid element only.
c
c     Valid element type?
c
c     Print out some info
      if(nint(r_matp(64)).eq.0) then
         r_matp(64)=1.
         write(iotty,1200)  (r_matp(8+i),i=1,6)
         write(iohsp,1200) (r_matp(8+i),i=1,6)
         write(iomsg,1200) (r_matp(8+i),i=1,6)
      endif
c
c     Calculate response
      c1=r_matp(8+1)
      c2=r_matp(8+2)
      c3=r_matp(8+3)
      cvl=r_matp(8+4)
      dcvdtl=0.0
      eid=nint(r_matp(8+6))
      if(nint(eid).gt.0) then
         call crvval(crv,eid,atime,cvlfac,tmp1)
         cvl=cvl*cvlfac
         dcvdtl=0.0
      endif
c
      eid=nint(r_matp(8+5))
      if(nint(eid).gt.0) then
         ihsrcl=1
         call crvval(crv,eid,atime,hsrcl,tmp1)
         dhsrcdtl=0.0
      endif
c
c     Update history variables
      hsv(1)=cvl
      hsv(2)=atime
      hsv(3)=hsv(3)+1.0
      hsv(4)=iep
      hsv(5)=0.0
      if (nint(r_matp(4)).eq.1) then
         hsv(5)=hsvm(7)
      endif
c
c     Done
      return
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1010 continue
      write(iotty,1110) eltype
      write(iohsp,1110) eltype
      write(iomsg,1110) eltype
      call adios(2)
 1100 format(/' *** Error thumat12 not defined ')
 1110 format(/' *** Error thumat12 undefined eltype: ',a)
 1200 format(/' This is thermal user defined material #12. '/
     1       /'  Material parameter c1-c3       : ',1p3e10.3
     2       /'  Material parameter cv          : ',e10.3
     3       /'  Heatsource load curve          : ',f10.0
     4       /'  Phase transformation load curve: ',f10.0
     5       /'  Thermal History variable 1     : cv'
     5       /'  Thermal History variable 2-5   : Stuff'/)
      return
      end
      subroutine thumat13(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,hsvm,nmecon,r_matp,crv,
     2 nel,nep,iep,eltype,dt,atime,ihsrcl,temp,ttconv,ttnext,
     3 tmconv,tmnext,inithis)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      character*(*) eltype
      logical inithis
      dimension hsv(*),hsvm(*),r_matp(*),crv(lq1,2,*)
c
c     Thermal usermaterial number 13.
c
c     See comments at the begining of subroutine thusrmat
c     for instructions.
c
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1100 format(/' *** Error thumat13 not defined ')
      return
      end
      subroutine thumat14(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,hsvm,nmecon,r_matp,crv,
     2 nel,nep,iep,eltype,dt,atime,ihsrcl,temp,ttconv,ttnext,
     3 tmconv,tmnext,inithis)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
      include 'nlqparm'
      include 'iounits.inc'
      character*(*) eltype
      logical inithis
      dimension hsv(*),hsvm(*),r_matp(*),crv(lq1,2,*)
c
c     Thermal usermaterial number 14.
c
c     See comments at the begining of subroutine thusrmat
c     for instructions.
c
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1100 format(/' *** Error thumat14 not defined ')
      return
      end
      subroutine thumat15(c1,c2,c3,cvl,dcvdtl,hsrcl,dhsrcdtl,
     1 hsv,hsvm,nmecon,r_matp,crv,
     2 nel,nep,iep,eltype,dt,atime,ihsrcl,temp,ttconv,ttnext,
     3 tmconv,tmnext,inithis)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     thermal usermaterial number 15.
c
c     see comments at the begining of subroutine thusrmat
c     for instructions.
c
      include 'nlqparm'
      include 'iounits.inc'
      character*(*) eltype
      logical inithis
c
      dimension hsv(*),hsvm(*),r_matp(*),crv(lq1,2,*)
 1000 continue
      write(iotty,1100)
      write(iohsp,1100)
      write(iomsg,1100)
      call adios(2)
 1100 format(/' *** Error thumat15 not defined ')
      return
      end
      subroutine uadpval(val, nmtcon, gl, ierr, gval)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c   extract the solution variables at an integration point, and
c   convert into the global coordinate system
c
c***  input variables
c     val(nmtcon) solution variables at an integration point (aux14loc)
c     gl(3,3)     element local coordinate system
c     ierr        the option for the error estimation
c                 (refer to CONTROL_ADAPTIVITY Card 4, ADPERR)
c
c***  output variables
c     gval(7)     converted variables for adaptivity (upto 7 variables)
c
      implicit none
      integer nmtcon,ierr
      real val(nmtcon),gl(3,3),gval(7)
c
      real sig(7)
      real a11,a12,a13,a21,a22,a23,a31,a32,a33
      integer i
c
      if(mod(ierr,100).ne.20)return
      call blkcpy (val,sig,7)
c
c     stresses are in local coordinate system
c
      a11=sig(1)*gl(1,1)+sig(4)*gl(1,2)+sig(6)*gl(1,3)
      a12=sig(1)*gl(2,1)+sig(4)*gl(2,2)+sig(6)*gl(2,3)
      a13=sig(1)*gl(3,1)+sig(4)*gl(3,2)+sig(6)*gl(3,3)
      a21=sig(4)*gl(1,1)+sig(2)*gl(1,2)+sig(5)*gl(1,3)
      a22=sig(4)*gl(2,1)+sig(2)*gl(2,2)+sig(5)*gl(2,3)
      a23=sig(4)*gl(3,1)+sig(2)*gl(3,2)+sig(5)*gl(3,3)
      a31=sig(6)*gl(1,1)+sig(5)*gl(1,2)
      a32=sig(6)*gl(2,1)+sig(5)*gl(2,2)
      a33=sig(6)*gl(3,1)+sig(5)*gl(3,2)
c
      gval(1) = gl(1,1)*a11+gl(1,2)*a21+gl(1,3)*a31
      gval(2) = gl(2,1)*a12+gl(2,2)*a22+gl(2,3)*a32
      gval(3) = gl(3,1)*a13+gl(3,2)*a23+gl(3,3)*a33
      gval(4) = gl(1,1)*a12+gl(1,2)*a22+gl(1,3)*a32
      gval(5) = gl(2,1)*a13+gl(2,2)*a23+gl(2,3)*a33
      gval(6) = gl(1,1)*a13+gl(1,2)*a23+gl(1,3)*a33
c
c     effective plastic strain
c      
      gval(7) = sig(7)
      return
      end
      subroutine uadpnorm(gval,dgval,gl,ym,pr,ierr,vnorm,dvnorm)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c   calculate the error norm based on the recovered solutions, at
c   an integration point
c
c***  input variables
c     gval(7)     converted variables for adaptivity (upto 7 variables)
c     dgval(7)    errors for adaptivity (upto 7 variables)
c                 dgval(i) = gval_recoverd(i) - gval(i)
c
c     gl(3,3)     local coordinate system
c     ym          material constant, E
c     pr          material constant, u
c
c     ierr        the option for the error estimation
c                 (refer to CONTROL_ADAPTIVITY Card 4, ADPERR)
c
c   OUTPUT
c     vnorm       Norm of the numerical variables, val
c     dvnorm      Norm of the error, valrecr-val
c
      implicit none
      real gl(3,3),gval(7),dgval(7),ym,pr,vnorm,dvnorm
      integer ierr
c
      real ymi
      integer i,ierrn
c
      if(mod(ierr,100).ne.20)return
c
c     default, energy norm
c
      vnorm = 0.0
      dvnorm = 0.0
      do i=1,6
        vnorm  = vnorm  + gval(i)*gval(i)
        dvnorm = dvnorm + dgval(i)*dgval(i)
      enddo
c
      ymi = 1.0/ym
      vnorm  =  vnorm*ymi
      dvnorm = dvnorm*ymi
c
      end
      subroutine user_inflator(V,P,T,tt,dt,
     .                       xdot,temp,dV)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c   calculate the error norm based on the recovered solutions, at
c   an integration point
c
c***  INPUT VARIABLES
c     V           volume
c     P           pressure
c     T           Temperature
c     tt          time
c     dt          delta time
c
c   OUTPUT
c     xdot        total mass release druing dt
c     temp        gas temperature
c     dV          volume increases due to burning
c
      implicit none
      real V,P,T,tt,dt
      real xdot,temp,dV
c
      dV = 0.
      temp = 296.0
      xdot = 2.0e-3*dt
      if(tt.gt.0.002) xdot = 0.
c      Print *,'xdot =',xdot,dt,tt
c
      return
      end
