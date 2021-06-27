      subroutine hghliu(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2mpusr,lav,nmel,nnm1,mxe,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for hughes-liu shell
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/failu/sieu(128),fail(128)
      common/aux00/
     1 ds11(128),ds12(128),ds13(128),ds22(128),
     2 ds23(128),ds33(128)
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhtnx4(128),yhtny4(128),yhtnz4(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128),
     & yhatx4(128),yhaty4(128),yhatz4(128)
      common/aux12/
     & dx13(128),dx24(128),det8(128),det4(128),
     & dy13(128),dy24(128),
     & dz13(128),dz24(128),
     & wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     & wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     & wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     &  a13(128), a23(128), a33(128),
     & fbl1(128),fbl2(128),fbl3(128),fbl4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/presc/voltot(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      common/shlopt/istrn,istupd,ibelyt
      common/energy/xinen
      common/hourg/ymod,gmod,ifsv
      common/rn/irnxx
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),xyz(12),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),ener(*),
     3 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),rule(mpusr,3,*),
     4 iblks(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      do 10 i=lft,llt
      ds11(i)=0.0
      ds12(i)=0.0
10      ds13(i)=0.0
      do 12 i=lft,llt
      ds22(i)=0.0
      ds23(i)=0.0
      ds33(i)=0.0
   12 continue
      rho=1./ro(mxe)
      nip=csprop(2,mxe)
      ipt=1
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      rhoa(lft)=ro(mxe)
      qhg=.25*hgq(mxe)
      ifsv=ihgq(mxe)
      mte=mtype(mxe)
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 40 i=lft,llt
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      x4(i) =x(1,ix4(i))
      y4(i) =x(2,ix4(i))
      z4(i) =x(3,ix4(i))
      vx1(i)=vt(1,ix1(i))
      vy1(i)=vt(2,ix1(i))
      vz1(i)=vt(3,ix1(i))
      vx2(i)=vt(1,ix2(i))
      vy2(i)=vt(2,ix2(i))
      vz2(i)=vt(3,ix2(i))
      vx3(i)=vt(1,ix3(i))
      vy3(i)=vt(2,ix3(i))
      vz3(i)=vt(3,ix3(i))
      vx4(i)=vt(1,ix4(i))
      vy4(i)=vt(2,ix4(i))
      vz4(i)=vt(3,ix4(i))
      vx5(i)=vr(1,ix1(i))
      vy5(i)=vr(2,ix1(i))
      vz5(i)=vr(3,ix1(i))
      vx6(i)=vr(1,ix2(i))
      vy6(i)=vr(2,ix2(i))
      vz6(i)=vr(3,ix2(i))
      vx7(i)=vr(1,ix3(i))
      vy7(i)=vr(2,ix3(i))
      vz7(i)=vr(3,ix3(i))
      vx8(i)=vr(1,ix4(i))
      vy8(i)=vr(2,ix4(i))
      vz8(i)=vr(3,ix4(i))
   40 continue
      do 50 i=lft,llt
      fail(i)=1.0
      sieu(i)=xies(nnm1+i)
      dx1(i) =dt1*vx1(i)
      dy1(i) =dt1*vy1(i)
      dz1(i) =dt1*vz1(i)
      dx2(i) =dt1*vx2(i)
      dy2(i) =dt1*vy2(i)
      dz2(i) =dt1*vz2(i)
      dx3(i) =dt1*vx3(i)
      dy3(i) =dt1*vy3(i)
      dz3(i) =dt1*vz3(i)
      dx4(i) =dt1*vx4(i)
      dy4(i) =dt1*vy4(i)
      dz4(i) =dt1*vz4(i)
      wxx1(i)=dt1*vx5(i)
      wyy1(i)=dt1*vy5(i)
      wzz1(i)=dt1*vz5(i)
      wxx2(i)=dt1*vx6(i)
      wyy2(i)=dt1*vy6(i)
      wzz2(i)=dt1*vz6(i)
      wxx3(i)=dt1*vx7(i)
      wyy3(i)=dt1*vy7(i)
      wzz3(i)=dt1*vz7(i)
      wxx4(i)=dt1*vx8(i)
      wyy4(i)=dt1*vy8(i)
      wzz4(i)=dt1*vz8(i)
   50 continue
c
      rho8th=.125*rhoa(lft)
      rho4th=.250*rhoa(lft)
      do 60 i=lft,llt
      vx=vx1(i)**2+vx2(i)**2+vx3(i)**2+vx4(i)**2
      vy=vy1(i)**2+vy2(i)**2+vy3(i)**2+vy4(i)**2
      vz=vz1(i)**2+vz2(i)**2+vz3(i)**2+vz4(i)**2
      j=nnm1+i
      fibln=.25*(fibl(1,j)+fibl(2,j)+fibl(3,j)+fibl(4,j))
      enkint(i)=fibln*rho8th*(vx+vy+vz)
      xmomnt(i)=fibln*rho4th*(vx1(i)+vx2(i)+vx3(i)+vx4(i))
      ymomnt(i)=fibln*rho4th*(vy1(i)+vy2(i)+vy3(i)+vy4(i))
      zmomnt(i)=fibln*rho4th*(vz1(i)+vz2(i)+vz3(i)+vz4(i))
   60 continue
      do 100 i=lft,llt
      voltot(i)=0.
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
  100 continue
c
      call gthats (fibl(1,nnm1+1),lft,llt)
c
      call hldfls
      do 250 m=1,nip
      ipt=m
      mtv=mxt(lft)
      if (irl.eq.0) then
      zta=zeta(m,nip)
      wgt=wgts(m,nip)
      elseif (irl.gt.0) then
      zta=(-1.0+(m-1)*2./(nip-1))
      wgt=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) wgt=wgt/2.
      else
      zta=rule(m,1,nrl)
      wgt=rule(m,2,nrl)
      mtu=nint(rule(m,3,nrl))
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
      call stdsps(zta,csprop(13,mxe))
      if (istrn.ne.0) then
      if (m.eq.isrn(1,nip)) then
      call rttrns (strain(1,nnm1+1))
      endif
      if (m.eq.isrn(2,nip)) then
      call rttrns (strain(7,nnm1+1))
      endif
      endif
      call conmds (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
      if (istupd.ne.0) then
      call gbsysd (.5*wgt)
      endif
      call forcis (rhs,wgt,b(ipi),b(iph),nnm1,xies(nnm1+1))
      mxt(lft)=mtv
  250 continue
      if (istupd.ne.0) then
      call tckchg (fibl(1,nnm1+1),lft,llt,b(istupd))
      endif
      sndspd=sqrt(sndspd*rho)
      if (irnxx.ge.0) then
      do 260 i=lft,llt
      yhatn(1,i+nnm1) =yhatx1(i)
      yhatn(2,i+nnm1) =yhaty1(i)
      yhatn(3,i+nnm1) =yhatz1(i)
      yhatn(4,i+nnm1) =yhatx2(i)
      yhatn(5,i+nnm1) =yhaty2(i)
      yhatn(6,i+nnm1) =yhatz2(i)
      yhatn(7,i+nnm1) =yhatx3(i)
      yhatn(8,i+nnm1) =yhaty3(i)
      yhatn(9,i+nnm1) =yhatz3(i)
      yhatn(10,i+nnm1)=yhatx4(i)
      yhatn(11,i+nnm1)=yhaty4(i)
      yhatn(12,i+nnm1)=yhatz4(i)
  260 continue
      else
      do 270 i=lft,llt
      yhatn(1,i+nnm1) =s31(i)
      yhatn(2,i+nnm1) =s32(i)
      yhatn(3,i+nnm1) =s33(i)
      yhatn(4,i+nnm1) =s31(i)
      yhatn(5,i+nnm1) =s32(i)
      yhatn(6,i+nnm1) =s33(i)
      yhatn(7,i+nnm1) =s31(i)
      yhatn(8,i+nnm1) =s32(i)
      yhatn(9,i+nnm1) =s33(i)
      yhatn(10,i+nnm1)=s31(i)
      yhatn(11,i+nnm1)=s32(i)
      yhatn(12,i+nnm1)=s33(i)
  270 continue
      endif
      call gtmnts
      call hltran
      call frc4ns (rhs,rhs(1+neq),fibl(1,nnm1+1),fibl(5,nnm1+1),
     1 iblks)
c
      return
      end
      subroutine cstc0t(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for the cst-c0t triangular shell element
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/failu/sieu(128),fail(128)
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),iblks(*),
     2 xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      mte=mtype(mxe)
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 40 i=lft,llt
      fail(i)=1.0
      diagm(i)=1.e+31
      sieu(i)=xies(nnm1+i)
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
      dz1(i)=vt(3,ix1(i))
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
      dz3(i)=vt(3,ix3(i))
      wxx1(i)=vr(1,ix1(i))
      wyy1(i)=vr(2,ix1(i))
      wzz1(i)=vr(3,ix1(i))
      wxx2(i)=vr(1,ix2(i))
      wyy2(i)=vr(2,ix2(i))
      wzz2(i)=vr(3,ix2(i))
      wxx3(i)=vr(1,ix3(i))
      wyy3(i)=vr(2,ix3(i))
      wzz3(i)=vr(3,ix3(i))
   40 continue
c
      call lsc0t (fibl(1,nnm1+1))
c
      call trnc0t
c
c
c     initialize resultants
c
      do 60 i=1,128
      sig1m(i)=0.0
      sig2m(i)=0.0
      sig4m(i)=0.0
60      sig1n(i)=0.0
      do 62 i=1,128
      sig2n(i)=0.0
      sig4n(i)=0.0
      sig5n(i)=0.0
      sig6n(i)=0.0
      str33(i)=0.0
   62 continue
c
c
c     through the thickness integration
c
      do 250 m=1,nip
      ipt=m
      if (irl.eq.0) then
      zta=.5*zet(m,nip)
      elseif (irl.gt.0) then
      zta=.5*(-1.0+(m-1)*2./(nip-1))
      else
      zta=.5*rule(m,1,nrl)
      mtu=nint(rule(m,3,nrl))
      mtv=mxt(lft)
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
c
c     strain increments
c
      do 90 i=lft,llt
      zeta(i)=zta*thick(i)
      d1(i)=b1vx(i)+zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
      d3(i)=0.
      d4(i)=bxyv(i)+zeta(i)*bxyt(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
      einc(i)=0.0
   90 continue
c
c     constitutive model evaluation
c
      call c0tdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if (istrn.ne.0) then
      if (irl.eq.0) then
      if (m.eq.isrn(1,nip)) then
      call tc0trn (strain(1,nnm1+1))
      endif
      if (m.eq.isrn(2,nip)) then
      call tc0trn (strain(7,nnm1+1))
      endif
      else
      if (m.eq.1  ) then
      call tc0trn (strain(1,nnm1+1))
      endif
      if (m.eq.nip) then
      call tc0trn (strain(7,nnm1+1))
      endif
      endif
      endif
c
c     force evaluation
c
      if (irl.eq.0) then
      fac=wgts(m,nip)
      elseif (irl.gt.0) then
      fac=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) fac=fac/2.
      else
      fac=rule(m,2,nrl)
      mxt(lft)=mtv
      endif
      if (istupd.ne.0) then
      call tc0tu1 (.50*fac)
      endif
      call frc0t1 (fac,b(ipi),b(iph),nnm1,xies(nnm1+1))
  250 continue
c
c     force components in local system
c
      call frc0t2
c
c
c     compute nodal shear forces from equilibrium considerations
c
      call c0tshr
c
      if (istupd.ne.0) then
      call tc0tu2 (fibl(1,nnm1+1),b(istupd))
      endif
c
c     transform force components to global system
c
      sndspd=sqrt(sndspd*rho)
      call c0tfbs (rhs,rhs(1+neq),mte,iblks)
c
      return
      end
      subroutine frc0t1 (factor,volf,epf,ioffst,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/aux36/lft,llt
      dimension volf(*),epf(*),xies(*)
      fac =factor
      faci=1./fac
c
c     integrate resultants
c
      do 10 i=lft,llt
      fga(i)=fac*fga(i)
      fgc(i)=zeta(i)*fga(i)
      sig1m(i)=sig1m(i)+fgc(i)*sig1(i)
      sig2m(i)=sig2m(i)+fgc(i)*sig2(i)
      sig4m(i)=sig4m(i)+fgc(i)*sig4(i)
      sig1n(i)=sig1n(i)+fga(i)*sig1(i)
      sig2n(i)=sig2n(i)+fga(i)*sig2(i)
      sig4n(i)=sig4n(i)+fga(i)*sig4(i)
      sig5n(i)=sig5n(i)+fga(i)*sig5(i)
      sig6n(i)=sig6n(i)+fga(i)*sig6(i)
   10 continue
c
c     tied nodes with failure or tie break slidelines
c
      if (ntbsl.eq.0.and.ntnwf.eq.0) go to 140
      do 130 i=lft,llt
      volf(i+ioffst)=volf(i+ioffst)+fga(i)
      epf(i+ioffst)=epf(i+ioffst)+ep(i)*fga(i)
  130 continue
c
c     internal energy
c
  140 do 150 i=lft,llt
      xies(i) =xies(i)+.50*fga(i)*sarea(i)*einc(i)
      fga(i) =faci*fga(i)
  150 continue
c
      return
      end
      subroutine frc0t2
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux36/lft,llt
      common/auxjn/
     1 bs11(128),bs12(128),bs13(128),bs14(128),
     2 bs15(128),bs16(128),bs21(128),bs22(128),
     3 bs23(128),bs24(128),bs25(128),bs26(128)
      do 10 i=lft,llt
      ft11(i)= px1(i)*sig1n(i)+py1(i)*sig4n(i)
      ft12(i)= py1(i)*sig2n(i)+px1(i)*sig4n(i)
      ft21(i)= px2(i)*sig1n(i)+py2(i)*sig4n(i)
      ft22(i)= py2(i)*sig2n(i)+px2(i)*sig4n(i)
      ft31(i)= py3(i)*sig4n(i)
      ft32(i)= py3(i)*sig2n(i)
      fm11(i)=-py1(i)*sig2m(i)-px1(i)*sig4m(i)+bs11(i)*sig5n(i)+
     1        bs21(i)*sig6n(i)
      fm12(i)= px1(i)*sig1m(i)+py1(i)*sig4m(i)+bs12(i)*sig5n(i)+
     1        bs22(i)*sig6n(i)
      fm21(i)=-py2(i)*sig2m(i)-px2(i)*sig4m(i)+bs13(i)*sig5n(i)+
     1        bs23(i)*sig6n(i)
      fm22(i)= px2(i)*sig1m(i)+py2(i)*sig4m(i)+bs14(i)*sig5n(i)+
     1        bs24(i)*sig6n(i)
      fm31(i)=-py3(i)*sig2m(i)+bs15(i)*sig5n(i)
      fm32(i)= py3(i)*sig4m(i)+bs16(i)*sig5n(i)+bs26(i)*sig6n(i)
   10 continue
      return
      end
      subroutine c0tshr
c     implicit double precision (a-h,o-z)                                    dp
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),area(128),dxl(128)
      const0=1./3.
      do 10 i=lft,llt
      xsr1=-fm11(i)-fm21(i)-fm31(i)
      ft33(i)=xsr1/y3(i)
      xsr2= fm12(i)+fm22(i)+fm32(i)
      ft23(i)=(xsr2-x3(i)*ft33(i))/x2(i)
      ft13(i)=-ft23(i)-ft33(i)
   10 continue
      return
      end
      subroutine c0tfbs(e,f,mte,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      common/failu/sieu(128),fail(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      dimension e(3,*),f(3,*),iblks(*)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      ifail=0
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)+gl13(i)*ft13(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)+gl23(i)*ft13(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)+gl13(i)*ft23(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)+gl23(i)*ft23(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      fx3(i)=gl11(i)*ft31(i)+gl12(i)*ft32(i)+gl13(i)*ft33(i)
      fy3(i)=gl21(i)*ft31(i)+gl22(i)*ft32(i)+gl23(i)*ft33(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)+gl33(i)*ft33(i)
      xmx1(i)=gl11(i)*fm11(i)+gl12(i)*fm12(i)
      xmy1(i)=gl21(i)*fm11(i)+gl22(i)*fm12(i)
      xmz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)
      xmx2(i)=gl11(i)*fm21(i)+gl12(i)*fm22(i)
      xmy2(i)=gl21(i)*fm21(i)+gl22(i)*fm22(i)
      xmz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)
      xmx3(i)=gl11(i)*fm31(i)+gl12(i)*fm32(i)
      xmy3(i)=gl21(i)*fm31(i)+gl22(i)*fm32(i)
      xmz3(i)=gl31(i)*fm31(i)+gl32(i)*fm32(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
      if (mte.ne.15.and.mte.ne.19.and.mte.ne.22.and.mte.ne.24) then
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-xmy3(i)
   65 f(3,ix3(i))=f(3,ix3(i))-xmz3(i)
      else
      ifail=1
cdir$ ivdep
      do 75 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fail(i)*fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fail(i)*fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fail(i)*fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-fail(i)*xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-fail(i)*xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-fail(i)*xmz1(i)
      e(1,ix2(i))=e(1,ix2(i))-fail(i)*fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fail(i)*fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fail(i)*fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-fail(i)*xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-fail(i)*xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-fail(i)*xmz2(i)
      e(1,ix3(i))=e(1,ix3(i))-fail(i)*fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fail(i)*fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fail(i)*fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-fail(i)*xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-fail(i)*xmy3(i)
   75 f(3,ix3(i))=f(3,ix3(i))-fail(i)*xmz3(i)
      endif
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=3
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
      subroutine tc0trn(strain)
c     implicit double precision (a-h,o-z)                                    dp
c
c     integrate average surface strains for taurus plotting
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      dimension strain(12,1)
      do 10 i=lft,llt
      strain(1,i)=strain(1,i)+d1(i)
      strain(2,i)=strain(2,i)+d2(i)
      strain(3,i)=strain(3,i)+d3(i)
      strain(4,i)=strain(4,i)+0.50*d4(i)
      strain(5,i)=strain(5,i)+0.50*d5(i)
   10 strain(6,i)=strain(6,i)+0.50*d6(i)
c
      return
      end
      subroutine tc0tu1 (wgt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      do 10 i=lft,llt
   10 str33(i)=str33(i)+wgt*d3(i)
      return
      end
      subroutine tc0tu2 (fibl,sthick)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      dimension fibl(9,1),sthick(*)
      do 10 i=lft,llt
      str33(i)=1.+str33(i)
      fibl(1,i)=str33(i)*fibl(1,i)
   10 continue
      do 20 i=lft,llt
      sthick(ix1(i))=  max(sthick(ix1(i)),fibl(1,i))
      sthick(ix2(i))=  max(sthick(ix2(i)),fibl(1,i))
      sthick(ix3(i))=  max(sthick(ix3(i)),fibl(1,i))
      sthick(ix4(i))=  max(sthick(ix4(i)),fibl(1,i))
   20 continue
      return
      end
      subroutine c0tdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 dampk,ym,pr,mxe)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/shlopt/istrn,istupd,ibelyt,miter
      common /  / a(1)
      dimension cm(48,*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
      if (mte.eq.1) then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl1s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.2)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl2s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.3)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh3sc (cm,capa)
      elseif (miter.eq.1) then
      call shl3s (cm,capa)
      elseif (miter.eq.2) then
      call sh3si (cm,capa)
      endif
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.4)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl4s (cm,a(ntmp0+1),a(n19))
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.12)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl12s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.15)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl15s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.19) then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh19sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl19s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh19si (cm,a(n8),a(n9),capa)
      endif
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.21) then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl21s(cm,capa,a(ntmp0+1),a(n19))
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.22) then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl22s(cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),a(lc11))
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.24)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh24sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl24s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh24si (cm,a(n8),a(n9),capa)
      endif
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.30) then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl30s(cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.33)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl33s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.34)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl34s (cm,capa)
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.35)  then
      call c0tc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call shl35s(cm,capa)
      elseif (miter.eq.1) then
      call shl35s(cm,capa)
      elseif (miter.eq.2) then
      call shl35s(cm,capa)
      endif
      call c0tc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      endif
      if (dampk.ne.0.) call rydmp2(dampk,ym,pr)
      return
      end
      subroutine c0tc1s (nc,aux,ncl)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 ax(n,m)=aux(m,n-k)
   20 continue
      return
      end
      subroutine c0tc2s (nc,aux,lav,ncl,nip,ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 aux(m,n-k)=ax(n,m)
   20 continue
      if (nip.ne.ipt) return
      lav=lav+(llt-lft+1)*ncl
      return
      end
      subroutine lsc0t (fibl)
c     implicit double precision (a-h,o-z)                                    dp
c
c     lamina coordinate system
c
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux36/lft,llt
      common/aux40/
     1x31(128),y31(128),z31(128),x32(128),y32(128),z32(128),
     2x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     3 x41(128),y41(128),z41(128)
      dimension fibl(9,1)
c
      do 10 i=lft,llt
      x21(i)=x2(i)-x1(i)
      y21(i)=y2(i)-y1(i)
      z21(i)=z2(i)-z1(i)
      x31(i)=x3(i)-x1(i)
      y31(i)=y3(i)-y1(i)
      z31(i)=z3(i)-z1(i)
      x32(i)=x3(i)-x2(i)
      y32(i)=y3(i)-y2(i)
      z32(i)=z3(i)-z2(i)
      thick(i) =fibl(1,i)
      fga(i)   =0.5*thick(i)
   10 continue
c
      do 20 i=lft,llt
      c1(i)=y31(i)*z32(i)-z31(i)*y32(i)
      c2(i)=z31(i)*x32(i)-x31(i)*z32(i)
      c3(i)=x31(i)*y32(i)-y31(i)*x32(i)
      xl(i)=1./(1.e-20+sqrt(c1(i)*c1(i)+c2(i)*c2(i)+c3(i)*c3(i)))
      gl13(i)=c1(i)*xl(i)
      gl23(i)=c2(i)*xl(i)
      gl33(i)=c3(i)*xl(i)
      xl(i)=1./sqrt(x21(i)*x21(i)+y21(i)*y21(i)+z21(i)*z21(i))
      gl11(i)=x21(i)*xl(i)
      gl21(i)=y21(i)*xl(i)
      gl31(i)=z21(i)*xl(i)
      gl12(i)=gl23(i)*gl31(i)-gl33(i)*gl21(i)
      gl22(i)=gl33(i)*gl11(i)-gl13(i)*gl31(i)
      gl32(i)=gl13(i)*gl21(i)-gl23(i)*gl11(i)
   20 continue
c
      return
      end
      subroutine trnc0t
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux5/
     1b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     2b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     3epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &gm11(128),gm12(128),gm13(128),gm21(128),gm22(128),gm23(128),
     &gm31(128),gm32(128),gm33(128),px1a(128),py1a(128),px2a(128),
     &py2a(128),px3a(128),py3a(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux36/lft,llt
      common/aux40/
     &x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     &x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     &x41(128),y41(128),z41(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/sides/sidmn(128)
      common/auxjn/
     1 bs11(128),bs12(128),bs13(128),bs14(128),
     2 bs15(128),bs16(128),bs21(128),bs22(128),
     3 bs23(128),bs24(128),bs25(128),bs26(128)
c
      do 10 i=lft,llt
      gm11(i)=dt1*gl11(i)
      gm21(i)=dt1*gl21(i)
      gm31(i)=dt1*gl31(i)
      gm12(i)=dt1*gl12(i)
      gm22(i)=dt1*gl22(i)
      gm32(i)=dt1*gl32(i)
      gm13(i)=dt1*gl13(i)
      gm23(i)=dt1*gl23(i)
      gm33(i)=dt1*gl33(i)
      vx1(i)=gm11(i)*dx1(i) +gm21(i)*dy1(i) +gm31(i)*dz1(i)
      vy1(i)=gm12(i)*dx1(i) +gm22(i)*dy1(i) +gm32(i)*dz1(i)
      vz1(i)=gm13(i)*dx1(i) +gm23(i)*dy1(i) +gm33(i)*dz1(i)
      vx2(i)=gm11(i)*dx2(i) +gm21(i)*dy2(i) +gm31(i)*dz2(i)
      vy2(i)=gm12(i)*dx2(i) +gm22(i)*dy2(i) +gm32(i)*dz2(i)
      vz2(i)=gm13(i)*dx2(i) +gm23(i)*dy2(i) +gm33(i)*dz2(i)
      vx3(i)=gm11(i)*dx3(i) +gm21(i)*dy3(i) +gm31(i)*dz3(i)
      vy3(i)=gm12(i)*dx3(i) +gm22(i)*dy3(i) +gm32(i)*dz3(i)
      vz3(i)=gm13(i)*dx3(i) +gm23(i)*dy3(i) +gm33(i)*dz3(i)
      x2(i) =gl11(i)*x21(i) +gl21(i)*y21(i) +gl31(i)*z21(i)
      x3(i) =gl11(i)*x31(i) +gl21(i)*y31(i) +gl31(i)*z31(i)
      y3(i) =gl12(i)*x31(i) +gl22(i)*y31(i) +gl32(i)*z31(i)
   10 continue
c
      do 20 i=lft,llt
      x2my3   =x2(i)*y3(i)
      vz2m1   =vz2(i)-vz1(i)
      rigidx  =-((vz3(i)-vz1(i))*x2(i)-vz2m1*x3(i))/x2my3
      rigidy  = vz2m1/x2(i)
      vx5(i)  =gm11(i)*wxx1(i)+gm21(i)*wyy1(i)+gm31(i)*wzz1(i)+rigidx
      vy5(i)  =gm12(i)*wxx1(i)+gm22(i)*wyy1(i)+gm32(i)*wzz1(i)+rigidy
      vx6(i)  =gm11(i)*wxx2(i)+gm21(i)*wyy2(i)+gm31(i)*wzz2(i)+rigidx
      vy6(i)  =gm12(i)*wxx2(i)+gm22(i)*wyy2(i)+gm32(i)*wzz2(i)+rigidy
      vx7(i)  =gm11(i)*wxx3(i)+gm21(i)*wyy3(i)+gm31(i)*wzz3(i)+rigidx
      vy7(i)  =gm12(i)*wxx3(i)+gm22(i)*wyy3(i)+gm32(i)*wzz3(i)+rigidy
      sarea(i)=.5*x2my3
   20 continue
c
      do 40 i=lft,llt
      y312   =y3(i)/12.
      tx2    =2.*x2(i)
      x22    =x2(i)*x2(i)
      x32    =x3(i)*x3(i)
      y32    =y3(i)*y3(i)
      px2(i) = .5* y3(i)
      px1(i) =-px2(i)
      py1(i) =-.5*(x2(i)-x3(i))
      py2(i) =-.5* x3(i)
      py3(i) = .5*x2(i)
      bs21(i)=-y3(i)*y312
      bs22(i)= y312*(tx2+x3(i))
      bs23(i)=-bs21(i)
      bs24(i)= y312*(3.*x2(i)-x3(i))
      bs26(i)= y312*x2(i)
      bs11(i)= y312*(x3(i)-tx2)
      bs12(i)= (x22-x32)/12.
      bs13(i)=-y312*(x2(i)+x3(i))
      bs14(i)= x3(i)*(x3(i)-tx2)/12.
      bs15(i)=-3.*bs26(i)
      bs16(i)= x2(i)*(2.*x3(i)-x2(i))/12.
      side1   = x22
      side2   =(x3(i)-x2(i))**2+y32
      side3   = x32+y32
      sidmn(i)=  min(side1,side2,side3)
      diagm(i)=.25*  max(side1,side2,side3)
   40 continue
c
      do 50 i=lft,llt
      area(i)=1./(sarea(i)+1.e-20)
      px1a(i)=area(i)*px1(i)
      py1a(i)=area(i)*py1(i)
      py2a(i)=area(i)*py2(i)
      py3a(i)=area(i)*py3(i)
      b1vx(i)= px1a(i)*(vx1(i)-vx2(i))
      b1vy(i)= px1a(i)*(vy1(i)-vy2(i))
      b2vy(i)= py1a(i)*vy1(i)+py2a(i)*vy2(i)+py3a(i)*vy3(i)
      b2vx(i)= py1a(i)*vx1(i)+py2a(i)*vx2(i)+py3a(i)*vx3(i)
      bxyv(i)=b2vx(i)+b1vy(i)
      b1tx(i)= px1a(i)*(vx5(i)-vx6(i))
      b1ty(i)= px1a(i)*(vy5(i)-vy6(i))
      b2ty(i)= py1a(i)*vy5(i)+py2a(i)*vy6(i)+py3a(i)*vy7(i)
      b2tx(i)= py1a(i)*vx5(i)+py2a(i)*vx6(i)+py3a(i)*vx7(i)
      bxyt(i)=b2ty(i)-b1tx(i)
      epyz(i)=area(i)*(bs11(i)*vx5(i)+bs12(i)*vy5(i)+bs13(i)*vx6(i)+
     1                 bs14(i)*vy6(i)+bs15(i)*vx7(i)+bs16(i)*vy7(i))
      epzx(i)=area(i)*(bs21(i)*vx5(i)+bs22(i)*vy5(i)+bs23(i)*vx6(i)+
     1                 bs24(i)*vy6(i)+               bs26(i)*vy7(i))
   50 continue
c
      return
      end
      subroutine cstbcz(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for the bciz triangular shell element
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/failu/sieu(128),fail(128)
      common/energy/xinen
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      sixth=1./6.
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))/3
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      mte=mtype(mxe)
      if (mte.eq.20) return
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 40 i=lft,llt
      fail(i)=1.0
      str33(i)=0.0
      diagm(i)=1.e+31
40      sieu(i)=xies(nnm1+i)
      do 41 i=lft,llt
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
41      z2(i) =x(3,ix2(i))
      do 42 i=lft,llt
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
42      dz1(i)=vt(3,ix1(i))
      do 43 i=lft,llt
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
43      dz3(i)=vt(3,ix3(i))
      do 44 i=lft,llt
      wxx1(i)=dt1*vr(1,ix1(i))
      wyy1(i)=dt1*vr(2,ix1(i))
      wzz1(i)=dt1*vr(3,ix1(i))
      wxx2(i)=dt1*vr(1,ix2(i))
44      wyy2(i)=dt1*vr(2,ix2(i))
      do 45 i=lft,llt
      wzz2(i)=dt1*vr(3,ix2(i))
      wxx3(i)=dt1*vr(1,ix3(i))
      wyy3(i)=dt1*vr(2,ix3(i))
      wzz3(i)=dt1*vr(3,ix3(i))
   45 continue
c
      call lsbcz (fibl(1,nnm1+1))
c
      do 50 i=lft,llt
      yhtnx1(i)=yhatn(1,i+nnm1)
      yhtny1(i)=yhatn(2,i+nnm1)
      yhtnz1(i)=yhatn(3,i+nnm1)
      yhtnx2(i)=yhatn(4,i+nnm1)
50      yhtny2(i)=yhatn(5,i+nnm1)
      do 51 i=lft,llt
      yhtnz2(i)=yhatn(6,i+nnm1)
      yhtnx3(i)=yhatn(7,i+nnm1)
      yhtny3(i)=yhatn(8,i+nnm1)
51      yhtnz3(i)=yhatn(9,i+nnm1)
c
      call tsrhts (lft,llt)
c
      call trnbcz
c
c
c
c     three plan integration points are used
c
      do 260 intp=1,3
c
c     initialize resultants
c
      do 60 i=1,128
      sig1m(i)=0.0
      sig2m(i)=0.0
60      sig4m(i)=0.0
      do 61 i=1,128
      sig1n(i)=0.0
      sig2n(i)=0.0
      sig4n(i)=0.0
   61 continue
c
c     compute curvatures
c
      call crvtr(intp)
c
c     through the thickness integration
c
      do 250 m=1,nip
      ipt=m+nip*(intp-1)
      if (irl.eq.0) then
      zta=.5*zet(m,nip)
      elseif (irl.gt.0) then
      zta=.5*(-1.0+(m-1)*2./(nip-1))
      else
      zta=.5*rule(m,1,nrl)
      mtu=nint(rule(m,3,nrl))
      mtv=mxt(lft)
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
c
c     strain increments
c
      do 90 i=lft,llt
      zeta(i)=zta*thick(i)
      d1(i)=b1vx(i)-zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
90      d3(i)=0.
      do 91 i=lft,llt
      d4(i)=bxyv(i)-zeta(i)*bxyt(i)
      d5(i)=0.0
      d6(i)=0.0
      einc(i)=0.0
   91 continue
c
c     constitutive model evaluation
c
      call bczdtb (nmtcon,auxvec,cm,lav,mte,3*nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if (istrn.ne.0) then
      if (irl.eq.0) then
      if (m.eq.isrn(1,nip)) then
      call tbczrn (strain(1,nnm1+1))
      endif
      if (m.eq.isrn(2,nip)) then
      call tbczrn (strain(7,nnm1+1))
      endif
      else
      if (m.eq.1  ) then
      call tbczrn (strain(1,nnm1+1))
      endif
      if (m.eq.nip) then
      call tbczrn (strain(7,nnm1+1))
      endif
      endif
      endif
c
c     force evaluation
c
      if (irl.eq.0) then
      fac=wgts(m,nip)
      elseif (irl.gt.0) then
      fac=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) fac=fac/2.
      else
      fac=rule(m,2,nrl)
      mxt(lft)=mtv
      endif
      if (istupd.ne.0) then
      call tbczu1 (sixth*fac)
      endif
      call frbcz1 (fac,b(ipi),b(iph),nnm1,xies(nnm1+1))
  250 continue
c
c     force components in local system
c
      call frbcz2
c
  260 continue
c
c
c     compute nodal shear forces from equilibrium considerations
c
      call tvshrs
c
      do 270 i=lft,llt
      yhatn(1,i+nnm1) =gl13(i)
      yhatn(2,i+nnm1) =gl23(i)
      yhatn(3,i+nnm1) =gl33(i)
      yhatn(4,i+nnm1) =gl13(i)
      yhatn(5,i+nnm1) =gl23(i)
270      yhatn(6,i+nnm1) =gl33(i)
      do 271 i=lft,llt
      yhatn(7,i+nnm1) =gl13(i)
      yhatn(8,i+nnm1) =gl23(i)
      yhatn(9,i+nnm1) =gl33(i)
      yhatn(10,i+nnm1)=gl13(i)
      yhatn(11,i+nnm1)=gl23(i)
      yhatn(12,i+nnm1)=gl33(i)
  271 continue
c
      if (istupd.ne.0) then
      call tbczu2 (fibl(1,nnm1+1),b(istupd))
      endif
c
c     transform force components to global system
c
      sndspd=sqrt(sndspd*rho)
      call bczfbs (rhs,rhs(1+neq),mte,iblks)
c
      return
      end
      subroutine tsrhts (lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
c
      call rwingv(wxx1,wyy1,wzz1,lft,llt)
      do 10 i=lft,llt
      yhatx1(i)=rot1(i)*yhtnx1(i)+rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
      yhaty1(i)=rot2(i)*yhtnx1(i)+rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
      yhatz1(i)=rot3(i)*yhtnx1(i)+rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
   10 continue
c
      call rwingv(wxx2,wyy2,wzz2,lft,llt)
      do 20 i=lft,llt
      yhatx2(i)=rot1(i)*yhtnx2(i)+rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
      yhaty2(i)=rot2(i)*yhtnx2(i)+rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
      yhatz2(i)=rot3(i)*yhtnx2(i)+rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
   20 continue
c
      call rwingv(wxx3,wyy3,wzz3,lft,llt)
      do 30 i=lft,llt
      yhatx3(i)=rot1(i)*yhtnx3(i)+rot4(i)*yhtny3(i)+rot7(i)*yhtnz3(i)
      yhaty3(i)=rot2(i)*yhtnx3(i)+rot5(i)*yhtny3(i)+rot8(i)*yhtnz3(i)
      yhatz3(i)=rot3(i)*yhtnx3(i)+rot6(i)*yhtny3(i)+rot9(i)*yhtnz3(i)
   30 continue
c
      return
      end
      subroutine frbcz1 (factor,volf,epf,ioffst,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/aux36/lft,llt
      dimension volf(*),epf(*),xies(*)
      fac =factor
      faci=1./fac
c
c     integrate resultants
c
      do 10 i=lft,llt
      fga(i)=fac*fga(i)
      fgc(i)=zeta(i)*fga(i)
      sig1m(i)=sig1m(i)-fgc(i)*sig1(i)
      sig2m(i)=sig2m(i)-fgc(i)*sig2(i)
      sig4m(i)=sig4m(i)-fgc(i)*sig4(i)
      sig1n(i)=sig1n(i)+fga(i)*sig1(i)
      sig2n(i)=sig2n(i)+fga(i)*sig2(i)
      sig4n(i)=sig4n(i)+fga(i)*sig4(i)
   10 continue
c
c     tied nodes with failure or tie break slidelines
c
      if (ntbsl.eq.0.and.ntnwf.eq.0) go to 140
      do 130 i=lft,llt
      volf(i+ioffst)=volf(i+ioffst)+fga(i)
      epf(i+ioffst)=epf(i+ioffst)+ep(i)*fga(i)
  130 continue
c
c     internal energy
c
  140 do 150 i=lft,llt
      xies(i) =xies(i)+.166666666666667*fga(i)*sarea(i)*einc(i)
      fga(i) =faci*fga(i)
  150 continue
c
      return
      end
      subroutine frbcz2
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux36/lft,llt
      common/auxjn/fctr(128,18)
      do 10 i=lft,llt
      ft11(i)=ft11(i)+px1(i)*sig1n(i)+py1(i)*sig4n(i)
      ft12(i)=ft12(i)+py1(i)*sig2n(i)+px1(i)*sig4n(i)
      ft21(i)=ft21(i)+px2(i)*sig1n(i)+py2(i)*sig4n(i)
      ft22(i)=ft22(i)+py2(i)*sig2n(i)+px2(i)*sig4n(i)
      ft31(i)=ft31(i)+px3(i)*sig1n(i)+py3(i)*sig4n(i)
      ft32(i)=ft32(i)+py3(i)*sig2n(i)+px3(i)*sig4n(i)
      fm11(i)=fm11(i)+fctr(i, 1)*sig1m(i)+fctr(i, 7)*sig2m(i)
     1               +fctr(i,13)*sig4m(i)
      fm12(i)=fm12(i)+fctr(i, 2)*sig1m(i)+fctr(i, 8)*sig2m(i)
     1               +fctr(i,14)*sig4m(i)
      fm21(i)=fm21(i)+fctr(i, 3)*sig1m(i)+fctr(i, 9)*sig2m(i)
     1               +fctr(i,15)*sig4m(i)
      fm22(i)=fm22(i)+fctr(i, 4)*sig1m(i)+fctr(i,10)*sig2m(i)
     1               +fctr(i,16)*sig4m(i)
      fm31(i)=fm31(i)+fctr(i, 5)*sig1m(i)+fctr(i,11)*sig2m(i)
     1               +fctr(i,17)*sig4m(i)
      fm32(i)=fm32(i)+fctr(i, 6)*sig1m(i)+fctr(i,12)*sig2m(i)
     1               +fctr(i,18)*sig4m(i)
   10 continue
      return
      end
      subroutine tvshrs
c     implicit double precision (a-h,o-z)                                    dp
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),area(128),dxl(128)
      const0=1./3.
      do 10 i=lft,llt
      g2=-x3(i)
      h2= y3(i)
      g3= x2(i)
      h3=-y2(i)
      xsr1=-fm11(i)-fm21(i)-fm31(i)
      xsr2= fm12(i)+fm22(i)+fm32(i)
      const1=1.0/(12.*area(i))
      const2=const1/(2.*area(i))
      ft11(i)=const0*ft11(i)
      ft12(i)=const0*ft12(i)
      ft21(i)=const0*ft21(i)
      ft22(i)=const0*ft22(i)
      ft31(i)=const0*ft31(i)
      ft32(i)=const0*ft32(i)
      ft23(i)=const2*(g2*xsr1+h2*xsr2)
      ft33(i)=const2*(g3*xsr1+h3*xsr2)
      ft13(i)=-ft23(i)-ft33(i)
      fm11(i)=const1*fm11(i)
      fm12(i)=const1*fm12(i)
      fm21(i)=const1*fm21(i)
      fm22(i)=const1*fm22(i)
      fm31(i)=const1*fm31(i)
      fm32(i)=const1*fm32(i)
   10 continue
      return
      end
      subroutine bczfbs(e,f,mte,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),
     &ft23(128),ft31(128),ft32(128),ft33(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),sg5(128),sg6(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      common/failu/sieu(128),fail(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      dimension e(3,*),f(3,*),iblks(*)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      ifail=0
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)+gl13(i)*ft13(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)+gl23(i)*ft13(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)+gl13(i)*ft23(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)+gl23(i)*ft23(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      fx3(i)=gl11(i)*ft31(i)+gl12(i)*ft32(i)+gl13(i)*ft33(i)
      fy3(i)=gl21(i)*ft31(i)+gl22(i)*ft32(i)+gl23(i)*ft33(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)+gl33(i)*ft33(i)
      xmx1(i)=gl11(i)*fm11(i)+gl12(i)*fm12(i)
      xmy1(i)=gl21(i)*fm11(i)+gl22(i)*fm12(i)
      xmz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)
      xmx2(i)=gl11(i)*fm21(i)+gl12(i)*fm22(i)
      xmy2(i)=gl21(i)*fm21(i)+gl22(i)*fm22(i)
      xmz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)
      xmx3(i)=gl11(i)*fm31(i)+gl12(i)*fm32(i)
      xmy3(i)=gl21(i)*fm31(i)+gl22(i)*fm32(i)
      xmz3(i)=gl31(i)*fm31(i)+gl32(i)*fm32(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
      if (mte.ne.15.and.mte.ne.19.and.mte.ne.22.and.mte.ne.24) then
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-xmy3(i)
   65 f(3,ix3(i))=f(3,ix3(i))-xmz3(i)
      else
      ifail=1
cdir$ ivdep
      do 75 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fail(i)*fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fail(i)*fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fail(i)*fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-fail(i)*xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-fail(i)*xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-fail(i)*xmz1(i)
      e(1,ix2(i))=e(1,ix2(i))-fail(i)*fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fail(i)*fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fail(i)*fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-fail(i)*xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-fail(i)*xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-fail(i)*xmz2(i)
      e(1,ix3(i))=e(1,ix3(i))-fail(i)*fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fail(i)*fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fail(i)*fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-fail(i)*xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-fail(i)*xmy3(i)
   75 f(3,ix3(i))=f(3,ix3(i))-fail(i)*xmz3(i)
      endif
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=3
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
      subroutine tbczrn(strain)
c     implicit double precision (a-h,o-z)                                    dp
c
c     integrate average surface strains for taurus plotting
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      dimension strain(12,1)
      third=1./3.
      sixth=.5*third
      do 10 i=lft,llt
      strain(1,i)=strain(1,i)+third*d1(i)
      strain(2,i)=strain(2,i)+third*d2(i)
      strain(3,i)=strain(3,i)+third*d3(i)
      strain(4,i)=strain(4,i)+sixth*d4(i)
      strain(5,i)=strain(5,i)+sixth*d5(i)
   10 strain(6,i)=strain(6,i)+sixth*d6(i)
c
      return
      end
      subroutine tbczu1 (wgt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      do 10 i=lft,llt
   10 str33(i)=str33(i)+wgt*d3(i)
      return
      end
      subroutine tbczu2 (fibl,sthick)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      dimension fibl(9,1),sthick(*)
      do 10 i=lft,llt
      str33(i)=1.+str33(i)
      fibl(1,i)=str33(i)*fibl(1,i)
   10 continue
      do 20 i=lft,llt
      sthick(ix1(i))=  max(sthick(ix1(i)),fibl(1,i))
      sthick(ix2(i))=  max(sthick(ix2(i)),fibl(1,i))
      sthick(ix3(i))=  max(sthick(ix3(i)),fibl(1,i))
      sthick(ix4(i))=  max(sthick(ix4(i)),fibl(1,i))
   20 continue
      return
      end
      subroutine bczdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 dampk,ym,pr,mxe)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/shlopt/istrn,istupd,ibelyt,miter
      common /  / a(1)
      dimension cm(48,*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
      if (mte.eq.1) then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl1s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.2)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl2s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.3)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh3sc (cm,capa)
      elseif (miter.eq.1) then
      call shl3s (cm,capa)
      elseif (miter.eq.2) then
      call sh3si (cm,capa)
      endif
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.4)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl4s (cm,a(ntmp0+1),a(n19))
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.12)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl12s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.15)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl15s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.19) then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh19sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl19s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh19si (cm,a(n8),a(n9),capa)
      endif
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.21) then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl21s(cm,capa,a(ntmp0+1),a(n19))
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.22) then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl22s(cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),a(lc11))
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.24)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh24sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl24s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh24si (cm,a(n8),a(n9),capa)
      endif
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.30) then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl30s(cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.33)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl33s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.34)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl34s (cm,capa)
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.3)  then
      call bciz1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call shl35s(cm,capa)
      elseif (miter.eq.1) then
      call shl35s(cm,capa)
      elseif (miter.eq.2) then
      call shl35s(cm,capa)
      endif
      call bciz2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      endif
      if (dampk.ne.0.) call rydmp2(dampk,ym,pr)
      return
      end
      subroutine bciz1s (nc,aux,ncl)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 ax(n,m)=aux(m,n-k)
   20 continue
      return
      end
      subroutine bciz2s (nc,aux,lav,ncl,nip,ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 aux(m,n-k)=ax(n,m)
   20 continue
      if (nip.ne.ipt) return
      lav=lav+(llt-lft+1)*ncl
      return
      end
      subroutine lsbcz (fibl)
c     implicit double precision (a-h,o-z)                                    dp
c
c     lamina coordinate system
c
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux36/lft,llt
      common/aux40/
     1x31(128),y31(128),z31(128),x32(128),y32(128),z32(128),
     2x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     3 x41(128),y41(128),z41(128)
      dimension fibl(9,1)
c
      do 10 i=lft,llt
      x21(i)=x2(i)-x1(i)
      y21(i)=y2(i)-y1(i)
      z21(i)=z2(i)-z1(i)
      x31(i)=x3(i)-x1(i)
      y31(i)=y3(i)-y1(i)
      z31(i)=z3(i)-z1(i)
      x32(i)=x3(i)-x2(i)
      y32(i)=y3(i)-y2(i)
      z32(i)=z3(i)-z2(i)
      thick(i) =fibl(1,i)
      fga(i)   =0.5*thick(i)
   10 continue
c
      do 20 i=lft,llt
      c1(i)=y31(i)*z32(i)-z31(i)*y32(i)
      c2(i)=z31(i)*x32(i)-x31(i)*z32(i)
      c3(i)=x31(i)*y32(i)-y31(i)*x32(i)
      xl(i)=1./(1.e-20+sqrt(c1(i)*c1(i)+c2(i)*c2(i)+c3(i)*c3(i)))
      gl13(i)=c1(i)*xl(i)
      gl23(i)=c2(i)*xl(i)
      gl33(i)=c3(i)*xl(i)
      xl(i)=1./sqrt(x21(i)*x21(i)+y21(i)*y21(i)+z21(i)*z21(i))
      gl11(i)=x21(i)*xl(i)
      gl21(i)=y21(i)*xl(i)
      gl31(i)=z21(i)*xl(i)
      gl12(i)=gl23(i)*gl31(i)-gl33(i)*gl21(i)
      gl22(i)=gl33(i)*gl11(i)-gl13(i)*gl31(i)
      gl32(i)=gl13(i)*gl21(i)-gl23(i)*gl11(i)
   20 continue
c
      return
      end
      subroutine trnbcz
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux5/
     1b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     2b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     3epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &gm11(128),gm12(128),gm13(128),gm21(128),gm22(128),gm23(128),
     &gm31(128),gm32(128),gm33(128),px1a(128),py1a(128),px2a(128),
     &py2a(128),px3a(128),py3a(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux36/lft,llt
      common/aux40/
     &x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     &x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     &x41(128),y41(128),z41(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/sides/sidmn(128)
c
      do 10 i=lft,llt
      gm11(i)=dt1*gl11(i)
      gm21(i)=dt1*gl21(i)
      gm31(i)=dt1*gl31(i)
      gm12(i)=dt1*gl12(i)
      gm22(i)=dt1*gl22(i)
      gm32(i)=dt1*gl32(i)
      gm13(i)=dt1*gl13(i)
      gm23(i)=dt1*gl23(i)
      gm33(i)=dt1*gl33(i)
      vx1(i)=gm11(i)*dx1(i) +gm21(i)*dy1(i) +gm31(i)*dz1(i)
      vy1(i)=gm12(i)*dx1(i) +gm22(i)*dy1(i) +gm32(i)*dz1(i)
      vz1(i)=gm13(i)*dx1(i) +gm23(i)*dy1(i) +gm33(i)*dz1(i)
      vx2(i)=gm11(i)*dx2(i) +gm21(i)*dy2(i) +gm31(i)*dz2(i)
      vy2(i)=gm12(i)*dx2(i) +gm22(i)*dy2(i) +gm32(i)*dz2(i)
      vz2(i)=gm13(i)*dx2(i) +gm23(i)*dy2(i) +gm33(i)*dz2(i)
      vx3(i)=gm11(i)*dx3(i) +gm21(i)*dy3(i) +gm31(i)*dz3(i)
      vy3(i)=gm12(i)*dx3(i) +gm22(i)*dy3(i) +gm32(i)*dz3(i)
      vz3(i)=gm13(i)*dx3(i) +gm23(i)*dy3(i) +gm33(i)*dz3(i)
      vy5(i)=  gl11(i)*yhatx1(i)+gl21(i)*yhaty1(i)+gl31(i)*yhatz1(i)
      vx5(i)=-(gl12(i)*yhatx1(i)+gl22(i)*yhaty1(i)+gl32(i)*yhatz1(i))
      vy6(i)=  gl11(i)*yhatx2(i)+gl21(i)*yhaty2(i)+gl31(i)*yhatz2(i)
      vx6(i)=-(gl12(i)*yhatx2(i)+gl22(i)*yhaty2(i)+gl32(i)*yhatz2(i))
      vy7(i)=  gl11(i)*yhatx3(i)+gl21(i)*yhaty3(i)+gl31(i)*yhatz3(i)
      vx7(i)=-(gl12(i)*yhatx3(i)+gl22(i)*yhaty3(i)+gl32(i)*yhatz3(i))
      x2(i) =gl11(i)*x21(i) +gl21(i)*y21(i) +gl31(i)*z21(i)
      y2(i) =gl12(i)*x21(i) +gl22(i)*y21(i) +gl32(i)*z21(i)
      x3(i) =gl11(i)*x31(i) +gl21(i)*y31(i) +gl31(i)*z31(i)
      y3(i) =gl12(i)*x31(i) +gl22(i)*y31(i) +gl32(i)*z31(i)
   10 continue
c
      do 40 i=lft,llt
      px1(i)  = .5*(y2(i)-y3(i))
      px2(i)  = .5* y3(i)
      px3(i)  =-.5*y2(i)
      py1(i)  =-.5*(x2(i)-x3(i))
      py2(i)  =-.5* x3(i)
      py3(i)  = .5*x2(i)
      sarea(i)=2.0*(py2(i)*px1(i)-py1(i)*px2(i))
      side1   =x2(i)*x2(i)+y2(i)*y2(i)
      side2   =(x3(i)-x2(i))**2+(y3(i)-y2(i))**2
      side3   =x3(i)*x3(i)+y3(i)*y3(i)
      sidmn(i)=  min(side1,side2,side3)
      diagm(i)=.25*  max(side1,side2,side3)
   40 continue
c
      do 50 i=lft,llt
      area(i)=1./(sarea(i)+1.e-20)
      px1a(i)=area(i)*px1(i)
      py1a(i)=area(i)*py1(i)
      px2a(i)=area(i)*px2(i)
      py2a(i)=area(i)*py2(i)
      px3a(i)=area(i)*px3(i)
      py3a(i)=area(i)*py3(i)
      b1vx(i)= px1a(i)*vx1(i)+px2a(i)*vx2(i)+px3a(i)*vx3(i)
      b1vy(i)= px1a(i)*vy1(i)+px2a(i)*vy2(i)+px3a(i)*vy3(i)
      b2vy(i)= py1a(i)*vy1(i)+py2a(i)*vy2(i)+py3a(i)*vy3(i)
      b2vx(i)= py1a(i)*vx1(i)+py2a(i)*vx2(i)+py3a(i)*vx3(i)
      bxyv(i)=b2vx(i)+b1vy(i)
   50 continue
c
      return
      end
      subroutine crvtr(intp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute curvatures for triangular shell element
c
      common/auxjn/fctr(128,18)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),area(128),dxl(128)
c
      if (intp.eq.1) then
      do 10 i=lft,llt
      g1= x3(i)-x2(i)
      h1= y2(i)-y3(i)
      g2=-x3(i)
      h2= y3(i)
      g3= x2(i)
      h3=-y2(i)
      gg1=.5*g1*g1
      hh1=.5*h1*h1
      gg2=g2*g2
      hh2=h2*h2
      gg3=g3*g3
      hh3=h3*h3
      w1=g2-g3
      w2=g1-g3
      w3=g2-g1
      v1=h2-h3
      v2=h1-h3
      v3=h2-h1
      area4=4.*area(i)
      fctr(i, 1)=hh1*v1
      fctr(i, 2)=hh1*w1
      fctr(i, 3)=h1*(0.5*h2*(3.*h3-h2)+hh3)
      fctr(i, 4)=hh1*w2-h2*(g1*h2+area4)
      fctr(i, 5)=h1*(0.5*h3*(h3-3.*h2)-hh2)
      fctr(i, 6)=hh1*w3+h3*(g1*h3-area4)
      fctr(i, 7)=gg1*v1
      fctr(i, 8)=gg1*w1
      fctr(i, 9)=gg1*v2-g2*(g2*h1-area4)
      fctr(i,10)=g1*(0.5*g2*(3.*g3-g2)+gg3)
      fctr(i,11)=gg1*v3+g3*(g3*h1+area4)
      fctr(i,12)=g1*(0.5*g3*(g3-3.*g2)-gg2)
      fctr(i,13)=g1*h1*v1
      fctr(i,14)=g1*h1*w1
      fctr(i,15)=g1*(2.*h3*v3+h1*h2)
      fctr(i,16)=h1*(2.*g3*w3+g1*g2)
      fctr(i,17)=g1*(2.*h2*v2-h1*h3)
      fctr(i,18)=h1*(2.*g2*w2-g1*g3)
   10 continue
      elseif (intp.eq.2) then
      do 20 i=lft,llt
      g1= x3(i)-x2(i)
      h1= y2(i)-y3(i)
      g2=-x3(i)
      h2= y3(i)
      g3= x2(i)
      h3=-y2(i)
      gg1=g1*g1
      hh1=h1*h1
      gg2=.5*g2*g2
      hh2=.5*h2*h2
      gg3=g3*g3
      hh3=h3*h3
      w1=g3-g2
      w2=g3-g1
      w3=g2-g1
      v1=h3-h2
      v2=h3-h1
      v3=h2-h1
      area4=4.*area(i)
      fctr(i, 1)=h2*(0.5*h1*(h1-3.*h3)-hh3)
      fctr(i, 2)=hh2*w1+h1*(g2*h1-area4)
      fctr(i, 3)=hh2*v2
      fctr(i, 4)=hh2*w2
      fctr(i, 5)=h2*(0.5*h3*(3.*h1-h3)+hh1)
      fctr(i, 6)=hh2*w3-h3*(g2*h3+area4)
      fctr(i, 7)=gg2*v1+g1*(g1*h2+area4)
      fctr(i, 8)=g2*(0.5*g1*(g1-3.*g3)-gg3)
      fctr(i, 9)=gg2*v2
      fctr(i,10)=gg2*w2
      fctr(i,11)=gg2*v3-g3*(g3*h2-area4)
      fctr(i,12)=g2*(0.5*g3*(3.*g1-g3)+gg1)
      fctr(i,13)=g2*(2.*h3*v3-h1*h2)
      fctr(i,14)=h2*(2.*g3*w3-g1*g2)
      fctr(i,15)=g2*h2*v2
      fctr(i,16)=g2*h2*w2
      fctr(i,17)=g2*(2.*h1*v1+h2*h3)
      fctr(i,18)=h2*(2.*g1*w1+g2*g3)
   20 continue
      elseif (intp.eq.3) then
      do 30 i=lft,llt
      g1= x3(i)-x2(i)
      h1= y2(i)-y3(i)
      g2=-x3(i)
      h2= y3(i)
      g3= x2(i)
      h3=-y2(i)
      gg1=g1*g1
      hh1=h1*h1
      gg2=g2*g2
      hh2=h2*h2
      gg3=.5*g3*g3
      hh3=.5*h3*h3
      w1=g3-g2
      w2=g1-g3
      w3=g1-g2
      v1=h3-h2
      v2=h1-h3
      v3=h1-h2
      area4=4.*area(i)
      fctr(i, 1)=h3*(0.5*h1*(3.*h2-h1)+hh2)
      fctr(i, 2)=hh3*w1-h1*(g3*h1+area4)
      fctr(i, 3)=h3*(0.5*h2*(h2-3.*h1)-hh1)
      fctr(i, 4)=hh3*w2+h2*(g3*h2-area4)
      fctr(i, 5)=hh3*v3
      fctr(i, 6)=hh3*w3
      fctr(i, 7)=gg3*v1-g1*(g1*h3-area4)
      fctr(i, 8)=g3*(0.5*g1*(3.*g2-g1)+gg2)
      fctr(i, 9)=gg3*v2+g2*(g2*h3+area4)
      fctr(i,10)=g3*(0.5*g2*(g2-3.*g1)-gg1)
      fctr(i,11)=gg3*v3
      fctr(i,12)=gg3*w3
      fctr(i,13)=g3*(2.*h2*v2+h1*h3)
      fctr(i,14)=h3*(2.*g2*w2+g1*g3)
      fctr(i,15)=g3*(2.*h1*v1-h2*h3)
      fctr(i,16)=h3*(2.*g1*w1-g2*g3)
      fctr(i,17)=g3*h3*v3
      fctr(i,18)=g3*h3*w3
   30 continue
      endif
      do 40 i=lft,llt
      con=.25/(area(i)*area(i))
      fxx=fctr(i, 1)*vx5(i)+fctr(i, 2)*vy5(i)+fctr(i, 3)*vx6(i)
     1   +fctr(i, 4)*vy6(i)+fctr(i, 5)*vx7(i)+fctr(i, 6)*vy7(i)
      fyy=fctr(i, 7)*vx5(i)+fctr(i, 8)*vy5(i)+fctr(i, 9)*vx6(i)
     1   +fctr(i,10)*vy6(i)+fctr(i,11)*vx7(i)+fctr(i,12)*vy7(i)
      fxy=fctr(i,13)*vx5(i)+fctr(i,14)*vy5(i)+fctr(i,15)*vx6(i)
     1   +fctr(i,16)*vy6(i)+fctr(i,17)*vx7(i)+fctr(i,18)*vy7(i)
      b1ty(i)=con*fxx
      b2tx(i)=con*fyy
      bxyt(i)=con*fxy
   40 continue
      return
      end
      subroutine gthats (fibl,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhtnx4(128),yhtny4(128),yhtnz4(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128),
     & yhatx4(128),yhaty4(128),yhatz4(128)
      common/aux12/
     & dx13(128),dx24(128),det8(128),det4(128),
     & dy13(128),dy24(128),
     & dz13(128),dz24(128),
     & wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     & wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     & wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     &  a13(128), a23(128), a33(128),
     & fbl1(128),fbl2(128),fbl3(128),fbl4(128)
      dimension fibl(9,1)
c
      call rwingv(wxx1,wyy1,wzz1,lft,llt)
      do 10 i=lft,llt
      yhatx1(i)=rot1(i)*yhtnx1(i)+rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
      yhaty1(i)=rot2(i)*yhtnx1(i)+rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
      yhatz1(i)=rot3(i)*yhtnx1(i)+rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
   10 continue
c
      call rwingv(wxx2,wyy2,wzz2,lft,llt)
      do 20 i=lft,llt
      yhatx2(i)=rot1(i)*yhtnx2(i)+rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
      yhaty2(i)=rot2(i)*yhtnx2(i)+rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
      yhatz2(i)=rot3(i)*yhtnx2(i)+rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
   20 continue
c
      call rwingv(wxx3,wyy3,wzz3,lft,llt)
      do 30 i=lft,llt
      yhatx3(i)=rot1(i)*yhtnx3(i)+rot4(i)*yhtny3(i)+rot7(i)*yhtnz3(i)
      yhaty3(i)=rot2(i)*yhtnx3(i)+rot5(i)*yhtny3(i)+rot8(i)*yhtnz3(i)
      yhatz3(i)=rot3(i)*yhtnx3(i)+rot6(i)*yhtny3(i)+rot9(i)*yhtnz3(i)
   30 continue
c
      call rwingv(wxx4,wyy4,wzz4,lft,llt)
      do 40 i=lft,llt
      yhatx4(i)=rot1(i)*yhtnx4(i)+rot4(i)*yhtny4(i)+rot7(i)*yhtnz4(i)
      yhaty4(i)=rot2(i)*yhtnx4(i)+rot5(i)*yhtny4(i)+rot8(i)*yhtnz4(i)
      yhatz4(i)=rot3(i)*yhtnx4(i)+rot6(i)*yhtny4(i)+rot9(i)*yhtnz4(i)
   40 continue
c
      do 50 i=lft,llt
      dx5(i)=yhatx1(i)-yhtnx1(i)
      dy5(i)=yhaty1(i)-yhtny1(i)
      dz5(i)=yhatz1(i)-yhtnz1(i)
      dx6(i)=yhatx2(i)-yhtnx2(i)
      dy6(i)=yhaty2(i)-yhtny2(i)
      dz6(i)=yhatz2(i)-yhtnz2(i)
      dx7(i)=yhatx3(i)-yhtnx3(i)
      dy7(i)=yhaty3(i)-yhtny3(i)
      dz7(i)=yhatz3(i)-yhtnz3(i)
      dx8(i)=yhatx4(i)-yhtnx4(i)
      dy8(i)=yhaty4(i)-yhtny4(i)
      dz8(i)=yhatz4(i)-yhtnz4(i)
   50 continue
c
      do 60 i=lft,llt
      fbl1(i)=fibl(1,i)
      fbl2(i)=fibl(2,i)
      fbl3(i)=fibl(3,i)
      fbl4(i)=fibl(4,i)
      a13(i)=.125*(fbl1(i)*yhatx1(i)+fbl2(i)*yhatx2(i)
     1            +fbl3(i)*yhatx3(i)+fbl4(i)*yhatx4(i))
      a23(i)=.125*(fbl1(i)*yhaty1(i)+fbl2(i)*yhaty2(i)
     1            +fbl3(i)*yhaty3(i)+fbl4(i)*yhaty4(i))
      a33(i)=.125*(fbl1(i)*yhatz1(i)+fbl2(i)*yhatz2(i)
     1            +fbl3(i)*yhatz3(i)+fbl4(i)*yhatz4(i))
      dx13(i)=dx1(i)-dx3(i)
      dx24(i)=dx2(i)-dx4(i)
      dy13(i)=dy1(i)-dy3(i)
      dy24(i)=dy2(i)-dy4(i)
      dz13(i)=dz1(i)-dz3(i)
      dz24(i)=dz2(i)-dz4(i)
   60 continue
      return
      end
      subroutine gtmnts
c     implicit double precision (a-h,o-z)                                    dp
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhtnx4(128),yhtny4(128),yhtnz4(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128),
     & yhatx4(128),yhaty4(128),yhatz4(128)
      common/aux36/lft,llt
      dimension c1(128),c2(128)
      do 10 i=lft,llt
      c1(i)  =-yhatz1(i)*ep25(i)+yhaty1(i)*ep35(i)
      c2(i)  = yhatz1(i)*ep15(i)-yhatx1(i)*ep35(i)
      ep35(i)=-yhaty1(i)*ep15(i)+yhatx1(i)*ep25(i)
      ep15(i)=c1(i)
      ep25(i)=c2(i)
   10 continue
      do 20 i=lft,llt
      c1(i)  =-yhatz2(i)*ep26(i)+yhaty2(i)*ep36(i)
      c2(i)  = yhatz2(i)*ep16(i)-yhatx2(i)*ep36(i)
      ep36(i)=-yhaty2(i)*ep16(i)+yhatx2(i)*ep26(i)
      ep16(i)=c1(i)
      ep26(i)=c2(i)
   20 continue
      do 30 i=lft,llt
      c1(i)  =-yhatz3(i)*ep27(i)+yhaty3(i)*ep37(i)
      c2(i)  = yhatz3(i)*ep17(i)-yhatx3(i)*ep37(i)
      ep37(i)=-yhaty3(i)*ep17(i)+yhatx3(i)*ep27(i)
      ep17(i)=c1(i)
      ep27(i)=c2(i)
   30 continue
      do 40 i=lft,llt
      c1(i)  =-yhatz4(i)*ep28(i)+yhaty4(i)*ep38(i)
      c2(i)  = yhatz4(i)*ep18(i)-yhatx4(i)*ep38(i)
      ep38(i)=-yhaty4(i)*ep18(i)+yhatx4(i)*ep28(i)
      ep18(i)=c1(i)
      ep28(i)=c2(i)
   40 continue
      return
      end
      subroutine frc4ns(e,f,fibl,qs,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk00/numnp,numpc,numlp,neq,ndofs,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &ft31(128),ft32(128),ft33(128),ft41(128),ft42(128),ft43(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &htx(128),hty(128),gm1(128),gm2(128),gm3(128),gm4(128),
     &bsum(128),qhx(128),qhy(128),qwz(128),qtx(128),qty(128)
      common/aux13/
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux19/c1(128),c2(128),c3(128),xl(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      dimension e(3,*),f(3,*),fibl(9,*),qs(9,*)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension iblks(*)
      tmode=qhg*ymod/1920.0
      ifail=0
      wmode=qhg*gmod/120.00
      xmmode=qhg*ymod/80.000
      do 10 i=lft,llt
      thick(i) =.25*(fibl(1,i)+fibl(2,i)+fibl(3,i)+fibl(4,i))
      htx(i)=area(i)*(x3(i)-x2(i)-x4(i))
      hty(i)=area(i)*(y3(i)-y2(i)-y4(i))
      gm1(i)= 1.-px1(i)*htx(i)-py1(i)*hty(i)
      gm2(i)=-1.-px2(i)*htx(i)-py2(i)*hty(i)
      gm3(i)= 2.-gm1(i)
      gm4(i)=-2.-gm2(i)
   10 continue
      if (ifsv.eq.3) then
      do 12 i=lft,llt
      bsum(i) =2.*(px1(i)**2+px2(i)**2+py1(i)**2+py2(i)**2)
      xl(i)=area(i)*bsum(i)*thick(i)
      c1(i)=xl(i)*thick(i)**2
      c2(i)=wmode*c1(i)*area(i)
      c3(i)=xmmode*xl(i)
      c1(i)=tmode*c1(i)
   12 continue
      else
      hgfac=qhg*rhoa(lft)*sndspd/(dt1+1.e-20)
      do 15 i=lft,llt
      c3(i)=hgfac*sqrt(sarea(i))*thick(i)
      c2(i)=c3(i)
      c1(i)=.05*c3(i)*thick(i)
   15 continue
      endif
      do 20 i=lft,llt
      qhx(i)=gm1(i)*vx1(i)+gm2(i)*vx2(i)+gm3(i)*vx3(i)+gm4(i)*vx4(i)
      qhy(i)=gm1(i)*vy1(i)+gm2(i)*vy2(i)+gm3(i)*vy3(i)+gm4(i)*vy4(i)
      qwz(i)=gm1(i)*vz1(i)+gm2(i)*vz2(i)+gm3(i)*vz3(i)+gm4(i)*vz4(i)
      qtx(i)=gm1(i)*vx5(i)+gm2(i)*vx6(i)+gm3(i)*vx7(i)+gm4(i)*vx8(i)
      qty(i)=gm1(i)*vy5(i)+gm2(i)*vy6(i)+gm3(i)*vy7(i)+gm4(i)*vy8(i)
   20 continue
      if (ifsv.ne.3) then
      do 25 i=lft,llt
      qs(1,i)=0.
      qs(2,i)=0.
      qs(3,i)=0.
      qs(4,i)=0.
      qs(5,i)=0.
   25 continue
      endif
      do 30 i=lft,llt
      qs(1,i)=qs(1,i)+c3(i)*qhx(i)
      qs(2,i)=qs(2,i)+c3(i)*qhy(i)
      qs(3,i)=qs(3,i)+c2(i)*qwz(i)
      qs(4,i)=qs(4,i)+c1(i)*qtx(i)
      qs(5,i)=qs(5,i)+c1(i)*qty(i)
      ft11(i)=gm1(i)*qs(1,i)
      ft12(i)=gm1(i)*qs(2,i)
      ft13(i)=gm1(i)*qs(3,i)
      ft21(i)=gm2(i)*qs(1,i)
      ft22(i)=gm2(i)*qs(2,i)
      ft23(i)=gm2(i)*qs(3,i)
      ft31(i)=gm3(i)*qs(1,i)
      ft32(i)=gm3(i)*qs(2,i)
      ft33(i)=gm3(i)*qs(3,i)
      ft41(i)=gm4(i)*qs(1,i)
      ft42(i)=gm4(i)*qs(2,i)
      ft43(i)=gm4(i)*qs(3,i)
      fm11(i)=gm1(i)*qs(4,i)
      fm12(i)=gm1(i)*qs(5,i)
      fm21(i)=gm2(i)*qs(4,i)
      fm22(i)=gm2(i)*qs(5,i)
      fm31(i)=gm3(i)*qs(4,i)
      fm32(i)=gm3(i)*qs(5,i)
      fm41(i)=gm4(i)*qs(4,i)
      fm42(i)=gm4(i)*qs(5,i)
   30 continue
      do 50 i=lft,llt
      fx1(i)=s11(i)*ft11(i)+s21(i)*ft12(i)+s31(i)*ft13(i)-ep11(i)
      fy1(i)=s12(i)*ft11(i)+s22(i)*ft12(i)+s32(i)*ft13(i)-ep21(i)
      fz1(i)=s13(i)*ft11(i)+s23(i)*ft12(i)+s33(i)*ft13(i)-ep31(i)
      fx2(i)=s11(i)*ft21(i)+s21(i)*ft22(i)+s31(i)*ft23(i)-ep12(i)
      fy2(i)=s12(i)*ft21(i)+s22(i)*ft22(i)+s32(i)*ft23(i)-ep22(i)
      fz2(i)=s13(i)*ft21(i)+s23(i)*ft22(i)+s33(i)*ft23(i)-ep32(i)
      fx3(i)=s11(i)*ft31(i)+s21(i)*ft32(i)+s31(i)*ft33(i)-ep13(i)
      fy3(i)=s12(i)*ft31(i)+s22(i)*ft32(i)+s32(i)*ft33(i)-ep23(i)
      fz3(i)=s13(i)*ft31(i)+s23(i)*ft32(i)+s33(i)*ft33(i)-ep33(i)
      fx4(i)=s11(i)*ft41(i)+s21(i)*ft42(i)+s31(i)*ft43(i)-ep14(i)
      fy4(i)=s12(i)*ft41(i)+s22(i)*ft42(i)+s32(i)*ft43(i)-ep24(i)
      fz4(i)=s13(i)*ft41(i)+s23(i)*ft42(i)+s33(i)*ft43(i)-ep34(i)
      xmx1(i)=s11(i)*fm11(i)+s21(i)*fm12(i)               -ep15(i)
      xmy1(i)=s12(i)*fm11(i)+s22(i)*fm12(i)               -ep25(i)
      xmz1(i)=s13(i)*fm11(i)+s23(i)*fm12(i)               -ep35(i)
      xmx2(i)=s11(i)*fm21(i)+s21(i)*fm22(i)               -ep16(i)
      xmy2(i)=s12(i)*fm21(i)+s22(i)*fm22(i)               -ep26(i)
      xmz2(i)=s13(i)*fm21(i)+s23(i)*fm22(i)               -ep36(i)
      xmx3(i)=s11(i)*fm31(i)+s21(i)*fm32(i)               -ep17(i)
      xmy3(i)=s12(i)*fm31(i)+s22(i)*fm32(i)               -ep27(i)
      xmz3(i)=s13(i)*fm31(i)+s23(i)*fm32(i)               -ep37(i)
      xmx4(i)=s11(i)*fm41(i)+s21(i)*fm42(i)               -ep18(i)
      xmy4(i)=s12(i)*fm41(i)+s22(i)*fm42(i)               -ep28(i)
      xmz4(i)=s13(i)*fm41(i)+s23(i)*fm42(i)               -ep38(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-xmy3(i)
   65 f(3,ix3(i))=f(3,ix3(i))-xmz3(i)
cdir$ ivdep
      do 70 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fz4(i)
      f(1,ix4(i))=f(1,ix4(i))-xmx4(i)
      f(2,ix4(i))=f(2,ix4(i))-xmy4(i)
   70 f(3,ix4(i))=f(3,ix4(i))-xmz4(i)
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=4
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
      subroutine hldfls
c     implicit double precision (a-h,o-z)                                    dp
c
c     lamina coordinate system
c
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux19/
     &x31(128),y31(128),z31(128),
     &x21(128),y21(128),z21(128),
     &x41(128),y41(128),z41(128),
     &x42(128),y42(128),z42(128)
      common/aux40/
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux36/lft,llt
c
      do 10 i=lft,llt
      x21(i)=x2(i)-x1(i)
      y21(i)=y2(i)-y1(i)
      z21(i)=z2(i)-z1(i)
      x31(i)=x3(i)-x1(i)
      y31(i)=y3(i)-y1(i)
      z31(i)=z3(i)-z1(i)
      x41(i)=x4(i)-x1(i)
      y41(i)=y4(i)-y1(i)
      z41(i)=z4(i)-z1(i)
   10 continue
c
      do 20 i=lft,llt
      x42(i)=x4(i)-x2(i)
      y42(i)=y4(i)-y2(i)
      z42(i)=z4(i)-z2(i)
      c1=y31(i)*z42(i)-z31(i)*y42(i)
      c2=z31(i)*x42(i)-x31(i)*z42(i)
      c3=x31(i)*y42(i)-y31(i)*x42(i)
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s31(i)=c1*xl
      s32(i)=c2*xl
      s33(i)=c3*xl
   20 continue
c
      do 30 i=lft,llt
      xl=x21(i)*s31(i)+y21(i)*s32(i)+z21(i)*s33(i)
      c1=x21(i)-s31(i)*xl
      c2=y21(i)-s32(i)*xl
      c3=z21(i)-s33(i)*xl
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s11(i)=c1*xl
      s12(i)=c2*xl
      s13(i)=c3*xl
      s21(i)=s32(i)*s13(i)-s33(i)*s12(i)
      s22(i)=s33(i)*s11(i)-s31(i)*s13(i)
      s23(i)=s31(i)*s12(i)-s32(i)*s11(i)
   30 continue
      return
      end
      subroutine hltran
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     & dx13(128),dx24(128),det8(128),det4(128),
     & dy13(128),dy24(128),
     & dz13(128),dz24(128),
     & wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     & wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     & wzz1(128),wzz2(128),wzz3(128),wzz4(128)
      common/aux19/
     &x31(128),y31(128),z31(128),
     &x21(128),y21(128),z21(128),
     &x41(128),y41(128),z41(128)
      common/aux40/
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux36/lft,llt
      common/sides/sidmn(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/double/iprec,ncpw,unit
c
      dimension sidem(128)
c
      do 10 i=lft,llt
      vx1(i)=s11(i)*dx1(i) +s12(i)*dy1(i) +s13(i)*dz1(i)
      vy1(i)=s21(i)*dx1(i) +s22(i)*dy1(i) +s23(i)*dz1(i)
      vz1(i)=s31(i)*dx1(i) +s32(i)*dy1(i) +s33(i)*dz1(i)
      vx2(i)=s11(i)*dx2(i) +s12(i)*dy2(i) +s13(i)*dz2(i)
      vy2(i)=s21(i)*dx2(i) +s22(i)*dy2(i) +s23(i)*dz2(i)
      vz2(i)=s31(i)*dx2(i) +s32(i)*dy2(i) +s33(i)*dz2(i)
      vx3(i)=s11(i)*dx3(i) +s12(i)*dy3(i) +s13(i)*dz3(i)
      vy3(i)=s21(i)*dx3(i) +s22(i)*dy3(i) +s23(i)*dz3(i)
      vz3(i)=s31(i)*dx3(i) +s32(i)*dy3(i) +s33(i)*dz3(i)
      vx4(i)=s11(i)*dx4(i) +s12(i)*dy4(i) +s13(i)*dz4(i)
      vy4(i)=s21(i)*dx4(i) +s22(i)*dy4(i) +s23(i)*dz4(i)
      vz4(i)=s31(i)*dx4(i) +s32(i)*dy4(i) +s33(i)*dz4(i)
   10 continue
      do 20 i=lft,llt
      vx5(i)=s11(i)*wxx1(i)+s12(i)*wyy1(i)+s13(i)*wzz1(i)
      vy5(i)=s21(i)*wxx1(i)+s22(i)*wyy1(i)+s23(i)*wzz1(i)
      vx6(i)=s11(i)*wxx2(i)+s12(i)*wyy2(i)+s13(i)*wzz2(i)
      vy6(i)=s21(i)*wxx2(i)+s22(i)*wyy2(i)+s23(i)*wzz2(i)
      vx7(i)=s11(i)*wxx3(i)+s12(i)*wyy3(i)+s13(i)*wzz3(i)
      vy7(i)=s21(i)*wxx3(i)+s22(i)*wyy3(i)+s23(i)*wzz3(i)
      vx8(i)=s11(i)*wxx4(i)+s12(i)*wyy4(i)+s13(i)*wzz4(i)
      vy8(i)=s21(i)*wxx4(i)+s22(i)*wyy4(i)+s23(i)*wzz4(i)
      x2(i) =s11(i)*x21(i) +s12(i)*y21(i) +s13(i)*z21(i)
      y2(i) =s21(i)*x21(i) +s22(i)*y21(i) +s23(i)*z21(i)
      x3(i) =s11(i)*x31(i) +s12(i)*y31(i) +s13(i)*z31(i)
      y3(i) =s21(i)*x31(i) +s22(i)*y31(i) +s23(i)*z31(i)
      x4(i) =s11(i)*x41(i) +s12(i)*y41(i) +s13(i)*z41(i)
      y4(i) =s21(i)*x41(i) +s22(i)*y41(i) +s23(i)*z41(i)
   20 continue
c
      do 40 i=lft,llt
      px1(i)  = .5*(y2(i)-y4(i))
      px2(i)  = .5* y3(i)
      py1(i)  =-.5*(x2(i)-x4(i))
      py2(i)  =-.5* x3(i)
      sarea(i)=2.0*(py2(i)*px1(i)-py1(i)*px2(i))
      diag1=x3(i)**2+y3(i)**2
      diag2=4.*(px1(i)*px1(i)+py1(i)*py1(i))
      diagm(i)=  max(diag1,diag2)
      side1   =x2(i)*x2(i)+y2(i)*y2(i)
      side2   =(x3(i)-x2(i))**2+(y3(i)-y2(i))**2
      side3   =(x4(i)-x3(i))**2+(y4(i)-y3(i))**2-1.e-14
      side4   =x4(i)*x4(i)+y4(i)*y4(i)
      sida3   =side4*(5.-sign(.5*unit,side3))+side3
      sidmn(i)=  min(side1,side2,sida3,side4)
      sidem(i)=  max(side1,side2,side3,side4)
     1  *(.625+sign(.375*unit,side3))
      area(i)=1./(sarea(i)+1.e-20)
   40 continue
c
      if (isdo.eq.0.or.isdo.eq.2) then
      do 60 i=lft,llt
   60 diagm(i)=  min(diagm(i),sidem(i))
      return
      else
      return
      endif
      end
      subroutine unpcts(ixp,nmel)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),matp(128)
      common/vect13/
     1 kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
      dimension ixp(5,1)                                                vax75
c     dimension ixp(2,1)                                                cray1
c     do 10 i=1,nmel                                                    cray1
c     kka(i)=ixp(1,i)                                                   cray1
c     kkb(i)=ixp(2,i)                                                   cray1
c  10 continue                                                          cray1
c     do 20 i=1,nmel                                                    cray1
c     matp(i)=and(kka(i),3777777b)                                      cray1
c     ix3(i) =and(kkb(i),3777777b)                                      cray1
c  20 continue                                                          cray1
c     do 30 i=1,nmel                                                    cray1
c     kk1(i)=shiftr(kka(i),21)                                          cray1
c     kk2(i)=shiftr(kkb(i),21)                                          cray1
c  30 continue                                                          cray1
c     do 40 i=1,nmel                                                    cray1
c     kka(i)=shiftr(kk1(i),21)                                          cray1
c     kkb(i)=shiftr(kk2(i),21)                                          cray1
c  40 continue                                                          cray1
c     do 50 i=1,nmel                                                    cray1
c     ix1(i)=and(kk1(i),3777777b)                                       cray1
c     ix2(i)=and(kka(i),3777777b)                                       cray1
c     ix4(i)=and(kk2(i),3777777b)                                       cray1
c  50 continue                                                          cray1
      do 10 i=1,nmel                                                    vax75
      matp(i)=ixp(1,i)                                                  vax75
      ix1(i) =ixp(2,i)                                                  vax75
      ix2(i) =ixp(3,i)                                                  vax75
      ix3(i) =ixp(4,i)                                                  vax75
      ix4(i) =ixp(5,i)                                                  vax75
   10 continue                                                          vax75
      return
      end
      subroutine forcis(e,wgt,volf,epf,ioffst,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux9/vlrho(128),vol(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/po(128),
     &sgw1(128),sgw2(128),sgw3(128),sgw4(128),sgw5(128),sgw6(128),
     &sgv1(128),sgv2(128),sgv3(128),sgv4(128),sgv5(128),sgv6(128),
     &e11(128),e21(128),e31(128),e12(128),e22(128),e32(128),
     &e13(128),e23(128),e33(128),e14(128),e24(128),e34(128),
     &e15(128),e25(128),e35(128),e16(128),e26(128),e36(128),
     &e17(128),e27(128),e37(128),e18(128),e28(128),e38(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     &             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/presc/voltot(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
c
      dimension e(3,*),volf(*),epf(*),xies(*)
c
      wgt2=4.*wgt
      do 10 i=lft,llt
      vol(i)=wgt2/vol(i)
      voltot(i)=voltot(i)+vol(i)
      sgv1(i)=sig1(i)*vol(i)
      sgv2(i)=sig2(i)*vol(i)
      sgv3(i)=sig3(i)*vol(i)
      sgv4(i)=sig4(i)*vol(i)
      sgv5(i)=sig5(i)*vol(i)
      sgv6(i)=sig6(i)*vol(i)
   10 xies(i) =xies(i) +0.50*vol(i)*einc(i)
c
c     stress divergence
c
      do 110 i=lft,llt
      e11(i)=sgv1(i)*px1(i)+sgv4(i)*py1(i)+sgv6(i)*pz1(i)
      e21(i)=sgv2(i)*py1(i)+sgv4(i)*px1(i)+sgv5(i)*pz1(i)
      e31(i)=sgv3(i)*pz1(i)+sgv6(i)*px1(i)+sgv5(i)*py1(i)
      e12(i)=sgv1(i)*px2(i)+sgv4(i)*py2(i)+sgv6(i)*pz2(i)
      e22(i)=sgv2(i)*py2(i)+sgv4(i)*px2(i)+sgv5(i)*pz2(i)
      e32(i)=sgv3(i)*pz2(i)+sgv6(i)*px2(i)+sgv5(i)*py2(i)
      e15(i)=sgv1(i)*px5(i)+sgv4(i)*py5(i)+sgv6(i)*pz5(i)
      e25(i)=sgv2(i)*py5(i)+sgv4(i)*px5(i)+sgv5(i)*pz5(i)
      e35(i)=sgv3(i)*pz5(i)+sgv6(i)*px5(i)+sgv5(i)*py5(i)
      e16(i)=sgv1(i)*px6(i)+sgv4(i)*py6(i)+sgv6(i)*pz6(i)
      e26(i)=sgv2(i)*py6(i)+sgv4(i)*px6(i)+sgv5(i)*pz6(i)
      e36(i)=sgv3(i)*pz6(i)+sgv6(i)*px6(i)+sgv5(i)*py6(i)
      e17(i)=sgv1(i)*px7(i)+sgv4(i)*py7(i)+sgv6(i)*pz7(i)
      e27(i)=sgv2(i)*py7(i)+sgv4(i)*px7(i)+sgv5(i)*pz7(i)
      e37(i)=sgv3(i)*pz7(i)+sgv6(i)*px7(i)+sgv5(i)*py7(i)
      e18(i)=sgv1(i)*px8(i)+sgv4(i)*py8(i)+sgv6(i)*pz8(i)
      e28(i)=sgv2(i)*py8(i)+sgv4(i)*px8(i)+sgv5(i)*pz8(i)
      e38(i)=sgv3(i)*pz8(i)+sgv6(i)*px8(i)+sgv5(i)*py8(i)
  110 continue
      do 120 i=lft,llt
      ep11(i)=ep11(i)-e11(i)
      ep21(i)=ep21(i)-e21(i)
      ep31(i)=ep31(i)-e31(i)
      ep12(i)=ep12(i)-e12(i)
      ep22(i)=ep22(i)-e22(i)
      ep32(i)=ep32(i)-e32(i)
      ep13(i)=ep13(i)+e11(i)
      ep23(i)=ep23(i)+e21(i)
      ep33(i)=ep33(i)+e31(i)
      ep14(i)=ep14(i)+e12(i)
      ep24(i)=ep24(i)+e22(i)
      ep34(i)=ep34(i)+e32(i)
      ep15(i)=ep15(i)-e15(i)
      ep25(i)=ep25(i)-e25(i)
      ep35(i)=ep35(i)-e35(i)
      ep16(i)=ep16(i)-e16(i)
      ep26(i)=ep26(i)-e26(i)
      ep36(i)=ep36(i)-e36(i)
      ep17(i)=ep17(i)-e17(i)
      ep27(i)=ep27(i)-e27(i)
      ep37(i)=ep37(i)-e37(i)
      ep18(i)=ep18(i)-e18(i)
      ep28(i)=ep28(i)-e28(i)
      ep38(i)=ep38(i)-e38(i)
  120 continue
      if (ntbsl.eq.0.and.ntnwf.eq.0) return
      do 130 i=lft,llt
      volf(i+ioffst)=volf(i+ioffst)+vol(i)
      epf(i+ioffst)=epf(i+ioffst)+epx1(i)*vol(i)
  130 continue
      return
      end
      subroutine stdsps(zeta,zbar)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     1  z1(128), z2(128), z3(128), z4(128),
     2 z1h(128),z2h(128),z3h(128),z4h(128),
     3  c1(128), c2(128), c3(128),enrm(128),
     4 xgj11(128),xgj21(128),xgj31(128),
     5 xgj12(128),xgj22(128),xgj32(128),
     6 xgj13(128),xgj23(128),xgj33(128)
      common/aux9/vlrho(128),det(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux12/
     & dx13(128),dx24(128),det8(128),det4(128),
     & dy13(128),dy24(128),
     & dz13(128),dz24(128),
     & wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     & wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     & wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     &  a13(128), a23(128), a33(128),
     & fbl1(128),fbl2(128),fbl3(128),fbl4(128)
      common/aux13/
     & cx1(128),cy1(128),cz1(128),cx2(128),cy2(128),cz2(128),
     & cx3(128),cy3(128),cz3(128),cx4(128),cy4(128),cz4(128),
     &  xnd11(128), xnd21(128), xnd31(128),
     &  xnd12(128), xnd22(128), xnd32(128),
     &  xnd13(128), xnd23(128), xnd33(128),
     &  xnd14(128), xnd24(128), xnd34(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128),
     & yhatx4(128),yhaty4(128),yhatz4(128)
      common/aux19/
     &x31(128),y31(128),z31(128),
     &x21(128),y21(128),z21(128),
     &x41(128),y41(128),z41(128),
     &x42(128),y42(128),z42(128)
      common/aux32/
     1 dxy(128),dyx(128),dyz(128),dzy(128),dzx(128),dxz(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      dimension zbar(*)
c
      zzbar1=.5*(zeta-zbar(1))
      zzbar2=.5*(zeta-zbar(2))
      zzbar3=.5*(zeta-zbar(3))
      zzbar4=.5*(zeta-zbar(4))
      do 10 i=lft,llt
      einc(i)=0.0
      z1h(i) =fbl1(i)*zzbar1
      z2h(i) =fbl2(i)*zzbar2
      z3h(i) =fbl3(i)*zzbar3
      z4h(i) =fbl4(i)*zzbar4
      xnd13(i)=x31(i)+z3h(i)*yhatx3(i)-z1h(i)*yhatx1(i)
      xnd23(i)=y31(i)+z3h(i)*yhaty3(i)-z1h(i)*yhaty1(i)
      xnd33(i)=z31(i)+z3h(i)*yhatz3(i)-z1h(i)*yhatz1(i)
      xnd14(i)=x42(i)+z4h(i)*yhatx4(i)-z2h(i)*yhatx2(i)
      xnd24(i)=y42(i)+z4h(i)*yhaty4(i)-z2h(i)*yhaty2(i)
      xnd34(i)=z42(i)+z4h(i)*yhatz4(i)-z2h(i)*yhatz2(i)
   10 continue
      do 30 i=lft,llt
      a11(i)=.25*(xnd13(i)-xnd14(i))
      a21(i)=.25*(xnd23(i)-xnd24(i))
      a31(i)=.25*(xnd33(i)-xnd34(i))
      a12(i)=.25*(xnd13(i)+xnd14(i))
      a22(i)=.25*(xnd23(i)+xnd24(i))
      a32(i)=.25*(xnd33(i)+xnd34(i))
   30 continue
      do 50 i=lft,llt
      c1(i)=a22(i)*a33(i)-a32(i)*a23(i)
      c2(i)=a32(i)*a13(i)-a12(i)*a33(i)
      c3(i)=a12(i)*a23(i)-a13(i)*a22(i)
      det(i)=.25/(a11(i)*c1(i)+a21(i)*c2(i)+a31(i)*c3(i))
      xgj11(i)= det(i)*c1(i)
      xgj21(i)= det(i)*c2(i)
      xgj31(i)= det(i)*c3(i)
      xgj12(i)=-det(i)*(a21(i)*a33(i)-a23(i)*a31(i))
      xgj22(i)= det(i)*(a11(i)*a33(i)-a13(i)*a31(i))
      xgj32(i)=-det(i)*(a11(i)*a23(i)-a13(i)*a21(i))
      xgj13(i)= .500*det(i)*(a21(i)*a32(i)-a22(i)*a31(i))
      xgj23(i)=-.500*det(i)*(a11(i)*a32(i)-a12(i)*a31(i))
      xgj33(i)= .500*det(i)*(a11(i)*a22(i)-a12(i)*a21(i))
      det(i)  =4.*det(i)
   50 continue
      do 90 i=lft,llt
      px1(i)= -xgj11(i)-xgj12(i)
      px2(i)=  xgj11(i)-xgj12(i)
      px5(i)= z1h(i)*px1(i)+fbl1(i)*xgj13(i)
      px6(i)= z2h(i)*px2(i)+fbl2(i)*xgj13(i)
      px7(i)=-z3h(i)*px1(i)+fbl3(i)*xgj13(i)
      px8(i)=-z4h(i)*px2(i)+fbl4(i)*xgj13(i)
      py1(i)= -xgj21(i)-xgj22(i)
      py2(i)=  xgj21(i)-xgj22(i)
      py5(i)= z1h(i)*py1(i)+fbl1(i)*xgj23(i)
      py6(i)= z2h(i)*py2(i)+fbl2(i)*xgj23(i)
      py7(i)=-z3h(i)*py1(i)+fbl3(i)*xgj23(i)
      py8(i)=-z4h(i)*py2(i)+fbl4(i)*xgj23(i)
      pz1(i)= -xgj31(i)-xgj32(i)
      pz2(i)=  xgj31(i)-xgj32(i)
      pz5(i)= z1h(i)*pz1(i)+fbl1(i)*xgj33(i)
      pz6(i)= z2h(i)*pz2(i)+fbl2(i)*xgj33(i)
      pz7(i)=-z3h(i)*pz1(i)+fbl3(i)*xgj33(i)
      pz8(i)=-z4h(i)*pz2(i)+fbl4(i)*xgj33(i)
   90 continue
      do 110  i=lft,llt
      dxx(i)= px1(i)*dx13(i)+px2(i)*dx24(i)+
     & px5(i)*dx5(i)+px6(i)*dx6(i)+px7(i)*dx7(i)+px8(i)*dx8(i)
      dyy(i)= py1(i)*dy13(i)+py2(i)*dy24(i)+
     & py5(i)*dy5(i)+py6(i)*dy6(i)+py7(i)*dy7(i)+py8(i)*dy8(i)
      dzz(i)= pz1(i)*dz13(i)+pz2(i)*dz24(i)+
     & pz5(i)*dz5(i)+pz6(i)*dz6(i)+pz7(i)*dz7(i)+pz8(i)*dz8(i)
      dxy(i)= py1(i)*dx13(i)+py2(i)*dx24(i)+
     & py5(i)*dx5(i)+py6(i)*dx6(i)+py7(i)*dx7(i)+py8(i)*dx8(i)
      dxz(i)= pz1(i)*dx13(i)+pz2(i)*dx24(i)+
     & pz5(i)*dx5(i)+pz6(i)*dx6(i)+pz7(i)*dx7(i)+pz8(i)*dx8(i)
      dyz(i)= pz1(i)*dy13(i)+pz2(i)*dy24(i)+
     & pz5(i)*dy5(i)+pz6(i)*dy6(i)+pz7(i)*dy7(i)+pz8(i)*dy8(i)
      dyx(i)= px1(i)*dy13(i)+px2(i)*dy24(i)+
     & px5(i)*dy5(i)+px6(i)*dy6(i)+px7(i)*dy7(i)+px8(i)*dy8(i)
      dzx(i)= px1(i)*dz13(i)+px2(i)*dz24(i)+
     & px5(i)*dz5(i)+px6(i)*dz6(i)+px7(i)*dz7(i)+px8(i)*dz8(i)
      dzy(i)= py1(i)*dz13(i)+py2(i)*dz24(i)+
     & py5(i)*dz5(i)+py6(i)*dz6(i)+py7(i)*dz7(i)+py8(i)*dz8(i)
  110 continue
      do 120 i=lft,llt
      d1(i)   =dxy(i)+dyx(i)
      d2(i)   =dyz(i)+dzy(i)
      d3(i)   =dxz(i)+dzx(i)
      wzzdt(i)=.5*(dyx(i)-dxy(i))
      wyydt(i)=.5*(dxz(i)-dzx(i))
  120 wxxdt(i)=.5*(dzy(i)-dyz(i))
      return
      end
      subroutine rttrns(strain)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),t7(128,2),
     2 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     3 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      dimension strain(12,1)
      do 10 i=lft,llt
      s1(i)=strain(1,i)
      s2(i)=strain(2,i)
      s3(i)=strain(3,i)
      s4(i)=strain(4,i)
      s5(i)=strain(5,i)
   10 s6(i)=strain(6,i)
      do 20 i=lft,llt
      q1(i)=2.*s4(i)*wzz(i)
      q2(i)=2.*s6(i)*wyy(i)
      q3(i)=2.*s5(i)*wxx(i)
      strain(1,i)=s1(i)-q1(i)+q2(i)
      strain(2,i)=s2(i)+q1(i)-q3(i)
      strain(3,i)=s3(i)-q2(i)+q3(i)
      strain(4,i)=s4(i)+wzz(i)*(s1(i)-s2(i))+wyy(i)*s5(i)-wxx(i)*s6(i)
      strain(5,i)=s5(i)+wxx(i)*(s2(i)-s3(i))+wzz(i)*s6(i)-wyy(i)*s4(i)
   20 strain(6,i)=s6(i)+wyy(i)*(s3(i)-s1(i))+wxx(i)*s4(i)-wzz(i)*s5(i)
      do 30 i=lft,llt
      strain(1,i)=strain(1,i)+dxx(i)
      strain(2,i)=strain(2,i)+dyy(i)
      strain(3,i)=strain(3,i)+dzz(i)
      strain(4,i)=strain(4,i)+.50*d1(i)
      strain(5,i)=strain(5,i)+.50*d2(i)
      strain(6,i)=strain(6,i)+.50*d3(i)
   30 continue
      do 40 i=lft,llt
      a11(i)=strain(1,i)*s11(i)+strain(4,i)*s12(i)+strain(6,i)*s13(i)
      a12(i)=strain(1,i)*s21(i)+strain(4,i)*s22(i)+strain(6,i)*s23(i)
      a13(i)=strain(1,i)*s31(i)+strain(4,i)*s32(i)+strain(6,i)*s33(i)
      a21(i)=strain(4,i)*s11(i)+strain(2,i)*s12(i)+strain(5,i)*s13(i)
      a22(i)=strain(4,i)*s21(i)+strain(2,i)*s22(i)+strain(5,i)*s23(i)
      a23(i)=strain(4,i)*s31(i)+strain(2,i)*s32(i)+strain(5,i)*s33(i)
      a31(i)=strain(6,i)*s11(i)+strain(5,i)*s12(i)+strain(3,i)*s13(i)
      a32(i)=strain(6,i)*s21(i)+strain(5,i)*s22(i)+strain(3,i)*s23(i)
      a33(i)=strain(6,i)*s31(i)+strain(5,i)*s32(i)+strain(3,i)*s33(i)
   40 continue
      do 50 i=lft,llt
      strain(1,i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      strain(2,i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      strain(4,i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      strain(5,i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      strain(6,i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   50 continue
      do 60 i=lft,llt
      a11(i)=strain(1,i)*s11(i)+strain(4,i)*s21(i)+strain(6,i)*s31(i)
      a12(i)=strain(1,i)*s12(i)+strain(4,i)*s22(i)+strain(6,i)*s32(i)
      a13(i)=strain(1,i)*s13(i)+strain(4,i)*s23(i)+strain(6,i)*s33(i)
      a21(i)=strain(4,i)*s11(i)+strain(2,i)*s21(i)+strain(5,i)*s31(i)
      a22(i)=strain(4,i)*s12(i)+strain(2,i)*s22(i)+strain(5,i)*s32(i)
      a23(i)=strain(4,i)*s13(i)+strain(2,i)*s23(i)+strain(5,i)*s33(i)
      a31(i)=strain(6,i)*s11(i)+strain(5,i)*s21(i)
      a32(i)=strain(6,i)*s12(i)+strain(5,i)*s22(i)
      a33(i)=strain(6,i)*s13(i)+strain(5,i)*s23(i)
   60 continue
      do 70 i=lft,llt
      strain(1,i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      strain(2,i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      strain(3,i)=s13(i)*a13(i)+s23(i)*a23(i)+s33(i)*a33(i)
      strain(4,i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      strain(5,i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      strain(6,i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   70 continue
      return
      end
      subroutine conmds (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 dampk,ym,pr,mxe)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/shlopt/istrn,istupd,ibelyt,miter
      common /  / a(1)
      dimension cm(48,*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
c
      call tspc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call rttrss
      call ldsigs
      call ldstrs
c
c     call constitutive model type mte
c
      if (mte.eq.1) then
      call shl1s (cm,capa)
      elseif (mte.eq.2)  then
      call shl2s (cm,capa)
      elseif (mte.eq.3)  then
      call rttrin
      if (miter.eq.0) then
      call sh3sc (cm,capa)
      elseif (miter.eq.1) then
      call shl3s (cm,capa)
      elseif (miter.eq.2) then
      call sh3si (cm,capa)
      endif
      elseif (mte.eq.4)  then
      call shl4s (cm,a(ntmp0+1),a(n19))
      elseif (mte.eq.12)  then
      call shl12s (cm,capa)
      elseif (mte.eq.15)  then
      call shl15s (cm,capa)
      elseif (mte.eq.19) then
      if (miter.eq.0) then
      call sh19sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl19s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh19si (cm,a(n8),a(n9),capa)
      endif
      elseif (mte.eq.21) then
      call shl21s(cm,capa,a(ntmp0+1),a(n19))
      elseif (mte.eq.22) then
      call shl22s(cm,capa)
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),a(lc11))
      elseif (mte.eq.24)  then
      if (miter.eq.0) then
      call sh24sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl24s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh24si (cm,a(n8),a(n9),capa)
      endif
      elseif (mte.eq.30) then
      call rttrin
      call shl30s(cm,capa)
      elseif (mte.eq.33)  then
      call shl33s (cm,capa)
      elseif (mte.eq.34)  then
      call shl34s (cm,capa)
      elseif (mte.eq.35)  then
      if (miter.eq.0) then
      call shl35s(cm,capa)
      elseif (miter.eq.1) then
      call shl35s(cm,capa)
      elseif (miter.eq.2) then
      call shl35s(cm,capa)
      endif
      endif
      if (dampk.ne.0.) call rydmp2(dampk,ym,pr)
c
      call gblsss
      call tspc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
c
      return
      end
      subroutine tspc1s (nc,aux,ncl)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 ax(n,m)=aux(m,n-k)
   20 continue
      return
      end
      subroutine tspc2s (nc,aux,lav,ncl,nip,ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 aux(m,n-k)=ax(n,m)
   20 continue
      if (nip.ne.ipt) return
      lav=lav+(llt-lft+1)*ncl
      return
      end
      subroutine ldsigs
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      do 30 i=lft,llt
      a11(i)=sig1(i)*s11(i)+sig4(i)*s12(i)+sig6(i)*s13(i)
      a12(i)=sig1(i)*s21(i)+sig4(i)*s22(i)+sig6(i)*s23(i)
      a13(i)=sig1(i)*s31(i)+sig4(i)*s32(i)+sig6(i)*s33(i)
      a21(i)=sig4(i)*s11(i)+sig2(i)*s12(i)+sig5(i)*s13(i)
      a22(i)=sig4(i)*s21(i)+sig2(i)*s22(i)+sig5(i)*s23(i)
      a23(i)=sig4(i)*s31(i)+sig2(i)*s32(i)+sig5(i)*s33(i)
      a31(i)=sig6(i)*s11(i)+sig5(i)*s12(i)+sig3(i)*s13(i)
      a32(i)=sig6(i)*s21(i)+sig5(i)*s22(i)+sig3(i)*s23(i)
      a33(i)=sig6(i)*s31(i)+sig5(i)*s32(i)+sig3(i)*s33(i)
   30 continue
      do 40 i=lft,llt
      sig1(i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      sig2(i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      sig3(i)=s31(i)*a13(i)+s32(i)*a23(i)+s33(i)*a33(i)
      sig4(i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      sig5(i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      sig6(i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   40 continue
      return
      end
      subroutine ldstrs
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      do 10 i=lft,llt
      d4(i)=.5*d4(i)
      d5(i)=.5*d5(i)
   10 d6(i)=.5*d6(i)
      do 30 i=lft,llt
      a11(i)=d1(i)*s11(i)+d4(i)*s12(i)+d6(i)*s13(i)
      a12(i)=d1(i)*s21(i)+d4(i)*s22(i)+d6(i)*s23(i)
      a13(i)=d1(i)*s31(i)+d4(i)*s32(i)+d6(i)*s33(i)
      a21(i)=d4(i)*s11(i)+d2(i)*s12(i)+d5(i)*s13(i)
      a22(i)=d4(i)*s21(i)+d2(i)*s22(i)+d5(i)*s23(i)
      a23(i)=d4(i)*s31(i)+d2(i)*s32(i)+d5(i)*s33(i)
      a31(i)=d6(i)*s11(i)+d5(i)*s12(i)+d3(i)*s13(i)
      a32(i)=d6(i)*s21(i)+d5(i)*s22(i)+d3(i)*s23(i)
      a33(i)=d6(i)*s31(i)+d5(i)*s32(i)+d3(i)*s33(i)
   30 continue
      do 40 i=lft,llt
      d1(i)=    s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      d2(i)=    s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      d4(i)=2.*(s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i))
      d5(i)=2.*(s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i))
      d6(i)=2.*(s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i))
   40 continue
      return
      end
      subroutine gblsss
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      do 10 i=lft,llt
      a11(i)=sig1(i)*s11(i)+sig4(i)*s21(i)+sig6(i)*s31(i)
      a12(i)=sig1(i)*s12(i)+sig4(i)*s22(i)+sig6(i)*s32(i)
      a13(i)=sig1(i)*s13(i)+sig4(i)*s23(i)+sig6(i)*s33(i)
      a21(i)=sig4(i)*s11(i)+sig2(i)*s21(i)+sig5(i)*s31(i)
      a22(i)=sig4(i)*s12(i)+sig2(i)*s22(i)+sig5(i)*s32(i)
      a23(i)=sig4(i)*s13(i)+sig2(i)*s23(i)+sig5(i)*s33(i)
      a31(i)=sig6(i)*s11(i)+sig5(i)*s21(i)
      a32(i)=sig6(i)*s12(i)+sig5(i)*s22(i)
      a33(i)=sig6(i)*s13(i)+sig5(i)*s23(i)
   10 continue
      do 20 i=lft,llt
      sig1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      sig2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      sig3(i)=s13(i)*a13(i)+s23(i)*a23(i)+s33(i)*a33(i)
      sig4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      sig5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      sig6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   20 continue
      return
      end
      subroutine gbsysd (wgt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     1 ds11(128),ds12(128),ds13(128),ds22(128),
     2 ds23(128),ds33(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      do 10 i=lft,llt
      a11(i)=d1(i)*s11(i)+d4(i)*s21(i)+d6(i)*s31(i)
      a12(i)=d1(i)*s12(i)+d4(i)*s22(i)+d6(i)*s32(i)
      a13(i)=d1(i)*s13(i)+d4(i)*s23(i)+d6(i)*s33(i)
      a21(i)=d4(i)*s11(i)+d2(i)*s21(i)+d5(i)*s31(i)
      a22(i)=d4(i)*s12(i)+d2(i)*s22(i)+d5(i)*s32(i)
      a23(i)=d4(i)*s13(i)+d2(i)*s23(i)+d5(i)*s33(i)
      a31(i)=d6(i)*s11(i)+d5(i)*s21(i)+d3(i)*s31(i)
      a32(i)=d6(i)*s12(i)+d5(i)*s22(i)+d3(i)*s32(i)
      a33(i)=d6(i)*s13(i)+d5(i)*s23(i)+d3(i)*s33(i)
   10 continue
      do 20 i=lft,llt
      d1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      d2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      d3(i)=s13(i)*a13(i)+s23(i)*a23(i)+s33(i)*a33(i)
      d4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      d5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      d6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   20 continue
      do 30 i=lft,llt
      ds11(i)=wgt*d1(i)+ds11(i)
      ds22(i)=wgt*d2(i)+ds22(i)
      ds33(i)=wgt*d3(i)+ds33(i)
      ds12(i)=wgt*d4(i)+ds12(i)
      ds23(i)=wgt*d5(i)+ds23(i)
   30 ds13(i)=wgt*d6(i)+ds13(i)
      return
      end
      subroutine tckchg (fibl,lft,llt,sthick)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     1 ds11(128),ds12(128),ds13(128),ds22(128),
     2 ds23(128),ds33(128)
      common/aux13/
     & x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     & yhtnx1(128),yhtny1(128),yhtnz1(128),
     & yhtnx2(128),yhtny2(128),yhtnz2(128),
     & yhtnx3(128),yhtny3(128),yhtnz3(128),
     & yhtnx4(128),yhtny4(128),yhtnz4(128),
     & yhatx1(128),yhaty1(128),yhatz1(128),
     & yhatx2(128),yhaty2(128),yhatz2(128),
     & yhatx3(128),yhaty3(128),yhatz3(128),
     & yhatx4(128),yhaty4(128),yhatz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      dimension sn(128),fibl(9,1),c1(128),c2(128),c3(128),
     1 sthick(1)
      do 10 i=lft,llt
      c1(i)=ds11(i)*yhatx1(i)+ds12(i)*yhaty1(i)+ds13(i)*yhatz1(i)
      c2(i)=ds12(i)*yhatx1(i)+ds22(i)*yhaty1(i)+ds23(i)*yhatz1(i)
      c3(i)=ds13(i)*yhatx1(i)+ds23(i)*yhaty1(i)+ds33(i)*yhatz1(i)
      sn(i)=  c1(i)*yhatx1(i)+  c2(i)*yhaty1(i)+  c3(i)*yhatz1(i)+1.
      fibl(1,i)=sn(i)*fibl(1,i)
   10 continue
      do 20 i=lft,llt
      c1(i)=ds11(i)*yhatx2(i)+ds12(i)*yhaty2(i)+ds13(i)*yhatz2(i)
      c2(i)=ds12(i)*yhatx2(i)+ds22(i)*yhaty2(i)+ds23(i)*yhatz2(i)
      c3(i)=ds13(i)*yhatx2(i)+ds23(i)*yhaty2(i)+ds33(i)*yhatz2(i)
      sn(i)=  c1(i)*yhatx2(i)+  c2(i)*yhaty2(i)+  c3(i)*yhatz2(i)+1.
      fibl(2,i)=sn(i)*fibl(2,i)
   20 continue
      do 30 i=lft,llt
      c1(i)=ds11(i)*yhatx3(i)+ds12(i)*yhaty3(i)+ds13(i)*yhatz3(i)
      c2(i)=ds12(i)*yhatx3(i)+ds22(i)*yhaty3(i)+ds23(i)*yhatz3(i)
      c3(i)=ds13(i)*yhatx3(i)+ds23(i)*yhaty3(i)+ds33(i)*yhatz3(i)
      sn(i)=  c1(i)*yhatx3(i)+  c2(i)*yhaty3(i)+  c3(i)*yhatz3(i)+1.
      fibl(3,i)=sn(i)*fibl(3,i)
   30 continue
      do 40 i=lft,llt
      c1(i)=ds11(i)*yhatx4(i)+ds12(i)*yhaty4(i)+ds13(i)*yhatz4(i)
      c2(i)=ds12(i)*yhatx4(i)+ds22(i)*yhaty4(i)+ds23(i)*yhatz4(i)
      c3(i)=ds13(i)*yhatx4(i)+ds23(i)*yhaty4(i)+ds33(i)*yhatz4(i)
      sn(i)=  c1(i)*yhatx4(i)+  c2(i)*yhaty4(i)+  c3(i)*yhatz4(i)+1.
      fibl(4,i)=sn(i)*fibl(4,i)
   40 continue
      do 50 i=lft,llt
      sthick(ix1(i))=  max(sthick(ix1(i)),fibl(1,i))
      sthick(ix2(i))=  max(sthick(ix2(i)),fibl(2,i))
      sthick(ix3(i))=  max(sthick(ix3(i)),fibl(3,i))
      sthick(ix4(i))=  max(sthick(ix4(i)),fibl(4,i))
   50 continue
      return
      end
      subroutine rttrss
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),t7(128,2),
     2 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     3 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      do 10 i=lft,llt
      s1(i)=sig1(i)
      s2(i)=sig2(i)
      s3(i)=sig3(i)
      s4(i)=sig4(i)
      s5(i)=sig5(i)
   10 s6(i)=sig6(i)
      do 20 i=lft,llt
      q1(i)=2.*s4(i)*wzz(i)
      q2(i)=2.*s6(i)*wyy(i)
   20 q3(i)=2.*s5(i)*wxx(i)
      do 30 i=lft,llt
      sig1(i)=s1(i)-q1(i)+q2(i)
      sig2(i)=s2(i)+q1(i)-q3(i)
      sig3(i)=s3(i)-q2(i)+q3(i)
      sig4(i)=s4(i)+wzz(i)*(s1(i)-s2(i))+wyy(i)*s5(i)-wxx(i)*s6(i)
      sig5(i)=s5(i)+wxx(i)*(s2(i)-s3(i))+wzz(i)*s6(i)-wyy(i)*s4(i)
   30 sig6(i)=s6(i)+wyy(i)*(s3(i)-s1(i))+wxx(i)*s4(i)-wzz(i)*s5(i)
      return
      end
      subroutine rttrin
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),t7(128,2),
     2 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     3 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      do 10 i=lft,llt
      s1(i)=epx1(i)
      s2(i)=epx2(i)
      s3(i)=-epx1(i)-epx2(i)
      s4(i)=epx4(i)
      s5(i)=epx5(i)
   10 s6(i)=epx6(i)
      do 20 i=lft,llt
      q1(i)=2.*s4(i)*wzz(i)
      q2(i)=2.*s6(i)*wyy(i)
   20 q3(i)=2.*s5(i)*wxx(i)
      do 30 i=lft,llt
      epx1(i)=s1(i)-q1(i)+q2(i)
      epx2(i)=s2(i)+q1(i)-q3(i)
      epx4(i)=s4(i)+wzz(i)*(s1(i)-s2(i))+wyy(i)*s5(i)-wxx(i)*s6(i)
      epx5(i)=s5(i)+wxx(i)*(s2(i)-s3(i))+wzz(i)*s6(i)-wyy(i)*s4(i)
   30 epx6(i)=s6(i)+wyy(i)*(s3(i)-s1(i))+wxx(i)*s4(i)-wzz(i)*s5(i)
c
      return
      end
      subroutine rwingv (wxx,wyy,wzz,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      dimension det(128),wzz2(128),wyy2(128),wxx2(128),wxxyy(128),
     1 wxxzz(128),wyyzz(128),twxx(128),twyy(128),twzz(128),
     2 wxx(1),wyy(1),wzz(1)
c
      do 10 i=lft,llt
      wxx2(i)=wxx(i)*wxx(i)
      wyy2(i)=wyy(i)*wyy(i)
      wzz2(i)=wzz(i)*wzz(i)
      wxxzz(i)=wxx(i)*wzz(i)
      wxxyy(i)=wxx(i)*wyy(i)
      wyyzz(i)=wyy(i)*wzz(i)
      twxx(i) =2.*wxx(i)
      twyy(i) =2.*wyy(i)
   10 twzz(i) =2.*wzz(i)
c
      do 20 i=lft,llt
      det(i) =2.0/(4.0+wxx2(i)+wyy2(i)+wzz2(i))
      rot1(i)=1.-det(i)*(wyy2(i)+wzz2(i))
      rot2(i)= (twzz(i)+wxxyy(i))*det(i)
      rot3(i)=-(twyy(i)-wxxzz(i))*det(i)
      rot4(i)=-(twzz(i)-wxxyy(i))*det(i)
      rot5(i)=1.-det(i)*(wxx2(i)+wzz2(i))
      rot6(i)= (twxx(i)+wyyzz(i))*det(i)
      rot7(i)= (twyy(i)+wxxzz(i))*det(i)
      rot8(i)=-(twxx(i)-wyyzz(i))*det(i)
      rot9(i)=1.-det(i)*(wyy2(i)+wxx2(i))
   20 continue
      return
      end
      subroutine q4rslt(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iop,qextra,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for the belytschko-tsay shell
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/hourg/ymod,gmod,ifsv
      common/failu/sieu(128),fail(128)
      common/energy/xinen
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),
     2 qextra(*),iblks(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
      data one/1.0/
c
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ifsv=ihgq(mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      qhg=.25*hgq(mxe)
      mte=mtype(mxe)
      if (mte.eq.20) return
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 30 i=lft,llt
      sig1m(i)=0.0
      sig2m(i)=0.0
      sig4m(i)=0.0
      sig1n(i)=0.0
30      sig2n(i)=0.0
      do 31 i=lft,llt
      sig4n(i)=0.0
      sig5n(i)=0.0
      sig6n(i)=0.0
      sig5l(i)=0.0
      sig6l(i)=0.0
      str33(i)=0.0
   31 continue
      do 40 i=lft,llt
      fail(i)=1.0
      sieu(i)=xies(nnm1+i)
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      x4(i) =x(1,ix4(i))
      y4(i) =x(2,ix4(i))
      z4(i) =x(3,ix4(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
      dz1(i)=vt(3,ix1(i))
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
      dz3(i)=vt(3,ix3(i))
      dx4(i)=vt(1,ix4(i))
      dy4(i)=vt(2,ix4(i))
      dz4(i)=vt(3,ix4(i))
      wxx1(i)=vr(1,ix1(i))
      wyy1(i)=vr(2,ix1(i))
      wzz1(i)=vr(3,ix1(i))
      wxx2(i)=vr(1,ix2(i))
      wyy2(i)=vr(2,ix2(i))
      wzz2(i)=vr(3,ix2(i))
      wxx3(i)=vr(1,ix3(i))
      wyy3(i)=vr(2,ix3(i))
      wzz3(i)=vr(3,ix3(i))
      wxx4(i)=vr(1,ix4(i))
      wyy4(i)=vr(2,ix4(i))
      wzz4(i)=vr(3,ix4(i))
   40 continue
c
      call dfnls (fibl(1,nnm1+1),nip)
c
      call tranbs
c
c     constitutive model evaluation
c
      call cnmdtb (nmtcon,auxvec,cm,lav,mte,1,1,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if (istrn.ne.0) then
      do 60 i=lft,llt
      zeta(i)=-.50*thick(i)
      d1(i)=b1vx(i)+zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
      d3(i)=-d1(i)-d2(i)
      d4(i)=bxyv(i)+zeta(i)*bxyt(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
   60 continue
      call tbstrn (strain(1,nnm1+1))
      do 70 i=lft,llt
      zeta(i)= .50*thick(i)
      d1(i)=b1vx(i)+zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
      d3(i)=-d1(i)-d2(i)
      d4(i)=bxyv(i)+zeta(i)*bxyt(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
   70 continue
      call tbstrn (strain(7,nnm1+1))
      endif
c
      do 80 i=lft,llt
   80 xies(nnm1+i)=xies(nnm1+i)+einc(i)
c
      if (istupd.ne.0) then
      do 90 i=lft,llt
      d1(i)=b1vx(i)
      d2(i)=b2vy(i)
      d3(i)=-d1(i)-d2(i)
      d4(i)=bxyv(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
   90 continue
      call tbstu1 (one)
      call tbstu2 (fibl(1,nnm1+1),b(istupd))
      endif
c
c     force components in local system
c
      call frcbs2
c
c     transform force components to global system + hourglass forces
c
      sndspd=sqrt(sndspd*rho)
      if (iop.eq.2) then
      call trnfbs (rhs,rhs(1+neq),fibl(2,nnm1+1),mte,iblks)
      elseif (iop.eq.6) then
      call pcoeff(fibl(2,nnm1+1))
      call prnfbs(rhs,rhs(1+neq),fibl(2,nnm1+1),mte,qextra(nnm1+1),
     1 iblks)
      else
      stop 'illegal resultant shell'
      endif
c
      return
      end
      subroutine berwsh(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,qextra,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for the belytschko-tsay shell
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/hourg/ymod,gmod,ifsv
      common/failu/sieu(128),fail(128)
      common/energy/xinen
      common/kinet/enkint(128)
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),
     2 qextra(*),iblks(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ifsv=ihgq(mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      qhg=.25*hgq(mxe)
      mte=mtype(mxe)
      if (mte.eq.20) return
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 30 i=lft,llt
      sig1m(i)=0.0
      sig2m(i)=0.0
      sig4m(i)=0.0
      sig1n(i)=0.0
  30    sig2n(i)=0.0
      do 31 i=lft,llt
      sig4n(i)=0.0
      sig5n(i)=0.0
      sig6n(i)=0.0
      sig5l(i)=0.0
      sig6l(i)=0.0
      str33(i)=0.0
   31 continue
      do 40 i=lft,llt
      fail(i)=1.0
      sieu(i)=xies(nnm1+i)
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      x4(i) =x(1,ix4(i))
      y4(i) =x(2,ix4(i))
      z4(i) =x(3,ix4(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
      dz1(i)=vt(3,ix1(i))
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
      dz3(i)=vt(3,ix3(i))
      dx4(i)=vt(1,ix4(i))
      dy4(i)=vt(2,ix4(i))
      dz4(i)=vt(3,ix4(i))
      wxx1(i)=vr(1,ix1(i))
      wyy1(i)=vr(2,ix1(i))
      wzz1(i)=vr(3,ix1(i))
      wxx2(i)=vr(1,ix2(i))
      wyy2(i)=vr(2,ix2(i))
      wzz2(i)=vr(3,ix2(i))
      wxx3(i)=vr(1,ix3(i))
      wyy3(i)=vr(2,ix3(i))
      wzz3(i)=vr(3,ix3(i))
      wxx4(i)=vr(1,ix4(i))
      wyy4(i)=vr(2,ix4(i))
      wzz4(i)=vr(3,ix4(i))
   40 continue
c
      rho8th=.125*rhoa(lft)
      do 60 i=lft,llt
      dx=dx1(i)**2+dx2(i)**2+dx3(i)**2+dx4(i)**2
      dy=dy1(i)**2+dy2(i)**2+dy3(i)**2+dy4(i)**2
      dz=dz1(i)**2+dz2(i)**2+dz3(i)**2+dz4(i)**2
      enkint(i)=fibl(1,nnm1+i)*rho8th*(dx+dy+dz)
   60 continue
c
      call dfnls (fibl(1,nnm1+1),nip)
c
      call tranbs
c
      zneg=-.5
      zpos= .5
      do 250 m=1,nip
      ipt=m
      if (irl.eq.0) then
      zta=.5*zet(m,nip)
      elseif (irl.gt.0) then
      zta=.5*(-1.0+(m-1)*2./(nip-1))
      else
      zta=.5*rule(m,1,nrl)
      mtu=nint(rule(m,3,nrl))
      mtv=mxt(lft)
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
      bs1=.5-zta
      bs2=.5+zta
      fac=zneg*bs1+zpos*bs2
c
c     strain increments
c
      do 90 i=lft,llt
      zeta(i)=fac*thick(i)
      d1(i)=b1vx(i)+zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
      d3(i)=0.
      d4(i)=bxyv(i)+zeta(i)*bxyt(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
      einc(i)=0.0
   90 continue
c
c     constitutive model evaluation
c
      call cnmdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if( m.eq.1) then
      do 245 ii=lft,llt
      ebarmn(ii)=ebar(ii)
  245 continue
      else
      do 247 ii=lft,llt
      ebarmn(ii)=min(ebarmn(ii),ebar(ii))
  247 continue
      endif
      if (istrn.ne.0) then
      if (irl.eq.0) then
      if (m.eq.isrn(1,nip)) then
      call tbstrn (strain(1,nnm1+1))
      endif
      if (m.eq.isrn(2,nip)) then
      call tbstrn (strain(7,nnm1+1))
      endif
      else
      if (m.eq.1  ) then
      call tbstrn (strain(1,nnm1+1))
      endif
      if (m.eq.nip) then
      call tbstrn (strain(7,nnm1+1))
      endif
      endif
      endif
c
c     force evaluation
c
      if (irl.eq.0) then
      fac=wgts(m,nip)
      elseif (irl.gt.0) then
      fac=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) fac=fac/2.
      else
      fac=rule(m,2,nrl)
      mxt(lft)=mtv
      endif
      if (istupd.ne.0) then
      call tbstu1 (.5*fac)
      endif
      call frcbs1 (fac,b(ipi),b(iph),nnm1,xies(nnm1+1))
  250 continue
c
      if (istupd.ne.0) then
      call tbstu2 (fibl(1,nnm1+1),b(istupd))
      endif
c
c     force components in local system
c
      call frcbs2
c
c     transform force components to global system + hourglass forces
c
      sndspd=sqrt(sndspd*rho)
      call pcoeff (fibl(2,nnm1+1))
      call prnfbs (rhs,rhs(1+neq),fibl(2,nnm1+1),mte,qextra(nnm1+1),
     1    iblks)
c
      return
      end
      subroutine pcoeff (qs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     formulation
c
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &ft31(128),ft32(128),ft33(128),ft41(128),ft42(128),ft43(128),
     &htx(128),hty(128),gm1(128),gm2(128),gm3(128),gm4(128),
     &bsum(128),qhx(128),qhy(128),qwz(128),qtx(128),qty(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/ps1/
     1 qstx(128),qsty(128),qntx(128),qnty(128),
     2 a1(128),a2(128),b1(128),b2(128)
      common/ps2/
     1 a1b1(128),a1b2(128),a2b1(128),a2b2(128),
     2 c1(128),rj0(128),rjs(128),
     3 rjn(128),ha(128),hb(128),hc(128),hd(128),xee(128),
     4 rinn(128),hnn(128),hsn(128),hss(128),
     5 h11(128),h12(128),h22(128),p(128)
      common/ps4/
     1 als1(128),als2(128),als3(128),als4(128),als5(128),
     2 als6(128),als7(128),als8(128),als9(128),als10(128),
     3 bet1(128),bet2(128),bet3(128),bet4(128),bet5(128),bet6(128),
     4 alm1(128),alm2(128),alm3(128),alm4(128),
     5 ri1(128),ri2(128)
      common/ps5/
     1 cs11(128),cs12(128),cs13(128),cs14(128),cs15(128),
     2 cs22(128),cs23(128),cs24(128),cs25(128),cs33(128),
     3 cs34(128),cs35(128),cs44(128),cs45(128),cs55(128),
     4 cm1(128),cm2(128),cm3(128),cb2(128)
      common/aux36/lft,llt
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      dimension qs(9,*)
c
      do 5 i=lft,llt
      xl12=sqrt(  x2(i)**2 + y2(i)**2 )
      xl14=sqrt(  x4(i)**2 + y4(i)**2 )
      xl23=sqrt( (x3(i)-x2(i))**2 + (y3(i)-y2(i))**2 )
      xl34=sqrt( (x4(i)-x3(i))**2 + (y4(i)-y3(i))**2 )
      xmom1=xl23*qs(1,i)
      xmom2=xl12*qs(2,i)
      xmom3=xl14*qs(1,i)
      xmom4=xl34*qs(2,i)
c
c.... compute hourglass stresses
c
      s1s=3.46*xmom3/(thick(i)*xl14**2)
      s1n=3.46*xmom2/(thick(i)*xl12**2)
      s2s=3.46*xmom1/(thick(i)*xl23**2)
      s2n=3.46*xmom2/(thick(i)*xl12**2)
      s3s=3.46*xmom1/(thick(i)*xl23**2)
      s3n=3.46*xmom4/(thick(i)*xl34**2)
      s4s=3.46*xmom3/(thick(i)*xl14**2)
      s4n=3.46*xmom4/(thick(i)*xl34**2)
c
      seff1=(s1s-s1n)**2 + s1s**2 + s1n**2
      seff2=(s2s-s2n)**2 + s2s**2 + s2n**2
      seff3=(s3s-s3n)**2 + s3s**2 + s3n**2
      seff4=(s4s-s4n)**2 + s4s**2 + s4n**2
c
      seffmx=sqrt(.5*max(seff1,seff2,seff3,seff4))
      if( (seffmx.ge.eyld(i)) .and. (eyld(i).gt.0.0)) then
      ebarmn(i)=etanmd(i)
      endif
    5 continue
c
c.... find gamma
      do 10 i=lft,llt
      htx(i)=x3(i)-x2(i)-x4(i)
      hty(i)=y3(i)-y2(i)-y4(i)
      gm1(i)=.25*( 1.-area(i)*(px1(i)*htx(i)+py1(i)*hty(i)))
      gm2(i)=.25*(-1.-area(i)*(px2(i)*htx(i)+py2(i)*hty(i)))
      gm3(i)= 0.5-gm1(i)
      gm4(i)=-0.5-gm2(i)
   10 continue
c
c.... geometry coefficients
      do 20 i=lft,llt
      a1(i)=x2(i)+x3(i)-x4(i)
      a2(i)=y2(i)+y3(i)-y4(i)
      b1(i)=-x2(i)+x3(i)+x4(i)
      b2(i)=-y2(i)+y3(i)+y4(i)
   20 continue
c
c.... compute jacobian
      do 30 i=lft,llt
      a1b1(i)=a1(i)*b1(i)
      a1b2(i)=a1(i)*b2(i)
      a2b1(i)=a2(i)*b1(i)
      a2b2(i)=a2(i)*b2(i)
      c1(i)=a1b2(i)-a2b1(i)
      rj0(i)=c1(i)/16.
      rjs(i)=(hty(i)*a1(i)-htx(i)*a2(i))/16.
      rjn(i)=(htx(i)*b2(i)-hty(i)*b1(i))/16.
   30 continue
c
c.... define coefficients for transverse shear strain
      do 40 i=lft,llt
      ha(i)=-b2(i)/(rj0(i)-rjn(i))
      hb(i)=b2(i)/(rj0(i)+rjn(i))
      hc(i)=a2(i)/(rj0(i)-rjs(i))
      hd(i)=-a2(i)/(rj0(i)+rjs(i))
      xee(i)=.125/c1(i)
      als1(i)=xee(i)*(a1b2(i)*(ha(i)+hb(i))
     1                  -a2b1(i)*(hc(i)+hd(i)))
      als2(i)=xee(i)*a1b2(i)*(hb(i)-ha(i))
      als3(i)=-xee(i)*a2b1(i)*(hd(i)-hc(i))
      als4(i)=xee(i)*(-a1b1(i)*(ha(i)+hb(i))
     1                   +a1b1(i)*(hc(i)+hd(i)))
      als5(i)=-xee(i)*a1b1(i)*(hb(i)-ha(i))
      als6(i)=xee(i)*a1b1(i)*(hd(i)-hc(i))
   40 continue
c
      do 50 i=lft,llt
      ha(i)=b1(i)/(rj0(i)-rjn(i))
      hb(i)=-b1(i)/(rj0(i)+rjn(i))
      hc(i)=-a1(i)/(rj0(i)-rjs(i))
      hd(i)=a1(i)/(rj0(i)+rjs(i))
      als1(i)=als1(i)+xee(i)*(a2b2(i)*(ha(i)+hb(i))
     1                       -a2b2(i)*(hc(i)+hd(i)))
      als2(i)=als2(i)+xee(i)*a2b2(i)*(hb(i)-ha(i))
      als3(i)=als3(i)-xee(i)*a2b2(i)*(hd(i)-hc(i))
      als4(i)=als4(i)+xee(i)*(-a2b1(i)*(ha(i)+hb(i))
     1                       +a1b2(i)*(hc(i)+hd(i)))
      als5(i)=als5(i)-xee(i)*a2b1(i)*(hb(i)-ha(i))
      als6(i)=als6(i)+xee(i)*a1b2(i)*(hd(i)-hc(i))
   50 continue
c
      do 60 i=lft,llt
      xee(i)=2.*xee(i)
      als7(i)=xee(i)*a2b2(i)
      als8(i)=-xee(i)*a2b1(i)
      als9(i)=xee(i)*a1b2(i)
      als10(i)=xee(i)*a1b1(i)
   60 continue
c
c.... evaluate integrals
      ccc=4./3.
      do 70 i=lft,llt
      ri1(i)=ccc*rjs(i)
      ri2(i)=ccc*rjn(i)
      rinn(i)=ccc*rj0(i)
      hnn(i)=rinn(i)-area(i)*ri2(i)*ri2(i)
      hsn(i)=-area(i)*ri1(i)*ri2(i)
      hss(i)=rinn(i)-area(i)*ri1(i)*ri1(i)
   70 continue
      do 80 i=lft,llt
      ri1(i)=ri1(i)*area(i)
      ri2(i)=ri2(i)*area(i)
   80 continue
c
c.... determine transverse shear part of hourglass stiffness
      do 90 i=lft,llt
      ha(i)=hss(i)*als3(i)+hsn(i)*als2(i)
      hb(i)=hsn(i)*als3(i)+hnn(i)*als2(i)
      hc(i)=hss(i)*als6(i)+hsn(i)*als5(i)
      hd(i)=hsn(i)*als6(i)+hnn(i)*als5(i)
      cs11(i)=(als1(i)**2+als4(i)**2)/area(i)+als3(i)*ha(i)
     1        +als2(i)*hb(i)+als6(i)*hc(i)+als5(i)*hd(i)
      cs12(i)=als7(i)*ha(i)-als9(i)*hc(i)
      cs13(i)=-als7(i)*hb(i)-als8(i)*hd(i)
      cs14(i)=als8(i)*ha(i)+als10(i)*hc(i)
      cs15(i)=als9(i)*hb(i)-als10(i)*hd(i)
      cs22(i)=hss(i)*(als7(i)**2+als9(i)**2)
      cs23(i)=hsn(i)*(-als7(i)**2+als9(i)*als8(i))
      cs24(i)=hss(i)*(als7(i)*als8(i)-als9(i)*als10(i))
      cs25(i)=hsn(i)*als9(i)*(als7(i)+als10(i))
      cs33(i)=hnn(i)*(als7(i)**2+als8(i)**2)
      cs34(i)=-hsn(i)*(als7(i)*als8(i)+als8(i)*als10(i))
      cs35(i)=hnn(i)*(-als7(i)*als9(i)+als8(i)*als10(i))
      cs44(i)=hss(i)*(als8(i)**2+als10(i)**2)
      cs45(i)=hsn(i)*(als8(i)*als9(i)-als10(i)**2)
      cs55(i)=hnn(i)*(als9(i)**2+als10(i)**2)
   90 continue
c
      do 100 i=lft,llt
      q11=1./ebarmn(i)
      q33=ymod/(ebarmn(i)*gmod)
      q12=q11-.5*q33
      bet1(i)=q11*a1(i)**2+q12*a2(i)**2
      bet2(i)=q12*a1(i)**2+q11*a2(i)**2
      bet3(i)=q33*a1(i)*a2(i)
      bet4(i)=q11*b1(i)**2+q12*b2(i)**2
      bet5(i)=q12*b1(i)**2+q11*b2(i)**2
      bet6(i)=q33*b1(i)*b2(i)
  100 continue
c
      do 110 i=lft,llt
      h11(i)=a1(i)**2*bet1(i)+a2(i)**2*bet2(i)+a1(i)*a2(i)*bet3(i)
      h12(i)=a1(i)**2*bet4(i)+a2(i)**2*bet5(i)+a1(i)*a2(i)*bet6(i)
      h22(i)=b1(i)**2*bet4(i)+b2(i)**2*bet5(i)+b1(i)*b2(i)*bet6(i)
      h11(i)=h11(i)*hnn(i)
      h12(i)=h12(i)*hsn(i)
      h22(i)=h22(i)*hss(i)
      ha(i)=a1(i)/3.*c1(i)
      hb(i)=b1(i)/3.*c1(i)
      hc(i)=a2(i)/3.*c1(i)
      hd(i)=b2(i)/3.*c1(i)
      p(i)=1./(h11(i)*h22(i)-h12(i)*h12(i))
  110 continue
c
      do 120 i=lft,llt
      alm1(i)=p(i)*(ha(i)*h22(i)-hb(i)*h12(i))
      alm2(i)=p(i)*(-ha(i)*h12(i)+hb(i)*h11(i))
      alm3(i)=p(i)*(hc(i)*h22(i)-hd(i)*h12(i))
      alm4(i)=p(i)*(-hc(i)*h12(i)+hd(i)*h11(i))
  120 continue
c
c.... determine membrane and bend. part of hourglass stiffness
      do 130 i=lft,llt
      cm1(i)=ha(i)*alm1(i)+hb(i)*alm2(i)
      cm2(i)=ha(i)*alm3(i)+hb(i)*alm4(i)
      cm3(i)=hc(i)*alm3(i)+hd(i)*alm4(i)
      cb2(i)=-hc(i)*alm1(i)-hd(i)*alm2(i)
  130 continue
c
      return
      end
      subroutine prnfbs(e,f,qs,mte,qsc9,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &ft31(128),ft32(128),ft33(128),ft41(128),ft42(128),ft43(128),
     &htx(128),hty(128),gm1(128),gm2(128),gm3(128),gm4(128),
     &bsum(128),qhx(128),qhy(128),qwz(128),qtx(128),qty(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux40/
     1 x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     2 x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128)
      common/hourg/ymod,gmod,ifsv
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/presc/voltot(128)
      common/failu/sieu(128),fail(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/ps1/
     1 qstx(128),qsty(128),qntx(128),qnty(128),
     2 a1(128),a2(128),bb1(128),bb2(128)
      common/ps5/
     1 cs11(128),cs12(128),cs13(128),cs14(128),cs15(128),
     2 cs22(128),cs23(128),cs24(128),cs25(128),cs33(128),
     3 cs34(128),cs35(128),cs44(128),cs45(128),cs55(128),
     4 cm1(128),cm2(128),cm3(128),cb2(128)
      common/rigid/xll(128)
      dimension e(3,*),f(3,*),qs(9,*),qsc9(*),iblks(*)
c
      do 10 i=lft,llt
      c1(i)=thick(i)**3/12.
      c2(i)=thick(i)*gmod
   10 continue
c
c.... find generalized strains
      do 12 i=lft,llt
      qhx(i)=gm1(i)*vx1(i)+gm2(i)*vx2(i)+gm3(i)*vx3(i)+gm4(i)*vx4(i)
      qhy(i)=gm1(i)*vy1(i)+gm2(i)*vy2(i)+gm3(i)*vy3(i)+gm4(i)*vy4(i)
      qwz(i)=gm1(i)*vz1(i)+gm2(i)*vz2(i)+gm3(i)*vz3(i)+gm4(i)*vz4(i)
      qtx(i)=gm1(i)*vx5(i)+gm2(i)*vx6(i)+gm3(i)*vx7(i)+gm4(i)*vx8(i)
      qty(i)=gm1(i)*vy5(i)+gm2(i)*vy6(i)+gm3(i)*vy7(i)+gm4(i)*vy8(i)
      qstx(i)=-vx5(i)+vx6(i)+vx7(i)-vx8(i)
      qsty(i)=-vy5(i)+vy6(i)+vy7(i)-vy8(i)
      qntx(i)=-vx5(i)-vx6(i)+vx7(i)+vx8(i)
      qnty(i)=-vy5(i)-vy6(i)+vy7(i)+vy8(i)
      rig1=vx5(i)+vx6(i)+vx7(i)+vx8(i)
      rig2=vy5(i)+vy6(i)+vy7(i)+vy8(i)
      qhx(i)=qhx(i)+.125*xll(i)*rig2
      qhy(i)=qhy(i)-.125*xll(i)*rig1
   12 continue
c
c.... update hourglass membrane and bending stresses
      do 14 i=lft,llt
      qs(1,i)=qs(1,i)+(cm1(i)*qhx(i)+cm2(i)*qhy(i))*thick(i)
      qs(2,i)=qs(2,i)+(cm2(i)*qhx(i)+cm3(i)*qhy(i))*thick(i)
      qs(3,i)=qs(3,i)+(cm3(i)*qtx(i)+cb2(i)*qty(i))*c1(i)
      qs(4,i)=qs(4,i)+(cb2(i)*qtx(i)+cm1(i)*qty(i))*c1(i)
   14 continue
c
c.... update hourglass transverse shear stresses
      do 16 i=lft,llt
      qs(5,i)=qs(5,i)+c2(i)*(cs11(i)*qwz(i)+cs12(i)*qstx(i)
     1 +cs13(i)*qntx(i)+cs14(i)*qsty(i)+cs15(i)*qnty(i))
      qs(6,i)=qs(6,i)+c2(i)*(cs12(i)*qwz(i)+cs22(i)*qstx(i)
     1 +cs23(i)*qntx(i)+cs24(i)*qsty(i)+cs25(i)*qnty(i))
      qs(7,i)=qs(7,i)+c2(i)*(cs13(i)*qwz(i)+cs23(i)*qstx(i)
     1 +cs33(i)*qntx(i)+cs34(i)*qsty(i)+cs35(i)*qnty(i))
      qs(8,i)=qs(8,i)+c2(i)*(cs14(i)*qwz(i)+cs24(i)*qstx(i)
     1 +cs34(i)*qntx(i)+cs44(i)*qsty(i)+cs45(i)*qnty(i))
      qsc9(i)=qsc9(i)+c2(i)*(cs15(i)*qwz(i)+cs25(i)*qstx(i)
     1 +cs35(i)*qntx(i)+cs45(i)*qsty(i)+cs55(i)*qnty(i))
   16 continue
c
      do 30 i=lft,llt
      ft31(i)=-ft11(i)+gm3(i)*qs(1,i)
      ft32(i)=-ft12(i)+gm3(i)*qs(2,i)
      ft33(i)=-ft13(i)+gm3(i)*qs(5,i)
      ft41(i)=-ft21(i)+gm4(i)*qs(1,i)
      ft42(i)=-ft22(i)+gm4(i)*qs(2,i)
      ft43(i)=-ft23(i)+gm4(i)*qs(5,i)
      ft11(i)= ft11(i)+gm1(i)*qs(1,i)
      ft12(i)= ft12(i)+gm1(i)*qs(2,i)
      ft13(i)= ft13(i)+gm1(i)*qs(5,i)
      ft21(i)= ft21(i)+gm2(i)*qs(1,i)
      ft22(i)= ft22(i)+gm2(i)*qs(2,i)
      ft23(i)= ft23(i)+gm2(i)*qs(5,i)
      fm11(i)= fm11(i)+gm1(i)*qs(3,i)-qs(6,i)-qs(7,i)
      fm12(i)= fm12(i)+gm1(i)*qs(4,i)-qs(8,i)-qsc9(i)
      fm21(i)= fm21(i)+gm2(i)*qs(3,i)+qs(6,i)-qs(7,i)
      fm22(i)= fm22(i)+gm2(i)*qs(4,i)+qs(8,i)-qsc9(i)
      fm31(i)= fm31(i)+gm3(i)*qs(3,i)+qs(6,i)+qs(7,i)
      fm32(i)= fm32(i)+gm3(i)*qs(4,i)+qs(8,i)+qsc9(i)
      fm41(i)= fm41(i)+gm4(i)*qs(3,i)-qs(6,i)+qs(7,i)
      fm42(i)= fm42(i)+gm4(i)*qs(4,i)-qs(8,i)+qsc9(i)
   30 continue
c
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)+gl13(i)*ft13(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)+gl23(i)*ft13(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)+gl13(i)*ft23(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)+gl23(i)*ft23(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      fx3(i)=gl11(i)*ft31(i)+gl12(i)*ft32(i)+gl13(i)*ft33(i)
      fy3(i)=gl21(i)*ft31(i)+gl22(i)*ft32(i)+gl23(i)*ft33(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)+gl33(i)*ft33(i)
      fx4(i)=gl11(i)*ft41(i)+gl12(i)*ft42(i)+gl13(i)*ft43(i)
      fy4(i)=gl21(i)*ft41(i)+gl22(i)*ft42(i)+gl23(i)*ft43(i)
      fz4(i)=gl31(i)*ft41(i)+gl32(i)*ft42(i)+gl33(i)*ft43(i)
      xmx1(i)=gl11(i)*fm11(i)+gl12(i)*fm12(i)
      xmy1(i)=gl21(i)*fm11(i)+gl22(i)*fm12(i)
      xmz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)
      xmx2(i)=gl11(i)*fm21(i)+gl12(i)*fm22(i)
      xmy2(i)=gl21(i)*fm21(i)+gl22(i)*fm22(i)
      xmz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)
      xmx3(i)=gl11(i)*fm31(i)+gl12(i)*fm32(i)
      xmy3(i)=gl21(i)*fm31(i)+gl22(i)*fm32(i)
      xmz3(i)=gl31(i)*fm31(i)+gl32(i)*fm32(i)
      xmx4(i)=gl11(i)*fm41(i)+gl12(i)*fm42(i)
      xmy4(i)=gl21(i)*fm41(i)+gl22(i)*fm42(i)
      xmz4(i)=gl31(i)*fm41(i)+gl32(i)*fm42(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
      if (mte.ne.15.and.mte.ne.19.and.mte.ne.22.and.mte.ne.24) then
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-xmy3(i)
   65 f(3,ix3(i))=f(3,ix3(i))-xmz3(i)
cdir$ ivdep
      do 70 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fz4(i)
      f(1,ix4(i))=f(1,ix4(i))-xmx4(i)
      f(2,ix4(i))=f(2,ix4(i))-xmy4(i)
   70 f(3,ix4(i))=f(3,ix4(i))-xmz4(i)
      else
      ifail=1
cdir$ ivdep
      do 75 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fail(i)*fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fail(i)*fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fail(i)*fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-fail(i)*xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-fail(i)*xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-fail(i)*xmz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fail(i)*fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fail(i)*fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fail(i)*fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-fail(i)*xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-fail(i)*xmy3(i)
   75 f(3,ix3(i))=f(3,ix3(i))-fail(i)*xmz3(i)
cdir$ ivdep
      do 80 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fail(i)*fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fail(i)*fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fail(i)*fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-fail(i)*xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-fail(i)*xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-fail(i)*xmz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fail(i)*fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fail(i)*fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fail(i)*fz4(i)
      f(1,ix4(i))=f(1,ix4(i))-fail(i)*xmx4(i)
      f(2,ix4(i))=f(2,ix4(i))-fail(i)*xmy4(i)
   80 f(3,ix4(i))=f(3,ix4(i))-fail(i)*xmz4(i)
      endif
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=4
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
      subroutine blytsy(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for the belytschko-tsay shell
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/hourg/ymod,gmod,ifsv
      common/failu/sieu(128),fail(128)
      common/energy/xinen
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),
     2 iblks(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ifsv=ihgq(mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      qhg=.25*hgq(mxe)
      mte=mtype(mxe)
      if (mte.eq.20) return
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 30 i=lft,llt
      sig1m(i)=0.0
      sig2m(i)=0.0
      sig4m(i)=0.0
      sig1n(i)=0.0
 30     sig2n(i)=0.0
      do 31 i=lft,llt
      sig4n(i)=0.0
      sig5n(i)=0.0
      sig6n(i)=0.0
      sig5l(i)=0.0
      sig6l(i)=0.0
      str33(i)=0.0
   31 continue
      do 40 i=lft,llt
      fail(i)=1.0
      sieu(i)=xies(nnm1+i)
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      x4(i) =x(1,ix4(i))
      y4(i) =x(2,ix4(i))
      z4(i) =x(3,ix4(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
      dz1(i)=vt(3,ix1(i))
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
      dz3(i)=vt(3,ix3(i))
      dx4(i)=vt(1,ix4(i))
      dy4(i)=vt(2,ix4(i))
      dz4(i)=vt(3,ix4(i))
      wxx1(i)=vr(1,ix1(i))
      wyy1(i)=vr(2,ix1(i))
      wzz1(i)=vr(3,ix1(i))
      wxx2(i)=vr(1,ix2(i))
      wyy2(i)=vr(2,ix2(i))
      wzz2(i)=vr(3,ix2(i))
      wxx3(i)=vr(1,ix3(i))
      wyy3(i)=vr(2,ix3(i))
      wzz3(i)=vr(3,ix3(i))
      wxx4(i)=vr(1,ix4(i))
      wyy4(i)=vr(2,ix4(i))
      wzz4(i)=vr(3,ix4(i))
   40 continue
c
      rho8th=.125*rhoa(lft)
      rho4th=.250*rhoa(lft)
      do 60 i=lft,llt
      dx=dx1(i)**2+dx2(i)**2+dx3(i)**2+dx4(i)**2
      dy=dy1(i)**2+dy2(i)**2+dy3(i)**2+dy4(i)**2
      dz=dz1(i)**2+dz2(i)**2+dz3(i)**2+dz4(i)**2
      enkint(i)=fibl(1,nnm1+i)*rho8th*(dx+dy+dz)
      xmomnt(i)=fibl(1,nnm1+i)*rho4th*(dx1(i)+dx2(i)+dx3(i)+dx4(i))
      ymomnt(i)=fibl(1,nnm1+i)*rho4th*(dy1(i)+dy2(i)+dy3(i)+dy4(i))
      zmomnt(i)=fibl(1,nnm1+i)*rho4th*(dz1(i)+dz2(i)+dz3(i)+dz4(i))
   60 continue
c
      call dfnls (fibl(1,nnm1+1),nip)
c
      call tranbs
c
      zneg=-.5
      zpos= .5
      do 250 m=1,nip
      ipt=m
      if (irl.eq.0) then
      zta=.5*zet(m,nip)
      elseif (irl.gt.0) then
      zta=.5*(-1.0+(m-1)*2./(nip-1))
      else
      zta=.5*rule(m,1,nrl)
      mtu=nint(rule(m,3,nrl))
      mtv=mxt(lft)
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
      bs1=.5-zta
      bs2=.5+zta
      fac=zneg*bs1+zpos*bs2
c
c     strain increments
c
      do 90 i=lft,llt
      zeta(i)=fac*thick(i)
      d1(i)=b1vx(i)+zeta(i)*b1ty(i)
      d2(i)=b2vy(i)-zeta(i)*b2tx(i)
      d3(i)=0.
      d4(i)=bxyv(i)+zeta(i)*bxyt(i)
      d5(i)=epyz(i)
      d6(i)=epzx(i)
      einc(i)=0.0
   90 continue
c
c     constitutive model evaluation
c
      call cnmdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if (istrn.ne.0) then
      if (irl.eq.0) then
      if (m.eq.isrn(1,nip)) then
      call tbstrn (strain(1,nnm1+1))
      endif
      if (m.eq.isrn(2,nip)) then
      call tbstrn (strain(7,nnm1+1))
      endif
      else
      if (m.eq.1  ) then
      call tbstrn (strain(1,nnm1+1))
      endif
      if (m.eq.nip) then
      call tbstrn (strain(7,nnm1+1))
      endif
      endif
      endif
c
c     force evaluation
c
      if (irl.eq.0) then
      fac=wgts(m,nip)
      elseif (irl.gt.0) then
      fac=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) fac=fac/2.
      else
      fac=rule(m,2,nrl)
      mxt(lft)=mtv
      endif
      if (istupd.ne.0) then
      call tbstu1 (.5*fac)
      endif
      call frcbs1 (fac,b(ipi),b(iph),nnm1,xies(nnm1+1))
  250 continue
c
      if (istupd.ne.0) then
      call tbstu2 (fibl(1,nnm1+1),b(istupd))
      endif
c
c     force components in local system
c
      call frcbs2
c
c     transform force components to global system + hourglass forces
c
      sndspd=sqrt(sndspd*rho)
      call trnfbs (rhs,rhs(1+neq),fibl(2,nnm1+1),mte,iblks)
c
      return
      end
      subroutine frcbs1 (factor,volf,epf,ioffst,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/aux36/lft,llt
      dimension volf(1),epf(1),xies(*)
      fac =factor
      faci=1./fac
      do 10 i=lft,llt
      fga(i)=fac*fga(i)
      fgb(i)=fac*fgb(i)
      fgc(i)=zeta(i)*fga(i)
      sig1m(i)=sig1m(i)+fgc(i)*sig1(i)
      sig2m(i)=sig2m(i)+fgc(i)*sig2(i)
      sig4m(i)=sig4m(i)+fgc(i)*sig4(i)
      sig1n(i)=sig1n(i)+fga(i)*sig1(i)
      sig2n(i)=sig2n(i)+fga(i)*sig2(i)
      sig4n(i)=sig4n(i)+fga(i)*sig4(i)
      sig5n(i)=sig5n(i)+fga(i)*sig5(i)
      sig6n(i)=sig6n(i)+fga(i)*sig6(i)
      sig5l(i)=sig5l(i)-fgb(i)*sig5(i)
      sig6l(i)=sig6l(i)+fgb(i)*sig6(i)
   10 continue
      if (ntbsl.eq.0.and.ntnwf.eq.0) go to 140
      do 130 i=lft,llt
      volf(i+ioffst)=volf(i+ioffst)+fga(i)
      epf(i+ioffst)=epf(i+ioffst)+ep(i)*fga(i)
  130 continue
  140 do 150 i=lft,llt
      xies(i) =xies(i)+.5*fga(i)*sarea(i)*einc(i)
      fga(i) =faci*fga(i)
      fgb(i) =faci*fgb(i)
  150 continue
      return
      end
      subroutine frcbs2
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux01/
     &fj11(128),fj12(128),fj13(128),fj21(128),fj22(128),fj23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux36/lft,llt
      common/failu/sieu(128),fail(128)
      do 10 i=lft,llt
      fmr11(i)=-py1(i)*sig2m(i)-px1(i)*sig4m(i)
      fmr12(i)= px1(i)*sig1m(i)+py1(i)*sig4m(i)
      fmr21(i)=-py2(i)*sig2m(i)-px2(i)*sig4m(i)
      fmr22(i)= px2(i)*sig1m(i)+py2(i)*sig4m(i)
      fj11(i)=px1(i)*sig1n(i)+py1(i)*sig4n(i)
      fj12(i)=py1(i)*sig2n(i)+px1(i)*sig4n(i)
      fj21(i)=px2(i)*sig1n(i)+py2(i)*sig4n(i)
      fj22(i)=py2(i)*sig2n(i)+px2(i)*sig4n(i)
      fj13(i)=px1(i)*sig6n(i)+py1(i)*sig5n(i)
      fj23(i)=px2(i)*sig6n(i)+py2(i)*sig5n(i)
      fm11(i)=sig5l(i)+fmr11(i)
      fm12(i)=sig6l(i)+fmr12(i)
      fm21(i)=sig5l(i)+fmr21(i)
      fm22(i)=sig6l(i)+fmr22(i)
      fm31(i)=sig5l(i)-fmr11(i)
      fm32(i)=sig6l(i)-fmr12(i)
      fm41(i)=sig5l(i)-fmr21(i)
      fm42(i)=sig6l(i)-fmr22(i)
   10 continue
      return
      end
      subroutine trnfbs(e,f,qs,mte,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &ft31(128),ft32(128),ft33(128),ft41(128),ft42(128),ft43(128),
     &htx(128),hty(128),gm1(128),gm2(128),gm3(128),gm4(128),
     &bsum(128),qhx(128),qhy(128),qwz(128),qtx(128),qty(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux40/
     1 x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     2 x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128)
      common/hourg/ymod,gmod,ifsv
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/presc/voltot(128)
      common/failu/sieu(128),fail(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension e(3,*),f(3,*),qs(9,*),iblks(*)
c
      ifail=0
      if (qhg.gt.1.e-04) then
      tmode=qhg*ymod/1920.0
      wmode=qhg*gmod/120.00
      xmmode=qhg*ymod/80.000
      do 10 i=lft,llt
      htx(i)=area(i)*(x3(i)-x2(i)-x4(i))
      hty(i)=area(i)*(y3(i)-y2(i)-y4(i))
      gm1(i)= 1.-px1(i)*htx(i)-py1(i)*hty(i)
      gm2(i)=-1.-px2(i)*htx(i)-py2(i)*hty(i)
      gm3(i)= 2.-gm1(i)
      gm4(i)=-2.-gm2(i)
   10 continue
      if (ifsv.eq.3) then
      do 12 i=lft,llt
      bsum(i) =2.*(px1(i)**2+px2(i)**2+py1(i)**2+py2(i)**2)
      xl(i)=area(i)*bsum(i)*thick(i)
      c1(i)=xl(i)*thick(i)**2
      c2(i)=wmode*c1(i)*area(i)
      c3(i)=xmmode*xl(i)
      c1(i)=tmode*c1(i)
   12 continue
      else
      hgfac=qhg*rhoa(lft)*sndspd/(dt1+1.e-20)
      do 15 i=lft,llt
      c3(i)=hgfac*sqrt(sarea(i))*thick(i)
      c2(i)=c3(i)
      c1(i)=.05*c3(i)*thick(i)
   15 continue
      endif
      do 20 i=lft,llt
      qhx(i)=gm1(i)*vx1(i)+gm2(i)*vx2(i)+gm3(i)*vx3(i)+gm4(i)*vx4(i)
      qhy(i)=gm1(i)*vy1(i)+gm2(i)*vy2(i)+gm3(i)*vy3(i)+gm4(i)*vy4(i)
      qwz(i)=gm1(i)*vz1(i)+gm2(i)*vz2(i)+gm3(i)*vz3(i)+gm4(i)*vz4(i)
      qtx(i)=gm1(i)*vx5(i)+gm2(i)*vx6(i)+gm3(i)*vx7(i)+gm4(i)*vx8(i)
      qty(i)=gm1(i)*vy5(i)+gm2(i)*vy6(i)+gm3(i)*vy7(i)+gm4(i)*vy8(i)
   20 continue
      if (ifsv.ne.3) then
      do 25 i=lft,llt
      qs(1,i)=0.
      qs(2,i)=0.
      qs(3,i)=0.
      qs(4,i)=0.
      qs(5,i)=0.
   25 continue
      endif
      do 30 i=lft,llt
      qs(1,i)=qs(1,i)+c3(i)*qhx(i)
      qs(2,i)=qs(2,i)+c3(i)*qhy(i)
      qs(3,i)=qs(3,i)+c2(i)*qwz(i)
      qs(4,i)=qs(4,i)+c1(i)*qtx(i)
      qs(5,i)=qs(5,i)+c1(i)*qty(i)
      ft31(i)=-ft11(i)+gm3(i)*qs(1,i)
      ft32(i)=-ft12(i)+gm3(i)*qs(2,i)
      ft33(i)=-ft13(i)+gm3(i)*qs(3,i)
      ft41(i)=-ft21(i)+gm4(i)*qs(1,i)
      ft42(i)=-ft22(i)+gm4(i)*qs(2,i)
      ft43(i)=-ft23(i)+gm4(i)*qs(3,i)
      ft11(i)= ft11(i)+gm1(i)*qs(1,i)
      ft12(i)= ft12(i)+gm1(i)*qs(2,i)
      ft13(i)= ft13(i)+gm1(i)*qs(3,i)
      ft21(i)= ft21(i)+gm2(i)*qs(1,i)
      ft22(i)= ft22(i)+gm2(i)*qs(2,i)
      ft23(i)= ft23(i)+gm2(i)*qs(3,i)
      fm11(i)= fm11(i)+gm1(i)*qs(4,i)
      fm12(i)= fm12(i)+gm1(i)*qs(5,i)
      fm21(i)= fm21(i)+gm2(i)*qs(4,i)
      fm22(i)= fm22(i)+gm2(i)*qs(5,i)
      fm31(i)= fm31(i)+gm3(i)*qs(4,i)
      fm32(i)= fm32(i)+gm3(i)*qs(5,i)
      fm41(i)= fm41(i)+gm4(i)*qs(4,i)
      fm42(i)= fm42(i)+gm4(i)*qs(5,i)
   30 continue
      else
      do 40 i=lft,llt
      ft31(i)=-ft11(i)
      ft32(i)=-ft12(i)
      ft33(i)=-ft13(i)
      ft41(i)=-ft21(i)
      ft42(i)=-ft22(i)
      ft43(i)=-ft23(i)
   40 continue
      endif
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)+gl13(i)*ft13(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)+gl23(i)*ft13(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)+gl13(i)*ft23(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)+gl23(i)*ft23(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      fx3(i)=gl11(i)*ft31(i)+gl12(i)*ft32(i)+gl13(i)*ft33(i)
      fy3(i)=gl21(i)*ft31(i)+gl22(i)*ft32(i)+gl23(i)*ft33(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)+gl33(i)*ft33(i)
      fx4(i)=gl11(i)*ft41(i)+gl12(i)*ft42(i)+gl13(i)*ft43(i)
      fy4(i)=gl21(i)*ft41(i)+gl22(i)*ft42(i)+gl23(i)*ft43(i)
      fz4(i)=gl31(i)*ft41(i)+gl32(i)*ft42(i)+gl33(i)*ft43(i)
      xmx1(i)=gl11(i)*fm11(i)+gl12(i)*fm12(i)
      xmy1(i)=gl21(i)*fm11(i)+gl22(i)*fm12(i)
      xmz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)
      xmx2(i)=gl11(i)*fm21(i)+gl12(i)*fm22(i)
      xmy2(i)=gl21(i)*fm21(i)+gl22(i)*fm22(i)
      xmz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)
      xmx3(i)=gl11(i)*fm31(i)+gl12(i)*fm32(i)
      xmy3(i)=gl21(i)*fm31(i)+gl22(i)*fm32(i)
      xmz3(i)=gl31(i)*fm31(i)+gl32(i)*fm32(i)
      xmx4(i)=gl11(i)*fm41(i)+gl12(i)*fm42(i)
      xmy4(i)=gl21(i)*fm41(i)+gl22(i)*fm42(i)
      xmz4(i)=gl31(i)*fm41(i)+gl32(i)*fm42(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
      if (mte.ne.15.and.mte.ne.19.and.mte.ne.22.and.mte.ne.24) then
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-xmy3(i)
   65 f(3,ix3(i))=f(3,ix3(i))-xmz3(i)
cdir$ ivdep
      do 70 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fz4(i)
      f(1,ix4(i))=f(1,ix4(i))-xmx4(i)
      f(2,ix4(i))=f(2,ix4(i))-xmy4(i)
   70 f(3,ix4(i))=f(3,ix4(i))-xmz4(i)
      else
      ifail=1
cdir$ ivdep
      do 75 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fail(i)*fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fail(i)*fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fail(i)*fz1(i)
      f(1,ix1(i))=f(1,ix1(i))-fail(i)*xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-fail(i)*xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-fail(i)*xmz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fail(i)*fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fail(i)*fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fail(i)*fz3(i)
      f(1,ix3(i))=f(1,ix3(i))-fail(i)*xmx3(i)
      f(2,ix3(i))=f(2,ix3(i))-fail(i)*xmy3(i)
   75 f(3,ix3(i))=f(3,ix3(i))-fail(i)*xmz3(i)
cdir$ ivdep
      do 80 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fail(i)*fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fail(i)*fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fail(i)*fz2(i)
      f(1,ix2(i))=f(1,ix2(i))-fail(i)*xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-fail(i)*xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-fail(i)*xmz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fail(i)*fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fail(i)*fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fail(i)*fz4(i)
      f(1,ix4(i))=f(1,ix4(i))-fail(i)*xmx4(i)
      f(2,ix4(i))=f(2,ix4(i))-fail(i)*xmy4(i)
   80 f(3,ix4(i))=f(3,ix4(i))-fail(i)*xmz4(i)
      endif
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=4
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
      subroutine unpctb(ixp,nmel)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),matp(128)
      common/vect13/
     1 kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
      dimension ixp(5,1)                                                vax75
c     dimension ixp(2,1)                                                cray1
c     do 10 i=1,nmel                                                    cray1
c     kka(i)=ixp(1,i)                                                   cray1
c     kkb(i)=ixp(2,i)                                                   cray1
c  10 continue                                                          cray1
c     do 20 i=1,nmel                                                    cray1
c     matp(i)=and(kka(i),3777777b)                                      cray1
c     ix3(i) =and(kkb(i),3777777b)                                      cray1
c  20 continue                                                          cray1
c     do 30 i=1,nmel                                                    cray1
c     kk1(i)=shiftr(kka(i),21)                                          cray1
c     kk2(i)=shiftr(kkb(i),21)                                          cray1
c  30 continue                                                          cray1
c     do 40 i=1,nmel                                                    cray1
c     kka(i)=shiftr(kk1(i),21)                                          cray1
c     kkb(i)=shiftr(kk2(i),21)                                          cray1
c  40 continue                                                          cray1
c     do 50 i=1,nmel                                                    cray1
c     ix1(i)=and(kk1(i),3777777b)                                       cray1
c     ix2(i)=and(kka(i),3777777b)                                       cray1
c     ix4(i)=and(kk2(i),3777777b)                                       cray1
c  50 continue                                                          cray1
      do 10 i=1,nmel                                                    vax75
      matp(i)=ixp(1,i)                                                  vax75
      ix1(i) =ixp(2,i)                                                  vax75
      ix2(i) =ixp(3,i)                                                  vax75
      ix3(i) =ixp(4,i)                                                  vax75
      ix4(i) =ixp(5,i)                                                  vax75
   10 continue                                                          vax75
      return
      end
      subroutine tbstrn(strain)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      dimension strain(12,1)
      do 10 i=lft,llt
      strain(1,i)=strain(1,i)+d1(i)
      strain(2,i)=strain(2,i)+d2(i)
      strain(3,i)=strain(3,i)+d3(i)
      strain(4,i)=strain(4,i)+.5*d4(i)
      strain(5,i)=strain(5,i)+.5*d5(i)
   10 strain(6,i)=strain(6,i)+.5*d6(i)
      return
      end
      subroutine tbstu1 (wgt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux36/lft,llt
      do 10 i=lft,llt
   10 str33(i)=str33(i)+wgt*d3(i)
      return
      end
      subroutine tbstu2 (fibl,sthick)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      dimension fibl(9,1),sthick(*)
      do 10 i=lft,llt
      str33(i)=1.+str33(i)
      fibl(1,i)=str33(i)*fibl(1,i)
   10 continue
      do 20 i=lft,llt
      sthick(ix1(i))=  max(sthick(ix1(i)),fibl(1,i))
      sthick(ix2(i))=  max(sthick(ix2(i)),fibl(1,i))
      sthick(ix3(i))=  max(sthick(ix3(i)),fibl(1,i))
      sthick(ix4(i))=  max(sthick(ix4(i)),fibl(1,i))
   20 continue
      return
      end
      subroutine cnmdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 dampk,ym,pr,mxe)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/shlopt/istrn,istupd,ibelyt,miter
      common /  / a(1)
      dimension cm(48,*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
      if (mte.eq.1) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl1s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.2)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl2s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.3)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh3sc (cm,capa)
      elseif (miter.eq.1) then
      call shl3s (cm,capa)
      elseif (miter.eq.2) then
      call sh3si (cm,capa)
      endif
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.4)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl4s (cm,a(ntmp0+1),a(n19))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.12)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl12s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.15)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl15s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.19) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh19sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl19s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh19si (cm,a(n8),a(n9),capa)
      endif
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.21) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl21s(cm,capa,a(ntmp0+1),a(n19))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.22) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl22s(cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),a(lc11))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.24)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call sh24sc (cm,a(n8),a(n9),capa)
      elseif (miter.eq.1) then
      call shl24s (cm,a(n8),a(n9),capa)
      elseif (miter.eq.2) then
      call sh24si (cm,a(n8),a(n9),capa)
      endif
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.28)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl28s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.30)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl30s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.33)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl33s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.34)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl34s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.35)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (miter.eq.0) then
      call shl35s(cm,capa)
      elseif (miter.eq.1) then
      call shl35s(cm,capa)
      elseif (miter.eq.2) then
      call shl35s(cm,capa)
      endif
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      else
      write( *,1000)
      write(13,1000)
      call adios(2)
 1000 format(//5x,'*** illegal material  for bely-tsay shell ***',
     1        /5x,'     execution aborted ')
      endif
      if (dampk.ne.0.) call rydmp2(dampk,ym,pr)
      return
      end
      subroutine tbsc1s (nc,aux,ncl)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 ax(n,m)=aux(m,n-k)
   20 continue
      return
      end
      subroutine tbsc2s (nc,aux,lav,ncl,nip,ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,71)
      common/aux36/lft,llt
      dimension aux(ncl,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 aux(m,n-k)=ax(n,m)
   20 continue
      if (nip.ne.ipt) return
      lav=lav+(llt-lft+1)*ncl
      return
      end
      subroutine dfnls (fibl,nip)
c     implicit double precision (a-h,o-z)                                    dp
c
c     lamina coordinate system
c
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux36/lft,llt
      common/rigid/xll(128)
      common/aux40/
     1x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     2x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     3 x41(128),y41(128),z41(128)
      dimension fibl(9,1)
      data third/.333333333333333/
c
      do 10 i=lft,llt
      x21(i)=x2(i)-x1(i)
      y21(i)=y2(i)-y1(i)
      z21(i)=z2(i)-z1(i)
      x31(i)=x3(i)-x1(i)
      y31(i)=y3(i)-y1(i)
      z31(i)=z3(i)-z1(i)
      x41(i)=x4(i)-x1(i)
      y41(i)=y4(i)-y1(i)
      z41(i)=z4(i)-z1(i)
      x42(i)=x4(i)-x2(i)
      y42(i)=y4(i)-y2(i)
      z42(i)=z4(i)-z2(i)
      thick(i) =fibl(1,i)
      fga(i)   =0.5*thick(i)
      fgb(i)   =.25*fga(i)
   10 continue
c
      do 20 i=lft,llt
      c1(i)=y31(i)*z42(i)-z31(i)*y42(i)
      c2(i)=z31(i)*x42(i)-x31(i)*z42(i)
      c3(i)=x31(i)*y42(i)-y31(i)*x42(i)
      xl(i)=1./sqrt(c1(i)*c1(i)+c2(i)*c2(i)+c3(i)*c3(i))
      gl13(i)=c1(i)*xl(i)
      gl23(i)=c2(i)*xl(i)
      gl33(i)=c3(i)*xl(i)
      xll(i)=x21(i)*gl13(i)+y21(i)*gl23(i)+z21(i)*gl33(i)
   20 continue
c
      do 30 i=lft,llt
      c1(i)=x21(i)-gl13(i)*xll(i)
      c2(i)=y21(i)-gl23(i)*xll(i)
      c3(i)=z21(i)-gl33(i)*xll(i)
      xl(i)=1./sqrt(c1(i)*c1(i)+c2(i)*c2(i)+c3(i)*c3(i))
      gl11(i)=c1(i)*xl(i)
      gl21(i)=c2(i)*xl(i)
      gl31(i)=c3(i)*xl(i)
      gl12(i)=gl23(i)*gl31(i)-gl33(i)*gl21(i)
      gl22(i)=gl33(i)*gl11(i)-gl13(i)*gl31(i)
      gl32(i)=gl13(i)*gl21(i)-gl23(i)*gl11(i)
   30 continue
c
      return
      end
      subroutine tranbs
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute hat quantities, b matrix (eq. 4.3), and strain
c     components outside thru thickness integration loop (eq. 3.9)
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux5/
     1b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     2b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     3epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &gm11(128),gm12(128),gm13(128),gm21(128),gm22(128),gm23(128),
     &gm31(128),gm32(128),gm33(128),px1a(128),py1a(128),px2a(128),
     &py2a(128),diag1(128),diag2(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux36/lft,llt
      common/aux40/
     &x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     &x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128),
     &x41(128),y41(128),z41(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/sides/sidmn(128)
      common/double/iprec,ncpw,unit
c
      dimension vx13(128),vx24(128),vy13(128),vy24(128),
     1 vz13(128),vz24(128),wxx13(128),wxx24(128),wyy13(128),
     2 wyy24(128),sidem(128)
      equivalence (wxx1,vx13),(wxx2,vx24),(wxx3,vy13),(wxx4,vy24),
     1        (wyy1,wxx13),(wyy2,wxx24),(wyy3,wyy13),(wyy4,wyy24),
     2        (wzz1,vz13),(wzz2,vz24)
c
      do 10 i=lft,llt
      gm11(i)=dt1*gl11(i)
      gm21(i)=dt1*gl21(i)
      gm31(i)=dt1*gl31(i)
      gm12(i)=dt1*gl12(i)
      gm22(i)=dt1*gl22(i)
      gm32(i)=dt1*gl32(i)
      gm13(i)=dt1*gl13(i)
      gm23(i)=dt1*gl23(i)
      gm33(i)=dt1*gl33(i)
      vx1(i)=gm11(i)*dx1(i) +gm21(i)*dy1(i) +gm31(i)*dz1(i)
      vy1(i)=gm12(i)*dx1(i) +gm22(i)*dy1(i) +gm32(i)*dz1(i)
      vz1(i)=gm13(i)*dx1(i) +gm23(i)*dy1(i) +gm33(i)*dz1(i)
      vx2(i)=gm11(i)*dx2(i) +gm21(i)*dy2(i) +gm31(i)*dz2(i)
      vy2(i)=gm12(i)*dx2(i) +gm22(i)*dy2(i) +gm32(i)*dz2(i)
      vz2(i)=gm13(i)*dx2(i) +gm23(i)*dy2(i) +gm33(i)*dz2(i)
      vx3(i)=gm11(i)*dx3(i) +gm21(i)*dy3(i) +gm31(i)*dz3(i)
      vy3(i)=gm12(i)*dx3(i) +gm22(i)*dy3(i) +gm32(i)*dz3(i)
      vz3(i)=gm13(i)*dx3(i) +gm23(i)*dy3(i) +gm33(i)*dz3(i)
   10 continue
      do 20 i=lft,llt
      vx4(i)=gm11(i)*dx4(i) +gm21(i)*dy4(i) +gm31(i)*dz4(i)
      vy4(i)=gm12(i)*dx4(i) +gm22(i)*dy4(i) +gm32(i)*dz4(i)
      vz4(i)=gm13(i)*dx4(i) +gm23(i)*dy4(i) +gm33(i)*dz4(i)
      vx5(i)=gm11(i)*wxx1(i)+gm21(i)*wyy1(i)+gm31(i)*wzz1(i)
      vy5(i)=gm12(i)*wxx1(i)+gm22(i)*wyy1(i)+gm32(i)*wzz1(i)
      vx6(i)=gm11(i)*wxx2(i)+gm21(i)*wyy2(i)+gm31(i)*wzz2(i)
      vy6(i)=gm12(i)*wxx2(i)+gm22(i)*wyy2(i)+gm32(i)*wzz2(i)
      vx7(i)=gm11(i)*wxx3(i)+gm21(i)*wyy3(i)+gm31(i)*wzz3(i)
      vy7(i)=gm12(i)*wxx3(i)+gm22(i)*wyy3(i)+gm32(i)*wzz3(i)
      vx8(i)=gm11(i)*wxx4(i)+gm21(i)*wyy4(i)+gm31(i)*wzz4(i)
      vy8(i)=gm12(i)*wxx4(i)+gm22(i)*wyy4(i)+gm32(i)*wzz4(i)
      x2(i) =gl11(i)*x21(i) +gl21(i)*y21(i) +gl31(i)*z21(i)
      y2(i) =gl12(i)*x21(i) +gl22(i)*y21(i) +gl32(i)*z21(i)
      x3(i) =gl11(i)*x31(i) +gl21(i)*y31(i) +gl31(i)*z31(i)
      y3(i) =gl12(i)*x31(i) +gl22(i)*y31(i) +gl32(i)*z31(i)
      x4(i) =gl11(i)*x41(i) +gl21(i)*y41(i) +gl31(i)*z41(i)
      y4(i) =gl12(i)*x41(i) +gl22(i)*y41(i) +gl32(i)*z41(i)
   20 continue
c
      do 40 i=lft,llt
      px1(i)  = .5*(y2(i)-y4(i))
      px2(i)  = .5* y3(i)
      py1(i)  =-.5*(x2(i)-x4(i))
      py2(i)  =-.5* x3(i)
      sarea(i)=2.0*(py2(i)*px1(i)-py1(i)*px2(i))
      diag1(i)=x3(i)**2+y3(i)**2
      diag2(i)=4.*(px1(i)*px1(i)+py1(i)*py1(i))
      diagm(i)=  max(diag1(i),diag2(i))
      side1   =x2(i)*x2(i)+y2(i)*y2(i)
      side2   =(x3(i)-x2(i))**2+(y3(i)-y2(i))**2
      side3   =(x4(i)-x3(i))**2+(y4(i)-y3(i))**2-1.e-14
      side4   =x4(i)*x4(i)+y4(i)*y4(i)
      sida3   =side4*(5.-sign(.5*unit,side3))+side3
      sidmn(i)=  min(side1,side2,sida3,side4)
      sidem(i)=  max(side1,side2,side3,side4)*
     1 (.625+sign(.375*unit,side3))
      fgb(i)  =sarea(i)*fgb(i)
      vx13(i) =vx1(i)-vx3(i)
      vx24(i) =vx2(i)-vx4(i)
      wxx13(i)=vx5(i)-vx7(i)
      wxx24(i)=vx6(i)-vx8(i)
      vy13(i) =vy1(i)-vy3(i)
      vy24(i) =vy2(i)-vy4(i)
      wyy13(i)=vy5(i)-vy7(i)
      wyy24(i)=vy6(i)-vy8(i)
      vz13(i) =vz1(i)-vz3(i)
      vz24(i) =vz2(i)-vz4(i)
   40 continue
c
      do 50 i=lft,llt
      area(i)=1./(sarea(i)+1.e-20)
      px1a(i)=area(i)*px1(i)
      py1a(i)=area(i)*py1(i)
      px2a(i)=area(i)*px2(i)
      py2a(i)=area(i)*py2(i)
      b1vx(i)=px1a(i)*vx13(i) +px2a(i)*vx24(i)
      b1vy(i)=px1a(i)*vy13(i) +px2a(i)*vy24(i)
      b1vz(i)=px1a(i)*vz13(i) +px2a(i)*vz24(i)
      b2vx(i)=py1a(i)*vx13(i) +py2a(i)*vx24(i)
      b2vy(i)=py1a(i)*vy13(i) +py2a(i)*vy24(i)
      b2vz(i)=py1a(i)*vz13(i) +py2a(i)*vz24(i)
      b1tx(i)=px1a(i)*wxx13(i)+px2a(i)*wxx24(i)
      b1ty(i)=px1a(i)*wyy13(i)+px2a(i)*wyy24(i)
      b2tx(i)=py1a(i)*wxx13(i)+py2a(i)*wxx24(i)
      b2ty(i)=py1a(i)*wyy13(i)+py2a(i)*wyy24(i)
      bxyv(i)=b2vx(i)+b1vy(i)
      bxyt(i)=b2ty(i)-b1tx(i)
      epyz(i)=b2vz(i)-.25*(vx5(i)+vx6(i)+vx7(i)+vx8(i))
      epzx(i)=b1vz(i)+.25*(vy5(i)+vy6(i)+vy7(i)+vy8(i))
   50 continue
c
      if (isdo.eq.0.or.isdo.eq.2) then
      do 60 i=lft,llt
   60 diagm(i)=  min(diagm(i),sidem(i))
      return
      else
      return
      endif
      end
      subroutine memb3d(rule,ixp,x,rhs,vt,strain,yhatn,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
c     simple membrane element
c
      common/    /b(1)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128),
     & str33(128)
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1 wxx1(128),wxx2(128),wxx3(128),wxx4(128),
     2 wyy1(128),wyy2(128),wyy3(128),wyy4(128),
     3 wzz1(128),wzz2(128),wzz3(128),wzz4(128),
     4 a13(128),a23(128),a33(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/shlopt/istrn,istupd,ibelyt
      common/hourg/ymod,gmod,ifsv
      common/failu/sieu(128),fail(128)
      common/energy/xinen
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),yhatn(12,*),             vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),yhatn(12,*),             cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),rule(mpusr,3,*),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),
     1 nfegp(*),ihgq(*),hgq(*),strain(12,*),isrn(2,6),ener(*),
     2 iblks(*),xies(*)
      data isrn/1,1,1,2,2,3,1,4,2,5,0,0/
c
      rho=1./ro(mxe)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      zbr=csprop(13,mxe)
      ifsv=ihgq(mxe)
      ipt=1
      rhoa(lft)=ro(mxe)
      qhg=.25*hgq(mxe)
      mte=mtype(mxe)
      if (mte.eq.20) return
      nmtcon=7+nconst(mte)
      sndspd=1.e-20
      do 30 i=lft,llt
      sig1n(i)=0.0
      sig2n(i)=0.0
      sig4n(i)=0.0
      str33(i)=0.0
   30 continue
      do 40 i=lft,llt
      fail(i)=1.0
      sieu(i)=xies(nnm1+i)
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      x3(i) =x(1,ix3(i))
      y3(i) =x(2,ix3(i))
      z3(i) =x(3,ix3(i))
      x4(i) =x(1,ix4(i))
      y4(i) =x(2,ix4(i))
      z4(i) =x(3,ix4(i))
      dx1(i)=vt(1,ix1(i))
      dy1(i)=vt(2,ix1(i))
      dz1(i)=vt(3,ix1(i))
      dx2(i)=vt(1,ix2(i))
      dy2(i)=vt(2,ix2(i))
      dz2(i)=vt(3,ix2(i))
      dx3(i)=vt(1,ix3(i))
      dy3(i)=vt(2,ix3(i))
      dz3(i)=vt(3,ix3(i))
      dx4(i)=vt(1,ix4(i))
      dy4(i)=vt(2,ix4(i))
      dz4(i)=vt(3,ix4(i))
   40 continue
c
      call dfnls (fibl(1,nnm1+1),nip)
c
      call m3dtrn
c
      do 250 m=1,nip
      ipt=m
      if (irl.lt.0) then
      mtu=nint(rule(m,3,nrl))
      mtv=mxt(lft)
      if (mtu.ne.0) then
      mxt(lft)=mtu
      endif
      endif
c
c     strain increments
c
      do 90 i=lft,llt
      d1(i)=b1vx(i)
      d2(i)=b2vy(i)
      d3(i)=0.
      d4(i)=bxyv(i)
      d5(i)=0.0
      d6(i)=0.0
      einc(i)=0.0
   90 continue
c
c     constitutive model evaluation
c
      call cnmdtb (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1 dampk,ym,prv,mxe)
c
      if (istrn.ne.0) then
      call tbstrn (strain(1,nnm1+1))
      call tbstrn (strain(7,nnm1+1))
      endif
c
c     force evaluation
c
      fac=2.0
      if (irl.lt.0) then
      mxt(lft)=mtv
      endif
      if (m.eq.1) then
      call tbstu1 (.5*fac)
      endif
      call fcm3d1 (fac,b(ipi),b(iph),nnm1,xies(nnm1+1))
  250 continue
c
      call tbstu2 (fibl(1,nnm1+1))
c
c     force components in local system
c
      call fcm3d2
c
c     transform force components to global system + hourglass forces
c
      sndspd=sqrt(sndspd*rho)
      call trnm3d (rhs,rhs(1+neq),fibl(2,nnm1+1),mte,iblks)
c
      return
      end
      subroutine fcm3d1 (factor,volf,epf,ioffst,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/aux36/lft,llt
      dimension volf(*),epf(*),xies(*)
      fac =factor
      faci=1./fac
      do 10 i=lft,llt
      fga(i)=fac*fga(i)
      fgb(i)=fac*fgb(i)
      sig1n(i)=sig1n(i)+fga(i)*sig1(i)
      sig2n(i)=sig2n(i)+fga(i)*sig2(i)
      sig4n(i)=sig4n(i)+fga(i)*sig4(i)
   10 continue
      if (ntbsl.eq.0.and.ntnwf.eq.0) go to 140
      do 130 i=lft,llt
      volf(i+ioffst)=volf(i+ioffst)+fga(i)
      epf(i+ioffst)=epf(i+ioffst)+ep(i)*fga(i)
  130 continue
  140 do 150 i=lft,llt
      xies(i) =xies(i)+.5*fga(i)*sarea(i)*einc(i)
      fga(i) =faci*fga(i)
      fgb(i) =faci*fgb(i)
  150 continue
      return
      end
      subroutine fcm3d2
c     implicit double precision (a-h,o-z)                                    dp
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux01/
     &fj11(128),fj12(128),fj13(128),fj21(128),fj22(128),fj23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux36/lft,llt
      common/failu/sieu(128),fail(128)
      do 10 i=lft,llt
      fj11(i)=px1(i)*sig1n(i)+py1(i)*sig4n(i)
      fj12(i)=py1(i)*sig2n(i)+px1(i)*sig4n(i)
      fj21(i)=px2(i)*sig1n(i)+py2(i)*sig4n(i)
      fj22(i)=py2(i)*sig2n(i)+px2(i)*sig4n(i)
   10 continue
      return
      end
      subroutine trnm3d(e,f,qs,mte,iblks)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/aux01/
     &ft11(128),ft12(128),ft13(128),ft21(128),ft22(128),ft23(128),
     &fm11(128),fm12(128),fm21(128),fm22(128),
     &fm31(128),fm32(128),fm41(128),fm42(128),
     &fmr11(128),fmr12(128),fmr21(128),fmr22(128),fmr31(128),
     &fmr32(128),fmr41(128),fmr42(128),sg5(128),sg6(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/area(128),
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 dx1(128),dx2(128),dx3(128),dx4(128),
     5 dx5(128),dx6(128),dx7(128),dx8(128),
     6 dy1(128),dy2(128),dy3(128),dy4(128),
     7 dy5(128),dy6(128),dy7(128),dy8(128),
     8 dz1(128),dz2(128),dz3(128),dz4(128),
     9 dz5(128),dz6(128),dz7(128),dz8(128)
      common/aux11/
     &ft31(128),ft32(128),ft33(128),ft41(128),ft42(128),ft43(128),
     &htx(128),hty(128),gm1(128),gm2(128),gm3(128),gm4(128),
     &bsum(128),qhx(128),qhy(128),qwz(128),qtx(128),qty(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     &fx3(128),fy3(128),fz3(128),fx4(128),fy4(128),fz4(128),
     &xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128),
     &xmx3(128),xmy3(128),xmz3(128),xmx4(128),xmy4(128),xmz4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux40/
     1 x31(128),y31(128),z31(128),x42(128),y42(128),z42(128),
     2 x21(128),y21(128),z21(128),c1(128),c2(128),c3(128),xl(128)
      common/hourg/ymod,gmod,ifsv
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/presc/voltot(128)
      common/failu/sieu(128),fail(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/savfrc(128,12),svfail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension e(3,*),f(3,*),qs(9,*),iblks(*)
c
      iflag=0
      if (qhg.gt.1.e-04) then
      xmmode=qhg*ymod/80.000
      do 10 i=lft,llt
      htx(i)=area(i)*(x3(i)-x2(i)-x4(i))
      hty(i)=area(i)*(y3(i)-y2(i)-y4(i))
      gm1(i)= 1.-px1(i)*htx(i)-py1(i)*hty(i)
      gm2(i)=-1.-px2(i)*htx(i)-py2(i)*hty(i)
      gm3(i)= 2.-gm1(i)
      gm4(i)=-2.-gm2(i)
   10 continue
      if (ifsv.eq.3) then
      do 12 i=lft,llt
      bsum(i) =2.*(px1(i)**2+px2(i)**2+py1(i)**2+py2(i)**2)
      xl(i)=area(i)*bsum(i)*thick(i)
      c3(i)=xmmode*xl(i)
   12 continue
      else
      hgfac=qhg*rhoa(lft)*sndspd/(dt1+1.e-20)
      do 15 i=lft,llt
      c3(i)=hgfac*sqrt(sarea(i))*thick(i)
   15 continue
      endif
      do 20 i=lft,llt
      qhx(i)=gm1(i)*vx1(i)+gm2(i)*vx2(i)+gm3(i)*vx3(i)+gm4(i)*vx4(i)
      qhy(i)=gm1(i)*vy1(i)+gm2(i)*vy2(i)+gm3(i)*vy3(i)+gm4(i)*vy4(i)
   20 continue
      if (ifsv.ne.3) then
      do 25 i=lft,llt
      qs(1,i)=0.
      qs(2,i)=0.
   25 continue
      endif
      do 30 i=lft,llt
      qs(1,i)=qs(1,i)+c3(i)*qhx(i)
      qs(2,i)=qs(2,i)+c3(i)*qhy(i)
      ft31(i)=-ft11(i)+gm3(i)*qs(1,i)
      ft32(i)=-ft12(i)+gm3(i)*qs(2,i)
      ft41(i)=-ft21(i)+gm4(i)*qs(1,i)
      ft42(i)=-ft22(i)+gm4(i)*qs(2,i)
      ft11(i)= ft11(i)+gm1(i)*qs(1,i)
      ft12(i)= ft12(i)+gm1(i)*qs(2,i)
      ft21(i)= ft21(i)+gm2(i)*qs(1,i)
      ft22(i)= ft22(i)+gm2(i)*qs(2,i)
   30 continue
      else
      do 40 i=lft,llt
      ft31(i)=-ft11(i)
      ft32(i)=-ft12(i)
      ft41(i)=-ft21(i)
      ft42(i)=-ft22(i)
   40 continue
      endif
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)
      fx3(i)=gl11(i)*ft31(i)+gl12(i)*ft32(i)
      fy3(i)=gl21(i)*ft31(i)+gl22(i)*ft32(i)
      fz3(i)=gl31(i)*ft31(i)+gl32(i)*ft32(i)
      fx4(i)=gl11(i)*ft41(i)+gl12(i)*ft42(i)
      fy4(i)=gl21(i)*ft41(i)+gl22(i)*ft42(i)
      fz4(i)=gl31(i)*ft41(i)+gl32(i)*ft42(i)
   50 continue
      do 85 n=1,nnc
      lcn=lczc+n
      i0=iblks(lcn)
      i1=iblks(lcn+1)-1
      if (mte.ne.15.and.mte.ne.19.and.mte.ne.22.and.mte.ne.24) then
cdir$ ivdep
      do 65 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fz3(i)
   65 continue
cdir$ ivdep
      do 70 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fz4(i)
   70 continue
      else
      ifail=1
cdir$ ivdep
      do 75 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fail(i)*fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fail(i)*fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fail(i)*fz1(i)
      e(1,ix3(i))=e(1,ix3(i))-fail(i)*fx3(i)
      e(2,ix3(i))=e(2,ix3(i))-fail(i)*fy3(i)
      e(3,ix3(i))=e(3,ix3(i))-fail(i)*fz3(i)
   75 continue
cdir$ ivdep
      do 80 i=i0,i1
      e(1,ix2(i))=e(1,ix2(i))-fail(i)*fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fail(i)*fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fail(i)*fz2(i)
      e(1,ix4(i))=e(1,ix4(i))-fail(i)*fx4(i)
      e(2,ix4(i))=e(2,ix4(i))-fail(i)*fy4(i)
      e(3,ix4(i))=e(3,ix4(i))-fail(i)*fz4(i)
   80 continue
      endif
   85 continue
      if (output) then
      nblksz=128
      nwords=12*nblksz
      call blkcpy(fx1,savfrc,nwords)
      ndof=4
      if (ifail.eq.1) call blkcpy (fail,svfail,128)
      endif
      return
      end
