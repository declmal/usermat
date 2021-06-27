      subroutine m3dtrn
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
      vx4(i)=gm11(i)*dx4(i) +gm21(i)*dy4(i) +gm31(i)*dz4(i)
      vy4(i)=gm12(i)*dx4(i) +gm22(i)*dy4(i) +gm32(i)*dz4(i)
      vz4(i)=gm13(i)*dx4(i) +gm23(i)*dy4(i) +gm33(i)*dz4(i)
      x2(i) =gl11(i)*x21(i) +gl21(i)*y21(i) +gl31(i)*z21(i)
      y2(i) =gl12(i)*x21(i) +gl22(i)*y21(i) +gl32(i)*z21(i)
      x3(i) =gl11(i)*x31(i) +gl21(i)*y31(i) +gl31(i)*z31(i)
      y3(i) =gl12(i)*x31(i) +gl22(i)*y31(i) +gl32(i)*z31(i)
      x4(i) =gl11(i)*x41(i) +gl21(i)*y41(i) +gl31(i)*z41(i)
      y4(i) =gl12(i)*x41(i) +gl22(i)*y41(i) +gl32(i)*z41(i)
   10 continue
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
     1  (.625+sign(.375*unit,side3))
      fgb(i)  =sarea(i)*fgb(i)
      vx13(i) =vx1(i)-vx3(i)
      vx24(i) =vx2(i)-vx4(i)
      vy13(i) =vy1(i)-vy3(i)
      vy24(i) =vy2(i)-vy4(i)
      vz13(i) =vz1(i)-vz3(i)
      vz24(i) =vz2(i)-vz4(i)
      area(i)=1./(sarea(i)+1.e-20)
      px1a(i)=area(i)*px1(i)
      py1a(i)=area(i)*py1(i)
      px2a(i)=area(i)*px2(i)
      py2a(i)=area(i)*py2(i)
      b1vx(i)=px1a(i)*vx13(i) +px2a(i)*vx24(i)
      b1vy(i)=px1a(i)*vy13(i) +px2a(i)*vy24(i)
      b2vx(i)=py1a(i)*vx13(i) +py2a(i)*vx24(i)
      b2vy(i)=py1a(i)*vy13(i) +py2a(i)*vy24(i)
      bxyv(i)=b2vx(i)+b1vy(i)
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
      subroutine tshell
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common /   / a(1)
c
      lc0n=lc0+numelh
      npmx=(nt11-nt06)/numelt
      call thicks (a(n1),a(n2),a(n3),a(n4f),a(lc0n),a(lc10),a(lc9),
     1 a(lc11),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),
     2 a(nt01),a(nt02),a(nt03),a,a(nt04),a(nt06),a(n4h),mpusr,npmx,
     3 a(nt14),a(nt17),a(lc1t),a(nt11),a(nt12),a(n80),a(n80+nmmat),
     4 a(n80+2*nmmat))
c
      return
      end
      subroutine thicks (mtype,ro,cm,csprop,u,a,v,x,ieost,eosp,
     1 ihgq,hgq,iqtype,bkqs,nsubgv,mtnum,nfegp,b,auxvec,thks,
     2 rule,mpusr,npmx,lochvt,ntshel,ipst,nnct,iblkt,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk12/b12,b2,qhg
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux5/
     1aj11(128),aj12(128),aj13(128),aj21(128),aj22(128),aj23(128),
     2aj31(128),aj32(128),aj33(128),ai11(128),ai12(128),ai13(128),
     3ai21(128),ai22(128),ai23(128),ai31(128),ai32(128),ai33(128),
     4a1111(128),a1113(128),a1115(128),a1117(128),a2111(128),
     5a2113(128),a2115(128),a2117(128),a3111(128),a3113(128),
     6a3115(128),a3117(128),a1221(128),a1222(128),a1225(128),
     7a1226(128),a2221(128),a2222(128),a2225(128),a2226(128),
     8a3221(128),a3222(128),a3225(128),a3226(128),a1331(128),
     9a1332(128),a1333(128),a1334(128),a2331(128),a2332(128),
     &a2333(128),a2334(128),a3331(128),a3332(128),a3333(128),
     &a3334(128),vlinv(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux13/
     1 x12(128),x34(128),x56(128),x78(128),y12(128),y34(128),
     2 y56(128),y78(128),z12(128),z34(128),z56(128),z78(128),
     3 x14(128),x23(128),x58(128),x67(128),y14(128),y23(128),
     4 y58(128),y67(128),z14(128),z23(128),z58(128),z67(128),
     6 x15(128),x26(128),x37(128),x48(128),y15(128),y26(128),
     7 y37(128),y48(128),z15(128),z26(128),z37(128),z48(128),
     8 x1234(128),x5678(128),x1423(128),x5867(128),
     9 y1234(128),y5678(128),y1423(128),y5867(128),
     & z1234(128),z5678(128),z1423(128),z5867(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/presc/centpr(128)
      common/nwixa/nwcon
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/tsarry/tslimt,tsarry(128)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      dimension mtype(*),ro(*),cm(48,*),csprop(24,*),u(*),a(*),v(*),
     1  x(*),ieost(*),eosp(48,*),ihgq(*),hgq(*),iqtype(*),bkqs(3,*),
     2  nsubgv(*),mtnum(*),nfegp(*),b(*),auxvec(*),thks(npmx,*),
     3  rule(mpusr,3,*),lochvt(*),ntshel(*),ipst(*),nnct(*),iblkt(*),
     4  dampk(*),ym(*),prv(*)
c
      third=-1./3.
      nelg=numelt/128
      l11=1
      lcz=0
      ic=0
      lczc=0
      if (128*nelg.lt.numelt) nelg=nelg+1
      do 10 i=1,128
   10 tsarry(i)=1.0
      do 410 nn=1,nelg
      noco=nn
      nnm1=128*(nn-1)
      nmel=128
      if (nn.eq.nelg) nmel=numelt-128*(nelg-1)
      call unpkts (ipst(l11),nwcon)
      nsubg=nsubgv(nn)
      do 400 n=1,nsubg
      lcn=lcz+n
      ic =ic+1
      nnc=nnct(ic)
      mxe=mtnum(lcn)
      nip=nint(csprop(2,mxe))
      irl=nint(csprop(4,mxe))
      nrl=iabs(irl)
      if (nip.gt.5.and.irl.eq.0) irl=1
      ipt=1
      if (mxe) 180,390,180
  180 lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvt(nnm1+lft)
      mte=mtype(mxe)
      if (mte.eq.20) go to 390
      nes=ieost(mxe)
      ibq=iqtype(mxe)
      ihg=ihgq(mxe)
      qhg=hgq(mxe)
      b12=bkqs(1,mxe)
      b2=bkqs(2,mxe)
      b3=bkqs(3,mxe)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      mx=mxt(lft)
      rhoa(lft)=ro(mx)
      call gather (x,v)
      sndspd=1.e-20
      do 200 i=lft,llt
      x12(i)=x1(i)-x2(i)
      x34(i)=x3(i)-x4(i)
      x56(i)=x5(i)-x6(i)
      x78(i)=x7(i)-x8(i)
      x14(i)=x1(i)-x4(i)
      x23(i)=x2(i)-x3(i)
      x58(i)=x5(i)-x8(i)
      x67(i)=x6(i)-x7(i)
      x15(i)=x1(i)-x5(i)
      x26(i)=x2(i)-x6(i)
      x37(i)=x3(i)-x7(i)
      x48(i)=x4(i)-x8(i)
      y12(i)=y1(i)-y2(i)
      y34(i)=y3(i)-y4(i)
      y56(i)=y5(i)-y6(i)
      y78(i)=y7(i)-y8(i)
      y14(i)=y1(i)-y4(i)
      y23(i)=y2(i)-y3(i)
      y58(i)=y5(i)-y8(i)
      y67(i)=y6(i)-y7(i)
      y15(i)=y1(i)-y5(i)
      y26(i)=y2(i)-y6(i)
      y37(i)=y3(i)-y7(i)
      y48(i)=y4(i)-y8(i)
      z12(i)=z1(i)-z2(i)
      z34(i)=z3(i)-z4(i)
      z56(i)=z5(i)-z6(i)
      z78(i)=z7(i)-z8(i)
      z14(i)=z1(i)-z4(i)
      z23(i)=z2(i)-z3(i)
      z58(i)=z5(i)-z8(i)
      z67(i)=z6(i)-z7(i)
      z15(i)=z1(i)-z5(i)
      z26(i)=z2(i)-z6(i)
      z37(i)=z3(i)-z7(i)
  200 z48(i)=z4(i)-z8(i)
      do 210 i=lft,llt
      x1234(i)=x12(i)-x34(i)
      y1234(i)=y12(i)-y34(i)
      z1234(i)=z12(i)-z34(i)
      x5678(i)=x56(i)-x78(i)
      y5678(i)=y56(i)-y78(i)
      z5678(i)=z56(i)-z78(i)
      x1423(i)=x14(i)+x23(i)
      y1423(i)=y14(i)+y23(i)
      z1423(i)=z14(i)+z23(i)
      x5867(i)=x58(i)+x67(i)
      y5867(i)=y58(i)+y67(i)
      z5867(i)=z58(i)+z67(i)
      aj31(i)=-.1250*(x15(i)+x26(i)+x37(i)+x48(i))
      aj32(i)=-.1250*(y15(i)+y26(i)+y37(i)+y48(i))
      aj33(i)=-.1250*(z15(i)+z26(i)+z37(i)+z48(i))
  210 continue
      call dfnlts
      do 250 m=1,nip
      ipt=m
      if (irl.eq.0) then
      zta=.5*zeta(m,nip)
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
      call strni (u(1+128*(nn-1)),irl,zta)
      call gthks(thks(1,1+128*(nn-1)),ipt,npmx)
      call conmd (nmtcon,auxvec,cm,lav,mte,nip,ipt,csprop(1,mxe),
     1   dampk(mxe),ym(mxe),prv(mxe))
      call srthks(thks(1,1+128*(nn-1)),ipt,npmx)
      if (irl.eq.0) then
      fac=wgts(m,nip)
      elseif (irl.gt.0) then
      fac=2./(nip-1)
      if (m.eq.1.or.m.eq.nip) fac=fac/2.
      else
      fac=rule(m,2,nrl)
      mxt(lft)=mtv
      endif
      call forci (fac,ipt)
  250 continue
      ipt=1
      call strn0 (ihg)
      call felts
      call tsblkq (ntshel)
      if (ihg.eq.2) then
      call tshgfb
      else
      call tshgmd
      endif
      call forc0 (a,iblkt)
  390 lczc=lczc+nnc+1
  400 continue
      l11=l11+nwcon*nmel
      lcz=lcz+nsubg+1
  410 continue
c
      return
      end
      subroutine conmd (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/shlopt/istrn,istupd,ibelyt,miter
      dimension cm(*),auxvec(*)
      ibtsav=ibelyt
      ibelyt=0
      lavloc=(ipt-1)*nmtcon+lav
      call tspc1 (nmtcon,auxvec(lavloc),nip*nmtcon)
      call rttrs
      if (mte.eq.1) then
      call ldsig(ipt)
      call ldstr
      call shl1s (cm,capa)
      elseif (mte.eq.3) then
      call rttrn
      call ldsig(ipt)
      call ldstr
      if (miter.eq.0) then
      call sh3sc (cm,capa)
      elseif (miter.eq.1) then
      call shl3s (cm,capa)
      elseif (miter.eq.2) then
      call sh3si (cm,capa)
      endif
      elseif (mte.eq.30) then
      call rttrn
      call ldsig(ipt)
      call ldstr
      call shl30s(cm,capa)
      elseif (mte.eq.33) then
      call ldsig(ipt)
      call ldstr
      call shl33s (cm,capa)
      elseif (mte.eq.34) then
      call ldsig(ipt)
      call ldstr
      call shl34s (cm,capa)
      elseif (mte.eq.35) then
      call ldsig(ipt)
      call ldstr
      if (miter.eq.0) then
      call shl35s(cm,capa)
      elseif (miter.eq.1) then
      call shl35s(cm,capa)
      elseif (miter.eq.2) then
      call shl35s(cm,capa)
      endif
      endif
      call rydmp2(dampk,ym,prv)
      call gblss
      call tspc2 (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      ibelyt=ibtsav
      return
      end
      subroutine dfnlts
c     implicit double precision (a-h,o-z)                                    dp
c
c     lamina coordinate system
c
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux36/lft,llt
c
      do 10 i=lft,llt
      xc1=.5*(x1(i)+x5(i))
      xc2=.5*(x2(i)+x6(i))
      xc3=.5*(x3(i)+x7(i))
      xc4=.5*(x4(i)+x8(i))
      yc1=.5*(y1(i)+y5(i))
      yc2=.5*(y2(i)+y6(i))
      yc3=.5*(y3(i)+y7(i))
      yc4=.5*(y4(i)+y8(i))
      zc1=.5*(z1(i)+z5(i))
      zc2=.5*(z2(i)+z6(i))
      zc3=.5*(z3(i)+z7(i))
      zc4=.5*(z4(i)+z8(i))
      x21=xc2-xc1
      y21=yc2-yc1
      z21=zc2-zc1
      x31=xc3-xc1
      y31=yc3-yc1
      z31=zc3-zc1
      x41=xc4-xc1
      y41=yc4-yc1
      z41=zc4-zc1
      x42=xc4-xc2
      y42=yc4-yc2
      z42=zc4-zc2
      c1=y31*z42-z31*y42
      c2=z31*x42-x31*z42
      c3=x31*y42-y31*x42
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s31(i)=c1*xl
      s32(i)=c2*xl
      s33(i)=c3*xl
      xl=x21*s31(i)+y21*s32(i)+z21*s33(i)
      c1=x21-s31(i)*xl
      c2=y21-s32(i)*xl
      c3=z21-s33(i)*xl
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s11(i)=c1*xl
      s12(i)=c2*xl
      s13(i)=c3*xl
      s21(i)=s32(i)*s13(i)-s33(i)*s12(i)
      s22(i)=s33(i)*s11(i)-s31(i)*s13(i)
      s23(i)=s31(i)*s12(i)-s32(i)*s11(i)
   10 continue
c
      return
      end
      subroutine ldsig(ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux5/
     1aj11(128),aj12(128),aj13(128),aj21(128),aj22(128),aj23(128),
     2aj31(128),aj32(128),aj33(128),ai11(128),ai12(128),ai13(128),
     3ai21(128),ai22(128),ai23(128),ai31(128),ai32(128),ai33(128),
     4 a1111(128),a1113(128),a1115(128),a1117(128),a2111(128),
     5 a2113(128),a2115(128),a2117(128),a3111(128),a3113(128),
     6 a3115(128),a3117(128),a1221(128),a1222(128),a1225(128),
     7 a1226(128),a2221(128),a2222(128),a2225(128),a2226(128),
     8 a3221(128),a3222(128),a3225(128),a3226(128),a1331(128),
     9 a1332(128),a1333(128),a1334(128),a2331(128),a2332(128),
     & a2333(128),a2334(128),a3331(128),a3332(128),a3333(128),
     & a3334(128),enrm(128)
c
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
      subroutine ldstr
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux5/
     1aj11(128),aj12(128),aj13(128),aj21(128),aj22(128),aj23(128),
     2aj31(128),aj32(128),aj33(128),ai11(128),ai12(128),ai13(128),
     3ai21(128),ai22(128),ai23(128),ai31(128),ai32(128),ai33(128),
     4 a1111(128),a1113(128),a1115(128),a1117(128),a2111(128),
     5 a2113(128),a2115(128),a2117(128),a3111(128),a3113(128),
     6 a3115(128),a3117(128),a1221(128),a1222(128),a1225(128),
     7 a1226(128),a2221(128),a2222(128),a2225(128),a2226(128),
     8 a3221(128),a3222(128),a3225(128),a3226(128),a1331(128),
     9 a1332(128),a1333(128),a1334(128),a2331(128),a2332(128),
     & a2333(128),a2334(128),a3331(128),a3332(128),a3333(128),
     & a3334(128),enrm(128)
c
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
      d3(i)=    s31(i)*a13(i)+s32(i)*a23(i)+s33(i)*a33(i)
      d4(i)=2.*(s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i))
      d5(i)=2.*(s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i))
      d6(i)=2.*(s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i))
   40 continue
      return
      end
      subroutine gblss
c     implicit double precision (a-h,o-z)                                    dp
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
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
      do 30 i=lft,llt
      a31(i)=sign3(i)*s31(i)
      a32(i)=sign3(i)*s32(i)
      a33(i)=sign3(i)*s33(i)
   30 continue
      do 40 i=lft,llt
      sign1(i)=s31(i)*a31(i)
      sign2(i)=s32(i)*a32(i)
      sign3(i)=s33(i)*a33(i)
      sign4(i)=s31(i)*a32(i)
      sign5(i)=s32(i)*a33(i)
      sign6(i)=s31(i)*a33(i)
   40 continue
      return
      end
      subroutine unpkts(ixp,nwcon)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux32/kka(128),kkb(128),kkc(128),
     1 kk1(128),kk2(128),kk3(128),dxy(128),
     2 dyx(128),dyz(128),dzy(128),dzx(128),
     3 dxz(128),vx17(128),vx28(128),vx35(128),
     4 vx46(128),vy17(128),vy28(128),
     5 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      dimension ixp(nwcon,1)
c     data ishft1,imask/ 21,3777777b/                                   cray1
c
c     do 10 i=1,nmel                                                    cray1
c     kka(i)=ixp(1,i)                                                   cray1
c     kkb(i)=ixp(2,i)                                                   cray1
c  10 kkc(i)=ixp(3,i)                                                   cray1
c     do 20 i=1,nmel                                                    cray1
c     mxt(i)=and(kka(i),imask)                                          cray1
c     ix3(i)=and(kkb(i),imask)                                          cray1
c  20 ix6(i)=and(kkc(i),imask)                                          cray1
c     do 30 i=1,nmel                                                    cray1
c     kk1(i)=shiftr(kka(i),ishft1)                                      cray1
c     kk2(i)=shiftr(kkb(i),ishft1)                                      cray1
c  30 kk3(i)=shiftr(kkc(i),ishft1)                                      cray1
c     do 40 i=1,nmel                                                    cray1
c     kka(i)=shiftr(kk1(i),ishft1)                                      cray1
c     kkb(i)=shiftr(kk2(i),ishft1)                                      cray1
c  40 kkc(i)=shiftr(kk3(i),ishft1)                                      cray1
c     do 50 i=1,nmel                                                    cray1
c     ix1(i)=and(kk1(i),imask)                                          cray1
c     ix2(i)=and(kka(i),imask)                                          cray1
c     ix4(i)=and(kk2(i),imask)                                          cray1
c     ix5(i)=and(kkb(i),imask)                                          cray1
c     ix7(i)=and(kk3(i),imask)                                          cray1
c  50 ix8(i)=and(kkc(i),imask)                                          cray1
      do 10 i=1,nmel                                                    vax75
      mxt(i)=ixp(1,i)                                                   vax75
      ix1(i)=ixp(2,i)                                                   vax75
      ix2(i)=ixp(3,i)                                                   vax75
      ix3(i)=ixp(4,i)                                                   vax75
      ix4(i)=ixp(5,i)                                                   vax75
      ix5(i)=ixp(6,i)                                                   vax75
      ix6(i)=ixp(7,i)                                                   vax75
      ix7(i)=ixp(8,i)                                                   vax75
      ix8(i)=ixp(9,i)                                                   vax75
   10 continue                                                          vax75
c
      return
      end
      subroutine tspc1 (nc,aux,ncl)
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
      subroutine tspc2 (nc,aux,lav,ncl,nip,ipt)
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
      subroutine gather(x,v)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 vx1(128),vx2(128),vx3(128),vx4(128),
     5 vx5(128),vx6(128),vx7(128),vx8(128),
     6 vy1(128),vy2(128),vy3(128),vy4(128),
     7 vy5(128),vy6(128),vy7(128),vy8(128),
     8 vz1(128),vz2(128),vz3(128),vz4(128),
     9 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
c
      dimension x(3,1),v(3,1)
c
      do 10 i=lft,llt
      x1(i)=x(1,ix1(i))
      y1(i)=x(2,ix1(i))
      z1(i)=x(3,ix1(i))
      vx1(i)=v(1,ix1(i))
      vy1(i)=v(2,ix1(i))
      vz1(i)=v(3,ix1(i))
      x2(i)=x(1,ix2(i))
      y2(i)=x(2,ix2(i))
      z2(i)=x(3,ix2(i))
      vx2(i)=v(1,ix2(i))
      vy2(i)=v(2,ix2(i))
      vz2(i)=v(3,ix2(i))
      x3(i)=x(1,ix3(i))
      y3(i)=x(2,ix3(i))
      z3(i)=x(3,ix3(i))
      vx3(i)=v(1,ix3(i))
      vy3(i)=v(2,ix3(i))
      vz3(i)=v(3,ix3(i))
      x4(i)=x(1,ix4(i))
      y4(i)=x(2,ix4(i))
      z4(i)=x(3,ix4(i))
      vx4(i)=v(1,ix4(i))
      vy4(i)=v(2,ix4(i))
      vz4(i)=v(3,ix4(i))
      x5(i)=x(1,ix5(i))
      y5(i)=x(2,ix5(i))
      z5(i)=x(3,ix5(i))
      vx5(i)=v(1,ix5(i))
      vy5(i)=v(2,ix5(i))
      vz5(i)=v(3,ix5(i))
      x6(i)=x(1,ix6(i))
      y6(i)=x(2,ix6(i))
      z6(i)=x(3,ix6(i))
      vx6(i)=v(1,ix6(i))
      vy6(i)=v(2,ix6(i))
      vz6(i)=v(3,ix6(i))
      x7(i)=x(1,ix7(i))
      y7(i)=x(2,ix7(i))
      z7(i)=x(3,ix7(i))
      vx7(i)=v(1,ix7(i))
      vy7(i)=v(2,ix7(i))
      vz7(i)=v(3,ix7(i))
      x8(i)=x(1,ix8(i))
      y8(i)=x(2,ix8(i))
      z8(i)=x(3,ix8(i))
      vx8(i)=v(1,ix8(i))
      vy8(i)=v(2,ix8(i))
   10 vz8(i)=v(3,ix8(i))
      return
      end
      subroutine gthks (thks,ipt,npmx)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux36/lft,llt
      dimension thks(npmx,1)
      do 10 i=lft,llt
      sign3(i)=thks(ipt,i)
   10 continue
      return
      end
      subroutine srthks (thks,ipt,npmx)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux36/lft,llt
      dimension thks(npmx,1)
      do 10 i=lft,llt
      thks(ipt,i)=sign0(i)
   10 continue
      return
      end
      subroutine strni (volo,irl,t)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk61/p11,p12,p13,p14,p15,p16,p17,p18,
     1            p21,p22,p23,p24,p25,p26,p27,p28,
     2            p31,p32,p33,p34,p35,p36,p37,p38
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     1aj11(128),aj12(128),aj13(128),aj21(128),aj22(128),aj23(128),
     2aj31(128),aj32(128),aj33(128),ai11(128),ai12(128),ai13(128),
     3ai21(128),ai22(128),ai23(128),ai31(128),ai32(128),ai33(128),
     4 a1111(128),a1113(128),a1115(128),a1117(128),a2111(128),
     5 a2113(128),a2115(128),a2117(128),a3111(128),a3113(128),
     6 a3115(128),a3117(128),a1221(128),a1222(128),a1225(128),
     7 a1226(128),a2221(128),a2222(128),a2225(128),a2226(128),
     8 a3221(128),a3222(128),a3225(128),a3226(128),a1331(128),
     9 a1332(128),a1333(128),a1334(128),a2331(128),a2332(128),
     & a2333(128),a2334(128),a3331(128),a3332(128),a3333(128),
     & a3334(128),vlinv(128)
      common/aux9/vlrho(128),vlinc(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 vx1(128),vx2(128),vx3(128),vx4(128),
     5 vx5(128),vx6(128),vx7(128),vx8(128),
     6 vy1(128),vy2(128),vy3(128),vy4(128),
     7 vy5(128),vy6(128),vy7(128),vy8(128),
     8 vz1(128),vz2(128),vz3(128),vz4(128),
     9 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux13/
     1 x12(128),x34(128),x56(128),x78(128),y12(128),y34(128),
     2 y56(128),y78(128),z12(128),z34(128),z56(128),z78(128),
     3 x14(128),x23(128),x58(128),x67(128),y14(128),y23(128),
     4 y58(128),y67(128),z14(128),z23(128),z58(128),z67(128),
     6 x15(128),x26(128),x37(128),x48(128),y15(128),y26(128),
     7 y37(128),y48(128),z15(128),z26(128),z37(128),z48(128),
     8 x1234(128),x5678(128),x1423(128),x5867(128),
     9 y1234(128),y5678(128),y1423(128),y5867(128),
     & z1234(128),z5678(128),z1423(128),z5867(128)
      common/aux18/dd(128),dfe(128)
      common/aux32/a17(128),a28(128),dett(128),
     1 aj1(128),aj2(128),aj3(128),dxy(128),
     2 dyx(128),dyz(128),dzy(128),dzx(128),
     3 dxz(128),vx17(128),vx28(128),vx35(128),
     4 vx46(128),vy17(128),vy28(128),
     5 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux36/lft,llt
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      dimension volo(1)
c
c     get basis functions for the current integration point
c
      call basisf (p11,p21,p31,irl,t)
c
      do 10 i=lft,llt
      aj11(i)=p11*x1234(i)+p15*x5678(i)
      aj12(i)=p11*y1234(i)+p15*y5678(i)
      aj13(i)=p11*z1234(i)+p15*z5678(i)
      aj21(i)=p21*x1423(i)+p25*x5867(i)
      aj22(i)=p21*y1423(i)+p25*y5867(i)
      aj23(i)=p21*z1423(i)+p25*z5867(i)
   10 continue
c
      do 20 i=lft,llt
      ai11(i)= aj22(i)*aj33(i)-aj23(i)*aj32(i)
      ai21(i)=-aj21(i)*aj33(i)+aj23(i)*aj31(i)
      ai31(i)= aj21(i)*aj32(i)-aj22(i)*aj31(i)
      vlinc(i)=aj11(i)*ai11(i)+aj12(i)*ai21(i)+aj13(i)*ai31(i)
      vlinv(i)=1./vlinc(i)
   20 continue
c
      do 30 i=lft,llt
      ai11(i)=vlinv(i)*ai11(i)
      ai21(i)=vlinv(i)*ai21(i)
      ai31(i)=vlinv(i)*ai31(i)
      ai12(i)=vlinv(i)*(-aj12(i)*aj33(i)+aj13(i)*aj32(i))
      ai22(i)=vlinv(i)*( aj11(i)*aj33(i)-aj13(i)*aj31(i))
      ai32(i)=vlinv(i)*(-aj11(i)*aj32(i)+aj12(i)*aj31(i))
      ai13(i)=vlinv(i)*( aj12(i)*aj23(i)-aj13(i)*aj22(i))
      ai23(i)=vlinv(i)*(-aj11(i)*aj23(i)+aj13(i)*aj21(i))
      ai33(i)=vlinv(i)*( aj11(i)*aj22(i)-aj12(i)*aj21(i))
   30 continue
c
      do 40 i=lft,llt
      a1111(i)=ai11(i)*p11
      a2111(i)=ai21(i)*p11
      a3111(i)=ai31(i)*p11
      a1221(i)=ai12(i)*p21
      a2221(i)=ai22(i)*p21
      a3221(i)=ai32(i)*p21
      a1331(i)=ai13(i)*p31
      a2331(i)=ai23(i)*p31
      a3331(i)=ai33(i)*p31
      a1113(i)=a1331(i)+a1221(i)
      a1117(i)=a1331(i)-a1221(i)
      a2113(i)=a2331(i)+a2221(i)
      a2117(i)=a2331(i)-a2221(i)
      a3113(i)=a3331(i)+a3221(i)
      a3117(i)=a3331(i)-a3221(i)
   40 continue
      do 50 i=lft,llt
      px1(i)= a1111(i)+a1113(i)
      px2(i)=-a1111(i)+a1113(i)
      px3(i)=-a1111(i)+a1117(i)
      px4(i)= a1111(i)+a1117(i)
      py1(i)= a2111(i)+a2113(i)
      py2(i)=-a2111(i)+a2113(i)
      py3(i)=-a2111(i)+a2117(i)
      py4(i)= a2111(i)+a2117(i)
      pz1(i)= a3111(i)+a3113(i)
      pz2(i)=-a3111(i)+a3113(i)
      pz3(i)=-a3111(i)+a3117(i)
      pz4(i)= a3111(i)+a3117(i)
   50 continue
c
      do 90 i=lft,llt
      a1115(i)=ai11(i)*p15
      a2115(i)=ai21(i)*p15
      a3115(i)=ai31(i)*p15
      a1225(i)=ai12(i)*p25
      a2225(i)=ai22(i)*p25
      a3225(i)=ai32(i)*p25
      a1113(i)=a1331(i)-a1225(i)
      a1117(i)=a1331(i)+a1225(i)
      a2113(i)=a2331(i)-a2225(i)
      a2117(i)=a2331(i)+a2225(i)
      a3113(i)=a3331(i)-a3225(i)
      a3117(i)=a3331(i)+a3225(i)
      px5(i)= a1115(i)-a1113(i)
      px6(i)=-a1115(i)-a1113(i)
      px7(i)=-a1115(i)-a1117(i)
      px8(i)= a1115(i)-a1117(i)
      py5(i)= a2115(i)-a2113(i)
      py6(i)=-a2115(i)-a2113(i)
      py7(i)=-a2115(i)-a2117(i)
      py8(i)= a2115(i)-a2117(i)
      pz5(i)= a3115(i)-a3113(i)
      pz6(i)=-a3115(i)-a3113(i)
      pz7(i)=-a3115(i)-a3117(i)
      pz8(i)= a3115(i)-a3117(i)
   90 continue
c
      do 100  i=lft,llt
      dxx(i)= px1(i)*vx1(i)+px2(i)*vx2(i)+px3(i)*vx3(i)+px4(i)*vx4(i)+
     & px5(i)*vx5(i)+px6(i)*vx6(i)+px7(i)*vx7(i)+px8(i)*vx8(i)
      dyy(i)= py1(i)*vy1(i)+py2(i)*vy2(i)+py3(i)*vy3(i)+py4(i)*vy4(i)+
     & py5(i)*vy5(i)+py6(i)*vy6(i)+py7(i)*vy7(i)+py8(i)*vy8(i)
      dzz(i)= pz1(i)*vz1(i)+pz2(i)*vz2(i)+pz3(i)*vz3(i)+pz4(i)*vz4(i)+
     & pz5(i)*vz5(i)+pz6(i)*vz6(i)+pz7(i)*vz7(i)+pz8(i)*vz8(i)
      dxy(i)= py1(i)*vx1(i)+py2(i)*vx2(i)+py3(i)*vx3(i)+py4(i)*vx4(i)+
     & py5(i)*vx5(i)+py6(i)*vx6(i)+py7(i)*vx7(i)+py8(i)*vx8(i)
      dxz(i)= pz1(i)*vx1(i)+pz2(i)*vx2(i)+pz3(i)*vx3(i)+pz4(i)*vx4(i)+
     & pz5(i)*vx5(i)+pz6(i)*vx6(i)+pz7(i)*vx7(i)+pz8(i)*vx8(i)
      dyz(i)= pz1(i)*vy1(i)+pz2(i)*vy2(i)+pz3(i)*vy3(i)+pz4(i)*vy4(i)+
     & pz5(i)*vy5(i)+pz6(i)*vy6(i)+pz7(i)*vy7(i)+pz8(i)*vy8(i)
      dyx(i)= px1(i)*vy1(i)+px2(i)*vy2(i)+px3(i)*vy3(i)+px4(i)*vy4(i)+
     & px5(i)*vy5(i)+px6(i)*vy6(i)+px7(i)*vy7(i)+px8(i)*vy8(i)
      dzx(i)= px1(i)*vz1(i)+px2(i)*vz2(i)+px3(i)*vz3(i)+px4(i)*vz4(i)+
     & px5(i)*vz5(i)+px6(i)*vz6(i)+px7(i)*vz7(i)+px8(i)*vz8(i)
      dzy(i)= py1(i)*vz1(i)+py2(i)*vz2(i)+py3(i)*vz3(i)+py4(i)*vz4(i)+
     & py5(i)*vz5(i)+py6(i)*vz6(i)+py7(i)*vz7(i)+py8(i)*vz8(i)
  100 continue
c
      dt1d2=.5*dt1
c
      do 120 i=lft,llt
      dxx(i)  =dt1*dxx(i)
      dyy(i)  =dt1*dyy(i)
      dzz(i)  =dt1*dzz(i)
      d1(i)   =dt1*(dxy(i)+dyx(i))
      d2(i)   =dt1*(dyz(i)+dzy(i))
      d3(i)   =dt1*(dxz(i)+dzx(i))
      dfe(i)  =8.*vlinc(i)/volo(i)
      wzzdt(i)=dt1d2*(dyx(i)-dxy(i))
      wyydt(i)=dt1d2*(dxz(i)-dzx(i))
  120 wxxdt(i)=dt1d2*(dzy(i)-dyz(i))
c
      return
c
      end
      subroutine strn0 (ihg)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk61/p11,p12,p13,p14,p15,p16,p17,p18,
     1            p21,p22,p23,p24,p25,p26,p27,p28,
     2            p31,p32,p33,p34,p35,p36,p37,p38
      common/aux5/
     1aj11(128),aj12(128),aj13(128),aj21(128),aj22(128),aj23(128),
     2aj31(128),aj32(128),aj33(128),ai11(128),ai12(128),ai13(128),
     3ai21(128),ai22(128),ai23(128),ai31(128),ai32(128),ai33(128),
     4 a1111(128),a1113(128),a1115(128),a1117(128),a2111(128),
     5 a2113(128),a2115(128),a2117(128),a3111(128),a3113(128),
     6 a3115(128),a3117(128),a1221(128),a1222(128),a1225(128),
     7 a1226(128),a2221(128),a2222(128),a2225(128),a2226(128),
     8 a3221(128),a3222(128),a3225(128),a3226(128),a1331(128),
     9 a1332(128),a1333(128),a1334(128),a2331(128),a2332(128),
     & a2333(128),a2334(128),a3331(128),a3332(128),a3333(128),
     & a3334(128),vlinv(128)
      common/aux9/vlrho(128),vlinc(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 vx1(128),vx2(128),vx3(128),vx4(128),
     5 vx5(128),vx6(128),vx7(128),vx8(128),
     6 vy1(128),vy2(128),vy3(128),vy4(128),
     7 vy5(128),vy6(128),vy7(128),vy8(128),
     8 vz1(128),vz2(128),vz3(128),vz4(128),
     9 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux13/
     1 x12(128),x34(128),x56(128),x78(128),y12(128),y34(128),
     2 y56(128),y78(128),z12(128),z34(128),z56(128),z78(128),
     3 x14(128),x23(128),x58(128),x67(128),y14(128),y23(128),
     4 y58(128),y67(128),z14(128),z23(128),z58(128),z67(128),
     6 x15(128),x26(128),x37(128),x48(128),y15(128),y26(128),
     7 y37(128),y48(128),z15(128),z26(128),z37(128),z48(128),
     8 x1234(128),x5678(128),x1423(128),x5867(128),
     9 y1234(128),y5678(128),y1423(128),y5867(128),
     & z1234(128),z5678(128),z1423(128),z5867(128)
      common/aux36/lft,llt
      data zero/0.0/
c
      call basisf (p11,p21,p31,1,zero)
c
      do 10 i=lft,llt
      aj11(i)=p11*x1234(i)+p15*x5678(i)
      aj12(i)=p11*y1234(i)+p15*y5678(i)
      aj13(i)=p11*z1234(i)+p15*z5678(i)
      aj21(i)=p21*x1423(i)+p25*x5867(i)
      aj22(i)=p21*y1423(i)+p25*y5867(i)
      aj23(i)=p21*z1423(i)+p25*z5867(i)
   10 continue
c
      do 20 i=lft,llt
      ai11(i)= aj22(i)*aj33(i)-aj23(i)*aj32(i)
      ai21(i)=-aj21(i)*aj33(i)+aj23(i)*aj31(i)
      ai31(i)= aj21(i)*aj32(i)-aj22(i)*aj31(i)
      vlinc(i)=aj11(i)*ai11(i)+aj12(i)*ai21(i)+aj13(i)*ai31(i)
      vlinv(i)=1./vlinc(i)
   20 continue
      if (ihg.lt.2) return
      do 30 i=lft,llt
      ai11(i)=vlinv(i)*ai11(i)
      ai21(i)=vlinv(i)*ai21(i)
      ai31(i)=vlinv(i)*ai31(i)
      ai12(i)=vlinv(i)*(-aj12(i)*aj33(i)+aj13(i)*aj32(i))
      ai22(i)=vlinv(i)*( aj11(i)*aj33(i)-aj13(i)*aj31(i))
      ai32(i)=vlinv(i)*(-aj11(i)*aj32(i)+aj12(i)*aj31(i))
      ai13(i)=vlinv(i)*( aj12(i)*aj23(i)-aj13(i)*aj22(i))
      ai23(i)=vlinv(i)*(-aj11(i)*aj23(i)+aj13(i)*aj21(i))
      ai33(i)=vlinv(i)*( aj11(i)*aj22(i)-aj12(i)*aj21(i))
   30 continue
c
      do 40 i=lft,llt
      a1111(i)=ai11(i)*p11
      a2111(i)=ai21(i)*p11
      a3111(i)=ai31(i)*p11
      a1221(i)=ai12(i)*p21
      a2221(i)=ai22(i)*p21
      a3221(i)=ai32(i)*p21
      a1331(i)=ai13(i)*p31
      a2331(i)=ai23(i)*p31
      a3331(i)=ai33(i)*p31
      a1113(i)=a1331(i)+a1221(i)
      a1117(i)=a1331(i)-a1221(i)
      a2113(i)=a2331(i)+a2221(i)
      a2117(i)=a2331(i)-a2221(i)
      a3113(i)=a3331(i)+a3221(i)
      a3117(i)=a3331(i)-a3221(i)
   40 continue
      do 50 i=lft,llt
      px1(i)= a1111(i)+a1113(i)
      px2(i)=-a1111(i)+a1113(i)
      px3(i)=-a1111(i)+a1117(i)
      px4(i)= a1111(i)+a1117(i)
      py1(i)= a2111(i)+a2113(i)
      py2(i)=-a2111(i)+a2113(i)
      py3(i)=-a2111(i)+a2117(i)
      py4(i)= a2111(i)+a2117(i)
      pz1(i)= a3111(i)+a3113(i)
      pz2(i)=-a3111(i)+a3113(i)
      pz3(i)=-a3111(i)+a3117(i)
      pz4(i)= a3111(i)+a3117(i)
   50 continue
      do 90 i=lft,llt
      a1115(i)=ai11(i)*p15
      a2115(i)=ai21(i)*p15
      a3115(i)=ai31(i)*p15
      a1225(i)=ai12(i)*p25
      a2225(i)=ai22(i)*p25
      a3225(i)=ai32(i)*p25
      a1113(i)=a1331(i)-a1225(i)
      a1117(i)=a1331(i)+a1225(i)
      a2113(i)=a2331(i)-a2225(i)
      a2117(i)=a2331(i)+a2225(i)
      a3113(i)=a3331(i)-a3225(i)
      a3117(i)=a3331(i)+a3225(i)
      px5(i)= a1115(i)-a1113(i)
      px6(i)=-a1115(i)-a1113(i)
      px7(i)=-a1115(i)-a1117(i)
      px8(i)= a1115(i)-a1117(i)
      py5(i)= a2115(i)-a2113(i)
      py6(i)=-a2115(i)-a2113(i)
      py7(i)=-a2115(i)-a2117(i)
      py8(i)= a2115(i)-a2117(i)
      pz5(i)= a3115(i)-a3113(i)
      pz6(i)=-a3115(i)-a3113(i)
      pz7(i)=-a3115(i)-a3117(i)
      pz8(i)= a3115(i)-a3117(i)
   90 continue
      return
      end
      subroutine basisf(p1,p2,p3,irl,t)
c     implicit double precision (a-h,o-z)                                    dp
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      dimension p1(1),p2(1),p3(1)
      if (irl.eq.0) then
      do 10 i=1,8
      p1(i)=pr(i,ipt,nip)
      p2(i)=ps(i,ipt,nip)
   10 p3(i)=pt(i,ipt,nip)
      else
      tp=1.0+t
      tm=1.0-t
      p1(1)=-.125*tm
      p1(2)=-p1(1)
      p1(3)= .125*tm
      p1(4)=-p1(3)
      p1(5)=-.125*tp
      p1(6)=-p1(5)
      p1(7)= .125*tp
      p1(8)=-p1(7)
      p2(1)=-.125*tm
      p2(2)=-.125*tm
      p2(3)=-p2(2)
      p2(4)=-p2(1)
      p2(5)=-.125*tp
      p2(6)=-.125*tp
      p2(7)=-p2(6)
      p2(8)=-p2(5)
      p3(1)=-.125
      p3(2)=-.125
      p3(3)=-.125
      p3(4)=-.125
      p3(5)=-p3(1)
      p3(6)=-p3(2)
      p3(7)=-p3(3)
      p3(8)=-p3(4)
      endif
      return
      end
      subroutine felts
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute largest area for characteristic length calculation
c
      common/aux9/vlrho(128),voln(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux32/areal(128),aream(128),at(128)
      common/aux34/xioff(128),dx(128)
      common/aux36/lft,llt
c
      do 10 i=lft,llt
      xioff(i)=1.0
      areal(i)=1.e20
      aream(i)=0.0
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
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
   10 areal(i)=  min(atest,areal(i))
      do 20 i=lft,llt
      x13=x7(i)-x5(i)
      x24=x8(i)-x6(i)
      y13=y7(i)-y5(i)
      y24=y8(i)-y6(i)
      z13=z7(i)-z5(i)
      z24=z8(i)-z6(i)
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
   20 areal(i)=  min(atest,areal(i))
      do 30 i=lft,llt
      x13=x6(i)-x1(i)
      x24=x5(i)-x2(i)
      y13=y6(i)-y1(i)
      y24=y5(i)-y2(i)
      z13=z6(i)-z1(i)
      z24=z5(i)-z2(i)
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
   30 areal(i)=  min(atest,areal(i))
      do 40 i=lft,llt
      x13=x7(i)-x2(i)
      x24=x6(i)-x3(i)
      y13=y7(i)-y2(i)
      y24=y6(i)-y3(i)
      z13=z7(i)-z2(i)
      z24=z6(i)-z3(i)
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
   40 areal(i)=  min(atest,areal(i))
      do 50 i=lft,llt
      x13=x8(i)-x3(i)
      x24=x7(i)-x4(i)
      y13=y8(i)-y3(i)
      y24=y7(i)-y4(i)
      z13=z8(i)-z3(i)
      z24=z7(i)-z4(i)
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
   50 areal(i)=  min(atest,areal(i))
      do 60 i=lft,llt
      x13=x5(i)-x4(i)
      x24=x8(i)-x1(i)
      y13=y5(i)-y4(i)
      y24=y8(i)-y1(i)
      z13=z5(i)-z4(i)
      z24=z8(i)-z1(i)
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      atest=e*g-f*f
      aream(i)=  max(atest,aream(i))
      areal(i)=  min(atest,areal(i))
      at(i)=.0001*aream(i)
   60 dx(i)=32.*voln(i)/sqrt(aream(i))
      do 80 i=lft,llt
   80 if (areal(i).lt.at(i)) xioff(i)=0.0
c
      return
c
      end
      subroutine tsblkq (ntshel)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,b3
      common/bk20/idummy(10),ndum
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux9/vlrho(128),vol(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dd(128),df(128)
      common/aux34/xioff(128),dx(128)
      common/aux35/rho(128),cxx(128),q1(128),cx(128)
      common/aux36/lft,llt
      common/aux37/ad(128),vol3rd(128),qx(128),dtx(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      character*4 mess                                                  vax75
      common/aux38/mess
      dimension ntshel(*)
      rhoi=1./rho(lft)
      b12r=b12*rho(lft)
      b2r=b2*rho(lft)
      ss2=rhoi*sndspd
      ss1=sqrt(ss2)
      do 10 i=lft,llt
      q1(i)=ss2
   10 cx(i)=ss1
      do 30 i=lft,llt
      dtx(i)=dx(i)/cx(i)
   30 cxx(i)=xioff(i)*cx(i)
      do 40 i=lft,llt
   40 vol3rd(i)=2.*vol(i)**.33333333333333
      do 60 i=lft,llt
   60 dt2=  min(dtx(i),dt2)
      if (mess.ne.'sw2.') return
      do 70 i=lft,llt
      if (dt2.eq.dtx(i)) then
      ielmtc=128*(ndum-1)+i
      ielmtc=ntshel(ielmtc)
      ityptc=5
      endif
   70 continue
      return
      end
      subroutine forc0(e,iblkt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux7/
     1 p11(128),p12(128),p13(128),p14(128),
     2 p15(128),p16(128),p17(128),p18(128),
     3 p21(128),p22(128),p23(128),p24(128),
     4 p25(128),p26(128),p27(128),p28(128),
     5 p31(128),p32(128),p33(128),p34(128),
     6 p35(128),p36(128),p37(128),p38(128)
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      dimension e(3,1),iblkt(*)
c
      do 30 i=lft,llt
      ep11(i)=ep11(i)+p11(i)
      ep21(i)=ep21(i)+p21(i)
      ep31(i)=ep31(i)+p31(i)
      ep17(i)=ep17(i)+p17(i)
      ep27(i)=ep27(i)+p27(i)
      ep37(i)=ep37(i)+p37(i)
      ep12(i)=ep12(i)+p12(i)
      ep22(i)=ep22(i)+p22(i)
      ep32(i)=ep32(i)+p32(i)
      ep18(i)=ep18(i)+p18(i)
      ep28(i)=ep28(i)+p28(i)
      ep38(i)=ep38(i)+p38(i)
      ep13(i)=ep13(i)+p13(i)
      ep23(i)=ep23(i)+p23(i)
      ep33(i)=ep33(i)+p33(i)
      ep15(i)=ep15(i)+p15(i)
      ep25(i)=ep25(i)+p25(i)
      ep35(i)=ep35(i)+p35(i)
      ep14(i)=ep14(i)+p14(i)
      ep24(i)=ep24(i)+p24(i)
      ep34(i)=ep34(i)+p34(i)
      ep16(i)=ep16(i)+p16(i)
      ep26(i)=ep26(i)+p26(i)
   30 ep36(i)=ep36(i)+p36(i)
      do 180 n=1,nnc
      lcn=lczc+n
      i0 =iblkt(lcn)
      i1 =iblkt(lcn+1)-1
cdir$ ivdep
      do 130 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))+ep11(i)
      e(2,ix1(i))=e(2,ix1(i))+ep21(i)
      e(3,ix1(i))=e(3,ix1(i))+ep31(i)
      e(1,ix2(i))=e(1,ix2(i))+ep12(i)
      e(2,ix2(i))=e(2,ix2(i))+ep22(i)
      e(3,ix2(i))=e(3,ix2(i))+ep32(i)
      e(1,ix3(i))=e(1,ix3(i))+ep13(i)
      e(2,ix3(i))=e(2,ix3(i))+ep23(i)
      e(3,ix3(i))=e(3,ix3(i))+ep33(i)
      e(1,ix4(i))=e(1,ix4(i))+ep14(i)
      e(2,ix4(i))=e(2,ix4(i))+ep24(i)
      e(3,ix4(i))=e(3,ix4(i))+ep34(i)
      e(1,ix5(i))=e(1,ix5(i))+ep15(i)
      e(2,ix5(i))=e(2,ix5(i))+ep25(i)
      e(3,ix5(i))=e(3,ix5(i))+ep35(i)
      e(1,ix6(i))=e(1,ix6(i))+ep16(i)
      e(2,ix6(i))=e(2,ix6(i))+ep26(i)
      e(3,ix6(i))=e(3,ix6(i))+ep36(i)
      e(1,ix7(i))=e(1,ix7(i))+ep17(i)
      e(2,ix7(i))=e(2,ix7(i))+ep27(i)
      e(3,ix7(i))=e(3,ix7(i))+ep37(i)
      e(1,ix8(i))=e(1,ix8(i))+ep18(i)
      e(2,ix8(i))=e(2,ix8(i))+ep28(i)
      e(3,ix8(i))=e(3,ix8(i))+ep38(i)
  130 continue
  180 continue
      return
      end
      subroutine tshgmd
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk12/b12,b2,caq
      common/aux9/vlrho(128),vol(128)
      common/aux7/
     1 p11(128),p12(128),p13(128),p14(128),
     2 p15(128),p16(128),p17(128),p18(128),
     3 p21(128),p22(128),p23(128),p24(128),
     4 p25(128),p26(128),p27(128),p28(128),
     5 p31(128),p32(128),p33(128),p34(128),
     6 p35(128),p36(128),p37(128),p38(128)
      common/aux18/fac(128),df(128)
      common/aux32/
     &hxap(128),hxam(128),hxbp(128),hxbm(128),hyap(128),
     &hyam(128),hybp(128),hybm(128),hzap(128),hzam(128),hzbp(128),
     &hzbm(128),hx1(128),hx2(128),hx3(128),hx4(128),hy1(128),
     &hy2(128),hy3(128),hy4(128),hz1(128),hz2(128),hz3(128),
     &hz4(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/aux37/ad(128),vol3rd(128),qx(128),dtx(128)
      data lvct/128/
c
      if (caq.eq.0.0) go to 50
c
      caqp25=.250*caq*rho(lft)
      caq100=100.*caq
      do 10 i=lft,llt
   10 fac(i)=caqp25*vol3rd(i)*vol3rd(i)
      do 20 i=lft,llt
      fcl(i)=fac(i)*cxx(i)
   20 fcq(i)=fac(i)*caq100
c
      call hgxts
c
      do 30 i=lft,llt
      hxap(i)=hx1(i)+hx4(i)
      hxam(i)=hx1(i)-hx4(i)
      hxbp(i)=hx2(i)+hx3(i)
      hxbm(i)=hx2(i)-hx3(i)
      hyap(i)=hy1(i)+hy4(i)
      hyam(i)=hy1(i)-hy4(i)
      hybp(i)=hy2(i)+hy3(i)
      hybm(i)=hy2(i)-hy3(i)
      hzap(i)=hz1(i)+hz4(i)
      hzam(i)=hz1(i)-hz4(i)
      hzbp(i)=hz2(i)+hz3(i)
   30 hzbm(i)=hz2(i)-hz3(i)
      do 40 i=lft,llt
      p11(i)=-hxap(i)-hxbp(i)
      p12(i)=hxap(i)-hxbm(i)
      p13(i)=-hxap(i)+hxbp(i)
      p14(i)=hxap(i)+hxbm(i)
      p15(i)=-hxam(i)+hxbp(i)
      p16(i)=hxam(i)+hxbm(i)
      p17(i)=-hxam(i)-hxbp(i)
      p18(i)=hxam(i)-hxbm(i)
      p21(i)=-hyap(i)-hybp(i)
      p22(i)=hyap(i)-hybm(i)
      p23(i)=-hyap(i)+hybp(i)
      p24(i)=hyap(i)+hybm(i)
      p25(i)=-hyam(i)+hybp(i)
      p26(i)=hyam(i)+hybm(i)
      p27(i)=-hyam(i)-hybp(i)
      p28(i)=hyam(i)-hybm(i)
      p31(i)=-hzap(i)-hzbp(i)
      p32(i)=hzap(i)-hzbm(i)
      p33(i)=-hzap(i)+hzbp(i)
      p34(i)=hzap(i)+hzbm(i)
      p35(i)=-hzam(i)+hzbp(i)
      p36(i)=hzam(i)+hzbm(i)
      p37(i)=-hzam(i)-hzbp(i)
   40 p38(i)=hzam(i)-hzbm(i)
      return
   50 nrng=24*lvct
      do 60 i=1,nrng
   60 p11(i)=0.0
      return
      end
      subroutine hgxts
c     implicit double precision (a-h,o-z)                                    dp
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 vx1(128),vx2(128),vx3(128),vx4(128),
     5 vx5(128),vx6(128),vx7(128),vx8(128),
     6 vy1(128),vy2(128),vy3(128),vy4(128),
     7 vy5(128),vy6(128),vy7(128),vy8(128),
     8 vz1(128),vz2(128),vz3(128),vz4(128),
     9 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux32/
     &hgx1(128),hgx2(128),hgx3(128),hgx4(128),hgy1(128),
     &hgy2(128),hgy3(128),hgy4(128),hgz1(128),hgz2(128),hgz3(128),
     &hgz4(128),hx1(128),hx2(128),hx3(128),hx4(128),hy1(128),
     &hy2(128),hy3(128),hy4(128),hz1(128),hz2(128),hz3(128),
     &hz4(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
c
      dimension vx3478(1),vx2358(1),vx1467(1),vx1256(1),
     1          vy3478(1),vy2358(1),vy1467(1),vy1256(1),
     2          vz3478(1),vz2358(1),vz1467(1),vz1256(1)
c
      equivalence (hx1,vx3478),(hx2,vx2358),(hx3,vx1467),(hx4,vx1256),
     1            (hy1,vy3478),(hy2,vy2358),(hy3,vy1467),(hy4,vy1256),
     2            (hz1,vz3478),(hz2,vz2358),(hz3,vz1467),(hz4,vz1256)
c
      do 10 i=lft,llt
      vx3478(i)=vx3(i)-vx4(i)-vx7(i)+vx8(i)
      vx2358(i)=vx2(i)-vx3(i)-vx5(i)+vx8(i)
      vx1467(i)=vx1(i)-vx4(i)-vx6(i)+vx7(i)
      vx1256(i)=vx1(i)-vx2(i)-vx5(i)+vx6(i)
      vy3478(i)=vy3(i)-vy4(i)-vy7(i)+vy8(i)
      vy2358(i)=vy2(i)-vy3(i)-vy5(i)+vy8(i)
      vy1467(i)=vy1(i)-vy4(i)-vy6(i)+vy7(i)
      vy1256(i)=vy1(i)-vy2(i)-vy5(i)+vy6(i)
      vz3478(i)=vz3(i)-vz4(i)-vz7(i)+vz8(i)
      vz2358(i)=vz2(i)-vz3(i)-vz5(i)+vz8(i)
      vz1467(i)=vz1(i)-vz4(i)-vz6(i)+vz7(i)
   10 vz1256(i)=vz1(i)-vz2(i)-vz5(i)+vz6(i)
      do 20 i=lft,llt
      hgx1(i)=vx1467(i)-vx2358(i)
      hgx2(i)=vx1467(i)+vx2358(i)
      hgx3(i)=vx1256(i)-vx3478(i)
      hgx4(i)=vx1256(i)+vx3478(i)
      hgy1(i)=vy1467(i)-vy2358(i)
      hgy2(i)=vy1467(i)+vy2358(i)
      hgy3(i)=vy1256(i)-vy3478(i)
      hgy4(i)=vy1256(i)+vy3478(i)
      hgz1(i)=vz1467(i)-vz2358(i)
      hgz2(i)=vz1467(i)+vz2358(i)
      hgz3(i)=vz1256(i)-vz3478(i)
   20 hgz4(i)=vz1256(i)+vz3478(i)
      do 30 i=lft,llt
      hx1(i)=hgx1(i)*(fcl(i)+abs(hgx1(i))*fcq(i))
      hx2(i)=hgx2(i)*(fcl(i)+abs(hgx2(i))*fcq(i))
      hx3(i)=hgx3(i)*(fcl(i)+abs(hgx3(i))*fcq(i))
      hx4(i)=hgx4(i)*(fcl(i)+abs(hgx4(i))*fcq(i))
      hy1(i)=hgy1(i)*(fcl(i)+abs(hgy1(i))*fcq(i))
      hy2(i)=hgy2(i)*(fcl(i)+abs(hgy2(i))*fcq(i))
      hy3(i)=hgy3(i)*(fcl(i)+abs(hgy3(i))*fcq(i))
      hy4(i)=hgy4(i)*(fcl(i)+abs(hgy4(i))*fcq(i))
      hz1(i)=hgz1(i)*(fcl(i)+abs(hgz1(i))*fcq(i))
      hz2(i)=hgz2(i)*(fcl(i)+abs(hgz2(i))*fcq(i))
      hz3(i)=hgz3(i)*(fcl(i)+abs(hgz3(i))*fcq(i))
   30 hz4(i)=hgz4(i)*(fcl(i)+abs(hgz4(i))*fcq(i))
      return
      end
      subroutine tshgfb
c     implicit double precision (a-h,o-z)                                    dp
      common/bk12/b12,b2,caq
      common/aux7/
     1 p11(128),p12(128),p13(128),p14(128),
     2 p15(128),p16(128),p17(128),p18(128),
     3 p21(128),p22(128),p23(128),p24(128),
     4 p25(128),p26(128),p27(128),p28(128),
     5 p31(128),p32(128),p33(128),p34(128),
     6 p35(128),p36(128),p37(128),p38(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux9/vlrho(128),vol(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 vx1(128),vx2(128),vx3(128),vx4(128),
     5 vx5(128),vx6(128),vx7(128),vx8(128),
     6 vy1(128),vy2(128),vy3(128),vy4(128),
     7 vy5(128),vy6(128),vy7(128),vy8(128),
     8 vz1(128),vz2(128),vz3(128),vz4(128),
     9 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux14/
     & sv11(128),sv21(128),sv31(128),sv41(128),
     & sv51(128),sv61(128),sv71(128),sv81(128),
     & sv12(128),sv22(128),sv32(128),sv42(128),
     & sv52(128),sv62(128),sv72(128),sv82(128),
     & sv13(128),sv23(128),sv33(128),sv43(128),
     & sv53(128),sv63(128),sv73(128),sv83(128),
     & sv14(128),sv24(128),sv34(128),sv44(128),
     & sv54(128),sv64(128),sv74(128),sv84(128)
      common/aux18/dd(128),df(128)
      common/aux32/
     &hgx1(128),hgx2(128),hgx3(128),hgx4(128),hgy1(128),hgy2(128),
     &hgy3(128),hgy4(128),hgz1(128),hgz2(128),hgz3(128),hgz4(128),
     &hx1(128) ,hx2(128) ,hx3(128) ,hx4(128) ,hy1(128) ,hy2(128) ,
     &hy3(128) ,hy4(128) ,hz1(128) ,hz2(128) ,hz3(128) ,hz4(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     &             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxx(128),fcl(128),fac(128)
      common/aux36/lft,llt
      common/aux37/ad(128),vol3rd(128),qx(128),dtx(128)
c
      dimension x3478(1),x2358(1),x1467(1),x1256(1),
     &          y3478(1),y2358(1),y1467(1),y1256(1),
     &          z3478(1),z2358(1),z1467(1),z1256(1)
      equivalence (hx1,x3478),(hx2,x2358),(hx3,x1467),(hx4,x1256),
     &            (hy1,y3478),(hy2,y2358),(hy3,y1467),(hy4,y1256),
     &            (hz1,z3478),(hz2,z2358),(hz3,z1467),(hz4,z1256)
      data lvct/128/
c
c     if hourglass coefficient equals zero, zero p
c
      if(caq.eq.0) go to 120
c
      do 10 i=lft,llt
      x3478(i)=x3(i)-x4(i)-x7(i)+x8(i)
      x2358(i)=x2(i)-x3(i)-x5(i)+x8(i)
      x1467(i)=x1(i)-x4(i)-x6(i)+x7(i)
      x1256(i)=x1(i)-x2(i)-x5(i)+x6(i)
      y3478(i)=y3(i)-y4(i)-y7(i)+y8(i)
      y2358(i)=y2(i)-y3(i)-y5(i)+y8(i)
      y1467(i)=y1(i)-y4(i)-y6(i)+y7(i)
      y1256(i)=y1(i)-y2(i)-y5(i)+y6(i)
      z3478(i)=z3(i)-z4(i)-z7(i)+z8(i)
      z2358(i)=z2(i)-z3(i)-z5(i)+z8(i)
      z1467(i)=z1(i)-z4(i)-z6(i)+z7(i)
   10 z1256(i)=z1(i)-z2(i)-z5(i)+z6(i)
      do 20 i=lft,llt
      hgx1(i)=x1467(i)-x2358(i)
      hgx2(i)=x1467(i)+x2358(i)
      hgx3(i)=x1256(i)-x3478(i)
      hgx4(i)=x1256(i)+x3478(i)
      hgy1(i)=y1467(i)-y2358(i)
      hgy2(i)=y1467(i)+y2358(i)
      hgy3(i)=y1256(i)-y3478(i)
      hgy4(i)=y1256(i)+y3478(i)
      hgz1(i)=z1467(i)-z2358(i)
      hgz2(i)=z1467(i)+z2358(i)
      hgz3(i)=z1256(i)-z3478(i)
   20 hgz4(i)=z1256(i)+z3478(i)
      do 30 i=lft,llt
      sv11(i)= 1.-hgx1(i)*px1(i)-hgy1(i)*py1(i)-hgz1(i)*pz1(i)
      sv21(i)=-1.-hgx1(i)*px2(i)-hgy1(i)*py2(i)-hgz1(i)*pz2(i)
      sv31(i)= 1.-hgx1(i)*px3(i)-hgy1(i)*py3(i)-hgz1(i)*pz3(i)
      sv41(i)=-1.-hgx1(i)*px4(i)-hgy1(i)*py4(i)-hgz1(i)*pz4(i)
      sv51(i)= 2.-sv31(i)
      sv61(i)=-2.-sv41(i)
      sv71(i)= 2.-sv11(i)
      sv81(i)=-2.-sv21(i)
      sv12(i)= 1.-hgx2(i)*px1(i)-hgy2(i)*py1(i)-hgz2(i)*pz1(i)
      sv22(i)= 1.-hgx2(i)*px2(i)-hgy2(i)*py2(i)-hgz2(i)*pz2(i)
      sv32(i)=-1.-hgx2(i)*px3(i)-hgy2(i)*py3(i)-hgz2(i)*pz3(i)
      sv42(i)=-1.-hgx2(i)*px4(i)-hgy2(i)*py4(i)-hgz2(i)*pz4(i)
      sv52(i)=-2.-sv32(i)
      sv62(i)=-2.-sv42(i)
      sv72(i)= 2.-sv12(i)
   30 sv82(i)= 2.-sv22(i)
      do 34 i=lft,llt
      sv13(i)= 1.-hgx3(i)*px1(i)-hgy3(i)*py1(i)-hgz3(i)*pz1(i)
      sv23(i)=-1.-hgx3(i)*px2(i)-hgy3(i)*py2(i)-hgz3(i)*pz2(i)
      sv33(i)=-1.-hgx3(i)*px3(i)-hgy3(i)*py3(i)-hgz3(i)*pz3(i)
      sv43(i)= 1.-hgx3(i)*px4(i)-hgy3(i)*py4(i)-hgz3(i)*pz4(i)
      sv53(i)=-2.-sv33(i)
      sv63(i)= 2.-sv43(i)
      sv73(i)= 2.-sv13(i)
      sv83(i)=-2.-sv23(i)
      sv14(i)= 1.-hgx4(i)*px1(i)-hgy4(i)*py1(i)-hgz4(i)*pz1(i)
      sv24(i)=-1.-hgx4(i)*px2(i)-hgy4(i)*py2(i)-hgz4(i)*pz2(i)
      sv34(i)= 1.-hgx4(i)*px3(i)-hgy4(i)*py3(i)-hgz4(i)*pz3(i)
      sv44(i)=-1.-hgx4(i)*px4(i)-hgy4(i)*py4(i)-hgz4(i)*pz4(i)
      sv54(i)=-sv34(i)
      sv64(i)=-sv44(i)
      sv74(i)=-sv14(i)
   34 sv84(i)=-sv24(i)
      do 40 i=lft,llt
      hx1(i)=vx1(i)*sv11(i)+vx2(i)*sv21(i)+vx3(i)*sv31(i)+vx4(i)*sv41(i)
     &      +vx5(i)*sv51(i)+vx6(i)*sv61(i)+vx7(i)*sv71(i)+vx8(i)*sv81(i)
      hx2(i)=vx1(i)*sv12(i)+vx2(i)*sv22(i)+vx3(i)*sv32(i)+vx4(i)*sv42(i)
     &      +vx5(i)*sv52(i)+vx6(i)*sv62(i)+vx7(i)*sv72(i)+vx8(i)*sv82(i)
      hx3(i)=vx1(i)*sv13(i)+vx2(i)*sv23(i)+vx3(i)*sv33(i)+vx4(i)*sv43(i)
     &      +vx5(i)*sv53(i)+vx6(i)*sv63(i)+vx7(i)*sv73(i)+vx8(i)*sv83(i)
   40 hx4(i)=vx1(i)*sv14(i)+vx2(i)*sv24(i)+vx3(i)*sv34(i)+vx4(i)*sv44(i)
     &      +vx5(i)*sv54(i)+vx6(i)*sv64(i)+vx7(i)*sv74(i)+vx8(i)*sv84(i)
      do 50 i=lft,llt
      hy1(i)=vy1(i)*sv11(i)+vy2(i)*sv21(i)+vy3(i)*sv31(i)+vy4(i)*sv41(i)
     &      +vy5(i)*sv51(i)+vy6(i)*sv61(i)+vy7(i)*sv71(i)+vy8(i)*sv81(i)
      hy2(i)=vy1(i)*sv12(i)+vy2(i)*sv22(i)+vy3(i)*sv32(i)+vy4(i)*sv42(i)
     &      +vy5(i)*sv52(i)+vy6(i)*sv62(i)+vy7(i)*sv72(i)+vy8(i)*sv82(i)
      hy3(i)=vy1(i)*sv13(i)+vy2(i)*sv23(i)+vy3(i)*sv33(i)+vy4(i)*sv43(i)
     &      +vy5(i)*sv53(i)+vy6(i)*sv63(i)+vy7(i)*sv73(i)+vy8(i)*sv83(i)
   50 hy4(i)=vy1(i)*sv14(i)+vy2(i)*sv24(i)+vy3(i)*sv34(i)+vy4(i)*sv44(i)
     &      +vy5(i)*sv54(i)+vy6(i)*sv64(i)+vy7(i)*sv74(i)+vy8(i)*sv84(i)
      do 60 i=lft,llt
      hz1(i)=vz1(i)*sv11(i)+vz2(i)*sv21(i)+vz3(i)*sv31(i)+vz4(i)*sv41(i)
     &      +vz5(i)*sv51(i)+vz6(i)*sv61(i)+vz7(i)*sv71(i)+vz8(i)*sv81(i)
      hz2(i)=vz1(i)*sv12(i)+vz2(i)*sv22(i)+vz3(i)*sv32(i)+vz4(i)*sv42(i)
     &      +vz5(i)*sv52(i)+vz6(i)*sv62(i)+vz7(i)*sv72(i)+vz8(i)*sv82(i)
      hz3(i)=vz1(i)*sv13(i)+vz2(i)*sv23(i)+vz3(i)*sv33(i)+vz4(i)*sv43(i)
     &      +vz5(i)*sv53(i)+vz6(i)*sv63(i)+vz7(i)*sv73(i)+vz8(i)*sv83(i)
   60 hz4(i)=vz1(i)*sv14(i)+vz2(i)*sv24(i)+vz3(i)*sv34(i)+vz4(i)*sv44(i)
     &      +vz5(i)*sv54(i)+vz6(i)*sv64(i)+vz7(i)*sv74(i)+vz8(i)*sv84(i)
      caqp25=-.250*caq*rho(lft)
      do 70 i=lft,llt
   70 fac(i)=cxx(i)*caqp25*vol3rd(i)*vol3rd(i)/df(i)
      do 80 i=lft,llt
      hx1(i)=fac(i)*hx1(i)
      hx2(i)=fac(i)*hx2(i)
      hx3(i)=fac(i)*hx3(i)
      hx4(i)=fac(i)*hx4(i)
      hy1(i)=fac(i)*hy1(i)
      hy2(i)=fac(i)*hy2(i)
      hy3(i)=fac(i)*hy3(i)
      hy4(i)=fac(i)*hy4(i)
      hz1(i)=fac(i)*hz1(i)
      hz2(i)=fac(i)*hz2(i)
      hz3(i)=fac(i)*hz3(i)
   80 hz4(i)=fac(i)*hz4(i)
      do 90 i=lft,llt
      p11(i)=hx1(i)*sv11(i)+hx2(i)*sv12(i)+hx3(i)*sv13(i)+hx4(i)*sv14(i)
      p12(i)=hx1(i)*sv21(i)+hx2(i)*sv22(i)+hx3(i)*sv23(i)+hx4(i)*sv24(i)
      p13(i)=hx1(i)*sv31(i)+hx2(i)*sv32(i)+hx3(i)*sv33(i)+hx4(i)*sv34(i)
      p14(i)=hx1(i)*sv41(i)+hx2(i)*sv42(i)+hx3(i)*sv43(i)+hx4(i)*sv44(i)
      p15(i)=hx1(i)*sv51(i)+hx2(i)*sv52(i)+hx3(i)*sv53(i)+hx4(i)*sv54(i)
      p16(i)=hx1(i)*sv61(i)+hx2(i)*sv62(i)+hx3(i)*sv63(i)+hx4(i)*sv64(i)
      p17(i)=hx1(i)*sv71(i)+hx2(i)*sv72(i)+hx3(i)*sv73(i)+hx4(i)*sv74(i)
   90 p18(i)=hx1(i)*sv81(i)+hx2(i)*sv82(i)+hx3(i)*sv83(i)+hx4(i)*sv84(i)
      do 100 i=lft,llt
      p21(i)=hy1(i)*sv11(i)+hy2(i)*sv12(i)+hy3(i)*sv13(i)+hy4(i)*sv14(i)
      p22(i)=hy1(i)*sv21(i)+hy2(i)*sv22(i)+hy3(i)*sv23(i)+hy4(i)*sv24(i)
      p23(i)=hy1(i)*sv31(i)+hy2(i)*sv32(i)+hy3(i)*sv33(i)+hy4(i)*sv34(i)
      p24(i)=hy1(i)*sv41(i)+hy2(i)*sv42(i)+hy3(i)*sv43(i)+hy4(i)*sv44(i)
      p25(i)=hy1(i)*sv51(i)+hy2(i)*sv52(i)+hy3(i)*sv53(i)+hy4(i)*sv54(i)
      p26(i)=hy1(i)*sv61(i)+hy2(i)*sv62(i)+hy3(i)*sv63(i)+hy4(i)*sv64(i)
      p27(i)=hy1(i)*sv71(i)+hy2(i)*sv72(i)+hy3(i)*sv73(i)+hy4(i)*sv74(i)
  100 p28(i)=hy1(i)*sv81(i)+hy2(i)*sv82(i)+hy3(i)*sv83(i)+hy4(i)*sv84(i)
      do 110 i=lft,llt
      p31(i)=hz1(i)*sv11(i)+hz2(i)*sv12(i)+hz3(i)*sv13(i)+hz4(i)*sv14(i)
      p32(i)=hz1(i)*sv21(i)+hz2(i)*sv22(i)+hz3(i)*sv23(i)+hz4(i)*sv24(i)
      p33(i)=hz1(i)*sv31(i)+hz2(i)*sv32(i)+hz3(i)*sv33(i)+hz4(i)*sv34(i)
      p34(i)=hz1(i)*sv41(i)+hz2(i)*sv42(i)+hz3(i)*sv43(i)+hz4(i)*sv44(i)
      p35(i)=hz1(i)*sv51(i)+hz2(i)*sv52(i)+hz3(i)*sv53(i)+hz4(i)*sv54(i)
      p36(i)=hz1(i)*sv61(i)+hz2(i)*sv62(i)+hz3(i)*sv63(i)+hz4(i)*sv64(i)
      p37(i)=hz1(i)*sv71(i)+hz2(i)*sv72(i)+hz3(i)*sv73(i)+hz4(i)*sv74(i)
  110 p38(i)=hz1(i)*sv81(i)+hz2(i)*sv82(i)+hz3(i)*sv83(i)+hz4(i)*sv84(i)
      return
c
  120 nrng=24*lvct
      do 130 i=1,nrng
  130 p11(i)=0.0
c
      return
      end
      subroutine forci(wgt,ipt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux9/vlrho(128),vol(128)
      common/aux10/
     & px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     & py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     & pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     & p11(128),p12(128),p13(128),p14(128),
     & p15(128),p16(128),p17(128),p18(128),
     & p21(128),p22(128),p23(128),p24(128),
     & p25(128),p26(128),p27(128),p28(128),
     & p31(128),p32(128),p33(128),p34(128),
     & p35(128),p36(128),p37(128),p38(128)
      common/aux11/po(128),
     &sgw1(128),sgw2(128),sgw3(128),sgw4(128),sgw5(128),sgw6(128),
     &sgv1(128),sgv2(128),sgv3(128),sgv4(128),sgv5(128),sgv6(128),
     &e11(128),e21(128),e31(128),e12(128),e22(128),e32(128),
     &e13(128),e23(128),e33(128),e14(128),e24(128),e34(128),
     &e15(128),e25(128),e35(128),e16(128),e26(128),e36(128),
     &e17(128),e27(128),e37(128),e18(128),e28(128),e38(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),epx3(128),
     3 epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     &             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/presc/centpr(128)
c
      wgt2=4.*wgt
      third=-1./3.
      do 10 i=lft,llt
      vol(i)=wgt2*vol(i)
      sgv1(i)=(sign1(i)+sig1(i))*vol(i)
      sgv2(i)=(sign2(i)+sig2(i))*vol(i)
      sgv3(i)=(sign3(i)+sig3(i))*vol(i)
      sgv4(i)=(sign4(i)+sig4(i))*vol(i)
      sgv5(i)=(sign5(i)+sig5(i))*vol(i)
   10 sgv6(i)=(sign6(i)+sig6(i))*vol(i)
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
      e13(i)=sgv1(i)*px3(i)+sgv4(i)*py3(i)+sgv6(i)*pz3(i)
      e23(i)=sgv2(i)*py3(i)+sgv4(i)*px3(i)+sgv5(i)*pz3(i)
      e33(i)=sgv3(i)*pz3(i)+sgv6(i)*px3(i)+sgv5(i)*py3(i)
      e14(i)=sgv1(i)*px4(i)+sgv4(i)*py4(i)+sgv6(i)*pz4(i)
      e24(i)=sgv2(i)*py4(i)+sgv4(i)*px4(i)+sgv5(i)*pz4(i)
      e34(i)=sgv3(i)*pz4(i)+sgv6(i)*px4(i)+sgv5(i)*py4(i)
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
      if (ipt.eq.1) then
      do 120 i=lft,llt
      ep11(i)=-e11(i)
      ep21(i)=-e21(i)
      ep31(i)=-e31(i)
      ep12(i)=-e12(i)
      ep22(i)=-e22(i)
      ep32(i)=-e32(i)
      ep13(i)=-e13(i)
      ep23(i)=-e23(i)
      ep33(i)=-e33(i)
      ep14(i)=-e14(i)
      ep24(i)=-e24(i)
      ep34(i)=-e34(i)
      ep15(i)=-e15(i)
      ep25(i)=-e25(i)
      ep35(i)=-e35(i)
      ep16(i)=-e16(i)
      ep26(i)=-e26(i)
      ep36(i)=-e36(i)
      ep17(i)=-e17(i)
      ep27(i)=-e27(i)
      ep37(i)=-e37(i)
      ep18(i)=-e18(i)
      ep28(i)=-e28(i)
      ep38(i)=-e38(i)
  120 continue
      else
      do 130 i=lft,llt
      ep11(i)=ep11(i)-e11(i)
      ep21(i)=ep21(i)-e21(i)
      ep31(i)=ep31(i)-e31(i)
      ep12(i)=ep12(i)-e12(i)
      ep22(i)=ep22(i)-e22(i)
      ep32(i)=ep32(i)-e32(i)
      ep13(i)=ep13(i)-e13(i)
      ep23(i)=ep23(i)-e23(i)
      ep33(i)=ep33(i)-e33(i)
      ep14(i)=ep14(i)-e14(i)
      ep24(i)=ep24(i)-e24(i)
      ep34(i)=ep34(i)-e34(i)
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
  130 continue
      endif
c
      return
      end
      subroutine rttrs
c     implicit double precision (a-h,o-z)                                    dp
c
c     rotate stresses
c
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
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
c
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
c
      return
      end
      subroutine rttrn
c     implicit double precision (a-h,o-z)                                    dp
c
c     rotate stresses
c
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
c
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
      subroutine shl1s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      dimension  cm(1)
      data third/-.3333333333333/
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      blk =-ym/(1.-2.*pr)
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      wq4=capa*wq2
      wq3=1./wq3
      ymod=ym
      gmod=wq4
      sndspd=.66666666666667*g+third*blk
      if (ibelyt.eq.0) then
      do 10 i=lft,llt
      sign0(i)=sign3(i)+ym*d3(i)
      sign3(i)=sign0(i)
   10 continue
      endif
      do 30 i=lft,llt
      eyld(i)=-1.0
      etanmd(i)=ym
      ebar(i)=ymod
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
      cb(i)  =ss
      d3(i)  =-wq1*(d1(i)+d2(i))*wq3
      sig4(i)=sig4(i)+wq2*d4(i)
      sig5(i)=sig5(i)+wq4*d5(i)
      sig6(i)=sig6(i)+wq4*d6(i)
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)   =blk*davg(i)
      sig1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      sig2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      sig3(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   30 continue
      return
      end
      subroutine shl2s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),c11(128),c12(128),c13(128),c14(128),
     3  c22(128),c23(128),c24(128),c33(128),c34(128),c44(128),
     4  c55(128),c56(128),c66(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      dimension  cm(*),d(6,6)
      mx=48*(mxt(lft)-1)+13
      call blkcpy(cm(mx),d,36)
      sndspd=  max(d(1,1),d(2,2),d(3,3))
      ymod   =sndspd
      gmod   =  max(d(4,4),capa*d(5,5),capa*d(6,6))
      do 10 i=lft,llt
      ebar(i)=ymod
      eyld(i)=-1.0
      etanmd(i)=ymod
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
      cb(i)=  max(c11(i),c22(i),c33(i))
      d3(i)=-(c13(i)*d1(i)+c23(i)*d2(i)+c34(i)*d4(i))/c33(i)
      sig1(i)=sig1(i)+c11(i)*d1(i)+c12(i)*d2(i)+c13(i)*d3(i)+c14(i)*d4
     1 (i)
      sig2(i)=sig2(i)+c12(i)*d1(i)+c22(i)*d2(i)+c23(i)*d3(i)+c24(i)*d4
     1 (i)
      sig3(i)=0.0
      sig4(i)=sig4(i)+c14(i)*d1(i)+c24(i)*d2(i)+c34(i)*d3(i)+c44(i)*d4
     1 (i)
      sig5(i)=sig5(i)+capa*(c55(i)*d5(i)+c56(i)*d6(i))
      sig6(i)=sig6(i)+capa*(c56(i)*d5(i)+c66(i)*d6(i))
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   10 continue
      return
      end
      subroutine shl3s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign11(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),sg3old(128),
     7 sg3lst(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),aux(128,43),
     4 sg1a(128),sg2a(128),d7(128),fac1(128),fac2(128),g(128),
     5 fac1qh(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),qbqh(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/tsarry/tslimt,tsarry(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/double/iprec,ncpw,unit
      dimension  cm(*)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+21)
      qs=cm(mx+11)
      pr=cm(mx+6)
      ss=cm(mx+2)
      qa=1.-qb
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*cm(mx+26)
      g(i)   =fac4*ym(i)
      qbqh(i)=qb*qh(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
      eyld(i)=qs
      etanmd(i)=qh(i)
    5 continue
c
      if (qb.lt.0.9999999.and.ibelyt.le.1) then
c
      do 10 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i) =epx1(i)*s11(i)+epx4(i)*s12(i)+epx6(i)*s13(i)
      a12(i) =epx1(i)*s21(i)+epx4(i)*s22(i)+epx6(i)*s23(i)
      a13(i) =epx1(i)*s31(i)+epx4(i)*s32(i)+epx6(i)*s33(i)
      a21(i) =epx4(i)*s11(i)+epx2(i)*s12(i)+epx5(i)*s13(i)
      a22(i) =epx4(i)*s21(i)+epx2(i)*s22(i)+epx5(i)*s23(i)
      a23(i) =epx4(i)*s31(i)+epx2(i)*s32(i)+epx5(i)*s33(i)
      a31(i) =epx6(i)*s11(i)+epx5(i)*s12(i)+epx3(i)*s13(i)
      a32(i) =epx6(i)*s21(i)+epx5(i)*s22(i)+epx3(i)*s23(i)
      a33(i) =epx6(i)*s31(i)+epx5(i)*s32(i)+epx3(i)*s33(i)
   10 continue
      do 20 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      epx2(i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      epx5(i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   20 continue
      endif
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym(i)*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
c
c     compute stress increment and add to rotated stresses
c
      scl=0.
      do 30 i=lft,llt
      blkg(i)=third*(blk(i)+g(i))
      cb(i)  =ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)  =da4(i)-epx4(i)
      t5(i)  =da5(i)-epx5(i)
      t6(i)  =da6(i)-epx6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak(i)  =qs+qbqh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
   30 continue
      do 34 i=lft,llt
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   34 continue
      do 40 i=lft,llt
   40 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      ebar(i)=ym(i)
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 75
      endif
      do 60 i=lft,llt
      fac1qh(i)=1./(fac1(i)+qh(i))
      fac2qh(i)=fac1(i)*fac1qh(i)
      fac2(i)  =qa*qh(i)/fac1(i)
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      ckdev=.5+sign(.5*unit,abs(d3(i)-epsnew(i))-1.e-14)
      d7(i)=ckdev*d3(i) + .5*(1. - ckdev)*d2(i)
      d3(i)=scle(i)*d7(i) + (1. - scle(i))*epsnew(i)
c
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
   60 continue
      do 65 i=lft,llt
      deps(i)=scle(i)*(aj1(i)-ak(i))*fac2qh(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
      deps(i)=scle(i)*(aj1(i)-ak(i))*fac2qh(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
   65 continue
      do 70 i=lft,llt
      ebar(i)=qh(i)*scle(i)+ym(i)*(1.-scle(i))
      depn(i)=fac2(i)*deps(i)
      sig1(i)=da1(i) -deps(i)*t1(i)
      sig2(i)=da2(i) -deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*t4(i)
      sig5(i)=da5(i) -deps(i)*t5(i)
      sig6(i)=da6(i) -deps(i)*t6(i)
      epx1(i)=epx1(i)+depn(i)*t1(i)
      epx2(i)=epx2(i)+depn(i)*t2(i)
      ep(i)  =  ep(i)+depi(i)
      epx4(i)=epx4(i)+depn(i)*t4(i)
      epx5(i)=epx5(i)+depn(i)*t5(i)
      epx6(i)=epx6(i)+depn(i)*t6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   75 if (qb.lt.0.9999999.and.ibelyt.le.1) then
      do 80 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i)=epx1(i)*s11(i)+epx4(i)*s21(i)+epx6(i)*s31(i)
      a12(i)=epx1(i)*s12(i)+epx4(i)*s22(i)+epx6(i)*s32(i)
      a13(i)=epx1(i)*s13(i)+epx4(i)*s23(i)+epx6(i)*s33(i)
      a21(i)=epx4(i)*s11(i)+epx2(i)*s21(i)+epx5(i)*s31(i)
      a22(i)=epx4(i)*s12(i)+epx2(i)*s22(i)+epx5(i)*s32(i)
      a23(i)=epx4(i)*s13(i)+epx2(i)*s23(i)+epx5(i)*s33(i)
      a31(i)=epx6(i)*s11(i)+epx5(i)*s21(i)+epx3(i)*s31(i)
      a32(i)=epx6(i)*s12(i)+epx5(i)*s22(i)+epx3(i)*s32(i)
      a33(i)=epx6(i)*s13(i)+epx5(i)*s23(i)+epx3(i)*s33(i)
   80 continue
      do 90 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      epx2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      epx5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   90 continue
      endif
      return
      end
      subroutine sh3sc (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign11(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),sg3old(128),
     7 sg3lst(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),aux(128,43),
     4 sg1a(128),sg2a(128),d7(128),fac1(128),fac2(128),g(128),
     5 fac1qh(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),qbqh(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/tsarry/tslimt,tsarry(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+21)
      qs=cm(mx+11)
      pr=cm(mx+6)
      ss=cm(mx+2)
      qa=1.-qb
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*cm(mx+26)
      g(i)   =fac4*ym(i)
      qbqh(i)=qb*qh(i)
      fac1(i)=1.5*g(i)
      eyld(i)=qs
      etanmd(i)=qh(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
    5 continue
c
      if (qb.lt.0.9999999.and.ibelyt.le.1) then
c
      do 10 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i) =epx1(i)*s11(i)+epx4(i)*s12(i)+epx6(i)*s13(i)
      a12(i) =epx1(i)*s21(i)+epx4(i)*s22(i)+epx6(i)*s23(i)
      a13(i) =epx1(i)*s31(i)+epx4(i)*s32(i)+epx6(i)*s33(i)
      a21(i) =epx4(i)*s11(i)+epx2(i)*s12(i)+epx5(i)*s13(i)
      a22(i) =epx4(i)*s21(i)+epx2(i)*s22(i)+epx5(i)*s23(i)
      a23(i) =epx4(i)*s31(i)+epx2(i)*s32(i)+epx5(i)*s33(i)
      a31(i) =epx6(i)*s11(i)+epx5(i)*s12(i)+epx3(i)*s13(i)
      a32(i) =epx6(i)*s21(i)+epx5(i)*s22(i)+epx3(i)*s23(i)
      a33(i) =epx6(i)*s31(i)+epx5(i)*s32(i)+epx3(i)*s33(i)
   10 continue
      do 20 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      epx2(i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      epx5(i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   20 continue
      endif
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym(i)*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
c
c     compute stress increment and add to rotated stresses
c
      do 30 i=lft,llt
      blkg(i)=third*(blk(i)+g(i))
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)=da4(i)-epx4(i)
      t5(i)=da5(i)-epx5(i)
      t6(i)=da6(i)-epx6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak(i)  =qs+qbqh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
   30 continue
      do 40 i=lft,llt
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      do 60 i=lft,llt
      ebar(i)=scle(i)*qh(i)+(1.-scle(i))*ym(i)
      fac1qh(i)=1./(fac1(i)+qh(i))
      fac2qh(i)=fac1(i)*fac1qh(i)
      fac2(i)=qa*qh(i)/fac1(i)
      aj1(i)=sqrt(aj2(i))+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      depn(i)=fac2(i)*deps(i)
      sig1(i)=da1(i) -deps(i)*(t1(i)-p(i))
      sig2(i)=da2(i) -deps(i)*(t2(i)-p(i))
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*t4(i)
      sig5(i)=da5(i) -deps(i)*t5(i)
      sig6(i)=da6(i) -deps(i)*t6(i)
      epx1(i)=epx1(i)+depn(i)*(t1(i)-p(i))
      epx2(i)=epx2(i)+depn(i)*(t2(i)-p(i))
      ep(i)  =  ep(i)+depi(i)
      epx4(i)=epx4(i)+depn(i)*t4(i)
      epx5(i)=epx5(i)+depn(i)*t5(i)
      epx6(i)=epx6(i)+depn(i)*t6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   60 continue
   75 if (qb.lt.0.9999999.and.ibelyt.le.1) then
      do 80 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i)=epx1(i)*s11(i)+epx4(i)*s21(i)+epx6(i)*s31(i)
      a12(i)=epx1(i)*s12(i)+epx4(i)*s22(i)+epx6(i)*s32(i)
      a13(i)=epx1(i)*s13(i)+epx4(i)*s23(i)+epx6(i)*s33(i)
      a21(i)=epx4(i)*s11(i)+epx2(i)*s21(i)+epx5(i)*s31(i)
      a22(i)=epx4(i)*s12(i)+epx2(i)*s22(i)+epx5(i)*s32(i)
      a23(i)=epx4(i)*s13(i)+epx2(i)*s23(i)+epx5(i)*s33(i)
      a31(i)=epx6(i)*s11(i)+epx5(i)*s21(i)+epx3(i)*s31(i)
      a32(i)=epx6(i)*s12(i)+epx5(i)*s22(i)+epx3(i)*s32(i)
      a33(i)=epx6(i)*s13(i)+epx5(i)*s23(i)+epx3(i)*s33(i)
   80 continue
      do 90 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      epx2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      epx5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   90 continue
      endif
      return
      end
      subroutine sh3si (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic/kinematic hardening
c     iterative version of plane stress plasticity
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign11(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),sg3lst(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),aux(128,43),
     4 sg1a(128),sg2a(128),d7(128),fac1(128),fac2(128),g(128),
     5 fac1qh(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),qbqh(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/tsarry/tslimt,tsarry(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+21)
      qs=cm(mx+11)
      pr=cm(mx+6)
      ss=cm(mx+2)
      qa=1.-qb
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*cm(mx+26)
      g(i)   =fac4*ym(i)
      qbqh(i)=qb*qh(i)
      eyld(i)=qs
      etanmd(i)=qh(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
    5 continue
c
      if (qb.lt.0.9999999.and.ibelyt.le.1) then
c
      do 10 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i) =epx1(i)*s11(i)+epx4(i)*s12(i)+epx6(i)*s13(i)
      a12(i) =epx1(i)*s21(i)+epx4(i)*s22(i)+epx6(i)*s23(i)
      a13(i) =epx1(i)*s31(i)+epx4(i)*s32(i)+epx6(i)*s33(i)
      a21(i) =epx4(i)*s11(i)+epx2(i)*s12(i)+epx5(i)*s13(i)
      a22(i) =epx4(i)*s21(i)+epx2(i)*s22(i)+epx5(i)*s23(i)
      a23(i) =epx4(i)*s31(i)+epx2(i)*s32(i)+epx5(i)*s33(i)
      a31(i) =epx6(i)*s11(i)+epx5(i)*s12(i)+epx3(i)*s13(i)
      a32(i) =epx6(i)*s21(i)+epx5(i)*s22(i)+epx3(i)*s23(i)
      a33(i) =epx6(i)*s31(i)+epx5(i)*s32(i)+epx3(i)*s33(i)
   10 continue
      do 20 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      epx2(i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      epx5(i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   20 continue
      endif
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym(i)*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
c
c     compute stress increment and add to rotated stresses
c
      do 30 i=lft,llt
      blkg(i)  =third*(blk(i)+g(i))
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)=da4(i)-epx4(i)
      t5(i)=da5(i)-epx5(i)
      t6(i)=da6(i)-epx6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak(i)  =qs+qbqh(i)*ep(i)
   30 aks(i) =ak(i)*ak(i)
      do 40 i=lft,llt
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      ebar(i)=ym(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 75
      endif
      do 47 i=lft,llt
      ebar(i)=scle(i)*qh(i)+(1.-scle(i))*ym(i)
      fac1qh(i)=1./(fac1(i)+qh(i))
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      ckdev=.5+sign(.5*unit,abs(d3(i)-epsnew(i))-1.e-14)
      d7(i)=ckdev*d3(i) + .5*(1. - ckdev)*d2(i)
      d3(i)=scle(i)*d7(i) + (1. - scle(i))*epsnew(i)
c
      deps(i)=0.0
   47 continue
      do 60 i=lft,llt
      if (scle(i).eq.0.0) go to 60
      do 50 iter=1,20
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))
      depi(i)=(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
      if(abs(epsnew(i)-d3(i))/(abs(d3(i))+1.e-07).lt..0001) go to 60
   50 continue
   60 continue
      do 70 i=lft,llt
      fac2(i)=qa*qh(i)/fac1(i)
      depn(i) =fac2(i)*deps(i)
      sig1(i)=da1(i) -deps(i)*(t1(i))
      sig2(i)=da2(i) -deps(i)*(t2(i))
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*t4(i)
      sig5(i)=da5(i) -deps(i)*t5(i)
      sig6(i)=da6(i) -deps(i)*t6(i)
      epx1(i)=epx1(i)+depn(i)*t1(i)
      epx2(i)=epx2(i)+depn(i)*t2(i)
      ep(i)  =  ep(i)+depi(i)
      epx4(i)=epx4(i)+depn(i)*t4(i)
      epx5(i)=epx5(i)+depn(i)*t5(i)
      epx6(i)=epx6(i)+depn(i)*t6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   75 if (qb.lt.0.9999999.and.ibelyt.le.1) then
      do 80 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i)=epx1(i)*s11(i)+epx4(i)*s21(i)+epx6(i)*s31(i)
      a12(i)=epx1(i)*s12(i)+epx4(i)*s22(i)+epx6(i)*s32(i)
      a13(i)=epx1(i)*s13(i)+epx4(i)*s23(i)+epx6(i)*s33(i)
      a21(i)=epx4(i)*s11(i)+epx2(i)*s21(i)+epx5(i)*s31(i)
      a22(i)=epx4(i)*s12(i)+epx2(i)*s22(i)+epx5(i)*s32(i)
      a23(i)=epx4(i)*s13(i)+epx2(i)*s23(i)+epx5(i)*s33(i)
      a31(i)=epx6(i)*s11(i)+epx5(i)*s21(i)+epx3(i)*s31(i)
      a32(i)=epx6(i)*s12(i)+epx5(i)*s22(i)+epx3(i)*s32(i)
      a33(i)=epx6(i)*s13(i)+epx5(i)*s23(i)+epx3(i)*s33(i)
   80 continue
      do 90 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      epx2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      epx5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   90 continue
      endif
      return
      end
      subroutine shl4s (cm,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     thermoelastic-plastic shell model
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),epx3(128),da4(128),
     2 da5(128),da6(128),t456(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),tref(128),otmp(128),ctmp(128),
     3 h1(128),h2(128),h3(128),h4(128),h5(128),h6(128),
     5 g1(128),g2(128),g3(128),g4(128),g5(128),g6(128),
     6 ym(128),pv(128),qs(128),qh(128),ap(128),
     7 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),
     8 s7(128),s8(128),c11i(128),c12i(128),d11t(128),d12t(128),
     9 d44t(128)
      common/aux18/dd(128),def(128)
      common/aux19/q1(128),q2(128),q3(128),q4(128)
      common/aux20/dymt(128),dpvt(128),dqst(128),dhmt(128),
     1 dalt(128),deltem(128),xmidtm(128),pres(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128),teps(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/double/iprec,ncpw,unit
      dimension  cm(*),tnew(*),fval(*),thrmo(5),thrmn(5)
c
      data third/-.3333333333333/
      mx=48*(mxt(lft)-1)
c     temperature at element center
c
      if (itemp.lt.0) go to 20
      do 10 i=lft,llt
   10 ctmp(i)=fval(itemp)
      go to 40
   20 do 30 i=lft,llt
      ctmp(i)=.25*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i)))
   30 continue
c
c     elas=0. if elastic properties
c
   40 elas=cm(mx+33)+cm(mx+34)
c
      do 50 i=lft,llt
      deltem(i)=ctmp(i)-otmp(i)
   50 xmidtm(i)=.5*(ctmp(i)+otmp(i))
c
c     get material properties at beginning and ending temperatures
c
      do 60 i=lft,llt
      dt=deltem(i)
      if (abs(dt).lt.1.e-20) dt=1.0
      dti=1./dt
      call itrpm (cm(mx+1),cm(mx+9),otmp(i),cm(mx+9),thrmo)
      call itrpm (cm(mx+1),cm(mx+9),ctmp(i),cm(mx+9),thrmn)
      ym(i)=.50*(thrmo(1)+thrmn(1))
      pv(i)=.50*(thrmo(2)+thrmn(2))
      qs(i)=.50*(thrmo(4)+thrmn(4))
      qh(i)=.50*(thrmo(5)+thrmn(5))
      ap(i)=.50*(thrmo(3)+thrmn(3))
      dymt(i)=dti*(thrmn(1)-thrmo(1))
      dpvt(i)=dti*(thrmn(2)-thrmo(2))
      dqst(i)=dti*(thrmn(4)-thrmo(4))
      dhmt(i)=dti*(thrmn(5)-thrmo(5))
      dalt(i)=thrmn(3)-thrmo(3)
      ebar(i)=ym(i)
      eyld(i)=qs(i)
      etanmd(i)=qh(i)
   60 continue
c
      sndspd=0.0
      ymod=0.
      gmod=0.
      do 70 i=lft,llt
      s1(i)=(1.+pv(i))*(1.0-2.0*pv(i))
      s2(i)=(1.-pv(i))/s1(i)
      s3(i)=ym(i)/(1.+pv(i))
      s4(i)=s3(i)/2.
      s5(i)=s3(i)/(1.-2.*pv(i))
      s6(i)=s5(i)*pv(i)
      s7(i)=s5(i)-s6(i)
      s8(i)=-ym(i)/(1-2.*pv(i))
      cb(i)=s6(i)+s3(i)
      ymod=  max(ymod,ym(i))
      gmod=  max(gmod,s4(i))
   70 sndspd=max(sndspd,cb(i))
c
      do 80 i=lft,llt
      c11i(i)=1.0/ym(i)
      c12i(i)=-pv(i)/ym(i)
      d11t(i)=dymt(i)*s2(i)+dpvt(i)*((s2(i)*(1.0+4.0*pv(i))*ym(i)-ym(i))
     1 /s1(i))
      d12t(i)=dymt(i)*pv(i)/s1(i)+dpvt(i)*((ym(i)+pv(i)*ym(i)*(1.+4.*pv
     1 (i))/s1(i))/s1(i))
   80 d44t(i)=(dymt(i)-dpvt(i)*s3(i))/(2.*(1.+pv(i)))
c
      do 90 i=lft,llt
      h1(i)=c11i(i)*sig1(i)+c12i(i)*(sig2(i)+sig3(i))
      h2(i)=c11i(i)*sig2(i)+c12i(i)*(sig1(i)+sig3(i))
      h3(i)=c11i(i)*sig3(i)+c12i(i)*(sig1(i)+sig2(i))
      h4(i)=sig4(i)/s4(i)
      h5(i)=sig5(i)/s4(i)
   90 h6(i)=sig6(i)/s4(i)
c
      do 100 i=lft,llt
      g1(i)=d11t(i)*h1(i)+d12t(i)*(h2(i)+h3(i))
      g2(i)=d11t(i)*h2(i)+d12t(i)*(h1(i)+h3(i))
      g3(i)=d11t(i)*h3(i)+d12t(i)*(h2(i)+h1(i))
      g4(i)=d44t(i)*h4(i)
      g5(i)=d44t(i)*h5(i)
  100 g6(i)=d44t(i)*h6(i)
c
      do 110 i=lft,llt
  110 teps(i)=ap(i)*deltem(i)+dalt(i)*(xmidtm(i)-tref(i))
c
      do 130 i=lft,llt
      d1(i)=d1(i)-teps(i)
      d2(i)=d2(i)-teps(i)
      d3(i)=-(sig3(i)+s6(i)*(d1(i)+d2(i))+g3(i)*deltem(i))/s7(i)
      sig4(i)=sig4(i)+s4(i)*d4(i)
      sig5(i)=sig5(i)+s4(i)*d5(i)
  130 sig6(i)=sig6(i)+s4(i)*d6(i)
c
c     compute stress increment and add to rotated stresses
c
      do 150 i=lft,llt
      da4(i)=sig4(i)+g4(i)*deltem(i)
      da5(i)=sig5(i)+g5(i)*deltem(i)
      da6(i)=sig6(i)+g6(i)*deltem(i)
      t4(i)=da4(i)
      t5(i)=da5(i)
      t6(i)=da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
  150 ak(i)=qs(i)+qh(i)*ep(i)
      do 160 i=lft,llt
      da1(i)=sig1(i)+s7(i)*d1(i)+s6(i)*d2(i)+s6(i)*d3(i)
      da2(i)=sig2(i)+s6(i)*d1(i)+s7(i)*d2(i)+s6(i)*d3(i)
      da3(i)=sig3(i)+s6(i)*d1(i)+s6(i)*d2(i)+s7(i)*d3(i)
      da1(i)=da1(i)+g1(i)*deltem(i)
      da2(i)=da2(i)+g2(i)*deltem(i)
      da3(i)=da3(i)+g3(i)*deltem(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=p(i)+da3(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =0.0
      depi(i)=0.0
      deps(i)=0.0
  160 continue
      scl=0.
      do 170 i=lft,llt
  170 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 180 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
  180 continue
      go to 220
      endif
      do 200 i=lft,llt
      if (scle(i).eq.0.)  go to 200
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3new=sg3old
      epsnew=d3(i)
      d3(i)=-d1(i)-d2(i)
      ebar(i)=qh(i)*scle(i)+ym(i)*(1.-scle(i))
      do 190 iter=2,20
      fac1qh=1./(1.5*s3(i)+qh(i))
      da1(i)=sig1(i)+s7(i)*d1(i)+s6(i)*d2(i)+s6(i)*d3(i)
      da2(i)=sig2(i)+s6(i)*d1(i)+s7(i)*d2(i)+s6(i)*d3(i)
      da3(i)=sig3(i)+s6(i)*d1(i)+s6(i)*d2(i)+s7(i)*d3(i)
      da1(i)=da1(i)+g1(i)*deltem(i)
      da2(i)=da2(i)+g2(i)*deltem(i)
      da3(i)=da3(i)+g3(i)*deltem(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=p(i)+da3(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =sqrt(aj2(i))+1.0-scle(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh
      deps(i)=1.5*s3(i)*depi(i)/aj1(i)
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3lst=sg3new
      sg3new=sg3old
      epslst=epsnew
      epsnew=d3(i)
      demn=1.e-20+sg3new-sg3lst
      if (abs(demn).lt.1.e-14) go to 200
      d3(i)=epslst-sg3lst*(epsnew-epslst)/demn
      if(abs(epsnew-epslst)/abs(d3(i)).lt.00.00001) go to 200
  190 continue
  200 continue
      do 210 i=lft,llt
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i)-deps(i)*t4(i)
      sig5(i)=da5(i)-deps(i)*t5(i)
      sig6(i)=da6(i)-deps(i)*t6(i)
      ep(i)  = ep(i)+depi(i)
  210 continue
  220 continue
c
  250 do 260 i=lft,llt
  260 otmp(i)=ctmp(i)
      return
      end
      subroutine shl12s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     plasticity subroutine based on simple radial return
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign11(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),sg3lst(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),aux(128,56),
     4 sg1a(128),sg2a(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt
      common/double/iprec,ncpw,unit
      dimension cm(*)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      g =cm(mx+ 1)
      qs=cm(mx+ 2)
      qh=cm(mx+ 3)
      ym=cm(mx+10)
      pr=cm(mx+11)
      blk=-ym/(1.-2.*pr)
      c11=ym/(1.-pr*pr)
      c12=pr*c11
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=g
      wq3=wq1+2.0*wq2
      wq4=capa*wq2
      wq3=1./wq3
      ymod=ym
      gmod=wq4
      sndspd=c11
c
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
c
c     compute stress increment and add to rotated stresses
c
      fac1qh=1./(3.*g+qh)
      do 30 i=lft,llt
      ebar(i)=ym
      eyld(i)=qs
      etanmd(i)=qh
      cb(i)=sndspd
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1*d1d2(i))*wq3
      da1(i)=sig1(i)+c11*d1(i)+c12*d2(i)
      da2(i)=sig2(i)+c12*d1(i)+c11*d2(i)
      da4(i)=sig4(i)+wq2*d4(i)
      da5(i)=sig5(i)+wq4*d5(i)
      da6(i)=sig6(i)+wq4*d6(i)
      t456(i)=3.*(da4(i)*da4(i)+da5(i)*da5(i)+da6(i)*da6(i))
      ak(i)  =qs+qh*ep(i)
      aj2(i)=sqrt(da1(i)**2+da2(i)**2-da1(i)*da2(i)+t456(i))
      ak2(i)=aj2(i)-ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      ep(i)=ep(i)+scle(i)*fac1qh*ak2(i)
      ak(i)  =qs+qh*ep(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
      sig3(i)=0.
   30 continue
      do 70 i=lft,llt
      ebar(i)=qh*scle(i)+ym*(1.-scle(i))
      scalef=1.-scle(i)+scle(i)*ak(i)/(aj2(i)+1.e-30)
      sig1(i)=scalef*da1(i)
      sig2(i)=scalef*da2(i)
      sig4(i)=scalef*da4(i)
      sig5(i)=scalef*da5(i)
      sig6(i)=scalef*da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
      end
      subroutine shl15s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     johnson/cook material model adapted for shell analysis
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),ywh(128),
     7 sg3lst(128),effs(128),qs(128),d1d(128),d2d(128),d3d(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),fail(128),xm(128),
     3 epx5(128),epx6(128),efrc(128),akt(128),sj2(128),
     4 b(128),tmp(128),tmpf(128),epsf(128),a(128),aux(128,47),
     5 dtflp(128),dtflm(128),sg1a(128),sg2a(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/ss,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension cm(*)
      data third/-.3333333333333/
      data zero/0.0/
c
      mx=48*(mxt(lft)-1)
      ymd=cm(mx+41)
      pr=cm(mx+42)
      tf=cm(mx+43)
      gm=ymd/(1.+pr)
      blkd=-ymd/(1.-2.*pr)
      wq1s=ymd*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2s=.5*gm
      wq3s=wq1s+2.0*wq2s
      wq4s=capa*wq2s
      wq3s=1./wq3s
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      ymod=ymd
      gmod=wq4s
      ss=.66666666666667*gm+third*blkd
      ac=cm(mx+2)
      bc=cm(mx+3)
      xn=cm(mx+4)
      c=cm(mx+5)
      xm2=cm(mx+6)
      tmpm=cm(mx+7)
      tmpr=cm(mx+8)
      epsi=1./cm(mx+9)
      if (cm(mx+10).ne.0.0) then
      cpi=1./cm(mx+10)/rhoa(lft)
      else
      cpi=0.
      endif
      ispl=nint(cm(mx+12))-1
      cd1=cm(mx+17)
      cd2=cm(mx+18)
      cd3=cm(mx+19)
      cd4=cm(mx+20)
      cd5=cm(mx+21)
      cd6=cm(mx+22)
      xn1=xn-1.
      gdt=dt1*gm
      xmu3=1.5*gm
      gd2=.5*gdt
      tfc=1./(tmpm-tmpr)
      opti=cm(mx+13)
c
c     compute stress increment and add to rotated stresses
c
      do 10 i=lft,llt
      ebar(i)=ymd
      etanmd(i)=ymd
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1s*d1d2(i))*wq3s
      dmean=third*(d1(i)+d2(i)+d3(i))
      d1d(i)=d1(i)+dmean
      d2d(i)=d2(i)+dmean
      d3d(i)=d3(i)+dmean
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2+.5*(d4(i)**2+d5(i)**2+d6
     1 (i)**2))/3.
      effs(i)=dti*sqrt(effs(i))
      tmp(i)=  max(zero,tfc*xm(i)*sieu(i)*cpi)
      tmpf(i)=1.0-tmp(i)**xm2
      effs(i)=  max(.0020*unit,epsi*effs(i))
      effs(i)=log(effs(i))
      epsf(i)=tmpf(i)*(1.+c*effs(i))
      epx(i)=epx(i)+.0001
   10 continue
c
      if (tf.ne.0.) then
c
      if (ipt.eq.1) then
      do 20 i=lft,llt
      failu(i)=0.0
   20 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(ss*(1./rhoa(lft)))
      do 30 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      fail(i)=dtflm(i)*fail(i)
   30 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 40 i=lft,llt
      fail(i)=dtflm(i)*fail(i)
   40 continue
      endif
      endif
      endif
c
      do 50 i=lft,llt
      a(i)=ac*epsf(i)
      b(i)=bc*epsf(i)
      ak(i)=a(i)+b(i)*epx(i)**xn
      eyld(i)=ak(i)
   50 aks(i)=ak(i)*ak(i)
      do 60 i=lft,llt
      da4(i)=sig4(i)+wq2s*d4(i)
      da5(i)=sig5(i)+wq4s*d5(i)
      da6(i)=sig6(i)+wq4s*d6(i)
      t4(i)=da4(i)
      t5(i)=da5(i)
      t6(i)=da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      blkg=third*(blkd+gm)
      p(i)=blkg*(d1d2(i)+d3(i))
      sg1a(i)=sig1(i)+gm*d1(i)
      sg2a(i)=sig2(i)+gm*d2(i)
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+gm*d3(i)
      p(i)=third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)+d6
     1 (i)*sig6(i)
   60 continue
      scl=0.
      do 70 i=lft,llt
   70 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 80 i=lft,llt
      sig1(i)=fail(i)*da1(i)
      sig2(i)=fail(i)*da2(i)
      sig3(i)=0.
      sig4(i)=fail(i)*da4(i)
      sig5(i)=fail(i)*da5(i)
      sig6(i)=fail(i)*da6(i)
      epx(i)=epx(i)-.0001
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)+d6
     1 (i)*sig6(i)+einc(i)
      failu(i)=  max(fail(i),failu(i))
   80 continue
      return
      endif
      do 90 i=lft,llt
      fac1=1.5*gm
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      deps(i)=0.0
   90 continue
      do 110 i=lft,llt
      if (scle(i).eq.0.0) go to 110
      etanmd(i)=xnb*epx(i)**xn1
      ebar(i)=etanmd(i)
      do 100 iter=1,20
      p(i)=blkg*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+gm*d3(i)
      p(i)=third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      aj1(i)=sqrt(aj2(i))
      xnb=xn*b(i)
      depi(i)=(aj1(i)-a(i)-b(i)*epx(i)**xn)/(xmu3+xnb*epx(i)**xn1)
      depi(i)=  max(zero,depi(i))
      deps(i)=fac1*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)-deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
      if (abs(epsnew(i)-epslst(i))/(abs(d3(i))+1.e-07).lt..0001) go to 1
     1 10
  100 continue
  110 continue
      do 120 i=lft,llt
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i)-deps(i)*t4(i)
      sig5(i)=da5(i)-deps(i)*t5(i)
      sig6(i)=da6(i)-deps(i)*t6(i)
      epx(i)=epx(i)+fail(i)*depi(i)-.0001
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)+d6
     1 (i)*sig6(i)+einc(i)
  120 continue
c
c     johnson/cook damage model
c
      if (cd6.gt.0.0) then
      do 130 i=lft,llt
      po(i)=third*(sig1(i)+sig2(i))
      t1(i)=sig1(i)+po(i)
      t2(i)=sig2(i)+po(i)
      t3(i)=po(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+3.*(sig4(i)*sig4(i)+sig5(i
     1 )*sig5(i)+sig6(i)*sig6(i))
      sj2(i)= cd3*po(i)/(sqrt(aj2(i))+1.e-20)
      efrc(i)=(cd1+cd2*exp(sj2(i)))*(1.+cd4*effs(i))*(1.+cd5*tmp(i))
      epx6(i)=epx6(i)+depi(i)/efrc(i)
  130 continue
      do 140 i=lft,llt
      if (epx6(i).ge.1.0) then
      epx6(i)=1.0
      fail(i)=0.0
      endif
  140 continue
      endif
c
      if (ispl.eq.1) then
      do 150 i=lft,llt
      po(i)=third*(sig1(i)+sig2(i))
      t1(i)=sig1(i)+po(i)
      t2(i)=sig2(i)+po(i)
      t3(i)=po(i)
      aj2(i)=.5*(t1(i)**2+t2(i)**2+t3(i)**2)+sig4(i)**2+sig5(i)**2+sig6
     1 (i)**2+1.e-12
      sj2(i)=t1(i)*sig5(i)**2+t2(i)*sig6(i)**2+t3(i)*sig4(i)**2-t1(i)*t2
     1 (i)*t3(i)-2.*sig4(i)*sig5(i)*sig6(i)
      akt(i)=-sqrt(27./aj2(i))*sj2(i)*0.5/aj2(i)
      akt(i)=sign(  min(abs(akt(i)),1.*unit),akt(i))
      ywh(i)=acos(akt(i))*abs(third)
      sj2(i)=2.*sqrt(aj2(i)*abs(third))*cos(ywh(i))
      davg(i)=po(i)-sj2(i)-epx5(i)
      fail(i)=  min(fail(i),.5*unit+sign(.5*unit,davg(i)))
      epx(i)=epx(i)-(1.-fail(i))*depi(i)
  150 epx5(i)=fail(i)*epx5(i)
      endif
c
      if (ispl.eq.2) then
      do 160 i=lft,llt
      davg(i)=po(i)-epx5(i)
      fail(i)=  min(fail(i),.5*unit+sign(.5*unit,davg(i)))
  160 epx5(i)=fail(i)*epx5(i)
      endif
c
      do 170 i=lft,llt
      sig1(i)=fail(i)*sig1(i)
      sig2(i)=fail(i)*sig2(i)
      sig3(i)=fail(i)*sig3(i)
      sig4(i)=fail(i)*sig4(i)
      sig5(i)=fail(i)*sig5(i)
      sig6(i)=fail(i)*sig6(i)
      failu(i)=  max(fail(i),failu(i))
  170 continue
c
      return
      end
      subroutine shl19s (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with strain rate sensitivity
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),blkg(128),
     7 sg3lst(128),effs(128),qs(128),d1d(128),d2d(128),d3d(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),fail(128),ymold(128),qh(128),
     3 fac1(128),blk(128),g(128),fac1qh(128),ym(128),fs(128),
     4 aj3(128),wq1(128),wq2(128),wq3(128),wq4(128),aux(128,47),
     5 dtflp(128),dtflm(128),sg1a(128),sg2a(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/ss,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/tsarry/tslimt,tsarry(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  cm(1),npc(1),plc(1)
      data third/-.3333333333333/
c
      mx  =48*(mxt(lft)-1)
      ymd =cm(mx+1)
      pr  =cm(mx+2)
      i1  =cm(mx+3)
      j1  =cm(mx+5)
      k1  =cm(mx+6)
      l1  =cm(mx+7)
      tf  =cm(mx+8)
      qhd =cm(mx+1)*cm(mx+4)/(cm(mx+1)-cm(mx+4))
      gm  =ymd/(1.+pr)
      blkd=-ymd/(1.-2.*pr)
      wq1s=ymd*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2s=.5*gm
      wq3s=wq1s+2.0*wq2s
      wq4s=capa*wq2s
      wq3s=1./wq3s
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      ymod=ymd
      gmod=wq4s
      ss  =.66666666666667*gm+third*blkd
c
c     compute stress increment and add to rotated stresses
c
      do 10 i=lft,llt
      ebar(i)=ymd
      etanmd(i)=qhd
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1s*d1d2(i))*wq3s
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   10 continue
c
      if (l1.ne.0.or.tf.ne.0.) then
c
      if (ipt.eq.1) then
      do 11 i=lft,llt
      failu(i)=0.0
   11 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(ss*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      fail(i)=dtflm(i)*fail(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 13 i=lft,llt
      fail(i)=dtflm(i)*fail(i)
   13 continue
      endif
c
      endif
      endif
      i2= npc(i1)
      if (j1.ne.0) then
      j2= npc(j1)
      else
      j2=i2
      endif
      if (k1.ne.0) then
      k2= npc(k1)
      else
      k2=i2
      endif
      if (l1.ne.0) then
      l2= npc(l1)
      else
      l2=i2
      endif
      n1=(npc(i1+1)-i2)/2
c
c     interpolate to obtain qs, ym, and qh
c     assuming same number of points in all 3 curves, same intervals
c     (n1=no. of points in curve)
c
      call gtp19 (plc(i2),plc(j2),plc(k2),plc(l2),effs,n1,qs,
     1 ym, qh,fs,lft,llt)
c
      do 14 i=lft,llt
      ym(i)=tsarry(i)*ym(i)
      qh(i)=tsarry(i)*qh(i)
   14 continue
c
      if (j1.eq.0) then
      do 15 i=lft,llt
   15 ym(i)=tsarry(i)*ymd
      else
      demn1 =1./(2.*(1.+pr))
      demn2 =1./(3.*(1.-2.*pr))
      do 16 i=lft,llt
      gmold=ymold(i)*demn1
      h1=(sig1(i)-pr*sig2(i))/ymold(i)
      h2=(sig2(i)-pr*sig1(i))/ymold(i)
      h4= sig4(i)/gmold
      h5= sig5(i)/gmold
      h6= sig6(i)/gmold
      blkmd=ym(i)*demn2
      shrmd=ym(i)*demn1
      xk43g=blkmd+4.*shrmd/3.
      xk23g=blkmd-2.*shrmd/3.
      wq1s=ym(i)*pr/((1.0+pr)*(1.0-2.0*pr))
      wq3s=1./(wq1s+2.0*shrmd)
      h3=-wq1s*(h1+h2)*wq3s
      sig1(i)=xk43g*h1+xk23g*(h2+h3)
      sig2(i)=xk43g*h2+xk23g*(h1+h3)
      sig3(i)=0.0
      sig4(i)=shrmd*h4
      sig5(i)=shrmd*h5
      sig6(i)=shrmd*h6
      ymold(i)=ym(i)
   16 continue
      endif
      if (k1.eq.0) then
      do 17 i=lft,llt
   17 qh(i)=tsarry(i)*qhd
      else
      do 18 i=lft,llt
   18 qh(i)=ym(i)*qh(i)/(ym(i)-qh(i))
      endif
      facnt=-1.0/(1.-2.*pr)
      facnu= 1.0/(1.+pr)
      facnv=pr/((1.0+pr)*(1.0-2.0*pr))
      do 20 i=lft,llt
      blk(i)=ym(i)*facnt
      g(i)  =ym(i)*facnu
      wq1(i)=ym(i)*facnv
      wq2(i)=.5*g(i)
      wq3(i)=wq1(i)+2.0*wq2(i)
      wq4(i)=capa*wq2(i)
      wq3(i)=1./wq3(i)
      ak(i)  =qs(i)+qh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
      ebar(i)=ym(i)
      eyld(i)=qs(i)
      etanmd(i)=qh(i)
   20 continue
      do 40 i=lft,llt
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)=da4(i)
      t5(i)=da5(i)
      t6(i)=da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      blkg(i)=third*(blk(i)+g(i))
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 48 i=lft,llt
      aj3(i)=sqrt(aj2(i))
      fail(i)=fail(i)*(.5+sign(.5,fs(i)-aj3(i)))
      sig1(i)=fail(i)*sig1(i)
      sig2(i)=fail(i)*sig2(i)
      sig3(i)=fail(i)*sig3(i)
      sig4(i)=fail(i)*sig4(i)
      sig5(i)=fail(i)*sig5(i)
      sig6(i)=fail(i)*sig6(i)
      failu(i)=max(fail(i),failu(i))
   48 continue
      return
      endif
      do 47 i=lft,llt
      fac1(i)  =1.5*g(i)
      fac1qh(i)=1./(fac1(i)+qh(i))
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      deps(i)=0.0
   47 continue
      do 60 i=lft,llt
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      aj3(i) =sqrt(aj2(i))
      aj1(i) =aj3(i)+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      aj3(i) =sqrt(aj2(i))
      aj1(i) =aj3(i)+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      aj3(i) =sqrt(aj2(i))
      aj1(i) =aj3(i)+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      ebar(i)=qh(i)*scle(i)+ym(i)*(1.-scle(i))
   60 continue
      do 70 i=lft,llt
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i)-deps(i)*t4(i)
      sig5(i)=da5(i)-deps(i)*t5(i)
      sig6(i)=da6(i)-deps(i)*t6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs(i)-aj3(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
c
      return
      end
      subroutine sh19sc (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with strain rate sensitivity
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),blkg(128),
     7 sg3lst(128),effs(128),qs(128),d1d(128),d2d(128),d3d(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),fail(128),ymold(128),qh(128),
     3 fac1(128),blk(128),g(128),fac1qh(128),ym(128),fs(128),
     4 aj3(128),wq1(128),wq2(128),wq3(128),wq4(128),aux(128,47),
     5 dtflp(128),dtflm(128),sg1a(128),sg2a(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/ss,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/tsarry/tslimt,tsarry(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  cm(1),npc(1),plc(1)
      data third/-.3333333333333/
c
      mx  =48*(mxt(lft)-1)
      ymd =cm(mx+1)
      pr  =cm(mx+2)
      i1  =cm(mx+3)
      j1  =cm(mx+5)
      k1  =cm(mx+6)
      l1  =cm(mx+7)
      tf  =cm(mx+8)
      qhd =cm(mx+1)*cm(mx+4)/(cm(mx+1)-cm(mx+4))
      gm  =ymd/(1.+pr)
      blkd=-ymd/(1.-2.*pr)
      wq1s=ymd*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2s=.5*gm
      wq3s=wq1s+2.0*wq2s
      wq4s=capa*wq2s
      wq3s=1./wq3s
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      ymod=ymd
      gmod=wq4s
      ss  =.66666666666667*gm+third*blkd
c
c     compute stress increment and add to rotated stresses
c
      do 10 i=lft,llt
      ebar(i)=ymd
      etanmd(i)=qhd
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1s*d1d2(i))*wq3s
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   10 continue
c
      if (l1.ne.0.or.tf.ne.0.) then
c
      if (ipt.eq.1) then
      do 11 i=lft,llt
      failu(i)=0.0
   11 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(ss*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      fail(i)=dtflm(i)*fail(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 13 i=lft,llt
      fail(i)=dtflm(i)*fail(i)
   13 continue
      endif
c
      endif
      endif
c
c
      i2= npc(i1)
      if (j1.ne.0) then
      j2= npc(j1)
      else
      j2=i2
      endif
      if (k1.ne.0) then
      k2= npc(k1)
      else
      k2=i2
      endif
      if (l1.ne.0) then
      l2= npc(l1)
      else
      l2=i2
      endif
      n1=(npc(i1+1)-i2)/2
c
c     interpolate to obtain qs, ym, and qh
c     assuming same number of points in all 3 curves, same intervals
c     (n1=no. of points in curve)
c
      call gtp19 (plc(i2),plc(j2),plc(k2),plc(l2),effs,n1,qs,
     1 ym, qh,fs,lft,llt)
c
      do 14 i=lft,llt
      ym(i)=tsarry(i)*ym(i)
      qh(i)=tsarry(i)*qh(i)
   14 continue
c
      if (j1.eq.0) then
      do 15 i=lft,llt
   15 ym(i)=tsarry(i)*ymd
      else
      demn1 =1./(2.*(1.+pr))
      demn2 =1./(3.*(1.-2.*pr))
      do 16 i=lft,llt
      gmold=ymold(i)*demn1
      h1=(sig1(i)-pr*sig2(i))/ymold(i)
      h2=(sig2(i)-pr*sig1(i))/ymold(i)
      h4= sig4(i)/gmold
      h5= sig5(i)/gmold
      h6= sig6(i)/gmold
      blkmd=ym(i)*demn2
      shrmd=ym(i)*demn1
      xk43g=blkmd+4.*shrmd/3.
      xk23g=blkmd-2.*shrmd/3.
      wq1s=ym(i)*pr/((1.0+pr)*(1.0-2.0*pr))
      wq3s=1./(wq1s+2.0*shrmd)
      h3=-wq1s*(h1+h2)*wq3s
      sig1(i)=xk43g*h1+xk23g*(h2+h3)
      sig2(i)=xk43g*h2+xk23g*(h1+h3)
      sig3(i)=0.0
      sig4(i)=shrmd*h4
      sig5(i)=shrmd*h5
      sig6(i)=shrmd*h6
      ymold(i)=ym(i)
   16 continue
      endif
      if (k1.eq.0) then
      do 17 i=lft,llt
   17 qh(i)=tsarry(i)*qhd
      else
      do 18 i=lft,llt
   18 qh(i)=ym(i)*qh(i)/(ym(i)-qh(i))
      endif
      facnt=-1.0/(1.-2.*pr)
      facnu= 1.0/(1.+pr)
      facnv=pr/((1.0+pr)*(1.0-2.0*pr))
      do 20 i=lft,llt
      blk(i)=ym(i)*facnt
      g(i)  =ym(i)*facnu
      wq1(i)=ym(i)*facnv
      wq2(i)=.5*g(i)
      wq3(i)=wq1(i)+2.0*wq2(i)
      wq4(i)=capa*wq2(i)
      wq3(i)=1./wq3(i)
      ak(i)  =qs(i)+qh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
      ebar(i)=ym(i)
      eyld(i)=qs(i)
      etanmd(i)=qh(i)
   20 continue
      do 40 i=lft,llt
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)=da4(i)
      t5(i)=da5(i)
      t6(i)=da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      blkg(i)=third*(blk(i)+g(i))
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 48 i=lft,llt
      aj3(i)=sqrt(aj2(i))
      fail(i)=fail(i)*(.5+sign(.5,fs(i)-aj3(i)))
      sig1(i)=fail(i)*sig1(i)
      sig2(i)=fail(i)*sig2(i)
      sig3(i)=fail(i)*sig3(i)
      sig4(i)=fail(i)*sig4(i)
      sig5(i)=fail(i)*sig5(i)
      sig6(i)=fail(i)*sig6(i)
      failu(i)=max(fail(i),failu(i))
   48 continue
      return
      endif
      do 70 i=lft,llt
      fac1(i)  =1.5*g(i)
      fac1qh(i)=1./(fac1(i)+qh(i))
      aj3(i) =sqrt(aj2(i))
      aj1(i) =aj3(i)+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      ebar(i)=qh(i)*scle(i)+ym(i)*(1.-scle(i))
      sig1(i)=da1(i)-deps(i)*(t1(i)-p(i))
      sig2(i)=da2(i)-deps(i)*(t2(i)-p(i))
      sig3(i)=0.0
      sig4(i)=da4(i)-deps(i)*t4(i)
      sig5(i)=da5(i)-deps(i)*t5(i)
      sig6(i)=da6(i)-deps(i)*t6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs(i)-aj3(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
c
      return
      end
      subroutine sh19si (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with strain rate sensitivity
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),blkg(128),
     7 sg3lst(128),effs(128),qs(128),d1d(128),d2d(128),d3d(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),fail(128),ymold(128),qh(128),
     3 fac1(128),blk(128),g(128),fac1qh(128),ym(128),fs(128),
     4 aj3(128),wq1(128),wq2(128),wq3(128),wq4(128),aux(128,47),
     5 dtflp(128),dtflm(128),sg1a(128),sg2a(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/ss,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/tsarry/tslimt,tsarry(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  cm(1),npc(1),plc(1)
      data third/-.3333333333333/
c
      mx  =48*(mxt(lft)-1)
      ymd =cm(mx+1)
      pr  =cm(mx+2)
      i1  =cm(mx+3)
      j1  =cm(mx+5)
      k1  =cm(mx+6)
      l1  =cm(mx+7)
      tf  =cm(mx+8)
      qhd =cm(mx+1)*cm(mx+4)/(cm(mx+1)-cm(mx+4))
      gm  =ymd/(1.+pr)
      blkd=-ymd/(1.-2.*pr)
      wq1s=ymd*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2s=.5*gm
      wq3s=wq1s+2.0*wq2s
      wq4s=capa*wq2s
      wq3s=1./wq3s
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      ymod=ymd
      gmod=wq4s
      ss  =.66666666666667*gm+third*blkd
c
c     compute stress increment and add to rotated stresses
c
      do 10 i=lft,llt
      ebar(i)=ymd
      etanmd(i)=qhd
      cb(i)=ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1s*d1d2(i))*wq3s
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   10 continue
c
      if (l1.ne.0.or.tf.ne.0.) then
c
      if (ipt.eq.1) then
      do 11 i=lft,llt
      failu(i)=0.0
   11 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(ss*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      fail(i)=dtflm(i)*fail(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 13 i=lft,llt
      fail(i)=dtflm(i)*fail(i)
   13 continue
      endif
c
      endif
      endif
      i2= npc(i1)
      if (j1.ne.0) then
      j2= npc(j1)
      else
      j2=i2
      endif
      if (k1.ne.0) then
      k2= npc(k1)
      else
      k2=i2
      endif
      if (l1.ne.0) then
      l2= npc(l1)
      else
      l2=i2
      endif
      n1=(npc(i1+1)-i2)/2
c
c     interpolate to obtain qs, ym, and qh
c     assuming same number of points in all 3 curves, same intervals
c     (n1=no. of points in curve)
c
      call gtp19 (plc(i2),plc(j2),plc(k2),plc(l2),effs,n1,qs,
     1 ym, qh,fs,lft,llt)
c
      do 14 i=lft,llt
      ym(i)=tsarry(i)*ym(i)
      qh(i)=tsarry(i)*qh(i)
   14 continue
c
      if (j1.eq.0) then
      do 15 i=lft,llt
   15 ym(i)=tsarry(i)*ymd
      else
      demn1 =1./(2.*(1.+pr))
      demn2 =1./(3.*(1.-2.*pr))
      do 16 i=lft,llt
      gmold=ymold(i)*demn1
      h1=(sig1(i)-pr*sig2(i))/ymold(i)
      h2=(sig2(i)-pr*sig1(i))/ymold(i)
      h4= sig4(i)/gmold
      h5= sig5(i)/gmold
      h6= sig6(i)/gmold
      blkmd=ym(i)*demn2
      shrmd=ym(i)*demn1
      xk43g=blkmd+4.*shrmd/3.
      xk23g=blkmd-2.*shrmd/3.
      wq1s=ym(i)*pr/((1.0+pr)*(1.0-2.0*pr))
      wq3s=1./(wq1s+2.0*shrmd)
      h3=-wq1s*(h1+h2)*wq3s
      sig1(i)=xk43g*h1+xk23g*(h2+h3)
      sig2(i)=xk43g*h2+xk23g*(h1+h3)
      sig3(i)=0.0
      sig4(i)=shrmd*h4
      sig5(i)=shrmd*h5
      sig6(i)=shrmd*h6
      ymold(i)=ym(i)
   16 continue
      endif
      if (k1.eq.0) then
      do 17 i=lft,llt
   17 qh(i)=tsarry(i)*qhd
      else
      do 18 i=lft,llt
   18 qh(i)=ym(i)*qh(i)/(ym(i)-qh(i))
      endif
      facnt=-1.0/(1.-2.*pr)
      facnu= 1.0/(1.+pr)
      facnv=pr/((1.0+pr)*(1.0-2.0*pr))
      do 20 i=lft,llt
      blk(i)=ym(i)*facnt
      g(i)  =ym(i)*facnu
      wq1(i)=ym(i)*facnv
      wq2(i)=.5*g(i)
      wq3(i)=wq1(i)+2.0*wq2(i)
      wq4(i)=capa*wq2(i)
      wq3(i)=1./wq3(i)
      ak(i)  =qs(i)+qh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
      ebar(i)=ym(i)
      eyld(i)=qs(i)
      etanmd(i)=qh(i)
   20 continue
      do 40 i=lft,llt
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t4(i)=da4(i)
      t5(i)=da5(i)
      t6(i)=da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      blkg(i)=third*(blk(i)+g(i))
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 48 i=lft,llt
      aj3(i)=sqrt(aj2(i))
      fail(i)=fail(i)*(.5+sign(.5,fs(i)-aj3(i)))
      sig1(i)=fail(i)*sig1(i)
      sig2(i)=fail(i)*sig2(i)
      sig3(i)=fail(i)*sig3(i)
      sig4(i)=fail(i)*sig4(i)
      sig5(i)=fail(i)*sig5(i)
      sig6(i)=fail(i)*sig6(i)
      failu(i)=max(fail(i),failu(i))
   48 continue
      return
      endif
      do 47 i=lft,llt
      fac1(i)  =1.5*g(i)
      fac1qh(i)=1./(fac1(i)+qh(i))
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      deps(i)=0.0
   47 continue
      do 60 i=lft,llt
      if (scle(i).eq.0.0) go to 60
      ebar(i)=qh(i)
      do 50 iter=1,20
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      aj3(i) =sqrt(aj2(i))
      aj1(i) =aj3(i)
      depi(i)=(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      epsd=epsnew(i)-epslst(i)
      d3(i)=(epslst(i)-sg3lst(i)*epsd/demn)
      epsd=epsnew(i)-d3(i)
      if(abs(epsd)/(abs(d3(i))+1.e-07).lt..0001) go to 60
   50 continue
   60 continue
      do 70 i=lft,llt
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i)-deps(i)*t4(i)
      sig5(i)=da5(i)-deps(i)*t5(i)
      sig6(i)=da6(i)-deps(i)*t6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
      if (l1.eq.0.and.tf.eq.0.0) return
      do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs(i)-aj3(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
c
      return
      end
      subroutine gtp19 (p,q,r,s,temp,numlp,v,w,x,y,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/double/iprec,ncpw,unit
      dimension n(128),p(2,*),q(2,*),r(2,*),s(2,*),v(*),w(*),
     & x(*),y(*),temp(*),factor(128)
      data tol/.000001/
c
      do 4 i=lft,llt
      n(i)=numlp
    4 continue
      do 20 m=2,numlp
      mm=numlp-m+2
      do 6 i=lft,llt
    6 factor(i)=temp(i)-p(1,mm)
      do 10 i=lft,llt
      fac0=nint(.5-sign(.5*unit,factor(i)))
      fac1=nint(1.-fac0)
      n(i)=n(i)*fac1+mm*fac0
   10 continue
   20 continue
      do 24 i=lft,llt
      ratio=(temp(i)-p(1,n(i)-1))/(p(1,n(i))-p(1,n(i)-1))
      v(i)=p(2,n(i)-1)+ratio*(p(2,n(i))-p(2,n(i)-1))
      w(i)=q(2,n(i)-1)+ratio*(q(2,n(i))-q(2,n(i)-1))
      x(i)=r(2,n(i)-1)+ratio*(r(2,n(i))-r(2,n(i)-1))
      y(i)=s(2,n(i)-1)+ratio*(s(2,n(i))-s(2,n(i)-1))
   24 continue
      return
      end
      subroutine shl21s (cm,capa,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),c11(128),c12(128),c13(128),c14(128),
     3  c22(128),c23(128),c24(128),c33(128),c34(128),c44(128),
     4  c55(128),c56(128),c66(128),tref(128),otmp(128),ctmp(128),
     5  t11(128),t12(128),t14(128),epsr(128),epss(128),epst(128),
     6 deltem(128),e1(128),e2(128),e4(128),e5(128),e6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      dimension  cm(*),tnew(*),fval(*),d(6,6)
      k=48*(mxt(lft)-1)+13
      do 10 i=1,6
      do 10 j=i,6
      d(i,j)=cm(k)
      d(j,i)=cm(k)
   10 k=k+1
      sndspd=  max(d(1,1),d(2,2),d(3,3))
      ymod   =sndspd
      gmod   =  max(d(4,4),capa*d(5,5),capa*d(6,6))
      if (itemp.ge.0) then
      do 20 i=lft,llt
   20 ctmp(i)=fval(itemp)
      else
      do 30 i=lft,llt
      ctmp(i)=.25*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i)))
   30 continue
      endif
      k=48*(mxt(lft)-1)+46
      ar=cm(k)
      as=cm(k+1)
      at=cm(k+2)
      do 40 i=lft,llt
      ebar(i)=sndspd
      eyld(i)=-1.0
      etanmd(i)=sndspd
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
      deltem(i)=ctmp(i)-otmp(i)
      epsr(i)=ar*deltem(i)
      epss(i)=as*deltem(i)
      epst(i)=at*deltem(i)
      e1(i)=d1(i)-t11(i)*epsr(i)-t12(i)*epss(i)
      e2(i)=d2(i)-t12(i)*epsr(i)-t11(i)*epss(i)
      e4(i)=d4(i)+2.*t14(i)*(epss(i)-epsr(i))
      otmp(i)=ctmp(i)
   40 continue
      do 50 i=lft,llt
      cb(i)=  max(c11(i),c22(i),c33(i))
      d3(i)=-(c13(i)*e1(i)+c23(i)*e2(i)+c34(i)*e4(i))/c33(i)
      sig1(i)=sig1(i)+c11(i)*e1(i)+c12(i)*e2(i)+c13(i)*d3(i)+c14(i)*e4
     1 (i)
      sig2(i)=sig2(i)+c12(i)*e1(i)+c22(i)*e2(i)+c23(i)*d3(i)+c24(i)*e4
     1 (i)
      sig3(i)=0.0
      sig4(i)=sig4(i)+c14(i)*e1(i)+c24(i)*e2(i)+c34(i)*d3(i)+c44(i)*e4
     1 (i)
      sig5(i)=sig5(i)+capa*(c55(i)*d5(i)+c56(i)*d6(i))
      sig6(i)=sig6(i)+capa*(c56(i)*d5(i)+c66(i)*d6(i))
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   50 continue
      return
      end
      subroutine shl22s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/e1(128),e2(128),d3(128),e4(128),e5(128),e6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),
     &sig5(128),sig6(128),ef(128),em(128),ed(128),q1(128),
     &q2(128),efail(128),ef2(128),em2(128),ed2(128),c11(128),
     &c12(128),c13(128),c22(128),c23(128),c33(128),
     &c44(128),c55(128),c66(128),prx(128),pry(128),ex(128),
     & ey(128),gxy(128),pxy(128),sg42(128),sg44(128),eg(128),
     &eh(128),stg1(128),stg2(128),stg4(128),sg22(128),si(128),
     &a11(128),a12(128),a21(128),a22(128),d1(128),d2(128),
     &d4(128),d5(128),d6(128),stg5(128),stg6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),fail(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      dimension  cm(1),d(6,6),qq1(128),qq2(128)
      data zero,half /0.0,0.5/
      mx=48*(mxt(lft)-1)
      sndspd=  max(sndspd,cm(mx+21))
      gmod  =  max(cm(mx+7),cm(mx+8),cm(mx+9))
      ymx =cm(mx+1)
      ymy =cm(mx+2)
      xnuy =cm(mx+4)
      xnux =xnuy*ymx/ymy
      sxy =cm(mx+7)
      syz =capa*cm(mx+8)
      szx =capa*cm(mx+9)
      sc  =cm(mx+26)
      sc2 =sc*sc
      xt  =cm(mx+27)
      yt  =cm(mx+28)
      yc  =cm(mx+29)
      xt2 =1./(xt*xt)
      yt2 =1./(yt*yt)
      sd2 =.25/sc2
      ycsc=sd2*yc*yc
      alp =cm(mx+30)
      d13 =cm(mx+22)
      d23 =cm(mx+23)
      ap3 =3.0*alp
      ap34=.75*alp
      od2g=.5/sxy
      xmlt1=1./(sc2*(od2g+ap34*sc2))
      xmlt2=(yc*yc*sd2-1.)/yc
      ymod=  min(ymx,ymy)
      if (ipt.eq.1) then
      do 10 i=lft,llt
      fail(i)=0.0
   10 continue
      endif
      do 20 i=lft,llt
      ebar(i)=ymod
      eyld(i)=-1.0
      etanmd(i)=ymod
      stg5(i)=sig5(i)
      stg6(i)=sig6(i)
      a11(i) =q1(i)*sig1(i)-q2(i)*sig4(i)
      a12(i) =q2(i)*sig1(i)+q1(i)*sig4(i)
      a21(i) =q1(i)*sig4(i)-q2(i)*sig2(i)
      a22(i) =q2(i)*sig4(i)+q1(i)*sig2(i)
      sig1(i)=q1(i)*a11(i)-q2(i)*a21(i)
      sig2(i)=q2(i)*a12(i)+q1(i)*a22(i)
      sig4(i)=q1(i)*a12(i)-q2(i)*a22(i)
      sig5(i)=q2(i)*stg6(i)+q1(i)*stg5(i)
      sig6(i)=q1(i)*stg6(i)-q2(i)*stg5(i)
   20 continue
      do 30 i=lft,llt
      d4(i)  =.5*e4(i)
      a11(i) =q1(i)*e1(i)-q2(i)*d4(i)
      a12(i) =q2(i)*e1(i)+q1(i)*d4(i)
      a21(i) =q1(i)*d4(i)-q2(i)*e2(i)
      a22(i) =q2(i)*d4(i)+q1(i)*e2(i)
      d1(i)  =q1(i)*a11(i)-q2(i)*a21(i)
      d2(i)  =q2(i)*a12(i)+q1(i)*a22(i)
      d4(i)  =2.*(q1(i)*a12(i)-q2(i)*a22(i))
      d5(i)  =q2(i)*e6(i)+q1(i)*e5(i)
      d6(i)  =q1(i)*e6(i)-q2(i)*e5(i)
   30 continue
      do 40 i=lft,llt
      eg(i) =em(i)*ef(i)
      eh(i) =eg(i)*ed(i)
      ex(i) =ef(i)*ymx
      ey(i) =eh(i)*ymy
      prx(i)=eh(i)*xnux
      pry(i)=eh(i)*xnuy
      gxy(i)=1.e-17+eg(i)*sxy
      pxy(i)=1.0/(1.-prx(i)*pry(i))
      c11(i)=pxy(i)*ex(i)
      c12(i)=pxy(i)*prx(i)*ey(i)
      c22(i)=pxy(i)*ey(i)
      c44(i)=1.0/(1.0/gxy(i)+ap3*sig4(i)*sig4(i))
      d3(i) =-(d13*d1(i)+d23*d2(i))
   40 continue
      do 50 i=lft,llt
      stg1(i)=sig1(i)+c11(i)*d1(i)+c12(i)*d2(i)
      stg2(i)=sig2(i)+c12(i)*d1(i)+c22(i)*d2(i)
      stg4(i)=sig4(i)+c44(i)*d4(i)
      sg42(i)=stg4(i)*stg4(i)
      sg44(i)=xmlt1*sg42(i)*(od2g+ap34*sg42(i))
      if (stg1(i).gt.0.) then
      ef2(i) =xt2*  max(zero,stg1(i))**2+sg44(i)-1.0
      else
      ef2(i)=-1.
      endif
      if (stg2(i).gt.0.) then
      em2(i) =yt2*  max(zero,stg2(i))**2+sg44(i)-1.0
      ed2(i)=-1.
      else
      em2(i)=-1.
      ed2(i) =sd2*  min(zero,stg2(i))**2+xmlt2*stg2(i)+sg44(i)-1.0
      endif
      ef(i)  =ef(i)*(.5-sign(half,ef2(i)))
      em(i)  =em(i)*(.5-sign(half,em2(i)))
      ed(i)  =ed(i)*(.5-sign(half,ed2(i)))
   50 continue
      do 60 i=lft,llt
      efail(i)=efail(i)-.01*(1.-ef(i))
      efail(i)=  max(zero,efail(i))
      fail(i) =  max(fail(i),(.5+sign(half,efail(i)-.01)))
      eg(i) =em(i)*ef(i)
      eh(i) =eg(i)*ed(i)
      ex(i) =ef(i)*ymx
      ey(i) =eh(i)*ymy
      prx(i)=eh(i)*xnux
      pry(i)=eh(i)*xnuy
      gxy(i)=1.e-17+eg(i)*sxy
      pxy(i)=1.0/(1.-prx(i)*pry(i))
      c11(i)=pxy(i)*ex(i)
      c12(i)=pxy(i)*prx(i)*ey(i)
      c22(i)=pxy(i)*ey(i)
      c44(i)=1.0/(1.0/gxy(i)+0.0*sig4(i)*sig4(i))
      sig1(i)=sig1(i)+c11(i)*d1(i)+c12(i)*d2(i)
      sig2(i)=sig2(i)+c12(i)*d1(i)+c22(i)*d2(i)
      sig3(i)=0.0
      sig4(i)=sig4(i)+c44(i)*d4(i)
      sig5(i)=sig5(i)+syz*d5(i)
      sig6(i)=sig6(i)+szx*d6(i)
   60 continue
      do 70 i=lft,llt
      stg5(i)=sig5(i)
      stg6(i)=sig6(i)
      a11(i) = sig1(i)*q1(i)+sig4(i)*q2(i)
      a12(i) =-sig1(i)*q2(i)+sig4(i)*q1(i)
      a21(i) = sig4(i)*q1(i)+sig2(i)*q2(i)
      a22(i) =-sig4(i)*q2(i)+sig2(i)*q1(i)
      qq1(i) =efail(i)*q1(i)
      qq2(i) =efail(i)*q2(i)
      sig1(i)= qq1(i)*a11(i)+qq2(i)*a21(i)
      sig2(i)=-qq2(i)*a12(i)+qq1(i)*a22(i)
      sig4(i)= qq1(i)*a12(i)+qq2(i)*a22(i)
      sig5(i)=-qq2(i)*stg6(i)+qq1(i)*stg5(i)
      sig6(i)= qq1(i)*stg6(i)+qq2(i)*stg5(i)
   70 continue
      return
      end
      subroutine shl23s (cm,capa,tnew,fval,thrmpr,npc,plc,x)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/e1(128),e2(128),d3(128),e4(128),e5(128),e6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),
     &sig5(128),sig6(128),otmp(128),q1(128),q2(128),
     &ctmp(128),d1t(128),d2t(128),d3t(128),deltem(128),c22(128),
     &s11o(128),s12o(128),s13o(128),s22o(128),s23o(128),s33o(128),
     &s11n(128),s12n(128),s13n(128),s22n(128),s23n(128),s33n(128),
     &s44o(128),s55o(128),s66o(128),s44n(128),s55n(128),s66n(128),
     &c11o(128),c12o(128),c13o(128),c22o(128),c23o(128),c33o(128),
     &c11n(128),c12n(128),c13n(128),c22n(128),c23n(128),c33n(128),
     &c44o(128),c55o(128),c66o(128),c44n(128),c55n(128),c66n(128),
     &alao(128),albo(128),alco(128),alan(128),albn(128),alcn(128),
     &g1(128),g2(128),g3(128),g4(128),g5(128),g6(128),
     &h1(128),h2(128),h3(128),h4(128),h5(128),h6(128),
     &a11(128),a12(128),a21(128),a22(128),d1(128),d2(128),
     &d4(128),d5(128),d6(128),stg5(128),stg6(128),scale(128),
     &dist(128),value(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/double/iprec,ncpw,unit
      dimension  cm(*),d(6,6),thrmpr(48,*),thrmo(21),thrmn(21),
     1 fval(*),npc(*),plc(*),x(3,*)
      mx=48*(mxt(lft)-1)
      sndspd=cm(mx+21)
      ymod=sndspd
      gmod=  max(thrmpr(1,11),thrmpr(1,12),thrmpr(1,13))
      numpts=nint(cm(mx+1))
      numlcr=nint(cm(mx+2))
      numlcp=nint(cm(mx+3))
      xcentr=cm(mx+4)
      ycentr=cm(mx+5)
      zcentr=cm(mx+6)
      do 10 i=lft,llt
      ebar(i)=ymod
      eyld(i)=-1.0
      etanmd(i)=ymod
      einc(i)=e1(i)*sig1(i)+e2(i)*sig2(i)+e4(i)*sig4(i)+e5(i)*sig5(i)
     1       +e6(i)*sig6(i)
      a11(i) =q1(i)*sig1(i)-q2(i)*sig4(i)
      a12(i) =q2(i)*sig1(i)+q1(i)*sig4(i)
      a21(i) =q1(i)*sig4(i)-q2(i)*sig2(i)
      a22(i) =q2(i)*sig4(i)+q1(i)*sig2(i)
      sig1(i)=q1(i)*a11(i)-q2(i)*a21(i)
      sig2(i)=q2(i)*a12(i)+q1(i)*a22(i)
      sig4(i)=q1(i)*a12(i)-q2(i)*a22(i)
      stg5(i)=sig5(i)
      stg6(i)=sig6(i)
      sig5(i)=q2(i)*stg6(i)+q1(i)*stg5(i)
      sig6(i)=q1(i)*stg6(i)-q2(i)*stg5(i)
   10 continue
      do 20 i=lft,llt
      d4(i)  =.5*e4(i)
      a11(i) =q1(i)*e1(i)-q2(i)*d4(i)
      a12(i) =q2(i)*e1(i)+q1(i)*d4(i)
      a21(i) =q1(i)*d4(i)-q2(i)*e2(i)
      a22(i) =q2(i)*d4(i)+q1(i)*e2(i)
      d1(i)  =q1(i)*a11(i)-q2(i)*a21(i)
      d2(i)  =q2(i)*a12(i)+q1(i)*a22(i)
      d4(i)  =2.*(q1(i)*a12(i)-q2(i)*a22(i))
      d5(i)  =q2(i)*e6(i)+q1(i)*e5(i)
      d6(i)  =q1(i)*e6(i)-q2(i)*e5(i)
   20 continue
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ymod*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
      if (itemp.gt.0) then
      do 30 i=lft,llt
   30 ctmp(i)=fval(itemp)
      if (numlcr.gt.0) then
      radius =fval(numlcr)+1.e-07
      radsqr =radius*radius
      do 32 i=lft,llt
      xcec=.25*(x(1,ix1(i))+x(1,ix2(i))+x(1,ix3(i))+x(1,ix4(i)))
      ycec=.25*(x(2,ix1(i))+x(2,ix2(i))+x(2,ix3(i))+x(2,ix4(i)))
      zcec=.25*(x(3,ix1(i))+x(3,ix2(i))+x(3,ix3(i))+x(3,ix4(i)))
      dist(i)=(xcec-xcentr)**2+(ycec-ycentr)**2+(zcec-zcentr)**2
      scale(i)=.5+sign(.5*unit,radsqr-dist(i))
      ctmp (i)=scale(i)*ctmp(i)
   32 continue
      do 33 i=lft,llt
      dist(i)=  min(sqrt(dist(i))/radius,.999999*unit)
   33 continue
      i2= npc(numlcp)
      n1=(npc(numlcp+1)-i2)/2
      call gtpcve(plc(i2),dist,n1,value,lft,llt)
      do 34 i=lft,llt
      ctmp(i)=value(i)*ctmp(i)
   34 continue
      endif
      else
      do 40 i=lft,llt
      ctmp(i)=.25*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i)))
   40 continue
      endif
      do 60 i=lft,llt
      call itrpo (thrmpr,otmp(i),thrmpr(1,2),thrmo,numpts)
      s11o(i)=1./thrmo(1)
      s22o(i)=1./thrmo(2)
      s33o(i)=1./thrmo(3)
      s44o(i)=1./thrmo(10)
      s55o(i)=1./thrmo(11)
      s66o(i)=1./thrmo(12)
      s12o(i)=-thrmo(4)*s22o(i)
      s13o(i)=-thrmo(5)*s33o(i)
      s23o(i)=-thrmo(6)*s33o(i)
      alao(i)=thrmo(7)
      albo(i)=thrmo(8)
      alco(i)=thrmo(9)
      call itrpo (thrmpr,ctmp(i),thrmpr(1,2),thrmn,numpts)
      s11n(i)=1./thrmn(1)
      s22n(i)=1./thrmn(2)
      s33n(i)=1./thrmn(3)
      s44n(i)=1./thrmn(10)
      s55n(i)=1./thrmn(11)
      s66n(i)=1./thrmn(12)
      s12n(i)=-thrmn(4)*s22n(i)
      s13n(i)=-thrmn(5)*s33n(i)
      s23n(i)=-thrmn(6)*s33n(i)
      alan(i)=thrmn(7)
      albn(i)=thrmn(8)
      alcn(i)=thrmn(9)
   60 continue
      do 70 i=lft,llt
      deltem(i)=ctmp(i)-otmp(i)
      s1122=s11o(i)*s22o(i)
      s1123=s11o(i)*s23o(i)
      s2213=s22o(i)*s13o(i)
      s1212=s12o(i)*s12o(i)
      s1223=s12o(i)*s23o(i)
      si=1./(s1122*s33o(i)-s1123*s23o(i)-s2213*s13o(i)-
     1  s33o(i)*s1212+2.*s1223*s13o(i))
      c11o(i)=si*(s22o(i)*s33o(i)-s23o(i)*s23o(i))
      c12o(i)=si*(s13o(i)*s23o(i)-s33o(i)*s12o(i))
      c22o(i)=si*(s33o(i)*s11o(i)-s13o(i)*s13o(i))
      c23o(i)=si*(s12o(i)*s13o(i)-s1123)
      c33o(i)=si*(s1122-s1212)
      c13o(i)=si*(s1223-s2213)
      c44o(i)=1./s44o(i)
      c55o(i)=1./s55o(i)
      c66o(i)=1./s66o(i)
   70 continue
      do 80 i=lft,llt
      s1122=s11n(i)*s22n(i)
      s1123=s11n(i)*s23n(i)
      s2213=s22n(i)*s13n(i)
      s1212=s12n(i)*s12n(i)
      s1223=s12n(i)*s23n(i)
      si=1./(s1122*s33n(i)-s1123*s23n(i)-s2213*s13n(i)-
     1  s33n(i)*s1212+2.*s1223*s13n(i))
      c11n(i)=si*(s22n(i)*s33n(i)-s23n(i)*s23n(i))
      c12n(i)=si*(s13n(i)*s23n(i)-s33n(i)*s12n(i))
      c22n(i)=si*(s33n(i)*s11n(i)-s13n(i)*s13n(i))
      c23n(i)=si*(s12n(i)*s13n(i)-s1123)
      c33n(i)=si*(s1122-s1212)
      c13n(i)=si*(s1223-s2213)
      c44n(i)=1./s44n(i)
      c55n(i)=1./s55n(i)
      c66n(i)=1./s66n(i)
   80 continue
      do 90 i=lft,llt
      s11m=.50*(s11o(i)+s11n(i))
      s22m=.50*(s22o(i)+s22n(i))
      s33m=.50*(s33o(i)+s33n(i))
      s44m=.50*(s44o(i)+s44n(i))
      s55m=.50*(s55o(i)+s55n(i))
      s66m=.50*(s66o(i)+s66n(i))
      s12m=.50*(s12o(i)+s12n(i))
      s13m=.50*(s13o(i)+s13n(i))
      s23m=.50*(s23o(i)+s23n(i))
      h1(i)=s11m*sig1(i)+s12m*sig2(i)+s13m*sig3(i)
      h2(i)=s22m*sig2(i)+s12m*sig1(i)+s23m*sig3(i)
      h3(i)=s33m*sig3(i)+s13m*sig1(i)+s23m*sig2(i)
      h4(i)=sig4(i)*s44m
      h5(i)=sig5(i)*s55m
   90 h6(i)=sig6(i)*s66m
c
      do 100 i=lft,llt
      d11t=c11n(i)-c11o(i)
      d12t=c12n(i)-c12o(i)
      d22t=c22n(i)-c22o(i)
      d23t=c23n(i)-c23o(i)
      d33t=c33n(i)-c33o(i)
      d13t=c13n(i)-c13o(i)
      d44t=c44n(i)-c44o(i)
      d55t=c55n(i)-c55o(i)
      d66t=c66n(i)-c66o(i)
      g1(i)=d11t*h1(i)+d12t*h2(i)+d13t*h3(i)
      g2(i)=d22t*h2(i)+d12t*h1(i)+d23t*h3(i)
      g3(i)=d33t*h3(i)+d23t*h2(i)+d13t*h1(i)
      g4(i)=d44t*h4(i)
      g5(i)=d55t*h5(i)
  100 g6(i)=d66t*h6(i)
      do 110 i=lft,llt
      d1(i)=d1(i)-.5*(alao(i)+alan(i))*deltem(i)
      d2(i)=d2(i)-.5*(albo(i)+albn(i))*deltem(i)
      c11n(i)=.50*(c11n(i)+c11o(i))
      c12n(i)=.50*(c12n(i)+c12o(i))
      c22n(i)=.50*(c22n(i)+c22o(i))
      c23n(i)=.50*(c23n(i)+c23o(i))
      c33n(i)=.50*(c33n(i)+c33o(i))
      c13n(i)=.50*(c13n(i)+c13o(i))
      c44n(i)=.50*(c44n(i)+c44o(i))
      c55n(i)=.50*(c55n(i)+c55o(i))
      c66n(i)=.50*(c66n(i)+c66o(i))
      d3(i)=-(c13n(i)*d1(i)+c23n(i)*d2(i)+g3(i))/c33n(i)
      sig1(i)=sig1(i)+c11n(i)*d1(i)+c12n(i)*d2(i)+c13n(i)*d3(i)+g1(i)
      sig2(i)=sig2(i)+c12n(i)*d1(i)+c22n(i)*d2(i)+c23n(i)*d3(i)+g2(i)
      sig3(i)=0.
      sig4(i)=sig4(i)+g4(i)+c44n(i)*d4(i)
      sig5(i)=sig5(i)+g5(i)+c55n(i)*d5(i)
  110 sig6(i)=sig6(i)+g6(i)+c66n(i)*d6(i)
      do 160 i=lft,llt
      a11(i) = sig1(i)*q1(i)+sig4(i)*q2(i)
      a12(i) =-sig1(i)*q2(i)+sig4(i)*q1(i)
      a21(i) = sig4(i)*q1(i)+sig2(i)*q2(i)
      a22(i) =-sig4(i)*q2(i)+sig2(i)*q1(i)
      sig1(i)= q1(i)*a11(i)+q2(i)*a21(i)
      sig2(i)=-q2(i)*a12(i)+q1(i)*a22(i)
      sig4(i)= q1(i)*a12(i)+q2(i)*a22(i)
      stg5(i)=sig5(i)
      stg6(i)=sig6(i)
      sig5(i)=-q2(i)*stg6(i)+q1(i)*stg5(i)
      sig6(i)= q1(i)*stg6(i)+q2(i)*stg5(i)
      otmp(i)=ctmp(i)
      einc(i)=e1(i)*sig1(i)+e2(i)*sig2(i)+e4(i)*sig4(i)+e5(i)*sig5(i)
     1       +e6(i)*sig6(i)+einc(i)
  160 continue
      return
      end
      subroutine itrpo(t,temp,cm,thrml,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension t(1),cm(48,1),thrml(1)
      do 10 l=2,n
      m=l-1
      if (temp.gt.t(l)) go to 10
      ratio=(temp-t(m))/(t(l)-t(m))
      go to 30
   10 continue
      m=m+1
   20 ratio=(temp-t(m-1))/(t(m)-t(m-1))
      m=m-1
   30 do 40 l=1,12
   40 thrml(l)=cm(m,l)+ratio*(cm(m+1,l)-cm(m,l))
      if (ratio.lt.1.05.and.ratio.gt.-.05) return
      write(13,50)
      write  ( *,50)
      call adios (2)
c
   50 format(/' temperatures out-of-range')
      end
      subroutine shl24s (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with piecewise linear isotropic and
c     failure based on plastic strain and dt
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),fac1qh(128),qhs(128),
     6 ak(128),epslst(128),epsnew(128),sg3new(128),sg3lst(128),
     7 value(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),effs(128),
     4 d1d(128),d2d(128),d3d(128),dtflp(128),dtflm(128),
     5 aux(128,38),fac1(128),sg1a(128),sg2a(128),
     5 g(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),wq5(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/tsarry/tslimt,tsarry(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  fail(128),plc(1),npc(1),cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qt=cm(mx+4)
      fs=cm(mx+5)
      tf=cm(mx+47)
      i1=cm(mx+48)
      qs=cm(mx+3)
      pr=cm(mx+2)
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      qhrd=ymod*qt/(ymod-qt)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*qhrd
      g(i)   =fac4*ym(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
      wq5(i) =-wq1(i)*wq3(i)
      ebar(i)=ym(i)
      etanmd(i)=qh(i)
    5 continue
      if (fs.eq.0.0) fs=10.e+20
      fsp =fs+.0001
c
c     compute stress increment and add to rotated stresses
c
      if (ipt.eq.1) then
      do 10 i=lft,llt
      failu(i)=0.0
   10 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(sndspd*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 14 i=lft,llt
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   14 continue
      endif
c
      endif
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      do 20 i=lft,llt
      qhs(i)=qh(i)
      cb(i)=sndspd
      d1d2(i)=d1(i)+d2(i)
      fail(i)=1.0
      effs(i)=1.0
      d3(i)=d1d2(i)*wq5(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t456(i)=3.*(da4(i)*da4(i)+da5(i)*da5(i)+da6(i)*da6(i))
      ak(i)  =qs+qh(i)*ep(i)
      value(i)=1.0
   20 continue
c
c     variable hardening moduli
c
      if (cm(mx+7).ne.0.0) then
      call vyield (cm(mx+6),cm(mx+14),ep,ak,qhs)
      do 15 i=lft,llt
      qhs(i)=tsarry(i)*qhs(i)
      etanmd(i)=qhs(i)
   15 continue
      endif
c
      if (i1.ne.0) then
      i2= npc(i1)
      n1=(npc(i1+1)-i2)/2
      do 30 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   30 continue
      call gtpcve(plc(i2),effs,n1,value,lft,llt)
      endif
c
      do 40 i=lft,llt
      blkg(i)  =third*(blk(i)+g(i))
      ak(i)=value(i)*ak(i)
      eyld(i)=ak(i)
      aks(i)=ak(i)*ak(i)
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=(.5+sign(.5*unit,ak2(i)))*(.5+sign(.5*unit,fs-ep(i)))
      ebar(i)=qhs(i)*scle(i)+ym(i)*(1.-scle(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 80
      endif
      do 50 i=lft,llt
      fac1qh(i)=1./(fac1(i)+qhs(i))
      fac2qh(i)=fac1(i)*fac1qh(i)
      sg3new(i)=da3(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
   50 continue
      do 60 i=lft,llt
      deps(i)=scle(i)*(aj1(i)-ak(i))*fac2qh(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
      deps(i)=scle(i)*(aj1(i)-ak(i))*fac2qh(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      d3(i)=scle(i)*(epslst(i)-sg3lst(i)*(epsnew(i)-epslst(i))/demn)
     &     +d3(i)*scl1(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
   60 continue
      do 70 i=lft,llt
      sig1(i)=da1(i) -deps(i)*t1(i)
      sig2(i)=da2(i) -deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*da4(i)
      sig5(i)=da5(i) -deps(i)*da5(i)
      sig6(i)=da6(i) -deps(i)*da6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   80 do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs-ep(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      ep(i)   =fail(i)*ep(i)+(1.-fail(i))*(fs+.000001)
      einc(i) =fail(i)*einc(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
      return
      end
      subroutine sh24sc (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with piecewise linear isotropic and
c     failure based on plastic strain and dt
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),fac1qh(128),qhs(128),
     6 ak(128),epslst(128),epsnew(128),sg3new(128),sg3lst(128),
     7 value(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),effs(128),
     4 d1d(128),d2d(128),d3d(128),dtflp(128),dtflm(128),
     5 aux(128,38),fac1(128),sg1a(128),sg2a(128),
     5 g(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),wq5(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/tsarry/tslimt,tsarry(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/failu/sieu(128),failu(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  fail(128),plc(1),npc(1),cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qt=cm(mx+4)
      fs=cm(mx+5)
      tf=cm(mx+47)
      i1=cm(mx+48)
      qs=cm(mx+3)
      pr=cm(mx+2)
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      qhrd=ymod*qt/(ymod-qt)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*qhrd
      g(i)   =fac4*ym(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
      wq5(i) =-wq1(i)*wq3(i)
      ebar(i)=ym(i)
      etanmd(i)=qh(i)
    5 continue
      if (fs.eq.0.0) fs=10.e+20
      fsp =fs+.0001
c
c     compute stress increment and add to rotated stresses
c
      if (ipt.eq.1) then
      do 10 i=lft,llt
      failu(i)=0.0
   10 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(sndspd*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 14 i=lft,llt
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   14 continue
      endif
c
      endif
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      do 20 i=lft,llt
      qhs(i)=qh(i)
      cb(i)=sndspd
      d1d2(i)=d1(i)+d2(i)
      fail(i)=1.0
      effs(i)=1.0
      d3(i)=d1d2(i)*wq5(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t456(i)=3.*(da4(i)*da4(i)+da5(i)*da5(i)+da6(i)*da6(i))
      ak(i)  =qs+qh(i)*ep(i)
      value(i)=1.0
   20 continue
c
c     variable hardening moduli
c
      if (cm(mx+7).ne.0.0) then
      call vyield (cm(mx+6),cm(mx+14),ep,ak,qhs)
      endif
c
      if (i1.ne.0) then
      i2= npc(i1)
      n1=(npc(i1+1)-i2)/2
      do 30 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   30 continue
      call gtpcve(plc(i2),effs,n1,value,lft,llt)
      endif
c
      do 40 i=lft,llt
      blkg(i)  =third*(blk(i)+g(i))
      ak(i)=value(i)*ak(i)
      eyld(i)=ak(i)
      etanmd(i)=qhs(i)
      aks(i)=ak(i)*ak(i)
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=(.5+sign(.5*unit,ak2(i)))*(.5+sign(.5*unit,fs-ep(i)))
      ebar(i)=qhs(i)*scle(i)+ym(i)*(1.-scle(i))
      scl1(i)=1.-scle(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 80
      endif
      do 60 i=lft,llt
      fac1qh(i)=1./(fac1(i)+qhs(i))
      fac2qh(i)=fac1(i)*fac1qh(i)
      aj1(i)=sqrt(aj2(i))+scl1(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
   60 continue
      do 70 i=lft,llt
      sig1(i)=da1(i) -deps(i)*(t1(i)-p(i))
      sig2(i)=da2(i) -deps(i)*(t2(i)-p(i))
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*da4(i)
      sig5(i)=da5(i) -deps(i)*da5(i)
      sig6(i)=da6(i) -deps(i)*da6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   80 do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs-ep(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      ep(i)   =fail(i)*ep(i)+(1.-fail(i))*(fs+.000001)
      einc(i) =fail(i)*einc(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
      return
      end
      subroutine sh24si (cm,npc,plc,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with piecewise linear isotropic and
c     failure based on plastic strain and dt
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign1(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),fac1qh(128),qhs(128),
     6 ak(128),epslst(128),epsnew(128),sg3new(128),sg3lst(128),
     7 value(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),epx3(128),effs(128),
     4 d1d(128),d2d(128),d3d(128),dtflp(128),dtflm(128),
     5 aux(128,38),fac1(128),sg1a(128),sg2a(128),
     5 g(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),wq5(128),qh(128),ym(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/tsarry/tslimt,tsarry(128)
      common/failu/sieu(128),failu(128)
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      common/double/iprec,ncpw,unit
      dimension  fail(128),plc(1),npc(1),cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qt=cm(mx+4)
      fs=cm(mx+5)
      tf=cm(mx+47)
      i1=cm(mx+48)
      qs=cm(mx+3)
      pr=cm(mx+2)
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      qhrd=ymod*qt/(ymod-qt)
      sndspd=cm(mx+1)/(1.-pr**2)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*qhrd
      g(i)   =fac4*ym(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
      wq5(i) =-wq1(i)*wq3(i)
      ebar(i)=ym(i)
      etanmd(i)=qh(i)
    5 continue
      if (fs.eq.0.0) fs=10.e+20
      fsp =fs+.0001
c
c     compute stress increment and add to rotated stresses
c
      if (ipt.eq.1) then
      do 10 i=lft,llt
      failu(i)=0.0
   10 continue
c
      if (tf.gt.0.0) then
c
c     if dt for element falls below minimum, tf, then set the
c     effective plastic strain to value that exceeds failure
c     value
c
      snds=sqrt(sndspd*(1./rhoa(lft)))
      do 12 i=lft,llt
      dtck=sarea(i)/(sqrt(diagm(i))*snds)
      dtflp(i)=.5+sign(.5*unit,tf-dtck)
      dtflm(i)=1.-dtflp(i)
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   12 continue
      endif
c
      else
c
      if (tf.gt.0.0) then
      do 14 i=lft,llt
      ep(i)=fsp*dtflp(i)+ep(i)*dtflm(i)
   14 continue
      endif
c
      endif
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      do 20 i=lft,llt
      qhs(i)=qh(i)
      cb(i)=sndspd
      d1d2(i)=d1(i)+d2(i)
      fail(i)=1.0
      effs(i)=1.0
      d3(i)=d1d2(i)*wq5(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      t456(i)=3.*(da4(i)*da4(i)+da5(i)*da5(i)+da6(i)*da6(i))
      ak(i)  =qs+qh(i)*ep(i)
      value(i)=1.0
   20 continue
c
c     variable hardening moduli
c
      if (cm(mx+7).ne.0.0) then
      call vyield (cm(mx+6),cm(mx+14),ep,ak,qhs)
      endif
c
      if (i1.ne.0) then
      i2= npc(i1)
      n1=(npc(i1+1)-i2)/2
      do 30 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i) =dti*sqrt(effs(i))
   30 continue
      call gtpcve(plc(i2),effs,n1,value,lft,llt)
      endif
c
      do 40 i=lft,llt
      blkg(i)  =third*(blk(i)+g(i))
      ak(i)=value(i)*ak(i)
      eyld(i)=ak(i)
      etanmd(i)=qhs(i)
      aks(i)=ak(i)*ak(i)
      sg1a(i)=sig1(i)+g(i)*d1(i)
      sg2a(i)=sig2(i)+g(i)*d2(i)
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-aks(i)
      scle(i)=(.5+sign(.5*unit,ak2(i)))*(.5+sign(.5*unit,fs-ep(i)))
      etanmd(i)=qhs(i)*scle(i)+ym(i)*(1.-scle(i))
      scl1(i)=1.-scle(i)
      depi(i)=0.0
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=da2(i)
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 80
      endif
      do 47 i=lft,llt
      fac1qh(i)=1./(fac1(i)+qhs(i))
      sg3new(i)=da3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      d3(i)=scle(i)*(-d1d2(i))+d3(i)*(1.-scle(i))
      deps(i)=0.0
   47 continue
      do 60 i=lft,llt
      if (scle(i).eq.0.0) go to 60
      do 50 iter=1,20
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=-(t1(i)+t2(i))
      aj1(i)=sqrt(1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i))
      depi(i)=(aj1(i)-ak(i))*fac1qh(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sg3lst(i)=sg3new(i)
      sg3new(i)=da3(i)  -deps(i)*t3(i)
      epslst(i)=epsnew(i)
      epsnew(i)=d3(i)
      demn=1.e-14+(sg3new(i)-sg3lst(i))
      epsd=epsnew(i)-epslst(i)
      d3(i)=(epslst(i)-sg3lst(i)*epsd/demn)
      if(abs(epsd)/(abs(d3(i))+1.e-07).lt..0001) go to 60
   50 continue
   60 continue
      do 70 i=lft,llt
      sig1(i)=da1(i) -deps(i)*t1(i)
      sig2(i)=da2(i) -deps(i)*t2(i)
      sig3(i)=0.0
      sig4(i)=da4(i) -deps(i)*da4(i)
      sig5(i)=da5(i) -deps(i)*da5(i)
      sig6(i)=da6(i) -deps(i)*da6(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   80 do 90 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs-ep(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
      ep(i)   =fail(i)*ep(i)+(1.-fail(i))*(fs+.000001)
      einc(i) =fail(i)*einc(i)
      failu(i)=  max(fail(i),failu(i))
   90 continue
      return
      end
      subroutine gtpcve(p,temp,numlp,v,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/double/iprec,ncpw,unit
      dimension p(2,*),v(*),temp(*),n(128),factor(128)
c
      do 4 i=lft,llt
      n(i)=numlp
    4 continue
      do 20 m=2,numlp
      mm=numlp-m+2
      do 6 i=lft,llt
    6 factor(i)=temp(i)-p(1,mm)
      do 10 i=lft,llt
      ifac0=nint(.5-sign(.5*unit,factor(i)))
      ifac1=nint(1.-ifac0)
      n(i) =n(i)*ifac1+mm*ifac0
   10 continue
   20 continue
      do 24 i=lft,llt
      ratio=(temp(i)-p(1,n(i)-1))/(p(1,n(i))-p(1,n(i)-1))
      v(i)=p(2,n(i)-1)+ratio*(p(2,n(i))-p(2,n(i)-1))
   24 continue
      return
      end
      subroutine vyield(eps,sg,ep,ak,qh)
c     implicit double precision (a-h,o-z)                                    dp
c
c     vectorized subroutine for interpolating tabulated curve
c
      common/aux36/lft,llt
      common/double/iprec,ncpw,unit
      dimension xmult(128,9),epsj1(128),epsj(128),sgj1(128),
     1 sgj(128),factor(128),qhj1(128),qhj(128)
      dimension eps(1),sg(1),ep(1),ak(1),qh(1)
c
      do 10 i=3,8
      n=i-2
      if (eps(i).eq.0.) go to 20
   10 n=n+1
   20 npoint=n+1
c
      do 40 i=lft,llt
      epsj1(i)=0.
      epsj(i)=0.
      sgj1(i)=0.
      sgj(i)=0.
      xmult(i,1)=0.
   40 xmult(i,n+1)=1.
      do 60 j=2,n
      do 50 i=lft,llt
   50 xmult(i,j)=.5+sign(.5*unit,eps(j)-ep(i))
   60 continue
      do 80 j=1,n
      nn=n-j+2
      mm=n-j+1
      do 70 i=lft,llt
   70 xmult(i,nn)=xmult(i,nn)-xmult(i,mm)
   80 continue
      do 100 j=1,n
      do 90 i=lft,llt
      epsj1(i)=epsj1(i)+eps(j)*xmult(i,j+1)
      epsj(i)=epsj(i)+eps(j+1)*xmult(i,j+1)
      sgj1(i)=sgj1(i)+sg(j)*xmult(i,j+1)
      sgj(i)=sgj(i)+sg(j+1)*xmult(i,j+1)
   90 continue
  100 continue
      do 110 i=lft,llt
      qhj(i)=epsj(i)-epsj1(i)
      qhj1(i)=1./qhj(i)
      factor(i)=(ep(i)-epsj1(i))*qhj1(i)
      qhj(i)=sgj(i)-sgj1(i)
      ak(i)=sgj1(i)+qhj(i)*factor(i)
  110 qh(i)=qhj(i)*qhj1(i)
      return
      end
      subroutine shl28s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
      common/aux00/
     & sig1m(128),sig2m(128),sig4m(128),sig1n(128),sig2n(128),
     & sig4n(128),sig5n(128),sig6n(128),sig5l(128),sig6l(128)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     &b1vx(128),b1vy(128),b1vz(128),b2vx(128),b2vy(128),b2vz(128),
     &b1tx(128),b1ty(128),b2tx(128),b2ty(128),bxyv(128),bxyt(128),
     &epyz(128),epzx(128)
      common/aux13/
     &zeta(128),thick(128),fga(128),fgb(128),fgc(128),
     &gl11(128),gl12(128),gl13(128),gl21(128),gl22(128),gl23(128),
     &gl31(128),gl32(128),gl33(128),
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1rsl1(128),rsl2(128),rsl3(128),rsl4(128),
     2rsl5(128),rsl6(128),eps(128),rsl7(128),rsl8(128),
     3rtl1(128),rtl2(128),rtl3(128),rtl4(128),
     4rtl5(128),rtl6(128),rtl7(128),rtl8(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/double/iprec,ncpw,unit
      dimension  cm(*)
c
      mx=48*(mxt(lft)-1)
      qh=cm(mx+26)
      q0=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      fac1qh=qh+1.5*g
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      wq4=capa*wq2
      wq3=1./wq3
      ymod=ym
      gmod=wq4
      sndspt=ym/(1.-pr**2)
      sndspd=  max(sndspt,sndspd)
      a1=ym/(1.-pr**2)
      b1=pr*a1
      c1=.5*g
      ts=capa*c1
      sq3=4./sqrt(3.0)
      tol=1.e-30
c
c     elastic update of resultants
c
      do 10 i=lft,llt
      eyld(i)=q0
      etanmd(i)=qh
      rtl1(i)=rsl1(i)
      rtl2(i)=rsl2(i)
      rtl3(i)=rsl3(i)
      rtl4(i)=rsl4(i)
      rtl5(i)=rsl5(i)
      rtl6(i)=rsl6(i)
      rtl7(i)=rsl7(i)
      rtl8(i)=rsl8(i)
      bfac=thick(i)*thick(i)*thick(i)/12.0
      rsl1(i)=rsl1(i)+thick(i)*(a1*b1vx(i)+b1*b2vy(i))
      rsl2(i)=rsl2(i)+thick(i)*(b1*b1vx(i)+a1*b2vy(i))
      rsl3(i)=rsl3(i)+thick(i)* c1*bxyv(i)
      rsl4(i)=rsl4(i)+thick(i)* ts*epyz(i)
      rsl5(i)=rsl5(i)+thick(i)* ts*epzx(i)
      rsl6(i)=rsl6(i)+bfac*(a1*b1ty(i)-b1*b2tx(i))
      rsl7(i)=rsl7(i)+bfac*(b1*b1ty(i)-a1*b2tx(i))
      rsl8(i)=rsl8(i)+bfac* c1*bxyt(i)
   10 continue
c
c     treat plastic behavior by radial return
c
      if (q0.ne.0) then
      do 20 i=lft,llt
      qs=q0+qh*eps(i)
      xnbar=rsl1(i)*(rsl1(i)-rsl2(i))+rsl2(i)**2+3.*(rsl3(i)**2
     1    +rsl4(i)**2+rsl5(i)**2)
      xmbar=rsl6(i)*(rsl6(i)-rsl7(i))+rsl7(i)**2+3.*rsl8(i)**2
      xmnbr=rsl6(i)*(rsl1(i)-.5*rsl2(i))+rsl7(i)*(rsl2(i)-.5*rsl1(i))
     1         +3.* rsl3(i)*rsl8(i)
      yldr=sqrt(xnbar+sq3*abs(xmnbr)/thick(i)+16.*xmbar/thick(i)**2)+tol
      qsrsl=qs*thick(i)
      sclr =qsrsl-yldr
      cc   =sign(.5*unit,sclr)
      sclr =(.5-cc)*qsrsl/yldr+.5+cc
      ebarmn(i)=(.5-cc)*qh + (.5+cc)*ym
      rsl1(i)=sclr*rsl1(i)
      rsl2(i)=sclr*rsl2(i)
      rsl3(i)=sclr*rsl3(i)
      rsl4(i)=sclr*rsl4(i)
      rsl5(i)=sclr*rsl5(i)
      rsl6(i)=sclr*rsl6(i)
      rsl7(i)=sclr*rsl7(i)
      rsl8(i)=sclr*rsl8(i)
      eps(i)=eps(i)+(1.-sclr)*yldr/(fac1qh*thick(i))
   20 continue
      endif
c
      do 30 i=lft,llt
      sig1n(i)=rsl1(i)
      sig2n(i)=rsl2(i)
      sig4n(i)=rsl3(i)
      sig5n(i)=rsl4(i)
      sig6n(i)=rsl5(i)
      sig1m(i)=rsl6(i)
      sig2m(i)=rsl7(i)
      sig4m(i)=rsl8(i)
      sig5l(i)=-.25*sarea(i)*rsl4(i)
      sig6l(i)= .25*sarea(i)*rsl5(i)
      rtl1(i)=rsl1(i)+rtl1(i)
      rtl2(i)=rsl2(i)+rtl2(i)
      rtl3(i)=rsl3(i)+rtl3(i)
      rtl4(i)=rsl4(i)+rtl4(i)
      rtl5(i)=rsl5(i)+rtl5(i)
      rtl6(i)=rsl6(i)+rtl6(i)
      rtl7(i)=rsl7(i)+rtl7(i)
      rtl8(i)=rsl8(i)+rtl8(i)
      einc(i)=(rtl1(i)*b1vx(i)+rtl2(i)*b2vy(i)+rtl3(i)*bxyv(i)+rtl4(i)
     1*epyz(i)+rtl5(i)*epzx(i)+rtl6(i)*b1ty(i)-rtl7(i)*b2tx(i)
     2        +rtl8(i)*bxyt(i))*.50*sarea(i)
   30 continue
      return
      end
      subroutine shl30s(cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with  kinematic hardening
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 sign11(128),sig33s(128),da4(128),
     2 da5(128),da6(128),t456(128),d1d2(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),sg3old(128),
     7 sg3lst(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),
     3 epx4(128),epx5(128),epx6(128),eps1(128),eps2(128),xlampr(128),
     a epx3(128),aux(128,39),
     b    eta1(128),eta2(128),eta4(128),etat1(128),etat2(128),
     1          etat4(128),phib(128),r(128),
     2          xlamkp1(128),gamht11(128),gamht12(128),
     3          gamht22(128),gamht33(128),
     4          ahat(128),bhat(128),bkfac(128)
      common/aux18/dd(128),def(128)
      common/aux19/
     1 sign0(128),sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
c
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/double/iprec,ncpw,unit
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
      common/shlopt/istrn,istupd,ibelyt,miter
      dimension  cm(1)
c
      data sixth/0.1666666666667/
      data two3/0.6666666666667/
      data sqtwo3/0.8164965809/
      data third/0.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qh=cm(mx+26)
      qb=cm(mx+21)
      qs=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      qbqh=qb*qh
      qa  =1.-qb
      qaqh = qa*qh
      ymod=ym
      gmod=capa*ym/(2*(1.+pr))
      g=ym/(1.+pr)
      blk=-ym/(1.-2.*pr)
      sndspt=.66666666666667*g+third*blk
      sndspt=ym/(1.-pr**2)
      sndspd=max(sndspt,sndspd)
c
      if (qb.lt.0.9999999.and.ibelyt.le.1) then
c
      do 10 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i) =epx1(i)*s11(i)+epx4(i)*s12(i)+epx6(i)*s13(i)
      a12(i) =epx1(i)*s21(i)+epx4(i)*s22(i)+epx6(i)*s23(i)
      a13(i) =epx1(i)*s31(i)+epx4(i)*s32(i)+epx6(i)*s33(i)
      a21(i) =epx4(i)*s11(i)+epx2(i)*s12(i)+epx5(i)*s13(i)
      a22(i) =epx4(i)*s21(i)+epx2(i)*s22(i)+epx5(i)*s23(i)
      a23(i) =epx4(i)*s31(i)+epx2(i)*s32(i)+epx5(i)*s33(i)
      a31(i) =epx6(i)*s11(i)+epx5(i)*s12(i)+epx3(i)*s13(i)
      a32(i) =epx6(i)*s21(i)+epx5(i)*s22(i)+epx3(i)*s23(i)
      a33(i) =epx6(i)*s31(i)+epx5(i)*s32(i)+epx3(i)*s33(i)
   10 continue
      do 20 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s12(i)*a21(i)+s13(i)*a31(i)
      epx2(i)=s21(i)*a12(i)+s22(i)*a22(i)+s23(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i)
      epx5(i)=s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i)
   20 continue
      endif
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
      d11 = ym/(1. - pr**2)
      d12 = pr*d11
      d22 = d11
      d33 = ym/(2.0*(1. + pr))
      do 30 i=lft,llt
      eyld(i)=qs
      etanmd(i)=qh
      sig5(i) = capa*d33*d5(i) + sig5(i)
      sig6(i) = capa*d33*d6(i) + sig6(i)
c
      cb(i)=ss
c
      t1(i) = d11*d1(i) + d12*d2(i) + sig1(i)
      t2(i) = d12*d1(i) + d22*d2(i) + sig2(i)
      t4(i) = d33*d4(i) + sig4(i)
      eta1(i) = t1(i) - epx1(i)
      eta2(i) = t2(i) - epx2(i)
      eta4(i) = t4(i) - epx4(i)
      r(i) = sqrt(2./3.)*(qs + qbqh*ep(i))
      phib(i) = sixth*(eta1(i) + eta2(i))**2 +
     1              0.5*(eta2(i) - eta1(i))**2 + 2.0*eta4(i)*eta4(i)
c
   30 continue
c
      do 34 i=lft,llt
      ak2(i) = phib(i) - r(i)**2
      scle(i) = .50 + sign(0.5*unit,ak2(i))
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)
   34 continue
      scl = 0.
      do 40 i=lft,llt
   40 scl=scl+scle(i)
      if (nint(scl).eq.0.) then
      do 46 i=lft,llt
      ebar(i)=ymod
      sig1(i) = d11*d1(i) + d12*d2(i) + sig1(i)
      sig2(i) = d12*d1(i) + d22*d2(i) + sig2(i)
      sig3(i) = 0.0
      sig4(i) = d33*d4(i) + sig4(i)
      d3(i) = -pr/ym*(sig1(i) + sig2(i)) - (eps1(i) + eps2(i))
c
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   46 continue
      go to 75
      endif
      alfaht = ym/(3.*(1. - pr)) + two3*qaqh
      betaht = ym/(1. + pr) + two3*qaqh
      call lamex3(eta1,eta2,eta4,r,scle,alfaht,betaht,ym,
     1                 lft,llt,xlamkp1)
c
      do 60 i=lft,llt
      ahat(i) = 1./(1. + alfaht*xlamkp1(i))
      bhat(i) = 1./(1. + betaht*xlamkp1(i))
      gamht11(i) = 0.5*( ahat(i) + bhat(i) )
      gamht12(i) = 0.5*( ahat(i) - bhat(i) )
      gamht22(i) = gamht11(i)
      gamht33(i) = bhat(i)
      etat1(i) = gamht11(i)*eta1(i) + gamht12(i)*eta2(i)
      etat2(i) = gamht12(i)*eta1(i) + gamht22(i)*eta2(i)
      etat4(i) = gamht33(i)*eta4(i)
      eta1(i) = etat1(i)
      eta2(i) = etat2(i)
      eta4(i) = etat4(i)
   60 continue
      do 70 i=lft,llt
      ebar(i)=scle(i)*qh + (1.-scle(i))*ymod
      bkfac(i) = two3*xlamkp1(i)*qaqh
      epx1(i) = epx1(i) + bkfac(i)*eta1(i)
      epx2(i) = epx2(i) + bkfac(i)*eta2(i)
      epx4(i) = epx4(i) + bkfac(i)*eta4(i)
      sig1(i) = eta1(i) + epx1(i)
      sig2(i) = eta2(i) + epx2(i)
      sig3(i) = 0.0
      sig4(i) = eta4(i) + epx4(i)
      phib(i) = two3*(eta1(i)**2 - eta1(i)*eta2(i)
     1                + eta2(i)**2) + 2.*eta4(i)**2
      ep(i) = ep(i) + xlamkp1(i)*sqrt(two3*phib(i))
      eps1(i) = eps1(i) + xlamkp1(i)*(two3*eta1(i) - third*eta2(i))
      eps2(i) = eps2(i) + xlamkp1(i)*(-third*eta1(i) + two3*eta2(i))
      d3(i) = -pr/ym*(sig1(i) + sig2(i)) - (eps1(i) + eps2(i))
c
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
c
   75 if (qb.lt.0.9999999.and.ibelyt.le.1) then
      do 80 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i)=epx1(i)*s11(i)+epx4(i)*s21(i)+epx6(i)*s31(i)
      a12(i)=epx1(i)*s12(i)+epx4(i)*s22(i)+epx6(i)*s32(i)
      a13(i)=epx1(i)*s13(i)+epx4(i)*s23(i)+epx6(i)*s33(i)
      a21(i)=epx4(i)*s11(i)+epx2(i)*s21(i)+epx5(i)*s31(i)
      a22(i)=epx4(i)*s12(i)+epx2(i)*s22(i)+epx5(i)*s32(i)
      a23(i)=epx4(i)*s13(i)+epx2(i)*s23(i)+epx5(i)*s33(i)
      a31(i)=epx6(i)*s11(i)+epx5(i)*s21(i)+epx3(i)*s31(i)
      a32(i)=epx6(i)*s12(i)+epx5(i)*s22(i)+epx3(i)*s32(i)
      a33(i)=epx6(i)*s13(i)+epx5(i)*s23(i)+epx3(i)*s33(i)
   80 continue
      do 90 i=lft,llt
      epx1(i)=s11(i)*a11(i)+s21(i)*a21(i)+s31(i)*a31(i)
      epx2(i)=s12(i)*a12(i)+s22(i)*a22(i)+s32(i)*a32(i)
      epx4(i)=s11(i)*a12(i)+s21(i)*a22(i)+s31(i)*a32(i)
      epx5(i)=s12(i)*a13(i)+s22(i)*a23(i)+s32(i)*a33(i)
      epx6(i)=s11(i)*a13(i)+s21(i)*a23(i)+s31(i)*a33(i)
   90 continue
      endif
      return
      end
