      subroutine f3dm33(cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c*********************************
c**       use std. Cep method, small de=.00002
c    thetar(m) = 'beta' angle for element nel=m+nftm1
c  110890, for solids, thetar= angle in a-b plane from '1-axis'
c          of element to 'a-axis' of material.
c          '3-axis' of element is assumed to be 'c-axis' of matl
c
c*********************************
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     * wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     * sign11(128),sig33s(128),da4(128),
     * da5(128),da6(128),t456(128),d1d2(128),da1(128),
     * da2(128),da3(128),deps(128),ak2(128),
     * aj2(128),depn(128),aks(128),scl1(128),scle(128),depi(128),
     * t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     * epalst(128),epanew(128),sg3new(128),sg3old(128),
     * sg3lst(128)
      common/aux14/
     * sig1(128),sig2(128),sig3(128),sig4(128),
     * sig5(128),sig6(128),  ep(128),epx1(128),epx2(128),epx4(128),
     * cc(128),ss(128),cs(128),
     * so1(128),so2(128),so3(128),so4(128),so5(128),so6(128),
     * dd1(128),dd2(128),dd3(128),dd4(128),dd5(128),dd6(128),
     * dp1(128),dp2(128),dp3(128),dp4(128),dp5(128),dp6(128),
     * st1(128),st2(128),st3(128),st4(128),st5(128),st6(128)
      common/aux33/
     * ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),p0(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dx1(128)
      common/hourg/ymod,gmod,ifsv
      common/vect15p/ thetar(128),
     * do1(128),do2(128),do3(128),do4(128),do5(128),do6(128)
c
      common/rwle1/
     * sf(128),del(6,6),ceo(6,6),dpl(6,6),dep(6,6),cep(6,6),dsav(6,6)
      common/rwle2/ du(6),de(6),dp(6),sigs(6),sigr(6),dsig(6),
     *sigt(6),dt(6),dsigs(6),sigo(6),sigy(6),cepn(6,6),qi(6),pi(6),
     *cq(6),cqcq(6,6),dpln(6,6),sigj(6),dpr(6),
     * dplh(6,6),cep0(6,6),cep1(6,6),cpl(6,6)
      common/rwle3/
     * eeo(6),sigc(6),een(6),ce(6,6)
c
      dimension cm(*),bqs(*)
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+10)
c **
c
      qb=1.
      qh=cm(mx+11)
      qs=cm(mx+9)
      qa=1.-qb
      ea =cm(mx+1)
      eb =cm(mx+1)
      ec =cm(mx+1)
      if(eb.eq.0.) eb=ea
      if(ec.eq.0.) eb=ea
      e=(ea+eb+ec)/3.
      r  =cm(mx+4)
      p  =cm(mx+5)
      zz=r/p
      qxy=cm(mx+6)
      qyz=cm(mx+7)
      qzx=cm(mx+8)
      vba=cm(mx+2)
      vca=cm(mx+2)
      vcb=cm(mx+2)
      if(vca.eq.0.) vca=vba
      if(vcb.eq.0.) vcb=vba
      qb =1.
c
c.... why not calc these from elastic relations, e, and pr?
c
      gab=cm(mx+3)
      gbc=cm(mx+3)
      gca=cm(mx+3)
      if(gbc.eq.0.) gbc=gab
      if(gca.eq.0.) gca=gab
      ymod=ea
      if(ymod.lt.eb) ymod=eb
      if(ymod.lt.ec) ymod=ec
      gmod=gab
      if(gmod.lt.gbc) gmod=gbc
      if(gmod.lt.gca) gmod=gca
      vmin=vba
      if(vmin.gt.vca) vmin=vca
      if(vmin.gt.vcb) vmin=vcb
      sndspd=ymod/(1.-vmin*vmin)
c
c.... convert strain rates to increments
c
      do 5 i=lft,llt
      d1(i)=d1(i)*dt1
      d2(i)=d2(i)*dt1
      d3(i)=d3(i)*dt1
      d4(i)=d4(i)*dt1
      d5(i)=d5(i)*dt1
      d6(i)=d6(i)*dt1
      cb(i)=sndspd
    5 continue
c ** 6-90 use dyna notation for ortho-e
      do 7 i=1,6
      do 7 j=1,6
      dpl(i,j)=0.
    7 del(i,j)=0.
      del(1,1)=  1./ea
      del(1,2)=-vba/eb
      del(1,3)=-vca/ec
      del(2,1)=del(1,2)
      del(2,2)=  1./eb
      del(2,3)=-vcb/ec
      del(3,1)=del(1,3)
      del(3,2)=del(2,3)
      del(3,3)=  1./ec
      del(4,4)=  1./gab
      del(5,5)=  1./gbc
      del(6,6)=  1./gca
      do 8 i=1,6
      do 8 j=1,6
    8 ce(i,j)=del(i,j)
c
            call inverr(ce)
c
c** initialise
      etan=qh
      ckb=1./(r+1.)
      hf=zz
      hg=1.
      hh=r
c** hl=2L, hm=2M,hn=2N see 3-12-88, 7-7-87
      hl=(2.*qyz+1.)*( r+1.)
      hm=(2.*qzx+1.)*( r+zz)
      hn=(2.*qxy+1.)*(1.+zz)
c
c** get the plastic flow rule matrix dpl(i,j)
      if(etan/qs.lt.1.0e-10) then
       write(17,*) ' tang mod goes to zero'
       write(59,*) ' tang mod goes to zero'
       call adios(2)
      endif
      ttt=1./(etan*(r+1.))
      dpl(1,1)= ttt*(hg+hh)
      dpl(1,2)=-ttt*hh
      dpl(1,3)=-ttt*hg
      dpl(2,1)=dpl(1,2)
      dpl(2,2)= ttt*(hh+hf)
      dpl(2,3)=-ttt*hf
      dpl(3,1)=dpl(1,3)
      dpl(3,2)=dpl(2,3)
      dpl(3,3)= ttt*(hg+hf)
      dpl(4,4)= ttt*hn
      dpl(5,5)= ttt*hl
      dpl(6,6)= ttt*hm
c
      gg=-dpl(1,3)
      do 102 i=1,6
      do 102 j=1,6
  102 cpl(i,j)=dpl(i,j)
      do 103 i=1,3
      do 103 j=1,3
  103 cpl(i,j)=cpl(i,j)+gg
c
            call inverr(cpl)
c
c** get current yield stress ak(m)
      do 55 m=lft,llt
      ak(m)=qs+qh*ep(m)
c**temp
      thetar(m)=0.
c**temp
   55 continue
c......................................................
      do 10 m=lft,llt
      epx1(m)=0.
      epx2(m)=0.
      epx4(m)=0.
      ct=cos(thetar(m))
      st=sin(thetar(m))
      cc(m)=ct*ct
      ss(m)=st*st
      cs(m)=ct*st
c
      so1(m)=cc(m)*sig1(m)+ss(m)*sig2(m)+2.*cs(m)*sig4(m)
      so2(m)=ss(m)*sig1(m)+cc(m)*sig2(m)-2.*cs(m)*sig4(m)
      so3(m)=sig3(m)
      so4(m)=cs(m)*(sig2(m)-sig1(m))+(cc(m)-ss(m))*sig4(m)
      so5(m)=sig5(m)
      so6(m)=sig6(m)
c
      dd1(m)=cc(m)*d1(m)+ss(m)*d2(m)+cs(m)*d4(m)
      dd2(m)=ss(m)*d1(m)+cc(m)*d2(m)-cs(m)*d4(m)
      dd3(m)=d3(m)
      dd4(m)=2.*cs(m)*(d2(m)-d1(m))+(cc(m)-ss(m))*d4(m)
      dd5(m)=d5(m)
      dd6(m)=d6(m)
   10 continue
      do 100 m=lft,llt
c
      if(m.gt.99999) go to 100
c
      sigo(1)=so1(m)
      sigo(2)=so2(m)
      sigo(3)=so3(m)
      sigo(4)=so4(m)
      sigo(5)=so5(m)
      sigo(6)=so6(m)
c
      dt(1)=dd1(m)
      dt(2)=dd2(m)
      dt(3)=dd3(m)
      dt(4)=dd4(m)
      dt(5)=dd5(m)
      dt(6)=dd6(m)
      do 45 j=1,6
   45 dp(j)=0.
c
      s2old =ckb*(hf*(sigo(2)-sigo(3))**2
     *           +hg*(sigo(3)-sigo(1))**2
     *           +hh*(sigo(1)-sigo(2))**2
     *           +hl*sigo(5)**2+hm*sigo(6)**2+hn*sigo(4)**2)
c
      s1old=sqrt(s2old)
      smold=(sigo(1)+sigo(2)+sigo(3))/3.
      if(s1old.le.ak(m)) go to 182
      ak(m)=s1old
  182 x1=ak(m)
      depix=0.
c
c** plain stress 050288
c     dt(3)=-(sigo(3)+ce(3,1)*dt(1)+ce(3,2)*dt(2))/ce(3,3)
c     epsnew=0.
c     signew=0.
c     do 500 iter=1,20
c     epslst=epsnew
c     siglst=signew
c
c** trail stress
      do 44 i=1,6
      dsigs(i)=0.
      do 4 j=1,6
    4 dsigs(i)=dsigs(i)+ce(i,j)*dt(j)
      sigs(i)=sigo(i)+dsigs(i)
      sigt(i)=sigs(i)
   44 continue
c
      s2str =ckb*(hf*(sigs(2)-sigs(3))**2
     *           +hg*(sigs(3)-sigs(1))**2
     *           +hh*(sigs(1)-sigs(2))**2
     *           +hl*sigs(5)**2+hm*sigs(6)**2+hn*sigs(4)**2)
c
      s1str=sqrt(s2str)
      dsm=(dsigs(1)+dsigs(2)+dsigs(3))/3.
      smnew=smold+dsm
      depi(m)=0.
      if(s1str.le.x1) go to 299
c
c** 6-27-90
      fela=1.
      s1org=s1old
      if(s1str.gt.s1org) fela=(x1-s1org)/(s1str-s1org)
      if(fela.gt.1.0) fela=1.0
      if(fela.lt.0.0) fela=0.0
      fpla=1.-fela
      sf(m)=fela
      do 191 i=1,6
      sigc(i)=sigo(i)+dsigs(i)*fela
  191 continue
c
c** loop on cep(i,j) ****
c
            call cepx(cep ,sigs,dpl,etan,ce,cpl)
c
c** get new stress sigt(i)
      do 90 i=1,6
      sigt(i)=sigc(i)
      do 91 j=1,6
   91 sigt(i)=sigt(i)+cep(i,j)*dt(j)*fpla
   90 continue
c
c** get incr in effective strain depi(m) 070787, 070990-1240p
      dp(1)=dpl(1,1)*sigt(1)+dpl(1,2)*sigt(2)+dpl(1,3)*sigt(3)
      dp(2)=dpl(2,1)*sigt(1)+dpl(2,2)*sigt(2)+dpl(2,3)*sigt(3)
      dp(3)=dpl(3,1)*sigt(1)+dpl(3,2)*sigt(2)+dpl(3,3)*sigt(3)
      dp(4)=dpl(4,4)*sigt(4)
      dp(5)=dpl(5,5)*sigt(5)
      dp(6)=dpl(6,6)*sigt(6)
      eb1=zz*(r*dp(1)+(r+1.)*dp(2))**2
      eb2=   (r*dp(2)+(r+zz)*dp(1))**2
      eb3=r*(zz*dp(1)-dp(2))**2
      eb4=dp(4)*dp(4)/hn/4.
      eb5=dp(5)*dp(5)/hl/4.
      eb6=dp(6)*dp(6)/hm/4.
      dedef=p*p/r*(eb1+eb2+eb3)+4.*r*(1.+r+p)**2*(eb4+eb5+eb6)
      dede=(1.+r)/r/(1.+r+p)**2*dedef
      depix=sqrt(dede)
  299 continue
c
c** plains stress
c     epsnew=dt(3)
c     signew=sigt(3)
c     if(iter.eq.1) then
c      dt(3)=-dt(1)-dt(2)
c      go to 500
c     endif
c     demn=1.0e-24*x1+signew-siglst
c     if(abs(demn).lt.(1.0e-20*x1)) go to 501
c     if(abs(signew).lt.(1.0e-08*e)) go to 501
c     dt(3)=epslst-siglst*(epsnew-epslst)/demn
c     if(abs((epsnew-epslst)/dt(3)).lt.1.0e-05) go to 501
c 500 continue
c     write(17,240)
c     write(35,240)
c     write(59,240)
c 240 format(' convergence failure in plane stress subrootine')
c 501 continue
c     d3(m)=dt(3)
c
c** make sure press change is ok
      smcalc=(sigt(1)+sigt(2)+sigt(3))/3.
c
      aj2(m)=ckb*(hf*(sigt(2)-sigt(3))**2
     *           +hg*(sigt(3)-sigt(1))**2
     *           +hh*(sigt(1)-sigt(2))**2
     *           +hl*sigt(5)**2+hm*sigt(6)**2+hn*sigt(4)**2)
      aj1(m)=sqrt(aj2(m))
c** 070990-1240p
      dsige=aj1(m)-x1
      if(dsige.lt.0.0) dsige=0.
      depi(m)=dsige/qh
      if(depix.gt.0.0) then
       do 23 j=1,6
   23  dp(j)=dp(j)*depi(m)/depix
      endif
      ep(m)=ep(m)+depi(m)
      epx4(m)=depi(m)
c
c** 7-90 rr correction
      x1nw=x1+etan*depi(m)
      x1aj=1.0
      if(aj1(m).gt.x1nw) x1aj=x1nw/aj1(m)
      sigmn=smcalc
      do 93 i=1,6
      if(i.eq.4) sigmn=0.
      sigt(i)=(sigt(i)-sigmn)*x1aj+sigmn
   93 continue
c
      dp1(m)=dp(1)
      dp2(m)=dp(2)
      dp3(m)=dp(3)
      dp4(m)=dp(4)
      dp5(m)=dp(5)
      dp6(m)=dp(6)
c
      st1(m)=sigt(1)
      st2(m)=sigt(2)
      st3(m)=sigt(3)
      st4(m)=sigt(4)
      st5(m)=sigt(5)
      st6(m)=sigt(6)
  100 continue
c
      do 70 m=lft,llt
      do1(m)=cc(m)*dp1(m)+ss(m)*dp2(m)-cs(m)*dp4(m)
      do2(m)=ss(m)*dp1(m)+cc(m)*dp2(m)+cs(m)*dp4(m)
      do3(m)=dp3(m)
      do4(m)=2.*cs(m)*(dp1(m)-dp2(m))+(cc(m)-ss(m))*dp4(m)
      do5(m)=dp5(m)
      do6(m)=dp6(m)
      sig1(m)=cc(m)*st1(m) +ss(m)*st2(m)-2.*cs(m)*st4(m)
      sig2(m)=ss(m)*st1(m) +cc(m)*st2(m)+2.*cs(m)*st4(m)
      sig3(m)=st3(m)
      sig4(m)=cs(m)*(st1(m)-st2(m))+(cc(m)-ss(m))*st4(m)
      sig5(m)=st5(m)
      sig6(m)=st6(m)
      einc(m)=d1(m)*sig1(m)+d2(m)*sig2(m)+d4(m)*sig4(m)+d5(m)*sig5(m)
     *       +d6(m)*sig6(m)+einc(m)
c
c.... convert increments back to strain rates
c

      d1(i)=d1(i)/dt1
      d2(i)=d2(i)/dt1
      d3(i)=d3(i)/dt1
      d4(i)=d4(i)/dt1
      d5(i)=d5(i)/dt1
      d6(i)=d6(i)/dt1
c
   70 continue
      return
      end
      subroutine sueos(eosp,mte,nes)
c     implicit double precision (a-h,o-z)                                    dp
      dimension eosp(*)
      go to (10,20,30,40,50,10,60,70,80,100,110), nes
   10 call sueos1 (eosp)
      return
   20 call sueos2 (eosp)
      return
   30 call sueos3 (eosp)
      return
   40 call sueos4 (eosp)
      return
   50 call sueos5 (eosp)
      return
   60 call sueos7 (eosp,mte)
      return
   70 call sueos8 (eosp,mte)
      return
   80 call sueos9 (eosp)
  100 return
  110 call sues11 ( eosp, mte )
      return
      end
      subroutine sueos1 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 1      7-term polynomial
c
      common/aux11/po(128)
      common/aux14/
     & sig(128,7),epx2(128),epx3(128),epx4(128),aux(128,5)
      common/aux18/dd(128),df(128)
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/a(128),b(128),xmu(128),xmu2(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eos10/c1,c2,c3,c4,c5,c6,c7,e0,v0,xcv
      common/eosd/pc(128),g(128)
      dimension  eosp(1),ct(1)
      equivalence (c1,ct)
      data zero /0.0/
      mx=48*(mxt(lft)-1)
      do 10 i=1,10
   10 ct(i)=eosp(mx+i)
      do 20 i=lft,llt
      xmu(i)=1.0/df(i)-1.
      xmua(i)=  max(zero,xmu(i))
   20 specen(i)=xm(i)*epx2(i)
      do 30 i=lft,llt
   30 xmu2(i)=xmua(i)*xmua(i)
      do 40 i=lft,llt
      a(i)=c1+xmu(i)*(c2+c4*xmu(i)*xmu(i))+c3*xmu2(i)
   40 b(i)=c5+xmu(i)*c6+xmu2(i)*c7
      do 50 i=lft,llt
      cb(i)=1.333*g(i)+c2+2.*c3*xmua(i)+3.*c4*xmu2(i)
   50 cc(i)=b(i)*po(i)*df(i)*df(i)+specen(i)*(c6+2.*xmua(i)*c7)
      do 60 i=lft,llt
   60 cb(i)=abs(cb(i)+cc(i))
      return
      end
      subroutine sueos2 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 2      jwl he
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux9/vlrho(128),volnew(128)
      common/aux11/po(128)
      common/aux14/
     & sig(128,7),epx2(128),epx3(128),epx4(128),aux(128),
     1 vr1(128),vr2(128),bfc1(128),bfc2(128)
      common/aux18/dd(128),df(128)
      common/aux19/
     & sp,bfac(128),dr1v(128),dr2v(128),w1,r1v(128),r2v(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/wdr1v(128),wdr2v(128),er1v(128),er2v(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eos2/b1,b2,r1,r2
      common/double/iprec,ncpw,unit
c
      dimension  eosp(1)
c
c     initialize equation of state parameters
c
      mx=48*(mxt(lft)-1)
      b1=eosp(mx+1)
      b2=eosp(mx+2)
      r1=eosp(mx+3)
      r2=eosp(mx+4)
      w1=eosp(mx+5)
      bw1dr1=b1*w1/r1
      bw2dr2=b2*w1/r2
      do 10 i=lft,llt
      r1v(i)=bw1dr1/df(i)
      r2v(i)=bw2dr2/df(i)
      wdr1v(i)=b1-r1v(i)
      wdr2v(i)=b2-r2v(i)
      dr1v(i)=w1*epx2(i)/volnew(i)
      er1v(i)=exp(-r1*df(i))
   10 er2v(i)=exp(-r2*df(i))
c
      do 20 i=lft,llt
   20 cb(i)=df(i)*df(i)*((wdr1v(i)*r1+r1v(i)/df(i))*er1v(i)+(wdr2v(i)*r2
     1 +r2v(i)/df(i))*er2v(i)+dr1v(i)/df(i)+po(i)*w1/df(i))
      rosp=rhm(lft)*sp
      if (iburn.eq.3) rosp=0.
      do 30 i=lft,llt
      cb(i)=  max(cb(i),rosp*(1.-bfac(i)))
   30 cb(i)=  max(1.e-09*unit,cb(i))
c
      return
      end
      subroutine sueos3 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 3      sack 'tuesday' he
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux9/vlrho(128),volnew(128)
      common/aux11/po(128)
      common/aux14/
     & sig(128,7),epx2(128),epx3(128),epx4(128),aux(128),
     & vr1(128),vr2(128),bfc1(128),bfc2(128)
      common/aux18/dd(128),df(128)
      common/aux19/
     & sp,bfac(128),dr1v(128),dr2v(128),b3,r1v(128),r2v(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/wdr1v(128),wdr2v(128),term1(128),ea2v(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eos2/b1,b2,r1,r2
      common/double/iprec,ncpw,unit
c
      dimension  eosp(1)
c
c     initialize equation of state parameters
c
      mx=48*(mxt(lft)-1)
      a1=eosp(mx+1)
      a2=eosp(mx+2)
      a3=eosp(mx+3)
      b1=eosp(mx+4)
      b2=eosp(mx+5)
      a3b1=a3*b1
      do 10 i=lft,llt
      r2v(i)=a3b1/df(i)
      wdr2v(i)=a3-r2v(i)
      dr1v(i)=b2*epx2(i)/volnew(i)
      ea2v(i)=exp(-a2*df(i))/(df(i)**a1)
   10 term1(i)=wdr2v(i)*ea2v(i)
c
      do 20 i=lft,llt
   20 cb(i)=df(i)*((a1+df(i)*a2)*ea2v(i)*wdr2v(i)-a3b1*ea2v(i)/df(i)
     1 +dr1v(i)+b2*po(i))
      rosp=rhm(lft)*sp
      do 30 i=lft,llt
      cb(i)=  max(cb(i),rosp*(1.-bfac(i)))
   30 cb(i)=  max(1.e-09*unit,cb(i))
c
      return
      end
      subroutine sueos4 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 4      gruneisen
c
      common/aux11/po(128)
      common/aux14/
     & sig(128,7),epx2(128),epx3(128),epx4(128),aux(128,5)
      common/aux18/dd(128),df(128)
      common/aux19/
     & sp,bfac(128),xnum(128),xdem(128),dfmu(128),dfmu2(128)
      common/aux20/
     & pdenro(128),pnumro(128),facp(128),facn(128),xdi2(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/a(128),b(128),xmu(128),xmu2(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eos10/c,s1,s2,s3,g0,sa
      common/eosd/pc(128),g(128)
      common/double/iprec,ncpw,unit
      dimension  eosp(1),ct(6)
      equivalence (c,ct)
      mx=48*(mxt(lft)-1)
      do 10 i=1,6
   10 ct(i)=eosp(mx+i)
      s11=s1-1.
      s22=2.*s2
      s33=3.*s3
      s32=2.*s3
      sad2=.5*sa
      g0d2=1.-.5*g0
      roc2=rho(lft)*c**2
      do 20 i=lft,llt
      xmu(i)=1.0/df(i)-1.
      dfmu(i)=df(i)*xmu(i)
      dfmu2(i)=dfmu(i)*dfmu(i)
      facp(i)=.5*(1.+sign(1.*unit,xmu(i)))
      facn(i)=1.-facp(i)
   20 specen(i)=xm(i)*epx2(i)
      do 30 i=lft,llt
   30 xmu2(i)=xmu(i)*xmu(i)
      do 40 i=lft,llt
      xnum(i)=1.+g0d2*xmu(i)-sad2*xmu2(i)
      xdem(i)=1.-s11*xmu(i)-s2*xmu(i)*dfmu(i)-s3*xmu(i)*dfmu2(i)
      xdi2(i)=1./(xdem(i)*xdem(i))
      a(i)=roc2*xmu(i)*(facn(i)+facp(i)*xnum(i)*xdi2(i))
   40 b(i)=g0+sa*xmu(i)
      do 50 i=lft,llt
      pnumro(i)=roc2*(facn(i)+facp(i)*(xnum(i)+xmu(i)*g0d2-sa*xmu2(i)))
   50 pdenro(i)=2.*xdem(i)*(-s11-s22*dfmu(i)+s2*dfmu2(i)-s33*dfmu2(i)
     1 +s32*dfmu2(i)*dfmu(i))
      do 60 i=lft,llt
   60 cb(i)=pnumro(i)*(facn(i)+facp(i)*xdi2(i))-facp(i)*a(i)*xdi2(i)
     1 *pdenro(i)
      do 70 i=lft,llt
   70 cb(i)=1.333333*g(i)+cb(i)+sa*specen(i)+b(i)*df(i)*df(i)*  max(pc(i
     1 ),(a(i)+b(i)*specen(i)))
      return
      end
      subroutine sueos5 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 5      ratio of polynomials
c
      common/aux11/po(128)
      common/aux14/
     & sig(128,7),epx2(128),epx3(128),epx4(128),aux(128,5)
      common/aux18/dd(128),df(128)
      common/aux19/sp,bfac(128),aa(128),bb(128),bb2(128),arho(128),
     1 brho(128)
      common/aux20/pf1(128),pf2(128),pf3(128),pf4(128),pf5(128),
     1 pf6(128),pf7(128),sie(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/
     & f1(128),f2(128),f3(128),f4(128),f5(128),f6(128),
     & f7(128),fa(128),xmu(128),xmu2(128),xmu3(128),xmu4(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eosd/pc(128),g(128)
      common/ratp/a10,a11,a12,a13,a20,a21,a22,a23,
     1            a30,a31,a32,a33,a40,a41,a42,a43,
     2            a50,a51,a52,a53,a60,a61,a62,a63,
     3            a70,a71,a72,a73,alfa,a12bta,a14,a24
      common/dltaph/ibtflg(128),idlflg
      common/double/iprec,ncpw,unit
      dimension  eosp(1),ct(1)
      equivalence (a10,ct)
      mx=48*(mxt(lft)-1)
      do 10 i=1,32
   10 ct(i)=eosp(mx+i)
c   if using dp , set the delta phase flag and reset a10
      if (a10 .eq. -1) then
        idlflg = 1
        a10 = 0.0
      else
        idlflg = 0
      endif
      do 20 i=lft,llt
      xmu(i)=1.0/df(i)-1.
   20 sie(i)=xm(i)*epx2(i)
      do 30 i=lft,llt
      xmu2(i)=xmu(i)*xmu(i)
      xmu3(i)=xmu(i)*xmu2(i)
   30 xmu4(i)=xmu(i)*xmu3(i)
      do 40 i=lft,llt
      f1(i)=a10+a11*xmu(i)+a12*xmu2(i)+a13*xmu3(i)+a14*xmu4(i)
      f2(i)=a20+a21*xmu(i)+a22*xmu2(i)+a23*xmu3(i)+a24*xmu4(i)
      f3(i)=a30+a31*xmu(i)+a32*xmu2(i)+a33*xmu3(i)
      f4(i)=a40+a41*xmu(i)+a42*xmu2(i)+a43*xmu3(i)
      f5(i)=a50+a51*xmu(i)+a52*xmu2(i)+a53*xmu3(i)
      f6(i)=a60+a61*xmu(i)+a62*xmu2(i)+a63*xmu3(i)
      f7(i)=a70+a71*xmu(i)+a72*xmu2(i)+a73*xmu3(i)
   40 fa(i)=1.+alfa*xmu(i)
      if (a12bta.eq.0.0) go to 60
      do 50 i=lft,llt
   50 f1(i)=f1(i)+a12bta*(.50-sign(.5*unit,xmu(i)))*xmu2(i)
   60 do 70 i=lft,llt
      pf1(i)=a11+2.*a12*xmu(i)+3.*a13*xmu2(i)+4.*a14*xmu3(i)
      pf2(i)=a21+2.*a22*xmu(i)+3.*a23*xmu2(i)+4.*a24*xmu3(i)
      pf3(i)=a31+2.*a32*xmu(i)+3.*a33*xmu2(i)
      pf4(i)=a41+2.*a42*xmu(i)+3.*a43*xmu2(i)
      pf5(i)=a51+2.*a52*xmu(i)+3.*a53*xmu2(i)
      pf6(i)=a61+2.*a62*xmu(i)+3.*a63*xmu2(i)
      pf7(i)=a71+2.*a72*xmu(i)+3.*a73*xmu2(i)
   70 xmu4(i)=df(i)*df(i)*po(i)
      do 80 i=lft,llt
      aa(i)=f1(i)+sie(i)*(f2(i)+sie(i)*(f3(i)+sie(i)*f4(i)))
      bb(i)=1./(f5(i)+sie(i)*(f6(i)+sie(i)*f7(i)))
      arho(i)=pf1(i)+sie(i)*(pf2(i)+sie(i)*(pf3(i)+sie(i)*pf4(i)))
   80 brho(i)=pf5(i)+sie(i)*(pf6(i)+sie(i)*pf7(i))
      do 90 i=lft,llt
      arho(i)=arho(i)+xmu4(i)*(f2(i)+sie(i)*(2.*f3(i)+3.*sie(i)*f4(i)))
   90 brho(i)=brho(i)+xmu4(i)*(f6(i)+2.*sie(i)*f7(i))
      do 100 i=lft,llt
      cb(i)=bb(i)*(fa(i)*arho(i)+aa(i)*(alfa-bb(i)*fa(i)*brho(i)))
  100 cb(i)=1.333333*g(i)+abs(cb(i))
      return
      end
      subroutine sueos7 (eosp,mte)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 7      ignition of growth of reaction in
c                                    high explosives
c
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),sign7(128),aux(128,13)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux41/q1(128),cbb(128),dd(128),cx(128),qx(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/bk55/a(30)
      dimension  eosp(1)
c
      mx=48*(mxt(lft)-1)
      do 10 i=1,30
   10 a(i)=eosp(mx+i)
c
      if (mte.eq.9) go to 40
c
c     as the burn fraction approaches one cb approaches zero
c
      do 20 i=lft,llt
   20 cb(i)=1.-aux(i,7)
      do 30 i=lft,llt
      sign1(i)=cb(i)*sign1(i)
      sign2(i)=cb(i)*sign2(i)
      sign3(i)=cb(i)*sign3(i)
      sign4(i)=cb(i)*sign4(i)
      sign5(i)=cb(i)*sign5(i)
   30 sign6(i)=cb(i)*sign6(i)
c
   40 do 50 i=lft,llt
      aux(i,13)=aux(i,2)
   50 cb(i)=aux(i,11)
      return
      end
      subroutine sueos8 (eosp,mte)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 8      compaction model
c
      common/bk19/nconst(60),lenma,ncneos(15)
      common/aux14/sig(128,7),aux(128,13)
      common/aux18/dd(128),def(128)
      common/aux20/rvj1(128),rvj(128),cej1(128),cej(128),tej1(128),
     1 tej(128),buj1(128),buj(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/
     & defm(128),defln(128),bulk(128),factor(128),cep(128),
     & pres(128),puld(128),pnew(128),tec(128),fc(128),fc1(128),
     & fjk(128)
      common/aux41/q1(128),cbb(128)
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      common/eosd/pc(128),shrm(128)
      common/double/iprec,ncpw,unit
      dimension  eosp(1),xmult(128,10)
      equivalence (bulk,xmult)
c
      mx=48*(mxt(lft)-1)
      nc=nconst(mte)
      do 10 i=1,40
   10 rv(i)=eosp(mx+i)
      n=eosp(mx+48)
      gamma=eosp(mx+41)
c
      do 20 i=lft,llt
      defln(i)=log(def(i))
   20 defm(i)=  min(defln(i),aux(i,nc+1))
c
      do 30 i=lft,llt
      rvj1(i)=0.
      rvj(i)=0.
      cej1(i)=0.
      cej(i)=0.
      tej1(i)=0.
      tej(i)=0.
      buj1(i)=0.
      buj(i)=0.
      xmult(i,1)=0.
   30 xmult(i,n+1)=1.
      do 50 j=2,n
      do 40 i=lft,llt
   40 xmult(i,j)=.5+sign(.5*unit,defm(i)-rv(j))
   50 continue
      do 70 j=1,n
      nn=n-j+2
      mm=n-j+1
      do 60 i=lft,llt
   60 xmult(i,nn)=xmult(i,nn)-xmult(i,mm)
   70 continue
      do 90 j=1,n
      do 80 i=lft,llt
      rvj1(i)=rvj1(i)+rv(j)*xmult(i,j+1)
      rvj(i)=rvj(i)+rv(j+1)*xmult(i,j+1)
      cej1(i)=cej1(i)+ce(j)*xmult(i,j+1)
      cej(i)=cej(i)+ce(j+1)*xmult(i,j+1)
      tej1(i)=tej1(i)+te(j)*xmult(i,j+1)
      tej(i)=tej(i)+te(j+1)*xmult(i,j+1)
      buj1(i)=buj1(i)+bu(j)*xmult(i,j+1)
   80 buj(i)=buj(i)+bu(j+1)*xmult(i,j+1)
   90 continue
      do 100 i=lft,llt
  100 factor(i)=(defm(i)-rvj1(i))/(rvj(i)-rvj1(i))
      do 110 i=lft,llt
      cep(i)=cej1(i)+(cej(i)-cej1(i))*factor(i)
      tec(i)=tej1(i)+(tej(i)-tej1(i))*factor(i)
  110 bulk(i)=buj1(i)+(buj(i)-buj1(i))*factor(i)
      do 120 i=lft,llt
      tec(i) =gamma*tec(i)
  120 bulk(i)=  max(bu(1),bulk(i))
      do 130 i=lft,llt
  130 cb(i)=1.33*shrm(i)+bulk(i)
      return
      end
      subroutine sueos9 (eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     setup equation-of-state 9      tabular
c
      common/bk19/nconst(60),lenma,ncneos(15)
      common/aux11/po(128)
      common/aux14/sig(128,7),epx2(128),aux(128,13)
      common/aux18/dd(128),def(128)
      common/aux20/rvj1(128),rvj(128),cej1(128),cej(128),tej1(128),
     1 tej(128),buj1(128),buj(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/
     & defm(128),defln(128),bulk(128),factor(128),cep(128),
     1 pres(128),puld(128),pnew(128),tec(128),fc(128),fc1(128),
     2 fjk(128)
      common/aux43/xm(128)
      common/double/iprec,ncpw,unit
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      common/eosd/pc(128),shrm(128)
      dimension  eosp(1),xmult(128,10)
      equivalence (bulk,xmult)
c
      mx=48*(mxt(lft)-1)
      do 10 i=1,30
   10 rv(i)=eosp(mx+i)
      n=eosp(mx+48)
      gamma=eosp(mx+41)
c
      do 20 i=lft,llt
   20 defm(i)=log(def(i))
c
      do 30 i=lft,llt
      rvj1(i)=0.
      rvj(i)=0.
      cej1(i)=0.
      cej(i)=0.
      tej1(i)=0.
      tej(i)=0.
      xmult(i,1)=0.
   30 xmult(i,n+1)=1.
      do 50 j=2,n
      do 40 i=lft,llt
   40 xmult(i,j)=.5+sign(.5*unit,defm(i)-rv(j))
   50 continue
      do 70 j=1,n
      nn=n-j+2
      mm=n-j+1
      do 60 i=lft,llt
   60 xmult(i,nn)=xmult(i,nn)-xmult(i,mm)
   70 continue
      do 90 j=1,n
      do 80 i=lft,llt
      rvj1(i)=rvj1(i)+rv(j)*xmult(i,j+1)
      rvj(i)=rvj(i)+rv(j+1)*xmult(i,j+1)
      cej1(i)=cej1(i)+ce(j)*xmult(i,j+1)
      cej(i)=cej(i)+ce(j+1)*xmult(i,j+1)
      tej1(i)=tej1(i)+te(j)*xmult(i,j+1)
   80 tej(i)=tej(i)+te(j+1)*xmult(i,j+1)
   90 continue
      do 100 i=lft,llt
      buj(i)=rvj(i)-rvj1(i)
      buj1(i)=1./buj(i)
  100 factor(i)=(defm(i)-rvj1(i))*buj1(i)
      do 110 i=lft,llt
      buj(i)=cej(i)-cej1(i)
      cb(i)=tej(i)-tej1(i)
      cep(i)=cej1(i)+buj(i)*factor(i)
      tec(i)=gamma*(tej1(i)+cb(i)*factor(i))
  110 bulk(i)=-buj(i)*(buj1(i)+gamma*cb(i)*epx2(i)*xm(i))
      do 120 i=lft,llt
  120 cb(i)=1.33*shrm(i)+po(i)*tec(i)*def(i)*def(i)+bulk(i)
      return
      end
      subroutine sues11 (eosp, mte )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     s  u  e  o  s  1  1                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  sueos11 is the dyna2d/dyna3d interface for the        *
c *            tensor pore collapse equation-of-state.  it will      *
c *            convert the current relative volumes (stored in       *
c *            the array def) into excess compressions.  it will     *
c *            then call the tensor pore collapse model (tnspore)    *
c *            with a temporary variable for the pressure and        *
c *            the maximum excess compression obtained thus far.     *
c *            it keeps the maximum excess compression as a time     *
c *            history variable in the array aux(*,nc+1), where      *
c *            nc is the next available time history storage.        *
c *                                                                  *
c *               the routine ldeos11 is called to bring in the      *
c *            current eos 11 set-up.  it dumps the current          *
c *            definitions needed into thier appropriate common      *
c *            blocks.  thus tnspore and all its lower routines      *
c *            do not know anything about other eos 11 set-ups,      *
c *            or anything associated with the dyna codes.           *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *   variables                                                      *
c *                                                                  *
c *            nvec      the current vector length used in dyna      *
c *                                                                  *
c *            mx        position for current eos setup.             *
c *                                                                  *
c *            ntmhist   position for next available time-history    *
c *                                                                  *
c *            xmu        current excess compression                  *
c *                                                                  *
c *            aux       dyna variable used to hold time-histories   *
c *                                                                  *
c *            ptmp      temporary storage used to store the         *
c *                      current pressure.  eqos11 will update       *
c *                      the pressure (pnew) using ptmp.             *
c *                                                                  *
c *            bulk      bulk modulus                                *
c *                                                                  *
c *            cb        bulk sound speed                            *
c *                                                                  *
c *            shrm      shear modulus                               *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *   notes    1)  due to some instabilities created in dyna with    *
c *                a sudden change in the bulk sound speed, the      *
c *                sound speed is calculated based upon the higher   *
c *                bulk modulus of the unloading curve, rather       *
c *                than the true bulk modulus (bulk).                *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     september 16, 1985                                    *
c *                                                                  *
c *                                                                  *
c *   externals   tnspore, ldeos11                                   *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
c
      external  ldes11, tnsor
c
      common/bk19/nconst(60),lenma,ncneos(15)
         common/aux14/sig7(128,7), aux(128,13)
         common/aux18/dd(128), def(128)
         common/aux33/ix(8,128), mxt(128), nmel
         common/aux35/rhm(128), cb(128), q1a(128), cxa(128)
         common/aux36/lft, llt
         common/aux40/defm(128), defln(128), bulk(128)
     x        ,           xmu(128), ptmp(128), pres(128), puld(128)
     x    ,           pnew(128), tec(128), fc(128), fc1(128)
         common/ eosd/pc(128), shrm(128)
c
      mx = 48*(mxt(lft)-1) + 1
      call ldes11 ( eosp(mx) )
c
      ntmhist = nconst(mte)
c
c     **  calculate the excess compression  **
c            u = v0/v - 1
c
      do 10  i = lft, llt
   10    xmu(i) = 1.0 / def(i)  - 1
c
c     **  calculate the bulk modulus  **
c
      n = llt - lft + 1
      call tnsor ( n, xmu(lft), aux(lft,ntmhist+1), ptmp(lft),
     1               bulk(lft) )
c
c     **  calculate the bulk sound speed  **
c
      do 20  i = lft, llt
   20    cb(i) =   1.333333333333e+00* shrm(i) + bulk(i)
      return
      end
      subroutine eqos11 ( xinen )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     e  q  o  s  1  1                             *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  eqos11 is the dyna2d/dyna3d interface for the         *
c *            tensor pore collapse equation-of-state.  it works     *
c *            in conjunction with the routine sueos11.  sueos11     *
c *            does almost all the work, and eqos11 simple is used   *
c *            to preserve the order in which things are required    *
c *            to be done in dyna.  eqos11 copies the current        *
c *            pressures calculated in sueos11 into the array pnew   *
c *            keeping the pressures above the pressure cutoffs.     *
c *            the components of the stress tensor, the changes      *
c *            in the internal energy of each element and the        *
c *            total internal energy are all updated.                *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *   variables                                                      *
c *                                                                  *
c *            nvec      the current vector length used in dyna      *
c *                                                                  *
c *            mx        position for current eos setup.             *
c *                                                                  *
c *            aux(*,1)  the current change in the internal          *
c *                      energy of each element                      *
c *                                                                  *
c *            ptmp      temporary storage passed from routine       *
c *                      sueos11 containing the current pressures    *
c *                                                                  *
c *            pnew      dyna variable to hold the current pressure  *
c *                                                                  *
c *            pc        pressure cut-offs describing the lowest     *
c *                      possible pressure obtainable                *
c *                                                                  *
c *            sign1, sign2, sign3    components of the stress       *
c *                                   tensor                         *
c *                                                                  *
c *            xinen     total internal energy                       *
c *                                                                  *
c *            dvol      current change in volume                    *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     september 16, 1985                                    *
c *                                                                  *
c *                                                                  *
c *   externals   tnspore, bulkmod, ldeos11                          *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
         common/aux14/sign1(128),sign2(128),sign3(128),
     x       sign47(4,128),aux(128,13)
         common/aux15/qp(128),spec(128),dvol(128),
     x  vold(128)
         common/aux33/ix(8,128),mxt(128),nmel
         common/aux36/lft,llt
         common/aux40/defm(128),defln(128),bulk(128)
     x        , xmu(128),ptmp(128),pres(128),puld(128)
     x    ,     pnew(128),tec(128),fc(128),fc1(128)
         common/ eosd/pc(128),shrm(128)
      dimension xinen(*)
c
c
c     **  calculate the  the internal energy, etc.
c
      mx = mxt(lft)
      do 20  i = lft, llt
         pnew(i) =   max( pc(i), ptmp(i) )
         sign1(i) = sign1(i) - pnew(i)
         sign2(i) = sign2(i) - pnew(i)
         sign3(i) = sign3(i) - pnew(i)
         aux(i,1) = aux(i,1) - pnew(i)*dvol(i)
         xinen(mx) = xinen(mx) + aux(i,1)
   20 continue
      return
      end
      subroutine ldes11 ( eosp )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     l  d  e  o  s  1  1                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose   since there can be several different materials       *
c *             and hence several different equation-of-state        *
c *             definitions, a mechanism for loading the approp-     *
c *             riate definition into the tensor pore collapse       *
c *             package is needed.  this is usually handled by       *
c *             dyna with the array (eosp).  however, since the      *
c *             cubic splines require so much storage space it       *
c *             was not feasible to bump up the storage space for    *
c *             all the equation-of-state packages in dyna.          *
c *             therefore a special storage was dynamically          *
c *             allocated and is used for this purpose.              *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
c
      common/es11/u1, u2
c     **  loading curve definition used in intrpld only.  **
      common/es11ld/nld, loadng(320)
c     **  completely crushed curve definition used in intrpcr only.  **
      common/es11cr/ncr, crushd(320)
c     **  unloading curve definition used in intrpun only.  **
      common/es11un/nun, unload(320)
c
      common/ /  a(1)
c
      save     nlast
c
c     **  if ldeos11 was called last time with the same  **
c     **  pointer, then the data is already loaded.      **
c
      nptr = int ( eosp(48) )
      if ( nptr .eq. nlast )  return
c
      nld = int ( eosp(1) )
      ncr = int ( eosp(2) )
      nun = ncr
      u1 = eosp(3)
      u2 = eosp(4)
      do 10  i = 1, 320
         loadng(i) = a(nptr+i-1)
         crushd(i) = a(nptr+320+i-1)
         unload(i)  = a(nptr+2*320+i-1)
   10 continue
      nlast = nptr
      return
      end
      subroutine tnsor ( n, u, ux, p, bulk )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     t  n  s  p  o  r  e                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  tnspore determines the pressure (p) of a material     *
c *            at the excess compression points (u).  the material   *
c *            is determined to be on one of three possible paths,   *
c *            the virgin loading curve, the completely crushed      *
c *            curve, or a partially crushed curve. the virgin       *
c *            loading and comepletely crushed curves are easily     *
c *            handled by interpolation of the inputted curves.      *
c *            if the material is partially crushed, then the        *
c *            pores of the material collapse and then possibly      *
c *            recover.  this elastic recovery of some of the pores  *
c *            is the main reason for this equation-of-state.  the   *
c *            first reference provides a good description of how    *
c *            this recovery is computed.                            *
c *                                                                  *
c *               the bulk modulus (bulk) and sound speed (cb) are   *
c *            also returned.  the sound speed is based on the       *
c *            slope of the completely crushed curve to avoid        *
c *            instabilities arising in dyna from a sudden change    *
c *            in the bulk sound speed.                              *
c *                                                                  *
c *               the problem is broken down into two cases, do      *
c *            interpolation on the virgin loading curve or on       *
c *            the completely crushed curve, since the partially     *
c *            crushed curve is defined in terms of the completely   *
c *            crushed curve.  the latter problem is then broken     *
c *            down into whether it is a completely crushed curve    *
c *            point or a partially crushed curve point.  variable   *
c *            f1 determines the first breakdown, and variable f2    *
c *            determines the second one.                            *
c *                                                                  *
c *                                                                  *
c *   references                                                     *
c *            crawfis, roger a.  guidelines for using the new       *
c *               tensor pore collapse model in dyna2d/dyna3d:       *
c *               eos-11, llnl internal document  (january 1986).    *
c *                                                                  *
c *            burton, donald e., et al, physics and numerics        *
c *               of the tensor code, llnl internal document         *
c *               ucid-19428                                         *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c *   externals                                                      *
c *            itrpld, itrpcr, itrpun                             *
c *                                                                  *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
      external  itrpld, itrpcr, itrpun
c
      common/es11/u1, u2
      dimension ux(*),u(*),p(*),bulk(*)
      dimension f1(128),f2(128),ua(128),ub(128),b1(128),
     1 p1(128),px(128)
c
c     for purposes of calculating the bulk modulus, calculate
c     the pressure from the completely crushed curve whenever the
c     excess compression is greater than or equal to the maximum
c     excess compression hence obtained.  thus, set the maximum excess
c     compression now, so as to get identical results in pressure from
c     both the loading curve and the completely crushed curve in this
c     case.  only the slope will differ ( and hence the bulk modulus ).
c
      do 5  i = 1, n
    5    ux(i) =   max ( ux(i), u(i) )
c
c     determine which curve the elements are on:  the
c     virgin loading curve (f1 = 1.0), the completely crushed curve
c     (f2 = 1.0) or a partially crushed curve.
c
      do 10  i = 1, n
         f1(i) = 0.0
         if ( (ux(i) .lt. u1) .or. (u(i) .ge. u2) )  f1(i) = 1.0
         f2(i) = 1.0
         if (ux(i) .lt. u2)  f2(i) = 0.0
   10 continue
c
c     define the partially crushed curve
c                      v . v
c                       b
c          p(mu) = p  (------)
c                   cc  v
c                        min
c
c     also determine the values for the virgin loading
c     curve points.
c
      call itrpld ( n, u,  p1, b1 )
      call itrpun ( n, p1, ua )
      call itrpld ( n, ux, px, bulk )
      call itrpun ( n, px, ub )

      do 20  i = 1, n
         ua(i) = f2(i) * ( f1(i)*ua(i) + (1.-f1(i))*u(i) )
     1         +  (1.-f2(i)) * ( (1.+u(i))*(1.+ub(i))/(1.+ux(i)) - 1. )
   20 continue
c
c     determine the values for the completely crushed points
c
      call itrpcr ( n, ua, p, bulk )
c
      do 30  i = 1, n
         p(i) = f1(i) * p1(i)  +  (1.-f1(i)) *   min (p(i), p1(i))
         bulk(i) = f1(i) * b1(i)  +  (1.-f1(i)) * bulk(i)
   30 continue
c
      return
      end
      subroutine itrpld ( n, x0, f0, b0 )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     i  n  t  r  p  l  d                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  intrpld evaluates the cubic spline of the loading     *
c *            curve at the points x0.  the cubic spline was set     *
c *            up by the routine pchim, which produces a monotone    *
c *            piecewise-cubic spline.  intermediate coefficients    *
c *            were calculated to increase the efficiency of the     *
c *            evaluator.  intrpld calls pchimev, the pchim eval-    *
c *            uation routine for the cubic spline evaluation.       *
c *            intrpld also returns the bulk modulus as the slope    *
c *            at the current point times one plus the current       *
c *            excess compression point (x0).                        *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c *   notes    called by routine tnspore                             *
c *            calls routine pchimev                                 *
c *                                                                  *
c ********************************************************************
c
      external  pchmev
c
      common/es11ld/nld, xld(64), fld(64), dld(64)
     x     ,                       c2ld(64), c3ld(64)
      dimension x0(*),f0(*),b0(*)
c
      call pchmev ( nld, xld, fld, dld, c2ld, c3ld, n, x0, f0, b0 )
c
      do 10  i = 1, n
   10    b0(i) = b0(i)  *  ( x0(i) + 1.0 )
      return
      end
      subroutine itrpcr ( n, x0, f0, b0 )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     i  n  t  r  p  c  r                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  intrpcr evaluates the cubic spline of the completely  *
c *            crushed curve at the excess compression points x0.    *
c *            the cubic spline was set up by the routine pchim,     *
c *            which produces a monotone piecewise-cubic spline.     *
c *            intermediate coefficients are calculated to increase  *
c *            the efficiency of the evaluator.                      *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c *   notes    called by routines tnspore, unload                    *
c *            cray optimized search routine:  luf                   *
c *                                                                  *
c ********************************************************************
c
      external  pchmev
c
      common/es11cr/ncr, xcr(64), fcr(64), dcr(64)
     x     ,                       c2cr(64), c3cr(64)
      dimension x0(*),f0(*),b0(*)
c
      call pchmev ( ncr, xcr, fcr, dcr, c2cr, c3cr, n, x0, f0, b0 )
c
      do 10  i = 1, n
   10    b0(i) = b0(i)  *  ( x0(i) + 1.0 )
      return
      end
      subroutine itrpun ( n, x0, f0 )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     i  n  t  r  p  u  n                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  intrpun evaluates the cubic spline of the completely  *
c *            crushed curve at the pressure points x0 for the       *
c *            excess compression points f0 ( i.e. mu(p)  ).         *
c *            the cubic spline was set up by the routine pchim,     *
c *            which produces a monotone piecewise-cubic spline.     *
c *            intermediate coefficients are calculated to increase  *
c *            the efficiency of the evaluator.                      *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c *   notes    called by routines tnspore, unload                    *
c *            cray optimized search routine:  luf                   *
c *                                                                  *
c ********************************************************************
c
      external  pchmev
c
      common/es11un/nun, xun(64), fun(64), dun(64)
     x     ,                       c2un(64), c3un(64)
c
      call pchmev ( nun, xun, fun, dun, c2un, c3un, n, x0, f0, d0 )
      return
      end
      subroutine pchmev ( n, x, f, d, c2, c3, npts, x0, f0, d0 )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     p  c  h  i  m  e  v                          *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  pchimev evaluates the cubic spline set up with        *
c *            the routine pchim.  it also does a linear inter-      *
c *            polation to determine the derivatives.                *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c *   notes    cray optimized search routine:  luf                   *
c *            the variable nvec is the maximum value of             *
c *               npts.  this is a dyna vector partitioning          *
c *               feature.                                           *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
c     **  local variables  **
      integer  i, il(128)
      dimension x(*),f(*),d(*),c2(*),c3(*),x0(*),f0(*),d0(*)
      external  sluf
c
      call sluf ( n, 1, x, npts, x0, il )
      do 20  i = 1, npts
         il(i) = min ( n-1, max ( 1, il(i) ) )
         dx = x0(i) - x(il(i))
         f0(i) = f(il(i))  +  dx * (  d(il(i))
     1                     +  dx * ( c2(il(i))
     2                     +  dx *   c3(il(i))  )  )
         d0(i) = d(il(i)) + dx * ( d(il(i)+1) - d(il(i)) )
     1                         / ( x(il(i)+1) - x(il(i)) )
   20 continue
      return
      end
      subroutine lieupd (ener,xies,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux9/vlrho(128),volnew(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      dimension xies(*),ener(*),bqs(*)
c
      dt25=.25*dt1
      mx=mxt(lft)
      do 10 i=lft,llt
      xies(i)  =xies(i)+.5*volnew(i)*(einc(i)+dt1*dde(i)*qp(i))
      ener(mx)=ener(mx)+xies(i)
      bqs(i)  =qp(i)
   10 continue
      return
      end
      subroutine hieupd
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 pold(128),vavg(128),ddv(128)
      common/aux9/vlrho(128),volnew(128)
      common/aux11/po(128),
     3 sgv1(128),sgv2(128),sgv3(128),sgv4(128),
     4 sgv5(128),sgv6(128),e1(128),e2(128),e3(128),e4(128),
     5 e5(128),e6(128),einc(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &epx1(128),epx2(128),epx3(128),epx4(128),epx5(128,5)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux43/xm(128)
      common/dltaph/ibtflg(128),idlflg
      data       third/0.33333333333333/

      dt25=.25*dt1
      do 10 i=lft,llt
      pold(i)=third*(sgv1(i)+sgv2(i)+sgv3(i))
   10 volold(i)=epx4(i)
      do 20 i=lft,llt
      e1(i)=d1(i)*(sgv1(i)+sig1(i)-pold(i))
      e2(i)=d2(i)*(sgv2(i)+sig2(i)-pold(i))
      e3(i)=d3(i)*(sgv3(i)+sig3(i)-pold(i))
      e4(i)=d4(i)*(sgv4(i)+sig4(i))
      e5(i)=d5(i)*(sgv5(i)+sig5(i))
   20 e6(i)=d6(i)*(sgv6(i)+sig6(i))
      do 30 i=lft,llt
      vavg(i)=volnew(i)+volold(i)
      dvol(i)=.50*(volnew(i)-volold(i))
   30 einc(i)=vavg(i)*(e1(i)+e2(i)+e3(i)+e4(i)+e5(i)+e6(i))
      do 40 i=lft,llt
      epx2(i)=epx2(i)+dt25*einc(i)+dvol(i)*(pold(i)-epx3(i)-qp(i))
      epx4(i)=volnew(i)
   40 epx3(i)=qp(i)

c   if using dp , get the last bit of specen to use as a flag.
c   specen is actually equivalent to sie in eqos5 , and is used as the
c   initial calculation of energy for the 2 step iteration calculation
c   of pressure in eqos5 .

      if ((idlflg .eq. 1) .and. (ncycle .ne. 0)) then
         do 15 i = lft,llt
c          ibtflg(i) = specen(i) .and. 1b                               cray1
   15    continue
      endif

      do 50 i=lft,llt
   50 specen(i)=epx2(i)*xm(i)
      return
      end
      subroutine eqos(fval,ener,mte,nes)
c     implicit double precision (a-h,o-z)                                    dp
      dimension fval(1),ener(1)
      go to (10,20,30,40,50,60,70,80,90,100,110), nes
   10 call eqos1 (ener)
      return
   20 if (mte.ne.8) call setbfc
      call eqos2 (ener)
      return
   30 if (mte.ne.8) call setbfc
      call eqos3 (ener)
      return
   40 call eqos1 (ener)
      return
   50 call eqos5 (ener)
      return
   60 call eqos6 (fval)
      go to 10
   70 call eqos7 (ener,mte)
      return
   80 call eqos8 (ener)
      return
   90 call eqos9 (ener)
  100 return
  110 call eqos11 (ener)
      return
      end
      subroutine setbfc
c     implicit double precision (a-h,o-z)                                    dp
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/aux36/lft,llt
      do 10 i=lft,llt
   10 bfac(i)=1.0
      return
      end
      subroutine eqos1(ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),epx1(128),epx2(128),epx3(128),epx4(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128),
     1 r2v(128),r1v(128),w1df(128),facm(128),pnew(128)
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/a(128),b(128),xmu(128),xmu2(128)
      common/aux43/xm(128),p(128),xmua(128)
      common/eosd/pc(128),g(128)
      dimension ener(1)
      do 10 i=lft,llt
   10 pnew(i)=(a(i)+specen(i)*b(i))/(1.+b(i)*dvol(i)*xm(i))
      do 20 i=lft,llt
   20 pnew(i)=  max(pnew(i),pc(i))
      do 30 i=lft,llt
   30 epx2(i)=epx2(i)-pnew(i)*dvol(i)
      do 40 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   40 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 50 i=lft,llt
   50 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine eqos2(ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),epx1(128),epx2(128),epx3(128),epx4(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128),
     1 w1df(128),facm(128),pnew(128)
      common/aux18/dd(128),df(128)
      common/aux19/
     & sp,bfac(128),dr1v(128),dr2v(128),w1,r1v(128),r2v(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/aux40/wdr1v(128),wdr2v(128),er1v(128),er2v(128)
      common/aux43/xm(128),p(128),xmua(128)
      dimension ener(1)
      data zero/0.0/
      do 10 i=lft,llt
      w1df(i)=bfac(i)*w1/df(i)
   10 facm(i)=bfac(i)*(wdr1v(i)*er1v(i)+wdr2v(i)*er2v(i))
      do 20 i=lft,llt
   20 pnew(i)=(facm(i)+specen(i)*w1df(i))/(1.+w1df(i)*dvol(i)*xm(i))
      do 30 i=lft,llt
   30 pnew(i)=  max(zero,pnew(i))
      do 40 i=lft,llt
   40 epx2(i)=epx2(i)-pnew(i)*dvol(i)
      do 50 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   50 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 60 i=lft,llt
   60 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine eqos3(ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),epx1(128),epx2(128),epx3(128),epx4(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128),
     1 b2df(128),facm(128),pnew(128)
      common/aux18/dd(128),df(128)
      common/aux19/
     & sp,bfac(128),dr1v(128),dr2v(128),b2,r1v(128),r2v(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhm(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/wdr1v(128),wdr2v(128),term1(128),ea2v(128)
      common/aux43/xm(128),p(128),xmua(128)
      dimension ener(1)
      data zero /0.0/
      do 10 i=lft,llt
      b2df(i)=bfac(i)*b2/df(i)
   10 facm(i)=bfac(i)*term1(i)
      do 20 i=lft,llt
   20 pnew(i)=(facm(i)+specen(i)*b2df(i))/(1.+b2df(i)*dvol(i)*xm(i))
      do 30 i=lft,llt
   30 pnew(i)=  max(zero,pnew(i))
      do 40 i=lft,llt
   40 epx2(i)=epx2(i)-pnew(i)*dvol(i)
      do 50 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   50 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 60 i=lft,llt
   60 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine eqos5 (ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),epx1(128),epx2(128),epx3(128),epx4(128)
      common/aux15/qp(128),sie(128),dvol(128),volold(128),r2v(128),
     1 r1v(128),b2df(128),facm(128),pnew(128)
      common/aux19/sp,bfac(128),aa(128),bb(128),bb2(128),arho(128),
     1 brho(128)
      common/aux20/pf1(128),pf2(128),pf3(128),pf4(128),pf5(128),
     1 pf6(128),pf7(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cb(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux40/
     & f1(128),f2(128),f3(128),f4(128),f5(128),f6(128),
     1 f7(128),fa(128),xmu(128),xmu2(128),xmu3(128),xmu4(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eosd/pc(128),g(128)
      common/dltaph/ibtflg(128),idlflg
      common/ratp/a10,a11,a12,a13,a20,a21,a22,a23,
     1            a30,a31,a32,a33,a40,a41,a42,a43,
     2            a50,a51,a52,a53,a60,a61,a62,a63,
     3            a70,a71,a72,a73,alfa,a12bta,a14,a24
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)

      dimension  ener(1)

c   if using dp , set flag bit upon compression

      if ((idlflg .eq. 1) .and. (ncycle .ne. 0)) then
         do 35 i = lft,llt
           if (xmu(i) .gt. .000001) then
c             ibtflg(i) = 1b                                            cray1
           endif
   35    continue
      endif

c   if using dp , set pmin to 0.0 before compression

      if (idlflg .eq. 1) then
        do 5 i = lft,llt
          if(ibtflg(i) .eq. 0) pc(i) = 0.0
   5    continue
      endif

      do 10 i=lft,llt
   10 pnew(i)=(f1(i)+sie(i)*(f2(i)+sie(i)*(f3(i)+sie(i)*f4(i))))*fa(i)/
     1 (f5(i)+sie(i)*(f6(i)+sie(i)*f7(i)))
      do 20 i=lft,llt
   20 pnew(i)=  max(pnew(i),pc(i))
      do 30 i=lft,llt
      epx2(i)=epx2(i)-pnew(i)*dvol(i)
   30 sie(i)=xm(i)*epx2(i)
      do 40 i=lft,llt
   40 pnew(i)=(f1(i)+sie(i)*(f2(i)+sie(i)*(f3(i)+sie(i)*f4(i))))*fa(i)/
     1 (f5(i)+sie(i)*(f6(i)+sie(i)*f7(i)))
      do 50 i=lft,llt
   50 pnew(i)=  max(pnew(i),pc(i))

c   if using dp , store bit flag into last bit of sie

      if (idlflg .eq. 1) then
        do 55 i = lft,llt
c         sie(i) = ( sie(i) .and. 1777777777777777777776b )             cray1
c    1                      .or. ibtflg(i)                              cray1
   55   continue
      idlflg = 0
      endif

      do 60 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   60 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 70 i=lft,llt
   70 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine eqos6(fval)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),epx1(128),epx2(128),epx3(128),epx4(128)
      common/aux15/
     & qp(128),specen(128),dvol(128),volold(128),r2v(128),
     1 r1v(128),w1df(128),facm(128),pnew(128)
      common/aux43/xm(128),p(128),xmua(128)
      common/aux36/lft,llt
      common/eos10/c1,c2,c3,c4,c5,c6,c7,e0,v0,xcv
      dimension fval(1)
      lcv=xcv
      einc=fval(lcv)*dt1
      do 10 i=lft,llt
      specen(i)=specen(i)+einc
   10 epx2(i)=epx2(i)+einc/xm(i)
      return
      end
      subroutine eqos7(ener,mte)
c     implicit double precision (a-h,o-z)                                    dp
c
c     s p e c i a l   tarver/cochran   h e   e o s  (dyna3d version)
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk53/sram,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
      common/bk56/dpdfc,drdta2,det2df,tem1,heat
      common/bk59/delvv,dpdmu,dpde
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128),
     1 kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
      common/aux36/lft,llt
      common/eosd/pc(128),shrm(128)
      data zero /0.0/
c
      dimension ener(1)
c
c     heterogeneous he burn model
c     nucleation  dependent on q, (1-f)**s
c     growth dependent on p**m,eta2,f**r,(1-f)**s
c
c     pressure and temperature equilibrium
c
c     complete, jwl equations of state
c     burn is operator split from hydro
c
c     tmp  = temperature
c     cv   = heat capacity - - mbars/kelvin
c     dedv = de/dv at fixed fc and t with pressure equil.
c     fc   = fraction of he burned by mass
c     enq  = he energy density
c
c     update temperature for plastic work
c
      if (mte.ne.9) call tupdte
c
      do 10 nml=lft,llt
c
      call intrhe (mte,nml,df)
c
      tem1=(pres+dedv+qq)/cv
      tmp=tmp-tem1*delvv
c
c     iterate for pressure equilibrium
c
      call mixfe (df)
c
c     second energy and temperature iteration
c
      trans=(pres+dedv+qq)/cv
      tmp=tmp-.5*(trans-tem1)*delvv
c
      call mixfe (df)
c
c     store new values for hydro
c
      tem1=fc
c
c     calculate new burn fraction,fc
c
      call burnfe (dtburn)
c
c     update temperature for next cycle
c
      tmp=tmp+(fc-tem1)*heat/cv
c
c     time step control for accuracy
c
      dt2=  min(dt2,.67*dtburn)
c
      csq=  max(zero,dpdmu+abs(pres)*df*df*dpde)+1.33*shrm(nml)
   10 call storhe (mte,nml,ener(mxt(lft)))
      return
      end
      subroutine intrhe(mte,n,df)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux11/po(128)
      common/aux14/sig(128,7),aux(128,13)
      common/aux15/
     &qp(128),specen(128),dvol(128),volold(128),r2v(128)
      common/aux18/dd(128),def(128)
      common/aux19/
     & sp,bfac(128),xnum(128),xdem(128),dfmu(128),dfmu2(128)
      common/aux20/
     & pdenro(128),pnumro(128),facp(128),facn(128),xdi2(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux43/xm(128)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk53/sram,mx,ix(6),stress(4),epx(20)
      common/bk59/delvv,dpdmu,dpde
      mx=mxt(n)
      nc=nconst(mte)
      stress(1)=po(n)
      do 10 i=1,8
   10 epx(i)=aux(n,i+nc)
      epx(9)=-.5*(aux(n,13)+qp(n))
      df=def(n)
      delvv=2.*dvol(n)*xm(n)
      return
      end
      subroutine storhe(mte,n,ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk53/dx,mx,ix(4),ilcc,ncon,stress(4),epx(20)
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),sign5(128),
     2 sign6(128),sign7(128),aux(128,8)
      common/aux15/
     &qp(128),specen(128),dvol(128),volold(128),r2v(128)
      common/aux16/tempra(128)
      nc=nconst(mte)
      aux(n,1)=aux(n,1)-stress(1)*dvol(n)
      ener=ener+aux(n,1)
      tempra(n)=epx(4)
      sign1(n)=sign1(n)-stress(1)
      sign2(n)=sign2(n)-stress(1)
      sign3(n)=sign3(n)-stress(1)
      do 10 i=1,8
   10 aux(n,i+nc)=epx(i)
      if (mte.eq.9) sign7(n)=epx(1)
      return
      end
      subroutine tupdte
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux14/sig(128,7),aux(128,8)
      common/aux15/qp(128),specen(128),dvol(128),volold(128),
     1 sig1(128),sig2(128),sig3(128),sig4(128),einc(128)
      common/aux36/lft,llt
      dt50=.50*dt1
      do 10 i=lft,llt
   10 aux(i,4)=aux(i,4)+dt50*(sig1(i)+sig2(i)+sig3(i)+sig4(i))/aux(i,6)
      return
      end
      subroutine burnfe (dtburn)
c     implicit double precision (a-h,o-z)                                    dp
c
c     this is burn routine
c     burn fraction fc is calculated implicitely
c     fdot=rate*(fcrit-fc)**s+grow*fc**r(1-fc)**s(p**m)*eta2**e
c     rate=freq*qq
c
c     hot spot nuc.and growth
c
c     cappa limits the burn in a shock
c
      common/bk56/dpdfc,drdta2,det2df,tem1,heat
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      common/bk02/iburn,dt1,dt2,isdo
      common/bk53/dx,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
c
      data tol/1.e-10/
      data zero /0.0/
c
      cappa=temsc(6)
c
c     chi limits the burn per cycle for accuracy of temp calc.
c
      chi=temsc(7)
c
      if (pres.le.0.) go to 30
      if (qq.ge.(cappa*pres)) go to 30
      if (fc.ge.fcrit) go to 10
c
c     nucleation
c     q dependent rate law
c     implicit in fc
c
c     burn is inhibited when q is less than qcrit
c
      rate=freq*(eta1-1.0-qcrit)**3*(1.0+frer*(eta1-1.0-qcrit))
c
      if ((eta1-1.0-qcrit).lt.0.) rate=0.
      if (qq.le.0.) rate=0.
      alpha=fcrit-fc
      rnn=((alpha+tol)**es)*rate
      if (rnn.eq.0.) go to 30
      drdf=-es*rnn/(alpha+tol)
      drdflm=drdf/(2.+drdf*dt1)
      fci=1.+(rnn*dt1-(1.-fc)*(1.-drdflm*dt1))/(1.-drdflm*dt1)-fc
      fc=fc+  max(fci,zero)
      transn=  max(rnn,drdf)
c
c     growth
c
   10 alpha=1.-fc
      if (alpha.le.0.) go to 20
      rng=grow*(pres**em)*((fc+tol)**ar)*((alpha+tol)**es)*((eta2+tol)*
     1 *eeta2)
      drdf=ar*rng/(fc+tol)-es*rng/(alpha+tol)
      drdp=em*rng/pres
      drdta2=eeta2*rng/(eta2+tol)
      dpdfc=  max(zero,dpdfc)
      det2df=  max(zero,det2df)
      drdf=drdf+drdp*dpdfc+drdta2*det2df
      drdflm=drdf/(2.+drdf*dt1)
      fci=1.+(rng*dt1-(1.-fc)*(1.-drdflm*dt1))/(1.-drdflm*dt1)-fc
      fc=fc+  max(fci,zero)
      if (fc.ge.1.0) go to 20
      transg=  max(rng,drdf)
      dtburn=1.0/  max(transn,transg)
c
c     limit on change in fcper cycle
c
      if ((fc-tem1).le.chi) return
      fc=tem1+  max(chi,zero)
      return
   20 fc=1.
   30 dtburn=1.e20
      return
      end
      subroutine mixfe (df)
c     implicit double precision (a-h,o-z)                                    dp
c
c     this routine iterates for pressure equilibrium
c     and calculates needed derivatives
c     delta (=p1-p2) is a function of beta (=v1/v)
c     eta1=(1.-fc)*eta/beta
c
c     accuracy is epslon iter < itrmax
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk56/dpdfc,drdta2,det2df,tem1,heat
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      common/bk53/dx,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/ab(2),xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,
     1 em,ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
      common/bk59/delvv,dpdmu,dpde
      common/double/iprec,ncpw,unit
c
      dimension ttys(10)
c
      det2df=0
      etac=1./df
      epslon=temsc(3)
      itrmax=temsc(4)
      check=temsc(5)
      check1=1.-check
c
      iter=0
      fc1=1.-fc
      if (fc.lt.check) go to 50
      if (fc.gt.check1) go to 60
      if (beta.eq.1.) eta2=eta1
      beta=fc1*eta2/(fc1*eta2+fc*eta1)
      guess=beta
c
c     begin iteration
c
   10 beta2=1.-beta
      eta1=fc1*etac/beta
c
      call rctfe
c
      eta2=fc*etac/beta2
c
      call prodfe
c
      pmax=  max(preact,pprod)
      delta=(preact-pprod)
      if (abs(delta/pmax).le.epslon) go to 70
      iter=iter+1
      if (iter.gt.30) go to 20
c
c     newton's method
c
      trans1=bth1*eta1/beta
      trans2=bth2*eta2/beta2
      slope=-(trans1+trans2)
      beta=beta-delta/slope
      if (beta.le.0..or.beta.ge.1.) beta=fc1
      go to 10
   20 iter=0
      beta=guess-.10
      if (beta.lt.0.00001) beta=0.00001
      tl=beta
      call ftnchm (iter,beta2,delta,fc1,etac,epslon)
      a=sign(1.*unit,delta)
      beta=beta+.20
      beta=  min(beta,.99999*unit)
      tu=beta
      call ftnchm (iter,beta2,delta,fc1,etac,epslon)
      b=sign(1.*unit,delta)
      if (a.ne.b) go to 40
      beta=.999999
      tl=beta
      call ftnchm (iter,beta2,delta,fc1,etac,epslon)
      a=sign(1.*unit,delta)
   30 beta=beta-.05
      beta=  max(beta,.000001*unit)
      tu=beta
      call ftnchm (iter,beta2,delta,fc1,etac,epslon)
      b=sign(1.*unit,delta)
      if (a.ne.b) go to 40
      tl=tu
      go to 30
   40 beta=.5*(tl+tu)
      call ftnchm (iter,beta2,delta,fc1,etac,epslon)
      if (delta.eq.0.0) go to 70
      if (iter.gt.200) go to 80
      c=sign(1.*unit,delta)
      if (c.eq.b) tu=beta
      if (c.eq.a) tl=beta
      go to 40
c
c     end iteration
c
   50 eta1=fc1*etac
c
      call rctfe
c
      beta=1.
      pres=preact
      eta2=1.e-06
      cv=cvract
c
      call prodfe
c
      dedv=dedv1
      heat=enq-en2+en1
      dpdfc=dpdt1*heat/cv
      dpde=dpdt1/cv
      dpdmu=bth1+dpdt1*dedv/(cv*etac*etac)
      return
c
   60 eta2=fc*etac
c
      call prodfe
c
      cv=cvprod
      beta=0.
      pres=pprod
      heat=enq
      dedv=dedv2
      dpdfc=dpdt2*heat/cv
      dpde=dpdt2/cv
      dpdmu=bth2+dpdt2*dedv/(cv*etac*etac)
      return
c
c     mixture formulas
c
   70 pres=(preact+pprod)*.5
c
c     all derivatives are evaluated at pressure equil.
c     this involves d(beta)/d(eta), d(beta)/d(tmp), d(beta)/d(fc)
c
      trans1=bth1/beta
      trans2=bth2/beta2
      trans=trans1*eta1+trans2*eta2
c
c     dbdeta=d(beta)/d(eta) to keep pressure equil.
c
      dbdeta=(trans1*fc1-trans2*fc)/trans
c
c     deta1 is d(eta1)/d(eta) to keep pressure equil.
c
      deta1=(fc1-eta1*dbdeta)/beta
      deta2=(fc+eta2*dbdeta)/beta2
c
c     ded1 is d(e1)/d(eta1)
c
      ded1=-dedv1/(eta1*eta1)
      ded2=-dedv2/(eta2*eta2)
      dedv=-etac*etac*(fc1*ded1*deta1+fc*ded2*deta2)
      bth=(bth1*deta1+bth2*deta2)*.5
c
c     dbdt is d(beta)/d(tmp) to keep pressure equil.
c
      dbdt=(dpdt1-dpdt2)/trans
      det1dt=-eta1*dbdt/beta
      det2dt=eta2*dbdt/beta2
      cv10=cvract+ded1*det1dt
      cv20=cvprod+ded2*det2dt
      cv=fc1*cv10+fc*cv20
      dpdt=.5*(dpdt1+dpdt2+dbdt*(trans2*eta2-trans1*eta1))
c
c     dbdf = d(beta)/d(fc) to keep pressure equil
c
      dbdf=-(trans1+trans2)*etac/trans
      det1df=-(etac+eta1*dbdf)/beta
      det2df=(etac+eta2*dbdf)/beta2
      dpdfc=.5*(bth1*det1df+bth2*det2df)
c
c     heat is used by burn
c
      heat=enq-en2+en1-fc*ded2*det2df-fc1*ded1*det1df
      dpdfc=dpdfc+.5*(dpdt1+dpdt2)*heat/cv
c
c     bth  = dp/dmu at constant tmp
c     dpdmu= dp/dmu at constant energy
c     dpde = dpdt/cv
c     dpdmu= bth-dpde*dedmu
c
      dpde=dpdt/cv
      dpdmu=bth+dpdt*dedv/(cv*etac*etac)
      return
c
   80 write(13,90)
      write ( *,90)
      call adios (2)
c
   90 format('no convergence in subroutine mixfe')
      end
      subroutine ftnchm(iter,beta2,delta,fc1,etac,epslon)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk56/dpdfc,drdta2,det2df,tem1,heat
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      common/bk53/dx,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
c
      iter=iter+1
      beta2=1.-beta
      eta1=fc1*etac/beta
      call rctfe
      eta2=fc*etac/beta2
      call prodfe
      pmax=  max(preact,pprod)
      delta=(preact-pprod)
      if (abs(delta/pmax).le.epslon) go to 10
      return
   10 delta=0.0
      return
      end
      subroutine prodfe
c     implicit double precision (a-h,o-z)                                    dp
c
c     product eos
c     p=a*exp(-xp1/eta)+b*exp(-xp2/eta)+g*eta*tmp
c     cvp is product heat capacity
c
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk53/dx,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
c
      trans1=a*exp(-xp1/eta2)
      trans2=b*exp(-xp2/eta2)
      dedv2=-trans1-trans2
      pprod=-dedv2+g*eta2*tmp
      cvprod=cvp
      bth2=g*tmp+(xp1*trans1+xp2*trans2)/eta2**2
      dpdt2=g*eta2
      en2=trans1/xp1+trans2/xp2+cvprod*tmp
      return
      end
      subroutine rctfe
c     implicit double precision (a-h,o-z)                                    dp
c
c     reactant eos
c     p=r1*exp(-r5/eta)+r2*exp(-r6/eta)+r3*eta*tmp
c     cvr is reactant heat capacity
c
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      common/bk53/dx,mx,ix(6),pres,sig(3),tmp,dedv,cv,fc,beta,
     1 eta1,eta2,csq,qq,epx(12),eps(5),exx(5)
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
      trans1=r1*exp(-r5/eta1)
      trans2=r2*exp(-r6/eta1)
      dedv1=-trans1-trans2
      preact=-dedv1+r3*eta1*tmp
      cvract=cvr
      bth1=r3*tmp+(r5*trans1+r6*trans2)/eta1**2
      dpdt1=r3*eta1
      en1=trans1/r5+trans2/r6+cvract*tmp
      return
      end
      subroutine eqos8 (ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),sign7(128),epx2(128),aux(128,13)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dd(128),def(128)
      common/aux20/rvj1(128),rvj(128),cej1(128),cej(128),tej1(128),
     1 tej(128),buj1(128),buj(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/aux40/
     & defm(128),defln(128),bulk(128),factor(128),cep(128),
     & pres(128),puld(128),pnew(128),tec(128),fc(128),fc1(128),
     & fjk(128)
      common/aux43/xm(128)
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      common/eosd/pc(128),shrm(128)
c
      dimension ener(1)
c
      do 10 i=lft,llt
      fc(i)=aux(i,nc)-defln(i)
   10 pnew(i)=(cep(i)+specen(i)*tec(i))/(1.+tec(i)*dvol(i)*xm(i))
      do 20 i=lft,llt
      puld(i)=pnew(i)+bulk(i)*fc(i)
      if(fc(i).lt.0.) pnew(i)=puld(i)
   20 pnew(i)=  max(pnew(i),pc(i))
      do 30 i=lft,llt
      aux(i,nc)=defm(i)
   30 epx2(i)=epx2(i)-pnew(i)*dvol(i)
      do 40 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   40 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 50 i=lft,llt
   50 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine eqos9 (ener)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),sign7(128),epx2(128),aux(128,13)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dd(128),def(128)
      common/aux20/rvj1(128),rvj(128),cej1(128),cej(128),tej1(128),
     1 tej(128),buj1(128),buj(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      common/aux40/
     & defm(128),defln(128),bulk(128),factor(128),cep(128),
     & pres(128),puld(128),pnew(128),tec(128),fc(128),fc1(128),
     & fjk(128)
      common/aux43/xm(128)
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      common/eosd/pc(128),shrm(128)
c
      dimension ener(1)
c
      do 10 i=lft,llt
   10 pnew(i)=(cep(i)+specen(i)*tec(i))/(1.+tec(i)*dvol(i)*xm(i))
      do 20 i=lft,llt
   20 pnew(i)=  max(pnew(i),pc(i))
      do 30 i=lft,llt
   30 epx2(i)=epx2(i)-pnew(i)*dvol(i)
      do 40 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
   40 sign3(i)=sign3(i)-pnew(i)
      mx=mxt(lft)
      do 50 i=lft,llt
   50 ener(mx)=ener(mx)+epx2(i)
      return
      end
      subroutine femdmp (iflag)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp(1),numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm(1),itemp,ntmp0,ntmp1
      common/bk02/iburn(1),dt1,dt2,isdo
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout(1),pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk05/
     1 nh01(1),nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iaddp(1),ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk07/n1(1),n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname,lendf
      common/bk08/n4a(1),n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/b10a/inpsd(1),nmmtde,nmelde,nummas
      common/b10b/ld01(1),ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk16/numcc(1),numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv(1),ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint(1)
      common/bk27/irestr(1),irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss(1),xke,xpe,tt
      common/bk76/nthpx(1),nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax(1),gay,gaz,slx,sly,slz
      common/bk78/njoydf(1),nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/thfl/madd(1),ifth,maddbg
      common/sbc0/yield(1),hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal(1),shob,range(64),stoa(64),pkp(64)
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/bttn/ntnwf(1),ixa(10)
      common/bktb/ntbsl(1),nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/beam/ pid4(1),xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/tsbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),szeta(5,5)
      common/ssbsis/sh(8,5,5),spr(8,5,5),sps(8,5,5),spt(8,5,5),ipts,
     1 nips,swgts(5,5),sszeta(5,5)
      common/rigidb/ nodtot(1), ncflag,numrbi
      common/rn/irnxx(1)
      common/shlopt/istrn(1),istupd,ibelyt,miter
      common/tmstp/tsmin(1),hmax,lctm
      logical ffifp
      common/slfl/nadd(1),numsls,ffifp
      common/tint/itsc(1),itbs,itlc
      common/tbsysp/numrcf(1),n6e,n6f,n6g
      common/bmsc/numsl(20)
      common/raydmp/dampm,ndamp
      common/csforc/ncs1(1),ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,future(10)
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      common/extwrk/locaxa,exwork,iewflg
      common/tsarry/tslimt,tsarry(144)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/fixnod/numfx1,numfx2,numfx3,numfx4,numfx5,numfx6
     1 ,numfx7,numfx8,numfx9,numfx10,numfx11,numfx12
      character*8 namef                                                 vax75
      common/double/iprec,ncpw,unit
      common /   / a(1)
c
c     set dumpfile size at first access
c     if(iname.eq.0) then                                               ltss
c          lendh=20+4+4+9   +13+40+32+83+14+26+8+76+10+1+6+4+6+6+7+2+11 ltss
c    1  +12+194+483+852+852+2+4+9+3+11+16+1+3+3+4+4+20+26+15+30+148+19  ltss
c          lendf=lendh+locend                                           ltss
c          call famsiz(iob2,lendf)                                      ltss
c     endif                                                             ltss
      lendf=maxsiz                                                      vaxcs
      lendh=20+4+4+9   +13+40+44+83+14+26+8+76+10+1+6+4+6+6+7+2+11      vax75
     1  +12+194+483+852+852+2+4+9+3+11+16+1+3+3+4+4+20+26+15+30+148+19  vax75
      lendat=lendh+locend                                               vax75
      nfil=lendat/maxsiz+1                                              vax75
c     nfil=1                                                            ltss
c
      if(iname.eq.0) then
      iname=1
      else
      iname=iname+nfil
      endif
      ifctor=iaddp(1)/maxsiz
      iaddp(1)=(ifctor+1)*maxsiz
      iadd=iname*lendf
      ifctor=madd(1)/maxsiz
      madd(1)=(ifctor+1)*maxsiz
      ifctor=nadd(1)/maxsiz
      nadd(1)=(ifctor+1)*maxsiz
c
      len=19+1*iprec
      call wrabsf (iob2,numnp,len,iadd)
      call riosta (iob2)
      call getfn (2,namef)
      iadd=iadd+len
      call wrabsf (iob2,itherm,4,iadd)
      call riosta (iob2)
      iadd=iadd+4
      len=2+2*iprec
      call wrabsf (iob2,iburn,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=6+3*iprec
      call wrabsf (iob2,ndthl,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=8+5*iprec
      call wrabsf (iob2,prtout,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf(iob2,nh01,40,iadd)
      call riosta (iob2)
      iadd=iadd+40
c     len=32                                                            cray1
      len=28+16*iprec                                                   vax75
      call wrabsf (iob2,iaddp,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf (iob2,n1,83,iadd)
      call riosta (iob2)
      iadd=iadd+83
      call wrabsf (iob2,n4a,14,iadd)
      call riosta (iob2)
      iadd=iadd+14
      call wrabsf (iob2,lc0,26,iadd)
      call riosta (iob2)
      iadd=iadd+26
      call wrabsf (iob2,numcc,8,iadd)
      call riosta (iob2)
      iadd=iadd+8
      call wrabsf (iob2,nconst,76,iadd)
      call riosta (iob2)
      iadd=iadd+76
      call wrabsf (iob2,numsv,10,iadd)
      call riosta (iob2)
      iadd=iadd+10
      call wrabsf (iob2,nint,1,iadd)
      call riosta (iob2)
      iadd=iadd+1
      call wrabsf (iob2,irestr,6,iadd)
      call riosta (iob2)
      iadd=iadd+6
      len=4*iprec
      call wrabsf (iob2,summss,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf (iob2,nthpx,6,iadd)
      call riosta (iob2)
      iadd=iadd+6
      len=6*iprec
      call wrabsf (iob2,gax,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf (iob2,njoydf,7,iadd)
      call riosta (iob2)
      iadd=iadd+7
      call wrabsf (iob2,madd,2,iadd)
      call riosta (iob2)
      iadd=iadd+2
      len=2+9*iprec
      call wrabsf (iob2,yield,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=194*iprec
      call wrabsf (iob2,scal,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=483*iprec
      call wrabsf (iob2,pid4,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=2+850*iprec
      call wrabsf (iob2,h,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=2+850*iprec
      call wrabsf (iob2,sh,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf (iob2,nodtot,2,iadd)
      call riosta (iob2)
      iadd=iadd+2
      call wrabsf(iob2,inpsd,4,iadd)
      call riosta (iob2)
      iadd=iadd+4
      call wrabsf(iob2,ld01,9,iadd)
      call riosta (iob2)
      iadd=iadd+9
      call wrabsf(iob2,istrn,4,iadd)
      call riosta (iob2)
      iadd=iadd+4
      call wrabsf(iob2,ntnwf,11,iadd)
      call riosta (iob2)
      iadd=iadd+11
      call wrabsf(iob2,ntbsl,16,iadd)
      call riosta (iob2)
      iadd=iadd+16
      call wrabsf(iob2,irnxx,1,iadd)
      call riosta (iob2)
      iadd=iadd+1
      len=1+2*iprec
      call wrabsf(iob2,tsmin,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf(iob2,nadd,3,iadd)
      call riosta (iob2)
      iadd=iadd+3
      call wrabsf(iob2,itsc,3,iadd)
      call riosta (iob2)
      iadd=iadd+3
      call wrabsf(iob2,numrcf,4,iadd)
      call riosta (iob2)
      iadd=iadd+4
      call wrabsf(iob2,numsl,20,iadd)
      call riosta (iob2)
      iadd=iadd+20
      len=1+iprec
      call wrabsf(iob2,dampm,len,iadd)
      call riosta(iob2)
      iadd=iadd+len
      len=10+12*iprec
      call wrabsf(iob2,ncs1,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=13+2*iprec
      call wrabsf(iob2,ilink,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      call wrabsf(iob2,nnc,30,iadd)
      call riosta (iob2)
      iadd=iadd+30
      len=145*iprec
      call wrabsf(iob2,tslimt,len,iadd)
      call riosta (iob2)
      iadd=iadd+len
      len=2+1*iprec
      call wrabsf(iob2,locaxa,len,iadd)
      call riosta(iob2)
      iadd=iadd+len
      call wrabsf(iob2,n30a,7,iadd)
      call riosta(iob2)
      iadd=iadd+7
      call wrabsf(iob2,numfx1,12,iadd)
      call riosta(iob2)
      iadd=iadd+12
c
c     write blank common
c
      len=locend*iprec
      call wrabsf (iob2,a(1),len,iadd)
      call riosta (iob2)
      iadd=iadd+len
c
      iadprt=iadd-iname*lendf
      write ( *,40) namef,iadprt
      write (13,40) namef,iadprt
c
      if (iflag.eq.1) return
c
      call adios (1)
c
c
   40 format (//' restart file',1x,a8,' written, ',i8,' words'//)
      end
      subroutine runrsd
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp(1),numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm(1),itemp,ntmp0,ntmp1
      common/bk02/iburn(1),dt1,dt2,isdo
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout(1),pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk05/
     1 nh01(1),nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iaddp(1),ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk07/n1(1),n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname,lendf
      common/bk08/n4a(1),n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/b10a/inpsd(1),nmmtde,nmelde,nummas
      common/b10b/ld01(1),ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk16/numcc(1),numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv(1),ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint(1)
      common/bk27/irestr(1),irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss(1),xke,xpe,tt
      common/bk76/nthpx(1),nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax(1),gay,gaz,slx,sly,slz
      common/bk78/njoydf(1),nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/thfl/madd(1),ifth,maddbg
      common/sbc0/yield(1),hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal(1),shob,range(64),stoa(64),pkp(64)
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/bttn/ntnwf(1),ixa(10)
      common/bktb/ntbsl(1),nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/beam/ pid4(1),xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/tsbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),szeta(5,5)
      common/ssbsis/sh(8,5,5),spr(8,5,5),sps(8,5,5),spt(8,5,5),ipts,
     1 nips,swgts(5,5),sszeta(5,5)
      common/rigidb/ nodtot(1), ncflag,numrbi
      common/rn/irnxx(1)
      common/shlopt/istrn(1),istupd,ibelyt,miter
      common/tmstp/tsmin(1),hmax,lctm
      logical ffifp
      common/slfl/nadd(1),numsls,ffifp
      common/tint/itsc(1),itbs,itlc
      common/tbsysp/numrcf(1),n6e,n6f,n6g
      common/bmsc/numsl(20)
      common/raydmp/dampm,ndamp
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      common/csforc/ncs1(1),ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,future(10)
      common/extwrk/locaxa,exwork,iewflg
      common/tsarry/tslimt,tsarry(144)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/fixnod/numfx1,numfx2,numfx3,numfx4,numfx5,numfx6
     1 ,numfx7,numfx8,numfx9,numfx10,numfx11,numfx12
      common/double/iprec,ncpw,unit
      character*8 namef                                                 vax75
      common /   / a(1)
c                                                                       ltss
c     data jname/0/                                                     ltss
c
c     set dumpfile size at first access
c     if(jname.eq.0) then                                               ltss
c          lendh=20+4+4+9   +13+40+32+83+14+26+8+76+10+1+6+4+6+6+7+2+11 ltss
c    1  +12+194+483+852+852+2+4+9+3+11+16+1+3+3+4+4+20+26+15+30+148+19  ltss
c          lendf=lendh+locend                                           ltss
c          jname=1                                                      ltss
c          call famsiz(iob14,lendf)                                     ltss
c     endif                                                             ltss
      lendf=maxsiz                                                      vaxcs
c
      iadd=0
c
      len=19+1*iprec
      call wrabsf(iob14,numnp,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call getfn (14,namef)
      call wrabsf(iob14,itherm,4,iadd)
      call riosta(iob14)
      iadd=iadd+4
      len=2+2*iprec
      call wrabsf(iob14,iburn,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=6+3*iprec
      call wrabsf(iob14,ndthl,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=8+5*iprec
      call wrabsf(iob14,prtout,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,nh01,40,iadd)
      call riosta(iob14)
      iadd=iadd+40
c     len=32                                                            cray1
      len=28+16*iprec                                                   vax75
      call wrabsf(iob14,iaddp,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,n1,83,iadd)
      call riosta(iob14)
      iadd=iadd+83
      call wrabsf(iob14,n4a,14,iadd)
      call riosta(iob14)
      iadd=iadd+14
      call wrabsf(iob14,lc0,26,iadd)
      call riosta(iob14)
      iadd=iadd+26
      call wrabsf(iob14,numcc,8,iadd)
      call riosta(iob14)
      iadd=iadd+8
      call wrabsf(iob14,nconst,76,iadd)
      call riosta(iob14)
      iadd=iadd+76
      call wrabsf(iob14,numsv,10,iadd)
      call riosta(iob14)
      iadd=iadd+10
      call wrabsf(iob14,nint,1,iadd)
      call riosta(iob14)
      iadd=iadd+1
      call wrabsf(iob14,irestr,6,iadd)
      call riosta(iob14)
      iadd=iadd+6
      len=4*iprec
      call wrabsf(iob14,summss,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,nthpx,6,iadd)
      call riosta(iob14)
      iadd=iadd+6
      len=6*iprec
      call wrabsf(iob14,gax,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,njoydf,7,iadd)
      call riosta(iob14)
      iadd=iadd+7
      call wrabsf(iob14,madd,2,iadd)
      call riosta(iob14)
      iadd=iadd+2
      len=2+9*iprec
      call wrabsf(iob14,yield,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=194*iprec
      call wrabsf(iob14,scal,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=483*iprec
      call wrabsf(iob14,pid4,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=2+850*iprec
      call wrabsf(iob14,h,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=2+850*iprec
      call wrabsf(iob14,sh,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,nodtot,2,iadd)
      call riosta(iob14)
      iadd=iadd+2
      call wrabsf(iob14,inpsd,4,iadd)
      call riosta(iob14)
      iadd=iadd+4
      call wrabsf(iob14,ld01,9,iadd)
      call riosta(iob14)
      iadd=iadd+9
      call wrabsf(iob14,istrn,4,iadd)
      call riosta(iob14)
      iadd=iadd+4
      call wrabsf(iob14,ntnwf,11,iadd)
      call riosta(iob14)
      iadd=iadd+11
      call wrabsf(iob14,ntbsl,16,iadd)
      call riosta(iob14)
      iadd=iadd+16
      call wrabsf(iob14,irnxx,1,iadd)
      call riosta(iob14)
      iadd=iadd+1
      len=1+2*iprec
      call wrabsf(iob14,tsmin,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,nadd,3,iadd)
      call riosta(iob14)
      iadd=iadd+3
      call wrabsf(iob14,itsc,3,iadd)
      call riosta(iob14)
      iadd=iadd+3
      call wrabsf(iob14,numrcf,4,iadd)
      call riosta(iob14)
      iadd=iadd+4
      call wrabsf(iob14,numsl,20,iadd)
      call riosta(iob14)
      iadd=iadd+20
      len=1+iprec
      call wrabsf(iob14,dampm,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=10+12*iprec
      call wrabsf(iob14,ncs1,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      len=13+2*iprec
      call wrabsf(iob14,ilink,len,iadd)
      call riosta (iob14)
      iadd=iadd+len
      call wrabsf(iob14,nnc,30,iadd)
      call riosta (iob14)
      iadd=iadd+30
      len=145*iprec
      call wrabsf(iob14,tslimt,len,iadd)
      call riosta (iob14)
      iadd=iadd+len
      len=2+1*iprec
      call wrabsf(iob14,locaxa,len,iadd)
      call riosta(iob14)
      iadd=iadd+len
      call wrabsf(iob14,n30a,7,iadd)
      call riosta(iob14)
      iadd=iadd+7
      call wrabsf(iob14,numfx1,12,iadd)
      call riosta(iob14)
      iadd=iadd+12
c
c     write blank common
c
      len=locend*iprec
      call wrabsf(iob14,a(1),len,iadd)
      call riosta(iob14)
c
      iadprt=len+iadd
      write ( *,40) namef,ncycle,iadprt
      write (13,50) namef,ncycle,iadprt
      return
c
   40 format( ' running restart file ',a8,' written at step',i8,
     1                   /i8,' words written')
   50 format(//' running restart file ',a8,' written at step',i8,
     1                   /i8,' words written')
      end
      subroutine beamel
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
      common/   /a(1)
      call elem1d (a(lc1b),a(lc11),a(lc10),a(lc9),a(lc9+neq),
     1a(nb03),a(nb04),a(nb05),a(nb06),a(n1),a(n2),a(n3),a(n4f),
     2a(nb08),a(nb09),a(nb10),a(n4c),a(n4c+nmmat),a(n4g),mpubr,
     3a(nb07),a(n25+7),a(n1+nmmat),a(nb02),a(nb14),a(nb17),a(nb11),
     4a(nb12),a(n80),a(n80+nmmat),a(n80+2*nmmat))
      return
      end
      subroutine elem1d(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,ener,ibemfm,xlngth,lochvb,nbeam,nncb,
     3 iblkb,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      character*4 mess                                                  vax75
      common/aux38/mess
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/energy/xinen
      character*6 elname
      common/elnme/ elname(5)
      common/eltspf/ietspf
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/   /a(1)
      dimension ixp(4,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),xyz(12),
     2 fibl(5,*),ybarn(12),strr(6,*),nsubgv(*),mtnum(*),xlngth(*),
     3 nfegp(*),ihgq(*),hgq(*),rule(mpubr,3,*),ener(*),ibemfm(*),
     4 lochvb(*),nbeam(*),nncb(*),iblkb(*),xies(*),dampk(*),ym(*),prv(*)
      lcz=0
      ic =0
      lczc=0
      nelg=(numelb-1)/128+1
      nmel=128
      nrng=24*nmel
      call azero (strr,6*numelb)
      do 410 nn=1,nelg
      do 10 i=1,nrng
   10 ep11(i)=0.
      noco=nn
      nnm1=128*(nn-1)
      nmel=128
      if (nn.eq.nelg) nmel=numelb-128*(nelg-1)
      call unpkbm (ixp(1,nnm1+1),nmel)
      nsubg=nsubgv(nn)
      do 400 n=1,nsubg
      ic =ic+1
      nnc=nncb(ic)
      lcn=lcz+n
      mxe=mtnum(lcn)
      mte=mtype(mxe)
      if (mxe.eq.0.or.mte.eq.20) go to 390
      iop=ibemfm(mxe)
      lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvb(nnm1+lft)
c
      if (iop.eq.1) then
      call hughbm(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,iblkb,dampk(mxe),
     3 ym(mxe),prv(mxe))
      endif
      if (iop.eq.2) then
      call belybm(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,xlngth,nbeam,
     3 iblkb,dampk(mxe),ym(mxe),prv(mxe))
      endif
      if (iop.eq.3) then
      call truss(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,iblkb,dampk(mxe),
     3 ym(mxe),prv(mxe))
      endif
c
c     print out computed step size for each element at time=0
c
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      do 60 i=lft,llt
      sndsp(i)=sqrt(sarea(i))*sndspd
      dt2=  min(sndsp(i),dt2)
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ielmtc=nbeam(ielmtc)
      ityptc=2
      endif
      ielmtd=nnm1+i
      ielmtd=nbeam(ielmtd)
      write (13,420) ielmtd,sndsp(i)
      write (13,430)elname(ityptc),ielmtc
   60 continue
      endif
      endif
      endif
c
      mxf = mxe+nmmat
      mxg=2*nmmat+3*(mxe-1)+1
      mxh=2*nmmat+3*(mxe-1)+2
      mxi=2*nmmat+3*(mxe-1)+3
      enere=ener(mxe)
      enerf=ener(mxf)
      energ=ener(mxg)
      enerh=ener(mxh)
      eneri=ener(mxi)
      do 280 i=lft,llt
      sqrta   =sqrt(sarea(i))
      sndsp(i)=sqrta*sndspd
      dt2=  min(dt2,sndsp(i))
      xinen    =xinen+xies(nnm1+i)
      enere=enere+xies(i+nnm1)
      enerf=enerf+sqrta*enkint(i)
      energ=energ+sqrta*xmomnt(i)
      enerh=enerh+sqrta*ymomnt(i)
      eneri=eneri+sqrta*zmomnt(i)
  280 continue
      ener(mxe)=enere
      ener(mxf)=enerf
      ener(mxg)=energ
      ener(mxh)=enerh
      ener(mxi)=eneri
      if (mess.eq.'sw2.') then
      do 290 i=lft,llt
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ielmtc=nbeam(ielmtc)
      ityptc=2
      endif
  290 continue
      endif
  390 lczc=lczc+nnc+1
      if (output) then
      call acmfcb (a(ncs1),a(ncs2),a(ncs5),a(ncs7),a(ncs9),numcsd,
     1 nnm1)
      endif
  400 continue
      lcz=lcz+nsubg+1
  410 continue
c
      return
  420 format('   axial time step size for beam  element',i6,' =',e14.5)
  430 format(5x,a6,' element #',i6,' is current controlling element')
      end
      subroutine acmfcb(icsd,mnmxb,nbbms,nbm,result,numcsd,nnm1)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux36/lft,llt
      common/csfsav/
     &sfx1(128),sfy1(128),sfz1(128),
     &sfx2(128),sfy2(128),sfz2(128),
     &sfx3(128),sfy3(128),sfz3(128),
     &sfx4(128),sfy4(128),sfz4(128),
     &fail(128),ndof,ifail
      dimension icsd(3,1),mnmxb(2,1),nbbms(1),nbm(4,1),result(9,1)
      k0=0
      nf=nnm1+lft
      nl=nnm1+llt
      do 50 i=1,numcsd
      nnbm=icsd(2,i)
      if (nnbm.eq.0) go to 50
      if (mnmxb(1,i).gt.nl) go to 40
      if (mnmxb(2,i).lt.nf) go to 40
      do 30 l=1,nnbm
      nel =nbbms(l+k0)
      if (nel.ge.nf.and.nel.le.nl) then
      nel=nel-nnm1
      if (nbm(1,l).eq.1) then
      result(1,i)=result(1,i)-sfx1(nel)
      result(2,i)=result(2,i)-sfy1(nel)
      result(3,i)=result(3,i)-sfz1(nel)
      endif
      if (nbm(2,l).eq.1) then
      result(1,i)=result(1,i)-sfx2(nel)
      result(2,i)=result(2,i)-sfy2(nel)
      result(3,i)=result(3,i)-sfz2(nel)
      endif
      endif
   30 continue
   40 k0=k0+nnbm
   50 continue
      return
      end
      subroutine truss(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,iblkb,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndofs,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/
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
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux12/
     1  xlenn0(128), xlenn1(128), xij(128),
     2  yij(128),   zij(128),   force(128),
     3forcex(128),forcey(128),forcez(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/beam/ pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/presc/voltot(128)
      character*4 mess                                                  vax75
      common/aux38/mess
      common/energy/xinen
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/
     &sfx1(128),sfy1(128),sfz1(128),
     &sfx2(128),sfy2(128),sfz2(128),
     &sfx3(128),sfy3(128),sfz3(128),
     &sfx4(128),sfy4(128),sfz4(128),
     &fail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      dimension ixp(4,*),x(3,*),rhs(3,*),vt(3,*),vr(3,*),yhatn(12,*),   vax75
c     dimension ixp(2,*),x(3,*),rhs(3,*),vt(3,*),vr(3,*),yhatn(12,*),   cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),xyz(12),
     2 fibl(5,*),ybarn(12),strr(6,*),nsubgv(*),mtnum(*),
     3 nfegp(*),ihgq(*),ni2a(6),hgq(*),rule(mpubr,3,*),
     4 iblkb(*),xies(*)
      data ni2a/1,4,9,9,16,25/
c
      rho=1./ro(mxe)
      nip=csprop(2,mxe)
      ic =nint(csprop(3,mxe))+1
      irl=nint(csprop(4,mxe))
      irl=iabs(irl)
      if (csprop(4,mxe).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      ipt=1
      rhoa(lft)=ro(mxe)
      mte=mtype(mxe)
      nmtcon=7+nconst(mte)
      do 40 i=lft,llt
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      vx1(i)=vt(1,ix1(i))
      vy1(i)=vt(2,ix1(i))
      vz1(i)=vt(3,ix1(i))
      vx2(i)=vt(1,ix2(i))
      vy2(i)=vt(2,ix2(i))
      vz2(i)=vt(3,ix2(i))
   40 continue
      do 50 i=lft,llt
      dx1(i)=x1(i)-dt1*vx1(i)
      dy1(i)=y1(i)-dt1*vy1(i)
      dz1(i)=z1(i)-dt1*vz1(i)
      dx2(i)=x2(i)-dt1*vx2(i)
      dy2(i)=y2(i)-dt1*vy2(i)
      dz2(i)=z2(i)-dt1*vz2(i)
      xij(i)=dx1(i)-dx2(i)
      yij(i)=dy1(i)-dy2(i)
      zij(i)=dz1(i)-dz2(i)
      xlenn0(i)=sqrt(xij(i)**2+yij(i)**2+zij(i)**2)
      xij(i)  =x1(i)-x2(i)
      yij(i)  =y1(i)-y2(i)
      zij(i)  =z1(i)-z2(i)
      sarea(i)=xij(i)**2+yij(i)**2+zij(i)**2
      xlenn1(i)=sqrt(sarea(i))
      force(i)=0.0
      dxx(i)  =2.*(xlenn1(i)-xlenn0(i))/(xlenn1(i)+xlenn0(i))
   50 continue
      rho8th = .250*rhoa(lft)
      rho4th = .500*rhoa(lft)
      do 60 i=lft,llt
      dx=vx1(i)**2+vx2(i)**2
      dy=vy1(i)**2+vy2(i)**2
      dz=vz1(i)**2+vz2(i)**2
      enkint(i)=fibl(1,nnm1+i)*rho8th*(dx+dy+dz)
      xmomnt(i)=fibl(1,nnm1+i)*rho4th*(vx1(i)+vx2(i))
      ymomnt(i)=fibl(1,nnm1+i)*rho4th*(vy1(i)+vy2(i))
      zmomnt(i)=fibl(1,nnm1+i)*rho4th*(vz1(i)+vz2(i))
   60 continue
      do 250 m=1,nip
      ipt=m
      call cnmtrs (nmtcon,auxvec,cm,lav,mte,nip,ipt,dampk,ym,prv)
      do 240 i=lft,llt
      force(i)   =force(i)+sig1(i)*fibl(1,nnm1+i)
      xies(nnm1+i)=xies(nnm1+i)+.5*fibl(1,nnm1+i)*xlenn1(i)*einc(i)
  240 continue
  250 continue
      do 260 i=lft,llt
      forcex(i)=force(i)*xij(i)/xlenn1(i)
      forcey(i)=force(i)*yij(i)/xlenn1(i)
      forcez(i)=force(i)*zij(i)/xlenn1(i)
      strr(1,nnm1+i)=force(i)
      strr(2,nnm1+i)=0.0
      strr(3,nnm1+i)=0.0
      strr(4,nnm1+i)=0.0
      strr(5,nnm1+i)=0.0
      strr(6,nnm1+i)=0.0
  260 continue
      sndspd=1./sqrt(sndspd*rho)
      do 280 n=1,nnc
      lcn=lczc+n
      i0 =iblkb(lcn)
      i1 =iblkb(lcn+1)-1
cdir$ ivdep
      do 270 i=i0,i1
      rhs(1,ix1(i))=rhs(1,ix1(i))-forcex(i)
      rhs(2,ix1(i))=rhs(2,ix1(i))-forcey(i)
      rhs(3,ix1(i))=rhs(3,ix1(i))-forcez(i)
      rhs(1,ix2(i))=rhs(1,ix2(i))+forcex(i)
      rhs(2,ix2(i))=rhs(2,ix2(i))+forcey(i)
      rhs(3,ix2(i))=rhs(3,ix2(i))+forcez(i)
  270 continue
  280 continue
c
      if (output) then
      nblksz=128
      nwords=3*nblksz
      call blkcpy (forcex,sfx1,nwords)
      do 290 i=1,nwords
  290 sfx2(i)=-sfx1(i)
      endif
      return
      end
      subroutine cnmtrs (nmtcon,auxvec,cm,lav,mte,nip,ipt,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      dimension cm(*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
      if(mte.eq.1) then
      call tspc1b (nmtcon,auxvec(lavloc),nip*nmtcon)
      call fhl1t (cm)
      call tspc2b (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif(mte.eq.3) then
      call tspc1b (nmtcon,auxvec(lavloc),nip*nmtcon)
      call fhl3t (cm)
      call tspc2b (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      else
      write(*,100)
      write(13,100)
      call adios(2)
      endif
      if( dampk.ne.0.) call rydmp3t(dampk,ym,prv)
      return
  100 format(//5x,'illegal material type for truss elements')
      end
      subroutine fhl1t (cm)
c     implicit double precision (a-h,o-z)                                    dp
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
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      dimension  cm(1)
      data third/-.3333333333333/
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      fac1=1.5*g
      blk =-ym/(1.-2.*pr)
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      sndspd=ym
      deti=1./(wq3*wq3-wq1*wq1)
      c22i=wq3*deti
      c23i=-wq1*deti
      fac=(c22i+c23i)*wq1
      do 30 i=lft,llt
      einc(i)=d1(i)*sig1(i)
      cb(i)=ss
      d2(i)=-d1(i)*fac-sig2(i)*c22i-sig3(i)*c23i
      d3(i)=-d1(i)*fac-sig2(i)*c23i-sig3(i)*c22i
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      sig1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      sig2(i)=0.
      sig3(i)=0.
      einc(i)=einc(i)+d1(i)*sig1(i)
   30 continue
      return
      end
      subroutine fhl3t (cm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
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
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qh=cm(mx+26)
      qb=cm(mx+21)
      qs=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      qbqh=qb*qh
      fac1=1.5*g
      qa  =1.-qb
      blk =-ym/(1.-2.*pr)
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      sndspd=.66666666666667*g+third*blk
c
      deti=1./(wq3*wq3-wq1*wq1)
      c22i=wq3*deti
      c23i=-wq1*deti
      fac=(c22i+c23i)*wq1
      do 30 i=lft,llt
      cb(i)=ss
      d2(i)=-d1(i)*fac
      d3(i)=-d1(i)*fac
   30 ak(i)=qs+qbqh*ep(i)
      do 40 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =0.0
      depi(i)=0.0
      deps(i)=0.0
      einc(i)=d1(i)*sig1(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0) then
      do 66 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=0.
      sig3(i)=0.
      sig4(i)=0.
      sig5(i)=0.
      sig6(i)=0.
      einc(i)=d1(i)*sig1(i)+einc(i)
   66 continue
      return
      endif
      fac1qh=1./(fac1+qh)
      do 60 i=lft,llt
      if (scle(i).eq.0.)  go to 60
      sig2(i)=0.
      sig3(i)=0.
      d2(i)=-d1(i)*fac
      d3(i)=-d1(i)*fac
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      if (scle(i).eq.0.)  go to 60
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3lst=sg3new
      sg3new=sg3old
      epslst=epsnew
      epsnew=d3(i)
      d3(i)=-.5*d1(i)
      d2(i)=d3(i)
      do 50 iter=2,20
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =sqrt(aj2(i))+1.0-scle(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh
      deps(i)=fac1*depi(i)/aj1(i)
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3lst=sg3new
      sg3new=sg3old
      epslst=epsnew
      epsnew=d3(i)
      demn=1.e-20+sg3new-sg3lst
      if (abs(demn).lt.1.e-14) go to 60
      d3(i)=epslst-sg3lst*(epsnew-epslst)/demn
      d2(i)=d3(i)
      if(abs(epsnew-epslst)/(abs(d3(i))+1.e-07).lt..0001) go to 60
   50 continue
   60 continue
      fac2=qa*qh/fac1
      do 70 i=lft,llt
      depn(i) =fac2*deps(i)
      sig1(i)=da1(i)  -deps(i)*(t1(i))
      sig2(i)=0.0
      sig3(i)=0.0
      sig4(i)=0.0
      sig5(i)=0.0
      sig6(i)=0.0
      epx1(i)=epx1(i)+depn(i)*t1(i)
      epx2(i)=epx2(i)+depn(i)*t2(i)
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+einc(i)
   70 continue
c
      return
      end
      subroutine belybm(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,xlngth,nbeam,iblkb,
     3 dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/
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
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128),
     & ddef1(128),ddef2(128),ddef3(128),
     & ddef4(128),ddef5(128),ddef6(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/beam/ pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),xe1(128)
      common/presc/voltot(128)
      character*4 mess                                                  vax75
      common/aux38/mess
      character*6 elname
      common/elnme/ elname(5)
      common/energy/xinen
      common/eltspf/ietspf
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      dimension ixp(4,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),xyz(12),
     2 fibl(5,*),ybarn(12),strr(6,*),nsubgv(*),mtnum(*),xlngth(*),
     3 nfegp(*),ihgq(*),ni2a(6),hgq(*),rule(mpubr,3,*),
     4 nbeam(*),iblkb(*),xies(*)
c
      rho=1./ro(mxe)
      rhoa(lft)=ro(mxe)
      mte=mtype(mxe)
      nmtcon=7+nconst(mte)
      do 40 i=lft,llt
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      vx1(i)=vt(1,ix1(i))
      vy1(i)=vt(2,ix1(i))
      vz1(i)=vt(3,ix1(i))
      vx2(i)=vt(1,ix2(i))
      vy2(i)=vt(2,ix2(i))
      vz2(i)=vt(3,ix2(i))
      vx5(i)=vr(1,ix1(i))
      vy5(i)=vr(2,ix1(i))
      vz5(i)=vr(3,ix1(i))
      vx6(i)=vr(1,ix2(i))
      vy6(i)=vr(2,ix2(i))
      vz6(i)=vr(3,ix2(i))
   40 continue
      do 50 i=lft,llt
      dx1(i) =dt1*vx1(i)
      dy1(i) =dt1*vy1(i)
      dz1(i) =dt1*vz1(i)
      dx2(i) =dt1*vx2(i)
      dy2(i) =dt1*vy2(i)
      dz2(i) =dt1*vz2(i)
      wxx1(i)=dt1*vx5(i)
      wyy1(i)=dt1*vy5(i)
      wzz1(i)=dt1*vz5(i)
      wxx2(i)=dt1*vx6(i)
      wyy2(i)=dt1*vy6(i)
      wzz2(i)=dt1*vz6(i)
   50 continue
      rho8th = .250*rhoa(lft)
      rho4th = .500*rhoa(lft)
      do 60 i=lft,llt
      dx=vx1(i)**2+vx2(i)**2
      dy=vy1(i)**2+vy2(i)**2
      dz=vz1(i)**2+vz2(i)**2
      enkint(i)=fibl(1,nnm1+i)*rho8th*(dx+dy+dz)
      xmomnt(i)=fibl(1,nnm1+i)*rho4th*(vx1(i)+vx2(i))
      ymomnt(i)=fibl(1,nnm1+i)*rho4th*(vy1(i)+vy2(i))
      zmomnt(i)=fibl(1,nnm1+i)*rho4th*(vz1(i)+vz2(i))
   60 continue
      do 100 i=lft,llt
      voltot(i)=0.
      yhtnx1(i)=yhatn(1,i+nnm1)
      yhtny1(i)=yhatn(2,i+nnm1)
      yhtnz1(i)=yhatn(3,i+nnm1)
      yhtnx2(i)=yhatn(4,i+nnm1)
      yhtny2(i)=yhatn(5,i+nnm1)
      yhtnz2(i)=yhatn(6,i+nnm1)
      zhtnx1(i)=yhatn(7,i+nnm1)
      zhtny1(i)=yhatn(8,i+nnm1)
      zhtnz1(i)=yhatn(9,i+nnm1)
      zhtnx2(i)=yhatn(10,i+nnm1)
      zhtny2(i)=yhatn(11,i+nnm1)
      zhtnz2(i)=yhatn(12,i+nnm1)
  100 continue
c
c     update nodal base vectors with rotational increments
c
      call gthatw (lft,llt)
c
c     define base vectors of local element system
c
      call locsyw (lft,llt)
c
c     compute the deformation displacements
c
      call dfdisw (lft,llt)
c
c     update force and moment resultants
c
      call conmbs (nmtcon,auxvec,cm,lav,1,1,fibl(1,nnm1+1),mte,
     1 xlngth(nnm1+1),dampk,ym,prv)
c
      sndspd=1./sqrt(sndspd*rho)
c
c     compute remaining nodal forces from equilibrium, transform to
c     global system, and assemble into global arrays
c
      call feqtrw (lft,llt,rhs,rhs(1+neq),strr(1,nnm1+1),xies(nnm1+1),
     1  iblkb)
c
c     reset nodal base vectors to element system
c
      do 270 i=lft,llt
      yhatn(1,i+nnm1) =gl12(i)
      yhatn(2,i+nnm1) =gl22(i)
      yhatn(3,i+nnm1) =gl32(i)
      yhatn(4,i+nnm1) =gl12(i)
      yhatn(5,i+nnm1) =gl22(i)
      yhatn(6,i+nnm1) =gl32(i)
      yhatn(7,i+nnm1) =gl13(i)
      yhatn(8,i+nnm1) =gl23(i)
      yhatn(9,i+nnm1) =gl33(i)
      yhatn(10,i+nnm1)=gl13(i)
      yhatn(11,i+nnm1)=gl23(i)
      yhatn(12,i+nnm1)=gl33(i)
  270 continue
      spdfac=48./(sndspd**2)
c
c     print out computed step size for each element at time=0
c
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      do 280 i=lft,llt
      xe12=xe1(i)**2
      areal2=fibl(1,nnm1+i)/xe12
      xmaxi=  max(fibl(2,nnm1+i),fibl(3,nnm1+i),fibl(4,nnm1+i))
      omega2=spdfac*xe12*xmaxi*(1./areal2+3./(12.*xmaxi+areal2))
      sndsp(i)=2./sqrt(omega2)
      dt2=  min(sndsp(i),dt2)
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ielmtc=nbeam(ielmtc)
      ityptc=2
      endif
      ielmtd=nnm1+i
      ielmtd=nbeam(ielmtd)
      write (13,420) ielmtd,sndsp(i)
      write (13,430)elname(ityptc),ielmtc
  280 continue
      endif
      endif
      endif
c
      do 300 i=lft,llt
      xe12=xe1(i)**2
      areal2=fibl(1,nnm1+i)/xe12
      xmaxi=  max(fibl(2,nnm1+i),fibl(3,nnm1+i),fibl(4,nnm1+i))
      omega2=spdfac*xe12*xmaxi*(1./areal2+3./(12.*xmaxi+areal2))
      sndsp(i)=2./sqrt(omega2)
      dt2=  min(dt2,sndsp(i))
  300 continue
      if (mess.eq.'sw2.') then
      do 310 i=lft,llt
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ielmtc=nbeam(ielmtc)
      ityptc=2
      endif
  310 continue
      endif
      return
  420 format(' bending time step size for beam  element',i6,' =',e14.5)
  430 format(5x,a6,' element #',i6,' is current controlling element')
      end
      subroutine gthatw (lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to find new nodal base vectors based on incremental
c     rotations
c
      common/aux01/
     & xhatx1(128),xhaty1(128),xhatz1(128),
     & xhatx2(128),xhaty2(128),xhatz2(128)
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
c
      call rwngvb(wxx1,wyy1,wzz1,lft,llt)
      do 10 i=lft,llt
      yhatx1(i)=rot1(i)*yhtnx1(i)+rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
      yhaty1(i)=rot2(i)*yhtnx1(i)+rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
      yhatz1(i)=rot3(i)*yhtnx1(i)+rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
      zhatx1(i)=rot1(i)*zhtnx1(i)+rot4(i)*zhtny1(i)+rot7(i)*zhtnz1(i)
      zhaty1(i)=rot2(i)*zhtnx1(i)+rot5(i)*zhtny1(i)+rot8(i)*zhtnz1(i)
      zhatz1(i)=rot3(i)*zhtnx1(i)+rot6(i)*zhtny1(i)+rot9(i)*zhtnz1(i)
   10 continue
      do 20 i=lft,llt
      c1=yhaty1(i)*zhatz1(i)-yhatz1(i)*zhaty1(i)
      c2=yhatz1(i)*zhatx1(i)-yhatx1(i)*zhatz1(i)
      c3=yhatx1(i)*zhaty1(i)-yhaty1(i)*zhatx1(i)
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      xhatx1(i)=c1*xl
      xhaty1(i)=c2*xl
      xhatz1(i)=c3*xl
   20 continue
      call rwngvb(wxx2,wyy2,wzz2,lft,llt)
      do 30 i=lft,llt
      yhatx2(i)=rot1(i)*yhtnx2(i)+rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
      yhaty2(i)=rot2(i)*yhtnx2(i)+rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
      yhatz2(i)=rot3(i)*yhtnx2(i)+rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
      zhatx2(i)=rot1(i)*zhtnx2(i)+rot4(i)*zhtny2(i)+rot7(i)*zhtnz2(i)
      zhaty2(i)=rot2(i)*zhtnx2(i)+rot5(i)*zhtny2(i)+rot8(i)*zhtnz2(i)
      zhatz2(i)=rot3(i)*zhtnx2(i)+rot6(i)*zhtny2(i)+rot9(i)*zhtnz2(i)
   30 continue
      do 40 i=lft,llt
      c1=yhaty2(i)*zhatz2(i)-yhatz2(i)*zhaty2(i)
      c2=yhatz2(i)*zhatx2(i)-yhatx2(i)*zhatz2(i)
      c3=yhatx2(i)*zhaty2(i)-yhaty2(i)*zhatx2(i)
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      xhatx2(i)=c1*xl
      xhaty2(i)=c2*xl
      xhatz2(i)=c3*xl
   40 continue
c
      return
      end
      subroutine locsyw (lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to find new nodal base vectors based on incremental
c     rotations
c
      common/aux01/
     & xhatx1(128),xhaty1(128),xhatz1(128),
     & xhatx2(128),xhaty2(128),xhatz2(128),
     & fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     & mx1(128),my1(128),mz1(128),mx2(128),my2(128),mz2(128)
      common/aux11/
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),xe1(128)
c
c
c     find base vector in axial (x) direction from difference in
c     nodal coordinates
c
      do 10 i=lft,llt
      x21=x2(i)-x1(i)
      y21=y2(i)-y1(i)
      z21=z2(i)-z1(i)
      sarea(i)=x21*x21+y21*y21+z21*z21
      diagm(i)=sqrt(sarea(i))
      xe1(i)=1./diagm(i)
      gl11(i)=x21*xe1(i)
      gl21(i)=y21*xe1(i)
      gl31(i)=z21*xe1(i)
   10 continue
c
c     find base vector in z direction by the cross product of the
c     base vector in the x direction and the average nodal base
c     vector in the y direction
c
      do 20 i=lft,llt
      yavex=.5*(yhatx1(i)+yhatx2(i))
      yavey=.5*(yhaty1(i)+yhaty2(i))
      yavez=.5*(yhatz1(i)+yhatz2(i))
      c1=gl21(i)*yavez-gl31(i)*yavey
      c2=gl31(i)*yavex-gl11(i)*yavez
      c3=gl11(i)*yavey-gl21(i)*yavex
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      gl13(i)=c1*xl
      gl23(i)=c2*xl
      gl33(i)=c3*xl
   20 continue
c
c     fine base vector in y direction by the cross product of the
c     base vector in the z direction and the base vector in the
c     x direction
c
      do 30 i=lft,llt
      c1=gl23(i)*gl31(i)-gl33(i)*gl21(i)
      c2=gl33(i)*gl11(i)-gl13(i)*gl31(i)
      c3=gl13(i)*gl21(i)-gl23(i)*gl11(i)
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      gl12(i)=c1*xl
      gl22(i)=c2*xl
      gl32(i)=c3*xl
   30 continue
c
      return
      end
      subroutine dfdisw (lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to compute the deformation displacements
c
      common/aux01/
     & xhatx1(128),xhaty1(128),xhatz1(128),
     & xhatx2(128),xhaty2(128),xhatz2(128),
     & fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     & mx1(128),my1(128),mz1(128),mx2(128),my2(128),mz2(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux10/
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
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128),
     & ddef1(128),ddef2(128),ddef3(128),
     & ddef4(128),ddef5(128),ddef6(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
c
c     find axial deformation displacement by difference in
c     displacement in local system
c
      do 10 i=lft,llt
      x1loc=gl11(i)*dx1(i)+gl21(i)*dy1(i)+gl31(i)*dz1(i)
      x2loc=gl11(i)*dx2(i)+gl21(i)*dy2(i)+gl31(i)*dz2(i)
      ddef1(i)=x2loc-x1loc
   10 continue
c
c     find torsional deformation rotation by crossing the y
c     nodal base vectors at each node and projecting on the
c     local x axis
c
      do 20 i=lft,llt
      yhlcy1=gl12(i)*yhatx1(i)+gl22(i)*yhaty1(i)+gl32(i)*yhatz1(i)
      yhlcz1=gl13(i)*yhatx1(i)+gl23(i)*yhaty1(i)+gl33(i)*yhatz1(i)
      yhlcy2=gl12(i)*yhatx2(i)+gl22(i)*yhaty2(i)+gl32(i)*yhatz2(i)
      yhlcz2=gl13(i)*yhatx2(i)+gl23(i)*yhaty2(i)+gl33(i)*yhatz2(i)
      ddef2(i)=yhlcy1*yhlcz2-yhlcy2*yhlcz1
   20 continue
c
c     find transverse deformation rotations by putting x nodal base
c     vectors in local system
c
      do 30 i=lft,llt
      ddef3(i)=-(gl13(i)*xhatx1(i)+gl23(i)*xhaty1(i)+gl33(i)*xhatz1(i))
      ddef4(i)=  gl12(i)*xhatx1(i)+gl22(i)*xhaty1(i)+gl32(i)*xhatz1(i)
      ddef5(i)=-(gl13(i)*xhatx2(i)+gl23(i)*xhaty2(i)+gl33(i)*xhatz2(i))
      ddef6(i)=  gl12(i)*xhatx2(i)+gl22(i)*xhaty2(i)+gl32(i)*xhatz2(i)
   30 continue
c
      return
      end
      subroutine feqtrw (lft,llt,e,f,strr,xies,iblkb)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute remaining nodal forces from equilibrium, transform to
c     global system, and assemble into global arrays
c
      common/aux01/
     & xhatx1(128),xhaty1(128),xhatz1(128),
     & xhatx2(128),xhaty2(128),xhatz2(128),
     & fx1(128),fy1(128),fz1(128),fx2(128),fy2(128),fz2(128),
     & xmx1(128),xmy1(128),xmz1(128),xmx2(128),xmy2(128),xmz2(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux10/
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
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128),
     & ddef1(128),ddef2(128),ddef3(128),
     & ddef4(128),ddef5(128),ddef6(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux14/
     1 ft21(128),fm21(128),fm12(128),fm13(128),
     2 fm22(128),fm23(128),pram(128),ft11(128),
     3 ft12(128),ft13(128),ft22(128),ft23(128),
     4 fm11(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),xe1(128)
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/
     &sfx1(128),sfy1(128),sfz1(128),
     &sfx2(128),sfy2(128),sfz2(128),
     &sfx3(128),sfy3(128),sfz3(128),
     &sfx4(128),sfy4(128),sfz4(128),
     &fail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension e(3,*),f(3,*),strr(6,*),iblkb(*),xies(*)
c
c     use equilibrium to solve for remaining forces
c
      do 10 i=lft,llt
      ft11(i)=-ft21(i)
      ft12(i)= (fm13(i)+fm23(i))*xe1(i)
      ft13(i)=-(fm12(i)+fm22(i))*xe1(i)
      fm11(i)=-fm21(i)
      ft22(i)=-ft12(i)
      ft23(i)=-ft13(i)
      strr(1,i)=-ft11(i)
      strr(2,i)= ft12(i)
      strr(3,i)= ft13(i)
      strr(4,i)= fm12(i)
      strr(5,i)= fm13(i)
      strr(6,i)= fm11(i)
      xies(i)=xies(i)+einc(i)
   10 continue
c
c     transform forces and moments to global system
c
      do 50 i=lft,llt
      fx1(i)=gl11(i)*ft11(i)+gl12(i)*ft12(i)+gl13(i)*ft13(i)
      fy1(i)=gl21(i)*ft11(i)+gl22(i)*ft12(i)+gl23(i)*ft13(i)
      fz1(i)=gl31(i)*ft11(i)+gl32(i)*ft12(i)+gl33(i)*ft13(i)
      fx2(i)=gl11(i)*ft21(i)+gl12(i)*ft22(i)+gl13(i)*ft23(i)
      fy2(i)=gl21(i)*ft21(i)+gl22(i)*ft22(i)+gl23(i)*ft23(i)
      fz2(i)=gl31(i)*ft21(i)+gl32(i)*ft22(i)+gl33(i)*ft23(i)
      xmx1(i)=gl11(i)*fm11(i)+gl12(i)*fm12(i)+gl13(i)*fm13(i)
      xmy1(i)=gl21(i)*fm11(i)+gl22(i)*fm12(i)+gl23(i)*fm13(i)
      xmz1(i)=gl31(i)*fm11(i)+gl32(i)*fm12(i)+gl33(i)*fm13(i)
      xmx2(i)=gl11(i)*fm21(i)+gl12(i)*fm22(i)+gl13(i)*fm23(i)
      xmy2(i)=gl21(i)*fm21(i)+gl22(i)*fm22(i)+gl23(i)*fm23(i)
      xmz2(i)=gl31(i)*fm21(i)+gl32(i)*fm22(i)+gl33(i)*fm23(i)
   50 continue
c
      do 80 n=1,nnc
      lcn=lczc+n
      i0 =iblkb(lcn)
      i1 =iblkb(lcn+1)-1
cdir$ ivdep
      do 70 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))-fx1(i)
      e(2,ix1(i))=e(2,ix1(i))-fy1(i)
      e(3,ix1(i))=e(3,ix1(i))-fz1(i)
      e(1,ix2(i))=e(1,ix2(i))-fx2(i)
      e(2,ix2(i))=e(2,ix2(i))-fy2(i)
      e(3,ix2(i))=e(3,ix2(i))-fz2(i)
      f(1,ix1(i))=f(1,ix1(i))-xmx1(i)
      f(2,ix1(i))=f(2,ix1(i))-xmy1(i)
      f(3,ix1(i))=f(3,ix1(i))-xmz1(i)
      f(1,ix2(i))=f(1,ix2(i))-xmx2(i)
      f(2,ix2(i))=f(2,ix2(i))-xmy2(i)
      f(3,ix2(i))=f(3,ix2(i))-xmz2(i)
   70 continue
   80 continue
c
      if (output) then
      nblksz=128
      nwords=6*nblksz
      call blkcpy (fx1,sfx1,nwords)
      endif
      return
      end
      subroutine conmbs (nmtcon,auxvec,cm,lav,nip,ipt,csprop,mte,
     1 xlngth,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common /  / a(1)
      dimension cm(*),auxvec(*),csprop(*),xlngth(*)
      lavloc=(ipt-1)*nmtcon+lav
c
      call tspc1b (nmtcon,auxvec(lavloc),nip*nmtcon)
      if (mte.lt.28) then
      call s1mnb (cm,csprop,csprop(2),csprop(3),csprop(4))
      elseif (mte.eq.28) then
      call s28mnb(cm,csprop,csprop(2),csprop(3),csprop(4))
      endif
      call tspc2b (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      if( dampk.ne.0.) call rydmp3r(cm,csprop,csprop(2),
     1 csprop(3),csprop(4),dampk,ym,prv)
      return
      end
      subroutine s1mnb (cm,ra,ri1,ri2,rj)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to compute generalized nodal forces from deformation
c     displacements
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128),
     & ddef1(128),ddef2(128),ddef3(128),
     & ddef4(128),ddef5(128),ddef6(128)
      common/aux14/
     1 ft21(128),fm21(128),fm12(128),fm13(128),
     2 fm22(128),fm23(128),pram(128),ft11(128),
     3 ft12(128),ft13(128),ft22(128),ft23(128),
     4  fm11(128),ft21o(128),fm21o(128),fm12o(128),
     5 fm22o(128),fm13o(128),fm23o(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),xe1(128)
      dimension cm(1),ra(5,1),ri1(5,1),ri2(5,1),rj(5,1)
c
c     use equilibrium to solve for remaining forces
c
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
c
c     ga & gb are axial and bending damping factors
      if ( dt1.lt.1.e-20) then
      ga=1.0
      gb=1.0
      else
      ga=1.0+cm(mx+31)/(dt1+1.e-20)
      gb=1.0+cm(mx+32)/(dt1+1.e-20)
      endif
c
      g=ym/(2.+2.*pr)
      sndspd=ym
      pr24  =24.*(1.+pr)
c
      ymga=ym*ga
      ymgb=ym*gb
      do 10 i=lft,llt
      ft21o(i)=ft21(i)
      fm21o(i)=fm21(i)
      fm12o(i)=fm12(i)
      fm22o(i)=fm22(i)
      fm13o(i)=fm13(i)
      fm23o(i)=fm23(i)
c
c     compute axial force
c
      ft21(i)=ft21(i)+ymga*ra(1,i)*xe1(i)*ddef1(i)
c
c     compute torsional moment
c
      fm21(i)=fm21(i)+g*rj(1,i)*xe1(i)*ddef2(i)
c
c     compute bending moments
c
      fac =pr24*xe1(i)*xe1(i)/rj(2,i)
      phi1=fac*ri1(1,i)
      phi2=fac*ri2(1,i)
      phi14=4.+phi1
      phi12=2.-phi1
      phi24=4.+phi2
      phi22=2.-phi2
      ct1=ymgb*ri1(1,i)*xe1(i)/(1.+phi1)
      fm12(i)=fm12(i)+ct1*(phi14*ddef3(i)+phi12*ddef5(i))
      fm22(i)=fm22(i)+ct1*(phi12*ddef3(i)+phi14*ddef5(i))
      ct2=ymgb*ri2(1,i)*xe1(i)/(1.+phi2)
      fm13(i)=fm13(i)+ct2*(phi24*ddef4(i)+phi22*ddef6(i))
      fm23(i)=fm23(i)+ct2*(phi22*ddef4(i)+phi24*ddef6(i))
   10 continue
c
c     update internal energy
c
      do 20 i=lft,llt
      einc(i)
     & =.5*((ft21o(i)+ft21(i))*ddef1(i)+(fm21o(i)+fm21(i))*ddef2(i)
     & +    (fm12o(i)+fm12(i))*ddef3(i)+(fm22o(i)+fm22(i))*ddef5(i)
     & +    (fm13o(i)+fm13(i))*ddef4(i)+(fm23o(i)+fm23(i))*ddef6(i))
   20 continue
c
      return
      end
      subroutine s28mnb (cm,ra,ri1,ri2,rj)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to compute generalized nodal forces from deformation
c     displacements using ilyushin criterion
c
      common/aux2/
     & yf1(128),yf2(128),axial(128),c1(128),c2(128),c3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/
     & gl11(128),gl12(128),gl13(128),
     & gl21(128),gl22(128),gl23(128),
     & gl31(128),gl32(128),gl33(128),
     & ddef1(128),ddef2(128),ddef3(128),
     & ddef4(128),ddef5(128),ddef6(128)
      common/aux14/
     1 ft21(128),fm21(128),fm12(128),fm13(128),
     2 fm22(128),fm23(128),pram(128),ft11(128),
     3 ft12(128),ft13(128),ft22(128),ft23(128),
     4  fm11(128),ft21o(128),fm21o(128),fm12o(128),
     5 fm22o(128),fm13o(128),fm23o(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),xe1(128)
      dimension cm(1),ra(5,1),ri1(5,1),ri2(5,1),rj(5,1)
      data one/1.0/
c
c     use equilibrium to solve for remaining forces
c
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
      q0=cm(mx+11)
      et=8./(3.*q0**2)
      f1=2./(q0**2)
      g=ym/(2.+2.*pr)
      sndspd=ym
      pr24  =24.*(1.+pr)
c
      do 10 i=lft,llt
      ft21o(i)=ft21(i)
      fm21o(i)=fm21(i)
      fm12o(i)=fm12(i)
      fm22o(i)=fm22(i)
      fm13o(i)=fm13(i)
      fm23o(i)=fm23(i)
c
c     compute axial force
c
      axial(i)=ft21(i)+ym*ra(1,i)*xe1(i)*ddef1(i)
c
c     compute torsional moment
c
      fm21(i)=fm21(i)+g*rj(1,i)*xe1(i)*ddef2(i)
c
c     compute bending moments
c
      fac =pr24*xe1(i)*xe1(i)/rj(2,i)
      phi1=fac*ri1(1,i)
      phi2=fac*ri2(1,i)
      phi14=4.+phi1
      phi12=2.-phi1
      phi24=4.+phi2
      phi22=2.-phi2
      ct1=ym*ri1(1,i)*xe1(i)/(1.+phi1)
      fm12(i)=fm12(i)+ct1*(phi14*ddef3(i)+phi12*ddef5(i))
      fm22(i)=fm22(i)+ct1*(phi12*ddef3(i)+phi14*ddef5(i))
      ct2=ym*ri2(1,i)*xe1(i)/(1.+phi2)
      fm13(i)=fm13(i)+ct2*(phi24*ddef4(i)+phi22*ddef6(i))
      fm23(i)=fm23(i)+ct2*(phi22*ddef4(i)+phi24*ddef6(i))
      f2   =et/ra(1,i)
      c1(i)=f1/(ra(1,i)**2)
      c2(i)=f2/(ri1(1,i))
      c3(i)=f2/(ri2(1,i))
   10 continue
c
      do 20 i=lft,llt
      ft212  =c1(i)*axial(i)*axial(i)
      yf1(i) =.5*(ft212+c2(i)*fm12(i)**2+c3(i)*fm13(i)**2)
      yf2(i) =.5*(ft212+c2(i)*fm22(i)**2+c3(i)*fm23(i)**2)
   20 continue
c
      do 30 i=lft,llt
      yf1(i)=1.0/sqrt(  max(one,yf1(i)))
      yf2(i)=1.0/sqrt(  max(one,yf2(i)))
      fm12(i)=yf1(i)*fm12(i)
      fm13(i)=yf1(i)*fm13(i)
      fm22(i)=yf2(i)*fm22(i)
      fm23(i)=yf2(i)*fm23(i)
      ft21(i)=.5*(yf1(i)+yf2(i))*axial(i)
   30 continue
c
c     update internal energy
c
      do 40 i=lft,llt
      einc(i)
     & =.5*((ft21o(i)+ft21(i))*ddef1(i)+(fm21o(i)+fm21(i))*ddef2(i)
     & +    (fm12o(i)+fm12(i))*ddef3(i)+(fm22o(i)+fm22(i))*ddef5(i)
     & +    (fm13o(i)+fm13(i))*ddef4(i)+(fm23o(i)+fm23(i))*ddef6(i))
   40 continue
c
      return
      end
      subroutine hughbm(ixp,x,rhs,vt,vr,yhatn,strr,fibl,
     1 auxvec,mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,
     2 rule,mpubr,xies,lav,nmel,nnm1,mxe,iblkb,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux7/
     1 vx1(128),vx2(128),vx3(128),vx4(128),
     2 vx5(128),vx6(128),vx7(128),vx8(128),
     3 vy1(128),vy2(128),vy3(128),vy4(128),
     4 vy5(128),vy6(128),vy7(128),vy8(128),
     5 vz1(128),vz2(128),vz3(128),vz4(128),
     6 vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux10/
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
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cxx(128),fcl(128),fcq(128)
      common/aux36/lft,llt
      common/beam/ pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      common/presc/voltot(128)
      character*4 mess                                                  vax75
      common/aux38/mess
      common/energy/xinen
      dimension ixp(4,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),xyz(12),
     2 fibl(5,*),ybarn(12),strr(6,*),nsubgv(*),mtnum(*),
     3 nfegp(*),ihgq(*),ni2a(6),hgq(*),rule(mpubr,3,*),
     4 iblkb(*),xies(*)
      data ni2a/1,4,9,9,16,25/
c
      rho=1./ro(mxe)
      nip=csprop(2,mxe)
      ic =nint(csprop(3,mxe))+1
      irl=nint(csprop(4,mxe))
      irl=iabs(irl)
      if (csprop(4,mxe).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      ipt=1
      rhoa(lft)=ro(mxe)
      mte=mtype(mxe)
      nmtcon=7+nconst(mte)
      do 40 i=lft,llt
      x1(i) =x(1,ix1(i))
      y1(i) =x(2,ix1(i))
      z1(i) =x(3,ix1(i))
      x2(i) =x(1,ix2(i))
      y2(i) =x(2,ix2(i))
      z2(i) =x(3,ix2(i))
      vx1(i)=vt(1,ix1(i))
      vy1(i)=vt(2,ix1(i))
      vz1(i)=vt(3,ix1(i))
      vx2(i)=vt(1,ix2(i))
      vy2(i)=vt(2,ix2(i))
      vz2(i)=vt(3,ix2(i))
      vx5(i)=vr(1,ix1(i))
      vy5(i)=vr(2,ix1(i))
      vz5(i)=vr(3,ix1(i))
      vx6(i)=vr(1,ix2(i))
      vy6(i)=vr(2,ix2(i))
      vz6(i)=vr(3,ix2(i))
   40 continue
      do 50 i=lft,llt
      dx1(i) =dt1*vx1(i)
      dy1(i) =dt1*vy1(i)
      dz1(i) =dt1*vz1(i)
      dx2(i) =dt1*vx2(i)
      dy2(i) =dt1*vy2(i)
      dz2(i) =dt1*vz2(i)
      wxx1(i)=dt1*vx5(i)
      wyy1(i)=dt1*vy5(i)
      wzz1(i)=dt1*vz5(i)
      wxx2(i)=dt1*vx6(i)
      wyy2(i)=dt1*vy6(i)
      wzz2(i)=dt1*vz6(i)
   50 continue
      rho8th = .250*rhoa(lft)
      rho4th = .500*rhoa(lft)
      do 60 i=lft,llt
      dx=vx1(i)**2+vx2(i)**2
      dy=vy1(i)**2+vy2(i)**2
      dz=vz1(i)**2+vz2(i)**2
      enkint(i)=fibl(1,nnm1+i)*rho8th*(dx+dy+dz)
      xmomnt(i)=fibl(1,nnm1+i)*rho4th*(vx1(i)+vx2(i))
      ymomnt(i)=fibl(1,nnm1+i)*rho4th*(vy1(i)+vy2(i))
      zmomnt(i)=fibl(1,nnm1+i)*rho4th*(vz1(i)+vz2(i))
   60 continue
      do 100 i=lft,llt
      voltot(i)=0.
      yhtnx1(i)=yhatn(1,i+nnm1)
      yhtny1(i)=yhatn(2,i+nnm1)
      yhtnz1(i)=yhatn(3,i+nnm1)
      yhtnx2(i)=yhatn(4,i+nnm1)
      yhtny2(i)=yhatn(5,i+nnm1)
      yhtnz2(i)=yhatn(6,i+nnm1)
      zhtnx1(i)=yhatn(7,i+nnm1)
      zhtny1(i)=yhatn(8,i+nnm1)
      zhtnz1(i)=yhatn(9,i+nnm1)
      zhtnx2(i)=yhatn(10,i+nnm1)
      zhtny2(i)=yhatn(11,i+nnm1)
      zhtnz2(i)=yhatn(12,i+nnm1)
  100 continue
      call gthatb (fibl(1,nnm1+1),lft,llt,ic,rule(mpubr,2,irl))
      do 250 m=1,npt
      ipt=m
      if (ic.lt.3) then
      etac=   eta(nip,ipt,ic)
      zetc=  zeta(nip,ipt,ic)
      wght=weight(nip,ipt,ic)
      else
      etac=rule(ipt,1,irl)
      zetc=rule(ipt,2,irl)
      wght=rule(ipt,3,irl)
      endif
      call stdspb(etac,zetc,csprop(13,mxe),csprop(15,mxe))
      call conmdb (nmtcon,auxvec,cm,lav,mte,npt,ipt,csprop(1,mxe),
     1 strr(1,nnm1+1),wght,dampk,ym,prv)
      call forcib (rhs,wght,xies(nnm1+1))
  250 continue
      sndspd=1./sqrt(sndspd*rho)
      call frc4nb (rhs,rhs(1+neq),csprop(13,mxe),iblkb)
      do 270 i=lft,llt
      factor   =voltot(i)/sqrt(sarea(i))
      enkint(i)=enkint(i)*factor
      xmomnt(i)=xmomnt(i)*factor
      ymomnt(i)=ymomnt(i)*factor
      zmomnt(i)=zmomnt(i)*factor
      yhatn(1,i+nnm1) =yhatx1(i)
      yhatn(2,i+nnm1) =yhaty1(i)
      yhatn(3,i+nnm1) =yhatz1(i)
      yhatn(4,i+nnm1) =yhatx2(i)
      yhatn(5,i+nnm1) =yhaty2(i)
      yhatn(6,i+nnm1) =yhatz2(i)
      yhatn(7,i+nnm1) =zhatx1(i)
      yhatn(8,i+nnm1) =zhaty1(i)
      yhatn(9,i+nnm1) =zhatz1(i)
      yhatn(10,i+nnm1)=zhatx2(i)
      yhatn(11,i+nnm1)=zhaty2(i)
      yhatn(12,i+nnm1)=zhatz2(i)
  270 continue
c
      return
      end
      subroutine gthatb (fibl,lft,llt,ic,factor)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      common/aux10/
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
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      dimension fibl(5,1)
      call rwngvb(wxx1,wyy1,wzz1,lft,llt)
      do 10 i=lft,llt
      yhatx1(i)=rot1(i)*yhtnx1(i)+rot4(i)*yhtny1(i)+rot7(i)*yhtnz1(i)
      yhaty1(i)=rot2(i)*yhtnx1(i)+rot5(i)*yhtny1(i)+rot8(i)*yhtnz1(i)
      yhatz1(i)=rot3(i)*yhtnx1(i)+rot6(i)*yhtny1(i)+rot9(i)*yhtnz1(i)
      zhatx1(i)=rot1(i)*zhtnx1(i)+rot4(i)*zhtny1(i)+rot7(i)*zhtnz1(i)
      zhaty1(i)=rot2(i)*zhtnx1(i)+rot5(i)*zhtny1(i)+rot8(i)*zhtnz1(i)
      zhatz1(i)=rot3(i)*zhtnx1(i)+rot6(i)*zhtny1(i)+rot9(i)*zhtnz1(i)
   10 continue
      call rwngvb(wxx2,wyy2,wzz2,lft,llt)
      do 20 i=lft,llt
      yhatx2(i)=rot1(i)*yhtnx2(i)+rot4(i)*yhtny2(i)+rot7(i)*yhtnz2(i)
      yhaty2(i)=rot2(i)*yhtnx2(i)+rot5(i)*yhtny2(i)+rot8(i)*yhtnz2(i)
      yhatz2(i)=rot3(i)*yhtnx2(i)+rot6(i)*yhtny2(i)+rot9(i)*yhtnz2(i)
      zhatx2(i)=rot1(i)*zhtnx2(i)+rot4(i)*zhtny2(i)+rot7(i)*zhtnz2(i)
      zhaty2(i)=rot2(i)*zhtnx2(i)+rot5(i)*zhtny2(i)+rot8(i)*zhtnz2(i)
      zhatz2(i)=rot3(i)*zhtnx2(i)+rot6(i)*zhtny2(i)+rot9(i)*zhtnz2(i)
   20 continue
      do 50 i=lft,llt
      dx5(i)=yhatx1(i)-yhtnx1(i)
      dy5(i)=yhaty1(i)-yhtny1(i)
      dz5(i)=yhatz1(i)-yhtnz1(i)
      dx6(i)=yhatx2(i)-yhtnx2(i)
      dy6(i)=yhaty2(i)-yhtny2(i)
      dz6(i)=yhatz2(i)-yhtnz2(i)
      dx7(i)=zhatx1(i)-zhtnx1(i)
      dy7(i)=zhaty1(i)-zhtny1(i)
      dz7(i)=zhatz1(i)-zhtnz1(i)
      dx8(i)=zhatx2(i)-zhtnx2(i)
      dy8(i)=zhaty2(i)-zhtny2(i)
      dz8(i)=zhatz2(i)-zhtnz2(i)
   50 continue
c
      call circi(ic,fibl,lft,llt,factor)
c
      do 60 i=lft,llt
      a12(i)=.25*(fbly1(i)*yhatx1(i)+fbly2(i)*yhatx2(i))
      a22(i)=.25*(fbly1(i)*yhaty1(i)+fbly2(i)*yhaty2(i))
      a32(i)=.25*(fbly1(i)*yhatz1(i)+fbly2(i)*yhatz2(i))
      a13(i)=.25*(fblz1(i)*zhatx1(i)+fblz2(i)*zhatx2(i))
      a23(i)=.25*(fblz1(i)*zhaty1(i)+fblz2(i)*zhaty2(i))
      a33(i)=.25*(fblz1(i)*zhatz1(i)+fblz2(i)*zhatz2(i))
      wxx1(i)=dx1(i)-dx2(i)
      wyy1(i)=dy1(i)-dy2(i)
      wzz1(i)=dz1(i)-dz2(i)
      sarea(i)=(x1(i)-x2(i))**2+(y1(i)-y2(i))**2+(z1(i)-z2(i))**2
   60 continue
      return
      end
      subroutine circi(ic,fibl,lft,llt,factor)
c     implicit double precision (a-h,o-z)                                    dp
      common/circ1/ys1(128),ys2(128),zs1(128),zs2(128),awgt(128)
      common/vect40/re1(128),re2(128)
      common/beam/pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/aux12/
     1  wxx1(128), wyy1(128), wzz1(128),
     2  wxx2(128), wyy2(128), wzz2(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
c
      dimension fibl(5,1)
      if (ic.eq.2) then
      do 110 j=lft,llt
      ys1(j)=fibl(1,j)
      ys2(j)=fibl(2,j)
      zs1(j)=fibl(3,j)
      zs2(j)=fibl(4,j)
      fbly1(j)=sqrt(.5*(ys1(j)**2+zs1(j)**2))
      fbly2(j)=sqrt(.5*(ys2(j)**2+zs2(j)**2))
      fblz1(j)=fbly1(j)
      fblz2(j)=fbly2(j)
      awgt(j)=pid4*((ys1(j)+ys2(j))**2-(zs1(j)+zs2(j))**2)/
     1              (fbly1(j)+fbly2(j))**2
  110 continue
      elseif (ic.eq.1) then
      do 130 j=lft,llt
      fbly1(j)=fibl(1,j)
      fbly2(j)=fibl(2,j)
      fblz1(j)=fibl(3,j)
      fblz2(j)=fibl(4,j)
      awgt(j)=1.
  130 continue
      elseif (ic.eq.3) then
      do 140 j=lft,llt
      fbly1(j)=fibl(1,j)
      fbly2(j)=fibl(2,j)
      fblz1(j)=fibl(3,j)
      fblz2(j)=fibl(4,j)
      awgt(j)=factor
  140 continue
      endif
      return
      end
      subroutine frc4nb(e,f,zbar,iblkb)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndofs,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux13/
     1 yhtnx1(128),yhtny1(128),yhtnz1(128),
     2 yhtnx2(128),yhtny2(128),yhtnz2(128),
     3 zhtnx1(128),zhtny1(128),zhtnz1(128),
     4 zhtnx2(128),zhtny2(128),zhtnz2(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     & x3(128),y3(128),z3(128),x4(128),y4(128),z4(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/csfsav/
     &sfx1(128),sfy1(128),sfz1(128),
     &sfx2(128),sfy2(128),sfz2(128),
     &sfx3(128),sfy3(128),sfz3(128),
     &sfx4(128),sfy4(128),sfz4(128),
     &fail(128),ndof,ifail
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension c1(128),c2(128),e(3,1),f(3,1),zbar(1),iblkb(*)
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
      c1(i)  =-zhatz1(i)*ep27(i)+zhaty1(i)*ep37(i)
      c2(i)  = zhatz1(i)*ep17(i)-zhatx1(i)*ep37(i)
      ep37(i)=ep35(i)-zhaty1(i)*ep17(i)+zhatx1(i)*ep27(i)
      ep17(i)=ep15(i)+c1(i)
      ep27(i)=ep25(i)+c2(i)
   30 continue
      do 40 i=lft,llt
      c1(i)  =-zhatz2(i)*ep28(i)+zhaty2(i)*ep38(i)
      c2(i)  = zhatz2(i)*ep18(i)-zhatx2(i)*ep38(i)
      ep38(i)=ep36(i)-zhaty2(i)*ep18(i)+zhatx2(i)*ep28(i)
      ep18(i)=ep16(i)+c1(i)
      ep28(i)=ep26(i)+c2(i)
   40 continue
      do 60 n=1,nnc
      lcn=lczc+n
      i0 =iblkb(lcn)
      i1 =iblkb(lcn+1)-1
cdir$ ivdep
      do 50 i=i0,i1
      e(1,ix1(i))=e(1,ix1(i))+ep11(i)
      e(2,ix1(i))=e(2,ix1(i))+ep21(i)
      e(3,ix1(i))=e(3,ix1(i))+ep31(i)
      e(1,ix2(i))=e(1,ix2(i))+ep12(i)
      e(2,ix2(i))=e(2,ix2(i))+ep22(i)
      e(3,ix2(i))=e(3,ix2(i))+ep32(i)
      f(1,ix1(i))=f(1,ix1(i))+ep17(i)
      f(2,ix1(i))=f(2,ix1(i))+ep27(i)
      f(3,ix1(i))=f(3,ix1(i))+ep37(i)
      f(1,ix2(i))=f(1,ix2(i))+ep18(i)
      f(2,ix2(i))=f(2,ix2(i))+ep28(i)
   50 f(3,ix2(i))=f(3,ix2(i))+ep38(i)
   60 continue
      if (output) then
      do 70 i=lft,llt
      sfx1(i)=-ep11(i)
      sfy1(i)=-ep21(i)
      sfz1(i)=-ep31(i)
      sfx2(i)=-ep12(i)
      sfy2(i)=-ep22(i)
      sfz3(i)=-ep32(i)
   70 continue
      endif
      return
      end
      subroutine unpkbm(ixp,nmel)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128),
     2 kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
c     dimension ixp(2,1)                                                cray1
      dimension ixp(4,1)                                                vax75
c     do 10 i=1,nmel                                                    cray1
c     kka(i)=ixp(1,i)                                                   cray1
c     kkb(i)=ixp(2,i)                                                   cray1
c  10 continue                                                          cray1
c     do 20 i=1,nmel                                                    cray1
c     mxt(i)=and(kka(i),3777777b)                                       cray1
c     ix3(i)=and(kkb(i),3777777b)                                       cray1
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
c     ix5(i)=and(kkb(i),3777777b)                                       cray1
c  50 continue                                                          cray1
      do 10 i=1,nmel                                                    vax75
      mxt(i)=ixp(1,i)                                                   vax75
      ix1(i)=ixp(2,i)                                                   vax75
   10 ix2(i)=ixp(3,i)                                                   vax75
      return
      end
      subroutine forcib(e,wgt,xies)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux9/vlrho(128),vol(128)
      common/aux10/
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
      common/circ1/ys1(128),ys2(128),zs1(128),zs2(128),awgt(128)
c
      dimension e(3,*),xies(*)
c
      wgt2=2.*wgt
      do 10 i=lft,llt
      vol(i)=wgt2*awgt(i)/vol(i)
      voltot(i)=voltot(i)+vol(i)
      sgv1(i)=sig1(i)*vol(i)
      sgv2(i)=sig2(i)*vol(i)
      sgv3(i)=sig3(i)*vol(i)
      sgv4(i)=sig4(i)*vol(i)
      sgv5(i)=sig5(i)*vol(i)
      sgv6(i)=sig6(i)*vol(i)
   10 xies(i) =xies(i) +0.50*vol(i)*einc(i)
      do 110 i=lft,llt
      e11(i)=sgv1(i)*px1(i)+sgv4(i)*py1(i)+sgv6(i)*pz1(i)
      e21(i)=sgv2(i)*py1(i)+sgv4(i)*px1(i)+sgv5(i)*pz1(i)
      e31(i)=sgv3(i)*pz1(i)+sgv6(i)*px1(i)+sgv5(i)*py1(i)
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
      ep12(i)=ep12(i)+e11(i)
      ep22(i)=ep22(i)+e21(i)
      ep32(i)=ep32(i)+e31(i)
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
      return
      end
      subroutine cmprsl(reslt,weight)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux36/lft,llt
      common/result/yint(128),zint(128),ywth(128),zwth(128)
      common/circ1/ys1(128),ys2(128),zs1(128),zs2(128),awgt(128)
      dimension reslt(6,1)
c
      do 10 i=lft,llt
      area=weight*awgt(i)*ywth(i)*zwth(i)
      reslt(1,i)=reslt(1,i)+area*sig1(i)
      reslt(2,i)=reslt(2,i)+area*sig4(i)
      reslt(3,i)=reslt(3,i)+area*sig6(i)
      reslt(4,i)=reslt(4,i)+zint(i)*area*sig1(i)
      reslt(5,i)=reslt(5,i)+yint(i)*area*sig1(i)
      reslt(6,i)=reslt(6,i)+yint(i)*area*sig6(i)-zint(i)*area*sig4(i)
   10 continue
      return
      end
      subroutine stdspb(eta,zeta,ybar,zbar)
c     implicit double precision (a-h,o-z)                                    dp
      common/circ1/ys1(128),ys2(128),zs1(128),zs2(128),awgt(128)
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/
     1  z1(128), z2(128), z3(128), z4(128),
     2 y1h(128),y2h(128),z1h(128),z2h(128),
     3  c1(128), c2(128), c3(128),enrm(128),
     4 xgj11(128),xgj21(128),xgj31(128),
     5 xgj12(128),xgj22(128),xgj32(128),
     6 xgj13(128),xgj23(128),xgj33(128)
      common/aux9/vlrho(128),det(128)
      common/aux10/
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
     1  dx12(128), dy12(128), dz12(128),
     2  dx24(128), dy24(128), dz24(128),
     3   a12(128),  a22(128),  a32(128),
     4   a13(128),  a23(128),  a33(128),
     5 fbly1(128),fbly2(128),fblz1(128),fblz2(128)
      common/aux13/
     1  xnd11(128), xnd21(128), xnd31(128),
     2  xnd12(128), xnd22(128), xnd32(128),
     3  xnd13(128), xnd23(128), xnd33(128),
     4  xnd14(128), xnd24(128), xnd34(128),
     5 yhatx1(128),yhaty1(128),yhatz1(128),
     6 yhatx2(128),yhaty2(128),yhatz2(128),
     7 zhatx1(128),zhaty1(128),zhatz1(128),
     8 zhatx2(128),zhaty2(128),zhatz2(128),
     9 cx1(128),cy1(128),cz1(128),cx2(128),cy2(128),cz2(128),
     & cx3(128),cy3(128),cz3(128),cx4(128),cy4(128),cz4(128)
      common/aux32/
     1 dxy(128),dyx(128),dyz(128),dzy(128),dzx(128),dxz(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),b12(128),b13(128),a21(128),b22(128),b23(128),
     2 a31(128),b32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/result/yint(128),zint(128),ywth(128),zwth(128)
      dimension ybar(*),zbar(*)
      yybar1=.5*( eta-ybar(1))
      yybar2=.5*( eta-ybar(2))
      zzbar1=.5*(zeta-zbar(1))
      zzbar2=.5*(zeta-zbar(2))
      do 10 i=lft,llt
      y1h(i)=fbly1(i)*yybar1
      y2h(i)=fbly2(i)*yybar2
      z1h(i)=fblz1(i)*zzbar1
      z2h(i)=fblz2(i)*zzbar2
      yint(i)=.5*(y1h(i)+y2h(i))
      zint(i)=.5*(z1h(i)+z2h(i))
      ywth(i)=.25*(fbly1(i)+fbly2(i))
      zwth(i)=.25*(fblz1(i)+fblz2(i))
   10 continue
      do 20 i=lft,llt
      xnd11(i)=cx1(i)+y1h(i)*yhatx1(i)+z1h(i)*zhatx1(i)
      xnd21(i)=cy1(i)+y1h(i)*yhaty1(i)+z1h(i)*zhaty1(i)
      xnd31(i)=cz1(i)+y1h(i)*yhatz1(i)+z1h(i)*zhatz1(i)
      xnd12(i)=cx2(i)+y2h(i)*yhatx2(i)+z2h(i)*zhatx2(i)
      xnd22(i)=cy2(i)+y2h(i)*yhaty2(i)+z2h(i)*zhaty2(i)
      xnd32(i)=cz2(i)+y2h(i)*yhatz2(i)+z2h(i)*zhatz2(i)
   20 continue
      do 30 i=lft,llt
      a11(i)=.50*(xnd12(i)-xnd11(i))
      a21(i)=.50*(xnd22(i)-xnd21(i))
      a31(i)=.50*(xnd32(i)-xnd31(i))
   30 continue
      do 50 i=lft,llt
      c1(i)=a22(i)*a33(i)-a32(i)*a23(i)
      c2(i)=a32(i)*a13(i)-a12(i)*a33(i)
      c3(i)=a12(i)*a23(i)-a13(i)*a22(i)
      det(i)=.25/(a11(i)*c1(i)+a21(i)*c2(i)+a31(i)*c3(i))
      xgj11(i)= 2.*det(i)*c1(i)
      xgj21(i)= 2.*det(i)*c2(i)
      xgj31(i)= 2.*det(i)*c3(i)
      xgj12(i)=-det(i)*(a21(i)*a33(i)-a23(i)*a31(i))
      xgj22(i)= det(i)*(a11(i)*a33(i)-a13(i)*a31(i))
      xgj32(i)=-det(i)*(a11(i)*a23(i)-a13(i)*a21(i))
      xgj13(i)= det(i)*(a21(i)*a32(i)-a22(i)*a31(i))
      xgj23(i)=-det(i)*(a11(i)*a32(i)-a12(i)*a31(i))
      xgj33(i)= det(i)*(a11(i)*a22(i)-a12(i)*a21(i))
      det(i)  =4.*det(i)
   50 continue
      do 60 i=lft,llt
      s31(i)=a21(i)*a32(i)-a22(i)*a31(i)
      s32(i)=a31(i)*a12(i)-a32(i)*a11(i)
      s33(i)=a11(i)*a22(i)-a12(i)*a21(i)
      enrm(i)  =1./sqrt(s31(i)**2+s32(i)**2+s33(i)**2)
      s31(i)=s31(i)*enrm(i)
      s32(i)=s32(i)*enrm(i)
      s33(i)=s33(i)*enrm(i)
   60 continue
      do 70 i=lft,llt
      enrm(i)  =1./sqrt(a11(i)**2+a21(i)**2+a31(i)**2)
      s11(i)=a11(i)*enrm(i)
      s12(i)=a21(i)*enrm(i)
      s13(i)=a31(i)*enrm(i)
      s21(i)=s32(i)*s13(i)-s12(i)*s33(i)
      s22(i)=s33(i)*s11(i)-s13(i)*s31(i)
      s23(i)=s31(i)*s12(i)-s11(i)*s32(i)
   70 continue
      do 90 i=lft,llt
      px1(i)= -xgj11(i)
      px2(i)=  xgj11(i)
      px5(i)= y1h(i)*px1(i)+fbly1(i)*xgj12(i)
      px6(i)= y2h(i)*px2(i)+fbly2(i)*xgj12(i)
      px7(i)= z1h(i)*px1(i)+fblz1(i)*xgj13(i)
      px8(i)= z2h(i)*px2(i)+fblz2(i)*xgj13(i)
      py1(i)= -xgj21(i)
      py2(i)=  xgj21(i)
      py5(i)= y1h(i)*py1(i)+fbly1(i)*xgj22(i)
      py6(i)= y2h(i)*py2(i)+fbly2(i)*xgj22(i)
      py7(i)= z1h(i)*py1(i)+fblz1(i)*xgj23(i)
      py8(i)= z2h(i)*py2(i)+fblz2(i)*xgj23(i)
      pz1(i)= -xgj31(i)
      pz2(i)=  xgj31(i)
      pz5(i)= y1h(i)*pz1(i)+fbly1(i)*xgj32(i)
      pz6(i)= y2h(i)*pz2(i)+fbly2(i)*xgj32(i)
      pz7(i)= z1h(i)*pz1(i)+fblz1(i)*xgj33(i)
      pz8(i)= z2h(i)*pz2(i)+fblz2(i)*xgj33(i)
   90 continue
      do 110  i=lft,llt
      dxx(i)= px1(i)*dx12(i)+
     & px5(i)*dx5(i)+px6(i)*dx6(i)+px7(i)*dx7(i)+px8(i)*dx8(i)
      dyy(i)= py1(i)*dy12(i)+
     & py5(i)*dy5(i)+py6(i)*dy6(i)+py7(i)*dy7(i)+py8(i)*dy8(i)
      dzz(i)= pz1(i)*dz12(i)+
     & pz5(i)*dz5(i)+pz6(i)*dz6(i)+pz7(i)*dz7(i)+pz8(i)*dz8(i)
      dxy(i)= py1(i)*dx12(i)+
     & py5(i)*dx5(i)+py6(i)*dx6(i)+py7(i)*dx7(i)+py8(i)*dx8(i)
      dxz(i)= pz1(i)*dx12(i)+
     & pz5(i)*dx5(i)+pz6(i)*dx6(i)+pz7(i)*dx7(i)+pz8(i)*dx8(i)
      dyz(i)= pz1(i)*dy12(i)+
     & pz5(i)*dy5(i)+pz6(i)*dy6(i)+pz7(i)*dy7(i)+pz8(i)*dy8(i)
      dyx(i)= px1(i)*dy12(i)+
     & px5(i)*dy5(i)+px6(i)*dy6(i)+px7(i)*dy7(i)+px8(i)*dy8(i)
      dzx(i)= px1(i)*dz12(i)+
     & px5(i)*dz5(i)+px6(i)*dz6(i)+px7(i)*dz7(i)+px8(i)*dz8(i)
      dzy(i)= py1(i)*dz12(i)+
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
      subroutine conmdb (nmtcon,auxvec,cm,lav,mte,nip,ipt,capa,
     1 strr,weight,dampk,ym,prv)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common /  / a(1)
      dimension cm(*),auxvec(*),strr(*)
      lavloc=(ipt-1)*nmtcon+lav
      if (mte.eq.1) then
      call tspc1b (nmtcon,auxvec(lavloc),nip*nmtcon)
      call rttrsb
      call ldsigb
      call ldstrb
      call fhl1b (cm,capa)
      call cmprsl (strr,weight)
      call gblssb
      call tspc2b (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      elseif (mte.eq.3) then
      call tspc1b (nmtcon,auxvec(lavloc),nip*nmtcon)
      call rttrsb
      call rttrnb
      call ldsigb
      call ldstrb
      call fhl3b (cm,capa)
      call cmprsl (strr,weight)
      call gblssb
      call tspc2b (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      else
      write (13,10) mte
      write ( *,10) mte
      call adios (2)
      endif
      if( dampk.ne.0.) call rydmp3(dampk,ym,prv)
      return
   10 format (///' material type',i5,' is not available for',/
     1           ' the hughes-liu beam element.',///)
      end
      subroutine tspc1b (nc,aux,ncl)
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
      subroutine tspc2b (nc,aux,lav,ncl,nip,ipt)
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
      subroutine ldsigb
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
      subroutine ldstrb
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
      d4(i)=2.*(s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i))
      d6(i)=2.*(s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i))
   40 continue
      return
      end
      subroutine gblssb
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
      a21(i)=sig4(i)*s11(i)
      a22(i)=sig4(i)*s12(i)
      a23(i)=sig4(i)*s13(i)
      a31(i)=sig6(i)*s11(i)
      a32(i)=sig6(i)*s12(i)
      a33(i)=sig6(i)*s13(i)
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
      subroutine rttrsb
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
      subroutine rttrnb
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
      subroutine rwngvb (wxx,wyy,wzz,lft,llt)
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
      subroutine fhl1b (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
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
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      dimension  cm(1)
      data third/-.3333333333333/
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      fac1=1.5*g
      blk =-ym/(1.-2.*pr)
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      wq4=capa*wq2
      sndspd=.67*g+third*blk
      deti=1./(wq3*wq3-wq1*wq1)
      c22i=wq3*deti
      c23i=-wq1*deti
      fac=(c22i+c23i)*wq1
      do 30 i=lft,llt
      einc(i)=d1(i)*sig1(i)+d4(i)*sig4(i)+d6(i)*sig6(i)
      cb(i)=ss
      d2(i)=-d1(i)*fac-sig2(i)*c22i-sig3(i)*c23i
      d3(i)=-d1(i)*fac-sig2(i)*c23i-sig3(i)*c22i
      sig4(i)=sig4(i)+wq4*d4(i)
      sig5(i)=0.
      sig6(i)=sig6(i)+wq4*d6(i)
   30 continue
      do 40 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      sig1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      sig2(i)=0.
      sig3(i)=0.
      einc(i)=d1(i)*sig1(i)+d4(i)*sig4(i)+d6(i)*sig6(i)+einc(i)
   40 continue
      return
      end
      subroutine fhl3b (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
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
     2 sig5(128),sig6(128),  ep(128),epx1(128),
     3 epx2(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qh=cm(mx+26)
      qb=cm(mx+21)
      qs=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      qbqh=qb*qh
      fac1=1.5*g
      qa  =1.-qb
      blk =-ym/(1.-2.*pr)
      wq1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      wq2=.5*g
      wq3=wq1+2.0*wq2
      wq4=capa*wq2
      sndspd=.66666666666667*g+third*blk
c
      if (qb.lt.0.9999999) then
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
      deti=1./(wq3*wq3-wq1*wq1)
      c22i=wq3*deti
      c23i=-wq1*deti
      fac=(c22i+c23i)*wq1
      do 30 i=lft,llt
      cb(i)=ss
      d2(i)=-d1(i)*fac-sig2(i)*c22i-sig3(i)*c23i
      d3(i)=-d1(i)*fac-sig2(i)*c23i-sig3(i)*c22i
      da4(i)=sig4(i)+wq4*d4(i)
      da6(i)=sig6(i)+wq4*d6(i)
      t4(i)=da4(i)-epx4(i)
      t6(i)=da6(i)-epx6(i)
      t456(i)=3.*(t4(i)*t4(i)+t6(i)*t6(i))
   30 ak(i)=qs+qbqh*ep(i)
      do 40 i=lft,llt
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =0.0
      depi(i)=0.0
      deps(i)=0.0
      einc(i)=d1(i)*sig1(i)+d4(i)*sig4(i)+d6(i)*sig6(i)
   40 continue
      scl=0.
      do 45 i=lft,llt
   45 scl=scl+scle(i)
      if (nint(scl).eq.0) then
      do 66 i=lft,llt
      sig1(i)=da1(i)
      sig2(i)=0.
      sig3(i)=0.
      sig4(i)=da4(i)
      sig5(i)=0.
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d4(i)*sig4(i)+d6(i)*sig6(i)+einc(i)
   66 continue
      go to 75
      endif
      fac1qh=1./(fac1+qh)
      do 60 i=lft,llt
      if (scle(i).eq.0.)  go to 60
      sig2(i)=0.
      sig3(i)=0.
      d2(i)=-d1(i)*fac
      d3(i)=-d1(i)*fac
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      if (scle(i).eq.0.)  go to 60
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3lst=sg3new
      sg3new=sg3old
      epslst=epsnew
      epsnew=d3(i)
      d3(i)=-.5*d1(i)
      d2(i)=d3(i)
      do 50 iter=2,20
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      p(i)=blk*davg(i)
      da1(i)=sig1(i)+p(i)+g*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+g*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+g*(d3(i)+davg(i))
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)-epx1(i)
      t2(i)=p(i)+da2(i)-epx2(i)
      t3(i)=p(i)+da3(i)+epx1(i)+epx2(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t456(i)
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      aj1(i) =sqrt(aj2(i))+1.0-scle(i)
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh
      deps(i)=fac1*depi(i)/aj1(i)
      sg3old=da3(i)  -deps(i)*t3(i)
      sg3lst=sg3new
      sg3new=sg3old
      epslst=epsnew
      epsnew=d3(i)
      demn=1.e-20+sg3new-sg3lst
      if (abs(demn).lt.1.e-14) go to 60
      d3(i)=epslst-sg3lst*(epsnew-epslst)/demn
      d2(i)=d3(i)
      if(abs(epsnew-epslst)/(abs(d3(i))+1.e-07).lt..0001) go to 60
   50 continue
   60 continue
      fac2=qa*qh/fac1
      do 70 i=lft,llt
      depn(i) =fac2*deps(i)
      sig1(i)=da1(i)  -deps(i)*(t1(i))
      sig2(i)=0.0
      sig3(i)=0.0
      sig4(i)=da4(i)  -deps(i)*t4(i)
      sig5(i)=0.0
      sig6(i)=da6(i)  -deps(i)*t6(i)
      epx1(i)=epx1(i)+depn(i)*t1(i)
      epx2(i)=epx2(i)+depn(i)*t2(i)
      ep(i)  =  ep(i)+depi(i)
      epx4(i)=epx4(i)+depn(i)*t4(i)
      epx6(i)=epx6(i)+depn(i)*t6(i)
      einc(i)=d1(i)*sig1(i)+d4(i)*sig4(i)+d6(i)*sig6(i)+einc(i)
   70 continue
c
   75 if (qb.lt.0.9999999) then
      do 80 i=lft,llt
      epx3(i)=-epx1(i)-epx2(i)
      a11(i)=epx1(i)*s11(i)+epx4(i)*s21(i)+epx6(i)*s31(i)
      a12(i)=epx1(i)*s12(i)+epx4(i)*s22(i)+epx6(i)*s32(i)
      a13(i)=epx1(i)*s13(i)+epx4(i)*s23(i)+epx6(i)*s33(i)
      a21(i)=epx4(i)*s11(i)+epx2(i)*s21(i)
      a22(i)=epx4(i)*s12(i)+epx2(i)*s22(i)
      a23(i)=epx4(i)*s13(i)+epx2(i)*s23(i)
      a31(i)=epx6(i)*s11(i)+epx3(i)*s31(i)
      a32(i)=epx6(i)*s12(i)+epx3(i)*s32(i)
      a33(i)=epx6(i)*s13(i)+epx3(i)*s33(i)
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
      subroutine renorm
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
      common/   /a(1)
      dimension ix(10)
      call inssnm(a(lc11),a(lc1s),a(ns03),ix,ix(2),numels)
      return
      end
      subroutine ishlel
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
      common/shlopt/istrn(1),istupd,ibelyt,miter
      common/   /a(1)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      if (istupd.ne.0) then
      call azero (a(istupd),numnp)
      endif
c
      isf=(ns08-ns07)/(2*numels)
      call elem2d (a(lc1s),a(lc11),a(lc10),a(lc9),a(lc9+neq),
     1a(ns01),a(ns03),a(ns05),a(ns06),a(n1),a(n2),a(n3),a(n4f),a(ns08),
     2a(ns09),a(ns10),a(n4c),a(n4c+nmmat),a(ns07),a(n25+7),a(n4h),
     3mpusr,a(n1+nmmat),a(ns07+numels),isf,a(ns14),a(ns05+9*numels),
     4 a(ns17),a(ns11),a(ns12),a(n80),a(n80+nmmat),a(n80+2*nmmat))
c
      return
      end
      subroutine inssnm(x,ipss,yhat,mx,ix,numels)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/xyzary/x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4
c     dimension x(3,*),ix(*),xyz(12),ipss(2,*),yhat(12,*),mx(*)         cray1
      dimension x(3,*),ix(*),xyz(12),ipss(5,*),yhat(12,*),mx(*)         vax75
      equivalence (xyz,x1)
      do 160 n=1,numels
      call unpk(mx,ix,ipss(1,n),2)
      do 10 i=1,4
      i2=3*i
      xyz(i2-2)=x(1,ix(i))
      xyz(i2-1)=x(2,ix(i))
   10 xyz(i2)  =x(3,ix(i))
      call fbvcsh(ix,yhat(1,n))
  160 continue
      return
      end
      subroutine fbvcsh(ix,yhat)
c     implicit double precision (a-h,o-z)                                    dp
      common/xyzary/x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4
      dimension ix(1),xyz(3,4),yhat(3,4),ehat(3,4)
      x21=x2-x1
      y21=y2-y1
      z21=z2-z1
      x31=x3-x1
      y31=y3-y1
      z31=z3-z1
      x41=x4-x1
      y41=y4-y1
      z41=z4-z1
      x42=x4-x2
      y42=y4-y2
      z42=z4-z2
      c1=y31*z42-z31*y42
      c2=z31*x42-x31*z42
      c3=x31*y42-y31*x42
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s31=c1*xl
      s32=c2*xl
      s33=c3*xl
      yhat(1,1)=s31
      yhat(2,1)=s32
      yhat(3,1)=s33
      yhat(1,2)=s31
      yhat(2,2)=s32
      yhat(3,2)=s33
      yhat(1,3)=s31
      yhat(2,3)=s32
      yhat(3,3)=s33
      yhat(1,4)=s31
      yhat(2,4)=s32
      yhat(3,4)=s33
      return
      end
      subroutine elem2d(ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,rule,
     2mpusr,ishlfm,tfail,isf,lochvs,qextra,nshel,nncs,iblks,
     3 dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     main subroutine for calling two-dimensional elements
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk28/summss,xke,xpe,tt
      common/failu/sieu(128),fail(128)
      common/aux01/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
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
      common/aux36/lft,llt
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dxl(128)
      common/energy/xinen
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      character*4 mess                                                  vax75
      common/aux38/mess
      character*6 elname
      common/elnme/ elname(5)
      common/bttn/ntnwf,ixa(10)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/kinet/enkint(128),xmomnt(128),ymomnt(128),zmomnt(128)
      common/eltspf/ietspf
      common/hourg/ymod,gmod,ifsv
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/sides/sidmn(128)
      common/   /a(1)
      common/tsarry/tslimt,tsarry(128)
      common/double/iprec,ncpw,unit
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension ixp(5,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     vax75
c     dimension ixp(2,*),x(3,*),rhs(*),vt(3,*),vr(3,*),yhatn(12,*),     cray1
     1 auxvec(*),mtype(*),ro(*),cm(*),csprop(24,*),ishlfm(*),xyz(12),
     2 fibl(9,*),ybarn(12),rhse(24),nsubgv(*),mtnum(*),ener(*),
     3 nfegp(*),ihgq(*),hgq(*),strain(12,*),rule(mpusr,3,*),tfail(*),
     4 lochvs(*),qextra(*),nshel(*),nncs(*),iblks(*),xies(*),
     5 dampk(*),ym(*),pr(*)
      lcz=0
      ic=0
      lczc=0
      nelg=(numels-1)/128+1
      nmel=128
      nrng=24*nmel
      do 10 i=1,128
   10 tsarry(i)=1.0
      do 410 nn=1,nelg
      do 20 i=1,nrng
   20 ep11(i)=0.
      nnm1=128*(nn-1)
      nmel=128
      if (nn.eq.nelg) nmel=numels-128*(nelg-1)
      call unpcts (ixp(1,nnm1+1),nmel)
      if (tslimt.ne.0.0) then
      call blkcpy (a(ns16+nnm1),tsarry,nmel)
      tsm=1./tslimt
      do 30 i=1,nmel
      tsarry(i)=  min(1.0*unit,tsarry(i)*tsm)**2
   30 continue
      endif
      nsubg=nsubgv(nn)
      do 400 n=1,nsubg
      ic=ic+1
      nnc=nncs(ic)
      lcn=lcz+n
      mxe=mtnum(lcn)
      mte=mtype(mxe)
      if (mxe.eq.0.or.mte.eq.20) go to 390
      lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvs(nnm1+lft)
c
c     call specified shell formulation
c
      iop=ishlfm(mxe)
      if (iop.eq.1) then
      call hghliu(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk(mxe),ym(mxe),pr(mxe))
      elseif (iop.eq.2) then
      if (mte.ne.28) then
      call blytsy(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk(mxe),ym(mxe),pr(mxe))
      else
      call q4rslt(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iop,iblks,dampk(mxe),
     3 ym(mxe),pr(mxe))
      endif
      elseif (iop.eq.3) then
      call cstbcz(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk(mxe),ym(mxe),pr(mxe))
      elseif (iop.eq.4) then
      call cstc0t(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk(mxe),ym(mxe),pr(mxe))
      elseif (iop.eq.5) then
      call memb3d(rule,ixp,x,rhs,vt,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iblks,dampk(mxe),ym(mxe),pr(mxe))
      elseif (iop.eq.6) then
      if (mte.ne.28) then
      call berwsh(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,qextra,iblks,dampk(mxe),
     3 ym(mxe),pr(mxe))
      else
      call q4rslt(rule,ixp,x,rhs,vt,vr,strain,yhatn,fibl,auxvec,
     1 mtype,ro,cm,csprop,nsubgv,mtnum,nfegp,ihgq,hgq,xies,ener,
     2 mpusr,lav,nmel,nnm1,mxe,iop,qextra,iblks,dampk(mxe),
     3 ym(mxe),pr(mxe))
      endif
      endif
c
c     print out computed step size for each element at time=0
c
      if (isdo.eq.2) sndspd=sqrt(ymod/ro(mxe))
c
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      do 60 i=lft,llt
      sndsp(i) =sarea(i)/(sqrt(diagm(i))*sndspd)+10000.*(1.-fail(i))
      dt2=  min(sndsp(i),dt2)
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ielmtc=nshel(ielmtc)
      ityptc=3
      endif
      ielmtd=nnm1+i
      ielmtd=nshel(ielmtd)
      write (13,420) ielmtd,elname(ityptc),ielmtc,sndsp(i)
   60 continue
      endif
      endif
      endif
c
c     compute the time step size, total energy, and energy by material
c
      mxf=mxe+nmmat
      mxg=2*nmmat+3*(mxe-1)+1
      mxh=2*nmmat+3*(mxe-1)+2
      mxi=2*nmmat+3*(mxe-1)+3
      enere=ener(mxe)
      enerf=ener(mxf)
      energ=ener(mxg)
      enerh=ener(mxh)
      eneri=ener(mxi)
      if (isdo.ne.2) then
      do 280 i=lft,llt
      sndsp(i) =sarea(i)/(sqrt(diagm(i))*sndspd)+10000.*(1.-fail(i))
      dt2=  min(dt2,sndsp(i))
      xinen    =xinen+xies(nnm1+i)
      enere=enere+xies(i+nnm1)
      enerf=enerf+sarea(i)*enkint(i)
      energ=energ+sarea(i)*xmomnt(i)
      enerh=enerh+sarea(i)*ymomnt(i)
      eneri=eneri+sarea(i)*zmomnt(i)
  280 continue
      else
      do 282 i=lft,llt
      xlngth=  max(sarea(i)/sqrt(diagm(i)),sqrt(sidmn(i)))
      sndsp(i) =xlngth/sndspd+10000.*(1.-fail(i))
      dt2=  min(dt2,sndsp(i))
      xinen    =xinen+xies(nnm1+i)
      enere=enere+xies(i+nnm1)
      enerf=enerf+sarea(i)*enkint(i)
      energ=energ+sarea(i)*xmomnt(i)
      enerh=enerh+sarea(i)*ymomnt(i)
      eneri=eneri+sarea(i)*zmomnt(i)
  282 continue
      endif
      ener(mxe)=enere
      ener(mxf)=enerf
      ener(mxg)=energ
      ener(mxh)=enerh
      ener(mxi)=eneri
c
      if (tslimt.ne.0.0) then
      if (mte.eq.3.or.mte.eq.19.or.mte.eq.24) then
      dt2=  max(dt2,tslimt)
      endif
      call strtss (a(ns16+nnm1),sndsp,lft,llt)
      endif
c
      if (isf.eq.1) then
      do 290 i=lft,llt
      if (fail(i).eq.0.0.and.tfail(i+nnm1).eq.0.0) then
      tfail(i+nnm1)=-tt
      endif
  290 continue
      endif
c
c     determine controlling element for delta-t if user types 'sw2.'
c
      if (mess.eq.'sw2.') then
      do 300 i=lft,llt
      if (dt2.eq.sndsp(i)) then
      ielmtc=nnm1+i
      ityptc=3
      ielmtc=nshel(ielmtc)
      endif
  300 continue
      endif
c
  390 lczc=lczc+nnc+1
      if (output) then
      call acmfcs (a(ncs1),a(ncs3),a(ncs6),a(ncs8),a(ncs9),numcsd,
     1 nnm1)
      endif
  400 continue
      lcz=lcz+nsubg+1
  410 continue
      if (ntbsl.ne.0.and.ntnwf.ne.0) then
      call blkcpy (a(ipi),a(ixa(8)),numels)
      call blkcpy (a(iph),a(ixa(7)),numels)
      endif
      return
  420 format(' initial dt for shell element',i6,' [',a6,i6,']',e10.3)
      end
      subroutine strtss (stsize,sndsp,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      dimension stsize(*),sndsp(*)
      do 10 i=lft,llt
      stsize(i)=  min(stsize(i),sndsp(i))
   10 continue
      return
      end
      subroutine acmfcs(icsd,mnmxs,nsshs,nsh,result,numcsd,nnm1)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux36/lft,llt
      common/csfsav/
     &sfx1(128),sfy1(128),sfz1(128),
     &sfx2(128),sfy2(128),sfz2(128),
     &sfx3(128),sfy3(128),sfz3(128),
     &sfx4(128),sfy4(128),sfz4(128),
     &fail(128),ndof,ifail
      dimension icsd(3,1),mnmxs(2,1),nsshs(1),nsh(4,1),result(9,1)
      k0=0
      nf=nnm1+lft
      nl=nnm1+llt
      do 50 i=1,numcsd
      nnsh=icsd(3,i)
      if (nnsh.eq.0) go to 50
      if (mnmxs(1,i).gt.nl) go to 40
      if (mnmxs(2,i).lt.nf) go to 40
      do 30 l=1,nnsh
      nel =nsshs(l+k0)
      if (nel.ge.nf.and.nel.le.nl) then
      nel=nel-nnm1
      if (ifail.eq.0) then
      if (nsh(1,l).eq.1) then
      result(1,i)=result(1,i)-sfx1(nel)
      result(2,i)=result(2,i)-sfy1(nel)
      result(3,i)=result(3,i)-sfz1(nel)
      endif
      if (nsh(2,l).eq.1) then
      result(1,i)=result(1,i)-sfx2(nel)
      result(2,i)=result(2,i)-sfy2(nel)
      result(3,i)=result(3,i)-sfz2(nel)
      endif
      if (nsh(3,l).eq.1) then
      result(1,i)=result(1,i)-sfx3(nel)
      result(2,i)=result(2,i)-sfy3(nel)
      result(3,i)=result(3,i)-sfz3(nel)
      endif
      if (ndof.eq.4) then
      if (nsh(4,l).eq.1) then
      result(1,i)=result(1,i)-sfx4(nel)
      result(2,i)=result(2,i)-sfy4(nel)
      result(3,i)=result(3,i)-sfz4(nel)
      endif
      endif
      else
      if (nsh(1,l).eq.1) then
      result(1,i)=result(1,i)-fail(nel)*sfx1(nel)
      result(2,i)=result(2,i)-fail(nel)*sfy1(nel)
      result(3,i)=result(3,i)-fail(nel)*sfz1(nel)
      endif
      if (nsh(2,l).eq.1) then
      result(1,i)=result(1,i)-fail(nel)*sfx2(nel)
      result(2,i)=result(2,i)-fail(nel)*sfy2(nel)
      result(3,i)=result(3,i)-fail(nel)*sfz2(nel)
      endif
      if (nsh(3,l).eq.1) then
      result(1,i)=result(1,i)-fail(nel)*sfx3(nel)
      result(2,i)=result(2,i)-fail(nel)*sfy3(nel)
      result(3,i)=result(3,i)-fail(nel)*sfz3(nel)
      endif
      if (ndof.eq.4) then
      if (nsh(4,l).eq.1) then
      result(1,i)=result(1,i)-fail(nel)*sfx4(nel)
      result(2,i)=result(2,i)-fail(nel)*sfy4(nel)
      result(3,i)=result(3,i)-fail(nel)*sfz4(nel)
      endif
      endif
      endif
      endif
   30 continue
   40 k0=k0+nnsh
   50 continue
      return
      end
