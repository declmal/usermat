      subroutine lamex3(eta1,eta2,eta4,r,scle,alfaht1,betaht1,ym,
     1           lft,llt,xlamkp1)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/double/iprec,ncpw,unit
      dimension eta1(*),eta2(*),eta4(*),r(*),scle(*),xlamkp1(*)
      dimension asq(128),bsq(128),alc(128),blc(128),clc(128),dlc(128),
     1          p(128),q(128),y(128),capq(128),s(128),t(128)
      dimension scr1(128),scr2(128),scr3(128),x1(128)
      dimension db2(128),db6(128),db7(128)
c
      data third,sixth/0.3333333333333,0.16666666666667/
      tol1 = 1.0e-12
      tol2 = 1.0e-8
      alfaht = alfaht1/ym
      betaht = betaht1/ym
c
      do 10 i=lft,llt
      asq(i) = (eta1(i) + eta2(i))**2/(6.*r(i)**2)
      bsq(i) = (0.5*(eta2(i) - eta1(i))**2 + 2.*eta4(i)**2)/r(i)**2
      alc(i) = 2./alfaht + 2./betaht
      blc(i) = 4./(alfaht*betaht) + (1. - bsq(i))/betaht/betaht
     1         + (1. - asq(i))/alfaht/alfaht
      clc(i) = 2.*( (1. - bsq(i))/alfaht/betaht/betaht
     1         + (1. - asq(i))/betaht/alfaht/alfaht )
      dlc(i) = (1. - asq(i) - bsq(i))/alfaht/alfaht/betaht/betaht
c
      p(i) = ((blc(i)/3.)**2 - (alc(i)*clc(i) - 4.*dlc(i))/3.)*scle(i)
     1    + (1. - scle(i))
      q(i) = (blc(i)*( (blc(i)**2)/27. - (alc(i)*clc(i) - 4.*dlc(i))/6.)
     1       + (alc(i)**2 *dlc(i) - 4.*blc(i)*dlc(i) + clc(i)**2)/2.)
     1    *scle(i) + 2.*(1. - scle(i))
c
      scr3(i) = p(i)**3 + tol1*sign(1.0*unit,p(i))
c
      if((abs(q(i)**2/scr3(i)) .ge. sign(1.*unit,p(i))) .and.
     1     (abs(q(i)/sqrt(abs(scr3(i))) + 1.0)) .gt. tol2) then
c
          x1(i) = sqrt(q(i)**2/abs(scr3(i))-sign(1.0*unit,scr3(i)) )
c
          scr1(i) = q(i)/sqrt(abs(scr3(i))) + x1(i)
          scr2(i) = q(i)/sqrt(abs(scr3(i))) - x1(i)
c
          y(i) = ((abs(scr1(i)))**third*sign(1.0*unit,scr1(i))
     1         +  (abs(scr2(i)))**third*sign(1.0*unit,scr2(i)))
     2         *abs(scr3(i))**sixth + third*blc(i)
      else
c
      db2(i) = q(i)/sqrt(scr3(i))
      db2(i) = sign(1.*unit,db2(i))*0.5*(1.0+sign(1.0*unit,abs(db2(i))-1.
     1   *unit))
     1          + db2(i) *0.5*(1.-sign(1.0*unit,abs(db2(i))-1.))
c
          y(i) = 2.*abs(scr3(i))**sixth*
     3         cos(third*acos(db2(i)))
     4         + third*blc(i)
      endif
c
      capq(i) = sqrt(abs(alc(i)**2/4. - blc(i) + y(i)))
c
      s(i) = 0.75*alc(i)**2 - 2.*blc(i) - capq(i)**2
c
      if(capq(i)*capq(i) .gt. tol1) then
          t(i) = (4.*alc(i)*blc(i) - 8.*clc(i) - alc(i)**3)/
     1       (4.*capq(i))
      else
          t(i) = 2.*sqrt( abs(y(i)**2 - 4.*dlc(i)) )
      endif
c
      db6(i) =(-alc(i)/4.+capq(i)/2.+sqrt(abs(s(i)+t(i)))/2.)*scle(i)/ym
      db7(i) =(-alc(i)/4.-capq(i)/2.+sqrt(abs(s(i)-t(i)))/2.)*scle(i)/ym
c
      if( (t(i) + s(i)) .gt. 0.0) then
          xlamkp1(i) =  db6(i)
      else
          xlamkp1(i) =  db7(i)
      endif
c
c.... correct for roundoff error causing a branch jump if needed
c
      if( xlamkp1(i) .lt. 0.0 ) xlamkp1(i) = max(db6(i),db7(i))
c
   10 continue
c
      return
      end
      subroutine shl33s(cm)
c
c*********************************
c**       use std. Cep method, small de=.00002
c    thetar(m) = 'beta' angle for element nel=m+nftm1
c
c*********************************
c
c***d3***
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
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/tsarry/tslimt,tsarry(128)
      common/sound/sndspd,sndsp(128),diagm(128),sarea(128),dx1(128)
      common/hourg/ymod,gmod,ifsv
      common/hour11/ebar(128),ebarmn(128),eyld(128),etanmd(128)
c***d3***
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
      dimension cm(*)
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+10)
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
       write( *,*) ' tang mod goes to zero'
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
      ebar(i)=ymod
      eyld(i)=qs
      etanmd(i)=qh
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
      dd1(m)=tsarry(m)*(cc(m)*d1(m)+ss(m)*d2(m)+cs(m)*d4(m))
      dd2(m)=tsarry(m)*(ss(m)*d1(m)+cc(m)*d2(m)-cs(m)*d4(m))
      dd3(m)=tsarry(m)*(d3(m)                              )
      dd4(m)=tsarry(m)*(2.*cs(m)*(d2(m)-d1(m))+(cc(m)-ss(m))*d4(m))
      dd5(m)=tsarry(m)*(d5(m)                                     )
      dd6(m)=tsarry(m)*(d6(m)                                     )
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
c** plane stress 050288
      dt(3)=-(sigo(3)+ce(3,1)*dt(1)+ce(3,2)*dt(2))/ce(3,3)
      epsnew=0.
      signew=0.
      do 500 iter=1,20
      epslst=epsnew
      siglst=signew
c
c** trial stress
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
      ebar(m)=ymod
      if(s1str.le.x1) go to 299
c
c** 6-27-90
      ebar(m)=qh
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
      epsnew=dt(3)
      signew=sigt(3)
      if(iter.eq.1) then
       dt(3)=-dt(1)-dt(2)
       go to 500
      endif
      demn=1.0e-24*x1+signew-siglst
      if(abs(demn).lt.(1.0e-20*x1)) go to 501
      if(abs(signew).lt.(1.0e-08*e)) go to 501
      dt(3)=epslst-siglst*(epsnew-epslst)/demn
      if(abs((epsnew-epslst)/dt(3)).lt.1.0e-05) go to 501
  500 continue
      write(17,240)
      write(35,240)
      write( *,240)
  240 format(' convergence failure in plane stress subrootine')
  501 continue
      d3(m)=dt(3)
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
   70 continue
      return
      end
c-----------------------------------------------------------------------
      subroutine cepx(cep ,sig ,dpl,etan,ce,cpl)
c
      dimension cep(6,*),sig(*),dpl(6,*),ce(6,*),cpl(6,*)
      dimension q(6),p(6),cq(6),cqcqd(6,6)
c
c** get cep(i,j).. see 4-19-88
      q(1)=dpl(1,1)*sig(1)+dpl(1,2)*sig(2)+dpl(1,3)*sig(3)
      q(2)=dpl(2,1)*sig(1)+dpl(2,2)*sig(2)+dpl(2,3)*sig(3)
      q(3)=dpl(3,1)*sig(1)+dpl(3,2)*sig(2)+dpl(3,3)*sig(3)
      q(4)=dpl(4,4)*sig(4)
      q(5)=dpl(5,5)*sig(5)
      q(6)=dpl(6,6)*sig(6)
c
      p(1)=cpl(1,1)*q(1)+cpl(1,2)*q(2)+cpl(1,3)*q(3)
      p(2)=cpl(2,1)*q(1)+cpl(2,2)*q(2)+cpl(2,3)*q(3)
      p(3)=cpl(3,1)*q(1)+cpl(3,2)*q(2)+cpl(3,3)*q(3)
      p(4)=cpl(4,4)*q(4)
      p(5)=cpl(5,5)*q(5)
      p(6)=cpl(6,6)*q(6)
c
      cq(1)=ce(1,1)*q(1)+ce(1,2)*q(2)+ce(1,3)*q(3)
      cq(2)=ce(2,1)*q(1)+ce(2,2)*q(2)+ce(2,3)*q(3)
      cq(3)=ce(3,1)*q(1)+ce(3,2)*q(2)+ce(3,3)*q(3)
      cq(4)=ce(4,4)*q(4)
      cq(5)=ce(5,5)*q(5)
      cq(6)=ce(6,6)*q(6)
c
      qcq=0.
      ptq=.0000001
      do 152 i=1,6
      qcq=qcq+q(i)*cq(i)
  152 ptq=ptq+p(i)*q(i)
      ptqqcq=ptq+qcq
c
      do 153 i=1,6
      cqd=cq(i)/ptqqcq
      do 153 j=1,6
  153 cqcqd(i,j)=cqd*cq(j)
c
      do 154 i=1,6
      do 154 j=1,6
      cep(i,j)=ce(i,j)-cqcqd(i,j)
  154 continue
      return
      end
c---------------------------------------------------------
      subroutine inverr(x)
c
      dimension x(6,*)
c
      a=x(1,1)
      b=x(1,2)
      c=x(1,3)
      d=x(2,2)
      e=x(2,3)
      f=x(3,3)
      adbb=a*d-b*b
      becd=b*e-c*d
      bcae=b*c-a*e
      den=f*adbb+c*becd+e*bcae
      cebf=c*e-b*f
      afcc=a*f-c*c
      dfee=d*f-e*e
      x(1,3)=becd/den
      x(2,3)=bcae/den
      x(3,3)=adbb/den
      x(1,2)=cebf/den
      x(2,2)=afcc/den
      x(1,1)=dfee/den
      x(2,1)=x(1,2)
      x(3,1)=x(1,3)
      x(3,2)=x(2,3)
      x(4,4)=1./x(4,4)
      x(5,5)=1./x(5,5)
      x(6,6)=1./x(6,6)
      return
      end
      subroutine shl34s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c           hill's normal anisotropic plasticity
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
     4 d7(128),fac1(128),fac2(128),g(128),
     5 fac1qh(128),fac2qh(128),wq1(128),wq2(128),wq3(128),
     6 wq4(128),blk(128),blkg(128),qh(128),ym(128)
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
      dimension  cm(*)
      dimension xl(128),phib2(128),xlnew(128),idone(128),
     1 dep(128),xk2(128),xlold(128),phatold(128),phat(128)
      dimension el1(128),el2(128),el3(128),error1(128)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      r=cm(mx+21)
      qs=cm(mx+11)
      pr=cm(mx+6)
      ss=cm(mx+2)
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      sndspd=cm(mx+1)/(1.-pr**2)
      c=r/(1.+r)
      d=2.*(1.+2.*r)/(1.+r)
      d11=ymod/(1.-pr**2)
      d12=pr*d11
      d33=gmod
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*cm(mx+16)
      g(i)   =fac4*ym(i)
      fac1(i)=1.5*g(i)
      blk(i) =fac5*ym(i)
      wq1(i) =fac3*ym(i)
      wq2(i) =.5*g(i)
      wq3(i) =1./(wq1(i)+2.0*wq2(i))
      wq4(i) =capa*wq2(i)
      eyld(i)=qs
      etanmd(i)=qh(i)
    5 continue
      if (ibelyt.eq.0) then
      do 25 i=lft,llt
      sign0(i)=sign3(i)+ym(i)*d3(i)
      sign3(i)=sign0(i)
   25 continue
      endif
      scl=0.
      do 30 i=lft,llt
      cb(i)  =ss
      d1d2(i)=d1(i)+d2(i)
      d3(i)=-(sig3(i)+wq1(i)*d1d2(i))*wq3(i)
      da1(i)=sig1(i)+d11*d1(i)+d12*d2(i)
      da2(i)=sig2(i)+d12*d1(i)+d11*d2(i)
      da4(i)=sig4(i)+wq2(i)*d4(i)
      da5(i)=sig5(i)+wq4(i)*d5(i)
      da6(i)=sig6(i)+wq4(i)*d6(i)
      ak(i)  =qs+qh(i)*ep(i)
      aks(i) =ak(i)*ak(i)
   30 continue
      do 34 i=lft,llt
      aj2(i)=da1(i)**2 + da2(i)**2-2.*r/(r+1.)*da1(i)*da2(i)
     1      + 2.*(2.*r+1.)/(r+1.)*da4(i)**2
      ak2(i)=aj2(i)-aks(i)
      scle(i)=.50+sign(.5,ak2(i))
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
      xleft=0.0
      right=2.0
      tol=1.0e-6
      do 60 i=lft,llt
      xlold(i)=xleft
      xl(i)=right
   60 continue
      call hilevl(xlold,ep,da1,da2,da4,pr,c,d,phatold,dep,ymod,
     1      qs,qh,el1,el2,el3)
      iter=1
  200 continue
      call hilevl(xl,ep,da1,da2,da4,pr,c,d,phat,dep,
     1     ymod,qs,qh,el1,el2,el3)
      do 70 i=lft,llt
      denom = phat(i)-phatold(i)
      if( scle(i) .ne. 0.0 ) then
      if( denom .eq. 0.0) stop 'denom 0 in hill'
      xlnew(i)=(xlold(i)*phat(i)-xl(i)*phatold(i))/denom
      endif
   70 continue
      do 72 i=lft,llt
      error1(i)=abs(xlnew(i)-xl(i))/(abs(xl(i))+1.e-15)
      if( scle(i) .eq. 0.) then
      error1(i)=0.0
      endif
   72 continue
c
      do 73 i=lft,llt
      idone(i)=0
      if( error1(i) .gt. tol) then
      xlold(i)=xl(i)
      phatold(i)=phat(i)
      xl(i)=xlnew(i)
      idone(i)=1
      endif
   73 continue
      ifin=0
      do 80 i=lft,llt
      ifin=ifin+idone(i)
   80 continue
      if(ifin.gt.0) then
      iter=iter+1
      go to 200
      endif
      do 90 i=lft,llt
      ebar(i)=qh(i)*scle(i)+ym(i)*(1.-scle(i))
      ep(i)=ep(i)+dep(i)
      sig1(i)=el1(i)*da1(i)+el2(i)*da2(i)
      sig2(i)=el2(i)*da1(i)+el1(i)*da2(i)
      sig3(i)=0.
      sig4(i)=el3(i)*da4(i)
      sig5(i)=da5(i)
      sig6(i)=da6(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   90 continue
      do 105 i=lft,llt
      d3(i)=((1.-pr**2)/ym(i)*xl(i)*(c-1.)-pr/ym(i))*(sig1(i)+sig2(i))
  105 continue
   75 return
      end
      subroutine hilevl(xlam,ep,da1,da2,da4,pr,c,d,phihat,dep,
     1 ymod,qs,qh,el1,el2,el3)
c     implicit double precision (a-h,o-z)                                    dp
      dimension dep(*),xlam(*),phihat(*),ep(*),da1(*),
     1 da2(*),da4(*),el1(*),el2(*),el3(*)
      dimension f1(128),f2(128),f3(128),f4(128),
     1  f5(128),f6(128),e1(128),e2(128),e3(128),xk2(128),phib2(128),
     2  p(128)
      common/aux36/lft,llt
      do 65 i=lft,llt
      p(i)=(1.+xlam(i)*(1.-pr*c))**2 - xlam(i)**2*(pr-c)**2
      el1(i)=1./p(i)*(1.+xlam(i)*(1.-pr*c))
      el2(i)=-1./p(i)*xlam(i)*(pr-c)
      el3(i)=2./(2.+xlam(i)*d*(1.-pr))
      f1(i)=el1(i)**2 - 2.*c*el1(i)*el2(i) + el2(i)**2
      f2(i)=-c*el1(i)**2 + 2.*el1(i)*el2(i) - c*el2(i)**2
      f3(i)=d*el3(i)**2
      e1(i)=4./3.*((1.-.5*c)-c*(.5-c))
      e2(i)=4./3.*((.5-c) - c*(1.-.5*c))
      e3(i)=1./3.*d**2
      f4(i)=el1(i)*(e1(i)*el1(i)+e2(i)*el2(i))
     1     +el2(i)*(e1(i)*el2(i)+e2(i)*el1(i))
      f5(i)=el1(i)*(e1(i)*el2(i)+e2(i)*el1(i))
     1     +el2(i)*(e1(i)*el1(i)+e2(i)*el2(i))
      f6(i)= e3(i)*el3(i)**2
      phib2(i)=f1(i)*(da1(i)**2+da2(i)**2) + 2.*f2(i)*da1(i)*da2(i)
     1         + f3(i)*da4(i)**2
   65 continue
      do 70 i=lft,llt
      xtemp=f4(i)*(da1(i)**2+da2(i)**2)
     1     + 2.*f5(i)*da1(i)*da2(i)+f6(i)*da4(i)**2
      if(xtemp.eq.0.) stop 'xtemp zero'
      dep(i)=xlam(i)*(1.-pr**2)/ymod*sqrt(xtemp)
      xk2(i)=(qs+qh*(ep(i)+dep(i)))**2
      phihat(i)=phib2(i)-xk2(i)
   70 continue
c
      return
      end
      subroutine shl35s (cm,capa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     fld+p model
c     hard-wired version, no rate-dep 032091
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
     2 sig5(128),sig6(128),  ep(128),
     * epij1(128),epij2(128),epij4(128),epij7(128),efail(128),
     3                                qsa(128),aux(128,43),
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
      common/failu/sieu(128),failu(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/-.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qb=cm(mx+26)
c**rwl
c-rgw
      time=-99.0
      qb=1.
      qs=cm(mx+17)
      pr=cm(mx+9)
      ss=cm(mx+8)
      qa=1.-qb
      fac3=pr/((1.0+pr)*(1.0-2.0*pr))
      fac4=1./(1.+pr)
      fac5=-1./(1.-2.*pr)
      ymod=cm(mx+1)
      gmod=.5*ymod/(1+pr)
      sndspd=cm(mx+1)/(1.-pr**2)
c**rwl 032091 - approx vals for hy80-100, also from navair vals
      fefmin=0.5
      fefmax=2.0
c
      efuc=cm(mx+13)
      efut=cm(mx+10)
      efpt=cm(mx+11)
      efbt=cm(mx+12)
      dfdr=0.
      fflg=1.
      dedpx=-efuc-2.*efbt
      if(dedpx.lt.0.0) dedpx=0.
      pi=3.1415926
      ifail=1
      scldev=cm(mx+18)
c
      do 5 i=lft,llt
      ym(i)  =tsarry(i)*cm(mx+1)
      qh(i)  =tsarry(i)*cm(mx+16)
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
c**rwl
      qsa(i)=qs
      epij1(i)=epij1(i)+tsarry(i)*d1(i)
      epij2(i)=epij2(i)+tsarry(i)*d2(i)
      epij4(i)=epij4(i)+tsarry(i)*d4(i)
    5 continue
      do 6 i=lft,llt
      if(ep(i).lt.-.001) then
       ep(i)=-ep(i)
       efail(i)=1.
      endif
      if(efail(i).lt.0.5) go to 6
      qh(i)=qh(i)*scldev
      qsa(i)=qsa(i)*scldev
      qbqh(i)=qbqh(i)*scldev
      eyld(i)=eyld(i)*scldev
      etanmd(i)=qh(i)
    6 continue
c
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
      t4(i)  =da4(i)
      t5(i)  =da5(i)
      t6(i)  =da6(i)
      t456(i)=3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak(i)  =qsa(i)+qbqh(i)*ep(i)
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
c**1st guess
      d3(i)=scle(i)*d7(i) + (1. - scle(i))*epsnew(i)
c
      p(i)=blkg(i)*(d1d2(i)+d3(i))
      da1(i)=sg1a(i)+p(i)
      da2(i)=sg2a(i)+p(i)
      da3(i)=sig3(i)+p(i)+g(i)*d3(i)
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
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
c**2nd guess
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
c**3rd guess
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
      ep(i)  =  ep(i)+depi(i)
      einc(i)=d1(i)*sig1(i)+d2(i)*sig2(i)+d4(i)*sig4(i)+d5(i)*sig5(i)
     1       +d6(i)*sig6(i)+einc(i)
   70 continue
   75 continue
c**rwl 032091 start fldp check
      do 101 i=lft,llt
      et1=epij1(i)
      et2=epij2(i)
      et4=epij4(i)
      exey2=(et1+et2)/2.
      eeg2=sqrt((et1-et2)**2+et4**2)/2.
      ep1=exey2+eeg2
      ep2=exey2-eeg2
      emaj=ep1
      emin=ep2
      if(emaj.lt.ep2) then
       emaj=ep2
       emin=ep1
      endif
      if(emaj.lt.0.001) go to 101
      slope=emin/emaj
      if(slope.gt.1.0) slope=1.0
      if(slope.lt.-0.5) slope=-0.5
      if(slope.ge.0.0) then
      slopd=efbt/(efbt-efpt)
       if(slopd.lt.1.2) slopd=1.2
      else
       slopd=efut/(efpt-efut)/2.
       if(slopd.gt.-.6) slopd=-.6
      endif
      elim=slopd*efpt/(slopd-slope)
      fef=1.
c** red strain corr
c** p/x corr
      epij7(i)=0.9*epij7(i)+0.1*p(i)
      px=epij7(i)/(qs+cm(mx+26)*ep(i))
c
      if(slope.ge.0.0) then
       pxd=-0.500-0.167*(elim-efpt)/(efbt-efpt)
      else
       pxd=-0.500+0.167*(elim-efpt)/(efut-efpt)
      endif
      elimp=elim+dedpx*(px-pxd)
      fef=fef*elimp/elim
      if(fef.lt.fefmin) fef=fefmin
      if(fef.gt.fefmax) fef=fefmax
      elimf=elim*fef
      if(emaj.gt.elimf) go to 121
      go to 101
  121 if(efail(i).lt.0.5) write(13,601) time,i,emaj,emin,ep(i),px
      if(efail(i).lt.0.5) write( *,601) time,i,emaj,emin,ep(i),px
  601 format(' t=',f10.2,' i=',i3,' emaj,emin=',2f7.4,
     *' ee=',f7.4,' p/x=',f7.3)
      efail(i)=1.
  101 continue
c-rgw
      do 102 i=lft,llt
      if(efail(i).gt.0.5) ep(i)=-ep(i)
c     if(efail(i).gt.0.5) failu(i)=0.
c-rgw
  102 continue
      return
      end
      subroutine prtdat (mpri,nfegp,nsubgv,u,b,x,mtnum,cm,accls)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
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
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/thfl/madd,ifth,maddbg
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/shlopt/istrn,istupd,ibelyt,miter
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/   /a(1)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/drelax/ladd,idrint
      dimension nfegp(*),nsubgv(*),u(*),b(*),x(*),mtnum(*),cm(*),
     1 accls(*)
c
      if(mpri.gt.0.and.mkthf.ne.0) go to 30
      if(mpri.lt.0) go to 10
c
c     print displacements, velocities, and accelerations
c
      call prtdv(a(lc11),a(lc9),a(lc10),numnp,numelb+numels,a(lc13),
     1 a(ndthl))
c
      go to 20
c
   10 icount=7+9*numnp+7*(numelh+numelt)+(33+12*min0(istrn,1))*numels
     1      +numrw+6*nmmat
      ifctor=iadd/maxsiz
      if (iadd+icount.gt.(ifctor+1)*maxsiz) then
      if (ifctor*maxsiz.ne.iadd) iadd=(ifctor+1)*maxsiz
      endif
c
      if (idrint.eq.0) then
      call outdv (a(n25),a(lc11),a(lc9),accls,neq,nmmat,numrw,iadd,iob5)
      else
      call outdv (a(n25),a(lc11),a(lc9),accls,neq,nmmat,numrw,ladd,iob6)
      endif
c
      if (ffifp) then
      icount=7+6*numnp+16*numsls
      ifctor=nadd/maxsiz
      if (nadd+icount.gt.(ifctor+1)*maxsiz) nadd=(ifctor+1)*maxsiz
      call outdv (a(n25),a(lc11),a(lc9),accls,neq,0,0,nadd,iob9)
      endif
c
c     print element stress for hexahedrons
c
   20 if (numelh+numelt.eq.0) go to 30
c
      if (numelh.ne.0)
     1call prttr (a(lc1h),a(lc15),a(n1),a(n4a),numelh,mpri,
     2 nfegp,nsubgv,u,b,x,mtnum,cm,a(nsthl),a(nh13),a(nh14))
      if (numelt.ne.0)
     1call tshprt(mpri)
c
c     print stress resultants for beam elements
c
   30 if (numelb.eq.0) go to 90
c
      call prtbem (a(nb04),a(lc1b),a(n1),a(nb06),a(n4f),numelb,mpri,
     1 a(n1+nmmat),a(nstbl),a(nb13),a(nb14))
c
c
c     print stress resultants for integrated shell elements
c
   90 if (numels.eq.0) go to 130
c
      call shprt (mpri)
c
  130 continue
      phony=-999999.                                                    vaxcs
      if(mpri.lt.0) then                                                vaxcs
       if (idrint.eq.0) then                                            vaxcs
            call wrabsg(iob5,phony,1,iadd,1)                            vaxcs
            call riosta (iob5)                                          vaxcs
       else                                                             vaxcs
            call wrabsg(iob6,phony,1,ladd,1)                            vaxcs
            call riosta (iob6)                                          vaxcs
       endif                                                            vaxcs
       endif                                                            vaxcs
       if(mkthf.ne.0) then                                              vaxcs
            call wrabsg(iob4,phony,1,madd,1)                            vaxcs
            call riosta (iob4)                                          vaxcs
      endif                                                             vaxcs
      return
c
      end
      subroutine prtdv(u,v,a,numnp,imass,x0,ndsout)
c     implicit double precision (a-h,o-z)                                    dp
c
c     write high speed printout
c
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk28/summss,xke,xpe,tt
c
      dimension u(3,1),v(3,1),a(3,1),x0(3,1),d(9),ndsout(2,*)
c
      if (ndth.eq.0) return
      ntimes=1
      if(imass.ne.0) ntimes=2
      do 150 l=1,ntimes
      ic=0
      do 150 j=1,ndth
      node1=ndsout(1,j)
      if (node1.eq.0) go to 150
      node2=ndsout(2,j)
      do 130 k=node1,node2
      kk=k+(l-1)*numnp
      if (l.eq.1) then
      d(1)=u(1,kk)-x0(1,kk)
      d(2)=u(2,kk)-x0(2,kk)
      d(3)=u(3,kk)-x0(3,kk)
      else
      d(1)=x0(1,kk)
      d(2)=x0(2,kk)
      d(3)=x0(3,kk)
      endif
      d(4)=v(1,kk)
      d(5)=v(2,kk)
      d(6)=v(3,kk)
      d(7)=a(1,kk)
      d(8)=a(2,kk)
      d(9)=a(3,kk)
      if(ic.gt.0) go to 125
      ic=50
      call header
      write(13,190) ncycle,tt
      if(l.eq.1) write(13,200)
      if(l.eq.2) write(13,220)
  125 ic=ic-1
      write(13,210) k,(d(i),i=1,9)
  130 continue
c
  150 continue
  180 return
c
  190 format(///' n o d a l   p r i n t   o u t   f o r   t i m e   '
     &,'s t e p ',i5,31x,' ( at time ',1pe10.4,' )')
  200 format(/' nodal point x-disp    y-disp     z-disp     ',
     &'x-vel      y-vel      z-vel      x-accl     y-accl     z-accl')
  210 format(i9,1x,9e11.3)
  220 format(/' nodal point x-rot     y-rot      z-rot      ',
     &'x-rot vel  y-rot vel  z-rot vel  x-rot acc  y-rot acc  z-rot acc'
     &)
c
      end
      subroutine outdv(tim,x,v,a,neq,nmmat,numrw,iadd,iobf)
c     implicit double precision (a-h,o-z)                                    dp
c
c     write displacements and velocities into plot file
c
      dimension tim(*),x(*),v(*),a(*)
c
      call wrabsg (iobf,tim,7+6*nmmat+numrw,iadd,1)
      call riosta (iobf)
      iadd=iadd+7+6*nmmat+numrw
      call wrabsg (iobf,x,neq,iadd,1)
      call riosta (iobf)
      iadd=iadd+neq
      call wrabsg (iobf,v,neq,iadd,1)
      call riosta (iobf)
      iadd=iadd+neq
      if (nmmat.gt.0) then
      call wrabsg (iobf,a,neq,iadd,1)
      call riosta (iobf)
      iadd=iadd+neq
      endif
      return
      end
      subroutine prttr(ixp,auxvec,matype,ieost,numelh,mpri,
     1     nfegp,nsubgv,u,b,x,mtnum,cm,nsout1,nhxpnt,lochvh)
c     implicit double precision (a-h,o-z)                                    dp
c
      character*8 astate                                                vax75
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk28/summss,xke,xpe,tt
      common/aux14/sig(7,1298)
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/drelax/ladd,idrint
c
      dimension ixp(9,*),matype(*),ieost(*),sgm(7),astate(5),auxvec(*), vax75
c     dimension ixp(3,*),matype(*),ieost(*),sgm(7),astate(5),auxvec(*), cray1
     1    nsout1(*),nfegp(*),nsubgv(*),u(*),b(*),x(*),mtnum(*),cm(*),
     2    nhxpnt(*),lochvh(*)
c
      data astate/'elastic','plastic','failure','       ','temp'/
c
      if (mpri.lt.0) go to 90
c
      if(nsth.eq.0) return
      nprint=0
      do 40 n=1,nsth
      i=nsout1(2*n-1)
      j=nsout1(2*n)
      if (min0(i,j).lt.1) go to 40
      do 30 k=i,j
      l=nhxpnt(k)
      lav=lochvh(l)
c     mtype=and(ixp(1,l),3777777b)                                      cray1
      mtype=ixp(1,l)                                                    vax75
      nes  =ieost(mtype)
      if (mtype.eq.0) go to 30
      mtyp =matype(mtype)
      if (mtyp.eq.20) go to 30
      nmecon=7+nconst(mtyp)
      if (nes.ne.0) nmecon=nmecon+ncneos(nes)
      call blkcpy (auxvec(lav),sgm,7)
      if(nprint.gt.0) go to 20
      nprint=40
      call header
      write(13,110) ncycle,tt
      write(13,120)
   20 nprint=nprint-1
      effs2=0.50*((sgm(1)-sgm(2))**2+(sgm(2)-sgm(3))**2+
     1      (sgm(3)-sgm(1))**2)+3.*(sgm(4)**2+sgm(5)**2+
     2      sgm(6)**2)
      effs2=sqrt(abs(effs2))
      nstate=4
      if ((mtyp.eq.3.or.mtyp.eq.5.or.mtyp.eq.10.or.mtyp.eq.11
     1 .or.mtyp.eq.16).and.(sgm(7).eq.0.0)) nstate=1
      if ((mtyp.eq.3.or.mtyp.eq.5.or.mtyp.eq.10.or.mtyp.eq.11
     1 .or.mtyp.eq.16).and.(sgm(7).ne.0.0)) nstate=2
      if (mtyp.eq.1.or.mtyp.eq.2.or.mtyp.eq.6 .or.mtyp.eq.7 )
     1 nstate=1
      write(13,130) k,mtype
      lprt=1
      write(13,140) lprt,astate(nstate),(sgm(jj),jj=1,6),effs2,sgm(7)
c
   30 continue
   40 continue
c
      return

   90 nelg=(numelh-1)/1298+1
      lav =1
      nel =0
      do 100 i=1,nelg
      nmel=1298
      if (i*1298.gt.numelh) nmel=numelh-1298*(i-1)
      do 92 j=1,nmel
      sig(1,j)=0.
      sig(2,j)=0.
      sig(3,j)=0.
      sig(4,j)=0.
      sig(5,j)=0.
      sig(6,j)=0.
   92 sig(7,j)=0.
      do 96 j=1,nmel
      nel=nel+1
      n=nhxpnt(nel)
c     mtype=and(ixp(1,n),3777777b)                                      cray1
      mtype=ixp(1,n)                                                    vax75
      nes  =ieost(mtype)
      if (mtype.eq.0) go to 96
      mtyp =matype(mtype)
      if (mtyp.eq.20) go to 96
      nmecon=7+nconst(mtyp)
      if (nes.ne.0) nmecon=nmecon+ncneos(nes)
      lav=lochvh(n)
      if (mtyp.ne.2) then
      call blkcpy(auxvec(lav),sig(1,j),7)
      else
      call blkcpy (auxvec(lav),sig(1,j),6)
      sig(7,j)=0.0
      endif
   96 continue
      if (idrint.eq.0) then
      call wrabsg (iob5,sig,7*nmel,iadd,1)
      call riosta (iob5)
      iadd=iadd+7*nmel
      else
      call wrabsg (iob6,sig,7*nmel,ladd,1)
      call riosta (iob6)
      ladd=ladd+7*nmel
      endif
  100 continue
      return
  110 format(///' e l e m e n t   s t r e s s   c a l c u l a t i o n s'
     &,'   f o r   t i m e   s t e p ',i5,7x,' ( at time ',1pe10.4,' )')
  120 format(/' element  stress      sig-xx     sig-yy     sig',
     1'-zz     sig-xy     sig-yz     sig-zx ',26x,'yield' /
     2' num/ipt   state',80x,'effsg         function')
  130 format(i5,'-',i2)
  140 format(5x,i2,2x,a7,1x,6e11.3,11x,e11.3,4x,e11.3)
c
      end
      subroutine prtbem(strr,ixp,matype,auxvec,csprop,numelb,mpri,
     1 ibemfm,nsout2,nbmpnt,lochvb)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk28/summss,xke,xpe,tt
      common/thfl/madd,icount,maddbg
      common/aux14/data(2241),ntotal
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/drelax/ladd,idrint
c
      dimension ixp(4,*),strr(6,*),matype(*),auxvec(*),                 vax75
c     dimension ixp(2,*),strr(6,*),matype(*),auxvec(*),                 cray1
     1 csprop(24,*),ibemfm(*),nsout2(*),ni2a(5),nbmpnt(*),lochvb(*)
      logical inlist
c
      data ni2a/1,4,9,9,16/
c
      if(mpri.gt.0 .and. nstb.eq.0) return
      locstr=1
      if (mpri.gt.0) locstr=2
      ntotal=2100
      nprint=0
      data(1)=tt
      do 80 k=1,numelb
      n=nbmpnt(k)
      nel   =n
c     mtype =and(ixp(1,n),3777777b)                                     cray1
      mtype =ixp(1,n)                                                   vax75
      mt    =matype(mtype)
      ni    =csprop(2,mtype)
      iop   =ibemfm(mtype)
      if (csprop(4,mtype).ge.0.0) then
      ni2   =ni2a(ni)
      else
      ni2=ni
      endif
      nmtcon=ni2*(7+nconst(mt))
      if(mpri.le.0) go to 50
      inlist=.false.
      do 10 j=1,nstb
      inlist=inlist .or.
     1       (nsout2(2*j-1).le.nel .and. nsout2(2*j).ge.nel)
   10 continue
      if(.not.inlist) go to 70
      if(mkthf.ne.0) go to 50
      if(nprint.gt.0) go to 20
c
      nprint=40
      call header
      write(13,110) ncycle,tt
c
   20 write(13,130) k,mtype
      write(13,120)
      write(13,140) (strr(j,n),j=1,6)
c
c     write beam stress for integrated beam element (hughes-liu)
c
      if (iop.eq.1) then
      write(13,150)
      mi2s=nconst(mt)+7
      locs=lochvb(n)
      do 40 l=1,ni2
      sig11=auxvec(locs)
      sig12=auxvec(locs+3)
      sig13=auxvec(locs+5)
      hisvr=auxvec(locs+6)
      locs =locs+mi2s
      write(13,160) l,sig11,sig12,sig13,hisvr
   40 continue
      endif
c
      go to 70
c
   50 continue
      do 60 l=1,6
      data(locstr+l-1)=strr(l,n)
   60 continue
      locstr=locstr+6
      if(locstr+6.le.ntotal) go to 70
      locstr=locstr-1
      if (mpri.le.0) then
      if (idrint.eq.0) then
      call wrabsg(iob5,data,locstr,iadd,1)
      call riosta (iob5)
      iadd=iadd+locstr
      else
      call wrabsg(iob6,data,locstr,ladd,1)
      call riosta (iob6)
      ladd=ladd+locstr
      endif
      else
      call wrabsg(iob4,data,locstr,madd,1)
      call riosta (iob4)
      madd=madd+locstr
      icount=icount+locstr
      endif
      locstr=1
   70 continue
   80 continue
      if(locstr.eq.1) return
      locstr=locstr-1
      if (mpri.le.0) then
      if (idrint.eq.0) then
      call wrabsg(iob5,data,locstr,iadd,1)
      call riosta (iob5)
      iadd=iadd+locstr
      else
      call wrabsg(iob6,data,locstr,ladd,1)
      call riosta (iob6)
      ladd=ladd+locstr
      endif
      else
      call wrabsg(iob4,data,locstr,madd,1)
      call riosta (iob4)
      madd=madd+locstr
      icount=icount+locstr
      endif
c
      return
  110 format(////' r e s u l t a n t s   a n d   s t r e s s e s   ',
     1'f o r   t i m e   s t e p ',i5,7x,' ( at time ',1pe10.4,' )')
  120 format(//'resultants    axial      shear-s    she',
     1'ar-t    moment-s   moment-t   torsion')
  130 format(////'beam/truss #  =',i5,'      material #  =',i3)
  140 format(11x,6e11.3)
  150 format(//
     1'integration point stresses'
     2/21x,'sigma 11      sigma 12      sigma 31      plastic  eps')
  160 format(i5,11x,4e14.4)
c
      end
      subroutine shprt(mpri)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
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
      common/shlopt/istrn,istupd,ibelyt,miter
      common/aux14/
     1 stress(45,128)
      common /   / a(1)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      lns=33+12*min0(istrn,1)
      call sprt (a(n1),a(n2),a(n3),a(n4f),a(lc10),a(lc9),
     1 a(lc11),a(n4a),a(n4b),a(ns08),a(ns09),a(ns10),a(lc1s),
     2 a(ns05),a(ns06),a(ns03),a(ns01),stress,lns,mpri,a(ns07),a(n4h),
     3 mpusr,a(n1+nmmat),a(nstsl),a(ns13),a(ns14))
c
      return
      end
      subroutine sprt (mtype,ro,cm,csprop,a,v,x,ieost,eosp,nsubgv,
     1 mtnum,nfegp,ixp,fibl,auxvec,yhat,strain,stress,lns,mpri,xies,
     2 rule,mpusr,ishlfm,nsout3,nshpnt,lochvs)
c     implicit double precision (a-h,o-z)                                    dp
c
      character*8 astate                                                vax75
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,ttime(2,8)
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk28/summss,xke,xpe,time
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/nwixa/nwcon
      common/thfl/madd,ifth,maddbg
      common/aux36/lft,llt
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/drelax/ladd,idrint
c
      dimension mtype(*),ro(*),cm(48,*),csprop(24,*),a(*),v(*),
     1 x(*),ieost(*),eosp(48,*),nsubgv(*),mtnum(*),nfegp(*),
     2 ixp(5,*),auxvec(*),astate(5),rslt(9),fibl(9,*),yhat(12,*),       vax75
c    2 ixp(2,*),auxvec(*),astate(5),rslt(9),fibl(9,*),yhat(12,*),       cray1
     3 strain(12,*),stress(lns,*), rule(mpusr,3,*),ishlfm(*),nsout3(*),
     4 ix(20),nshpnt(*),lochvs(*),xies(*)
      logical inlist
      data astate/'elastic','plastic','failure','       ','temp'/
      nelg=numels/128
      lav=1
      lcz=0
      if (128*nelg.lt.numels) nelg=nelg+1
      if (mpri.lt.0) go to 180
      if(nsts.eq.0) return
      if (mkthf.ne.0)go to 410
      call header
      write(13,510) ncycle,time
      write(13,520)
      do 160 nn=1,nelg
      noco=nn
      nnm1=128*(nn-1)
      nmel=128
      if (nn.eq.nelg) nmel=numels-128*(nelg-1)
      nsubg=nsubgv(nn)
      do 140 n=1,nsubg
      lcn=lcz+n
      mxe=mtnum(lcn)
      nip=csprop(2,mxe)
      if (mxe) 90,140,90
   90 lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      mte=mtype(mxe)
      nes=ieost(mxe)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      do 120 i=lft,llt
      nelm=nnm1+i
      nel =nshpnt(nelm)
      lav =lochvs(nel)
      inlist=.false.
      do 100 j=1,nsts
      inlist=inlist .or.
     1       (nsout3(2*j-1).le.nel .and. nsout3(2*j).ge.nel)
  100 continue
      if(inlist) then
      write(13,530) nelm,mxe
      do 110 ipt=1,nip
      if(mte.eq.20) then
           stress(1,ipt)=0.
           stress(2,ipt)=0.
           stress(3,ipt)=0.
           stress(4,ipt)=0.
           stress(5,ipt)=0.
           stress(6,ipt)=0.
           stress(7,ipt)=0.
      else
           call blkcpy (auxvec(lav),stress(1,ipt),7)
      endif
      nstate=1
      if ((mte.eq.3.or.mte.eq.5.or.mte.eq.10.or.mte.eq.11)
     1 .and.(stress(7,ipt).ne.0.0)) nstate=2
      if (mte.eq.22.and.stress(7,ipt).eq.0.0) nstate=3
      write(13,540) ipt,astate(nstate),(stress(j,ipt),j=1,7)
      lav=lav+nmtcon
  110 continue
      endif
  120 continue
  140 continue
      l11=l11+nwcon*nmel
      lcz=lcz+nsubg+1
  160 continue
      return
  180 i=0
      do 400 n=1,numels
      i=i+1
      do 190 kk=1,lns
  190 stress(kk,i)=0.0
      nnel=nshpnt(n)
      lav=lochvs(nnel)
      call unpk(mx,ix,ixp(1,nnel),2)
c     if (n.eq.numels) pause
      if (mx.eq.0) go to 390
      nip=csprop(2,mx)
      irl=nint(csprop(4,mx))
      nrl=iabs(irl)
      iop=ishlfm(mx)
      if (iop.eq.3) nip=nip/3
      if (nip.gt.5.and.irl.eq.0) irl=1
      mte=mtype(mx)
      nes=ieost(mx)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      stress(lns,i)=xies(nnel)
      call iedens(fibl(1,nnel),stress(lns,i),ixp(1,nnel),x)
      if (iop.ne.3) then
      if (nip.eq.1) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav),stress(15,i),7)
      if (mte.eq.28) then
      call azero (stress(1,i),6)
      call azero (stress(8,i),6)
      call azero (stress(15,i),6)
      endif
      endif
      if (nip.eq.3) then
      if (irl.eq.0) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(8,i),7)
      else
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(1,i),7)
      endif
      call blkcpy(auxvec(lav+2*nmtcon),stress(15,i),7)
      endif
      if (nip.eq.5) then
      if (irl.eq.0) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(8,i),7)
      else
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+2*nmtcon),stress(1,i),7)
      endif
      call blkcpy(auxvec(lav+4*nmtcon),stress(15,i),7)
      endif
      if ((nip/2)*2.eq.nip) then
      np2=nip/2
      np1=np2-1
      nmtcn1=np1*nmtcon
      nmtcn2=np2*nmtcon
      do 210 j=1,7
  210 stress(j,i)=.5*(auxvec(lav+nmtcn1+j-1)+auxvec(lav+nmtcn2+j-1))
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+(nip-1)*nmtcon),stress(15,i),7)
      endif
      if ((nip/2)*nip.ne.nip.and.nip.gt.5) then
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+(nip/2)*nmtcon),stress(1,i),7)
      call blkcpy(auxvec(lav+(nip-1)*nmtcon),stress(15,i),7)
      endif
      else
      if (nip.eq.1) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav),stress(15,i),nip)
      if (mte.eq.28) then
      call azero (stress(1,i),6)
      call azero (stress(8,i),6)
      call azero (stress(15,i),6)
      endif
      endif
      if (nip.eq.3) then
      if (irl.eq.0) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(8,i),nip)
      else
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(1,i),nip)
      endif
      call avgsig(nmtcon,auxvec(lav+2*nmtcon),stress(15,i),nip)
      endif
      if (nip.eq.5) then
      if (irl.eq.0) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(8,i),nip)
      else
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+2*nmtcon),stress(1,i),nip)
      endif
      call avgsig(nmtcon,auxvec(lav+4*nmtcon),stress(15,i),nip)
      endif
      if ((nip/2)*2.eq.nip) then
      np2=nip/2
      np1=np2-1
      nmtcn1=np1*nmtcon
      nmtcn2=np2*nmtcon
      do 215 j=1,7
  215 stress(j,i)=.5*(auxvec(lav+nmtcn1+j-1)+auxvec(lav+nmtcn2+j-1))
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip-1)*nmtcon),stress(15,i),nip)
      endif
      if ((nip/2)*nip.ne.nip.and.nip.gt.5) then
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip/2)*nmtcon),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip-1)*nmtcon),stress(15,i),nip)
      endif
      endif
      if (mte.ne.28) then
      call irslt(iop,auxvec(lav),fibl(1,nnel),nip,csprop(13,mx),
     1 rslt,ixp(1,nnel),x,yhat(1,nnel),nmtcon,irl,rule(1,1,nrl),mpusr)
      else
      rslt(1)=auxvec(lav+5)
      rslt(2)=auxvec(lav+7)
      rslt(3)=auxvec(lav+8)
      rslt(4)=auxvec(lav+3)
      rslt(5)=auxvec(lav+4)
      rslt(6)=auxvec(lav  )
      rslt(7)=auxvec(lav+1)
      rslt(8)=auxvec(lav+2)
      endif
      call blkcpy(rslt,stress(22,i),9)
      stress(31,i)=stress(14,i)
      stress(32,i)=stress(21,i)
      if (istrn.eq.1) then
      call blkcpy (strain(1,nnel),stress(33,i),12)
      endif
      if (mte.eq.20) then
      do 220 j=1,lns
  220 stress(j,i)=0.0
      endif
      if (mte.eq.22) then
      hstv1=0.0
      hstv2=0.0
      hstv3=0.0
      do 225 j=1,nip
      locip=6+(j-1)*nmtcon
      hstv1=hstv1+auxvec(lav+locip)
      hstv2=hstv2+auxvec(lav+locip+1)
      hstv3=hstv3+auxvec(lav+locip+2)
  225 continue
      stress(7 ,i)=hstv1/float(nip)
      stress(14,i)=hstv2/float(nip)
      stress(21,i)=hstv3/float(nip)
      endif
      if (iop.ne.1) then
      call gtglob(stress(1,i),ixp(1,nnel),x,7,3)
      if (istrn.eq.1) then
      call gtglob(stress(33,i),ixp(1,nnel),x,6,2)
      endif
      endif
      if (mte.ne.20) then
      lav=lav+nip*nmtcon
      if (iop.eq.3) lav=lav+2*nip*nmtcon
      endif
  390 if (i.eq.128) then
      if (idrint.eq.0) then
      call wrabsg (iob5,stress,lns*i,iadd,1)
      call riosta (iob5)
      iadd=iadd+lns*i
      else
      call wrabsg (iob6,stress,lns*i,ladd,1)
      call riosta (iob6)
      ladd=ladd+lns*i
      endif
      i=0
      endif
  400 continue
      if (i.ne.0) then
      if (idrint.eq.0) then
      call wrabsg (iob5,stress,lns*i,iadd,1)
      call riosta (iob5)
      iadd=iadd+lns*i
      else
      call wrabsg (iob6,stress,lns*i,ladd,1)
      call riosta (iob6)
      ladd=ladd+lns*i
      endif
      endif
      return
  410 i=0
      do 500 k=1,nsts
      nlowr=nsout3(2*k-1)
      nuppr=nsout3(2*k  )
      do 480 n=nlowr,nuppr
      i=i+1
      do 420 kk=1,lns
  420 stress(kk,i)=0.0
      nnel=nshpnt(n)
      lav=lochvs(nnel)
      call unpk(mx,ix,ixp(1,nnel),2)
      if (mx.eq.0) go to 470
      nip=csprop(2,mx)
      irl=nint(csprop(4,mx))
      nrl=iabs(irl)
      iop=ishlfm(mx)
      if (iop.eq.3) nip=nip/3
      if (nip.gt.5.and.irl.eq.0) irl=1
      mte=mtype(mx)
      nes=ieost(mx)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      stress(lns,i)=xies(nnel)
      call iedens(fibl(1,nnel),stress(lns,i),ixp(1,nnel),x)
      if (iop.ne.3) then
      if (nip.eq.1) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav),stress(15,i),7)
      if (mte.eq.28) then
      call azero (stress(1,i),6)
      call azero (stress(8,i),6)
      call azero (stress(15,i),6)
      endif
      endif
      if (nip.eq.3) then
      if (irl.eq.0) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(8,i),7)
      else
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(1,i),7)
      endif
      call blkcpy(auxvec(lav+2*nmtcon),stress(15,i),7)
      endif
      if (nip.eq.5) then
      if (irl.eq.0) then
      call blkcpy(auxvec(lav),stress(1,i),7)
      call blkcpy(auxvec(lav+nmtcon),stress(8,i),7)
      else
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+2*nmtcon),stress(1,i),7)
      endif
      call blkcpy(auxvec(lav+4*nmtcon),stress(15,i),7)
      endif
      if ((nip/2)*2.eq.nip) then
      np2=nip/2
      np1=np2-1
      nmtcn1=np1*nmtcon
      nmtcn2=np2*nmtcon
      do 430 j=1,7
  430 stress(j,i)=.5*(auxvec(lav+nmtcn1+j-1)+auxvec(lav+nmtcn2+j-1))
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+(nip-1)*nmtcon),stress(15,i),7)
      endif
      if ((nip/2)*nip.ne.nip.and.nip.gt.5) then
      call blkcpy(auxvec(lav),stress(8,i),7)
      call blkcpy(auxvec(lav+(nip/2)*nmtcon),stress(1,i),7)
      call blkcpy(auxvec(lav+(nip-1)*nmtcon),stress(15,i),7)
      endif
      else
      if (nip.eq.1) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav),stress(15,i),nip)
      if (mte.eq.28) then
      call azero (stress(1,i),6)
      call azero (stress(8,i),6)
      call azero (stress(15,i),6)
      endif
      endif
      if (nip.eq.3) then
      if (irl.eq.0) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(8,i),nip)
      else
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(1,i),nip)
      endif
      call avgsig(nmtcon,auxvec(lav+2*nmtcon),stress(15,i),nip)
      endif
      if (nip.eq.5) then
      if (irl.eq.0) then
      call avgsig(nmtcon,auxvec(lav),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+nmtcon),stress(8,i),nip)
      else
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+2*nmtcon),stress(1,i),nip)
      endif
      call avgsig(nmtcon,auxvec(lav+4*nmtcon),stress(15,i),nip)
      endif
      if ((nip/2)*2.eq.nip) then
      np2=nip/2
      np1=np2-1
      nmtcn1=np1*nmtcon
      nmtcn2=np2*nmtcon
      do 440 j=1,7
  440 stress(j,i)=.5*(auxvec(lav+nmtcn1+j-1)+auxvec(lav+nmtcn2+j-1))
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip-1)*nmtcon),stress(15,i),nip)
      endif
      if ((nip/2)*nip.ne.nip.and.nip.gt.5) then
      call avgsig(nmtcon,auxvec(lav),stress(8,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip/2)*nmtcon),stress(1,i),nip)
      call avgsig(nmtcon,auxvec(lav+(nip-1)*nmtcon),stress(15,i),nip)
      endif
      endif
      if (mte.ne.28) then
      call irslt(iop,auxvec(lav),fibl(1,nnel),nip,csprop(13,mx),
     1 rslt,ixp(1,nnel),x,yhat(1,nnel),nmtcon,irl,rule(1,1,nrl),mpusr)
      else
      rslt(1)=auxvec(lav+5)
      rslt(2)=auxvec(lav+7)
      rslt(3)=auxvec(lav+8)
      rslt(4)=auxvec(lav+3)
      rslt(5)=auxvec(lav+4)
      rslt(6)=auxvec(lav  )
      rslt(7)=auxvec(lav+1)
      rslt(8)=auxvec(lav+2)
      endif
      call blkcpy(rslt,stress(22,i),9)
      stress(31,i)=stress(14,i)
      stress(32,i)=stress(21,i)
      if (istrn.eq.1) then
      call blkcpy (strain(1,nnel),stress(33,i),12)
      endif
      if (mte.eq.20) then
      do 450 j=1,lns
  450 stress(j,i)=0.0
      endif
      if (mte.eq.22) then
      hstv1=0.0
      hstv2=0.0
      hstv3=0.0
      do 460 j=1,nip
      locip=6+(j-1)*nmtcon
      hstv1=hstv1+auxvec(lav+locip)
      hstv2=hstv2+auxvec(lav+locip+1)
      hstv3=hstv3+auxvec(lav+locip+2)
  460 continue
      stress(7 ,i)=hstv1/float(nip)
      stress(14,i)=hstv2/float(nip)
      stress(21,i)=hstv3/float(nip)
      endif
      if (iop.ne.1) then
      call gtglob(stress(1,i),ixp(1,nnel),x,7,3)
      if (istrn.eq.1) then
      call gtglob(stress(33,i),ixp(1,nnel),x,6,2)
      endif
      endif
      if (mte.ne.20) then
      lav=lav+nip*nmtcon
      if (iop.eq.3) lav=lav+2*nip*nmtcon
      endif
  470 if (i.eq.128) then
      call wrabsg (iob4,stress,lns*i,madd,1)
      call riosta (iob4)
      madd=madd+lns*i
      ifth=ifth+lns*i
      i=0
      endif
  480 continue
  500 continue
      if (i.ne.0) then
      call wrabsg (iob4,stress,lns*i,madd,1)
      call riosta (iob4)
      madd=madd+lns*i
      ifth=ifth+lns*i
      endif
      return
  510 format(///' e l e m e n t   s t r e s s   c a l c u l a t i o n s'
     &,'   f o r   t i m e   s t e p ',i5,7x,' ( at time ',1pe10.4,' )')
  520 format(/' element  stress      sig-xx     sig-yy     sig',
     1'-zz     sig-xy     sig-yz     sig-zx ',26x,'yield' /
     2' num/ipt   state',80x,'   function')
  530 format(i5,'-',i2)
  540 format(5x,i2,2x,a7,1x,6e11.3,11x,4x,e11.3)
      end
      subroutine avgsig(nmtcon,sig,stress,nip)
c     implicit double precision (a-h,o-z)                                    dp
      dimension sig(nmtcon,*),stress(*)
      nip1=nip+1
      nip2=nip+nip+1
      do 10 i=1,7
   10 stress(i)=(sig(i,1)+sig(i,nip1)+sig(i,nip2))/3.0
      return
      end
      subroutine irslt(iop,sig,fibl,nzgpt,zbar,rslt,ixp,x,yhat,nmtcon,
     1 irl,rule,mpusr)
c     implicit double precision (a-h,o-z)                                    dp
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      dimension sig(nmtcon,*),zbar(*),rslt(9),fibl(*),ixp(*),
     1 ix(4),x(3,*),yhat(*),rule(mpusr,*)
      call azero(rslt,9)
      ni=nzgpt
      do 10 j = 1,nzgpt
      if (irl.eq.0) then
      zetjni=zeta(j,ni)
      wgtjni =wgts(j,ni)
      elseif (irl.gt.0) then
      zetjni=(-1.0+(j-1)*2./(ni-1))
      wgtjni=2./(ni-1)
      if (j.eq.1.or.j.eq.ni) wgtjni=wgtjni/2.
      else
      zetjni=rule(j,1)
      wgtjni=rule(j,2)
      endif
      if (iop.eq.1) then
      shp1=.125*fibl(1)
      shp2=.125*fibl(2)
      shp3=.125*fibl(3)
      shp4=.125*fibl(4)
      zdz  =shp1+shp2+shp3+shp4
      zzdz =(shp1*(zetjni-zbar(1))+shp2*(zetjni-zbar(2))
     1      +shp3*(zetjni-zbar(3))+shp4*(zetjni-zbar(4)))*zdz
      if (j.eq.1) then
c     kka=ixp(1)                                                        cray1
c     kkb=ixp(2)                                                        cray1
c     matp=and(kka,3777777b)                                            cray1
c     ix(3) =and(kkb,3777777b)                                          cray1
c     kk1=shiftr(kka,21)                                                cray1
c     kk2=shiftr(kkb,21)                                                cray1
c     kka=shiftr(kk1,21)                                                cray1
c     kkb=shiftr(kk2,21)                                                cray1
c     ix(1)=and(kk1,3777777b)                                           cray1
c     ix(2)=and(kka,3777777b)                                           cray1
c     ix(4)=and(kk2,3777777b)                                           cray1
      matp=ixp(1)                                                       vax75
      ix(1)=ixp(2)                                                      vax75
      ix(2)=ixp(3)                                                      vax75
      ix(3)=ixp(4)                                                      vax75
      ix(4)=ixp(5)                                                      vax75
      cn1x1=x(1,ix(1))
      cn1x2=x(1,ix(2))
      cn1x3=x(1,ix(3))
      cn1x4=x(1,ix(4))
      cn1y1=x(2,ix(1))
      cn1y2=x(2,ix(2))
      cn1y3=x(2,ix(3))
      cn1y4=x(2,ix(4))
      cn1z1=x(3,ix(1))
      cn1z2=x(3,ix(2))
      cn1z3=x(3,ix(3))
      cn1z4=x(3,ix(4))
      x21=cn1x2-cn1x1
      y21=cn1y2-cn1y1
      z21=cn1z2-cn1z1
      x31=cn1x3-cn1x1
      y31=cn1y3-cn1y1
      z31=cn1z3-cn1z1
      x41=cn1x4-cn1x1
      y41=cn1y4-cn1y1
      z41=cn1z4-cn1z1
      x42=cn1x4-cn1x2
      y42=cn1y4-cn1y2
      z42=cn1z4-cn1z2
      c1=y31*z42-z31*y42
      c2=z31*x42-x31*z42
      c3=x31*y42-y31*x42
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s31=c1*xl
      s32=c2*xl
      s33=c3*xl
      xl=x21*s31+y21*s32+z21*s33
      c1=x21-s31*xl
      c2=y21-s32*xl
      c3=z21-s33*xl
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s11=c1*xl
      s12=c2*xl
      s13=c3*xl
      s21=s32*s13-s33*s12
      s22=s33*s11-s31*s13
      s23=s31*s12-s32*s11
      endif
      a11=sig(1,j)*s11+sig(4,j)*s12+sig(6,j)*s13
      a12=sig(1,j)*s21+sig(4,j)*s22+sig(6,j)*s23
      a13=sig(1,j)*s31+sig(4,j)*s32+sig(6,j)*s33
      a21=sig(4,j)*s11+sig(2,j)*s12+sig(5,j)*s13
      a22=sig(4,j)*s21+sig(2,j)*s22+sig(5,j)*s23
      a23=sig(4,j)*s31+sig(2,j)*s32+sig(5,j)*s33
      a31=sig(6,j)*s11+sig(5,j)*s12+sig(3,j)*s13
      a32=sig(6,j)*s21+sig(5,j)*s22+sig(3,j)*s23
      a33=sig(6,j)*s31+sig(5,j)*s32+sig(3,j)*s33
      sig1=s11*a11+s12*a21+s13*a31
      sig2=s21*a12+s22*a22+s23*a32
      sig4=s11*a12+s12*a22+s13*a32
      sig5=s21*a13+s22*a23+s23*a33
      sig6=s11*a13+s12*a23+s13*a33
      elseif (iop.eq.2.or.iop.eq.4.or.iop.eq.5.or.iop.eq.6) then
      sig1=sig(1,j)
      sig2=sig(2,j)
      sig4=sig(4,j)
      sig5=sig(5,j)
      sig6=sig(6,j)
      zdz =.50*fibl(1)
      zzdz=.50*zdz*fibl(1)*(zetjni-zbar(1))
      elseif (iop.eq.3) then
      sig1=(sig(1,j)+sig(1,j+ni)+sig(1,j+ni+ni))/3.0
      sig2=(sig(2,j)+sig(2,j+ni)+sig(2,j+ni+ni))/3.0
      sig4=(sig(4,j)+sig(4,j+ni)+sig(4,j+ni+ni))/3.0
      sig5=(sig(5,j)+sig(5,j+ni)+sig(5,j+ni+ni))/3.0
      sig6=(sig(6,j)+sig(6,j+ni)+sig(6,j+ni+ni))/3.0
      zdz =.50*fibl(1)
      zzdz=.50*zdz*fibl(1)*(zetjni-zbar(1))
      endif
      rslt(1)=rslt(1)-sig1*zzdz*wgtjni
      rslt(2)=rslt(2)-sig2*zzdz*wgtjni
      rslt(3)=rslt(3)-sig4*zzdz*wgtjni
      rslt(4)=rslt(4)+sig6*zdz*wgtjni
      rslt(5)=rslt(5)+sig5*zdz*wgtjni
      rslt(6)=rslt(6)+sig1*zdz*wgtjni
      rslt(7)=rslt(7)+sig2*zdz*wgtjni
      rslt(8)=rslt(8)+sig4*zdz*wgtjni
10    continue
      if (iop.eq.1) then
      rslt(9)=.25*(fibl(1)+fibl(2)+fibl(3)+fibl(4))
      else
      rslt(9)=fibl(1)
      endif
      return
      end
      subroutine iedens(thick,xie,ixp,x)
c     implicit double precision (a-h,o-z)                                    dp
      dimension ixp(*),ix(4),x(3,*)
c     kka=ixp(1)                                                        cray1
c     kkb=ixp(2)                                                        cray1
c     matp=and(kka,3777777b)                                            cray1
c     ix(3) =and(kkb,3777777b)                                          cray1
c     kk1=shiftr(kka,21)                                                cray1
c     kk2=shiftr(kkb,21)                                                cray1
c     kka=shiftr(kk1,21)                                                cray1
c     kkb=shiftr(kk2,21)                                                cray1
c     ix(1)=and(kk1,3777777b)                                           cray1
c     ix(2)=and(kka,3777777b)                                           cray1
c     ix(4)=and(kk2,3777777b)                                           cray1
      matp=ixp(1)                                                       vax75
      ix(1)=ixp(2)                                                      vax75
      ix(2)=ixp(3)                                                      vax75
      ix(3)=ixp(4)                                                      vax75
      ix(4)=ixp(5)                                                      vax75
      x1=x(1,ix(1))
      x2=x(1,ix(2))
      x3=x(1,ix(3))
      x4=x(1,ix(4))
      y1=x(2,ix(1))
      y2=x(2,ix(2))
      y3=x(2,ix(3))
      y4=x(2,ix(4))
      z1=x(3,ix(1))
      z2=x(3,ix(2))
      z3=x(3,ix(3))
      z4=x(3,ix(4))
      fs1=-x1+x2+x3-x4
      fs2=-y1+y2+y3-y4
      fs3=-z1+z2+z3-z4
      ft1=-x1-x2+x3+x4
      ft2=-y1-y2+y3+y4
      ft3=-z1-z2+z3+z4
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      volm=thick*sqrt((e*g-f*f)/16.)
      xie=xie/volm
      return
      end
      subroutine gtglob(sig,ixp,x,n,m)
c     implicit double precision (a-h,o-z)                                    dp
      dimension sig(n,1),ixp(*),ix(4),x(3,1)
c     kka=ixp(1)                                                        cray1
c     kkb=ixp(2)                                                        cray1
c     matp=and(kka,3777777b)                                            cray1
c     ix(3) =and(kkb,3777777b)                                          cray1
c     kk1=shiftr(kka,21)                                                cray1
c     kk2=shiftr(kkb,21)                                                cray1
c     kka=shiftr(kk1,21)                                                cray1
c     kkb=shiftr(kk2,21)                                                cray1
c     ix(1)=and(kk1,3777777b)                                           cray1
c     ix(2)=and(kka,3777777b)                                           cray1
c     ix(4)=and(kk2,3777777b)                                           cray1
      matp=ixp(1)                                                       vax75
      ix(1)=ixp(2)                                                      vax75
      ix(2)=ixp(3)                                                      vax75
      ix(3)=ixp(4)                                                      vax75
      ix(4)=ixp(5)                                                      vax75
      x1=x(1,ix(1))
      x2=x(1,ix(2))
      x3=x(1,ix(3))
      x4=x(1,ix(4))
      y1=x(2,ix(1))
      y2=x(2,ix(2))
      y3=x(2,ix(3))
      y4=x(2,ix(4))
      z1=x(3,ix(1))
      z2=x(3,ix(2))
      z3=x(3,ix(3))
      z4=x(3,ix(4))
      a11=.25*(x2-x1+x3-x4)
      a21=.25*(y2-y1+y3-y4)
      a31=.25*(z2-z1+z3-z4)
      a12=.25*(x4-x1+x3-x2)
      a22=.25*(y4-y1+y3-y2)
      a32=.25*(z4-z1+z3-z2)
      s31=a21*a32-a22*a31
      s32=a31*a12-a32*a11
      s33=a11*a22-a12*a21
      enrm  =1./sqrt(s31**2+s32**2+s33**2)
      s31=s31*enrm
      s32=s32*enrm
      s33=s33*enrm
      enrm  =1./sqrt(a11**2+a21**2+a31**2)
      s11=a11*enrm
      s12=a21*enrm
      s13=a31*enrm
      s21=s32*s13-s12*s33
      s22=s33*s11-s13*s31
      s23=s31*s12-s11*s32
      do 10 i=1,m
      a11=sig(1,i)*s11+sig(4,i)*s21+sig(6,i)*s31
      a12=sig(1,i)*s12+sig(4,i)*s22+sig(6,i)*s32
      a13=sig(1,i)*s13+sig(4,i)*s23+sig(6,i)*s33
      a21=sig(4,i)*s11+sig(2,i)*s21+sig(5,i)*s31
      a22=sig(4,i)*s12+sig(2,i)*s22+sig(5,i)*s32
      a23=sig(4,i)*s13+sig(2,i)*s23+sig(5,i)*s33
      a31=sig(6,i)*s11+sig(5,i)*s21+sig(3,i)*s31
      a32=sig(6,i)*s12+sig(5,i)*s22+sig(3,i)*s32
      a33=sig(6,i)*s13+sig(5,i)*s23+sig(3,i)*s33
      sig(1,i)=s11*a11+s21*a21+s31*a31
      sig(2,i)=s12*a12+s22*a22+s32*a32
      sig(3,i)=s13*a13+s23*a23+s33*a33
      sig(4,i)=s11*a12+s21*a22+s31*a32
      sig(5,i)=s12*a13+s22*a23+s32*a33
      sig(6,i)=s11*a13+s21*a23+s31*a33
   10 continue
      return
      end
      subroutine gtlocl(sig,ix,x,n,m)
c     implicit double precision (a-h,o-z)                                    dp
      dimension sig(n,1),ix(*),x(3,1)
      x1=x(1,ix(1))
      x2=x(1,ix(2))
      x3=x(1,ix(3))
      x4=x(1,ix(4))
      y1=x(2,ix(1))
      y2=x(2,ix(2))
      y3=x(2,ix(3))
      y4=x(2,ix(4))
      z1=x(3,ix(1))
      z2=x(3,ix(2))
      z3=x(3,ix(3))
      z4=x(3,ix(4))
      a11=.25*(x2-x1+x3-x4)
      a21=.25*(y2-y1+y3-y4)
      a31=.25*(z2-z1+z3-z4)
      a12=.25*(x4-x1+x3-x2)
      a22=.25*(y4-y1+y3-y2)
      a32=.25*(z4-z1+z3-z2)
      s13=a21*a32-a22*a31
      s23=a31*a12-a32*a11
      s33=a11*a22-a12*a21
      enrm  =1./sqrt(s13**2+s23**2+s33**2)
      s13=s13*enrm
      s23=s23*enrm
      s33=s33*enrm
      enrm  =1./sqrt(a11**2+a21**2+a31**2)
      s11=a11*enrm
      s21=a21*enrm
      s31=a31*enrm
      s12=s23*s31-s21*s33
      s22=s33*s11-s31*s13
      s32=s13*s21-s11*s23
      do 10 i=1,m
      a11=sig(1,i)*s11+sig(4,i)*s21+sig(6,i)*s31
      a12=sig(1,i)*s12+sig(4,i)*s22+sig(6,i)*s32
      a13=sig(1,i)*s13+sig(4,i)*s23+sig(6,i)*s33
      a21=sig(4,i)*s11+sig(2,i)*s21+sig(5,i)*s31
      a22=sig(4,i)*s12+sig(2,i)*s22+sig(5,i)*s32
      a23=sig(4,i)*s13+sig(2,i)*s23+sig(5,i)*s33
      a31=sig(6,i)*s11+sig(5,i)*s21+sig(3,i)*s31
      a32=sig(6,i)*s12+sig(5,i)*s22+sig(3,i)*s32
      a33=sig(6,i)*s13+sig(5,i)*s23+sig(3,i)*s33
      sig(1,i)=s11*a11+s21*a21+s31*a31
      sig(2,i)=s12*a12+s22*a22+s32*a32
      sig(3,i)=s13*a13+s23*a23+s33*a33
      sig(4,i)=s11*a12+s21*a22+s31*a32
      sig(5,i)=s12*a13+s22*a23+s32*a33
      sig(6,i)=s11*a13+s21*a23+s31*a33
   10 continue
      return
      end
      subroutine tshprt(mpri)
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
      call thprt (a(n1),a(n2),a(n3),a(n4f),a(lc0n),a(lc10),a(lc9),
     1 a(lc11),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),
     2 a(nt01),a(nt02),a(nt03),a(nt04),mpri,a(nt13),a(nt14),
     3 a(lc1t))
c
      return
      end
      subroutine thprt (mtype,ro,cm,csprop,u,a,v,x,ieost,eosp,
     1 ihgq,hgq,iqtype,bkqs,nsubgv,mtnum,nfegp,auxvec,mpri,
     3 nhxpnt,lochvh,ixp)
c     implicit double precision (a-h,o-z)                                    dp
c
      character*8 astate                                                vax75
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      real*8 head                                                       vax75
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk28/summss,xke,xpe,tt
      common/aux14/
     1 stress(7,128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/nwixa/nwcon
      common/aux36/lft,llt
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/drelax/ladd,idrint
c
      dimension mtype(1),ro(1),cm(48,1),csprop(24,1),u(1),a(1),v(1),
     1  x(1),ieost(1),eosp(48,1),ihgq(1),hgq(1),iqtype(1),bkqs(3,1),
     2  nsubgv(1),mtnum(1),nfegp(1),auxvec(1),astate(5),
     3  nhxpnt(*),lochvh(*)
c     dimension ixp(3,*)                                                cray1
      dimension ixp(9,*)                                                vax75
      data astate/'elastic','plastic','failure','       ','temp'/
c
      nelg=1+(numelt-1)/128
      if (mpri.lt.0) go to 180
      if (nstt.eq.0) return
      call header
      write(13,510) ncycle,tt
      write(13,520)
      nel=0
      do 160 nn=1,nelg
      nmel=128
      if (nn.eq.nelg) nmel=numelt-128*(nelg-1)
      do 140 j=1,nmel
      nel=nel+1
      n=nhxpnt(nel)
c     mxe=and(ixp(1,n),3777777b)                                        cray1
      mxe=ixp(1,n)                                                      vax75
      if (mxe.eq.0) go to 140
      nip=csprop(2,mxe)
      mte=mtype(mxe)
      nes=ieost(mxe)
      lav=lochvh(n)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      write(13,530) nel,mxe
      do 110 ipt=1,nip
      stress(1,ipt)=0.
      stress(2,ipt)=0.
      stress(3,ipt)=0.
      stress(4,ipt)=0.
      stress(5,ipt)=0.
      stress(6,ipt)=0.
      stress(7,ipt)=0.
      if (mte.eq.20) go to 100
      call blkcpy (auxvec(lav),stress(1,ipt),7)
      lav=lav+nmtcon
  100 nstate=1
      if ((mte.eq.3.or.mte.eq.5.or.mte.eq.10.or.mte.eq.11)
     1 .and.(stress(7,ipt).ne.0.0)) nstate=2
      write(13,540) ipt,astate(nstate),(stress(i,ipt),i=1,7)
  110 continue
  140 continue
  160 continue
      return
  180 nel=0
      do 410 nn=1,nelg
      noco=nn
      nnm1=128*(nn-1)
      nmel=128
      if (nn.eq.nelg) nmel=numelt-128*(nelg-1)
      do 400 j=1,nmel
      stress(1,j)=0.
      stress(2,j)=0.
      stress(3,j)=0.
      stress(4,j)=0.
      stress(5,j)=0.
      stress(6,j)=0.
      stress(7,j)=0.
      nel=nel+1
      n=nhxpnt(nel)
c     mxe=and(ixp(1,n),3777777b)                                        cray1
      mxe=ixp(1,n)                                                      vax75
      if (mxe.eq.0) go to 400
      nip=csprop(2,mxe)
      mte=mtype(mxe)
      nes=ieost(mxe)
      nmtcon=7+nconst(mte)+ncneos(max0(nes,1))
      if (mte.eq.20) go to 400
      lav=lochvh(n)
      call blkcpy (auxvec(lav),stress(1,j),7)
  400 continue
      if (idrint.eq.0) then
      call wrabsg (iob5,stress,7*nmel,iadd,1)
      call riosta (iob5)
      iadd=iadd+7*nmel
      else
      call wrabsg (iob6,stress,7*nmel,ladd,1)
      call riosta (iob6)
      ladd=ladd+7*nmel
      endif
  410 continue
c
      return
  510 format(///' e l e m e n t   s t r e s s   c a l c u l a t i o n s'
     &,'   f o r   t i m e   s t e p ',i5,7x,' ( at time ',1pe10.4,' )')
  520 format(/' element  stress      sig-xx     sig-yy     sig',
     1'-zz     sig-xy     sig-yz     sig-zx ',26x,'yield' /
     2' num/ipt   state',80x,'   function')
  530 format(i5,'-',i2)
  540 format(5x,i2,2x,a7,1x,6e11.3,11x,4x,e11.3)
      end
c     subroutine panic                                                  ltss
c     implicit double precision (a-h,o-z)                               ltss dp
c**********************************************************************
c                                                                     *
c        routine to stop the run and print out any neccessary         *
c        information needed to restart the job.  used mainly          *
c        by the operators when running under pad to relieve           *
c        panic attacks.                                               *
c                                                                     *
c        roger a. crawfis, asd division                               *
c                                                                     *
c**********************************************************************
c     character  command*72, restart*8, plot*8, dump*8,                 ltss
c    1           timehist*8, temp*8, code*8, joy*8                      ltss
c     integer    n, trim                                                ltss
c     external   trim                                                   ltss
c     external     femdmp,getfn, getnam , adios, getprg                 ltss
c
c     data  command  /  '        '  /                                   ltss
c     call getprg ( code )                                              ltss
c     command(1:8) = code                                               ltss
c     n = trim ( command, 8 )                                           ltss
c     call femdmp (1)                                                   ltss
c     call getfn ( 2, restart )                                         ltss
c     command(:) = command(1:n) // ' o=o.rst r=' // restart             ltss
c     n = trim ( command, 72 )                                          ltss
c
c     call getnam  ( 5, plot )                                          ltss
c     if ( plot .ne. 'd3plot' )  then                                   ltss
c        command(:) = command(1:n) // ' g=' // plot                     ltss
c        n = trim ( command, 72 )                                       ltss
c     endif                                                             ltss
c     call getfn ( 5, plot )                                            ltss
c
c     call getnam  ( 2, dump )                                          ltss
c     if ( dump .ne. 'd3dump' )  then                                   ltss
c        command(:) = command(1:n) // ' d=' // dump                     ltss
c        n = trim ( command, 72 )                                       ltss
c     endif                                                             ltss
c
c     call getnam  ( 4, timehist )                                      ltss
c     if ( timehist .ne. 'd3thdt' )  then                               ltss
c        command(:) = command(1:n) // ' f=' // timehist                 ltss
c        n = trim ( command, 72 )                                       ltss
c     endif                                                             ltss
c     call getfn ( 4, timehist )                                        ltss
c
c     call getnam  ( 11, temp )                                         ltss
c     if ( temp .ne. 'thrmfl' )  then                                   ltss
c        command(:) = command(1:n) // ' t=' // temp                     ltss
c        n = trim ( command, 72 )                                       ltss
c     endif                                                             ltss
c
c     call getnam  ( 10, joy )                                          ltss
c     if ( joy .ne. 'joyfil' )  then                                    ltss
c        command(:) = command(1:n) // ' j=' // joy                      ltss
c        n = trim ( command, 72 )                                       ltss
c     endif                                                             ltss
c
c     write (13, 1001)  command, plot, timehist, restart,               ltss
c    1                  joy, temp                                       ltss
c     write ( *, 1001)  command, plot, timehist, restart,               ltss
c    1                  joy, temp                                       ltss
c1001 format (/'  to restart this job type:'                            ltss
c    1       //6x,a72                                                   ltss
c    2       //'  you will need the files:'                             ltss
c    3       //5x, 3(1x,a8)                                             ltss
c    4        /'   and'                                                 ltss
c    5        /6x, 'alwith.', 2(1x,a6)                                  ltss
c    6       //'   please save any other files.'  // )                  ltss
c     call adios (1)                                                    ltss
c     end                                                               ltss
c     integer function trim ( string, len )                             ltss
c     character  string*(*)                                             ltss
c**********************************************************************
c                                                                     *
c     function to return location of last non-blank character         *
c                                                                     *
c**********************************************************************
c     character  blank*1                                                ltss
c     parameter  (  blank = ' '  )                                      ltss
c
c     do 10  n = len, 1, -1                                             ltss
c        if ( string(n:n) .ne. blank )  then                            ltss
c           trim = n                                                    ltss
c           return                                                      ltss
c        endif                                                          ltss
c  10 continue                                                          ltss
c     trim = 1                                                          ltss
c     return                                                            ltss
c     end                                                               ltss
c     subroutine getprg ( name )                                        ltss
c     implicit double precision (a-h,o-z)                               ltss dp
c**********************************************************************
c                                                                     *
c        routine to return the programs name                          *
c        the name is kept in lowcore address 100b                     *
c                                                                     *
c**********************************************************************
c     pointer    (ip, lowcore)                                          ltss
c     data       ip  /  64  /                                           ltss
c
c     name = lowcore                                                    ltss
c     return                                                            ltss
c     end                                                               ltss
      subroutine sets16(cm,nes)
c     implicit double precision (a-h,o-z)                                    dp
c
c                  p s e u d o   t e n s o r
c        c o n c r e t e / g e o l o g i c a l   m o d e l
c
      dimension cm(*)
      common/bk22/prop(48)
      character*80 txts,mssg
c
c.... principal material properties:
c     cm( 1)=prop( 1)= poisson's ratio or negative of shear modulus
c     cm( 2)=prop( 2)= tensile cutoff (max. prin. stress failure, sigf)
c     cm( 3)=prop( 3)= cohesion (a0)
c     cm( 4)=prop( 4)= pressure hardening coefficient a1
c     cm( 5)=prop( 5)= pressure hardening coefficient a2
c     cm( 6)=prop( 6)= damage scaling factor (b1)
c     cm( 7)=prop( 7)= cohesion for failed material (a0f)
c     cm( 8)=prop( 8)= pressure hardening coef. for failed material (a1f)
c            prop(16)= not currently used
c     cm(16)=          number of points in yield stress table
c     cm(17)=prop(17)= effective plastic strain (epx) or pressure
c         through      (p) values for yield stress table
c     cm(32)=prop(32)
c     cm(33)=prop(33)= yield stress values for yield stress table
c         through
c     cm(48)=prop(48)
c
c     if zero values are specified for prop(3) and prop(4), the entries
c     for prop(17) through prop(32) are assumed to be pressure values
c     instead of values of effective plastic strain.
c
c     if a negative value is specified for prop(3), the value given for
c     prop(2) is assumed to be the unconfined compressive strength of
c     the matrix material instead of the tensile cutoff value. in this
c     case values for the tensile cutoff, cohesion, and pressure hardening
c     coefficients are calculated internally as follows:
c
c          sigf=1.7*((f'c)**2/ucf)**(1/3)
c          a0  =(1/4)*(f'c)
c          a1  =1/3
c          a2  =(1/3)/(f'c)
c          a0f =0.
c          a1f =0.385
c
c     where ucf=-prop(3) is a unit conversion factor for f'c (psi/(dyna
c     pressure unit)).
c
c     a zero equation of state number can also be specified in this case
c     and data for a tri-linear eos8 model (good for pressures below
c     approximately 5 kbars) will be generated internally using the
c     values given for poisson's ratio and f'c. otherwise equation of
c     state 8, 9, or 11 must be specified and the corresponding data
c     provided in the user's material input.
c
c.... reinforcement properties:
c            prop( 9)= % reinforcement
c     cm( 9)=          reinforcement fraction (fr)
c            prop(10)= elastic (young's) modulus (e)
c     cm(10)=          bulk modulus (bkr)
c            prop(11)= poisson's ratio
c     cm(11)=          shear modulus (gr)
c     cm(12)=prop(12)= initial yield stress (qs)
c            prop(13)= hardening (tangent) modulus (et)
c     cm(13)=          plastic hardening modulus (qh=(et*e)/(e-et))
c
c.... strain-rate sensitivity
c     cm(14)=prop(14)= load curve giving sensitivity for principal
c                      material (k1)
c     cm(15)=prop(15)= load curve giving sensitivity for reinforcing
c                      material (k2)
c
c.... mixture properties and failure criterion
c     principal material and reinforcement properties are combined using
c     a rule of mixtures as follows:
c
c          bulk=(1-fr)*bkm+fr*bkr
c          shrm=(1-fr)*gm +fr*gr
c          sigy=(1-fr)*sym+fr*syr
c
c     where sym=f(k1,edot)*(a0+p/(a1+a2*p))*g(dmg) or f(k1,edot)*g(p)
c     and syr=f(k2,edot)*qs+qh*epx. f(k,edot) denotes the yield stress
c     strain-rate scaling factor obtained by linear interpolation from
c     load curve k (if k=0, f=1.0), and g denotes either the damage or
c     pressure scaling factor obtained by linear interpolation from the
c     yield stress table. dmg is an isotropic measure of damage defined
c     as integral[depx/(1+p/sigf)**b1].
c
c     if the maximum principal stress in an element exceeds the tensile
c     cutoff, the matrix material in that element is assumed to have
c     fractured. after fracture the matrix material in an element can
c     support only compressive loads and its shear strength is limited
c     by the yield surface for failed material symf=a0f+p/(a1f+a2*p).
c
c
c     parameter giving minimum value for tensile cutoff
      parameter (sfmin=1.e-10)
c
c.... read material property data (6 cards, 8 fields of 10 per card).
      lfst=1
      llst=8
      do 6 i=1,6
      call gttxsg (txts,lcount)
      read(unit=txts,fmt=60,err=40)(prop(k),k=lfst,llst)
      lfst=lfst+8
      llst=llst+8
    6 continue
c.... if the tensile cutoff is <= 0, set it to sfmin; also set b1=0.
      if(prop(2).le.0.) then
           prop(2)=sfmin
           prop(6)=0.
      endif
c.... copy the data into the working array
      do 10 i=1,48
   10 cm(i)=prop(i)
c.... if reinforcement is specified, calculate elastic constants for it.
      if(prop( 9).ne.0.) then
           cm( 9)=.01*prop( 9)
           cm(10)=prop(10)/(3.*(1.-2.*prop(11)))
           cm(11)=prop(10)/(2.*(1.+prop(11)))
           cm(13)=prop(13)*prop(10)/(prop(10)-prop(13))
      endif
c.... count the number of points in the yield function table.
      cm(16)=0.
      do 30 i=2,16
      if(prop(i+15).lt.prop(i+16)) cm(16)=i
   30 continue
c.... if a yield function table is not specified, set b1=0.
      if(cm(16).eq.0.) cm(6)=0.
c.... abort if a legal eos number is not specified.
      if(.not.(nes.eq.0 .and. prop(3).lt.0.) .and.
     1   nes.ne.8 .and. nes.ne.9 .and. nes.ne.11) then
           write(13,61)
           call adios(2)
      endif
c.... fini
      return
c
c     error termination due to badly formatted data
c
   40 call termin (txts,mssg,lcount,0)
c
   60 format(8e10.0)
   61 format(/' only eos 8, eos 9 and eos 11 are valid with material mo'
     1,'del 16')
      end
      subroutine prmt16(lus,cm,ieost,eosp,head)
c     implicit double precision (a-h,o-z)                                    dp
c
c                  p s e u d o   t e n s o r
c        c o n c r e t e / g e o l o g i c a l   m o d e l
c
      dimension cm(*),eosp(*),head(2,12)
      real*8 head                                                       vax75
      common/bk22/prop(48)
      equivalence (prop(1),gnu),(prop(2),fpc),(prop(3),ucf)
      parameter (cc0=1./3.)
      character nug*4,mlbl(2)*24
      data mlbl/'effective plastic strain'
     1         ,'pressure ...............'/
c
      if(gnu.lt.0.) then
c.... constant shear modulus modulus model
           nug='g ..'
           gnu=-gnu
      else
c.... constant poisson's ratio model
           nug='vnu '
      endif
      write(lus,100)nug,gnu
c.... assume yield function table is given as a function of strain
      l=1
      if(prop(3).lt.0.) then
c.... generate principal material yield parameters based on f'c
           ucf=-prop(3)
           cm(2)=1.7*(fpc**2/ucf)**cc0
           cm(3)=.25*fpc
           cm(4)=cc0
           cm(5)=cc0/fpc
           if(cm(8).eq.0.) cm(8)=.385
           write(lus,110)fpc,ucf
           write(lus,120)(cm(i),i=2,6),cm(14)
           if(ieost.eq.0) then
c.... generate eos8 equation of state data based on f'c
                call m16esg(cm(1),fpc,ucf,ieost,eosp)
                do 10 i=1,12
                head(2,i)=head(1,i)
   10           continue
           endif
      elseif(prop(3).ne.0. .or. prop(4).ne.0.) then
c.... yield pressure dependence given directly using analytic form
           write(lus,120)(cm(i),i=2,6),cm(14)
      else
c.... yield function table is given as a function of pressure
           l=2
           write(lus,130)prop(2)
      endif
      if(cm(8).eq.0.) cm(8)=cc0
      write(lus,140)cm(7),cm(8)
      if(cm(16).ne.0.) then
c.... yield function table with cm(16) points specified
           write(lus,150)mlbl(l),(prop(i),i=17,24)
     1                  ,mlbl(l),(prop(i),i=25,48)
      endif
      if(cm(13).ne.0.) then
c.... print material properties for reinforcement
           write(lus,160)(prop(i),i=9,13),prop(15)
      endif
c.... space for next material printout
      write(lus,170)
c.... fini
      return
c
  100 format(/5x,'principal material properties:'/,
     1 5x,a4, '.............................. =',e12.4)
  110 format(
     1 5x,'unconfined compressive strength .. =',e12.4/,
     2 5x,'unit converison factor for f''c ... =',e12.4)
  120 format(
     1 5x,'tensile cutoff (max. prin. stress) =',e12.4/,
     2 5x,'cohesion ......................... =',e12.4/,
     3 5x,'pressure hardening coefficient a1  =',e12.4/,
     4 5x,'pressure hardening coefficient a2  =',e12.4/,
     5 5x,'damage scaling factor ............ =',e12.4/,
     6 5x,'load curve for strain-rate scaling =',f8.0)
  130 format(
     1 5x,'tensile cutoff (max. prin. stress) =',e12.4)
  140 format(
     1 5x,'cohesion for failed material ..... =',e12.4/,
     2 5x,'pressure hardening coefficient a1f =',e12.4)
  150 format(
     1 5x,a24,                    ' ......... =',8(e8.1,1x)/,
     2 5x,a24,                    ' ......... =',8(e8.1,1x)/,
     3 5x,'effective stress ................. =',8(e8.1,1x)/,
     4 5x,'effective stress ................. =',8(e8.1,1x))
  160 format(/5x,'reinforcement properties:'/,
     1 5x,'% reinforcement .................. =',e12.4/,
     2 5x,'e ................................ =',e12.4/,
     3 5x,'vnu .............................. =',e12.4/,
     4 5x,'initial yield .................... =',e12.4/,
     5 5x,'e (harden) ....................... =',e12.4/,
     6 5x,'load curve for strain-rate scaling =',f8.0)
  170 format(//1x)
      end
      subroutine m16esg(vnu,fpc,ccf,ieost,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     this routine generates eos8 data for plain concrete
c     based on its unconfined compressive strength (f'c)
c
      dimension eosp(*)
      dimension ev(4),p(4),uk(4)
      parameter (cck=57000./3.)
c
c     vnu = poisson's ratio (0.14 < vnu < 0.20)
c     fpc = unconfined compressive strength (f'c)
c     ccf = psi/(dyna pressure unit)
c
      ieost=8
      do 10 i=1,42
      eosp(i)=0.
   10 continue
c
      uk(1)=cck*sqrt(fpc/ccf)
      if(vnu.lt.0.) then
           uk(1)=(vnu*uk(1))/(3.*(3.*vnu+uk(1)))
      else
           uk(1)=uk(1)/(1.-2.*vnu)
      endif
      uk(2)=120.*fpc
      uk(3)=300.*fpc
      p(1)=0.0
      ev(1)=0.0
      p(2)=.2*fpc
      ev(2)=-p(2)/uk(1)
      ev(3)=-.068
      p(3)=p(2)-uk(2)*(ev(3)-ev(2))
      ev(4)=-0.1
      p(4)=p(3)-uk(3)*(ev(4)-ev(3))
      uk(4)=  max(uk(1),uk(3))
      uk(3)=uk(4)
      uk(2)=uk(1)
c
      do 20 i=1,4
      eosp(i)=ev(i)
      eosp(i+10)=p(i)
      eosp(i+30)=uk(i)
   20 continue
      eosp(43)=1.0
      call setes8(eosp)
c
      return
      end
      subroutine inse16(rvl,eosp,vol,sigf)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/auxstr/sig(7),aux(93)
c
c     epx1=sig(7)= effective plastic strain
c     epx2=aux(1)= internal energy
c     epx3=aux(2)= bulk viscosity
c     epx4=aux(3)= current volume
c     epx5=aux(4)= tensile cutoff (max. prin. stress)
c     epx6=aux(5)= fracture relaxation factor
c                  (1 = not failed; 0 = completely failed)
c     epx7=aux(6)= accumulated damage
c     epx8=aux(7)= largest compressive volumetric strain
c
      aux(1)=eosp*vol
      aux(2)=0.
      aux(3)=rvl*vol
      aux(4)=sigf
      aux(5)=1.0
      aux(6)=0.0
      return
      end
      subroutine f3dm16(cm,npc,plc,eosp,mte,nes,ibq,ener,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c                  p s e u d o   t e n s o r
c        c o n c r e t e / g e o l o g i c a l   m o d e l
c
      common/bk02/iburn,dt1,dt2
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128)
      common/aux9/vlrho(128),volnew(128)
      common/aux11/po(128)
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),
     3 epx1(128),epx2(128),epx3(128),epx4(128),epx5(128),epx6(128),
     4 dmg(128),vsm(128),d1d(128),d2d(128),d3d(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux18/dd(128),def(128)
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/aux20/aj2(128),sj2(128),scale(128),dscal(128),cc(128),
     1 p(128),pold(128),davg(128),strt(128),speceo(128),ak(128),
     2 akt(128),ywh(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128)
      common/aux36/lft,llt
      common/aux40/defm(128),defln(128),bulk(128),factor(128),cep(128),
     1 pres(128),puld(128),pnew(128),tec(128),fc(128),fc1(128),fc2(128)
      common/aux43/xm(128)
      common/eosd/pc(128),shrm(128)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common /   / a(1)
c
      dimension  cm(*),npc(*),plc(*),eosp(*),ener(*),fval(*)
      logical ppyf
      parameter (third=1./3.)
c
c     parameter giving fracture relaxation time (1/nsteps)
      parameter (frf=1./20.)
      parameter (frf1=2.*frf)
      parameter (frf2=frf*frf)
c
c     zero tolerance
      parameter (zot=1.e-20)
      data zero,one/0.0,1.0/
c
      sp=0.0
c.... obtain material constants
      mx=48*(mxt(lft)-1)
      gm=cm(mx+1)
      sfi=1./cm(mx+2)
      a0=cm(mx+3)
      a1=cm(mx+4)
      a2=cm(mx+5)
      b1=cm(mx+6)
      a0f=cm(mx+7)
      a1f=cm(mx+8)
      npt=cm(mx+16)
      ppyf=a0.ne.0. .or. a1.ne.0.
      if(ppyf) then
           pmin=-a0*a1/(1.+a0*a2)
      endif
      pminf=  max(-a0f*a1f/(1.+a0f*a2),-third*cm(mx+2))
      k1=cm(mx+14)
      if(k1.ne.0) then
           im=npc(k1)
           nm=(npc(k1+1)-im)/2
      endif
      fr=cm(mx+9)
      if(fr.ne.0.) then
           bkr=cm(mx+10)
           gr=cm(mx+11)
           qs=cm(mx+12)
           qh=cm(mx+13)
           k2=cm(mx+15)
           if(k2.ne.0) then
                ir=npc(k2)
                nr=(npc(k2+1)-ir)/2
           endif
      endif
c.... get current values for elastic moduli of principal material
      do 10 i=lft,llt
      pc(i)=  min(pminf,-third*epx5(i))
      shrm(i)=dim(zero,gm)
   10 continue
      call sueos(eosp,mte,nes)
      if(0..lt.gm) then
c.... constant poisson's ratio model
           do 20 i=lft,llt
           shrm(i)=bulk(i)*((1.5-3.*gm)/(1.+gm))
   20      continue
      endif
c.... calculate strain rate and volume change
      do 30 i=lft,llt
      davg(i)=-third*(d1(i)+d2(i)+d3(i))
      d1d(i)=d1(i)+davg(i)
      d2d(i)=d2(i)+davg(i)
      d3d(i)=d3(i)+davg(i)
      dvol(i)=.5*(volnew(i)-epx4(i))
   30 continue
      if(k1.ne.0 .or. k2.ne.0) then
           do 40 i=lft,llt
           strt(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     1            +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
           strt(i)=sqrt(strt(i))
   40      continue
      endif
c.... calculate new pressure in principal material
      do 50 i=lft,llt
      sign1(i)=sign1(i)+po(i)
      sign2(i)=sign2(i)+po(i)
      sign3(i)=sign3(i)+po(i)
      speceo(i)=epx2(i)
      epx2(i)=0.
   50 continue
      call eqos(fval,ener,mte,nes)
      do 60 i=lft,llt
      sign1(i)=sign1(i)+pnew(i)
      sign2(i)=sign2(i)+pnew(i)
      sign3(i)=sign3(i)+pnew(i)
      p(i)=pnew(i)
   60 continue
      cinen=ener(mxt(lft))
      if(fr.ne.0.) then
c.... add partial pressure due to reinforcement and update energy
           if(nes.eq.9) then
                do 64 i=lft,llt
                defln(i)=defm(i)
   64           continue
           elseif(nes.eq.11) then
                do 66 i=lft,llt
                defln(i)=log(def(i))
   66           continue
           endif
           do 70 i=lft,llt
           ywh(i)=fr*(-bkr*defln(i)-p(i))
           pnew(i)=ywh(i)+p(i)
           ywh(i)=ywh(i)*dvol(i)
           epx2(i)=epx2(i)-ywh(i)
   70      continue
           do 80 i=lft,llt
           cinen=cinen-ywh(i)
   80      continue
c.... calculate scaling factor for load sharing
           er=bkr*gr/(3.*bkr+gr)
           do 90 i=lft,llt
           cc(i)=(3.*bulk(i)+shrm(i))/(bulk(i)*shrm(i))
           cc(i)=1./(fr*(er*cc(i)-1.)+1.)
   90      continue
c.... calculate effective bulk modulus and shear modulus
           do 100 i=lft,llt
           bulk(i)=bulk(i)+fr*(bkr-bulk(i))
           shrm(i)=shrm(i)+fr*(gr-shrm(i))
  100      continue
      endif
c.... calculate elastic update to stress deviator
      do 110 i=lft,llt
      sign1(i)=sign1(i)+2.*dt1*shrm(i)*d1d(i)
      sign2(i)=sign2(i)+2.*dt1*shrm(i)*d2d(i)
      sign3(i)=sign3(i)+2.*dt1*shrm(i)*d3d(i)
      sign4(i)=sign4(i)+dt1*shrm(i)*d4(i)
      sign5(i)=sign5(i)+dt1*shrm(i)*d5(i)
      sign6(i)=sign6(i)+dt1*shrm(i)*d6(i)
  110 continue
c.... compute maximum principal stress
      do 112 i=lft,llt
      aj2(i)=.5*(sign1(i)**2+sign2(i)**2+sign3(i)**2)
     1+sign4(i)**2+sign5(i)**2+sign6(i)**2
      sj2(i)=sign1(i)*sign5(i)**2+sign2(i)*sign6(i)**2
     1+sign3(i)*sign4(i)**2-sign1(i)*sign2(i)*sign3(i)
     2-2.*sign4(i)*sign5(i)*sign6(i)
  112 continue
      seps=zot*bulk(lft)
      do 114 i=lft,llt
      akt(i)=2.*sqrt(third*aj2(i))
      sj2(i)=sj2(i)+sign(seps,sj2(i))
      ywh(i)=  min(abs(sj2(i))/(third*aj2(i)*akt(i)+seps),one)
  114 continue
      do 116 i=lft,llt
      ywh(i)=third*acos(sign(ywh(i),-sj2(i)))
      sj2(i)=akt(i)*cos(ywh(i))-pnew(i)
  116 continue
c.... scale max. prin. stress to account for load sharing
      if(fr.ne.0.) then
           do 118 i=lft,llt
           sj2(i)=cc(i)*sj2(i)
  118      continue
      endif
c.... calculate minimum yield stress in principal material
      do 120 i=lft,llt
      fc1(i)=  max(p(i),pminf)
      fc1(i)=a0f+fc1(i)/(a1f+a2*fc1(i))
      akt(i)=0.
  120 continue
c.... calculate yield stress (ak = sigy, akt = dak/depx1)
      if(ppyf) then
c.... yield stress is given as an analytic function of pressure
           do 130 i=lft,llt
      ak(i)=  max(p(i),pmin)
           ak(i)=a0+ak(i)/(a1+a2*ak(i))
  130      continue
           if(npt.gt.0) then
c.... adjust yield stress for damage effects
                call yields(npt,cm(mx+17),cm(mx+33),dmg,fc,akt,lft,llt)
                if(b1.ne.0.) then
                     do 135 i=lft,llt
                     dscal(i)=  max(1.+sfi*p(i),zot)
                     dscal(i)=dscal(i)**(-b1)
                     akt(i)=dscal(i)*akt(i)
  135                continue
                endif
                do 140 i=lft,llt
                ak(i)=ak(i)-fc1(i)
                akt(i)=ak(i)*akt(i)
                ak(i)=fc(i)*ak(i)+fc1(i)
  140           continue
           endif
      else
c.... yield stress is given as a tabular function of pressure
           call yields(npt,cm(mx+17),cm(mx+33),p,ak,cc,lft,llt)
      endif
c.... adjust yield stress and tensile cutoff for rate effects
      if(k1.ne.0) then
           call srscal(nm,plc(im),strt,ywh,lft,llt)
           do 150 i=lft,llt
           ak(i)=ywh(i)*ak(i)
           akt(i)=ywh(i)*akt(i)
           fc1(i)=ywh(i)*fc1(i)
           sj2(i)=sj2(i)/ywh(i)
  150      continue
      endif
c.... adjust yield stress for tensile state in failed material
      do 160 i=lft,llt
      if(epx5(i).lt.sj2(i)) then
           epx6(i)=dim(epx6(i),frf1*sqrt(1.-epx6(i))+frf2)
           epx5(i)=epx6(i)*epx5(i)
           ak(i)=epx6(i)*ak(i)+(1.-epx6(i))*  min(ak(i),fc1(i))
           akt(i)=epx6(i)*akt(i)
      endif
  160 continue
c.... add contribution to yield stress from reinforcement
      if(fr.ne.0.) then
           if(k2.ne.0) then
                call srscal(nr,plc(ir),strt,ywh,lft,llt)
                do 180 i=lft,llt
                ywh(i)=qs*ywh(i)
  180           continue
           else
                do 190 i=lft,llt
                ywh(i)=qs
  190           continue
           endif
           do 200 i=lft,llt
           ak(i)=ak(i)+fr*((ywh(i)+qh*epx1(i))-ak(i))
           akt(i)=akt(i)+fr*(qh-akt(i))
  200      continue
      endif
c.... calculate second invariant of the stress deviator
      do 210 i=lft,llt
      sj2(i)=sqrt(3.*aj2(i))
  210 continue
c.... calculate increment in effective plastic strain & new yield stress
      do 220 i=lft,llt
      scale(i)=third/shrm(i)
      cc(i)=dim(sj2(i),ak(i))/(zot+dim(one,-scale(i)*akt(i)))
      cc(i)=scale(i)*  min(sj2(i),cc(i))
      ak(i)=  max(ak(i)+akt(i)*cc(i),zero)
  220 continue
c.... calculate factor to scale stresses back to yield surface
      do 230 i=lft,llt
      scale(i)=(ak(i)+seps)/(  max(ak(i),sj2(i))+seps)
  230 continue
c.... scale back stress and increment plastic strain
      do 240 i=lft,llt
      sign1(i)=scale(i)*sign1(i)
      sign2(i)=scale(i)*sign2(i)
      sign3(i)=scale(i)*sign3(i)
      sign4(i)=scale(i)*sign4(i)
      sign5(i)=scale(i)*sign5(i)
      sign6(i)=scale(i)*sign6(i)
      epx1(i)=epx1(i)+cc(i)
  240 continue
c.... update damage accumulation
      if(b1.ne.0.) then
           do 250 i=lft,llt
           cc(i)=dscal(i)*cc(i)
  250      continue
      endif
      do 260 i=lft,llt
      dmg(i)=dmg(i)+cc(i)
  260 continue
c.... update bulk viscosity, time step size, internal energy
      do 300 i=lft,llt
      ywh(i)=epx2(i)
      epx2(i)=speceo(i)
      cb(i)=1.33*shrm(i)+bulk(i)
  300 continue
      call bulkq(ibq,a(nh17))
      call hieupd
      do 310 i=lft,llt
      cinen=cinen+epx2(i)
  310 continue
      ener(mxt(lft))=cinen
      do 320 i=lft,llt
      epx2(i)=epx2(i)+ywh(i)
  320 continue
c.... update stress components
      do 330 i=lft,llt
      sign1(i)=sign1(i)-pnew(i)
      sign2(i)=sign2(i)-pnew(i)
      sign3(i)=sign3(i)-pnew(i)
  330 continue
c
c.... fini
      return
c
      end
      subroutine srscal(n,f,x,s,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension f(2,*),x(*),s(*)
      dimension il(128)
c
      if(n.lt.3) then
           do 10  i=lft,llt
           il(i)=0
   10      continue
      else
           call sluf(n-2,2,f(1,2),llt-lft+1,x(lft),il(lft))
      endif
      do 30 i=lft,llt
      s(i)=((f(2,il(i)+2)-f(2,il(i)+1))/(f(1,il(i)+2)-f(1,il(i)+1)))
      s(i)=f(2,il(i)+1)+s(i)*(x(i)-f(1,il(i)+1))
   30 continue
      return
      end
      subroutine yields(npt,eps,sige,epx,ak,qh,lft,llt )
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension     eps(16),sige(16),epx(llt),ak(llt),qh(llt)
c
      integer  il(128),i
      external sluf
c
      if(npt.lt.3) then
           do 10  i=lft,llt
           il(i)=0
   10      continue
      else
           call sluf(npt-2,1,eps(2),llt-lft+1,epx(lft),il(lft))
      endif
      do 20  i=lft,llt
      qh(i)=(sige(il(i)+2)-sige(il(i)+1))/(eps(il(i)+2)-eps(il(i)+1))
      ak(i)=sige(il(i)+1)+qh(i)*(epx(i)-eps(il(i)+1))
   20 continue
      return
      end
      subroutine sluf ( n, stride, table, neval, x, index )             vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      integer   n, stride, neval, index(neval)                          vaxcs
      dimension x(neval), table(stride,n)                               vaxcs
c***************************************************************
c*                                                             *
c*     vax version (standard fortran) of the ctss cal coded    *
c*     routine sluf - a table look-up function.                *
c*                                                             *
c*     author:  roger a. crawfis                               *
c*                                                             *
c***************************************************************
c
      do 100  i = 1, neval                                              vaxcs
      iloc = 2                                                          vaxcs
   10 continue                                                          vaxcs
      if ( ( x(i) .lt. table(1,iloc) ) .or. ( iloc .ge. n ) )  goto  20 vaxcs
         iloc = iloc + 1                                                vaxcs
         goto  10                                                       vaxcs
   20 continue                                                          vaxcs
      index(i) = iloc - 1                                               vaxcs
  100 continue                                                          vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine drver(mtype,ro,cm,ic,bcs,npc,pld,nod,idirn,ncur,clfac,
     1lc,nvel,vx,vy,vz,fvalue,rd,ilcw,nsw,numtp,nodtie,tim,iparm,irects,
     2irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,ilocs,ilocm,stfs,stfm,
     3irtls,irtlm,amsm,e,crst,b,tcode,u,v,a,x,xms,ac,nsubgv,mtnum,
     4nfegp,auxvec,rhsi,zfac,ieost,eosp,ihgq,hgq,iqtype,bkqs,rbu,rbv,
     5rba,rbi,rbm,rbcor,nrbn,nrba,nrb,xrb,yrb,zrb,axrb,ayrb,azrb,
     6rbfx,rbfy,rbfz,rbcods,mxrb,xyzkcn,lpntbk,lbcket,chrlen,
     7ethik,fric,iseg,fdat,fthik,icls,irctsi,irctmi,ilcf,itcode,
     8atcode,ifo,slvfrc,msrfrc,ener,rots,fails,sfail,fl9s,tfail,isf,
     9drdsps,nnfpln,thkslv,thkmsr,tarray,carry1,carry2,harray,
     1curves,nhv,maxp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),itts,time(2,8)
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/b10a/inpsd,nmmtde,nmelde,nmmass
      common/b10b/ld01,ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(7)
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk26/nint
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/tmstp/tsmin,hmax,lctm
      character*4 mess                                                  vax75
      common/aux38/mess
      common/energy/xinen
      common/rigidb/ nodtot, ncflag,numrbi
      common/nwixa/nwcon
      common/sbic/ijne,nblk
      common/rn/irnxx
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/tint/itsc,itbs,itlc
      common/slv2/thk,isrch
      common/bttn/ntnwf,ixa(10)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/shlopt/istrns(1),istupd,ibelyt,miter
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/swmke/swxmom,swymom,swzmom,swkeng
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/eltspf/ietspf
      common/tbsysp/numrcf,n6e,n6f,n6g
      character*6 elname
      common/elnme/ elname(5)
      common/bmsc/numsl(20)
      common/thfl/iaddth(3)
      common/extwrk/locaxa,exwk,iewflg
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/drelax/ladd,idrint
      character*72 txtsg1,txtsg2,txtsg3
      common/drvlbs/ txtsg1,txtsg2,txtsg3
      common /colvss/ ltvg,ltvb
      common /iaio/ iosave, iunit, itty, ncolum
      common/rang/xmin,xmax,ymin,ymax,tvxmin(4)
      common /mnmax/ xmnset, xmxset, ymnset, ymxset
      logical ffopt,logo
      common/logocm/ffopt,logo,sizlll,xlll,ylll
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      common /strnsc/ istrn, icmtyp
      common /crvmod/ npavg, xscale, xsclei, fscale
      character*8 ifhed
      common /ifhcom/ ifhed(50)
      logical igrdl, nofram, iwrite
      common/grdl/igrdl,nofram
      common/helpme/ihelp,nu
      common /finges/ igol, fringe, xres, yres, numcol, icolr(6)
      common /charc/ ichar, txtsiz
      logical colgf
      common /colvsc/ iclnum,iclfr8,colgf,icurcl
      logical gson
      common/solidf/gson
      logical viewp
      common/viewp/viewp
      character*4 icm1a,icm1b
      character*8 give,class,ftype
      common /fr80o/ give,class,ftype
      common /rjcom/ ipltft,nrjet,irjet
      logical print
      common/pamper/print
      common/cmlaba/mlloc(30),iesloc(30)
      character*72 mtls,eoslbs
      common/cmlabb/mtls(200),eoslbs(200)
c
      dimension ilcf(*)
      dimension mtype(*),ro(*),cm(48,*),ic(*),bcs(4,*),npc(*),nfegp(*),
     1pld(*),nod(*),idirn(*),ncur(*),clfac(*),lc(*),nvel(*),vx(*),
     2vy(*),vz(*),fvalue(*),rd(14,*),ilcw(*),nsw(*),numtp(*),nodtie(*),
     3tim(*),iparm(8,*),irects(*),irectm(*),nsv(*),msr(*),nsegs(*),
     4nsegm(*),lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),
     5irtls(*),irtlm(*),amsm(*),e(*),crst(2,*),nsubgv(*),mtnum(*),b(*),
     6tcode(*),u(*),v(*),a(*),x(*),xms(*),ac(3,*),auxvec(*),ener(*),
     7timusd(5),rhsi(*),zfac(*),ieost(*),eosp(48,*),ihgq(*),hgq(*),
     8iqtype(*),bkqs(3,*),rbu(6,*),rbv(6,*),rba(6,*),rbi(3,3,*),rots(*),
     9rbm(*),rbcor(3,*),nrbn(*),nrba(*),nrb(*),xrb(*),yrb(*),zrb(*),
     aaxrb(*),ayrb(*),azrb(*),rbfx(*),rbfy(*),rbfz(*),rbcods(2,*),
     bmxrb(*),xyzkcn(*),lpntbk(*),lbcket(*),chrlen(*),irctsi(*),
c    c  ethik(*),fric(3,*),iseg(*),fdat(5,*),fthik(*),icls(*),irctmi(*) cray1
     cethik(*),fric(3,*),iseg(*),fdat(5,*),fthik(*),icls(3,*),irctmi(*) vax75
      dimension itcode(*),atcode(9,*),ifo(2,*),slvfrc(3,*),msrfrc(3,*),
     1nnfpln(*),fails(*),sfail(2,*),fl9s(5,*),tfail(*),drdsps(*),
     2thkslv(*),thkmsr(*)
      dimension auxstr(90),curves(2,maxp,*),npts(9)
      dimension fval(30,3)
      dimension harray(nhv,*),tarray(*),carry1(*),carry2(*)
      character*8 namef,icmd,icmdsv
      character*72 txstrn(99)
      equivalence (aux,buffer)
c     data ieot  / 20000000000000000000b /                              cray1
c
      logical sw6
c
       third = 1.0 / 3.0
c
      call lodset (npc,pld,curves,npts,maxp)
      icmd  ='    '
      give  ='keep'
      class ='unclass'
      ftype ='fiche48'
      ichar =1
      xres  =1024.
      yres  =1024.
      istrn =0
      ionce =0
      sizlll=.04
      xlll  =.95
      ylll  =.95
      iunit =59
      itty  =59
      nu    =59
      iosave=12
      ncolum=82
      fscale=0.
      xscale=0.
      xmnset=0.
      xmxset=0.
      ymnset=0.
      ymxset=0.
      viewp =.false.
      print =.false.
      gson  =.false.
      colgf =.false.
      ffopt =.true.
      igrdl =.false.
      logo=.true.
      tvxmin(1)=.10
      tvxmin(2)=.95
      tvxmin(3)=.10
      tvxmin(4)=.95
c
      txtsg3='dyna3d material model driver'
c
      call blkcpy (auxvec,auxstr,50)
c
c     store labels
c
      do 10 i=1,99
   10 call getthl (1,i,txstrn(i))
c
   20 continue
c
      harray(11,1)=0.0
      harray(12,1)=0.0
      harray(13,1)=0.0
      harray(14,1)=0.0
      tt=0.0
c
      if (icmd.eq.'cont') then
      call blkcpy (auxstr,auxvec,50)
      endif
c
      do 100 ncycle=1,itts
      tarray(ncycle)=tt
c
c     compute and store current value of load functions
c
      if (ncycle.ne.1) then
      call lodstc (fval(1,2),npts,curves,tt,maxp)
      else
      fval(1,2)=0.0
      fval(2,2)=0.0
      fval(3,2)=0.0
      fval(4,2)=0.0
      fval(5,2)=0.0
      fval(6,2)=0.0
      fval(7,2)=0.0
      fval(8,2)=0.0
      fval(9,2)=0.0
      endif
c
c      zero strain rate on first cycle
c
      if(ncycle .eq. 1) then
      fval(1,3)=0.0
      fval(2,3)=0.0
      fval(3,3)=0.0
      fval(4,3)=0.0
      fval(5,3)=0.0
      fval(6,3)=0.0
      fval(7,3)=0.0
      fval(8,3)=0.0
      fval(9,3)=0.0
      else
      fval(1,3)=(fval(1,2)-fval(1,1))/dt1
      fval(2,3)=(fval(2,2)-fval(2,1))/dt1
      fval(3,3)=(fval(3,2)-fval(3,1))/dt1
      fval(4,3)=(fval(4,2)-fval(4,1))/dt1
      fval(5,3)=(fval(5,2)-fval(5,1))/dt1
      fval(6,3)=(fval(6,2)-fval(6,1))/dt1
      fval(7,3)=(fval(7,2)-fval(7,1))/dt1
      fval(8,3)=(fval(8,2)-fval(8,1))/dt1
      fval(9,3)=(fval(9,2)-fval(9,1))/dt1
      endif
c
c
      ener(1)=0.
c
      dt2=1.0e+6
      lcz=0
      call slddrv (harray(1,ncycle),harray(1,ncycle-1),ncycle,fval)
c
      call blkcpy (fval(1,2),fval(1,1),30)
  100 tt=tt+dt1
      if (icmd.eq.'cont') go to 120
  110 call seldev (16)                                                  diglb
c 110 call getnum (6,'.tmds?',crud,l)                                   cray1
      l=ldvnme                                                          diglb
      if (itmds.ne.0) call endtv
      if (l.lt.0) call getnum (15,'.green channel?',argum,ltvg)
      if (l.lt.0) call getnum (14,'.blue channel?',argum,ltvb)
      l=iabs(l)
      if (l.eq.2137) then
      ltvg=1625
      ltvb=2140
      elseif (l.eq.2474) then
      ltvg=1302
      ltvb=2475
      elseif (l.eq.2007) then
      ltvg=2010
      ltvb=2011
      elseif (l.eq.306) then
      ltvg=632
      ltvb=1501
      elseif (l.eq.2264) then
      ltvg=2265
      ltvb=2266
      elseif (l.eq.2267) then
      ltvg=2270
      ltvb=2271
      elseif (l.eq.604 ) then
      ltvg=551
      ltvb=562
      elseif (l.eq.2272) then
      ltvg=2273
      ltvb=2274
      endif
      call gettvm (l)
      if (ionce.eq.0) then
      ionce=1
      call drvhlp
      endif
  120 call getsym (1,'.',icmd,4)
      icmdsv=icmd
c
      if (icmd.eq.'tv') then
      go to 110
      endif
c
      if (icmd.eq.'ecom') then
      call show
      go to 120
      endif
c
      if (icmd.eq.'head') then
      write(*,260)
      read(*,265) txtsg3
      go to 120
      endif
c
c     time history plots
c
      if (icmd.eq.'time') then
      xmin=0.0
      xmax=itts*dt1
      call getnum (25,'.component number please?',argum,ncomp)
      ibgn=1
      istrn=ncomp/100
      if (istrn.gt.0) ibgn=15
      if (istrn.eq.4) ibgn=8
      nc=ncomp-100*istrn
      txtsg1=' time'
      do 130 i=1,itts
      q=harray(10,i)
      call gtvars (harray(ibgn,i),carry2(i),harray(22,i),q,nc
     1 ,harray(21,i),ierr,denpro,istrn)
      if (i.eq.1) then
      call getthl (ibgn/7+1,nc,txtsg2)
      if (ibgn.eq.1) txtsg2=txstrn(nc)
      endif
      if (ierr.eq.1) go to 120
  130 continue
      call cspltc (itts,tarray,carry2,ncomp)
      go to 120
      endif
c
c     cross plots
c
      if (icmd.eq.'cros') then
      call getnum (31,'.first component number please?',argum,ncmp1)
      call getnum (31,'.second component number please?',argum,ncomp)
      xmin=1.e+30
      xmax=-xmin
      ibgn1=1
      istrn1=ncmp1/100
      if (istrn1.gt.0) ibgn1=15
      if (istrn1.eq.4) ibgn1=8
      nc1=ncmp1-istrn1*100
      ibgn2=1
      istrn2=ncomp/100
      if (istrn2.gt.0) ibgn2=15
      if (istrn2.eq.4) ibgn2=8
      nc2=ncomp-istrn2*100
      do 140 i=1,itts
      q=harray(10,i)
      call gtvars (harray(ibgn1,i),carry1(i),harray(22,i),q,nc1
     1 ,harray(21,i),ierr,denpro,istrn1)
      if (i.eq.1) then
      call getthl (ibgn1/7+1,nc1,txtsg2)
      if (ibgn1.eq.1) txtsg2=txstrn(nc1)
      endif
      if (ierr.eq.1) go to 120
      call gtvars (harray(ibgn2,i),carry2(i),harray(22,i),q,nc2
     1 ,harray(21,i),ierr,denpro,istrn2)
      if (i.eq.1) then
      call getthl (ibgn2/7+1,nc2,txtsg1)
      if (ibgn2.eq.1) txtsg1=txstrn(nc2)
      endif
      if (ierr.eq.1) go to 120
      xmin=  min(xmin,carry2(i))
      xmax=  max(xmax,carry2(i))
  140 continue
      if (xmin.eq.xmax) then
      write ( *,250) ncomp
      go to 120
      endif
      call cspltc (itts,carry2,carry1,ncomp)
      go to 120
      endif
c
c     set abcissa min and max values
c
      if (icmd.eq.'aset') then
      call getnum (23,'.abcissa minimum value?',xmnset,iargum)
      call getnum (23,'.abcissa maximum value?',xmxset,iargum)
      go to 120
      endif
c
c     set ordinate min and max values
c
      if (icmd.eq.'oset') then
      call getnum (24,'.ordinate minimum value?',ymnset,iargum)
      call getnum (24,'.ordinate maximum value?',ymxset,iargum)
      go to 120
      endif
c
c     scale factor for abcissa
c
      if (icmd.eq.'ascl') then
      call getnum (22,'.abcissa scale factor?',xscale,iargum)
      if (xscale.ne.0.0) xsclei=1./xscale
      go to 120
      endif
c
c     scale factor for ordinate values
c
      if (icmd.eq.'oscl') then
      call getnum (23,'.ordinate scale factor?',fscale,iargum)
      go to 120
      endif
c
c     turn on plotting
c
      if (icmd.eq.'plot') then
      call getsym (18,'.enter box number?',icm1a,4)
      icm1b=icm1a
      if (icm1a.eq.'box') call getsym (6,ibmes,icm1b,4)
      icm1a='box'
      write(ifhed(36),170) icm1a,icm1b
      call getpdv
      go to 120
      endif
c
      if (icmd.eq.'tv') go to 110
c
c     grid overlaying plot options
c
      if (icmd.eq.'grid') then
      igrdl=.true.
      go to 120
      endif
      if (icmd.eq.'nogr') then
      igrdl=.false.
      go to 120
      endif
c
c     write pampers database
c
      if (icmd.eq.'prin') then
      print=.true.
      call getsym (32,'.enter filename for data output:',namef,8)
      close(unit=21,status='keep')
      open(unit=21,file=namef,status='unknown',form='formatted')
      go to 120
      endif
c
c     help package
c
      if (icmd.eq.'help') then
      call drvhlp
      go to 120
      endif
c
c     check for termination
c
      if (icmd.eq.'t'.or.icmd.eq.'quit'.or.icmd.eq.'end') then
      call endgr
      if (print) close(unit=21,status='keep')
      call adios (1)
      endif
c
c     give fr80 files
c
      if (icmd.eq.'give') then
      give='give'
      go to 120
      endif
c
c     define fr80 type
c
      if (icmd.eq.'fr80') then
      call getsym (18,'.define fr80 type:',ftype,8)
      if (ftype.eq.'fiche48' ) go to 120
      if (ftype.eq.'fich48d' ) go to 120
      if (ftype.eq.'fiche24' ) go to 120
      if (ftype.eq.'fich24d' ) go to 120
      if (ftype.eq.'35mm'    ) go to 120
      if (ftype.eq.'color35' ) go to 120
      if (ftype.eq.'dico35'  ) go to 120
      if (ftype.eq.'p16mm'   ) go to 120
      if (ftype.eq.'color16' ) go to 120
      if (ftype.eq.'dico16'  ) go to 120
      if (ftype.eq.'cslide35') go to 120
      if (ftype.eq.'hardcopy') go to 120
      if (ftype.eq.'report'  ) go to 120
      if (ftype.eq.'vugraph' ) go to 120
      if (ftype.eq.'vugraf11') go to 120
      if (ftype.eq.'help') write ( *, 180)
      if (ftype.ne.'help') write ( *, 190)
      go to 120
      endif
c
c     specify classification level for output
c
      if (icmd.eq.'clas') then
      call getsym (26,'.give classification level:',class,8)
      if (class.eq.'unclass' ) go to 120
      if (class.eq.'proglev' ) go to 120
      if (class.eq.'pard'    ) go to 120
      if (class.eq.'adp'     ) go to 120
      if (class.eq.'confidnt') go to 120
      if (class.eq.'srd'     ) go to 120
      if (class.eq.'system'  ) go to 120
      if (class.eq.'help'    ) write ( *, 200)
      if (class.ne.'help'    ) write ( *, 210)
      go to 120
      endif
c
c     set plots to rjet
c
      if (icmd.eq.'rjet') then
      call getnum (31,'.enter your rjet number please:',argum,nnrjet)
      nrjet=nnrjet
      ipltft=2
      go to 120
      endif
c
c     change label
c
      if (icmd.eq.'chgl') then
      call getnum (32,'.define variable number (1-99):',argum,i)
      write (  *,220) ieot
      read  (  *,230) txstrn(i)
      go to 120
      endif
c
c     redefine time vs. strndr curves
c
      if (icmd.eq.'rdlc') then
      call getnum (20,'.enter curve number: ',argum,i)
      if (i.lt.1.or.i.gt.5) then
      write (  *,240) i
      go to 120
      endif
      nlcur=max0(nlcur,i)
      call getnum (24,'.enter number of points: ',argum,npts(i))
      npoint=npts(i)
      do 150 n=1,npoint
      call getnum(18,'.enter time value:',curves(1,n,i),iargu)
      call getnum(20,'.enter strndr value:',curves(2,n,i),iargu)
  150 continue
      go to 120
      endif
c
c     rename pampers file
c
      if (icmd.eq.'file') then
      call getsym (36,'.enter new filename for data output:',namef,8)
      close(unit=21,status='keep')
      open(unit=21,file=namef,status='unknown',form='formatted')
      go to 120
      endif
c
c     logo for llnl
c
      if (icmd.eq.'logo') then
      logo=.not.logo
      go to 120
      endif
c
c     repeat analysis
c
      if (icmd.eq.'cont') then
      go to 20
      endif
c
      write (  *,160) icmd
      go to 120
c
  160 format (' unrecognizable command ',a4,' please try another.')
  170 format (a3,1x,a3)
  180 format(' available film types include:',
     1 /'     fiche48, fich48d, fiche24, fich24d, 35mm, color35,',
     2 /'     dico35, p16mm, color16, dico16, cslide35, hardcopy,',
     3 /'     report, vugraph, vugraf11',/)
  190 format(' invalid film type specified. available types include:',
     1 /'     fiche48, fich48d, fiche24, fich24d, 35mm, color35,',
     2 /'     dico35, p16mm, color16, dico16, cslide35, hardcopy,',
     3 /'     report, vugraph, vugraf11',/)
  200 format (' available classification levels include:',
     1 /'     proglev, unclass, pard, adp, confidnt, srd, system'/)
  210 format (' invalid level specified, available levels include:',
     1 /'     proglev, unclass, pard, adp, confidnt, srd, system'/)
  220 format ('.enter label:',a8)
  230 format (a72)
  240 format (i5,' is an illegal load curve number (0<i<6)'/)
  250 format (' the minimum and maximum values of component',i3,/
     1        ' are identical-plotting aborted.')
  260 format( ' enter new heading (28 char max):' )
  265 format(a28)
      end
      subroutine cspltc (np,x,f,nf)
c     implicit double precision (a-h,o-z)                                    dp
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      common /colvss/ ltvg,ltvb
      logical colgf
      common /colvsc/ iclnum,iclfr8,colgf,icurcl
      logical gson
      common/solidf/gson
      common /finges/ igol, fringe, xres, yres, numcol, icolr(6)
      logical viewp
      common/viewp/viewp
      logical print
      common/pamper/print
      dimension tx(5), ty(5), x(1), f(1), ixc(1)
      data tx/0.,1.,1.,0.,0./,ty/0.,0.,1.,1.,0./
c
c     for fr80 graphics we must sort by colors
c
      ipf=0
c     if ((ifr80.eq.0).or.(ltvg.eq.0)) then                             pltlb
      call crsplt (ipf,np,x,f,nf)
      return
c     endif                                                             pltlb
c
c     red
c
c     call colro (2)                                                    pltlb
c     iclnum =2                                                         pltlb
c     colgf=.true.                                                      pltlb
c     call crsplt (ipf,np,x,f,nf)                                       pltlb
c     colgf=.false.                                                     pltlb
c
c     green
c
c     icurcl =-3                                                        pltlb
c     call colro (3)                                                    pltlb
c     iclnum =3                                                         pltlb
c     colgf=.true.                                                      pltlb
c     call crsplt (ipf,np,x,f,nf)                                       pltlb
c     colgf=.false.                                                     pltlb
c
c     blue
c
c     icurcl =-3                                                        pltlb
c     call colro (4)                                                    pltlb
c     iclnum =4                                                         pltlb
c     colgf=.true.                                                      pltlb
c     if (gson) then                                                    pltlb
c     call crsplt (ipf,np,x,f,nf)                                       pltlb
c     else                                                              pltlb
c     if (viewp) call setvwprt(0.0,1.0,0.0,1.0)                         pltlb
c     call shader (tx,ty,4,xres,yres,16.,0)                             pltlb
c     if (viewp) call setvwprt(.15,.85,.15,.85)                         pltlb
c     endif                                                             pltlb
c     colgf=.false.                                                     pltlb
c     call plotpc                                                       pltlb
c     return                                                            pltlb
      end
      subroutine crsplt (ipf,np,x,f,nf)
c     implicit double precision (a-h,o-z)                                    dp
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      character*1 label
      common /alphab/ label(26)
      common /mnmax/ xmnset, xmxset, ymnset, ymxset
      logical colgf
      common /colvsc/ iclnum,iclfr8,colgf,icurcl
      common /crvmod/ npavg, xscale, xsclei, fscale
      logical iprofl
      common /profle/ iprofl, noreop, numcmp
      character*72 txtsg1,txtsg2,txtsg3
      common/drvlbs/ txtsg1,txtsg2,txtsg3
      logical print
      common/pamper/print
      character*81 textsg
      dimension x(*), f(*), xl(2), fl(2)
      data numppm/0/
      ipf=ipf+1
      ymin=f(1)
      ymax=ymin
      do 10 i=2,np
      ymin=  min(ymin,f(i))
      ymax=  max(ymax,f(i))
   10 continue
      if (ymin.eq.ymax) then
      write (  *,60) nf
      return
      endif
      tvxmin=.20
      tvymin=.20
      if (xscale.ne.0.0) call skale (x,xscale,np,xmin,xmax)
      if (fscale.ne.0.0) call skale (f,fscale,np,ymin,ymax)
      if (npavg.eq.0) go to 20
      nf=min0(nf,9)
      call filter (f,f(nf+1),np,nf,npavg)
   20 if (abs(xmnset)+abs(xmxset).eq.0.0) go to 30
      xmin=  min(xmnset,xmxset)
      xmax=  max(xmnset,xmxset)
      call zfmnmx (np,x,f)
   30 if (abs(ymnset)+abs(ymxset).ne.0.0) then
      ymin=  min(ymnset,ymxset)
      ymax=  max(ymnset,ymxset)
      endif
c
c     plot grid
c
      call tvgrid (30,30)
c
c     write labels
c
      xc=.40
      yc=.075
      tsiz=65.0
      fac1=0.0
      call plotxt (xc,yc,txtsg1,60,tsiz,fac1)
      fac1=90.0
      call plotxt (yc,xc,txtsg2,60,tsiz,fac1)
      xc=.20
      yc=.975
      tsiz=50.0
      fac1=0.0
      call plotxt (xc,yc,txtsg3,60,tsiz,fac1)
c
      call stwndw (xmin,xmax,ymin,ymax)
      call stvprt (tvxmin,tvxmax,tvxmin,tvxmax)
      np1=np-1
c
      if (print) then
      if (ipf.eq.1) then
      numppm=numppm+1
      write (21,70) numppm,txtsg1,txtsg2
      do 40 l=1,np
      write (21,80) x(l),f(l)
   40 continue
      endif
      endif
      do 50 l=1,np1
      xl(1)=x(l)
      fl(1)=f(l)
      xl(2)=x(l+1)
      fl(2)=f(l+1)
   50 call plotln (xl,fl,2)
      if (.not.colgf) call plotpc
      if (xscale.ne.0.0) call skale (x,xsclei,np,xmin,xmax)
      tvxmin=.125
      tvymin=.125
      return
c
   60 format (' the minimum and maximum values of component',i3,/
     1        ' are identical-plotting aborted.')
   70 format (' #',i3,3x,a17,3x,a17)
   80 format (2e20.6)
      end
      subroutine gtvars(at,comp,ax,q,nc,dfe,ierr,ro,istrn)
c     implicit double precision (a-h,o-z)                                    dp
      dimension sigshl(20),c(6,6),att(9),at(9),bt(6)
c
      if (nc.eq.63) then
      comp=at(1)
      return
      endif
      if (nc.gt.19) then
      comp=ax(nc-19)
      return
      endif
      if(nc.gt.25) go to 120
      if(nc.lt.8) comp=at(nc)
      if (nc.gt.16.and.nc.lt.20) then
      if (nc.eq.17) comp=log(dfe)
      if (nc.eq.18) comp=dfe
      if (nc.eq.19) comp=1./dfe-1.
      return
      endif
      if(nc.eq.8.and.istrn.eq.0) comp=-(at(1)+at(2)+at(3))/3.
      if(nc.eq.8.and.istrn.ne.0) comp= (at(1)+at(2)+at(3))/3.
      if(nc.lt.9 .or. nc.gt.16) return
      pr=-(at(1)+at(2)+at(3))/3.
      bt(1)=at(1)+pr
      bt(2)=at(2)+pr
      bt(3)=at(3)+pr
      bt(4)=at(4)
      bt(5)=at(5)
      bt(6)=at(6)
      aa=bt(4)**2+bt(5)**2+bt(6)**2-bt(1)*bt(2)-bt(2)*bt(3)-bt(1)*bt(3)
      aa=abs(aa)
      if(nc.gt.9) go to 10
      if (istrn.eq.0) comp=sqrt(3.*abs(aa))
      if (istrn.ne.0) comp=sqrt(4.*abs(aa)/3.)
      return
   10 bb=bt(1)*bt(5)**2+bt(2)*bt(6)**2+bt(3)*bt(4)**2-bt(1)*bt(2)*bt(3)
     1  -2.*bt(4)*bt(5)*bt(6)
      if(abs(aa).gt.1.e-20) go to 20
      s1=0.
      s2=0.
      s3=0.
      go to 30
   20 cc=-sqrt(27./aa)*bb*0.5/aa
      if (cc .gt. 1.) cc=1.
      if (cc .lt. -1.) cc=-1.
      angp=acos(cc)/3.
      dd=2.*sqrt(aa/3.)
      s1=dd*cos(angp)
      s2=dd*cos(angp+4.188790407)
      s3=dd*cos(angp+2.094395303)
c
   30 if(nc.eq.10) comp=s1
      if(nc.eq.11) comp=s2
      if(nc.eq.12) comp=s3
      if(nc.eq.13) comp=(s1-s3)/2.
      if(nc.eq.14) comp=s1-pr
      if(nc.eq.15) comp=s2-pr
      if(nc.eq.16) comp=s3-pr
      return
c
c     get shell variables
c
120   thick=at(9)
      if (nc.gt.42) then
      if (nc.lt.57) then
      comp=at(9+nc-42)
      return
      else
      comp=.5*(at(11+nc-56)+at(11+nc-50))
      return
      endif
      endif
      do 130 i=1,8
130   sigshl(i)=at(i)
      thick2=thick*thick/6.
      ii=9
      do 140 i=1,3
      aa=sigshl(i+5)/thick
      bb=sigshl(i)/thick2
      if(i.eq.1) bb=-bb
      sigshl(ii)=aa-bb
      sigshl(ii+1)=aa+bb
      ii=ii+2
140   continue
      ii=9
      sigshl(17)=0.0
      do 150 i=1,2
      sx=sigshl(ii  )
      sy=sigshl(ii+2)
      sxy=sigshl(ii+4)
      sigef=sqrt(sx*sx-sx*sy+sy*sy+3.*sxy*sxy)
      if(sigef.gt.sigshl(17)) sigshl(17)=sigef
      sigshl(ii+6)=sigef
      ii=ii+1
150   continue
      comp=sigshl(nc-25)
c
      return
      end
      subroutine getthl (icmtyp,nc,textsg)
c     implicit double precision (a-h,o-z)                                    dp
c*******************************************************************
c
c     labels for time history plots
c
c*******************************************************************
      character*16 lbl,lbl1,lbl2
      dimension lbl(3)
      character*(*) textsg
      data lbl /' stress    ',' strainrate',' strain    '/
c
      nc19=nc-19
      if (nc.eq.1) write (unit=textsg,fmt=10) lbl(icmtyp)
      if (nc.eq.2) write (unit=textsg,fmt=20) lbl(icmtyp)
      if (nc.eq.3) write (unit=textsg,fmt=30) lbl(icmtyp)
      if (nc.eq.4) write (unit=textsg,fmt=40) lbl(icmtyp)
      if (nc.eq.5) write (unit=textsg,fmt=50) lbl(icmtyp)
      if (nc.eq.6) write (unit=textsg,fmt=60) lbl(icmtyp)
      if (nc.eq.7) write (unit=textsg,fmt=70)
      if (nc.eq.9) write (unit=textsg,fmt=90) lbl(icmtyp)
      if (nc.eq.10) write (unit=textsg,fmt=100) lbl(icmtyp)
      if (nc.eq.11) write (unit=textsg,fmt=110) lbl(icmtyp)
      if (nc.eq.12) write (unit=textsg,fmt=120) lbl(icmtyp)
      if (nc.eq.13) write (unit=textsg,fmt=130) lbl(icmtyp)
      if (nc.eq.14) write (unit=textsg,fmt=140) lbl(icmtyp)
      if (nc.eq.15) write (unit=textsg,fmt=150) lbl(icmtyp)
      if (nc.eq.16) write (unit=textsg,fmt=160) lbl(icmtyp)
      if (nc.eq.17) write (unit=textsg,fmt=170)
      if (nc.eq.18) write (unit=textsg,fmt=180)
      if (nc.eq.19) write (unit=textsg,fmt=190)
      if (nc.eq.8.and.icmtyp-1.eq.0) write (unit=textsg,fmt=80)
      if (nc.eq.8.and.icmtyp-1.ne.0) write (unit=textsg,fmt=210)
      if (nc.gt.19) write (unit=textsg,fmt=360) nc19
      return
c
   10 format (' x-',a11)
   20 format (' y-',a11)
   30 format (' z-',a11)
   40 format (' xy-shear',a11)
   50 format (' yz-shear',a11)
   60 format (' zx-shear',a11)
   70 format (' effective plastic strain')
   80 format (' pressure')
   90 format (' effective',a11)
  100 format (' 1st principal deviatoric',a11)
  110 format (' 2nd principal deviatoric',a11)
  120 format (' 3rd principal deviatoric',a11)
  130 format (' maximum shear',a11)
  140 format (' 1st principal',a11)
  150 format (' 2nd principal',a11)
  160 format (' 3rd principal',a11)
  170 format (' ln(v/v0)')
  180 format (' relative volume')
  190 format (' v0/v-1')
  210 format (' mean strain')
  360 format ('  history variable #',i2,'  ')
      end
      subroutine zfmnmx(np,x,f)
c     implicit double precision (a-h,o-z)                                    dp
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      dimension x(1),f(*)
      ymin=1.e20
      ymax=-ymin
      do 20 j=1,np
      if(xmin.gt.x(j)) go to 20
      if(xmax.lt.x(j)) go to 20
      ymin=  min(f(j),ymin)
      ymax=  max(f(j),ymax)
   20 continue
      if (abs(ymax-ymin).lt.1.e-14) ymax=ymin+1.0e-06
      return
      end
      subroutine drvhlp
c     implicit double precision (a-h,o-z)                                    dp
      common /charc/ ichar, txtsiz
      common /colvss/ ltvg,ltvb
      common /finges/ igol, fringe, xres, yres, numcol, icolr(6)
      dimension tx(5),ty(5)
      character*68 textsg
      data tx/0.,1.,1.,0.,0./,ty/0.,0.,1.,1.,0./
      data zero,one,fift2/0.0,1.0,52.0/
      data ifirst/0/
    1 call stwndw(zero,fift2,zero,fift2)
      call stvprt(zero,one,zero,one)
      call bgnplt                                                       diglb
      lower=1
      lhigh=1
c     if (ltvg.ne.0) then                                               cray1
c     call colro (4)                                                    cray1
c     call shader (tx,ty,4,xres,yres,16.,0)                             cray1
c     lower=2                                                           cray1
c     lhigh=3                                                           cray1
c     endif                                                             cray1
      ifs=1
      xc=10.0
      yc=50.0
      tsiz=50.0
      do 2 ichr=lower,lhigh
      ichar=ichr
      write(unit=textsg,fmt=280)
      if (ifs.eq.0) write (  *,330) textsg
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=10)
      xc=9.0
      tsiz=65.0
      if (ifs.eq.0) write (  *,330) textsg
      yc=48.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=20)
      if (ifs.eq.0) write (  *,330) textsg
      yc=46.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=30)
      if (ifs.eq.0) write (  *,330) textsg
      yc=45.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=40)
      if (ifs.eq.0) write (  *,330) textsg
      yc=44.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=50)
      if (ifs.eq.0) write (  *,330) textsg
      yc=43.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=60)
      if (ifs.eq.0) write (  *,330) textsg
      yc=42.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=61)
      if (ifs.eq.0) write (  *,330) textsg
      yc=41.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=70)
      if (ifs.eq.0) write (  *,330) textsg
      yc=38.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=80)
      if (ifs.eq.0) write (  *,330) textsg
      yc=36.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=90)
      if (ifs.eq.0) write (  *,330) textsg
      yc=35.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=100)
      if (ifs.eq.0) write (  *,330) textsg
      yc=34.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=110)
      if (ifs.eq.0) write (  *,330) textsg
      yc=33.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=111)
      yc=32.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=120)
      if (ifs.eq.0) write (  *,330) textsg
      yc=29.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=130)
      if (ifs.eq.0) write (  *,330) textsg
      yc=27.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=140)
      if (ifs.eq.0) write (  *,330) textsg
      yc=26.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=150)
      if (ifs.eq.0) write (  *,330) textsg
      yc=25.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=160)
      yc=23.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=170)
      if (ifs.eq.0) write (  *,330) textsg
      yc=22.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=180)
      if (ifs.eq.0) write (  *,330) textsg
      yc=20.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=190)
      if (ifs.eq.0) write (  *,330) textsg
      yc=18.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=200)
      yc=17.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=210)
      yc=15.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=220)
      if (ifs.eq.0) write (  *,330) textsg
      yc=14.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=290)
      if (ifs.eq.0) write (  *,330) textsg
      yc=11.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=300)
      if (ifs.eq.0) write (  *,330) textsg
      yc=10.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=310)
      if (ifs.eq.0) write (  *,330) textsg
      yc=9.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=230)
      yc=7.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=240)
      yc=5.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write(unit=textsg,fmt=250)
      yc=4.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      if (ifirst.eq.1) then
      write(unit=textsg,fmt=260)
      yc=2.0
      call plotxt  (xc,yc,textsg,62,tsiz,zero)
      write(unit=textsg,fmt=270)
      yc=1.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      endif
      ifs=1
    2 continue
      call plotpc
      if (ifirst.eq.0) then
      ifirst=1
      ichar =1
      return
      endif
      irplt=0
      call cmndrv(irplt)
      if (irplt.eq.1) go to 1
      ichar=1
      return
   10 format(' .driver plotting  commands:')
   20 format('                                                       ')
   30 format('     command for setting grid parameters               ')
   40 format('         ascl         oscl         aset         oset   ')
   50 format('         grid       nogrid         logo         head   ')
   60 format('                                                       ')
   61 format('                                                       ')
   70 format('     commands for changing tmds # and creating plotfile')
   80 format('           tv         fr80         give        plots   ')
   90 format('        class         rjet                             ')
  100 format('                                                       ')
  110 format('                                                       ')
  111 format('                                                       ')
  120 format('     commands for time histroy and cross plots         ')
  130 format('         time        cross        print         file   ')
  140 format('         rdlc     continue        ecomp         chgl   ')
  150 format('                                                       ')
  160 format('                                                       ')
  170 format('     commands to terminate                             ')
  180 format('         quit          end            t                ')
  190 format('                                                       ')
  200 format('                                                       ')
  210 format('                                                       ')
  220 format(' to display component numbers for plotting type:ecomp  ')
  230 format('                                                       ')
  240 format('                                                       ')
  250 format('                                                       ')
  260 format(' to obtain a discription of a command, enter the command n
     1ame')
  270 format(' *********  exit from help package by typing: return   ')
c 280 format('            d y n a 3 d   d r i v e r                  ') cray1
  280 format(' d y n a 3 d   d r i v e r                             ') vax75
  290 format('                                                       ')
  300 format('                                                       ')
  310 format('                                                       ')
  330 format (a)
      end
      subroutine lodset (npc,p,curves,npts,maxp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     move load curves to a local array
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      dimension npc(*),p(*),curves(2,maxp,*),npts(*)
c
      do 10 n=1,9
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      npts(n)=npoint
      call blkcpy (p(loc),curves(1,1,n),2*npoint)
 10   continue
c
      return
c
      end
      subroutine lodstc (fval,npc,curves,tt,maxp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute current value of load curves
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      dimension fval(*),npc(*),curves(2,maxp,*)
c
      do 10 n=1,9
      ierr=0
      xmag=1.0
      npoint=npc(n)
      call inter6(curves(1,1,n),tt,npoint,fvl,xmag,ierr)
      if(ierr.gt.0) then
      write(*,1000) tt,n
      write(13,1000) tt,n
      call adios(2)
      endif
      fval(n)=fvl
 10   continue
      return
 1000 format(///5x,'simulation time:  ',e14.4,
     1  5x,'  exceeds limits of load curve: ',i5)
      end
      block data cmlabl
c     implicit double precision (a-h,o-z)                                    dp
      common/cmlaba/mlloc(30),iesloc(30)
      character*72 mtls,eoslbs
      common/cmlabb/mtls(200),eoslbs(200)
c
c      material model 16
c
      data mtls(1)/'specific internal energy per unit reference volume'/
      data mtls(2)/'bulk viscosity                                    '/
      data mtls(3)/'current volume                                    '/
      data mtls(4)/'maximum principal stress                          '/
      data mtls(5)/'fracture state                                    '/
      data mtls(6)/'failure volume                                    '/
c
c     equation of state 18
c
      data eoslbs( 1)/'peak matrix pressure       '/
      data eoslbs( 2)/'matrix density             '/
      data eoslbs( 3)/'matrix pressure            '/
      data eoslbs( 4)/'matrix bulk modulus        '/
      data eoslbs( 5)/'matrix sound speed         '/
      data eoslbs( 6)/'peak solid pressure        '/
      data eoslbs( 7)/'solid density              '/
      data eoslbs( 8)/'solid pressure             '/
      data eoslbs( 9)/'solid bulk modulus         '/
      data eoslbs(10)/'peak water pressure        '/
      data eoslbs(11)/'water density              '/
      data eoslbs(12)/'water pressure             '/
      data eoslbs(13)/'water-filled porosity      '/
      data eoslbs(14)/'water bulk modulus         '/
      data eoslbs(15)/'effective pressure         '/
      data eoslbs(16)/'overburden pressure        '/
      data mlloc/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
     1           0,0,0,0,0,0,0,0,0,0,0,0,0,0/
      data iesloc/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     1           0,1,0,0,0,0,0,0,0,0,0,0,0,0/
      end
      subroutine cmndrv(irplt)
c     implicit double precision (a-h,o-z)                                    dp
      common/helpme/ihelp,nu
      character*4 icmd
      character*12 icomd
      data icomd/'.command ? :'/
   10 if (ihelp.eq.0.and.irplt.eq.999) return
      if (irplt.eq.998) go to 15
      call getsym (12,icomd,icmd,4)
      if (ihelp.gt.0.and.icmd.eq.'help') call getsym(12,icomd,icmd,4)
      ihelp=ihelp-1
      ihelp=max0(ihelp,0)
      if (icmd.eq.'ecom') then
      call show
      go to 10
      endif
      if (icmd.eq.'tens') then
      go to 10
      endif
      if (icmd.eq.'help') then
      irplt=1
      return
      endif
      if (icmd.eq.'retu') then
      return
      endif
   15 if (irplt.eq.998.or.icmd.eq.'tv'  ) then
      write ( *,20)
   20 format ('tv  n      use tmds where n is the monitor number',/,
     1        'tv -n1 n2 n3',/,
     2        '           use color tmds with monitor numbers n1,',/,
     3        '           n2, and n3 for the red, green, and blue',/,
     4        '           channels, respectively.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'t'
     1                .or.icmd.eq.'quit'.or.icmd.eq.'end') then
      write ( *,40)
   40 format(' t,end,quit terminate without remap'/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'plot') then
      write ( *,320)
  320 format(' plots box ann',/
     1       '             create a plotfile for box number ann that',/,
     2       '             contains a record of the tmds display',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'grid') then
      write ( *,330)
  330 format(' grid       tmds or terminal displays will be overlayed',/
     1       '            by a grid of orthogonal lines.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'nogr') then
      write ( *,340)
  340 format(' nogrid     tmds or terminal displays will not be',/,
     1       '            overlayed by a grid of orthogonal lines.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'fram') then
      write ( *,350)
  350 format(' frame      reference grid frames plots (default).',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'nofr') then
      write ( *,360)
  360 format(' noframe    no reference grid is plotted',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'file') then
      write ( *,370)
  370 format(' file name  change pampers filename to name',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'head') then
      write ( *,405)
  405 format(' head       enter new plot title (28 character max)',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'ascl') then
      write ( *,410)
  410 format(' ascl f     scale all abscissa data by f.  the',/,
     1       '            default value is f=1.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'oscl') then
      write ( *,420)
  420 format(' oscl f     scale all ordinate data by f.  the',/,
     1       '            default value is f=1.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'fr80') then
      write ( *,530)
  530 format(' fr80 filmtype',/,
     1       '            select fr80 camera.  fr80 default filmtype',/,
     2       '            if fiche48.  other options include: p16mm',/,
     3       '            fich48d, fiche24, fich24d, 35mm, color35,',/,
     4       '            dico35, color16, dico16, cslide35, hard-',/,
     5       '            copy, report, vugraph, and vugraf11. this',/,
     6       '            command if used must precede the plots',/,
     7       '            command.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'clas') then
      write ( *,540)
  540 format(' class lev. reset classification level of fr80 output',/,
     1       '            default from unclass to: proglev, pard,',/,
     2       '            adp, confidnt, srd, or system.  this',/,
     3       '            command if used must precede the plots',/,
     4       '            command.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'give') then
      write ( *,550)
  550 format(' give       give the fr80 file to the system for ',/,
     1       '            plotting upon termination.  this command',/,
     2       '            if used must precede the plots command.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'rjet') then
      write ( *,560)
  560 format(' rjet n     send a copy of the fr80 file to rjet n',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'chgl') then
      write ( *,570)
  570 format(' chgl n     change label for component n.',/,
     1       '            dyna3d prompts for the new label',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'aset') then
      write ( *,590)
  590 format(' aset amin amax',/,
     1       '            set min and max values on abscissa to',/,
     2       '            amin and amax, respectively.  if amin=',/,
     3       '            amax=0 dyna3d determines the min and',/,
     4       '            max values.  ',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'oset') then
      write ( *,600)
  600 format(' aset omin omax',/,
     1       '            set min and max values on ordinate to',/,
     2       '            omin and omax, respectively.  if omin=',/,
     3       '            =omax=0 dyna3d determines the min and',/,
     4       '            max values.  ',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'rdlc') then
      write ( *,770)
  770 format(' rdlc m n r1 z1 ... rn zn',/,
     1       '            redefine load curve m using n',/,
     2       '            coordinate pairs (r1,z1), (r2,z2), ',/,
     3       '            ... (rn,zn).  ',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'time') then
      write ( *,1070)
 1070 format(' time  c ',/,
     1       '            plot component c versus time',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'cros') then
      write ( *,1140)
 1140 format(' cross c1 c2',/,
     1       '            plot component c1 versus c2 ',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'cont') then
      write ( *,1150)
 1150 format(' continue',/,
     1       '            reanalyze material model.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'prin') then
      write ( *,1160)
 1160 format(' print      print plotted time history data in file',/,
     1       '            "pamper".  only data plotted after this',/,
     2       '            command is printed.   the file name can be ',/
     3       '            changed with the "file" command.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'logo') then
      write ( *,1200)
 1200 format(' logo       put llnl logo on all plots (def.).',/
     1       '            retyping this commands removes the logo.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998.or.icmd.eq.'ff'  ) then
      write ( *,1210)
 1210 format(' ff         encircle picture with reference grid with ',/
     1       '            tickmarks. default grid is plotted along ',/
     2       '            bottom and left side of picture.',/)
      if (irplt.ne.998) go to 10
      endif
      if (irplt.eq.998) return
      write (  *,2000) icmd
 2000 format (//,' *** unrecognized command:',a4,' ***',/)
      go to 10
      end
      subroutine slddrv (harnew,harold,ncycle,fval)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
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
      common/nwixa/nwcon
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      logical idrvon
      common/driver/ idrvon,idrvdm(10)
      common /   / a(1)
      dimension harnew(*),harold(*),fval(*)
c
      itype=idrvdm(4)
c
      if (itype.ne.2) then
      call soldrv(a(n1),a(n2),a(n3),a(lc0),a(lc9),a(lc10),a(lc11),a(n26)
     1 ,a(n27),a(n28),a(lc15),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),
     2 a(n4e),a(lc1h),a(ntmp0+1),a(n19),numelh,nwcon,a(n25+7),nmmat,
     3 a(n8),a(n9),a(nh01),a(nh02),a(nh03),a(nh14),harnew,harold,ncycle,
     4 fval,nlcur,a(nh11))
c
      else
      call shldrv(a(n1),a(n2),a(n3),a(lc0),a(lc9),a(lc10),a(lc11),a(n26)
     1 ,a(n27),a(n28),a(lc15),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),
     2 a(n4e),a(lc1h),a(ntmp0+1),a(n19),numelh,nwcon,a(n25+7),nmmat,
     3 a(n8),a(n9),a(nh01),a(nh02),a(nh03),a(nh14),harnew,harold,ncycle,
     4 fval,nlcur,a(nh11))
      endif
c
      return
      end
      subroutine soldrv (mtype,ro,cm,u,v,fv,x,nsubgv,mtnum,nfegp,auxvec,
     1 ieost,eosp,ihgq,hgq,iqtype,bkqs,ix,tnew,fvalue,numelh,nwcon,ener,
     2 nmmat,npc,pld,hgfrcs,xies,bqs,lochvh,harnew,harold,ncycle,
     3 fval,nlcur,nnch)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/aux18/dd(128),dfe(128)
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/energy/xinen
      common /   / a(1)
      common/aux2/
     & d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension mtype(*),ro(*),cm(48,*),u(*),v(*),fv(*),x(*),nsubgv(*),
     1 mtnum(*),nfegp(*),auxvec(*),ieost(*),eosp(*),ihgq(*),hgq(*),
     2 iqtype(*),bkqs(3,*),ix(nwcon,*),fvalue(*),tnew(*),ener(*),npc(*),
     3 pld(*),hgfrcs(12,*),xies(*),bqs(*),lochvh(*),harnew(*),harold(*),
     4 fval(30,*),nnch(*)
      lczc=0
      lcz=0
      ic =0
      nn=1
      noco=nn
      nnm1=128*(nn-1)
      nnm2=nnm1+1
      nmel=1
      call unpki (ix(1,nnm2),nwcon,nmel)
      nsubg=nsubgv(nn)
      n=1
      ic =ic +1
      lcn=lcz+n
      nnc=nnch(ic)
      mxe=mtnum(lcn)
      lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvh(nnm1+lft)
      mte=mtype(mxe)
      nes=ieost(mxe)
      ibq=iqtype(mxe)
      ihg=ihgq(mxe)
      qhg=hgq(mxe)
      b12=bkqs(1,mxe)
      b2 =bkqs(2,mxe)
      nmtcon=7+nconst(mte)
      if (nes.ne.0) nmtcon=nmtcon+ncneos(nes)
      rhoa(lft)=ro(mxe)
      call strndr (x,u(nnm2),v,fval,nlcur)
      call felen
      if (mte.gt.10) go to 120
      go to (20,30,40,50,60,70,80,90,100,110), mte
   20 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm1 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   30 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm2 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   40 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call rstrn
      call f3dm3 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   50 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm4 (cm,bqs(nnm2),tnew,fvalue)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   60 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm5 (cm,bqs(nnm2),0)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   70 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm6 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   80 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm7 (cm,bqs(nnm2),ihg)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   90 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm8 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call hieupd
      call eqos (fvalue,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  100 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm9 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call hieupd
      call eqos (fvalue,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  110 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm10 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
c     if(idmp.eq.1.and.kp.lt.0) call psqrt(tta(nnml+1),0)
      call hieupd
      call eqos (fvalue,ener,mte,nes)
c     if(idmp.eq.1.and.kp.lt.0) call psqrt(tta(nnml+1),1)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  120 mte10=mte-10
      if (mte.eq.31) then
      call hvpac1 (nmtcon,auxvec(lav))
      call f3dm31(cm)
      call bulkq (ibq,a(nh17))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
      endif
      if (mte.eq.32) then
      call hvpac1 (nmtcon,auxvec(lav))
      call f3dm32(cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
      endif
      if (mte.eq.33) then
      call hvpac1 (nmtcon,auxvec(lav))
      call f3dm33(cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
      endif
      go to (130,140,150,160,170,180,190,192,194,207,196,197,198,199,
     1 200,204,205),mte10
  130 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm11 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call hieupd
      call eqos (fvalue,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  140 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm12 (cm,bqs(nnm2),0)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  150 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm13 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  160 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm14 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  170 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm15 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call hieupd
      call eqos (fvalue,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  180 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm16 (cm,npc,pld,eosp,mte,nes,ibq,ener,fvalue)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  190 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm17 (cm,x,nnm1)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call hieupd
      call eqos (fvalue,ener,mte,nes)
      call fzero
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  192 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm18 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  194 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm19 (cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  196 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm21(cm,bqs(nnm2),tnew,fvalue)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  197 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm22(cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  198 call hvpac1 (nmtcon,auxvec(lav))
      lthrpr=nint(cm(48,mxe))
      call f3dm23(cm,bqs(nnm2),tnew,fvalue,a(lthrpr))
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  199 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm24(cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  200 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm25(cm)
      call bulkq (ibq,a(nh17))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  204 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm26(cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  205 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm27(cm,bqs(nnm2),ihg)
      call bulkq (ibq,a(nh17))
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
  207 x1nen=x1nen+ener(1)
      harnew(1) =auxvec(1)
      harnew(2) =auxvec(2)
      harnew(3) =auxvec(3)
      harnew(4) =auxvec(4)
      harnew(5) =auxvec(5)
      harnew(6) =auxvec(6)
      harnew(7) =auxvec(7)
      harnew(8) =d1(1)
      harnew(9) =d2(1)
      harnew(10)=d3(1)
      harnew(11)=d4(1)
      harnew(12)=d5(1)
      harnew(13)=d6(1)
      harnew(14)=bqs(1)
      if (ncycle.gt.1) then
      harnew(15)=harold(15)+dt1*d1(1)
      harnew(16)=harold(16)+dt1*d2(1)
      harnew(17)=harold(17)+dt1*d3(1)
      harnew(18)=harold(18)+dt1*d4(1)
      harnew(19)=harold(19)+dt1*d5(1)
      harnew(20)=harold(20)+dt1*d6(1)
      endif
      harnew(21)=dfe(1)
      if (nmtcon.gt.7) then
      call blkcpy (auxvec(8),harnew(22),nmtcon-7)
      endif
      return
      end
      subroutine shldrv (mtype,ro,cm,u,v,fv,x,nsubgv,mtnum,nfegp,auxvec,
     1 ieost,eosp,ihgq,hgq,iqtype,bkqs,ix,tnew,fvalue,numelh,nwcon,ener,
     2 nmmat,npc,pld,hgfrcs,xies,bqs,lochvh,harnew,harold,ncycle,
     3 fval,nlcur,nnch)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/aux18/dd(128),dfe(128)
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/energy/xinen
      common /   / a(1)
      common/aux2/
     & d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension mtype(*),ro(*),cm(48,*),u(*),v(*),fv(*),x(*),nsubgv(*),
     1 mtnum(*),nfegp(*),auxvec(*),ieost(*),eosp(*),ihgq(*),hgq(*),
     2 iqtype(*),bkqs(3,*),ix(nwcon,*),fvalue(*),tnew(*),ener(*),npc(*),
     3 pld(*),hgfrcs(12,*),xies(*),bqs(*),lochvh(*),harnew(*),harold(*),
     4 fval(30,*),nnch(*)
      lczc=0
      lcz=0
      ic =0
      nn=1
      noco=nn
      nnm1=128*(nn-1)
      nnm2=nnm1+1
      nmel=1
      call unpki (ix(1,nnm2),nwcon,nmel)
      nsubg=nsubgv(nn)
      n=1
      ic =ic +1
      lcn=lcz+n
      nnc=nnch(ic)
      mxe=mtnum(lcn)
      lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvh(nnm1+lft)
      mte=mtype(mxe)
      nes=ieost(mxe)
      ibq=iqtype(mxe)
      ihg=ihgq(mxe)
      qhg=hgq(mxe)
      b12=bkqs(1,mxe)
      b2 =bkqs(2,mxe)
      nmtcon=7+nconst(mte)
      if (nes.ne.0) nmtcon=nmtcon+ncneos(nes)
      rhoa(lft)=ro(mxe)
c
c.... compute strains and rotations for shell element materials
c
      call stn2dr (x,u(nnm2),v,fval,nlcur)
c
      nip=1
      ipt=1
c
c.... strains are now available, call shell material model
c
      call shlmat (nmtcon,auxvec,cm,lav,mte,nip,ipt,mxe)
c
c.... stresses and history now updated; store data in driver arrays
c
      x1nen=x1nen+ener(1)
      harnew(1) =auxvec(1)
      harnew(2) =auxvec(2)
      harnew(3) =auxvec(3)
      harnew(4) =auxvec(4)
      harnew(5) =auxvec(5)
      harnew(6) =auxvec(6)
      harnew(7) =auxvec(7)
      harnew(8)=d1(1)/dt1
      harnew(9)=d2(1)/dt1
      harnew(10)=harold(10)+d3(1)
      harnew(11)=d4(1)/dt1
      harnew(12)=d5(1)/dt1
      harnew(13)=d6(1)/dt1
      harnew(14)=bqs(1)
      if (ncycle.gt.1) then
      harnew(15)=harold(15)+d1(1)
      harnew(16)=harold(16)+d2(1)
      harnew(17)=harold(17)+d3(1)
      harnew(18)=harold(18)+d4(1)
      harnew(19)=harold(19)+d5(1)
      harnew(20)=harold(20)+d6(1)
      endif
      harnew(21)=dfe(1)
      if (nmtcon.gt.7) then
      call blkcpy (auxvec(8),harnew(22),nmtcon-7)
      endif
      return
      end
      subroutine shlmat (nmtcon,auxvec,cm,lav,mte,nip,ipt,mxe)
c
c.... routine to call matl models for shell model driver
c
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
      common/tsarry/tslimt,tsarry(128)
      common /  / a(1)
      dimension cm(48,*),auxvec(*)
      lavloc=(ipt-1)*nmtcon+lav
      capa=0.
      do 10 i=1,128
      tsarry(i)=1.
   10 continue
      if (mte.eq.1) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl1s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.2)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl2s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
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
      return
      elseif (mte.eq.4)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl4s (cm,a(ntmp0+1),a(n19))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.12)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl12s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.15)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl15s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
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
      return
      elseif (mte.eq.21) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl21s(cm,capa,a(ntmp0+1),a(n19))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.22) then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl22s(cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.23) then
      lthrpr=nint(cm(48,mxe))
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl23s(cm,capa,a(ntmp0+1),a(n19),a(lthrpr),
     1 a(n8),a(n9),a(lc11))
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
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
      return
      elseif (mte.eq.28)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl28s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.30)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl30s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.33)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl33s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
      elseif (mte.eq.34)  then
      call tbsc1s (nmtcon,auxvec(lavloc),nip*nmtcon)
      call shl34s (cm,capa)
      call tbsc2s (nmtcon,auxvec(lavloc),lav,nip*nmtcon,nip,ipt)
      return
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
      return
      else
      write( *,1000)
      write(13,1000)
      call adios(2)
 1000 format(//5x,'*** illegal material  for shell model driver ***',
     1        /5x,'     execution aborted ')
      endif
      end
      subroutine skale (a,f,la,amin,amax)
c     implicit double precision (a-h,o-z)                                    dp
      dimension a(1)
      do 10 l=1,la
   10 a(l)=a(l)*f
      amin=f*amin
      amax=f*amax
      if (amax.lt.amin) then
      save=amax
      amax=amin
      amin=save
      endif
      return
      end
      subroutine strndr(x,volo,v,fval,nlcur)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux9/vlrho(128),voln(128)
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
      common/aux18/dd(128),dfe(128)
      common/aux32/a17(128),a28(128),dett(128),
     1 aj1(128),aj2(128),aj3(128),dxy(128),
     2 dyx(128),dyz(128),dzy(128),dzx(128),
     3 dxz(128),vx17(128),vx28(128),vx35(128),
     4 vx46(128),vy17(128),vy28(128),
     5 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
c
      dimension x(3,1),v(3,1),dux(1),duy(1),duz(1),volo(1),
     1 fval(30,*)
      equivalence (dux,vx1),(duy,vy1),(duz,vz1)
      data dfci/1.0/
      data iflag/0/
c
      do 10 i=lft,llt
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
   10 vz8(i)=v(3,ix8(i))
c
c     calculate velocity strndr rates, spins, and deformation gradients
c
      call prtal
c
      do 20 i=lft,llt
      vx17(i)=vx1(i)-vx7(i)
      vx28(i)=vx2(i)-vx8(i)
      vx35(i)=vx3(i)-vx5(i)
      vx46(i)=vx4(i)-vx6(i)
      vy17(i)=vy1(i)-vy7(i)
      vy28(i)=vy2(i)-vy8(i)
      vy35(i)=vy3(i)-vy5(i)
      vy46(i)=vy4(i)-vy6(i)
      vz17(i)=vz1(i)-vz7(i)
      vz28(i)=vz2(i)-vz8(i)
      vz35(i)=vz3(i)-vz5(i)
   20 vz46(i)=vz4(i)-vz6(i)
      do 30 i=lft,llt
      dxx(i)=fval(1,3)
      dyy(i)=fval(2,3)
      dzz(i)=fval(3,3)
      dxy(i)=fval(4,3)
      dyx(i)=fval(5,3)
      dxz(i)=fval(6,3)
      dzx(i)=fval(7,3)
      dyz(i)=fval(8,3)
      dzy(i)=fval(9,3)
      dfc0=dfci
      dfci=exp(dt1*(dxx(i)+dyy(i)+dzz(i)))*dfci
      dfmi=.5*(dfci+dfc0)
   30 continue
c
      dt1d2=.5*dt1
      crho =.0625*rhoa(lft)
c
      do 50 i=lft,llt
      dd(i)=-dxx(i)-dyy(i)-dzz(i)
      d1(i)=dxy(i)+dyx(i)
      d2(i)=dyz(i)+dzy(i)
      d3(i)=dxz(i)+dzx(i)
      xm(i)=1./volo(i)
      dfe(i)=dfci
      voln(i)=dfci
      vlrho(i)=crho*volo(i)
      wzzdt(i)=dt1d2*(dyx(i)-dxy(i))
      wyydt(i)=dt1d2*(dxz(i)-dzx(i))
   50 wxxdt(i)=dt1d2*(dzy(i)-dyz(i))
c
      return
c
      end
      subroutine stn2dr(x,volo,v,fval,nlcur)
c
c.... shell material model driver -- strain computations
c
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/
     & dxx(128),dyy(128),dzz(128),d1(128),d2(128),d3(128),
     & wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux9/vlrho(128),voln(128)
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
      common/aux18/dd(128),dfe(128)
      common/aux32/a17(128),a28(128),dett(128),
     1 aj1(128),aj2(128),aj3(128),dxy(128),
     2 dyx(128),dyz(128),dzy(128),dzx(128),
     3 dxz(128),vx17(128),vx28(128),vx35(128),
     4 vx46(128),vy17(128),vy28(128),
     5 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
c
      dimension x(3,*),v(3,*),dux(1),duy(1),duz(1),volo(*),
     1 fval(30,*)
      equivalence (dux,vx1),(duy,vy1),(duz,vz1)
      data dfci/1.0/
      data iflag/0/
c
c.... compute strains
c
      do 30 i=lft,llt
c
c.... normal strain increments
c
      dxx(i)=fval(1,3)*dt1
      dyy(i)=fval(2,3)*dt1
c     dzz(i)=fval(3,3)
      dzz(i)=0.
c
c.... off-diag spatial velocity gradient
c
      dxy(i)=fval(4,3)
      dyx(i)=fval(5,3)
      dxz(i)=fval(6,3)
      dzx(i)=fval(7,3)
      dyz(i)=fval(8,3)
      dzy(i)=fval(9,3)
c
      dfc0=dfci
      dfci=exp((dxx(i)+dyy(i)+dzz(i)))*dfci
      dfmi=.5*(dfci+dfc0)
   30 continue
c
      dt1d2=.5*dt1
      crho =.0625*rhoa(lft)
c
      do 50 i=lft,llt
      dd(i)=-dxx(i)-dyy(i)-dzz(i)
      d1(i)=(dxy(i)+dyx(i))*dt1
      d2(i)=(dyz(i)+dzy(i))*dt1
      d3(i)=(dxz(i)+dzx(i))*dt1
      xm(i)=1./volo(i)
      dfe(i)=dfci
      voln(i)=dfci
      vlrho(i)=crho*volo(i)
c
c... compute spins
c
      wzzdt(i)=dt1d2*(dyx(i)-dxy(i))
      wyydt(i)=dt1d2*(dxz(i)-dzx(i))
   50 wxxdt(i)=dt1d2*(dzy(i)-dyz(i))
c
      return
c
      end
      subroutine filter (f,ff,np,nf,nspan)
c     implicit double precision (a-h,o-z)                                    dp
      dimension f(np,nf), ff(*)
c
      do 40 m=1,nf
      do 20 n=1,np
      nu=nspan
      if (n-nu.lt.1) nu=n-1
      if (n+nu.gt.np) nu=np-n
      j1=n-nu
      j2=n+nu
      div=2*nu+1
      sum=0.
      do 10 j=j1,j2
   10 sum=sum+f(j,m)
   20 ff(n)=sum/div
      do 30 n=1,np
   30 f(n,m)=ff(n)
   40 continue
      return
      end
      subroutine fminmx(np,x,f,nf)
c     implicit double precision (a-h,o-z)                                    dp
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      dimension x(1),f(np,1)
      ymin=1.e20
      ymax=-ymin
      do 20 j=1,np
      if(xmin.gt.x(j)) go to 20
      if(xmax.lt.x(j)) go to 20
      do 10 i=1,nf
      ymin=  min(f(j,i),ymin)
      ymax=  max(f(j,i),ymax)
   10 continue
   20 continue
      if (abs(ymax-ymin).lt.1.e-14) ymax=ymin+1.0e-06
      return
      end
      subroutine aminmx (a,n)
c     implicit double precision (a-h,o-z)                                    dp
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      dimension a(1)
      ymin=a(1)
      ymax=ymin
      do 10 i=2,n
      ymin=  min(ymin,a(i))
   10 ymax=  max(ymax,a(i))
      if (abs(ymin-ymax).lt.1.e-14) ymax=ymin+1.e-5
      return
      end
      subroutine plotln(xb,yb,n)
c     implicit double precision (a-h,o-z)                                    dp
      common/rang/xmin,xmax,ymin,ymax,tvxmin,tvxmax,tvymin,tvymax
      common/devs/ifr80,itmds,xx1,xx2,yy1,yy2
      common/xlnn/xc1,xc2,yc1,yc2
      common/ercd/ic
      dimension xb(1),yb(1)
      nl=n-1
      do 90 m=1,nl
      xx1=xb(m)
      xx2=xb(m+1)
      yy1=yb(m)
      yy2=yb(m+1)
      am1=  min(xx1-xmin,yy1-ymin,xmax-xx1,ymax-yy1)
      am2=  min(xx2-xmin,yy2-ymin,xmax-xx2,ymax-yy2)
      if(  min(am1,am2)) 20,10,10
   10 call plines(xx1,yy1,2)
      ic=1
      go to 90
   20 ic=0
      am1=  max(xx1,xx2)-xmin
      am2=xmax-min(xx1,xx2)
      am3=  max(yy1,yy2)-ymin
      am4=ymax-min(yy1,yy2)
      if(  min(am1,am2,am3,am4).lt.0.0) go to 90
      call tatb(xmin,xmax,xx1,xx2,ymin,ymin,yy1,yy2,ic,xc1,yc1)
      call tatb(xmax,xmax,xx1,xx2,ymin,ymax,yy1,yy2,ic,xc1,yc1)
      if(ic.eq.2) go to 80
      call tatb(xmax,xmin,xx1,xx2,ymax,ymax,yy1,yy2,ic,xc1,yc1)
      if(ic.eq.2) go to 80
      call tatb(xmin,xmin,xx1,xx2,ymax,ymin,yy1,yy2,ic,xc1,yc1)
      if(ic.eq.2) go to 80
      if(ic.eq.0) go to 90
      if(xx1.lt.xmin.or.xx1.gt.xmax) go to 40
      if(yy1.lt.ymin.or.yy1.gt.ymax) go to 40
      xc2=xx1
      yc2=yy1
      go to 80
   40 xc2=xx2
      yc2=yy2
   80 call plines(xc1,yc1,2)
   90 continue
      return
      end
      subroutine tatb(x1,x2,x3,x4,y1,y2,y3,y4,ic,xx,yy)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xx(1),yy(1)
      x31=x3-x1
      y31=y3-y1
      x43=x4-x3
      y43=y4-y3
      x21=x2-x1
      y21=y2-y1
      denm=y21*x43-x21*y43
      if(abs(denm).lt.1.e-20) return
      ta=(y31*x43-x31*y43)/denm
      tb=(x21*y31-y21*x31)/denm
      if(  min(ta,tb).lt.0.) return
      if(  max(ta,tb).gt.1.) return
      ic=ic+1
      xx(ic)=x21*ta+x1
      yy(ic)=y21*ta+y1
      return
      end
      subroutine getsym(nchar,msge,cmdr,lword)
c     implicit double precision (a-h,o-z)                                    dp
      character*(*) cmdr,msge
      call parse(msge,nchar,cmdr,data,1,lword)
      return
      end
      subroutine getnum(nchar,msge,data,idata)
c     implicit double precision (a-h,o-z)                                    dp
      character*4 cmd,msge
      call parse(msge,nchar,cmd,data,3,lword)
      idata=data
      return
      end
      subroutine parse(msge,nchar,cmd,dat,ityp,lword)
c     implicit double precision (a-h,o-z)                                    dp
      character*(*) cmd,msge
      common /iaio/ iosave, iunit, itty, ncolum
      character*1 cbuf
      common /ibio/ cbuf(82)
      character*4 rbuf(20)
      character*81 temp
      character*1  iblank,ihat
      data iblank/' '/,ihat/'^'/
    5 continue
      if(ncolum.gt.80)call newlin(msge,nchar)
c
c     find first character of item
c
   10 if(cbuf(ncolum).ne.' ')go to 20
      ncolum=ncolum+1
      if(ncolum.gt.80)call newlin(msge,nchar)
      go to 10
c
c     find last character of item
c
   20 continue
      ncol1=ncolum
   25 continue
      if(cbuf(ncolum).eq.' ')go to 30
      ncolum=ncolum+1
      if(ncolum.gt.80)go to 30
      go to 25
   30 continue
      ncol2=ncolum-1
c
c     translate the item
c
      lchar=ncol2-ncol1+1
      go to (40,50,50),ityp
   40 lchar=min0(lchar,lword)
      ncol2=ncol1+lchar-1
      write(unit=cmd,fmt=1010)(cbuf(i),i=ncol1,ncol2)
      return
   50 continue
      do 55 i=1,20
   55 rbuf(i)=' '
      do 60 i=ncol1,ncol2
      i1=20+i-ncol2
      rbuf(i1)=cbuf(i)
   60 continue
      write(unit=temp,fmt=1010)(rbuf(i),i=1,20)
      if(ityp.eq.3)go to 70
      read (unit=temp,fmt=1020,err=400) dat
      return
   70 continue
      read (unit=temp,fmt=1030,err=400) dat
      return
  400 continue
      if(ityp.eq.2)write(  *,2010)
      if(ityp.eq.3)write(  *,2020)
c     iunit=itty
      ncolum=81
      write(  *,1010)(cbuf(i),i=1,80)
      ic1=ncol1-1
      if(ic1.le.0)write(  *,1010)ihat
      if(ic1.ge.1)write(  *,1010)(iblank,i=1,ic1),ihat
      go to 5
 1010 format(80a1)
 1020 format(i20)
 1030 format(e20.0)
 2010 format(' *** integer decode error ***')
 2020 format(' *** floating point decode error ***')
      end
      subroutine newlin(msge,nchar)
c     implicit double precision (a-h,o-z)                                    dp
      common /iaio/ iosave, iunit, itty, ncolum
      character*1 cbuf
      character*8 ftm                                                   unix
c     character*11 ftm                                                  vms
      character*80 msge
      common /ibio/ cbuf(82)
      common/helpme/ihelp,nu
      dimension temp(80)
      dimension msge(1)
c     data ipmtm /  ''/                                                cray1
   10 continue
      if(iunit.ne.itty)go to 20
      read (unit=msge,fmt=1010)(temp(i),i=1,nchar)
c     write(*,1020)(temp(i),i=1,nchar),ipmtm                            cray1
      write(ftm,1021) nchar                                             vax75
      write(*,ftm)(temp(i),i=1,nchar)                                   vax75
c     read(*,1010)(cbuf(i),i=1,80)                                      cray1
      read( *,1010)(cbuf(i),i=1,80)                                     vax75
      do 15 i=1,80                                                      vax75
   15 call lwrcas(  cbuf(i),  cbuf(i))                                  vax75
      do 16 i=5,80
      if (cbuf(i).eq.'/') then
      if (cbuf(i-4).eq.'h'.and.cbuf(i-3).eq.'e'.and.cbuf(i-2).eq.'l'
     1 .and.cbuf(i-1).eq.'p') ihelp=ihelp+1
      cbuf(i)=' '
      endif
   16 continue
      ncolum=1
      go to 40
   20 continue
c     read(iunit,1010)(cbuf(i),i=1,80)                                  cray1
c     if(iostatus(iunit,ier).ne.0)go to 30                              cray1
      read(iunit,1010,end=30)(cbuf(i),i=1,80)                           vax75
      do 25 i=1,80                                                      vax75
   25 call lwrcas(  cbuf(i),  cbuf(i))                                  vax75
      ncolum=1
      go to 40
   30 continue
      iunit=itty
c     write(iunit,1020) ipmtm                                           cray1
      go to 10
   40 if (iosave.eq.12) write (iosave,1010) (cbuf(l),l=1,80)
c     if(iosave.eq.12) call  empty(iosave)                              cray1
      return
 1010 format(80a1)
c1020 format(80a1)                                                      cray1
 1021 format('(',i2,'a1,$)')                                            unix
c1021 format('($,1x,',i2,'a1)')                                         vms
      end
      subroutine purge
c     implicit double precision (a-h,o-z)                                    dp
      common /iaio/ iosave, iunit, itty, ncolum
      ncolum=81
      return
      end
      subroutine tvgrid (n1,n2)
c     implicit double precision (a-h,o-z)                                    dp
c
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      logical igrdl, nofram
      common /grdl/ igrdl, nofram
      logical ffopt,logo
      common/logocm/ffopt,logo,sizlll,xlll,ylll
      logical iuzg
      common/uzoom/iuzg
      character*81 textsg
      dimension tckwg(5), tckng(5), ticks(5), xtick(1), ytick(1)
      equivalence (ticks,ytick), (ticks(2),xtick)
      data tckng /.0078,0.,-.0078,0.,.0078/
      data tckwg /.9220,0.,-.9220,0.,.9220/
      data zero,one/0.0,1.0/
      data isz /0/, tveps /.0005/
      call stwndw(zero,one,zero,one)
      call stvprt(zero,one,zero,one)
      if (.not.igrdl) call blkcpy (tckng,ticks,5)
      if (igrdl) then
      tckwg(1)= tvxmax-tvxmin
      tckwg(2)=0.
      tckwg(3)=-tckwg(1)
      tckwg(4)=0.
      tckwg(5)= tckwg(1)
      call blkcpy (tckwg,ticks,5)
      endif
      call bgnplt                                                       diglb
      call colro(1)
      if (nofram) go to 61
      tvxmn=tvxmin-tveps
      tvxmx=tvxmax+tveps
      tvymn=tvymin-tveps
      tvymx=tvymax+tveps
      tvcx=tvxmin-.078
      tvcy=tvymin-.078
      tvxd=(tvxmax-tvxmin)/(xmax-xmin)
      tvyd=(tvymax-tvymin)/(ymax-ymin)
      nx=n1
      call rnd (xmin,xmax,nx,clx,chx)
      xd=(chx-clx)/nx
      ny=n2
      call rnd (ymin,ymax,ny,cly,chy)
      yd=(chy-cly)/ny
      xend=tvxmax
      yend=tvymin
      xx1=tvxmin
      yy1=tvymin
      clxc=clx
      xdc=xd
      tvy=tvymin
      ydc=0.
      n=nx
      k=1
      go to 70
   10 yend=tvymax
      tvx=tvxmax
      xdc=0.
      clyc=cly
      ydc=yd
      n=ny
      k=2
      go to 70
   20 xend=tvxmin
      clxc=chx
      xdc=-xd
      tvy=tvymax
      ydc=0.
      n=nx
      k=3
      go to 70
   30 xend=tvxmin
      yend=tvymin
      tvx=tvxmin
      xdc=0.
      clyc=chy
      ydc=-yd
      n=ny
      k=4
      xx1=tvxmin
      yy1=tvymax
      go to 70
   40 v=  max(abs(clx),abs(chx))
      j=1
      if ((v.lt.1000.).and.(xd.ge.0.1)) j=2
      do 50 i=isz,nx
      x=clx+i*xd
      if (abs(x/xd).lt.1.0e-06) x=0.
      tvx=x-xmin
      tvx=tvx*tvxd+tvxmin
      if ((tvx.lt.tvxmn).or.(tvxmx.lt.tvx)) go to 50
      if (j.eq.1) write(unit=textsg,fmt=130) x
      if (j.eq.2) write(unit=textsg,fmt=120) x
c     call plotxt (tvx,tvcy-.015,textsg,9,140.,90.)                     pltlb
      tvxp=tvx
      tvyp=tvcy-0.015
      xpsiz=110.0
      xpang=90.0
      call plotxt (tvxp,tvyp,textsg,9,xpsiz,xpang)                      diglb
   50 continue
      v=  max(abs(cly),abs(chy))
      j=1
      if ((v.lt.1000.).and.(yd.ge.0.1)) j=2
      do 60 i=isz,ny
      y=cly+i*yd
      if (abs(y/yd).lt.1.0e-06) y=0.
      tvy=y-ymin
      tvy=tvy*tvyd+tvymin
      if ((tvy.lt.tvymn).or.(tvymx.lt.tvy)) go to 60
      if (j.eq.1) write(unit=textsg,fmt=130) y
      if (j.eq.2) write(unit=textsg,fmt=120) y
c     call plotxt (tvcx-.015,tvy,textsg,9,140.,0.0)                     pltlb
      tvxp=tvcx-.015
      tvyp=tvy
      xpsiz=110.0
      xpang=0.0
      call plotxt (tvxp,tvyp,textsg,9,xpsiz,xpang)                      diglb
   60 continue
   61 if (.not.iuzg) go to 68
      call stlnty(1)
      call colro(7)
      xdel=(tvxmax-tvxmin)/10.
      ydel=(tvymax-tvymin)/10.
      xbgn1=tvxmin+xdel
      xbgn2=xbgn1
      ybgn1=tvymin
      ybgn2=tvymax
      do 62 i=1,9
      call onelne(xbgn1,ybgn1,xbgn2,ybgn2)
      xbgn1=xbgn1+xdel
   62 xbgn2=xbgn1
      xbgn1=tvxmin
      xbgn2=tvxmax
      ybgn1=tvymin+ydel
      ybgn2=ybgn1
      do 64 i=1,9
      call onelne(xbgn1,ybgn1,xbgn2,ybgn2)
      ybgn1=ybgn1+ydel
   64 ybgn2=ybgn1
      iuzg=.false.
      call stlnty(0)
   68 if (logo) call lllogo(sizlll,xlll,ylll)
      return
   70 do 100 i=isz,n
      if (xdc.eq.0.) go to 80
      x=clxc+i*xdc
      if (abs(x/xdc).lt.1.0e-06) x=0.
      tvx=x-xmin
      tvx=tvx*tvxd+tvxmin
      if ((tvx.lt.tvxmn).or.(tvxmx.lt.tvx)) go to 100
      go to 90
   80 y=clyc+i*ydc
      if (abs(y/ydc).lt.1.0e-06) y=0.
      tvy=y-ymin
      tvy=tvy*tvyd+tvymin
      if ((tvy.lt.tvymn).or.(tvymx.lt.tvy)) go to 100
   90 continue
      xx2=tvx
      yy2=tvy
      call plines (xx1,yy1,2)
      xx1=xx2
      yy1=yy2
      xx2=tvx+xtick(k)
      yy2=tvy+ytick(k)
      call plines (xx1,yy1,2)
      xx1=tvx
      yy1=tvy
  100 continue
      xx2=xend
      yy2=yend
      call plines (xx1,yy1,2)
      xx1=xx2
      yy1=yy2
      if (k.lt.3.and..not.ffopt) k=3
      go to (10,20,30,40), k
c
  110 format (1pe10.2)
  120 format (f8.2)
  130 format (1pe9.2)
      end
      subroutine lllogo(size1,xxx,yyy)
c     implicit double precision (a-h,o-z)                                    dp
c
c draws lll logo (ripped-off version)
c
c size1 sets size of the logo on 0 to 1 mapping
c
c xxx and yyy are the coordinates of the lower left hand corner
c     of the logo on (0,1)
c
      real zero,one,ten24
      dimension xb(34),xe1(22),xe2(22),xh1(7),xh2(7),xplot(40),yplot(40)
      data xb/98.,37.,34.,32.,30.,28.,26.4,25.,24.,22.8,
     x  21.6,20.6,20.,19.,18.4,17.6,17.,16.4,15.8,15.4,
     x 15.,14.5,14.1,13.7,13.4,13.1,12.9,12.6,12.5,12.4,
     x 12.2,12.1,12.1,12.1/
      data xe1/43.5,39.,36.4,34.5,33.0,31.7,
     x  30.5,29.6,28.8,28.2,27.4,26.7,26.3,25.7,25.4,25.1,
     x  24.6,24.5,24.3,24.1,24.0,24./
      data xe2/98.,98.,98.,98.,39.4,37.,
     x  35.2,34.,32.9,31.9,31.1,30.3,29.6,29.1,28.6,28.3,
     x  27.9,27.6,27.4,27.1,27.0,27.0/
      data xh1/44.5,42.2,41.1,40.2,39.7,39.3,39.1/
      data xh2/98.,98.,98.,98.,43.1,42.3,42.1/
      data zero,one,ten24/0.0,1.0,1024.0/
c
      call stwndw(one,ten24,one,ten24)
      call stvprt(zero,one,zero,one)
c
c convert from (0,1) to (1,1024)
c
      x=1023.*xxx+1.
      y=1023.*yyy+1.
      size=10.44897959*size1
      call colro (4)
c
c initial values
c
      dx=1.*size
      dy=1.*size
      z12=12.*size
      z13=13.*size
      z24=24.*size
      z27=27.*size
      z28=28.*size
      z39=39.*size
      z42=42.*size
      z43=43.*size
      z46=46.*size
      z54=54.*size
      z58=58.*size
      z73=73.*size
      z88=88.*size
      z98=98.*size
c
c
c******
c draw section a
      y1=y+z46
      x1=x
      x2=x+z12
      do 10  kyy=1,53
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
10    continue
c
c******
c draw section b
c
      y1=y+z12
      x1=x
      do 20  kyy=1,34
        x2=x+xb(kyy)*size
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
20    continue
c
c******
c draw section c
c
      y1=y
      x1=x
      x2=x+z98
      do 30  kyy=1,12
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
30    continue
c
c******
c draw section d
c
      x1=x+z24
      x2=x+z27
      y1=y+z46
      do 40  kyy=1,53
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
40    continue
c
c******
c draw section e
c
      y1=y+z24
      do 50  kyy=1,22
        x1=x+xe1(kyy)*size
        x2=x+xe2(kyy)*size
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
50    continue
c
c******
c draw section g
      x1=x+z39
      x2=x+z42
      y1=y+z46
      do 60  kyy=1,53
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
60    continue
c
c******
c draw section h
      y1=y+z39
      do 70  kyy=1,7
        x1=x+xh1(kyy)*size
        x2=x+xh2(kyy)*size
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
70    continue
c
c draw section j
      y1=y+z54
      x1=x+z54
      x2=x+z98
      do 80  kyy=1,45
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
80    continue
c
c******
c draw section k
      y1=y+z43
      x1=x+z88
      x2=x+z98
      do 90  kyy=1,11
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
        x1=x1+dx
90    continue
c
c******
c draw section l
      y1=y+z28
      x1=x+z73
      x2=x+z98
      do 100  kyy=1,11
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
        x1=x1+dx
100   continue
c
c******
c draw section m
      y1=y+z13
      x1=x+z58
      x2=x+z98
      do 110  kyy=1,11
        call onelne(x1,y1,x2,y1)
        y1=y1+dy
        x1=x1+dx
110   continue
c
c******
c draw outlines
c     left and right outside lines
      call onelne(x,y,x,y+z46+52.*dy)
      call onelne(x+z98,y,x+z98,y+z46+52.*dy)
c l verticals
      call onelne(x+z12,y+z46,x+z12,y+z46+52.*dy)
      call onelne(x+z24,y+z46,x+z24,y+z46+52.*dy)
      call onelne(x+z27,y+z46,x+z27,y+z46+52.*dy)
      call onelne(x+z39,y+z46,x+z39,y+z46+52.*dy)
      call onelne(x+z42,y+z46,x+z42,y+z46+52.*dy)
      call onelne(x+z54,y+z54,x+z54,y+z54+44.*dy)
c l slants
      call onelne(x+z58-dy,y+z13-dy,x+z58+11.*dy,y+z13+11.*dy)
      call onelne(x+z73-dy,y+z28-dy,x+z73+11.*dy,y+z28+11.*dy)
      call onelne(x+z88-dy,y+z43-dy,x+z88+10.*dy,y+z43+10.*dy)
c l curves
c   sec b
      do 200  kxx=1,34
          xplot(kxx)=x+xb(kxx)*size
          yplot(kxx)=y+z12+dy*(kxx-1)
200   continue
      xplot(1)=x+(xb(2)+2.*(xb(2)-xb(3)))*size
      xplot(35)=x+z12
      yplot(35)=y+z12+dy*34.
      call plines (xplot,yplot,35)
c   sec e
      do 210  kxx=1,22
          xplot(kxx)=x+xe1(kxx)*size
          yplot(kxx)=y+z24+dy*(kxx-1)
210   continue
      xplot(23)=x+z24
      yplot(23)=y+z24+dy*22.
      call plines (xplot,yplot,23)
c   sec e
      do 220  kxx=4,22
          xplot(kxx-3)=x+xe2(kxx)*size
          yplot(kxx-3)=y+z24+dy*(kxx-1)
220   continue
      xplot(1)=x+(xe2(5)+2.*(xe2(5)-xe2(6)))*size
      xplot(20)=x+z27
      yplot(20)=y+z24+dy*22.
      call plines (xplot,yplot,20)
c   sec h
      do 240  kxx=1,7
          xplot(kxx)=x+xh1(kxx)*size
          yplot(kxx)=y+z39+(kxx-1)*dy
240   continue
      xplot(8)=x+z39
      yplot(8)=y+z39+7.*dy
      call plines (xplot,yplot,8)
c   sec h
      do 260  kxx=4,7
          xplot(kxx-3)=x+xh2(kxx)*size
          yplot(kxx-3)=y+z39+(kxx-1)*dy
260   continue
      xplot(1)=x+(xh2(5)+2.*(xh2(5)-xh2(6)))*size
      xplot(5)=x+z42
      yplot(5)=y+z39+7.*dy
      call plines (xplot,yplot,5)
c
      call stwndw(zero,one,zero,one)
      return
      end
      subroutine rnd (al,au,ni,als,aus)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension rd(4)
      data nr, rd /4,1.,2.,2.5,5./
      data almost /.99999/
      data zero /0.0/
c
      amin=al
      amax=au
      r=amax-amin
      if (r.ne.0.) go to 40
      if (amax) 30,10,20
   10 amax=1.
      amin=-amax
      r=2.
      go to 40
   20 amin=0.
      amax=amax+amax
      r=amax
      go to 40
   30 amax=0.
      amin=amin+amin
      r=-amin
   40 r=r/ni
      e=log10(r)
      if (e.lt.0.) e=e-almost
      p=10.**int(e)
      rk=r/p
      l=nr
      if (rd(l)-rk) 50,80,60
   50 p=10.*p
      rk=rk/10.
   60 continue
      do 70 i=1,nr
      if (rd(i).lt.rk) go to 70
      l=i
      go to 80
   70 continue
   80 s=rd(l)*p
      imin=amin/s+  min(zero,sign(almost,amin))
      als=imin*s
      imax=amax/s+  max(zero,sign(almost,amax))
      aus=imax*s
      ni=max0(imax-imin,1)
      return
      end
      subroutine stvprt(xvpmin,xvpmax,yvpmin,yvpmax)
c     implicit double precision (a-h,o-z)                                    dp
      common/vport/xvpmn,yvpmn,vpdx,vpdy
      xvpmn=xvpmin
      yvpmn=yvpmin
      vpdx =xvpmax-xvpmin
      vpdy =yvpmax-yvpmin
      return
      end
      subroutine stwndw(xwdmin,xwdmax,ywdmin,ywdmax)
c     implicit double precision (a-h,o-z)                                    dp
      common/wdort/xwdmn,ywdmn,wddx,wddy
      xwdmn=xwdmin
      ywdmn=ywdmin
      wddx =xwdmax-xwdmin
      wddy =ywdmax-ywdmin
      return
      end
      subroutine plotxt (xc,yc,textsg,n,txsize,txtang)
c     implicit double precision (a-h,o-z)                                    dp
c
c     plot text string
c
      common/devscl/ xvlen,yvlen,delta
      common /vport/ xvpmn,yvpmn,vpdx,vpdy
      common /wdort/ xwdmn,ywdmn,wddx,wddy
      common /charc/ ichar, txtsiz
c     integer*4 textline(20)                                            di3000
c     real txtszp,txtanp,spxc,spyc                                           dp
c     character*1 text(120)                                             pltlb
      character*1 text(120)                                             unix
c     byte text(120)                                                    vms
      character*(*) textsg
      txtsiz=1.0                                                        diglb
      call colro(ichar)
c     call colro(ichar)                                                 di3000
      read (unit=textsg,fmt=10)(text(i),i=1,n)
      textsz  =txtsiz/txsize
      angle= txtang/57.29577951
c     call settxsiz(textsz  ,textsz  )                                  pltlb
c     call settxang( txtang)                                            pltlb
      call sclpnt (xc,yc,sxc,syc)
      textsz  =xvlen*textsz                                             vax75
      txtszp=textsz
      txtanp=txtang
      spxc=sxc
      spyc=syc
      call gssetc(txtszp  , txtanp)                                     diglb
      call gsmove(spxc,spyc)                                            diglb
c     text (n+1)=0                                                      vms
      text(n+1)=char(0)                                                 unix
c     call gspstr(text)                                                 wkstn
c     call gspstr(text)                                                 vms
      call gspstr(textsg)                                               unics
c     cs=cos( txtang/57.29577951)                                       di3000
c     sn=sin( txtang/57.29577951)                                       di3000
c     call jbase (cs, sn, 0.)                                           di3000
c     call jjust(1,1)                                                   di3000
c     call jsize(textsz  ,textsz  )                                     di3000
c     call jmove (sxc,syc)                                              di3000
c     call jhtext(n,textline)                                           di3000
c     call plottext(sxc,syc, text ,n)                                   pltlb
      call colro(5)
c
      return
   10 format(110a1)
      end
      subroutine endtv
c     implicit double precision (a-h,o-z)                                    dp
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      common /colvss/ ltvg,ltvb
c     if (itmds.ne.0) call termtv('redchn ')                            pltlb
c     if(ltvg.ne.0) call termtv('grnchn ')                              pltlb
c     if(ltvb.ne.0) call termtv('bluchn ')                              pltlb
c     ltvg=0                                                            pltlb
c     ltvb=0                                                            pltlb
      return
      end
      subroutine endgr
c     implicit double precision (a-h,o-z)                                    dp
      common /devs/ ifr80,itmds,xx1,xx2,yy1,yy2
      common /colvss/ ltvg,ltvb
      character*8 dlname
c
      call rlsdev                                                       diglb
c     if(itmds.ne.0) call termtv('redchn')                              pltlb
c     if(ltvg.ne.0) call termtv('grnchn')                               pltlb
c     if(ltvb.ne.0) call termtv('bluchn')                               pltlb
c     if (ifr80.ne.0) then                                              pltlb
c        call getdlfn('johnfr',dlname)                                  pltlb
c        write( *,20) dlname                                            pltlb
c  20    format(' output file is: ',a8)                                 pltlb
c     endif                                                             pltlb
c     if (ifr80.ne.0) call termdl('johnfr')                             pltlb
c     call jdevof(0)                                                    di3000
c     call jdevof(1)                                                    di3000
c     call jend                                                         di3000
      return
      end
      subroutine gettvm (l)
c     implicit double precision (a-h,o-z)                                    dp
c     common/worksp/worksp(20000)                                       pltlb
      common /colvss/ ltvg,ltvb
      common /ifhcom/ ifhed(50)
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      common /isoplt/ nmt, nc, inn, ww(26), mat(20), xlower, higher
      common/devscl/ xvlen,yvlen,delta
      common/gd410d/ ldvice                                             diglb
      logical setatt
c     real gsxlcm,gsylcm                                                     dp
      data setatt/.false./
      ldvice =l                                                         diglb
c     if (setatt) go to 10                                              pltlb
c     call setdtlev('unclass ')                                         pltlb
c     call setiobuf(worksp,20000)                                       pltlb
c     setatt=.true.                                                     pltlb
      ldvice =l                                                         diglb
      if (l.eq.9.or.l.eq.10) ltvg=1                                     diglb
      xvlen=gsxlcm()                                                    diglb
      yvlen=gsylcm()                                                    diglb
      delta=.5*(xvlen-yvlen)                                            diglb
      xvlen=yvlen                                                       diglb
   10 if (l.eq.0) return
      inn=9
      itmds = l
      ioct=ioctal(l)
c     call inittv('redchn',ioct)                                        pltlb
c     if(ltvg.eq.0.and.ltvb.eq.0) return                                pltlb
      inn=5
c     ioct=ioctal(ltvg)                                                 pltlb
c     call inittv('grnchn',ioct)                                        pltlb
c     ioct=ioctal(ltvb)                                                 pltlb
c     call inittv('bluchn',ioct)                                        pltlb
      return
      end
      integer function ioctal (l)
      l1=l-(l/10)*10
      l2=(l-(l/100)*100-l1)/10
      l3=(l-(l/1000)*1000-l1-l2*10)/100
      l4=(l-(l/10000)*10000-l1-l2*10-l3*100)/1000
      ioctal=l1+8*l2+64*l3+512*l4
      return
      end
      subroutine getpdv
c     implicit double precision (a-h,o-z)                                    dp
      character*8 ifhed
      common /ifhcom/ ifhed(50)
      common /devs/ ifr80,itmds,xx1,xx2,yy1,yy2
      character*8 give,class,ftype
      common /fr80o/ give,class,ftype
      common /charc/ ichar, txtsiz
      logical viewp
      common/viewp/viewp
      character*8 info(2)
c
c     if (ifr80.ne.0) call termdl('johnfr')                             pltlb
      ifr80=1
      viewp=.false.
c     txtsiz=1.0                                                        pltlb
c     call setftbsz(2000b)                                              pltlb
c     call setvwprt(.0,1.,.0,1.)                                        pltlb
c     call setdllev('johnfr',class)                                     pltlb
c     call setdlid ('johnfr',ifhed(36),24)                              pltlb
c     call setdlgiv('johnfr',give)                                      pltlb
c     call setdlrt('johnfr','orio',1)                                   pltlb
c     info(1)='dli'                                                     pltlb
c     info(2)=ftype                                                     pltlb
c     if (give.ne.'keep') info(1)='fr80'                                pltlb
c     call initdl('johnfr',info,2)                                      pltlb
c     if (ftype.ne.'cslide35') return                                   pltlb
c     call setvwprt(.1818,.8182,.1818,.8182)                            pltlb
c     viewp=.true.                                                      pltlb
c     txtsiz=.64                                                        pltlb
      return
      end
      subroutine plotpc
c     implicit double precision (a-h,o-z)                                    dp
      common /devs/ ifr80, itmds, xx1, xx2, yy1, yy2
      call colro(1)
c     call newpict                                                      pltlb
      call endplt                                                       diglb
      return
      end
      subroutine plines (x,y,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(1), y(1)
      n1=n-1
      do 10 i=1,n1
   10 call onelne(x(i),y(i),x(i+1),y(i+1))
      return
      end
      subroutine onelne(x1,y1,x2,y2)
c     implicit double precision (a-h,o-z)                                    dp
c
c     plot single line
c
      dimension x(2),y(2)
      dimension xp(2),yp(2)
c     real xp,yp                                                             dp
      call sclpnt (x1,y1,x(1),y(1))
      call sclpnt (x2,y2,x(2),y(2))
      xp(1)=x(1)
      xp(2)=x(2)
      yp(1)=y(1)
      yp(2)=y(2)
      call gsmove(xp(1),yp(1))                                          diglb
      call gsdraw(xp(2),yp(2))                                          diglb
c     call plotcnln(x,y,2,1)                                            pltlb
      return
      end
      subroutine sclpnt (x1,y1,x,y)
c     implicit double precision (a-h,o-z)                                    dp
      common/wdort/xwdmn,ywdmn,wddx,wddy
      common/vport/xvpmn,yvpmn,vpdx,vpdy
      common/devscl/ xvlen,yvlen,delta
      x=xvpmn+vpdx*(x1-xwdmn)/wddx
      y=yvpmn+vpdy*(y1-ywdmn)/wddy
      x=delta+xvlen*x                                                   vax75
      y=yvlen*y                                                         vax75
      return
      end
      subroutine point (x,y)
c     implicit double precision (a-h,o-z)                                    dp
c
c     plot point at location x,y
c
      common /rang/ xmin, xmax, ymin, ymax, tvxmin, tvxmax, tvymin,
     1 tvymax
      dimension xx(2),yy(2)
      xm=  min(xmax-x,x-xmin,ymax-y,y-ymin)
      if (xm.lt.0.) return
      xx(1)=x
      xx(2)=x
      yy(1)=y
      yy(2)=y
c     call plines (xx,yy,2)                                             cray1
      return
      end
      subroutine colro(icolor)
c     implicit double precision (a-h,o-z)                                    dp
      common /colvss/ ltvg,ltvb
      logical colgf
      common /colvsc/ iclnum,iclfr8,colgf,icurcl
c     dimension colors(7),red(32),grn(32),blu(32)                       pltlb
c     dimension redbuf(4225), grnbuf(4225), blubuf(4225)                pltlb
c     data colors/'white','red','green','blue','cyan','magenta',        pltlb
c    1'yellow'/                                                         pltlb
      data ir,ig,ib/1,1,1/
      if (icurcl .eq.icolor) return
      icurcl =icolor
c     if (colgf) then                                                   pltlb
c     if (icolor.eq.1) then                                             pltlb
c     icurcl =iclnum                                                    pltlb
c     return                                                            pltlb
c     endif                                                             pltlb
c     if (iclnum .eq.2) then                                            pltlb
c     if (icolor.eq.6.or.icolor.eq.7) icurcl =2                         pltlb
c     return                                                            pltlb
c     endif                                                             pltlb
c     if (iclnum .eq.3) then                                            pltlb
c     if (icolor.eq.5.or.icolor.eq.7) icurcl =3                         pltlb
c     return                                                            pltlb
c     endif                                                             pltlb
c     if (iclnum .eq.4) then                                            pltlb
c     if (icolor.eq.5.or.icolor.eq.6) icurcl =4                         pltlb
c     return                                                            pltlb
c     endif                                                             pltlb
c     endif                                                             pltlb
c     call setcolor(colors(icolor))                                     pltlb
      call gscolr(icolor,ierr)                                          diglb
c     if(ltvg.eq.0) return                                              pltlb
c     go to (10,20,30,40,50,60,70),icolor                               pltlb
c  10 if(ir .eq. 0) then                                                pltlb
c     call reacttv('redchn', red)                                       pltlb
c     call rstiobuf('redchn', redbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ig .eq. 0) then                                                pltlb
c     call reacttv('grnchn', grn)                                       pltlb
c     call rstiobuf('grnchn', grnbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ib .eq. 0) then                                                pltlb
c     call reacttv('bluchn', blu)                                       pltlb
c     call rstiobuf('bluchn', blubuf)                                   pltlb
c     endif                                                             pltlb
c     ir=1                                                              pltlb
c     ig=1                                                              pltlb
c     ib=1                                                              pltlb
c     return                                                            pltlb
c  20 if(ir .eq. 0) then                                                pltlb
c     call reacttv('redchn', red)                                       pltlb
c     call rstiobuf('redchn', redbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ig .eq. 1) then                                                pltlb
c     call saviobuf('grnchn', grnbuf)                                   pltlb
c     call deacttv('grnchn', grn)                                       pltlb
c     endif                                                             pltlb
c     if(ib .eq. 1) then                                                pltlb
c     call saviobuf('bluchn', blubuf)                                   pltlb
c     call deacttv('bluchn', blu)                                       pltlb
c     endif                                                             pltlb
c     ir=1                                                              pltlb
c     ig=0                                                              pltlb
c     ib=0                                                              pltlb
c     return                                                            pltlb
c  30 if(ir .eq. 1) then                                                pltlb
c     call saviobuf('redchn', redbuf)                                   pltlb
c     call deacttv('redchn', red)                                       pltlb
c     endif                                                             pltlb
c     if(ig .eq. 0) then                                                pltlb
c     call reacttv('grnchn', grn)                                       pltlb
c     call rstiobuf('grnchn', grnbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ib .eq. 1) then                                                pltlb
c     call saviobuf('bluchn', blubuf)                                   pltlb
c     call deacttv('bluchn', blu)                                       pltlb
c     endif                                                             pltlb
c     ir=0                                                              pltlb
c     ig=1                                                              pltlb
c     ib=0                                                              pltlb
c     return                                                            pltlb
c  40 if(ir .eq. 1) then                                                pltlb
c     call saviobuf('redchn', redbuf)                                   pltlb
c     call deacttv('redchn', red)                                       pltlb
c     endif                                                             pltlb
c     if(ig .eq. 1) then                                                pltlb
c     call saviobuf('grnchn', grnbuf)                                   pltlb
c     call deacttv('grnchn', grn)                                       pltlb
c     endif                                                             pltlb
c     if(ib .eq. 0) then                                                pltlb
c     call reacttv('bluchn', blu)                                       pltlb
c     call rstiobuf('bluchn', blubuf)                                   pltlb
c     endif                                                             pltlb
c     ir=0                                                              pltlb
c     ig=0                                                              pltlb
c     ib=1                                                              pltlb
c     return                                                            pltlb
c  50 if(ir .eq. 1) then                                                pltlb
c     call saviobuf('redchn', redbuf)                                   pltlb
c     call deacttv('redchn', red)                                       pltlb
c     endif                                                             pltlb
c     if(ig .eq. 0) then                                                pltlb
c     call reacttv('grnchn', grn)                                       pltlb
c     call rstiobuf('grnchn', grnbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ib .eq. 0) then                                                pltlb
c     call reacttv('bluchn', blu)                                       pltlb
c     call rstiobuf('bluchn', blubuf)                                   pltlb
c     endif                                                             pltlb
c     ir=0                                                              pltlb
c     ig=1                                                              pltlb
c     ib=1                                                              pltlb
c     return                                                            pltlb
c  60 if(ir .eq. 0) then                                                pltlb
c     call reacttv('redchn', red)                                       pltlb
c     call rstiobuf('redchn', redbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ig .eq. 1) then                                                pltlb
c     call saviobuf('grnchn', grnbuf)                                   pltlb
c     call deacttv('grnchn', grn)                                       pltlb
c     endif                                                             pltlb
c     if(ib .eq. 0) then                                                pltlb
c     call reacttv('bluchn', blu)                                       pltlb
c     call rstiobuf('bluchn', blubuf)                                   pltlb
c     endif                                                             pltlb
c     ir=1                                                              pltlb
c     ig=0                                                              pltlb
c     ib=1                                                              pltlb
c     return                                                            pltlb
c  70 if(ir .eq. 0) then                                                pltlb
c     call reacttv('redchn', red)                                       pltlb
c     call rstiobuf('redchn', redbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ig .eq. 0) then                                                pltlb
c     call reacttv('grnchn', grn)                                       pltlb
c     call rstiobuf('grnchn', grnbuf)                                   pltlb
c     endif                                                             pltlb
c     if(ib .eq. 1) then                                                pltlb
c     call saviobuf('bluchn', blubuf)                                   pltlb
c     call deacttv('bluchn', blu)                                       pltlb
c     endif                                                             pltlb
c     ir=1                                                              pltlb
c     ig=1                                                              pltlb
c     ib=0                                                              pltlb
      return
      end
      subroutine stlnty(itype)
c     implicit double precision (a-h,o-z)                                    dp
      character*6 lintyp
      dimension lintyp(2)
      data lintyp/'solid','dotted'/
c     call setlnsty(lintyp(itype+1))                                    pltlb
      return
      end
      subroutine shader (x,y,n,xres,yres,fact,iclip)
c     implicit double precision (a-h,o-z)                                    dp
      logical colgf
      common /colvsc/ iclnum,iclfr8,colgf,icurcl
      common /vport/ xvpmn,yvpmn,vpdx,vpdy
      common/devscl/ xvlen,yvlen,delta
      logical ffopt,logo
      common/logocm/ffopt,logo,sizlll,xlll,ylll
      dimension x(*), y(*), xins(40), s(2), t(2)
      dimension sp(2),tp(2)
c     real sp,tp                                                             dp
      logical left
      ymap(yyy)=2.0*aint(yscale*yyy+0.5)+1.0
c
      if (colgf.and.(iclnum .ne.icurcl )) return
      fac=1.03
      if(ffopt) fac=1.21
      vdx=vpdx/fac
      vdy=vpdy/fac
      xmin=  min(x(1),x(2))
      xmax=  max(x(1),x(2))
      ymin=  min(y(1),y(2))
      ymax=  max(y(1),y(2))
      do 5 i=2,n
      xmin=  min(x(i),xmin)
      ymin=  min(y(i),ymin)
      xmax=  max(x(i),xmax)
    5 ymax=  max(y(i),ymax)
c
      left=.true.
      yscale=yres*fact
      dlines=2.00*fact
      ymin=aint(ymap(ymin)/dlines)*dlines+dlines
      ymax=aint(ymap(ymax)/dlines)*dlines
      yscan=ymin
   10 continue
      inisec=0
      ifirst=0
      ybegin=ymap(y(n))
      xbegin=x(n)
      do 30 l=1,n
      yend=ymap(y(l))
      dy=yscan-ybegin
      if (dy*(yscan-yend).gt.0.) go to 20
      inisec=inisec+1
      xins(inisec)=dy*(x(l)-xbegin)/(yend-ybegin)+xbegin
   20 continue
      ybegin=yend
      xbegin=x(l)
   30 continue
      if (inisec.eq.0) go to 90
      inisc1=inisec-1
      do 70 i=1,inisc1
      xkey=xins(i)
      i1=i+1
      do 60 j=i1,inisec
      if (.not.left) go to 50
      if (xkey.ge.xins(j)) go to 60
   40 continue
      temp=xkey
      xkey=xins(j)
      xins(j)=temp
      go to 60
   50 if (xkey.gt.xins(j)) go to 40
   60 continue
      xins(i)=xkey
   70 continue
      yy=yscan/(2.*yscale)
      do 80 i=1,inisec,2
      s(1)=xins(i)
      s(2)=xins(i+1)
      t(1)=yy
      t(2)=yy
      if (iclip.eq.1) then
c     if (yy.lt.yvpmn    ) go to 80                                     pltlb
c     if (yy.gt.yvpmn+vdy) go to 80                                     pltlb
c     s(1)=  max(s(1),xvpmn)                                            pltlb
c     s(1)=  min(s(1),xvpmn+vdx)                                        pltlb
c     s(2)=  max(s(2),xvpmn)                                            pltlb
c     s(2)=  min(s(2),xvpmn+vdx)                                        pltlb
      if (yy.lt.yvlen*yvpmn    )   go to 80                             diglb
      if (yy.gt.yvlen*(yvpmn+vdy)) go to 80                             diglb
      s(1)=  max(s(1),delta+xvlen*xvpmn)                                diglb
      s(1)=  min(s(1),delta+xvlen*(xvpmn+vdx))                          diglb
      s(2)=  max(s(2),delta+xvlen*xvpmn)                                diglb
      s(2)=  min(s(2),delta+xvlen*(xvpmn+vdx))                          diglb
      if (s(1).eq.s(2)) go to 80
      endif
      sp(1)=s(1)
      sp(2)=s(2)
      tp(1)=t(1)
      tp(2)=t(2)
c     call plotcnln (s,t,2,1)                                           pltlb
      call gsmove (sp(1),tp(1))                                         diglb
      call gsdraw (sp(2),tp(2))                                         diglb
   80 continue
   90 continue
      yscan=yscan+dlines
      left=.not.left
      if (yscan.le.ymax) go to 10
      return
      end
      subroutine inter6(p,tau,numlp,f,xmag,ierr)
c     implicit double precision (a-h,o-z)                                    dp
      dimension p(2,1)
      if (tau-p(1,numlp).gt.0.0) go to 50
      do 20 m=2,numlp
      if (tau-p(1,m).le.0.0) go to 30
   20 continue
      go to 50
   30 dt1=tau-p(1,m-1)
      d1=p(1,m)-p(1,m-1)
      d2=p(2,m)-p(2,m-1)
      f=p(2,m-1)+dt1*d2/d1
      f=xmag*f
      return
   50 ierr=1
      return
      end
      subroutine show
c     implicit double precision (a-h,o-z)                                    dp
      logical viewp
      common/viewp/viewp
      common /charc/ ichar, txtsiz
      common /colvss/ ltvg,ltvb
      common /finges/ igol, fringe, xres, yres, numcol, icolr(6)
      dimension tx(5),ty(5)
      character*110 textsg
      data tx/0.,1.,1.,0.,0./,ty/0.,0.,1.,1.,0./
      data zero,one,fiftsx,xninty/0.0,1.0,56.0,90.0/
      call stwndw (zero,fiftsx,zero,fiftsx)
      call stvprt(zero,one,zero,one)
      call bgnplt                                                       vax75
      lower=1
      lhigh=1
      if (ltvg.ne.0) then
      call colro (4)
      call stwndw (zero, one,zero, one)
      if (viewp) then
c     call setvwprt(0.0,1.0,.1818,.8182)                                pltlb
      endif
c     call shader (tx,ty,4,xres,yres,16.,0)                             pltlb
      if (viewp) then
c     call setvwprt(.1818,.8182,.1818,.8182)                            pltlb
      endif
      call stwndw (zero,fiftsx,zero,fiftsx)
c     lower=2                                                           cray1
c     lhigh=3                                                           cray1
      endif
      do 1 ichr=lower,lhigh
      ichar=ichr
      write (unit=textsg,fmt=250)
      xc=7.0
      yc=49.0
      tsiz=65.0
      call plotxt  (xc,yc,textsg,55,tsiz,zero)
      write (unit=textsg,fmt=10)
      xc=6.0
      yc=47.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=20)
      xc=6.0
      yc=46.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=30)
      yc=45.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=40)
      yc=44.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=50)
      yc=43.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=60)
      yc=42.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=70)
      yc=41.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=80)
      yc=40.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=90)
      yc=39.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=100)
      yc=38.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=110)
      yc=37.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=120)
      yc=36.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=130)
      yc=35.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=140)
      yc=34.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=150)
      yc=33.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=160)
      yc=32.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=170)
      yc=31.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=180)
      yc=30.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=190)
      yc=29.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=200)
      yc=28.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=210)
      yc=27.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=220)
      yc=26.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=221)
      yc=25.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=222)
      yc=24.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=223)
      yc=23.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=224)
      yc=22.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=225)
      yc=21.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=226)
      yc=20.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=227)
      yc=19.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=228)
      yc=18.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=229)
      yc=17.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=230)
      yc=14.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=240)
      yc=13.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=260)
      yc=12.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=270)
      yc=11.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
      write (unit=textsg,fmt=280)
      yc=10.0
      call plotxt  (xc,yc,textsg,55,xninty,zero)
    1 continue
      call plotpc
      ichar=1
      return
   10 format('  #     component            #     component           ')
   20 format('  1  x - stress             22 3rd history variable    ')
   30 format('  2  y - stress             23 4th histroy variable    ')
   40 format('  3  z - stress             24       etc.              ')
   50 format('  4  xy - stress            25                         ')
   60 format('  5  yz - stress            26                         ')
   70 format('  6  zx - stress            27                         ')
   80 format('  7  plastic strain         28                         ')
   90 format('  8  pressure               29                         ')
  100 format('  9  von mises stress       30                         ')
  110 format(' 10  1st prin. deviatoric   31                         ')
  120 format(' 11  2nd prin. deviatoric   32                         ')
  130 format(' 12  3rd prin. deviatoric   33                         ')
  140 format(' 13  maximum shear stress   34                         ')
  150 format(' 14  1st prin. stress       35                         ')
  160 format(' 15  2nd prin. stress       36                         ')
  170 format(' 16  3rd prin. stress       37                         ')
  180 format(' 17  ln (v/v0)              38                         ')
  190 format(' 18  relative volume        39                         ')
  200 format(' 19  v0/v-1.                40                         ')
  210 format(' 20 1st history variable    41                         ')
  220 format(' 21 2nd history variable    42                         ')
  221 format(' ------------------------------------------------------')
  222 format('                                                       ')
  223 format('                                                       ')
  224 format('                                                       ')
  225 format('                                                       ')
  226 format('                                                       ')
  227 format('                                                       ')
  228 format('                                                       ')
  229 format(' ------------------------------------------------------')
  230 format('                                                       ')
  240 format('                                                       ')
  250 format(' e l e m e n t   c o m p o n e n t s                   ')
  260 format(' component numbers for element variables. by adding    ')
  270 format(' 100and 400 to component numbers 1-16, strains and     ')
  280 format(' strainrates are obtained.                             ')
      end
