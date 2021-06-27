      subroutine f3dm2 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     orthotropic-elastic material
c
c     valid for large displacements and rotations but small strain
c     major reference: cook,'concepts and applications for finite
c                      element analysis',wiley
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &xl1(128),xm1(128),xn1(128),xl2(128),
     &xm2(128),xn2(128),xl3(128),xm3(128),xn3(128),
     &x17(128),x28(128),x35(128),x46(128),y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),df31(128),
     &rx(128),sp1(128),sp2(128),sp3(128),
     &sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),qf(128),q1(128),q2(128),
     &q3(128),q4(128),q5(128),q6(128),q7(128),q8(128),q9(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/e1(128),e2(128),e3(128),e4(128),e5(128),e6(128)
      common/aux41/qs(128)
      common/ortho/c11,c21,c31,c41,c51,c61,c12,c22,c32,c42,c52,c62,
     1 c13,c23,c33,c43,c53,c63,c14,c24,c34,c44,c54,c64,c15,c25,c35,
     2 c45,c55,c65,c16,c26,c36,c46,c56,c66
      dimension cm(*),bqs(*),c(6,6,128),tm(6,6,128),d(6,6),
     1  t(6,6,128)
c
c
c     compute deformation gradients in undeformed configuration
c
      k=48*(mxt(lft)-1)+13
      do 10 i=1,6
      do 10 j=1,6
      d(i,j)=cm(k)
   10 k=k+1
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
      z46(i)=z4(i)-z6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dde(i)*bqs(i))*dt1
   20 continue
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
c
c     green-st.venant strain  e=(c-1)/2
c     where c is the right cauchy-green tensor
c
      do 40 i=lft,llt
      e1(i)=.5*(eps1(i)*eps1(i)+eps4(i)*eps4(i)+eps7(i)*eps7(i)-1.)
      e2(i)=.5*(eps2(i)*eps2(i)+eps5(i)*eps5(i)+eps8(i)*eps8(i)-1.)
      e3(i)=.5*(eps3(i)*eps3(i)+eps6(i)*eps6(i)+eps9(i)*eps9(i)-1.)
      e4(i)=eps1(i)*eps2(i)+eps4(i)*eps5(i)+eps7(i)*eps8(i)
      e5(i)=eps2(i)*eps3(i)+eps5(i)*eps6(i)+eps8(i)*eps9(i)
   40 e6(i)=eps1(i)*eps3(i)+eps4(i)*eps6(i)+eps7(i)*eps9(i)
c
      do 50 i=lft,llt
      xl3(i)=xm1(i)*xn2(i)-xm2(i)*xn1(i)
      xm3(i)=xn1(i)*xl2(i)-xn2(i)*xl1(i)
   50 xn3(i)=xl1(i)*xm2(i)-xl2(i)*xm1(i)
c
c     calculate transformation matrix
c
      do 55 i=lft,llt
      t(1,1,i)=xl1(i)*xl1(i)
      t(1,2,i)=xm1(i)*xm1(i)
      t(1,3,i)=xn1(i)*xn1(i)
      t(1,4,i)=xl1(i)*xm1(i)
      t(1,5,i)=xm1(i)*xn1(i)
      t(1,6,i)=xn1(i)*xl1(i)
      t(2,1,i)=xl2(i)*xl2(i)
      t(2,2,i)=xm2(i)*xm2(i)
      t(2,3,i)=xn2(i)*xn2(i)
      t(2,4,i)=xl2(i)*xm2(i)
      t(2,5,i)=xm2(i)*xn2(i)
      t(2,6,i)=xn2(i)*xl2(i)
      t(3,1,i)=xl3(i)*xl3(i)
      t(3,2,i)=xm3(i)*xm3(i)
      t(3,3,i)=xn3(i)*xn3(i)
      t(3,4,i)=xl3(i)*xm3(i)
      t(3,5,i)=xm3(i)*xn3(i)
      t(3,6,i)=xn3(i)*xl3(i)
      t(4,1,i)=2.*xl1(i)*xl2(i)
      t(4,2,i)=2.*xm1(i)*xm2(i)
      t(4,3,i)=2.*xn1(i)*xn2(i)
      t(4,4,i)=xl1(i)*xm2(i)+xl2(i)*xm1(i)
      t(4,5,i)=xm1(i)*xn2(i)+xm2(i)*xn1(i)
      t(4,6,i)=xn1(i)*xl2(i)+xn2(i)*xl1(i)
      t(5,1,i)=2.*xl2(i)*xl3(i)
      t(5,2,i)=2.*xm2(i)*xm3(i)
      t(5,3,i)=2.*xn2(i)*xn3(i)
      t(5,4,i)=xl2(i)*xm3(i)+xl3(i)*xm2(i)
      t(5,5,i)=xm2(i)*xn3(i)+xm3(i)*xn2(i)
      t(5,6,i)=xn2(i)*xl3(i)+xn3(i)*xl2(i)
      t(6,1,i)=2.*xl3(i)*xl1(i)
      t(6,2,i)=2.*xm3(i)*xm1(i)
      t(6,3,i)=2.*xn3(i)*xn1(i)
      t(6,4,i)=xl3(i)*xm1(i)+xl1(i)*xm3(i)
      t(6,5,i)=xm3(i)*xn1(i)+xm1(i)*xn3(i)
      t(6,6,i)=xn3(i)*xl1(i)+xn1(i)*xl3(i)
   55 continue
c
c     transform constitutive matrix
c
c.... calculate tm=d*t
c
      do 60 m=1,6
      do 60 n=1,6
      do 58 i=lft,llt
      tm(m,n,i)=d(m,1)*t(1,n,i)+d(m,2)*t(2,n,i)+d(m,3)*t(3,n,i)
     1           +d(m,4)*t(4,n,i)+d(m,5)*t(5,n,i)+d(m,6)*t(6,n,i)
   58 continue
   60 continue
c
c.... compute c=t-transpose*tm
c
      do 70 m=1,6
      do 70 n=1,6
      do 65 i=lft,llt
      c(m,n,i)=t(1,m,i)*tm(1,n,i)+t(2,m,i)*tm(2,n,i)+t(3,m,i)*tm(3,n,i)
     1        +t(4,m,i)*tm(4,n,i)+t(5,m,i)*tm(5,n,i)+t(6,m,i)*tm(6,n,i)
   65 continue
   70 continue
c
      do 80 i=lft,llt
      sp1(i)=c(1,1,i)*e1(i)+c(1,2,i)*e2(i)+c(1,3,i)*e3(i)
     1      +c(1,4,i)*e4(i)+c(1,5,i)*e5(i)+c(1,6,i)*e6(i)
      sp2(i)=c(2,1,i)*e1(i)+c(2,2,i)*e2(i)+c(2,3,i)*e3(i)
     1      +c(2,4,i)*e4(i)+c(2,5,i)*e5(i)+c(2,6,i)*e6(i)
      sp3(i)=c(3,1,i)*e1(i)+c(3,2,i)*e2(i)+c(3,3,i)*e3(i)
     1      +c(3,4,i)*e4(i)+c(3,5,i)*e5(i)+c(3,6,i)*e6(i)
      sp4(i)=c(4,1,i)*e1(i)+c(4,2,i)*e2(i)+c(4,3,i)*e3(i)
     1      +c(4,4,i)*e4(i)+c(4,5,i)*e5(i)+c(4,6,i)*e6(i)
      sp5(i)=c(5,1,i)*e1(i)+c(5,2,i)*e2(i)+c(5,3,i)*e3(i)
     1      +c(5,4,i)*e4(i)+c(5,5,i)*e5(i)+c(5,6,i)*e6(i)
      sp6(i)=c(6,1,i)*e1(i)+c(6,2,i)*e2(i)+c(6,3,i)*e3(i)
     1      +c(6,4,i)*e4(i)+c(6,5,i)*e5(i)+c(6,6,i)*e6(i)
   80 continue
c
c     cauchy stress
c
      do 90 i=lft,llt
      rx(i)=1./df(i)
      q1(i)=eps1(i)*sp1(i)+eps2(i)*sp4(i)+eps3(i)*sp6(i)
      q2(i)=eps2(i)*sp2(i)+eps1(i)*sp4(i)+eps3(i)*sp5(i)
      q3(i)=eps3(i)*sp3(i)+eps1(i)*sp6(i)+eps2(i)*sp5(i)
      q4(i)=eps4(i)*sp1(i)+eps5(i)*sp4(i)+eps6(i)*sp6(i)
      q5(i)=eps5(i)*sp2(i)+eps4(i)*sp4(i)+eps6(i)*sp5(i)
      q6(i)=eps6(i)*sp3(i)+eps4(i)*sp6(i)+eps5(i)*sp5(i)
      q7(i)=eps7(i)*sp1(i)+eps8(i)*sp4(i)+eps9(i)*sp6(i)
      q8(i)=eps8(i)*sp2(i)+eps7(i)*sp4(i)+eps9(i)*sp5(i)
   90 q9(i)=eps9(i)*sp3(i)+eps7(i)*sp6(i)+eps8(i)*sp5(i)
c
      do 100 i=lft,llt
      sig1(i)=rx(i)*(eps1(i)*q1(i)+eps2(i)*q2(i)+eps3(i)*q3(i))
      sig2(i)=rx(i)*(eps4(i)*q4(i)+eps5(i)*q5(i)+eps6(i)*q6(i))
      sig3(i)=rx(i)*(eps7(i)*q7(i)+eps8(i)*q8(i)+eps9(i)*q9(i))
      sig4(i)=rx(i)*(eps4(i)*q1(i)+eps5(i)*q2(i)+eps6(i)*q3(i))
      sig5(i)=rx(i)*(eps7(i)*q4(i)+eps8(i)*q5(i)+eps9(i)*q6(i))
      sig6(i)=rx(i)*(eps7(i)*q1(i)+eps8(i)*q2(i)+eps9(i)*q3(i))
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  100 cb(i)=  max(d(1,1),d(2,2),d(3,3))
c
      return
      end
      subroutine f3dm3 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic material with isotropic and kinematic hardening
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 da1(128),da2(128),da3(128),deps(128),ak2(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),ak(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),
     3 epx1(128),epx2(128),epx4(128),epx5(128),epx6(128)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/double/iprec,ncpw,unit
      dimension  cm(*),bqs(*)
      data third/.3333333333333/
c
      mx=48*(mxt(lft)-1)
      qh=cm(mx+26)
      qb=cm(mx+21)
      qs=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      gdt=dt1*g
      gd2=.5*gdt
      blk=-dt1*ym/((1.-2.*pr))
      qbqh=qb*qh
c
c     compute trial stress
c
      do 10 i=lft,llt
      cb(i)=ss
      davg(i)=third*dd(i)
      p(i)=blk*davg(i)
      po(i)=po(i)-p(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   10 continue
      do 30 i=lft,llt
      sig1(i)=sig1(i)+p(i)+gdt*(d1(i)+davg(i))
      sig2(i)=sig2(i)+p(i)+gdt*(d2(i)+davg(i))
      sig3(i)=sig3(i)+p(i)+gdt*(d3(i)+davg(i))
      sig4(i)=sig4(i)+gd2*d4(i)
      sig5(i)=sig5(i)+gd2*d5(i)
      sig6(i)=sig6(i)+gd2*d6(i)
      ak(i)=qs+qbqh*ep(i)
      q1(i)=po(i)+sig1(i)-epx1(i)
      q2(i)=po(i)+sig2(i)-epx2(i)
      q3(i)=po(i)+sig3(i)+epx1(i)+epx2(i)
      q4(i)=sig4(i)-epx4(i)
      q5(i)=sig5(i)-epx5(i)
   30 q6(i)=sig6(i)-epx6(i)
      do 40 i=lft,llt
      aj2(i)=q4(i)*q4(i)+q5(i)*q5(i)+q6(i)*q6(i)-q1(i)*q2(i)-q2(i)*q3(i)
     1      -q1(i)*q3(i)
      ak2(i)=3.0*aj2(i)-ak(i)*ak(i)
   40 scle(i)=.5*(1.0+sign(1.*unit,ak2(i)))
      fac1=1./(1.5*g+qh)
      fac2=1.5*g
      fac3=(1.-qb)*qh
      do 50 i=lft,llt
   50 aj1(i)=sqrt(3.0*abs(aj2(i)))+1.0-scle(i)
      do 60 i=lft,llt
      depi(i)=scle(i)*fac1*(aj1(i)-ak(i))
      ep(i)=ep(i)+depi(i)
      deps(i)=scle(i)*fac2*depi(i)/aj1(i)
   60 depn(i)=scle(i)*fac3*depi(i)/aj1(i)
      do 70 i=lft,llt
      sig1(i)=sig1(i)-deps(i)*q1(i)
      sig2(i)=sig2(i)-deps(i)*q2(i)
      sig3(i)=sig3(i)-deps(i)*q3(i)
      sig4(i)=sig4(i)-deps(i)*q4(i)
      sig5(i)=sig5(i)-deps(i)*q5(i)
      sig6(i)=sig6(i)-deps(i)*q6(i)
      epx1(i)=epx1(i)+depn(i)*q1(i)
      epx2(i)=epx2(i)+depn(i)*q2(i)
      epx4(i)=epx4(i)+depn(i)*q4(i)
      epx5(i)=epx5(i)+depn(i)*q5(i)
      epx6(i)=epx6(i)+depn(i)*q6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   70 continue
c
      return
      end
      subroutine f3dm4(cm,bqs,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     thermoelastic-plastic
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/bk28/summss,xke,xpe,tt
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 h1(128),h2(128),h3(128),h4(128),h5(128),h6(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 g1(128),g2(128),g3(128),g4(128),g5(128),g6(128),ep(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),
     3 tref(128),otmp(128),ctmp(128),ym(128),
     4 pv(128),qs(128),qh(128),ap(128),shrm(128)
      common/aux18/dd(128),def(128)
      common/aux19/q1(128),q2(128),q3(128),q4(128),scale(128),
     1 cc(128),ak(128)
      common/aux20/dymt(128),dpvt(128),dqst(128),dhmt(128),
     1 dalt(128),deltem(128),xmidtm(128),pres(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     & s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),
     & s7(128),c11i(128),c12i(128),d11t(128),d12t(128),d44t(128)
      common/aux41/qq1(128),cbb(128),sj2(128),teps(128)
      common/double/iprec,ncpw,unit
c
      dimension cm(*),bqs(*),tnew(1),fval(1),thrmo(5),thrmn(5)
c
      data zero /0.0/
c
      mx=48*(mxt(lft)-1)
c
c     temperature at element center
c
      if (itemp.lt.0) go to 20
      do 10 i=lft,llt
   10 ctmp(i)=fval(itemp)
      go to 40
   20 do 30 i=lft,llt
   30 ctmp(i)=.125*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i))+
     1              tnew(ix5(i))+tnew(ix6(i))+tnew(ix7(i))+tnew(ix8(i)))
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
      qs(i)=thrmn(4)
      qh(i)=.50*(thrmo(5)+thrmn(5))
      ap(i)=.50*(thrmo(3)+thrmn(3))
      dymt(i)=dti*(thrmn(1)-thrmo(1))
      dpvt(i)=dti*(thrmn(2)-thrmo(2))
      dqst(i)=dti*(thrmn(4)-thrmo(4))
      dhmt(i)=dti*(thrmn(5)-thrmo(5))
   60 dalt(i)=thrmn(3)-thrmo(3)
c
      do 70 i=lft,llt
      s1(i)=(1.+pv(i))*(1.0-2.0*pv(i))
      s2(i)=(1.-pv(i))/s1(i)
      s3(i)=ym(i)/(1.+pv(i))
      s4(i)=s3(i)/2.
      s5(i)=s3(i)/(1.-2.*pv(i))
      s6(i)=s5(i)*pv(i)
      s7(i)=s5(i)-s6(i)
      cb(i)=s6(i)+s3(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   70 continue
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
      do 120 i=lft,llt
      h1(i)=dt1*d1(i)-teps(i)
      h2(i)=dt1*d2(i)-teps(i)
      h3(i)=dt1*d3(i)-teps(i)
      h4(i)=dt1*d4(i)
      h5(i)=dt1*d5(i)
  120 h6(i)=dt1*d6(i)
c
      do 130 i=lft,llt
      sig1(i)=sig1(i)+s7(i)*h1(i)+s6(i)*h2(i)+s6(i)*h3(i)
      sig2(i)=sig2(i)+s6(i)*h1(i)+s7(i)*h2(i)+s6(i)*h3(i)
      sig3(i)=sig3(i)+s6(i)*h1(i)+s6(i)*h2(i)+s7(i)*h3(i)
      sig4(i)=sig4(i)+s4(i)*h4(i)
      sig5(i)=sig5(i)+s4(i)*h5(i)
  130 sig6(i)=sig6(i)+s4(i)*h6(i)
c
      do 140 i=lft,llt
      sig1(i)=sig1(i)+g1(i)*deltem(i)
      sig2(i)=sig2(i)+g2(i)*deltem(i)
      sig3(i)=sig3(i)+g3(i)*deltem(i)
      sig4(i)=sig4(i)+g4(i)*deltem(i)
      sig5(i)=sig5(i)+g5(i)*deltem(i)
  140 sig6(i)=sig6(i)+g6(i)*deltem(i)
c
      if (elas.eq.0.0) go to 210
c
c     scale back deviatoric stress to yield surface
c
      do 150 i=lft,llt
      shrm(i)=ym(i)/(2.*(1.+pv(i)))
      pres(i)=-(sig1(i)+sig2(i)+sig3(i))/3.0
  150 ak(i)=qs(i)+qh(i)*epx1(i)
      do 160 i=lft,llt
      sig1(i)=sig1(i)+pres(i)
      sig2(i)=sig2(i)+pres(i)
  160 sig3(i)=sig3(i)+pres(i)
      do 170 i=lft,llt
      aj2(i)=.5*(sig1(i)**2+sig2(i)**2+sig3(i)**2)+sig4(i)**2+sig5
     1 (i)**2+sig6(i)**2
      sj2(i)=sqrt(3.*aj2(i))
  170 scale(i)=ak(i)-sj2(i)
      do 180 i=lft,llt
      sj2(i)=  max(1.e-30*unit,sj2(i))
  180 cc(i)=sign(1.*unit,scale(i))
      do 190 i=lft,llt
  190 scale(i)=(1.-cc(i))*.5*ak(i)/sj2(i)+  max(zero,cc(i))
      do 200 i=lft,llt
      sig1(i)=scale(i)*sig1(i)-pres(i)
      sig2(i)=scale(i)*sig2(i)-pres(i)
      sig3(i)=scale(i)*sig3(i)-pres(i)
      sig4(i)=scale(i)*sig4(i)
      sig5(i)=scale(i)*sig5(i)
      sig6(i)=scale(i)*sig6(i)
      epx1(i)=epx1(i)+(1.-scale(i))*sj2(i)/(3.*shrm(i)+qh(i))
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  200 continue
c
  210 do 220 i=lft,llt
  220 otmp(i)=ctmp(i)
c
      return
      end
      subroutine itrpm(t,ym,temp,cm,thrml)
c     implicit double precision (a-h,o-z)                                    dp
      dimension t(*),ym(*),cm(8,*),thrml(*)
      do 10 l=2,8
      m=l-1
      if (ym(l).eq.0.0) go to 20
      if (temp.gt.t(l)) go to 10
      ratio=(temp-t(m))/(t(l)-t(m))
      go to 30
   10 continue
      m=m+1
   20 ratio=(temp-t(m-1))/(t(m)-t(m-1))
      m=m-1
   30 do 40 l=1,5
   40 thrml(l)=cm(m,l)+ratio*(cm(m+1,l)-cm(m,l))
      if (ratio.lt.1.05.and.ratio.gt.-.05) return
      write(13,50)
      write ( *,50)
      call adios (2)
c
   50 format(/' temperatures out-of-range')
      end
      subroutine f3dm5 (cm,bqs,ifail)
c     implicit double precision (a-h,o-z)                                    dp
c
c     soil and crushable foam
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 pold(128),p(128),g(128),bulk(128),a0(128),
     2 a1(128),a2(128),n(128),pfrac(128),
     3 gg(128),gg43(128),sv(128),svrt(128),ratio(128),
     4 yield2(128),aj2(128),g0(128)
      common/aux14/
     1 t1(128),t2(128),t3(128),t4(128),
     2 t5(128),t6(128),epx(128)
      common/aux18/dd(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),gg0(128),ee(128)
      common/aux36/lft,llt
c
      dimension  cm(*),bqs(*)
      data       third/0.33333333333333/
c
      do 10 i=lft,llt
      mx=48*(mxt(i)-1)
      g(i)=dt1*cm(mx+1)
      gg(i)=dt1*cm(mx+8)
      a0(i)=cm(mx+3)
      a1(i)=cm(mx+4)
      a2(i)=cm(mx+5)
      n(i)=cm(mx+7)
      bulk(i)=cm(mx+2)
      pfrac(i)=cm(mx+6)
   10 gg43(i)=cm(mx+10)
      do 20 i=lft,llt
      sv(i)=log(df(i))
      cxxa(i)=bulk(i)+gg43(i)
      pold(i)=-third*(t1(i)+t2(i)+t3(i))
      svrt(i)=third*(d1(i)+d2(i)+d3(i))
      einc(i)=(d1(i)*t1(i)+d2(i)*t2(i)+d3(i)*t3(i)+d4(i)*t4(i)
     1        +d5(i)*t5(i)+d6(i)*t6(i)+dd(i)*bqs(i))*dt1
   20 continue
c
c     decomposition into spherical and deviatoric parts
c
      do 30 i=lft,llt
      t1(i)=t1(i)+pold(i)
      t2(i)=t2(i)+pold(i)
   30 t3(i)=t3(i)+pold(i)
c
c     volumetric process
c
      do 200 i=lft,llt
      mx=48*(mxt(i)-1)
      if (sv(i).lt.epx(i)) go to 130
      if (epx(i)) 50,40,40
   40 epp=-pfrac(i)/bulk(i)
      go to 110
   50 j=2
   60 if (epx(i)-cm(mx+j+10)) 90,70,80
   70 p(i)=cm(mx+j+20)
      go to 100
c
c     interpolate p
c
   80 p(i)=cm(mx+j+19)+((cm(mx+j+20)-cm(mx+j+19))*(epx(i)-cm(mx+j+9)))/
     1 (cm(mx+j+10)-cm(mx+j+9))
      go to 100
   90 j=j+1
      if (j.le.n(i)) go to 60
c
c     extrapolate p
c
      j=n(i)
      go to 80
  100 epp=epx(i)+(p(i)-pfrac(i))/bulk(i)
  110 if (sv(i).gt.epp) go to 120
c
c     elastic volumetric process
c
      p(i)=pfrac(i)-bulk(i)*(sv(i)-epp)
      go to 200
c
c     tensile failure
c
  120 p(i)=pfrac(i)
      a0(i)=0.
      a1(i)=0.
      a2(i)=0.
      go to 200
c
c     plastic volumetric  process
c
  130 epx(i)=sv(i)
      if (sv(i)-0.0) 150,140,140
  140 if (cm(mx+11).ne.0.0) go to 150
      p(i)=cm(mx+21)
      go to 200
  150 j=2
  160 if (sv(i)-cm(mx+j+10)) 190,170,180
  170 p(i)=cm(mx+j+20)
      go to 200
c
c     interpolate p
c
  180 p(i)=cm(mx+j+19)+((cm(mx+j+20)-cm(mx+j+19))*(sv(i)-cm(mx+j+9)))/
     1 (cm(mx+j+10)-cm(mx+j+9))
      go to 200
  190 j=j+1
      if (j.le.n(i)) go to 160
c
c     extrapolate p
c
      j=n(i)
      go to 180
  200 continue
c
c     deviatoric process
c
      do 210 i=lft,llt
      t1(i)=t1(i)+gg(i)*(d1(i)-svrt(i))
      t2(i)=t2(i)+gg(i)*(d2(i)-svrt(i))
      t3(i)=t3(i)+gg(i)*(d3(i)-svrt(i))
      t4(i)=t4(i)+g(i)*d4(i)
      t5(i)=t5(i)+g(i)*d5(i)
  210 t6(i)=t6(i)+g(i)*d6(i)
      do 220 i=lft,llt
      aj2(i)=.5*(t1(i)**2+t2(i)**2+t3(i)**2)+t4(i)**2+t5(i)**2+t6(i)**2
      g0(i)=a0(i)+a1(i)*p(i)+a2(i)*p(i)*p(i)
  220 yield2(i)=aj2(i)-g0(i)
      do 260 i=lft,llt
      if (yield2(i)) 230,240,250
  230 ratio(i)=1.0
      go to 260
  240 ratio(i)=0.0
      go to 260
  250 ratio(i)=sqrt(g0(i)/(aj2(i)+1.e-10))
  260 continue
c
      do 270 i=lft,llt
      t1(i)=ratio(i)*t1(i)-p(i)
      t2(i)=ratio(i)*t2(i)-p(i)
      t3(i)=ratio(i)*t3(i)-p(i)
      t4(i)=ratio(i)*t4(i)
      t5(i)=ratio(i)*t5(i)
      t6(i)=ratio(i)*t6(i)
  270 continue
      if (ifail.eq.1) return
      do 280 i=lft,llt
      einc(i)=(d1(i)*t1(i)+d2(i)*t2(i)+d3(i)*t3(i)+d4(i)*t4(i)
     1        +d5(i)*t5(i)+d6(i)*t6(i))*dt1+einc(i)
  280 continue
c
      return
c
      end
      subroutine f3dm6 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... combined routine  2-14-90
c
c     viscoelastic behavior
c
c            cm(1)=bulk modulus,qk
c            cm(2)=short time shear modulus,qs
c            cm(3)=long time shear modulus,ql
c            cm(4)=decay constant,qb
c
c     l.r.herrmann and f.e.peterson, a numerical procedure for
c     viscoelastic stress analysis,proccmdings of the seventh i.c.r.p.g.
c     mechanical behavior working group mcmting,c.p.i.a.publication
c     no.177,(october 1968).
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 tem(128),de(128,6),t(128,6)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &ei(128,6)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),g(128),ee(128)
      common/aux36/lft,llt
      dimension cm(*),bqs(*),sig(128,6)
      dimension s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),
     1 pres(128),epsvol(128)
      equivalence (sig,sig1)
c
      data       third/0.33333333333333/
c
      mx=48*(mxt(lft)-1)
      ityp=cm(mx+21)
      if (ityp.eq.0) then
c
c.... dyna3d viscoelastic model
c
      qk=cm(mx+1)
      qs=cm(mx+6)
      ql=cm(mx+11)
      qb=cm(mx+16)
      qd=qs-ql
      q1=exp(-qb*dt1)
      q2=(1.0-q1)/qb
      q3=2.*ql*dt1
      q4=(3.*qk-2.*ql)*dt1
      q5=qk+1.34*qs
      q6=ql*dt1
c
      do 10 i=lft,llt
   10 tem(i)=(d1(i)+d2(i)+d3(i))*third
c
      do 20 i=lft,llt
      de(i,1)=d1(i)-tem(i)
      de(i,2)=d2(i)-tem(i)
      de(i,3)=d3(i)-tem(i)
      de(i,4)=.5*d4(i)
      de(i,5)=.5*d5(i)
      de(i,6)=.5*d6(i)
      tem(i)=q4*tem(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   20 continue
c
      do 30 j=1,6
      do 30 i=lft,llt
      t(i,j)=ei(i,j)
   30 ei(i,j)=q1*ei(i,j)+q2*de(i,j)
c
c     update stress
c
      do 50 i=lft,llt
      sig1(i)=sig1(i)+tem(i)+q3*d1(i)
      sig2(i)=sig2(i)+tem(i)+q3*d2(i)
      sig3(i)=sig3(i)+tem(i)+q3*d3(i)
      sig4(i)=sig4(i)+q6*d4(i)
      sig5(i)=sig5(i)+q6*d5(i)
      sig6(i)=sig6(i)+q6*d6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
      cxxa(i)=q5*def(i)
   50 continue
c
      qd=2.*qd
      do 60 j=1,6
      do 60 i=lft,llt
      sig(i,j)=sig(i,j)+qd*(ei(i,j)-t(i,j))
   60 continue
c
      return
      else
c
c
c.... kelvin-voigt 3 element viscoelastic model
c          robert whirley and bruce engelmann
c                 january, 1990
c
      qk=cm(mx+1)
      qg=cm(mx+6)
      qr=cm(mx+11)
      qtau=cm(mx+16)
      do 110 i=lft,llt
      pres(i)=(sig1(i)+sig2(i)+sig3(i))/3.
      epsvol(i)=(d1(i)+d2(i)+d3(i))*third
  110 continue
      do 120 i=lft,llt
      de(i,1)=d1(i)-epsvol(i)
      de(i,2)=d2(i)-epsvol(i)
      de(i,3)=d3(i)-epsvol(i)
      de(i,4)=d4(i)
      de(i,5)=d5(i)
      de(i,6)=d6(i)
      s1(i)=sig1(i)-pres(i)
      s2(i)=sig2(i)-pres(i)
      s3(i)=sig3(i)-pres(i)
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
c
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
  120 continue
      c1n=2.*qg
      c1s=qg
      c2n=2.*qr
      c2s=qr
      fac1=exp(-1./qtau*dt1)
      do 130 i=lft,llt
      s1(i) = (s1(i)-qtau*de(i,1)*(c1n-c2n) - c2n*ei(i,1))*fac1
     1 + qtau*de(i,1)*(c1n-c2n) + c2n*(ei(i,1) + de(i,1)*dt1)
      s2(i) = (s2(i)-qtau*de(i,2)*(c1n-c2n) - c2n*ei(i,2))*fac1
     1 + qtau*de(i,2)*(c1n-c2n) + c2n*(ei(i,2) + de(i,2)*dt1)
      s3(i) = (s3(i)-qtau*de(i,3)*(c1n-c2n) - c2n*ei(i,3))*fac1
     1 + qtau*de(i,3)*(c1n-c2n) + c2n*(ei(i,3) + de(i,3)*dt1)
c
      s4(i) = (s4(i)-qtau*de(i,4)*(c1s-c2s) - c2s*ei(i,4))*fac1
     1 + qtau*de(i,4)*(c1s-c2s) + c2s*(ei(i,4) + de(i,4)*dt1)
      s5(i) = (s5(i)-qtau*de(i,5)*(c1s-c2s) - c2s*ei(i,5))*fac1
     1 + qtau*de(i,5)*(c1s-c2s) + c2s*(ei(i,5) + de(i,5)*dt1)
      s6(i) = (s6(i)-qtau*de(i,6)*(c1s-c2s) - c2s*ei(i,6))*fac1
     1 + qtau*de(i,6)*(c1s-c2s) + c2s*(ei(i,6) + de(i,6)*dt1)
c
  130 continue
c
      do 140 i=lft,llt
      pres(i) = pres(i) + 3.*qk*epsvol(i)*dt1
  140 continue
c
      tcor=(s1(i)+s2(i)+s3(i))/3.
      s1(i)=s1(i)-tcor
      s2(i)=s2(i)-tcor
      s3(i)=s3(i)-tcor
  145 continue
      do 150 i=lft,llt
      sig1(i)=s1(i) + pres(i)
      sig2(i)=s2(i) + pres(i)
      sig3(i)=s3(i) + pres(i)
      sig4(i)=s4(i)
      sig5(i)=s5(i)
      sig6(i)=s6(i)
c
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
      cxxa(i)=(qk+4./3.*qg)*def(i)
  150 continue
      do 170 j=1,6
      do 160 i=lft,llt
      ei(i,j)=ei(i,j)+de(i,j)*dt1
  160 continue
  170 continue
c
      return
      endif
      end
      subroutine f3dm7 (cm,bqs,ihg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     finite elastic continuum rubber
c
c     p.j.blatz and w.l.ko, application of finite elastic theory to the
c     deformation of rubbery materials,transactions of the society of
c     rheology,vol.vi,223-251,(1962).
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &px5(128),px6(128),px7(128),px8(128),py5(128),py6(128),
     &py7(128),py8(128),pz5(128),pz6(128),pz7(128),pz8(128),
     &x17(128),x28(128),x35(128),x46(128),y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),df31(128),
     &c11(128),c22(128),c33(128),
     &c44(128),c55(128),c66(128),c12(128),
     &c13(128),c23(128),df2(128),c11c(128),c22c(128),c33c(128),
     &rx(128),sp1(128),sp2(128),sp3(128),
     &sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),q1(128),q2(128),q3(128),
     & q4(128),q5(128),q6(128),q7(128),q8(128),q9(128),qf(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/eg1(128),eg2(128),eg3(128),eg4(128),eg5(128),
     1 eg6(128)
      dimension cm(*),bqs(*)
c
c     compute deformation gradients in undeformed configuration
c
      mx=48*(mxt(lft)-1)
      qs=cm(mx+1)
      pr=cm(mx+2)
      if (ihg.ne.3.and.ihg.lt.5) then
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
      z46(i)=z4(i)-z6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dde(i)*bqs(i))*dt1
   20 continue
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
      else
      do 31  i=lft,llt
      eps1(i)=px1(i)*x1(i)+px2(i)*x2(i)+px3(i)*x3(i)+px4(i)*x4(i)+
     & px5(i)*x5(i)+px6(i)*x6(i)+px7(i)*x7(i)+px8(i)*x8(i)
      eps5(i)=py1(i)*y1(i)+py2(i)*y2(i)+py3(i)*y3(i)+py4(i)*y4(i)+
     & py5(i)*y5(i)+py6(i)*y6(i)+py7(i)*y7(i)+py8(i)*y8(i)
      eps9(i)=pz1(i)*z1(i)+pz2(i)*z2(i)+pz3(i)*z3(i)+pz4(i)*z4(i)+
     & pz5(i)*z5(i)+pz6(i)*z6(i)+pz7(i)*z7(i)+pz8(i)*z8(i)
   31 eps2(i)=py1(i)*x1(i)+py2(i)*x2(i)+py3(i)*x3(i)+py4(i)*x4(i)+
     & py5(i)*x5(i)+py6(i)*x6(i)+py7(i)*x7(i)+py8(i)*x8(i)
      do 33 i=lft,llt
      eps3(i)=pz1(i)*x1(i)+pz2(i)*x2(i)+pz3(i)*x3(i)+pz4(i)*x4(i)+
     & pz5(i)*x5(i)+pz6(i)*x6(i)+pz7(i)*x7(i)+pz8(i)*x8(i)
      eps6(i)=pz1(i)*y1(i)+pz2(i)*y2(i)+pz3(i)*y3(i)+pz4(i)*y4(i)+
     & pz5(i)*y5(i)+pz6(i)*y6(i)+pz7(i)*y7(i)+pz8(i)*y8(i)
      eps4(i)=px1(i)*y1(i)+px2(i)*y2(i)+px3(i)*y3(i)+px4(i)*y4(i)+
     & px5(i)*y5(i)+px6(i)*y6(i)+px7(i)*y7(i)+px8(i)*y8(i)
      eps7(i)=px1(i)*z1(i)+px2(i)*z2(i)+px3(i)*z3(i)+px4(i)*z4(i)+
     & px5(i)*z5(i)+px6(i)*z6(i)+px7(i)*z7(i)+px8(i)*z8(i)
   33 eps8(i)=py1(i)*z1(i)+py2(i)*z2(i)+py3(i)*z3(i)+py4(i)*z4(i)+
     & py5(i)*z5(i)+py6(i)*z6(i)+py7(i)*z7(i)+py8(i)*z8(i)
      endif
c
c     left cauchy-green tensor
c
      do 40 i=lft,llt
      eg1(i)=eps1(i)*eps1(i)+eps2(i)*eps2(i)+eps3(i)*eps3(i)
      eg2(i)=eps4(i)*eps4(i)+eps5(i)*eps5(i)+eps6(i)*eps6(i)
      eg3(i)=eps7(i)*eps7(i)+eps8(i)*eps8(i)+eps9(i)*eps9(i)
      eg4(i)=eps1(i)*eps4(i)+eps2(i)*eps5(i)+eps3(i)*eps6(i)
      eg5(i)=eps4(i)*eps7(i)+eps5(i)*eps8(i)+eps6(i)*eps9(i)
      eg6(i)=eps1(i)*eps7(i)+eps2(i)*eps8(i)+eps3(i)*eps9(i)
   40 continue
c
c     cauchy stress
c
      factor=1./(1.-2.*pr)
      sounds=qs*(1.+pr)*factor +4.*qs/3.
      do 50 i=lft,llt
      rx(i)=qs/df(i)
      qf(i)=qs*(1./df(i))**factor
      sig1(i)=eg1(i)*rx(i)-qf(i)
      sig2(i)=eg2(i)*rx(i)-qf(i)
      sig3(i)=eg3(i)*rx(i)-qf(i)
      sig4(i)=eg4(i)*rx(i)
      sig5(i)=eg5(i)*rx(i)
      sig6(i)=eg6(i)*rx(i)
      cb(i)=sounds
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   50 continue
c
      return
      end
      subroutine f3dm8 (cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk28/summss,xke,xpe,tt
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 w1df(128),facm(128),pnew(128)
      common/aux9/vlrho(128),volnew(128)
      common/aux11/po(128),
     1 sgv1(128),sgv2(128),sgv3(128),sgv4(128),
     2 sgv5(128),sgv6(128),volold(128),einc(128),e1(128),
     3 p(128),e4(128),e5(128),e6(128),specen(128),dvol(128)
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),
     3 epx1(128),epx2(128),epx3(128),epx4(128),epx5(128,5),
     4 vr1(128),vr2(128),bfc1(128),bfc2(128)
      common/aux18/dde(128),df(128)
      common/aux19/
     & sp,bfac(128),dr1v(128),dr2v(128),w1,r1v(128),r2v(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux34/xioff(128),dxl(128)
      common/aux35/rhoa(128),cb(128)
      common/aux36/lft,llt
c
      dimension  cm(1)
c
      mx=48*(mxt(lft)-1)
      bhe=cm(mx+1)
      bbn=cm(mx+3)
      sp=cm(mx+30)
c
      if (bbn.eq.1.0) go to 20
      do 10 i=lft,llt
      tb=-epx1(i)
      bffc1=0.0
      if (tt.gt.tb) bffc1=sp*(tt-tb)/1.5/dxl(i)
      bffc2=bhe*(1.-df(i))
      bfac(i)=  max(bffc1,bffc2)
      if (bfac(i).lt.0.0001) bfac(i)=0.0
      if (bfac(i).lt.1.0) go to 10
      bfac(i)=1.0
      epx1(i)=1.0
   10 continue
      go to 70
   20 do 60 i=lft,llt
      bfcod=epx1(i)
      if (bfcod.eq.1.0) go to 40
      bfac(i)=bhe*(1.-df(i))
      if (bfcod.ge.0.0)  go to 30
      if (bfac(i).gt.0.0001) go to 30
      bfac(i)=0.0
      go to 60
   30 if (bfac(i).lt.0.0001) bfac(i)=0.0
      if (bfac(i).gt.0.9500) bfac(i)=1.0
      if (bfac(i).lt.bfcod.and.bfcod.gt.0.05) bfac(i)=1.0
      if (bfac(i).lt.bfcod.and.bfcod.le.0.05) bfac(i)=0.0
      go to 50
   40 bfac(i)=bfcod
   50 epx1(i)=bfac(i)
   60 continue
   70 do 80 i=lft,llt
      sgv1(i)=sign1(i)
      sgv2(i)=sign2(i)
      sgv3(i)=sign3(i)
      sgv4(i)=0.0
      sgv5(i)=0.0
      sgv6(i)=0.0
  80    sign1(i)=0.0
      do 81 i=lft,llt
      sign2(i)=0.0
      sign3(i)=0.0
      sign4(i)=0.0
      sign5(i)=0.0
   81 sign6(i)=0.0
c
      return
c
      end
      subroutine f3dm9(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),epx1(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/eosd/pc(128),shrm(128)
      common/aux36/lft,llt
      dimension  cm(*)
      data third/.3333333333333/
      sp=0.0
      mx=48*(mxt(lft)-1)
      xmu=cm(mx+2)
      if (xmu.eq.0.0) then
      do 10 i=lft,llt
      shrm(i)=0.
      pc(i)=cm(mx+1)
      sign1(i)=0.
      sign2(i)=0.
      sign3(i)=0.
      sign4(i)=0.
      sign5(i)=0.
   10 sign6(i)=0.
      else
      do 20 i=lft,llt
      shrm(i)=0.
      pc(i)=cm(mx+1)
      davg=-third*(d1(i)+d2(i)+d3(i))
      sign1(i)=xmu*(d1(i)+davg)
      sign2(i)=xmu*(d2(i)+davg)
      sign3(i)=xmu*(d3(i)+davg)
      sign4(i)=.50*xmu*d4(i)
      sign5(i)=.50*xmu*d5(i)
      sign6(i)=.50*xmu*d6(i)
   20 continue
      endif
      return
      end
      subroutine f3dm10(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128)
      common/aux14/
     &sign1(128),sign2(128),sign3(128),sign4(128),
     &sign5(128),sign6(128),
     &epx1(128),epx2(128),epx3(128),epx4(128),epx5(128),aux(128,5)
      common/aux18/dd(128),df(128)
      common/aux19/sp,bfac(128),dr1v(128),dr2v(128),w1
      common/aux20/
     & aj2(128),sj2(128),scale(128),fjl(128),cc(128),p(128),
     & pold(128),davg(128),fjk(128),specen(128),ak(128),akt(128),
     & ywh(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128)
      common/aux36/lft,llt
      common/eosd/pc(128),shrm(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1)
      data third/.3333333333333/
      data zero /0.0/
      mx=48*(mxt(lft)-1)
      sp=0.0
      g=cm(mx+1)
      qh=cm(mx+3)
      qs=cm(mx+2)
      a1=cm(mx+5)
      a2=cm(mx+6)
      ispall=cm(mx+8)-1.
      npt=cm(mx+16)
      g2=2.*dt1*g
      do 10 i=lft,llt
   10 davg(i)=-third*(d1(i)+d2(i)+d3(i))
      do 20 i=lft,llt
      shrm(i)=g
      akt(i)=qh
      pc(i)=epx5(i)
   20 ak(i)=qs+qh*epx1(i)+(a1+a2*po(i))*  max(zero,po(i))
      do 30 i=lft,llt
      sign1(i)=sign1(i)+po(i)+g2*(d1(i)+davg(i))
      sign2(i)=sign2(i)+po(i)+g2*(d2(i)+davg(i))
      sign3(i)=sign3(i)+po(i)+g2*(d3(i)+davg(i))
      sign4(i)=sign4(i)+.50*g2*d4(i)
      sign5(i)=sign5(i)+.50*g2*d5(i)
   30 sign6(i)=sign6(i)+.50*g2*d6(i)
      if (ispall.ne.2) go to 80
      do 40 i=lft,llt
   40 davg(i)=po(i)-epx5(i)
      do 50 i=lft,llt
   50 scale(i)=.50*(1.+sign(1.*unit,davg(i)))
      do 60 i=lft,llt
      pc(i)=scale(i)*epx5(i)
      sign1(i)=scale(i)*sign1(i)
      sign2(i)=scale(i)*sign2(i)
      sign3(i)=scale(i)*sign3(i)
      sign4(i)=scale(i)*sign4(i)
      sign5(i)=scale(i)*sign5(i)
   60 sign6(i)=scale(i)*sign6(i)
      do 70 i=lft,llt
   70 epx5(i)=pc(i)
   80 if (npt.eq.0) go to 100
      call yields(npt,cm(mx+17),cm(mx+33),epx1,ak,akt,lft,llt)
  100 do 110 i=lft,llt
      aj2(i)=.5*(sign1(i)**2+sign2(i)**2+sign3(i)**2)+sign4(i)**2+sign5
     1 (i)**2+sign6(i)**2
      sj2(i)=sqrt(3.*aj2(i))
  110 continue
c.... calculate increment in effective plastic strain & new yield stress
      thrg=third/g
      do 120 i=lft,llt
      cc(i)=dim(sj2(i),ak(i))/(1.e-30+dim(1.*unit,-thrg*akt(i)))
      cc(i)=thrg*  min(sj2(i),cc(i))
      ak(i)=  max(ak(i)+akt(i)*cc(i),zero)
  120 continue
c.... calculate factor to scale stresses back to yield surface
      seps=1.e-30*g
      do 130 i=lft,llt
      scale(i)=(ak(i)+seps)/(  max(ak(i),sj2(i))+seps)
  130 continue
c.... scale back stress and increment plastic strain
      do 140 i=lft,llt
      sign1(i)=scale(i)*sign1(i)
      sign2(i)=scale(i)*sign2(i)
      sign3(i)=scale(i)*sign3(i)
      sign4(i)=scale(i)*sign4(i)
      sign5(i)=scale(i)*sign5(i)
      sign6(i)=scale(i)*sign6(i)
      epx1(i)=epx1(i)+cc(i)
  140 continue
      if(ispall.ne.1) return
      do 150 i=lft,llt
      aj2(i)=.5*(sign1(i)**2+sign2(i)**2+sign3(i)**2)+sign4(i)**2+sign5
     1 (i)**2+sign6(i)**2+1.e-12
  150 sj2(i)=sign1(i)*sign5(i)**2+sign2(i)*sign6(i)**2+sign3(i)*sign4(i)
     1      **2-sign1(i)*sign2(i)*sign3(i)-2.*sign4(i)*sign5(i)*sign6(i)
      do 160 i=lft,llt
      akt(i)=-sqrt(27./aj2(i))*sj2(i)*0.5/aj2(i)
      akt(i)=sign(min(abs(akt(i)),1.*unit),akt(i))
  160 ywh(i)=acos(akt(i))*third
      do 170 i=lft,llt
  170 sj2(i)=2.*sqrt(aj2(i)*third)*cos(ywh(i))
      do 180 i=lft,llt
  180 davg(i)=po(i)-sj2(i)-epx5(i)
      do 190 i=lft,llt
  190 scale(i)=.50*(1.+sign(1.*unit,davg(i)))
      do 200 i=lft,llt
      pc(i)   =scale(i)*epx5(i)
      sign1(i)=scale(i)*sign1(i)
      sign2(i)=scale(i)*sign2(i)
      sign3(i)=scale(i)*sign3(i)
      sign4(i)=scale(i)*sign4(i)
      sign5(i)=scale(i)*sign5(i)
  200 sign6(i)=scale(i)*sign6(i)
      do 210 i=lft,llt
  210 epx5(i)=pc(i)
      return
      end
      subroutine f3dm11(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     steinberg-guinan high strain rate constitutive model
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux9/vlrho(128),volnew(128)
      common/aux11/p(128)
      common/aux14/
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),epx1(128),
     3 epx2(128),epx3(128),epx4(128),epx5(128),aux(128,10),
     4 tmelt(128),sa(128),sb(128),sc(128),scale(128)
      common/aux18/dd(128),df(128)
      common/aux19/sp,sgnf(128),ywh(128),coef(128),pdft(128),
     1 akt(128),gt(128),cc6(128),xmu(128)
      common/aux20/
     & aj2(128),sj2(128),shrm(128),ecold(128),emelt(128),
     & x(128),cc(128),davg(128),g2(128),specen(128),ak(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxxa(128)
      common/aux36/lft,llt
      common/aux39/temp(128)
      common/aux42/elmu,yelc,yb,yc,gamo,ywhmx,au,ye,bu,yp,ayz,tmyz,
     1 gzyz,wyz,varib(8),c0,c1,c2,c3,c4,c5,c6,c7,c8,c9
      common/eosd/pc(128),g(128)
      common/double/iprec,ncpw,unit
c
      dimension  cm(1),a(1),b(1),e4567(1),econ(32)
      equivalence (aj2,a),(sj2,b),(ak,e4567),(elmu,econ)
      data othrd/.33333333333333/,r/.00008314/
      data zero,one/0.0,1.0/
c
      mx=48*(mxt(lft)-1)
      sp=0.0
      do 10 j=1,32
   10 econ(j)=cm(mx+j)
      qh=0.
      rp3=3.0*r*rho(lft)/ayz
      ispall=econ(16)-1.
c
c calculate compression (eta) and check for etamin , etamax .
c
      if (varib(6) .eq. 1.0) then
        etamin = varib(7) - 1.0
        etamax = varib(8) - 1.0
        do 20 i=lft,llt
          x(i) = 1. / min(one,df(i)) - one
 20     continue
      else
        etamin = varib(7)
        etamax = varib(8)
        do 22 i=lft,llt
          x(i) = 1. /min(one,df(i))
 22     continue
      endif
c
      if (etamin .ne. etamax) then
        do 25 i=lft,llt
          x(i) =   max(x(i),etamin)
          x(i) =   min(x(i),etamax)
 25     continue
      endif
c
      do 30 i=lft,llt
   30 e4567(i)=x(i)*(c4+x(i)*(c5+x(i)*(c6+x(i)*(c7+x(i)*(c8+x(i)*c9)))))
      do 40 i=lft,llt
   40 ecold(i)=c0+x(i)*(c1+x(i)*(c2+x(i)*(c3+e4567(i))))
      c1=2.0*(gzyz-wyz-othrd)
      wyz2=2.*wyz
      do 50 i=lft,llt
   50 tmelt(i)=tmyz*exp(wyz2*(one-min(one,df(i))))/(min(one,df(i))**c1)
      do 60 i=lft,llt
      emelt(i)=ecold(i)+rp3*tmelt(i)
      sign1(i)=sign1(i)+p(i)
      sign2(i)=sign2(i)+p(i)
      sign3(i)=sign3(i)+p(i)
   60 specen(i)=df(i)*epx2(i)/volnew(i)
      do 70 i=lft,llt
   70 g2(i)=emelt(i)-specen(i)
      do 80 i=lft,llt
      g(i)=0.0
      ak(i)=0.0
      sgnf(i)=.5*(1.+sign(1.*unit,g2(i)))
   80 temp(i)=(specen(i)-ecold(i))/rp3-300.
      do 90 i=lft,llt
      ywh(i)=yelc*(1.+yb*epx1(i))**yc
      ywh(i)=  min(ywh(i),ywhmx)
      coef(i)=exp(-yp*specen(i)*sgnf(i)/g2(i))
      pdft(i)=p(i)*df(i)**othrd
      akt(i)=ywh(i)*(1.+ye*pdft(i)-bu*temp(i))*coef(i)
      ak(i)=sgnf(i)*  max(akt(i),.5*yelc)
      gt(i)=elmu*(1.+au*pdft(i)-bu*temp(i))*coef(i)
   90 g(i)=sgnf(i)*  max(gt(i),.5*elmu)
      do 100 i=lft,llt
      g2(i)=2.0*dt1*g(i)
      temp(i)=specen(i)
  100 davg(i)=-othrd*(d1(i)+d2(i)+d3(i))
      do 110 i=lft,llt
      sign1(i)=sign1(i)+g2(i)*(d1(i)+davg(i))
      sign2(i)=sign2(i)+g2(i)*(d2(i)+davg(i))
      sign3(i)=sign3(i)+g2(i)*(d3(i)+davg(i))
      sign4(i)=sign4(i)+.50*g2(i)*d4(i)
      sign5(i)=sign5(i)+.50*g2(i)*d5(i)
  110 sign6(i)=sign6(i)+.50*g2(i)*d6(i)
      if (ispall.eq.1) go to 180
      if (ispall.eq.2) go to 130
      do 120 i=lft,llt
  120 pc(i)=epx5(i)
      go to 180
  130 do 140 i=lft,llt
  140 davg(i)=p(i)-epx5(i)
      do 150 i=lft,llt
  150 g2(i)=.50*(1.+sign(1.*unit,davg(i)))
      do 160 i=lft,llt
      pc(i)=g2(i)*epx5(i)
      sign1(i)=g2(i)*sign1(i)
      sign2(i)=g2(i)*sign2(i)
      sign3(i)=g2(i)*sign3(i)
      sign4(i)=g2(i)*sign4(i)
      sign5(i)=g2(i)*sign5(i)
  160 sign6(i)=g2(i)*sign6(i)
      do 170 i=lft,llt
  170 epx5(i)=pc(i)
  180 do 190 i=lft,llt
      aj2(i)=.5*(sign1(i)**2+sign2(i)**2+sign3(i)**2)+sign4(i)**2+sign5
     1 (i)**2+sign6(i)**2
      sj2(i)=sqrt(3.*aj2(i))
  190 sa(i)=ak(i)-sj2(i)
      do 200 i=lft,llt
      sj2(i)=  max(1.e-30*unit,sj2(i))
  200 sc(i)=sign(1.*unit,sa(i))
      do 210 i=lft,llt
  210 sa(i)=(1.-sc(i))*.5*ak(i)/sj2(i)+  max(zero,sc(i))
      do 220 i=lft,llt
      sign1(i)=sa(i)*sign1(i)
      sign2(i)=sa(i)*sign2(i)
      sign3(i)=sa(i)*sign3(i)
      sign4(i)=sa(i)*sign4(i)
      sign5(i)=sa(i)*sign5(i)
      sign6(i)=sa(i)*sign6(i)
  220 epx1(i)=epx1(i)+(1.-sa(i))*sj2(i)/(3.*g(i)+1.e-10)
      if(ispall.ne.1) return
      do 230 i=lft,llt
      aj2(i)=.5*(sign1(i)**2+sign2(i)**2+sign3(i)**2)+sign4(i)**2+sign5
     1 (i)**2+sign6(i)**2+1.e-12
  230 sj2(i)=sign1(i)*sign5(i)**2+sign2(i)*sign6(i)**2+sign3(i)*sign4(i)
     1      **2-sign1(i)*sign2(i)*sign3(i)-2.*sign4(i)*sign5(i)*sign6(i)
      do 240 i=lft,llt
      akt(i)=-sqrt(27./aj2(i))*sj2(i)*0.5/aj2(i)
      akt(i)=sign(  min(abs(akt(i)),one),akt(i))
  240 ywh(i)=acos(akt(i))*othrd
      do 250 i=lft,llt
  250 sj2(i)=2.*sqrt(aj2(i)*othrd)*cos(ywh(i))
      do 260 i=lft,llt
  260 davg(i)=p(i)-sj2(i)-epx5(i)
      do 270 i=lft,llt
  270 g2(i)=.50*(1.+sign(1.*unit,davg(i)))
      do 280 i=lft,llt
      pc(i)   =g2(i)*epx5(i)
      sign1(i)=g2(i)*sign1(i)
      sign2(i)=g2(i)*sign2(i)
      sign3(i)=g2(i)*sign3(i)
      sign4(i)=g2(i)*sign4(i)
      sign5(i)=g2(i)*sign5(i)
  280 sign6(i)=g2(i)*sign6(i)
      do 290 i=lft,llt
  290 epx5(i)=pc(i)
      return
      end
      subroutine f3dm12 (cm,bqs,ifail)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic - plastic material with isotropic hardening
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128),einc(128)
      common/aux11/pold(128),
     1 pnew(128),cc(128),scale(128),aj2(128),ak(128),davg(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128)
      common/aux18/dd(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),sj2(128)
      common/aux36/lft,llt
      common/mat12/g0,qs,qh,ee,xjnk1,xjnk2,bk
      common/double/iprec,ncpw,unit
      dimension cm(*),bqs(*),cmat(7)
      equivalence (g0,cmat)
      data zero/0.0/
c
c     set material constant arrays
c
      mx=48*(mxt(lft)-1)
      do 10 i=1,7
   10 cmat(i)=cm(mx+i)
      g=dt1*g0
      g2=2.0*g
c
c     update elastic stresses
c
      do 20 i=lft,llt
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
      davg(i)=dd(i)*.33333333333333
      sig1(i)=pold(i)+sig1(i)+g2*(d1(i)+davg(i))
      sig2(i)=pold(i)+sig2(i)+g2*(d2(i)+davg(i))
      sig3(i)=pold(i)+sig3(i)+g2*(d3(i)+davg(i))
      sig4(i)=sig4(i)+g*d4(i)
      sig5(i)=sig5(i)+g*d5(i)
      sig6(i)=sig6(i)+g*d6(i)
   20 continue
c
      bkdt=dt1*bk
      do 30 i=lft,llt
      ak(i)=qh*epx1(i)+qs
      pnew(i)=pold(i)+bkdt*dd(i)
   30 cxxa(i)=ee*df(i)
c
      do 40 i=lft,llt
      aj2(i)=.50*(sig1(i)**2+sig2(i)**2+sig3(i)**2)+sig4(i)**2+sig5(i)**
     1 2+sig6(i)**2
      sj2(i)=sqrt(3.*aj2(i))
   40 scale(i)=ak(i)-sj2(i)
      do 50 i=lft,llt
      sj2(i)=  max(1.e-30*unit,sj2(i))
   50 cc(i)=sign(1.*unit,scale(i))
      do 60 i=lft,llt
   60 scale(i)=(1.-cc(i))*.5*ak(i)/sj2(i)+  max(zero,cc(i))
      check=0.
      do 70 i=lft,llt
   70 check=check+scale(i)
      chk=1.+llt-lft-.000001
      if (check.gt.chk) go to 90
      g3qh=1./(3.*g0+qh)
      do 80 i=lft,llt
      sig1(i)=scale(i)*sig1(i)
      sig2(i)=scale(i)*sig2(i)
      sig3(i)=scale(i)*sig3(i)
      sig4(i)=scale(i)*sig4(i)
      sig5(i)=scale(i)*sig5(i)
      sig6(i)=scale(i)*sig6(i)
   80 epx1(i)=epx1(i)+(1.-scale(i))*sj2(i)*g3qh
   90 if (ifail.eq.1) return
      do 100 i=lft,llt
      sig1(i)=sig1(i)-pnew(i)
      sig2(i)=sig2(i)-pnew(i)
      sig3(i)=sig3(i)-pnew(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  100 continue
c
      return
      end
      subroutine f3dm13 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     elastic-plastic with failure
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128),einc(128)
      common/aux11/po(128),
     1 pnew(128),qh(128),qs(128),bulkm(128),sj2(128),
     2 aj2(128),pold(128),ak(128),davg(128),t1(128),
     3 t2(128),t3(128),t4(128),t5(128),t6(128),pf(128),ef(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),ax(128,1)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      dimension cm(*),bqs(*)
      call f3dm12 (cm,bqs,1)
      do 5 i=lft,llt
      sig1(i)=sig1(i)-pnew(i)
      sig2(i)=sig2(i)-pnew(i)
      sig3(i)=sig3(i)-pnew(i)
    5 continue
      do 10 i=lft,llt
      mx=48*(mxt(i)-1)
   10 ef(i)=cm(mx+8)
      do 40 i=lft,llt
      if (pnew(i).lt.ax(i,1)) go to 20
      if (epx(i).gt.ef(i)) go to 30
      go to 40
   20 ax(i,1)=0.0
      sig1(i)=0.0
      sig2(i)=0.0
      sig3(i)=0.0
      sig4(i)=0.0
      sig5(i)=0.0
      sig6(i)=0.0
      epx(i)=ef(i)+.0000001
      go to 40
   30 sig1(i)=-pnew(i)
      sig2(i)=-pnew(i)
      sig3(i)=-pnew(i)
      sig4(i)=0.0
      sig5(i)=0.0
      sig6(i)=0.0
      epx(i)=ef(i)+.0000001
   40 einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
      return
      end
      subroutine f3dm14 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     soil and crushable foam with failure model
c         modified 7-30-90 by rgw
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128),einc(128)
      common/aux11/po(128),
     1 pold(128),p(128),g(128),bulk(128),a0(128),
     2 a1(128),a2(128),n(128),pfrac(128),
     3 gg(128),gg43(128),sv(128),svrt(128),ratio(128),
     4 yield2(128),aj2(128),g0(128)
      common/aux14/
     1 t1(128),t2(128),t3(128),t4(128),
     2 t5(128),t6(128),epx(128),ax(128),faild(128),poe(128),
     3 sp1(128),sp3(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      data third/.3333333333333/
      data thrdpi /2.094395102 /
c
      dimension  cm(*),bqs(*)
c
c.... set pfrac to a large negative value (prevent this failure mode)
c
      mx=48*(mxt(lft)-1)
      iflag=cm(mx+47)
      sigmax=cm(mx+48)
      if (iflag.gt.0) then
      do 10 i=lft,llt
      ax(i)=epx(i)
      epx(i)=faild(i)
      faild(i)=ax(i)
   10 continue
      endif
c
      call f3dm5 (cm,bqs,1)
c
c.... calculate principal stresses
c
      if (iflag.gt.0) then
      do 22 i=lft,llt
      poe(i)=-third*(t1(i)+t2(i)+t3(i))
      aa=t4(i)**2+t5(i)**2+t6(i)**2-t1(i)*t2(i)
     1    -t2(i)*t3(i)-t1(i)*t3(i)
      bb=t1(i)*t5(i)**2+t2(i)*t6(i)**2+t3(i)*t4(i)**2
     1    -t1(i)*t2(i)*t3(i)-2.*t4(i)*t5(i)*t6(i)
c
      sp1(i)=-poe(i)
      sp3(i)=-poe(i)
c
      if (aa.gt.1.e-25) then
      cc=-sqrt(27./aa)*bb*0.5/aa
      theta=acos(cc)/3.
      ej=2.*sqrt(aa/3.)
      sp1(i)=sp1(i)+ej*cos(theta)
      sp3(i)=sp3(i)+ej*cos(theta+thrdpi)
      endif
   22 continue
c
      do 23 i=lft,llt
      if (sp1(i).ge.sigmax) faild(i)=1.
   23 continue
      do 24 i=lft,llt
      if (faild(i).gt.0.5) then
      t1(i)=t1(i)+poe(i)
      t2(i)=t2(i)+poe(i)
      t3(i)=t3(i)+poe(i)
c
      t1(i)=.5*t1(i)
      t2(i)=.5*t2(i)
      t3(i)=.5*t3(i)
      t4(i)=.5*t4(i)
      t5(i)=.5*t5(i)
      t6(i)=.5*t6(i)
      t1(i)=t1(i)-poe(i)
      t2(i)=t2(i)-poe(i)
      t3(i)=t3(i)-poe(i)
      endif
   24 continue
c
      do 25 i=lft,llt
      ax(i)=epx(i)
      epx(i)=faild(i)
      faild(i)=ax(i)
   25 continue
      endif
c
      if (iflag.eq.0) then
      do 40 i=lft,llt
      if (p(i).gt.ax(i)) go to 40
      ax(i)=0.0
      t1(i)=0.0
      t2(i)=0.0
      t3(i)=0.0
      t4(i)=0.0
      t5(i)=0.0
      t6(i)=0.0
   40 continue
      endif
      do 50 i=lft,llt
      einc(i)=(d1(i)*t1(i)+d2(i)*t2(i)+d3(i)*t3(i)+d4(i)*t4(i)
     1        +d5(i)*t5(i)+d6(i)*t6(i))*dt1+einc(i)
   50 continue
      return
      end
      subroutine f3dm15 (cm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     power law isotropic elastic plastic material
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),sigo(128,6),deps(128),ak2(128),
     1 aj2(128),depn(128),scle(128),depi(128),
     2 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),
     &sig5(128),sig6(128),epx(128),
     &epx2(128),epx3(128),epx4(128),epx5(128),epx6(128),tmpf(128),
     &epsf(128),sj2(128),a(128),b(128),ywh(128),akt(128),
     &scale(128),d1d(128),d2d(128),d3d(128),effs(128),tmp(128),
     &efrc(128),sgmd(128)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),pp(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
      common/eosd/pc(128),shrm(128)
      common/double/iprec,ncpw,unit
      dimension cm(*)
      data third /-.333333333333333/
      data zero,one /0.0,1.0/
      data iter /20/
c
      mx =48*(mxt(lft)-1)
      g  =cm(mx+1)
      ac =cm(mx+2)
      bc =cm(mx+3)
      xn  =cm(mx+4)
      c  =cm(mx+5)
      xm1  =cm(mx+6)
      tmpm=cm(mx+7)
      tmpr=cm(mx+8)
      epsi=1./cm(mx+9)
      cpi =1./cm(mx+10)/rhoa(lft)
      ispl=nint(cm(mx+12))-1
      cd1 =cm(mx+17)
      cd2 =cm(mx+18)
      cd3 =cm(mx+19)
      cd4 =cm(mx+20)
      cd5 =cm(mx+21)
      cd6 =cm(mx+22)
      xn1=xn-1.
      gdt=2.*dt1*g
      xmu3=3.0*g
      gd2=.5*gdt
      tfc=1./(tmpm-tmpr)
      opti=cm(mx+13)
c
      do 20 i=lft,llt
      pc(i)=epx5(i)
      shrm(i)=g
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      epx2(i)=  max(epx2(i),zero)
      tmp(i) =tfc*xm(i)*epx2(i)*cpi
      tmpf(i)=1.0-tmp(i)**xm1
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
      effs(i)=  max(.0020*unit,epsi*sqrt(effs(i)))
      effs(i)=log(effs(i))
      epsf(i)=tmpf(i)*(1.+c*effs(i))
      epx(i)=epx(i)+.0001
      t1(i)  =sig1(i)+po(i)+gdt*d1d(i)
      t2(i)  =sig2(i)+po(i)+gdt*d2d(i)
      t3(i)  =sig3(i)+po(i)+gdt*d3d(i)
      t4(i)  =sig4(i)+gd2*d4(i)
      t5(i)  =sig5(i)+gd2*d5(i)
   20 t6(i)  =sig6(i)+gd2*d6(i)
      if (ispl.eq.2) then
      do 30 i=lft,llt
      davg(i) =po(i)-epx5(i)
      scale(i)=.5+sign(.5*unit,davg(i))
      pc(i)   =scale(i)*epx5(i)
      sig1(i)=scale(i)*sig1(i)
      sig2(i)=scale(i)*sig2(i)
      sig3(i)=scale(i)*sig3(i)
      sig4(i)=scale(i)*sig4(i)
      sig5(i)=scale(i)*sig5(i)
      sig6(i)=scale(i)*sig6(i)
      t1(i)  =scale(i)*t1(i)
      t2(i)  =scale(i)*t2(i)
      t3(i)  =scale(i)*t3(i)
      t4(i)  =scale(i)*t4(i)
      t5(i)  =scale(i)*t5(i)
      t6(i)  =scale(i)*t6(i)
   30 epx5(i)=pc(i)
      endif
      do 40 i=lft,llt
      a(i) =ac*epsf(i)
      b(i) =bc*epsf(i)
      ak(i)=a(i)+b(i)*epx(i)**xn
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)
     &      +3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
   40 aj1(i)=sqrt(aj2(i))+1.-scle(i)
      if (opti.ne.0.0) then
      do 80 i=lft,llt
      if (scle(i).eq.0.0) go to 80
      xnb=xn*b(i)
      depi(i)=(aj1(i)-a(i)-b(i)*epx(i)**xn)/(xmu3+xnb*epx(i)**xn1)
      depsav=depi(i)
      do 70 ite=1,iter
      funct=xmu3*depi(i)+a(i)+b(i)*(epx(i)+depi(i))**xn-aj1(i)
      fntpr=xmu3+xnb*(epx(i)+depi(i))**xn1
      deldp=-funct/fntpr
      depi(i)=depi(i)+deldp
      if (abs(deldp/depi(i)).lt.000.000010) go to 80
   70 continue
      depi(i)=depsav
   80 continue
      else
      do 90 i=lft,llt
      xnb=xn*b(i)
   90 depi(i)=scle(i)*(aj1(i)-a(i)-b(i)*epx(i)**xn)/
     1  (xmu3+xnb*epx(i)**xn1)
      endif
      do 100 i=lft,llt
      depi(i)=scle(i)*  max(zero,depi(i))
      epx(i)=epx(i)+depi(i)
      ak(i)=a(i)+b(i)*epx(i)**xn
      deps(i)=1.-scle(i)*(1.0-ak(i)/aj1(i))
  100 continue
      do 110 i=lft,llt
      epx(i) =epx(i) -.0001
      sig1(i)=deps(i)*t1(i)
      sig2(i)=deps(i)*t2(i)
      sig3(i)=deps(i)*t3(i)
      sig4(i)=deps(i)*t4(i)
      sig5(i)=deps(i)*t5(i)
      sig6(i)=deps(i)*t6(i)
  110 continue
      if (cd6.gt.0.0) then
      do 120 i=lft,llt
      aj2(i)=1.5*(sig1(i)**2+sig2(i)**2+sig3(i)**2)
     &      +3.*(sig4(i)*sig4(i)+sig5(i)*sig5(i)+sig6(i)*sig6(i))
      sj2(i)=cd3*po(i)/(sqrt(aj2(i))+1.e-20)
      efrc(i)=(cd1+cd2*exp(sj2(i)))*(1.+cd4*effs(i))*(1.+cd5*tmp(i))
      epx6(i)=epx6(i)+depi(i)/efrc(i)
  120 continue
      do 130 i=lft,llt
      if(epx6(i).ge.1.0) then
      epx6(i)=1.0
      pc(i)  =0.0
      sig1(i)=0.0
      sig2(i)=0.0
      sig3(i)=0.0
      sig4(i)=0.0
      sig5(i)=0.0
      sig6(i)=0.0
      endif
  130 continue
      endif
      if(ispl.eq.1) then
      do 150 i=lft,llt
      aj2(i)=.5*(sig1(i)**2+sig2(i)**2+sig3(i)**2)+sig4(i)**2+sig5
     1 (i)**2+sig6(i)**2+1.e-12
  150 sj2(i)=sig1(i)*sig5(i)**2+sig2(i)*sig6(i)**2+sig3(i)*sig4(i)
     1      **2-sig1(i)*sig2(i)*sig3(i)-2.*sig4(i)*sig5(i)*sig6(i)
      do 160 i=lft,llt
      akt(i)=-sqrt(27./aj2(i))*sj2(i)*0.5/aj2(i)
      akt(i)=sign(  min(abs(akt(i)),one),akt(i))
  160 ywh(i)=acos(akt(i))*third
      do 170 i=lft,llt
  170 sj2(i)=2.*sqrt(aj2(i)*third)*cos(ywh(i))
      do 180 i=lft,llt
  180 davg(i)=po(i)-sj2(i)-epx5(i)
      do 190 i=lft,llt
  190 scale(i)=.50+sign(.5*unit,davg(i))
      do 200 i=lft,llt
      epx(i) =epx(i)-scle(i)*depi(i)
      pc(i)  =scale(i)*epx5(i)
      sig1(i)=scale(i)*sig1(i)
      sig2(i)=scale(i)*sig2(i)
      sig3(i)=scale(i)*sig3(i)
      sig4(i)=scale(i)*sig4(i)
      sig5(i)=scale(i)*sig5(i)
  200 sig6(i)=scale(i)*sig6(i)
      do 210 i=lft,llt
  210 epx5(i)=pc(i)
      endif
      return
      end
      subroutine f3dm17 (cm,xyz,nnm1)
c     implicit double precision (a-h,o-z)                                    dp
c
c*********************************************************************
c*                                                                   *
c*    elastic-plastic material with isotropic hardening              *
c*    and fracture using max. sig fracture criterion                 *
c*                                                                   *
c*********************************************************************
c
c***  fracture flag values
c***       ff .lt. 0.  not fractured
c***       0 .lt. ff .lt. 1. single fracture surface
c***       ff .eq. 3. isotropic failure
c
c
      logical convg
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 da1(128),da2(128),da3(128),deps(128),ak2(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),ak(128)
      common/aux14/
     &      sign1(128),sign2(128),sign3(128),sign4(128),
     &      sign5(128),sign6(128),ep(128),
     &      epx2(128),epx3(128),epx4(128),
     &      ff(128),e11(128),e21(128),e31(128),
     &      e12(128),e22(128),e32(128),
     &      s56(128),s45(128),c11(128),ui1(128),ui2(128),
     &      ui3(128),ui1s(128),q(128),rp(128),xmod(128),
     &      scl0(128),scl1(128),scl2(128),sdetm(128),ct3(128),
     &      st3(128),el1(128),el2(128),el3(128),s44(128),
     &      s55(128),s66(128),pn(128),osmax(128)
      common/aux18/dd(128),def(128)
      common/bkfr/x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     1            x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/bk28/summss,xke,xpe,tt
      common/eosd/pc(128),shrm(128)
      common/aux15/qp(128),sie(128),dvol(128),volold(128),r2v(128),
     1 r1v(128),b2df(128),facm(128),pnew(128)
      common/double/iprec,ncpw,unit
      dimension  cm(1),xyz(3,1)
c
      dimension  r(3,3),ev(3,3),b(2,6),db(6),
     1           ipq(3),ipi(3),dp(6),s(6),eta(3),re(3)
c
      data o3,root3/3.333333333333333e-01,1.732050807568877e+00/
      data o8/.1250e0/
      data ipq/2,3,1/,ipi/3,1,2/
      data s/0.,0.,0.,0.,0.,0./
      data zero/0.0/
c
      mx=48*(mxt(lft)-1)
      pcrit=cm(mx+27)
      qh=cm(mx+26)
      fs=cm(mx+21)
      qs=cm(mx+11)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      gdt=dt1*g
      gd2=.5*gdt
      blk=-dt1*ym/((1.-2.*pr))
      blkgdt=-o3*(blk+gdt)
c
c*********************************************************************
c*                                                                   *
c*    max. sig. fracture criterion                                   *
c*                                                                   *
c*********************************************************************
c
      do 10 i=lft,llt
      osmax(i)=1./  max(1.*unit,abs(sign1(i)),
     1    abs(sign2(i)),abs(sign3(i)))
      sign1(i)=osmax(i)*sign1(i)
      sign2(i)=osmax(i)*sign2(i)
      sign3(i)=osmax(i)*sign3(i)
      sign4(i)=osmax(i)*sign4(i)
      sign5(i)=osmax(i)*sign5(i)
      sign6(i)=osmax(i)*sign6(i)
   10 continue
c
      do 20 i=lft,llt
      s44(i)=sign4(i)**2
      s55(i)=sign5(i)**2
      s66(i)=sign6(i)**2
      s56(i)=sign5(i)*sign6(i)
      s45(i)=sign4(i)*sign5(i)
      c11(i)=sign2(i)*sign3(i)-s55(i)
      ui1(i)=o3*(sign1(i)+sign2(i)+sign3(i))
      ui2(i)=c11(i)+sign1(i)*sign2(i)+sign3(i)*sign1(i)
     1      -s44(i)-s66(i)
      ui3(i)=sign1(i)*c11(i)+sign4(i)*(s56(i)-sign4(i)*sign3(i))
     1      +sign6(i)*(s45(i)-sign2(i)*sign6(i))
      ui1s(i)=ui1(i)**2
   20 continue
c
      do 30 i=lft,llt
      q(i)=sqrt(-min(o3*ui2(i)-ui1s(i),zero))
      rp(i)=.50*(ui3(i)-ui1(i)*ui2(i))+ui1(i)*ui1s(i)
      xmod(i)=q(i)**3
      scl1(i)=.50+sign(.5*unit,xmod(i)-1.0e-30)
   30 continue
c
      do 40 i=lft,llt
      scl2(i)=.50+sign(.5*unit,xmod(i)-abs(rp(i)))
      scl0(i)=  min(scl1(i),scl2(i))
      scl1(i)=1.0-scl0(i)
      el1(i)=rp(i)/(xmod(i)+scl1(i))
      sdetm(i)=acos(el1(i))*o3
      q(i)=scl0(i)*q(i)
      ct3(i)=q(i)*cos(sdetm(i))
      st3(i)=q(i)*root3*sin(sdetm(i))
   40 continue
c
      do 50 i=lft,llt
      sdetm(i)=scl1(i)*sqrt(  max(zero,rp(i)))
      el1(i)=2.00*(ct3(i)+sdetm(i))+ui1(i)
      el2(i)=-ct3(i)+st3(i)-sdetm(i)+ui1(i)
      el3(i)=-ct3(i)-st3(i)-sdetm(i)+ui1(i)
      osmax(i)=1./osmax(i)
      sign1(i)=osmax(i)*sign1(i)
      sign2(i)=osmax(i)*sign2(i)
      sign3(i)=osmax(i)*sign3(i)
      sign4(i)=osmax(i)*sign4(i)
      sign5(i)=osmax(i)*sign5(i)
      sign6(i)=osmax(i)*sign6(i)
   50 continue
c
      do 60 i=lft,llt
      el1(i)=  max(el1(i),el2(i),el3(i))*osmax(i)
      q(i)=sign(1.*unit,ff(i)+.50)
      ff(i)=.50*(1.0+q(i))*ff(i)+(1.0-q(i))*sign(1.*unit,el1(i)-fs)
   60 continue
c
c***  isotropic failure
c***  failure delayed until normal stress is relaxed to zero
   65 do 70 i=lft,llt
      if (ff(i).ne.0.0.or.el1(i).lt.fs) go to 70
      ff(i)=3.0
      nelpt=nnm1+i
      write(13,1) nelpt,tt
   70 continue
c
c*********************************************************************
c*                                                                   *
c*    map fracture surface                                           *
c*                                                                   *
c*********************************************************************
c
      do 200 i=lft,llt
      if (ff(i).lt.1.50.or.ff(i).gt.2.5) go to 200
      ff(i)=1.0
c
      nelpt=nnm1+i
      write(13,2) nelpt,tt
c
      r(1,1)=sign1(i)
      r(2,2)=sign2(i)
      r(3,3)=sign3(i)
      r(1,2)=sign4(i)
      r(2,1)=r(1,2)
      r(2,3)=sign5(i)
      r(3,2)=r(2,3)
      r(1,3)=sign6(i)
      r(3,1)=r(1,3)
c
      do 110 k=1,3
      do 105 j=1,3
      ev(j,k)=0.0
  105 continue
      ev(k,k)=1.0
  110 continue
c
      omag=1.0/  max(1.0*unit,r(1,1)**2+r(2,2)**2+r(3,3)**2)
c
c***  calculate stress tensor eigenvectors
      do 140 iter=1,20
      do 130 ip=1,3
      iq=ipq(ip)
      ii=ipi(ip)
c
      a=sign(.5*unit,r(ip,ip)-r(iq,iq))*  max(abs(r(ip,ip)-r(iq,iq)),
     1  1.e-6*unit)
      otwob=.50/sqrt(r(ip,iq)**2+a**2)
      ct=sqrt(.50+abs(a)*otwob)
      st=-sign(1.*unit,a)*r(ip,iq)*otwob/ct
c
      ctsq=ct**2
      stsq=st**2
      ctst=2.0*ct*st*r(ip,iq)
c
      rpp=r(ip,ip)
      r(ip,ip)=ctsq*rpp+stsq*r(iq,iq)-ctst
      r(iq,iq)=ctsq*r(iq,iq)+stsq*rpp+ctst
      rpi=r(ip,ii)
      r(ip,ii)=ct*rpi-st*r(iq,ii)
      r(iq,ii)=st*rpi+ct*r(iq,ii)
      r(ii,ip)=r(ip,ii)
      r(ii,iq)=r(iq,ii)
      r(ip,iq)=0.
      r(iq,ip)=0.
c
      do 120 ii=1,3
      evip=ev(ii,ip)
      ev(ii,ip)=ct*evip-st*ev(ii,iq)
      ev(ii,iq)=st*evip+ct*ev(ii,iq)
  120 continue
c
  130 continue
c
      if ((r(1,2)**2+r(1,3)**2+r(2,3)**2)*omag.lt.1.0e-6) go to 150
c
  140 continue
c
      write(13,3)
c
  150 iv=1
      if (r(2,2).gt.r(1,1)) iv=2
      if (r(3,3).gt.r(iv,iv)) iv=3
c
c***  fracture normal
      xn=ev(1,iv)
      yn=ev(2,iv)
      zn=ev(3,iv)
c
      x1=xyz(1,ix1(i))
      y1=xyz(2,ix1(i))
      z1=xyz(3,ix1(i))
      x2=xyz(1,ix2(i))
      y2=xyz(2,ix2(i))
      z2=xyz(3,ix2(i))
      x3=xyz(1,ix3(i))
      y3=xyz(2,ix3(i))
      z3=xyz(3,ix3(i))
      x4=xyz(1,ix4(i))
      y4=xyz(2,ix4(i))
      z4=xyz(3,ix4(i))
      x5=xyz(1,ix5(i))
      y5=xyz(2,ix5(i))
      z5=xyz(3,ix5(i))
      x6=xyz(1,ix6(i))
      y6=xyz(2,ix6(i))
      z6=xyz(3,ix6(i))
      x7=xyz(1,ix7(i))
      y7=xyz(2,ix7(i))
      z7=xyz(3,ix7(i))
      x8=xyz(1,ix8(i))
      y8=xyz(2,ix8(i))
      z8=xyz(3,ix8(i))
c
c***  jacobian for e1, e2, e3 equal to 0.0
      dxe1=o8*(x2-x1+x3-x4+x6-x5+x7-x8)
      dxe2=o8*(x4-x1+x3-x2+x8-x5+x7-x6)
      dxe3=o8*(x5-x1+x6-x2+x8-x4+x7-x3)
c
      dye1=o8*(y2-y1+y3-y4+y6-y5+y7-y8)
      dye2=o8*(y4-y1+y3-y2+y8-y5+y7-y6)
      dye3=o8*(y5-y1+y6-y2+y8-y4+y7-y3)
c
      dze1=o8*(z2-z1+z3-z4+z6-z5+z7-z8)
      dze2=o8*(z4-z1+z3-z2+z8-z5+z7-z6)
      dze3=o8*(z5-z1+z6-z2+z8-z4+z7-z3)
c
      xavg=o8*(x1+x2+x3+x4+x5+x6+x7+x8)
      yavg=o8*(y1+y2+y3+y4+y5+y6+y7+y8)
      zavg=o8*(z1+z2+z3+z4+z5+z6+z7+z8)
c
      odet=1./(dxe1*(dye2*dze3-dye3*dze2)
     1        +dxe2*(dye3*dze1-dye1*dze3)
     1        +dxe3*(dye1*dze2-dye2*dze1))
c
      elmean=(8./odet)**o3
      orn=.1*elmean/sqrt(xn**2+yn**2+zn**2)
      epsc=1.e-6*elmean
c
      xn=orn*xn
      yn=orn*yn
      zn=orn*zn
c
c***  map fracture surface to two vectors perpendicular to the
c***  fracture normal
      rsq=xn**2+yn**2
      if (rsq.lt.1.e-2) go to 155
      re(1)=-yn
      re(2)=xn
      re(3)=0.
      go to 157
  155 re(1)=zn
      re(2)=0.
      re(3)=-xn
c
  157 do 180 l=1,2
      ierr=0
      call frcmap(re,eta,ierr)
      if (ierr.ne.0) write(13,4)
c
  165 if (l.eq.2) go to 170
      e11(i)=eta(1)
      e21(i)=eta(2)
      e31(i)=eta(3)
c
      r1n=yn*re(3)-zn*re(2)
      r2n=zn*re(1)-xn*re(3)
      r3n=xn*re(2)-yn*re(1)
c
      orn=.1*elmean/sqrt(r1n**2+r2n**2+r3n**2)
c
      re(1)=orn*r1n
      re(2)=orn*r2n
      re(3)=orn*r3n
c
      go to 180
c
  170 e12(i)=eta(1)
      e22(i)=eta(2)
      e32(i)=eta(3)
c
  180 continue
c
  200 continue
c
c
c*********************************************************************
c*                                                                   *
c*    radial return for isotropic plasticity                         *
c*                                                                   *
c*********************************************************************
c
c***  compute trial stress
      do 210 i=lft,llt
      shrm(i)=ss
      pc(i)=pcrit
      davg(i)=-o3*(d1(i)+d2(i)+d3(i))
      q1(i)=sign1(i)
      q2(i)=sign2(i)
      q3(i)=sign3(i)
      q4(i)=sign4(i)
      q5(i)=sign5(i)
      q6(i)=sign6(i)
  210 continue
      do 220 i=lft,llt
      sign1(i)=sign1(i)+po(i)+gdt*(d1(i)+davg(i))
      sign2(i)=sign2(i)+po(i)+gdt*(d2(i)+davg(i))
      sign3(i)=sign3(i)+po(i)+gdt*(d3(i)+davg(i))
      sign4(i)=sign4(i)+gd2*d4(i)
      sign5(i)=sign5(i)+gd2*d5(i)
      sign6(i)=sign6(i)+gd2*d6(i)
  220 continue
c
c***  radial return
      do 230 i=lft,llt
      ak(i)=qs+qh*ep(i)
  230 continue
      do 240 i=lft,llt
      aj2(i)=sign4(i)**2+sign5(i)**2+sign6(i)**2-sign1(i)*sign2(i)
     1      -sign2(i)*sign3(i)-sign3(i)*sign1(i)
      ak2(i)=3.0*aj2(i)-ak(i)*ak(i)
  240 scle(i)=.5*(1.0+sign(1.*unit,ak2(i)))
      fac1=1./(1.5*g+qh)
      fac2=1.5*g
      fac3=qh
      fac12=fac1*fac2
      do 250 i=lft,llt
  250 aj1(i)=sqrt(3.0*abs(aj2(i)))+1.0-scle(i)
      do 260 i=lft,llt
      depi(i)=scle(i)*fac1*(aj1(i)-ak(i))
      ep(i)=ep(i)+depi(i)
  260 deps(i)=fac2*depi(i)/aj1(i)
      do 270 i=lft,llt
      sign1(i)=(1.0-deps(i))*sign1(i)
      sign2(i)=(1.0-deps(i))*sign2(i)
      sign3(i)=(1.0-deps(i))*sign3(i)
      sign4(i)=(1.0-deps(i))*sign4(i)
      sign5(i)=(1.0-deps(i))*sign5(i)
      sign6(i)=(1.0-deps(i))*sign6(i)
  270 continue
c
c*********************************************************************
c*                                                                   *
c*    fractured elements                                             *
c*                                                                   *
c*********************************************************************
c
      do 800 i=lft,llt
c
c***  if the element is not fractured, then skip
c***  if it failed isotropically, then zero the stresses
c***  if it is a single surface fracture and is in
c***  compression, then skip
      if (ff(i).lt.-.5) go to 800
      if (ff(i).gt.1.5) go to 700
      s1old=xn*(xn*sign1(i)+2.*(yn*sign4(i)
     1     +zn*sign6(i)))+yn*(yn*sign2(i)
     1     +2.*zn*sign5(i))+zn**2*sign3(i)
      if (def(i).lt.1..and.s1old.lt.0.) go to 800
c
c***  single surface fracture in tension
      x1=xyz(1,ix1(i))
      y1=xyz(2,ix1(i))
      z1=xyz(3,ix1(i))
      x2=xyz(1,ix2(i))
      y2=xyz(2,ix2(i))
      z2=xyz(3,ix2(i))
      x3=xyz(1,ix3(i))
      y3=xyz(2,ix3(i))
      z3=xyz(3,ix3(i))
      x4=xyz(1,ix4(i))
      y4=xyz(2,ix4(i))
      z4=xyz(3,ix4(i))
      x5=xyz(1,ix5(i))
      y5=xyz(2,ix5(i))
      z5=xyz(3,ix5(i))
      x6=xyz(1,ix6(i))
      y6=xyz(2,ix6(i))
      z6=xyz(3,ix6(i))
      x7=xyz(1,ix7(i))
      y7=xyz(2,ix7(i))
      z7=xyz(3,ix7(i))
      x8=xyz(1,ix8(i))
      y8=xyz(2,ix8(i))
      z8=xyz(3,ix8(i))
c
      xavg=o8*(x1+x2+x3+x4+x5+x6+x7+x8)
      yavg=o8*(y1+y2+y3+y4+y5+y6+y7+y8)
      zavg=o8*(z1+z2+z3+z4+z5+z6+z7+z8)
c
c***  calculate the local fracture coordinate system
c***  r(i,3) is the normal to the fracture system
      e1=e11(i)
      e2=e21(i)
      e3=e31(i)
c
      do 320 l=1,2
c
      ope1=1.+e1
      ome1=1.-e1
      ope2=1.+e2
      ome2=1.-e2
      ope3=1.+e3
      ome3=1.-e3
c
      x=  o8*(((x1*ome1+x2*ope1)*ome2
     1        +(x3*ope1+x4*ome1)*ope2)*ome3
     1       +((x5*ome1+x6*ope1)*ome2
     1        +(x7*ope1+x8*ome1)*ope2)*ope3)-xavg
c
      y=  o8*(((y1*ome1+y2*ope1)*ome2
     1        +(y3*ope1+y4*ome1)*ope2)*ome3
     1       +((y5*ome1+y6*ope1)*ome2
     1        +(y7*ope1+y8*ome1)*ope2)*ope3)-yavg
c
      z=  o8*(((z1*ome1+z2*ope1)*ome2
     1        +(z3*ope1+z4*ome1)*ope2)*ome3
     1       +((z5*ome1+z6*ope1)*ome2
     1        +(z7*ope1+z8*ome1)*ope2)*ope3)-zavg
c
      if (l.eq.2) go to 310
c
      omag=1./sqrt(x**2+y**2+z**2)
      r(2,1)=omag*x
      r(2,2)=omag*y
      r(2,3)=omag*z
c
      e1=e12(i)
      e2=e22(i)
      e3=e32(i)
c
      go to 320
c
  310 r(1,1)=r(2,2)*z-r(2,3)*y
      r(1,2)=r(2,3)*x-r(2,1)*z
      r(1,3)=r(2,1)*y-r(2,2)*x
c
      omag=1./sqrt(r(1,1)**2+r(1,2)**2+r(1,3)**2)
      r(1,1)=omag*r(1,1)
      r(1,2)=omag*r(1,2)
      r(1,3)=omag*r(1,3)
c
      r(3,1)=r(1,2)*r(2,3)-r(1,3)*r(2,2)
      r(3,2)=r(1,3)*r(2,1)-r(1,1)*r(2,3)
      r(3,3)=r(1,1)*r(2,2)-r(1,2)*r(2,1)
c
  320 continue
c
c***  use stresses from last step
  330 sign1(i)=q1(i)
      sign2(i)=q2(i)
      sign3(i)=q3(i)
      sign4(i)=q4(i)
      sign5(i)=q5(i)
      sign6(i)=q6(i)
c
      ep(i)=ep(i)-depi(i)
c
c***  for compressive strain but positive normal stress,
c***  retain strain rates but make the normal stress zero
      if (def(i).ge.1.0) go to 335
      ff(i)=0.
      db(1)=d1(i)
      db(2)=d2(i)
      db(3)=d3(i)
      db(4)=d4(i)
      db(5)=d5(i)
      db(6)=d6(i)
      go to 340
c
c***  project strain rates so that the shear rates are zero
c***  in the local coordinate system
  335 b(1,1)=r(1,1)*r(2,1)
      b(2,1)=r(1,1)*r(3,1)
      b(1,2)=r(1,2)*r(2,2)
      b(2,2)=r(1,2)*r(3,2)
      b(1,3)=r(1,3)*r(2,3)
      b(2,3)=r(1,3)*r(3,3)
      b(1,4)=.50*(r(1,1)*r(2,2)+r(1,2)*r(2,1))
      b(2,4)=.50*(r(1,1)*r(3,2)+r(1,2)*r(3,1))
      b(1,5)=.50*(r(1,2)*r(2,3)+r(1,3)*r(2,2))
      b(2,5)=.50*(r(1,2)*r(3,3)+r(1,3)*r(3,2))
      b(1,6)=.50*(r(1,1)*r(2,3)+r(1,3)*r(2,1))
      b(2,6)=.50*(r(1,1)*r(3,3)+r(1,3)*r(3,1))
c
      bbt1=b(1,1)*b(1,1)+b(1,2)*b(1,2)+b(1,3)*b(1,3)
     1    +b(1,4)*b(1,4)+b(1,5)*b(1,5)+b(1,6)*b(1,6)
      bbt2=b(1,1)*b(2,1)+b(1,2)*b(2,2)+b(1,3)*b(2,3)
     1    +b(1,4)*b(2,4)+b(1,5)*b(2,5)+b(1,6)*b(2,6)
      bbt3=b(2,1)*b(2,1)+b(2,2)*b(2,2)+b(2,3)*b(2,3)
     1    +b(2,4)*b(2,4)+b(2,5)*b(2,5)+b(2,6)*b(2,6)
c
      obbt=1.0/(bbt1*bbt3-bbt2*bbt2)
      r1=b(1,1)*d1(i)+b(1,2)*d2(i)+b(1,3)*d3(i)
     1  +b(1,4)*d4(i)+b(1,5)*d5(i)+b(1,6)*d6(i)
      r2=b(2,1)*d1(i)+b(2,2)*d2(i)+b(2,3)*d3(i)
     1  +b(2,4)*d4(i)+b(2,5)*d5(i)+b(2,6)*d6(i)
      sol1=obbt*(r1*bbt3-r2*bbt2)
      sol2=obbt*(r2*bbt1-r1*bbt2)
c
      db(1)=d1(i)-sol1*b(1,1)-sol2*b(2,1)
      db(2)=d2(i)-sol1*b(1,2)-sol2*b(2,2)
      db(3)=d3(i)-sol1*b(1,3)-sol2*b(2,3)
      db(4)=d4(i)-sol1*b(1,4)-sol2*b(2,4)
      db(5)=d5(i)-sol1*b(1,5)-sol2*b(2,5)
      db(6)=d6(i)-sol1*b(1,6)-sol2*b(2,6)
c
c***  stresses and strain rates are rotated into local
c***  coordinate system.
  340 d1old=r(1,1)*(r(1,1)*d1(i)+r(1,2)*d4(i)
     1     +r(1,3)*d6(i))+r(1,2)*(r(1,2)*d2(i)
     1     +r(1,3)*d5(i))+r(1,3)**2*d3(i)
c
      s(1)=r(1,1)*(r(1,1)*sign1(i)+2.*(r(1,2)*sign4(i)
     1    +r(1,3)*sign6(i)))+r(1,2)*(r(1,2)*sign2(i)
     1    +2.*r(1,3)*sign5(i))+r(1,3)**2*sign3(i)
      dp(1)=r(1,1)*(r(1,1)*db(1)+r(1,2)*db(4)+r(1,3)*db(6))
     1     +r(1,2)*(r(1,2)*db(2)+r(1,3)*db(5))+r(1,3)**2*db(3)
      s(2)=r(2,1)*(r(2,1)*sign1(i)+2.*(r(2,2)*sign4(i)
     1    +r(2,3)*sign6(i)))+r(2,2)*(r(2,2)*sign2(i)
     1    +2.*r(2,3)*sign5(i))+r(2,3)**2*sign3(i)
      dp(2)=r(2,1)*(r(2,1)*db(1)+r(2,2)*db(4)+r(2,3)*db(6))
     1     +r(2,2)*(r(2,2)*db(2)+r(2,3)*db(5))+r(2,3)**2*db(3)
      s(3)=r(3,1)*(r(3,1)*sign1(i)+2.*(r(3,2)*sign4(i)
     1    +r(3,3)*sign6(i)))+r(3,2)*(r(3,2)*sign2(i)
     1    +2.*r(3,3)*sign5(i))+r(3,3)**2*sign3(i)
      dp(3)=r(3,1)*(r(3,1)*db(1)+r(3,2)*db(4)+r(3,3)*db(6))
     1     +r(3,2)*(r(3,2)*db(2)+r(3,3)*db(5))+r(3,3)**2*db(3)
      s(5)=r(2,1)*(r(3,1)*sign1(i)+r(3,2)*sign4(i)+r(3,3)*sign6(i))
     1    +r(2,2)*(r(3,1)*sign4(i)+r(3,2)*sign2(i)+r(3,3)*sign5(i))
     1    +r(2,3)*(r(3,1)*sign6(i)+r(3,2)*sign5(i)+r(3,3)*sign3(i))
      dp(5)=r(2,1)*(2.*r(3,1)*db(1)+r(3,2)*db(4)+r(3,3)*db(6))
     1     +r(2,2)*(2.*r(3,2)*db(2)+r(3,1)*db(4)+r(3,3)*db(5))
     1     +r(2,3)*(2.*r(3,3)*db(3)+r(3,2)*db(5)+r(3,1)*db(6))
c
c***  secant iteration to drive the normal stress to zero
c***  normal stress relaxed to zero over several cycles
c***  using parabola. see recursive expression for ff.
      s(2)=s(2)+gdt*dp(2)
      s(3)=s(3)+gdt*dp(3)
      s(5)=s(5)+gd2*dp(5)
      d1new=-d1old
      aksq=ak(i)**2
      convg=.false.
      sigma=ff(i)*fs
      ff(i)=  max(zero,ff(i)-.1*sqrt(1-ff(i))-.0025)
      shrsq=s(5)**2
      dp2n3=dp(2)+dp(3)
c
      do 500 iter=1,20
c
      pni=blkgdt*(d1new+dp2n3)
      s1new=s(1)+pni+gdt*d1new
      s2i=s(2)+pni
      s3i=s(3)+pni
      ui1i=o3*(s1new+s2i+s3i)
c
      aj2i=shrsq-s1new*s2i-s2i*s3i-s3i*s1new
      ak2i=3.0*aj2i-aksq
      scli=.5*(1.0+sign(1.*unit,ak2i))
      aj1i=sqrt(3.0*abs(aj2i))+1.0-scli
      depsi=fac12*scli*(aj1i-ak(i))/aj1i
      s1new=s1new-depsi*(s1new-ui1i)
c
      if (convg) go to 550
      d1sav=d1new
      den1=s1old-s1new
      if (den1.ne.0.) go to 480
      convg=.true.
      go to 500
c
  480 d1new=d1new-(d1old-d1new)*(s1new-sigma)/den1
      den2=  max(1.e-4*unit,abs(d1new),abs(d1sav))
      if (abs((d1new-d1sav)/den2).lt.1.0e-6) convg=.true.
      d1old=d1sav
      s1old=s1new
c
  500 continue
c
      nelpt=nnm1+i
      if (.not.convg) write(13,5) nelpt,tt,d1new,d1sav
c
  550 s(1)=s1new
      s(2)=s2i-depsi*(s2i-ui1i)
      s(3)=s3i-depsi*(s3i-ui1i)
      s(5)=s(5)-depsi*s(5)
c
c***  rotate stresses back into global frame
      sign1(i)=r(2,1)*(r(2,1)*s(2)+2.*r(3,1)*s(5))
     1        +r(3,1)**2*s(3)+r(1,1)**2*s(1)
      sign2(i)=r(2,2)*(r(2,2)*s(2)+2.*r(3,2)*s(5))
     1        +r(3,2)**2*s(3)+r(1,2)**2*s(1)
      sign3(i)=r(2,3)*(r(2,3)*s(2)+2.*r(3,3)*s(5))
     1        +r(3,3)**2*s(3)+r(1,3)**2*s(1)
      sign4(i)=r(2,1)*(r(2,2)*s(2)+r(3,2)*s(5))+r(1,1)*r(1,2)
     1        *s(1)+r(3,1)*(r(2,2)*s(5)+r(3,2)*s(3))
      sign5(i)=r(2,2)*(r(2,3)*s(2)+r(3,3)*s(5))+r(1,2)*r(1,3)
     1        *s(1)+r(3,2)*(r(2,3)*s(5)+r(3,3)*s(3))
      sign6(i)=r(2,1)*(r(2,3)*s(2)+r(3,3)*s(5))+r(1,1)*r(1,3)
     1        *s(1)+r(3,1)*(r(2,3)*s(5)+r(3,3)*s(3))
      go to 800
c
c***  zero deviator for isotropically failed elements
  700 sign1(i)=0.
      sign2(i)=0.
      sign3(i)=0.
      sign4(i)=0.
      sign5(i)=0.
      sign6(i)=0.
      ep(i)=ep(i)-depi(i)
c
  800 continue
c
      return
c
    1 format(' total failure of element ',i5,' at time = ',1pe14.7)
    2 format(1x/' element ',i5,' fractured at time = ',1pe14.7)
    3 format(' eigenvector convergence failure')
    4 format(' fracture surface convergence failure')
    5 format(' normal stress .ne. 0 on element ',
     1      i5,' at ',1pe14.7,' d:',2(1x,1pe14.7))
c
      end
      subroutine frcmap(xx,eta,ierr)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bkfr/x(3,8)
c
      dimension eta(3),a(3),b(3),c(3),d(3),e(3),f(3),g(3),h(3),
     1          xe(3,3),dje(3),di(3),xs(3),xg(3),hh(3,3),eto(3),
     1          xx(3)
c
      kk=0
c
      do 10 i=1,3
      x12=x(i,1)+x(i,2)
      x12b=x(i,2)-x(i,1)
      x34=x(i,3)+x(i,4)
      x34b=x(i,3)-x(i,4)
      x56=x(i,5)+x(i,6)
      x56b=x(i,6)-x(i,5)
      x78=x(i,7)+x(i,8)
      x78b=x(i,7)-x(i,8)
      x1234=x12+x34
      x1234p=x12-x34
      x5678=x56+x78
      x5678p=x56-x78
      x1234b=x12b+x34b
      x1234y=x12b-x34b
      x5678b=x56b+x78b
      x5678y=x56b-x78b
      a(i)=x1234+x5678
      b(i)=x1234b+x5678b
      c(i)=-x1234p-x5678p
      d(i)=x5678-x1234
      e(i)=-x1234y-x5678y
      f(i)=x1234p-x5678p
      g(i)=x5678b-x1234b
      h(i)=x1234y-x5678y
      eta(i)=0.
      eto(i)=1.
      xg(i)=a(i)
      xs(i)=8.0*xx(i)+a(i)
   10 continue
c
      do 100 ll=1,4
c
      do 20 i=1,3
      di(i)=xs(i)-xg(i)
      comt=e(i)+h(i)*eta(3)
      xe(i,1)=b(i)+comt*eta(2)+g(i)*eta(3)
      xe(i,2)=c(i)+comt*eta(1)+f(i)*eta(3)
      xe(i,3)=d(i)+(f(i)+h(i)*eta(1))*eta(2)+g(i)*eta(1)
   20 continue
c
      call fslv(di,xe,eta)
c
      if ((eta(1)-eto(1))**2+(eta(2)-eto(2))**2
     1   +(eta(3)-eto(3))**2.lt.1.e-10) return
c
      do 30 i=1,3
      eto(i)=eta(i)
      xg(i)=a(i)+(b(i)+(e(i)+h(i)*eta(3))*eta(2)
     1     +g(i)*eta(3))*eta(1)+(c(i)+f(i)*eta(3))*eta(2)
     2     +d(i)*eta(3)
   30 continue
c
  100 continue
c
      do 200 kk=1,10
c
      do 120 i=1,3
      di(i)=xs(i)-xg(i)
      comt=e(i)+h(i)*eta(3)
      xe(i,1)=b(i)+comt*eta(2)+g(i)*eta(3)
      xe(i,2)=c(i)+comt*eta(1)+f(i)*eta(3)
      xe(i,3)=d(i)+(f(i)+h(i)*eta(1))*eta(2)+g(i)*eta(1)
  120 continue
c
      do 140 ii=1,3
      dje(ii)=di(1)*xe(1,ii)+di(2)*xe(2,ii)+di(3)*xe(3,ii)
      do 130 i=1,3
      hh(i,ii)=xe(1,ii)*xe(1,i)+xe(2,ii)*xe(2,i)+xe(3,i)*xe(3,ii)
  130 continue
  140 continue
c
      dih=di(1)*h(1)+di(2)*h(2)+di(3)*h(3)
      hh(1,2)=hh(1,2)-di(1)*e(1)-di(2)*e(2)-di(3)*e(3)-dih*eta(3)
      hh(2,1)=hh(1,2)
      hh(1,3)=hh(1,3)-di(1)*g(1)-di(2)*g(2)-di(3)*g(3)-dih*eta(2)
      hh(3,1)=hh(1,3)
      hh(2,3)=hh(2,3)-di(1)*f(1)-di(2)*f(2)-di(3)*f(3)-dih*eta(1)
      hh(3,2)=hh(2,3)
c
      call fslv(dje,hh,eta)
c
      if ((eta(1)-eto(1))**2+(eta(2)-eto(2))**2
     1   +(eta(3)-eto(3))**2.lt.1.e-10) return
c
      do 160 i=1,3
      eto(i)=eta(i)
      xg(i)=a(i)+(b(i)+(e(i)+h(i)*eta(3))*eta(2)
     1     +g(i)*eta(3))*eta(1)+(c(i)+f(i)*eta(3))*eta(2)
     1     +d(i)*eta(3)
  160 continue
c
  200 continue
c
      write(13,1)
    1 format(' convergence failure in routine frcmap')
      write(13,2) xs,x
    2 format(1x/' xs='/3(1x,1pe14.7)/' xnodes='/(3(1x,1pe14.7)))
      call empty(13)
c
      return
      end
      subroutine fslv(b,a,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension b(*),a(3,*),x(*)
c
      cf5968=a(2,2)*a(3,3)-a(3,2)*a(2,3)
      cf6749=a(3,2)*a(1,3)-a(1,2)*a(3,3)
      cf4857=a(1,2)*a(2,3)-a(2,2)*a(1,3)
      deti=1./(a(1,1)*cf5968+a(2,1)*cf6749+a(3,1)*cf4857)
      a21=-a(2,1)*a(3,3)+a(3,1)*a(2,3)
      a22= a(1,1)*a(3,3)-a(3,1)*a(1,3)
      a23=-a(1,1)*a(2,3)+a(2,1)*a(1,3)
      a31= a(2,1)*a(3,2)-a(3,1)*a(2,2)
      a32=-a(1,1)*a(3,2)+a(3,1)*a(1,2)
      a33= a(1,1)*a(2,2)-a(2,1)*a(1,2)
      x(1)=x(1)+deti*(cf5968*b(1)+cf6749*b(2)+cf4857*b(3))
      x(2)=x(2)+deti*(a21*b(1)+a22*b(2)+a23*b(3))
      x(3)=x(3)+deti*(a31*b(1)+a32*b(2)+a33*b(3))
      return
      end
      subroutine fzero
c     implicit double precision (a-h,o-z)                                    dp
c
c***  zeroes stresses after equations of state for
c***  material model 17
c
      common/aux11/po(128),
     1 sign1(128),sign2(128),sign3(128),sign4(128),
     2 sign5(128),sign6(128),ep(128),da1(128),
     3 da2(128),da3(128),deps(128),ak2(128),
     4 aj2(128),depn(128),scle(128),depi(128),
     5 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),ak(128)
      common/aux14/epx2(128),epx3(128),epx4(128),
     &      ff(128),e11(128),e21(128),e31(128),
     &      e12(128),e22(128),e32(128),
     &      s56(128),s45(128),c11(128),ui1(128),ui2(128),
     &      ui3(128),ui1s(128),q(128),rp(128),xmod(128),
     &      scl0(128),scl1(128),scl2(128),sdetm(128),ct3(128),
     &      st3(128),el1(128),el2(128),el3(128),s44(128),
     &      s55(128),s66(128),pn(128)
      common/aux15/qp(128),sie(128),dvol(128),volold(128),r2v(128),
     1 r1v(128),b2df(128),facm(128),pnew(128)
      common/aux18/dd(128),def(128)
      common/aux36/lft,llt
      common/energy/xinen
      common/double/iprec,ncpw,unit
c
      do 10 i=lft,llt
      scl0(i)=.50*(sign(1.0*unit,ff(i)+.50)+sign(1.0*unit,2.5-ff(i)))
      scl1(i)=1.-(.5+sign(.5*unit,ff(i)-2.5))
     1  *(.5+sign(.5*unit,def(i)-1.))
      fpnew=scl0(i)*pnew(i)
      sign1(i)=scl1(i)*(sign1(i)+fpnew)
      sign2(i)=scl1(i)*(sign2(i)+fpnew)
      sign3(i)=scl1(i)*(sign3(i)+fpnew)
   10 continue
c
      do 20 i=lft,llt
      xinen=xinen-max(scl0(i),scl1(i))*epx2(i)
   20 continue
c
      return
      end
      subroutine f3dm18 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     power law isotropic elastic plastic material
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 da1(128),da2(128),da3(128),deps(128),ak2(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),
     3 epx1(128),epx2(128),epx4(128),epx5(128),epx6(128)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/double/iprec,ncpw,unit
      dimension cm(*),bqs(*)
      data third /-.333333333333333/
      data iter /20/
c
      mx =48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+2)
      qk=cm(mx+3)
      qm=cm(mx+4)
      q1=ym*pr/((1.0+pr)*(1.0-2.0*pr))
      g=ym/(1.+pr)
      ss=q1+g
      blk=-dt1*ym/(1.-2.*pr)
      epi=(ym/qk)**(1./(qm-1.0))
      qmqk=qm*qk
      qm1=qm-1.
      gdt=dt1*g
      gd2=.5*gdt
c
      do 10 i=lft,llt
      cb(i) =ss
      davg(i)=third*(d1(i)+d2(i)+d3(i))
   10 p(i)=blk*davg(i)
      do 20 i=lft,llt
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
      da1(i)=sig1(i)+p(i)+gdt*(d1(i)+davg(i))
      da2(i)=sig2(i)+p(i)+gdt*(d2(i)+davg(i))
      da3(i)=sig3(i)+p(i)+gdt*(d3(i)+davg(i))
      t4(i) =sig4(i)+gd2*d4(i)
      t5(i) =sig5(i)+gd2*d5(i)
   20 t6(i) =sig6(i)+gd2*d6(i)
      do 30 i=lft,llt
   30 ak(i)=qk*(epi+epx(i))**qm
      do 40 i=lft,llt
      p(i)=third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
   40 t3(i)=p(i)+da3(i)
      do 50 i=lft,llt
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)
     &      +3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
   50 ak2(i)=aj2(i)-ak(i)*ak(i)
      do 60 i=lft,llt
   60 scle(i)=.50+sign(.5*unit,ak2(i))
      fac1=1.5*g
      do 70 i=lft,llt
   70 aj1(i)=sqrt(aj2(i))+1.-scle(i)
      do 90 i=lft,llt
      if (scle(i).eq.0) go to 90
      epix=epi+epx(i)
      depi(i)=(aj1(i)-qk*epix**qm)/(fac1+qmqk*epix**qm1)
      depsav=depi(i)
      do 80 ite=1,iter
      funct=fac1*depi(i)+qk*(epix+depi(i))**qm-aj1(i)
      fntpr=fac1+qmqk*(epix+depi(i))**qm1
      deldp=-funct/fntpr
      depi(i)=depi(i)+deldp
      if (abs(deldp)/depi(i).lt.000.000010) go to 90
   80 continue
      depi(i)=depsav
   90 continue
      do 100 i=lft,llt
      epx(i)=epx(i)+scle(i)*depi(i)
      ak(i)=qk*(epi+epx(i))**qm
      deps(i)=scle(i)*(1.0-ak(i)/aj1(i))
  100 continue
      do 110 i=lft,llt
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=da3(i)-deps(i)*t3(i)
      sig4(i)= t4(i)-deps(i)*t4(i)
      sig5(i)= t5(i)-deps(i)*t5(i)
      sig6(i)= t6(i)-deps(i)*t6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  110 continue
      return
      end
      subroutine f3dm19 (cm,bqs,npc,plc)
c     implicit double precision (a-h,o-z)                                    dp
c
c     viscoplastic material model
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 da1(128),da2(128),da3(128),deps(128),ak2(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),fail(128),ymold(128),fs(128),
     3 aj3(128),d1d(128),d2d(128),d3d(128),qk(128),effs(128)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/double/iprec,ncpw,unit
      dimension cm(*),bqs(*),npc(1),plc(1),qh(128),fac1(128),blk(128),
     1 g(128),fac1qh(128),ym(128),gdt(128),gd2(128)
      data third/-.333333333333333/
c
      mx =48*(mxt(lft)-1)
      ymd=cm(mx+1)
      pr=cm(mx+2)
      i1=nint(cm(mx+3))
      j1=nint(cm(mx+5))
      k1=nint(cm(mx+6))
      l1=nint(cm(mx+7))
      qhd =cm(mx+1)*cm(mx+4)/(cm(mx+1)-cm(mx+4))
      q1=ymd*pr/((1.0+pr)*(1.0-2.0*pr))
      gm=ymd/(1.+pr)
      ss=q1+gm
c
      do 10 i=lft,llt
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
      cb(i) =ss
      davg(i)=third*(d1(i)+d2(i)+d3(i))
      d1d(i) =d1(i)+davg(i)
      d2d(i) =d2(i)+davg(i)
      d3d(i) =d3(i)+davg(i)
      effs(i)=2.*(d1d(i)**2+d2d(i)**2+d3d(i)**2
     &       +.5*(d4(i)**2+d5(i)**2+d6(i)**2))/3.
   10 effs(i) =sqrt(effs(i))
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
c     interpolate to obtain qk, ym, and qh
c     assuming same number of points in all 3 curves, same intervals
c     (n1=no. of points in curve)
c
      call gtp19 (plc(i2),plc(j2),plc(k2),plc(l2),effs,n1,qk,
     1 ym, qh,fs,lft,llt)
c
      if (j1.eq.0) then
      do 15 i=lft,llt
   15 ym(i)=ymd
      else
      demn1 =1./(2.*(1.+pr))
      demn2 =1./(3.*(1.-2.*pr))
      do 16 i=lft,llt
      gmold=ymold(i)*demn1
      h1=(sig1(i)-pr*(sig2(i)+sig3(i)))/ymold(i)
      h2=(sig2(i)-pr*(sig1(i)+sig3(i)))/ymold(i)
      h3=(sig3(i)-pr*(sig1(i)+sig2(i)))/ymold(i)
      h4= sig4(i)/gmold
      h5= sig5(i)/gmold
      h6= sig6(i)/gmold
      blkmd=ym(i)*demn2
      shrmd=ym(i)*demn1
      xk43g=blkmd+4.*shrmd/3.
      xk23g=blkmd-2.*shrmd/3.
      sig1(i)=xk43g*h1+xk23g*(h2+h3)
      sig2(i)=xk43g*h2+xk23g*(h1+h3)
      sig3(i)=xk43g*h3+xk23g*(h1+h2)
      sig4(i)=shrmd*h4
      sig5(i)=shrmd*h5
      sig6(i)=shrmd*h6
      ymold(i)=ym(i)
   16 continue
      endif
      if (k1.eq.0) then
      do 17 i=lft,llt
   17 qh(i)=qhd
      else
      do 18 i=lft,llt
   18 qh(i)=ym(i)*qh(i)/(ym(i)-qh(i))
      endif
      facnt=-dt1/(1.-2.*pr)
      facnu= 1.0/(1.+pr)
      do 20 i=lft,llt
      blk(i)=ym(i)*facnt
      g(i)  =ym(i)*facnu
      gdt(i)=dt1*g(i)
      gd2(i)=.5*gdt(i)
      p(i)=blk(i)*davg(i)
      da1(i)=sig1(i)+p(i)+gdt(i)*d1d(i)
      da2(i)=sig2(i)+p(i)+gdt(i)*d2d(i)
      da3(i)=sig3(i)+p(i)+gdt(i)*d3d(i)
      t4(i) =sig4(i)+gd2(i)*d4(i)
      t5(i) =sig5(i)+gd2(i)*d5(i)
      t6(i) =sig6(i)+gd2(i)*d6(i)
   20 ak(i) =qk(i)+qh(i)*epx(i)
      do 60 i=lft,llt
      p(i) =third*(da1(i)+da2(i)+da3(i))
      t1(i)=p(i)+da1(i)
      t2(i)=p(i)+da2(i)
      t3(i)=p(i)+da3(i)
      aj2(i)=1.5*(t1(i)**2+t2(i)**2+t3(i)**2)
     & +3.*(t4(i)*t4(i)+t5(i)*t5(i)+t6(i)*t6(i))
      ak2(i)=aj2(i)-ak(i)*ak(i)
      scle(i)=.50+sign(.5*unit,ak2(i))
      fac1(i)=1.5*g(i)
   60 fac1qh(i)=1./(fac1(i)+qh(i))
      do 120 i=lft,llt
      aj3(i)=sqrt(aj2(i))
  120 aj1(i)=aj3(i)+1.0-scle(i)
      do 130 i=lft,llt
      depi(i)=scle(i)*(aj1(i)-ak(i))*fac1qh(i)
      epx(i) =epx(i)+fail(i)*depi(i)
      deps(i)=fac1(i)*depi(i)/aj1(i)
      sig1(i)=da1(i)-deps(i)*t1(i)
      sig2(i)=da2(i)-deps(i)*t2(i)
      sig3(i)=da3(i)-deps(i)*t3(i)
      sig4(i)= t4(i)-deps(i)*t4(i)
      sig5(i)= t5(i)-deps(i)*t5(i)
  130 sig6(i)= t6(i)-deps(i)*t6(i)
      if (l1.ne.0) then
      do 140 i=lft,llt
      fail(i) =fail(i)*(.5+sign(.5*unit,fs(i)-aj3(i)))
      sig1(i) =fail(i)*sig1(i)
      sig2(i) =fail(i)*sig2(i)
      sig3(i) =fail(i)*sig3(i)
      sig4(i) =fail(i)*sig4(i)
      sig5(i) =fail(i)*sig5(i)
      sig6(i) =fail(i)*sig6(i)
  140 continue
      endif
      do 150 i=lft,llt
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  150 continue
      return
c
      end
      subroutine f3dm21(cm,bqs,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     thermal-orthotropic-elastic material
c
c     valid for large displacements and rotations but small strain
c     major reference: cook,'concepts and applications for finite
c                      element analysis',wiley
c     (this model has not been completely debugged)
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &xl1(128),xm1(128),xn1(128),xl2(128),xm2(128),xn2(128),tref(128),
     &otmp(128),ctmp(128),x17(128),x28(128),x35(128),x46(128),
     &y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),
     &df31(128),rx(128),
     &sp1(128),sp2(128),sp3(128),sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),qf(128),q1(128),q2(128),
     &q3(128),q4(128),q5(128),q6(128),q7(128),q8(128),q9(128)
      common/aux18/dde(128),df(128)
      common/aux19/epsr(128),epss(128),epst(128),xl3(128),xm3(128),
     1 xn3(128),deltem(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/e1(128),e2(128),e3(128),e4(128),e5(128),e6(128)
      common/aux41/qs(128)
      common/ortho/c11,c21,c31,c41,c51,c61,c12,c22,c32,c42,c52,c62,
     1 c13,c23,c33,c43,c53,c63,c14,c24,c34,c44,c54,c64,c15,c25,c35,
     2 c45,c55,c65,c16,c26,c36,c46,c56,c66
      dimension cm(*),bqs(*),c(6,6),temp(6,6),d(6,6),t(6,6)
c
      equivalence (c11,c)
c
c     compute deformation gradients in undeformed configuration
c
      k=48*(mxt(lft)-1)+13
      do 10 i=1,6
      do 10 j=i,6
      d(i,j)=cm(k)
      d(j,i)=cm(k)
   10 k=k+1
      if (itemp.gt.0) then
      do 12 i=lft,llt
   12 ctmp(i)=fval(itemp)
      else
      do 14 i=lft,llt
   14 ctmp(i)=.125*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i))+
     1              tnew(ix5(i))+tnew(ix6(i))+tnew(ix7(i))+tnew(ix8(i)))
      endif
      k=48*(mxt(lft)-1)+46
      ar=cm(k)
      as=cm(k+1)
      at=cm(k+2)
      do 16 i=lft,llt
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dde(i)*bqs(i))*dt1
      deltem(i)=ctmp(i)-tref(i)
      epsr(i)=deltem(i)*ar
      epss(i)=deltem(i)*as
      epst(i)=deltem(i)*at
      otmp(i)=ctmp(i)
   16 continue
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
   20 z46(i)=z4(i)-z6(i)
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
c
c     green-st.venant strain  e=(c-1)/2
c     where c is the right cauchy-green tensor
c
      do 40 i=lft,llt
      e1(i)=.5*(eps1(i)*eps1(i)+eps4(i)*eps4(i)+eps7(i)*eps7(i)-1.)
      e2(i)=.5*(eps2(i)*eps2(i)+eps5(i)*eps5(i)+eps8(i)*eps8(i)-1.)
      e3(i)=.5*(eps3(i)*eps3(i)+eps6(i)*eps6(i)+eps9(i)*eps9(i)-1.)
      e4(i)=eps1(i)*eps2(i)+eps4(i)*eps5(i)+eps7(i)*eps8(i)
      e5(i)=eps2(i)*eps3(i)+eps5(i)*eps6(i)+eps8(i)*eps9(i)
   40 e6(i)=eps1(i)*eps3(i)+eps4(i)*eps6(i)+eps7(i)*eps9(i)
c
      do 50 i=lft,llt
      xl3(i)=xm1(i)*xn2(i)-xm2(i)*xn1(i)
      xm3(i)=xn1(i)*xl2(i)-xn2(i)*xl1(i)
   50 xn3(i)=xl1(i)*xm2(i)-xl2(i)*xm1(i)
c
c     calculate transformation matrix
c
      do 80 i=lft,llt
      t(1,1)=xl1(i)*xl1(i)
      t(1,2)=xm1(i)*xm1(i)
      t(1,3)=xn1(i)*xn1(i)
      t(1,4)=xl1(i)*xm1(i)
      t(1,5)=xm1(i)*xn1(i)
      t(1,6)=xn1(i)*xl1(i)
      t(2,1)=xl2(i)*xl2(i)
      t(2,2)=xm2(i)*xm2(i)
      t(2,3)=xn2(i)*xn2(i)
      t(2,4)=xl2(i)*xm2(i)
      t(2,5)=xm2(i)*xn2(i)
      t(2,6)=xn2(i)*xl2(i)
      t(3,1)=xl3(i)*xl3(i)
      t(3,2)=xm3(i)*xm3(i)
      t(3,3)=xn3(i)*xn3(i)
      t(3,4)=xl3(i)*xm3(i)
      t(3,5)=xm3(i)*xn3(i)
      t(3,6)=xn3(i)*xl3(i)
      t(4,1)=2.*xl1(i)*xl2(i)
      t(4,2)=2.*xm1(i)*xm2(i)
      t(4,3)=2.*xn1(i)*xn2(i)
      t(4,4)=xl1(i)*xm2(i)+xl2(i)*xm1(i)
      t(4,5)=xm1(i)*xn2(i)+xm2(i)*xn1(i)
      t(4,6)=xn1(i)*xl2(i)+xn2(i)*xl1(i)
      t(5,1)=2.*xl2(i)*xl3(i)
      t(5,2)=2.*xm2(i)*xm3(i)
      t(5,3)=2.*xn2(i)*xn3(i)
      t(5,4)=xl2(i)*xm3(i)+xl3(i)*xm2(i)
      t(5,5)=xm2(i)*xn3(i)+xm3(i)*xn2(i)
      t(5,6)=xn2(i)*xl3(i)+xn3(i)*xl2(i)
      t(6,1)=2.*xl3(i)*xl1(i)
      t(6,2)=2.*xm3(i)*xm1(i)
      t(6,3)=2.*xn3(i)*xn1(i)
      t(6,4)=xl3(i)*xm1(i)+xl1(i)*xm3(i)
      t(6,5)=xm3(i)*xn1(i)+xm1(i)*xn3(i)
      t(6,6)=xn3(i)*xl1(i)+xn1(i)*xl3(i)
c
c     transform constitutive matrix
c
      do 60 m=1,6
      do 60 n=1,6
      temp(m,n)=0.0
      do 60 k=1,6
   60 temp(m,n)=temp(m,n)+d(m,k)*t(k,n)
      do 70 m=1,6
      do 70 n=m,6
      c(m,n)=0.0
      do 70 k=1,6
      c(m,n)=c(m,n)+t(k,m)*temp(k,n)
   70 c(n,m)=c(m,n)
c
      e1(i)=e1(i)-t(1,1)*epsr(i)-t(2,1)*epss(i)-t(3,1)*epst(i)
      e2(i)=e2(i)-t(1,2)*epsr(i)-t(2,2)*epss(i)-t(3,2)*epst(i)
      e3(i)=e3(i)-t(1,3)*epsr(i)-t(2,3)*epss(i)-t(3,3)*epst(i)
      e4(i)=e4(i)-2.*(t(1,4)*epsr(i)+t(2,4)*epss(i)+t(3,4)*epst(i))
      e5(i)=e5(i)-2.*(t(1,5)*epsr(i)+t(2,5)*epss(i)+t(3,5)*epst(i))
      e6(i)=e6(i)-2.*(t(1,6)*epsr(i)+t(2,6)*epss(i)+t(3,6)*epst(i))
      sp1(i)=c11*e1(i)+c12*e2(i)+c13*e3(i)+c14*e4(i)+c15*e5(i)+c16*e6(i)
      sp2(i)=c21*e1(i)+c22*e2(i)+c23*e3(i)+c24*e4(i)+c25*e5(i)+c26*e6(i)
      sp3(i)=c31*e1(i)+c32*e2(i)+c33*e3(i)+c34*e4(i)+c35*e5(i)+c36*e6(i)
      sp4(i)=c41*e1(i)+c42*e2(i)+c43*e3(i)+c44*e4(i)+c45*e5(i)+c46*e6(i)
      sp5(i)=c51*e1(i)+c52*e2(i)+c53*e3(i)+c54*e4(i)+c55*e5(i)+c56*e6(i)
   80 sp6(i)=c61*e1(i)+c62*e2(i)+c63*e3(i)+c64*e4(i)+c65*e5(i)+c66*e6(i)
c
c     cauchy stress
c
      do 90 i=lft,llt
      rx(i)=1./df(i)
      q1(i)=eps1(i)*sp1(i)+eps2(i)*sp4(i)+eps3(i)*sp6(i)
      q2(i)=eps2(i)*sp2(i)+eps1(i)*sp4(i)+eps3(i)*sp5(i)
      q3(i)=eps3(i)*sp3(i)+eps1(i)*sp6(i)+eps2(i)*sp5(i)
      q4(i)=eps4(i)*sp1(i)+eps5(i)*sp4(i)+eps6(i)*sp6(i)
      q5(i)=eps5(i)*sp2(i)+eps4(i)*sp4(i)+eps6(i)*sp5(i)
      q6(i)=eps6(i)*sp3(i)+eps4(i)*sp6(i)+eps5(i)*sp5(i)
      q7(i)=eps7(i)*sp1(i)+eps8(i)*sp4(i)+eps9(i)*sp6(i)
      q8(i)=eps8(i)*sp2(i)+eps7(i)*sp4(i)+eps9(i)*sp5(i)
   90 q9(i)=eps9(i)*sp3(i)+eps7(i)*sp6(i)+eps8(i)*sp5(i)
c
      do 100 i=lft,llt
      sig1(i)=rx(i)*(eps1(i)*q1(i)+eps2(i)*q2(i)+eps3(i)*q3(i))
      sig2(i)=rx(i)*(eps4(i)*q4(i)+eps5(i)*q5(i)+eps6(i)*q6(i))
      sig3(i)=rx(i)*(eps7(i)*q7(i)+eps8(i)*q8(i)+eps9(i)*q9(i))
      sig4(i)=rx(i)*(eps4(i)*q1(i)+eps5(i)*q2(i)+eps6(i)*q3(i))
      sig5(i)=rx(i)*(eps7(i)*q4(i)+eps8(i)*q5(i)+eps9(i)*q6(i))
      sig6(i)=rx(i)*(eps7(i)*q1(i)+eps8(i)*q2(i)+eps9(i)*q3(i))
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  100 cb(i)=  max(d(1,1),d(2,2),d(3,3))
c
      return
      end
      subroutine f3dm22 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     composite failure model (chang-chang)
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),
     &sig5(128),sig6(128),check(128),ef(128),em(128),ed(128),
     &efs(128),ems(128),eds(128),el(128),els(128),
     &q11(128),q12(128),q13(128),q31(128),
     &q32(128),q33(128),q21(128),q22(128),q23(128),
     &q2(128),ef2(128),em2(128),ed2(128),el2(128),c11(128),
     &c12(128),c13(128),c22(128),c23(128),c33(128),
     &c44(128),c55(128),c66(128),prx(128),pry(128),ex(128),
     & ey(128),gxy(128),pxy(128),sg42(128),sg44(128),eg(128),
     &eh(128),stg1(128),stg2(128),stg4(128),sg22(128),
     &stg3(128),stg5(128),stg6(128)
      common/aux18/dd(128),def(128)
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux40/
     1 a11(128),a12(128),a13(128),a21(128),a22(128),a23(128),
     2 a31(128),a32(128),a33(128),z11(128),z12(128),z13(128),
     3 z21(128),z22(128),z23(128),z31(128),z32(128),z33(128)
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/sound/ss,sndsp(128),diagm(128),sarea(128),dxl(128)
      dimension  cm(*),bqs(*)
      data zero,half /0.0,0.5/
c
      mx  =48*(mxt(lft)-1)
      sc  =cm(mx+26)
      sc2 =sc*sc
      xt  =cm(mx+27)
      yt  =cm(mx+28)
      yc  =cm(mx+29)
      xt2 =1./(xt*xt)
      yt2 =1./(yt*yt)
      sd2 =.25/sc2
      sn2 =cm(mx+45)
      syz2=cm(mx+46)
      szx2=cm(mx+47)
      bulk=cm(mx+48)
      ycsc=sd2*yc*yc
      alp =cm(mx+30)
      d13 =cm(mx+22)
      d23 =cm(mx+23)
      ap3 =3.0*alp
      ap34=.75*alp
      od2g=.5/cm(mx+7)
      xmlt1=1./(sc2*(od2g+ap34*sc2))
      xmlt2=(yc*yc*sd2-1.)/yc
c
c     global to local element axes transformation matrix
c
      call lcsm22
c
c     global to material axes transformation matrix
c     q is the transformation matrix from element axes to material axes
c
      do 10 i=lft,llt
      q21(i)=q32(i)*q13(i)-q12(i)*q33(i)
      q22(i)=q33(i)*q11(i)-q13(i)*q31(i)
      q23(i)=q31(i)*q12(i)-q11(i)*q32(i)
      z11(i)=a11(i)*q11(i)+a21(i)*q12(i)+a31(i)*q13(i)
      z12(i)=a12(i)*q11(i)+a22(i)*q12(i)+a32(i)*q13(i)
      z13(i)=a13(i)*q11(i)+a23(i)*q12(i)+a33(i)*q13(i)
      z21(i)=a11(i)*q21(i)+a21(i)*q22(i)+a31(i)*q23(i)
      z22(i)=a12(i)*q21(i)+a22(i)*q22(i)+a32(i)*q23(i)
      z23(i)=a13(i)*q21(i)+a23(i)*q22(i)+a33(i)*q23(i)
      z31(i)=a11(i)*q31(i)+a21(i)*q32(i)+a31(i)*q33(i)
      z32(i)=a12(i)*q31(i)+a22(i)*q32(i)+a32(i)*q33(i)
      z33(i)=a13(i)*q31(i)+a23(i)*q32(i)+a33(i)*q33(i)
   10 continue
c
c     transform stresses and strains to local system
c
      call ldsm22
      call ldem22
c
c     constitutive matrix for trial stresses
c
      do 20 i=lft,llt
      eg(i) =em(i)*ef(i)
      eh(i) =eg(i)*ed(i)
      s11=1./cm(mx+1)
      s22=1./cm(mx+2)
      s33=1./cm(mx+3)
      s44=1./cm(mx+7)
      s55=1./cm(mx+8)
      s66=1./cm(mx+9)
      s12=-cm(mx+4)*s22
      s13=-cm(mx+5)*s33
      s23=-cm(mx+6)*s33
      s1122=s11*s22
      s1123=s11*s23
      s2213=s22*s13
      s1212=s12*s12
      s1223=s12*s23
      si=1./(s1122*s33-s1123*s23-s2213*s13-s33*s1212+2.*s1223*s13)
      c11(i)=si*(s22*s33-s23*s23)
      c22(i)=si*(s33*s11-s13*s13)
      c33(i)=si*(s1122-s1212)
      cb(i) =  max(c11(i),c22(i),c33(i))
      si    =dt1*si
      c11(i)=dt1*c11(i)
      c22(i)=dt1*c22(i)
      c33(i)=dt1*c33(i)
      c12(i)=si*(s13*s23-s33*s12)
      c23(i)=si*(s12*s13-s1123)
      c13(i)=si*(s1223-s2213)
      c44(i)=dt1/s44
      c55(i)=dt1/s55
      c66(i)=dt1/s66
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   20 continue
c
c     stress update
c
      do 30 i=lft,llt
      sig1(i)=sig1(i)+c11(i)*d1(i)+c12(i)*d2(i)+c13(i)*d3(i)
      sig2(i)=sig2(i)+c12(i)*d1(i)+c22(i)*d2(i)+c23(i)*d3(i)
      sig3(i)=sig3(i)+c13(i)*d1(i)+c23(i)*d2(i)+c33(i)*d3(i)
      sig4(i)=sig4(i)+c44(i)*d4(i)
      sig5(i)=sig5(i)+c55(i)*d5(i)
      sig6(i)=sig6(i)+c66(i)*d6(i)
      sg42(i)=sig4(i)*sig4(i)
      sg44(i)=xmlt1*sg42(i)*(od2g+ap34*sg42(i))
   30 continue
      do 40 i=lft,llt
c
c     fiber breakage
c
      if (sig1(i).gt.0.) then
      ef2(i) =xt2*  max(zero,sig1(i))**2+sg44(i)-1.0
      else
      ef2(i)=-1.
      endif
c
c     matrix cracking
c
      if (sig2(i).gt.0.) then
      em2(i) =yt2*  max(zero,sig2(i))**2+sg44(i)-1.0
      ed2(i)=-1.
      else
c
c     matrix crushing
c
      em2(i)=-1.
      ed2(i) =sd2*  min(zero,sig2(i))**2+xmlt2*sig2(i)+sg44(i)-1.0
      endif
c
c     delamination
c
      el2(i) =sn2*  max(zero,sig3(i))**2+syz2*sig5(i)**2+szx2*
     1        sig6(i)**2-1.0
   40 continue
c
      do 50 i=lft,llt
      ef(i)  =ef(i)*(.5-sign(half,ef2(i)))
      em(i)  =em(i)*(.5-sign(half,em2(i)))
      ed(i)  =ed(i)*(.5-sign(half,ed2(i)))
      el(i)  =el(i)*(.5-sign(half,el2(i)))
      efs(i) =efs(i)-.01*(1.-ef(i))
      ems(i) =ems(i)-.01*(1.-em(i))
      eds(i) =eds(i)-.01*(1.-ed(i))
      els(i) =els(i)-.01*(1.-el(i))
      efs(i) =  max(zero,efs(i))
      ems(i) =  max(zero,ems(i))
      eds(i) =  max(zero,eds(i))
      els(i) =  max(zero,els(i))
      efsels =efs(i)*els(i)
      sig1(i)=  min(sig1(i),efs(i)*sig1(i))
      sig2(i)=  min(sig2(i),efs(i)*sig2(i))
      sig3(i)=  min(sig3(i),efsels*sig3(i))
      sig4(i)=ems(i)*efs(i)*sig4(i)
      sig5(i)=efsels*sig5(i)
      sig6(i)=efsels*sig6(i)
      if (ed(i)*em(i).eq.0.0) then
      sig2(i)=  min(ems(i)*eds(i)*sig2(i),sig2(i))
      endif
   50 continue
c
      do 60 i=lft,llt
      prtr    =bulk*log(def(i))
      if (prtr.lt.0.0) then
      prssur  =(1.-ef(i))*  min(prtr,zero)
      sig1(i) =ef(i)*sig1(i)+prssur
      sig2(i) =ef(i)*sig2(i)+prssur
      sig3(i) =ef(i)*sig3(i)+prssur
      endif
      check(i)=(ef(i)+em(i)+eh(i))/3.0
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   60 continue
c
c     global stresses
c
      call gblm22
c
      return
      end
      subroutine lcsm22
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
      common/aux36/lft,llt
      common/aux40/
     1 s11(128),s12(128),s13(128),s21(128),s22(128),s23(128),
     2 s31(128),s32(128),s33(128),b11(128),b12(128),b13(128),
     3 b21(128),b22(128),b23(128),b31(128),b32(128),b33(128)
c
      do 10 i=lft,llt
      x21=x2(i)-x1(i)
      y21=y2(i)-y1(i)
      z21=z2(i)-z1(i)
      x31=x3(i)-x1(i)
      y31=y3(i)-y1(i)
      z31=z3(i)-z1(i)
      x41=x4(i)-x1(i)
      y41=y4(i)-y1(i)
      z41=z4(i)-z1(i)
      x42=x4(i)-x2(i)
      y42=y4(i)-y2(i)
      z42=z4(i)-z2(i)
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
      return
      end
      subroutine ldsm22
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
      subroutine ldem22
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
      d3(i)=    s31(i)*a13(i)+s32(i)*a23(i)+s33(i)*a33(i)
      d4(i)=2.*(s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i))
      d5(i)=2.*(s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i))
      d6(i)=2.*(s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i))
   40 continue
      return
      end
      subroutine gblm22
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
      a31(i)=sig6(i)*s11(i)+sig5(i)*s21(i)+sig3(i)*s31(i)
      a32(i)=sig6(i)*s12(i)+sig5(i)*s22(i)+sig3(i)*s32(i)
      a33(i)=sig6(i)*s13(i)+sig5(i)*s23(i)+sig3(i)*s33(i)
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
      subroutine f3dm23(cm,bqs,tnew,fval,thrmpr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     thermal-orthotropic-elastic material
c     this model is like model 21 except that the modulii are
c     temperature dependent
c
c     valid for large displacements and rotations but small strain
c     major reference: cook,'concepts and applications for finite
c                      element analysis',wiley
c     (this model has not been completely debugged)
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &xl1(128),xm1(128),xn1(128),xl2(128),xm2(128),xn2(128),tref(128),
     &otmp(128),ctmp(128),x17(128),x28(128),x35(128),x46(128),
     &y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),
     &df31(128),rx(128),
     &sp1(128),sp2(128),sp3(128),sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),qf(128),q1(128),q2(128),
     &q3(128),q4(128),q5(128),q6(128),q7(128),q8(128),q9(128)
      common/aux18/dde(128),df(128)
      common/aux19/epsr(128),epss(128),epst(128),xl3(128),xm3(128),
     1 xn3(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/e1(128),e2(128),e3(128),e4(128),e5(128),e6(128)
      common/aux41/qs(128)
      common/ortho/c11,c21,c31,c41,c51,c61,c12,c22,c32,c42,c52,c62,
     1 c13,c23,c33,c43,c53,c63,c14,c24,c34,c44,c54,c64,c15,c25,c35,
     2 c45,c55,c65,c16,c26,c36,c46,c56,c66
      dimension cm(*),c(6,6),temp(6,6),d(6,6),t(6,6),thrmpr(48,*),
     1 bqs(*),fval(*),tnew(*),thrmo(21),thrmn(21)
c
      equivalence (c11,c)
c
c     compute deformation gradients in undeformed configuration
c
      mx=48*(mxt(lft)-1)
      sndspd=cm(mx+21)
      numpts=nint(cm(mx+1))
      do 10 i=1,36
   10 d(i,1)=0.0
      if (itemp.gt.0) then
      do 12 i=lft,llt
   12 ctmp(i)=fval(itemp)
      else
      do 14 i=lft,llt
   14 ctmp(i)=.125*(tnew(ix1(i))+tnew(ix2(i))+tnew(ix3(i))+tnew(ix4(i))+
     1              tnew(ix5(i))+tnew(ix6(i))+tnew(ix7(i))+tnew(ix8(i)))
      endif
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
      z46(i)=z4(i)-z6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dde(i)*bqs(i))*dt1
   20 continue
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
c
c     green-st.venant strain  e=(c-1)/2
c     where c is the right cauchy-green tensor
c
      do 40 i=lft,llt
      e1(i)=.5*(eps1(i)*eps1(i)+eps4(i)*eps4(i)+eps7(i)*eps7(i)-1.)
      e2(i)=.5*(eps2(i)*eps2(i)+eps5(i)*eps5(i)+eps8(i)*eps8(i)-1.)
      e3(i)=.5*(eps3(i)*eps3(i)+eps6(i)*eps6(i)+eps9(i)*eps9(i)-1.)
      e4(i)=eps1(i)*eps2(i)+eps4(i)*eps5(i)+eps7(i)*eps8(i)
      e5(i)=eps2(i)*eps3(i)+eps5(i)*eps6(i)+eps8(i)*eps9(i)
   40 e6(i)=eps1(i)*eps3(i)+eps4(i)*eps6(i)+eps7(i)*eps9(i)
c
      do 50 i=lft,llt
      xl3(i)=xm1(i)*xn2(i)-xm2(i)*xn1(i)
      xm3(i)=xn1(i)*xl2(i)-xn2(i)*xl1(i)
   50 xn3(i)=xl1(i)*xm2(i)-xl2(i)*xm1(i)
c
c     calculate transformation matrix
c
      do 80 i=lft,llt
      t(1,1)=xl1(i)*xl1(i)
      t(1,2)=xm1(i)*xm1(i)
      t(1,3)=xn1(i)*xn1(i)
      t(1,4)=xl1(i)*xm1(i)
      t(1,5)=xm1(i)*xn1(i)
      t(1,6)=xn1(i)*xl1(i)
      t(2,1)=xl2(i)*xl2(i)
      t(2,2)=xm2(i)*xm2(i)
      t(2,3)=xn2(i)*xn2(i)
      t(2,4)=xl2(i)*xm2(i)
      t(2,5)=xm2(i)*xn2(i)
      t(2,6)=xn2(i)*xl2(i)
      t(3,1)=xl3(i)*xl3(i)
      t(3,2)=xm3(i)*xm3(i)
      t(3,3)=xn3(i)*xn3(i)
      t(3,4)=xl3(i)*xm3(i)
      t(3,5)=xm3(i)*xn3(i)
      t(3,6)=xn3(i)*xl3(i)
      t(4,1)=2.*xl1(i)*xl2(i)
      t(4,2)=2.*xm1(i)*xm2(i)
      t(4,3)=2.*xn1(i)*xn2(i)
      t(4,4)=xl1(i)*xm2(i)+xl2(i)*xm1(i)
      t(4,5)=xm1(i)*xn2(i)+xm2(i)*xn1(i)
      t(4,6)=xn1(i)*xl2(i)+xn2(i)*xl1(i)
      t(5,1)=2.*xl2(i)*xl3(i)
      t(5,2)=2.*xm2(i)*xm3(i)
      t(5,3)=2.*xn2(i)*xn3(i)
      t(5,4)=xl2(i)*xm3(i)+xl3(i)*xm2(i)
      t(5,5)=xm2(i)*xn3(i)+xm3(i)*xn2(i)
      t(5,6)=xn2(i)*xl3(i)+xn3(i)*xl2(i)
      t(6,1)=2.*xl3(i)*xl1(i)
      t(6,2)=2.*xm3(i)*xm1(i)
      t(6,3)=2.*xn3(i)*xn1(i)
      t(6,4)=xl3(i)*xm1(i)+xl1(i)*xm3(i)
      t(6,5)=xm3(i)*xn1(i)+xm1(i)*xn3(i)
      t(6,6)=xn3(i)*xl1(i)+xn1(i)*xl3(i)
      call itrpo (thrmpr,ctmp(i),thrmpr(1,2),thrmn,numpts)
      s11n=1./thrmn(1)
      s22n=1./thrmn(2)
      s33n=1./thrmn(3)
      s44n=1./thrmn(10)
      s55n=1./thrmn(11)
      s66n=1./thrmn(12)
      s12n=-thrmn(4)*s22n
      s13n=-thrmn(5)*s33n
      s23n=-thrmn(6)*s33n
      alan=thrmn(7)
      albn=thrmn(8)
      alcn=thrmn(9)
      deltem=ctmp(i)-tref(i)
      s1122=s11n*s22n
      s1123=s11n*s23n
      s2213=s22n*s13n
      s1212=s12n*s12n
      s1223=s12n*s23n
      si=1./(s1122*s33n-s1123*s23n-s2213*s13n-
     1  s33n*s1212+2.*s1223*s13n)
      d(1,1)=si*(s22n*s33n-s23n*s23n)
      d(1,2)=si*(s13n*s23n-s33n*s12n)
      d(2,2)=si*(s33n*s11n-s13n*s13n)
      d(2,3)=si*(s12n*s13n-s1123)
      d(3,3)=si*(s1122-s1212)
      d(1,3)=si*(s1223-s2213)
      d(4,4)=1./s44n
      d(5,5)=1./s55n
      d(6,6)=1./s66n
      d(3,2)=d(2,3)
      d(3,1)=d(1,3)
      d(2,1)=d(1,2)
      epsr(i)=deltem*alan
      epss(i)=deltem*albn
      epst(i)=deltem*alcn
c
c     transform constitutive matrix
c
      do 60 m=1,6
      do 60 n=1,6
      temp(m,n)=0.0
      do 60 k=1,6
   60 temp(m,n)=temp(m,n)+d(m,k)*t(k,n)
      do 70 m=1,6
      do 70 n=m,6
      c(m,n)=0.0
      do 70 k=1,6
      c(m,n)=c(m,n)+t(k,m)*temp(k,n)
   70 c(n,m)=c(m,n)
c
      e1(i)=e1(i)-t(1,1)*epsr(i)-t(2,1)*epss(i)-t(3,1)*epst(i)
      e2(i)=e2(i)-t(1,2)*epsr(i)-t(2,2)*epss(i)-t(3,2)*epst(i)
      e3(i)=e3(i)-t(1,3)*epsr(i)-t(2,3)*epss(i)-t(3,3)*epst(i)
      e4(i)=e4(i)-2.*(t(1,4)*epsr(i)+t(2,4)*epss(i)+t(3,4)*epst(i))
      e5(i)=e5(i)-2.*(t(1,5)*epsr(i)+t(2,5)*epss(i)+t(3,5)*epst(i))
      e6(i)=e6(i)-2.*(t(1,6)*epsr(i)+t(2,6)*epss(i)+t(3,6)*epst(i))
      sp1(i)=c11*e1(i)+c12*e2(i)+c13*e3(i)+c14*e4(i)+c15*e5(i)+c16*e6(i)
      sp2(i)=c21*e1(i)+c22*e2(i)+c23*e3(i)+c24*e4(i)+c25*e5(i)+c26*e6(i)
      sp3(i)=c31*e1(i)+c32*e2(i)+c33*e3(i)+c34*e4(i)+c35*e5(i)+c36*e6(i)
      sp4(i)=c41*e1(i)+c42*e2(i)+c43*e3(i)+c44*e4(i)+c45*e5(i)+c46*e6(i)
      sp5(i)=c51*e1(i)+c52*e2(i)+c53*e3(i)+c54*e4(i)+c55*e5(i)+c56*e6(i)
   80 sp6(i)=c61*e1(i)+c62*e2(i)+c63*e3(i)+c64*e4(i)+c65*e5(i)+c66*e6(i)
c
c     cauchy stress
c
      do 90 i=lft,llt
      rx(i)=1./df(i)
      q1(i)=eps1(i)*sp1(i)+eps2(i)*sp4(i)+eps3(i)*sp6(i)
      q2(i)=eps2(i)*sp2(i)+eps1(i)*sp4(i)+eps3(i)*sp5(i)
      q3(i)=eps3(i)*sp3(i)+eps1(i)*sp6(i)+eps2(i)*sp5(i)
      q4(i)=eps4(i)*sp1(i)+eps5(i)*sp4(i)+eps6(i)*sp6(i)
      q5(i)=eps5(i)*sp2(i)+eps4(i)*sp4(i)+eps6(i)*sp5(i)
      q6(i)=eps6(i)*sp3(i)+eps4(i)*sp6(i)+eps5(i)*sp5(i)
      q7(i)=eps7(i)*sp1(i)+eps8(i)*sp4(i)+eps9(i)*sp6(i)
      q8(i)=eps8(i)*sp2(i)+eps7(i)*sp4(i)+eps9(i)*sp5(i)
   90 q9(i)=eps9(i)*sp3(i)+eps7(i)*sp6(i)+eps8(i)*sp5(i)
c
      do 100 i=lft,llt
      sig1(i)=rx(i)*(eps1(i)*q1(i)+eps2(i)*q2(i)+eps3(i)*q3(i))
      sig2(i)=rx(i)*(eps4(i)*q4(i)+eps5(i)*q5(i)+eps6(i)*q6(i))
      sig3(i)=rx(i)*(eps7(i)*q7(i)+eps8(i)*q8(i)+eps9(i)*q9(i))
      sig4(i)=rx(i)*(eps4(i)*q1(i)+eps5(i)*q2(i)+eps6(i)*q3(i))
      sig5(i)=rx(i)*(eps7(i)*q4(i)+eps8(i)*q5(i)+eps9(i)*q6(i))
      sig6(i)=rx(i)*(eps7(i)*q1(i)+eps8(i)*q2(i)+eps9(i)*q3(i))
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
  100 cb(i)=sndspd
c
      return
      end
      subroutine f3dm24 (cm,bqs,npc,plc)
c     implicit double precision (a-h,o-z)                                    dp
c
c     isotropic elastic-plastic material w/rate dependence
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 da1(128),da2(128),da3(128),deps(128),ak2(128),
     2 aj2(128),depn(128),scle(128),depi(128),
     3 q1(128),q2(128),q3(128),q4(128),fac1(128),qhs(128),ak(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),ep(128),
     3 epx1(128),epx2(128),epx4(128),epx5(128),epx6(128),
     4 value(128),effs(128)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),p(128)
      common/aux36/lft,llt
      common/aux41/qq1(128),cbb(128),aj1(128)
      common/double/iprec,ncpw,unit
      dimension  cm(*),bqs(*),npc(*),plc(*)
      data third/.3333333333333/
c
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+2)
      qs=cm(mx+3)
      qt=cm(mx+4)
      fs=cm(mx+5)
      i1=cm(mx+48)
      qh=ym*qt/(ym-qt)
      g =ym/(1.+pr)
      bk=-ym/(1.-2.*pr)
      ss=.66666666666667*g-third*bk
      gdt=dt1*g
      gd2=.5*gdt
      blk=dt1*bk
c
c     compute trial stress
c
      do 20 i=lft,llt
      value(i)=1.0
      cb(i)  =ss
      qhs(i) =qh
      davg(i)=-third*(d1(i)+d2(i)+d3(i))
      p(i)   =blk*davg(i)
      po(i)  =po(i)-p(i)
      if(ep(i).gt.fs) qh=qh*.001
      ak(i)  =qs+qh*ep(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
      sig1(i)=sig1(i)+p(i)+gdt*(d1(i)+davg(i))
      sig2(i)=sig2(i)+p(i)+gdt*(d2(i)+davg(i))
      sig3(i)=sig3(i)+p(i)+gdt*(d3(i)+davg(i))
      sig4(i)=sig4(i)+gd2*d4(i)
      sig5(i)=sig5(i)+gd2*d5(i)
   20 sig6(i)=sig6(i)+gd2*d6(i)
c
c     variable hardening moduli
c
      if (cm(mx+7).ne.0.0) then
      call vyield (cm(mx+6),cm(mx+14),ep,ak,qhs)
      endif
      if (i1.ne.0) then
      i2=npc(i1)
      n1=(npc(i1+1)-i2)/2
      do 30 i=lft,llt
      d1d=d1(i)+davg(i)
      d2d=d2(i)+davg(i)
      d3d=d3(i)+davg(i)
      effs(i)=sqrt(2.*(d1d**2+d2d**2+d3d**2
     1       +0.5*(d4(i)**2+d5(i)**2+d6(i)**2))/2.)
   30 continue
      call gtpcve(plc(i2),effs,n1,value,lft,llt)
      endif
c
      fac2=1.5*g
      do 40 i=lft,llt
      ak(i)=value(i)*ak(i)
      if(ep(i).gt.fs) ak(i)=ak(i)*.001
      if(ep(i).gt.fs) qhs(i)=qhs(i)*.001
      q1(i)=po(i)+sig1(i)
      q2(i)=po(i)+sig2(i)
      q3(i)=po(i)+sig3(i)
      aj2(i)=sig4(i)*sig4(i)+sig5(i)*sig5(i)+sig6(i)*sig6(i)
     1      -q1(i)*q2(i)-q2(i)*q3(i)-q1(i)*q3(i)
      ak2(i)=3.0*aj2(i)-ak(i)*ak(i)
      fac1(i)=1./(fac2+qhs(i))
   40 scle(i)=.5*(1.0+sign(1.*unit,ak2(i)))
      do 50 i=lft,llt
   50 aj1(i)=sqrt(3.0*abs(aj2(i)))+1.0-scle(i)
      do 70 i=lft,llt
      depi(i)=scle(i)*fac1(i)*(aj1(i)-ak(i))
      ep(i)=ep(i)+depi(i)
      deps(i)=scle(i)*fac2*depi(i)/aj1(i)
      sig1(i)=sig1(i)-deps(i)*q1(i)
      sig2(i)=sig2(i)-deps(i)*q2(i)
      sig3(i)=sig3(i)-deps(i)*q3(i)
      sig4(i)=sig4(i)-deps(i)*sig4(i)
      sig5(i)=sig5(i)-deps(i)*sig5(i)
      sig6(i)=sig6(i)-deps(i)*sig6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   70 continue
c
      return
      end
      subroutine f3dm25(cm)                                             vax75
c     implicit double precision (a-h,o-w,y-z)                           vax75dp
c     implicit double precision (x)                                     vax75
      common/bk02/iburn,dt1,dt2,isdo                                    vax75
      common/aux2/dd1(128),dd2(128),dd3(128),dd4(128),dd5(128),         vax75
     1 dd6(128),wzzdt(128),wyydt(128),wxxdt(128)                        vax75
      common/aux14/                                                     vax75
     1 sig1(128),sig2(128),sig3(128),sig4(128),                         vax75
     2 sig5(128),sig6(128),epx1(128),epx2(128),epa1(128),epa2(128),     vax75
     3 epa4(128),epa5(128),epa6(128)                                    vax75
      common/aux18/dd(128),def(128)                                     vax75
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),        vax75
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel             vax75
      common/aux35/rhoa(128),cb(128),davg(128),p(128)                   vax75
      common/aux36/lft,llt                                              vax75
      dimension  cm(*)                                                  vax75
      common/dbk02/jiburn,xdt1,xdt2,jisdo                               vax75
      common/daux2/xdd1(128),xdd2(128),xdd3(128),xdd4(128),xdd5(128),   vax75
     1 xdd6(128),xwzzdt(128),xwyydt(128),xwxxdt(128)                    vax75
      common/daux14/                                                    vax75
     1 xsig1(128),xsig2(128),xsig3(128),xsig4(128),                     vax75
     2 xsig5(128),xsig6(128),xepx1(128),xepx2(128),xepa1(128),          vax75
     3 xepa2(128),xepa4(128),xepa5(128),xepa6(128)                      vax75
      common/daux18/xdd(128),xdef(128)                                  vax75
      common/daux33/jix1(128),jix2(128),jix3(128),jix4(128),jix5(128),  vax75
     1        jix6(128),jix7(128),jix8(128),jmxt(128),jnmel             vax75
      common/daux35/xrhoa(128),xcb(128),xdavg(128),xp(128)              vax75
      common/daux36/jlft,jllt                                           vax75
      dimension  xcm(19)                                                vax75
c
      jiburn=iburn                                                      vax75
      xdt1=dble(dt1)                                                    vax75
      xdt2=dble(dt2)                                                    vax75
      jisdo=isdo                                                        vax75
      jnmel=nmel                                                        vax75
      jlft=lft                                                          vax75
      jllt=llt                                                          vax75
      do 10 i=lft,llt                                                   vax75
      xdd1(i)=dble(dd1(i))                                              vax75
      xdd2(i)=dble(dd2(i))                                              vax75
      xdd3(i)=dble(dd3(i))                                              vax75
      xdd4(i)=dble(dd4(i))                                              vax75
      xdd5(i)=dble(dd5(i))                                              vax75
      xdd6(i)=dble(dd6(i))                                              vax75
      xsig1(i)=dble(sig1(i))                                            vax75
      xsig2(i)=dble(sig2(i))                                            vax75
      xsig3(i)=dble(sig3(i))                                            vax75
      xsig4(i)=dble(sig4(i))                                            vax75
      xsig5(i)=dble(sig5(i))                                            vax75
      xsig6(i)=dble(sig6(i))                                            vax75
      xepx1(i)=dble(epx1(i))                                            vax75
      xepx2(i)=dble(epx2(i))                                            vax75
      xepa1(i)=dble(epa1(i))                                            vax75
      xepa2(i)=dble(epa2(i))                                            vax75
      xepa4(i)=dble(epa4(i))                                            vax75
      xepa5(i)=dble(epa5(i))                                            vax75
      xepa6(i)=dble(epa6(i))                                            vax75
      xdd(i)=dble(dd(i))                                                vax75
      xdef(i)=dble(def(i))                                              vax75
      jmxt(i)=mxt(i)                                                    vax75
      xrhoa(i)=dble(rhoa(i))                                            vax75
      xcb(i)=dble(cb(i))                                                vax75
      xdavg(i)=dble(davg(i))                                            vax75
      xp(i)=dble(p(i))                                                  vax75
   10 continue                                                          vax75
      mx=48*(mxt(lft)-1)                                                vax75
      do 15 i=1,19                                                      vax75
      xcm(i)=dble(cm(mx+i))                                             vax75
   15 continue                                                          vax75
c
      call f3dm25a(xcm)                                                 vax75
c
      do 20 i=lft,llt                                                   vax75
      sig1(i)=xsig1(i)                                                  vax75
      sig2(i)=xsig2(i)                                                  vax75
      sig3(i)=xsig3(i)                                                  vax75
      sig4(i)=xsig4(i)                                                  vax75
      sig5(i)=xsig5(i)                                                  vax75
      sig6(i)=xsig6(i)                                                  vax75
      epx1(i)=xepx1(i)                                                  vax75
      epx2(i)=xepx2(i)                                                  vax75
      epa1(i)=xepa1(i)                                                  vax75
      epa2(i)=xepa2(i)                                                  vax75
      epa4(i)=xepa4(i)                                                  vax75
      epa5(i)=xepa5(i)                                                  vax75
      epa6(i)=xepa6(i)                                                  vax75
      cb(i)=xcb(i)                                                      vax75
      davg(i)=xdavg(i)                                                  vax75
      p(i)=xp(i)                                                        vax75
   20 continue                                                          vax75
c
      return                                                            vax75
      end                                                               vax75
c     subroutine f3dm25(cm)                                             cray1
      subroutine f3dm25a(cm)                                            vax75
c
c.... optional vectorized version
c
c     implicit double precision(a-h,o-z)                                vax75
      parameter(tol=1.0e-7)
c     common/bk02/iburn,dt1,dt2,isdo                                    cray1
c     common/aux2/dd1(128),dd2(128),dd3(128),dd4(128),dd5(128),         cray1
c    1 dd6(128),wzzdt(128),wyydt(128),wxxdt(128)                        cray1
c     common/aux14/                                                     cray1
c    1 sig1(128),sig2(128),sig3(128),sig4(128),                         cray1
c    2 sig5(128),sig6(128),epx1(128), capa(128),                        cray1
c    3 epa1(128),epa2(128),epa4(128),epa5(128),epa6(128)                cray1
c     common/aux18/dd(128),def(128)                                     cray1
c     common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),        cray1
c    1             ix6(128),ix7(128),ix8(128),mxt(128),nmel             cray1
c     common/aux35/rhoa(128),cb(128),davg(128),p(128)                   cray1
c     common/aux36/lft,llt                                              cray1
      common/dbk02/iburn,dt1,dt2,isdo                                   vax75
      common/daux2/dd1(128),dd2(128),dd3(128),dd4(128),dd5(128),        vax75
     1 dd6(128),wzzdt(128),wyydt(128),wxxdt(128)                        vax75
      common/daux14/                                                    vax75
     1 sig1(128),sig2(128),sig3(128),sig4(128),                         vax75
     2 sig5(128),sig6(128),epx1(128), capa(128),                        vax75
     3 epa1(128),epa2(128),epa4(128),epa5(128),epa6(128)                vax75
      common/daux18/dd(128),def(128)                                    vax75
      common/daux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),       vax75
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel             vax75
      common/daux35/rhoa(128),cb(128),davg(128),p(128)                  vax75
      common/daux36/lft,llt                                             vax75
      common/dauxplt/ el,xl,evpi,sj1,sj2,tmises,fj1,xmtype,xit,nocon    vax75
      common/double/iprec,ncpw,unit
      dimension gtr11(128),gtr22(128),gtr33(128),gtr12(128),
     1          gtr23(128),gtr31(128)
      dimension s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),
     1      x1tr(128),
     1      x2dtr(128),x1(128),x2d(128),tmises(128),tcrit(128)
      dimension f1tr(128),fe(128),fep(128),fc2tr(128),f2tr(128)
      dimension capan(128),capal(128),xk(128),
     1          evpn(128),devp(128),
     2          xkn(128),elcapn(128),
     3          devpb(128),x1crt(128),evp(128),iupd(128),imode(128),
     4          iter(128)
      dimension dhdk(128),dfdk(128),elcap(128),omega(128),dcd(128),
     1  f(128),delcap(128),delam(128),fac1(128),fac2(128),
     1  sdtr11(128),sdtr22(128),sdtr33(128),sdev(6,128),fbar(128)
      dimension  cm(1)
      data third /0.333333333333333/
c
c.... mtype :  0 = tension cutoff, 1 = elastic, 2 = failure,
c....          3 = cap mode,       4= compression corner mode
c....          5 = tension corner mode
c....          6 = 'gap region' (cap will catch up)
c.... ltype : 1 = soil, 2 = rock
c
      unit=1.0
      geop=0.
      mx=48*(mxt(lft)-1)
      bulk=cm(mx+1)
      shear=cm(mx+2)
      alpha=cm(mx+3)
      theta=cm(mx+4)
      gama=cm(mx+5)
      beta=cm(mx+6)
      r=cm(mx+7)
      d=cm(mx+8)
      w=cm(mx+9)
      cbar=cm(mx+18)
      falfac=cm(mx+19)
      alpha=alpha-falfac
      z=cm(mx+10)
      nplot=cm(mx+11)
      ivec=1
      if (cm(mx+17).ne.0.) ivec=0
      if (falfac .ne. 0.) then
      do 500 i=lft,llt
      trs=(sig1(i)+sig2(i)+sig3(i))/3.
      sdev(1,i)=sig1(i)-trs
      sdev(2,i)=sig2(i)-trs
      sdev(3,i)=sig3(i)-trs
      sdev(4,i)=sig4(i)
      sdev(5,i)=sig5(i)
      sdev(6,i)=sig6(i)
  500 continue
      do 510 i=lft,llt
      sig1(i)=sig1(i)-epa1(i)
      sig2(i)=sig2(i)-epa2(i)
      sig3(i)=sig3(i)+epa1(i)+epa2(i)
      sig4(i)=sig4(i)-epa4(i)
      sig5(i)=sig5(i)-epa5(i)
      sig6(i)=sig6(i)-epa6(i)
  510 continue
      endif
      ltype=cm(mx+12)
      tcut=cm(mx+13)
      fcut=cm(mx+16)
      t=max(fcut,tcut+3.*geop)
      fet=alpha - gama*exp(-beta*t) + theta*t
      fept = gama*beta*exp(-beta*t) + theta
c
      do 10 i=lft,llt
      cb(i)=bulk+4.*shear/3.
      sig1(i) = -sig1(i)
      sig2(i) = -sig2(i)
      sig3(i) = -sig3(i)
      sig4(i) = -sig4(i)
      sig5(i) = -sig5(i)
      sig6(i) = -sig6(i)
      dd1(i) = -dd1(i)
      dd2(i) = -dd2(i)
      dd3(i) = -dd3(i)
      dd4(i) = -dd4(i)/2.0
      dd5(i) = -dd5(i)/2.0
      dd6(i) = -dd6(i)/2.0
      iter(i)=0
   10 continue
      do 11 i=lft,llt
      davg(i)=(1./3.)*dd(i)
      p(i)=3.0*bulk*dt1*davg(i)
      gtr11(i) = sig1(i) + p(i) +2.0*shear*dt1*(dd1(i) - davg(i))
      gtr22(i) = sig2(i) + p(i) +2.0*shear*dt1*(dd2(i) - davg(i))
      gtr33(i) = sig3(i) + p(i) +2.0*shear*dt1*(dd3(i) - davg(i))
      gtr12(i) = sig4(i)        +2.0*shear*dt1* dd4(i)
      gtr23(i) = sig5(i)        +2.0*shear*dt1* dd5(i)
      gtr31(i) = sig6(i)        +2.0*shear*dt1* dd6(i)
      x1tr(i) = gtr11(i) + gtr22(i) + gtr33(i)
      s1(i) = gtr11(i) - (1./3.)*x1tr(i)
      s2(i) = gtr22(i) - (1./3.)*x1tr(i)
      s3(i) = gtr33(i) - (1./3.)*x1tr(i)
      sdtr11(i)=s1(i)
      sdtr22(i)=s2(i)
      sdtr33(i)=s3(i)
      x2dtr(i)=0.5*(s1(i)**2 + s2(i)**2 + s3(i)**2
     1 + 2.*(gtr12(i)**2 + gtr23(i)**2 + gtr31(i)**2) )
      x2dtr(i)=sqrt(x2dtr(i))
   11 continue
      do 13 i=lft,llt
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xkn(i) = capa(i) + r*fe(i)
      elcapn(i) = max(capa(i),0.0*unit)
      tmises(i)=abs(xkn(i) - elcapn(i))/r
   13 continue
c
      do 12 i=lft,llt
      fe(i) = alpha - gama*exp(-beta*x1tr(i)) + theta*x1tr(i)
      f1tr(i) = x2dtr(i) - min(fe(i),tmises(i))
      tcrit(i) = t - 9.*bulk/shear*(x2dtr(i) - fet)*fept
      fe(i) = alpha - gama*exp(-beta*elcapn(i)) + theta*elcapn(i)
      fep(i) = beta*gama*exp(-beta*elcapn(i)) + theta
      fc2tr(i)=((xkn(i)-elcapn(i))**2-(x1tr(i)-elcapn(i))**2)/r**2
      f2tr(i)=x2dtr(i)**2 - fc2tr(i)
      x1crt(i) = elcapn(i) - (x2dtr(i) - fe(i))
     1    *9.*bulk*fep(i)/shear
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      capal(i) = min(0.0*unit,capa(i) - tol*fe(i))
      fe(i) = alpha - gama*exp(-beta*capal(i)) + theta*capal(i)
      xk(i) = capal(i) + r*fe(i)
      evpn(i) =  w*(1. - exp(d*(z-xkn(i))))
      devpb(i) = w*(1. - exp(d*(z-xk(i)))) - evpn(i)
      capan(i) = capa(i)
      iupd(i)=1
   12 continue
      do 20 i=lft,llt
      if( (x1tr(i).le. t) .and. (x2dtr(i).le.fet) .and. (ltype.eq.1)
     1  .and. (capan(i).gt.0.0) .and. (devpb(i).ge.0.0) ) then
           imode(i)=2
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).le.fet) .and.
     1   (ltype.eq.1)
     1  .and. (capan(i).gt.0.0) .and. (devpb(i).lt.0.0) ) then
           imode(i)=7
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).le.fet) .and.
     1  ((ltype.eq.2).or.(capan(i).le.0.0)) ) then
           imode(i)=3
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet) .and.
     1  (x1tr(i).le.tcrit(i)) .and. (ltype.eq.1)
     2  .and. (capan(i).gt.0.0) .and. (devpb(i).ge.0.0) ) then
           imode(i)=4
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet) .and.
     1  (x1tr(i).le.tcrit(i)) .and. (ltype.eq.1)
     2  .and. (capan(i).gt.0.0) .and. (devpb(i).lt.0.0) ) then
           imode(i)=6
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet) .and.
     1  (x1tr(i).le.tcrit(i)) .and. ((ltype.eq.2) .or.
     2  (capan(i).le.0.0) ) ) then
           imode(i)=5
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet) .and.
     1  (x1tr(i).gt.tcrit(i)) .and. (capan(i).ge.0.0) ) then
           imode(i)=10
      elseif( (x1tr(i).le. t) .and. (x2dtr(i).gt.fet) .and.
     1  (x1tr(i).gt.tcrit(i)) .and. (capan(i).lt.0.0) ) then
           imode(i)=8
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).le.tol) ) then
           imode(i)=1
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).ge. x1crt(i)) ) then
           imode(i)=9
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).lt. x1crt(i))
     2  .and. (capan(i).ge.0.0) ) then
           imode(i)=10
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).le.elcapn(i))
     1  .and. (f1tr(i).gt.tol) .and. (x1tr(i).lt. x1crt(i))
     2  .and. (capan(i).lt.0.0) ) then
           imode(i)=8
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).gt.elcapn(i))
     1  .and. (f2tr(i).lt.tol)) then
           imode(i)=1
      elseif( (x1tr(i).gt. t) .and. (x1tr(i).gt.elcapn(i))
     1  .and. (f2tr(i).ge.tol)) then
           imode(i)=11
      else
           stop 'f3dm251'
      endif
   20 continue
c
      do 110 i=lft,llt
      if(imode(i).eq.1) then
      x1(i) = x1tr(i)
      x2d(i) = x2dtr(i)
      capa(i) = capan(i)
      p(i) = third*x1(i) + geop
      sig1(i) = s1(i) + p(i)
      sig2(i) = s2(i) + p(i)
      sig3(i) = s3(i) + p(i)
      sig4(i) = gtr12(i)
      sig5(i) = gtr23(i)
      sig6(i) = gtr31(i)
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
  110 continue
      idone=0
      do 15 i=lft,llt
      idone=idone+iupd(i)
   15 continue
      if(idone.eq.0) go to 900
      do 30 i=lft,llt
      if(imode(i).eq.2) then
      x1(i)=t
      x2d(i)=x2dtr(i)
      capa(i)=0.0
      p(i)=third*x1(i) + geop
      sig1(i) = s1(i) + p(i)
      sig2(i) = s2(i) + p(i)
      sig3(i) = s3(i) + p(i)
      sig4(i) = gtr12(i)
      sig5(i) = gtr23(i)
      sig6(i) = gtr31(i)
      xk(i) = alpha - gama
      evp(i)=w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   30 continue
      do 50 i=lft,llt
      if(imode(i).eq.3) then
      x1(i)=t
      x2d(i)=x2dtr(i)
      capa(i)=capan(i)
      p(i)=third*x1(i) + geop
      sig1(i) = s1(i) + p(i)
      sig2(i) = s2(i) + p(i)
      sig3(i) = s3(i) + p(i)
      sig4(i) = gtr12(i)
      sig5(i) = gtr23(i)
      sig6(i) = gtr31(i)
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i)=w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   50 continue
c
c.... new mode
c
      do 60 i=lft,llt
      if(imode(i).eq.4) then
      x1(i)=t
      x2d(i)=fet
      capa(i)=0.0
      p(i)=third*x1(i) + geop
      ratio = x2d(i)/x2dtr(i)
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i)=w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   60 continue
      do 80 i=lft,llt
      if(imode(i).eq.5) then
      x1(i)=t
      x2d(i)=fet
      capa(i) = capan(i)
      p(i) = third*x1(i) + geop
      ratio=x2d(i)/x2dtr(i)
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   80 continue
      do 70 i=lft,llt
      if (imode(i).eq.6) then
      x1(i)=t
      x2d(i)=fet
      x1old = sig1(i) + sig2(i) + sig3(i)
      devp(i) =  dd(i) - (x1(i) - x1old)/(3.*bulk)
      xhat = min(devpb(i),devp(i))
      capa(i) = capa(i) + devp(i)*(capal(i) - capan(i))/xhat
      capa(i) = max(capa(i),0.0*unit)
      p(i) = third*x1(i) + geop
      ratio=x2d(i)/x2dtr(i)
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   70 continue
      do 40 i=lft,llt
      if( imode(i).eq.7) then
      x1(i)=t
      x2d(i)=x2dtr(i)
      x1old = sig1(i) + sig2(i) + sig3(i)
      devp(i) =  dd(i) - (x1(i) - x1old)/(3.*bulk)
      xhat = min(devpb(i),devp(i))
      capa(i) = capa(i) + devp(i)*(capal(i) - capan(i))/xhat
      capa(i) = max(capa(i),0.0*unit)
      p(i) = third*x1(i) + geop
      sig1(i) = s1(i) + p(i)
      sig2(i) = s2(i) + p(i)
      sig3(i) = s3(i) + p(i)
      sig4(i) = gtr12(i)
      sig5(i) = gtr23(i)
      sig6(i) = gtr31(i)
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   40 continue
c
c.... new mode
c
      do 100 i=lft,llt
      if(imode(i).eq.8) then
      x1(i) = x1tr(i)
      fe(i) = alpha - gama*exp(-beta*x1(i)) + theta*x1(i)
      x2d(i)=min(fe(i),tmises(i))
      capa(i) = capan(i)
      ratio = x2d(i)/x2dtr(i)
      p(i) = third*x1(i) + geop
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
  100 continue
      do 120 i=lft,llt
      if(imode(i).eq.9) then
      x1(i) = elcapn(i)
      x2d(i) = alpha - gama*exp(-beta*x1(i)) + theta*x1(i)
      capa(i) = capan(i)
      ratio = x2d(i)/x2dtr(i)
      p(i) = third*x1(i) + geop
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
  120 continue
      idone=0
      do 25 i=lft,llt
      idone=idone+iupd(i)
   25 continue
      if(idone.eq.0) go to 900
c
      do 90 i=lft,llt
      if(imode(i).eq.10 .and. ivec.eq.0) then
      sig1(i)=gtr11(i)
      sig2(i)=gtr22(i)
      sig3(i)=gtr33(i)
      sig4(i)=gtr12(i)
      sig5(i)=gtr23(i)
      sig6(i)=gtr31(i)
c
      call feit (capan(i),x1tr(i),x2dtr(i),t,alpha,gama,beta,
     1 theta,w,d,r,bulk,shear,x1(i),capa(i),sig1(i),sig2(i),
     2 sig3(i),sig4(i),sig5(i),sig6(i),iter(i),nocon)
c
      if(x1(i) .gt. elcapn(i)) x1(i)=elcapn(i)
      if( (ltype.eq.2) .or. (capan(i).eq.0.0) ) capa(i)=capan(i)
      if( x1(i) .gt. capa(i)) capa(i) = x1(i)
      if((ltype.eq.1).and.(capan(i).ge.0.0))
     1    capa(i)=max(capa(i),0.0*unit)
      fe(i) = alpha - gama*exp(-beta*x1(i)) + theta*x1(i)
      x2d(i)=min(fe(i),tmises(i))
      ratio = x2d(i)/x2dtr(i)
      p(i) = third*x1(i) + geop
      sig1(i) = s1(i)*ratio + p(i)
      sig2(i) = s2(i)*ratio + p(i)
      sig3(i) = s3(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   90 continue
c
      do 95 i=lft,llt
      if(imode(i).eq.10 .and. ivec.eq.1) then
      sig1(i)=gtr11(i)
      sig2(i)=gtr22(i)
      sig3(i)=gtr33(i)
      sig4(i)=gtr12(i)
      sig5(i)=gtr23(i)
      sig6(i)=gtr31(i)
      iter(i)=4
      capa(i)=capan(i)
      x1(i)=x1tr(i)
      x2d(i)=x2dtr(i)
      s1(i)=sig1(i)-x1tr(i)/3.
      s2(i)=sig2(i)-x1tr(i)/3.
      s3(i)=sig3(i)-x1tr(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama*beta
     1     *exp(-beta*capa(i))))
      omega(i)=theta+gama*beta*exp(-beta*x1(i))
      dcd(i)=shear+9.*bulk*omega(i)**2
      f(i)=x2d(i)-(alpha-gama*exp(-beta*x1(i))+theta*x1(i))
      delcap(i)=-3.*omega(i)*f(i)/(dcd(i)*dhdk(i))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      fac1(i)=shear/x2d(i)
      fac2(i)=3.*bulk*omega(i)
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)-fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)-fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)-fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1       +s4(i)**2+s5(i)**2+s6(i)**2)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama*beta
     1  *exp(-beta*capa(i))))
      omega(i)=theta+gama*beta*exp(-beta*x1(i))
      dcd(i)=shear+9.*bulk*omega(i)**2
      f(i)=x2d(i)-(alpha-gama*exp(-beta*x1(i))+theta*x1(i))
      delcap(i)=-3.*omega(i)*f(i)/(dcd(i)*dhdk(i))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      fac1(i)=shear/x2d(i)
      fac2(i)=3.*bulk*omega(i)
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)-fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)-fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)-fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1       +s4(i)**2+s5(i)**2+s6(i)**2)
      endif
   95 continue
      do 97 i=lft,llt
      if(imode(i).eq.10 .and. ivec.eq.1) then
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1  *beta*exp(-beta*capa(i))))
      omega(i)=theta+gama*beta*exp(-beta*x1(i))
      dcd(i)=shear+9.*bulk*omega(i)**2
      f(i)=x2d(i)-(alpha-gama*exp(-beta*x1(i))+theta*x1(i))
      delcap(i)=-3.*omega(i)*f(i)/(dcd(i)*dhdk(i))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      fac1(i)=shear/x2d(i)
      fac2(i)=3.*bulk*omega(i)
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)-fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)-fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)-fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1       +s4(i)**2+s5(i)**2+s6(i)**2)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1   *beta*exp(-beta*capa(i))))
      omega(i)=theta+gama*beta*exp(-beta*x1(i))
      dcd(i)=shear+9.*bulk*omega(i)**2
      f(i)=x2d(i)-(alpha-gama*exp(-beta*x1(i))+theta*x1(i))
      delcap(i)=-3.*omega(i)*f(i)/(dcd(i)*dhdk(i))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      fac1(i)=shear/x2d(i)
      fac2(i)=3.*bulk*omega(i)
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)-fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)-fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)-fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1       +s4(i)**2+s5(i)**2+s6(i)**2)
      endif
   97 continue
      do 98 i=lft,llt
      if(imode(i).eq.10 .and. ivec.eq.1) then
      if(x1(i) .gt. elcapn(i)) x1(i)=elcapn(i)
      if( (ltype.eq.2) .or. (capan(i).eq.0.0) ) capa(i)=capan(i)
      if( x1(i) .gt. capa(i)) capa(i) = x1(i)
      if((ltype.eq.1).and.(capan(i).ge.0.0))
     1        capa(i)=max(capa(i),0.0*unit)
      endif
   98 continue
c
      do 99 i=lft,llt
      if(imode(i).eq.10 .and. ivec.eq.1) then
      fe(i) = alpha - gama*exp(-beta*x1(i)) + theta*x1(i)
      x2d(i)=min(fe(i),tmises(i))
      ratio = x2d(i)/x2dtr(i)
      p(i) = third*x1(i) + geop
      sig1(i) = sdtr11(i)*ratio + p(i)
      sig2(i) = sdtr22(i)*ratio + p(i)
      sig3(i) = sdtr33(i)*ratio + p(i)
      sig4(i) = gtr12(i)*ratio
      sig5(i) = gtr23(i)*ratio
      sig6(i) = gtr31(i)*ratio
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
   99 continue
      do 160 i=lft,llt
      if(imode(i).eq.11 .and. ivec.eq.0) then
      sig1(i)=gtr11(i)
      sig2(i)=gtr22(i)
      sig3(i)=gtr33(i)
      sig4(i)=gtr12(i)
      sig5(i)=gtr23(i)
      sig6(i)=gtr31(i)
c
      call fcit (capan(i),x1tr(i),x2dtr(i),t,alpha,gama,beta,
     1 theta,w,d,r,bulk,shear,x1(i),capa(i),sig1(i),sig2(i),
     2 sig3(i),sig4(i),sig5(i),sig6(i),iter(i),nocon)
c
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
  160 continue
      do 165 i=lft,llt
      if(imode(i).eq.11 .and. ivec.eq.1) then
      sig1(i)=gtr11(i)
      sig2(i)=gtr22(i)
      sig3(i)=gtr33(i)
      sig4(i)=gtr12(i)
      sig5(i)=gtr23(i)
      sig6(i)=gtr31(i)
      iter(i)=4
      capa(i)=capan(i)
      x1(i)=x1tr(i)
      x2d(i)=x2dtr(i)
      s1(i)=sig1(i)-x1tr(i)/3.
      s2(i)=sig2(i)-x1tr(i)/3.
      s3(i)=sig3(i)-x1tr(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1    *beta*exp(-beta*capa(i))))
      if(capa(i).ge.0.0) then
      dfdk(i)=-2.*(abs(x1(i)-capa(i))/r**2
     1    + (alpha-gama*exp(-beta*capa(i))
     1    +theta*capa(i))*(theta+gama*beta*exp(-beta*capa(i))))
      else
      dfdk(i)=-2.*xk(i)*(1.+r*(theta+gama*beta
     1    *exp(-beta*capa(i))))/r**2
      endif
      elcap(i)=max(capa(i),0.0*unit)
      omega(i)=min(-2.*abs(x1(i)-elcap(i))/r**2,-1.e-15*unit)
      dcd(i)=4.*shear*x2d(i)**2 + 9.*bulk*(2.*(x1(i)
     1   -elcap(i))/r**2)**2
      f(i)=x2d(i)**2 + (x1(i)-elcap(i))**2/r**2
     1     - (xk(i)-elcap(i))**2/r**2
      delcap(i)=-f(i)/(dfdk(i)+(dcd(i)*dhdk(i))/(3.*omega(i)))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
c     elcap=max(capa(i),0.0*unit)
      fac1(i)=2.*shear
      fac2(i)=6.*bulk*abs(x1(i)-elcap(i))/r**2
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)+fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)+fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)+fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1 +s4(i)**2+s5(i)**2+s6(i)**2)
      endif
  165 continue
      do 166 i=lft,llt
      if(imode(i).eq.11 .and. ivec.eq.1) then
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1    *beta*exp(-beta*capa(i))))
      if(capa(i).ge.0.0) then
      dfdk(i)=-2.*(abs(x1(i)-capa(i))/r**2
     1    + (alpha-gama*exp(-beta*capa(i))
     1    +theta*capa(i))*(theta+gama*beta*exp(-beta*capa(i))))
      else
      dfdk(i)=-2.*xk(i)*(1.+r*(theta+gama*beta
     1    *exp(-beta*capa(i))))/r**2
      endif
      elcap(i)=max(capa(i),0.0*unit)
      omega(i)=min(-2.*abs(x1(i)-elcap(i))/r**2,-1.e-15*unit)
      dcd(i)=4.*shear*x2d(i)**2 + 9.*bulk*(2.*(x1(i)
     1   -elcap(i))/r**2)**2
      f(i)=x2d(i)**2 + (x1(i)-elcap(i))**2/r**2
     1     - (xk(i)-elcap(i))**2/r**2
      delcap(i)=-f(i)/(dfdk(i)+(dcd(i)*dhdk(i))/(3.*omega(i)))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
c     elcap=max(capa(i),0.0*unit)
      fac1(i)=2.*shear
      fac2(i)=6.*bulk*abs(x1(i)-elcap(i))/r**2
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)+fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)+fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)+fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1 +s4(i)**2+s5(i)**2+s6(i)**2)
      endif
  166 continue
      do 167 i=lft,llt
      if(imode(i).eq.11 .and. ivec.eq.1) then
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1    *beta*exp(-beta*capa(i))))
      if(capa(i).ge.0.0) then
      dfdk(i)=-2.*(abs(x1(i)-capa(i))/r**2
     1    + (alpha-gama*exp(-beta*capa(i))
     1    +theta*capa(i))*(theta+gama*beta*exp(-beta*capa(i))))
      else
      dfdk(i)=-2.*xk(i)*(1.+r*(theta+gama*beta
     1    *exp(-beta*capa(i))))/r**2
      endif
      elcap(i)=max(capa(i),0.0*unit)
      omega(i)=min(-2.*abs(x1(i)-elcap(i))/r**2,-1.e-15*unit)
      dcd(i)=4.*shear*x2d(i)**2 + 9.*bulk*(2.*(x1(i)
     1   -elcap(i))/r**2)**2
      f(i)=x2d(i)**2 + (x1(i)-elcap(i))**2/r**2
     1     - (xk(i)-elcap(i))**2/r**2
      delcap(i)=-f(i)/(dfdk(i)+(dcd(i)*dhdk(i))/(3.*omega(i)))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
c     elcap=max(capa(i),0.0*unit)
      fac1(i)=2.*shear
      fac2(i)=6.*bulk*abs(x1(i)-elcap(i))/r**2
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)+fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)+fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)+fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1 +s4(i)**2+s5(i)**2+s6(i)**2)
      endif
  167 continue
c
      do 168 i=lft,llt
      if(imode(i).eq.11 .and. ivec.eq.1) then
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
      dhdk(i)=w*d*exp(-d*xk(i))*(1.+r*(theta+gama
     1    *beta*exp(-beta*capa(i))))
      if(capa(i).ge.0.0) then
      dfdk(i)=-2.*(abs(x1(i)-capa(i))/r**2
     1    + (alpha-gama*exp(-beta*capa(i))
     1    +theta*capa(i))*(theta+gama*beta*exp(-beta*capa(i))))
      else
      dfdk(i)=-2.*xk(i)*(1.+r*(theta+gama*beta
     1    *exp(-beta*capa(i))))/r**2
      endif
      elcap(i)=max(capa(i),0.0*unit)
      omega(i)=min(-2.*abs(x1(i)-elcap(i))/r**2,-1.e-15*unit)
      dcd(i)=4.*shear*x2d(i)**2 + 9.*bulk*(2.*(x1(i)
     1   -elcap(i))/r**2)**2
      f(i)=x2d(i)**2 + (x1(i)-elcap(i))**2/r**2
     1     - (xk(i)-elcap(i))**2/r**2
      delcap(i)=-f(i)/(dfdk(i)+(dcd(i)*dhdk(i))/(3.*omega(i)))
      delam(i)=-dhdk(i)*delcap(i)/(3.*omega(i))
      capa(i)=capa(i)+delcap(i)
      xk(i)=capa(i)+r*(alpha-gama*exp(-beta*capa(i))+theta*capa(i))
c     elcap=max(capa(i),0.0*unit)
      fac1(i)=2.*shear
      fac2(i)=6.*bulk*abs(x1(i)-elcap(i))/r**2
      sig1(i)=sig1(i)-delam(i)*(fac1(i)*s1(i)+fac2(i))
      sig2(i)=sig2(i)-delam(i)*(fac1(i)*s2(i)+fac2(i))
      sig3(i)=sig3(i)-delam(i)*(fac1(i)*s3(i)+fac2(i))
      sig4(i)=sig4(i)-delam(i)*fac1(i)*s4(i)
      sig5(i)=sig5(i)-delam(i)*fac1(i)*s5(i)
      sig6(i)=sig6(i)-delam(i)*fac1(i)*s6(i)
      x1(i)=sig1(i)+sig2(i)+sig3(i)
      s1(i)=sig1(i)-x1(i)/3.
      s2(i)=sig2(i)-x1(i)/3.
      s3(i)=sig3(i)-x1(i)/3.
      s4(i)=sig4(i)
      s5(i)=sig5(i)
      s6(i)=sig6(i)
      x2d(i)=sqrt(.5*(s1(i)**2+s2(i)**2+s3(i)**2)
     1 +s4(i)**2+s5(i)**2+s6(i)**2)
      if(x1(i).lt.elcap(i)) then
      x1(i)=elcap(i)+abs(x1(i)-elcap(i))
      sig1(i)=s1(i)+x1(i)/3.
      sig2(i)=s2(i)+x1(i)/3.
      sig3(i)=s3(i)+x1(i)/3.
      endif
      fe(i) = alpha - gama*exp(-beta*capa(i)) + theta*capa(i)
      xk(i) = capa(i) + r*fe(i)
      evp(i) = w*(1. - exp(d*(z-xk(i))))
      iupd(i)=0
      endif
  168 continue
  900 continue
      idone=0
      do 180 i=lft,llt
      sig1(i) = -sig1(i)
      sig2(i) = -sig2(i)
      sig3(i) = -sig3(i)
      sig4(i) = -sig4(i)
      sig5(i) = -sig5(i)
      sig6(i) = -sig6(i)
      dd1(i) = -dd1(i)
      dd2(i) = -dd2(i)
      dd3(i) = -dd3(i)
      dd4(i) = -dd4(i)*2.0
      dd5(i) = -dd5(i)*2.0
      dd6(i) = -dd6(i)*2.0
      idone=idone+iupd(i)
  180 continue
      do 190 i=lft,llt
      if (nplot.eq.0) epx1(i)=0.0
      if (nplot.eq.1) epx1(i)=capa(i)
      if (nplot.eq.2) epx1(i)=xk(i)
      if (nplot.eq.3) epx1(i)=evp(i)
      if (nplot.eq.4) epx1(i)=x1(i)
      if (nplot.eq.5) epx1(i)=x2d(i)
      if (nplot.eq.6) epx1(i)=tmises(i)
      if (nplot.eq.7) epx1(i)=0.0
      if (nplot.eq.8) epx1(i)=imode(i)
      if (nplot.eq.9) epx1(i)=iter(i)
  190 continue
      if(idone.ne.0) stop 'f3dm252'
      if (falfac .ne. 0.) then
      do 600 i=lft,llt
      epa3=-epa1(i)-epa2(i)
      fac7= 0.5*(sig1(i)*epa1(i)+sig2(i)*epa2(i)+sig3(i)*epa3
     1    +2.*(sig4(i)*epa4(i)+sig5(i)*epa5(i)+sig6(i)*epa6(i)))
      fac8 = alpha - gama*exp(-beta*x1(i)) + theta*x1(i)
      fbar(i)=max(0.0*unit,1.-fac7/(1.*falfac*fac8))
  600 continue
      fac7=1./(2.*shear)
      do 610 i=lft,llt
      dtrs=(dd1(i)+dd2(i)+dd3(i))/3.
      ddev1=dd1(i)-dtrs
      ddev2=dd2(i)-dtrs
      strs=(sig1(i)+sig2(i)+sig3(i))/3.
      sx1=sig1(i)-strs
      sx2=sig2(i)-strs
      epa1(i)=epa1(i)+cbar*fbar(i)*(ddev1*dt1
     1        -fac7*(sx1-(sdev(1,i)-epa1(i))))
      epa2(i)=epa2(i)+cbar*fbar(i)*(ddev2*dt1
     1        -fac7*(sx2-(sdev(2,i)-epa2(i))))
      epa4(i)=epa4(i)+cbar*fbar(i)*(dd4(i)/2.*dt1
     1        -fac7*(sig4(i)-(sdev(4,i)-epa4(i))))
      epa5(i)=epa5(i)+cbar*fbar(i)*(dd5(i)/2.*dt1
     1        -fac7*(sig5(i)-(sdev(5,i)-epa5(i))))
      epa6(i)=epa6(i)+cbar*fbar(i)*(dd6(i)/2.*dt1
     1        -fac7*(sig6(i)-(sdev(6,i)-epa6(i))))
  610 continue
c
c.... radial return
c
      do 620 i=lft,llt
      epa3=-epa1(i)-epa2(i)
      anorm=sqrt(0.5*(epa1(i)**2+epa2(i)**2+epa3**2+2.*(epa4(i)**2
     1  +epa5(i)**2+epa6(i)**2)))
      if (anorm .gt. falfac) then
      fac8=falfac/anorm
      epa1(i)=epa1(i)*fac8
      epa2(i)=epa2(i)*fac8
      epa4(i)=epa4(i)*fac8
      epa5(i)=epa5(i)*fac8
      epa6(i)=epa6(i)*fac8
      endif
  620 continue
      do 630 i=lft,llt
      sig1(i)=sig1(i)+epa1(i)
      sig2(i)=sig2(i)+epa2(i)
      sig3(i)=sig3(i)-epa1(i)-epa2(i)
      sig4(i)=sig4(i)+epa4(i)
      sig5(i)=sig5(i)+epa5(i)
      sig6(i)=sig6(i)+epa6(i)
  630 continue
      endif
      return
      end
      subroutine feit(capan,xj1tr,xj2dtr,t,alpha,gama,beta,
     1                  theta,w,d,r,bulk,shear,xj1,capa,sig1,
     2                  sig2,sig3,sig4,sig5,sig6,iter,nocon)
      parameter (tol=1.0e-8,maxit=100)
c     implicit double precision (a-h,o-z)                               vax75
c
      iter=1
      nocon=1
      capa=capan
      xj1=xj1tr
      xj2d=xj2dtr
      s1=sig1-xj1tr/3.
      s2=sig2-xj1tr/3.
      s3=sig3-xj1tr/3.
      s4=sig4
      s5=sig5
      s6=sig6
   10 continue
      xk=capa+r*(alpha-gama*exp(-beta*capa)+theta*capa)
      dhdk=w*d*exp(-d*xk)*(1.+r*(theta+gama*beta*exp(-beta*capa)))
      omega=theta+gama*beta*exp(-beta*xj1)
      dcd=shear+9.*bulk*omega**2
      f=xj2d-(alpha-gama*exp(-beta*xj1)+theta*xj1)
      delcap=-3.*omega*f/(dcd*dhdk)
      delam=-dhdk*delcap/(3.*omega)
      capa=capa+delcap
      xk=capa+r*(alpha-gama*exp(-beta*capa)+theta*capa)
      fac1=shear/xj2d
      fac2=3.*bulk*omega
      sig1=sig1-delam*(fac1*s1-fac2)
      sig2=sig2-delam*(fac1*s2-fac2)
      sig3=sig3-delam*(fac1*s3-fac2)
      sig4=sig4-delam*fac1*s4
      sig5=sig5-delam*fac1*s5
      sig6=sig6-delam*fac1*s6
      xj1=sig1+sig2+sig3
      s1=sig1-xj1/3.
      s2=sig2-xj1/3.
      s3=sig3-xj1/3.
      s4=sig4
      s5=sig5
      s6=sig6
      xj2d= sqrt(.5*(s1**2+s2**2+s3**2)+s4**2+s5**2+s6**2)
      f=xj2d-(alpha-gama*exp(-beta*xj1)+theta*xj1)
      if(abs(f).le.tol) return
      if( iter.lt.maxit ) then
      iter=iter+1
      go to 10
      else
      write(13,1000) iter,f
      write( *,1000) iter,f
 1000 format(/5x,'cap model failure mode iterations unconverged',
     1       /10x,'iterations: ',i5,5x,'fe:  ',e14.4)
      stop 'feit1'
      endif
      end
      subroutine fcit(capan,xj1tr,xj2dtr,t,alpha,gama,beta,
     1                  theta,w,d,r,bulk,shear,xj1,capa,sig1,
     2                  sig2,sig3,sig4,sig5,sig6,iter,nocon)
      parameter (tol=1.0e-8,maxit=100)
c     implicit double precision (a-h,o-z)                               vax75
c
      unit=1.0
      iter=1
      nocon=1
      capa=capan
      xj1=xj1tr
      xj2d=xj2dtr
      s1=sig1-xj1tr/3.
      s2=sig2-xj1tr/3.
      s3=sig3-xj1tr/3.
      s4=sig4
      s5=sig5
      s6=sig6
   10 continue
      xk=capa+r*(alpha-gama*exp(-beta*capa)+theta*capa)
      dhdk=w*d*exp(-d*xk)*(1.+r*(theta+gama*beta*exp(-beta*capa)))
      if(capa.ge.0.0) then
      dfdk=-2.*(abs(xj1-capa)/r**2 + (alpha-gama*exp(-beta*capa)
     1    +theta*capa)*(theta+gama*beta*exp(-beta*capa)))
      else
      dfdk=-2.*xk*(1.+r*(theta+gama*beta*exp(-beta*capa)))/r**2
      endif
      elcap=max(capa,0.0*unit)
      omega=min(-2.*abs(xj1-elcap)/r**2,-1.e-15*unit)
      dcd=4.*shear*xj2d**2 + 9.*bulk*(2.*(xj1-elcap)/r**2)**2
      f=xj2d**2 + (xj1-elcap)**2/r**2 - (xk-elcap)**2/r**2
      delcap=-f/(dfdk+(dcd*dhdk)/(3.*omega))
      delam=-dhdk*delcap/(3.*omega)
      capa=capa+delcap
      xk=capa+r*(alpha-gama*exp(-beta*capa)+theta*capa)
c     elcap=max(capa,0.0*unit)
      fac1=2.*shear
      fac2=6.*bulk*abs(xj1-elcap)/r**2
      sig1=sig1-delam*(fac1*s1+fac2)
      sig2=sig2-delam*(fac1*s2+fac2)
      sig3=sig3-delam*(fac1*s3+fac2)
      sig4=sig4-delam*fac1*s4
      sig5=sig5-delam*fac1*s5
      sig6=sig6-delam*fac1*s6
      xj1=sig1+sig2+sig3
      s1=sig1-xj1/3.
      s2=sig2-xj1/3.
      s3=sig3-xj1/3.
      s4=sig4
      s5=sig5
      s6=sig6
      xj2d= sqrt(.5*(s1**2+s2**2+s3**2)+s4**2+s5**2+s6**2)
      f=xj2d**2 + (xj1-elcap)**2/r**2 - (xk-elcap)**2/r**2
      if(abs(f).le.tol) then
      if(xj1.lt.elcap) then
      xj1=elcap+abs(xj1-elcap)
      sig1=s1+xj1/3.
      sig2=s2+xj1/3.
      sig3=s3+xj1/3.
      endif
      return
      endif
      if( iter.lt.maxit ) then
      iter=iter+1
      go to 10
      else
      write(13,1000) iter,f
      write( *,1000) iter,f
 1000 format(/5x,'cap model cap mode iterations unconverged',
     1       /10x,'iterations: ',i5,5x,'fe:  ',e14.4)
      stop 'fcit1'
      endif
      end
      subroutine f3dm26 (cm,bqs,npc,plc)
c     implicit double precision (a-h,o-z)                                    dp
c
c     model of crushable honeycomb
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 scle0(128),scle1(128),qx(128),qy(128),qz(128),
     2 sigu1(128),sigu2(128),sigu3(128),sigu4(128),sigu5(128),
     3 sigu6(128),sigc1(128),sigc2(128),sigc3(128),sigc4(128),
     4 sigc5(128),sigc6(128),scl1(128),scle(128),depi(128),
     5 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),ak(128),
     6 epslst(128),epsnew(128),sg3new(128),blkg(128),
     7 sg3lst(128),effs(128),qs(128),d1d(128),d2d(128),d3d(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),df(128),fail(128),ymold(128),qh(128),
     3 fac1(128),fac1qh(128),fs(128),
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
      common/failu/sieu(128),failu(128)
      common/double/iprec,ncpw,unit
      common/ssbsis/h(8,5,5),hpr(8,5,5),hps(8,5,5),hpt(8,5,5),ipt,
     1 nip,wgts(5,5),zet(5,5)
      dimension  cm(*),bqs(*),npc(1),plc(1)
      data third/-.3333333333333/
      data zero,one /0.0,1.0/
c
      mx  =48*(mxt(lft)-1)
      ym  =cm(mx+1)
      pr  =cm(mx+2)
      sgy0=cm(mx+3)
      i1  =cm(mx+4)
      j1  =cm(mx+5)
      k1  =cm(mx+6)
      l1  =cm(mx+7)
      dffl=cm(mx+8)
      ymx0=cm(mx+9)
      ymy0=cm(mx+10)
      ymz0=cm(mx+11)
      gxy0=cm(mx+12)
      gyz0=cm(mx+13)
      gzx0=cm(mx+14)
      ymymx0=ym-cm(mx+9)
      ymymy0=ym-cm(mx+10)
      ymymz0=ym-cm(mx+11)
      g   =ym /(1.+pr)
      gdt =dt1*g
      gd2 =.5*gdt
      gmgxy0=0.5*g -cm(mx+12)
      gmgyz0=0.5*g -cm(mx+13)
      gmgzx0=0.5*g -cm(mx+14)
      blkd=-ym /(1.-2.*pr)
      ss  =.66666666666667*g +third*blkd
      blk = dt1*blkd
c
c     global to local transformation matrix
c
      call lcsm26
c
c     transform stresses and strains to local system
c
      call ldsm26
      call ldem26
c
c     compute trial stress
c
      onemdf=1.0/(1.0-dffl)
      do 20 i=lft,llt
      df(i)=  min(df(i),def(i))
      factr=(df(i)-dffl)*onemdf
      scle1(i)=  min(one,factr)
      scle1(i)=  max(zero,scle1(i))
      scle0(i)=1.-scle1(i)
      cb(i)=ss
      dvge=third*(d1(i)+d2(i)+d3(i))
      prssur=blk*dvge
      ymx=dt1*(ymx0+ymymx0*scle0(i))
      ymy=dt1*(ymy0+ymymy0*scle0(i))
      ymz=dt1*(ymz0+ymymz0*scle0(i))
      gxy=dt1*(gxy0+gmgxy0*scle0(i))
      gyz=dt1*(gyz0+gmgyz0*scle0(i))
      gzx=dt1*(gzx0+gmgzx0*scle0(i))
      sigu1(i)=sig1(i)+ymx*d1(i)
      sigu2(i)=sig2(i)+ymy*d2(i)
      sigu3(i)=sig3(i)+ymz*d3(i)
      sigu4(i)=sig4(i)+gxy*d4(i)
      sigu5(i)=sig5(i)+gyz*d5(i)
      sigu6(i)=sig6(i)+gzx*d6(i)
      sigc1(i)=sig1(i)+prssur+gdt*(d1(i)+dvge)
      sigc2(i)=sig2(i)+prssur+gdt*(d2(i)+dvge)
      sigc3(i)=sig3(i)+prssur+gdt*(d3(i)+dvge)
      sigc4(i)=sig4(i)+gd2*d4(i)
      sigc5(i)=sig5(i)+gd2*d5(i)
      sigc6(i)=sig6(i)+gd2*d6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   20 continue
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
c     assuming same number of points in all 3 curves, same intervals
c     (n1=# of points in curve)
c
      call gtp19 (plc(i2),plc(j2),plc(k2),plc(l2),def,n1,qx,
     1 qy,qz,qs,lft,llt)
c
      if (j1.eq.0) then
      do 30 i=lft,llt
   30 qy(i)=qx(i)
      endif
      if (k1.eq.0) then
      do 40 i=lft,llt
   40 qz(i)=qy(i)
      endif
      if (l1.eq.0) then
      do 50 i=lft,llt
   50 qs(i)=qz(i)
      endif
      do 60 i=lft,llt
      qxx=qx(i)/(abs(sigu1(i))+1.e-15)
      qyy=qy(i)/(abs(sigu2(i))+1.e-15)
      qzz=qz(i)/(abs(sigu3(i))+1.e-15)
      qxy=qs(i)/(abs(sigu4(i))+1.e-15)
      qyz=qs(i)/(abs(sigu5(i))+1.e-15)
      qzx=qs(i)/(abs(sigu6(i))+1.e-15)
      sigu1(i)=   min(1.0*unit,qxx)*sigu1(i)
      sigu2(i)=   min(1.0*unit,qyy)*sigu2(i)
      sigu3(i)=   min(1.0*unit,qzz)*sigu3(i)
      sigu4(i)=   min(1.0*unit,qxy)*sigu4(i)
      sigu5(i)=   min(1.0*unit,qyz)*sigu5(i)
      sigu6(i)=   min(1.0*unit,qzx)*sigu6(i)
   60 continue
      seps=1.e-30*g
      do 70 i=lft,llt
      aj2  =.5*(sigc1(i)**2+sigc2(i)**2+sigc3(i)**2)+sigc4(i)**2+sigc5
     1      (i)**2+sigc6(i)**2
      sj2  =sqrt(3.*aj2)
      scale=(sgy0+seps)/(  max(sgy0,sj2)+seps)
      presr=third*(sigc1(i)+sigc2(i)+sigc3(i))
      sigc1(i)=scale*(presr+sigc1(i))-presr
      sigc2(i)=scale*(presr+sigc2(i))-presr
      sigc3(i)=scale*(presr+sigc3(i))-presr
      sigc4(i)=scale*sigc4(i)
      sigc5(i)=scale*sigc5(i)
      sigc6(i)=scale*sigc6(i)
      scle1(i)=.5-sign(.5*unit,scle0(i)-.999)
      scle0(i)=1.-scle1(i)
   70 continue
      do 80 i=lft,llt
      sig1(i)=scle0(i)*sigc1(i)+scle1(i)*sigu1(i)
      sig2(i)=scle0(i)*sigc2(i)+scle1(i)*sigu2(i)
      sig3(i)=scle0(i)*sigc3(i)+scle1(i)*sigu3(i)
      sig4(i)=scle0(i)*sigc4(i)+scle1(i)*sigu4(i)
      sig5(i)=scle0(i)*sigc5(i)+scle1(i)*sigu5(i)
      sig6(i)=scle0(i)*sigc6(i)+scle1(i)*sigu6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   80 continue
c
c     global stresses
c
      call gblm26
c
      return
      end
      subroutine f3dm27(cm,bqs,ihg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     hyperelastic mooney-rivlin material
c
c          w = a*(i-3) + b*(ii-3)
c            + c*[iii**(-2)-1] + d*(iii-1)**2
c
c      where:   c = a/2 + b
c               d = [a*(5*pr-2) + b*(11*pr-5)] / (2-4*pr)
c               a , b , pr  are input variables
c               i , ii , iii  are invariants of right c-g tensor
c
c                                                        bnm 3/89
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &px5(128),px6(128),px7(128),px8(128),py5(128),py6(128),
     &py7(128),py8(128),pz5(128),pz6(128),pz7(128),pz8(128),
     &x17(128),x28(128),x35(128),x46(128),y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),df31(128),
     &egi1(128),egi2(128),egi3(128),egi4(128),
     &egi5(128),egi6(128),cdfac(128),trace(128),cdeti(128),cdet(128),
     &rx(128),sp1(128),sp2(128),sp3(128),
     &sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),q1(128),q2(128),q3(128),
     & q4(128),q5(128),q6(128),q7(128),q8(128),q9(128),qf(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/eg1(128),eg2(128),eg3(128),eg4(128),eg5(128),
     1 eg6(128)
c
      dimension cm(*),bqs(*)
c
      mx=48*(mxt(lft)-1)
      ta=2.*cm(mx+1)
      tb=2.*cm(mx+2)
      pr=cm(mx+3)
      tc=0.5*ta + tb
      dd=(ta*(5.*pr-2.) + tb*(11.*pr-5.))/(4.-8.*pr)
      scb=2.*(ta+tb)*(1.+pr)/(3.-6.*pr)
c
c     compute deformation gradients in undeformed configuration
c
      if (ihg.ne.3.and.ihg.lt.5) then
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
      z46(i)=z4(i)-z6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dde(i)*bqs(i))*dt1
   20 continue
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
      else
      do 31  i=lft,llt
      eps1(i)=px1(i)*x1(i)+px2(i)*x2(i)+px3(i)*x3(i)+px4(i)*x4(i)+
     & px5(i)*x5(i)+px6(i)*x6(i)+px7(i)*x7(i)+px8(i)*x8(i)
      eps5(i)=py1(i)*y1(i)+py2(i)*y2(i)+py3(i)*y3(i)+py4(i)*y4(i)+
     & py5(i)*y5(i)+py6(i)*y6(i)+py7(i)*y7(i)+py8(i)*y8(i)
      eps9(i)=pz1(i)*z1(i)+pz2(i)*z2(i)+pz3(i)*z3(i)+pz4(i)*z4(i)+
     & pz5(i)*z5(i)+pz6(i)*z6(i)+pz7(i)*z7(i)+pz8(i)*z8(i)
   31 eps2(i)=py1(i)*x1(i)+py2(i)*x2(i)+py3(i)*x3(i)+py4(i)*x4(i)+
     & py5(i)*x5(i)+py6(i)*x6(i)+py7(i)*x7(i)+py8(i)*x8(i)
      do 33 i=lft,llt
      eps3(i)=pz1(i)*x1(i)+pz2(i)*x2(i)+pz3(i)*x3(i)+pz4(i)*x4(i)+
     & pz5(i)*x5(i)+pz6(i)*x6(i)+pz7(i)*x7(i)+pz8(i)*x8(i)
      eps6(i)=pz1(i)*y1(i)+pz2(i)*y2(i)+pz3(i)*y3(i)+pz4(i)*y4(i)+
     & pz5(i)*y5(i)+pz6(i)*y6(i)+pz7(i)*y7(i)+pz8(i)*y8(i)
      eps4(i)=px1(i)*y1(i)+px2(i)*y2(i)+px3(i)*y3(i)+px4(i)*y4(i)+
     & px5(i)*y5(i)+px6(i)*y6(i)+px7(i)*y7(i)+px8(i)*y8(i)
      eps7(i)=px1(i)*z1(i)+px2(i)*z2(i)+px3(i)*z3(i)+px4(i)*z4(i)+
     & px5(i)*z5(i)+px6(i)*z6(i)+px7(i)*z7(i)+px8(i)*z8(i)
   33 eps8(i)=py1(i)*z1(i)+py2(i)*z2(i)+py3(i)*z3(i)+py4(i)*z4(i)+
     & py5(i)*z5(i)+py6(i)*z6(i)+py7(i)*z7(i)+py8(i)*z8(i)
      endif
c
c     right cauchy-green tensor
c
      do 40 i=lft,llt
      eg1(i)=eps1(i)*eps1(i)+eps4(i)*eps4(i)+eps7(i)*eps7(i)
      eg2(i)=eps2(i)*eps2(i)+eps5(i)*eps5(i)+eps8(i)*eps8(i)
      eg3(i)=eps3(i)*eps3(i)+eps6(i)*eps6(i)+eps9(i)*eps9(i)
      eg4(i)=eps1(i)*eps2(i)+eps4(i)*eps5(i)+eps7(i)*eps8(i)
      eg5(i)=eps2(i)*eps3(i)+eps5(i)*eps6(i)+eps8(i)*eps9(i)
      eg6(i)=eps1(i)*eps3(i)+eps4(i)*eps6(i)+eps7(i)*eps9(i)
   40 continue
c
c     determinant, trace, and inverse
      do 50 i=lft,llt
      cdet(i)=df(i)*df(i)
      rx(i)=1./df(i)
      cdeti(i)=rx(i)*rx(i)
      trace(i)=eg1(i)+eg2(i)+eg3(i)
      egi1(i)=cdeti(i)*(eg2(i)*eg3(i)-eg5(i)*eg5(i))
      egi2(i)=cdeti(i)*(eg1(i)*eg3(i)-eg6(i)*eg6(i))
      egi3(i)=cdeti(i)*(eg1(i)*eg2(i)-eg4(i)*eg4(i))
      egi4(i)=cdeti(i)*(eg5(i)*eg6(i)-eg4(i)*eg3(i))
      egi5(i)=cdeti(i)*(eg4(i)*eg6(i)-eg1(i)*eg5(i))
      egi6(i)=cdeti(i)*(eg4(i)*eg5(i)-eg2(i)*eg6(i))
   50 continue
c
c     second p-k stress
      do 60 i=lft,llt
      cdfac(i)=4.*dd*cdet(i)*(cdet(i)-1.)-2.*tc*cdeti(i)*cdeti(i)
      sp1(i)=ta+tb*(trace(i)-eg1(i))+cdfac(i)*egi1(i)
      sp2(i)=ta+tb*(trace(i)-eg2(i))+cdfac(i)*egi2(i)
      sp3(i)=ta+tb*(trace(i)-eg3(i))+cdfac(i)*egi3(i)
      sp4(i)=-eg4(i)*tb+cdfac(i)*egi4(i)
      sp5(i)=-eg5(i)*tb+cdfac(i)*egi5(i)
      sp6(i)=-eg6(i)*tb+cdfac(i)*egi6(i)
   60 continue
c
c     cauchy stress
      do 70 i=lft,llt
      q1(i)=eps1(i)*sp1(i)+eps2(i)*sp4(i)+eps3(i)*sp6(i)
      q2(i)=eps2(i)*sp2(i)+eps1(i)*sp4(i)+eps3(i)*sp5(i)
      q3(i)=eps3(i)*sp3(i)+eps1(i)*sp6(i)+eps2(i)*sp5(i)
      q4(i)=eps4(i)*sp1(i)+eps5(i)*sp4(i)+eps6(i)*sp6(i)
      q5(i)=eps5(i)*sp2(i)+eps4(i)*sp4(i)+eps6(i)*sp5(i)
      q6(i)=eps6(i)*sp3(i)+eps4(i)*sp6(i)+eps5(i)*sp5(i)
      q7(i)=eps7(i)*sp1(i)+eps8(i)*sp4(i)+eps9(i)*sp6(i)
      q8(i)=eps8(i)*sp2(i)+eps7(i)*sp4(i)+eps9(i)*sp5(i)
      q9(i)=eps9(i)*sp3(i)+eps7(i)*sp6(i)+eps8(i)*sp5(i)
   70 continue
c
      do 80 i=lft,llt
      sig1(i)=rx(i)*(eps1(i)*q1(i)+eps2(i)*q2(i)+eps3(i)*q3(i))
      sig2(i)=rx(i)*(eps4(i)*q4(i)+eps5(i)*q5(i)+eps6(i)*q6(i))
      sig3(i)=rx(i)*(eps7(i)*q7(i)+eps8(i)*q8(i)+eps9(i)*q9(i))
      sig4(i)=rx(i)*(eps4(i)*q1(i)+eps5(i)*q2(i)+eps6(i)*q3(i))
      sig5(i)=rx(i)*(eps7(i)*q4(i)+eps8(i)*q5(i)+eps9(i)*q6(i))
      sig6(i)=rx(i)*(eps7(i)*q1(i)+eps8(i)*q2(i)+eps9(i)*q3(i))
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   80 continue
c
c     soundspeed
      do 90 i=lft,llt
      cb(i)=scb
   90 continue
c
      return
      end
      subroutine lcsm26
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
     1 a11(128),a12(128),b13(128),a21(128),a22(128),b23(128),
     2 a31(128),a32(128),b33(128),s11(128),s12(128),s13(128),
     3 s21(128),s22(128),s23(128),s31(128),s32(128),s33(128)
      common/aux36/lft,llt
c
      do 10 i=lft,llt
      x21=x2(i)-x1(i)
      y21=y2(i)-y1(i)
      z21=z2(i)-z1(i)
      x31=x3(i)-x1(i)
      y31=y3(i)-y1(i)
      z31=z3(i)-z1(i)
      x41=x4(i)-x1(i)
      y41=y4(i)-y1(i)
      z41=z4(i)-z1(i)
      x42=x4(i)-x2(i)
      y42=y4(i)-y2(i)
      z42=z4(i)-z2(i)
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
      return
      end
      subroutine ldsm26
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
      subroutine ldem26
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
      d3(i)=    s31(i)*a13(i)+s32(i)*a23(i)+s33(i)*a33(i)
      d4(i)=2.*(s11(i)*a12(i)+s12(i)*a22(i)+s13(i)*a32(i))
      d5(i)=2.*(s21(i)*a13(i)+s22(i)*a23(i)+s23(i)*a33(i))
      d6(i)=2.*(s11(i)*a13(i)+s12(i)*a23(i)+s13(i)*a33(i))
   40 continue
      return
      end
      subroutine gblm26
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
      a31(i)=sig6(i)*s11(i)+sig5(i)*s21(i)+sig3(i)*s31(i)
      a32(i)=sig6(i)*s12(i)+sig5(i)*s22(i)+sig3(i)*s32(i)
      a33(i)=sig6(i)*s13(i)+sig5(i)*s23(i)+sig3(i)*s33(i)
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
      subroutine f3dm31 (cm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     non linear elastic material
c
c     frazer-nash consultancy hyperelastic material model
c     ---------------------------------------------------
c
c     subroutine developed by  dr c.j.kenchington/ s.a.challener
c     at frazer-nash consultancy ltd,leatherhead,surrey,england
c                 november 1987
c
c     ref: 'a non-linear elastic material model for dyna3d'
c           dr c.j.kenchington
c           presented at dyna3d user group conference 1988
c
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &px1(128),px2(128),px3(128),px4(128),py1(128),py2(128),
     &py3(128),py4(128),pz1(128),pz2(128),pz3(128),pz4(128),
     &px5(128),px6(128),px7(128),px8(128),py5(128),py6(128),
     &py7(128),py8(128),pz5(128),pz6(128),pz7(128),pz8(128),
     &x17(128),x28(128),x35(128),x46(128),y17(128),y28(128),
     &y35(128),y46(128),z17(128),z28(128),z35(128),z46(128),
     &df11(128),df22(128),df33(128),df12(128),df23(128),df31(128),
     &c11(128),c22(128),c33(128),
     &c44(128),c55(128),c66(128),c12(128),
     &c13(128),c23(128),df2(128),c11c(128),c22c(128),c33c(128),
     &rx(128),sp1(128),sp2(128),sp3(128),
     &sp4(128),sp5(128),sp6(128),
     &eps1(128),eps2(128),eps3(128),eps4(128),eps5(128),eps6(128),
     &eps7(128),eps8(128),eps9(128),q1(128),q2(128),q3(128),
     & q4(128),q5(128),q6(128),q7(128),q8(128),q9(128),qf(128)
      common/aux18/dde(128),df(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),davg(128),cc(128)
      common/aux36/lft,llt
      common/aux40/e1(128),e2(128),e3(128),e4(128),e5(128),
     1 e6(128),var1(128),var2(128),var3(128),var4(128),var5(128),
     & snd1(128),snd2(128),snd3(128)
      dimension cm(1)
c
c     pick up strain energy coefficients
c
      mx=48*(mxt(lft)-1)
      g001=cm(mx+1)
      g010=cm(mx+2)
      g020=cm(mx+3)
      g100=cm(mx+4)
      g101=cm(mx+5)
      g110=cm(mx+6)
      g200=cm(mx+7)
      g210=cm(mx+8)
      g300=cm(mx+9)
      g400=cm(mx+10)
c
c     compute deformation gradients in undeformed configuration
c
      do 20 i=lft,llt
      x17(i)=x1(i)-x7(i)
      x28(i)=x2(i)-x8(i)
      x35(i)=x3(i)-x5(i)
      x46(i)=x4(i)-x6(i)
      y17(i)=y1(i)-y7(i)
      y28(i)=y2(i)-y8(i)
      y35(i)=y3(i)-y5(i)
      y46(i)=y4(i)-y6(i)
      z17(i)=z1(i)-z7(i)
      z28(i)=z2(i)-z8(i)
      z35(i)=z3(i)-z5(i)
   20 z46(i)=z4(i)-z6(i)
      do 30 i=lft,llt
      eps1(i)=px1(i)*x17(i)+px2(i)*x28(i)+px3(i)*x35(i)+px4(i)*x46(i)
      eps2(i)=py1(i)*x17(i)+py2(i)*x28(i)+py3(i)*x35(i)+py4(i)*x46(i)
      eps3(i)=pz1(i)*x17(i)+pz2(i)*x28(i)+pz3(i)*x35(i)+pz4(i)*x46(i)
      eps4(i)=px1(i)*y17(i)+px2(i)*y28(i)+px3(i)*y35(i)+px4(i)*y46(i)
      eps5(i)=py1(i)*y17(i)+py2(i)*y28(i)+py3(i)*y35(i)+py4(i)*y46(i)
      eps6(i)=pz1(i)*y17(i)+pz2(i)*y28(i)+pz3(i)*y35(i)+pz4(i)*y46(i)
      eps7(i)=px1(i)*z17(i)+px2(i)*z28(i)+px3(i)*z35(i)+px4(i)*z46(i)
      eps8(i)=py1(i)*z17(i)+py2(i)*z28(i)+py3(i)*z35(i)+py4(i)*z46(i)
   30 eps9(i)=pz1(i)*z17(i)+pz2(i)*z28(i)+pz3(i)*z35(i)+pz4(i)*z46(i)
c
c     green-st.venant strain e=(c-1)/2
c
c     where c is the right-green tensor
c
      do 40 i=lft,llt
      e1(i)=.5*(eps1(i)*eps1(i)+eps4(i)*eps4(i)+eps7(i)*eps7(i)-1.)
      e2(i)=.5*(eps2(i)*eps2(i)+eps5(i)*eps5(i)+eps8(i)*eps8(i)-1.)
      e3(i)=.5*(eps3(i)*eps3(i)+eps6(i)*eps6(i)+eps9(i)*eps9(i)-1.)
      e4(i)=.5*(eps1(i)*eps2(i)+eps4(i)*eps5(i)+eps7(i)*eps8(i))
      e5(i)=.5*(eps2(i)*eps3(i)+eps5(i)*eps6(i)+eps8(i)*eps9(i))
   40 e6(i)=.5*(eps1(i)*eps3(i)+eps4(i)*eps6(i)+eps7(i)*eps9(i))
c
c     carry out checks for strain limits
c
      do 50 i=lft,llt
      if ( (max(e1(i),e2(i),e3(i)).gt.cm(mx+16)) .or.
     .     (min(e1(i),e2(i),e3(i)).lt.cm(mx+17)) ) then
      if (cm(mx+11).eq.0.0) then
      write(13,999)
      write( *,999)
      call adios(2)
      else
c
      if (cm(mx+30).lt.0.5) then
      cm(mx+30)=1.0
      write(13,999)
      write( *,999)
      endif
c
      endif
      endif
   50 continue
c
c     calculate green st venant strain invariants
c
      do 60 i=lft,llt
      var1(i)=e1(i)+e2(i)+e3(i)
      var2(i)=e1(i)*e2(i)+e2(i)*e3(i)+e1(i)*e3(i)-e4(i)*e4(i)
     .-e5(i)*e5(i)-e6(i)*e6(i)
   60 var3(i)=e1(i)*e2(i)*e3(i)+2.*e4(i)*e5(i)*e6(i)
     .-e1(i)*e5(i)*e5(i)-e2(i)*e6(i)*e6(i)-e3(i)*e4(i)*e4(i)
c
c    calculate equation constants
c
      do 70 i=lft,llt
      var4(i)=g100+g110*var2(i)+g101*var3(i)+2.*g200*var1(i)
     .+2.*g210*var1(i)*var2(i)+3.*g300*var1(i)**2+4.*g400*var1(i)**3
   70 var5(i)=g010+g110*var1(i)+2.*g020*var2(i)+g210*var1(i)**2
c
c     second piola kirchoff stress
c
      do 80 i=lft,llt
      sp1(i)=var4(i)+var5(i)*(e2(i)+e3(i))+(g001+g101*var1(i))
     .*(e2(i)*e3(i)-e5(i)*e5(i))
      sp2(i)=var4(i)+var5(i)*(e1(i)+e3(i))+(g001+g101*var1(i))
     .*(e1(i)*e3(i)-e6(i)*e6(i))
      sp3(i)=var4(i)+var5(i)*(e1(i)+e2(i))+(g001+g101*var1(i))
     .*(e1(i)*e2(i)-e4(i)*e4(i))
      sp4(i)=(g001+g101*var1(i))*(e5(i)*e6(i)-e3(i)*e4(i))-var5(i)*e4(i)
      sp5(i)=(g001+g101*var1(i))*(e4(i)*e6(i)-e1(i)*e5(i))-var5(i)*e5(i)
   80 sp6(i)=(g001+g101*var1(i))*(e4(i)*e5(i)-e2(i)*e6(i))-var5(i)*e6(i)
c
c     cauchy stress
c
      do 90 i=lft,llt
      rx(i)=1./df(i)
      q1(i)=eps1(i)*sp1(i)+eps2(i)*sp4(i)+eps3(i)*sp6(i)
      q2(i)=eps2(i)*sp2(i)+eps1(i)*sp4(i)+eps3(i)*sp5(i)
      q3(i)=eps3(i)*sp3(i)+eps1(i)*sp6(i)+eps2(i)*sp5(i)
      q4(i)=eps4(i)*sp1(i)+eps5(i)*sp4(i)+eps6(i)*sp6(i)
      q5(i)=eps5(i)*sp2(i)+eps4(i)*sp4(i)+eps6(i)*sp5(i)
      q6(i)=eps6(i)*sp3(i)+eps4(i)*sp6(i)+eps5(i)*sp5(i)
      q7(i)=eps7(i)*sp1(i)+eps8(i)*sp4(i)+eps9(i)*sp6(i)
      q8(i)=eps8(i)*sp2(i)+eps7(i)*sp4(i)+eps9(i)*sp5(i)
   90 q9(i)=eps9(i)*sp3(i)+eps7(i)*sp6(i)+eps8(i)*sp5(i)
c
      do 100 i=lft,llt
      sig1(i)=rx(i)*(eps1(i)*q1(i)+eps2(i)*q2(i)+eps3(i)*q3(i))
      sig2(i)=rx(i)*(eps4(i)*q4(i)+eps5(i)*q5(i)+eps6(i)*q6(i))
      sig3(i)=rx(i)*(eps7(i)*q7(i)+eps8(i)*q8(i)+eps9(i)*q9(i))
      sig4(i)=rx(i)*(eps4(i)*q1(i)+eps5(i)*q2(i)+eps6(i)*q3(i))
      sig5(i)=rx(i)*(eps7(i)*q4(i)+eps8(i)*q5(i)+eps9(i)*q6(i))
  100 sig6(i)=rx(i)*(eps7(i)*q1(i)+eps8(i)*q2(i)+eps9(i)*q3(i))
c
c     calculate the effective youngs modulus for the speed of sound
c
      do 110 i=lft,llt
      snd1(i)=2.*(g200+3.*g300*var1(i)+6.*g400*var1(i)**2
     .+g110*(e2(i)+e3(i))+g210*(2.*var1(i)*(e2(i)+e3(i))+var2(i))
     .+g020*(e2(i)+e3(i))**2+g101*(e2(i)*e3(i)-e5(i)*e5(i)))
c
      snd2(i)=2.*(g200+3.*g300*var1(i)+6.*g400*var1(i)**2
     .+g110*(e1(i)+e3(i))+g210*(2.*var1(i)*(e1(i)+e3(i))+var2(i))
     .+g020*(e1(i)+e3(i))**2+g101*(e1(i)*e3(i)-e6(i)*e6(i)))
c
      snd3(i)=2.*(g200+3.*g300*var1(i)+6.*g400*var1(i)**2
     .+g110*(e1(i)+e2(i))+g210*(2.*var1(i)*(e1(i)+e2(i))+var2(i))
     .+g020*(e1(i)+e2(i))**2+g101*(e1(i)*e2(i)-e4(i)*e4(i)))
c
  110 cb(i)=  max(snd1(i),snd2(i),snd3(i))
c
  999 format (5x,'strain range exceeded')
      return
      end
      subroutine f3dm32 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... vectorized ramberg-osgood material
c
      parameter(tol=1.0e-3,maxit=100)
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux11/po(128),
     1 tem(128),de(128,6),t(128,6)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128),
     &ei(128,6),gamma0(128,6),tau0(128,6)
      common/aux18/dd(128),def(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),g(128),ee(128)
      common/aux36/lft,llt
      dimension cm(*),bqs(*),sig(128,6)
      dimension s(128,6),pres(128),epsvol(128),ein(128,6),
     1 c1(128),c3(128),c5(128),f(128),fp(128),idone(128)
      equivalence (sig,sig1)
c
      data       third/0.33333333333333/
c
      mx=48*(mxt(lft)-1)
      gammay=cm(mx+1)/2.
      tauy=cm(mx+6)
      alfa=cm(mx+11)
      rexp=cm(mx+16)
      bulk=cm(mx+21)
      qg=0.5*tauy/gammay
c
      do 110 i=lft,llt
      pres(i)=(sig1(i)+sig2(i)+sig3(i))/3.
      epsvol(i)=(d1(i)+d2(i)+d3(i))*third
  110 continue
      do 120 i=lft,llt
      de(i,1)=d1(i)-epsvol(i)
      de(i,2)=d2(i)-epsvol(i)
      de(i,3)=d3(i)-epsvol(i)
      de(i,4)=.5*d4(i)
      de(i,5)=.5*d5(i)
      de(i,6)=.5*d6(i)
      s(i,1)=sig1(i)-pres(i)
      s(i,2)=sig2(i)-pres(i)
      s(i,3)=sig3(i)-pres(i)
      s(i,4)=sig4(i)
      s(i,5)=sig5(i)
      s(i,6)=sig6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
  120 continue
      do 170 j=1,6
      do 160 i=lft,llt
      ein(i,j)=ei(i,j)
      ei(i,j)=ei(i,j)+de(i,j)*dt1
  160 continue
  170 continue
      do 300 n=1,6
      do 410 i=lft,llt
      idone(i)=1
      if (de(i,n).eq.0.0) idone(i)=0
  410 continue
      do 420 i=lft,llt
      if( (tau0(i,n).eq.0.).and.(gamma0(i,n).eq.0.).and.
     1 ((ein(i,n)-gamma0(i,n))*de(i,n)).ge.0.0 .and. ei(i,n).ge.0) then
      c1(i)=1.0
      c3(i)=1.0
      c5(i)=1.0
      endif
  420 continue
      do 430 i=lft,llt
      if( (tau0(i,n).eq.0.).and.(gamma0(i,n).eq.0.).and.
     1 ((ein(i,n)-gamma0(i,n))*de(i,n)).ge.0.0 .and. ei(i,n).lt.0) then
      c1(i)=1.0
      c3(i)=-1.0
      c5(i)=-1.0
      endif
  430 continue
      do 440 i=lft,llt
      if ((ein(i,n)-gamma0(i,n))*de(i,n) .lt. 0.) then
      gamma0(i,n)=ein(i,n)
      tau0(i,n)=s(i,n)
      endif
  440 continue
      do 450 i=lft,llt
      if (((ei(i,n)-gamma0(i,n)).gt.0.).and.(tau0(i,n).ne.0.0)
     1     .and.(gamma0(i,n).ne.0.0)) then
      c1(i)=2.0
      c3(i)=1.0
      c5(i)=2.0
      endif
  450 continue
      do 460 i=lft,llt
      if (((ei(i,n)-gamma0(i,n)).lt.0.).and.(tau0(i,n).ne.0.0)
     1     .and.(gamma0(i,n).ne.0.0)) then
      c1(i)=2.0
      c3(i)=-1.0
      c5(i)=-2.0
      endif
  460 continue
      iter=0
  500 continue
      iter=iter+1
c
      do 470 i=lft,llt
      f(i)=-(ei(i,n)-gamma0(i,n))/(c1(i)*gammay)
     1     +(s(i,n)-tau0(i,n))/(c1(i)*tauy)
     2     +c3(i)*alfa*( (s(i,n)-tau0(i,n))/(c5(i)*tauy) )**rexp
      fp(i)=1./(c1(i)*tauy) +rexp*c3(i)*alfa/(c5(i)*tauy)
     1     *( (s(i,n)-tau0(i,n))/(c5(i)*tauy))**(rexp-1.)
      s(i,n)=s(i,n)-f(i)/fp(i)
      if ( abs(f(i)).lt.tol ) idone(i)=0
  470 continue
      ifin=0
      do 480 i=lft,llt
      ifin=ifin+idone(i)
  480 continue
      if ((ifin.ne.0).and.(iter.lt.maxit)) then
      go to 500
      elseif(iter.ge.maxit) then
      write(13,9900) ifin,maxit
      write(59,9900) ifin,maxit
 9900 format(//5x,'error in f3dm32,',i8,' points not converged',
     1        /5x,'in',i8,' iterations')
      endif
  300 continue
c
      do 140 i=lft,llt
      pres(i) = pres(i) + 3.*bulk*epsvol(i)*dt1
  140 continue
c
      tcor=(s(i,1)+s(i,2)+s(i,3))/3.
      s(i,1)=s(i,1)-tcor
      s(i,2)=s(i,2)-tcor
      s(i,3)=s(i,3)-tcor
  145 continue
      do 150 i=lft,llt
      sig1(i)=s(i,1) + pres(i)
      sig2(i)=s(i,2) + pres(i)
      sig3(i)=s(i,3) + pres(i)
      sig4(i)=s(i,4)
      sig5(i)=s(i,5)
      sig6(i)=s(i,6)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
      cxxa(i)=(bulk+4./3.*qg)*def(i)
  150 continue
c
      return
      end
