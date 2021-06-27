      subroutine stvlut(rbcor,x,v,a,rbv,dt2,n,nrb,
     1 xrb,yrb,zrb,axrb,ayrb,azrb,dtx)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... compute velocity and accelerations for rigid bodies for taurus
c
      common/rotat/s11,s21,s31,s12,s22,s32,s13,s23,s33
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1 ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2 grvity,idirgv,nodspc,nspcor
c
      dimension rbcor(3),x(3,1),v(3,1),a(3,1),rbv(6),nrb(1),
     1 xrb(1),yrb(1),zrb(1),axrb(1),ayrb(1),azrb(1)
c
      call rimat (s11,dt2*rbv(4),dt2*rbv(5),dt2*rbv(6))
c
      if (dtx.ne.0.0) then
      dtxi=1./dtx
      else
      dtxi=0.0
      endif
      do 10 i=1,n
      xrb(i)=x(1,nrb(i))
      yrb(i)=x(2,nrb(i))
      zrb(i)=x(3,nrb(i))
   10 continue
      rbcor1=rbcor(1)-dt2*rbv(1)
      rbcor2=rbcor(2)-dt2*rbv(2)
      rbcor3=rbcor(3)-dt2*rbv(3)
      do 20 i=1,n
      xrb(i)=xrb(i)-rbcor1
      yrb(i)=yrb(i)-rbcor2
      zrb(i)=zrb(i)-rbcor3
   20 continue
      dt2i=1./dt2
      do 30 i=1,n
      axrb(i)=rbv(1)+dt2i*(s11*xrb(i)+s12*yrb(i)+s13*zrb(i)-xrb(i))
      ayrb(i)=rbv(2)+dt2i*(s21*xrb(i)+s22*yrb(i)+s23*zrb(i)-yrb(i))
      azrb(i)=rbv(3)+dt2i*(s31*xrb(i)+s32*yrb(i)+s33*zrb(i)-zrb(i))
   30 continue
      do 40 i=1,n
      a(1,nrb(i))=dtxi*(axrb(i)-v(1,nrb(i)))
      a(2,nrb(i))=dtxi*(ayrb(i)-v(2,nrb(i)))
      a(3,nrb(i))=dtxi*(azrb(i)-v(3,nrb(i)))
   40 continue
c
      return
      end
      subroutine exwork (forcn0,forcn1,v,dt,n,exwk)
c     implicit double precision (a-h,o-z)                                    dp
      dimension forcn0(*),v(*),forcn1(*)
      exinc=0.0
      do 10 i=1,n
      exinc=exinc+(forcn0(i)+forcn1(i))*v(i)
   10 continue
      exwk=exwk+.5*dt*exinc
      return
      end
      subroutine dslnsg (ixp,irect,stf,nrt,nel)
c     implicit double precision (a-h,o-z)                                    dp
c     dimension ix(4),ixp(2,*),irect(4,1),stf(*)                        cray1
      dimension ix(4),ixp(5,*),irect(4,1),stf(*)                        vax75
      if (nrt.eq.0) return
c     kka=ixp(1,nel)                                                    cray1
c     kkb=ixp(2,nel)                                                    cray1
c     matp=and(kka,3777777b)                                            cray1
c     ix(3) =and(kkb,3777777b)                                          cray1
c     kk1=shiftr(kka,21)                                                cray1
c     kk2=shiftr(kkb,21)                                                cray1
c     kka=shiftr(kk1,21)                                                cray1
c     kkb=shiftr(kk2,21)                                                cray1
c     ix(1)=and(kk1,3777777b)                                           cray1
c     ix(2)=and(kka,3777777b)                                           cray1
c     ix(4)=and(kk2,3777777b)                                           cray1
      matp=ixp(1,nel)                                                   vax75
      ix(1)=ixp(2,nel)                                                  vax75
      ix(2)=ixp(3,nel)                                                  vax75
      ix(3)=ixp(4,nel)                                                  vax75
      ix(4)=ixp(5,nel)                                                  vax75
      ixsum=ix(1)+ix(2)+ix(3)+ix(4)
      do 20 i=1,nrt
      irects=irect(1,i)+irect(2,i)+irect(3,i)+irect(4,i)
      if (irects.ne.ixsum) go to 20
      do 10 j=1,4
      j1=iabs(irect(j,i)-ix(1))
      j2=iabs(irect(j,i)-ix(2))
      j3=iabs(irect(j,i)-ix(3))
      j4=iabs(irect(j,i)-ix(4))
      if (min0(j1,j2,j3,j4).ne.0) go to 20
   10 continue
      stf(i)=0.0
   20 continue
      return
      end
      subroutine prntrb (rbcor,rbu,rbv,rba,ncycle,time,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension rbcor(1),rbu(1),rbv(1),rba(1)
      data ncy/-1/
c
      if (ncy.ne.ncycle) write(13,60) ncycle,time
      write(13,10) n
      write(13,20)(rbcor(i),i=1,3)
      write(13,30)(rbu(i),i=1,6)
      write(13,40)(rbv(i),i=1,6)
      write(13,50)(rba(i),i=1,6)
      ncy=ncycle
      return
   10 format (///' rigid body',i3,/
     1        '                 ',5x,'x',11x,'y',11x,'z',10x,
     2        'x-rot',7x,'y-rot',7x,'zrot')
   20 format ('   coordinates:  ',3e12.3)
   30 format (' displacements:  ',6e12.3)
   40 format ('    velocities:  ',6e12.3)
   50 format (' accelerations:  ',6e12.3)
   60 format ('1',///'  r i g i d   b o d y   m o t i o n   a t',
     1        '    step=',i7,'    time=',e15.6)
      end
      subroutine outcsd (forces,numcsd)
c     implicit double precision (a-h,o-z)                                    dp
      dimension forces(9,*)
      do 10 i=1,numcsd
      write (13,20) i,(forces(j,i),j=1,3)
   10 continue
      return
   20 format(//' cross-section#',i2,'   x-force=',e14.4,
     1 '   y-force=',e14.4,'   z-force=',e14.4)
      end
      subroutine thkupd(nsv,msr,thk,nsn,nmn,thksv,thkmr,nty)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nsv(*),msr(*),thk(*),thksv(*),thkmr(*)
      if (nty.ne.5) then
      do 10 i=1,nsn
   10 thksv(i)=thk(nsv(i))
      else
      do 20 i=1,nsn
   20 thksv(i)=0.0
      endif
      if (nmn.ne.0) then
      do 30 i=1,nmn
   30 thkmr(i)=thk(msr(i))
      endif
      return
      end
      subroutine savdvg (savfrc,nodes,force,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension savfrc(3,1),nodes(1),force(3,1)
      do 10 i=1,n
      savfrc(1,i)=force(1,nodes(i))
      savfrc(2,i)=force(2,nodes(i))
      savfrc(3,i)=force(3,nodes(i))
   10 continue
      return
      end
      subroutine getfrc (frcinf,nodes,accl,xms,n,f,irect,x,nrts,
     1 wgts,nty)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux14/buffer(16)
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      dimension frcinf(3,*),nodes(*),accl(3,*),xms(*),f(3,*),irect(4,*),
     * x(3,*),wgts(*)
      icount=0
      if (nrts.eq.0) return
      do 5 k=1,numnp
    5 wgts(k)=0.
      do 10 k=1,n
      xm=1./xms(nodes(k))
      f(1,nodes(k))=xm*accl(1,nodes(k))-frcinf(1,k)
      f(2,nodes(k))=xm*accl(2,nodes(k))-frcinf(2,k)
      f(3,nodes(k))=xm*accl(3,nodes(k))-frcinf(3,k)
   10 continue
      do 25 k=1,nrts
      node1=irect(1,k)
      node2=irect(2,k)
      node3=irect(3,k)
      node4=irect(4,k)
      if (nty.eq.2.or.nty.eq.4) then
      node1=nodes(node1)
      node2=nodes(node2)
      node3=nodes(node3)
      node4=nodes(node4)
      endif
      x1=x(1,node1)
      y1=x(2,node1)
      z1=x(3,node1)
      x2=x(1,node2)
      y2=x(2,node2)
      z2=x(3,node2)
      x3=x(1,node3)
      y3=x(2,node3)
      z3=x(3,node3)
      x4=x(1,node4)
      y4=x(2,node4)
      z4=x(3,node4)
      fs1=-x1+x2+x3-x4
      fs2=-y1+y2+y3-y4
      fs3=-z1+z2+z3-z4
      ft1=-x1-x2+x3+x4
      ft2=-y1-y2+y3+y4
      ft3=-z1-z2+z3+z4
      e=fs1*fs1+fs2*fs2+fs3*fs3
      q=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=.25*sqrt((e*g-q*q)/16.)
      wgts(node1)=wgts(node1)+area
      wgts(node2)=wgts(node2)+area
      wgts(node3)=wgts(node3)+area
      wgts(node4)=wgts(node4)+area
   25 continue
      do 30 k=1,nrts
      node1=irect(1,k)
      node2=irect(2,k)
      node3=irect(3,k)
      node4=irect(4,k)
      if (nty.eq.2.or.nty.eq.4) then
      node1=nodes(node1)
      node2=nodes(node2)
      node3=nodes(node3)
      node4=nodes(node4)
      endif
      x1=x(1,node1)
      y1=x(2,node1)
      z1=x(3,node1)
      x2=x(1,node2)
      y2=x(2,node2)
      z2=x(3,node2)
      x3=x(1,node3)
      y3=x(2,node3)
      z3=x(3,node3)
      x4=x(1,node4)
      y4=x(2,node4)
      z4=x(3,node4)
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
      gl13=c1*xl
      gl23=c2*xl
      gl33=c3*xl
      xl=x21*gl13+y21*gl23+z21*gl33
      c1=x21-gl13*xl
      c2=y21-gl23*xl
      c3=z21-gl33*xl
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      gl11=c1*xl
      gl21=c2*xl
      gl31=c3*xl
      gl12=gl23*gl31-gl33*gl21
      gl22=gl33*gl11-gl13*gl31
      gl32=gl13*gl21-gl23*gl11
      fx=f(1,node1)+f(1,node2)+f(1,node3)+f(1,node4)
      fy=f(2,node1)+f(2,node2)+f(2,node3)+f(2,node4)
      fz=f(3,node1)+f(3,node2)+f(3,node3)+f(3,node4)
      fr=gl11*fx+gl21*fy+gl31*fz
      fs=gl12*fx+gl22*fy+gl32*fz
      ft=gl13*fx+gl23*fy+gl33*fz
      fm=sqrt(fr*fr+fs*fs)
      area=wgts(node1)+wgts(node2)+wgts(node3)+wgts(node4)
      buffer( 1+icount)=-ft/area
      buffer( 2+icount)= fm/area
      buffer( 3+icount)= fr/area
      buffer( 4+icount)= fs/area
      buffer( 5+icount)= f(1,node1)
      buffer( 6+icount)= f(2,node1)
      buffer( 7+icount)= f(3,node1)
      buffer( 8+icount)= f(1,node2)
      buffer( 9+icount)= f(2,node2)
      buffer(10+icount)= f(3,node2)
      buffer(11+icount)= f(1,node3)
      buffer(12+icount)= f(2,node3)
      buffer(13+icount)= f(3,node3)
      buffer(14+icount)= f(1,node4)
      buffer(15+icount)= f(2,node4)
      buffer(16+icount)= f(3,node4)
      icount=icount+16
      if (icount.lt.9500) go to 30
      call wrabsg (iob9,buffer,icount,nadd,1)
      call riosta (iob9)
      nadd=nadd+icount
      icount=0
   30 continue
      if (icount.eq.0) return
      call wrabsg (iob9,buffer,icount,nadd,1)
      call riosta (iob9)
      nadd=nadd+icount
      return
      end
      subroutine gtfrcp (frcinf,nodes,accerl,xms,n,x,i,itype,tt,
     1 vector,wgts,irect,nrts,nty)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,junk(19)
      dimension frcinf(3,1),nodes(1),accerl(3,1),xms(1),x(3,1),lbl(2),
     1 vector(3,1),wgts(1),irect(4,1)
      character*6 lbl
      data lbl/'slave','master'/
      write (12,70) tt,i,lbl(itype)
      do 10 k=1,numnp
      vector(1,k)=0.0
      vector(2,k)=0.0
      vector(3,k)=0.0
   10 wgts(k)=0.0
      do 20 k=1,n
      xm=1./xms(nodes(k))
      frcinf(1,k)=xm*accerl(1,nodes(k))-frcinf(1,k)
      frcinf(2,k)=xm*accerl(2,nodes(k))-frcinf(2,k)
      frcinf(3,k)=xm*accerl(3,nodes(k))-frcinf(3,k)
   20 continue
      do 40 k=1,nrts
      node1=irect(1,k)
      node2=irect(2,k)
      node3=irect(3,k)
      node4=irect(4,k)
      if (nty.eq.2.or.nty.eq.4) then
      node1=nodes(node1)
      node2=nodes(node2)
      node3=nodes(node3)
      node4=nodes(node4)
      endif
      x1=x(1,node1)
      y1=x(2,node1)
      z1=x(3,node1)
      x2=x(1,node2)
      y2=x(2,node2)
      z2=x(3,node2)
      x3=x(1,node3)
      y3=x(2,node3)
      z3=x(3,node3)
      x4=x(1,node4)
      y4=x(2,node4)
      z4=x(3,node4)
      fs1=-x1+x2+x3-x4
      fs2=-y1+y2+y3-y4
      fs3=-z1+z2+z3-z4
      ft1=-x1-x2+x3+x4
      ft2=-y1-y2+y3+y4
      ft3=-z1-z2+z3+z4
      e=fs1*fs1+fs2*fs2+fs3*fs3
      q=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=.25*sqrt((e*g-q*q)/16.)
      wgts(node1)=wgts(node1)+area
      wgts(node2)=wgts(node2)+area
      wgts(node3)=wgts(node3)+area
      wgts(node4)=wgts(node4)+area
      x21=x2-x1
      x31=x3-x1
      x41=x4-x1
      x42=x4-x2
      y21=y2-y1
      y31=y3-y1
      y41=y4-y1
      y42=y4-y2
      z21=z2-z1
      z31=z3-z1
      z41=z4-z1
      z42=z4-z2
      c1=y31*z42-z31*y42
      c2=z31*x42-x31*z42
      c3=x31*y42-y31*x42
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      vn1=c1*xl
      vn2=c2*xl
      vn3=c3*xl
      vector(1,node1)=vector(1,node1)+area*vn1
      vector(1,node2)=vector(1,node2)+area*vn1
      vector(1,node3)=vector(1,node3)+area*vn1
      vector(1,node4)=vector(1,node4)+area*vn1
      vector(2,node1)=vector(2,node1)+area*vn2
      vector(2,node2)=vector(2,node2)+area*vn2
      vector(2,node3)=vector(2,node3)+area*vn2
      vector(2,node4)=vector(2,node4)+area*vn2
      vector(3,node1)=vector(3,node1)+area*vn3
      vector(3,node2)=vector(3,node2)+area*vn3
      vector(3,node3)=vector(3,node3)+area*vn3
      vector(3,node4)=vector(3,node4)+area*vn3
   40 continue
      do 50 k=1,numnp
      if (abs(wgts(k)).lt.1.e-15) go to 50
      vecmag=sqrt(vector(1,k)**2+vector(2,k)**2+vector(3,k)**2)
      vector(1,k)=vector(1,k)/vecmag
      vector(2,k)=vector(2,k)/vecmag
      vector(3,k)=vector(3,k)/vecmag
   50 continue
      do 60 k=1,n
      fnorm=frcinf(1,k)*vector(1,nodes(k))+frcinf(2,k)
     &    *vector(2,nodes(k))+frcinf(3,k)*vector(3,nodes(k))
      gl11=-vector(1,nodes(k))*vector(3,nodes(k))
      gl21=-vector(2,nodes(k))*vector(3,nodes(k))
      gl31=1.-vector(3,nodes(k))*vector(3,nodes(k))
      xl  =1./(1.0e-06+sqrt(gl11**2+gl21**2+gl31**2))
      gl11=xl*gl11
      gl21=xl*gl21
      gl31=xl*gl31
      gl12=vector(2,nodes(k))*gl31-vector(3,nodes(k))*gl21
      gl22=vector(3,nodes(k))*gl11-vector(1,nodes(k))*gl31
      gl32=vector(1,nodes(k))*gl21-vector(2,nodes(k))*gl11
      fvert=frcinf(1,k)*gl11+frcinf(2,k)*gl21+frcinf(3,k)*gl31
      fcirc=frcinf(1,k)*gl12+frcinf(2,k)*gl22+frcinf(3,k)*gl32
      write(12,80) nodes(k),(frcinf(j,k),j=1,3)
      write(12,90) fnorm,fvert,fcirc
      write(12,90) (x(j,nodes(k)),j=1,3)
   60 continue
      return
   70 format(///' forces (t=',e9.3,') for interface',i3,' ',a6,
     1' side',//,
     2'     node           x-force/      y-force/      z-force/',/,
     3'                    n-force/      v-force/      c-force/',/,
     4'                   coordinate    coordinate    coordinate')
   80 format (5x,i5,6x,3e14.5)
   90 format (16x,3e14.5)
      end
      subroutine addstf(mtypde,cmde,ixde,sclf,dehv,nmmtde,nmelde,
     1 e,x,v,npc,p,xms,tcode)
c     implicit double precision (a-h,o-z)                                    dp
c
c     discrete springs and dampers
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      character*4 mess                                                  vax75
      common/aux14/aux(10000)
      common/aux38/mess
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/eltspf/ietspf
      character*6 elname
      common/elnme/ elname(5)
      data zero/0.0/
      dimension mtypde(*),cmde(24,*),ixde(3,*),sclf(*),dehv(9,*),
     1 e(3,*),x(3,*),v(3,*),npc(*),p(*),xms(*),tcode(*)
      dt2sav=dt2
      dti=1./(dt1+1.e-20)
      do 100 i=1,nmelde
      aux(i)=9999.
      mx=ixde(3,i)
      mt=mtypde(mx)
      xc1=x(1,ixde(1,i))
      yc1=x(2,ixde(1,i))
      zc1=x(3,ixde(1,i))
      xc2=x(1,ixde(2,i))
      yc2=x(2,ixde(2,i))
      zc2=x(3,ixde(2,i))
      if( tcode(ixde(1,i)).eq.7 ) then
      xms(ixde(1,i)) = 1.e-15
      endif
      if( tcode(ixde(2,i)).eq.7 ) then
      xms(ixde(2,i)) = 1.e-15
      endif
      dx=xc2-xc1
      dy=yc2-yc1
      dz=zc2-zc1
      xl=sqrt(dx**2+dy**2+dz**2)
      xmsix1=0.50/xms(ixde(1,i))
      xmsix2=0.50/xms(ixde(2,i))
      du=xl-dehv(7,i)
      xnx=dx/(xl+1.e-20)
      xny=dy/(xl+1.e-20)
      xnz=dz/(xl+1.e-20)
      go to (10,20,30,40,50,60),mt
c
c     linear spring
c
   10 force=dehv(6,i)+sclf(i)*cmde(1,mx)*du
      forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
      dehv(6,i)=force
      dehv(7,i)=xl
c
c     compute approximate time step size
c
      dtt=2./sqrt(cmde(1,mx)*(xmsix1+xmsix2)/(xmsix1*xmsix2))
      dt2=  min(dt2,dtt)
      aux(i)=dtt
      if (mess.eq.'sw2.') then
      if (dt2.eq.dtt)     then
      ityptc=4
      ielmtc=i
      endif
      endif
c
      go to 90
c
c     linear viscous damper
c
   20 force=sclf(i)*cmde(1,mx)*dti*du
      forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
      dehv(7,i)=xl
      go to 90
c
c     elastic plastic spring
c
   30 force=dehv(6,i)+sclf(i)*cmde(1,mx)*du
      forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
      fcyld=sclf(i)*(cmde(3,mx)+dehv(4,i)*cmde(4,mx))
      fceff=abs(force)
      dehv(6,i)=force
      dehv(7,i)=xl
c
      if (force.ge.0.0) then
      fcyld=sclf(i)*(cmde(3,mx)+dehv(4,i)*cmde(2,mx))
      if(force.le.fcyld) go to 90
      dehv(4,i)=dehv(4,i)+du
      fcyld=sclf(i)*(cmde(3,mx)+dehv(4,i)*cmde(2,mx))
      else
      fcyld=-sclf(i)*(cmde(3,mx)+dehv(4,i)*cmde(2,mx))
      if(force.gt.fcyld) go to 90
      dehv(4,i)=dehv(4,i)-du
      fcyld=-sclf(i)*(cmde(3,mx)+dehv(4,i)*cmde(2,mx))
      endif
      scale=fcyld/force
      forcx=scale*forcx
      forcy=scale*forcy
      forcz=scale*forcz
      dehv(6,i)=scale*force
c
c     compute approximate time step size
c
      dtt=2./sqrt(cmde(1,mx)*(xmsix1+xmsix2)/(xmsix1*xmsix2))
      dt2=  min(dt2,dtt)
      aux(i)=dtt
      if (mess.eq.'sw2.') then
      if (dt2.eq.dtt)     then
      ityptc=4
      ielmtc=i
      endif
      endif
c
      go to 90
c
c     nonlinear elastic
c
   40 ierr=0
      xmag=1.0
      n   =nint(cmde(1,mx))
      loc =npc(n)
      npoint=(npc(n+1)-loc)/2
c     strain=log(xl/dehv(7,i))
      force =0.0
c     call itrpde(p(loc),strain,npoint,force,sclf(i))
      call itrpde(p(loc),du,npoint,force,sclf(i),slope)
      forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
c
c     compute approximate time step size
c
      dtt=2./sqrt(slope*(xmsix1+xmsix2)/(xmsix1*xmsix2))
      dt2=  min(dt2,dtt)
      aux(i)=dtt
      if (mess.eq.'sw2.') then
      if (dt2.eq.dtt)     then
      ityptc=4
      ielmtc=i
      endif
      endif
      go to 90
c
c     nonlinear viscous
c
   50 ierr=0
      xmag=1.0
      n   =nint(cmde(1,mx))
      loc =npc(n)
      npoint=(npc(n+1)-loc)/2
c     dldt  =dti*du/(.5*(xl+dehv(7,i)))
      dldt=du*dti
      force =0.0
      call itrpde(p(loc),dldt  ,npoint,force,sclf(i),slope)
      forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
      dehv(7,i)=xl
      go to 90
c
c     general nonlinear springs
c
   60 iflag=nint(dehv(1,i))
      dyt  =dehv(2,i)
      dyc  =dehv(3,i)
      if (iflag.ne.0.or.du.gt.dyt.or.du.lt.dyc) go to 62
      n=nint(cmde(1,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(4,i),slope)
      go to 80
   62 if (du.lt.dyt) go to 64
      n=nint(cmde(1,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(4,i),slope)
      dehv(2,i)=du
      dehv(8,i)=force
      dehv(1,i)=1.0
      go to 80
   64 if (du.gt.dyc) go to 66
      n=nint(cmde(1,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(5,i),slope)
      dehv(3,i)=du
      dehv(9,i)=force
      dehv(1,i)=3.0
      go to 80
   66 if (iflag.ne.2) go to 68
      n=nint(cmde(2,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dehv(6,i),slope)
      go to 80
   68 if (iflag.ne.1) go to 70
      fyt=dehv(8,i)
      n=nint(cmde(2,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),dl1,npoint,fyt,sclf(i),2,zero,slope)
      dlu=dyt-dl1
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dlu,slope)
      fy0=sclf(i)*(cmde(4,mx)-cmde(5,mx))
      fyc=fyt-fy0-cmde(3,mx)*(-fy0+2.*fyt)
      call itrpd2(p(loc),dyc,npoint,fyc,sclf(i),2,dlu,slope)
      n=nint(cmde(1,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),dl1,npoint,fyc,sclf(i),2,zero,slope)
      dehv(1,i)=2.
      dehv(3,i)=dyc
      dehv(5,i)=dyc-dl1
      dehv(6,i)=dlu
      dehv(9,i)=fyc
      go to 80
   70 fyc=dehv(9,i)
      n=nint(cmde(2,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),dl1,npoint,fyc,sclf(i),2,zero,slope)
      dlu=dyc-dl1
      call itrpd2(p(loc),du,npoint,force,sclf(i),1,dlu,slope)
      fy0=sclf(i)*(cmde(4,mx)-cmde(5,mx))
      fyt=fyc+fy0-cmde(3,mx)*(fy0+2.*fyc)
      call itrpd2(p(loc),dyt,npoint,fyt,sclf(i),2,dlu,slope)
      n=nint(cmde(1,mx))
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      call itrpd2(p(loc),dl1,npoint,fyt,sclf(i),2,zero,slope)
      dehv(1,i)=2.0
      dehv(2,i)=dyt
      dehv(4,i)=dyt-dl1
      dehv(6,i)=dlu
      dehv(8,i)=fyt
   80 forcx=xnx*force
      forcy=xny*force
      forcz=xnz*force
c
c     compute approximate time step size
c
      dtt=2./sqrt(slope*(xmsix1+xmsix2)/(xmsix1*xmsix2))
      dt2=  min(dt2,dtt)
      aux(i)=dtt
      if (mess.eq.'sw2.') then
      if (dt2.eq.dtt)     then
      ityptc=4
      ielmtc=i
      endif
      endif
c
   90 e(1,ixde(1,i))=e(1,ixde(1,i))+forcx
      e(2,ixde(1,i))=e(2,ixde(1,i))+forcy
      e(3,ixde(1,i))=e(3,ixde(1,i))+forcz
      e(1,ixde(2,i))=e(1,ixde(2,i))-forcx
      e(2,ixde(2,i))=e(2,ixde(2,i))-forcy
      e(3,ixde(2,i))=e(3,ixde(2,i))-forcz
c     dehv(1,i)=forcx
c     dehv(2,i)=forcy
c     dehv(3,i)=forcz
  100 continue
c
c     print out computed step size for each element at time=0
c
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      dt2=dt2sav
      do 120 i=1,nmelde
      if (aux(i).eq.9999.0) go to 120
      dt2=  min(aux(i),dt2)
      if (dt2.eq.aux(i)) then
      ielmtc=i
      ityptc=4
      endif
      ielmtd=i
      write (13,420) ielmtd,aux(i)
      write (13,430)elname(ityptc),ielmtc
  120 continue
      endif
      endif
      endif
c
      return
  420 format('   axial time step size for spring element',i6,'=',e14.5)
  430 format(5x,a6,' element #',i6,' is current controlling element')
      end
      subroutine itrpde(p,tau,numlp,f,xmag,slope)
c     implicit double precision (a-h,o-z)                                    dp
      dimension p(2,1)
      do 20 m=2,numlp
      if (tau-p(1,m).le.0.0) go to 30
   20 continue
      m=numlp
   30 dt1=tau-p(1,m-1)
      d1=p(1,m)-p(1,m-1)
      d2=p(2,m)-p(2,m-1)
      slope=d2/d1
      f=p(2,m-1)+dt1*slope
      f=xmag*f
      slope=1.e-14+xmag*slope
      return
      end
      subroutine sbif(jne,nblk,x,f,b)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux33/ix(128,8),is(128),len
      common/aux14/
     &f1(128),f2(128),f3(128),f4(128),f5(128),f6(128),
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &y1(128),y2(128),y3(128),fx(128),fy(128),fz(128),
     &dot(128)
      common/nwixa/nwcon
c
      dimension jne(3,1),x(1),f(1),b(1)
c
      ipnt=1
      do 2000 lo=1,nblk
      nelg=jne(1,lo)/128
      irem=jne(1,lo)-128*nelg
      len=128
      nnodes=jne(2,lo)
      ibp=jne(3,1)
      if (irem.eq.0) go to 100
      nelg=nelg+1
      len=irem
  100 continue
c
      call sbifs(nelg,nnodes,ipnt,b(ibp),x,f)
c
 2000 continue
      end
      subroutine sbifs(nelg,nnodes,ipnt,ib,x,f)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux33/ix(128,8),is(128),len
      common/aux14/
     &f1(128),f2(128),f3(128),f4(128),f5(128),f6(128),
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &y1(128),y2(128),y3(128),fx(128),fy(128),fz(128),
     &dot(128)
      common/nwixa/nwcon
c
      dimension x(*),f(*),ib(*)
c
      do 1000 ng=1,nelg
c
      call unpki(ib(ipnt),nwcon,len)
c
      do 120 i=1,len
      f1(i)=0.
      f2(i)=0.
      f3(i)=0.
120   continue
      do 121 i=1,len
      f4(i)=0.
      f5(i)=0.
121   f6(i)=0.
c
      do 130 i=1,len
      ip=3*is(i)
      x3(i)=x(ip)
      x2(i)=x(ip-1)
      x1(i)=x(ip-2)
      x6(i)=x(ip+neq)
      x5(i)=x(ip+neq-1)
      x4(i)=x(ip+neq-2)
  130 continue
c
      do 140 i=1,len
      dot(i)=x1(i)*x4(i)+x2(i)*x5(i)+x3(i)*x6(i)
  140 continue
c
      do 200 nn=1,nnodes
c
      do 150 i=1,len
      ip=3*ix(i,nn)
      y3(i)=x(ip)
      y2(i)=x(ip-1)
      y1(i)=x(ip-2)
      fz(i)=f(ip)
      fy(i)=f(ip-1)
      fx(i)=f(ip-2)
  150 continue
c
      do 160 i=1,len
      f1(i)=f1(i)+fx(i)
      f2(i)=f2(i)+fy(i)
      f3(i)=f3(i)+fz(i)
      el=y1(i)*x4(i)+y2(i)*x5(i)+y3(i)*x6(i)-dot(i)
      f4(i)=f4(i)+el*(x5(i)*fz(i)-x6(i)*fy(i))
      f5(i)=f5(i)+el*(x6(i)*fx(i)-x4(i)*fz(i))
      f6(i)=f6(i)+el*(x4(i)*fy(i)-x5(i)*fx(i))
  160 continue
c
  200 continue
c
      do 230 i=1,len
      ip=3*is(i)
      f(ip)=f(ip)+f3(i)
      f(ip-1)=f(ip-1)+f2(i)
      f(ip-2)=f(ip-2)+f1(i)
      f(ip+neq)=f(ip+neq)+f6(i)
      f(ip+neq-1)=f(ip+neq-1)+f5(i)
      f(ip+neq-2)=f(ip+neq-2)+f4(i)
  230 continue
c
      ipnt=ipnt+nwcon*len
      len=128
c
 1000 continue
c
      return
      end
      subroutine sbix(jne,nblk,x,v,a,b)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/aux11/
     &y1(128),y2(128),y3(128),w1(128),w2(128),w3(128),
     &b1(128),b2(128),b3(128)
      common/aux14/
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &v1(128),v2(128),v3(128),v4(128),v5(128),v6(128),
     &a1(128),a2(128),a3(128),a4(128),a5(128),a6(128),
     &d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     &dot1(128),el(128)
      common/aux33/ix(128,8),is(128),len
      common/nwixa/nwcon
c
      dimension jne(3,1),x(1),v(1),a(1),b(1)
c
      ipnt=1
      do 2000 lo=1,nblk
      nelg=jne(1,lo)/128
      irem=jne(1,lo)-128*nelg
      len=128
      nnodes=jne(2,lo)
      ibp=jne(3,1)
      if (irem.eq.0) go to 100
      nelg=nelg+1
      len=irem
  100 continue
c
      call sbixs(nelg,nnodes,ipnt,b(ibp),x,v,a)
c
 2000 continue
      return
      end
      subroutine sbixs(nelg,nnodes,ipnt,ib,x,v,a)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/aux11/
     &y1(128),y2(128),y3(128),w1(128),w2(128),w3(128),
     &b1(128),b2(128),b3(128)
      common/aux14/
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &v1(128),v2(128),v3(128),v4(128),v5(128),v6(128),
     &a1(128),a2(128),a3(128),a4(128),a5(128),a6(128),
     &d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     &dot1(128),el(128)
      common/aux33/ix(128,8),is(128),len
      common/nwixa/nwcon
      dimension x(*),v(*),a(*),ib(*)
c
      dtx=.50*(dt1+dt2)
      ot=1./dt2
c
      do 1000 ng=1,nelg
c
      call unpki(ib(ipnt),nwcon,len)
c
      do 130 i=1,len
      ip=3*is(i)
      x3(i)=x(ip)
      x2(i)=x(ip-1)
      x1(i)=x(ip-2)
      x6(i)=x(ip+neq)
      x5(i)=x(ip+neq-1)
      x4(i)=x(ip+neq-2)
      v3(i)=v(ip)
      v2(i)=v(ip-1)
      v1(i)=v(ip-2)
      v6(i)=v(ip+neq)
      v5(i)=v(ip+neq-1)
      v4(i)=v(ip+neq-2)
      a3(i)=a(ip)
      a2(i)=a(ip-1)
      a1(i)=a(ip-2)
      a6(i)=a(ip+neq)
      a5(i)=a(ip+neq-1)
      a4(i)=a(ip+neq-2)
  130 continue
c
      do 140 i=1,len
      v1(i)=v1(i)+dtx*a1(i)
      v2(i)=v2(i)+dtx*a2(i)
      v3(i)=v3(i)+dtx*a3(i)
      v4(i)=v4(i)+dtx*a4(i)
      v5(i)=v5(i)+dtx*a5(i)
      v6(i)=v6(i)+dtx*a6(i)
      d1(i)=x1(i)+dt2*v1(i)
      d2(i)=x2(i)+dt2*v2(i)
      d3(i)=x3(i)+dt2*v3(i)
      dot1(i)=x1(i)*x4(i)+x2(i)*x5(i)+x3(i)*x6(i)
  140 continue
c
      call sbirot(1,len)
c
      do 300 nn=1,nnodes
c
      do 250 i=1,len
      ip=3*ix(i,nn)
      y3(i)=x(ip)
      y2(i)=x(ip-1)
      y1(i)=x(ip-2)
  250 continue
c
  260 do 280 i=1,len
      el(i)=(y1(i)*x4(i)+y2(i)*x5(i)+y3(i)*x6(i)-dot1(i))
      w1(i)=ot*(d1(i)+el(i)*d4(i)-y1(i))
      w2(i)=ot*(d2(i)+el(i)*d5(i)-y2(i))
      w3(i)=ot*(d3(i)+el(i)*d6(i)-y3(i))
  280 continue
c
      do 290 i=1,len
      ip=3*ix(i,nn)
      v(ip)=w3(i)
      v(ip-1)=w2(i)
      v(ip-2)=w1(i)
      a(ip)=0.
      a(ip-1)=0.
      a(ip-2)=0.
  290 continue
c
  300 continue
c
      ipnt=ipnt+nwcon*len
      len=128
c
 1000 continue
c
      return
      end
      subroutine sbirot (ibgn,iend)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      common/bk02/iburn,dt1,dt2,isdo
      common/aux14/
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &v1(128),v2(128),v3(128),v4(128),v5(128),v6(128),
     &a1(128),a2(128),a3(128),a4(128),a5(128),a6(128),
     &d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     &dot1(128),eldot(128)
      dimension det(128),wzz2(128),wyy2(128),wxx2(128),wxxyy(128),
     1 wxxzz(128),wyyzz(128),twxx(128),twyy(128),twzz(128),
     2 wxx(1),wyy(1),wzz(1),x(1),y(1),z(1),xt(1),yt(1)
c
      dt2sq=dt2**2
      twodt2=2.*dt2
c
      do 10 i=ibgn,iend
      wxx2(i)=dt2sq*v4(i)*v4(i)
      wyy2(i)=dt2sq*v5(i)*v5(i)
      wzz2(i)=dt2sq*v6(i)*v6(i)
      wxxzz(i)=dt2sq*v4(i)*v6(i)
      wxxyy(i)=dt2sq*v4(i)*v5(i)
      wyyzz(i)=dt2sq*v5(i)*v6(i)
      twxx(i)=twodt2*v4(i)
      twyy(i)=twodt2*v5(i)
   10 twzz(i)=twodt2*v6(i)
c
      do 20 i=ibgn,iend
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
c
      do 30 i=ibgn,iend
      d4(i)=rot1(i)*x4(i)+rot4(i)*x5(i)+rot7(i)*x6(i)
      d5(i)=rot2(i)*x4(i)+rot5(i)*x5(i)+rot8(i)*x6(i)
      d6(i)=rot3(i)*x4(i)+rot6(i)*x5(i)+rot9(i)*x6(i)
   30 continue
c
      return
      end
      subroutine yhintg (wxx,wyy,wzz,x,y,z,ibgn,iend)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/aux5/
     1 rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     2 rot6(128),rot7(128),rot8(128),rot9(128)
      dimension det(128),wzz2(128),wyy2(128),wxx2(128),wxxyy(128),
     1 wxxzz(128),wyyzz(128),twxx(128),twyy(128),twzz(128),
     2 wxx(3,1),wyy(3,1),wzz(3,1),x(3,1),y(3,1),z(3,1),xt(1),yt(1)
c
      equivalence (xt(1),det(1)),(yt(1),wzz2(1))
c
      len=iend-ibgn+1
      ip=0
      nelg=len/128
      irem=len-128*nelg
      len=128
      if (irem.eq.0) go to 5
      nelg=nelg+1
      len=irem
c
    5 dt2sq=dt2**2
      twodt2=2.*dt2
c
      do 100 ig=1,nelg
c
      do 10 i=1,len
      wxx2(i)=dt2sq*wxx(1,i+ip)*wxx(1,i+ip)
      wyy2(i)=dt2sq*wyy(1,i+ip)*wyy(1,i+ip)
      wzz2(i)=dt2sq*wzz(1,i+ip)*wzz(1,i+ip)
      wxxzz(i)=dt2sq*wxx(1,i+ip)*wzz(1,i+ip)
      wxxyy(i)=dt2sq*wxx(1,i+ip)*wyy(1,i+ip)
      wyyzz(i)=dt2sq*wyy(1,i+ip)*wzz(1,i+ip)
      twxx(i)=twodt2*wxx(1,i+ip)
      twyy(i)=twodt2*wyy(1,i+ip)
   10 twzz(i)=twodt2*wzz(1,i+ip)
c
      do 20 i=1,len
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
c
      do 30 i=1,len
      xt(i)=rot1(i)*x(1,i+ip)+rot4(i)*y(1,i+ip)+rot7(i)*z(1,i+ip)
      yt(i)=rot2(i)*x(1,i+ip)+rot5(i)*y(1,i+ip)+rot8(i)*z(1,i+ip)
      z(1,i+ip)=rot3(i)*x(1,i+ip)+rot6(i)*y(1,i+ip)+rot9(i)*z(1,i+ip)
      x(1,i+ip)=xt(i)
      y(1,i+ip)=yt(i)
   30 continue
c
      ip=ip+len
      len=128
c
  100 continue
c
      return
      end
      subroutine sgscat(ixp,yhatn,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ixs(128,4),mxt(128)
      common/aux36/lft,llt
c
      dimension ixp(5,1),yhatn(12,1),x(3,1)                             vax75
c     dimension ixp(2,1),yhatn(12,1),x(3,1)                             cray1
c
      nelg=(numels-1)/128+1
      do 90 ng=1,nelg
      lft=1
      llt=min0(128,numels-128*(ng-1))
      nft=1+(ng-1)*128
      call unpcts(ixp(1,nft),llt)
c
      do 10 i=lft,llt
      x(1,ix1(i)+numnp)=yhatn(1,i)
      x(2,ix1(i)+numnp)=yhatn(2,i)
      x(3,ix1(i)+numnp)=yhatn(3,i)
      x(1,ix2(i)+numnp)=yhatn(4,i)
      x(2,ix2(i)+numnp)=yhatn(5,i)
      x(3,ix2(i)+numnp)=yhatn(6,i)
      x(1,ix3(i)+numnp)=yhatn(7,i)
      x(2,ix3(i)+numnp)=yhatn(8,i)
      x(3,ix3(i)+numnp)=yhatn(9,i)
      x(1,ix4(i)+numnp)=yhatn(10,i)
      x(2,ix4(i)+numnp)=yhatn(11,i)
      x(3,ix4(i)+numnp)=yhatn(12,i)
   10 continue
c
   90 continue
c
      return
      end
      subroutine sbinit(jne,nblk,x,b)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux14/
     1 x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     1 y1(128),y2(128),y3(128),z1(128),z2(128),z3(128),
     1 u1(128),u2(128),u3(128),a(128),omag(128),dot(128),
     1 proj(128)
      common/aux33/ix(128,8),is(128),len
      common/nwixa/nwcon
c
      dimension jne(3,*),x(*),b(*)
c
      ipnt=1
      do 2000 lo=1,nblk
      nelg=jne(1,lo)/128
      irem=jne(1,lo)-128*nelg
      len=128
      nnodes=jne(2,lo)
      ibp=jne(3,1)
      if (irem.eq.0) go to 100
      nelg=nelg+1
      len=irem
  100 continue
c
      call sbints(nelg,nnodes,ipnt,b(ibp),x)
c
 2000 continue
      return
      end
      subroutine sbints(nelg,nnodes,ipnt,ib,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux14/
     1 x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     1 y1(128),y2(128),y3(128),z1(128),z2(128),z3(128),
     1 u1(128),u2(128),u3(128),a(128),omag(128),dot(128),
     1 proj(128)
      common/aux33/ix(128,8),is(128),len
      common/nwixa/nwcon
      common/double/iprec,ncpw,unit
c
      dimension x(*),ib(*)
c
      do 1000 ng=1,nelg
c
      call unpki(ib(ipnt),nwcon,len)
c
      do 130 i=1,len
      ip=3*is(i)
      x3(i)=x(ip)
      x2(i)=x(ip-1)
      x1(i)=x(ip-2)
      x6(i)=x(ip+neq)
      x5(i)=x(ip+neq-1)
      x4(i)=x(ip+neq-2)
  130 continue
c
      do 140 i=1,len
      ip1=3*ix(i,1)
      y3(i)=x(ip1)
      y2(i)=x(ip1-1)
      y1(i)=x(ip1-2)
      ip2=3*ix(i,nnodes)
      z3(i)=x(ip2)
      z2(i)=x(ip2-1)
      z1(i)=x(ip2-2)
  140 continue
c
      do 160 i=1,len
      u1(i)=z1(i)-y1(i)
      u2(i)=z2(i)-y2(i)
      u3(i)=z3(i)-y3(i)
  160 continue
c
      do 180 i=1,len
      omag(i)=1./sqrt(u1(i)**2+u2(i)**2+u3(i)**2)
      u1(i)=omag(i)*u1(i)
      u2(i)=omag(i)*u2(i)
      u3(i)=omag(i)*u3(i)
  180 continue
c
      do 200 i=1,len
      a(i)=sign(1.*unit,u1(i)*x4(i)+u2(i)*x5(i)+u3(i)*x6(i))
      u1(i)=a(i)*u1(i)
      u2(i)=a(i)*u2(i)
      u3(i)=a(i)*u3(i)
  200 continue
c
      do 220 i=1,len
      dot(i)=x1(i)*u1(i)+x2(i)*u2(i)+x3(i)*u3(i)
  220 continue
c
      do 400 nn=1,nnodes
c
      do 260 i=1,len
      ip=3*ix(i,nn)
      y3(i)=x(ip)
      y2(i)=x(ip-1)
      y1(i)=x(ip-2)
  260 continue
c
      do 280 i=1,len
      proj(i)=u1(i)*y1(i)+u2(i)*y2(i)+u3(i)*y3(i)-dot(i)
      y1(i)=x1(i)+proj(i)*u1(i)
      y2(i)=x2(i)+proj(i)*u2(i)
      y3(i)=x3(i)+proj(i)*u3(i)
  280 continue
c
      do 300 i=1,len
      ip=3*ix(i,nn)
      x(ip)=y3(i)
      x(ip-1)=y2(i)
      x(ip-2)=y1(i)
  300 continue
c
  400 continue
c
      do 420 i=1,len
      ip=3*is(i)+neq
      x(ip)=u3(i)
      x(ip-1)=u2(i)
      x(ip-2)=u1(i)
  420 continue
c
      ipnt=ipnt+nwcon*len
      len=128
c
 1000 continue
c
      return
      end
      subroutine movinf(dispo,dispn,vel,nn,time,timo,timn,x,v,a,
     1 drdsps)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      dimension dispo(*),dispn(*),vel(3,*),x(3,*),v(3,*),a(3,*),nn(*),
     1 drdsps(3,*)
      if(nwdadd.lt.0) return
      tol=1.e-12
      nmsnc3=3*numsnc
      if(time.lt.timn) go to 40
      call blkcpy(timn,timo,nmsnc3+1)
   10 ioerr=1
      call rdabsf(njoydf,timn,nmsnc3+1,nwdadd,ioerr)
      nwdadd=nwdadd+nmsnc3+1
      if(ioerr.ne.0) then
           nwdadd=-1
           return
      endif
      call riosta (njoydf)
      if(time+dt2.ge.timn) go to 10
   40 k=0
      if (numdp.ge.0) then
      do 50 i=1,numsnc
      vel(1,i)=(dispn(k+1)-x(1,nn(i)))/(timn-time+tol)
      vel(2,i)=(dispn(k+2)-x(2,nn(i)))/(timn-time+tol)
      vel(3,i)=(dispn(k+3)-x(3,nn(i)))/(timn-time+tol)
   50 k=k+3
      else
      do 60 i=1,numsnc
      vel(1,i)=(dispn(k+1)+drdsps(1,nn(i))-x(1,nn(i)))/(timn-time+tol)
      vel(2,i)=(dispn(k+2)+drdsps(2,nn(i))-x(2,nn(i)))/(timn-time+tol)
      vel(3,i)=(dispn(k+3)+drdsps(3,nn(i))-x(3,nn(i)))/(timn-time+tol)
   60 k=k+3
      endif
      do 70 i=1,numsnc
      a(1,nn(i))=0.0
      a(2,nn(i))=0.0
      a(3,nn(i))=0.0
      v(1,nn(i))=vel(1,i)
      v(2,nn(i))=vel(2,i)
   70 v(3,nn(i))=vel(3,i)
      return
c
   80 format(45x,e20.0)
   90 format(5x,3e20.0)
      end
      subroutine dvkeep(x,v,xstore,vstore,len,numsnc,iflg)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(1),v(1),xstore(1),vstore(1)
      go to (100,200),iflg
  100 do 110 i=1,len
      vstore(i)=v(i)
      v(i)     =0.0
  110 continue
      if (numsnc.eq.0) return
      do 120 i=1,len
      xstore(i)=x(i)
  120 continue
      return
  200 do 210 i=1,len
      v(i)=vstore(i)
  210 continue
      return
      end
      subroutine moment(tim,a,v,xms)
c     implicit double precision (a-h,o-z)                                    dp
c
c     momentum and kinetic energy calculation
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk28/summss,xke,xie,tt
      common/energy/xpe
      common/swmke/swxmom,swymom,swzmom,swkeng
      dimension tim(*),a(*),v(*),xms(*)
c
      xke=2.*swkeng
      xm=swxmom
      ym=swymom
      zm=swzmom
      dtx5=.25*(dt1+dt2)
      do 10 n=1,numnp
      n3=3*n
      n2=n3-1
      n1=n2-1
      xmsn=1./xms(n)
      vn1=v(n1)+dtx5*a(n1)
      vn2=v(n2)+dtx5*a(n2)
      vn3=v(n3)+dtx5*a(n3)
      xm=xm+xmsn*vn1
      ym=ym+xmsn*vn2
      zm=zm+xmsn*vn3
      xke=xke+xmsn*(vn1*vn1+vn2*vn2+vn3*vn3)
   10 continue
      if (numelb+numels.ne.0) then
      numnp1=numnp+1
      numnp2=numnp+numnp
      do 20 n=numnp1,numnp2
      n3=3*n
      n2=n3-1
      n1=n2-1
      xmsn=1./xms(n)
      vn1=v(n1)+dtx5*a(n1)
      vn2=v(n2)+dtx5*a(n2)
      vn3=v(n3)+dtx5*a(n3)
      xke=xke+xmsn*(vn1*vn1+vn2*vn2+vn3*vn3)
   20 continue
      endif
      xke=.5*xke
      tim(1)=tt
      tim(2)=xke
      tim(3)=xpe
      tim(4)=xke+xpe
      tim(5)=xm/summss
      tim(6)=ym/summss
      tim(7)=zm/summss
      ibgn=7+5*nmmat
      do 30 n=1,nmmat
      loc1=8+2*nmmat+3*(n-1)
      loc2=loc1+1
      loc3=loc2+1
      locm=ibgn+n
      xmassi=1./(tim(locm)+1.e-07)
      tim(loc1)=tim(loc1)*xmassi
      tim(loc2)=tim(loc2)*xmassi
      tim(loc3)=tim(loc3)*xmassi
   30 continue
      return
      end
      subroutine addmom(v,xms,b,nelm,addm,tmadd,nelmd,time)
c     implicit double precision (a-h,o-z)                                    dp
c
c     momentum deposition
c
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(7)
      common/nwixa/nwcon
      dimension v(3,*),xms(1),b(1),nelm(*),addm(3,*),tmadd(1)
      if (nelmd.eq.0) go to 30
      do 20 i=1,nelmd
      if (time.lt.tmadd(i)) go to 20
      xm=.125*addm(1,i)
      ym=.125*addm(2,i)
      zm=.125*addm(3,i)
      locdat=1+nwcon*(i-1)
      call unpk(mx,ix,nelm(locdat),3)
      do 10 k=1,8
      l=ix(k)
      v(1,l)=(v(1,l)/xms(l)+xm)*xms(l)
      v(2,l)=(v(2,l)/xms(l)+ym)*xms(l)
   10 v(3,l)=(v(3,l)/xms(l)+zm)*xms(l)
      tmadd(i)=1.e20
   20 continue
   30 return
      end
      subroutine lodbfe(x,xms,a,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     add body force loads
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk76/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax,gay,gaz,slx,sly,slz
      dimension x(3,1),xms(1),a(3,1),fval(1)
      if (nthpx+nthpy+nthpz+nthsx+nthsy+nthsz.eq.0) return
      if (nthpx.eq.0) go to 20
      facx=fval(nthpx)*gax
      if (facx.eq.0.0) go to 20
      do 10 i=1,numnp
   10 a(1,i)=a(1,i)-facx/xms(i)
   20 if (nthpy.eq.0) go to 40
      facy=fval(nthpy)*gay
      if (facy.eq.0.0) go to 40
      do 30 i=1,numnp
   30 a(2,i)=a(2,i)-facy/xms(i)
   40 if (nthpz.eq.0) go to 60
      facz=fval(nthpz)*gaz
      if (facz.eq.0.0) go to 60
      do 50 i=1,numnp
   50 a(3,i)=a(3,i)-facz/xms(i)
   60 if (nthsx.eq.0) go to 80
      facsx=(fval(nthsx)*slx)**2
      if (facsx.eq.0.) go to 80
      do 70 i=1,numnp
      a(2,i)=a(2,i)+facsx*x(2,i)/xms(i)
   70 a(3,i)=a(3,i)+facsx*x(3,i)/xms(i)
   80 if (nthsy.eq.0) go to 100
      facsy=(fval(nthsy)*sly)**2
      if (facsy.eq.0.) go to 100
      do 90 i=1,numnp
      a(1,i)=a(1,i)+facsy*x(1,i)/xms(i)
   90 a(3,i)=a(3,i)+facsy*x(3,i)/xms(i)
  100 if (nthsz.eq.0) go to 120
      facsz=(fval(nthsz)*slz)**2
      if (facsz.eq.0.) go to 120
      do 110 i=1,numnp
      a(1,i)=a(1,i)+facsz*x(1,i)/xms(i)
  110 a(2,i)=a(2,i)+facsz*x(2,i)/xms(i)
  120 return
      end
      subroutine cnstr0(ic,a,numtp,axyz,nodtie,xms)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      dimension a(3,1),ic(1),xms(1),numtp(1),nodtie(1),axyz(3,1)
      k=0
      do 20 i=1,numcc
      j=k+1
      k=numtp(i+1)-1
      xm=0.0
      ax=0.0
      ay=0.0
      az=0.0
      do 10 l=j,k
      xma=1./xms(nodtie(l))
      ax=ax+xma*a(1,nodtie(l))
      ay=ay+xma*a(2,nodtie(l))
      az=az+xma*a(3,nodtie(l))
      xm=xm+xma
   10 continue
      axyz(1,i)=ax/xm
      axyz(2,i)=ay/xm
      axyz(3,i)=az/xm
   20 continue
      return
      end
      subroutine cnstr1(ic,a,numtp,axyz,nodtie,xms,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      dimension a(3,1),ic(1),xms(1),numtp(1),nodtie(1),axyz(3,1),x(3,1)
      k=0
      do 100 i=1,numcc
      j=k+1
      k=numtp(i+1)-1
      ib=ic(i)
c
c     ib=0 is for spotwelds
c
      if (ib.eq.0) then
      l=nodtie(j)
      m=nodtie(k)
      vx=x(1,m)-x(1,l)
      vy=x(2,m)-x(2,l)
      vz=x(3,m)-x(3,l)
      xl=sqrt(vx**2+vy**2+vz**2)
      vx=vx/xl
      vy=vy/xl
      vz=vz/xl
      ac=axyz(1,i)*vx+axyz(2,i)*vy+axyz(3,i)*vz
      al=a(1,l)*vx+a(2,l)*vy+a(3,l)*vz-ac
      am=a(1,m)*vx+a(2,m)*vy+a(3,m)*vz-ac
      a(1,l)=a(1,l)-al*vx
      a(2,l)=a(2,l)-al*vy
      a(3,l)=a(3,l)-al*vz
      a(1,m)=a(1,m)-am*vx
      a(2,m)=a(2,m)-am*vy
      a(3,m)=a(3,m)-am*vz
c
      else
c
      go to (10,10,10,30,40,50,80), ib
   10 do 20 l=j,k
   20 a(ib,nodtie(l))=axyz(ib,i)
      go to 100
   30 m1=1
      m2=2
      go to 60
   40 m1=2
      m2=3
      go to 60
   50 m1=1
      m2=3
   60 do 70 l=j,k
      a(m1,nodtie(l))=axyz(m1,i)
   70 a(m2,nodtie(l))=axyz(m2,i)
      go to 100
   80 do 90 l=j,k
      a(1,nodtie(l))=axyz(1,i)
      a(2,nodtie(l))=axyz(2,i)
   90 a(3,nodtie(l))=axyz(3,i)
      endif
c
  100 continue
      return
      end
      subroutine symwfl(x,a,v,rd,nsw,nsn,xms,failvl)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
c
      dimension x(*),v(*),a(*),rd(*),nsw(*),xms(*),failvl(*)
c
      dtx=(dt1+dt2)/2.
      do 20 i=1,nsn
      n=nsw(i)
      n3=3*n
      n2=n3-1
      n1=n2-1
      vx=v(n1)+a(n1)*dtx
      vy=v(n2)+a(n2)*dtx
      vz=v(n3)+a(n3)*dtx
      ux=x(n1)+vx*dt2
      uy=x(n2)+vy*dt2
      uz=x(n3)+vz*dt2
      xc=ux-rd(1)
      yc=uy-rd(2)
      zc=uz-rd(3)
      dp=xc*rd(4)+yc*rd(5)+zc*rd(6)
      if (dp.le.0.0.and.failvl(i).eq.0.0) then
      da=a(n1)*rd(4)+a(n2)*rd(5)+a(n3)*rd(6)
      a(n1)=a(n1)-da*rd(4)
      a(n2)=a(n2)-da*rd(5)
      a(n3)=a(n3)-da*rd(6)
      dv=v(n1)*rd(4)+v(n2)*rd(5)+v(n3)*rd(6)
      v(n1)=v(n1)-dv*rd(4)
      v(n2)=v(n2)-dv*rd(5)
      v(n3)=v(n3)-dv*rd(6)
      endif
      if (failvl(i).ne.0.0) then
      da=a(n1)*rd(4)+a(n2)*rd(5)+a(n3)*rd(6)
      force=da/xms(n)
      if (force.lt.failvl(i)) then
      a(n1)=a(n1)-da*rd(4)
      a(n2)=a(n2)-da*rd(5)
      a(n3)=a(n3)-da*rd(6)
      dv=v(n1)*rd(4)+v(n2)*rd(5)+v(n3)*rd(6)
      v(n1)=v(n1)-dv*rd(4)
      v(n2)=v(n2)-dv*rd(5)
      v(n3)=v(n3)-dv*rd(6)
      else
      failvl(i)=0.0
      endif
      endif
   20 continue
c
      return
c
      end
      subroutine rigidw(x,a,v,rd,nsw,nsn,swmass,swvelo,xms,force,
     1 istick)
c     implicit double precision (a-h,o-z)                                    dp
c
c     rigid wall impact
c
      common/bk02/iburn,dt1,dt2,isdo
      common/swmke/swxmom,swymom,swzmom,swkeng
c
      dimension x(*),v(*),a(*),rd(*),nsw(*),xms(*)
c
      dtx=(dt1+dt2)/2.
      force=0.0
      if (swmass.eq.0.0) then
      do 20 i=1,nsn
      n=nsw(i)
      n3=3*n
      n2=n3-1
      n1=n2-1
      vx=v(n1)+a(n1)*dtx
      vy=v(n2)+a(n2)*dtx
      vz=v(n3)+a(n3)*dtx
      ux=x(n1)+vx*dt2
      uy=x(n2)+vy*dt2
      uz=x(n3)+vz*dt2
      xc=ux-rd(1)
      yc=uy-rd(2)
      zc=uz-rd(3)
c
c     check extent of rigid wall
c
      dp=xc*rd(4)+yc*rd(5)+zc*rd(6)
      if (rd(13)+rd(14).ne.0.0) then
      if (rd(13).gt.0.0) then
      dq=xc*rd(7)+yc*rd(8)+zc*rd(9)
      if (dq.lt.0.0.or.dq.gt.rd(13)) go to 20
      endif
      if (rd(14).gt.0.0) then
      dr=xc*rd(10)+yc*rd(11)+zc*rd(12)
      if (dr.lt.0.0.or.dr.gt.rd(14)) go to 20
      endif
      endif
c
      if (dp.le.0.0) then
      da=a(n1)*rd(4)+a(n2)*rd(5)+a(n3)*rd(6)
      if (istick.eq.1) then
      a(n1)=0.0
      a(n2)=0.0
      a(n3)=0.0
      v(n1)=0.0
      v(n2)=0.0
      v(n3)=0.0
      else
      a(n1)=a(n1)-da*rd(4)
      a(n2)=a(n2)-da*rd(5)
      a(n3)=a(n3)-da*rd(6)
      dv=v(n1)*rd(4)+v(n2)*rd(5)+v(n3)*rd(6)
      v(n1)=v(n1)-dv*rd(4)
      v(n2)=v(n2)-dv*rd(5)
      v(n3)=v(n3)-dv*rd(6)
      endif
      force=force-da/xms(n)
      endif
   20 continue
      else
      totalm=swmass
      forcex=0.0
      forcey=0.0
      forcez=0.0
      slavem=swmass*swvelo
      do 30 i=1,nsn
      n=nsw(i)
      n3=3*n
      n2=n3-1
      n1=n2-1
      vx=v(n1)+a(n1)*dtx
      vy=v(n2)+a(n2)*dtx
      vz=v(n3)+a(n3)*dtx
      ux=x(n1)+vx*dt2
      uy=x(n2)+vy*dt2
      uz=x(n3)+vz*dt2
      di=swvelo*dt2
      rd1=rd(1)+rd(4)*di
      rd2=rd(2)+rd(5)*di
      rd3=rd(3)+rd(6)*di
      xc=ux-rd1
      yc=uy-rd2
      zc=uz-rd3
c
c     check extent of rigid wall
c
      if (rd(13)+rd(14).ne.0.0) then
      if (rd(13).gt.0.0) then
      dq=xc*rd(7)+yc*rd(8)+zc*rd(9)
      if (dq.lt.0.0.or.dq.gt.rd(13)) go to 30
      endif
      if (rd(14).gt.0.0) then
      dr=xc*rd(10)+yc*rd(11)+zc*rd(12)
      if (dr.lt.0.0.or.dr.gt.rd(14)) go to 30
      endif
      endif
c
      dp=xc*rd(4)+yc*rd(5)+zc*rd(6)
      if (dp.le.0.0) then
      nsw(i) =-nsw(i)
      svmass =1./xms(n)
      totalm=totalm+svmass
      forcex=forcex+svmass*a(n1)
      forcey=forcey+svmass*a(n2)
      forcez=forcez+svmass*a(n3)
      dv=v(n1)*rd(4)+v(n2)*rd(5)+v(n3)*rd(6)
      if (istick.eq.1) then
      a(n1)=0.0
      a(n2)=0.0
      a(n3)=0.0
      v(n1)=0.0
      v(n2)=0.0
      v(n3)=0.0
      else
      da=a(n1)*rd(4)+a(n2)*rd(5)+a(n3)*rd(6)
      a(n1)=a(n1)-da*rd(4)
      a(n2)=a(n2)-da*rd(5)
      a(n3)=a(n3)-da*rd(6)
      v(n1)=v(n1)-dv*rd(4)
      v(n2)=v(n2)-dv*rd(5)
      v(n3)=v(n3)-dv*rd(6)
      endif
      slavem=slavem+dv*svmass
      endif
   30 continue
      swvelo=slavem/totalm
      swoldv=swvelo
      forcen=forcex*rd(4)+forcey*rd(5)+forcez*rd(6)
      anorml=forcen/totalm
      swvelo=swvelo+anorml*dtx
      dspinc=swvelo*dt2
      force =anorml*swmass
      swkeng=swkeng+.5*swmass*swvelo**2
      swxmom=swxmom+rd(4)*swmass*(swvelo-anorml*.5*dtx)
      swymom=swymom+rd(5)*swmass*(swvelo-anorml*.5*dtx)
      swzmom=swzmom+rd(6)*swmass*(swvelo-anorml*.5*dtx)
      rd(1)=rd(1)+rd(4)*dspinc
      rd(2)=rd(2)+rd(5)*dspinc
      rd(3)=rd(3)+rd(6)*dspinc
      do 40 i=1,nsn
      n=nsw(i)
      if (n.lt.0) then
      nsw(i)=-nsw(i)
      n3=-3*n
      n2=n3-1
      n1=n2-1
      a(n1)=a(n1)+anorml*rd(4)
      a(n2)=a(n2)+anorml*rd(5)
      a(n3)=a(n3)+anorml*rd(6)
      v(n1)=v(n1)+swoldv*rd(4)
      v(n2)=v(n2)+swoldv*rd(5)
      v(n3)=v(n3)+swoldv*rd(6)
      endif
   40 continue
      endif
c
      return
c
      end
      subroutine datout (ix,x,v,tt,nwcon,matype,ieost,cm,auxvec,u,ixs,
     1 a,ndsout,nsout1,nsout3,tim,icnt0)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/   /b(1)
c
      dimension   ix(nwcon,1),auxvec(1),x(3,1),v(3,1),u(1),cm(1),
c    1            matype(1),ieost(1),ixs(2,1),a(3,1),paccl(9),          cray1
     1            matype(1),ieost(1),ixs(5,1),a(3,1),paccl(9),          vax75
     2            ndsout(*),nsout1(*),nsout3(*),tim(*)
c
      call dtout (ix,x,v,tt,tim,nwcon,matype,ieost,cm,auxvec,u,ixs,
     1 a,ndsout,nsout1,nsout3,icnt0,b(ns13),b(nh13),b(nh14))
c
      return
      end
      subroutine dtout (ix,x,v,tt,tim,nwcon,matype,ieost,cm,auxvec,u,
     1 ixs,a,ndsout,nsout1,nsout3,icnt0,nshpnt,nhxpnt,lochvh)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk11/ixx(9),xx(3,8),vv(3,8),ixxs(5),xxs(3,4),vvs(3,4)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/thfl/iadd,icount,maddbg
      common/aux5/axx(7,128),buffer(6144)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
c
      dimension   ix(nwcon,*),auxvec(*),x(3,*),v(3,*),u(*),cm(*),
c    1matype(*),ieost(*),ixs(2,*),a(3,*),paccl(9),                      cray1
     1matype(*),ieost(*),ixs(5,*),a(3,*),paccl(9),                      vax75
     2ndsout(*),nsout1(*),nsout3(*),nshpnt(*),nhxpnt(*),lochvh(*),tim(*)
      real rbuffr                                                       vaxcs
      dimension rbuffr(6144)                                            vaxcs
      save thfl
c     data maxsiz/1600000b/                                             ltss
c     data maxsiz/7000000b/                                             cos
      data maxsiz/ 2621440  /                                           unics
c     data maxsiz/  262144  /                                           wkstn
c     data maxsiz/  262144  /                                           vms
c
      if (ndth.eq.0.and.nmst.eq.0) return
c
      ifctor=iadd/maxsiz
      if (iadd+icount.gt.(ifctor+1)*maxsiz) iadd=(ifctor+1)*maxsiz
c
      call wrabsg(iob4,tim,icnt0,iadd,1)
      call riosta(iob4)
      iadd=iadd+icnt0
      time=tt
      icount=icnt0

c  nodal time history blocks

      if (ndth.ne.0) then
      ibffr=2
      buffer(1)=time
      l1=0
      do 20 m=1,ndth
      kk=ndsout(2*m-1)
      ll=ndsout(2*m)
      do 10 n=kk,ll
      paccl(1)=x(1,n)
      paccl(2)=x(2,n)
      paccl(3)=x(3,n)
      paccl(4)=v(1,n)
      paccl(5)=v(2,n)
      paccl(6)=v(3,n)
      paccl(7)=a(1,n)
      paccl(8)=a(2,n)
      paccl(9)=a(3,n)
      call blkcpy (paccl,buffer(ibffr),9)
      ibffr=ibffr+9
      if (ibffr+9.gt.6144) then
      ibffr=ibffr-1
      call wrabsg (iob4,buffer,ibffr,iadd,1)
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      ibffr=1
      endif
   10 continue
   20 continue
      if (ibffr.ne.1) then
      ibffr=ibffr-1
      call wrabsg (iob4,buffer,ibffr,iadd,1)
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      endif
      endif
c
c     element time history blocks
c
      if (nsth.eq.0) go to 130
      call wrabsg (iob4,time,1,iadd,1)
      call riosta (iob4)
      iadd=iadd+1
      icount=icount+1
      l1=0
      ibffr=1
      do 60 m=1,nsth
      kk=nsout1(2*m-1)
      ll=nsout1(2*m)
      if (min0(kk,ll).lt.1) go to 60
      do 50 k=kk,ll
      n  =nhxpnt(k)
      call unpk(ixx(1),ixx(2),ix(1,n),3)
      call blkcpi (ixx(2),rbuffr(ibffr), 8)                             vaxcs
      do 40 i=1,8
      xx(1,i)=x(1,ixx(i+1))
      xx(2,i)=x(2,ixx(i+1))
      xx(3,i)=x(3,ixx(i+1))
      vv(1,i)=v(1,ixx(i+1))
      vv(2,i)=v(2,ixx(i+1))
   40 vv(3,i)=v(3,ixx(i+1))
      do 45 i=1,8                                                       vaxcs
      iptr=ibffr+7+(i-1)*3                                              vaxcs
      rbuffr(iptr+1)=xx(1,i)                                            vaxcs
      rbuffr(iptr+2)=xx(2,i)                                            vaxcs
      rbuffr(iptr+3)=xx(3,i)                                            vaxcs
      rbuffr(iptr+25)=vv(1,i)                                           vaxcs
      rbuffr(iptr+26)=vv(2,i)                                           vaxcs
      rbuffr(iptr+27)=vv(3,i)                                           vaxcs
   45 continue                                                          vaxcs
c     call blkcpy (ixx(2),buffer(ibffr),56)                             cray1
      ibffr=ibffr+56
      if (ibffr+56.gt.6144) then
      ibffr=ibffr-1
c     call wrabsg (iob4,buffer,ibffr,iadd,1)                            cray1
      call wrabsg (iob4,rbuffr,ibffr,iadd,0)                            vaxcs
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      ibffr=1
      endif
   50 continue
   60 continue
      if (ibffr.ne.1) then
      ibffr=ibffr-1
c     call wrabsg (iob4,buffer,ibffr,iadd,1)                            cray1
      call wrabsg (iob4,rbuffr,ibffr,iadd,0)                            vaxcs
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      endif
      ibffr=1
      do 120 m=1,nsth
      kk=nsout1(2*m-1)
      ll=nsout1(2*m)
      if (min0(kk,ll).lt.1) go to 120
      do 100 k=kk,ll
      n  =nhxpnt(k)
      lav=lochvh(n)
      call unpk(ixx(1),ixx(2),ix(1,n),3)
c     mtype=and(ix(1,n),3777777b)                                       cray1
      mtype=ix(1,n)                                                     vax75
      nes  =ieost(mtype)
      nmecon=0
      if (mtype.eq.0) go to 90
      mtyp =matype(mtype)
      if (mtyp.eq.20) go to 90
      nmecon=7+nconst(mtyp)
      if (nes.ne.0) nmecon=nmecon+ncneos(nes)
   90 call blkcpy (auxvec(lav),buffer(ibffr),7)
      ibffr=ibffr+7
      if (ibffr+7.gt.6144) then
      ibffr=ibffr-1
      call wrabsg (iob4,buffer,ibffr,iadd,1)
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      ibffr=1
      endif
  100 continue
  120 continue
      if (ibffr.ne.1) then
      ibffr=ibffr-1
      call wrabsg (iob4,buffer,ibffr,iadd,1)
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      endif
  130 if (nsts.eq.0) return
      call wrabsg (iob4,time,1,iadd,1)
      call riosta (iob4)
      iadd=iadd+1
      icount=icount+1
      ibffr=1
      do 160 m=1,nsts
      kk=nsout3(2*m-1)
      ll=nsout3(2*m)
      if (min0(kk,ll).lt.1) go to 160
      do 150 k=kk,ll
      n=nshpnt(k)
      call unpk(ixxs(1),ixxs(2),ixs(1,n),2)
      do 140 i=1,4
      xxs(1,i)=x(1,ixxs(i+1))
      xxs(2,i)=x(2,ixxs(i+1))
      xxs(3,i)=x(3,ixxs(i+1))
      vvs(1,i)=v(1,ixxs(i+1))
      vvs(2,i)=v(2,ixxs(i+1))
  140 vvs(3,i)=v(3,ixxs(i+1))
c     call blkcpy (ixxs(2),buffer(ibffr),28)                            cray1
      call blkcpi (ixxs(2),rbuffr(ibffr), 4)                            vaxcs
      do 145 i=1,4                                                      vaxcs
      iptr=ibffr+4+(i-1)*6                                              vaxcs
      rbuffr(iptr+1)=xxs(1,i)                                           vaxcs
      rbuffr(iptr+2)=xxs(2,i)                                           vaxcs
      rbuffr(iptr+3)=xxs(3,i)                                           vaxcs
      rbuffr(iptr+4)=vvs(1,i)                                           vaxcs
      rbuffr(iptr+5)=vvs(2,i)                                           vaxcs
      rbuffr(iptr+6)=vvs(3,i)                                           vaxcs
  145 continue                                                          vaxcs
      ibffr=ibffr+28
      if (ibffr+28.gt.6144) then
      ibffr=ibffr-1
c     call wrabsg (iob4,buffer,ibffr,iadd,1)                            cray1
      call wrabsg (iob4,rbuffr,ibffr,iadd,0)                            vaxcs
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      ibffr=1
      endif
  150 continue
  160 continue
      if (ibffr.ne.1) then
      ibffr=ibffr-1
c     call wrabsg (iob4,buffer,ibffr,iadd,1)                            cray1
      call wrabsg (iob4,rbuffr,ibffr,iadd,0)                            vaxcs
      call riosta (iob4)
      iadd=iadd+ibffr
      icount=icount+ibffr
      endif
      return
      end
      subroutine tempr(told,temp1,tnew,temp2,time,numnp,
     1                 tmode,tbase,lct,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and interpolate (if necessary) temperature states
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &  iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),        vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/tint/itsc,itbs,itlc
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      logical large
      common/bigprb/large
      dimension temp1(1),temp2(1),tmode(1),tbase(1),lct(1),fval(1)
      character*80 txts,mssg
c
c     interpolate
c
      if (itemp.eq.-2) then
      if (time .gt.0.) return
      if (idrflg.eq.1) return
      k1=1
    1 call gttxsg (txts,lcount)
      if (large) then
      read (unit=txts,fmt=120,err=106) k2,temp2(k2)
      else
      read (unit=txts,fmt=110,err=106) k2,temp2(k2)
      endif
      tsav1=temp2(k2)
      if (k1.eq.1) tsave=temp2(k2)
      dtemp=temp2(k2)-tsave
      kd=k2-k1+1
      do 2 k=k1,k2
      tscale=float(k-k1+1)/float(kd)
      temp2(k)=tsave+tscale*dtemp
      temp1(k)=temp2(k)
    2 continue
      k1=k2+1
      tsave=tsav1
      if (k2.lt.numnp) go to 1
      told=time
      return
      else
      if (time.eq.0.0) return
      if (itemp.eq.-9999) go to 50
      if (time.le.tnew) go to 30
   10 do 20 j=1,numnp
   20 temp1(j)=temp2(j)
      told=tnew
      call rdabsg (iob11,tnew,numnp+1,itherm,1)
      call riosta (iob11)
      itherm=itherm+numnp+1
      if (time.lt.tnew) go to 30
      go to 10
   30 scale=(time-told)/(tnew-told)
c
      do 40 j=1,numnp
   40 temp1(j)=temp1(j)+scale*(temp2(j)-temp1(j))
      told=time
      return
c
   50 do 100 j=1,numnp
      temp1(j)=tmode(j)*fval(lct(j))+tbase(j)
  100 continue
      told=time
c
      return
      endif
  106 call termin (txts,mssg,lcount,0)
  110 format(i5,10x,e10.2)
  120 format(i8,10x,e10.2)
      end
      subroutine ldcst (fval,npc,p,tt,iclf)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute current value of load curves
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      dimension fval(1),npc(1),p(1),iclf(1)
c
      if (nlcur.eq.0) return
c
      do 10 n=1,nlcur
      ierr=0
      xmag=1.0
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      ierr=0
      f=0.
      fval(n)=0.0
      call interp (p(loc),tt,npoint,f,xmag,ierr)
       if (iclf(n).eq.idrflg.or.iclf(n).eq.2) fval(n)=f
      if (ierr.eq.1) fval(n)=0.0
   10 continue
c
      return
c
      end
      subroutine interp(p,tau,numlp,f,xmag,ierr)
c     implicit double precision (a-h,o-z)                                    dp
      dimension p(2,1)
      if (tau-p(1,numlp).gt.0.0) go to 30
      do 10 m=2,numlp
      if (tau-p(1,m).le.0.0) go to 20
   10 continue
      go to 30
   20 dt=tau-p(1,m-1)
      d1=p(1,m)-p(1,m-1)
      d2=p(2,m)-p(2,m-1)
      f=p(2,m-1)+dt*d2/d1
      f=xmag*f
      return
   30 ierr=1
      return
      end
      subroutine loadcn (x,nod,idirn,lc,fac,m,fval,e,f,tt,numcl)
c     implicit double precision (a-h,o-z)                                    dp
c
c     concentrated loads and follower forces
c
      dimension x(3,1),nod(1),idirn(1),lc(1),fac(1),m(3,1),fval(1),
     1 e(3,1),f(3,1)
      do 10 n=1,numcl
      p=fval(lc(n))*fac(n)
      if (idirn(n).lt.4)     then
      e(idirn(n),nod(n))=e(idirn(n),nod(n))+p
      go to 10
      elseif (idirn(n).eq.4) then
      xm21=x(1,m(2,n))-x(1,m(1,n))
      ym21=x(2,m(2,n))-x(2,m(1,n))
      zm21=x(3,m(2,n))-x(3,m(1,n))
      xm31=x(1,m(3,n))-x(1,m(1,n))
      ym31=x(2,m(3,n))-x(2,m(1,n))
      zm31=x(3,m(3,n))-x(3,m(1,n))
      e1 = ym21*zm31 - ym31*zm21
      e2 = zm21*xm31 - zm31*xm21
      e3 = xm21*ym31 - xm31*ym21
      p=p/sqrt(e1*e1 + e2*e2 + e3*e3)
      e(1,nod(n))=e(1,nod(n))+p*e1
      e(2,nod(n))=e(2,nod(n))+p*e2
      e(3,nod(n))=e(3,nod(n))+p*e3
      go to 10
      elseif (idirn(n).gt.4) then
      f(idirn(n)-4,nod(n))=f(idirn(n)-4,nod(n))+p
      endif
   10 continue
      return
      end
      subroutine load (nodes,t,lc,pmult,x,fval,p,npc,tt,e,e3,rhsi,neq2)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux14/
     & xx11(128),xx21(128),xx31(128),
     & xx12(128),xx22(128),xx32(128),
     & xx13(128),xx23(128),xx33(128),
     & xx14(128),xx24(128),xx34(128),
     & xctr(128),yctr(128),zctr(128),
     &    f(128), tr1(128), tr2(128), tr3(128)
c
      dimension x(3,1),p(1),fval(1),npc(1),e(1),rhsi(1),
     1 e3(3,*),nodes(7,*),t(7,*),lc(7,*),pmult(7,*)
      save tcheck,lcnum,ido
      data tcheck,lcnum,ido/0.0,0,0/
c
c     zero e
c
      do 10 i=1,neq2
   10 e(i)=0.
      if (numdp.gt.0) then
      do 20 i=1,neq
   20 e(i)=rhsi(i)
      endif
c
c
c     apply pressure load
c
      if (numpc.eq.0) return
c
      npcg=numpc/128
      ilen=128
      irem=numpc-npcg*128
      if (irem.ne.0) then
      ilen=irem
      npcg=npcg+1
      endif
c
c     set flags
c
      if (ido.eq.0) then
      tcheck=0.0
      lcnum =0
      do 30 i=1,numpc
      tcheck=  max(tcheck,t(1,i))
      lcnum = min0(lcnum,lc(1,i))
   30 continue
      ido=1
      endif
c
      ip=0
      xmag=1.0
      do 200 ng=1,npcg
c
c     get current value of load curve
c
      if (lcnum.ge.0) then
      if (tcheck.eq.0.0) then
      do 50 i=1,ilen
      f(i)=fval(lc(1,ip+i))
   50 continue
      else
      do 60 i=1,ilen
      if (t(1,ip+i).eq.0.0) then
      f(i)=fval(lc(1,ip+i))
      else
      time=tt-t(1,ip+i)
      f(i)=0.0
      if (time.lt.0.0) go to 60
      locc=npc(lc(1,ip+i))
      npoint=(npc(lc(1,ip+i)+1)-locc)/2
      ierr=0
      call interp (p(locc),time,npoint,f(i),xmag,ierr)
      endif
   60 continue
      endif
c
      if (f(1)+f(ilen).eq.0.0) then
      fimax=0.0
      do 70 i=1,ilen
   70 fimax=  max(abs(f(i)),fimax)
      if (fimax.lt.1.e-20) go to 190
      endif
      endif
c
      do 80 i=1,ilen
      xx11(i)=x(1,nodes(1,ip+i))
      xx21(i)=x(2,nodes(1,ip+i))
      xx31(i)=x(3,nodes(1,ip+i))
      xx12(i)=x(1,nodes(2,ip+i))
      xx22(i)=x(2,nodes(2,ip+i))
      xx32(i)=x(3,nodes(2,ip+i))
      xx13(i)=x(1,nodes(3,ip+i))
      xx23(i)=x(2,nodes(3,ip+i))
      xx33(i)=x(3,nodes(3,ip+i))
      xx14(i)=x(1,nodes(4,ip+i))
      xx24(i)=x(2,nodes(4,ip+i))
      xx34(i)=x(3,nodes(4,ip+i))
   80 continue
c
      if (lcnum.lt.0) then
      do 90 i=1,ilen
      xctr(i)=.25*(xx11(i)+xx12(i)+xx13(i)+xx14(i))
      yctr(i)=.25*(xx21(i)+xx22(i)+xx23(i)+xx24(i))
      zctr(i)=.25*(xx31(i)+xx32(i)+xx33(i)+xx34(i))
   90 continue
      do 100 i=1,ilen
      if (lc(1,ip+i).lt.0) then
      call sbrode(xctr(i),yctr(i),zctr(i),tt,p,npc,f(i))
      else
      if (t(1,ip+i).eq.0.0) then
      f(i)=fval(lc(1,ip+i))
      else
      time=tt-t(1,ip+i)
      f(i)=0.0
      if (time.lt.0.0) go to 100
      locc=npc(lc(1,ip+i))
      npoint=(npc(lc(1,ip+i)+1)-locc)/2
      ierr=0
      call interp (p(locc),time,npoint,f(i),xmag,ierr)
      endif
      endif
  100 continue
      endif
c
c     gaussian quadrature
c
      do 110 i=1,ilen
      fs1=-xx11(i)+xx12(i)+xx13(i)-xx14(i)
      fs2=-xx21(i)+xx22(i)+xx23(i)-xx24(i)
      fs3=-xx31(i)+xx32(i)+xx33(i)-xx34(i)
      ft1=-xx11(i)-xx12(i)+xx13(i)+xx14(i)
      ft2=-xx21(i)-xx22(i)+xx23(i)+xx24(i)
      ft3=-xx31(i)-xx32(i)+xx33(i)+xx34(i)
      prs=f(i)*pmult(1,ip+i)/16.
      tr1(i)=(fs2*ft3-fs3*ft2)*prs
      tr2(i)=(fs3*ft1-fs1*ft3)*prs
      tr3(i)=(fs1*ft2-fs2*ft1)*prs
  110 continue
c
      do 120 i=1,ilen
      e3(1,nodes(1,ip+i))=e3(1,nodes(1,ip+i))-tr1(i)
      e3(2,nodes(1,ip+i))=e3(2,nodes(1,ip+i))-tr2(i)
      e3(3,nodes(1,ip+i))=e3(3,nodes(1,ip+i))-tr3(i)
      e3(1,nodes(2,ip+i))=e3(1,nodes(2,ip+i))-tr1(i)
      e3(2,nodes(2,ip+i))=e3(2,nodes(2,ip+i))-tr2(i)
      e3(3,nodes(2,ip+i))=e3(3,nodes(2,ip+i))-tr3(i)
      e3(1,nodes(3,ip+i))=e3(1,nodes(3,ip+i))-tr1(i)
      e3(2,nodes(3,ip+i))=e3(2,nodes(3,ip+i))-tr2(i)
      e3(3,nodes(3,ip+i))=e3(3,nodes(3,ip+i))-tr3(i)
      e3(1,nodes(4,ip+i))=e3(1,nodes(4,ip+i))-tr1(i)
      e3(2,nodes(4,ip+i))=e3(2,nodes(4,ip+i))-tr2(i)
      e3(3,nodes(4,ip+i))=e3(3,nodes(4,ip+i))-tr3(i)
  120 continue
  190 continue
      ip=ip+ilen
      ilen=128
  200 continue
      return
      end
      subroutine nbscal (x,v,irect,bulk,shear,e,nrt)
c     implicit double precision (a-h,o-z)                                    dp
c
c     non-reflecting boundary segments
c
      dimension x(3,*),v(3,*),irect(4,*),e(3,*),bulk(*),shear(*)
      do 10 n=1,nrt
      xx11=x(1,irect(1,n))
      xx21=x(2,irect(1,n))
      xx31=x(3,irect(1,n))
      xx12=x(1,irect(2,n))
      xx22=x(2,irect(2,n))
      xx32=x(3,irect(2,n))
      xx13=x(1,irect(3,n))
      xx23=x(2,irect(3,n))
      xx33=x(3,irect(3,n))
      xx14=x(1,irect(4,n))
      xx24=x(2,irect(4,n))
      xx34=x(3,irect(4,n))
      vxav=.25*(v(1,irect(1,n))+v(1,irect(2,n))+v(1,irect(3,n))
     1    +     v(1,irect(4,n)))
      vyav=.25*(v(2,irect(1,n))+v(2,irect(2,n))+v(2,irect(3,n))
     1    +     v(2,irect(4,n)))
      vzav=.25*(v(3,irect(1,n))+v(3,irect(2,n))+v(3,irect(3,n))
     1    +     v(3,irect(4,n)))
      fs1=.25*(-xx11+xx12+xx13-xx14)
      fs2=.25*(-xx21+xx22+xx23-xx24)
      fs3=.25*(-xx31+xx32+xx33-xx34)
      ft1=.25*(-xx11-xx12+xx13+xx14)
      ft2=.25*(-xx21-xx22+xx23+xx24)
      ft3=.25*(-xx31-xx32+xx33+xx34)
      tr1=fs2*ft3-fs3*ft2
      tr2=fs3*ft1-fs1*ft3
      tr3=fs1*ft2-fs2*ft1
      fsm=sqrt(fs1**2+fs2**2+fs3**2)
      trm=sqrt(tr1**2+tr2**2+tr3**2)
      xl1 =fs1/fsm
      xl2 =fs2/fsm
      xl3 =fs3/fsm
      xn1 =tr1/trm
      xn2 =tr2/trm
      xn3 =tr3/trm
      xm1 =xn2*xl3-xn3*xl2
      xm2 =xn3*xl1-xn1*xl3
      xm3 =xn1*xl2-xn2*xl1
      vl =xl1*vxav+xl2*vyav+xl3*vzav
      vm =xm1*vxav+xm2*vyav+xm3*vzav
      vn =xn1*vxav+xn2*vyav+xn3*vzav
      fcl=trm*vl*shear(n)
      fcm=trm*vm*shear(n)
      fcn=trm*vn*bulk(n)
      tn1=fcn*xn1+fcl*xl1+fcm*xm1
      tn2=fcn*xn2+fcl*xl2+fcm*xm2
      tn3=fcn*xn3+fcl*xl3+fcm*xm3
      e(1,irect(1,n))=e(1,irect(1,n))-tn1
      e(2,irect(1,n))=e(2,irect(1,n))-tn2
      e(3,irect(1,n))=e(3,irect(1,n))-tn3
      e(1,irect(2,n))=e(1,irect(2,n))-tn1
      e(2,irect(2,n))=e(2,irect(2,n))-tn2
      e(3,irect(2,n))=e(3,irect(2,n))-tn3
      e(1,irect(3,n))=e(1,irect(3,n))-tn1
      e(2,irect(3,n))=e(2,irect(3,n))-tn2
      e(3,irect(3,n))=e(3,irect(3,n))-tn3
      e(1,irect(4,n))=e(1,irect(4,n))-tn1
      e(2,irect(4,n))=e(2,irect(4,n))-tn2
      e(3,irect(4,n))=e(3,irect(4,n))-tn3
   10 continue
      return
      end
      subroutine sbrode(xx,yy,zz,tt,table,npt,op)
c     implicit double precision (a-h,o-z)                                    dp
c
c     sbrode computes the overpressure at the point (xx,yy,zb0,tt) due
c     to a nuclear blast of specified yield at (xb0,yb0,zb0+hob,tb0)
c
c------- arguments are (in order):
c------   xx : x coordinate of center of element face
c------   yy : y coordinate of center of element face
c------   zz : z coordinate of center of element face
c------   tt : time
c------   op : overpressure at (xx,yy,zz,tt)
c
      dimension table(*),npt(*)
c
c     this is a fortran implementation of the analytic expression
c     for pressure time history (h.l. brode, pse rpt. 1419-3, 09/86)
c
c     internal parameters are:
c
c     x = scaled ground range (kft/kt**(1/3))
c     y = scaled hob          (kft/kt**(1/3))
c     t = time after burst    (msec)
c     p = pressure            (psi)
c
c     note that limits are placed on values such as x,y,z,dt,etc...
c     this is done to avoid overflows and the values are machine
c     dependent.
c
c
      common/sbc0/yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal,shob,range(64),stoa(64),pkp(64)
      common/sbcom/yp5,y2,y3,y3p5,y4,y4p3,y7,y9,r2,r3,r4,r6,r8,xm
      common/sbthc/x,y,x4,ta,pp,dp,f,g,h
      save sbcom,sbthc
c
      parameter (third=1./3.)
      save ylast,yc1,yc2,yc3,yc4
      data ylast/-1./
      data one/1.0/
c
c------ yield in kt, gri and hob in kft, t in msec
      rr=sqrt((xx-xb0)**2+(yy-yb0)**2)
      gri=rr/bunits(1)
      tb=tt-tb0
      t=tb/bunits(2)
c
c------ obtain shock time of arrival at range gri
      if(lcyld.eq.0) then
           x=scal*gri
           y=shob
      else
           i=npt(lcyld)
           n=(npt(lcyld+1)-i)/2
           call interp(table(i),scal*tb,n,yldt,yield,ierr)
           vys=yldt**(-third)
           x=vys*gri
           y=vys*hob
      endif
      call sbcomv(x,y,r,z)
      call sbstoa(x,y,r,ta)
      if(lctoa.eq.0) then
           taa=t-ta
      else
           i=npt(lctoa)
           n=(npt(lctoa+1)-i)/2
           call interp(table(i),rr,n,tta,one,ierr)
           taa=t-tta/bunits(2)
      endif
c.... pressure is zero if t < ta
      if(taa.lt.0.) then
           op=0.
           return
      endif
c.... calculate peak overpressure
      call sbpkp(x,y,r,z,pp)
c.... calculate values for pressure formulas
      tap5=sqrt(ta)
      ta1p5=ta*tap5
      tap25=sqrt(tap5)
      tap75=tap25*tap5
      ta2=ta*ta
      ta3=ta2*ta
      ta4=ta3*ta
      ta8=ta4*ta4
      x4=x**4
      x4p13=x**4.13
      if(y.ne.ylast) then
           ylast=y
           yc1=2.441e-8*y2/(1.+9.e10*y7)
           yc2=1.-48000.*y3p5/(1.+48000.*y3p5)
           yc3=1.-1.1e10*y7/(1.+1.1e10*y7)
           yc4=1.-1.223e9*y4p3/(1.+1.223e9*y4p3)
      endif
      sc=yc1/(4.41e-11+x**10)
      s2=yc2-sc
      dp=((1640700.+24629.*ta+416.15*ta2)/(10880.+619.76*ta+ta2))
     1 *((.4+.001204*ta1p5/(1.+.001559*ta1p5))+(.6126+.5486*tap25
     2 /(1.+.00357*ta1p5)-3.47*ta**.637/(1.+5.696*ta**.645))*s2)
      s=yc3-sc
      f1=s*(.01477*tap75/(1.+.005836*ta)
     1 +7.402e-5*ta2*tap5/(1.+1.429e-8*ta4*tap75)-.216)
     2 +.7076-3.077e-5*ta3/(1.+4.367e-5*ta3)
      f2=.445-5.44*r**1.02/(1.+1.e5*r**5.84)
     1 +7.571*z**7.15/(1.+5.135*z**12.9)
     2 -8.07*z**7.31/(1.+5.583*z**12.23)
      f3=(150.*y**1.26/(1.+53800.*y**3.12))
     1 *(1.-1.9e-5*ta8/(1.+1.9e-5*ta8))
      f4=(.452-2.44e6*x4p13/(1.+5.368e6*x4p13))*yc4
      f=f1+f2*f3-f4
      g=10.+s*(77.58-64.99*ta**.125/(1.+.04348*tap5))
      h=3.003+.05601*ta/(1.+1.473e-9*ta4*ta)
     1 +s*(.01769*ta/(1.+3.207e-10*ta4*tap25)
     2 -.03209*ta*tap25/(1.+9.914e-8*ta4)-1.6)
c.... calculate pressure at (x,t)
      p=sbpth(taa)
c
c------ convert overpressure to dyna units
      op=p*bunits(3)
c
c------ return to calling program
      return
c
      end
      function sbpth(taa)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c     this is a fortran implementation of the analytic expression
c     for pressure time history (h.l. brode, psr rpt. 1419-3, 09/86)
c
c     the parameters are:
c
c     taa = time after shock time of arrival at scaled range x
c
c     note that limits are placed on values such as x,y,z,dt,etc...
c     this is done to avoid overflows and the values are machine
c     dependent.
c
c
      common/sbcom/yp5,y2,y3,y3p5,y4,y4p3,y7,y9,r2,r3,r4,r6,r8,xm
      common/sbthc/x,y,x4,ta,pp,dp,f,g,h
      common/double/iprec,ncpw,unit
      save sbcom,sbthc
c
      save ylast,xe,d2,dc,vc,cc,c2
      data ylast/-1./
      data zero /0.0/
c
      tadt=ta/(taa+ta)
      b=(f*tadt**g+(1.-f)*tadt**h)*(1.-taa/dp)
      if(x.lt.xm .or. y.gt..38) then
           p=pp*b
      else
           if(y.ne.ylast) then
                ylast=y
                xe=3.039*y/(1.+6.7*y)
                d2=2.99+2.263e14*y**9.86/(1.+1.151e14*y**9.87)
                dc=.583*y2/(.026667+y2)
                vc=8.777e7*y**5.185/(1.+1.1185e6*y**4.189)
     1            +5.9584e13*y**8.049/(1.+3.3966e12*y**7.497)
     2            -340371.*y**3.423/(1.+3619.4*y**2.538)
                cc=1.-2.625e8*y3*y3p5/(1.+1.889e8*y**6.04)
                c2=2.3e13*y9/(1.+2.3e13*y9)
           endif
           ee=  min(abs((x-xm)/(xe-xm)),50.*unit)
           ee2=ee*ee
           ee2p5=ee2*sqrt(ee)
           ee20=ee**20
           d=.23+dc+.27*ee+(.5-dc)*ee**d2
           a=(d-1.)*(1.-ee20/(1.+ee20))
           dt=474.2*y*(x-xm)**1.25
           ga=  min(taa/dt,200.*unit)
           ga3=ga*ga*ga
           v=1.+vc*(ga3/(6.13+ga3))/(1.+9.23*ee2)
           c3=(1.094*ee**.738/(1.+3.687*ee**2.63))*cc-.15
           c3=1.+c3/(1.+.5089*ee**13)
           c=(1.+a)*(1.+.923*ga**8.5)
           c=(1.04-240.9*x4/(1.+231.7*x4))*ga**7/c
           c=c*(c2+(1.-c2)*(1.-.09*ee2p5/(1.+.09*ee2p5)))
           c=c*c3*(1.-(taa/dp)**8)
           p=pp*(1.+a)*(b*v+c)
      endif
c
      sbpth=  max(zero,p)
      return
      end
      subroutine sbcomv(x,y,r,z)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c     this routine calculates common values for the speicher-brode
c     analytic expressions for pressure time history (psr 1419-3)
c
c     the parameters are:
c
c     x = scaled ground range (kft/kt**(1/3))
c     y = scaled hob          (kft/kt**(1/3))
c     r = (x*x+y*y)**(1/2)    (kft/kt**(1/3))
c     z = y/x
c
c
      common/sbcom/yp5,y2,y3,y3p5,y4,y4p3,y7,y9,r2,r3,r4,r6,r8,xm
      common/double/iprec,ncpw,unit
      save sbcom
c
      save ylast
      data ylast/-1./
c
      x=  max(x,1.e-12*unit)
      y=  max(y,1.e-12*unit)
      r=sqrt(x*x+y*y)
      if(y.ne.ylast) then
           ylast=y
           yp5=sqrt(y)
           y2=y*y
           y3=y2*y
           y3p5=y3*yp5
           y4=y2*y2
           y4p3=y**4.3
           y7=y4*y2*y
           y9=y7*y2
           xm=170.*y/(1.+337.*sqrt(yp5))+.914*y2*yp5
      endif
      r2=r*r
      r3=r2*r
      r4=r3*r
      r6=r4*r2
      r8=r6*r2
      z=y/x
      z=  min(z,100.*unit)
      return
      end
      subroutine sbstoa(x,y,r,ta)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c     this is a fortran implementation of the analytic expression for
c     shock time of arrival (h.l. brode, psr rpt. 1419-3, 09/86)
c
c     the parameters are:
c
c     x = scaled ground range (kft/kt**(1/3))
c     y = scaled hob          (kft/kt**(1/3))
c     r = (x*x+y*y)**(1/2)    (kft/kt**(1/3))
c     ta= time of arrival     (msec)
c
c
      common/sbcom/yp5,y2,y3,y3p5,y4,y4p3,y7,y9,r2,r3,r4,r6,r8,xm
      save sbcom
c
      u=2.99e-14-1.91e-10*r2+1.032e-6*r4-4.43e-6*r6
     1 +(1.028+2.087*r+2.69*r2)*r8
      u=(.543-21.8*r+386.*r2+2383.*r3)*r8/u
      if(x.lt.xm) then
           ta=u
      else
           w=3.0137e-13-1.2128e-9*r2+4.128e-6*r4-1.116e-5*r6
     1      +(1.632+2.629*r+2.69*r2)*r8
           w=(1.086-34.605*r+486.3*r2+2383.*r3)*r8/w
           xmdx=xm/x
           ta=u*xmdx+w*(1.-xmdx)
      endif
      return
      end
      subroutine sbpkp(x,y,r,z,pp)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c     this is a fortran implementation of the analytic expression
c     for peak overpressure (h.l. brode, psr rpt. 1419-3, 09/86)
c
c     the parameters are:
c
c     x = scaled ground range (kft/kt**(1/3))
c     y = scaled hob          (kft/kt**(1/3))
c     r = (x*x+y*y)**(1/2)    (kft/kt**(1/3))
c     z = y/x
c     pp= peak overpressure   (psi)
c
c
      common/sbcom/yp5,y2,y3,y3p5,y4,y4p3,y7,y9,r2,r3,r4,r6,r8,xm
      save sbcom
c
      save ylast,hc,p,q
      data ylast/-1./
c
      if(y.ne.ylast) then
           ylast=y
           hc=1.+2.*y
c.... note: 10/84 formula
c          p=.00629-2.67*y2/(1.+1.e7*y4p3)
           p=.000629*y4/(3.493e-9+y4)-2.67*y2/(1.+1.e7*y4p3)
           q=5.18+.2803*y3p5/(3.788e-6+y4)
      endif
      zp5=sqrt(z)
      z2=z*z
      z2p5=z2*zp5
      z3=z2*z
      z3p5=z2p5*z
      z5=z3*z2
      z6=z5*z
      z8=z6*z2
      z14p5=z6*z8*zp5
      z17=z8*z8*z
      z18=z17*z
      a=1.22-3.908*z2/(1.+810.2*z5)
      b=2.321+6.195*z18/(1.+1.113*z18)-.03831*z17/(1.+.02415*z17)
     1 +.6692/(1.+4164.*z8)
      c=4.153-1.149*z18/(1.+1.641*z18)-1.1/(1.+2.771*z2p5)
      d=25.76*z**1.75/(1.+1.382*z18)+8.257*z/(1.+3.219*z)-4.166
      e=1.-.004642*z18/(1.+.003886*z18)
      f=.6096+2.879*z**9.25/(1.+2.359*z14p5)-17.15*z2/(1.+71.66*z3)
      g=1.83+5.361*z2/(1.+.3139*z6)
      h=1.094*r2/((781.2-123.4*r+37.98*r*sqrt(r)+r2)*hc)
     1 -(.2905+64.67*z5)/(1.+441.5*z5)-1.389*z/(1.+49.03*z5)
     2 +8.808*z*zp5/(1.+154.5*z3p5)
      pp=10.47/r**a+b/r**c+(d*e)/(1.+f*r**g)+h+p/r**q
      return
      end
      subroutine velth(tcode,x,a,v,nvel,vx,vy,vz,lc,fval,ilcf,rbv,
     1 iaccfg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     interpolate and set specified nodal velocities
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
c
      dimension nvel(1),vx(1),vy(1),vz(1),lc(1),fval(1),tcode(1),v(1),
     1          x(1),a(1),ilcf(1),rbv(6,1),iaccfg(*)
c
      if (numvc.eq.0) return
      ip=3*numnp
      do 90 i=1,numvc
      lcc=lc(i)
      if(ilcf(lcc).ne.idrflg .and. ilcf(lcc).ne.2) go to 90
      vi=fval(lcc)
      k=nvel(i)
      ib=tcode(i)
      if (k.gt.0) then
      k3=3*k
      k2=k3-1
      k1=k2-1
      iadd=0
      if (iaccfg(i).eq.0) then
      if (ib.le.4) go to 5
      if (ib.gt.8) go to 42
      ib=ib-4
      iadd=ip
    5 go to (10,20,30,40), ib
   10 v(k1+iadd)=vi*vx(i)
      a(k1+iadd)=0.
      go to 90
   20 v(k2+iadd)=vi*vy(i)
      a(k2+iadd)=0.
      go to 90
   30 v(k3+iadd)=vi*vz(i)
      a(k3+iadd)=0.
      go to 90
   40 scale=sqrt(vx(i)**2+vy(i)**2+vz(i)**2)
      bcs1=vx(i)/scale
      bcs2=vy(i)/scale
      bcs3=vz(i)/scale
      vinew=vi*scale
      dv=v(k1+iadd)*bcs1+v(k2+iadd)*bcs2+v(k3+iadd)*bcs3-vinew
      da=a(k1+iadd)*bcs1+a(k2+iadd)*bcs2+a(k3+iadd)*bcs3
      v(k1+iadd)=v(k1+iadd)-dv*bcs1
      v(k2+iadd)=v(k2+iadd)-dv*bcs2
      v(k3+iadd)=v(k3+iadd)-dv*bcs3
      a(k1+iadd)=a(k1+iadd)-da*bcs1
      a(k2+iadd)=a(k2+iadd)-da*bcs2
      a(k3+iadd)=a(k3+iadd)-da*bcs3
      go to 90
   42 xk=x(k1)
      yk=x(k2)
      zk=x(k3)
      ib=ib-8
      if (ib.eq.1) then
      v(k2)=-vi*vx(i)*zk
      v(k3)= vi*vx(i)*yk
      a(k2)=0.
      a(k3)=0.
      elseif (ib.eq.2) then
      v(k1)= vi*vy(i)*zk
      v(k3)=-vi*vy(i)*xk
      a(k1)=0.
      a(k3)=0.
      elseif (ib.eq.3) then
      v(k1)=-vi*vz(i)*yk
      v(k2)= vi*vz(i)*xk
      a(k1)=0.
      a(k2)=0.
      endif
      else
      if (ib.le.4) go to 108
      if (ib.gt.8) go to 142
      ib=ib-4
      iadd=ip
  108 go to (110,120,130,140), ib
  110 v(k1+iadd)=vi*vx(i)
      go to 90
  120 a(k2+iadd)=vi*vy(i)
      go to 90
  130 a(k3+iadd)=vi*vz(i)
      go to 90
  140 scale=sqrt(vx(i)**2+vy(i)**2+vz(i)**2)
      bcs1=vx(i)/scale
      bcs2=vy(i)/scale
      bcs3=vz(i)/scale
      vinew=vi*scale
      dv=a(k1+iadd)*bcs1+a(k2+iadd)*bcs2+a(k3+iadd)*bcs3-vinew
      a(k1+iadd)=a(k1+iadd)-dv*bcs1
      a(k2+iadd)=a(k2+iadd)-dv*bcs2
      a(k3+iadd)=a(k3+iadd)-dv*bcs3
      go to 90
  142 xk=x(k1)
      yk=x(k2)
      zk=x(k3)
      ib=ib-8
      if (ib.eq.1) then
      a(k2)=-vi*vx(i)*zk
      a(k3)= vi*vx(i)*yk
      elseif (ib.eq.2) then
      a(k1)= vi*vy(i)*zk
      a(k3)=-vi*vy(i)*xk
      elseif (ib.eq.3) then
      a(k1)=-vi*vz(i)*yk
      a(k2)= vi*vz(i)*xk
      endif
      endif
      else
      k=-k
      if (ib.eq.1) then
      rbv(1,k)=vi*vx(i)
      elseif (ib.eq.2) then
      rbv(2,k)=vi*vy(i)
      elseif (ib.eq.3) then
      rbv(3,k)=vi*vz(i)
      elseif (ib.eq.4) then
      scale=sqrt(vx(i)**2+vy(i)**2+vz(i)**2)
      bcs1=vx(i)/scale
      bcs2=vy(i)/scale
      bcs3=vz(i)/scale
      vinew=vi*scale
      dv=rbv(1,k)*bcs1+rbv(2,k)*bcs2+rbv(3,k)*bcs3-vinew
      rbv(1,k)=rbv(1,k)-dv*bcs1
      rbv(2,k)=rbv(2,k)-dv*bcs2
      rbv(3,k)=rbv(3,k)-dv*bcs3
      elseif (ib.eq.5) then
      rbv(4,k)=vi*vx(i)
      elseif (ib.eq.6) then
      rbv(5,k)=vi*vy(i)
      elseif (ib.eq.7) then
      rbv(6,k)=vi*vz(i)
      elseif (ib.eq.8) then
      scale=sqrt(vx(i)**2+vy(i)**2+vz(i)**2)
      bcs1=vx(i)/scale
      bcs2=vy(i)/scale
      bcs3=vz(i)/scale
      vinew=vi*scale
      dv=rbv(4,k)*bcs1+rbv(5,k)*bcs2+rbv(6,k)*bcs3-vinew
      rbv(4,k)=rbv(4,k)-dv*bcs1
      rbv(5,k)=rbv(5,k)-dv*bcs2
      rbv(6,k)=rbv(6,k)-dv*bcs3
      endif
      endif
   90 continue
      return
      end
      subroutine solide
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
      common /   / a(1)
      call solde (a(n1),a(n2),a(n3),a(lc0),a(lc9),a(lc10),a(lc11),a(n26)
     1 ,a(n27),a(n28),a(lc15),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),
     2 a(n4e),a(lc1h),a(ntmp0+1),a(n19),numelh,nwcon,a(n25+7),nmmat,
     3 a(n8),a(n9),a(nh01),a(nh02),a(nh03),a(nh14),a(nh11),a(nh12),
     4 a(n80),a(n80+nmmat),a(n80+2*nmmat))
      return
      end
      subroutine solde (mtype,ro,cm,u,v,fv,x,nsubgv,mtnum,nfegp,auxvec,
     1 ieost,eosp,ihgq,hgq,iqtype,bkqs,ix,tnew,fval,numelh,nwcon,ener,
     2 nmmat,npc,pld,hgfrcs,xies,bqs,lochvh,nnch,iblkh,dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk12/b12,b2,qhg
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/energy/xinen
      common /   / a(1)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      dimension mtype(*),ro(*),cm(48,*),u(*),v(*),fv(*),x(*),nsubgv(*),
     1 mtnum(*),nfegp(*),auxvec(*),ieost(*),eosp(*),ihgq(*),hgq(*),
     2 iqtype(*),bkqs(3,*),ix(nwcon,*),tnew(*),fval(*),ener(*),npc(*),
     3 pld(*),hgfrcs(12,*),xies(*),bqs(*),lochvh(*),nnch(*),iblkh(*),
     4 dampk(*),ym(*),pr(*)
      nelg=(numelh-1)/128+1
      lczc=0
      lcz=0
      ic =0
      do 220 nn=1,nelg
      noco=nn
      nnm1=128*(nn-1)
      nnm2=nnm1+1
      nmel=128
      if (nn.eq.nelg) nmel=numelh-128*(nelg-1)
      call unpki (ix(1,nnm2),nwcon,nmel)
      nsubg=nsubgv(nn)
      do 210 n=1,nsubg
      ic =ic +1
      lcn=lcz+n
      nnc=nnch(ic)
      mxe=mtnum(lcn)
      if (mxe.eq.0)  go to 210
      lft=nfegp(lcn)
      llt=nfegp(lcn+1)-1
      lav=lochvh(nnm1+lft)
      mte=mtype(mxe)
      if (mte.eq.20) go to 210
      nes=ieost(mxe)
      ibq=iqtype(mxe)
      ihg=ihgq(mxe)
      qhg=hgq(mxe)
      b12=bkqs(1,mxe)
      b2 =bkqs(2,mxe)
      nmtcon=7+nconst(mte)
      if (nes.ne.0) nmtcon=nmtcon+ncneos(nes)
      rhoa(lft)=ro(mxe)
      if (ihg.ne.3.and.ihg.lt.5) then
      call strain (x,u(nnm2),v)
      else
      call strnfb (x,u(nnm2),v)
      endif
      if (ihg.gt.3) then
      call hgfrot(hgfrcs(1,nnm2),lft,llt)
      endif
      call felen
      if (mte.gt.10) go to 120
      go to (20,30,40,50,60,70,80,90,100,110), mte
   20 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm1 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   30 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm2 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   40 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call rstrn
      call f3dm3 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   50 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm4 (cm,bqs(nnm2),tnew,fval)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   60 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm5 (cm,bqs(nnm2),0)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   70 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call rstrn2
      call f3dm6 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   80 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm7 (cm,bqs(nnm2),ihg)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
   90 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm8 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hieupd
      call eqos (fval,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  100 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm9 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hieupd
      call eqos (fval,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  110 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm10 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
c     if(idmp.eq.1.and.kp.lt.0) call psqrt(tta(nnml+1),0)
      call hieupd
      call eqos (fval,ener,mte,nes)
c     if(idmp.eq.1.and.kp.lt.0) call psqrt(tta(nnml+1),1)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  120 mte10=mte-10
      if (mte.eq.31) then
      call hvpac1 (nmtcon,auxvec(lav))
      call f3dm31(cm)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
      endif
      if (mte.eq.32) then
      call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call rstrn2
      call f3dm32(cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
      endif
      if (mte.eq.33) then
      call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
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
      call engbrk(ener)
      call hieupd
      call eqos (fval,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  140 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm12 (cm,bqs(nnm2),0)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  150 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm13 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  160 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm14 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  170 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm15 (cm)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hieupd
      call eqos (fval,ener,mte,nes)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  180 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm16 (cm,npc,pld,eosp,mte,nes,ibq,ener,fval)
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  190 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm17 (cm,x,nnm1)
      call sueos (eosp,mte,nes)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hieupd
      call eqos (fval,ener,mte,nes)
      call fzero
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  192 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm18 (cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  194 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm19 (cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  196 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm21(cm,bqs(nnm2),tnew,fval)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  197 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm22(cm,bqs(nnm2))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  198 call hvpac1 (nmtcon,auxvec(lav))
      lthrpr=nint(cm(48,mxe))
      call f3dm23(cm,bqs(nnm2),tnew,fval,a(lthrpr))
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  199 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm24(cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  200 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm25(cm)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  204 call hvpac1 (nmtcon,auxvec(lav))
      call rstrss
      call f3dm26(cm,bqs(nnm2),npc,pld)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
      go to 207
  205 call hvpac1 (nmtcon,auxvec(lav))
      call f3dm27(cm,bqs(nnm2),ihg)
      call bulkq (ibq,a(nh17))
      call engbrk(ener)
      call lieupd (ener,xies(nnm2),bqs(nnm2))
      call hvpac2 (nmtcon,auxvec(lav))
  207 continue
      if (dampk(mxe).ne.0.) call rydmp1(dampk(mxe),ym(mxe),pr(mxe))
      if (ihg.ne.3.and.ihg.lt.5) then
      if (ihg.eq. 2.or.ihg.eq.4) then
      call hrgfb (hgfrcs(1,nnm2),ihg)
      else
      call hrgmd
      endif
      call force (fv,iblkh)
      else
      call hrgffb (hgfrcs(1,nnm2),ihg)
      call forcef (fv,iblkh)
      endif
  210 lczc=lczc+nnc+1
      lcz=lcz+nsubg+1
  220 continue
      do 230 i=1,nmmat
  230 xinen=xinen+ener(i)
      return
      end
      subroutine rydmp1 (dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     rayleigh damping for solid elements
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),davg(128),pd(128)
      common/aux18/dd(128),dfe(128)
      common/aux36/lft,llt
      data third/.3333333333333/
c
      blk=-ym/((1.-2.*pr))
      do 10 i=lft,llt
      davg(i)=third*dd(i)
      pd(i)=blk*davg(i)
   10 continue
c
      g=ym/(1.+pr)
      fac3=g*dampk
      fac2=0.5*fac3
c
      do 20 i=lft,llt
      sig1(i)=sig1(i)+pd(i)*dampk+fac3*(d1(i)+davg(i))
      sig2(i)=sig2(i)+pd(i)*dampk+fac3*(d2(i)+davg(i))
      sig3(i)=sig3(i)+pd(i)*dampk+fac3*(d3(i)+davg(i))
      sig4(i)=sig4(i)+fac2*d4(i)
      sig5(i)=sig5(i)+fac2*d5(i)
      sig6(i)=sig6(i)+fac2*d6(i)
   20 continue
c
      return
      end
      subroutine rydmp2 (dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     rayleigh damping for shell elements
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),davg(128),pd(128)
      common/aux36/lft,llt
      data third/.3333333333333/
c
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      fac1=0.5*dampk*dti*ym/(1.+pr)
      fac2=dampk*dti*ym/(1.-pr*pr)
      fac3=pr*fac2
c
      do 20 i=lft,llt
      sig1(i)=sig1(i)+fac2*d1(i)+fac3*d2(i)
      sig2(i)=sig2(i)+fac3*d1(i)+fac2*d2(i)
      sig4(i)=sig4(i)+fac1*d4(i)
      sig5(i)=sig5(i)+fac1*d5(i)
      sig6(i)=sig6(i)+fac1*d6(i)
   20 continue
c
      return
      end
      subroutine rydmp3(dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     rayleigh damping for integrated beam elements
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),davg(128),pd(128)
      common/aux36/lft,llt
      data third/.3333333333333/
c
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      fac1=0.5*dampk*dti*ym/(1.+pr)
      fac2=dampk*dti*ym
c
      do 20 i=lft,llt
      sig1(i)=sig1(i)+fac2*d1(i)
      sig4(i)=sig4(i)+fac1*d4(i)
      sig6(i)=sig6(i)+fac1*d6(i)
   20 continue
c
      return
      end
      subroutine rydmp3r(cm,ra,ri1,ri2,rj,dampk,ym,pr)
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
      dimension cm(*),ra(5,*),ri1(5,*),ri2(5,*),rj(5,*)
c
      mx=48*(mxt(lft)-1)
c
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      g=ym/(2.+2.*pr)
      sndspd=ym
      pr24  =24.*(1.+pr)
c
      do 10 i=lft,llt
      ft21(i)=ft21(i)+dampk*dti*ym*ra(1,i)*xe1(i)*ddef1(i)
      fm21(i)=fm21(i)+dampk*dti*g*rj(1,i)*xe1(i)*ddef2(i)
c
      fac =pr24*xe1(i)*xe1(i)/rj(2,i)
      phi1=fac*ri1(1,i)
      phi2=fac*ri2(1,i)
      phi14=4.+phi1
      phi12=2.-phi1
      phi24=4.+phi2
      phi22=2.-phi2
      ct1=dti*dampk*ym*ri1(1,i)*xe1(i)/(1.+phi1)
      fm12(i)=fm12(i)+ct1*(phi14*ddef3(i)+phi12*ddef5(i))
      fm22(i)=fm22(i)+ct1*(phi12*ddef3(i)+phi14*ddef5(i))
      ct2=dti*dampk*ym*ri2(1,i)*xe1(i)/(1.+phi2)
      fm13(i)=fm13(i)+ct2*(phi24*ddef4(i)+phi22*ddef6(i))
      fm23(i)=fm23(i)+ct2*(phi22*ddef4(i)+phi24*ddef6(i))
   10 continue
c
      return
      end
      subroutine rydmp3t(dampk,ym,pr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     rayleigh damping for truss elements
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),davg(128),pd(128)
      common/aux36/lft,llt
      data third/.3333333333333/
c
      dti=1.0
      if (dt1.gt.1.e-14) dti=1./dt1
      fac2=dampk*dti*ym
      do 20 i=lft,llt
      sig1(i)=sig1(i)+fac2*d1(i)
   20 continue
c
      return
      end
      subroutine hgfrot(hf,lft,llt)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux2/dxx(128),dyy(128),dzz(128),d1(128),d2(128),
     1 d3(128),wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux5/rot1(128),rot2(128),rot3(128),rot4(128),rot5(128),
     1 rot6(128),rot7(128),rot8(128),rot9(128)
      common/aux13/p11(128),p12(128),p13(128),p14(128),
     1             p15(128),p16(128),p17(128),p18(128),
     2             p21(128),p22(128),p23(128),p24(128),
     3             p25(128),p26(128),p27(128),p28(128),
     4             p31(128),p32(128),p33(128),p34(128),
     5             p35(128),p36(128),p37(128),p38(128),
     6             hfq(4,3,128)
      dimension hf(4,3,*)
c
      call rwingv(wxxdt,wyydt,wzzdt,lft,llt)
c
      do 10 i=lft,llt
      hfq(1,1,i)=rot1(i)*hf(1,1,i)+rot4(i)*hf(1,2,i)+rot7(i)*hf(1,3,i)
      hfq(1,2,i)=rot2(i)*hf(1,1,i)+rot5(i)*hf(1,2,i)+rot8(i)*hf(1,3,i)
      hfq(1,3,i)=rot3(i)*hf(1,1,i)+rot6(i)*hf(1,2,i)+rot9(i)*hf(1,3,i)
      hfq(2,1,i)=rot1(i)*hf(2,1,i)+rot4(i)*hf(2,2,i)+rot7(i)*hf(2,3,i)
      hfq(2,2,i)=rot2(i)*hf(2,1,i)+rot5(i)*hf(2,2,i)+rot8(i)*hf(2,3,i)
      hfq(2,3,i)=rot3(i)*hf(2,1,i)+rot6(i)*hf(2,2,i)+rot9(i)*hf(2,3,i)
      hfq(3,1,i)=rot1(i)*hf(3,1,i)+rot4(i)*hf(3,2,i)+rot7(i)*hf(3,3,i)
      hfq(3,2,i)=rot2(i)*hf(3,1,i)+rot5(i)*hf(3,2,i)+rot8(i)*hf(3,3,i)
      hfq(3,3,i)=rot3(i)*hf(3,1,i)+rot6(i)*hf(3,2,i)+rot9(i)*hf(3,3,i)
      hfq(4,1,i)=rot1(i)*hf(4,1,i)+rot4(i)*hf(4,2,i)+rot7(i)*hf(4,3,i)
      hfq(4,2,i)=rot2(i)*hf(4,1,i)+rot5(i)*hf(4,2,i)+rot8(i)*hf(4,3,i)
      hfq(4,3,i)=rot3(i)*hf(4,1,i)+rot6(i)*hf(4,2,i)+rot9(i)*hf(4,3,i)
   10 continue
      do 30 k=1,12
      do 20 i=lft,llt
      hf(k,1,i)=hfq(k,1,i)
   20 continue
   30 continue
      return
      end
      subroutine unpki(ixp,nwcon,nmel)
c     implicit double precision (a-h,o-z)                                    dp
c
c     unpack connection data
c
      common/aux32/kka(128),kkb(128),kkc(128),
     1 kk1(128),kk2(128),kk3(128),dxy(128),
     2 dyx(128),dyz(128),dzy(128),dzx(128),
     3 dxz(128),vx17(128),vx28(128),vx35(128),
     4 vx46(128),vy17(128),vy28(128),
     5 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128)
      dimension ixp(nwcon,1)
c     data ishft1,ishft2,imask/ 21,42,3777777b/                         cray1
c
c     do 40 i=1,nmel                                                    cray1
c     kk1(i)=shiftr(ixp(1,i),ishft1)                                    cray1
c     kk2(i)=shiftr(ixp(2,i),ishft1)                                    cray1
c     kk3(i)=shiftr(ixp(3,i),ishft1)                                    cray1
c     kka(i)=shiftr(ixp(1,i),ishft2)                                    cray1
c     kkb(i)=shiftr(ixp(2,i),ishft2)                                    cray1
c  40 kkc(i)=shiftr(ixp(3,i),ishft2)                                    cray1
c     do 50 i=1,nmel                                                    cray1
c     mxt(i)=and(ixp(1,i),imask)                                        cray1
c     ix3(i)=and(ixp(2,i),imask)                                        cray1
c     ix6(i)=and(ixp(3,i),imask)                                        cray1
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
      subroutine hvpac1 (nc,aux)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,8)
      common/aux36/lft,llt
      dimension aux(nc,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 ax(n,m)=aux(m,n-k)
   20 continue
      return
      end
      subroutine hvpac2 (nc,aux)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ax(128,8)
      common/aux36/lft,llt
      dimension aux(nc,1)
      k=lft-1
      do 20 m=1,nc
      do 10 n=lft,llt
   10 aux(m,n-k)=ax(n,m)
   20 continue
      return
      end
      subroutine strain(x,volo,v)
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
      dimension x(3,1),v(3,1),dux(1),duy(1),duz(1),volo(1)
c
      equivalence (dux,vx1),(duy,vy1),(duz,vz1)
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
c     calculate velocity strain rates, spins, and deformation gradients
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
      dxx(i)=px1(i)*vx17(i)+px2(i)*vx28(i)+px3(i)*vx35(i)+px4(i)*vx46(i)
      dyy(i)=py1(i)*vy17(i)+py2(i)*vy28(i)+py3(i)*vy35(i)+py4(i)*vy46(i)
      dzz(i)=pz1(i)*vz17(i)+pz2(i)*vz28(i)+pz3(i)*vz35(i)+pz4(i)*vz46(i)
      dxy(i)=py1(i)*vx17(i)+py2(i)*vx28(i)+py3(i)*vx35(i)+py4(i)*vx46(i)
   30 dxz(i)=pz1(i)*vx17(i)+pz2(i)*vx28(i)+pz3(i)*vx35(i)+pz4(i)*vx46(i)
      do 40 i=lft,llt
      dyx(i)=px1(i)*vy17(i)+px2(i)*vy28(i)+px3(i)*vy35(i)+px4(i)*vy46(i)
      dyz(i)=pz1(i)*vy17(i)+pz2(i)*vy28(i)+pz3(i)*vy35(i)+pz4(i)*vy46(i)
      dzx(i)=px1(i)*vz17(i)+px2(i)*vz28(i)+px3(i)*vz35(i)+px4(i)*vz46(i)
   40 dzy(i)=py1(i)*vz17(i)+py2(i)*vz28(i)+py3(i)*vz35(i)+py4(i)*vz46(i)
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
      dfe(i)=voln(i)*xm(i)
      vlrho(i)=crho*volo(i)
      wzzdt(i)=dt1d2*(dyx(i)-dxy(i))
      wyydt(i)=dt1d2*(dxz(i)-dzx(i))
   50 wxxdt(i)=dt1d2*(dzy(i)-dyz(i))
c
      return
c
      end
      subroutine prtal
c     implicit double precision (a-h,o-z)                                    dp
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux9/vlrho(128),det(128)
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
      common/aux32/a17(128),a28(128),dett(128),
     1 aj1(128),aj2(128),aj3(128),aj4(128),
     2 aj5(128),aj6(128),aj7(128),aj8(128),
     3 aj9(128),x17(128),x28(128),x35(128),
     4 x46(128),y17(128),y28(128),y35(128),
     5 y46(128),z17(128),z28(128),z35(128),z46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
c
      dimension aj5968(1),aj6749(1),aj4857(1),aji1(1),aji2(1),aji3(1),
     1          aji4(1),aji5(1),aji6(1),aji7(1),aji8(1),aji9(1),aj12(1),
     2          aj45(1),aj78(1),b17(1),b28(1),c17(1),c28(1)
c
      equivalence (x17,aj5968),(x28,aj6749),(x35,aj4857),(x46,aji1),
     1 (y17,aji2),(y28,aji3),(y35,aji4),(y46,aji5),(z17,aji6),
     2 (z28,aji7),(z35,aji8),(z46,aji9),(aj1,aj12),(aj2,aj45),
     3 (aj3,aj78),(px1,b17),(px2,b28),(px3,c17),(px4,c28)
      data o64th/0.0156250/
c
c     jacobian matrix
c
      do 10 i=lft,llt
      x17(i)=x7(i)-x1(i)
      x28(i)=x8(i)-x2(i)
      x35(i)=x5(i)-x3(i)
      x46(i)=x6(i)-x4(i)
      y17(i)=y7(i)-y1(i)
      y28(i)=y8(i)-y2(i)
      y35(i)=y5(i)-y3(i)
      y46(i)=y6(i)-y4(i)
      z17(i)=z7(i)-z1(i)
      z28(i)=z8(i)-z2(i)
      z35(i)=z5(i)-z3(i)
   10 z46(i)=z6(i)-z4(i)
      do 20 i=lft,llt
      aj1(i)=x17(i)+x28(i)-x35(i)-x46(i)
      aj2(i)=y17(i)+y28(i)-y35(i)-y46(i)
      aj3(i)=z17(i)+z28(i)-z35(i)-z46(i)
      a17(i)=x17(i)+x46(i)
      a28(i)=x28(i)+x35(i)
      b17(i)=y17(i)+y46(i)
      b28(i)=y28(i)+y35(i)
      c17(i)=z17(i)+z46(i)
   20 c28(i)=z28(i)+z35(i)
      do 30 i=lft,llt
      aj4(i)=a17(i)+a28(i)
      aj5(i)=b17(i)+b28(i)
      aj6(i)=c17(i)+c28(i)
      aj7(i)=a17(i)-a28(i)
      aj8(i)=b17(i)-b28(i)
   30 aj9(i)=c17(i)-c28(i)
c
c     jacobian
c
      do 40 i=lft,llt
      aj5968(i)=aj5(i)*aj9(i)-aj6(i)*aj8(i)
      aj6749(i)=aj6(i)*aj7(i)-aj4(i)*aj9(i)
   40 aj4857(i)=aj4(i)*aj8(i)-aj5(i)*aj7(i)
      do 50 i=lft,llt
   50 det(i)=o64th*(aj1(i)*aj5968(i)+aj2(i)*aj6749(i)+aj3(i)*aj4857(i))
      do 60 i=lft,llt
   60 dett(i)=o64th/det(i)
c
c     inverse of the jacobian matrix
c
      do 70 i=lft,llt
      aji1(i)=dett(i)*aj5968(i)
      aji4(i)=dett(i)*aj6749(i)
      aji7(i)=dett(i)*aj4857(i)
      aji2(i)=dett(i)*(-aj2(i)*aj9(i)+aj3(i)*aj8(i))
      aji5(i)=dett(i)*(aj1(i)*aj9(i)-aj3(i)*aj7(i))
      aji8(i)=dett(i)*(-aj1(i)*aj8(i)+aj2(i)*aj7(i))
      aji3(i)=dett(i)*(aj2(i)*aj6(i)-aj3(i)*aj5(i))
      aji6(i)=dett(i)*(-aj1(i)*aj6(i)+aj3(i)*aj4(i))
      aji9(i)=dett(i)*(aj1(i)*aj5(i)-aj2(i)*aj4(i))
   70 continue
c
      do 80 i=lft,llt
      aj12(i)=aji1(i)-aji2(i)
      aj45(i)=aji4(i)-aji5(i)
      aj78(i)=aji7(i)-aji8(i)
      px3(i)=aj12(i)+aji3(i)
      py3(i)=aj45(i)+aji6(i)
      pz3(i)=aj78(i)+aji9(i)
      px4(i)=aj12(i)-aji3(i)
      py4(i)=aj45(i)-aji6(i)
   80 pz4(i)=aj78(i)-aji9(i)
      do 90 i=lft,llt
      aj12(i)=aji1(i)+aji2(i)
      aj45(i)=aji4(i)+aji5(i)
      aj78(i)=aji7(i)+aji8(i)
      px1(i)=-aj12(i)-aji3(i)
      py1(i)=-aj45(i)-aji6(i)
      pz1(i)=-aj78(i)-aji9(i)
      px2(i)=-aj12(i)+aji3(i)
      py2(i)=-aj45(i)+aji6(i)
   90 pz2(i)=-aj78(i)+aji9(i)
c
      return
c
      end
      subroutine strnfb(x,volo,v)
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
     & px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     & py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     & pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     & vx1(128),vx2(128),vx3(128),vx4(128),
     & vx5(128),vx6(128),vx7(128),vx8(128),
     & vy1(128),vy2(128),vy3(128),vy4(128),
     & vy5(128),vy6(128),vy7(128),vy8(128),
     & vz1(128),vz2(128),vz3(128),vz4(128),
     & vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux18/dd(128),dfe(128)
      common/aux32/a17(128),a28(128),dett(128),
     1 aj1(128),aj2(128),aj3(128),dxy(128),
     1 dyx(128),dyz(128),dzy(128),dzx(128),
     2 dxz(128),vx17(128),vx28(128),vx35(128),
     2 vx46(128),vy17(128),vy28(128),
     3 vy35(128),vy46(128),vz17(128),vz28(128),vz35(128),vz46(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/aux43/xm(128),p(128),xmua(128),specen(128),cc(128)
c
      dimension x(3,1),v(3,1),dux(1),duy(1),duz(1),volo(1)
c
      equivalence (dux,vx1),(duy,vy1),(duz,vz1)
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
c
c     calculate velocity strain rates, spins, and deformation gradients
c
      call prtalf
c
      do 30  i=lft,llt
      dxx(i)=(px1(i)*vx1(i)+px2(i)*vx2(i)+px3(i)*vx3(i)+px4(i)*vx4(i)+
     & px5(i)*vx5(i)+px6(i)*vx6(i)+px7(i)*vx7(i)+px8(i)*vx8(i))/voln(i)
      dyy(i)=(py1(i)*vy1(i)+py2(i)*vy2(i)+py3(i)*vy3(i)+py4(i)*vy4(i)+
     & py5(i)*vy5(i)+py6(i)*vy6(i)+py7(i)*vy7(i)+py8(i)*vy8(i))/voln(i)
      dzz(i)=(pz1(i)*vz1(i)+pz2(i)*vz2(i)+pz3(i)*vz3(i)+pz4(i)*vz4(i)+
     & pz5(i)*vz5(i)+pz6(i)*vz6(i)+pz7(i)*vz7(i)+pz8(i)*vz8(i))/voln(i)
   30 dxy(i)=(py1(i)*vx1(i)+py2(i)*vx2(i)+py3(i)*vx3(i)+py4(i)*vx4(i)+
     & py5(i)*vx5(i)+py6(i)*vx6(i)+py7(i)*vx7(i)+py8(i)*vx8(i))/voln(i)
      do 40 i=lft,llt
      dxz(i)=(pz1(i)*vx1(i)+pz2(i)*vx2(i)+pz3(i)*vx3(i)+pz4(i)*vx4(i)+
     & pz5(i)*vx5(i)+pz6(i)*vx6(i)+pz7(i)*vx7(i)+pz8(i)*vx8(i))/voln(i)
      dyz(i)=(pz1(i)*vy1(i)+pz2(i)*vy2(i)+pz3(i)*vy3(i)+pz4(i)*vy4(i)+
     & pz5(i)*vy5(i)+pz6(i)*vy6(i)+pz7(i)*vy7(i)+pz8(i)*vy8(i))/voln(i)
      dyx(i)=(px1(i)*vy1(i)+px2(i)*vy2(i)+px3(i)*vy3(i)+px4(i)*vy4(i)+
     & px5(i)*vy5(i)+px6(i)*vy6(i)+px7(i)*vy7(i)+px8(i)*vy8(i))/voln(i)
      dzx(i)=(px1(i)*vz1(i)+px2(i)*vz2(i)+px3(i)*vz3(i)+px4(i)*vz4(i)+
     & px5(i)*vz5(i)+px6(i)*vz6(i)+px7(i)*vz7(i)+px8(i)*vz8(i))/voln(i)
   40 dzy(i)=(py1(i)*vz1(i)+py2(i)*vz2(i)+py3(i)*vz3(i)+py4(i)*vz4(i)+
     & py5(i)*vz5(i)+py6(i)*vz6(i)+py7(i)*vz7(i)+py8(i)*vz8(i))/voln(i)
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
      dfe(i)=voln(i)*xm(i)
      vlrho(i)=crho*volo(i)
      wzzdt(i)=dt1d2*(dyx(i)-dxy(i))
      wyydt(i)=dt1d2*(dxz(i)-dzx(i))
   50 wxxdt(i)=dt1d2*(dzy(i)-dyz(i))
c
      return
c
      end
      subroutine prtalf
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute strain-displacement matrix
c
      common/aux8/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux9/vlrho(128),vol(128)
      common/aux10/
     & px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     & py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     & pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     & vx1(128),vx2(128),vx3(128),vx4(128),
     & vx5(128),vx6(128),vx7(128),vx8(128),
     & vy1(128),vy2(128),vy3(128),vy4(128),
     & vy5(128),vy6(128),vy7(128),vy8(128),
     & vz1(128),vz2(128),vz3(128),vz4(128),
     & vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux32/
     & a45(128),a24(128),a52(128),a16(128),a31(128),a63(128),
     & a27(128),a74(128),a38(128),a81(128),a86(128),a57(128),
     & a6345(128),a5238(128),a8624(128),
     & a7416(128),a5731(128),a8127(128)
      common/aux36/lft,llt
c
      do 40 i=lft,llt
      a45(i)=z4(i)-z5(i)
      a24(i)=z2(i)-z4(i)
      a52(i)=z5(i)-z2(i)
      a16(i)=z1(i)-z6(i)
      a31(i)=z3(i)-z1(i)
      a63(i)=z6(i)-z3(i)
      a27(i)=z2(i)-z7(i)
      a74(i)=z7(i)-z4(i)
      a38(i)=z3(i)-z8(i)
      a81(i)=z8(i)-z1(i)
      a86(i)=z8(i)-z6(i)
      a57(i)=z5(i)-z7(i)
   40 continue
      do 50 i=lft,llt
      a6345(i)=a63(i)-a45(i)
      a5238(i)=a52(i)-a38(i)
      a8624(i)=a86(i)-a24(i)
      a7416(i)=a74(i)-a16(i)
      a5731(i)=a57(i)-a31(i)
      a8127(i)=a81(i)-a27(i)
   50 continue
      do 60 i=lft,llt
      px1(i)=( y2(i)*a6345(i)+y3(i)*a24(i)-y4(i)*a5238(i)+y5(i)*a8624(i)
     &      +y6(i)*a52(i)+y8(i)*a45(i))/12.
      px2(i)=( y3(i)*a7416(i)+y4(i)*a31(i)-y1(i)*a6345(i)+y6(i)*a5731(i)
     &      +y7(i)*a63(i)+y5(i)*a16(i))/12.
      px3(i)=( y4(i)*a8127(i)-y1(i)*a24(i)-y2(i)*a7416(i)-y7(i)*a8624(i)
     &      +y8(i)*a74(i)+y6(i)*a27(i))/12.
      px4(i)=( y1(i)*a5238(i)-y2(i)*a31(i)-y3(i)*a8127(i)-y8(i)*a5731(i)
     &      +y5(i)*a81(i)+y7(i)*a38(i))/12.
   60 continue
      do 70 i=lft,llt
      px5(i)=(-y8(i)*a7416(i)+y7(i)*a86(i)+y6(i)*a8127(i)-y1(i)*a8624(i)
     &      -y4(i)*a81(i)-y2(i)*a16(i))/12.
      px6(i)=(-y5(i)*a8127(i)+y8(i)*a57(i)+y7(i)*a5238(i)-y2(i)*a5731(i)
     &      -y1(i)*a52(i)-y3(i)*a27(i))/12.
      px7(i)=(-y6(i)*a5238(i)-y5(i)*a86(i)+y8(i)*a6345(i)+y3(i)*a8624(i)
     &      -y2(i)*a63(i)-y4(i)*a38(i))/12.
      px8(i)=(-y7(i)*a6345(i)-y6(i)*a57(i)+y5(i)*a7416(i)+y4(i)*a5731(i)
     &      -y3(i)*a74(i)-y1(i)*a45(i))/12.
   70 continue
      do 80 i=lft,llt
      a45(i)=x4(i)-x5(i)
      a24(i)=x2(i)-x4(i)
      a52(i)=x5(i)-x2(i)
      a16(i)=x1(i)-x6(i)
      a31(i)=x3(i)-x1(i)
      a63(i)=x6(i)-x3(i)
      a27(i)=x2(i)-x7(i)
      a74(i)=x7(i)-x4(i)
      a38(i)=x3(i)-x8(i)
      a81(i)=x8(i)-x1(i)
      a86(i)=x8(i)-x6(i)
      a57(i)=x5(i)-x7(i)
   80 continue
      do 90 i=lft,llt
      a6345(i)=a63(i)-a45(i)
      a5238(i)=a52(i)-a38(i)
      a8624(i)=a86(i)-a24(i)
      a7416(i)=a74(i)-a16(i)
      a5731(i)=a57(i)-a31(i)
      a8127(i)=a81(i)-a27(i)
   90 continue
      do 100 i=lft,llt
      py1(i)=( z2(i)*a6345(i)+z3(i)*a24(i)-z4(i)*a5238(i)+z5(i)*a8624(i)
     &      +z6(i)*a52(i)+z8(i)*a45(i))/12.
      py2(i)=( z3(i)*a7416(i)+z4(i)*a31(i)-z1(i)*a6345(i)+z6(i)*a5731(i)
     &      +z7(i)*a63(i)+z5(i)*a16(i))/12.
      py3(i)=( z4(i)*a8127(i)-z1(i)*a24(i)-z2(i)*a7416(i)-z7(i)*a8624(i)
     &      +z8(i)*a74(i)+z6(i)*a27(i))/12.
      py4(i)=( z1(i)*a5238(i)-z2(i)*a31(i)-z3(i)*a8127(i)-z8(i)*a5731(i)
     &      +z5(i)*a81(i)+z7(i)*a38(i))/12.
  100 continue
      do 110 i=lft,llt
      py5(i)=(-z8(i)*a7416(i)+z7(i)*a86(i)+z6(i)*a8127(i)-z1(i)*a8624(i)
     &      -z4(i)*a81(i)-z2(i)*a16(i))/12.
      py6(i)=(-z5(i)*a8127(i)+z8(i)*a57(i)+z7(i)*a5238(i)-z2(i)*a5731(i)
     &      -z1(i)*a52(i)-z3(i)*a27(i))/12.
      py7(i)=(-z6(i)*a5238(i)-z5(i)*a86(i)+z8(i)*a6345(i)+z3(i)*a8624(i)
     &      -z2(i)*a63(i)-z4(i)*a38(i))/12.
      py8(i)=(-z7(i)*a6345(i)-z6(i)*a57(i)+z5(i)*a7416(i)+z4(i)*a5731(i)
     &      -z3(i)*a74(i)-z1(i)*a45(i))/12.
  110 continue
      do 120 i=lft,llt
      a45(i)=y4(i)-y5(i)
      a24(i)=y2(i)-y4(i)
      a52(i)=y5(i)-y2(i)
      a16(i)=y1(i)-y6(i)
      a31(i)=y3(i)-y1(i)
      a63(i)=y6(i)-y3(i)
      a27(i)=y2(i)-y7(i)
      a74(i)=y7(i)-y4(i)
      a38(i)=y3(i)-y8(i)
      a81(i)=y8(i)-y1(i)
      a86(i)=y8(i)-y6(i)
      a57(i)=y5(i)-y7(i)
  120 continue
      do 130 i=lft,llt
      a6345(i)=a63(i)-a45(i)
      a5238(i)=a52(i)-a38(i)
      a8624(i)=a86(i)-a24(i)
      a7416(i)=a74(i)-a16(i)
      a5731(i)=a57(i)-a31(i)
      a8127(i)=a81(i)-a27(i)
  130 continue
      do 140 i=lft,llt
      pz1(i)=( x2(i)*a6345(i)+x3(i)*a24(i)-x4(i)*a5238(i)+x5(i)*a8624(i)
     &      +x6(i)*a52(i)+x8(i)*a45(i))/12.
      pz2(i)=( x3(i)*a7416(i)+x4(i)*a31(i)-x1(i)*a6345(i)+x6(i)*a5731(i)
     &      +x7(i)*a63(i)+x5(i)*a16(i))/12.
      pz3(i)=( x4(i)*a8127(i)-x1(i)*a24(i)-x2(i)*a7416(i)-x7(i)*a8624(i)
     &      +x8(i)*a74(i)+x6(i)*a27(i))/12.
      pz4(i)=( x1(i)*a5238(i)-x2(i)*a31(i)-x3(i)*a8127(i)-x8(i)*a5731(i)
     &      +x5(i)*a81(i)+x7(i)*a38(i))/12.
  140 continue
      do 150 i=lft,llt
      pz5(i)=(-x8(i)*a7416(i)+x7(i)*a86(i)+x6(i)*a8127(i)-x1(i)*a8624(i)
     &      -x4(i)*a81(i)-x2(i)*a16(i))/12.
      pz6(i)=(-x5(i)*a8127(i)+x8(i)*a57(i)+x7(i)*a5238(i)-x2(i)*a5731(i)
     &      -x1(i)*a52(i)-x3(i)*a27(i))/12.
      pz7(i)=(-x6(i)*a5238(i)-x5(i)*a86(i)+x8(i)*a6345(i)+x3(i)*a8624(i)
     &      -x2(i)*a63(i)-x4(i)*a38(i))/12.
      pz8(i)=(-x7(i)*a6345(i)-x6(i)*a57(i)+x5(i)*a7416(i)+x4(i)*a5731(i)
     &      -x3(i)*a74(i)-x1(i)*a45(i))/12.
  150 continue
c
c     volume
c
      do 160 i=lft,llt
  160 vol(i)=px1(i)*x1(i)+px2(i)*x2(i)+px3(i)*x3(i)+px4(i)*x4(i)
     &      +px5(i)*x5(i)+px6(i)*x6(i)+px7(i)*x7(i)+px8(i)*x8(i)
c
      return
c
      end
      subroutine forcef(e,iblkh)
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
     &sgv1(128),sgv2(128),sgv3(128),sgv4(128),sgv5(128),sgv6(128),
     &e11(128),e21(128),e31(128),e12(128),e22(128),e32(128),
     &e13(128),e23(128),e33(128),e14(128),e24(128),e34(128),
     &e15(128),e25(128),e35(128),e16(128),e26(128),e36(128),
     &e17(128),e27(128),e37(128),e18(128),e28(128),e38(128)
      common/aux14/
     &sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux32/
     &ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     &ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     &ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     &ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     &             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxx(128),q1(128),cx(128)
      common/aux36/lft,llt
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      dimension e(3,1),iblkh(*)
c
      do 30 i=lft,llt
      sgv1(i)=sig1(i)-qp(i)
      sgv2(i)=sig2(i)-qp(i)
   30 sgv3(i)=sig3(i)-qp(i)
c
c     stress divergence
c
      do 40 i=lft,llt
      e11(i)=sgv1(i)*px1(i)+sig4(i)*py1(i)+sig6(i)*pz1(i)
      e21(i)=sgv2(i)*py1(i)+sig4(i)*px1(i)+sig5(i)*pz1(i)
      e31(i)=sgv3(i)*pz1(i)+sig6(i)*px1(i)+sig5(i)*py1(i)
      e12(i)=sgv1(i)*px2(i)+sig4(i)*py2(i)+sig6(i)*pz2(i)
      e22(i)=sgv2(i)*py2(i)+sig4(i)*px2(i)+sig5(i)*pz2(i)
   40 e32(i)=sgv3(i)*pz2(i)+sig6(i)*px2(i)+sig5(i)*py2(i)
      do 50 i=lft,llt
      e13(i)=sgv1(i)*px3(i)+sig4(i)*py3(i)+sig6(i)*pz3(i)
      e23(i)=sgv2(i)*py3(i)+sig4(i)*px3(i)+sig5(i)*pz3(i)
      e33(i)=sgv3(i)*pz3(i)+sig6(i)*px3(i)+sig5(i)*py3(i)
      e14(i)=sgv1(i)*px4(i)+sig4(i)*py4(i)+sig6(i)*pz4(i)
      e24(i)=sgv2(i)*py4(i)+sig4(i)*px4(i)+sig5(i)*pz4(i)
   50 e34(i)=sgv3(i)*pz4(i)+sig6(i)*px4(i)+sig5(i)*py4(i)
      do 60 i=lft,llt
      e15(i)=sgv1(i)*px5(i)+sig4(i)*py5(i)+sig6(i)*pz5(i)
      e25(i)=sgv2(i)*py5(i)+sig4(i)*px5(i)+sig5(i)*pz5(i)
      e35(i)=sgv3(i)*pz5(i)+sig6(i)*px5(i)+sig5(i)*py5(i)
      e16(i)=sgv1(i)*px6(i)+sig4(i)*py6(i)+sig6(i)*pz6(i)
      e26(i)=sgv2(i)*py6(i)+sig4(i)*px6(i)+sig5(i)*pz6(i)
   60 e36(i)=sgv3(i)*pz6(i)+sig6(i)*px6(i)+sig5(i)*py6(i)
      do 70 i=lft,llt
      e17(i)=sgv1(i)*px7(i)+sig4(i)*py7(i)+sig6(i)*pz7(i)
      e27(i)=sgv2(i)*py7(i)+sig4(i)*px7(i)+sig5(i)*pz7(i)
      e37(i)=sgv3(i)*pz7(i)+sig6(i)*px7(i)+sig5(i)*py7(i)
      e18(i)=sgv1(i)*px8(i)+sig4(i)*py8(i)+sig6(i)*pz8(i)
      e28(i)=sgv2(i)*py8(i)+sig4(i)*px8(i)+sig5(i)*pz8(i)
   70 e38(i)=sgv3(i)*pz8(i)+sig6(i)*px8(i)+sig5(i)*py8(i)
c
c     sum stress divergence forces to hourglass resistance
c
      do 80 i=lft,llt
      ep11(i)=p11(i)-e11(i)
      ep21(i)=p21(i)-e21(i)
      ep31(i)=p31(i)-e31(i)
      ep12(i)=p12(i)-e12(i)
      ep22(i)=p22(i)-e22(i)
      ep32(i)=p32(i)-e32(i)
      ep13(i)=p13(i)-e13(i)
      ep23(i)=p23(i)-e23(i)
      ep33(i)=p33(i)-e33(i)
      ep14(i)=p14(i)-e14(i)
   80 ep24(i)=p24(i)-e24(i)
      do 90 i=lft,llt
      ep34(i)=p34(i)-e34(i)
      ep15(i)=p15(i)-e15(i)
      ep25(i)=p25(i)-e25(i)
      ep35(i)=p35(i)-e35(i)
      ep16(i)=p16(i)-e16(i)
      ep26(i)=p26(i)-e26(i)
      ep36(i)=p36(i)-e36(i)
      ep17(i)=p17(i)-e17(i)
      ep27(i)=p27(i)-e27(i)
      ep37(i)=p37(i)-e37(i)
      ep18(i)=p18(i)-e18(i)
      ep28(i)=p28(i)-e28(i)
   90 ep38(i)=p38(i)-e38(i)
c
c     add nodal force vector into global array
c
      do 180 n=1,nnc
      lcn=lczc+n
      i0 =iblkh(lcn)
      i1 =iblkh(lcn+1)-1
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
  130 e(3,ix4(i))=e(3,ix4(i))+ep34(i)
cdir$ ivdep
      do 140 i=i0,i1
      e(1,ix5(i))=e(1,ix5(i))+ep15(i)
      e(2,ix5(i))=e(2,ix5(i))+ep25(i)
  140 e(3,ix5(i))=e(3,ix5(i))+ep35(i)
cdir$ ivdep
      do 150 i=i0,i1
      e(1,ix6(i))=e(1,ix6(i))+ep16(i)
      e(2,ix6(i))=e(2,ix6(i))+ep26(i)
  150 e(3,ix6(i))=e(3,ix6(i))+ep36(i)
cdir$ ivdep
      do 160 i=i0,i1
      e(1,ix7(i))=e(1,ix7(i))+ep17(i)
      e(2,ix7(i))=e(2,ix7(i))+ep27(i)
  160 e(3,ix7(i))=e(3,ix7(i))+ep37(i)
cdir$ ivdep
      do 170 i=i0,i1
      e(1,ix8(i))=e(1,ix8(i))+ep18(i)
      e(2,ix8(i))=e(2,ix8(i))+ep28(i)
  170 e(3,ix8(i))=e(3,ix8(i))+ep38(i)
  180 continue
      return
      end
      subroutine cnstrs(x,xyzkcn,a,bcs,ncflag)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,1),xyzkcn(6,1),a(3,1),bcs(4,1)
      do 100 i=1,ncflag
      stif=xyzkcn(4,i)
      icod=xyzkcn(5,i)
      node=xyzkcn(6,i)
      xdif=x(1,node)-xyzkcn(1,i)
      ydif=x(2,node)-xyzkcn(2,i)
      zdif=x(3,node)-xyzkcn(3,i)
      if (icod.gt.7) go to 80
      go to (10,20,30,40,50,60,70),icod
   10 a(1,node)=a(1,node)-stif*xdif
      go to 100
   20 a(2,node)=a(2,node)-stif*ydif
      go to 100
   30 a(3,node)=a(3,node)-stif*zdif
      go to 100
   40 a(1,node)=a(1,node)-stif*xdif
      a(2,node)=a(2,node)-stif*ydif
      go to 100
   50 a(2,node)=a(2,node)-stif*ydif
      a(3,node)=a(3,node)-stif*zdif
      go to 100
   60 a(3,node)=a(3,node)-stif*zdif
      a(1,node)=a(1,node)-stif*xdif
      go to 100
   70 a(1,node)=a(1,node)-stif*xdif
      a(2,node)=a(2,node)-stif*ydif
      a(3,node)=a(3,node)-stif*zdif
      go to 100
   80 m=icod-7
      force=stif*(xdif*bcs(1,m)+ydif*bcs(2,m)+zdif*bcs(3,m))
      if (bcs(4,m).ne.0.0) go to 90
      a(1,node)=a(1,node)-force*bcs(1,m)
      a(2,node)=a(2,node)-force*bcs(2,m)
      a(3,node)=a(3,node)-force*bcs(3,m)
      go to 100
   90 a(1,node)=a(1,node)-stif*xdif+force*bcs(1,m)
      a(2,node)=a(2,node)-stif*ydif+force*bcs(2,m)
      a(3,node)=a(3,node)-stif*zdif+force*bcs(3,m)
  100 continue
      return
      end
      subroutine rbjcon(x,x2t)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/   /a(1)
      common/bk02/iburn,dt1,dt2,isdo
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/bk81/fold,scp
c
      dimension x(3,1),x2t(3,1)
c
      if (dt1.eq.0.00) then
      f=1.
      else
      f=scp/dt1**2
      endif
      ratio=f/fold
      if (abs(1.0-ratio).lt.0.01) go to 200
      fold=f
      ipnt=irdat-1
      do 100 i=1,njt
      a(ipnt+i)=ratio*a(ipnt+i)
  100 continue
c
  200 if (nl(1).ne.0) call sfcon(a(irdat),a(iidat),njt,x,x2t)
      if (nl(2).ne.0) call rfcon(a(irdat),a(iidat),njt,x,x2t)
      if (nl(3).ne.0) call cfcon(a(irdat),a(iidat),njt,x,x2t)
      if (nl(4).ne.0) call pfcon(a(irdat),a(iidat),njt,x,x2t)
      if (nl(5).ne.0) call ufcon(a(irdat),a(iidat),njt,x,x2t)
      if (nl(6).ne.0) call tfcon(a(irdat),a(iidat),njt,x,x2t)
c
      return
      end
      subroutine sfcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(1)
      iend=ll(1)
      l=lo(1)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
   50 continue
c
      do 80 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
   80 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine rfcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(2)
      iend=ll(2)
      l=lo(2)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
      xi(i,4)=x(1,idat(i+l,4))
      xi(i,5)=x(2,idat(i+l,4))
      xi(i,6)=x(3,idat(i+l,4))
      xj(i,4)=x(1,idat(i+l,5))
      xj(i,5)=x(2,idat(i+l,5))
      xj(i,6)=x(3,idat(i+l,5))
   50 continue
c
      do 80 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
      f(i,4)=rdat(i)*(xj(i,4)-xi(i,4))
      f(i,5)=rdat(i)*(xj(i,5)-xi(i,5))
      f(i,6)=rdat(i)*(xj(i,6)-xi(i,6))
   80 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
      a(1,idat(i+l,4))=a(1,idat(i+l,4))+f(i,4)
      a(2,idat(i+l,4))=a(2,idat(i+l,4))+f(i,5)
      a(3,idat(i+l,4))=a(3,idat(i+l,4))+f(i,6)
      a(1,idat(i+l,5))=a(1,idat(i+l,5))-f(i,4)
      a(2,idat(i+l,5))=a(2,idat(i+l,5))-f(i,5)
      a(3,idat(i+l,5))=a(3,idat(i+l,5))-f(i,6)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine cfcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/aux11/del(128,3)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(3)
      iend=ll(3)
      l=lo(3)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
      xi(i,4)=x(1,idat(i+l,4))
      xi(i,5)=x(2,idat(i+l,4))
      xi(i,6)=x(3,idat(i+l,4))
      xj(i,4)=x(1,idat(i+l,5))
      xj(i,5)=x(2,idat(i+l,5))
      xj(i,6)=x(3,idat(i+l,5))
   50 continue
c
      do 60 i=1,iend
      del(i,1)=xi(i,1)-xi(i,4)
      del(i,2)=xi(i,2)-xi(i,5)
      del(i,3)=xi(i,3)-xi(i,6)
   60 continue
c
      do 70 i=1,iend
      orad=1./sqrt(del(i,1)**2+del(i,2)**2+del(i,3)**2)
      del(i,1)=orad*del(i,1)
      del(i,2)=orad*del(i,2)
      del(i,3)=orad*del(i,3)
   70 continue
c
      do 80 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
      f(i,4)=rdat(i)*(xj(i,4)-xi(i,4))
      f(i,5)=rdat(i)*(xj(i,5)-xi(i,5))
      f(i,6)=rdat(i)*(xj(i,6)-xi(i,6))
   80 continue
c
      do 90 i=1,iend
      proj1=del(i,1)*f(i,1)+del(i,2)*f(i,2)+del(i,3)*f(i,3)
      f(i,1)=f(i,1)-proj1*del(i,1)
      f(i,2)=f(i,2)-proj1*del(i,2)
      f(i,3)=f(i,3)-proj1*del(i,3)
      proj2=del(i,1)*f(i,4)+del(i,2)*f(i,5)+del(i,3)*f(i,6)
      f(i,4)=f(i,4)-proj2*del(i,1)
      f(i,5)=f(i,5)-proj2*del(i,2)
      f(i,6)=f(i,6)-proj2*del(i,3)
   90 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
      a(1,idat(i+l,4))=a(1,idat(i+l,4))+f(i,4)
      a(2,idat(i+l,4))=a(2,idat(i+l,4))+f(i,5)
      a(3,idat(i+l,4))=a(3,idat(i+l,4))+f(i,6)
      a(1,idat(i+l,5))=a(1,idat(i+l,5))-f(i,4)
      a(2,idat(i+l,5))=a(2,idat(i+l,5))-f(i,5)
      a(3,idat(i+l,5))=a(3,idat(i+l,5))-f(i,6)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine pfcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/aux11/del(128,3)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(4)
      iend=ll(4)
      l=lo(4)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
      xi(i,4)=x(1,idat(i+l,4))
      xi(i,5)=x(2,idat(i+l,4))
      xi(i,6)=x(3,idat(i+l,4))
      xj(i,4)=x(1,idat(i+l,5))
      xj(i,5)=x(2,idat(i+l,5))
      xj(i,6)=x(3,idat(i+l,5))
   50 continue
c
      do 60 i=1,iend
      del(i,1)=xi(i,1)-xi(i,4)
      del(i,2)=xi(i,2)-xi(i,5)
      del(i,3)=xi(i,3)-xi(i,6)
   60 continue
c
      do 70 i=1,iend
      orad=1./sqrt(del(i,1)**2+del(i,2)**2+del(i,3)**2)
      del(i,1)=orad*del(i,1)
      del(i,2)=orad*del(i,2)
      del(i,3)=orad*del(i,3)
   70 continue
c
      do 80 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
      f(i,4)=rdat(i)*(xj(i,4)-xi(i,4))
      f(i,5)=rdat(i)*(xj(i,5)-xi(i,5))
      f(i,6)=rdat(i)*(xj(i,6)-xi(i,6))
   80 continue
c
      do 90 i=1,iend
      proj1=del(i,1)*f(i,1)+del(i,2)*f(i,2)+del(i,3)*f(i,3)
      f(i,1)=proj1*del(i,1)
      f(i,2)=proj1*del(i,2)
      f(i,3)=proj1*del(i,3)
      proj2=del(i,1)*f(i,4)+del(i,2)*f(i,5)+del(i,3)*f(i,6)
      f(i,4)=proj2*del(i,1)
      f(i,5)=proj2*del(i,2)
      f(i,6)=proj2*del(i,3)
   90 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
      a(1,idat(i+l,4))=a(1,idat(i+l,4))+f(i,4)
      a(2,idat(i+l,4))=a(2,idat(i+l,4))+f(i,5)
      a(3,idat(i+l,4))=a(3,idat(i+l,4))+f(i,6)
      a(1,idat(i+l,5))=a(1,idat(i+l,5))-f(i,4)
      a(2,idat(i+l,5))=a(2,idat(i+l,5))-f(i,5)
      a(3,idat(i+l,5))=a(3,idat(i+l,5))-f(i,6)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine ufcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/aux11/del(128,3)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(5)
      iend=ll(5)
      l=lo(5)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
      xi(i,4)=x(1,idat(i+l,4))
      xi(i,5)=x(2,idat(i+l,4))
      xi(i,6)=x(3,idat(i+l,4))
      xj(i,4)=x(1,idat(i+l,5))
      xj(i,5)=x(2,idat(i+l,5))
      xj(i,6)=x(3,idat(i+l,5))
   50 continue
c
      do 60 i=1,iend
      del(i,1)=xi(i,4)-xj(i,4)
      del(i,2)=xi(i,5)-xj(i,5)
      del(i,3)=xi(i,6)-xj(i,6)
   60 continue
c
      do 70 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
      rad1=sqrt(del(i,1)**2+del(i,2)**2+del(i,3)**2)
      rad2=sqrt((xi(i,1)-xi(i,4))**2+(xi(i,2)-xi(i,5))**2
     1         +(xi(i,3)-xi(i,6))**2+(xj(i,1)-xj(i,4))**2
     1         +(xj(i,2)-xj(i,5))**2+(xj(i,3)-xj(i,6))**2)
      fdivr=rdat(i)*(rad2-rad1)/rad1
      f(i,4)=fdivr*del(i,1)
      f(i,5)=fdivr*del(i,2)
      f(i,6)=fdivr*del(i,3)
   70 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
      a(1,idat(i+l,4))=a(1,idat(i+l,4))+f(i,4)
      a(2,idat(i+l,4))=a(2,idat(i+l,4))+f(i,5)
      a(3,idat(i+l,4))=a(3,idat(i+l,4))+f(i,6)
      a(1,idat(i+l,5))=a(1,idat(i+l,5))-f(i,4)
      a(2,idat(i+l,5))=a(2,idat(i+l,5))-f(i,5)
      a(3,idat(i+l,5))=a(3,idat(i+l,5))-f(i,6)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine tfcon(rdat,idat,kdat,x,a)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/aux10/xi(128,9),xj(128,9),f(128,9)
      common/aux11/del(128,3)
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
c
      dimension rdat(1),idat(kdat,1),x(3,1),a(3,1)
c
      jnl=nl(6)
      iend=ll(6)
      l=lo(6)
c
      do 200 j=1,jnl
c
      do 50 i=1,iend
      xi(i,1)=x(1,idat(i+l,2))
      xi(i,2)=x(2,idat(i+l,2))
      xi(i,3)=x(3,idat(i+l,2))
      xj(i,1)=x(1,idat(i+l,3))
      xj(i,2)=x(2,idat(i+l,3))
      xj(i,3)=x(3,idat(i+l,3))
      xi(i,4)=x(1,idat(i+l,4))
      xi(i,5)=x(2,idat(i+l,4))
      xi(i,6)=x(3,idat(i+l,4))
      xj(i,4)=x(1,idat(i+l,5))
      xj(i,5)=x(2,idat(i+l,5))
      xj(i,6)=x(3,idat(i+l,5))
      xi(i,7)=x(1,idat(i+l,6))
      xi(i,8)=x(2,idat(i+l,6))
      xi(i,9)=x(3,idat(i+l,6))
      xj(i,7)=x(1,idat(i+l,7))
      xj(i,8)=x(2,idat(i+l,7))
      xj(i,9)=x(3,idat(i+l,7))
   50 continue
c
      do 60 i=1,iend
      del(i,1)=xi(i,1)-xi(i,4)
      del(i,2)=xi(i,2)-xi(i,5)
      del(i,3)=xi(i,3)-xi(i,6)
   60 continue
c
      do 70 i=1,iend
      orad=1./sqrt(del(i,1)**2+del(i,2)**2+del(i,3)**2)
      del(i,1)=orad*del(i,1)
      del(i,2)=orad*del(i,2)
      del(i,3)=orad*del(i,3)
   70 continue
c
      do 80 i=1,iend
      f(i,1)=rdat(i)*(xj(i,1)-xi(i,1))
      f(i,2)=rdat(i)*(xj(i,2)-xi(i,2))
      f(i,3)=rdat(i)*(xj(i,3)-xi(i,3))
      f(i,4)=rdat(i)*(xj(i,4)-xi(i,4))
      f(i,5)=rdat(i)*(xj(i,5)-xi(i,5))
      f(i,6)=rdat(i)*(xj(i,6)-xi(i,6))
      f(i,7)=rdat(i)*(xj(i,7)-xi(i,7))
      f(i,8)=rdat(i)*(xj(i,8)-xi(i,8))
      f(i,9)=rdat(i)*(xj(i,9)-xi(i,9))
   80 continue
c
      do 90 i=1,iend
      proj1=del(i,1)*f(i,1)+del(i,2)*f(i,2)+del(i,3)*f(i,3)
      f(i,1)=f(i,1)-proj1*del(i,1)
      f(i,2)=f(i,2)-proj1*del(i,2)
      f(i,3)=f(i,3)-proj1*del(i,3)
      proj2=del(i,1)*f(i,4)+del(i,2)*f(i,5)+del(i,3)*f(i,6)
      f(i,4)=f(i,4)-proj2*del(i,1)
      f(i,5)=f(i,5)-proj2*del(i,2)
      f(i,6)=f(i,6)-proj2*del(i,3)
      proj3=del(i,1)*f(i,7)+del(i,2)*f(i,8)+del(i,3)*f(i,9)
      f(i,7)=f(i,7)-proj3*del(i,1)
      f(i,8)=f(i,8)-proj3*del(i,2)
      f(i,9)=f(i,9)-proj3*del(i,3)
   90 continue
c
      do 100 i=1,iend
      a(1,idat(i+l,2))=a(1,idat(i+l,2))+f(i,1)
      a(2,idat(i+l,2))=a(2,idat(i+l,2))+f(i,2)
      a(3,idat(i+l,2))=a(3,idat(i+l,2))+f(i,3)
      a(1,idat(i+l,3))=a(1,idat(i+l,3))-f(i,1)
      a(2,idat(i+l,3))=a(2,idat(i+l,3))-f(i,2)
      a(3,idat(i+l,3))=a(3,idat(i+l,3))-f(i,3)
      a(1,idat(i+l,4))=a(1,idat(i+l,4))+f(i,4)
      a(2,idat(i+l,4))=a(2,idat(i+l,4))+f(i,5)
      a(3,idat(i+l,4))=a(3,idat(i+l,4))+f(i,6)
      a(1,idat(i+l,5))=a(1,idat(i+l,5))-f(i,4)
      a(2,idat(i+l,5))=a(2,idat(i+l,5))-f(i,5)
      a(3,idat(i+l,5))=a(3,idat(i+l,5))-f(i,6)
      a(1,idat(i+l,6))=a(1,idat(i+l,6))+f(i,7)
      a(2,idat(i+l,6))=a(2,idat(i+l,6))+f(i,8)
      a(3,idat(i+l,6))=a(3,idat(i+l,6))+f(i,9)
      a(1,idat(i+l,7))=a(1,idat(i+l,7))-f(i,7)
      a(2,idat(i+l,7))=a(2,idat(i+l,7))-f(i,8)
      a(3,idat(i+l,7))=a(3,idat(i+l,7))-f(i,9)
  100 continue
c
      l=l+iend
      iend=128
c
  200 continue
c
      return
      end
      subroutine sumupb(rba,rbcor,x,f,xrb,yrb,zrb,fxrb,fyrb,fzrb,
     1 fx,fy,fz,n,nrb)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
c
      dimension rba(6),rbcor(3),x(3,1),f(3,1),xrb(1),yrb(1),zrb(1),
     1 fxrb(1),fyrb(1),fzrb(1),fx(1),fy(1),fz(1),nrb(1)
c
      do 10 i=1,n
      xrb(i)=x(1,nrb(i))
      yrb(i)=x(2,nrb(i))
      zrb(i)=x(3,nrb(i))
      fxrb(i)=f(1,nrb(i))
      fyrb(i)=f(2,nrb(i))
      fzrb(i)=f(3,nrb(i))
      f(1,nrb(i))=0.0
      f(2,nrb(i))=0.0
      f(3,nrb(i))=0.0
   10 continue
      do 20 i=1,n
      xrb(i)=xrb(i)-rbcor(1)
      yrb(i)=yrb(i)-rbcor(2)
      zrb(i)=zrb(i)-rbcor(3)
   20 continue
      do 30 i=1,n
      rba(1)=rba(1)+fxrb(i)
      rba(2)=rba(2)+fyrb(i)
   30 rba(3)=rba(3)+fzrb(i)
      do 40 i=1,n
      fx(i)=yrb(i)*fzrb(i)-zrb(i)*fyrb(i)
      fy(i)=zrb(i)*fxrb(i)-xrb(i)*fzrb(i)
      fz(i)=xrb(i)*fyrb(i)-yrb(i)*fxrb(i)
   40 continue
      do 50 i=1,n
      rba(4)=rba(4)+fx(i)
      rba(5)=rba(5)+fy(i)
   50 rba(6)=rba(6)+fz(i)
c
      if (numelb+numels.eq.0) return
c
      do 110 i=1,n
      fxrb(i)=f(1,nrb(i)+numnp)
      fyrb(i)=f(2,nrb(i)+numnp)
      fzrb(i)=f(3,nrb(i)+numnp)
  110 continue
      do 150 i=1,n
      rba(4)=rba(4)+fxrb(i)
      rba(5)=rba(5)+fyrb(i)
  150 rba(6)=rba(6)+fzrb(i)
c
      return
      end
      subroutine rbudat (rbi,rbm,rbcor,rbu,rbv,rbf,rbcods,dt1,dt2,bcs)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/rotat/s11,s21,s31,s12,s22,s32,s13,s23,s33
      dimension rbi(3,3),rbcor(3),rbu(6),rbv(6),rbf(6),rbcods(2),bcs(1)
c
c     inverse of inertia matrix
c
      cf5968=rbi(2,2)*rbi(3,3)-rbi(3,2)*rbi(2,3)
      cf6749=rbi(3,2)*rbi(1,3)-rbi(1,2)*rbi(3,3)
      cf4857=rbi(1,2)*rbi(2,3)-rbi(2,2)*rbi(1,3)
      deti=1./(rbi(1,1)*cf5968+rbi(2,1)*cf6749+rbi(3,1)*cf4857)
      rbi11=deti*cf5968
      rbi12=deti*cf6749
      rbi13=deti*cf4857
      rbi21=deti*(-rbi(2,1)*rbi(3,3)+rbi(3,1)*rbi(2,3))
      rbi22=deti*( rbi(1,1)*rbi(3,3)-rbi(3,1)*rbi(1,3))
      rbi23=deti*(-rbi(1,1)*rbi(2,3)+rbi(2,1)*rbi(1,3))
      rbi31=deti*( rbi(2,1)*rbi(3,2)-rbi(3,1)*rbi(2,2))
      rbi32=deti*(-rbi(1,1)*rbi(3,2)+rbi(3,1)*rbi(1,2))
      rbi33=deti*( rbi(1,1)*rbi(2,2)-rbi(2,1)*rbi(1,2))
c
c     translational and rotational accelerations
c
      rbf(4)=rbf(4)+(rbi(2,2)-rbi(3,3))*rbv(5)*rbv(6)
     1      +rbi(1,2)*rbv(6)*rbv(4)-rbi(1,3)*rbv(5)*rbv(4)-rbi(2,3)
     2      *(rbv(5)**2-rbv(6)**2)
      rbf(5)=rbf(5)+(rbi(3,3)-rbi(1,1))*rbv(6)*rbv(4)
     1      +rbi(2,3)*rbv(4)*rbv(5)-rbi(2,1)*rbv(6)*rbv(5)-rbi(3,1)
     2      *(rbv(6)**2-rbv(4)**2)
      rbf(6)=rbf(6)+(rbi(1,1)-rbi(2,2))*rbv(4)*rbv(5)
     1      +rbi(3,1)*rbv(5)*rbv(6)-rbi(3,2)*rbv(4)*rbv(6)-rbi(1,2)
     2      *(rbv(4)**2-rbv(5)**2)
      ib1=rbcods(1)
      ib2=rbcods(2)
      if (ib2.ne.0) call bound (bcs,rbf(4),rbf(5),rbf(6),ib2)
      rba4=rbi11*rbf(4)+rbi12*rbf(5)+rbi13*rbf(6)
      rba5=rbi21*rbf(4)+rbi22*rbf(5)+rbi23*rbf(6)
      rba6=rbi31*rbf(4)+rbi32*rbf(5)+rbi33*rbf(6)
      rbf(1)=rbf(1)/rbm
      rbf(2)=rbf(2)/rbm
      rbf(3)=rbf(3)/rbm
      rbf(4)=rba4
      rbf(5)=rba5
      rbf(6)=rba6
      if (ib2.ne.0) call bound (bcs,rbf(4),rbf(5),rbf(6),ib2)
      if (ib1.ne.0) call bound (bcs,rbf(1),rbf(2),rbf(3),ib1)
c
c
c     rigid body velocities
c
      dtx=.5*(dt1+dt2)
      rbv(1)=rbv(1)+dtx*rbf(1)
      rbv(2)=rbv(2)+dtx*rbf(2)
      rbv(3)=rbv(3)+dtx*rbf(3)
      rbv(4)=rbv(4)+dtx*rbf(4)
      rbv(5)=rbv(5)+dtx*rbf(5)
      rbv(6)=rbv(6)+dtx*rbf(6)
      h1=rbi(1,1)*rbv(4)+rbi(1,2)*rbv(5)+rbi(1,3)*rbv(6)
      h2=rbi(2,1)*rbv(4)+rbi(2,2)*rbv(5)+rbi(2,3)*rbv(6)
      h3=rbi(3,1)*rbv(4)+rbi(3,2)*rbv(5)+rbi(3,3)*rbv(6)
c
c     rigid body displacements and coordinates
c
      rbu(1)=rbu(1)+dt2*rbv(1)
      rbu(2)=rbu(2)+dt2*rbv(2)
      rbu(3)=rbu(3)+dt2*rbv(3)
      rbu(4)=rbu(4)+dt2*rbv(4)
      rbu(5)=rbu(5)+dt2*rbv(5)
      rbu(6)=rbu(6)+dt2*rbv(6)
      rbcor(1)=rbcor(1)+dt2*rbv(1)
      rbcor(2)=rbcor(2)+dt2*rbv(2)
      rbcor(3)=rbcor(3)+dt2*rbv(3)
c
      call rimat (s11,dt2*rbv(4),dt2*rbv(5),dt2*rbv(6))
c
      call rotaim (rbi)
c
      return
      end
      subroutine rimat(rot,wxx,wyy,wzz)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute the rotation matrix 'rot' from spin terms
c     using hughes - winget algorithm
c
      dimension rot(*)
c
      det   =2.0/(4.0+wxx*wxx+wyy*wyy+wzz*wzz)
      rot(1)=1.-det*(wyy*wyy+wzz*wzz)
      rot(2)= (2.*wzz+wxx*wyy)*det
      rot(3)=-(2.*wyy-wxx*wzz)*det
      rot(4)=-(2.*wzz-wxx*wyy)*det
      rot(5)=1.-det*(wxx*wxx+wzz*wzz)
      rot(6)= (2.*wxx+wyy*wzz)*det
      rot(7)= (2.*wyy+wxx*wzz)*det
      rot(8)=-(2.*wxx-wyy*wzz)*det
      rot(9)=1.-det*(wyy*wyy+wxx*wxx)
      return
      end
      subroutine rotaim (rbi)
c     implicit double precision (a-h,o-z)                                    dp
      common/rotat/s11,s21,s31,s12,s22,s32,s13,s23,s33
      dimension rbi(3,3)
c
      a11=rbi(1,1)*s11+rbi(1,2)*s12+rbi(1,3)*s13
      a12=rbi(1,1)*s21+rbi(1,2)*s22+rbi(1,3)*s23
      a13=rbi(1,1)*s31+rbi(1,2)*s32+rbi(1,3)*s33
      a21=rbi(2,1)*s11+rbi(2,2)*s12+rbi(2,3)*s13
      a22=rbi(2,1)*s21+rbi(2,2)*s22+rbi(2,3)*s23
      a23=rbi(2,1)*s31+rbi(2,2)*s32+rbi(2,3)*s33
      a31=rbi(3,1)*s11+rbi(3,2)*s12+rbi(3,3)*s13
      a32=rbi(3,1)*s21+rbi(3,2)*s22+rbi(3,3)*s23
      a33=rbi(3,1)*s31+rbi(3,2)*s32+rbi(3,3)*s33
      rbi(1,1)=s11*a11+s12*a21+s13*a31
      rbi(1,2)=s11*a12+s12*a22+s13*a32
      rbi(1,3)=s11*a13+s12*a23+s13*a33
      rbi(2,1)=s21*a11+s22*a21+s23*a31
      rbi(2,2)=s21*a12+s22*a22+s23*a32
      rbi(2,3)=s21*a13+s22*a23+s23*a33
      rbi(3,1)=s31*a11+s32*a21+s33*a31
      rbi(3,2)=s31*a12+s32*a22+s33*a32
      rbi(3,3)=s31*a13+s32*a23+s33*a33
      return
      end
      subroutine setvlo(rbcor,x,v,a,rbv,dt2,n,nrb,
     1 xrb,yrb,zrb,axrb,ayrb,azrb)
c     implicit double precision (a-h,o-z)                                    dp
      common/rotat/s11,s21,s31,s12,s22,s32,s13,s23,s33
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
c
c     level 1,rba,rbcor,x,f
      dimension rbcor(3),x(3,1),v(3,1),a(3,1),rbv(6),nrb(1),
     1 xrb(1),yrb(1),zrb(1),axrb(1),ayrb(1),azrb(1)
c
      call rimat (s11,dt2*rbv(4),dt2*rbv(5),dt2*rbv(6))
c
      do 10 i=1,n
      xrb(i)=x(1,nrb(i))
      yrb(i)=x(2,nrb(i))
      zrb(i)=x(3,nrb(i))
      a(1,nrb(i))=0.0
      a(2,nrb(i))=0.0
      a(3,nrb(i))=0.0
   10 continue
      rbcor1=rbcor(1)-dt2*rbv(1)
      rbcor2=rbcor(2)-dt2*rbv(2)
      rbcor3=rbcor(3)-dt2*rbv(3)
      do 20 i=1,n
      xrb(i)=xrb(i)-rbcor1
      yrb(i)=yrb(i)-rbcor2
      zrb(i)=zrb(i)-rbcor3
   20 continue
      dt2i=1./dt2
      do 30 i=1,n
      axrb(i)=rbv(1)+dt2i*(s11*xrb(i)+s12*yrb(i)+s13*zrb(i)-xrb(i))
      ayrb(i)=rbv(2)+dt2i*(s21*xrb(i)+s22*yrb(i)+s23*zrb(i)-yrb(i))
      azrb(i)=rbv(3)+dt2i*(s31*xrb(i)+s32*yrb(i)+s33*zrb(i)-zrb(i))
   30 continue
      do 40 i=1,n
      v(1,nrb(i))=axrb(i)
      v(2,nrb(i))=ayrb(i)
      v(3,nrb(i))=azrb(i)
   40 continue
c
      if (numels+numelb.eq.0) return
c
      do 110 i=1,n
      xrb(i)=x(1,nrb(i)+numnp)
      yrb(i)=x(2,nrb(i)+numnp)
      zrb(i)=x(3,nrb(i)+numnp)
      a(1,nrb(i)+numnp)=0.0
      a(2,nrb(i)+numnp)=0.0
      a(3,nrb(i)+numnp)=0.0
  110 continue
      axrbi=rbv(4)
      ayrbi=rbv(5)
      azrbi=rbv(6)
      do 140 i=1,n
      v(1,nrb(i)+numnp)=axrbi
      v(2,nrb(i)+numnp)=ayrbi
      v(3,nrb(i)+numnp)=azrbi
  140 continue
c
      return
      end

      subroutine felen
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
      common/double/iprec,ncpw,unit
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
      at(i)=areal(i)-.0001*aream(i)
      dx(i)=4.*voln(i)/sqrt(aream(i))
c  60 xioff(i)=(.50+sign(.5*unit,at(i)))
   60 xioff(i)=1.0
c
      return
c
      end
      subroutine bulkq (ibq,nhex)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nhex(*)
      real*8 head                                                       vax75
      common/bk02/iburn,dt1,dt2,isdo
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
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
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/eltspf/ietspf
      common/double/iprec,ncpw,unit
      character*4 mess                                                  vax75
      common/aux38/mess
      data zero/0.0/
      rhoi=1./rho(lft)
      b12r=b12*rho(lft)
      b2r=b2*rho(lft)
      do 30 i=lft,llt
      q1(i)=rhoi*cxx(i)
      cx(i)=sqrt(q1(i))
      qx(i)=(.5+sign(.5*unit,dd(i)))*(b2*cx(i)+b12*dx(i)*abs(dd(i)))
      dtx(i)=dx(i)/(qx(i)+sqrt(qx(i)*qx(i)+q1(i)))
   30 cxx(i)=xioff(i)*cx(i)
      do 50 i=lft,llt
      vol3rd(i)=vol(i)**.33333333333333
      ad(i)=  max(zero,dd(i))*dx(i)
   50 qp(i)=ad(i)*(b12r*ad(i)+b2r*cx(i))/df(i)
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      do 60 i=lft,llt
      dt2=  min(dtx(i),dt2)
      if (dt2.eq.dtx(i)) then
      ielmtc=128*(ndum-1)+i
      ielmtc=nhex(ielmtc)
      ityptc=1
      endif
      ielmtd=128*(ndum-1)+i
      ielmtd=nhex(ielmtd)
      write (13,90) ielmtd,dtx(i)
      write (13,100)ielmtc
   60 continue
      endif
      endif
      endif
      do 70 i=lft,llt
   70 dt2=  min(dtx(i),dt2)
      if (mess.ne.'sw2.') return
      do 80 i=lft,llt
      if (dt2.eq.dtx(i)) then
      ielmtc=128*(ndum-1)+i
      ielmtc=nhex(ielmtc)
      ityptc=1
      endif
   80 continue
      return
   90 format(' initial time step size for solid element',i6,' =',e14.5)
  100 format(5x,'solid  element #',i6,' is current controlling element')
      end
      subroutine engbrk(ener)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... kinetic energy by material for solid elements
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1 ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2 grvity,idirgv,nodspc,nspcor
      common/aux9/vlrho(128),voln(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     2 px5(128),px6(128),px7(128),px8(128),
     3 py1(128),py2(128),py3(128),py4(128),
     4 py5(128),py6(128),py7(128),py8(128),
     5 pz1(128),pz2(128),pz3(128),pz4(128),
     6 pz5(128),pz6(128),pz7(128),pz8(128),
     7 vx1(128),vx2(128),vx3(128),vx4(128),
     8 vx5(128),vx6(128),vx7(128),vx8(128),
     9 vy1(128),vy2(128),vy3(128),vy4(128),
     a vy5(128),vy6(128),vy7(128),vy8(128),
     b vz1(128),vz2(128),vz3(128),vz4(128),
     c vz5(128),vz6(128),vz7(128),vz8(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1 ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux36/lft,llt
      dimension ener(*)
c
      mxm=nmmat+mxt(lft)
      mxg=2*nmmat+3*(mxt(lft)-1)+1
      mxh=2*nmmat+3*(mxt(lft)-1)+2
      mxi=2*nmmat+3*(mxt(lft)-1)+3
      emxm=ener(mxm)
      emxg=ener(mxg)
      emxh=ener(mxh)
      emxi=ener(mxi)
      do 10 i=lft,llt
      xm=vx1(i)+vx2(i)+vx3(i)+vx4(i)+vx5(i)+vx6(i)+vx7(i)+vx8(i)
      ym=vy1(i)+vy2(i)+vy3(i)+vy4(i)+vy5(i)+vy6(i)+vy7(i)+vy8(i)
      zm=vz1(i)+vz2(i)+vz3(i)+vz4(i)+vz5(i)+vz6(i)+vz7(i)+vz8(i)
c
      vx=vx1(i)**2+vx2(i)**2+vx3(i)**2+vx4(i)**2
     1  +vx5(i)**2+vx6(i)**2+vx7(i)**2+vx8(i)**2
      vy=vy1(i)**2+vy2(i)**2+vy3(i)**2+vy4(i)**2
     1  +vy5(i)**2+vy6(i)**2+vy7(i)**2+vy8(i)**2
      vz=vz1(i)**2+vz2(i)**2+vz3(i)**2+vz4(i)**2
     1  +vz5(i)**2+vz6(i)**2+vz7(i)**2+vz8(i)**2
      vlrho2 = 2.*vlrho(i)
c
      emxm=emxm+vlrho(i)*(vx+vy+vz)
      emxg=emxg+vlrho2*xm
      emxh=emxh+vlrho2*ym
      emxi=emxi+vlrho2*zm
   10 continue
      ener(mxm)=emxm
      ener(mxg)=emxg
      ener(mxh)=emxh
      ener(mxi)=emxi
      return
      end
      subroutine force(e,iblkh)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux9/vlrho(128),vol(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 p11(128),p12(128),p13(128),p14(128),
     5 p15(128),p16(128),p17(128),p18(128),
     6 p21(128),p22(128),p23(128),p24(128),
     7 p25(128),p26(128),p27(128),p28(128),
     8 p31(128),p32(128),p33(128),p34(128),
     9 p35(128),p36(128),p37(128),p38(128)
      common/aux11/po(128),
     1sgv1(128),sgv2(128),sgv3(128),sgv4(128),sgv5(128),sgv6(128),
     2e11(128),e21(128),e31(128),e12(128),e22(128),e32(128),
     3e13(128),e23(128),e33(128),e14(128),e24(128),e34(128)
      common/aux14/
     1sig1(128),sig2(128),sig3(128),sig4(128),sig5(128),sig6(128)
      common/aux15/qp(128),specen(128),dvol(128),volold(128)
      common/aux32/
     1ep11(128),ep21(128),ep31(128),ep17(128),ep27(128),ep37(128),
     2ep12(128),ep22(128),ep32(128),ep18(128),ep28(128),ep38(128),
     3ep13(128),ep23(128),ep33(128),ep15(128),ep25(128),ep35(128),
     4ep14(128),ep24(128),ep34(128),ep16(128),ep26(128),ep36(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rho(128),cxx(128),q1(128),cx(128)
      common/aux36/lft,llt
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      dimension e(3,1),iblkh(*)
c
      do 10 i=lft,llt
      sgv1(i)=(sig1(i)-qp(i))*vol(i)
      sgv2(i)=(sig2(i)-qp(i))*vol(i)
      sgv3(i)=(sig3(i)-qp(i))*vol(i)
      sgv4(i)=sig4(i)*vol(i)
      sgv5(i)=sig5(i)*vol(i)
   10 sgv6(i)=sig6(i)*vol(i)
c
      do 20 i=lft,llt
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
   20 e34(i)=sgv3(i)*pz4(i)+sgv6(i)*px4(i)+sgv5(i)*py4(i)
c
      do 30 i=lft,llt
      ep11(i)=p11(i)-e11(i)
      ep21(i)=p21(i)-e21(i)
      ep31(i)=p31(i)-e31(i)
      ep17(i)=p17(i)+e11(i)
      ep27(i)=p27(i)+e21(i)
      ep37(i)=p37(i)+e31(i)
      ep12(i)=p12(i)-e12(i)
      ep22(i)=p22(i)-e22(i)
      ep32(i)=p32(i)-e32(i)
      ep18(i)=p18(i)+e12(i)
      ep28(i)=p28(i)+e22(i)
   30 ep38(i)=p38(i)+e32(i)
      do 40 i=lft,llt
      ep13(i)=p13(i)-e13(i)
      ep23(i)=p23(i)-e23(i)
      ep33(i)=p33(i)-e33(i)
      ep15(i)=p15(i)+e13(i)
      ep25(i)=p25(i)+e23(i)
      ep35(i)=p35(i)+e33(i)
      ep14(i)=p14(i)-e14(i)
      ep24(i)=p24(i)-e24(i)
      ep34(i)=p34(i)-e34(i)
      ep16(i)=p16(i)+e14(i)
      ep26(i)=p26(i)+e24(i)
   40 ep36(i)=p36(i)+e34(i)
      do 60 n=1,nnc
      lcn=lczc+n
      i0 =iblkh(lcn)
      i1 =iblkh(lcn+1)-1
cdir$ ivdep
      do 50 i=i0,i1
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
   50 e(3,ix4(i))=e(3,ix4(i))+ep34(i)
cdir$ ivdep
      do 57 i=i0,i1
      e(1,ix7(i))=e(1,ix7(i))+ep17(i)
      e(2,ix7(i))=e(2,ix7(i))+ep27(i)
   57 e(3,ix7(i))=e(3,ix7(i))+ep37(i)
cdir$ ivdep
      do 58 i=i0,i1
      e(1,ix8(i))=e(1,ix8(i))+ep18(i)
      e(2,ix8(i))=e(2,ix8(i))+ep28(i)
   58 e(3,ix8(i))=e(3,ix8(i))+ep38(i)
cdir$ ivdep
      do 55 i=i0,i1
      e(1,ix5(i))=e(1,ix5(i))+ep15(i)
      e(2,ix5(i))=e(2,ix5(i))+ep25(i)
   55 e(3,ix5(i))=e(3,ix5(i))+ep35(i)
cdir$ ivdep
      do 56 i=i0,i1
      e(1,ix6(i))=e(1,ix6(i))+ep16(i)
      e(2,ix6(i))=e(2,ix6(i))+ep26(i)
   56 e(3,ix6(i))=e(3,ix6(i))+ep36(i)
   60 continue
      return
      end
      subroutine hrgmd
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk12/b12,b2,caq
      common/aux9/vlrho(128),vol(128)
      common/aux10/
     1 px1(128),px2(128),px3(128),px4(128),
     & px5(128),px6(128),px7(128),px8(128),
     2 py1(128),py2(128),py3(128),py4(128),
     & py5(128),py6(128),py7(128),py8(128),
     3 pz1(128),pz2(128),pz3(128),pz4(128),
     & pz5(128),pz6(128),pz7(128),pz8(128),
     4 p11(128),p12(128),p13(128),p14(128),
     5 p15(128),p16(128),p17(128),p18(128),
     6 p21(128),p22(128),p23(128),p24(128),
     7 p25(128),p26(128),p27(128),p28(128),
     8 p31(128),p32(128),p33(128),p34(128),
     9 p35(128),p36(128),p37(128),p38(128)
      common/aux18/fac(128),df(128)
      common/aux32/
     &hxap(128),hxam(128),hxbp(128),hxbm(128),hyap(128),
     &hyam(128),hybp(128),hybm(128),hzap(128),hzam(128),hzbp(128),
     &hzbm(128),hx1(128),hx2(128),hx3(128),
     &hx4(128),hy1(128),hy2(128),
     &hy3(128),hy4(128),hz1(128),hz2(128),hz3(128),hz4(128)
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
      do 20 i=lft,llt
      fac(i)=caqp25*vol3rd(i)*vol3rd(i)/df(i)
      fcl(i)=fac(i)*cxx(i)
   20 fcq(i)=fac(i)*caq100
c
      call hgx
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
      do 35 i=lft,llt
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
   35 p24(i)=hyap(i)+hybm(i)
      do 40 i=lft,llt
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
      subroutine hgx
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
     &hgz4(128),hx1(128),hx2(128),hx3(128),
     &hx4(128),hy1(128),hy2(128),
     &hy3(128),hy4(128),hz1(128),hz2(128),hz3(128),hz4(128)
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
   30 hy2(i)=hgy2(i)*(fcl(i)+abs(hgy2(i))*fcq(i))
      do 40 i=lft,llt
      hy3(i)=hgy3(i)*(fcl(i)+abs(hgy3(i))*fcq(i))
      hy4(i)=hgy4(i)*(fcl(i)+abs(hgy4(i))*fcq(i))
      hz1(i)=hgz1(i)*(fcl(i)+abs(hgz1(i))*fcq(i))
      hz2(i)=hgz2(i)*(fcl(i)+abs(hgz2(i))*fcq(i))
      hz3(i)=hgz3(i)*(fcl(i)+abs(hgz3(i))*fcq(i))
   40 hz4(i)=hgz4(i)*(fcl(i)+abs(hgz4(i))*fcq(i))
      return
      end
      subroutine hrgfb (hgforc,ihg)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,caq
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
      common/aux14/aux(128,6),
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
     &          z3478(1),z2358(1),z1467(1),z1256(1),
     &          p11(1)  ,p12(1)  ,p13(1)  ,p14(1)  ,
     &          p15(1)  ,p16(1)  ,p17(1)  ,p18(1)  ,
     &          p21(1)  ,p22(1)  ,p23(1)  ,p24(1)  ,
     &          p25(1)  ,p26(1)  ,p27(1)  ,p28(1)  ,
     &          p31(1)  ,p32(1)  ,p33(1)  ,p34(1)  ,
     &          p35(1)  ,p36(1)  ,p37(1)  ,p38(1)
      dimension hgforc(12,1)
c
      equivalence (hx1,x3478),(hx2,x2358),(hx3,x1467),(hx4,x1256),
     &            (hy1,y3478),(hy2,y2358),(hy3,y1467),(hy4,y1256),
     &            (hz1,z3478),(hz2,z2358),(hz3,z1467),(hz4,z1256),
     &            (vx1,p11)  ,(vx2,p12)  ,(vx3,p13)  ,(vx4,p14)  ,
     &            (vx5,p15)  ,(vx6,p16)  ,(vx7,p17)  ,(vx8,p18)  ,
     &            (vy1,p21)  ,(vy2,p22)  ,(vy3,p23)  ,(vy4,p24)  ,
     &            (vy5,p25)  ,(vy6,p26)  ,(vy7,p27)  ,(vy8,p28)  ,
     &            (vz1,p31)  ,(vz2,p32)  ,(vz3,p33)  ,(vz4,p34)  ,
     &            (vz5,p35)  ,(vz6,p36)  ,(vz7,p37)  ,(vz8,p38)
      data lvct/128/
c
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
      if (ihg.ne.4) then
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
      else
      caqp25=-.050*caq*dt1*rho(lft)
      do 84 i=lft,llt
      factor=px1(i)**2+px2(i)**2+px3(i)**2+px4(i)**2
     1      +py1(i)**2+py2(i)**2+py3(i)**2+py4(i)**2
     2      +pz1(i)**2+pz2(i)**2+pz3(i)**2+pz4(i)**2
   84 fac(i)=caqp25*factor*cxx(i)*cxx(i)*vol(i)
      do 85 i=lft,llt
      hgforc(1,i) =hgforc(1,i) +fac(i)*hx1(i)
      hgforc(2,i) =hgforc(2,i) +fac(i)*hx2(i)
      hgforc(3,i) =hgforc(3,i) +fac(i)*hx3(i)
      hgforc(4,i) =hgforc(4,i) +fac(i)*hx4(i)
      hgforc(5,i) =hgforc(5,i) +fac(i)*hy1(i)
      hgforc(6,i) =hgforc(6,i) +fac(i)*hy2(i)
      hgforc(7,i) =hgforc(7,i) +fac(i)*hy3(i)
      hgforc(8,i) =hgforc(8,i) +fac(i)*hy4(i)
      hgforc(9,i) =hgforc(9,i) +fac(i)*hz1(i)
      hgforc(10,i)=hgforc(10,i)+fac(i)*hz2(i)
      hgforc(11,i)=hgforc(11,i)+fac(i)*hz3(i)
      hgforc(12,i)=hgforc(12,i)+fac(i)*hz4(i)
      hx1(i)=hgforc(1,i)
      hx2(i)=hgforc(2,i)
      hx3(i)=hgforc(3,i)
      hx4(i)=hgforc(4,i)
      hy1(i)=hgforc(5,i)
      hy2(i)=hgforc(6,i)
      hy3(i)=hgforc(7,i)
      hy4(i)=hgforc(8,i)
      hz1(i)=hgforc(9,i)
      hz2(i)=hgforc(10,i)
      hz3(i)=hgforc(11,i)
      hz4(i)=hgforc(12,i)
   85 continue
      endif
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
      subroutine hrgffb (hgforc,ihg)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/bk12/b12,b2,caq
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
      common/aux14/aux(128,6),
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
     &          z3478(1),z2358(1),z1467(1),z1256(1),
     &          p11(1)  ,p12(1)  ,p13(1)  ,p14(1)  ,
     &          p15(1)  ,p16(1)  ,p17(1)  ,p18(1)  ,
     &          p21(1)  ,p22(1)  ,p23(1)  ,p24(1)  ,
     &          p25(1)  ,p26(1)  ,p27(1)  ,p28(1)  ,
     &          p31(1)  ,p32(1)  ,p33(1)  ,p34(1)  ,
     &          p35(1)  ,p36(1)  ,p37(1)  ,p38(1)
c
      equivalence (hx1,x3478),(hx2,x2358),(hx3,x1467),(hx4,x1256),
     &            (hy1,y3478),(hy2,y2358),(hy3,y1467),(hy4,y1256),
     &            (hz1,z3478),(hz2,z2358),(hz3,z1467),(hz4,z1256),
     &            (vx1,p11)  ,(vx2,p12)  ,(vx3,p13)  ,(vx4,p14)  ,
     &            (vx5,p15)  ,(vx6,p16)  ,(vx7,p17)  ,(vx8,p18)  ,
     &            (vy1,p21)  ,(vy2,p22)  ,(vy3,p23)  ,(vy4,p24)  ,
     &            (vy5,p25)  ,(vy6,p26)  ,(vy7,p27)  ,(vy8,p28)  ,
     &            (vz1,p31)  ,(vz2,p32)  ,(vz3,p33)  ,(vz4,p34)  ,
     &            (vz5,p35)  ,(vz6,p36)  ,(vz7,p37)  ,(vz8,p38)
      dimension hgforc(12,1)
      data lvct/128/
c
c
c     if hourglass coefficient equals zero, zero p
c
      if(caq.eq.0) go to 120
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
      hgx1(i)=(x1467(i)-x2358(i))/vol(i)
      hgx2(i)=(x1467(i)+x2358(i))/vol(i)
      hgx3(i)=(x1256(i)-x3478(i))/vol(i)
      hgx4(i)=(x1256(i)+x3478(i))/vol(i)
      hgy1(i)=(y1467(i)-y2358(i))/vol(i)
      hgy2(i)=(y1467(i)+y2358(i))/vol(i)
      hgy3(i)=(y1256(i)-y3478(i))/vol(i)
      hgy4(i)=(y1256(i)+y3478(i))/vol(i)
      hgz1(i)=(z1467(i)-z2358(i))/vol(i)
      hgz2(i)=(z1467(i)+z2358(i))/vol(i)
      hgz3(i)=(z1256(i)-z3478(i))/vol(i)
   20 hgz4(i)=(z1256(i)+z3478(i))/vol(i)
      do 30 i=lft,llt
      sv11(i)= 1.-hgx1(i)*px1(i)-hgy1(i)*py1(i)-hgz1(i)*pz1(i)
      sv21(i)=-1.-hgx1(i)*px2(i)-hgy1(i)*py2(i)-hgz1(i)*pz2(i)
      sv31(i)= 1.-hgx1(i)*px3(i)-hgy1(i)*py3(i)-hgz1(i)*pz3(i)
      sv41(i)=-1.-hgx1(i)*px4(i)-hgy1(i)*py4(i)-hgz1(i)*pz4(i)
      sv51(i)= 1.-hgx1(i)*px5(i)-hgy1(i)*py5(i)-hgz1(i)*pz5(i)
      sv61(i)=-1.-hgx1(i)*px6(i)-hgy1(i)*py6(i)-hgz1(i)*pz6(i)
      sv71(i)= 1.-hgx1(i)*px7(i)-hgy1(i)*py7(i)-hgz1(i)*pz7(i)
   30 sv81(i)=-1.-hgx1(i)*px8(i)-hgy1(i)*py8(i)-hgz1(i)*pz8(i)
      do 32 i=lft,llt
      sv12(i)= 1.-hgx2(i)*px1(i)-hgy2(i)*py1(i)-hgz2(i)*pz1(i)
      sv22(i)= 1.-hgx2(i)*px2(i)-hgy2(i)*py2(i)-hgz2(i)*pz2(i)
      sv32(i)=-1.-hgx2(i)*px3(i)-hgy2(i)*py3(i)-hgz2(i)*pz3(i)
      sv42(i)=-1.-hgx2(i)*px4(i)-hgy2(i)*py4(i)-hgz2(i)*pz4(i)
      sv52(i)=-1.-hgx2(i)*px5(i)-hgy2(i)*py5(i)-hgz2(i)*pz5(i)
      sv62(i)=-1.-hgx2(i)*px6(i)-hgy2(i)*py6(i)-hgz2(i)*pz6(i)
      sv72(i)= 1.-hgx2(i)*px7(i)-hgy2(i)*py7(i)-hgz2(i)*pz7(i)
   32 sv82(i)= 1.-hgx2(i)*px8(i)-hgy2(i)*py8(i)-hgz2(i)*pz8(i)
      do 34 i=lft,llt
      sv13(i)= 1.-hgx3(i)*px1(i)-hgy3(i)*py1(i)-hgz3(i)*pz1(i)
      sv23(i)=-1.-hgx3(i)*px2(i)-hgy3(i)*py2(i)-hgz3(i)*pz2(i)
      sv33(i)=-1.-hgx3(i)*px3(i)-hgy3(i)*py3(i)-hgz3(i)*pz3(i)
      sv43(i)= 1.-hgx3(i)*px4(i)-hgy3(i)*py4(i)-hgz3(i)*pz4(i)
      sv53(i)=-1.-hgx3(i)*px5(i)-hgy3(i)*py5(i)-hgz3(i)*pz5(i)
      sv63(i)= 1.-hgx3(i)*px6(i)-hgy3(i)*py6(i)-hgz3(i)*pz6(i)
      sv73(i)= 1.-hgx3(i)*px7(i)-hgy3(i)*py7(i)-hgz3(i)*pz7(i)
   34 sv83(i)=-1.-hgx3(i)*px8(i)-hgy3(i)*py8(i)-hgz3(i)*pz8(i)
      do 36 i=lft,llt
      sv14(i)= 1.-hgx4(i)*px1(i)-hgy4(i)*py1(i)-hgz4(i)*pz1(i)
      sv24(i)=-1.-hgx4(i)*px2(i)-hgy4(i)*py2(i)-hgz4(i)*pz2(i)
      sv34(i)= 1.-hgx4(i)*px3(i)-hgy4(i)*py3(i)-hgz4(i)*pz3(i)
      sv44(i)=-1.-hgx4(i)*px4(i)-hgy4(i)*py4(i)-hgz4(i)*pz4(i)
      sv54(i)=-1.-hgx4(i)*px5(i)-hgy4(i)*py5(i)-hgz4(i)*pz5(i)
      sv64(i)= 1.-hgx4(i)*px6(i)-hgy4(i)*py6(i)-hgz4(i)*pz6(i)
      sv74(i)=-1.-hgx4(i)*px7(i)-hgy4(i)*py7(i)-hgz4(i)*pz7(i)
   36 sv84(i)= 1.-hgx4(i)*px8(i)-hgy4(i)*py8(i)-hgz4(i)*pz8(i)
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
      if (ihg.lt.5) then
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
      else
      caqp25=-.025*caq*dt1*rho(lft)
      do 84 i=lft,llt
      factor=px1(i)**2+px2(i)**2+px3(i)**2+px4(i)**2
     1      +py1(i)**2+py2(i)**2+py3(i)**2+py4(i)**2
     2      +pz1(i)**2+pz2(i)**2+pz3(i)**2+pz4(i)**2
     3      +px5(i)**2+px6(i)**2+px7(i)**2+px8(i)**2
     4      +py5(i)**2+py6(i)**2+py7(i)**2+py8(i)**2
     5      +pz5(i)**2+pz6(i)**2+pz7(i)**2+pz8(i)**2
   84 fac(i)=caqp25*factor*cxx(i)*cxx(i)/vol(i)
      do 85 i=lft,llt
      hgforc(1,i) =hgforc(1,i) +fac(i)*hx1(i)
      hgforc(2,i) =hgforc(2,i) +fac(i)*hx2(i)
      hgforc(3,i) =hgforc(3,i) +fac(i)*hx3(i)
      hgforc(4,i) =hgforc(4,i) +fac(i)*hx4(i)
      hgforc(5,i) =hgforc(5,i) +fac(i)*hy1(i)
      hgforc(6,i) =hgforc(6,i) +fac(i)*hy2(i)
      hgforc(7,i) =hgforc(7,i) +fac(i)*hy3(i)
      hgforc(8,i) =hgforc(8,i) +fac(i)*hy4(i)
      hgforc(9,i) =hgforc(9,i) +fac(i)*hz1(i)
      hgforc(10,i)=hgforc(10,i)+fac(i)*hz2(i)
      hgforc(11,i)=hgforc(11,i)+fac(i)*hz3(i)
      hgforc(12,i)=hgforc(12,i)+fac(i)*hz4(i)
      hx1(i)=hgforc(1,i)
      hx2(i)=hgforc(2,i)
      hx3(i)=hgforc(3,i)
      hx4(i)=hgforc(4,i)
      hy1(i)=hgforc(5,i)
      hy2(i)=hgforc(6,i)
      hy3(i)=hgforc(7,i)
      hy4(i)=hgforc(8,i)
      hz1(i)=hgforc(9,i)
      hz2(i)=hgforc(10,i)
      hz3(i)=hgforc(11,i)
      hz4(i)=hgforc(12,i)
   85 continue
      endif
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
      subroutine rstrss
c     implicit double precision (a-h,o-z)                                    dp
c
c     rotate stresses
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     2 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux14/
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128)
      common/aux36/lft,llt
      data       third/0.333333333333333/
c
      do 10 i=lft,llt
      s1(i)=t1(i)
      s2(i)=t2(i)
      s3(i)=t3(i)
      s4(i)=t4(i)
      s5(i)=t5(i)
      s6(i)=t6(i)
   10 po(i)=-third*(s1(i)+s2(i)+s3(i))
      do 20 i=lft,llt
      q1(i)=2.*s4(i)*wzz(i)
      q2(i)=2.*s6(i)*wyy(i)
   20 q3(i)=2.*s5(i)*wxx(i)
      do 30 i=lft,llt
      t1(i)=s1(i)-q1(i)+q2(i)
      t2(i)=s2(i)+q1(i)-q3(i)
      t3(i)=s3(i)-q2(i)+q3(i)
      t4(i)=s4(i)+wzz(i)*(s1(i)-s2(i))+wyy(i)*s5(i)-wxx(i)*s6(i)
      t5(i)=s5(i)+wxx(i)*(s2(i)-s3(i))+wzz(i)*s6(i)-wyy(i)*s4(i)
   30 t6(i)=s6(i)+wyy(i)*(s3(i)-s1(i))+wxx(i)*s4(i)-wzz(i)*s5(i)
c
      return
      end
      subroutine rstrn
c     implicit double precision (a-h,o-z)                                    dp
c
c     rotate stresses
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     2 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux14/
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),t7(128),
     2 epx1(128),epx2(128),epx4(128),epx5(128),epx6(128)
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
      subroutine rstrn2
c     implicit double precision (a-h,o-z)                                    dp
c
c     rotate stresses
c
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzz(128),wyy(128),wxx(128)
      common/aux11/po(128),
     1 s1(128),s2(128),s3(128),s4(128),s5(128),s6(128),s7(128,2),
     2 q1(128),q2(128),q3(128),q4(128),q5(128),q6(128),q7(128,2)
      common/aux14/
     1 t1(128),t2(128),t3(128),t4(128),t5(128),t6(128),
     2 epx1(128),epx2(128),epx3(128),epx4(128),epx5(128),epx6(128)
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
      subroutine f3dm1 (cm,bqs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     isotropic elastic material
c
      common/bk02/iburn,dt1,dt2,isdo
      common/aux2/d1(128),d2(128),d3(128),d4(128),d5(128),d6(128),
     1 wzzdt(128),wyydt(128),wxxdt(128),einc(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx(128),davg(128)
      common/aux18/dd(128),dfe(128)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cb(128),p(128)
      common/aux36/lft,llt
      dimension  cm(*),bqs(*)
      data third/.3333333333333/
c
      mx=48*(mxt(lft)-1)
      ym=cm(mx+1)
      pr=cm(mx+6)
      ss=cm(mx+2)
      g=ym/(1.+pr)
      gdt=dt1*g
      gd2=.5*gdt
      blk=-dt1*ym/((1.-2.*pr))
      do 10 i=lft,llt
      cb(i)=ss
      davg(i)=third*dd(i)
      p(i)=blk*davg(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i)+dd(i)*bqs(i))*dt1
   10 continue
      do 20 i=lft,llt
      sig1(i)=sig1(i)+p(i)+gdt*(d1(i)+davg(i))
      sig2(i)=sig2(i)+p(i)+gdt*(d2(i)+davg(i))
      sig3(i)=sig3(i)+p(i)+gdt*(d3(i)+davg(i))
      sig4(i)=sig4(i)+gd2*d4(i)
      sig5(i)=sig5(i)+gd2*d5(i)
      sig6(i)=sig6(i)+gd2*d6(i)
      einc(i)=(d1(i)*sig1(i)+d2(i)*sig2(i)+d3(i)*sig3(i)+d4(i)*sig4(i)
     1        +d5(i)*sig5(i)+d6(i)*sig6(i))*dt1+einc(i)
   20 continue
c
      return
      end
