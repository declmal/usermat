c
c $Id: dynrfn_user.F 85679 2013-11-26 21:37:43Z brian $
c
      subroutine alerfn_criteria5(test,valuelimit,
     .                            iel,nmmg,lstmmg,gvp,nalegp,
     .                            v,x,ixh,nwcon,
     .                            auxvc,nmcx,lochvh)
      dimension lstmmg(*),gvp(nalegp,*),auxvc(*),nmcx(*),
     .          lochvh(*),v(3,*),x(3,*),ixh(nwcon,*)
      real*8 x
      logical test
      dimension ioppcorner(2,4)
      data ioppcorner/1,7,
     .                2,8,
     .                3,5,
     .                4,6/
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : rot(v)
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=0.0
      volfrctot=0.0
      do immg=1,iabs(nmmg)
        ici=lstmmg(immg)
        l44=l44+nmcx(ici)
        pres=pres-gvp(ici,iel)*
     .          (auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
        volfrctot=volfrctot+gvp(ici,iel)
      enddo
      if (volfrctot.ne.0.0) then
        pres=pres/volfrctot
        test=pres.ge.valuelimit
      endif
c     
      elseif (icrteria_example.eq.2) then
c     
      rotmax=0.0
      do idiag=1,4
        ic1=ioppcorner(1,idiag)+1
        ic2=ioppcorner(2,idiag)+1
        nd1=ixh(ic1,iel)
        nd2=ixh(ic2,iel)
        dvx=v(1,nd1)-v(1,nd2)
        dvy=v(2,nd1)-v(2,nd2)
        dvz=v(3,nd1)-v(3,nd2)
        dx=x(1,nd1)-x(1,nd2)
        dy=x(2,nd1)-x(2,nd2)
        dz=x(3,nd1)-x(3,nd2)
        rotx=dvz/dy-dvy/dz
        roty=dvx/dz-dvz/dx
        rotz=dvy/dx-dvx/dy
        rot=rotx*rotx+roty*roty+rotz*rotz
        rot=sqrt(rot)
        rotmax=max(rot,rotmax)
      enddo
      test=rotmax.ge.valuelimit
c      
      endif
c
      return
      end
c
      subroutine alermv_criteria5(test,valuelimit,
     .                            iel,nmmg,lstmmg,gvp,nalegp,
     .                            v,x,ixh,nwcon,
     .                            auxvc,nmcx,lochvh)
      dimension lstmmg(*),gvp(nalegp,*),auxvc(*),nmcx(*),
     .          lochvh(*),v(3,*),x(3,*),ixh(nwcon,*)
      real*8 x
      logical test
      dimension ioppcorner(2,4)
      data ioppcorner/1,7,
     .                2,8,
     .                3,5,
     .                4,6/
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : rot(v)
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=0.0
      volfrctot=0.0
      do immg=1,iabs(nmmg)
        ici=lstmmg(immg)
        l44=l44+nmcx(ici)
        pres=pres-gvp(ici,iel)*
     .          (auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
        volfrctot=volfrctot+gvp(ici,iel)
      enddo
      if (volfrctot.ne.0.0) then
        pres=pres/volfrctot
        test=pres.lt.valuelimit
      endif
c
      elseif (icrteria_example.eq.2) then
c     
      rotmax=0.0
      do idiag=1,4
        ic1=ioppcorner(1,idiag)+1
        ic2=ioppcorner(2,idiag)+1
        nd1=ixh(ic1,iel)
        nd2=ixh(ic2,iel)
        dvx=v(1,nd1)-v(1,nd2)
        dvy=v(2,nd1)-v(2,nd2)
        dvz=v(3,nd1)-v(3,nd2)
        dx=x(1,nd1)-x(1,nd2)
        dy=x(2,nd1)-x(2,nd2)
        dz=x(3,nd1)-x(3,nd2)
        rotx=dvz/dy-dvy/dz
        roty=dvx/dz-dvz/dx
        rotz=dvy/dx-dvx/dy
        rot=rotx*rotx+roty*roty+rotz*rotz
        rot=sqrt(rot)
        rotmax=max(rot,rotmax)
      enddo
      test=rotmax.lt.valuelimit
c      
      endif
c
      return
      end
c
      subroutine sldrfn_criteria5(test,valuelimit,
     .                            iel,v,x,ixh,nwcon,
     .                            auxvc,lochvh)
      dimension auxvc(*),lochvh(*),v(3,*),x(3,*),ixh(nwcon,*)
      real*8 x
      logical test
      dimension isten(4,8)
      data isten/2,4,5,7,
     .           1,3,6,8,
     .           4,2,7,5,
     .           3,1,8,6,
     .           6,8,1,3,
     .           5,7,2,4,
     .           8,6,3,1,
     .           7,5,4,2/
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : element shape ratio
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=(auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
      test=pres.ge.valuelimit
c     
      elseif (icrteria_example.eq.2) then
c     
      distratiomax=0
      do i=1,8,2
        if (i.ge.5) then
          k=i+1
        else
          k=i
        endif
        nd1=ixh(k+1,iel)
        distratioref=0
        do idir=1,3
          j=isten(idir,k)
          nd2=ixh(j+1,iel)
          dx=x(1,nd1)-x(1,nd2)
          dy=x(2,nd1)-x(2,nd2)
          dz=x(3,nd1)-x(3,nd2)
          distratio=sqrt(dx*dx+dy*dy+dz*dz)
          if (distratio.ne.0.0.and.distratioref.eq.0.0) then
            distratioref=distratio
          endif
          if (distratioref.ne.0) then
            if (distratio.ge.distratioref) then
              distratio=distratio/distratioref
            elseif (distratio.ne.0.0) then
              distratio=distratioref/distratio
            endif
            distratiomax=max(distratio,distratiomax)
          endif 
        enddo
      enddo
      test=distratiomax.ge.valuelimit
c      
      endif
c
      return
      end
c
      subroutine sldrmv_criteria5(test,valuelimit,
     .                            iel,v,x,ixh,nwcon,
     .                            auxvc,lochvh)
      dimension auxvc(*),lochvh(*),v(3,*),x(3,*),ixh(nwcon,*)
      real*8 x
      logical test
      dimension isten(4,8)
      data isten/2,4,5,7,
     .           1,3,6,8,
     .           4,2,7,5,
     .           3,1,8,6,
     .           6,8,1,3,
     .           5,7,2,4,
     .           8,6,3,1,
     .           7,5,4,2/
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : element shape ratio
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=(auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
      test=pres.lt.valuelimit
c
      elseif (icrteria_example.eq.2) then
c     
      distratiomax=0
      do i=1,8,2
        if (i.ge.5) then
          k=i+1
        else
          k=i
        endif
        nd1=ixh(k+1,iel)
        distratioref=0
        do idir=1,3
          j=isten(idir,k)
          nd2=ixh(j+1,iel) 
          dx=x(1,nd1)-x(1,nd2)
          dy=x(2,nd1)-x(2,nd2)
          dz=x(3,nd1)-x(3,nd2)
          distratio=sqrt(dx*dx+dy*dy+dz*dz)
          if (distratio.ne.0.0.and.distratioref.eq.0.0) then
            distratioref=distratio 
          endif
          if (distratioref.ne.0) then
            if (distratio.ge.distratioref) then
              distratio=distratio/distratioref
            elseif (distratio.ne.0.0) then
              distratio=distratioref/distratio
            endif 
            distratiomax=max(distratio,distratiomax) 
          endif
        enddo
      enddo
      test=distratiomax.lt.valuelimit
c      
      endif
c
      return
      end
c
      subroutine shlrfn_criteria5(test,valuelimit,
     .                            iel,v,x,ixs,nwcon,
     .                            auxvc,lochvh,isadj,
     .                            iprnt,maxelsold1)
      dimension auxvc(*),lochvh(*),v(3,*),x(3,*),ixs(nwcon,*),isadj(4,*)
      dimension iprnt(*)
      real*8 x
      logical test
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : code close to adptol=2 in *CONTROL_ADAPTIVE
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=(auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
      test=pres.ge.valuelimit
c     
      elseif (icrteria_example.eq.2) then
c
      nlvl=1
      iterprnt=iel
 1    if (iterprnt.gt.maxelsold1) then
        iterprnt=iprnt(iterprnt-maxelsold1)
        nlvl=nlvl+1
        goto 1
      endif 
c
      dotmin=1.0
      do i=1,5  
        if (i.eq.1) then
          is=1
          jel=iel
        else
          is=2
          jel=isadj(i-1,iel)
        endif
        if (jel.gt.0 ) then
          nd1=ixs(2,jel)
          nd2=ixs(3,jel)
          nd3=ixs(4,jel)
          nd4=ixs(5,jel)
          x1=x(1,nd1)
          y1=x(2,nd1)
          z1=x(3,nd1)
          x2=x(1,nd2)
          y2=x(2,nd2)
          z2=x(3,nd2)
          x3=x(1,nd3)
          y3=x(2,nd3)
          z3=x(3,nd3)
          x4=x(1,nd4)
          y4=x(2,nd4)
          z4=x(3,nd4)
          x31=x3-x1
          y31=y3-y1
          z31=z3-z1
          x42=x4-x2
          y42=y4-y2
          z42=z4-z2
          c1=y31*z42-z31*y42
          c2=z31*x42-x31*z42
          c3=x31*y42-y31*x42
          xl=1./sqrt(c1*c1+c2*c2+c3*c3)
          c1=c1*xl
          c2=c2*xl
          c3=c3*xl
          if (i.eq.1) then
            c1s=c1
            c2s=c2
            c3s=c3
          else
            dot=c1*c1s+c2*c2s+c3*c3s
            dotmin=min(dotmin,dot) 
          endif
        endif
      enddo
      pi=3.14159265359
      angle=acos(dotmin)*180/pi
      test=angle.ge.valuelimit*nlvl
c      
      endif
c
      return
      end
c
      subroutine shlrmv_criteria5(test,valuelimit,
     .                            iel,v,x,ixs,nwcon,
     .                            auxvc,lochvh,isadj,
     .                            iprnt,maxelsold1)
      dimension auxvc(*),lochvh(*),v(3,*),x(3,*),ixs(nwcon,*),isadj(4,*)
      dimension iprnt(*)
      real*8 x
      logical test
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : code close to adptol=2 in *CONTROL_ADAPTIVE
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=(auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
      test=pres.lt.valuelimit
c
      elseif (icrteria_example.eq.2) then
c     
      nlvl=1
      iterprnt=iel
 1    if (iterprnt.gt.maxelsold1) then
        iterprnt=iprnt(iterprnt-maxelsold1)
        nlvl=nlvl+1
        goto 1
      endif 
c
      dotmin=1.0
      do i=1,5  
        if (i.eq.1) then
          is=1
          jel=iel
        else
          is=2
          jel=isadj(i-1,iel)
        endif
        if (jel.gt.0 ) then
          nd1=ixs(2,jel)
          nd2=ixs(3,jel)
          nd3=ixs(4,jel)
          nd4=ixs(5,jel)
          x1=x(1,nd1)
          y1=x(2,nd1)
          z1=x(3,nd1)
          x2=x(1,nd2)
          y2=x(2,nd2)
          z2=x(3,nd2)
          x3=x(1,nd3)
          y3=x(2,nd3)
          z3=x(3,nd3)
          x4=x(1,nd4)
          y4=x(2,nd4)
          z4=x(3,nd4)
          x31=x3-x1
          y31=y3-y1
          z31=z3-z1
          x42=x4-x2
          y42=y4-y2
          z42=z4-z2
          c1=y31*z42-z31*y42
          c2=z31*x42-x31*z42
          c3=x31*y42-y31*x42
          xl=1./sqrt(c1*c1+c2*c2+c3*c3)
          c1=c1*xl
          c2=c2*xl
          c3=c3*xl
          if (i.eq.1) then
            c1s=c1
            c2s=c2
            c3s=c3
          else
            dot=c1*c1s+c2*c2s+c3*c3s
            dotmin=min(dotmin,dot) 
          endif
        endif
      enddo
      pi=3.14159265359
      angle=acos(dotmin)*180/pi
      test=angle.lt.valuelimit*nlvl
c      
      endif
c
      return
      end
c
      subroutine al2rfn_criteria5(test,valuelimit,
     .                            iel,nmmg,lstmmg,gvp,nalegp,
     .                            v,x,ixs,nwcon,
     .                            auxvc,nmcx,lochvh)
      dimension lstmmg(*),gvp(nalegp,*),auxvc(*),nmcx(*),
     .          lochvh(*),v(3,*),x(3,*),ixs(nwcon,*)
      real*8 x
      logical test
      dimension ioppcorner(2,2)
      data ioppcorner/1,3,
     .                2,4/
c
c icrteria_example=1 : pressure
c icrteria_example=2 : gradient of velocity
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=0.0
      volfrctot=0.0
      do immg=1,iabs(nmmg)
        ici=lstmmg(immg)
        l44=l44+nmcx(ici)
        pres=pres-gvp(ici,iel)*
     .          (auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
        volfrctot=volfrctot+gvp(ici,iel)
      enddo
      if (volfrctot.ne.0.0) then
        pres=pres/volfrctot
        test=pres.ge.valuelimit
      endif
c     
      elseif (icrteria_example.eq.2) then
c     
      gradmax=0.0
      do idiag=1,2
        ic1=ioppcorner(1,idiag)+1
        ic2=ioppcorner(2,idiag)+1
        nd1=ixs(ic1,iel)
        nd2=ixs(ic2,iel)
        do idir=1,3
          dv=v(idir,nd1)-v(idir,nd2)
          dx=x(1,nd1)-x(1,nd2)
          dy=x(2,nd1)-x(2,nd2)
          gradx=dv/dx
          grady=dv/dy
          grad=gradx*gradx+grady*grady
          grad=sqrt(grad)
          gradmax=max(grad,gradmax)
        enddo
      enddo
      test=gradmax.ge.valuelimit
c      
      endif
c
      return
      end
c
      subroutine al2rmv_criteria5(test,valuelimit,
     .                            iel,nmmg,lstmmg,gvp,nalegp,
     .                            v,x,ixs,nwcon,
     .                            auxvc,nmcx,lochvh)
      dimension lstmmg(*),gvp(nalegp,*),auxvc(*),nmcx(*),
     .          lochvh(*),v(3,*),x(3,*),ixs(nwcon,*)
      real*8 x
      logical test
      dimension ioppcorner(2,2)
      data ioppcorner/1,3,
     .                2,4/
c
c icrteria_example=1 : Pressure
c icrteria_example=2 : rot(v)
c
      icrteria_example=2
c
      if (icrteria_example.eq.1) then
c
      l44=lochvh(iel)-1
      pres=0.0
      volfrctot=0.0
      do immg=1,iabs(nmmg)
        ici=lstmmg(immg)
        l44=l44+nmcx(ici)
        pres=pres-gvp(ici,iel)*
     .          (auxvc(l44+1)+auxvc(l44+2)+auxvc(l44+3))/3.0
        volfrctot=volfrctot+gvp(ici,iel)
      enddo
      if (volfrctot.ne.0.0) then
        pres=pres/volfrctot
        test=pres.lt.valuelimit
      endif
c
      elseif (icrteria_example.eq.2) then
c     
      gradmax=0.0
      do idiag=1,2
        ic1=ioppcorner(1,idiag)+1
        ic2=ioppcorner(2,idiag)+1
        nd1=ixs(ic1,iel)
        nd2=ixs(ic2,iel)
        do idir=1,3
          dv=v(idir,nd1)-v(idir,nd2)
          dx=x(1,nd1)-x(1,nd2)
          dy=x(2,nd1)-x(2,nd2)
          gradx=dv/dx
          grady=dv/dy
          grad=gradx*gradx+grady*grady
          grad=sqrt(grad)
          gradmax=max(grad,gradmax)
        enddo
      enddo
      test=gradmax.lt.valuelimit
c      
      endif
c
      return
      end
