      subroutine inclination_angle(s, pp, pn, alpha0, li, lt, alpha)
      real*8 s, pp, pn, alpha0, li, lt, alpha, sps, spp, spn
      spp = s + pp
      spn = s - pn
      if (spp.le.-li) then
        alpha = 0.0
      elseif (spp.le.-li+lt) then
        alpha = alpha0 * (spp-li) / lt
      elseif (spp.le.-lt) then
        alpha = -alpha0
      elseif (spp.le.0) then
        alpha = alpha0 * (s+pp) / lt
      elseif (spn.le.0) then
        alpha = 0.0
      elseif (spn.le.lt) then
        alpha = alpha0 * spn / lt
      elseif (spn.le.li-lt) then
        alpha = alpha0 * alpha0
      elseif (spn.le.li) then
        alpha = alpha0 * (li-spn) / lt
      else
        alpha = 0.0
      endif

      return
      end


      subroutine umat41c(idpart,cm,lft,llt,fc,dx,dxdt,aux,ek,
     & ifail,dt1siz,crv,nhxbwp,cma)
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
c          cm -------- material constants
c          lft,llt --- start and end of block
c          fc -------- components of the cohesive force
c          dx -------- components of the displacement
c          dxdt ------ components of the velocity
c          aux ------- history storage
c          ek -------- max. stiffness/area for time step calculation
c          ifail ----- =.false. not failed
c                      =.true. failed
c          dt1siz ---- time step size
c          crv ------- curve array
c          nhxbwp ---- internal element id array, lqfinv(nhxbwp(i),2)
c                      gives external element id
c         cma=additional memory for material data defined by LMCA at 
c           6th field of 2nd crad of *DATA_USER_DEFINED
c
c***  dx, dxdt, and fc are in the local coordinate system:
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
      include 'nlqparm'
      logical ifail
      dimension cm(*),fc(nlq,*),dx(nlq,*),dxdt(nlq,*),
     & aux(nlq,*),ek(*),ifail(*),dt1siz(*),crv(lq1,2,*),
     & nhxbwp(*),cma(*)
c
      et=cm(3)
      en=cm(4)
      eki=max(et,en)
      fcfail=cm(5)
c
      do i=lft,llt
        fc(i,1)=et*dx(i,1)
        fc(i,2)=et*dx(i,2)
        fc(i,3)=en*dx(i,3)
        ek(i)=eki
        ifail(i)=fc(i,3).gt.fcfail
      enddo
c
      return
      end
