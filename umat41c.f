c     
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
      dimension dt1siz(*), crv(lq1,2,*), nhxbwp(*), cma(*)
      double precision cm(5)
      double precision fc(nlq,3)
      double precision dx(nlq,3)
      double precision dxdt(nlq,3)
      double precision aux(nlq,5)
      double precision ek(nlq)
      logical ifail(nlq)
c
      et=cm(3)
      en=cm(4)
      fcfail=cm(5)
c
      do i=lft,llt
        call umatc_murcia(
     &    et, en, fcfail,
     &    fc(i,1), fc(i,2), fc(i,3),
     &    dx(i,1), dx(i,2), dx(i,3),
     &    ek(i), ifail(i)
     &  )
      enddo
c
      return
      end
