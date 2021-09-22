c
c     cm(3) = sR
c     cm(4) = fcpr
c     cm(5) = c0
c     cm(6) = mua0
c     cm(7) = k1
c     cm(8) = k2
c     cm(9) = hR
c     cm(10) = k3
c     cm(11) = k4
c     cm(12) = k5
c     cm(13) = lI
c     cm(14) = lT
c     cm(15) = alpha0
c     cm(16) = mub
c
c     fc(1) = sigma
c     fc(2) = tau
c
c     aux(1) = pp
c     aux(2) = pn
c     aux(3) = r
c     aux(4) = s
c
      subroutine inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      real*8 li, lt, alpha0
      real*8 pp, pn, s
      real*8 alpha
      real*8 spp, spn
      spp = s + pp
      spn = s - pn
      if (spp.le.-li) then
        alpha = 0.0
      elseif (spp.le.-li+lt) then
        alpha = alpha0 * (spp-li) / lt
      elseif (spp.le.-lt) then
        alpha = -alpha0
      elseif (spp.le.0.0) then
        alpha = alpha0 * spp / lt
      elseif (spn.le.0.0) then
        alpha = 0.0
      elseif (spn.le.lt) then
        alpha = alpha0 * spn / lt
      elseif (spn.le.li-lt) then
        alpha = alpha0
      elseif (spn.le.li) then
        alpha = alpha0 * (li-spn) / lt
      else
        alpha = 0.0
      endif
      return
      end

      subroutine yield_surface_a(sigma, tau, sr, c0, mua0, k2, fcpr, k1,
     & pp, pn, fa)
      real*8 sigma, tau
      real*8 sr, c0, mua0, k2, fcpr, k1
      real*8 pp, pn
      real*8 fa
      real*8 rhop, c, mua
      rhop = (pp+pn) / sr
      c = c0 * max(0,1-rhop)
      mua = mua0 * exp(-k2*rhop)
      fa = abs(sigma/fcpr)**k1 - (c/fcpr)**k1 + mua*tau/fcpr
      return
      end

      subroutine plastic_flow_a1(sigma, tau, fcpr, k3, k4, hr, k5, pp,
     & pn, r, ma1)
      real*8 sigma, tau
      real*8 fcpr, k3, k4, hr, k5
      real*8 pp, pn, r
      real*8 ma1
      real*8 rhosigma
      rhosigma = max(0,-sigma) / fcpr
      ma1 = k3 * max(0,1-rhosigma)*exp(-k4*(pp+pn)/hr)
     & - k5*rhosigma*max(0,r)/hr
      return
      end

      subroutine yield_surface_b(sigma, tau, li, lt, alpha0, mub, pp,
     & pn, s, fb)
      real*8 sigma, tau
      real*8 li, lt, alpha0, mub
      real*8 pp, pn, s
      real*8 fb
      real*8 alpha, sina, cosa
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      sina = sin(alpha)
      cosa = cos(alpha)
      fb = abs(tau*cosa+sigma*sina) + mub*(sigma*cosa-tau*sina)
      return
      end

      subroutine plastic_flow_bp1(li, lt, alpha0, pp, pn, s, mbp1)
      real*8 li, lt, alpha0
      real*8 pp, pn, s
      real*8 mbp1
      real*8 alpha
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      mbp1 = tan(alpha)
      return
      end

      subroutine plastic_flow_bn1(li, lt, alpha0, pp, pn, s, mbn1)
      real*8 li, lt, alpha0
      real*8 pp, pn, s
      real*8 mbn1
      real*8 alpha
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      mbn1 = -tan(alpha)
      return
      end

      subroutine stress_return_b()
      real*8 sigma, tau
      real*8 li, lt, alpha0, dnn, dtt
      real*8 pp, pn, s
      real*8 sigmae, taue
      real*8 alpha, fb, sigmap, taup
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      if (sigmae/dnn.gt.taue/dtt*tan(alpha)) then
        sigma = 0.0
        tau = 0.0
        s = s + taue/dtt
      else
        sigmap = sigma
        taup = tau
        do while (fb.gt.0.0)
        end do
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
      real*8 sigma, tau
      real*8 dn, dt
      real*8 dnn, dtt
      real*8 sigmae, taue
c
      ! et=cm(3)
      ! en=cm(4)
      ! eki=max(et,en)
      ! fcfail=cm(5)
      dnn = cm(3)
      dtt = cm(4)
c
      do i=lft,llt
        sigma = fc(i, 1)
        dn = dx(i, 1)
        sigmae = sigma + dnn*dn
        tau = fc(i, 2)
        dt = dx(i, 2)
        taue = sigma + dtt*dt
        ! fc(i,1)=et*dx(i,1)
        ! fc(i,2)=et*dx(i,2)
        ! fc(i,3)=en*dx(i,3)
        ! ek(i)=eki
        ! ifail(i)=fc(i,3).gt.fcfail
      enddo
c
      return
      end
