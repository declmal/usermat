c
c     definition of plastic flow a1
c
      subroutine plastic_flow_a1(sigma, tau, fcpr, k3, k4, k5, hr, pp,
     & pn, r, ma1)
      real*8 sigma, tau
      real*8 fcpr, k3, k4, k5, hr
      real*8 pp, pn, r
      real*8 ma1
      real*8 rhosigma
      rhosigma = max(0.0,-sigma) / fcpr
      ma1 = k3 * max(0.0,1.0-rhosigma)*exp(-k4*(pp+pn)/hr)
     & - k5*rhosigma*max(0.0,r)/hr
      return
      end
c
c     stress return to yield surface a (inner)
c
      subroutine return_a_inner(n, x, fvec, iflag, ext, lext)
      implicit none
      integer n, iflag, lext
      double precision x(n), fvec(n), ext(lext)
c     material constants, state variables, flow variables
      double precision fcpr, hr, dnn, k3, k4, k5
      double precision sigmat1e
      double precision sigmat, taut, ppt, pnt, rt
      double precision sigmat1, taut1, ppt1, pnt1, rt1
      double precision dlambda, theta
c     function-scale parameters
      double precision ma1t, ma1t1, depsp
c     variable acquisition
      fcpr = ext(1)
      hr = ext(3)
      dnn = ext(10)
      k3 = ext(14)
      k4 = ext(15)
      k5 = ext(16)
      sigmat1e = ext(17)
      sigmat = ext(19)
      taut = ext(20)
      ppt = ext(21)
      pnt = ext(22)
      rt = ext(23)
      sigmat1 = ext(25)
      taut1 = ext(26)
      ppt1 = ext(27)
      pnt1 = ext(28)
      rt1 = ext(29)
      dlambda = ext(31)
      theta = ext(32)
c     residual
      call plastic_flow_a1(sigmat, taut, fcpr, hr, k3, k4, k5, ppt,
     & pnt, rt, ma1t)
      call plastic_flow_a1(sigmat1, taut1, fcpr, hr, k3, k4, k5,
     & ppt1, pnt1, rt1, ma1t1)
      depsp = dlambda * ((1.0D+00-theta)*ma1t+theta*ma1t1)
      fvec(1) = sigmat1e - dnn*depsp - sigmat1
      fvec(2) = rt + depsp - rt1
      return
      end
c
c     definition of yield surface a
c
      subroutine yield_surface_a(fcpr, sr, c0, mua0, k1, k2, sigma, tau,
     & pp, pn, fa)
      double precision fcpr, sr, c0, mua0, k1, k2
      double precision sigma, tau, pp, pn
      double precision fa
      double precision rhop, c, mua
      rhop = (pp+pn) / sr
      c = c0 * max(0.0D+00,1.0D+00-rhop)
      mua = mua0 * exp(-k2*rhop)
      fa = abs(sigma/fcpr)**k1 - (c/fcpr)**k1 + mua*tau/fcpr
      return
      end
c
c     stress return to yield surface a
c
      subroutine return_a_outer(n, x, fvec, iflag, ext, lext)
      implicit none
      integer n, iflag, lext
      double precision x(n), fvec(n), ext(lext)
      external return_a_inner
c     material constants, state variables, flow variables
      double precision fcpr, sr, c0, mua0, dtt, k1, k2
      double precision taut1e
      double precision sigmat, ppt, pnt, rt
      double precision sigmat1, taut1, ppt1, pnt1, rt1
      double precision dlambda
c     function-scale parameters
      double precision mt1, fa
      double precision xi(2), fveci(2), infoi, wai(19)
c     variable acquisition
      fcpr = ext(1)
      sr = ext(2)
      c0 = ext(7)
      mua0 = ext(8)
      dtt = ext(11)
      k1 = ext(12)
      k2 = ext(13)
      sigmat = ext(19)
      ppt = ext(21)
      pnt = ext(22)
      rt = ext(23)
      dlambda = ext(31)
c     outer-loop flow
      mt1 =sign(1.0D+00, taut1e)
      taut1 = taut1e - dlambda*dtt*mt1
      ppt1 = ppt + dlambda*max(0.0D+00,mt1)
      pnt1 = pnt + dlambda*max(0.0D+00,-mt1)
c     inner-loop flow
      ext(26) = taut1
      ext(27) = ppt1
      ext(28) = pnt1
      xi(1) = sigmat
      xi(2) = rt
      call hybrdext1(return_a_inner, 2, xi, fveci, 0.00001D+00, infoi,
     & wai, 19, ext, 32)
      sigmat1 = xi(1)
      rt1 = xi(2)
c     residual
      call yield_surface_a(fcpr, sr, c0, mua0, k1, k2, sigmat1, taut1,
     & ppt1, pnt1, fa)
      fvec(1) = fa
      return
      end
c
c     inclination angle
c
      subroutine inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      double precision li, lt, alpha0
      double precision pp, pn, s
      double precision alpha
      double precision spp, spn
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
c
c     definition of plastic flow b1
c
      subroutine plastic_flow_b1(li, lt, alpha0, pp, pn, s, mb1)
      double precision li, lt, alpha0
      double precision pp, pn, s
      double precision mbn1
      double precision alpha
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      mb1 = -tan(alpha)
      return
      end
c
c     definition of yield surface b
c
      subroutine yield_surface_b(li, lt, alpha0, mub, sigma, tau, pp,
     & pn, s, fb)
      double precision li, lt, alpha0, mub
      double precision sigma, tau, pp, pn, s
      double precision fb
      double precision alpha, sina, cosa
      call inclination_angle(li, lt, alpha0, pp, pn, s, alpha)
      sina = sin(alpha)
      cosa = cos(alpha)
      fb = abs(tau*cosa+sigma*sina) + mub*(sigma*cosa-tau*sina)
      return
      end
c
c     stress return to yield surface b (loop1)
c
      subroutine return_b_loop1(n, x, fvec, iflag, ext, lext)
      implicit none
      integer n, iflag, lext
      double precision x(n), fvec(n), ext(lext)
c     material constants, state variables, flow variables
      double precision li, lt, alpha0, mub, dnn, dtt
      double precision sigmat1e, taut1e
      double precision ppt, pnt, st
      double precision sigmat1, taut1, st1
      double precision dlambda, theta
c     function-scale parameters
      double precision mt1, mb1t, mb1t1, fb
c     variable acquisition
      li = ext(4)
      lt = ext(5)
      alpha0 = ext(6)
      mub = ext(9)
      dnn = ext(10)
      dtt = ext(11)
      sigmat1e = ext(17)
      taut1e = ext(18)
      ppt = ext(21)
      pnt = ext(22)
      st = ext(24)
      theta = ext(32)
      mt1 = ext(33)
      dlambda = x(1)
c     residual
      taut1 = taut1e - dlambda*dtt*mt1
      st1 = st + dlambda*mt1
      call plastic_flow_b1(li, lt, alpha0, ppt, pnt, st, mb1t)
      call plastic_flow_b1(li, lt, alpha0, ppt, pnt, st1, mb1t1)
      sigmat1 = sigmat1e - dlambda*dnn*((1.0D+00-theta)*mb1t+
     &theta*mb1t1)*mt1
      call yield_surface_b(
     & li, lt, alpha0, mub, sigmat1, taut1, ppt, pnt, st1, fb)
      fvec(1) = fb
      return
      end
c
c     stress return to yield surface b
c
      subroutine return_b(
     & ext, lext, sigmat1, taut1, ppt1, pnt1, rt1, st1)
      implicit none
      integer lext
      double precision ext(lext)
      external return_b_loop1
c     residual
      double precision li, lt, alpha0, c0, dnn, dtt
      double precision sigmat1e, taut1e
      double precision ppt, pnt, rt, st
      double precision sigmat1, taut1, ppt1, pnt1, rt1, st1
      double precision dlambda, theta, mt1
c     function-scale parameters
      double precision alphat, mb1t, mb1t1
      double precision xi(1), fveci(1), infoi, wai(8)
c     variable acquisition
      li = ext(4)
      lt = ext(5)
      alpha0 = ext(6)
      dnn = ext(10)
      dtt = ext(11)
      sigmat1e = ext(17)
      taut1e = ext(18)
      ppt = ext(21)
      pnt = ext(22)
      st = ext(24)
      theta = ext(32)
c     get lambda
      call inclination_angle(li, lt, alpha0, ppt, pnt, st, alphat)
      mt1 = sign(1.0D+00, taut1e*cos(alphat)+sigmat1e*sin(alphat))
      ext(33) = mt1
      call hybrdext1(return_b_loop1, 2, xi, fveci, 0.00001D+00, infoi,
     & wai, 8, ext, 33)
      dlambda = xi(1)
c     get stress state
      taut1 = taut1e - dlambda*dtt*mt1
      ppt1 = ppt
      pnt1 = pnt
      rt1 = rt
      st1 = st + dlambda*mt1
      call plastic_flow_b1(li, lt, alpha0, ppt, pnt, st, mb1t)
      call plastic_flow_b1(li, lt, alpha0, ppt, pnt, st1, mb1t1)
      sigmat1 = sigmat1e - dlambda*dnn*((1.0D+00-theta)*mb1t+
     &theta*mb1t1)*mt1
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
