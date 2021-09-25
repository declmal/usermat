c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       yield surface a and flow rules
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_a_flow(
     &  fcpr, hr, k3, k4, k5, sigma, tau, pp, pn, r, ma1)
      double precision fcpr, hr, k3, k4, k5
      double precision sigma, tau, pp, pn, r
      double precision ma1
      double precision rhosigma
      rhosigma = max(0.0D+00,-sigma) / fcpr
      ma1 = k3 * max(0.0D+00,1.0D+00-rhosigma)*exp(-k4*(pp+pn)/hr)
     &      - k5*rhosigma*max(0.0D+00,r)/hr
      return
      end
      subroutine murcia_a_yield(
     &  fcpr, sr, c0, mua0, k1, k2, sigma, tau, pp, pn, fa)
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
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       yield surface b and flow rules
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_angle(li, lt, alpha0, pp, pn, s, alpha)
c     input arguments
      double precision li, lt, alpha0
      double precision pp, pn, s
c     output arguments
      double precision alpha
c     function-scale parameters
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
      subroutine murcia_b_flow(li, lt, alpha0, pp, pn, s, mb1)
c     input arguments
      double precision li, lt, alpha0
      double precision pp, pn, s
c     output arguments
      double precision mb1
c     function-scale parameters
      double precision alpha
      call murcia_angle(li, lt, alpha0, pp, pn, s, alpha)
      mb1 = -tan(alpha)
      return
      end
      subroutine murcia_b_yield(
     &  li, lt, alpha0, mub, sigma, tau, pp, pn, s, fb)
c     input arguments
      double precision li, lt, alpha0, mub
      double precision sigma, tau, pp, pn, s
c     output arguments
      double precision fb
c     function-scale parameters
      double precision alpha, sina, cosa
      call murcia_angle(li, lt, alpha0, pp, pn, s, alpha)
      sina = sin(alpha)
      cosa = cos(alpha)
      fb = abs(tau*cosa+sigma*sina) + mub*(sigma*cosa-tau*sina)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      return to a
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_a_f2(n, x, fvec, iflag, cm, lcm, ext, lext)
      integer n, iflag, lcm, lext
      double precision x(n), fvec(n), cm(lcm), ext(lext)
      double precision ma1t, ma1t1, ddns
      call murcia_a_flow(
     &  cm(3), cm(5), cm(16), cm(17), cm(18), ext(3), ext(4), ext(5),
     &  ext(6), ext(7), ma1t)
      call murcia_a_flow(
     &  cm(3), cm(5), cm(16), cm(17), cm(18), x(1), ext(10), ext(11),
     &  ext(11), ext(12), x(2), ma1t1)
      ddns = ext(18) * ((1.0D+00-cm(19))*ma1t+cm(19)*ma1t1)
      fvec(1) = ext(1) - cm(12)*ddns - ext(9)
      fvec(2) = ext(7) + ddns - ext(13)
      return
      end
      subroutine murcia_a_f1(n, x, fvec, iflag, cm, lcm, ext, lext)
      integer n, iflag, lcm, lext
      double precision x(n), fvec(n), cm(lcm), ext(lext)
      external murcia_a_f2
      integer n2, lwa2
      parameter (n2 = 2, lwa2 = (n2*(3*n2+13)) / 2)
      double precision x2(n2), fvec2(n2), info2, wa2(lwa2)
      double precision tol2/0.00001D+00/
      ext(10) = ext(2) - x(1)*cm(13)*ext(15)
      ext(11) = ext(5) + x(1)*max(0.0D+00,ext(15))
      ext(12) = ext(6) + x(1)*max(0.0D+00,-ext(15))
      ext(18) = x(1)
      x2(1) = ext(3)
      x2(2) = ext(7)
      call hybrdext1(
     &  murcia_a_f2, n2, x2, fvec2, tol2, info2, wa2, lwa2, cm, lcm,
     &  ext, lext)
      ext(9) = x2(1)
      ext(13) = x2(2)
      call murcia_a_yield(
     &  cm(3), cm(4), cm(9), cm(10), cm(14), cm(15), ext(9), ext(10),
     &  ext(11), ext(12), fvec(1))
      return
      end
      subroutine murcia_a_return(
     &  cm, lcm, ext, lext, sigmat1, taut1, ppt1, pnt1, rt1, st1)
      integer lcm, lext
      double precision cm(lcm), ext(lext)
      external murcia_a_f1
      integer n, lwa
      parameter (n = 1, lwa = (n*(3*n+13)) / 2)
      double precision x(n), fvec(n), info, wa(lwa)
      double precision tol/0.00001D+00/
      ext(15) = sign(1.0D+00, ext(2))
      x(1) = 0.0D+00
      call hybrdext1(
     &  murcia_a_f1, n, x, fvec, tol, info, wa, lwa, cm, lcm,
     &  ext, lext)
      ext(14) = ext(8)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      return to b 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_b_f1(n, x, fvec, iflag, cm, lcm, ext, lext)
      integer n, iflag, lcm, lext
      double precision x(n), fvec(n), cm(lcm), ext(lext)
      double precision mb1t, mb1t1
      ext(10) = ext(2) - x(1)*cm(13)*ext(16)
      ext(14) = ext(8) + x(1)*ext(16)
      call murcia_b_flow(
     &  cm(6), cm(7), cm(8), ext(5), ext(6), ext(8), mb1t)
      call murcia_b_flow(
     &  cm(6), cm(7), cm(8), ext(5), ext(6), ext(14), mb1t1)
      ext(9) = 
     &  ext(1) - x(1)*cm(12)*((1.0D+00-cm(19))*mb1t+cm(19)*mb1t1)
     &  *ext(16)
      call murcia_b_yield(
     &  cm(6), cm(7), cm(8), cm(11), ext(9), ext(10), ext(5), ext(6),
     &  ext(14), fvec(1))
      return
      end
      subroutine murcia_b_return(
     &  cm, lcm, ext, lext, sigmat1, taut1, ppt1, pnt1, rt1, st1)
      integer lcm, lext
      double precision cm(lcm), ext(lext)
      external murcia_a_f1
      integer n, lwa
      parameter (n = 1, lwa = (n*(3*n+13)) / 2)
      double precision x(n), fvec(n), info, wa(lwa)
      double precision tol/0.00001D+00/
      if (ext(17).le.0.0D+00) then
        ext(9) = 0.0D+00
        ext(10) = 0.0D+00
        ext(11) = ext(5)
        ext(12) = ext(6)
        ext(13) = ext(7)
        ext(14) = ext(8) + ext(2)/cm(13)
        return
      endif
      x(1) = 0.0D+00
      call hybrdext1(
     &  murcia_b_f1, n, x, fvec, tol, info, wa, lwa, cm, lcm,
     &  ext, lext)
      ext(11) = ext(5)
      ext(12) = ext(6)
      ext(13) = ext(7)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      return to the intersection a and b 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_ab_f2(n, x, fvec, iflag, cm, lcm, ext, lext)
      integer n, iflag, lcm, lext
      double precision x(n), fvec(n), cm(lcm), ext(lext)
      double precision ma1t, ma1t1, ddns, mb1t, mb1t1
      call murcia_a_flow(
     &  cm(3), cm(5), cm(16), cm(17), cm(18), ext(3), ext(4), ext(5),
     &  ext(6), ext(7), ma1t)
      call murcia_a_flow(
     &  cm(3), cm(5), cm(16), cm(17), cm(18), x(1), ext(10), ext(11),
     &  ext(11), ext(12), x(2), ma1t1)
      ddns = ext(18) * ((1.0D+00-cm(19))*ma1t+cm(19)*ma1t1)
      call murcia_b_flow(
     &  cm(6), cm(7), cm(8), ext(5), ext(6), ext(8), mb1t)
      call murcia_b_flow(
     &  cm(6), cm(7), cm(8), ext(5), ext(6), ext(14), mb1t1)
      fvec(1) = ext(1) - cm(12)*ddns - ext(9)
     &  - ext(19)*cm(12)*((1.0D+00-cm(19))*mb1t+cm(19)*mb1t1)*ext(16)
      fvec(2) = ext(7) + ddns - ext(13)
      return
      end
      subroutine murcia_ab_f1(n, x, fvec, iflag, cm, lcm, ext, lext)
      integer n, iflag, lcm, lext
      double precision x(n), fvec(n), cm(lcm), ext(lext)
      external murcia_ab_f2
      integer n2, lwa2
      parameter (n2 = 2, lwa2 = (n2*(3*n2+13)) / 2)
      double precision x2(n2), fvec2(n2), info2, wa2(lwa2)
      double precision tol2/0.00001D+00/
      ext(10) = ext(2) - x(1)*cm(13)*ext(15) - x(2)*cm(11)*ext(16)
      ext(11) = ext(5) + x(1)*max(0.0D+00,ext(15))
      ext(12) = ext(6) + x(1)*max(0.0D+00,-ext(15))
      ext(14) = ext(8) + x(2)*ext(16)
      ext(18) = x(1)
      ext(19) = x(2)
      x2(1) = ext(3)
      x2(2) = ext(7)
      call hybrdext1(
     &  murcia_ab_f2, n2, x2, fvec2, tol2, info2, wa2, lwa2, cm, lcm,
     &  ext, lext)
      ext(9) = x2(1)
      ext(13) = x2(2)
      call murcia_a_yield(
     &  cm(3), cm(4), cm(9), cm(10), cm(14), cm(15), ext(9), ext(10),
     &  ext(11), ext(12), fvec(1))
      call murcia_b_yield(
     &  cm(6), cm(7), cm(8), cm(11), ext(9), ext(10), ext(5), ext(6),
     &  ext(14), fvec(2))
      return
      end
      subroutine murcia_ab_return(cm, lcm, ext, lext)
      integer lcm, lext
      double precision cm(lcm), ext(lext)
      external murcia_ab_f1
      integer n, lwa
      parameter (n = 2, lwa = (n*(3*n+13)) / 2)
      double precision x(n), fvec(n), info, wa(lwa)
      double precision tol/0.00001D+00/
      x(1) = 0.0D+00
      x(2) = 0.0D+00
      call hybrdext1(
     &  murcia_ab_f1, n, x, fvec, tol, info, wa, lwa, cm, lcm,
     &  ext, lext)
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      return to the intersection a and b 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine murcia_return(cm, lcm, ext, lext)
      integer lcm, lext
      double precision cm(lcm), ext(lext)
      double precision alphat
      double precision fa, fb, swa
      double precision sigmat1a, taut1a, ppt1a, pnt1a, rt1a, st1a
      double precision sigmat1b, taut1b, ppt1b, pnt1b, rt1b, st1b
      double precision fab, fba
      call murcia_a_yield(
     &  cm(3), cm(4), cm(9), cm(10), cm(14), cm(15), ext(1), ext(2),
     &  ext(11), ext(12), fa)
      call murcia_b_yield(
     &  cm(6), cm(7), cm(8), cm(11), ext(9), ext(10), ext(1), ext(2),
     &  ext(14), fb)
      if (fa.le.0.0D+00.and.fb.le.0.0D+00) then
        ext(9) = ext(3)
        ext(10) = ext(4)
        ext(11) = ext(5)
        ext(12) = ext(6)
        ext(13) = ext(7)
        ext(14) = ext(8)
        return
      endif
      ext(15) = sign(1.0D+00, ext(2))
      call murcia_angle(
     &  cm(6), cm(7), cm(8), ext(5), ext(6), ext(8), alphat)
      ext(16) = sign(1.0D+00, ext(2)*cos(alphat)+ext(1)*sin(alphat))
      swa = 
     &  abs(ext(2))/cm(13)*cm(16)*exp(-cm(17)*(ext(5)+ext(6))/cm(5))
     &  - ext(1)/cm(12)
      ext(17) = ext(2)/cm(13)*tan(alphat) - ext(1)/cm(12)
      if (fa.gt.0.0D+00.and.swa.gt.0.0D+00.and.fb.gt.0.0D+00) then
        call murcia_a_return(cm, lcm, ext, lext)
        sigmat1a = ext(9)
        taut1a = ext(10)
        ppt1a = ext(11)
        pnt1a = ext(12)
        rt1a = ext(13)
        st1a = ext(14)
        call murcia_b_return(cm, lcm, ext, lext)
        sigmat1b = ext(9)
        taut1b = ext(10)
        ppt1b = ext(11)
        pnt1b = ext(12)
        rt1b = ext(13)
        st1b = ext(14)
        call murcia_a_yield(
     &    cm(3), cm(4), cm(9), cm(10), cm(14), cm(15), sigmat1b, taut1b,
     &    ppt1b, pnt1b, fab)
        call murcia_b_yield(
     &    cm(6), cm(7), cm(8), cm(11), sigmat1a, taut1a, ppt1a, pnt1a,
     &    st1a, fba)
        if (fab.gt.0.0D+00) then
          if (fba.gt.0.0D+00) then
            call murcia_ab_return(cm, lcm, ext, lext)
          else
            ext(9) = sigmat1a
            ext(10) = taut1a
            ext(11) = ppt1a
            ext(12) = pnt1a
            ext(13) = rt1a
            ext(14) = st1a
          endif
        else
          if (fba.gt.0.0D+00) then
            ext(9) = sigmat1b
            ext(10) = taut1b
            ext(11) = ppt1b
            ext(12) = pnt1b
            ext(13) = rt1b
            ext(14) = st1b
          else
            call murcia_ab_return(cm, lcm, ext, lext)
          endif
        endif
        return
      endif
      if (fa.gt.0.0D+00.and.swa.gt.0.0D+00) then
        call murcia_a_return(cm, lcm, ext, lext)
        call murcia_b_yield(
     &    cm(6), cm(7), cm(8), cm(11), ext(9), ext(10), ext(5), ext(6),
     &    ext(14), fba)
        if (fba.gt.0.0D+00) then
          call murcia_ab_return(cm, lcm, ext, lext)
        endif
        return
      endif
      call murcia_b_return(cm, lcm, ext, lext)
      call murcia_a_yield(
     &  cm(3), cm(4), cm(9), cm(10), cm(14), cm(15), ext(9), ext(10),
     &  ext(11), ext(12), fab)
      if (fab.gt.0.0D+00) then
        call murcia_ab_return(cm, lcm, ext, lext)
      endif
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     usermat definition
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      subroutine umat41c(
     &  idpart, cm, lft, llt, fc, dx, dxdt, aux, ek, ifail, dt1siz, crv,
     &  nhxbwp,cma)
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
      dimension cm(*), fc(nlq,*), dx(nlq,*), dxdt(nlq,*), aux(nlq,*),
     &  ek(*), ifail(*), dt1siz(*), crv(lq1,2,*), nhxbwp(*), cma(*)
      integer lext, lcm
      parameter (lext = 19, lcm = 19)
      double precision ext(lext)
c
      ! et=cm(3)
      ! en=cm(4)
      ! eki=max(et,en)
      ! fcfail=cm(5)
c
      do i=lft,llt
        ext(1) = aux(i,1) + cm(12)*dx(i,1)
        ext(2) = aux(i,2) + cm(13)*dx(i,2)
        ext(3) = aux(i,1)
        ext(4) = aux(i,2)
        ext(5) = aux(i,3)
        ext(6) = aux(i,4)
        ext(7) = aux(i,5)
        ext(8) = aux(i,6)
        call murcia_return(cm, lcm, ext, lext)
        fc(i,1) = ext(3)
        fc(i,2) = ext(4)
        aux(i,1) = ext(9)
        aux(i,2) = ext(10)
        aux(i,3) = ext(11)
        aux(i,4) = ext(12)
        aux(i,5) = ext(13)
        aux(i,6) = ext(14)
c        fc(i,1)=et*dx(i,1)
c        fc(i,2)=et*dx(i,2)
c        fc(i,3)=en*dx(i,3)
c        ek(i)=eki
c        ifail(i)=fc(i,3).gt.fcfail
      enddo
c
      return
      end
