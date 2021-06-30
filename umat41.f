c
c $Id: dyn21.F 96707 2015-03-18 22:29:10Z ubasu $
c
c
      subroutine umat41 (cm,eps,sig,epsp,hsv,dt1,capa,etype,tt,
     1 temper,failel,crv,cma,qmat,elsiz,idele)
c
c******************************************************************
c|  Livermore Software Technology Corporation  (LSTC)             |
c|  ------------------------------------------------------------  |
c|  Copyright 1987-2008 Livermore Software Tech. Corp             |
c|  All rights reserved                                           |
c******************************************************************
c
c     isotropic elastic material (sample user subroutine)
c
c     Variables
c
c     cm(1)=first material constant, here young's modulus
c     cm(2)=second material constant, here poisson's ratio
c        .
c        .
c        .
c     cm(n)=nth material constant
c
c     eps(1)=local x  strain increment
c     eps(2)=local y  strain increment
c     eps(3)=local z  strain increment
c     eps(4)=local xy strain increment
c     eps(5)=local yz strain increment
c     eps(6)=local zx strain increment
c
c     sig(1)=local x  stress
c     sig(2)=local y  stress
c     sig(3)=local z  stress
c     sig(4)=local xy stress
c     sig(5)=local yz stress
c     sig(6)=local zx stress
c
c     hsv(1)=1st history variable
c     hsv(2)=2nd history variable
c        .
c        .
c        .
c        .
c     hsv(n)=nth history variable
c
c     dt1=current time step size
c     capa=reduction factor for transverse shear
c     etype:
c        eq."solid" for solid elements
c        eq."sph" for smoothed particle hydrodynamics
c        eq."sld2d" for shell forms 13 (2D solids - plane strain)
c        eq."sldax" for shell forms 14, and 15 (2D solids - axisymmetric)
c        eq."shl_t" for shell forms 25, 26, and 27 (shells with thickness stretch)
c        eq."shell" for all other shell elements plus thick shell forms 1 and 2
c        eq."tshel" for thick shell forms 3 and 5
c        eq."hbeam" for beam element forms 1 and 11
c        eq."tbeam" for beam element form 3 (truss)
c        eq."dbeam" for beam element form 6 (discrete)
c        eq."beam " for all other beam elements
c
c     tt=current problem time.
c
c     temper=current temperature
c
c     failel=flag for failure, set to .true. to fail an integration point,
c            if .true. on input the integration point has failed earlier
c
c     crv=array representation of curves in keyword deck
c
c     cma=additional memory for material data defined by LMCA at 
c       6th field of 2nd crad of *DATA_USER_DEFINED
c
c     elsiz=characteristic element size
c
c     idele=element id
c
c     All transformations into the element local system are
c     performed prior to entering this subroutine.  Transformations
c     back to the global system are performed after exiting this
c     routine.
c
c     All history variables are initialized to zero in the input
c     phase. Initialization of history variables to nonzero values
c     may be done during the first call to this subroutine for each
c     element.
c
c     Energy calculations for the dyna3d energy balance are done
c     outside this subroutine.
c
      include 'nlqparm'
      include 'bk06.inc'
      include 'iounits.inc'
      dimension cm(*),eps(*),sig(*),hsv(*),crv(lq1,2,*),cma(*)
      logical failel
      character*5 etype
      double precision mult, bas, powp, effs
c
c     parameter definition
      parameter (one_third=1./3.)
c
      if (ncycle.eq.1) then
        if (cm(16).ne.1234567) then
          call usermsg('mat41')
        endif
      endif
c
c     material constants
c
      E=cm(1)
      nu=cm(2)
      a0=cm(3)
      a1=cm(4)
      a2=cm(5)
      b1=cm(6)
      a0f=cm(7)
      a1f=cm(8)
c
c     compute shear modulus, G
c
      G=.5*E/(1.+nu)
c
c     compute bulk modulus, K
c
      K=E/(3.*(1.-2.nu))

      if (etype.eq.'solid') then
        if (cm(16).eq.1234567) then
          call mitfail3d(cm,eps,sig,epsp,hsv,dt1,capa,failel,tt,crv)
        else
          if (.not.failel) then
c
c     calculate strain rate and volume change
c
          endif
        endif
      endif
c
c10   format(/
c    1 ' *** Error element type ',a,' can not be',
c    2 '           run with the current material model.')
      return
      end
