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
      if (ncycle.eq.1) then
        if (cm(16).ne.1234567) then
          call usermsg('mat41')
        endif
      endif
c
c     compute shear modulus, g
c
      mult=1.0
      g2 =abs(cm(1))/(1.+cm(2))
      g  =.5*g2
c
      if (etype.eq.'solid'.or.etype.eq.'shl_t'.or.
     1     etype.eq.'sld2d') then
        if (cm(16).eq.1234567) then
          call mitfail3d(cm,eps,sig,epsp,hsv,dt1,capa,failel,tt,crv)
        else
          if (.not.failel) then
            davg=(-eps(1)-eps(2)-eps(3))/3.
c Computing Hydrostatic Stress(Incremental)
            p=-davg*cm(1)/(1.-2.*cm(2))
c Computing Total Hydrostatic Stress
            hsv(2)=hsv(2)-p
c Computing Trial Stress
            sig(1)=sig(1)+p+g2*(eps(1)+davg)
            sig(2)=sig(2)+p+g2*(eps(2)+davg)
            sig(3)=sig(3)+p+g2*(eps(3)+davg)
            sig(4)=sig(4)+g*eps(4)
            sig(5)=sig(5)+g*eps(5)
            sig(6)=sig(6)+g*eps(6)
c Computing Plastic Hardening Modulus
            qh=cm(5)*cm(1)/(cm(1)-cm(5))
c Updating The Yield Stress
            ak=cm(6)+qh*hsv(1)
c
c Begin strain rate effect
c
            if (cm(8).ne.0) then
              d1d=eps(1)+davg
              d2d=eps(2)+davg
              d3d=eps(3)+davg
              d4d=eps(4)
              d5d=eps(5)
              d6d=eps(6)
              ds=d4d*d4d+d5d*d5d+d6d*d6d
c Computing Effective Strain of Current Time Step
              effs=sqrt(2.*(d1d*d1d+d2d*d2d+d3d*d3d+2.*ds)/3.)
c Computing Strain Rate
              if (tt.ne.0) then
                effs=effs/dt1
              endif
c Computing the Strain rate Sensitivity using Cowper-Symond
              powp=1/cm(9)
              bas=effs/cm(8)
              mult1=bas**powp
              mult=1.+bas**powp
            endif
            ak=mult*ak
c
c End strain rate effect
c
c
c Computing Deviatoric Stress
c
            q1=hsv(2)+sig(1)
            q2=hsv(2)+sig(2)
            q3=hsv(2)+sig(3)
            q4=sig(4)
            q5=sig(5)
            q6=sig(6)
            aj2=q4*q4+q5*q5+q6*q6+(q1*q1+q2*q2+q3*q3)/2
c Computing Yield Function
            ak2=3*aj2-ak*ak
            scle=0
c Checking Yield
            if (ak2.gt.0) then
              scle=1
            endif
            fac1=1/(3.0*g+qh)
            fac2=3.0*g
            aj1=sqrt(3*abs(aj2))+1-scle
c Computing plastic strain Increment
            depi=scle*fac1*(aj1-ak)
c Computing Total Plastic Strain
            hsv(1)=hsv(1)+depi
            deps=scle*fac2*depi/aj1
c Stress Update
            sig(1)=sig(1)-deps*q1
            sig(2)=sig(2)-deps*q2
            sig(3)=sig(3)-deps*q3
            sig(4)=sig(4)-deps*q4
            sig(5)=sig(5)-deps*q5
            sig(6)=sig(6)-deps*q6
          endif
        endif
      endif
c
c10   format(/
c    1 ' *** Error element type ',a,' can not be',
c    2 '           run with the current material model.')
      return
      end
