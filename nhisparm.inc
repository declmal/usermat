c
      integer    NHISVAR
      parameter (NHISVAR=200)
c
c     NHISVAR - maximum number of history variables
c               for user materials (/aux14loc/)
c
c     the default and the minumum number is 200
c     NHISVAR must be greater than or equal to 200
c
      integer    NHISVUE
      parameter (NHISVUE=100)
c
c     NHISVUE - maximum number of history variables 
c               for user elements (/bel8loc/)
c
c     the default and the minumum number is 100
c     NHISVUE must be greater than or equal to 100
c
      integer    NXDOFUE
      parameter (NXDOFUE=3)
c
c     NXDOFUE - maximum number of extra degrees of freedom 
c               for user elements (/ue1loc/)
c
c     the default and the minumum number is 3 
c     NXDOFUE must be greater than or equal to 3
c     ! maximum allowable number is NXDOFUE=15
c
