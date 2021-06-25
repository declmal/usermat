      integer iptadj,ipta2,ipta3,ipta4,ipta5,ipta6,ipta7,ipta8,
     . ipta9,ipta10,ipta11,ipta12,ipta13
      real optsz
      common/adjcom/iptadj,ipta2,ipta3,ipta4,ipta5,ipta6,ipta7,ipta8,
     . ipta9,ipta10,ipta11,ipta12,ipta13,optsz
      integer conparmdim
      parameter (conparmdim =29)
c
c ipta13 points to a chunk of memory that is dimensioned
c (comparmdim,numsv), to hold whatever contact related
c parameters need to be added as capabilities are added.
c
c
c  Current storage allocations:

c  1- 6 Used for the Contraction Joint capability.
c  7- 7 Flag to activate local coordinate system for slave box
c  8-16 Coordinate transformation matrix for slave box
c 17-17 Flag to activate local coordinate system for master box
c 18-26 Coordinate transformation matrix for master box
c 27-29 Transformed origin for master box
c
