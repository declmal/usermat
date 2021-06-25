      integer igmprt,ngmprt,massout,iprtms,iprtlc,iprtop,lcoutp
      real prtims,prtlst,prtnxt
      integer iascii_file
      parameter (iascii_file=100)
      common/ptimes/igmprt,ngmprt,massout(10),
     1 prtims(iascii_file),prtlst(iascii_file),
     2 iprtms(iascii_file),iprtlc(iascii_file),
     3 prtnxt(iascii_file),iprtop(iascii_file),
     4 lcoutp(45)
c
c     ascii output control data
c
c     prtims-fixed ascii output intervals
c      ascii files:
c           ( 1)-SECFORC cross section forces
c           ( 2)-RWFORC rigid wall forces
c           ( 3)-NODOUT nodal data
c           ( 4)-ELOUT  element data
c           ( 5)-GLSTAT global data
c           ( 6)-DEFORC discrete elements
c           ( 7)-MATSUM material energies
c           ( 8)-NCFORC nodal interface forces
c           ( 9)-RCFORC resultant interface forces
c           (10)-DEFGEO smug animator
c           (11)-SPCFORC spc reaction forces
c           (12)-SWFORC nodal constraint resultant forces
c           (13)-ABSTAT airbag statistics
c           (14)-AVSFLT avs database
c           (15)-NODFOR nodal force groups
c           (16)-BNDOUT output intervals for nodal boundary conditions
c           (17)-RBDOUT
c           (18)-GCEOUT
c           (19)-MPGS
c           (20)-MOVIE
c           (21)-SLEOUT
c           (22)-SBTOUT
c           (23)-JNTFORC
c           (24)-TRHIST
c           (25)-DCFAIL
c           (26)-SPHOUT
c           (27)-H3OUT
c           (28)-mcol
c           (29)-mcolmod
c           (30)-mcolkine
c           (31)-NODOUT
c           (32)-TPRINT 
c           (33)-FSI
c           (34)-SSSTAT
c           (35)-MASSOUT
c           (36)-ELOUTDET
c           (37)-ATDOUT
c           (38)-BEPRES
c           (39)-BKSTAT
c           (40)-CURVOUT
c           (41)-SENSOR_CPM
c           (42)-ELOUT_SSD binout only
c           (43)-NODOUT_SSD binout only
c           (44)-ELOUT_SPCM
c           (45)-NODOUT_SPCM
c           (46)-ELOUT_PSD
c           (47)-NODOUT_PSD
c           (48)-DISBOUT
c           (49)-PLLYOUT
c           (50)-PBSSTAT
c           (51)-BEARINGOUT
c           (65)-PGSTAT
c           (66)-PGSNSR
c     prtlst-current ascii output time
c     prtnxt-next ascii output time
c     iprtms-output type  for ascii output
c     iprtlc-curve id foer ascii output interval
c     iprtop-option for ascii output time
c     lcoutp-misc. numbers
c     igmprt-flag for any ascii output 0-no 1-yes
c     ngmprt-number of ascii output files
c
