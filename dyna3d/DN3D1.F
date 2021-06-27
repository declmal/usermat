      program dyna3d
c     implicit double precision (a-h,o-z)                                    dp
c
c***********************************************************************
c                                                                      *
c          ******   *     *  *     *    ***     *****   ******         *
c           *    *   *   *   **    *   *   *   *     *   *    *        *
c           *    *    * *    * *   *  *     *        *   *    *        *
c           *    *     *     *  *  *  *******     ***    *    *        *
c           *    *     *     *   * *  *     *        *   *    *        *
c           *    *     *     *    **  *     *  *     *   *    *        *
c          ******      *     *     *  *     *   *****   ******         *
c                                                                      *
c***********************************************************************
c                                                                      *
c     an explicit finite element code for dynamic analysis             *
c     in three dimensions                                              *
c                                                                      *
c     reference: r. g. whirley and j. o. hallquist, 'dyna3d: a         *
c                nonlinear, explicit, three-dimensional finite         *
c                element code for solid and structural mechanics,      *
c                user manual', university of california,               *
c                lawrence livermore national laboratory,               *
c                ucrl report, may, 1991                                *
c                                                                      *
c                                                                      *
c     copyright by j. o. hallquist  (1984)  all rights reserved        *
c     copyright by j. o. hallquist, d. j. benson  (1985)               *
c     copyright by j. o. hallquist, r. g. whirley  (1989)              *
c     copyright by r. g. whirley, j. o hallquist   (1991)              *
c                          all rights reserved                         *
c                                                                      *
c***********************************************************************
c                                                                      *
c                          p l e a s e   n o t e                       *
c                                                                      *
c                                                                      *
c             recipients of dyna3d are asked not to distribute         *
c             their source to other facilities.  rather, request for   *
c             the source should be directed to:                        *
c                                                                      *
c                 robert g. whirley                                    *
c                 methods development group, l-122                     *
c                 lawrence livermore national laboratory               *
c                 p. o. box 808                                        *
c                 livermore, california 94550                          *
c                                                                      *
c             phone: 415-423-0907                                      *
c                                                                      *
c                                                                      *
c***********************************************************************
c                                                                      *
c                          l e g a l   n o t i c e                     *
c                                                                      *
c                                                                      *
c             this computer code material was prepared as an account   *
c             of work sponsored by the united states government.       *
c             neither the united states nor the united states          *
c             department of energy, nor any of their employees,        *
c             nor any of their contractors, subcontractors, or their   *
c             employees, makes any warranty, express or implied, or    *
c             assumes any legal liability or responsibility for the    *
c             accuracy, completeness or usefulness of any information, *
c             apparatus, produce or process disclosed, or represents   *
c             that its use would not infringe privately-owned rights   *
c                                                                      *
c***********************************************************************
      character*8 vs                                                    vax75
      character*8 execut,date,namef                                     vax75
      logical large
      common/bigprb/large
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname,lendf
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/bk76/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax,gay,gaz,slx,sly,slz
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/thfl/madd,ifth,maddbg
      common/rigidb/ nodtot, ncflag,numrbi
      common/sbc0/yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal,shob,range(64),stoa(64),pkp(64)
      common/effort/number
      common/rn/irnxx
      common/vsnum/vs,date
      common/beam/ pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/tsbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),szeta(5,5)
      common/ssbsis/sh(8,5,5),spr(8,5,5),sps(8,5,5),spt(8,5,5),ipts,
     1 nips,swgts(5,5),sszeta(5,5)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/zname/execut(17)                                           vax75
      common/tmstp/tsmin,hmax,lctm
c     common/zname/execut(218)                                          cray1
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
c     integer*4 tt_chan,sys$assign                                      vms
c     common/ttchn/tt_chan                                              vms
c     common /aux2/   ax2zz(128,10)                                     cos
c     common /aux5/   ax5zz(128,55)                                     cos
c     common /aux00/  ax00zz(128,11)                                    cos
c     common /aux10/  ax10zz(128,50)                                    cos
c     common /aux11/  ax11zz(128,38)                                    cos
c     common /aux12/  ax12zz(128,27)                                    cos
c     common /aux13/  ax13zz(128,50)                                    cos
c     common /aux14/  ax14zz(128,96)                                    cos
c     common /aux15/  ax15zz(128,9)                                     cos
c     common /aux19/  ax19zz(128,12)                                    cos
c     common /aux20/  ax20zz(128,13)                                    cos
c     common /aux33/  ax33zz(128,12)                                    cos
c     common /aux40/  ax40zz(128,18)                                    cos
c     common /aux41/  ax41zz(128,5)                                     cos
c     common /aux43/  ax43zz(128,5)                                     cos
c     common /bk61/  bk61zz(24)                                         cos
      common/ortho/orthoc(50)
      common/auxstr/aux(700)
      common/auxjn/fctr(128,18)
      common/bmsc/numsl(20)
      common/drelax/ladd,idrint
      character*63 logo
      common/clogo/logo(10)
c
c     common /   / a(1)                                                 cray1
      common /   / a(1024000)                                           unics
c     common /   / a(500000)                                            vms
c     common /   / a(500000)                                            wkstn
      common /mesage/ nsym,mes1,mes2,mes3
c
c     define a 'heap' for the file buffers (minimum length = sum of
c     buffer lengths + 2*(number of buffers) +2).
c     note: 'hloc=heap' must be included in the input to ldr.
c
c     common/heap/bufrs(25100b)                                         cray1
c     common/stack/bozos(16100)                                         ltss
c
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      character*80 lnkarg                                               unix
      common/args/lnkarg,numargs                                        unix
      common/strhx/nmsi,matsi(3,400),mxix(9),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/double/iprec,ncpw,unit
      dimension itbuf(512)
c
      character*8 thfile
c
c     call link(execut,1,bufrs)                                         ltss
      call rdarg                                                        unix
      call link(execut)                                                 vaxcs
c     call idtous('print',6)                                            ltss
c     call q8ebm                                                        ltss
c
c     istat =sys$assign('tt',tt_chan,,)                                 vms
c     if (.not.istat) call lib$stop(%val(istat))                        vms
c     call enable_ctrlc                                                 vms
      call getnam(17,namef)                                             unix
      if (namef.eq.'xyzlgg') then                                       unix
c      call enablc                                                       unix
      endif                                                             unix
c
c     call chef                                                         ltss
c
      call timing (1)
c
      write(*,30) (logo(i),i=1,10),vs,date
      write ( *,40)                                                     vax75
      if (iprec.eq.2) write( *,35)
c
c     call timer(13,'ti      ',itbuf,512,'dyna3d  ',1)                  ltss
      call getnam  (4,thfile)
      if (thfile.eq.'hsp') then
           mkthf=0
      else
           mkthf=1
      endif
      call getnam  (3,namef)
      if (namef.eq.'rstxyz') go to 10
c
      nmsi=0
      call overly  (1)
      if (nmsi.ne.0) go to 10
c
      irestr=1
c
      go to 20
c
c     i n p u t    p h a s e
c
   10 call overly  (2)
c
      call timing (1)
c
c     i n i t i a l i z a t i o n   p h a s e
c
      call overly  (3)
c
c     f i n i t e   e l e m e n t
c
   20 call overly  (4)
c
      call timing (5)
c
      call adios (1)
c
   30 format(//,1x,70('*'),//,10(5x,a63,/),
     1   /10x,'dyna3d (version ',a8,')  compiled ',a8)
   35 format('  ** llnl double precision version **  ')
   40 format(//' on cray, sun, and vax computers note the following:'/, vax75
     &/,' ctrl-c interrupts dyna3d and prompts for a sense  ',          vax75
     &'switch.',/,' type the desired sense switch: sw1., sw2., etc.',   vax75
     &' to continue',/,' the execution.  dyna3d will respond as',       vax75
     &' explained in the users manual'//                                vax75
     & 3x,' type                      response'/                        vax75
     & 3x,'-----   ------------------------------------------------'    vax75
     &   ,'------------'/                                               vax75
     & 3x,'sw1.    a restart file is written and dyna3d terminates.'/   vax75
     & 3x,'sw2.    dyna3d responds with time and time step numbers.'/   vax75
     & 3x,'sw3.    a restart file is written and dyna3d continues '     vax75
     &   ,'calculations.'/                                              vax75
     & 3x,'sw4.    a plot state is written and dyna3d continues '       vax75
     &   ,'calculations.'///)                                           vax75
      end
      blockdata blkdat
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname,idummy
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/bk76/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/bk81/fold,scp
      common/thfl/madd,ifth,maddbg
      common/effort/number
      common/bttn/ntnwf,ixa(10)
      common/sbic/ijne,nblk
      common/bkfr/xfr(3,8)
      common/tmstp/tsmin,hmax,lctm
      common/eltspf/ietspf
      common/double/iprec,ncpw,unit
      character*8 vs                                                    vax75
      character*8 date,execut                                           vax75
      common/vsnum/vs,date
      common/bkspc/ jbase(7)
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/zname/execut(17)                                           vax75
      common/nwixa/nwcon
      common/zrray/maxa                                                 vax75
c     common/zname/execut(218)                                          cray1
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      character*6 elname
      common/elnme/ elname(5)
      common/strhx/nmsi,matsi(3,400),mx,ix(8),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/dltaph/ibtflg(128),idlflg
      common/sbc0/yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal,shob,range(64),stoa(64),pkp(64)
      common/visc/ihgqdf,hgqcdf,ibqsdf,bqs1df,bqs2df
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      character*63 logo
      common/clogo/logo(10)
      data logo(1) /
     1'*******    **    **  *      **    *****    *********  *******  '/
      data logo(2) /
     1'********   **    **  **     **   *******   ********   ******** '/
      data logo(3) /
     1' **   ***  **    **  ***    **  ***   ***       **     **   ***'/
      data logo(4) /
     1' **    **   **  **   ****   **  **     **      ***     **    **'/
      data logo(5) /
     1' **    **    ****    ** **  **  **     **     *****    **    **'/
      data logo(6) /
     1' **    **     **     **  ** **  *********        ***   **    **'/
      data logo(7) /
     1' **    **     **     **   ****  *********         **   **    **'/
      data logo(8) /
     1' **   ***     **     **    ***  **     **  ***   ***   **   ***'/
      data logo(9) /
     1'********      **     **     **  **     **   *******   ******** '/
      data logo(10)/
     1'*******       **     **      *  **     **    *****    *******  '/
      data ihgqdf,hgqcdf,ibqsdf,bqs1df,bqs2df/1,.10,1,1.44,.06/
      data bunits/304.8,.001,6.894757e+03/
      data ibtflg /128*0/
      data idump/0/
      data jdump/0/
c     data execut/'i=xyz123,unit1=(i,buf=4000b)'                        ltss
c    &,',m=meshfl,unit7=(m,abs,len=1600000b,buf=0000b)'                 ltss
c    &,',o=d3hsp,unit13=(o,hc,create,len=2000000b,buf=4000b)'           ltss
c    &,',p=forces,unit12=(p,hc,create,len=2000000b,buf=4000b)'          ltss
c    &,',f=d3thdt,unit4=(f,abs,len=1600000b,buf=0000b)'                 ltss
c    &,',r=rstxyz,unit3=(r,abs,len=1600000b,buf=0000b)'                 ltss
c    &,',g=d3plot,unit5=(g,abs,len=1600000b,buf=0000b)'                 ltss
c    &,',b=d3drlf,unit39=(b,abs,len=1600000b,buf=0000b)'                ltss
c    &,',a=runrst,unit14=(a,abs,len=1000000b,buf=0000b)'                ltss
c    &,',u=infmak,unit15=(u,abs,len=1000000b,buf=0000b)'                ltss
c    &,',v=r$$$$$,unit16=(v,abs,len=1000000b,buf=0000b)'                ltss
c    &,',d=d3dump,unit2=(d,abs,len=1000000b,buf=0000b)'                 ltss
c    &,',j=joyfil,unit10=(j,abs,len=1000000b,buf=0000b)'                ltss
c    &,',unit8=(disk8,abs,len=1000000b,buf=0000b,destroy=on)'           ltss
c    &,',s=disk9,unit9=(s,abs,len=1600000b,buf=0000b)'                  ltss
c    &,',t=thrmfl,unit11=(t,abs,len=1000000b,buf=0000b)//'/             ltss
      data execut/'xyz123','d3dump','rstxyz','d3thdt','d3plot',         vaxcs
     1            'd3hsp' ,'meshfl','disk8' ,'disk9' ,'joyfil',         vaxcs
     2       'thrmfl','forces','d3drlf','runrsf','infmak','r$$$$$',     vaxcs
     3         'xyzlgg'/                                                vaxcs
c     data ioerr,iob2,iob3,iob4,iob5,iob8,iob9,iob11/0,2,3,4,5,8,9,11/  ltss
c     data iob7,iob14,iob15,iob16/7,14,15,16/                           ltss
c     data iob6/39/                                                     ltss
c
      data elname/'solid',' beam','shell','spring','tshell'/
      data lctm,hmax/0,1.0e20/
      data ffifp/.false./,nadd/0/
c     data nwcon/3/                                                     cray1
      data nwcon/9/                                                     vax75
      data nint/0/
      data ifth/0/
      data iprec,ncpw /1,8/
c     data iprec,ncpw /2,4/                                                 dp
      data unit /1.0/
      data ietspf/0/
      data irdeck/0/
      data iname/0/
      data ncycle/0/
      data tt/0.0/
      data idrflg/0/
      data jbase/100000,10000,1000,100,10,1,1000000/
c     data maxsiz/1600000b/,iadd/0/,irestr/0/,madd/0/                   ltss
c     data maxsiz/7000000b/,iadd/0/,irestr/0/,madd/0/                   cos
      data maxsiz/  2621440 /,iadd/0/,irestr/0/,madd/0/                 unics
c     data maxsiz/  262144  /,iadd/0/,irestr/0/,madd/0/                 wkstn
c     data maxsiz/  262144  /,iadd/0/,irestr/0/,madd/0/                 vms
      data nconst/0,17,5,3,0,6,12,3,3,4,4,0,1,2,5,6,10,0,2,0,20,5,2,0,2,
     1 0,12,5,5,9,12,18,1,0,5,25*0/
      data ncneos/0,0,0,0,0,0,8,1,0,0,1,0,0,0,0/
      data vs/'2.0.1   '/,date/' 6/25/91'/
      data njoydf,nwdadd/10,0/
c     data number/3r682/                                                cray1
      data maxa/1024000/
      data nmsi/0/
      data dt2/0.0/
      end
c     subroutine enable_ctrlc                                           vms
c     implicit double precision (a-h,o-z)                               vms  dp
c     integer*4 tt_chan,sys$qiow,dvi$_devclass,item_list(3),            vms
c    1 sys$getdvi,dc$k_term                                             vms
c     common/ttchn/tt_chan                                              vms
c     external ctrlc_rout                                               vms
c     parameter (dvi$_devclass=4)                                       vms
c     parameter (dc$k_term=66)                                          vms
c     include '($iodef)'                                                vms
c     ibuf=0                                                            vms
c     ilen=0                                                            vms
c     item_list(1)=ior(ishft(dvi$_devclass,16),4)                       vms
c     item_list(2)=%loc(ibuf)                                           vms
c     item_list(3)=%loc(ilen)                                           vms
c     istat=sys$getdvi(,,'sys$input',item_list,,,,)                     vms
c     if(ibuf.ne.dc$k_term) return                                      vms
c     if(.not. istat) call lib$stop(%val(istat))                        vms
c     istat=sys$qiow(,%val(tt_chan),%val(io$_setmode.or.io$m_ctrlcast), vms
c    1 ,,,ctrlc_rout,,%val(3),,,)                                       vms
c     if(.not. istat) call lib$stop(%val(istat))                        vms
c     return                                                            vms
c     end                                                               vms
c     subroutine ctrlc_rout                                             vms
c     implicit double precision (a-h,o-z)                               vms  dp
c     character*4 msg                                                   vms
c     common/mssg/msg                                                   vms
c     character*1 letter                                                vms
c     dimension letter(80)                                              vms
c     data msg/'    '/                                                  vms
c     call enable_ctrlc                                                 vms
c     write( *,40)                                                      vms
c     read ( *,30)(letter(i),i=1,80)                                    vms
c     do 10 i=1,80                                                      vms
c  10 call lwrcas(letter(i),letter(i))                                  vms
c     do 20 i=1,80                                                      vms
c     if (letter(i).eq.' ') go to 20                                    vms
c     letter(i+3)='.'                                                   vms
c     msg=letter(i)//letter(i+1)//letter(i+2)//letter(i+3)              vms
c     return                                                            vms
c  20 continue                                                          vms
c     return                                                            vms
c  30 format(80a1)                                                      vms
c  40 format($,1x,'enter sense switch:')                                vms
c     end                                                               vms
      subroutine intrup (mess,nwr,i)                                    vax75
      character*4 mess,msg                                              vax75
      common/mssg/msg                                                   vax75
      mess=msg                                                          vax75
      msg='    '                                                        vax75
      return                                                            vax75
      end                                                               vax75
c      subroutine enablc                                                 unix
c      external ctrlco                                                   unix
c      integer signum,flag                                               unix
c      integer signal                                                    unix
c      signum=2                                                          unix
c      flag  =-1                                                         unix
c     inum  =signal(signum,ctrlco,flag)                                 wkstn
c      call fsigctl('REGISTER','SIGINT',ctrlco)                          unics
c      return                                                            unix
c      end                                                               unix
      subroutine lwrcas(dest,sour)                                      vax75
c     implicit double precision (a-h,o-z)                               vax75dp
      character*1 dest,sour                                             vax75
      data iauc/65/,izuc/90/,idist/32/                                  vax75
      is=ichar(sour)                                                    vax75
      if (is.ge.iauc.and.is.le.izuc) then                               vax75
      id=is+idist                                                       vax75
      dest=char(id)                                                     vax75
      else                                                              vax75
      dest=sour                                                         vax75
      endif                                                             vax75
      return                                                            vax75
      end                                                               vax75
      subroutine ctrlco(signum)                                         unix
c     implicit double precision (a-h,o-z)                               unix dp
      integer signum                                                    unix
      character*4 msg                                                   unix
      common/mssg/msg                                                   unix
      character*1 letter                                                unix
      dimension letter(80)                                              unix
      save iopen                                                        unix
      data msg/'    '/                                                  unix
      write( *,41)                                                      unix
      write( *,40)                                                      unix
      read ( *,30)(letter(i),i=1,50)                                    unix
c
c      call enablc                                                       unix
c
      do 20 i=1,46                                                      unix
      if (letter(i).eq.' ') go to 20                                    unix
      letter(i+3)='.'                                                   unix
      msg=letter(i)//letter(i+1)//letter(i+2)//letter(i+3)              unix
      if (msg.eq.'sto.'.or.msg.eq.'qui.') then                          unix
      call adios(1)                                                     unix
      endif                                                             unix
      return                                                            unix
   20 continue                                                          unix
      return                                                            unix
   30 format(80a1)                                                      unix
   40 format(1x,'.enter sense switch:',$)                               unix
   41 format(2x,/)                                                      unix
      end                                                               unix
      subroutine overly (novl)
      if(novl.eq.1) call rstart
      if(novl.eq.2) call dynai
      if(novl.eq.3) call intial
      if(novl.eq.4) call soltn
      return
      end
      subroutine adios (n)
c     implicit double precision (a-h,o-z)                                    dp
c
c     terminate solution
c
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk28/summss,xke,xpe,tt
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      character*8 keep                                                  vaxcs
      keep='keep'                                                       vaxcs
c
      call rwabsf (iob2,keep,0,0,0)                                     vaxcs
      call rwabsf (iob4,keep,0,0,0)                                     vaxcs
      call rwabsf (iob5,keep,0,0,0)                                     vaxcs
      call rwabsf (iob14,keep,0,0,0)                                    vaxcs
c
      if (n.eq.1) write(13,20)
      if (n.eq.2) write(13,30)
      if (n.eq.2) write ( *,30)
      if (n.eq.1) write ( *,20)
c
      call timing (5)
c     call timend                                                       ltss
c
      do 10 i=1,5
      time(1,6)=time(1,6)+time(1,i)
   10 time(2,6)=time(2,6)+time(2,i)
c
      write(13,40) ((time(i,j),i=1,2),j=1,6)
      write ( *,50) tt
      write ( *,60) ncycle,time(1,6),time(2,6)
      write(13,50) tt
      write(13,60) ncycle,time(1,6),time(2,6)
c
      close (unit=1,status='keep')                                      unix
      close (unit=12,status='keep')                                     unix
      close (unit=13,status='keep')                                     unix
      close (unit=98,status='delete')                                   unix
c     call exita(idim(n,1))                                             ltss
      stop                                                              vaxcs
c
c
   20 format(/' n o r m a l    t e r m i n a t i o n')
   30 format(/' e r r o r   t e r m i n a t i o n'//)
   40 format('1',///' t i m i n g   i n f o r m a  t i o n'//
     1 60x,'cpu(sec)       io(sec)                       '//
     2 5x,'initialization ...................................',2e14.4//
     3 5x,'solution (excl. slidelines).......................',2e14.4//
     4 5x,'write binary database for plotting ...............',2e14.4//
     5 5x,'write high speed printer file.....................',2e14.4//
     6 5x,'slideline algorithm ..............................',2e14.4//
     7 5x,'t o t a l s                                       ',2e14.4//)
   50 format(//' problem time =',e14.4)
   60 format(' number of time steps=',i6,' cpu=',e14.4,'  io=',e14.4,/)
      end
      subroutine expndm(n)
c     implicit double precision (a-h,o-z)                                    dp
c
      common /   / b(1000)
      data lngthm/1/
c
c     expand large core memory
c
      lmin=min0(lngthm,n)
c     if (n-lngthm.ne.0) call memory (b(lmin),n-lngthm)                 ltss
c     if (n-lngthm.ne.0) call memory (b(lmin),n-lngthm)                 vms
c     if (n-lngthm.ne.0) call memory (b(lmin),n-lngthm)                 wkstn
c     if (n-lngthm.ne.0) call mmemory(b(lmin),n-lngthm)                 cos
      if (n-lngthm.ne.0) call mmemory(b(lmin),n-lngthm)                 unics
      lngthm=n
c
      return
      end
      subroutine timing (k)
c     implicit double precision (a-h,o-z)                                    dp
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      dimension timusd(4)
      data tcold/0.0/,tiold/0.0/
c      tall=timuse(timusd)
      tcpu=timusd(1)
      tio=timusd(2)
      if (tcold.eq.0.) go to 10
      time(1,k)=time(1,k)+tcpu-tcold
      time(2,k)=time(2,k)+tio-tiold
   10 tcold=tcpu
      tiold=tio
      return
      end
      subroutine tied2(x,a,e,xmsm,irect,lmsr,crst,msr,
     1                 nsv,iloc,irtl,tcode,v,xms)
c     implicit double precision (a-h,o-z)                                    dp
c
c     tied interface logic
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint
      dimension x(3,*),a(*),e(*),xmsm(*),crst(2,*),
     1          irect(4,*),lmsr(4,*),msr(*),nsv(*),iloc(*),irtl(*),
     2          tcode(*),v(*),xms(*)
c
c     initialize mass matrix
c
      do 10 i=1,nmn
      j=msr(i)
      i3=3*i
      j3=3*j
      i2=i3-1
      j2=j3-1
      i1=i2-1
      j1=j2-1
      xmsj=1.0/xms(j)
      xmsm(i1)=xmsj
      xmsm(i2)=xmsj
      xmsm(i3)=xmsj
      e(i1)=a(j1)*xmsj
      e(i2)=a(j2)*xmsj
   10 e(i3)=a(j3)*xmsj
c
      do 50 ii=1,nsn
c
c     calculate s,t
c
      i=nsv(ii)
      j=iloc(ii)
c
      if (j.lt.1) go to 50
      k=msr(j)
      l=irtl(ii)
      do 20 jj=1,4
      nn=irect(jj,l)
      iy(jj)=nn
      nn=msr(nn)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   20 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
c
      ierr=0
      iopt=1
      ss=crst(1,ii)
      tt=crst(2,ii)
c
      if (nint.gt.0) go to 30
      call st (xn1,xn2,xn3,ss,tt,ierr)
      crst(1,ii)=ss
      crst(2,ii)=tt
      if (ierr.eq.1) iloc(ii)=-1
      if (ierr.eq.1) go to 50
c
   30 i3=3*i
      i2=i3-1
      i1=i2-1
c
c     tied sliding
c
      xmsi=1.0/xms(i)
      fxi=a(i1)*xmsi
      fyi=a(i2)*xmsi
      fzi=a(i3)*xmsi
      an1=xmsi
      an2=xmsi
      an3=xmsi
c
c     update force and mass vectors
c
      call shapef (h,ss,tt)
      do 40 jj=1,4
      j3=3*iy(jj)
      j2=j3-1
      j1=j2-1
      e(j1)=e(j1)+fxi*h(jj)
      e(j2)=e(j2)+fyi*h(jj)
      e(j3)=e(j3)+fzi*h(jj)
      xmsm(j1)=xmsm(j1)+an1*h(jj)
      xmsm(j2)=xmsm(j2)+an2*h(jj)
   40 xmsm(j3)=xmsm(j3)+an3*h(jj)
   50 continue
c
      return
c
      end
      subroutine tied3(x,bcs,a,e,xmsm,irect,lmsr,crst,msr,nsv,
     1                  iloc,irtl,tcode,v)
c     implicit double precision (a-h,o-z)                                    dp
c
c     t i e d - i n t e r f a c e   l o g i c
c
c        1. compute accelerations of all master nodes
c        2. compute accelerations of all constrained slave nodes
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
c
      dimension x(3,*),bcs(4,*),a(*),e(*),xmsm(*),irect(4,*),
     1          lmsr(4,*),msr(*),nsv(*),iloc(*),irtl(*),crst(2,*),
     2          tcode(*),v(*)
c
c     calculate accelerations of master nodes
c
      do 10 i=1,nmn
      j=msr(i)
      i3=3*i
      j3=3*j
      i2=i3-1
      j2=j3-1
      i1=i2-1
      j1=j2-1
      a(j1)=e(i1)/xmsm(i1)
      a(j2)=e(i2)/xmsm(i2)
      a(j3)=e(i3)/xmsm(i3)
      ib=tcode(j)
      if (ib.ne.0) call bound (bcs,a(j1),a(j2),a(j3),ib)
   10 continue
c
      do 40 ii=1,nsn
c
      i=nsv(ii)
      j=iloc(ii)
      if (j.lt.1) go to 40
      k=msr(j)
      l=irtl(ii)
      do 20 jj=1,4
      nn=msr(irect(jj,l))
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   20 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
c
c     calculate components of unit normal vector at contact point
c
      ss=crst(1,ii)
      tt=crst(2,ii)
      call shapef (h,ss,tt)
      i3=3*i
      i2=i3-1
      i1=i2-1
      amx=0.0
      amy=0.0
      amz=0.0
      vmx=0.0
      vmy=0.0
      vmz=0.0
c
c     compute master segment velocity and acceleration at contact
c     point
c
      do 30 jj=1,4
      j3=3*ix(jj)
      j2=j3-1
      j1=j2-1
      amx=amx+a(j1)*h(jj)
      amy=amy+a(j2)*h(jj)
      amz=amz+a(j3)*h(jj)
      vmx=vmx+v(j1)*h(jj)
      vmy=vmy+v(j2)*h(jj)
   30 vmz=vmz+v(j3)*h(jj)
      a(i1)=amx
      a(i2)=amy
      a(i3)=amz
      v(i1)=vmx
      v(i2)=vmy
      v(i3)=vmz
c
   40 continue
      return
      end
      subroutine tiedr2(x,at,ar,et,er,xmsmt,xmsmr,irect,
     1  lmsr,crst,msr,nsv,iloc,irtl,tcode,rcode,vt,vr,xmst,xmsr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     tied interface logic for shell edges onto surfaces
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint
      dimension
     & x(3,*),at(*),ar(*),et(*),er(*),xmsmt(*),xmsmr(*),crst(2,*),
     & irect(4,*),lmsr(4,*),msr(*),nsv(*),iloc(*),irtl(*),
     & tcode(*),rcode(*),vt(*),vr(*),xmst(*),xmsr(*)
c
c     initialize mass matrix
c
      do 10 i=1,nmn
      j=msr(i)
      i3=3*i
      j3=3*j
      i2=i3-1
      j2=j3-1
      i1=i2-1
      j1=j2-1
      xmsj=1.0/xmst(j)
      xmsmt(i1)=xmsj
      xmsmt(i2)=xmsj
      xmsmt(i3)=xmsj
      et(i1)=at(j1)*xmsj
      et(i2)=at(j2)*xmsj
      et(i3)=at(j3)*xmsj
      xmsj=1.0/xmsr(j)
      xmsmr(i1)=xmsj
      xmsmr(i2)=xmsj
      xmsmr(i3)=xmsj
      er(i1)=ar(j1)*xmsj
      er(i2)=ar(j2)*xmsj
   10 er(i3)=ar(j3)*xmsj
c
      do 50 ii=1,nsn
c
c     calculate s,t
c
      i=nsv(ii)
      j=iloc(ii)
c
      if (j.lt.1) go to 50
      k=msr(j)
      l=irtl(ii)
      do 20 jj=1,4
      nn=irect(jj,l)
      iy(jj)=nn
      nn=msr(nn)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   20 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
c
      ierr=0
      iopt=1
      ss=crst(1,ii)
      tt=crst(2,ii)
c
      if (nint.gt.0) go to 30
      call st (xn1,xn2,xn3,ss,tt,ierr)
      crst(1,ii)=ss
      crst(2,ii)=tt
      if (ierr.eq.1) iloc(ii)=-1
      if (ierr.eq.1) go to 50
c
   30 i3=3*i
      i2=i3-1
      i1=i2-1
c
c     tied sliding
c
      xmsi=1.0/xmst(i)
      fxi=at(i1)*xmsi
      fyi=at(i2)*xmsi
      fzi=at(i3)*xmsi
      an1=xmsi
      an2=xmsi
      an3=xmsi
      xmsi=1.0/xmsr(i)
      gxi=ar(i1)*xmsi
      gyi=ar(i2)*xmsi
      gzi=ar(i3)*xmsi
      bn1=xmsi
      bn2=xmsi
      bn3=xmsi
c
c     update force and mass vectors
c
      sp=1.0+ss
      sm=1.0-ss
      tp=.25*(1.0+tt)
      tm=.25*(1.0-tt)
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      do 40 jj=1,4
      j3=3*iy(jj)
      j2=j3-1
      j1=j2-1
      et(j1)=et(j1)+fxi*h(jj)
      et(j2)=et(j2)+fyi*h(jj)
      et(j3)=et(j3)+fzi*h(jj)
      xmsmt(j1)=xmsmt(j1)+an1*h(jj)
      xmsmt(j2)=xmsmt(j2)+an2*h(jj)
      xmsmt(j3)=xmsmt(j3)+an3*h(jj)
      er(j1)=er(j1)+gxi*h(jj)
      er(j2)=er(j2)+gyi*h(jj)
      er(j3)=er(j3)+gzi*h(jj)
      xmsmr(j1)=xmsmr(j1)+bn1*h(jj)
      xmsmr(j2)=xmsmr(j2)+bn2*h(jj)
   40 xmsmr(j3)=xmsmr(j3)+bn3*h(jj)
   50 continue
c
      return
c
      end
      subroutine tiedr3(x,bcs,at,ar,et,er,xmsmt,xmsmr,
     1  irect,lmsr,crst,msr,nsv,iloc,irtl,tcode,rcode,vt,vr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     tied interface logic for tying shell edges to surfaces
c
c        1. compute accelerations of all master nodes
c        2. compute accelerations of all constrained slave nodes
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
c
      dimension x(3,*),bcs(4,*),at(*),ar(*),et(*),er(*),xmsmt(*),
     1   irect(4,*),lmsr(4,*),msr(*),nsv(*),iloc(*),irtl(*),crst(2,*),
     2   tcode(*),rcode(*),vt(*),vr(*),xmsmr(*)
c
c     calculate accelerations of master nodes
c
      do 10 i=1,nmn
      j=msr(i)
      i3=3*i
      j3=3*j
      i2=i3-1
      j2=j3-1
      i1=i2-1
      j1=j2-1
      at(j1)=et(i1)/xmsmt(i1)
      at(j2)=et(i2)/xmsmt(i2)
      at(j3)=et(i3)/xmsmt(i3)
      ar(j1)=er(i1)/xmsmr(i1)
      ar(j2)=er(i2)/xmsmr(i2)
      ar(j3)=er(i3)/xmsmr(i3)
      ib=tcode(j)
      if (ib.ne.0) call bound (bcs,at(j1),at(j2),at(j3),ib)
      ib=rcode(j)
      if (ib.ne.0) call bound (bcs,ar(j1),ar(j2),ar(j3),ib)
   10 continue
c
      do 40 ii=1,nsn
c
      i=nsv(ii)
      j=iloc(ii)
      if (j.lt.1) go to 40
      k=msr(j)
      l=irtl(ii)
      do 20 jj=1,4
      nn=msr(irect(jj,l))
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   20 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
c
c     calculate components of unit normal vector at contact point
c
      ss=crst(1,ii)
      tt=crst(2,ii)
      sp=1.0+ss
      sm=1.0-ss
      tp=.25*(1.0+tt)
      tm=.25*(1.0-tt)
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      i3=3*i
      i2=i3-1
      i1=i2-1
      amx=0.0
      amy=0.0
      amz=0.0
      vmx=0.0
      vmy=0.0
      vmz=0.0
      anx=0.0
      any=0.0
      anz=0.0
      vnx=0.0
      vny=0.0
      vnz=0.0
c
c     compute master segment velocity and acceleration at contact
c     point
c
      do 30 jj=1,4
      j3=3*ix(jj)
      j2=j3-1
      j1=j2-1
      amx=amx+at(j1)*h(jj)
      amy=amy+at(j2)*h(jj)
      amz=amz+at(j3)*h(jj)
      vmx=vmx+vt(j1)*h(jj)
      vmy=vmy+vt(j2)*h(jj)
      vmz=vmz+vt(j3)*h(jj)
      anx=anx+ar(j1)*h(jj)
      any=any+ar(j2)*h(jj)
      anz=anz+ar(j3)*h(jj)
      vnx=vnx+vr(j1)*h(jj)
      vny=vny+vr(j2)*h(jj)
   30 vnz=vnz+vr(j3)*h(jj)
      at(i1)=amx
      at(i2)=amy
      at(i3)=amz
      vt(i1)=vmx
      vt(i2)=vmy
      vt(i3)=vmz
      ar(i1)=anx
      ar(i2)=any
      ar(i3)=anz
      vr(i1)=vnx
      vr(i2)=vny
      vr(i3)=vnz
c
   40 continue
      return
      end
      subroutine slave1(x,irect,lmsr,msr,nsv,iloc,irtl,nseg,nsn,nmn)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
c
      dimension x(3,1),irect(4,1),lmsr(1),msr(1),nsv(1),
     1          iloc(1),irtl(1),nseg(1)
c
      do 170 ii=1,nsn
      knew=0
      i=nsv(ii)
      j=iloc(ii)
      k=msr(j)
      cms=(x(1,i)-x(1,k))**2+(x(2,i)-x(2,k))**2+(x(3,i)-x(3,k))**2
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      do 70 jj=1,num
      ll=lmsr(npt+jj)
      if (k.ne.irect(1,ll)) go to 10
      l=irect(2,ll)
      m=irect(3,ll)
      n=irect(4,ll)
      go to 40
   10 if (k.ne.irect(2,ll)) go to 20
      l=irect(1,ll)
      m=irect(3,ll)
      n=irect(4,ll)
      go to 40
   20 if (k.ne.irect(3,ll)) go to 30
      l=irect(1,ll)
      m=irect(2,ll)
      n=irect(4,ll)
      go to 40
   30 if (k.ne.irect(4,ll)) go to 180
      l=irect(1,ll)
      m=irect(2,ll)
      n=irect(3,ll)
   40 dms=(x(1,i)-x(1,l))**2+(x(2,i)-x(2,l))**2+(x(3,i)-x(3,l))**2
      ems=(x(1,i)-x(1,m))**2+(x(2,i)-x(2,m))**2+(x(3,i)-x(3,m))**2
      fms=(x(1,i)-x(1,n))**2+(x(2,i)-x(2,n))**2+(x(3,i)-x(3,n))**2
      if (dms.gt.cms) go to 50
      cms=dms
      knew=l
   50 if (ems.gt.cms) go to 60
      cms=ems
      knew=m
   60 if (fms.gt.cms) go to 70
      cms=fms
      knew=n
   70 continue
      if (knew.ne.0) k=knew
      if (k.eq.msr(j)) go to 90
      do 80 jj=1,nmn
      if (msr(jj).ne.k) go to 80
      iloc(ii)=jj
      go to 90
   80 continue
   90 j=iloc(ii)
      l=irtl(ii)
      resltl=-10.e+20
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      if (l.eq.0) go to 120
      llc=l
      do 100 jj=1,4
      if (irect(jj,l).ne.k) go to 100
      j1=jj+1
      j2=jj-1
      if (jj.ne.3) go to 110
      if (irect(3,l).eq.irect(4,l)) j1=1
      go to 110
  100 continue
      l=0
      go to 120
  110 if (j1.eq.5) j1=1
      if (j2.eq.0) j2=4
      m=irect(j1,l)
      n=irect(j2,l)
      call cross (x)
      if (result.gt.0.0) go to 170
  120 do 160 jj=1,num
      ll=lmsr(jj+npt)
      llc=ll
      if (l.eq.ll) go to 160
      do 130 kk=1,4
      if (irect(kk,ll).ne.k) go to 130
      k1=kk+1
      k2=kk-1
      if (kk.ne.3) go to 140
      if (irect(3,ll).eq.irect(4,ll)) k1=1
      go to 140
  130 continue
  140 if (k1.eq.5) k1=1
      if (k2.eq.0) k2=4
      m=irect(k1,ll)
      n=irect(k2,ll)
      call cross (x)
      if (result) 160,160,150
  150 irtl(ii)=ll
      go to 170
  160 continue
      irtl(ii)=lrsort
  170 continue
      return
  180 write ( *,190)
      write(13,190)
      call adios (2)
  190 format('fatal error in subroutine slave1')
      end
      subroutine slave2(x,e,irect,lmsr,msr,nsv,iloc,irtl,stf,
     1 nsn,nmn,nty,fdat,iseg,fric,nseg,thk)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/bk28/summss,xke,xpe,timx
      common/slv2/thks,isrch
      common/double/iprec,ncpw,unit
      dimension x(3,*),e(*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
     1          irtl(*),stf(*),fdat(5,*),iseg(*),fric(*),thk(*)
      fcoeff=fric(1)**2+fric(2)**2+fric(3)**2
      do 100 ii=1,nsn
      i=nsv(ii)
      j=iloc(ii)
      k=msr(j)
      l=irtl(ii)
      do 10 jj=1,4
      nn=irect(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   10 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
      if (k.ne.ix(1)) go to 20
      k1=1
      k2=2
      k3=4
      go to 50
   20 if (k.ne.ix(2)) go to 30
      k1=2
      k2=3
      k3=1
      go to 50
   30 if (k.ne.ix(3)) go to 40
      k1=3
      k2=4
      k3=2
      if (ix(3).eq.ix(4)) k2=1
      go to 50
   40 if (k.ne.ix(4)) go to 50
      k1=4
      k2=1
      k3=3
   50 call ptime (k1,k2,k3,detv)
      if (detv.gt.0.) go to 80
      ierr=0
      i3=3*i
      i2=i3-1
      i1=i2-1
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      thkl=thk(l)
      thkl=thks
      if (isrch.eq.1) call thkgs(xx1,xx2,xx3,thkl)
      ans=xn1*(xs1-amx)+xn2*(ys1-amy)+xn3*(zs1-amz)
      if (ierr.eq.0.and.ans.gt.0.) go to 80
      if ((ierr.ne.0.and.  max(abs(ss),abs(tt)).le.5.).or.
     1    ans.lt.-thkl)
     1 call salvex(ans,i,ierr,xn1,xn2,xn3,ss,tt,l,x,irect,lmsr,
     1             msr,nseg,nmn)
      if (ierr.ne.0.or.ans.gt.0..or.ans.lt.-thkl) go to 80
   60 fni=ans*stf(l)
      fxi=xn1*fni
      fyi=xn2*fni
      fzi=xn3*fni
      if (fcoeff.eq.0.0) go to 68
      jj=iseg(ii)
      if (jj.ne.0) go to 65
      iseg(ii)=l
      fdat(1,ii)=ss
      fdat(2,ii)=tt
      go to 68
   65 tp=.25*(1.0+fdat(2,ii))
      tm=.25*(1.0-fdat(2,ii))
      sp=1.0+fdat(1,ii)
      sm=1.0-fdat(1,ii)
      h1=tm*sm
      h2=tm*sp
      h3=tp*sp
      h4=tp*sm
      nn1=irect(1,jj)
      nn2=irect(2,jj)
      nn3=irect(3,jj)
      nn4=irect(4,jj)
      dx=amx-h1*x(1,nn1)-h2*x(1,nn2)-h3*x(1,nn3)-h4*x(1,nn4)
      dy=amy-h1*x(2,nn1)-h2*x(2,nn2)-h3*x(2,nn3)-h4*x(2,nn4)
      dz=amz-h1*x(3,nn1)-h2*x(3,nn2)-h3*x(3,nn3)-h4*x(3,nn4)
      vel=sqrt(dx**2+dy**2+dz**2)/  max(1.e-20*unit,dt2)
      fdat(3,ii)=fdat(3,ii)+stf(l)*dx
      fdat(4,ii)=fdat(4,ii)+stf(l)*dy
      fdat(5,ii)=fdat(5,ii)+stf(l)*dz
      proj=fdat(3,ii)*xn1+fdat(4,ii)*xn2+fdat(5,ii)*xn3
      fdat(3,ii)=fdat(3,ii)-proj*xn1
      fdat(4,ii)=fdat(4,ii)-proj*xn2
      fdat(5,ii)=fdat(5,ii)-proj*xn3
      fmax=-(fric(2)+(fric(1)-fric(2))*exp(-fric(3)*vel))*fni
      fmag=sqrt(fdat(3,ii)**2+fdat(4,ii)**2+fdat(5,ii)**2)
      if (fmax.ge.fmag.or.fmag.eq.0.) go to 67
      sclf=fmax/fmag
      fdat(3,ii)=sclf*fdat(3,ii)
      fdat(4,ii)=sclf*fdat(4,ii)
      fdat(5,ii)=sclf*fdat(5,ii)
   67 fxi=fxi+fdat(3,ii)
      fyi=fyi+fdat(4,ii)
      fzi=fzi+fdat(5,ii)
      iseg(ii)=l
      fdat(1,ii)=ss
      fdat(2,ii)=tt
   68 do 70 jj=1,4
      j3=3*ix(jj)
      j2=j3-1
      j1=j2-1
      e(j1)=e(j1)+fxi*h(jj)
      e(j2)=e(j2)+fyi*h(jj)
      e(j3)=e(j3)+fzi*h(jj)
   70 continue
      e(i1)=e(i1)-fxi
      e(i2)=e(i2)-fyi
      e(i3)=e(i3)-fzi
      go to 100
   80 if (fcoeff.eq.0.0) go to 100
      iseg(ii)=0
      fdat(3,ii)=0.
      fdat(4,ii)=0.
      fdat(5,ii)=0.
  100 continue
      return
      end
      subroutine ssslv3(ilb,ierr,xn1,xn2,xn3,ss,tt,
     1      l,x,irect,lmsr,msr,nseg,thic,iyy,anv1,anv2,anv3)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/shls/unit,en1,en2,en3,thkn,ilbn
      common/double/iprec,ncpw,xunit
      dimension ixx(4),x(3,*),irect(4,*),lmsr(*),msr(*),nseg(*),
     1 anv1(*),anv2(*),anv3(*),jlist(4,4),ilist(4),thic(*),iyy(4)
      data jlist/1,2,4,3,4,1,3,2,2,1,3,4,4,2,3,1/
      isig=sign(1.*xunit,ss)+1.
      jsig=sign(1.*xunit,tt)+1.
      ipic=isig+jsig/2+1
      ilist(1)=irect(1,l)
      ilist(2)=irect(2,l)
      ilist(3)=irect(3,l)
      ilist(4)=irect(4,l)
      do 60 ii=1,4
      j=ilist(jlist(ii,ipic))
      k=msr(j)
      resltl=-1.e20
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      do 30 lpp=1,num
      ll=lmsr(lpp+npt)
      llc=ll
      do 10 jj=1,4
      if (irect(jj,ll).ne.j) go to 10
      j1=jj+1
      j2=jj-1
      if (jj.ne.3) go to 20
      if (irect(3,ll).eq.irect(4,ll)) j1=1
      go to 20
   10 continue
   20 if (j1.eq.5) j1=1
      if (j2.eq.0) j2=4
      m=msr(irect(j1,ll))
      n=msr(irect(j2,ll))
      call cross (x)
      if (result.le.0.0) go to 30
      l=ll
      go to 40
   30 continue
      l=lrsort
   40 do 50 jj=1,4
      nn=msr(irect(jj,l))
      iyy(jj)=irect(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   50 xx3(jj)=x(3,nn)
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.gt.0.0) then
      delta=.5*thic(iyy(1))
      xx1(1)=xx1(1)+delta*anv1(iyy(1))
      xx2(1)=xx2(1)+delta*anv2(iyy(1))
      xx3(1)=xx3(1)+delta*anv3(iyy(1))
      delta=.5*thic(iyy(2))
      xx1(2)=xx1(2)+delta*anv1(iyy(2))
      xx2(2)=xx2(2)+delta*anv2(iyy(2))
      xx3(2)=xx3(2)+delta*anv3(iyy(2))
      delta=.5*thic(iyy(3))
      xx1(3)=xx1(3)+delta*anv1(iyy(3))
      xx2(3)=xx2(3)+delta*anv2(iyy(3))
      xx3(3)=xx3(3)+delta*anv3(iyy(3))
      delta=.5*thic(iyy(4))
      xx1(4)=xx1(4)+delta*anv1(iyy(4))
      xx2(4)=xx2(4)+delta*anv2(iyy(4))
      xx3(4)=xx3(4)+delta*anv3(iyy(4))
      endif
      xs1=x(1,ilbn)
      ys1=x(2,ilbn)
      zs1=x(3,ilbn)
      if (thkn.gt.0.0) then
      ut=.5*thkn
      xs1=xs1+ut*en1
      ys1=ys1+ut*en2
      zs1=zs1+ut*en3
      endif
      ixx(1)=1-min0(1,iabs(ix(1)-k))
      ixx(2)=1-min0(1,iabs(ix(2)-k))
      ixx(3)=1-min0(1,iabs(ix(3)-k))
      ixx(4)=1-min0(1,iabs(ix(4)-k))
      ifac=min0(1,iabs(ix(3)-ix(4))+ixx(1)+ixx(2))
      ixx(4)=ifac*ixx(4)
      k1=ixx(1)+2*ixx(2)+3*ixx(3)+4*ixx(4)
      k2=2*ixx(1)+3*ixx(2)+ifac*4*ixx(3)+ixx(4)+1-ifac
      k3=4*ixx(1)+ixx(2)+2*ixx(3)+3*ixx(4)
      call ptime (k1,k2,k3,detv)
      if (detv.gt.0.) go to 60
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      if (ierr.eq.0) go to 70
   60 continue
   70 return
      end
      subroutine chkgn3(nsv,ilb,ierr,xn1,xn2,xn3,ss,tt,
     1  l,x,irect,msr,thic,nseg,iloc,iyy,anv1,anv2,anv3)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/shls/unit,en1,en2,en3,thkn,ilbn
      dimension x(3,*),irect(4,*),msr(*),nseg(*),
     1  nsv(*),anv1(*),anv2(*),anv3(*),iloc(*)
      dimension jlist(4,4),ilist(4),thic(*),ixx(4),iyy(4)
      nn=nsv(ilb)
      xs1=x(1,nn)
      ys1=x(2,nn)
      zs1=x(3,nn)
      do 10 jj=1,4
      iyy(jj)=irect(jj,l)
      nn=msr(irect(jj,l))
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
      xx3(jj)=x(3,nn)
   10 continue
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.ne.0.0) then
      delta=.50*thic(iyy(1))
      xx1(1)=xx1(1)+delta*anv1(iyy(1))
      xx2(1)=xx2(1)+delta*anv2(iyy(1))
      xx3(1)=xx3(1)+delta*anv3(iyy(1))
      delta=.50*thic(iyy(2))
      xx1(2)=xx1(2)+delta*anv1(iyy(2))
      xx2(2)=xx2(2)+delta*anv2(iyy(2))
      xx3(2)=xx3(2)+delta*anv3(iyy(2))
      delta=.50*thic(iyy(3))
      xx1(3)=xx1(3)+delta*anv1(iyy(3))
      xx2(3)=xx2(3)+delta*anv2(iyy(3))
      xx3(3)=xx3(3)+delta*anv3(iyy(3))
      delta=.50*thic(iyy(4))
      xx1(4)=xx1(4)+delta*anv1(iyy(4))
      xx2(4)=xx2(4)+delta*anv2(iyy(4))
      xx3(4)=xx3(4)+delta*anv3(iyy(4))
      endif
      if (thkn.gt.0.0) then
      ut=.5*thkn
      xs1=xs1+ut*en1
      ys1=ys1+ut*en2
      zs1=zs1+ut*en3
      endif
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      return
      end
      subroutine slavf2(x,e,irect,lmsr,msr,nsv,iloc,irtl,stf,
     1 nsn,nmn,nty,fdat,iseg,fric,nseg,fails,crst)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/bk26/ncycle
      common/bk28/summss,xke,xpe,timx
      common/slv2/thk,isrch
      common/double/iprec,ncpw,unit
      dimension x(3,*),e(*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
     1 irtl(*),stf(*),fdat(5,*),iseg(*),fric(*),fails(5,*),crst(2,*)
      data zero /0.0/
      fcoeff=fric(1)**2+fric(2)**2+fric(3)**2
      do 100 ii=1,nsn
      i=nsv(ii)
      j=iloc(ii)
      k=msr(j)
      l=irtl(ii)
      do 10 jj=1,4
      nn=irect(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   10 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
      if (fails(1,ii).eq.0) then
      if (k.ne.ix(1)) go to 20
      k1=1
      k2=2
      k3=4
      go to 50
   20 if (k.ne.ix(2)) go to 30
      k1=2
      k2=3
      k3=1
      go to 50
   30 if (k.ne.ix(3)) go to 40
      k1=3
      k2=4
      k3=2
      if (ix(3).eq.ix(4)) k2=1
      go to 50
   40 if (k.ne.ix(4)) go to 50
      k1=4
      k2=1
      k3=3
   50 call ptime (k1,k2,k3,detv)
      if (detv.gt.0.) go to 80
      endif
      ierr=0
      i3=3*i
      i2=i3-1
      i1=i2-1
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      ans=xn1*(xs1-amx)+xn2*(ys1-amy)+xn3*(zs1-amz)
      if (fails(1,ii).gt.0.0) then
      if (ncycle.eq.0) then
      if (ierr.ne.0) then
      fails(1,ii)=0.0
      go to 80
      endif
      crst(1,ii)=ss
      crst(2,ii)=tt
      x(1,i)=amx
      x(2,i)=amy
      x(3,i)=amz
      ans=0.0
      fails(5,ii)=float(l)
      else
      jj=nint(fails(5,ii))
      tp=.25*(1.0+crst(2,ii))
      tm=.25*(1.0-crst(2,ii))
      sp=1.0+crst(1,ii)
      sm=1.0-crst(1,ii)
      h1=tm*sm
      h2=tm*sp
      h3=tp*sp
      h4=tp*sm
      nn1=irect(1,jj)
      nn2=irect(2,jj)
      nn3=irect(3,jj)
      nn4=irect(4,jj)
      dx=x(1,i)-h1*x(1,nn1)-h2*x(1,nn2)-h3*x(1,nn3)-h4*x(1,nn4)
      dy=x(2,i)-h1*x(2,nn1)-h2*x(2,nn2)-h3*x(2,nn3)-h4*x(2,nn4)
      dz=x(3,i)-h1*x(3,nn1)-h2*x(3,nn2)-h3*x(3,nn3)-h4*x(3,nn4)
      fxi=stf(l)*dx
      fyi=stf(l)*dy
      fzi=stf(l)*dz
      frcmag=fxi*fxi+fyi*fyi+fzi*fzi
      frcnrm=fxi*xn1+fyi*xn2+fzi*xn3
      frctan=sqrt(max(frcmag-frcnrm**2,zero))
      fltest=(  max(zero,frcnrm)/fails(1,ii))**fails(3,ii)
     1      +(         frctan /fails(2,ii))**fails(4,ii)
      if (fltest.gt.1.0) then
      write (13,120) ii,i,ncycle
      fails(1,ii)=0.
      fails(2,ii)=0.
      go to 58
      endif
      endif
      go to 68
      endif
   58 if (ierr.eq.0.and.ans.gt.0.) go to 80
      if (isrch.eq.1) call thkgs(xx1,xx2,xx3,thk)
      if ((ierr.ne.0.and.  max(abs(ss),abs(tt)).le.2.).or.
     1    ans.lt.-thk)
     1 call salvex(ans,i,ierr,xn1,xn2,xn3,ss,tt,l,x,irect,lmsr,
     1             msr,nseg,nmn)
      if (ierr.ne.0.or.ans.gt.0..or.ans.lt.-thk) go to 80
   60 fni=ans*stf(l)
      fxi=xn1*fni
      fyi=xn2*fni
      fzi=xn3*fni
      if (fcoeff.eq.0.0) go to 68
      jj=iseg(ii)
      if (jj.ne.0) go to 65
      iseg(ii)=l
      fdat(1,ii)=ss
      fdat(2,ii)=tt
      go to 68
   65 tp=.25*(1.0+fdat(2,ii))
      tm=.25*(1.0-fdat(2,ii))
      sp=1.0+fdat(1,ii)
      sm=1.0-fdat(1,ii)
      h1=tm*sm
      h2=tm*sp
      h3=tp*sp
      h4=tp*sm
      nn1=irect(1,jj)
      nn2=irect(2,jj)
      nn3=irect(3,jj)
      nn4=irect(4,jj)
      dx=amx-h1*x(1,nn1)-h2*x(1,nn2)-h3*x(1,nn3)-h4*x(1,nn4)
      dy=amy-h1*x(2,nn1)-h2*x(2,nn2)-h3*x(2,nn3)-h4*x(2,nn4)
      dz=amz-h1*x(3,nn1)-h2*x(3,nn2)-h3*x(3,nn3)-h4*x(3,nn4)
      vel=sqrt(dx**2+dy**2+dz**2)/  max(1.e-20*unit,dt2)
      fdat(3,ii)=fdat(3,ii)+stf(l)*dx
      fdat(4,ii)=fdat(4,ii)+stf(l)*dy
      fdat(5,ii)=fdat(5,ii)+stf(l)*dz
      proj=fdat(3,ii)*xn1+fdat(4,ii)*xn2+fdat(5,ii)*xn3
      fdat(3,ii)=fdat(3,ii)-proj*xn1
      fdat(4,ii)=fdat(4,ii)-proj*xn2
      fdat(5,ii)=fdat(5,ii)-proj*xn3
      fmax=-(fric(2)+(fric(1)-fric(2))*exp(-fric(3)*vel))*fni
      fmag=sqrt(fdat(3,ii)**2+fdat(4,ii)**2+fdat(5,ii)**2)
      if (fmax.ge.fmag.or.fmag.eq.0.) go to 67
      sclf=fmax/fmag
      fdat(3,ii)=sclf*fdat(3,ii)
      fdat(4,ii)=sclf*fdat(4,ii)
      fdat(5,ii)=sclf*fdat(5,ii)
   67 fxi=fxi+fdat(3,ii)
      fyi=fyi+fdat(4,ii)
      fzi=fzi+fdat(5,ii)
      iseg(ii)=l
      fdat(1,ii)=ss
      fdat(2,ii)=tt
   68 do 70 jj=1,4
      j3=3*ix(jj)
      j2=j3-1
      j1=j2-1
      e(j1)=e(j1)+fxi*h(jj)
      e(j2)=e(j2)+fyi*h(jj)
      e(j3)=e(j3)+fzi*h(jj)
   70 continue
      e(i1)=e(i1)-fxi
      e(i2)=e(i2)-fyi
      e(i3)=e(i3)-fzi
      go to 100
   80 if (fcoeff.eq.0.0) go to 100
      iseg(ii)=0
      fdat(3,ii)=0.
      fdat(4,ii)=0.
      fdat(5,ii)=0.
  100 continue
      return
  120 format(//,' slave node number',i4,' node number', i7,' has',/
     1 ' failed on time step number',i7,////)
      end
      subroutine sscntc(anv,x,e,irect,lmsr,nsv,iloc,nrts,irtl,
     1nseg,stf,thic,chrlen,lpntbk,lbcket,nsn,
     2fdat,iseg,fric,icls,icount,thk)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/bk28/summss,xke,xpe,timx
      common/aux14/jja(1000),x1(1000),x2(1000),x3(1000),dsq(1000),
     1 iw(1000),dw(1000)
      dimension x(3,*),e(*),irect(4,*),lmsr(*),nsv(*),iloc(*),
     1irtl(*),stf(*),thic(*),nseg(*),lpntbk(*),lbcket(*),numbuk(201),
     2rmin(3),rmax(3),s(3),nb(201,3),lp(201,3),lb(1000,3),ibgn(3),
     3iend(3),icol(3),jrefa(3),fdat(5,*),iseg(*),fric(*),anv(nsn,3)
      dimension jcol(3),lpt(201,3,3),nbt(201,3,3),lbt(1000,3,3),
c    1   thk(*),icls(*)                                                 cray1
     1   thk(*),icls(3,*)                                               vax75
      data jrefa/2,3,1/
c
      fcoeff=fric(1)**2.+fric(2)**2.+fric(3)**2.
      clensq=chrlen**2
c
      if (icount.ne.1) go to 2500
c
      call chupdt(irect,nsv,x,nrts,chrlen,stf)
c
      clensq=chrlen**2
c
c     initialize arrays
c
      call iazero (lpt,1809)
      call iazero (nbt,1809)
      call iazero (lbt,9000)
      call iazero (lpntbk,201)
      call iazero (lbcket,nsn)
      call iazero (jja,7000)
      call iazero (lb ,3000)
      call iazero (nb ,603 )
      call iazero (lp ,603 )
      call iazero (numbuk,201)
c
      rmin(1)=1.e+20
      rmin(2)=1.e+20
      rmin(3)=1.e+20
      rmax(1)=-rmin(1)
      rmax(2)=-rmin(2)
      rmax(3)=-rmin(3)
      do 10 i=1,nsn
      rmin(1)=  min(rmin(1),x(1,nsv(i)))
      rmin(2)=  min(rmin(2),x(2,nsv(i)))
      rmin(3)=  min(rmin(3),x(3,nsv(i)))
      rmax(1)=  max(rmax(1),x(1,nsv(i)))
      rmax(2)=  max(rmax(2),x(2,nsv(i)))
      rmax(3)=  max(rmax(3),x(3,nsv(i)))
      iloc(i)=0
   10 continue
      s(1)=rmax(1)-rmin(1)
      s(2)=rmax(2)-rmin(2)
      s(3)=rmax(3)-rmin(3)
      iref=1
      if (s(2).gt.s(1)) iref=2
      if (s(3).gt.s(iref)) iref=3
      jref=jrefa(iref)
      if (s(6-iref-jref).gt.s(jref)) jref=6-iref-jref
      kref=6-iref-jref
      xxm=s(iref)
      xmin=rmin(iref)
      yym=s(jref)
      ymin=rmin(jref)
      zzm=s(kref)
      zmin=rmin(kref)
      ricchr=1./chrlen
      numbin=1+xxm*ricchr
      numbin=min0(200,numbin)
      nbin=1+yym*ricchr
      nbin=min0(200,nbin)
      nbint=1+zzm*ricchr
      nbint=min0(200,nbint)
      call bksrt1(lpntbk,lbcket,numbuk,numbin,x(iref,1),
     1           xmin,nsv,1,nsn,ricchr)
      nmax=0
      do 60 i=1,numbin
      nmax=max0(nmax,numbuk(i))
   60 continue
      if (nmax.gt.1000) go to 3000
      iminc=1
      icol(1)=0
      icol(2)=0
      icol(3)=0
      if (numbuk(1).eq.0) go to 1000
      icol(1)=1
      ipb=lpntbk(1)
      ipe=lpntbk(2)-1
      call bksrt2(lp(1,1),lb(1,1),nb(1,1),nbin,x(jref,1),
     1           ymin,nsv,ipb,ipe,ricchr,lbcket)
 1000 do 2000 ibb=1,numbin
      icc=iminc
      iminc=1
      if (icol(2).lt.icol(1)) iminc=2
      if (icol(3).lt.icol(iminc)) iminc=3
      icol(iminc)=0
      if (ibb.eq.numbin) go to 1030
      np1=ibb+1
      icol(iminc)=np1
      if (numbuk(np1).eq.0) go to 1030
      ipb=lpntbk(np1)
      ipe=lpntbk(np1+1)-1
      call bksrt2(lp(1,iminc),lb(1,iminc),nb(1,iminc),nbin,x(jref,1),
     1           ymin,nsv,ipb,ipe,ricchr,lbcket)
 1030 jminc=1
      jcol(1)=1
      jcol(2)=0
      jcol(3)=0
      do 1050 i=1,3
      if (nb(1,i).eq.0.or.icol(i).eq.0) go to 1050
      ipb=lp(1,i)
      ipe=lp(2,i)-1
      call bksrt2(lpt(1,1,i),lbt(1,1,i),nbt(1,1,i),
     1            nbint,x(kref,1),zmin,nsv,ipb,ipe,ricchr,lb(1,i))
 1050 continue
c
 1100 if (numbuk(ibb).eq.0) go to 2000
c
      do 1500 isb=1,nbin
      jcc=jminc
      jminc=1
      if (jcol(2).lt.jcol(1)) jminc=2
      if (jcol(3).lt.jcol(jminc)) jminc=3
      jcol(jminc)=0
      if (isb.eq.nbin) go to 1125
      np1=isb+1
      jcol(jminc)=np1
      do 1120 ijk=1,3
      if (nb(np1,ijk).eq.0.or.icol(ijk).eq.0) go to 1120
      ipb=lp(np1,ijk)
      ipe=lp(np1+1,ijk)-1
      call bksrt2(lpt(1,jminc,ijk),lbt(1,jminc,ijk),nbt(1,jminc,ijk),
     1            nbint,x(kref,1),zmin,nsv,ipb,ipe,ricchr,lb(1,ijk))
 1120 continue
c
 1125 if (nb(isb,icc).eq.0) go to 1500
c
      do 1400 isbt=1,nbint
      if (nbt(isbt,jcc,icc).eq.0) go to 1400
      isbb=lpt(isbt,jcc,icc)
      isbe=lpt(isbt+1,jcc,icc)-1
      isbn0=max0(1,isbt-1)
      isbn1=isbt+1
c
      ipnt=1
      if (icol(1).ne.0) then
      n11=nbt(isbn0,1,1)+nbt(isbt,1,1)+nbt(isbn1,1,1)
      if (n11.ne.0.and.jcol(1).ne.0) then
      ib11=lpt(isbn0,1,1)
      ie11=lpt(isbn1,1,1)-1
      do 1130 lpp=ib11,ie11
      jja(ipnt)=lbt(lpp,1,1)
      ipnt=ipnt+1
 1130 continue
      endif
      n21=nbt(isbn0,2,1)+nbt(isbt,2,1)+nbt(isbn1,2,1)
      if (n21.ne.0.and.jcol(2).ne.0) then
      ib21=lpt(isbn0,2,1)
      ie21=lpt(isbn1,2,1)-1
      do 1135 lpp=ib21,ie21
      jja(ipnt)=lbt(lpp,2,1)
      ipnt=ipnt+1
 1135 continue
      endif
      n31=nbt(isbn0,3,1)+nbt(isbt,3,1)+nbt(isbn1,3,1)
      if (n31.ne.0.and.jcol(3).ne.0) then
      ib31=lpt(isbn0,3,1)
      ie31=lpt(isbn1,3,1)-1
      do 1140 lpp=ib31,ie31
      jja(ipnt)=lbt(lpp,3,1)
      ipnt=ipnt+1
 1140 continue
      endif
      endif
c
      if (icol(2).ne.0) then
      n12=nbt(isbn0,1,2)+nbt(isbt,1,2)+nbt(isbn1,1,2)
      if (n12.ne.0.and.jcol(1).ne.0) then
      ib12=lpt(isbn0,1,2)
      ie12=lpt(isbn1,1,2)-1
      do 1145 lpp=ib12,ie12
      jja(ipnt)=lbt(lpp,1,2)
      ipnt=ipnt+1
 1145 continue
      endif
      n22=nbt(isbn0,2,2)+nbt(isbt,2,2)+nbt(isbn1,2,2)
      if (n22.ne.0.and.jcol(2).ne.0) then
      ib22=lpt(isbn0,2,2)
      ie22=lpt(isbn1,2,2)-1
      do 1150 lpp=ib22,ie22
      jja(ipnt)=lbt(lpp,2,2)
      ipnt=ipnt+1
 1150 continue
      endif
      n32=nbt(isbn0,3,2)+nbt(isbt,3,2)+nbt(isbn1,3,2)
      if (n32.ne.0.and.jcol(3).ne.0) then
      ib32=lpt(isbn0,3,2)
      ie32=lpt(isbn1,3,2)-1
      do 1155 lpp=ib32,ie32
      jja(ipnt)=lbt(lpp,3,2)
      ipnt=ipnt+1
 1155 continue
      endif
      endif
c
      if (icol(3).ne.0) then
      n13=nbt(isbn0,1,3)+nbt(isbt,1,3)+nbt(isbn1,1,3)
      if (n13.ne.0.and.jcol(1).ne.0) then
      ib13=lpt(isbn0,1,3)
      ie13=lpt(isbn1,1,3)-1
      do 1160 lpp=ib13,ie13
      jja(ipnt)=lbt(lpp,1,3)
      ipnt=ipnt+1
 1160 continue
      endif
      n23=nbt(isbn0,2,3)+nbt(isbt,2,3)+nbt(isbn1,2,3)
      if (n23.ne.0.and.jcol(2).ne.0) then
      ib23=lpt(isbn0,2,3)
      ie23=lpt(isbn1,2,3)-1
      do 1165 lpp=ib23,ie23
      jja(ipnt)=lbt(lpp,2,3)
      ipnt=ipnt+1
 1165 continue
      endif
      n33=nbt(isbn0,3,3)+nbt(isbt,3,3)+nbt(isbn1,3,3)
      if (n33.ne.0.and.jcol(3).ne.0) then
      ib33=lpt(isbn0,3,3)
      ie33=lpt(isbn1,3,3)-1
      do 1170 lpp=ib33,ie33
      jja(ipnt)=lbt(lpp,3,3)
      ipnt=ipnt+1
 1170 continue
      endif
      endif
c
      len=ipnt-1
      if (len.gt.1000) go to 3000
c
      do 1175 lpp=1,len
      k=nsv(jja(lpp))
      x1(lpp)=x(1,k)
      x2(lpp)=x(2,k)
      x3(lpp)=x(3,k)
 1175 continue
c
      do 1380 isbn=isbb,isbe
      ilb=lbt(isbn,jcc,icc)
      i=nsv(ilb)
      xsi=x(1,i)
      ysi=x(2,i)
      zsi=x(3,i)
      num=nseg(ilb+1)-nseg(ilb)
      npt=nseg(ilb)-1
c
      do 1180 lpp=1,num
      ll=lmsr(lpp+npt)
      iloc(irect(1,ll))=1
      iloc(irect(2,ll))=1
      iloc(irect(3,ll))=1
      iloc(irect(4,ll))=1
 1180 continue
c
      do 1185 lpp=1,len
      dsq(lpp)=(xsi-x1(lpp))**2+(ysi-x2(lpp))**2+(zsi-x3(lpp))**2
 1185 continue
      len2=1
      iw(1)=ilb
      do 1200 lpp=1,len
      if (iloc(jja(lpp)).eq.0) then
      iw(len2)=jja(lpp)
      dw(len2)=dsq(lpp)
      len2=len2+1
      endif
 1200 continue
      len2=len2-1
      if (len2.lt.3) go to 1260
c
      do 1220 lpp=2,len2
      if (dw(lpp).ge.dw(1)) go to 1220
      isav=iw(1)
      iw(1)=iw(lpp)
      iw(lpp)=isav
      dsav=dw(1)
      dw(1)=dw(lpp)
      dw(lpp)=dsav
 1220 continue
      do 1230 lpp=3,len2
      if (dw(lpp).ge.dw(2)) go to 1230
      isav=iw(2)
      iw(2)=iw(lpp)
      iw(lpp)=isav
      dsav=dw(2)
      dw(2)=dw(lpp)
      dw(lpp)=dsav
 1230 continue
      if (len2.eq.3) go to 1250
      do 1240 lpp=4,len2
      if (dw(lpp).ge.dw(3)) go to 1240
      isav=iw(3)
      iw(3)=iw(lpp)
      iw(lpp)=isav
      dsav=dw(3)
      dw(3)=dw(lpp)
      dw(lpp)=dsav
 1240 continue
c
 1250 continue
      go to 1300
 1260 if (len2.ge.3) go to 1300
      len3=len2+1
      do 1279 lpp=len3,3
      iw(lpp)=iw(1)
 1279 continue
c1300 call packss(icls(ilb),iw)                                         cray1
 1300 call packss(icls(1,ilb),iw)                                       vax75
c
      do 1320 lpp=1,num
      ll=lmsr(lpp+npt)
      iloc(irect(1,ll))=0
      iloc(irect(2,ll))=0
      iloc(irect(3,ll))=0
      iloc(irect(4,ll))=0
 1320 continue
c
 1380 continue
 1400 continue
 1500 continue
 2000 continue
 2500 call ssfnd1(fric,nsv,nsn,icls,x,lmsr,nseg,irect,fdat,fcoeff,
     1 e,irtl,iseg,thic,stf,clensq,nrts,anv(1,1),anv(1,2),anv(1,3),thk)
      return
 3000 write(13,1)
      call adios(2)
      return
    1 format(1x/1x/' error(sscntc) --- more than',
     1       ' 1000 nodes in a bucket')
      end
      subroutine ssfnd1(fric,nsv,nsn,icls,x,lmsr,nseg,irect,fdat,fcoeff,
     1      e,irtl,iseg,thic,stf,clensq,nrts,anv1,anv2,anv3,thk)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/bk28/summss,xke,xpe,timx
      common/aux33/
     1 ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128),
     1 kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
      common/aux14/xl(128),yl(128),zl(128),xn1(128),xn2(128),
     1          yn1(128),yn2(128),yn3(128),zn1(128),zn2(128),
     1          f1(128),f2(128),f3(128),dqmin(128),xn3(128),
     1          icl(128),d1(128),d2(128),d3(128),zn3(128),
     1          f4(128)
      common/shls/unit,en1,en2,en3,thkn,ilbn
      common/double/iprec,ncpw,xunit
      dimension ixx(4),nsv(*),x(3,*),lmsr(*),nseg(*),irect(4,*),
c    1 thk(*),icls(*),fdat(5,*),e(3,*),fric(*),                         cray1
     1 thk(*),icls(3,*),fdat(5,*),e(3,*),fric(*),                       vax75
     2 irtl(*),iseg(*),thic(*),stf(*),anv1(*),anv2(*),anv3(*),iyy(4)
      ip=1
      nelg=nsn/128
      ilen=128
      irem=nsn-nelg*128
      if (irem.eq.0) go to 10
      ilen=irem
      nelg=nelg+1
c
   10 call vctor2(nseg,lmsr,irect,x,nsv,nsn,anv1,anv2,anv3,nrts)
c
      do 1500 nn=1,nelg
      ipm1=ip-1
c
c     call unpkss(icls(ip),ilen)                                        cray1
      call unpkss(icls(1,ip),ilen)                                      vax75
c
      do 100 i=1,ilen
      k=nsv(i+ipm1)
      xl(i)=x(1,k)
      yl(i)=x(2,k)
      zl(i)=x(3,k)
      k1=nsv(ix1(i))
      xn1(i)=x(1,k1)
      yn1(i)=x(2,k1)
      zn1(i)=x(3,k1)
      k2=nsv(ix2(i))
      xn2(i)=x(1,k2)
      yn2(i)=x(2,k2)
      zn2(i)=x(3,k2)
      k3=nsv(ix3(i))
      xn3(i)=x(1,k3)
      yn3(i)=x(2,k3)
      zn3(i)=x(3,k3)
  100 continue
c
      do 120 i=1,ilen
      d1(i)=(xl(i)-xn1(i))**2+(yl(i)-yn1(i))**2+(zl(i)-zn1(i))**2
      d2(i)=(xl(i)-xn2(i))**2+(yl(i)-yn2(i))**2+(zl(i)-zn2(i))**2
      d3(i)=(xl(i)-xn3(i))**2+(yl(i)-yn3(i))**2+(zl(i)-zn3(i))**2
  120 continue
c
      do 130 i=1,ilen
      dqmin(i)=  min(d1(i),d2(i),d3(i))
      f1(i)=.5+sign(.5*unit,dqmin(i)-d1(i))
      f2(i)=.5+sign(.5*unit,dqmin(i)-d2(i))
      f2(i)=.5+sign(.5*unit,f2(i)-f1(i)-.100)
      f3(i)=1.-f1(i)-f2(i)
      f4(i)=.5+sign(.5*unit,clensq-dqmin(i))
  130 continue
c
      do 140 i=1,ilen
      icl(i)=f4(i)*(f1(i)*ix1(i)+f2(i)*ix2(i)+f3(i)*ix3(i))
  140 continue
c
      do 1380 lz=1,ilen
      ilb=lz+ipm1
      thkn=thic(ilb)
      if (icl(lz).eq.ilb) go to 1370
      unit=1.
      epx =0.0
      ixcs=0
      lold=0
      i=nsv(ilb)
      ilbn=nsv(ilb)
      if (irtl(ilb).eq.0) go to 1175
      l=iabs(irtl(ilb))
      if (l.gt.1000000) l=l-1000000
      ixcs=iabs(irtl(ilb))-l
      if (irtl(ilb).lt.0) unit=-1.
      lold=l
      if (l.eq.0) go to 1175
      ierr=0
      call chkagn(ilb,ierr,xnn1,xnn2,xnn3,ss,tt,
     1  l,x,irect,nsv,thic,nseg,lmsr,iyy,anv1,anv2,anv3)
      if (ierr.ne.0) call sssalv(ixcs,ilb,ierr,xnn1,xnn2,xnn3,ss,tt,
     1 l,x,irect,lmsr,nsv,nseg,thic,iyy,anv1,anv2,anv3)
      if (ierr.eq.0) go to 1302
 1175 j=icl(lz)
      if (j.eq.0) go to 1370
      en1=anv1(ilb)
      en2=anv2(ilb)
      en3=anv3(ilb)
      k=nsv(j)
      resltl=-1.e20
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      do 1240 lpp=1,num
      ll=lmsr(lpp+npt)
      llc=ll
      ixx(1)=1-min0(1,iabs(irect(1,ll)-j))
      ixx(2)=1-min0(1,iabs(irect(2,ll)-j))
      ixx(3)=1-min0(1,iabs(irect(3,ll)-j))
      ixx(4)=1-min0(1,iabs(irect(4,ll)-j))
      ifac=min0(1,iabs(irect(3,ll)-irect(4,ll))+ixx(1)+ixx(2))
      ixx(4)=ifac*ixx(4)
      j1=2*ixx(1)+3*ixx(2)+ifac*4*ixx(3)+ixx(4)+1-ifac
      j2=4*ixx(1)+  ixx(2)+2*ixx(3)+3*ixx(4)
      m=nsv(irect(j1,ll))
      n=nsv(irect(j2,ll))
      call cross (x)
      if (result.le.0.0) go to 1240
      l=ll
      go to 1250
 1240 continue
      l=lrsort
 1250 iyy(1)=irect(1,l)
      ix(1) =nsv(irect(1,l))
      xx1(1)=x(1,nsv(irect(1,l)))
      xx2(1)=x(2,nsv(irect(1,l)))
      xx3(1)=x(3,nsv(irect(1,l)))
      iyy(2)=irect(2,l)
      ix(2) =nsv(irect(2,l))
      xx1(2)=x(1,nsv(irect(2,l)))
      xx2(2)=x(2,nsv(irect(2,l)))
      xx3(2)=x(3,nsv(irect(2,l)))
      iyy(3)=irect(3,l)
      ix(3) =nsv(irect(3,l))
      xx1(3)=x(1,nsv(irect(3,l)))
      xx2(3)=x(2,nsv(irect(3,l)))
      xx3(3)=x(3,nsv(irect(3,l)))
      iyy(4)=irect(4,l)
      ix(4) =nsv(irect(4,l))
      xx1(4)=x(1,nsv(irect(4,l)))
      xx2(4)=x(2,nsv(irect(4,l)))
      xx3(4)=x(3,nsv(irect(4,l)))
      if (ixcs.ne.0) then
      unit2=.5*unit
      delta=unit2*thic(iyy(1))
      xx1(1)=xx1(1)+delta*anv1(iyy(1))
      xx2(1)=xx2(1)+delta*anv2(iyy(1))
      xx3(1)=xx3(1)+delta*anv3(iyy(1))
      delta=unit2*thic(iyy(2))
      xx1(2)=xx1(2)+delta*anv1(iyy(2))
      xx2(2)=xx2(2)+delta*anv2(iyy(2))
      xx3(2)=xx3(2)+delta*anv3(iyy(2))
      delta=unit2*thic(iyy(3))
      xx1(3)=xx1(3)+delta*anv1(iyy(3))
      xx2(3)=xx2(3)+delta*anv2(iyy(3))
      xx3(3)=xx3(3)+delta*anv3(iyy(3))
      delta=unit2*thic(iyy(4))
      xx1(4)=xx1(4)+delta*anv1(iyy(4))
      xx2(4)=xx2(4)+delta*anv2(iyy(4))
      xx3(4)=xx3(4)+delta*anv3(iyy(4))
      endif
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
      e1=x(1,k)-xs1
      e2=x(2,k)-ys1
      e3=x(3,k)-zs1
      dx1=xx1(1)-xx1(3)
      dx2=xx2(1)-xx2(3)
      dx3=xx3(1)-xx3(3)
      dy1=xx1(2)-xx1(4)
      dy2=xx2(2)-xx2(4)
      dy3=xx3(2)-xx3(4)
      a1=dx2*dy3-dx3*dy2
      a2=dx3*dy1-dx1*dy3
      a3=dx1*dy2-dx2*dy1
      dot1=e1*a1 +e2*a2 +e3*a3
      dot2=en1*a1+en2*a2+en3*a3
      ut=sign(.5*unit,dot1*dot2)*thkn
      xs1=xs1+ut*en1
      ys1=ys1+ut*en2
      zs1=zs1+ut*en3
      ixx(1)=1-min0(1,iabs(ix(1)-k))
      ixx(2)=1-min0(1,iabs(ix(2)-k))
      ixx(3)=1-min0(1,iabs(ix(3)-k))
      ixx(4)=1-min0(1,iabs(ix(4)-k))
      ifac=min0(1,iabs(ix(3)-ix(4))+ixx(1)+ixx(2))
      ixx(4)=ifac*ixx(4)
      k1=  ixx(1)+2*ixx(2)+3*ixx(3)+4*ixx(4)
      k2=2*ixx(1)+3*ixx(2)+ifac*4*ixx(3)+ixx(4)+1-ifac
      k3=4*ixx(1)+  ixx(2)+2*ixx(3)+3*ixx(4)
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.eq.0.0) then
      de1=dx1*dx1+dx2*dx2+dx3*dx3
      de2=dy1*dy1+dy2*dy2+dy3*dy3
      epx=.1*sqrt(  max(de1,de2))
      endif
      if (ixcs.eq.0) go to 1301
      call ptime(k1,k2,k3,detv)
      if (unit*detv.gt.0.) go to 1370
      go to 1306
 1301 call pptime(k1,k2,k3,detv)
      if (detv.gt.0.) go to 1370
 1306 ierr=0
      if (l.eq.lold) go to 1372
      call stex (xnn1,xnn2,xnn3,ss,tt,ierr,0)
      if (ierr.eq.1) go to 1370
      if (ierr.ne.0) call sssalv(ixcs,ilb,ierr,xnn1,xnn2,xnn3,ss,tt,
     1 l,x,irect,lmsr,nsv,nseg,thic,iyy,anv1,anv2,anv3)
      if (ierr.ne.0) go to 1370
 1302 ans=xnn1*(xs1-amx)+xnn2*(ys1-amy)+xnn3*(zs1-amz)
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.eq.0.0) then
c     de1=(xx1(1)-xx1(3))**2+(xx2(1)-xx2(3))**2+(xx3(1)-xx3(3))**2
c     de2=(xx1(2)-xx1(4))**2+(xx2(2)-xx2(4))**2+(xx3(1)-xx3(4))**2
c     epx=.1*sqrt(  max(de1,de2))
      epx=thk(l)
      endif
      if (ixcs.ge.1000000.or.abs(ans).ge..6*epx)go to 1303
      if (thic(ilb).gt.0.) unit=sign(1.*unit,ans)
      ixcs=1000000
      irtl(ilb)=unit*(ixcs+l)
      go to 1380
 1303 irtl(ilb)=unit*(ixcs+l)
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      uans=unit*ans
      if (uans.ge.0.) go to 1372
      if (epx.ne.0..and.uans.lt.-.4*epx) go to 1372
      a1=anv1(ilb)
      a2=anv2(ilb)
      a3=anv3(ilb)
      dotan=sign(1.*unit,a1*xnn1+a2*xnn2+a3*xnn3)
      a1=a1*dotan
      a2=a2*dotan
      a3=a3*dotan
      a1=xnn1
      a2=xnn2
      a3=xnn3
c     ans=a1*(xs1-amx)+a2*(ys1-amy)+a3*(zs1-amz)
      fni=ans*stf(l)
c     write(13,13) timx,ilbn,ans,fni,xs1,ys1,zs1,x(1,ilbn),
c    1 x(2,ilbn),x(3,ilbn)
c  13 format(' timx,ilbn,ans,fni=',1x,1pe14.7,1x,i5,2(1x,1pe14.7)/
c    1 (3(1x,1pe14.7)))
c     call empty(13)
      fxi=a1*fni
      fyi=a2*fni
      fzi=a3*fni
      if (fcoeff.eq.0.0) go to 1308
      jj=iseg(ilb)
      if (jj.ne.0) go to 1305
      iseg(ilb)=l
      fdat(1,ilb)=ss
      fdat(2,ilb)=tt
      go to 1308
 1305 tp=.25*(1.0+fdat(2,ilb))
      tm=.25*(1.0-fdat(2,ilb))
      sp=1.0+fdat(1,ilb)
      sm=1.0-fdat(1,ilb)
      h1=tm*sm
      h2=tm*sp
      h3=tp*sp
      h4=tp*sm
      nn1=nsv(irect(1,jj))
      nn2=nsv(irect(2,jj))
      nn3=nsv(irect(3,jj))
      nn4=nsv(irect(4,jj))
      dx=amx-h1*x(1,nn1)-h2*x(1,nn2)-h3*x(1,nn3)-h4*x(1,nn4)
      dy=amy-h1*x(2,nn1)-h2*x(2,nn2)-h3*x(2,nn3)-h4*x(2,nn4)
      dz=amz-h1*x(3,nn1)-h2*x(3,nn2)-h3*x(3,nn3)-h4*x(3,nn4)
      vel=sqrt(dx**2+dy**2+dz**2)/  max(1.e-20*xunit,dt2)
      fdat(3,ilb)=fdat(3,ilb)+stf(l)*dx
      fdat(4,ilb)=fdat(4,ilb)+stf(l)*dy
      fdat(5,ilb)=fdat(5,ilb)+stf(l)*dz
      proj=fdat(3,ilb)*xnn1+fdat(4,ilb)*xnn2+fdat(5,ilb)*xnn3
      fdat(3,ilb)=fdat(3,ilb)-proj*xnn1
      fdat(4,ilb)=fdat(4,ilb)-proj*xnn2
      fdat(5,ilb)=fdat(5,ilb)-proj*xnn3
      fmax=-(fric(2)+(fric(1)-fric(2))*exp(-fric(3)*vel))*fni
      fmag=sqrt(fdat(3,ilb)**2+fdat(4,ilb)**2+fdat(5,ilb)**2)
      if (fmax.ge.fmag.or.fmag.eq.0.) go to 1307
      sclf=fmax/fmag
      fdat(3,ilb)=sclf*fdat(3,ilb)
      fdat(4,ilb)=sclf*fdat(4,ilb)
      fdat(5,ilb)=sclf*fdat(5,ilb)
 1307 fxi=fxi+fdat(3,ilb)
      fyi=fyi+fdat(4,ilb)
      fzi=fzi+fdat(5,ilb)
      iseg(ilb)=l
      fdat(1,ilb)=ss
      fdat(2,ilb)=tt
 1308 tp=.25*(1.0+tt)
      tm=.25*(1.0-tt)
      sp=1.0+ss
      sm=1.0-ss
      h1=tm*sm
      h2=tm*sp
      h3=tp*sp
      h4=tp*sm
      e(1,ix(1))=e(1,ix(1))+fxi*h1
      e(2,ix(1))=e(2,ix(1))+fyi*h1
      e(3,ix(1))=e(3,ix(1))+fzi*h1
      e(1,ix(2))=e(1,ix(2))+fxi*h2
      e(2,ix(2))=e(2,ix(2))+fyi*h2
      e(3,ix(2))=e(3,ix(2))+fzi*h2
      e(1,ix(3))=e(1,ix(3))+fxi*h3
      e(2,ix(3))=e(2,ix(3))+fyi*h3
      e(3,ix(3))=e(3,ix(3))+fzi*h3
      e(1,ix(4))=e(1,ix(4))+fxi*h4
      e(2,ix(4))=e(2,ix(4))+fyi*h4
      e(3,ix(4))=e(3,ix(4))+fzi*h4
      e(1,i)=e(1,i)-fxi
      e(2,i)=e(2,i)-fyi
      e(3,i)=e(3,i)-fzi
      go to 1380
 1370 irtl(ilb)=0
 1372 if (fcoeff.eq.0.0) go to 1380
      iseg(ilb)=0
      fdat(3,ilb)=0.
      fdat(4,ilb)=0.
      fdat(5,ilb)=0.
 1380 continue
      ip=ip+ilen
      ilen=128
 1500 continue
c
      return
      end
      subroutine thkgs(xx1,xx2,xx3,thk)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xx1(*),xx2(*),xx3(*)
      e1=(xx1(1)-xx1(3))**2+(xx2(1)-xx2(3))**2+(xx3(1)-xx3(3))**2
      e2=(xx1(2)-xx1(4))**2+(xx2(2)-xx2(4))**2+(xx3(1)-xx3(4))**2
      thk=.05*sqrt(  min(e1,e2))
      return
      end
      subroutine vctor2(nseg,iloc,irect,x,nsv,nsn,xn1,xn2,xn3,nrts)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nseg(*),iloc(*),irect(4,*),x(3,*),nsv(*),ixx(4),
     1 xn1(*),xn2(*),xn3(*)
      do 10 ii=1,nsn
      xn1(ii)=0.
      xn2(ii)=0.
      xn3(ii)=0.
   10 continue
      do 20 l=1,nrts
      xx11=x(1,nsv(irect(1,l)))
      xx21=x(2,nsv(irect(1,l)))
      xx31=x(3,nsv(irect(1,l)))
      xx12=x(1,nsv(irect(2,l)))
      xx22=x(2,nsv(irect(2,l)))
      xx32=x(3,nsv(irect(2,l)))
      xx13=x(1,nsv(irect(3,l)))
      xx23=x(2,nsv(irect(3,l)))
      xx33=x(3,nsv(irect(3,l)))
      xx14=x(1,nsv(irect(4,l)))
      xx24=x(2,nsv(irect(4,l)))
      xx34=x(3,nsv(irect(4,l)))
      x13=xx13-xx11
      x24=xx14-xx12
      y13=xx23-xx21
      y24=xx24-xx22
      z13=xx33-xx31
      z24=xx34-xx32
      fs1=x13-x24
      ft1=x13+x24
      fs2=y13-y24
      ft2=y13+y24
      fs3=z13-z24
      ft3=z13+z24
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      rarea=sqrt((e*g-f*f)/16.)
      x21=xx12-xx11
      y21=xx22-xx21
      z21=xx32-xx31
      x41=xx14-xx11
      y41=xx24-xx21
      z41=xx34-xx31
      ii=irect(1,l)
      xn1(ii)=xn1(ii)+rarea*(y21*z41-z21*y41)
      xn2(ii)=xn2(ii)+rarea*(z21*x41-x21*z41)
      xn3(ii)=xn3(ii)+rarea*(x21*y41-y21*x41)
      x23=xx12-xx13
      y23=xx22-xx23
      z23=xx32-xx33
      ii=irect(2,l)
      xn1(ii)=xn1(ii)+rarea*(y23*z21-z23*y21)
      xn2(ii)=xn2(ii)+rarea*(z23*x21-x23*z21)
      xn3(ii)=xn3(ii)+rarea*(x23*y21-y23*x21)
      if (irect(3,l).ne.irect(4,l)) then
      x43=xx14-xx13
      y43=xx24-xx23
      z43=xx34-xx33
      ii=irect(3,l)
      xn1(ii)=xn1(ii)+rarea*(y43*z23-z43*y23)
      xn2(ii)=xn2(ii)+rarea*(z43*x23-x43*z23)
      xn3(ii)=xn3(ii)+rarea*(x43*y23-y43*x23)
      ii=irect(4,l)
      xn1(ii)=xn1(ii)+rarea*(y41*z43-z41*y43)
      xn2(ii)=xn2(ii)+rarea*(z41*x43-x41*z43)
      xn3(ii)=xn3(ii)+rarea*(x41*y43-y41*x43)
      else
      ii=irect(3,l)
      xn1(ii)=xn1(ii)-rarea*(y41*z23-z41*y23)
      xn2(ii)=xn2(ii)-rarea*(z41*x23-x41*z23)
      xn3(ii)=xn3(ii)-rarea*(x41*y23-y41*x23)
      endif
   20 continue
      do 100 ii=1,nsn
      xmg=sqrt(xn1(ii)*xn1(ii)+xn2(ii)*xn2(ii)+xn3(ii)*xn3(ii))
      xn1(ii)=xn1(ii)/xmg
      xn2(ii)=xn2(ii)/xmg
      xn3(ii)=xn3(ii)/xmg
  100 continue
      return
      end
      subroutine sssalv(ixcs,ilb,ierr,xn1,xn2,xn3,ss,tt,
     1      l,x,irect,lmsr,nsv,nseg,thic,iyy,anv1,anv2,anv3)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/shls/unit,en1,en2,en3,thkn,ilbn
      common/double/iprec,ncpw,xunit
      dimension ixx(4),x(3,*),irect(4,*),lmsr(*),nsv(*),nseg(*),
     1 anv1(*),anv2(*),anv3(*),jlist(4,4),ilist(4),thic(*),iyy(4)
c
      data jlist/1,2,4,3,4,1,3,2,2,1,3,4,4,2,3,1/
      isig=sign(1.*xunit,ss)+1.
      jsig=sign(1.*xunit,tt)+1.
      ipic=isig+jsig/2+1
      ilist(1)=irect(1,l)
      ilist(2)=irect(2,l)
      ilist(3)=irect(3,l)
      ilist(4)=irect(4,l)
      do 200 ii=1,4
      j=ilist(jlist(ii,ipic))
      k=nsv(j)
      resltl=-1.e20
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      do 40 lpp=1,num
      ll=lmsr(lpp+npt)
      llc=ll
      if (ilb.ne.irect(1,ll).and.ilb.ne.irect(2,ll)
     1.and.ilb.ne.irect(3,ll).and.ilb.ne.irect(4,ll)) go to 15
      ierr=3
      go to 1000
   15 do 20 jj=1,4
      if (irect(jj,ll).ne.j) go to 20
      j1=jj+1
      j2=jj-1
      if (jj.ne.3) go to 30
      if (irect(3,ll).eq.irect(4,ll)) j1=1
      go to 30
   20 continue
   30 if (j1.eq.5) j1=1
      if (j2.eq.0) j2=4
      m=nsv(irect(j1,ll))
      n=nsv(irect(j2,ll))
      call cross (x)
      if (result.le.0.0) go to 40
      l=ll
      go to 50
   40 continue
      l=lrsort
   50 do 60 jj=1,4
      nn=nsv(irect(jj,l))
      iyy(jj)=irect(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   60 xx3(jj)=x(3,nn)
      if (ixcs.ne.0) then
      unit2=.5*unit
      delta=unit2*thic(iyy(1))
      xx1(1)=xx1(1)+delta*anv1(iyy(1))
      xx2(1)=xx2(1)+delta*anv2(iyy(1))
      xx3(1)=xx3(1)+delta*anv3(iyy(1))
      delta=unit2*thic(iyy(2))
      xx1(2)=xx1(2)+delta*anv1(iyy(2))
      xx2(2)=xx2(2)+delta*anv2(iyy(2))
      xx3(2)=xx3(2)+delta*anv3(iyy(2))
      delta=unit2*thic(iyy(3))
      xx1(3)=xx1(3)+delta*anv1(iyy(3))
      xx2(3)=xx2(3)+delta*anv2(iyy(3))
      xx3(3)=xx3(3)+delta*anv3(iyy(3))
      delta=unit2*thic(iyy(4))
      xx1(4)=xx1(4)+delta*anv1(iyy(4))
      xx2(4)=xx2(4)+delta*anv2(iyy(4))
      xx3(4)=xx3(4)+delta*anv3(iyy(4))
      endif
      xs1=x(1,ilbn)
      ys1=x(2,ilbn)
      zs1=x(3,ilbn)
      e1=x(1,k)-xs1
      e2=x(2,k)-ys1
      e3=x(3,k)-zs1
      dx1=xx1(1)-xx1(3)
      dx2=xx2(1)-xx2(3)
      dx3=xx3(1)-xx3(3)
      dy1=xx1(2)-xx1(4)
      dy2=xx2(2)-xx2(4)
      dy3=xx3(2)-xx3(4)
      a1=dx2*dy3-dx3*dy2
      a2=dx3*dy1-dx1*dy3
      a3=dx1*dy2-dx2*dy1
      dot1=e1*a1 +e2*a2 +e3*a3
      dot2=en1*a1+en2*a2+en3*a3
      ut=sign(.5*xunit,dot1*dot2)*thkn
      xs1=xs1+ut*en1
      ys1=ys1+ut*en2
      zs1=zs1+ut*en3
      ixx(1)=1-min0(1,iabs(ix(1)-k))
      ixx(2)=1-min0(1,iabs(ix(2)-k))
      ixx(3)=1-min0(1,iabs(ix(3)-k))
      ixx(4)=1-min0(1,iabs(ix(4)-k))
      ifac=min0(1,iabs(ix(3)-ix(4))+ixx(1)+ixx(2))
      ixx(4)=ifac*ixx(4)
      k1=  ixx(1)+2*ixx(2)+3*ixx(3)+4*ixx(4)
      k2=2*ixx(1)+3*ixx(2)+ifac*4*ixx(3)+ixx(4)+1-ifac
      k3=4*ixx(1)+  ixx(2)+2*ixx(3)+3*ixx(4)
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.eq.0.0) then
      de1=dx1*dx1+dx2*dx2+dx3*dx3
      de2=dy1*dy1+dy2*dy2+dy3*dy3
      epx=.1*sqrt(  max(de1,de2))
      endif
      if (ixcs.eq.0) go to 105
      call ptime(k1,k2,k3,detv)
      if (unit*detv.gt.0.) go to 200
      go to 110
  105 call pptime(k1,k2,k3,detv)
      if (detv.gt.0.) go to 200
  110 ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      if (ierr.eq.0) go to 1000
  200 continue
c
 1000 return
      end
      subroutine chkagn(ilb,ierr,xn1,xn2,xn3,ss,tt,
     1  l,x,irect,nsv,thic,nseg,iloc,iyy,anv1,anv2,anv3)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/shls/unit,en1,en2,en3,thkn,ilbn
      common/double/iprec,ncpw,xunit
      dimension x(3,*),irect(4,*),nsv(*),nseg(*),
     1          anv1(*),anv2(*),anv3(*),iloc(*)
      dimension jlist(4,4),ilist(4),thic(*),ixx(4),iyy(4)
      ierr=1
      k=0
      d2=1.e+6
      nn=nsv(ilb)
      xs1=x(1,nn)
      ys1=x(2,nn)
      zs1=x(3,nn)
      do 60 jj=1,4
      iyy(jj)=irect(jj,l)
      nn=nsv(irect(jj,l))
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
      xx3(jj)=x(3,nn)
      dsq=(xs1-xx1(jj))**2+(ys1-xx2(jj))**2+(zs1-xx3(jj))**2
      if (dsq.ge.d2) go to 60
      d2=dsq
      k=nn
   60 continue
      epx=  max(thic(iyy(1)),thic(iyy(2)),thic(iyy(3)),thic(iyy(4)))
      if (epx.eq.0.0) then
      de1=(xx1(1)-xx1(3))**2+(xx2(1)-xx2(3))**2+(xx3(1)-xx3(3))**2
      de2=(xx1(2)-xx1(4))**2+(xx2(2)-xx2(4))**2+(xx3(1)-xx3(4))**2
      epx=.1*sqrt(  max(de1,de2))
      endif
      unit2=.5*unit
      delta=unit2*thic(iyy(1))
      xx1(1)=xx1(1)+delta*anv1(iyy(1))
      xx2(1)=xx2(1)+delta*anv2(iyy(1))
      xx3(1)=xx3(1)+delta*anv3(iyy(1))
      delta=unit2*thic(iyy(2))
      xx1(2)=xx1(2)+delta*anv1(iyy(2))
      xx2(2)=xx2(2)+delta*anv2(iyy(2))
      xx3(2)=xx3(2)+delta*anv3(iyy(2))
      delta=unit2*thic(iyy(3))
      xx1(3)=xx1(3)+delta*anv1(iyy(3))
      xx2(3)=xx2(3)+delta*anv2(iyy(3))
      xx3(3)=xx3(3)+delta*anv3(iyy(3))
      delta=unit2*thic(iyy(4))
      xx1(4)=xx1(4)+delta*anv1(iyy(4))
      xx2(4)=xx2(4)+delta*anv2(iyy(4))
      xx3(4)=xx3(4)+delta*anv3(iyy(4))
      en1=anv1(ilb)
      en2=anv2(ilb)
      en3=anv3(ilb)
      xs1=x(1,ilbn)
      ys1=x(2,ilbn)
      zs1=x(3,ilbn)
      e1=x(1,k)-xs1
      e2=x(2,k)-ys1
      e3=x(3,k)-zs1
      dx1=xx1(1)-xx1(3)
      dx2=xx2(1)-xx2(3)
      dx3=xx3(1)-xx3(3)
      dy1=xx1(2)-xx1(4)
      dy2=xx2(2)-xx2(4)
      dy3=xx3(2)-xx3(4)
      a1=dx2*dy3-dx3*dy2
      a2=dx3*dy1-dx1*dy3
      a3=dx1*dy2-dx2*dy1
      dot1=e1*a1 +e2*a2 +e3*a3
      dot2=en1*a1+en2*a2+en3*a3
      ut=sign(.5*xunit,dot1*dot2)*thkn
      xs1=xs1+ut*en1
      ys1=ys1+ut*en2
      zs1=zs1+ut*en3
c     ixx(1)=1-min0(1,iabs(ix(1)-k))
c     ixx(2)=1-min0(1,iabs(ix(2)-k))
c     ixx(3)=1-min0(1,iabs(ix(3)-k))
c     ixx(4)=1-min0(1,iabs(ix(4)-k))
c     ifac=min0(1,iabs(ix(3)-ix(4)))
c     ixx(4)=ifac*ixx(4)
c     k1=  ixx(1)+2*ixx(2)+3*ixx(3)+4*ixx(4)
c     k2=2*ixx(1)+3*ixx(2)+ifac*4*ixx(3)+ixx(4)+1-ifac
c     k3=4*ixx(1)+  ixx(2)+2*ixx(3)+3*ixx(4)
c     call ptime (k1,k2,k3,detv)
c     if (unit*detv.gt.0.) return
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      return
      end
      subroutine pptime(k1,k2,k3,det)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      common/shls/unit,en1,en2,en3,thkn,ilbn
      aj1=xs-xx1(k1)
      aj2=ys-xx2(k1)
      aj3=zs-xx3(k1)
      aj4=xx1(k2)-xx1(k1)
      aj5=xx2(k2)-xx2(k1)
      aj6=xx3(k2)-xx3(k1)
      aj7=xx1(k3)-xx1(k1)
      aj8=xx2(k3)-xx2(k1)
      aj9=xx3(k3)-xx3(k1)
      det=abs(aj1*aj5*aj9+aj2*aj6*aj7+aj3*aj4*aj8
     1-aj3*aj5*aj7-aj2*aj4*aj9
     1-aj1*aj6*aj8)/sqrt((aj4**2+aj5**2+aj6**2)*
     1(aj7**2+aj8**2+aj9**2))-.5*epx
      return
      end
      subroutine packss(ixp,ixc)
c     implicit double precision (a-h,o-z)                                    dp
c     dimension ixc(3)                                                  cray1
      dimension ixc(3),ixp(3)                                           vax75
c     data izero/0/,ibits1,ibits2/21,42/                                cray1
c     ixc(1)=ixc(1)+izero                                               cray1
c     ixp=ixc(1)                                                        cray1
c     ixp=or(shift(ixc(2),ibits1),ixp)                                  cray1
c     ixp=or(shift(ixc(3),ibits2),ixp)                                  cray1
      ixp(1)=ixc(1)                                                     vax75
      ixp(2)=ixc(2)                                                     vax75
      ixp(3)=ixc(3)                                                     vax75
      return
      end
      subroutine unpkss(ixp,nmel)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/
     1ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),mxt(128),
     2kka(128),kkb(128),kkc(128),kk1(128),kk2(128),kk3(128)
c     dimension ixp(1)                                                  cray1
      dimension ixp(3,1)                                                vax75
c     do 10 i=1,nmel                                                    cray1
c     kka(i)=ixp(i)                                                     cray1
c  10 continue                                                          cray1
c     do 20 i=1,nmel                                                    cray1
c     ix1(i)=and(kka(i),3777777b)                                       cray1
c  20 continue                                                          cray1
c     do 30 i=1,nmel                                                    cray1
c     kk1(i)=shiftr(kka(i),21)                                          cray1
c  30 continue                                                          cray1
c     do 40 i=1,nmel                                                    cray1
c     kka(i)=shiftr(kk1(i),21)                                          cray1
c  40 continue                                                          cray1
c     do 50 i=1,nmel                                                    cray1
c     ix2(i)=and(kk1(i),3777777b)                                       cray1
c     ix3(i)=and(kka(i),3777777b)                                       cray1
c  50 continue                                                          cray1
      do 10 i=1,nmel                                                    vax75
      ix1(i)=ixp(1,i)                                                   vax75
      ix2(i)=ixp(2,i)                                                   vax75
   10 ix3(i)=ixp(3,i)                                                   vax75
      return
      end
      subroutine bksrt1(lpntbk,lbcket,numbuk,numbin,x,xmin,
     1                  nsv,ipb,ipe,ricchr)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension lpntbk(1),lbcket(1),numbuk(1),x(3,1),nsv(1)
c
      do 20 i=1,numbin
      numbuk(i)=0
   20 lpntbk(i+1)=0
      do 30 i=ipb,ipe
      nbin=2+(x(1,nsv(i))-xmin)*ricchr
      lpntbk(nbin)=lpntbk(nbin)+1
   30 continue
      lpntbk(1)=1
      do 40 i=1,numbin
   40 lpntbk(i+1)=lpntbk(i)+lpntbk(i+1)
      do 50 i=ipb,ipe
      nbin=1+(x(1,nsv(i))-xmin)*ricchr
      lcbck=lpntbk(nbin)+numbuk(nbin)
      lbcket(lcbck)=i
      numbuk(nbin)=numbuk(nbin)+1
   50 continue
      return
      end
      subroutine bksrt2(lpntbk,lbcket,numbuk,numbin,x,xmin,
     1                  nsv,ipb,ipe,ricchr,lb)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension lpntbk(1),lbcket(1),numbuk(1),x(3,1),nsv(1),
     1          lb(1)
c
      do 20 i=1,numbin
      numbuk(i)=0
   20 lpntbk(i+1)=0
      do 30 i=ipb,ipe
      lbi=lb(i)
      nsvlb=nsv(lbi)
      xnsv =x(1,nsvlb)
      nbin=2+(xnsv-xmin)*ricchr
      lpntbk(nbin)=lpntbk(nbin)+1
   30 continue
      lpntbk(1)=1
      do 40 i=1,numbin
   40 lpntbk(i+1)=lpntbk(i)+lpntbk(i+1)
      do 50 i=ipb,ipe
      lbi=lb(i)
      nsvlb=nsv(lbi)
      xnsv =x(1,nsvlb)
      nbin=1+(xnsv-xmin)*ricchr
      lcbck=lpntbk(nbin)+numbuk(nbin)
      lbcket(lcbck)=lb(i)
      numbuk(nbin)=numbuk(nbin)+1
   50 continue
      return
      end
      subroutine stex(xn1,xn2,xn3,sn,tn,ier,irtn)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      common/bk21/xxx,yyy,zzz,fs1,fs2,fs3,ft1,ft2,ft3,sjunk(4)
      common/pmtr/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
      if (ix(3).eq.ix(4)) then
      call st (xn1,xn2,xn3,sn,tn,ier)
      return
      endif
      x13=xx1(1)+xx1(3)
      x42=xx1(4)+xx1(2)
      x31=xx1(3)-xx1(1)
      x24=xx1(2)-xx1(4)
      a1=.25*(x13+x42)
      b1=.25*(x31+x24)
      c1=.25*(x31-x24)
      d1=.25*(x13-x42)
      x13=xx2(1)+xx2(3)
      x42=xx2(4)+xx2(2)
      x31=xx2(3)-xx2(1)
      x24=xx2(2)-xx2(4)
      a2=.25*(x13+x42)
      b2=.25*(x31+x24)
      c2=.25*(x31-x24)
      d2=.25*(x13-x42)
      x13=xx3(1)+xx3(3)
      x42=xx3(4)+xx3(2)
      x31=xx3(3)-xx3(1)
      x24=xx3(2)-xx3(4)
      a3=.25*(x13+x42)
      b3=.25*(x31+x24)
      c3=.25*(x31-x24)
      d3=.25*(x13-x42)
      xg=a1
      yg=a2
      zg=a3
      sn=0.
      tn=0.
      e1=xs-xg
      e2=ys-yg
      e3=zs-zg
      a=b1**2+b2**2+b3**2
      b=b1*c1+b2*c2+b3*c3
      c=c1**2+c2**2+c3**2
      r1=b1*e1+b2*e2+b3*e3
      r2=c1*e1+c2*e2+c3*e3
      odet=1./(a*c-b**2)
      sn=(r1*c-r2*b)*odet
      tn=(r2*a-r1*b)*odet
      xg=a1+(b1+d1*tn)*sn+c1*tn
      yg=a2+(b2+d2*tn)*sn+c2*tn
      zg=a3+(b3+d3*tn)*sn+c3*tn
      e1=xs-xg
      e2=ys-yg
      e3=zs-zg
      pxs=b1+d1*tn
      pys=b2+d2*tn
      pzs=b3+d3*tn
      pxt=c1+d1*sn
      pyt=c2+d2*sn
      pzt=c3+d3*sn
      a=pxs**2+pys**2+pzs**2
      b=pxs*pxt+pys*pyt+pzs*pzt
      c=pxt**2+pyt**2+pzt**2
      odet=a*c-b*b                                                      vax75
      if (abs(odet).lt.1.e-14) then                                     vax75
      call st (xn1,xn2,xn3,sn,tn,ier)                                   vax75
      return                                                            vax75
      else                                                              vax75
      odet=1./odet                                                      vax75
      endif                                                             vax75
      r1=pxs*e1+pys*e2+pzs*e3
      r2=pxt*e1+pyt*e2+pzt*e3
c     odet=1./(a*c-b**2)                                                cray1
      sn=sn+(r1*c-r2*b)*odet
      tn=tn+(r2*a-r1*b)*odet
      xg=a1+(b1+d1*tn)*sn+c1*tn
      yg=a2+(b2+d2*tn)*sn+c2*tn
      zg=a3+(b3+d3*tn)*sn+c3*tn
      e1=xs-xg
      e2=ys-yg
      e3=zs-zg
      pxs=b1+d1*tn
      pys=b2+d2*tn
      pzs=b3+d3*tn
      pxt=c1+d1*sn
      pyt=c2+d2*sn
      pzt=c3+d3*sn
      a=pxs**2+pys**2+pzs**2
      b=pxs*pxt+pys*pyt+pzs*pzt
      c=pxt**2+pyt**2+pzt**2
      odet=a*c-b*b                                                      vax75
      if (abs(odet).lt.1.e-14) then                                     vax75
      call st (xn1,xn2,xn3,sn,tn,ier)                                   vax75
      return                                                            vax75
      else                                                              vax75
      odet=1./odet                                                      vax75
      endif                                                             vax75
      r1=pxs*e1+pys*e2+pzs*e3
      r2=pxt*e1+pyt*e2+pzt*e3
c     odet=1./(a*c-b**2)                                                cray1
      sn=sn+(r1*c-r2*b)*odet
      tn=tn+(r2*a-r1*b)*odet
      xg=a1+(b1+d1*tn)*sn+c1*tn
      yg=a2+(b2+d2*tn)*sn+c2*tn
      zg=a3+(b3+d3*tn)*sn+c3*tn
      ier=1
      sold=sn
      told=tn
      do 10 i=1,10
      pxs=b1+d1*tn
      pys=b2+d2*tn
      pzs=b3+d3*tn
      pxt=c1+d1*sn
      pyt=c2+d2*sn
      pzt=c3+d3*sn
      e1=xs-xg
      e2=ys-yg
      e3=zs-zg
      a=pxs**2+pys**2+pzs**2
      b=pxs*pxt+pys*pyt+pzs*pzt-e1*d1-e2*d2-e3*d3
      c=pxt**2+pyt**2+pzt**2
      odet=a*c-b*b                                                      vax75
      if (abs(odet).lt.1.e-14) then                                     vax75
      call st (xn1,xn2,xn3,sn,tn,ier)                                   vax75
      return                                                            vax75
      else                                                              vax75
      odet=1./odet                                                      vax75
      endif                                                             vax75
      r1=pxs*e1+pys*e2+pzs*e3
      r2=pxt*e1+pyt*e2+pzt*e3
c     odet=1./(a*c-b**2)                                                cray1
      sn=sn+(r1*c-r2*b)*odet
      tn=tn+(r2*a-r1*b)*odet
      xg=a1+(b1+d1*tn)*sn+c1*tn
      yg=a2+(b2+d2*tn)*sn+c2*tn
      zg=a3+(b3+d3*tn)*sn+c3*tn
      error=(sn-sold)**2+(tn-told)**2
      if (error/(1.e-30+sn**2+tn**2).lt.2.e-08) go to 20
      sold=sn
      told=tn
   10 continue
      if (abs(sn).gt.1.05.or.abs(tn).gt.1.05) ier=2
      go to 30
   20 ier=0
      if (abs(sn).gt.1.05.or.abs(tn).gt.1.05) ier=2
      if (irtn.eq.1.or.ier.eq.2) go to 30
c     xxx=a1+(b1+d1*tn)*sn+c1*tn
c     yyy=a2+(b2+d2*tn)*sn+c2*tn
c     zzz=a3+(b3+d3*tn)*sn+c3*tn
      xxx=xg
      yyy=yg
      zzz=zg
      pxs=b1+d1*tn
      pys=b2+d2*tn
      pzs=b3+d3*tn
      pxt=c1+d1*sn
      pyt=c2+d2*sn
      pzt=c3+d3*sn
      xn1=pys*pzt-pzs*pyt
      xn2=pzs*pxt-pxs*pzt
      xn3=pxs*pyt-pys*pxt
      omag=1./sqrt(xn1**2+xn2**2+xn3**2)
      xn1=omag*xn1
      xn2=omag*xn2
      xn3=omag*xn3
   30 tp=.25*(1.0+tn)
      tm=.25*(1.0-tn)
      sp=1.0+sn
      sm=1.0-sn
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      return
      end
      subroutine chupdt(irect,nsv,x,nrts,chrlen,stf)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/aux14/
     &x1(128),y1(128),z1(128),x2(128),y2(128),z2(128),
     &x3(128),y3(128),z3(128),x4(128),y4(128),z4(128),
     &a1(128),a2(128),sm(128)
      common/double/iprec,ncpw,unit
c
      dimension irect(4,*),x(3,*),nsv(*),stf(*)
c
      chrlen=0.
c
      ip=0
      ilen=128
      ng=nrts/128
      irem=nrts-128*ng
      if (irem.eq.0) go to 10
      ilen=irem
      ng=ng+1
c
   10 do 100 n=1,ng
c
      do 20 i=1,ilen
      sm(i)=.5+sign(.5*unit,stf(i+ip)-1.e-20)
      x1(i)=x(1,nsv(irect(1,i+ip)))
      y1(i)=x(2,nsv(irect(1,i+ip)))
      z1(i)=x(3,nsv(irect(1,i+ip)))
      x2(i)=x(1,nsv(irect(2,i+ip)))
      y2(i)=x(2,nsv(irect(2,i+ip)))
      z2(i)=x(3,nsv(irect(2,i+ip)))
      x3(i)=x(1,nsv(irect(3,i+ip)))
      y3(i)=x(2,nsv(irect(3,i+ip)))
      z3(i)=x(3,nsv(irect(3,i+ip)))
      x4(i)=x(1,nsv(irect(4,i+ip)))
      y4(i)=x(2,nsv(irect(4,i+ip)))
      z4(i)=x(3,nsv(irect(4,i+ip)))
   20 continue
c
      do 30 i=1,ilen
      a1(i)=sm(i)*((x1(i)-x3(i))**2+(y1(i)-y3(i))**2+(z1(i)-z3(i))**2)
      a2(i)=sm(i)*((x2(i)-x4(i))**2+(y2(i)-y4(i))**2+(z2(i)-z4(i))**2)
   30 continue
c
      do 40 i=1,ilen
      chrlen=  max(chrlen,a1(i),a2(i))
   40 continue
c
      ip=ip+ilen
      ilen=128
c
  100 continue
c
      chrlen=.75*sqrt(chrlen)
c
      return
      end
      subroutine st(xn1,xn2,xn3,s1,t1,ier)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      common/bk21/xxx,yyy,zzz,fs1,fs2,fs3,ft1,ft2,ft3,sjunk(4)
      common/pmtr/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
      common/double/iprec,ncpw,unit
      dimension ii(2)
c
      xx112=xx1(1)-xx1(2)
      xx134=xx1(3)-xx1(4)
      xx212=xx2(1)-xx2(2)
      xx234=xx2(3)-xx2(4)
      xx312=xx3(1)-xx3(2)
      xx334=xx3(3)-xx3(4)
      xx114=xx1(1)-xx1(4)
      xx123=xx1(2)-xx1(3)
      xx214=xx2(1)-xx2(4)
      xx223=xx2(2)-xx2(3)
      xx314=xx3(1)-xx3(4)
      xx323=xx3(2)-xx3(3)
      xx112p=xx1(1)+xx1(2)
      xx134p=xx1(3)+xx1(4)
      xx212p=xx2(1)+xx2(2)
      xx234p=xx2(3)+xx2(4)
      xx312p=xx3(1)+xx3(2)
      xx334p=xx3(3)+xx3(4)
      x1423 =-xx114-xx123
      y1423 =-xx214-xx223
      z1423 =-xx314-xx323
      x1423p= xx114-xx123
      y1423p= xx214-xx223
      z1423p= xx314-xx323
      if (ix(3).eq.ix(4)) go to 10
      anum=-xx112*(2.*xs-xx112p)
     1     -xx212*(2.*ys-xx212p)
     2     -xx312*(2.*zs-xx312p)
      s1=anum/(xx112*xx112+xx212*xx212+xx312*xx312)
      h1=0.5-.5*s1
      h2=0.5+.5*s1
      xxx=h1*xx1(1)+h2*xx1(2)
      yyy=h1*xx2(1)+h2*xx2(2)
      zzz=h1*xx3(1)+h2*xx3(2)
      ft1=x1423+s1*x1423p
      ft2=y1423+s1*y1423p
      ft3=z1423+s1*z1423p
      ftn1=.25*(ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz))
      anum=xx134*(2.*xs-xx134p)+
     1     xx234*(2.*ys-xx234p)+
     2     xx334*(2.*zs-xx334p)
      s1=anum/(xx134*xx134+xx234*xx234+xx334*xx334)
      h3=0.5+.5*s1
      h4=0.5-.5*s1
      xxx=h3*xx1(3)+h4*xx1(4)
      yyy=h3*xx2(3)+h4*xx2(4)
      zzz=h3*xx3(3)+h4*xx3(4)
      ft1=x1423+s1*x1423p
      ft2=y1423+s1*y1423p
      ft3=z1423+s1*z1423p
      ftn3=.25*(ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz))
      fs1=xx134-xx112
      fs2=xx234-xx212
      fs3=xx334-xx312
      anum=fs1*(4.*xs-xx112p-xx134p)+
     1     fs2*(4.*ys-xx212p-xx234p)+
     2     fs3*(4.*zs-xx312p-xx334p)
      s1=anum/(fs1*fs1+fs2*fs2+fs3*fs3)
      sp=.25*(1.0+s1)
      sm=.25*(1.0-s1)
      xxx=sm*(xx1(1)+xx1(4))+sp*(xx1(2)+xx1(3))
      yyy=sm*(xx2(1)+xx2(4))+sp*(xx2(2)+xx2(3))
      zzz=sm*(xx3(1)+xx3(4))+sp*(xx3(2)+xx3(3))
      ft1=x1423+s1*x1423p
      ft2=y1423+s1*y1423p
      ft3=z1423+s1*z1423p
      ftn2=ft1*(xs-xxx)+ft2*(ys-yyy)+ft3*(zs-zzz)
      ca=ftn1-.5*ftn2+ftn3
      t1=-1.-2.*ftn1/(ftn3-ftn1)
      go to 20
   10 ftn1=funct(s1,-1.15)
      ftn2=funct(s1, 1.15)
      t1=-1.15-2.30*ftn1/(ftn2-ftn1)
      if(abs(t1-1.0).lt.1.e-10) t1=.990
      go to 30
   20 cb=ftn1-ftn3
      arg=cb*cb-2.*ca*ftn2
      brg=  min(arg,abs(ca)-.0001)
      t2=(cb-sqrt(abs(arg)))/(2.*ca+1.e-20)
      scl=.5+sign(.5*unit,brg)
      t1=scl*t2+(1.-scl)*t1
   30 ftn1=funct(s1,t1)
      if (abs(s1).gt.1.020) go to 40
      if (abs(t1).gt.1.020) go to 40
      go to 50
   40 call salvge (ier,s1,t1)
   50 sct1=fs2*ft3-fs3*ft2
      sct2=fs3*ft1-fs1*ft3
      sct3=fs1*ft2-fs2*ft1
      xmagn=sqrt(sct1*sct1+sct2*sct2+sct3*sct3)
      xn1=sct1/xmagn
      xn2=sct2/xmagn
      xn3=sct3/xmagn
      return
      end
      subroutine salvex(ans,ilb,ierr,xn1,xn2,xn3,ss,tt,
     1                  l,x,irect,lmsr,msr,nseg,nmn)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/slv2/thk,isrch
      common/double/iprec,ncpw,unit
      dimension x(3,*),irect(4,*),lmsr(*),msr(*),nseg(*)
      dimension jlist(4,4),ilist(4)
c
      data jlist/1,2,4,3,4,1,3,2,2,1,3,4,4,2,3,1/
      isig=sign(1.*unit,ss)+1.
      jsig=sign(1.*unit,tt)+1.
      ipic=isig+jsig/2+1
      do 10 ii=1,4
      ind=irect(ii,l)
      do 5 jj=1,nmn
      if (msr(jj).ne.ind) go to 5
      ilist(ii)=jj
      go to 10
    5 continue
   10 continue
      do 200 ii=1,4
      j=ilist(jlist(ii,ipic))
      k=irect(jlist(ii,ipic),l)
      resltl=-1.e20
      num=nseg(j+1)-nseg(j)
      npt=nseg(j)-1
      do 40 lpp=1,num
      ll=lmsr(lpp+npt)
      llc=ll
      if (ilb.ne.irect(1,ll).and.ilb.ne.irect(2,ll)
     1.and.ilb.ne.irect(3,ll).and.ilb.ne.irect(4,ll)) go to 15
      ierr=3
      return
   15 do 20 jj=1,4
      if (irect(jj,ll).ne.k) go to 20
      j1=jj+1
      j2=jj-1
      if (jj.ne.3) go to 30
      if (irect(3,ll).eq.irect(4,ll)) j1=1
      go to 30
   20 continue
   30 if (j1.eq.5) j1=1
      if (j2.eq.0) j2=4
      m=irect(j1,ll)
      n=irect(j2,ll)
      call cross (x)
      if (result.le.0.0) go to 40
      l=ll
      go to 50
   40 continue
      l=lrsort
   50 do 60 jj=1,4
      nn=irect(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   60 xx3(jj)=x(3,nn)
      if (k.ne.ix(1)) go to 70
      k1=1
      k2=2
      k3=4
      go to 100
   70 if (k.ne.ix(2)) go to 80
      k1=2
      k2=3
      k3=1
      go to 100
   80 if (k.ne.ix(3)) go to 90
      k1=3
      k2=4
      k3=2
      if (ix(3).eq.ix(4)) k2=1
      go to 100
   90 if (k.ne.ix(4)) go to 100
      k1=4
      k2=1
      k3=3
  100 call ptime (k1,k2,k3,detv)
      if (detv.gt.0.) go to 200
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      ans=xn1*(xs1-amx)+xn2*(ys1-amy)+xn3*(zs1-amz)
      if (isrch.eq.1) call thkgs(xx1,xx2,xx3,thk)
      if (ans.lt.-thk) go to 200
      if (ierr.eq.0) return
  200 continue
      ierr=3
      return
      end
      subroutine ptime(k1,k2,k3,det)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      aj1=xs-xx1(k1)
      aj2=ys-xx2(k1)
      aj3=zs-xx3(k1)
      aj4=xx1(k2)-xx1(k1)
      aj5=xx2(k2)-xx2(k1)
      aj6=xx3(k2)-xx3(k1)
      aj7=xx1(k3)-xx1(k1)
      aj8=xx2(k3)-xx2(k1)
      aj9=xx3(k3)-xx3(k1)
      det=aj1*aj5*aj9+aj2*aj6*aj7+aj3*aj4*aj8-aj3*aj5*aj7-aj2*aj4*aj9
     1 -aj1*aj6*aj8
      return
      end
      subroutine salvge(ier,s,t)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/det,h(20),
     1           p11,p12,p13,p14,p1(16),p21,p22,p23,p24,p2(16),p3(38)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      common/bk21/xxx,yyy,zzz,ps1,ps2,ps3,pt1,pt2,pt3,sp,sm,tp,tm
      common/pmtr/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
      xx112=xx1(1)-xx1(2)
      xx134=xx1(3)-xx1(4)
      xx212=xx2(1)-xx2(2)
      xx234=xx2(3)-xx2(4)
      xx312=xx3(1)-xx3(2)
      xx334=xx3(3)-xx3(4)
      xx114=xx1(1)-xx1(4)
      xx123=xx1(2)-xx1(3)
      xx214=xx2(1)-xx2(4)
      xx223=xx2(2)-xx2(3)
      xx314=xx3(1)-xx3(4)
      xx323=xx3(2)-xx3(3)
      xx112p=xx1(1)+xx1(2)
      xx134p=xx1(3)+xx1(4)
      xx212p=xx2(1)+xx2(2)
      xx234p=xx2(3)+xx2(4)
      xx312p=xx3(1)+xx3(2)
      xx334p=xx3(3)+xx3(4)
      dist1=(xx1(1)-xx1(3))**2+(xx2(1)-xx2(3))**2+(xx3(1)-xx3(3))**2
      dist2=(xx1(2)-xx1(4))**2+(xx2(2)-xx2(4))**2+(xx3(2)-xx3(4))**2
      xavg=.25*(xx1(1)+xx1(2)+xx1(3)+xx1(4))
      yavg=.25*(xx2(1)+xx2(2)+xx2(3)+xx2(4))
      zavg=.25*(xx3(1)+xx3(2)+xx3(3)+xx3(4))
      dists=sqrt((xs-xavg)**2+(ys-yavg)**2+(zs-zavg)**2)
      distd=sqrt(  max(dist1,dist2))
      if (dists.lt.0.70*distd) go to 10
      ier=1
      return
   10 if (abs(t).gt.1.0) t=t/abs(t)
      if (ix(3).ne.ix(4)) go to 20
      if (t.ge.0.990) s=0.
      if (abs(t-1.0).lt.1.e-10) t=.990
   20 tp=.25*(1.0+t)
      tm=.25*(1.0-t)
      xxx=0.0
      yyy=0.0
      zzz=0.0
      ps1=tp*xx134-tm*xx112
      ps2=tp*xx234-tm*xx212
      ps3=tp*xx334-tm*xx312
      anum=ps1*(xs-tm*xx112p-tp*xx134p)+ps2*(ys-tm*xx212p-tp*xx234p)+ps3
     1 *(zs-tm*xx312p-tp*xx334p)
      s=anum/(ps1*ps1+ps2*ps2+ps3*ps3)
      if (abs(s).gt.1.0) s=s/abs(s)
      sp=1.0+s
      sm=1.0-s
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      do 30 i=1,4
      xxx=xxx+h(i)*xx1(i)
      yyy=yyy+h(i)*xx2(i)
   30 zzz=zzz+h(i)*xx3(i)
      pt1=-sm*xx114-sp*xx123
      pt2=-sm*xx214-sp*xx223
      pt3=-sm*xx314-sp*xx323
c
      return
      end
      function funct(s,t)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/det,h(20),
     1           p11,p12,p13,p14,p1(16),p21,p22,p23,p24,p2(16),p3(38)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs,ys,zs,sig(3),epx
      common/bk21/xxx,yyy,zzz,ps1,ps2,ps3,pt1,pt2,pt3,sp,sm,tp,tm
      common/pmtr/xx112,xx134,xx212,xx234,xx312,xx334,xx112p,xx134p,
     1      xx212p,xx234p,xx312p,xx334p,xx114,xx123,xx214,
     2      xx223,xx314,xx323,x1423,y1423,z1423,x1423p,y1423p,z1423p
      tp=.25*(1.0+t)
      tm=.25*(1.0-t)
      ps1=tp*xx134-tm*xx112
      ps2=tp*xx234-tm*xx212
      ps3=tp*xx334-tm*xx312
      s=(ps1*(xs-tm*xx112p-tp*xx134p)+
     1   ps2*(ys-tm*xx212p-tp*xx234p)+
     2   ps3*(zs-tm*xx312p-tp*xx334p))/(ps1*ps1+ps2*ps2+ps3*ps3)
      sp=1.0+s
      sm=1.0-s
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      xxx=h(1)*xx1(1)+h(2)*xx1(2)+h(3)*xx1(3)+h(4)*xx1(4)
      yyy=h(1)*xx2(1)+h(2)*xx2(2)+h(3)*xx2(3)+h(4)*xx2(4)
      zzz=h(1)*xx3(1)+h(2)*xx3(2)+h(3)*xx3(3)+h(4)*xx3(4)
      pt1=x1423+s*x1423p
      pt2=y1423+s*y1423p
      pt3=z1423+s*z1423p
      funct=pt1*(xs-xxx)+pt2*(ys-yyy)+pt3*(zs-zzz)
      return
      end
      subroutine shapef(h,s,t)
c     implicit double precision (a-h,o-z)                                    dp
      dimension h(4)
      sp=1.0+s
      sm=1.0-s
      tp=.25*(1.0+t)
      tm=.25*(1.0-t)
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      return
      end
      subroutine cross(x)
c     implicit double precision (a-h,o-z)                                    dp
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      dimension x(3,1)
      c11=x(1,m)-x(1,k)
      c12=x(2,m)-x(2,k)
      c13=x(3,m)-x(3,k)
      c21=x(1,n)-x(1,k)
      c22=x(2,n)-x(2,k)
      c23=x(3,n)-x(3,k)
      c31=c12*c23-c13*c22
      c32=c13*c21-c11*c23
      c33=c11*c22-c12*c21
      sl1=x(1,i)-x(1,k)
      sl2=x(2,i)-x(2,k)
      sl3=x(3,i)-x(3,k)
      da=(c31*sl1+c32*sl2+c33*sl3)/(c31*c31+c32*c32+c33*c33)
      sl1=sl1-da*c31
      sl2=sl2-da*c32
      sl3=sl3-da*c33
      c1dc1=c11*c11+c12*c12+c13*c13
      c2dc2=c21*c21+c22*c22+c23*c23
      sldc2=sl1*c21+sl2*c22+sl3*c23
      c1dc2=c11*c21+c12*c22+c13*c23
      sldc1=sl1*c11+sl2*c12+sl3*c13
      sldsl=sl1*sl1+sl2*sl2+sl3*sl3
      dprd1=sldc2/sqrt(c2dc2)
      dprd2=sldc1/sqrt(c1dc1)
      dprdf=  max(dprd1,dprd2)
      if (resltl.gt.dprdf) go to 10
      lrsort=llc
      resltl=dprdf
   10 resul1=c1dc1*sldc2-c1dc2*sldc1
      resul2=sldc1*sldc2-c1dc2*sldsl
      resul3=c2dc2*sldc1-c1dc2*sldc2
      result=-1.0
      if (  min(resul1,resul2,resul3).lt.0.0) return
      result=1.0
      return
      end
      subroutine slide1(x,e,irects,lnsv,nsv,nsn,xms,pres,nseg,zfsa,code,
     1 saven,bcs)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,*),e(3,*),irects(4,*),lnsv(*),nsv(*),nseg(*),xms(*),
     1          pres(*),zfsa(*),code(*),saven(3,*),bcs(*)
      do 10 ii=1,nsn
      k=nsv(ii)
      call vcarea (xn1,xn2,xn3,area,ii,k,nseg,lnsv,irects,x,code,bcs)
      saven(1,ii)=xn1
      saven(2,ii)=xn2
      saven(3,ii)=xn3
      forcen=(xn1*e(1,k)+xn2*e(2,k)+xn3*e(3,k))/xms(k)
      zfsa(ii)=4./(xms(k)*area)
      pres(ii)=4.*forcen/area
   10 continue
      return
      end
      subroutine slide2(x,e,irects,lnsv,nsv,msr,iloc,irtl,
     1 nmn,nsn,code,bcs,pres,presm,irectm,lmsr,nseg,nrctm,xms,zfac,
     2 zfsa,zfsmn,irctsi,irctmi)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
c
      dimension x(3,*),e(3,*),irects(4,*),lnsv(*),nsv(*),msr(*),iloc(*),
     1 irtl(*),pres(*),presm(*),irectm(4,*),xms(*),lmsr(*),nseg(*),
     2 code(*),bcs(*),zfsa(*),zfsmn(*),irctsi(4,*),irctmi(4,*)
c
      do 30 ii=1,nmn
      zfsmn(ii)=0.0
      presm(ii)=0.0
      i=msr(ii)
      l=irtl(ii)
      do 10 jj=1,4
      nn=irects(jj,l)
      iy(jj)=irctsi(jj,l)
      ix(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
   10 xx3(jj)=x(3,nn)
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,1)
      tp=.25*(1.0+tt)
      tm=.25*(1.0-tt)
      sp=1.0+ss
      sm=1.0-ss
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      if (ierr.ne.0) then
      ierr=0
      call st   (xn1,xn2,xn3,ss,tt,ierr)
      if (ierr.eq.1) go to 30
      endif
      zfsmn(ii)=zfsa(iy(1))*h(1)+zfsa(iy(2))*h(2)+zfsa(iy(3))*h(3)
     1         +zfsa(iy(4))*h(4)
      presm(ii)=pres(iy(1))*h(1)+pres(iy(2))*h(2)+pres(iy(3))*h(3)
     1         +pres(iy(4))*h(4)
   30 continue
      do 60 ii=1,nrctm
      f=0.
      do 40 i=1,4
      j=irectm(i,ii)
      k=irctmi(i,ii)
      f=f+presm(k)
      xx1(i)=x(1,j)
      xx2(i)=x(2,j)
   40 xx3(i)=x(3,j)
      fs1=-xx1(1)+xx1(2)+xx1(3)-xx1(4)
      fs2=-xx2(1)+xx2(2)+xx2(3)-xx2(4)
      fs3=-xx3(1)+xx3(2)+xx3(3)-xx3(4)
      ft1=-xx1(1)-xx1(2)+xx1(3)+xx1(4)
      ft2=-xx2(1)-xx2(2)+xx2(3)+xx2(4)
      ft3=-xx3(1)-xx3(2)+xx3(3)+xx3(4)
      prs=f/64.
      tr1=(fs2*ft3-fs3*ft2)*prs
      tr2=(fs3*ft1-fs1*ft3)*prs
      tr3=(fs1*ft2-fs2*ft1)*prs
      e(1,irectm(1,ii))=e(1,irectm(1,ii))-tr1*xms(irectm(1,ii))
      e(2,irectm(1,ii))=e(2,irectm(1,ii))-tr2*xms(irectm(1,ii))
      e(3,irectm(1,ii))=e(3,irectm(1,ii))-tr3*xms(irectm(1,ii))
      e(1,irectm(2,ii))=e(1,irectm(2,ii))-tr1*xms(irectm(2,ii))
      e(2,irectm(2,ii))=e(2,irectm(2,ii))-tr2*xms(irectm(2,ii))
      e(3,irectm(2,ii))=e(3,irectm(2,ii))-tr3*xms(irectm(2,ii))
      e(1,irectm(3,ii))=e(1,irectm(3,ii))-tr1*xms(irectm(3,ii))
      e(2,irectm(3,ii))=e(2,irectm(3,ii))-tr2*xms(irectm(3,ii))
      e(3,irectm(3,ii))=e(3,irectm(3,ii))-tr3*xms(irectm(3,ii))
      e(1,irectm(4,ii))=e(1,irectm(4,ii))-tr1*xms(irectm(4,ii))
      e(2,irectm(4,ii))=e(2,irectm(4,ii))-tr2*xms(irectm(4,ii))
      e(3,irectm(4,ii))=e(3,irectm(4,ii))-tr3*xms(irectm(4,ii))
   60 continue
      do 70 ii=1,nmn
      k=msr(ii)
      call vcarea (xn1,xn2,xn3,area,ii,k,nseg,lmsr,irectm,x,code,bcs)
      area=4./(xms(k)*area)
      fctr=area/(area+zfsmn(ii))-1.0
      ib=code(k)
      anchg=(xn1*e(1,k)+xn2*e(2,k)+xn3*e(3,k))*fctr
      e(1,k)=e(1,k)+anchg*xn1
      e(2,k)=e(2,k)+anchg*xn2
      e(3,k)=e(3,k)+anchg*xn3
      if (ib.ne.0) call bound (bcs,e(1,k),e(2,k),e(3,k),ib)
   70 continue
      return
      end
      subroutine slide3(x,a,v,irect,lmsr,msr,nsv,iloc,irtl,
     1 nsn,nmn,code,bcs,nseg,irects,lnsv,nsegs,saven)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk02/iburn,dt1,dt2,isdo
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
c
      dimension x(3,*),v(3,*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
     1 irtl(*),a(3,*),code(*),bcs(*),nseg(*),irects(4,*),lnsv(*),
     2 nsegs(*),saven(3,*)
c
      do 30 ii=1,nsn
      i=nsv(ii)
      l=irtl(ii)
      ix(1)=irect(1,l)
      iy(1)=irect(1,l)
      xx1(1)=x(1,irect(1,l))
      xx2(1)=x(2,irect(1,l))
      xx3(1)=x(3,irect(1,l))
      ix(2)=irect(2,l)
      iy(2)=irect(2,l)
      xx1(2)=x(1,irect(2,l))
      xx2(2)=x(2,irect(2,l))
      xx3(2)=x(3,irect(2,l))
      ix(3)=irect(3,l)
      iy(3)=irect(3,l)
      xx1(3)=x(1,irect(3,l))
      xx2(3)=x(2,irect(3,l))
      xx3(3)=x(3,irect(3,l))
      ix(4)=irect(4,l)
      iy(4)=irect(4,l)
      xx1(4)=x(1,irect(4,l))
      xx2(4)=x(2,irect(4,l))
      xx3(4)=x(3,irect(4,l))
      xs1=x(1,i)
      ys1=x(2,i)
      zs1=x(3,i)
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,1)
      tp=.25*(1.0+tt)
      tm=.25*(1.0-tt)
      sp=1.0+ss
      sm=1.0-ss
      h(1)=tm*sm
      h(2)=tm*sp
      h(3)=tp*sp
      h(4)=tp*sm
      if (ierr.ne.0) then
      ierr=0
      call st   (xn1,xn2,xn3,ss,tt,ierr)
      if (ierr.eq.1) go to 30
      endif
      xn1=saven(1,ii)
      xn2=saven(2,ii)
      xn3=saven(3,ii)
      ax=a(1,iy(1))*h(1)+a(1,iy(2))*h(2)+a(1,iy(3))*h(3)+a(1,iy(4))*h(4)
      ay=a(2,iy(1))*h(1)+a(2,iy(2))*h(2)+a(2,iy(3))*h(3)+a(2,iy(4))*h(4)
      az=a(3,iy(1))*h(1)+a(3,iy(2))*h(2)+a(3,iy(3))*h(3)+a(3,iy(4))*h(4)
      vx=v(1,iy(1))*h(1)+v(1,iy(2))*h(2)+v(1,iy(3))*h(3)+v(1,iy(4))*h(4)
      vy=v(2,iy(1))*h(1)+v(2,iy(2))*h(2)+v(2,iy(3))*h(3)+v(2,iy(4))*h(4)
      vz=v(3,iy(1))*h(1)+v(3,iy(2))*h(2)+v(3,iy(3))*h(3)+v(3,iy(4))*h(4)
      amn=xn1*ax+xn2*ay+xn3*az
      vmn=xn1*vx+xn2*vy+xn3*vz
      amod=amn-xn1*a(1,i)-xn2*a(2,i)-xn3*a(3,i)
      vmod=vmn-xn1*v(1,i)-xn2*v(2,i)-xn3*v(3,i)
      a(1,i)=a(1,i)+amod*xn1
      a(2,i)=a(2,i)+amod*xn2
      a(3,i)=a(3,i)+amod*xn3
      v(1,i)=v(1,i)+vmod*xn1
      v(2,i)=v(2,i)+vmod*xn2
      v(3,i)=v(3,i)+vmod*xn3
      ib=code(i)
      if (ib.eq.0) go to 30
      call bound (bcs,a(1,i),a(2,i),a(3,i),ib)
      call bound (bcs,v(1,i),v(2,i),v(3,i),ib)
   30 continue
      return
      end
      subroutine vcarea(xn1,xn2,xn3,area,ii,k,nseg,iloc,irect,
     1 x,code,bcs)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      dimension nseg(*),iloc(*),irect(4,*),x(3,*),code(*),bcs(*)
      area=0.
      xn1=0.
      xn2=0.
      xn3=0.
      num=nseg(ii+1)-nseg(ii)
      npt=nseg(ii)-1
      do 60 jj=1,num
      l=iloc(npt+jj)
      do 10 kk=1,4
      nn=irect(kk,l)
      ix(kk)=1-min0(1,iabs(irect(kk,l)-k))
      xx1(kk)=x(1,irect(kk,l))
      xx2(kk)=x(2,irect(kk,l))
   10 xx3(kk)=x(3,irect(kk,l))
      ifac=min0(1,iabs(irect(3,l)-irect(4,l)))
      ix(4)=ifac*ix(4)
      k1=  ix(1)+2*ix(2)+3*ix(3)+4*ix(4)
      k2=2*ix(1)+3*ix(2)+ifac*4*ix(3)+ix(4)+1-ifac
      k3=4*ix(1)+  ix(2)+2*ix(3)+3*ix(4)
      aj4=xx1(k2)-xx1(k1)
      aj5=xx2(k2)-xx2(k1)
      aj6=xx3(k2)-xx3(k1)
      aj7=xx1(k3)-xx1(k1)
      aj8=xx2(k3)-xx2(k1)
      aj9=xx3(k3)-xx3(k1)
      fs1=-xx1(1)+xx1(2)+xx1(3)-xx1(4)
      fs2=-xx2(1)+xx2(2)+xx2(3)-xx2(4)
      fs3=-xx3(1)+xx3(2)+xx3(3)-xx3(4)
      ft1=-xx1(1)-xx1(2)+xx1(3)+xx1(4)
      ft2=-xx2(1)-xx2(2)+xx2(3)+xx2(4)
      ft3=-xx3(1)-xx3(2)+xx3(3)+xx3(4)
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      areai=sqrt((e*g-f*f)/16.)
      xn1=xn1+areai*(aj5*aj9-aj6*aj8)
      xn2=xn2+areai*(aj6*aj7-aj4*aj9)
      xn3=xn3+areai*(aj4*aj8-aj5*aj7)
      area =area+areai
   60 continue
      ib=code(k)
      if (ib.ne.0) call bound(bcs,xn1,xn2,xn3,ib)
      xmg=sqrt(xn1*xn1+xn2*xn2+xn3*xn3)
      xn1=xn1/xmg
      xn2=xn2/xmg
      xn3=xn3/xmg
      return
      end
      subroutine tbrk(xmt,xmr,at,ar,is,im,im1,im2,
     1                epe,volm,ep,fail,ip,isp,nsn,nmn,
     2                alpha,numels,ntbsl,nods,nodm)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xmt(1),xmr(1),at(1),ar(1),is(1),im(1),
     1          im1(1),im2(1),epe(1),volm(1),ep(1),fail(1),
     2          nsn(1),nmn(1),ip(1),isp(1),alpha(1)
      call tbrk1(ep,epe,volm,ip,isp,nods)
      i1=1
      i2=1
      do 10 i=1,ntbsl
      nsni=nsn(i)
      nmni=nmn(i)
      call tbrk2(xmt,xmr,at,ar,is(i1),im(i2),im1(i1),im2(i1),
     1           epe,volm,ep(i1),fail(i1),alpha(i1),nsni,nmni)
      i1=i1+nsni
      i2=i2+nmni
   10 continue
      do 20 i=1,numels
      epe(i)=0.
      volm(i)=0.
   20 continue
      return
      end
      subroutine tbrk1(ep,epe,volm,ip,isp,nsn)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      dimension ep(1),epe(1),volm(1),ip(1),isp(1)
      do 100 i=1,nsn
      strn=0.
      vol=0.
      ibgn=ip(i)
      iend=ip(i+1)-1
      do 40 j=ibgn,iend
      strn=strn+epe(isp(j))
      vol=vol+volm(isp(j))
   40 continue
      ep(i)=strn/vol
  100 continue
      do 200 i=1,numels
      epe(i)=0.
      volm(i)=0.
  200 continue
      return
      end
      subroutine tbrk2(xmt,xmr,at,ar,is,im,im1,im2,am1,am2,
     1                 ep,fail,alpha,nsn,nmn)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/
     1 a1(128),a2(128),a3(128),a4(128),a5(128),a6(128),
     1 b1(128),b2(128),b3(128),b4(128),b5(128),b6(128),
     1 c1(128),c2(128),c3(128),c4(128),c5(128),c6(128),
     1 am3(128),am4(128),em1(128),em2(128),alf3(128),
     1 bet3(128),bet4(128),alf(128),bet(128),epm(128),
     1 alf4(128)
      common/double/iprec,ncpw,unit
      dimension xmt(1),xmr(1),at(1),ar(1),is(1),im1(1),im2(1),
     1          am1(1),am2(1),ep(1),fail(1),alpha(1),im(1)
      ip=0
      nblk=nsn/128
      irem=nsn-128*nblk
      len=128
      if (irem.eq.0) go to 10
      len=irem
      nblk=nblk+1
   10 do 30 i=1,nmn
      am1(i)=xmt(im(i))
      am2(i)=xmr(im(i))
      xmt(im(i))=1./xmt(im(i))
      xmr(im(i))=1./xmr(im(i))
   30 continue
      do 70 i=1,nsn
      ep(i)=.50+.50*sign(1.*unit,fail(i)-ep(i))
   70 continue
      do 200 n=1,nblk
      do 90 i=1,len
      isi=3*is(ip+i)-2
      a1(i)=at(isi)
      a2(i)=at(isi+1)
      a3(i)=at(isi+2)
      a4(i)=ar(isi)
      a5(i)=ar(isi+1)
      a6(i)=ar(isi+2)
      am3(i)=xmt(is(ip+i))
      am4(i)=xmr(is(ip+i))
   90 continue
      do 100 i=1,len
      j=ip+i
      am3(i)=1./am3(i)
      am4(i)=1./am4(i)
      alf(i)=ep(j)*alpha(j)
      bet(i)=ep(j)-alf(i)
  100 continue
      do 120 i=1,len
      b1(i)=alf(i)*a1(i)
      b2(i)=alf(i)*a2(i)
      b3(i)=alf(i)*a3(i)
      b4(i)=alf(i)*a4(i)
      b5(i)=alf(i)*a5(i)
      b6(i)=alf(i)*a6(i)
      em1(i)=alf(i)*am3(i)
      em2(i)=alf(i)*am4(i)
  120 continue
      do 140 i=1,len
      imi=3*im1(ip+i)-2
      at(imi)=at(imi)+b1(i)
      at(imi+1)=at(imi+1)+b2(i)
      at(imi+2)=at(imi+2)+b3(i)
      ar(imi)=ar(imi)+b4(i)
      ar(imi+1)=ar(imi+1)+b5(i)
      ar(imi+2)=ar(imi+2)+b6(i)
      xmt(im1(ip+i))=xmt(im1(ip+i))+em1(i)
      xmr(im1(ip+i))=xmr(im1(ip+i))+em2(i)
  140 continue
      do 160 i=1,len
      b1(i)=bet(i)*a1(i)
      b2(i)=bet(i)*a2(i)
      b3(i)=bet(i)*a3(i)
      b4(i)=bet(i)*a4(i)
      b5(i)=bet(i)*a5(i)
      b6(i)=bet(i)*a6(i)
      em1(i)=bet(i)*am3(i)
      em2(i)=bet(i)*am4(i)
  160 continue
      do 170 i=1,len
      imi=3*im2(ip+i)-2
      at(imi)=at(imi)+b1(i)
      at(imi+1)=at(imi+1)+b2(i)
      at(imi+2)=at(imi+2)+b3(i)
      ar(imi)=ar(imi)+b4(i)
      ar(imi+1)=ar(imi+1)+b5(i)
      ar(imi+2)=ar(imi+2)+b6(i)
      xmt(im2(ip+i))=xmt(im2(ip+i))+em1(i)
      xmr(im2(ip+i))=xmr(im2(ip+i))+em2(i)
  170 continue
      ip=ip+len
      len=128
  200 continue
      do 300 i=1,nmn
      imi=3*im(i)-2
      om=1./xmt(im(i))
      oj=1./xmr(im(i))
      at(imi)=om*at(imi)
      at(imi+1)=om*at(imi+1)
      at(imi+2)=om*at(imi+2)
      ar(imi)=oj*ar(imi)
      ar(imi+1)=oj*ar(imi+1)
      ar(imi+2)=oj*ar(imi+2)
  300 continue
      ip=0
      len=irem
      do 400 n=1,nblk
      do 310 i=1,len
      isi=3*is(ip+i)-2
      a1(i)=at(isi)
      a2(i)=at(isi+1)
      a3(i)=at(isi+2)
      a4(i)=ar(isi)
      a5(i)=ar(isi+1)
      a6(i)=ar(isi+2)
      am3(i)=xmt(is(ip+i))
      am4(i)=xmr(is(ip+i))
      im1i=3*im1(ip+i)-2
      b1(i)=at(im1i)
      b2(i)=at(im1i+1)
      b3(i)=at(im1i+2)
      b4(i)=ar(im1i)
      b5(i)=ar(im1i+1)
      b6(i)=ar(im1i+2)
      im2i=3*im2(ip+i)-2
      c1(i)=at(im2i)
      c2(i)=at(im2i+1)
      c3(i)=at(im2i+2)
      c4(i)=ar(im2i)
      c5(i)=ar(im2i+1)
      c6(i)=ar(im2i+2)
  310 continue
      do 320 i=1,len
      j=ip+i
      am3(i)=1./am3(i)
      am4(i)=1./am4(i)
      alf(i)=ep(j)*alpha(j)
      bet(i)=ep(j)-alf(i)
      epm(i)=1.-ep(j)
  320 continue
      do 340 i=1,len
      alf3(i)=alf(i)*am3(i)
      alf4(i)=alf(i)*am4(i)
      bet3(i)=bet(i)*am3(i)
      bet4(i)=bet(i)*am4(i)
  340 continue
      do 360 i=1,len
      a1(i)=epm(i)*a1(i)+alf3(i)*b1(i)+bet3(i)*c1(i)
      a2(i)=epm(i)*a2(i)+alf3(i)*b2(i)+bet3(i)*c2(i)
      a3(i)=epm(i)*a3(i)+alf3(i)*b3(i)+bet3(i)*c3(i)
      a4(i)=epm(i)*a4(i)+alf4(i)*b4(i)+bet4(i)*c4(i)
      a5(i)=epm(i)*a5(i)+alf4(i)*b5(i)+bet4(i)*c5(i)
      a6(i)=epm(i)*a6(i)+alf4(i)*b6(i)+bet4(i)*c6(i)
  360 continue
      do 380 i=1,len
      isi=3*is(ip+i)-2
      at(isi)=a1(i)
      at(isi+1)=a2(i)
      at(isi+2)=a3(i)
      ar(isi)=a4(i)
      ar(isi+1)=a5(i)
      ar(isi+2)=a6(i)
  380 continue
      ip=ip+len
      len=128
  400 continue
      do 500 i=1,nmn
      xmt(im(i))=am1(i)
      xmr(im(i))=am2(i)
      imi=3*im(i)-2
      om=1./am1(i)
      oj=1./am2(i)
      at(imi)=om*at(imi)
      at(imi+1)=om*at(imi+1)
      at(imi+2)=om*at(imi+2)
      ar(imi)=oj*ar(imi)
      ar(imi+1)=oj*ar(imi+1)
      ar(imi+2)=oj*ar(imi+2)
  500 continue
      return
      end
      subroutine tnwf(xmt,xmr,at,ar,is,
     1                epe,volm,ep,fail,ip,isp,
     2                numels,ntnwf,ntie,nl)
c     implicit double precision (a-h,o-z)                                    dp
c.... tied node sets with failure
c
      dimension xmt(*),xmr(*),at(*),ar(*),is(*),
     1          epe(*),volm(*),ep(*),fail(*),
     2          ip(*),isp(*),ntie(*),nl(*)
c
      i1=1
      i2=1
      i3=1
      do 10 i=1,ntnwf
      ntiei=ntie(i)
      nli=nl(i)
      call tnwf1(ep,epe,volm,ip(i1+i-1),isp(i3),
     1           ntiei,iend)
      call tnwf2(xmt,xmr,at,ar,is(i2),
     1           fail(i1),ep,ntiei,nli)
      i1=i1+ntiei
      i2=i2+nli*ntiei
      i3=i3+iend
   10 continue
c
c***  zero strain and volume
      do 20 i=1,numels
      epe(i)=0.
      volm(i)=0.
   20 continue
c
      return
      end
      subroutine tnwf1(ep,epe,volm,ip,isp,ntie,iend)
c     implicit double precision (a-h,o-z)                                    dp
c
c***  calculates volume-weighted plastic strain
c
c***  called from fem3d
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
c
      dimension ep(*),epe(*),volm(*),ip(*),isp(*)
c
      do 100 i=1,ntie
      strn=0.
      vol=0.
      ibgn=ip(i)
      iend=ip(i+1)-1
      do 40 j=ibgn,iend
      strn=strn+epe(isp(j))
      vol=vol+volm(isp(j))
   40 continue
      ep(i)=strn/vol
  100 continue
c
      return
      end
      subroutine tnwf2(xmt,xmr,ft,fr,is,fail,pw,nties,nl)
c     implicit double precision (a-h,o-z)                                    dp
c
c***  tied nodes with failure
c
      common/aux14/
     1 em(128),er(128),ema(128),era(128),
     2 f1(128),f2(128),f3(128),f4(128),f5(128),f6(128),
     3 f1a(128),f2a(128),f3a(128),f4a(128),f5a(128),f6a(128),
     4 alf(128),bet(128)
      common/double/iprec,ncpw,unit
c
      dimension xmt(*),xmr(*),ft(*),fr(*),is(nties,*),fail(*),pw(*)
c
      ip=0
      nblk=nties/128
      irem=nties-128*nblk
      len=128
      if (irem.eq.0) go to 10
      nblk=nblk+1
      len=irem
   10 do 400 nb=1,nblk
c
c***  accumulate forces and masses
      do 30 i=1,len
      isi=is(ip+i,1)
      isip=3*isi-2
      em(i)=xmt(isi)
      er(i)=xmr(isi)
      f1(i)=ft(isip)
      f2(i)=ft(isip+1)
      f3(i)=ft(isip+2)
      f4(i)=fr(isip)
      f5(i)=fr(isip+1)
      f6(i)=fr(isip+2)
   30 continue
c
      do 40 i=1,len
      em(i)=1./em(i)
      er(i)=1./er(i)
   40 continue
c
      do 100 l=2,nl
c
      do 50 i=1,len
      isi=is(ip+i,l)
      isip=3*isi-2
      ema(i)=xmt(isi)
      era(i)=xmr(isi)
      f1a(i)=ft(isip)
      f2a(i)=ft(isip+1)
      f3a(i)=ft(isip+2)
      f4a(i)=fr(isip)
      f5a(i)=fr(isip+1)
      f6a(i)=fr(isip+2)
   50 continue
c
      do 60 i=1,len
      em(i)=em(i)+1./ema(i)
      er(i)=er(i)+1./era(i)
      f1(i)=f1(i)+f1a(i)
      f2(i)=f2(i)+f2a(i)
      f3(i)=f3(i)+f3a(i)
      f4(i)=f4(i)+f4a(i)
      f5(i)=f5(i)+f5a(i)
      f6(i)=f6(i)+f6a(i)
   60 continue
c
  100 continue
c
      do 110 i=1,len
      em(i)=1./em(i)
      er(i)=1./er(i)
  110 continue
c
c***  tied acceleration and flags
      do 120 i=1,len
      alf(i)=.5+.5*sign(1.*unit,fail(ip+i)-pw(ip+i))
      bet(i)=1.-alf(i)
      f1(i)=em(i)*f1(i)
      f2(i)=em(i)*f2(i)
      f3(i)=em(i)*f3(i)
      f4(i)=er(i)*f4(i)
      f5(i)=er(i)*f5(i)
      f6(i)=er(i)*f6(i)
  120 continue
c
c***  distribute forces
      do 200 l=1,nl
c
      do 130 i=1,len
      isi=is(ip+i,l)
      isip=3*isi-2
      ema(i)=xmt(isi)
      era(i)=xmr(isi)
      f1a(i)=ft(isip)
      f2a(i)=ft(isip+1)
      f3a(i)=ft(isip+2)
      f4a(i)=fr(isip)
      f5a(i)=fr(isip+1)
      f6a(i)=fr(isip+2)
  130 continue
c
      do 140 i=1,len
      ema(i)=alf(i)/ema(i)
      era(i)=alf(i)/era(i)
  140 continue
c
      do 160 i=1,len
      f1a(i)=bet(i)*f1a(i)+ema(i)*f1(i)
      f2a(i)=bet(i)*f2a(i)+ema(i)*f2(i)
      f3a(i)=bet(i)*f3a(i)+ema(i)*f3(i)
      f4a(i)=bet(i)*f4a(i)+era(i)*f4(i)
      f5a(i)=bet(i)*f5a(i)+era(i)*f5(i)
      f6a(i)=bet(i)*f6a(i)+era(i)*f6(i)
  160 continue
c
      do 180 i=1,len
      isi=is(ip+i,l)
      isip=3*isi-2
      ft(isip)=f1a(i)
      ft(isip+1)=f2a(i)
      ft(isip+2)=f3a(i)
      fr(isip)=f4a(i)
      fr(isip+1)=f5a(i)
      fr(isip+2)=f6a(i)
  180 continue
c
  200 continue
c
      ip=ip+len
      len=128
c
  400 continue
c
      return
      end
      subroutine detnds(node1,node2,xn1,xn2,xn3,k1,k2,k3)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      aj4=xx1(k2)-xx1(k1)
      aj5=xx2(k2)-xx2(k1)
      aj6=xx3(k2)-xx3(k1)
      aj7=xx1(k3)-xx1(k1)
      aj8=xx2(k3)-xx2(k1)
      aj9=xx3(k3)-xx3(k1)
      xn1=xn1+aj5*aj9-aj6*aj8
      xn2=xn2+aj6*aj7-aj4*aj9
      xn3=xn3+aj4*aj8-aj5*aj7
      v1=xs1-xx1(k1)
      v2=ys1-xx2(k1)
      v3=zs1-xx3(k1)
      xmg=sqrt(aj4**2+aj5**2+aj6**2)
      xm1=(v1*aj4+v2*aj5+v3*aj6)/xmg
      xmg=sqrt(aj7**2+aj8**2+aj9**2)
      xm2=(v1*aj7+v2*aj8+v3*aj9)/xmg
      node1=ix(k1)
      node2=ix(k2)
      if (xm2.gt.xm1) node2=ix(k3)
      return
      end
      subroutine bound(bcs,ax,ay,az,ibb)
c     implicit double precision (a-h,o-z)                                    dp
c
c     apply displacement b.c.'s
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      dimension bcs(4,1)
      ib=ibb
      if (nodspc.gt.0) then
      ispc=iabs(ib)/1000000
      if (ispc.ne.0) then
      call spcbd(ax,ay,az,ispc)
      if(ibb.gt.0) ib = ibb - 1000000*ispc
      if(ibb.lt.0) ib = ibb + 1000000*ispc
      if (ib.eq.0) return
      endif
      endif
      if (ib.gt.7) go to 80
      go to (10,20,30,40,50,60,70), ib
   10 ax=0.0
      return
   20 ay=0.0
      return
   30 az=0.0
      return
   40 ax=0.0
      ay=0.0
      return
   50 ay=0.0
      az=0.0
      return
   60 az=0.0
      ax=0.0
      return
   70 ax=0.0
      ay=0.0
      az=0.0
      return
   80 m=ib-7
      da=ax*bcs(1,m)+ay*bcs(2,m)+az*bcs(3,m)
      if (bcs(4,m).ne.0.0) go to 90
      ax=ax-da*bcs(1,m)
      ay=ay-da*bcs(2,m)
      az=az-da*bcs(3,m)
      return
   90 ax=da*bcs(1,m)
      ay=da*bcs(2,m)
      az=da*bcs(3,m)
      return
      end
      subroutine spcdec(itcode,icode,ispcor)
c     implicit double precision (a-h,o-z)                                    dp
c
c     decode spc cards
c
      common/bkspc/ jbase(7)
      dimension icode(6)
c
      ispcor = itcode / jbase(7)
      ileft  = itcode - ispcor * jbase(7)
c
      do 10 i = 1, 6
      icode(i) = ileft / jbase(i)
      ileft = ileft - icode(i)*jbase(i)
10    continue
      return
      end
      subroutine unpk(mx,ixc,ixp,nwd)
c     implicit double precision (a-h,o-z)                                    dp
c
c     unpk connection data
c
      dimension ixc(*),ixp(*)
c     data ishft1,imask/ 21,3777777b/                                   cray1
c
      kka=ixp(1)
c     mx    =and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(1)=and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(2)=and(kka,imask)                                             cray1
c     kka=ixp(2)                                                        cray1
c     ixc(3)=and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(4)=and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(5)=and(kka,imask)                                             cray1
c     if (nwd.eq.2) return                                              cray1
c     kka=ixp(3)                                                        cray1
c     ixc(6)=and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(7)=and(kka,imask)                                             cray1
c     kka=shiftr(kka,ishft1)                                            cray1
c     ixc(8)=and(kka,imask)                                             cray1
      mx    =ixp(1)                                                     vax75
      ixc(1)=ixp(2)                                                     vax75
      ixc(2)=ixp(3)                                                     vax75
      ixc(3)=ixp(4)                                                     vax75
      ixc(4)=ixp(5)                                                     vax75
      if(nwd.eq.2) return                                               vax75
      ixc(5)=ixp(6)                                                     vax75
      ixc(6)=ixp(7)                                                     vax75
      ixc(7)=ixp(8)                                                     vax75
      ixc(8)=ixp(9)                                                     vax75
      return
c
      end
      subroutine memory(a,n)                                            wkstn
c     implicit double precision (a-h,o-z)                               wkstndp
c
c     this routine will check dimension of a array
c
      dimension a(*)                                                    wkstn
      common/zrray/maxa                                                 wkstn
c
      data length/1/                                                    wkstn
c
      length=length+n                                                   wkstn
      if (length.ge.maxa) then                                          wkstn
      nshort=length-maxa                                                wkstn
      write( *,20)nshort                                                wkstn
   20 format(//' --- error --- common block is ',i5,' words short')     wkstn
      call adios(2)                                                     wkstn
      endif                                                             wkstn
      if (n.gt.0) then                                                  wkstn
      do 10 i=1,n                                                       wkstn
   10 a(i)=0.0                                                          wkstn
      endif                                                             wkstn
      return                                                            wkstn
      end                                                               wkstn
c     subroutine memory(a,n)                                            vms
c     implicit double precision (a-h,o-z)                               vms  dp
c
c     this routine will check dimension of a array
c
c     dimension a(1)                                                    vms
c     common/zrray/maxa                                                 vms
c
c     data length/1/                                                    vms
c
c     length=length+n                                                   vms
c     if (length.ge.maxa) then                                          vms
c     nshort=length-maxa                                                vms
c     write( *,20)nshort                                                vms
c  20 format(//' --- error --- common block is ',i5,' words short')     vms
c     call adios(2)                                                     vms
c     endif                                                             vms
c     if (n.gt.0) then                                                  vms
c     do 10 i=1,n                                                       vms
c  10 a(i)=0.0                                                          vms
c     endif                                                             vms
c     return                                                            vms
c     end                                                               vms
      subroutine link (namef)                                           vax75
c     implicit double precision (a-h,o-z)                               vax75dp
c****************************************************************
c
c     reads and cracks execute line
c
c****************************************************************
      character*1 name(8),letter(80)                                    vax75
      character*8 namef                                                 vax75
      dimension namef(11)                                               vax75
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vax75
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vax75
     &       iob14(1080),iob15(1080),iob16(1080)                        vax75
      character*80 lnkarg                                               unix
      common/args/lnkarg,numargs                                        unix
c
c     assign output unit 59 to terminal
c
      if (numargs.eq.0) then                                            unix
      write( *,110)                                                     vax75
      write( *,120)                                                     vax75
      read ( *,100) letter                                              vax75
c
      do 10 i=1,80                                                      vax75
   10 call lwrcas(letter(i),letter(i))                                  vax75
      else                                                              unix
      do 12 i=1,80                                                      unix
      call lwrcas(letter(i),lnkarg(i:i))                                unix
   12 continue                                                          unix
      endif                                                             unix
      i=0                                                               vax75
   20 i=i+1                                                             vax75
      if (i.gt.80) go to 90                                             vax75
      if (letter(i).ne.'=' ) go to 20                                   vax75
      j=i                                                               vax75
   30 j=j-1                                                             vax75
      if (letter(j).eq.' ' ) go to 30                                   vax75
      l=i                                                               vax75
   40 l=l+1                                                             vax75
      if (letter(l).eq.' ' ) go to 40                                   vax75
      do 50 ilet=1,8                                                    vax75
      name(ilet)=' '                                                    vax75
   50 continue                                                          vax75
      name(1)= letter(l)                                                vax75
      m=l                                                               vax75
   60 m=m+1                                                             vax75
      if (letter(m).eq.' ' ) go to 70                                   vax75
      if (letter(m).eq.',' ) go to 70                                   vax75
      if (m-l+1.gt.8) go to 70                                          vax75
      name(m-l+1)= letter(m)                                            vax75
      go to 60                                                          vax75
   70 n=0                                                               vax75
      if (letter(j).eq.'i' ) n=1                                        vax75
      if (letter(j).eq.'d' ) n=2                                        vax75
      if (letter(j).eq.'r' ) n=3                                        vax75
      if (letter(j).eq.'f' ) n=4                                        vax75
      if (letter(j).eq.'g' ) n=5                                        vax75
      if (letter(j).eq.'o' ) n=6                                        vax75
      if (letter(j).eq.'m' ) n=7                                        vax75
      if (letter(j).eq.'s' ) n=9                                        vax75
      if (letter(j).eq.'t' ) n=11                                       vax75
      if (letter(j).eq.'z' ) n=15                                       vax75
      if (letter(j).eq.'v' ) n=16                                       vax75
      if (letter(j).eq.'l' ) n=17                                       vax75
      if(n.eq.0) go to 80                                               vax75
      namef(n)=name(1)//name(2)//name(3)//name(4)//name(5)//name(6)     vax75
     1//name(7)//name(8)                                                vax75
   80 i=m                                                               vax75
      go to 20                                                          vax75
c
c     assign input files
c
   90 if(namef(1).ne.'xyz123')                                          vax75
     1open(unit=1,file=namef(1),status='old',                           vax75
     2               form='formatted')                                  vax75
c
c     assign output files
c
      ifsiz=2621440                                                     unics
c     ifsiz=262144                                                      wkstn
c     ifsiz=262144                                                      vms
      call rwabsf(iob2, namef(2), 2, 512,  ifsiz   )                    vax75
      if (namef(3).ne.'rstxyz')                                         vax75
     1call rwabsf(iob3, namef(3), 3, 512,  ifsiz   )                    vax75
      call rwabsf(iob4, namef(4), 4, 512,  ifsiz   )                    vax75
      call rwabsf(iob5, namef(5),15, 512,  ifsiz   )                    vax75
      call rwabsf(iob6, namef(13),6, 512,  ifsiz   )                    vax75
      call rwabsf(iob7, namef(7), 7, 512,  ifsiz   )                    vax75
      call rwabsf(iob8, namef(8), 8, 512,  ifsiz   )                    vax75
      call rwabsf(iob9, namef(9), 9, 512,  ifsiz   )                    vax75
      call rwabsf(iob14,namef(14),14,512,  ifsiz   )                    vax75
      call rwabsf(iob15,namef(15),17,512,  ifsiz   )                    vax75
      call rwabsf(iob16,namef(16),16,512,  ifsiz   )                    vax75
      if (namef(11).ne.'thrmfl')                                        vax75
     1call rwabsf(iob11, namef(11), 11, 512,  ifsiz   )                 vax75
c
c     open(unit=13,file=namef(6),status='new',                          vms
c    1                form='formatted')                                 vms
      open(unit=13,file=namef(6),status='unknown',                      unix
     1                form='formatted')                                 unix
c     open(unit=12,file='forces',status='new',                          vms
c    1               form='formatted')                                  vms
      open(unit=12,file='forces',status='unknown',                      unix
     1               form='formatted')                                  unix
c
      return                                                            vax75
c
  100 format (80a1)                                                     vax75
  110 format(//                                                         vax75
     1'  please define input file names or change defaults :')          vax75
  120 format(1x,'>',$)                                                  vax75
      end                                                               vax75
      subroutine rdarg                                                  unix
c     implicit double precision (a-h,o-z)                               unix dp
c
c.... subroutine to parse input command line for unix
c
      character*80 lnkarg,arg                                           unix
      dimension arg(14)                                                 unix
      common/args/lnkarg,numargs                                        unix
      logical comflg                                                    unix
      data comflg /.false./                                             unix
      do 10 i=1,80                                                      unix
      lnkarg(i:i)=' '                                                   unix
   10 continue                                                          unix
      numargs=iargc()                                                   unix
      if (numargs.eq.0) return                                          unix
      if (numargs.gt.8) then                                            unix
      write( *,1001)                                                    unix
      stop 'rdarg'                                                      unix
      endif                                                             unix
      do 20 i=1,numargs                                                 unix
      call getarg(i,arg(i))                                             unix
   20 continue                                                          unix
      if (index(arg(1),',') .gt. 0) comflg=.true.                       unix
      if (comflg) then                                                  unix
      do 21 i=1,80                                                      unix
      if (arg(1)(i:i).eq.',') then                                      unix
      lnkarg(i:i)=' '                                                   unix
      else                                                              unix
      lnkarg(i:i)=arg(1)(i:i)                                           unix
      endif                                                             unix
   21 continue                                                          unix
      return                                                            unix
      endif                                                             unix
      do 40 i=1,numargs                                                 unix
      ic=11*(i-1)+1                                                     unix
      il=ic+10                                                          unix
      lnkarg(ic:il)=arg(i)                                              unix
   40 continue                                                          unix
      return                                                            unix
 1001 format(//5x,'Too many arguments in rdarg, max is 8')              unix
      end                                                               unix
      function fdot(a,b,n)                                              vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      dimension a(1),b(1)                                               vaxcs
      fdot=0.0                                                          vaxcs
      do 10 i=1,n                                                       vaxcs
   10 fdot=fdot+a(i)*b(i)                                               vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine empty(iun)                                             vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine getfn(iun,name)                                        vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      character*8 name,namef                                            vaxcs
      common/zname/namef(14)                                            vaxcs
      name=namef(iun)                                                   vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine getnam(iun,name)                                       vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      character*8 name,namef                                            vaxcs
      common/zname/namef(14)                                            vaxcs
      name=namef(iun)                                                   vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine blkcpy( src, sink, nword)                              vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      dimension src(*),sink(*)                                          vaxcs
      do 10 i=1,nword                                                   vaxcs
   10 sink(i)=src(i)                                                    vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine blkcpi( isrc, isink, nword)                            vaxcs
      dimension isrc(*),isink(*)                                        vaxcs
      do 10 i=1,nword                                                   vaxcs
   10 isink(i)=isrc(i)                                                  vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
      subroutine exita( irs )                                           vaxcs
c     implicit double precision (a-h,o-z)                               vaxcsdp
      stop                                                              vaxcs
      end                                                               vaxcs
      subroutine chef                                                   vaxcs
      return                                                            vaxcs
      end                                                               vaxcs
c     function timuse(ait)                                              vax75
c     implicit double precision (a-h,o-z)                               vax75dp
c     implicit integer*4(i-n)                                           vax75
c     real etime,rait,elapt                                             vax75
c     dimension ait(*),rait(2)                                          vax75
c     data len/0/                                                       vax75
c     if ( len .eq. 0 ) call lib$init_timer                             vms
c     len=1                                                             vax75
c     call lib$stat_timer(2,io1)                                        vms
c     call lib$stat_timer(3,io2)                                        vms
c     call lib$stat_timer(4,io3)                                        vms
c     timuse=0.                                                         vax75
c     rait(1)=ait(1)                                                    wkstn
c     rait(2)=ait(2)                                                    wkstn
c     elapt=etime(rait)                                                 wkstn
c     elapt=second()                                                    unics
c     ait(1)=elapt                                                      unics
c     ait(2)=0.                                                         unics
c     cpu=io1                                                           vms
c     oin=io2+io3                                                       vms
c     ait(1)=cpu/100.0                                                  vms
c     ait(2)=oin/1000.                                                  vms
c     ait(1)=rait(1)                                                    wkstn
c     ait(2)=rait(2)                                                    wkstn
c     return                                                            vax75
c     end                                                               vax75
      subroutine wrabsg(iunit,dpdata,len,iadd,iflag)
c
c.... routine to write single-precision binary files from double precision
c     real data (iflag=1), integer data (iflag=0) or
c     single precision real data (iflag=0)
c
      parameter(ibufsz=512)
c     implicit double precision (a-h,o-z)                                    dp
      real spdata
      dimension dpdata(*)
      dimension spdata(ibufsz)
      common/double/iprec,ncpw,unit
c
      if ((iflag.eq.0).or.(iprec.eq.1)) then
c
c.... single precision data so just pass through
c
      call wrabsf(iunit,dpdata,len,iadd)
      else
c
c.... convert double precision data to single precision
c
      iaddt=iadd
      nblk=len/ibufsz
      irem=len-ibufsz*nblk
      if (nblk.gt.0) then
      do 20 iblk=1,nblk
      do 10 i=1,ibufsz
      ipt=(iblk-1)*ibufsz + i
      spdata(i)=real(dpdata(ipt))
   10 continue
      call wrabsf(iunit,spdata,ibufsz,iaddt)
      iaddt=iaddt+ibufsz
   20 continue
      endif
c
      if (irem .gt. 0) then
      do 30 i=1,irem
      ipt=nblk*ibufsz + i
      spdata(i)=real(dpdata(ipt))
   30 continue
      call wrabsf(iunit,spdata,irem,iaddt)
      endif
      endif
c
      return
      end
      subroutine rdabsg(iunit,dpdata,len,iadd,iflag)
c
c.... routine to read single-precision binary files and return
c     double precision real data (iflag=1), integer data (iflag=0) or
c     single precision real data (iflag=0)
c
      parameter(ibufsz=512)
c     implicit double precision (a-h,o-z)                                    dp
      real spdata
      dimension dpdata(*)
      dimension spdata(ibufsz)
      common/double/iprec,ncpw,unit
c
      if ((iflag.eq.0).or.(iprec.eq.1)) then
c
c.... single precision data requested so just pass through
c
      call rdabsf(iunit,dpdata,len,iadd,ierr)
      else
c
c.... convert single precision data to double precision
c
      iaddt=iadd
      nblk=len/ibufsz
      irem=len-ibufsz*nblk
      if (nblk.gt.0) then
      do 20 iblk=1,nblk
      call rdabsf(iunit,spdata,ibufsz,iaddt,ierr)
      do 10 i=1,ibufsz
      ipt=(iblk-1)*ibufsz + i
      dpdata(ipt)=dble(spdata(i))
   10 continue
      iaddt=iaddt+ibufsz
   20 continue
      endif
c
      if (irem .gt. 0) then
      call rdabsf(iunit,spdata,irem,iaddt,ierr)
      do 30 i=1,irem
      ipt=nblk*ibufsz + i
      dpdata(ipt)=dble(spdata(i))
   30 continue
      endif
      endif
c
      return
      end
      subroutine rwabsf (fit,lfn,us,bfs,fcs)                            vax75
c
c     ansi fortran equivalents to llnl familied file random i/o routines
c
c     steven j. sackett
c
c     lawrence livermore laboratory
c     livermore, california 94550
c
c     january 3, 1984
c
c-----------------------------------------------------------------------
c     this package consists of seven routines for handling word
c     addressable random i/o on familied files:
c
c               asgrfm  (not called directly)
c               nrfnam  (not called directly)
c               rdabsf  (fit,w,nw,da)
c               rdiska  (not called directly)
c                wdiska
c               riosta  (fit)
c               rwabsf  (fit,lfn,us,bfs,fcs)
c               wrabsf  (fit,w,nw,da)
c
c
c     if the disk address given in the argument list to rdabsf/wrabsf
c     is greater than or equal to the family (file) size, it is biased
c     to access the correct family member. here the family size is
c     defined to be the size at which family members are created.
c
c     the root name for a family, which is the name of the first family
c     member, is taken to be the name associated with the given unit
c     specifier (logical unit). names for succeeding family members are
c     generated by appending a two digit integer to the root name (or
c     to its first six characters). the disk address bias for a member,
c     which is also the first word address for the member, is equal to
c     this integer times the family (file) size. assuming, for example,
c     the root name 'diska' and a family size of 1000000b words, a
c     family with five members would appear as follows:
c             --------------------------------------
c             member     name     first word address
c             --------------------------------------
c               1        diska         0
c               2        diska01       1000000b
c               3        diska02       2000000b
c               4        diska03       3000000b
c               5        diska04       4000000b
c             --------------------------------------
c     note that with this naming scheme errors can occur if an eight
c     character root name of the form axxxxxnn is used, where 'a'
c     denotes an alphabetic character, 'x' denotes any character, and
c     'n' denotes a numeric character.
c
c     if a family member exists, it is opened and used. if it does
c     not exist, a new file is created at the family (file) size.
c     note that this will result in an error exit if a write is
c     attempted on an existing file that is read-only.
c
c-----------------------------------------------------------------------
c
c.... open/close a file for word addressable random i/o
c
c     calling sequence: call rwabsf(fit,lfn,us,bfs,fcs)
c
c     input arguments
c            fit      array to use for the file information table
c            lfn      the file name for an open call; the file
c                     disposition status ('keep' or 'delete') for
c                     a close call
c            us       the file unit specifier (logical unit no.)
c                     for an open call; zero (0) or omitted for a
c                     close call
c            bfs      the buffer size in words for an open call;
c                     zero (0) or omitted for a close call
c            fcs      file creation size (family size) for an open
c                     call; zero (0) or omitted for a close call
c
c     fit must be dimensioned as an array of at least bfs+7 words in
c     the user's program and must not be changed while the file is open
c     bfs must be a multiple of 512 (1000b). fcs must be a multiple of
c     bfs.
c
      implicit integer (a-z)                                            vax75
      dimension fit(8)                                                  vax75
      character lfn*8                                                   vax75
c
      common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     vax75
c            mxfrf     dimension of the familied random file name
c                      table (currently set to 16) - this is the
c                      maximum number of familied random files
c                      allowed to be open at the same time
c            ifrf      index in familied random file name table for
c                      the file accessed last
c
      character*8 frfn,frn,kfn                                          vax75
      common/frfcm2/frfn(2,16),frn,kfn                                  vax75
c            frfn      familied random file name table
c
      logical fxist                                                     vax75
c
      data mxfrf/16/                                                    vax75
      data frfn/32*'        '/                                          vax75
c
c     fit(1) = us
c     fit(2) = index in random file root name table for this file
c     fit(3) = bfs
c     fit(4) = fcs
c     fit(5) = disk address of first word in the buffer
c     fit(6) = number of words of data currently in the buffer
c     fit(7) = disk address of last word in file + 1
c
c     parameter giving number of record units per integer word
c     for most systems a single character is used as a record unit
      ncpw=4                                                            vax75
      ncpw=8                                                            unics
c
      if ((lfn.eq.'keep').or.(lfn.eq.'delete')) go to 50                vax75
c.... initialize fit and put name in familied random file name table
      do 10 n=1,mxfrf                                                   vax75
      if (frfn(1,n).ne.'        ') go to 10                             vax75
      frfn(1,n)=lfn                                                     vax75
      fit(2)=n                                                          vax75
      go to 12                                                          vax75
   10 continue                                                          vax75
      stop        ' rwabsf open error- too many random files '          vax75
   12 continue                                                          vax75
      buflen=bfs-mod(bfs,512)                                           vax75
      inquire (file=lfn,recl=rcl)                                       vax75
      rcl=rcl/ncpw                                                      vax75
      if (buflen.lt.rcl) then                                           vax75
      stop        ' rwabsf open error- buffer too small '               vax75
      endif                                                             vax75
      if (rcl.ne.0) buflen=rcl                                          vax75
      fit(1)=us                                                         vax75
      fit(3)=buflen                                                     vax75
      fit(4)=fcs-mod(fcs,buflen)                                        vax75
      fit(5)=fit(4)                                                     vax75
      fit(6)=0                                                          vax75
      fit(7)=0                                                          vax75
      return                                                            vax75
c.... flush the buffer if data is present which is not on disk
   50 continue                                                          vax75
      if (fit(3).lt.0) then                                             vax75
      fit(3)=-fit(3)                                                    vax75
      call wdiska (fit(1),fit(8),fit(3),fit(5))                         vax75
      fit(7)=max0(fit(7),fit(5)+fit(3))                                 vax75
      endif                                                             vax75
c.... close the file
      if (fit(7).eq.0) return                                           vax75
      close (fit(1),status='keep')                                      vax75
      ifrf=fit(2)                                                       vax75
      frn=frfn(1,ifrf)                                                  vax75
      frfn(1,ifrf)='        '                                           vax75
      frfn(2,ifrf)='        '                                           vax75
      if (lfn.ne.'delete') return                                       vax75
c.... destroy the family if requested
      n=0                                                               vax75
      kfn=frn                                                           vax75
   60 continue                                                          vax75
      inquire (file=kfn,exist=fxist)                                    vax75
      if (.not.fxist) return                                            vax75
      open (fit(1),file=kfn,access='direct',form='unformatted',         vax75
     1 recl=rcl,status='old')                                           vax75
      close (fit(1),status='delete')                                    vax75
      n=n+1                                                             vax75
      call nrfnam (frn,n,kfn)                                           vax75
      go to 60                                                          vax75
      end                                                               vax75
      subroutine asgrfm(da,fit)                                         vax75
c
c     assign next family member for random i/o
c
      implicit integer(a-z)                                             vax75
      dimension fit(8)                                                  vax75
c
      common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     vax75
      character*8 frfn,frn,kfn                                          vax75
      common/frfcm2/frfn(2,16),frn,kfn                                  vax75
c
      logical fxist                                                     vax75
      character nfn*8,msg*49                                            vax75
c
      data msg/' read requested from nonexistent family member - '/     vax75
c     parameter giving number of record units per integer word
c     for most systems a single character is used as a record unit
      ncpw=4                                                            vax75
      ncpw=8                                                            unics
c
c.... compute family member index & bias disk address for correct access
      i=da/fcsize                                                       vax75
      da=da-i*fcsize                                                    vax75
c.... get the name of the requested family member
      call nrfnam(frn,i,nfn)                                            vax75
c.... return if current family member is the desired one
      if(kfn.eq.nfn) return                                             vax75
c.... flush the buffer if data is present which is not on disk
      if (fit(3).lt.0) then                                             vax75
      fit(3)=-fit(3)                                                    vax75
      call wdiska (fit(1),fit(8),buflen,disklc )                        vax75
      fit(7)=max0(fit(7),disklc +buflen)                                vax75
      endif                                                             vax75
c.... determine if requested family member exists
      if (kop.ne.0) go to 20                                            vax75
      inquire (file=nfn,exist=fxist)                                    vax75
      if (fxist) go to 20                                               vax75
      ier=-ier                                                          vax75
      if (ier.lt.0) return                                              vax75
      stop        ' read attempted from nonexistent file '              vax75
c.... close the current family member
   20 continue                                                          vax75
      if (kfn.ne.'        ') then                                       vax75
      close (fit(1),status='keep')                                      vax75
      endif                                                             vax75
c.... open/create the requested family member
      lrecl=ncpw*buflen                                                 unix
c     lrecl=buflen                                                      vms
      inquire(file=nfn,exist=fxist)                                     vax75
      open (fit(1),file=nfn,access='direct',form='unformatted'          vax75
     1,recl=lrecl,status='unknown')                                     vax75
      kfn=nfn                                                           vax75
      frfn(2,ifrf)=kfn                                                  vax75
      fit(5)=fit(4)                                                     vax75
      disklc =fit(5)                                                    vax75
      fit(6)=0                                                          vax75
      curlen=fit(6)                                                     vax75
      fit(7)=0                                                          vax75
      if (fxist) fit(7)=fit(4)                                          vax75
      return                                                            vax75
      end                                                               vax75
      subroutine nrfnam(frn,i,nfn)                                      vax75
c
c     form the file name for member i+1 of random family kfn
c
c     input arguments
c           frn       family root name (name of the first family member)
c            i        family member index for member i+1
c
c     output arguments
c           nfn       file name for member i+1
c
      implicit integer (a-z)                                            vax75
      character*8 frn,nfn                                               vax75
c
      character msg*43,ni*1                                             vax75
      dimension ni(10)                                                  vax75
      data msg/' family member index exceeds 99 for file - '/           vax75
      data ni/'0','1','2','3','4','5','6','7','8','9'/                  vax75
c
      if(i.ne.0) go to 11                                               vax75
      nfn=frn                                                           vax75
      return                                                            vax75
   11 if(i.lt.100) go to 21                                             vax75
      stop        ' family member index exceeds 99 '                    vax75
   21 continue                                                          vax75
      do 30 k=1,6                                                       vax75
      if( frn(k:k).eq.' ') go to 40                                     vax75
   30 continue                                                          vax75
   40 k=k-1                                                             vax75
      j=i/10                                                            vax75
      nfn=frn(1:k)//ni(j+1)//ni(i-10*j+1)                               vax75
      return                                                            vax75
      end                                                               vax75
      subroutine famnum(namef,ifam,rname)
c
c.... get family member number given the full name
c     return ifam=0 and rname=namef if not a familied file
c
      character*8 namef,rname
      rname='       '
      do 10 nchar=8,1,-1
      if( namef(nchar:nchar) .ne. ' ') then
      is=ichar(namef(nchar:nchar))
      if((is.ge.65.and.is.le.90).or.(is.ge.97.and.is.le.122)) then
c
c....  not a familied file since last letter is a character
c
      rname=namef
      ifam=0
      return
      else
      read(namef(nchar-1:nchar),100) ifam
      rname=namef(1:nchar-2)
      return
      endif
      endif
   10 continue
      write(*,200)
      write(13,200)
      call adios(2)
      return
  100 format(i2)
  200 format(///5x,'error in famnum - no number found')
      end
      subroutine rdabsf (fit,w,nw,da,ir)                                vax75
c
c.... entry for random read
c
c     calling sequence: call rdabsf(fit,w,nw,da)
c
c     input arguments
c            fit      the file information table
c            nw       number of words to read from disk
c            da       zero base disk address
c            ir       open error return flag
c                         = 0    abort if an open error occurs
c                         = 1    return if an open error occurs
c
c     output arguments
c            w        data read from disk
c            ir       open error return flag
c                         = 0    no open errors occured
c                         =-1    read attempted from nonexistent
c                                family member
c
      implicit integer(a-z)                                             vax75
      dimension fit(8),w(nw)                                            vax75
c
      common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     vax75
      character*8 frfn,frn,kfn                                          vax75
      common/frfcm2/frfn(2,16),frn,kfn                                  vax75
c
c.... get family size, family root name, and name of open family member
      ifrf=fit(2)                                                       vax75
      fcsize=fit(4)                                                     vax75
      frn=frfn(1,ifrf)                                                  vax75
      kfn=frfn(2,ifrf)                                                  vax75
c.... get buffer pointers
      buflen=iabs(fit(3))                                               vax75
      disklc =fit(5)                                                    vax75
      curlen=fit(6)                                                     vax75
      kop=0                                                             vax75
      ier=iabs(ir)                                                      vax75
      ir=0                                                              vax75
      l=nw                                                              vax75
      m=0                                                               vax75
c.... set up access to correct family member
   30 kd=da+m                                                           vax75
      call asgrfm (kd,fit)                                              vax75
      if (ier.lt.0) then                                                vax75
      ir=ier                                                            vax75
      return                                                            vax75
      endif                                                             vax75
      ll=min0(l,fit(4)-kd)                                              vax75
c.... see if requested data is in the buffer
   40 i=kd-disklc                                                       vax75
      if (i.lt.0) go to 50                                              vax75
      bloc=i                                                            vax75
      blen=min0(ll,curlen-i)                                            vax75
      i=0                                                               vax75
      go to 60                                                          vax75
   50 bloc=0                                                            vax75
      blen=ll+i                                                         vax75
      if (blen.gt.curlen) go to 80                                      vax75
c.... branch if none of the data is in the buffer
   60 if (blen.le.0) go to 80                                           vax75
      ll=blen                                                           vax75
      do 70 k=1,ll                                                      vax75
      w(k+m-i)=fit(k+bloc+7)                                            vax75
   70 continue                                                          vax75
      if (i.lt.0) m=m-ll                                                vax75
      l=l-ll                                                            vax75
      m=m+ll                                                            vax75
c.... loop if all requested data has not been transferred
      if (l.ne.0) go to 30                                              vax75
      return                                                            vax75
c.... flush the buffer if data is present which is not on disk
   80 if (fit(3).lt.0) then                                             vax75
      fit(3)=-fit(3)                                                    vax75
      call wdiska (fit(1),fit(8),buflen,disklc )                        vax75
      fit(7)=max0(fit(7),disklc +buflen)                                vax75
      endif                                                             vax75
c.... for blocks larger than the buffer, read the data directly
      if (ll.lt.buflen) go to 110                                       vax75
      if (mod(kd,buflen).ne.0) go to 110                                vax75
      nr=ll/buflen                                                      vax75
      do 100 n=1,nr                                                     vax75
      call rdiska (fit(1),w(m+1),buflen,kd)                             vax75
      m=m+buflen                                                        vax75
      kd=kd+buflen                                                      vax75
  100 continue                                                          vax75
      l=l-nr*buflen                                                     vax75
      if (l.eq.0) return                                                vax75
      ll=ll-nr*buflen                                                   vax75
      if (ll.eq.0) go to 30                                             vax75
c.... fill the buffer
  110 continue                                                          vax75
      disklc =kd-mod(kd,buflen)                                         vax75
      fit(5)=disklc                                                     vax75
      call rdiska (fit(1),fit(8),buflen,disklc )                        vax75
      curlen=buflen                                                     vax75
      fit(6)=curlen                                                     vax75
      go to 40                                                          vax75
      end                                                               vax75
      subroutine rdiska (lus,w,nw,da)                                   vax75
c
c     interface to direct access i/o for rwabsf random i/o routines
c
c.... entry to transfer one record from disk to a buffer
c
c     input arguments
c            lus      the file unit specifier (logical unit no.)
c            nw       number of words to read from disk
c                     (must be a multiple of 512)
c            da       zero base disk word.address
c                     (must be on a sector boundary)
c
c     output arguments
c            w        data read from disk
c
      implicit integer(a-z)                                             vax75
      dimension w(nw)                                                   vax75
c
      common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     vax75
c
      lda=da/buflen+1                                                   vax75
      read (lus,rec=lda,iostat=ios) w                                   vax75
      return                                                            vax75
c
c.... entry to transfer one record from a buffer to disk
c
c     input arguments
c            lus      the file unit specifier (logical unit no.)
c            w        data to be written to disk
c            nw       number of words to write to disk
c                     (must be a multiple of 512)
c            da       zero base disk word.address
c                     (must be on a sector boundary)
c
      entry wdiska(lus,w,nw,da)                                         vax75
      lda=da/buflen+1                                                   vax75
      write (lus,rec=lda) w                                             vax75
      return                                                            vax75
c
      end                                                               vax75
      subroutine riosta  (fit)                                          vax75
c
c.... entry to check if i/o is complete without errors (dummy)
c
c     calling sequence: call riosta (us)
c
c     input arguments
c            fit      the file information table
c
      implicit integer(a-z)                                             vax75
      dimension fit(8)                                                  vax75
c
      character kfn*8,nerr*4,msgi*10,msgj*27                            vax75
      data msgi/' i/o error'/, msgj/' on familied random file - '/      vax75
c
c     inquire (unit=fit(1),name=kfn,nextrec=numrec)
c     if (ios.eq.0) return
c     write (nerr,'(i4)')ios
c     stop        ' i/o error on familied random file '
      return                                                            vax75
      end                                                               vax75
      subroutine wrabsf (fit,w,nw,da)                                   vax75
c
c.... entry for random write
c
c     calling sequence: call wrabsf (fit,w,nw,da)
c
c     input arguments
c            fit      the file information table
c            w        data to be written to disk
c            nw       number of words to write to disk
c            da       zero base disk address
c
      implicit integer (a-z)                                            vax75
      dimension fit(8),w(nw)                                            vax75
c
      common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     vax75
      character*8 frfn,frn,kfn                                          vax75
      common/frfcm2/frfn(2,16),frn,kfn                                  vax75
c
c.... get family size, family root name, and name of open family member
      ifrf=fit(2)                                                       vax75
      fcsize=fit(4)                                                     vax75
      frn=frfn(1,ifrf)                                                  vax75
      kfn=frfn(2,ifrf)                                                  vax75
c.... get buffer pointers
      buflen=iabs(fit(3))                                               vax75
      disklc =fit(5)                                                    vax75
      curlen=fit(6)                                                     vax75
      kop=1                                                             vax75
      l=nw                                                              vax75
      m=0                                                               vax75
c.... set up access to correct family member
   30 kd=da+m                                                           vax75
      call asgrfm (kd,fit)                                              vax75
      ll=min0(l,fit(4)-kd)                                              vax75
c.... move data into buffer
   40 i=kd-disklc                                                       vax75
      if (i.lt.0) go to 50                                              vax75
      bloc=i                                                            vax75
      blen=min0(ll,buflen-i)                                            vax75
      i=0                                                               vax75
      go to 60                                                          vax75
   50 bloc=0                                                            vax75
      blen=ll+i                                                         vax75
      if (blen.gt.buflen) go to 80                                      vax75
c.... branch if no overlap
   60 if (blen.le.0) go to 80                                           vax75
      ll=blen                                                           vax75
      do 70 k=1,ll                                                      vax75
      fit(k+bloc+7)=w(k+m-i)                                            vax75
   70 continue                                                          vax75
      fit(3)=-buflen                                                    vax75
      if (i.lt.0) m=m-ll                                                vax75
      l=l-ll                                                            vax75
      m=m+ll                                                            vax75
c.... loop if all requested data has not been transferred
      if (l.ne.0) go to 30                                              vax75
      return                                                            vax75
c.... flush the buffer if data is present which is not on disk
   80 if (fit(3).lt.0) then                                             vax75
      fit(3)=-fit(3)                                                    vax75
      call wdiska (fit(1),fit(8),buflen,disklc )                        vax75
      fit(7)=max0(fit(7),disklc +buflen)                                vax75
      endif                                                             vax75
c.... for blocks larger than the buffer, write the data directly
   90 if (ll.lt.buflen) go to 110                                       vax75
      if (mod(kd,buflen).ne.0) go to 110                                vax75
      nr=ll/buflen                                                      vax75
      do 100 n=1,nr                                                     vax75
      call wdiska (fit(1),w(m+1),buflen,kd)                             vax75
      m=m+buflen                                                        vax75
      kd=kd+buflen                                                      vax75
  100 continue                                                          vax75
      fit(7)=max0(fit(7),kd)                                            vax75
      l=l-nr*buflen                                                     vax75
      if (l.eq.0) return                                                vax75
      ll=ll-nr*buflen                                                   vax75
      if (ll.eq.0) go to 30                                             vax75
c.... initialize the buffer if required
  110 continue                                                          vax75
      disklc =kd-mod(kd,buflen)                                         vax75
      fit(5)=disklc                                                     vax75
      curlen=min0(idim(fit(7),disklc ),buflen)                          vax75
      if (curlen.lt.1) go to 120                                        vax75
      if ((kd.ne.disklc ).or.(ll.lt.curlen)) then                       vax75
      call rdiska (fit(1),fit(8),curlen,disklc )                        vax75
      endif                                                             vax75
  120 curlen=buflen                                                     vax75
      fit(6)=curlen                                                     vax75
      go to 40                                                          vax75
c
      end                                                               vax75
      subroutine famsiz(n,m)                                            vax75
c     implicit double precision (a-h,o-z)                               vax75dp
      return                                                            vax75
      end                                                               vax75
      subroutine header
c     implicit double precision (a-h,o-z)                                    dp
c
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      character*8 vs                                                    vax75
      character*8 date                                                  vax75
      common/vsnum/vs,date
c
      write(13,20) head,vs,date
      return
c
   20 format('1',12a6,/24x,' dyna3d (version ',a8,') compiled ',a8)
      end
      subroutine azero(a,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension a(*)
      do 10 i=1,n
   10 a(i)=0.
      return
      end
      subroutine iazero(ia,n)
      dimension ia(*)
      do 10 i=1,n
   10 ia(i)=0
      return
      end
c     subroutine link (namef)                                           cos
c     implicit double precision (a-h,o-z)                               cos  dp
c****************************************************************
c
c     reads and cracks execute line
c
c****************************************************************
c     character*1 name(8),letter(80)                                    cos
c     character*8 namef                                                 cos
c     dimension namef(14)                                               cos
c     character*10 sopt                                                 cos
c     character*50 mhsopt1                                              cos
c     common/mhs1/ sopt,mhsopt1                                         cos
c     common /mhs2/ inewlt                                              cos
c     common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   cos
c    &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   cos
c    &       iob14(1080),iob15(1080),iob16(1080)                        cos
c
c     assign output unit 59 to terminal
c
c     open(unit=59,file='messag',status='new')                          cos
c
c     write( *,110)                                                     cos
c     write( *,120)                                                     cos
c
c     open(unit=88,file='names',status='unknown')                       cos
c     read (88,100) letter                                              cos
c     read (88,101) sopt,mhsopt1                                        cos
c
c     i=0                                                               cos
c  20 i=i+1                                                             cos
c     if (i.gt.80) go to 90                                             cos
c     if (letter(i).ne.'=' ) go to 20                                   cos
c     j=i                                                               cos
c  30 j=j-1                                                             cos
c     if (letter(j).eq.' ' ) go to 30                                   cos
c     l=i                                                               cos
c  40 l=l+1                                                             cos
c     if (letter(l).eq.' ' ) go to 40                                   cos
c     do 50 ilet=1,8                                                    cos
c     name(ilet)=' '                                                    cos
c  50 continue                                                          cos
c     name(1)= letter(l)                                                cos
c     m=l                                                               cos
c  60 m=m+1                                                             cos
c     if (letter(m).eq.' ' ) go to 70                                   cos
c     if (letter(m).eq.',' ) go to 70                                   cos
c     if (m-l+1.gt.8) go to 70                                          cos
c     name(m-l+1)= letter(m)                                            cos
c     go to 60                                                          cos
c  70 n=0                                                               cos
c     if (letter(j).eq.'i' ) n=1                                        cos
c     if (letter(j).eq.'d' ) n=2                                        cos
c     if (letter(j).eq.'r' ) n=3                                        cos
c     if (letter(j).eq.'f' ) n=4                                        cos
c     if (letter(j).eq.'g' ) n=5                                        cos
c     if (letter(j).eq.'o' ) n=6                                        cos
c     if (letter(j).eq.'s' ) n=9                                        cos
c     if (letter(j).eq.'t' ) n=11                                       cos
c     if (letter(j).eq.'a' ) n=14                                       cos
c     if (letter(j).eq.'z' ) n=15                                       cos
c     if (letter(j).eq.'v' ) n=16                                       cos
c     if(n.eq.0) go to 80                                               cos
c     namef(n)=name(1)//name(2)//name(3)//name(4)//name(5)//name(6)     cos
c    1//name(7)//name(8)                                                cos
c  80 i=m                                                               cos
c     go to 20                                                          cos
c  90 continue                                                          cos
c
c     assign input files
c
c     if(namef(1).ne.'xyz123')                                          cos
c    1open(unit=1,file=namef(1),status='old',                           cos
c    2 access='sequential' )                                            cos
c
c     assign output files
c
c     call rwabsf(iob2, namef(2), 2, 512,7000000b)                      cos
c     if (namef(3).ne.'rstxyz')                                         cos
c    1call rwabsf(iob3, namef(3), 3, 512,7000000b)                      cos
c     call rwabsf(iob4, namef(4), 4, 512,7000000b)                      cos
c     call rwabsf(iob5, namef(5), 5, 512,7000000b)                      cos
c     call rwabsf(iob6, namef(13),5, 512,7000000b)                      cos
c     call rwabsf(iob8, namef(8), 8, 512,7000000b)                      cos
c     call rwabsf(iob9, namef(9), 9, 512,7000000b)                      cos
c     call rwabsf(iob14,namef(14),14,512,7000000b)                      cos
c     call rwabsf(iob15,namef(15),15,512,7000000b)                      cos
c     call rwabsf(iob16,namef(16),16,512,7000000b)                      cos
c     if (namef(11).ne.'thrmfl')                                        cos
c    1call rwabsf(iob11, namef(11), 11, 512,7000000b)                   cos
c
c     open(unit=13,file=namef(6),status='new',                          cos
c    1 access='sequential')                                             cos
c     open(unit=12,file='forces',status='new',                          cos
c    1 access='sequential')                                             cos
c
c     return                                                            cos
c 100 format (80a1)                                                     cos
c 101 format(a10,5x,a50)                                                cos
c 110 format(//                                                         cos
c    1'  please define input file names or change defaults :')          cos
c 120 format(1x,'>',$)                                                  cos
c     end                                                               cos
c     subroutine rwabsf (fit,lfn,us,bfs,fcs)                            cos
c
c     ansi fortran equivalents to llnl familied file random i/o routines
c
c     steven j. sackett
c
c     lawrence livermore laboratory
c     livermore, california 94550
c
c     january 3, 1984
c
c-----------------------------------------------------------------------
c     this package consists of seven routines for handling word
c     addressable random i/o on familied files:
c
c               asgrfm  (not called directly)
c               nrfnam  (not called directly)
c               rdabsf  (fit,w,nw,da)
c               rdiska  (not called directly)
c                wdiska
c               riosta  (fit)
c               rwabsf  (fit,lfn,us,bfs,fcs)
c               wrabsf  (fit,w,nw,da)
c
c
c     if the disk address given in the argument list to rdabsf/wrabsf
c     is greater than or equal to the family (file) size, it is biased
c     to access the correct family member. here the family size is
c     defined to be the size at which family members are created.
c
c     the root name for a family, which is the name of the first family
c     member, is taken to be the name associated with the given unit
c     specifier (logical unit). names for succeeding family members are
c     generated by appending a two digit integer to the root name (or
c     to its first six characters). the disk address bias for a member,
c     which is also the first word address for the member, is equal to
c     this integer times the family (file) size. assuming, for example,
c     the root name 'diska' and a family size of 1000000b words, a
c     family with five members would appear as follows:
c             --------------------------------------
c             member     name     first word address
c             --------------------------------------
c               1        diska         0
c               2        diska01       1000000b
c               3        diska02       2000000b
c               4        diska03       3000000b
c               5        diska04       4000000b
c             --------------------------------------
c     note that with this naming scheme errors can occur if an eight
c     character root name of the form axxxxxnn is used, where 'a'
c     denotes an alphabetic character, 'x' denotes any character, and
c     'n' denotes a numeric character.
c
c     if a family member exists, it is opened and used. if it does
c     not exist, a new file is created at the family (file) size.
c     note that this will result in an error exit if a write is
c     attempted on an existing file that is read-only.
c
c-----------------------------------------------------------------------
c
c.... open/close a file for word addressable random i/o
c
c     calling sequence: call rwabsf(fit,lfn,us,bfs,fcs)
c
c     input arguments
c            fit      array to use for the file information table
c            lfn      the file name for an open call; the file
c                     disposition status ('keep' or 'delete') for
c                     a close call
c            us       the file unit specifier (logical unit no.)
c                     for an open call; zero (0) or omitted for a
c                     close call
c            bfs      the buffer size in words for an open call;
c                     zero (0) or omitted for a close call
c            fcs      file creation size (family size) for an open
c                     call; zero (0) or omitted for a close call
c
c     fit must be dimensioned as an array of at least bfs+7 words in
c     the user's program and must not be changed while the file is open
c     bfs must be a multiple of 512 (1000b). fcs must be a multiple of
c     bfs.
c
c     implicit integer (a-z)                                            cos
c     dimension fit(8)                                                  cos
c     character lfn*8                                                   cos
c
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier
c            mxfrf     dimension of the familied random file name
c                      table (currently set to 16) - this is the
c                      maximum number of familied random files
c                      allowed to be open at the same time
c            ifrf      index in familied random file name table for
c                      the file accessed last
c
c     character*8 frfn,frn,kfn                                          cos
c     common/frfcm2/frfn(2,16),frn,kfn                                  cos
c            frfn      familied random file name table
c
c     logical fxist                                                     cos
c
c     parameter giving number of record units per integer word
c     for most systems a single character is used as a record unit
c     parameter (ncpw=8)                                                cos
c
c     data mxfrf/16/                                                    cos
c     data frfn/32*'        '/                                          cos
c
c     fit(1) = us
c     fit(2) = index in random file root name table for this file
c     fit(3) = bfs
c     fit(4) = fcs
c     fit(5) = disk address of first word in the buffer
c     fit(6) = number of words of data currently in the buffer
c     fit(7) = disk address of last word in file + 1
c
c     if ((lfn.eq.'keep').or.(lfn.eq.'delete')) go to 50                cos
c.... initialize fit and put name in familied random file name table
c     do 10 n=1,mxfrf                                                   cos
c     if (frfn(1,n).ne.'        ') go to 10                             cos
c     frfn(1,n)=lfn                                                     cos
c     fit(2)=n                                                          cos
c     go to 12                                                          cos
c  10 continue                                                          cos
c     stop        ' rwabsf open error- too many random files '          cos
c  12 continue                                                          cos
c     buflen=bfs-mod(bfs,512)                                           cos
c     inquire (file=lfn,recl=rcl)                                       cos
c     if( rcl.lt.0 ) rcl = 0                                            cos
c     rcl=rcl/ncpw                                                      cos
c     if (buflen.lt.rcl) then                                           cos
c     stop        ' rwabsf open error- buffer too small '               cos
c     endif                                                             cos
c     if (rcl.ne.0) buflen=rcl                                          cos
c     fit(1)=us                                                         cos
c     fit(3)=buflen                                                     cos
c     fit(4)=fcs-mod(fcs,buflen)                                        cos
c     fit(5)=fit(4)                                                     cos
c     fit(6)=0                                                          cos
c     fit(7)=0                                                          cos
c     return                                                            cos
c.... flush the buffer if data is present which is not on disk
c  50 continue                                                          cos
c     if (fit(3).lt.0) then                                             cos
c     fit(3)=-fit(3)                                                    cos
c     call wdiska (fit(1),fit(8),fit(3),fit(5))                         cos
c     fit(7)=max0(fit(7),fit(5)+fit(3))                                 cos
c     endif                                                             cos
c.... close the file
c     if (fit(7).eq.0) return                                           cos
c     close (fit(1),status='keep')                                      cos
c     ifrf=fit(2)                                                       cos
c     if( lfn.ne.'delete'.and.fit(1).ne.3) call dtaman ( fit(2) )       cos
c     frn=frfn(1,ifrf)                                                  cos
c     frfn(1,ifrf)='        '                                           cos
c     frfn(2,ifrf)='        '                                           cos
c     if (lfn.ne.'delete') return                                       cos
c.... destroy the family if requested
c     n=0                                                               cos
c     kfn=frn                                                           cos
c  60 continue                                                          cos
c     inquire (file=kfn,exist=fxist)                                    cos
c     if (.not.fxist) return                                            cos
c     open (fit(1),file=kfn,access='direct',form='unformatted'          cos
c    1,status='old')                                                    cos
c     close (fit(1),status='delete')                                    cos
c     n=n+1                                                             cos
c     call nrfnam (frn,n,kfn)                                           cos
c     go to 60                                                          cos
c     end                                                               cos
c     subroutine rdabsf (fit,w,nw,da,ir)                                cos
c
c.... entry for random read
c
c     calling sequence: call rdabsf(fit,w,nw,da)
c
c     input arguments
c            fit      the file information table
c            nw       number of words to read from disk
c            da       zero base disk address
c            ir       open error return flag
c                         = 0    abort if an open error occurs
c                         = 1    return if an open error occurs
c
c     output arguments
c            w        data read from disk
c            ir       open error return flag
c                         = 0    no open errors occured
c                         =-1    read attempted from nonexistent
c                                family member
c
c     implicit integer(a-z)                                             cos
c     dimension fit(8),w(nw)                                            cos
c
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     cos
c     character*8 frfn,frn,kfn                                          cos
c     common/frfcm2/frfn(2,16),frn,kfn                                  cos
c
c.... get family size, family root name, and name of open family member
c     ifrf=fit(2)                                                       cos
c     fcsize=fit(4)                                                     cos
c     frn=frfn(1,ifrf)                                                  cos
c     kfn=frfn(2,ifrf)                                                  cos
c.... get buffer pointers
c     buflen=iabs(fit(3))                                               cos
c     disklc =fit(5)                                                    cos
c     curlen=fit(6)                                                     cos
c     kop=0                                                             cos
c     ier=iabs(ir)                                                      cos
c     ir=0                                                              cos
c     l=nw                                                              cos
c     m=0                                                               cos
c.... set up access to correct family member
c  30 kd=da+m                                                           cos
c     call asgrfm (kd,fit)                                              cos
c     if (ier.lt.0) then                                                cos
c     ir=ier                                                            cos
c     return                                                            cos
c     endif                                                             cos
c     ll=min0(l,fit(4)-kd)                                              cos
c.... see if requested data is in the buffer
c  40 i=kd-disklc                                                       cos
c     if (i.lt.0) go to 50                                              cos
c     bloc=i                                                            cos
c     blen=min0(ll,curlen-i)                                            cos
c     i=0                                                               cos
c     go to 60                                                          cos
c  50 bloc=0                                                            cos
c     blen=ll+i                                                         cos
c     if (blen.gt.curlen) go to 80                                      cos
c.... branch if none of the data is in the buffer
c  60 if (blen.le.0) go to 80                                           cos
c     ll=blen                                                           cos
c     do 70 k=1,ll                                                      cos
c     w(k+m-i)=fit(k+bloc+7)                                            cos
c  70 continue                                                          cos
c     if (i.lt.0) m=m-ll                                                cos
c     l=l-ll                                                            cos
c     m=m+ll                                                            cos
c.... loop if all requested data has not been transferred
c     if (l.ne.0) go to 30                                              cos
c     return                                                            cos
c.... flush the buffer if data is present which is not on disk
c  80 if (fit(3).lt.0) then                                             cos
c     fit(3)=-fit(3)                                                    cos
c     call wdiska (fit(1),fit(8),buflen,disklc )                        cos
c     fit(7)=max0(fit(7),disklc +buflen)                                cos
c     endif                                                             cos
c.... for blocks larger than the buffer, read the data directly
c     if (ll.lt.buflen) go to 110                                       cos
c     if (mod(kd,buflen).ne.0) go to 110                                cos
c     nr=ll/buflen                                                      cos
c     do 100 n=1,nr                                                     cos
c     call rdiska (fit(1),w(m+1),buflen,kd)                             cos
c     m=m+buflen                                                        cos
c     kd=kd+buflen                                                      cos
c 100 continue                                                          cos
c     l=l-nr*buflen                                                     cos
c     if (l.eq.0) return                                                cos
c     ll=ll-nr*buflen                                                   cos
c     if (ll.eq.0) go to 30                                             cos
c.... fill the buffer
c 110 continue                                                          cos
c     disklc =kd-mod(kd,buflen)                                         cos
c     fit(5)=disklc                                                     cos
c     call rdiska (fit(1),fit(8),buflen,disklc )                        cos
c     curlen=buflen                                                     cos
c     fit(6)=curlen                                                     cos
c     go to 40                                                          cos
c     end                                                               cos
c     subroutine rdiska (lus,w,nw,da)                                   cos
c
c     interface to direct access i/o for rwabsf random i/o routines
c
c.... entry to transfer one record from disk to a buffer
c
c     input arguments
c            lus      the file unit specifier (logical unit no.)
c            nw       number of words to read from disk
c                     (must be a multiple of 512)
c            da       zero base disk word.address
c                     (must be on a sector boundary)
c
c     output arguments
c            w        data read from disk
c
c     implicit integer(a-z)                                             cos
c     dimension w(nw)                                                   cos
c
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     cos
c
c     lda=da/buflen+1                                                   cos
c     read (lus,rec=lda,iostat=ios) w                                   cos
c     return                                                            cos
c     end                                                               cos
c     subroutine wdiska(lus,w,nw,da)                                    cos
c     implicit integer(a-z)                                             cos
c     dimension w(nw)                                                   cos
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     cos
c
c
c.... entry to transfer one record from a buffer to disk
c
c     input arguments
c            lus      the file unit specifier (logical unit no.)
c            w        data to be written to disk
c            nw       number of words to write to disk
c                     (must be a multiple of 512)
c            da       zero base disk word.address
c                     (must be on a sector boundary)
c
c     lda=da/buflen+1                                                   cos
c     print59,lus,w,nw,da,buflen
c     write (lus,rec=lda) w                                             cos
c     return                                                            cos
c     end                                                               cos
c     subroutine riosta  (fit)                                          cos
c
c.... entry to check if i/o is complete without errors (dummy)
c
c     calling sequence: call riosta (us)
c
c     input arguments
c            fit      the file information table
c
c     implicit integer(a-z)                                             cos
c     dimension fit(8)                                                  cos
c
c     character kfn*8,nerr*4,msgi*10,msgj*27                            cos
c     data msgi/' i/o error'/, msgj/' on familied random file - '/      cos
c
c     inquire (unit=fit(1),name=kfn,nextrec=numrec)                     cos
c     if (ios.eq.0) return                                              cos
c     write (nerr,'(i4)')ios                                            cos
c     stop        ' i/o error on familied random file '                 cos
c     end                                                               cos
c     subroutine wrabsf (fit,w,nw,da)                                   cos
c
c.... entry for random write
c
c     calling sequence: call wrabsf (fit,w,nw,da)
c
c     input arguments
c            fit      the file information table
c            w        data to be written to disk
c            nw       number of words to write to disk
c            da       zero base disk address
c
c     implicit integer (a-z)                                            cos
c     dimension fit(8),w(nw)                                            cos
c
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     cos
c     character*8 frfn,frn,kfn                                          cos
c     common/frfcm2/frfn(2,16),frn,kfn                                  cos
c
c.... get family size, family root name, and name of open family member
c     ifrf=fit(2)                                                       cos
c     fcsize=fit(4)                                                     cos
c     frn=frfn(1,ifrf)                                                  cos
c     kfn=frfn(2,ifrf)                                                  cos
c.... get buffer pointers
c     buflen=iabs(fit(3))                                               cos
c     disklc =fit(5)                                                    cos
c     curlen=fit(6)                                                     cos
c     kop=1                                                             cos
c     l=nw                                                              cos
c     m=0                                                               cos
c.... set up access to correct family member
c  30 kd=da+m                                                           cos
c     call asgrfm (kd,fit)                                              cos
c     ll=min0(l,fit(4)-kd)                                              cos
c.... move data into buffer
c  40 i=kd-disklc                                                       cos
c     if (i.lt.0) go to 50                                              cos
c     bloc=i                                                            cos
c     blen=min0(ll,buflen-i)                                            cos
c     i=0                                                               cos
c     go to 60                                                          cos
c  50 bloc=0                                                            cos
c     blen=ll+i                                                         cos
c     if (blen.gt.buflen) go to 80                                      cos
c.... branch if no overlap
c  60 if (blen.le.0) go to 80                                           cos
c     ll=blen                                                           cos
c     do 70 k=1,ll                                                      cos
c     fit(k+bloc+7)=w(k+m-i)                                            cos
c  70 continue                                                          cos
c     fit(3)=-buflen                                                    cos
c     if (i.lt.0) m=m-ll                                                cos
c     l=l-ll                                                            cos
c     m=m+ll                                                            cos
c.... loop if all requested data has not been transferred
c     if (l.ne.0) go to 30                                              cos
c     return                                                            cos
c.... flush the buffer if data is present which is not on disk
c  80 if (fit(3).lt.0) then                                             cos
c     fit(3)=-fit(3)                                                    cos
c     call wdiska (fit(1),fit(8),buflen,disklc )                        cos
c     fit(7)=max0(fit(7),disklc +buflen)                                cos
c     endif                                                             cos
c.... for blocks larger than the buffer, write the data directly
c  90 if (ll.lt.buflen) go to 110                                       cos
c     if (mod(kd,buflen).ne.0) go to 110                                cos
c     nr=ll/buflen                                                      cos
c     do 100 n=1,nr                                                     cos
c     call wdiska (fit(1),w(m+1),buflen,kd)                             cos
c     m=m+buflen                                                        cos
c     kd=kd+buflen                                                      cos
c 100 continue                                                          cos
c     fit(7)=max0(fit(7),kd)                                            cos
c     l=l-nr*buflen                                                     cos
c     if (l.eq.0) return                                                cos
c     ll=ll-nr*buflen                                                   cos
c     if (ll.eq.0) go to 30                                             cos
c.... initialize the buffer if required
c 110 continue                                                          cos
c     disklc =kd-mod(kd,buflen)                                         cos
c     fit(5)=disklc                                                     cos
c     curlen=min0(idim(fit(7),disklc ),buflen)                          cos
c     if (curlen.lt.1) go to 120                                        cos
c     if ((kd.ne.disklc ).or.(ll.lt.curlen)) then                       cos
c     call rdiska (fit(1),fit(8),curlen,disklc )                        cos
c     endif                                                             cos
c 120 curlen=buflen                                                     cos
c     fit(6)=curlen                                                     cos
c     go to 40                                                          cos
c     end                                                               cos
c     subroutine dtaman ( lun )                                         cos
c     implicit double precision (a-h,o-z)                               cos  dp
c     character*10 sopt                                                 cos
c     character*8  frfn                                                 cos
c     character*50 mhsopt1                                              cos
c     character*80 fname                                                cos
c     common/mhs1/ sopt,mhsopt1                                         cos
c     common/frfcm2/frfn(2,16)                                          cos
c
c     fname = mhsopt1//frfn(2,lun)                                      cos
c
c     if ( sopt .eq. 'save' )                                           cos
c    1  call save( result,'dn'l, frfn(2,lun), 'pdn'l, frfn(2,lun)       cos
c    1                  , 'id'l, mhsopt1 )                              cos
c
c     if( sopt . eq. 'dispose' )                                        cos
c    1  call dispose( result, 'dn'l, frfn(2,lun) , 'dc'l, 'st'l,        cos
c    1                        'df'l, 'tr'l,'text',fname)                cos
c
c     if( sopt .eq. 'delete' ) call release(result,'dn'l, frfn(2,lun))  cos
c
c     if( result.eq.0 )                                                 cos
c    1  then                                                            cos
c         assign 100 to label                                           cos
c         call remarkf( label, result, lun, frfn(2,lun) )               cos
c       else                                                            cos
c         assign 200 to label                                           cos
c         call remarkf( label, result, lun, frfn(2,lun) )               cos
c     endif                                                             cos
c
c     return                                                            cos
c100  format('dyna2 - file op complete ',f4.0,2x,i4,2x,a8)              cos
c200  format('dyna2 - file op error ',f4.0,2x,i4,2x,a8)                 cos
c     end                                                               cos
c     subroutine mmemory(a,n)                                           cos
c     implicit double precision (a-h,o-z)                               cos  dp
c     dimension a(1)                                                    cos
c     common/zrray/maxa                                                 cos
c     data length/1/                                                    cos
c     length=length+n                                                   cos
c     write( *,10)n,length                                              cos
c  10 format(//,1x,'increasing blank common by ',i5,' words to',i6 )    cos
c     call memory ('uc',n)                                              cos
c     end                                                               cos
      subroutine mmemory(a,n)                                           unics
c     implicit double precision (a-h,o-z)                               unicsdp
      dimension a(1)                                                    unics
      common/zrray/maxa                                                 unics
      data length/1/                                                    unics
      length=length+n                                                   unics
      call memory (a,n)                                                 unics
      end                                                               unics
c     subroutine timuse (ait)                                           cos
c     implicit double precision (a-h,o-z)                               cos  dp
c     dimension ait(1)                                                  cos
c     call second(cpu)                                                  cos
c     oin = 0.0                                                         cos
c     ait(1)=cpu                                                        cos
c     ait(2)=oin                                                        cos
c     return                                                            cos
c     end                                                               cos
c     subroutine asgrfm(da,fit)                                         cos
c
c     assign next family member for random i/o
c
c     implicit integer(a-z)                                             cos
c     dimension fit(8)                                                  cos
c
c     common/frfcm1/mxfrf,ifrf,buflen,fcsize,disklc ,curlen,kop,ier     cos
c     character*8 frfn,frn,kfn                                          cos
c     common/frfcm2/frfn(2,16),frn,kfn                                  cos
c
c     logical fxist                                                     cos
c     character nfn*8,msg*49                                            cos
c
c     parameter giving number of record units per integer word
c     for most systems a single character is used as a record unit
c     parameter (ncpw=8)                                                cos
c
c     data msg/' read requested from nonexistent family member - '/     cos
c
c.... compute family member index & bias disk address for correct access
c     i=da/fcsize                                                       cos
c     da=da-i*fcsize                                                    cos
c.... get the name of the requested family member
c     call nrfnam(frn,i,nfn)                                            cos
c.... return if current family member is the desired one
c     if(kfn.eq.nfn) return                                             cos
c.... flush the buffer if data is present which is not on disk
c     if (fit(3).lt.0) then                                             cos
c     fit(3)=-fit(3)                                                    cos
c     call wdiska (fit(1),fit(8),buflen,disklc )                        cos
c     fit(7)=max0(fit(7),disklc +buflen)                                cos
c     endif                                                             cos
c.... determine if requested family member exists
c     if (kop.ne.0) go to 20                                            cos
c     inquire (file=nfn,exist=fxist)                                    cos
c     if (fxist) go to 20                                               cos
c     ier=-ier                                                          cos
c     if (ier.lt.0) return                                              cos
c     stop        ' read attempted from nonexistent file '              cos
c.... close the current family member
c  20 continue                                                          cos
c     if (kfn.ne.'        ') then                                       cos
c     close (fit(1),status='keep')                                      cos
c     if( fit(1).ne.3) call dtaman (fit(2) )                            cos
c     endif                                                             cos
c.... open/create the requested family member
c     cbuflen= buflen*ncpw                                              cos
c     open (fit(1),file=nfn,access='direct',form='unformatted'          cos
c    1,recl=cbuflen,status='unknown')                                   cos
c     kfn=nfn                                                           cos
c     frfn(2,ifrf)=kfn                                                  cos
c     fit(5)=fit(4)                                                     cos
c     disklc =fit(5)                                                    cos
c     fit(6)=0                                                          cos
c     curlen=fit(6)                                                     cos
c     fit(7)=0                                                          cos
c     if (fxist) fit(7)=fit(4)                                          cos
c     return                                                            cos
c     end                                                               cos
c     subroutine nrfnam(frn,i,nfn)                                      cos
c
c     form the file name for member i+1 of random family kfn
c
c     input arguments
c           frn       family root name (name of the first family member)
c            i        family member index for member i+1
c
c     output arguments
c           nfn       file name for member i+1
c
c     implicit integer (a-z)                                            cos
c     character*8 frn,nfn                                               cos
c
c     character msg*43,ni*1                                             cos
c     dimension ni(10)                                                  cos
c     data msg/' family member index exceeds 99 for file - '/           cos
c     data ni/'0','1','2','3','4','5','6','7','8','9'/                  cos
c
c     if(i.ne.0) go to 11                                               cos
c     nfn=frn                                                           cos
c     return                                                            cos
c  11 if(i.lt.100) go to 21                                             cos
c     stop        ' family member index exceeds 99 '                    cos
c  21 continue                                                          cos
c     j=i/10                                                            cos
c     nfn=frn(1:5)//ni(j+1)//ni(i-10*j+1)                               cos
c     return                                                            cos
c     end                                                               cos
c     subroutine famsiz(n,m)                                            cos
c     implicit double precision (a-h,o-z)                               cos  dp
c     return                                                            cos
c     end                                                               cos
      subroutine spcbc (r,v,itcode,neq,ndof)
c     implicit double precision (a-h,o-z)                                    dp
c
c....  this routine applies the boundary condition on the local
c      coordinate system of v
c
c      r     = rotation matrix
c      v     = vector
c      icode = spc code
c      neq   = numnp * 3
c      ndof = number of degree of freedoms
c
      dimension r(3,3,*), v(*), t(3) ,jj(6)
c
c     decode icode
c
      call spcdec(itcode,jj,ic)
c
      if(ic.eq.0) go to 400
c
      if (jj(1)+jj(2)+jj(3).ne.0) then
c
c  1. apply spc on translational degree of freedoms
c
c
c     rotate v to local coordinate system
c      t = r * v
c
      t(1) = r(1,1,ic)*v(1)+r(2,1,ic)*v(2)+r(3,1,ic)*v(3)
      t(2) = r(1,2,ic)*v(1)+r(2,2,ic)*v(2)+r(3,2,ic)*v(3)
      t(3) = r(1,3,ic)*v(1)+r(2,3,ic)*v(2)+r(3,3,ic)*v(3)
c
      if(jj(1).ne.0) t(1) = 0
      if(jj(2).ne.0) t(2) = 0
      if(jj(3).ne.0) t(3) = 0
c
c     rotate t back to global coordineate xystem
c
      v(1) = r(1,1,ic)*t(1)+r(1,2,ic)*t(2)+r(1,3,ic)*t(3)
      v(2) = r(2,1,ic)*t(1)+r(2,2,ic)*t(2)+r(2,3,ic)*t(3)
      v(3) = r(3,1,ic)*t(1)+r(3,2,ic)*t(2)+r(3,3,ic)*t(3)
      endif
c
c
c
c  2. to apply spc on rotation degree of freedoms
c
      if (ndof.lt.6) return
      if (jj(4)+jj(5)+jj(6).eq.0) return
c
c     rotate v to local coordinate system
c      t = r * v
c
      t(1) = r(1,1,ic)*v(neq+1)+r(2,1,ic)*v(neq+2)+r(3,1,ic)*v(neq+3)
      t(2) = r(1,2,ic)*v(neq+1)+r(2,2,ic)*v(neq+2)+r(3,2,ic)*v(neq+3)
      t(3) = r(1,3,ic)*v(neq+1)+r(2,3,ic)*v(neq+2)+r(3,3,ic)*v(neq+3)
c
      if(jj(4).ne.0) t(1) = 0
      if(jj(5).ne.0) t(2) = 0
      if(jj(6).ne.0) t(3) = 0
c
c     rotate t back to global coordineate xystem
c
      v(neq+1) = r(1,1,ic)*t(1)+r(1,2,ic)*t(2)+r(1,3,ic)*t(3)
      v(neq+2) = r(2,1,ic)*t(1)+r(2,2,ic)*t(2)+r(2,3,ic)*t(3)
      v(neq+3) = r(3,1,ic)*t(1)+r(3,2,ic)*t(2)+r(3,3,ic)*t(3)
      return
c
c     apply b.c. on global system
c
400   if(jj(1).ne.0) v(1) = 0
      if(jj(2).ne.0) v(2) = 0
      if(jj(3).ne.0) v(3) = 0
      if(ndof.lt.6) return
      if(jj(4).ne.0) v(neq+1) = 0
      if(jj(5).ne.0) v(neq+2) = 0
      if(jj(6).ne.0) v(neq+3) = 0
c
      return
      end
      subroutine spcbd(ax,ay,az,ispc)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
c
      common /   / a(1)
c
      dimension v(3)
c
      v(1) = ax
      v(2) = ay
      v(3) = az
      l2 = lc7a + (ispc-1)
      call spcbc(a(lc7b),v,a(l2),neq,ndof)
c
      ax = v(1)
      ay = v(2)
      az = v(3)
c
      return
      end
      subroutine itrpd2(p,du,numlp,f,xmag,mark,dl,slope)
c     implicit double precision (a-h,o-z)                                    dp
      dimension p(2,1)
c
c     interpolation for general nonlinear springs
c
c     mark=1 finds force for a given displ.
c     mark=2 finds displ for a given force
c     dl     displacement at f=0.
c
      if (mark.eq.1) then
      k1=1
      k2=2
      tin=du-dl
      else
      k1=2
      k2=1
      tin=f/xmag
      endif
c
      do 20 m=2,numlp
      if (tin-p(k1,m).le.0.0) go to 30
   20 continue
      m=numlp
c
   30 dt1=tin-p(k1,m-1)
      d1 =p(k1,m)-p(k1,m-1)
      d2 =p(k2,m)-p(k2,m-1)
      tout=p(k2,m-1)+dt1*d2/d1
      if (mark.eq.1) then
      slope=xmag*d2/d1
      f    =xmag*tout
      endif
      if (mark.eq.2)du=tout+dl
      return
      end
      subroutine gttxsg(txts,lcount)
c     implicit double precision (a-h,o-z)                                    dp
      character*80 txts,mssg
      integer count
      save count
      data count/0/
   10 read (1,20) txts
      count=count+1
      if (txts(1:1).eq.'*'.or.txts(1:1).eq.'$') then
      go to 10
      else
      lcount=count
      return
      endif
   20 format (a80)
      end
      subroutine termin (txts,mssg,lcount,iprint)
c     implicit double precision (a-h,o-z)                                    dp
      character*80 txts,mssg
c
      if (iprint.eq.0) then
      write (13,30) lcount,lcount,txts
      write ( *,30) lcount,lcount,txts
      else
      write (13,20) lcount,lcount,txts,mssg
      write ( *,20) lcount,lcount,txts,mssg
      endif
      call adios (2)
      return
   20 format(///
     1 '     line number',i7,' contains improperly formatted data',//
     2,'************************************************line#',i7
     3,/,a80,/
     4,'************************************************************',/,
     5   a80,/)
   30 format(///
     1 '     line number',i7,' contains improperly formatted data',//
     2,'************************************************line#',i7
     3,/,a80,/
     4,'************************************************************',/)
      end
      subroutine bmslid (x, ac)
c     implicit double precision (a-h,o-z)                                    dp
      common/   / a(1)
      common / bk00 / numnp
      common / bmsc / numsl, locp, lenp, lhp, buf(1),
     .                nmn, nsn, lmsr, lnsv, lilocs, lfshr, lssm, lsa,
     .                ldmg, radius, fcci, shearm, umax, ibond, hdmg
c     ---- eliminate le, lmsm, lilocm
      loc = locp - 1
      do 20 isl = 1, numsl
c       --  load slideline parameters and pointers  --
c     void = memcpy (a(loc), buf, lenp)
        do 10 i = 1, lenp
   10     buf(i+1) = a(loc+i)
        call sl1ds1 (x, ac, a(lfshr), a(lhp), a(lmsr), a(lnsv),
     .               a(lilocs), a(lsa), a(lssm), a(ldmg))
        loc = loc + lenp
   20 continue
c
c     --  clear array hp  --
c      void = memclr (a(lhp), numnp)
      do 30 i = 0, numnp-1
        a(lhp+i) = 0.
   30 continue
      return
      end
      subroutine sl1ds1 (x,f,fshr,hp,msr,nsv,ilocs,sa,ssm,dmg)
c     implicit double precision (a-h,o-z)                                    dp
      parameter (nel = 128)
      common / bmsc / numsl,locp,lenp,lhp,buf(1),
     .                nmn,nsn,lmsr,lnsv,lilocs,lfshr,lssm,lsa,
     .                ldmg,radius,fcci,shearm,umax,ibond,hdmg
      dimension x(3,*),f(3,*),fshr(*),hp(*),
     .          msr(*),nsv(*),ilocs(*),sa(*),ssm(*),dmg(*)
      common /aux14 /  y1(nel), y2(nel), y3(nel),xa1(nel),xa2(nel),
     .                 xa3(nel),xb1(nel),xb2(nel),xb3(nel), xn1(nel),
     .                 xn2(nel),xn3(nel), ss(nel),d(nel),dmax(nel),
     .                 hpa(nel),hpb(nel),pf(nel),press(nel),
     .                 xbxa1(nel),xbxa2(nel),xbxa3(nel),xbxal(nel),
     .                 yxa1(nel),yxa2(nel),yxa3(nel),xbxali(nel),
     .                 ydist1(nel),ydist2(nel),ydist3(nel),
     .                 f1(nel),f2(nel),f3(nel),fa1(nel),fa2(nel),
     .                 fa3(nel),fb1(nel),fb2(nel),fb3(nel),
     .                 fshr2(nel),dplst(nel)
      common / bk02 / iburn,dt1,dt2
c
c     this subroutine implements a slide line interface to constrain
c     a series of beam elements (rebars) to move along
c     a series of master nodes. in this formulation sliding
c     is allowed.
c     notation.
c     is, im, the first letter i refers to local index (local to the
c             slideline)
c     js, jm, the letter j refers to the global indexing scheme for
c             the nodal elements
c     is, js, the second letter s refers to slave line
c     im, jm, the second letter m refers to slave line
c     array msr stores master nodes sequentially
c     array msr stores master nodes sequentially
c     ilocs(is) store index of master segment closer to slave node is
c     ilocm(im) store index of slave segment closer to master node im
c
c     . loop over slave nodes
      npass = (nsn+nel-1) / nel
      do 70 ipass = 1, npass
        nfirst = nel*(ipass-1)
        nlast = min(nsn-nfirst,nel)
        do 10 i = 1, nlast
          is = nfirst + i
          js = nsv(is)
c         --  master segment ma mb containing node s  --
          im = ilocs(is)
          jma = msr(im)
          jmb = msr(im+1)
c         --  transfer nodal coordinates for vectorization  --
          y1(i) = x(1,js)
          y2(i) = x(2,js)
          y3(i) = x(3,js)
          xa1(i) = x(1,jma)
          xa2(i) = x(2,jma)
          xa3(i) = x(3,jma)
          xb1(i) = x(1,jmb)
          xb2(i) = x(2,jmb)
          xb3(i) = x(3,jmb)
   10   continue
c       --  coordinate ss of s and unitary vector parallel to mb-ma  --
        call sl1ds2 (nlast, ssm(1+nfirst))
        do 20 i = 1, nlast
          is = nfirst + i
          js = nsv(is)
          im = ilocs(is)
c         --  check if slave nodes have moved to adjacent master
c             segments  --
          if (ss(i).ge.1.0001 .and. im.lt.nmn-1) then
c           --  slave node (is) moves from segment [im] to
c               segment [im+1] --
c           slave side       [is-1]    (is)* ->   [is]
c           master side   (im)*  [im]  (im+1)*  [im+1]  (im+2)*
            ilocs(is) = im + 1
            call sl1ds3 (x, nsv, msr, ssm, ilocs, i, is)
          end if
          if (ss(i).le.-.0001 .and. im.gt.1) then
c           --  slave node (is) moves from segment [im] to
c               segment [im-1] --
c               slave side          [is-1]   <- *(is)   [is]
c               master side   *(im-1)  [im-1]  *(im)  [im]  *(im+1)
            ilocs(is) = im - 1
            call sl1ds3 (x, nsv, msr, ssm, ilocs, i, is)
          end if
c         --  hydrostatic pressure at ma and mb  --
          im = ilocs(is)
          jma = msr(im)
          jmb = msr(im+1)
          hpa(i) = hp(jma)
          hpb(i) = hp(jmb)
   20   continue
        do 30 i = 1, nlast
c         --  pressure at m  --
c          press(i) = hpa(i) + ss(i)*(hpb(i)-hpa(i))
          press(i) = 0.
c         --  maximum allowable displacement  --
          pf(i) = press(i) * fcci
          dmax(i) = umax * (1. + 1.5 * pf(i)*pf(i)) *
     .              exp (- hdmg *dmg(i+nfirst))
          fshr2(i) = fshr(i+nfirst)
   30   continue
c
        if (ibond.eq.0) then
c         --  perfectly elasto plastic bond model (incremental)  --
          do 40 i = 1, nlast
            fshr(i+nfirst) = fshr(i+nfirst) - sa(i+nfirst)*d(i)
            fshr(i+nfirst) = sign(
     .            min( abs(fshr(i+nfirst)), sa(i+nfirst)*dmax(i) ),
     .          fshr(i+nfirst))
   40     continue
        else
c         --  elasto-plastic with hardening bond model (incremental)  --
          do 45 i = 1, nlast
            fshr(i+nfirst) =  fshr(i+nfirst) +
     .          (sign(sa(i+nfirst)*dmax(i),d(i)) - fshr(i+nfirst)) *
     .          ( 1. - exp(-abs(d(i))/dmax(i)) )
   45     continue
        end if
c
        do 50 i = 1, nlast
c         --  update damage parameter  --
          dplst(i) = abs(d(i)) -
     .               abs(fshr(i+nfirst)-fshr2(i))/sa(i+nfirst)
          dmg(i+nfirst) = dmg(i+nfirst) + dplst(i)
          f1(i) = fshr(i+nfirst)*xn1(i) - sa(i+nfirst)*ydist1(i)
          f2(i) = fshr(i+nfirst)*xn2(i) - sa(i+nfirst)*ydist2(i)
          f3(i) = fshr(i+nfirst)*xn3(i) - sa(i+nfirst)*ydist3(i)
          fb1(i) = f1(i) * ss(i)
          fb2(i) = f2(i) * ss(i)
          fb3(i) = f3(i) * ss(i)
          fa1(i) = f1(i) - fb1(i)
          fa2(i) = f2(i) - fb2(i)
          fa3(i) = f3(i) - fb3(i)
   50   continue
        do 60 i = 1, nlast
          is = nfirst + i
          js = nsv(is)
          im = ilocs(is)
          jma = msr(im)
          jmb = msr(im+1)
c         --  add interaction forces to slave node s  --
          f(1,js)  = f(1,js)  + f1(i)
          f(2,js)  = f(2,js)  + f2(i)
          f(3,js)  = f(3,js)  + f3(i)
c         --  add interaction forces to master nodes ma, mb  --
          f(1,jma) = f(1,jma) - fa1(i)
          f(2,jma) = f(2,jma) - fa2(i)
          f(3,jma) = f(3,jma) - fa3(i)
          f(1,jmb) = f(1,jmb) - fb1(i)
          f(2,jmb) = f(2,jmb) - fb2(i)
          f(3,jmb) = f(3,jmb) - fb3(i)
   60     continue
   70   continue
      return
      end
