      subroutine bgnfim (iextra,nextra,x,code)
c     implicit double precision (a-h,o-z)                                    dp
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      dimension x(3,1),code(*)
      ipoint=2+4*iextra
      call rdabsf (iob16,x,3*nextra,ipoint,ioerr)
      call riosta (iob16)
      do 10 i=1,nextra
   10 code(i)=7.0
      return
      end
      subroutine outifs (nsv,xnsv,x,tt)
c     implicit double precision (a-h,o-z)                                    dp
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      dimension nsv(*),xnsv(3,*),x(3,*)
      if (tt.lt.opnext) return
      opnext=opnext+opifs
      do 10 i=1,nsnifs
      xnsv(1,i)=x(1,nsv(i))
      xnsv(2,i)=x(2,nsv(i))
   10 xnsv(3,i)=x(3,nsv(i))
      call wrabsf (iob15,tt,1,ilink)
      call riosta (iob15)
      ilink=ilink+1
      call wrabsf (iob15,xnsv,3*nsnifs,ilink)
      call riosta (iob15)
      ilink=ilink+3*nsnifs
      tlong=10.*tt+1000.
      call wrabsf (iob15,tlong,1,ilink)
      call riosta (iob15)
      call wrabsf (iob15,xnsv,3*nsnifs,ilink+1)
      call riosta (iob15)
      return
      end
      subroutine infmov(dispn,time,timn,x,v)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      dimension dispn(3,*),x(3,*),v(3,*)
      if(ilk16.lt.0) return
      tol=1.e-12
      nxtra3=3*nextra
      if(time.lt.timn) go to 40
   10 ioerr=1
      call rdabsf(iob16,timn,nxtra3+1,ilk16,ioerr)
      ilk16=ilk16+nxtra3+1
      if(ioerr.ne.0) then
           ilk16=-1
           return
      endif
      call riosta (iob16)
      if(time+dt2.ge.timn) go to 10
   40 demn=1./(timn-time+tol)
      do 50 i=1,nextra
      v(1,i)=(dispn(1,i)-x(1,i))*demn
      v(2,i)=(dispn(2,i)-x(2,i))*demn
      v(3,i)=(dispn(3,i)-x(3,i))*demn
   50 continue
      return
      end
      subroutine infmvi(dispn,timn,x)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk02/iburn,dt1,dt2,isdo
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      dimension dispn(3,*),x(3,*)
      nxtra3=3*nextra
      call rdabsf(iob16,timn,nxtra3+1,ilk16,ioerr)
      ilk16=ilk16+3*nextra+1
      call riosta (iob16)
      do 50 i=1,nextra
      x(1,i)=dispn(1,i)
      x(2,i)=dispn(2,i)
      x(3,i)=dispn(3,i)
   50 continue
      return
      end
      subroutine intial
c     implicit double precision (a-h,o-z)                                    dp
c***********************************************************************
c                                                                      *
c          ***  *     *  ***  *******  ***    ***    *                 *
c           *   **    *   *      *      *    *   *   *                 *
c           *   * *   *   *      *      *   *     *  *                 *
c           *   *  *  *   *      *      *   *******  *                 *
c           *   *   * *   *      *      *   *     *  *                 *
c           *   *    **   *      *      *   *     *  *                 *
c          ***  *     *  ***     *     ***  *     *  *******           *
c                                                                      *
c***********************************************************************
c
c     overlay to initialize mass matrix, compute basis functions, etc.
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      common /   / a(1)
c
      nttls=(n32-n31)/2
      nttlm=(n33-n32)/2
      nrtls=(n42-n41)/2
      nrtlm=(n43-n42)/2
      call initlz (a(n1),a(n2),a(n3),a(n7),a(n8),a(n9),a(n10),a(n11),a
     1 (n12),a(n13),a(n14),a(n15),a(n16),a(n17),a(n18),a(n19),a(n20),a
     2 (n21),a(n22),a(n23),a(n24),a(n25),a(n30),a(n31),a(n32),a(n33),a
     3 (n34),a(n35),a(n36),a(n37),a(n38),a(n39),a(n40),a(n41),a(n42),a
     4 (n43),a(n44),a,a(lc7),a(lc0),a(lc9),a(lc10),a(lc11),a(lc14),a
     5 (lc15),a(n4),a(n5),a(n29),a(n48),a(n4a),a(n4b),a(ntmp0),a(ntmp1),
     6 a(n49),a(n50),a(n51),a(n52),a(n53),a(n54),a(n55),a(n56),a(n57),
     7 a(n58),a(n59),a(n60),a(n61),a(n62),a(n63),a(n64),a(n68),a(n69),
     8 a(n67),a(n77),a(n73),a(n74),a(ips),a(ipm),a(lc1s),a(n4c),a(n78),
     9 a(n31+nttls),a(n32+nttlm),a(n8+nlcur+1),a(n72+3*numsv),a(ju),
     $ a(jv),a(n41+nrtls),a(n42+nrtlm),a(nh17),a(lc1h))
c
      return
      end
      subroutine initlz(mtype,ro,cm,bcs,npc,pld,nod,idirn,ncur,clfac,
     1lc,nvel,vx,vy,vz,fval,rd,ilcw,nsw,numtp,nodtie,tim,iparm,irects,
     2irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,ilocs,ilocm,stfs,stfm,
     3irtls,irtlm,b,tcode,u,v,a,x,xms,auxvec,rhsi,rhodp,burn,zfac,
     4ieost,eosp,told,tnew,rbu,rbv,rda,rbi,rbm,rbcor,nrbn,nrba,nrb,
     5xrb,yrb,zrb,axrb,ayrb,azrb,rbms,rbcods,mxrb,rbncod,xyzkcn,
     6chrlen,ethik,nsntb,nmntb,ixps,ihgq,fthik,irctsi,irctmi,iclf,
     7fails,sfail,fl9s,thkslv,thkmsr,nhex,ipsh)
c     implicit double precision (a-h,o-z)                                    dp
c
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
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/b10a/inpsd,nmmtde,nmelde,nmmass
      common/b10b/ld01,ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(27)
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/tmstp/tsmin,hmax,lctm
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/auxstr/aux(700)
      common/nwixa/nwcon
      common/slv2/thk,isrch
      common/thfl/madd,ifth,maddbg
      common/sbic/ijne,nblk
      common/tint/itsc,itbs,itlc
      common/ingstr/nmigs,mtigs(12)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/strhx/nmsi,matsi(3,400),mxix(9),xe(3,8),ve(3,8),
     1 hisvar(100)
      logical output,slnew
      common/csforc/ncs1(1),ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/bmsc/numsl(20)
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/drelax/ladd,idrint
      common/rwint/intiad,ncint1,nhint1,nsint1
c
      dimension mtype(*),ro(*),cm(48,*),bcs(4,*),npc(*),irtls(*),
     1 pld(*),nod(*),idirn(*),ncur(*),clfac(*),lc(*),nvel(*),vx(*),
     2 vy(*),vz(*),fval(*),rd(14,*),ilcw(*),nsw(*),numtp(*),nodtie(*),
     3 tim(*),iparm(8,*),irects(*),irectm(*),nsv(*),msr(*),nsegs(*),
     4 nsegm(*),lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),b(*),
     5 tcode(*),u(*),v(*),a(*),x(3,*),xms(*),auxvec(*),rhsi(*),rhodp(*),
     6 irtlm(*),burn(*),zfac(*),ieost(*),irctsi(*),irctmi(*),fails(*),
     7 eosp(*),told(*),tnew(*),rbu(6,*),rbv(6,*),rbi(3,3,*),
     8 rbm(*),rbcor(3,*),nrbn(*),nrba(*),nrb(*),xrb(*),yrb(*),zrb(*),
     9 axrb(*),ayrb(*),azrb(*),rbms(*),rbcods(2,*),mxrb(*),rbncod(*),
     a xyzkcn(*),chrlen(*),ethik(*),nsntb(*),nmntb(*),ixps(2,*),ihgq(*)
      dimension sfail(2,*),fl9s(5,*),fthik(*),iclf(*),thkslv(*),
     1 thkmsr(*),nhex(*),ipsh(*)
      dimension xlinrt(3,3)
      dimension ginrt(3,3),rbcorl(3),rbcorg(3)
      logical active
c
c***  load curve for timestep
      if (lctm.le.nlcur) go to 5
      write(13,670) lctm,nlcur
      call adios(2)
c
c     initialize joy-dyna3d interface
c
    5 if (numsnc.ne.0) call setvli (b(n6a),b(n6a+1),b(n6b),b(n6b+1),b
     1 (n6c),3*numsnc)
c
      if (nnrbs+numsv.gt.0) then
      call elgen(numelh,ibins1,ibins2,locsav,locend,ipsh)
      endif
c
      if (numsv.gt.0) then
c
c     initialize slide and void logic
c
      do 40 m=1,2
      k1=1
      k2=1
      k3=1
      k4=1
      k5=1
      k6=1
      k7=1
      k8=1
      k9=1
      k10=1
      k13=1
      k14=1
      do 30 n=1,numsv
      nrts=iparm(1,n)
      nrtm=iparm(2,n)
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      nty=iparm(5,n)
      nst=iparm(6,n)
      mst=iparm(7,n)
      isrch=iparm(8,n)
      if ((nty.eq.1.or.nty.eq.3.or.nty.eq.5.or.nty.eq.9
     1   .or.nty.eq.10).and.(nsn.eq.0.or.nmn.eq.0)) then
      write( *,1000) n,nmn,nsn
      call adios(2)
      endif
      nlcslv=n30a+n-1
      nlcmsr=n30b+n-1
      chgflg=b(n30c+n-1)
      if (isrch.eq.0) thk=1.e10
      zfcm=0.
      zfcs=0.
      if (m.eq.2) go to 10
      call slavi0 (x,irects(k1),nsegs(k2),lnsv(k3),nsv(k4),msr(k5),ilocs
     1 (k4),nmn,nsn,nrts,b(lc10),nty)
      if (nmn.ne.0)
     1call slavi0 (x,irectm(k6),nsegm(k7),lmsr(k8),msr(k5),nsv(k4),ilocm
     1 (k5),nsn,nmn,nrtm,b(lc10),nty)
      if (nty.lt.5.or.nty.eq.9) then
      call stiffs (x,irects(k1),stfs(k9),ipsh,b(lc1s),cm,mtype,eosp,
     1 ieost,numelh,numels,numelt,nrts,nmmat,ro,zfcs,b(ns05),ethik(k4),
     2 nty,chrlen(n),b(lc1t),b(ibins1),b(ibins2),nsv(k4),nsn,stfss,
     3 thkslv(k9),b(nlcslv))
      if (nty.eq.9) then
      call nffcs(sfail(1,k13),fl9s(1,k14),irects(k1),nrts,nsv(k4),
     1 nsn,b(lc10))
      k13=k13+nrts
      k14=k14+nsn
      endif
      endif
      call stiffs (x,irectm(k6),stfm(k10),ipsh,b(lc1s),cm,mtype,eosp,
     1 ieost,numelh,numels,numelt,nrtm,nmmat,ro,zfcm,b(ns05),fthik(k5),
     2 nty,chrlen(n),b(lc1t),b(ibins1),b(ibins2),msr(k5),nmn,stfsm,
     3 thkmsr(k10),b(nlcmsr))
      zfac(n)=zfcm/(zfcm+zfcs)
      if (nty.eq.3) then
      stfmin=  min(stfss,stfsm)
      stfmax=  max(stfss,stfsm)
      if (stfmax/stfmin.gt.100.) then
      avgs=stfmin*sqrt(stfmax/stfmin)
      call mdstf3(stfs(k9) ,nrts,stfss,avgs,1,n)
      call mdstf3(stfm(k10),nrtm,stfsm,avgs,2,n)
      endif
      endif
      if (nty.eq.9) then
      stfmin=  min(stfss,stfsm)
      stfmax=  max(stfss,stfsm)
      if (stfmin.ne.0.0) then
      if (stfmax/stfmin.gt.100.) then
      avgs=stfmin*sqrt(stfmax/stfmin)
      call mdstf3(stfs(k9) ,nrts,stfss,avgs,1,n)
      call mdstf3(stfm(k10),nrtm,stfsm,avgs,2,n)
      endif
      else
      avgs=stfmax
      call mdstf9(stfs(k9) ,nrts,avgs)
      call mdstf9(stfm(k10),nrtm,avgs)
      endif
      endif
      if (nty.eq.2.or.nty.eq.6.or.nty.eq.7)
     1call slave1 (x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),irtlm
     1 (k4),nsegm(k7),nsn,nmn)
      if (nty.eq.1) then
c     call blkcpy(irects(k1),irctsi(k1),4*nrts)                         cray1
c     call blkcpy(irectm(k6),irctmi(k6),4*nrtm)                         cray1
      call blkcpi(irects(k1),irctsi(k1),4*nrts)                         vaxcs
      call blkcpi(irectm(k6),irctmi(k6),4*nrtm)                         vaxcs
      call sscnv(irctsi(k1),nrts,nsv(k4),nsn,b(lc10))
      call sscnv(irctmi(k6),nrtm,msr(k5),nmn,b(lc10))
      endif
      if (nty.eq.4) then
      call sscnv(irects(k1),nrts,nsv(k4),nsn,b(lc10))
      go to 20
      endif
      if (nty.eq.2.or.nty.eq.6.or.nty.eq.7) then
      call sscnv(irectm(k6),nrtm,msr(k5),nmn,b(lc10))
c
      if (nty.eq.2)
     1 call sscnv(irects(k1),nrts,nsv(k4),nsn,b(lc10))
c
      go to 20
      endif
   10 if (nty.eq.2.or.nty.eq.4.or.nty.eq.6.or.nty.eq.7) go to 20
      call slave1 (x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),irtlm
     1 (k4),nsegm(k7),nsn,nmn)
      if (nty.lt.5.or.nty.eq.9) then
      call slave1 (x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),irtls
     1 (k5),nsegs(k2),nmn,nsn)
      endif
      call slavi2 (x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),irtlm
     1 (k4),stfm(k10),nsn,nmn,nty,fthik(k5),nsegm(k7),n)
      if (nty.lt.5.or.nty.eq.9) then
      call slavi2 (x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),irtls
     1 (k5),stfs(k9),nmn,nsn,nty,ethik(k4),nsegs(k2),n)
      endif
      if (slnew) then
      if (m.eq.2.and.(nty.eq.3.or.nty.eq.5)) then
      call sscnv(irectm(k6),nrtm,msr(k5),nmn,b(lc10))
      call iazero (ilocs(k4),nsn)
      if (nty.eq.3) then
      call sscnv(irects(k1),nrts,nsv(k4),nsn,b(lc10))
      call iazero (ilocm(k5),nmn)
      endif
      endif
      endif
   20 k1=k1+4*nrts
      k2=k2+1+nsn
      k3=k3+nst
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+4*nrtm
      k7=k7+1+nmn
      k8=k8+mst
      k9=k9+nrts
      k10=k10+nrtm
      if (m.eq.2.and.chgflg.gt.0.50) then
      iparm(5,n)=5
      endif
   30 continue
   40 continue
      call wrabsg (iob5,x,neq,64,1)
      call riosta (iob5)
      endif
c
c     initialize 1d slidelines
c
      if (numsl(1).gt.0) then
      call int1ds (x)
      endif
c
c
c     initialize transmitting boundary segments
c
      if (nnrbs.gt.0) then
      call nbsint (b(lrb1),b(lrb2),b(lrb3),nnrbs,cm,mtype,eosp,ieost,
     & numelh,ipsh,b(ibins1),b(ibins2),x,ro,nmmat)
      endif
c
      if (numsv+nnrbs.gt.0) then
      call expndm(locsav)
      locend=locsav
      endif
c
      if (mkthf.ne.0) then
      call wrabsg (iob4,x,neq,maddbg,1)
      call riosta (iob4)
      endif
      if (idrint.gt.0) then
      call wrabsg (iob6,x,neq,64,1)
      call riosta (iob6)
      endif
c
c     if thermal behavior get temperatures
c
      zero=0.0
      if (itemp.gt.0) call ldcsti (fval,npc,pld,zero,iclf)
      if (itemp.lt.0) call tempri (told,told(2),tnew,tnew(2),numelh
     1 ,numnp,b(itsc),b(itbs),b(itlc),fval)
c
c
c     initialize material parameters and mass for solid hexahedrons
c
      if (numelh.eq.0) go to 310
c
      if ((istrnt.ne.0).and.(nhint1.gt.0)) then
      call rdabsg(iob7,nhint,1,intiad,0)
      call riosta(iob7)
      intiad=intiad+1
      write( *,9011) nhint
c
      if(nhint.ne.nhint1) then
      write(13,1030) nhint,nhint1
      write( *,1030) nhint,nhint1
      endif
c
      endif
      lav=1
      l11=1
      do 300 n=1,numelh
      call unpk(mx,ix,ipsh(l11),3)
      mt=mtype(mx)
      ihg=ihgq(mx)
      loc=48*(mx-1)
      nes=ieost(mx)
      rvl=1.0
      nmecon=7+nconst(mt)
      if (nes.ne.0) then
           nmecon=nmecon+ncneos(nes)
           if (nes.eq.1) locv=loc+9
           if (nes.eq.2) locv=loc+7
           if (nes.eq.3) locv=loc+7
           if (nes.eq.4) locv=loc+8
           if (nes.eq.5) locv=loc+34
           if (nes.eq.6) locv=loc+9
           if (nes.eq.7) locv=loc+34
           if (nes.eq.8) locv=loc+43
           if (nes.eq.9) locv=loc+33
           if (nes.eq.11)locv=loc+10
           rvl=eosp(locv)
      endif
c
      call massb (x,ro(mx),xms,rbm(mx),nhex(n),rvl,ihg)
      u(n)=det
c
      if (mt.gt.0.and.mt.ne.20) then
c
c     unpack history variable for initialization
c
      call hipac1 (nmecon,auxvec(lav))
c
c
c
c     initialize history variables
c
      if (mt.eq.2.or.mt.eq.21.or.mt.eq.23) then
      call inse2 (x,cm(1,mx),tnew(2),fval)
c
      elseif (mt.eq.3.or.mt.eq.30) then
      call inse3 (cm(1,mx))
c
      elseif (mt.eq.4) then
      call inse4 (cm(1,mx),tnew(2),fval)
c
      elseif (mt.eq.1.or.mt.eq.5.or.mt.eq.6.or.mt.eq.12.or.
     1 mt.eq.32) then
      call inse6 (cm(1,mx))
c
      elseif (mt.eq.7.or.mt.eq.27.or.mt.eq.31) then
      call inse7 (x,ihg)
c
      elseif (mt.eq.8) then
      call inse8 (rvl,cm(1,mx),eosp(locv-1),u(n),x,burn,ix,
     1 ndtpts,mx,n)
c
      elseif (mt.eq.9) then
      call inse10 (rvl,eosp(locv-1),u(n),cm(1,mx))
      if (nes.eq.7) call intes7 (eosp(loc+1))
c
      elseif (mt.eq.10) then
      call inse10 (rvl,eosp(locv-1),u(n),cm(4,mx))
      if (nes.eq.7) call intes7 (eosp(loc+1))
c
      elseif (mt.eq.11) then
      call inse11 (rvl,eosp(locv-1),u(n),cm(15,mx),cm(5,mx))
      if (nes.eq.7) call intes7 (eosp(loc+1))
c
      elseif (mt.eq.13) then
      call inse13 (cm(1,mx))
c
      elseif (mt.eq.14) then
      call inse14 (cm(1,mx))
c
      elseif (mt.eq.15) then
      call inse15 (rvl,eosp(locv-1),u(n),cm(11,mx))
c
      elseif (mt.eq.16) then
      call inse16 (rvl,eosp(locv-1),u(n),cm(2,mx))
c
      elseif (mt.eq.17) then
      call inse17 (cm(1,mx))
c
      elseif (mt.eq.18) then
      call inse6 (cm(1,mx))
c
      elseif (mt.eq.19) then
      call inse19 (cm(1,mx))
c
      elseif (mt.eq.22) then
      call inse22 (x,cm(10,mx))
c
      elseif (mt.eq.24) then
      call inse6 (cm(1,mx))
c
      elseif (mt.eq.25) then
      call inse25 (cm(1,mx))
c
      elseif (mt.eq.26) then
      call inse26 (cm(1,mx))
c
      endif
      if ((istrnt.ne.0).and.(nhex(n).le.nhint)) call hexint(n,nhex)
c
c     repack initialized history variables
c
      call hipac2 (nmecon,auxvec(lav),lav)
c
      endif
c
      l11=l11+nwcon
      if (numdp.ne.0) then
           if (nmigs.ne.0) then
                do 270 mgi=1,nmigs
                if(mtigs(mgi).eq.mx) go to 280
  270           continue
                go to 300
           endif
  280      if(nes.eq.0) then
                call gravs(u(n),x,cm(11,mx),cm(21,mx),cm(7,mx),rhodp
     1                    ,mt,nes)
           else
                call gravs(u(n),x,eosp(loc+1),eosp(loc+11),eosp(loc+48)
     1                    ,rhodp,mt,nes)
           endif
           if(aux(1).ne.0.) then
                call prtlb(x)
                call forceg(rhsi)
           endif
      endif
c
  300 continue
      if(istrnt.ne.0) intiad=intiad+nhint*7
c
c     initialize beam and shell elements
c
  310 if (numelb+numels.gt.0) then
c
      call ibmsh
c
      endif
c
c
c     compute total mass, inertia, for body and parts
c
      call azero (ginrt,9)
      call azero (rbcorg,3)
      gmass=0.0
      do 329 mtnbr=1,nmmat
      active=.false.
      call azero (xlinrt,9)
      call azero (rbcorl,3)
      xlmass=0.0
c
c     solid elements
c
      if (numelh.gt.0) then
      lav=1
      l11=1
      do 323 n=1,numelh
      call unpk(mx,ix,ipsh(l11),3)
      if (mx.ne.mtnbr) go to 323
      active=.true.
      mt=mtype(mx)
      ihg=ihgq(mx)
      loc=48*(mx-1)
      nes=ieost(mx)
      rvl=1.0
      if (nes.ne.0) then
           if (nes.eq.1) locv=loc+9
           if (nes.eq.2) locv=loc+7
           if (nes.eq.3) locv=loc+7
           if (nes.eq.4) locv=loc+8
           if (nes.eq.5) locv=loc+34
           if (nes.eq.6) locv=loc+9
           if (nes.eq.7) locv=loc+34
           if (nes.eq.8) locv=loc+43
           if (nes.eq.9) locv=loc+33
           if (nes.eq.11)locv=loc+10
           rvl=eosp(locv)
      endif
      call miprph (x,ro(mx),gmass,xlmass,ginrt,xlinrt,n,rvl,ihg,
     1 rbcorl,rbcorg)
  323 l11=l11+nwcon
      if (active) go to 327
      endif
c
c     shell elements
c
      if (numels.gt.0) then
      call miprps(x,ro(mtnbr),gmass,xlmass,ginrt,xlinrt,mtype,b(lc1s),
     1 b(ns05),mtnbr,b(n1+nmmat),rbcorl,rbcorg,active,numels)
      if (active) go to 327
      endif
c
c     beam elements
c
      if (numelb.gt.0) then
      call miprpb(x,ro(mtnbr),gmass,xlmass,ginrt,xlinrt,mtype,b(lc1b),
     1 b(nb05),mtnbr,b(n1+nmmat),rbcorl,rbcorg,numelb,b(n4f),b(n4g),
     2 mpubr)
      endif
  327 continue
c
      xmc=rbcorl(1)/(xlmass+1.e-20)
      ymc=rbcorl(2)/(xlmass+1.e-20)
      zmc=rbcorl(3)/(xlmass+1.e-20)
c
      xmc2=xmc*xmc
      ymc2=ymc*ymc
      zmc2=zmc*zmc
      xlinrt(1,1)=xlinrt(1,1)-xlmass*(ymc2+zmc2)
      xlinrt(2,2)=xlinrt(2,2)-xlmass*(xmc2+zmc2)
      xlinrt(3,3)=xlinrt(3,3)-xlmass*(xmc2+ymc2)
      xlinrt(1,2)=xlinrt(1,2)+xlmass*xmc*ymc
      xlinrt(1,3)=xlinrt(1,3)+xlmass*xmc*zmc
      xlinrt(2,1)=xlinrt(1,2)
      xlinrt(2,3)=xlinrt(2,3)+xlmass*ymc*zmc
      xlinrt(3,1)=xlinrt(1,3)
      xlinrt(3,2)=xlinrt(2,3)
c
      write (13,700)
     1 mtnbr,xlmass,xmc,ymc,zmc,((xlinrt(i,j),i=1,3),j=1,3)
c
      tim(7+5*nmmat+mtnbr)=xlmass
  329 continue
c
c     add in lumped masses
c
      if (nmmass.ne.0) then
      call azero (xlinrt,9)
      call azero (rbcorl,3)
      xlmass=0.0
      call adlump(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,
     1 b(ld06),b(ld07),b(ld08),x,nmmass)
      xmc=rbcorl(1)/(xlmass+1.e-20)
      ymc=rbcorl(2)/(xlmass+1.e-20)
      zmc=rbcorl(3)/(xlmass+1.e-20)
      xmc2=xmc*xmc
      ymc2=ymc*ymc
      zmc2=zmc*zmc
      xlinrt(1,1)=xlinrt(1,1)-xlmass*(ymc2+zmc2)
      xlinrt(2,2)=xlinrt(2,2)-xlmass*(xmc2+zmc2)
      xlinrt(3,3)=xlinrt(3,3)-xlmass*(xmc2+ymc2)
      xlinrt(1,2)=xlinrt(1,2)+xlmass*xmc*ymc
      xlinrt(1,3)=xlinrt(1,3)+xlmass*xmc*zmc
      xlinrt(2,1)=xlinrt(1,2)
      xlinrt(2,3)=xlinrt(2,3)+xlmass*ymc*zmc
      xlinrt(3,1)=xlinrt(1,3)
      xlinrt(3,2)=xlinrt(2,3)
      write (13,720) xlmass,xmc,ymc,zmc,((xlinrt(i,j),i=1,3),j=1,3)
      endif
c
      xmc=rbcorg(1)/(gmass+1.e-20)
      ymc=rbcorg(2)/(gmass+1.e-20)
      zmc=rbcorg(3)/(gmass+1.e-20)
c
      xmc2=xmc*xmc
      ymc2=ymc*ymc
      zmc2=zmc*zmc
      ginrt(1,1)=ginrt(1,1)-gmass*(ymc2+zmc2)
      ginrt(2,2)=ginrt(2,2)-gmass*(xmc2+zmc2)
      ginrt(3,3)=ginrt(3,3)-gmass*(xmc2+ymc2)
      ginrt(1,2)=ginrt(1,2)+gmass*xmc*ymc
      ginrt(1,3)=ginrt(1,3)+gmass*xmc*zmc
      ginrt(2,1)=ginrt(1,2)
      ginrt(2,3)=ginrt(2,3)+gmass*ymc*zmc
      ginrt(3,1)=ginrt(1,3)
      ginrt(3,2)=ginrt(2,3)
c
      write (13,710) gmass,xmc,ymc,zmc,((ginrt(i,j),i=1,3),j=1,3)
c
      if (ntbsl.eq.0) go to 400
      ipim=ipc
      ipis=ipa
      ipm1=ipd
      ipm2=ipe
      ipal=ipf
      do 330 i=1,ntbsl
      call itbrk1(x,b(ipim),b(ipis),b(ipm1),b(ipm2),b(ipal),
     1            nsntb(i),nmntb(i))
      ipim=ipim+nmntb(i)
      ipis=ipis+nsntb(i)
      ipm1=ipm1+nsntb(i)
      ipm2=ipm2+nsntb(i)
      ipal=ipal+nsntb(i)
  330 continue
c
  400 if (numelt.ne.0) call itsel
c
      xpe=0.0
      xke=0.0
c
c     add discrete lumped masses to mass vector
c
      if (nmmass.ne.0) then
      call addmss(b(ld06),b(ld07),b(ld08),xms,nmmass)
      endif
c
c     kinetic energy
c
      xm=0.0
      ym=0.0
      zm=0.0
      do 520 nn=1,numnp
      nn3=3*nn
      nn2=nn3-1
      nn1=nn2-1
      xm=xm+xms(nn)*v(nn1)
      ym=ym+xms(nn)*v(nn2)
      zm=zm+xms(nn)*v(nn3)
      summss=summss+xms(nn)
      xke=xke+xms(nn)*(v(nn1)*v(nn1)+v(nn2)*v(nn2)+v(nn3)*v(nn3))
  520 continue
      xke=.5*xke
      if (nmsi.eq.0) write ( *,640) xke
c
c     write out total mass and mass by material model
c
      write(13,650) summss
      write(13,660) (n,rbm(n),n=1,nmmat)
c
c     compute form of equation of state if material type=10
c     compute polynomial fit for cold compression energy
c     curve if material type=11
c
      do 530 i=1,nmmat
      j=48*(i-1)
      mt=mtype(i)
      nes=ieost(i)
      if (mt.eq.11) call pecfit (cm(1,i),eosp(1+j),cm(23,i),ro(i),nes)
  530 continue
c
c***  alter masses for shell-solid interfaces
      if (nblk.ne.0) call sbim(b(ijne),nblk,x,xms,b)
c
      do 560 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 560
      lrbn=nrba(n)
      if (rbm(n+3*nmmat).eq.0.0) then
      if (rbm(n).ne.0.0) then
      call sumupi (x,xms,rbm(n),rbi(1,1,n),rbcor(1,n),nrbn(n),
     1 nrb(lrbn),xrb,yrb,zrb,axrb,ayrb,azrb,rbms)
      call inrta (rbm(n),rbi(1,1,n),rbcor(1,n))
      call sumupm(rbv(1,n),rbcor(1,n),x,v,xms,nrbn(n),nrb(lrbn),xrb,
     1 yrb,zrb,axrb,ayrb,azrb,rbms)
      call inirbv (rbi(1,1,n),rbm(n),rbv(1,n),rbcods(1,n),bcs)
      endif
      else
      rbm(n)=rbm(n+3*nmmat)
      call blkcpy(rbcor(1,n+nmmat),rbcor(1,n),3)
      call blkcpy( rbi(1,1,n+nmmat), rbi(1,1,n),9)
      call blkcpy(   rbv(1,n+nmmat),   rbv(1,n),6)
      endif
      call setrbv(rbv(1,n),rbcor(1,n),x,v,nrbn(n),nrb(lrbn),xrb,yrb,
     1 zrb,axrb,ayrb,azrb)
  560 continue
c
c     check for free nodal points
c
      do 570 nn=1,numnp
      if (xms(nn).ne.0.0) go to 565
      xms(nn)=1.e-6
      nn3=3*nn
      v(nn3-2)=0.
      v(nn3-1)=0.
      v(nn3)=0.
  565 xms(nn)=1./xms(nn)
  570 continue
c
      if (numelb+numels.eq.0) go to 590
      do 580 n=1,numnp
      if (xms(n+numnp).eq.0.0) xms(n+numnp)=1.0
  580 xms(n+numnp)=1./xms(n+numnp)
c
  590 call penstf (x,nrb,xyzkcn,rbncod,ipsh,b(lc1s),b(lc1b),cm,mtype,
     1eosp,ieost,numelh,numels,numelb,nmmat,ro,zfcs,b(ns05),
     2b(nb05))
c
      if (njt.ne.0) call cjfac(mtype,mxrb,nrba,nrbn,
     1              b(iidat),njt,b(irdat),nrb,x,rbi,rbm)
c
      if (nmsi.ne.0) then
      call itbrks (b,b(lc11),b(lc9),b(n1),b(n4a),b(ns01),b(ns03),
     1 b(ns05),b(ns06),b(n4f),b(ns07),b(nb03),b(nb05),b(nb06),
     2 b(nt04),b(nt05),b(lc1h),b(lc1s),b(lc1b),b(lc1t))
      endif
      return
c
  640 format(' initial kinetic energy =',e15.6)
  650 format('1'///' t o t a l  m a s s  = ',e14.6//)
  660 format(' material number       mass'/(i9,8x,e11.3))
  670 format(' error(initlz) --- maximum timestep load curve is ',i5/
     1       '      but the last load curve is ',i5///)
  690 format(5x,3e20.0)
  700 format(/,
     $'  m a s s   p r o p e r t i e s   o f   m a t e r i a l #',i3,/
     $'     total mass of material      =',e14.4,/
     $'     x-coordinate of mass center =',e14.4,/
     $'     y-coordinate of mass center =',e14.4,/
     $'     z-coordinate of mass center =',e14.4,//
     $'     inertia tensor of material',/,
     $'     row1=',3e15.4,/,
     $'     row2=',3e15.4,/,
     $'     row3=',3e15.4,/// )
  710 format(/,
     $'  m a s s   p r o p e r t i e s   o f   b o d y',/,
     $'     total mass of body          =',e14.4,/
     $'     x-coordinate of mass center =',e14.4,/
     $'     y-coordinate of mass center =',e14.4,/
     $'     z-coordinate of mass center =',e14.4,//
     $'     inertia tensor of body',/,
     $'     row1=',3e15.4,/,
     $'     row2=',3e15.4,/,
     $'     row3=',3e15.4,/// )
  720 format(/,
     $'  m a s s   p r o p e r t i e s   o f  d i s c r e t e   m',
     $' a s s e s',/
     $'     total mass of lumped masses =',e14.4,/
     $'     x-coordinate of mass center =',e14.4,/
     $'     y-coordinate of mass center =',e14.4,/
     $'     z-coordinate of mass center =',e14.4,//
     $'     inertia tensor of lumped masses',/,
     $'     row1=',3e15.4,/,
     $'     row2=',3e15.4,/,
     $'     row3=',3e15.4,/// )
 9011 format(/5x,'Initializing stresses for',i8, ' solids')
 1000 format(//5x,'*** error in slide surface definition ',i5,
     1        /5x,'           number of master nodes:  ',i8,
     2        /5x,'           number of  slave nodes:  ',i8,
     3        /5x,' *cannot have zero master or slave nodes*')
 1030 format(//5x,'Error - Nike3d stress initialization file ',
     1        /5x,'Number of solid elements found:  ',i8,
     2        /5x,'Number of solid elements expcted:',i8)
      end
      subroutine addmss(nodm,xmass,radius,xms,nmmass)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nodm(1),xmass(1),radius(1),xms(1)
      do 10 i=1,nmmass
      node=nodm(i)
      xms(node)=xms(node)+xmass(i)
   10 continue
      return
      end
      subroutine itbrks(b,x,v,mtype,ieost,strain,yhtns,fibls,
     1 auxvcs,csprop,xies,yhtnb,fiblb,auxvcb,auxvct,thks,ipsh,
     2 ipss,ipsb,ipst)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialization of stresses from restart file
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/my,iy(27)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/nwixa/nwcon
      common/strhx/nmsi,matsi(3,400),mx,ix(8),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/strsh/mxs,ixs(5),xs(6,4),vs(6,4),yhat(12),fibs(9),
     1 strns(12),xiess,hisv(1000)
      common/strbm/mxb,ixb(3),xb(6,2),vb(6,2),yhtb(12),fibb(5),
     1 hisvbm(200)
      common/strts/mxt,ixt(8),xt(3,8),vt(3,8),thk(5),hisvts(120)
      common/iobuf/ioerr,iob2(1080,4),iob8(1080),iob9(1080,2)           vaxcs
      common/shlopt/istrn,istupd,ibelyt,miter
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/double/iprec,ncpw,unit
      dimension b(*),x(3,*),v(3,*),mtype(*),ieost(*),strain(12,*),
     1 yhtns(12,*),fibls(9,*),auxvcs(*),csprop(24,*),xies(*),
     2 fiblb(5,*),auxvcb(*),ni2a(6),auxvct(*),thks(5,*),ipsh(*),ipss(*),
     3 ipsb(*),ipst(*),yhtnb(12,*)
      data ni2a/1,4,9,9,16,25/
c
      if (numelh.ne.0) then
      lavold=lc15
      do 20 nn=1,numelh
      l11=1+nwcon*(nn-1)
      call unpk(my,iy,ipsh(l11),3)
      if (my.eq.0) go to 20
      ms=matsi(1,my)
      mtyp=mtype(my)
      nes =ieost(my)
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) then
      ncon=0
      endif
      ncon=9+iprec*48+ncon*iprec
      if (ms.ne.0) then
      call rdabsf(iob8,mx,ncon,matsi(2,ms),ioerr)
      call riosta (iob8)
      matsi(2,mx)=matsi(2,mx)+ncon
c     if (ncon.ne.57) then
      if (ncon.ne.(9+iprec*48)) then
c     call blkcpy (hisvar,b(lavold),ncon-57)
      call blkcpy (hisvar,b(lavold),ncon-(9+iprec*48))
      endif
      do 10 i=1,8
      v(1,iy(i))=ve(1,i)
      v(2,iy(i))=ve(2,i)
      v(3,iy(i))=ve(3,i)
      x(1,iy(i))=xe(1,i)
      x(2,iy(i))=xe(2,i)
      x(3,iy(i))=xe(3,i)
   10 continue
      endif
      lavold=lavold+ncon-(9+iprec*48)
   20 continue
      endif
      if (numels.ne.0) then
      lavold=1
      do 60 nn=1,numels
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+5*(nn-1)                                                    vax75
      call unpk(my,iy,ipss(l11),2)
      if (my.eq.0) go to 60
      ms=matsi(1,my)
      mtyp=mtype(my)
      nes =ieost(my)
      nip =csprop(2,my)
      ncon=nip*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      endif
      ncon=7+iprec*81+iprec*ncon
      if (ms.ne.0) then
      call rdabsf(iob8,mxs,ncon,matsi(2,ms),ioerr)
      call riosta (iob8)
      matsi(2,mxs)=matsi(2,mxs)+ncon
c     if (ncon.ne.88) then
      if (ncon.ne.(7+81*iprec)) then
c     call blkcpy (hisv,auxvcs(lavold),ncon-88)
      call blkcpy (hisv,auxvcs(lavold),ncon-(7+81*iprec))
      if (istrn.ne.0)
     1call blkcpy(strns,strain(1,nn),12)
      call blkcpy(yhat,yhtns(1,nn),12)
      call blkcpy(fibs,fibls(1,nn),9)
      xies(nn)=xiess
      endif
      do 50 i=1,4
      v(1,ixs(i))      =vs(1,i)
      v(2,ixs(i))      =vs(2,i)
      v(3,ixs(i))      =vs(3,i)
      v(1,ixs(i)+numnp)=vs(4,i)
      v(2,ixs(i)+numnp)=vs(5,i)
      v(3,ixs(i)+numnp)=vs(6,i)
      x(1,ixs(i))      =xs(1,i)
      x(2,ixs(i))      =xs(2,i)
      x(3,ixs(i))      =xs(3,i)
      x(1,ixs(i)+numnp)=xs(4,i)
      x(2,ixs(i)+numnp)=xs(5,i)
      x(3,ixs(i)+numnp)=xs(6,i)
   50 continue
      endif
      lavold=lavold+ncon-(7+81*iprec)
   60 continue
      endif
      if (numelb.ne.0) then
      lavold=1
      do 80 nn=1,numelb
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+4*(nn-1)                                                    vax75
      call unpk(mxb,ixb,ipsb(l11),2)
      if (mxb.eq.0) go to 80
      ms=matsi(1,mxb)
      mtyp=mtype(mxb)
      nes =ieost(mxb)
      nip =csprop(2,mxb)
      if (csprop(4,mxb).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      ncon=npt*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      endif
      ncon=4+iprec*41+ncon*iprec
      if (ms.ne.0) then
      call rdabsf(iob8,mxb,ncon,matsi(2,ms),ioerr)
      call riosta (iob8)
      matsi(2,mxb)=matsi(2,mxb)+ncon
      if (ncon.ne.(4+41*iprec)) then
      call blkcpy (hisvbm,auxvcb(lavold),ncon-(4+41*iprec))
      call blkcpy(yhtb,yhtnb(1,nn),12)
      call blkcpy(fibb,fiblb(1,nn),5)
      endif
      do 70 i=1,2
      v(1,ixb(i))      =vb(1,i)
      v(2,ixb(i))      =vb(2,i)
      v(3,ixb(i))      =vb(3,i)
      v(1,ixb(i)+numnp)=vb(4,i)
      v(2,ixb(i)+numnp)=vb(5,i)
      v(3,ixb(i)+numnp)=vb(6,i)
      x(1,ixb(i))      =xb(1,i)
      x(2,ixb(i))      =xb(2,i)
      x(3,ixb(i))      =xb(3,i)
      x(1,ixb(i)+numnp)=xb(4,i)
      x(2,ixb(i)+numnp)=xb(5,i)
      x(3,ixb(i)+numnp)=xb(6,i)
   70 continue
      endif
      lavold=lavold+ncon-(4+41*iprec)
   80 continue
      endif
      if (numelt.ne.0) then
      lavold=1
      do 100 nn=1,numelt
      l11=1+nwcon*(nn-1)
      call unpk(my,iy,ipst(l11),3)
      if (my.eq.0) go to 100
      ms=matsi(1,my)
      mtyp=mtype(my)
      nes =ieost(my)
      nip =nint(csprop(2,my))
      ncon=nip*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      endif
      ncon=9+53*iprec+iprec*ncon
      if (ms.ne.0) then
      call rdabsf(iob8,mxt,ncon,matsi(2,ms),ioerr)
      call riosta (iob8)
      matsi(2,mxt)=matsi(2,mxt)+ncon
      if (ncon.ne.(9+53*iprec)) then
      call blkcpy (hisvts,auxvct(lavold),ncon-(9+53*iprec))
      endif
      call blkcpy (thk,thks(1,nn),5)
      do 90 i=1,8
      v(1,iy(i))=vt(1,i)
      v(2,iy(i))=vt(2,i)
      v(3,iy(i))=vt(3,i)
      x(1,iy(i))=xt(1,i)
      x(2,iy(i))=xt(2,i)
      x(3,iy(i))=xt(3,i)
   90 continue
      endif
      lavold=lavold+ncon-(9+53*iprec)
  100 continue
      return
      endif
      end
      subroutine cjfac(mtype,mxrb,nrba,nrbn,idat,kdat,rdat,
     1                 nrb,x,rbi,rbm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/bk81/fold,scp
      common/double/iprec,ncpw,unit
c
      dimension mtype(1),mxrb(1),nrba(1),idat(kdat,1),rdat(1),
     1          nrb(1),x(3,1),rbi(3,3,1),rbm(1),nrbn(1)
c
      data o3,root3/3.3333333333333e-01,1.732050807568877e+00/
      data zero /0.0/
c
c
      omeg2=0.0
      do 1000 m=1,nmmat
      if (mtype(m).ne.20.or.mxrb(m).ne.m) go to 1000
      stifft=0.
      stiffr=0.
      ipnt=nrba(m)-1
      n=nrbn(m)
c
      do 200 j=1,njt
      do 180 l=2,7
      if (idat(j,l).eq.0) go to 180
c
      do 150 i=1,n
      if (nrb(i+ipnt).eq.idat(j,l)) go to 170
  150 continue
      go to 180
c
  170 i=idat(j,l)
      stifft=stifft+rdat(j)
      stiffr=stiffr+rdat(j)*(x(1,i)**2+x(2,i)**2+x(3,i)**2)
c
  180 continue
  200 continue
c
      s44=rbi(1,2,m)**2
      s55=rbi(2,3,m)**2
      s66=rbi(1,3,m)**2
      s56=rbi(2,3,m)*rbi(1,3,m)
      s45=rbi(1,2,m)*rbi(2,3,m)
      c11=rbi(2,2,m)*rbi(3,3,m)-s55
      ui1=o3*(rbi(1,1,m)+rbi(2,2,m)+rbi(3,3,m))
      ui2=c11+rbi(1,1,m)*rbi(2,2,m)+rbi(3,3,m)*rbi(1,1,m)
     1   -s44-s66
      ui3=rbi(1,1,m)*c11+rbi(1,2,m)*(s56-rbi(1,2,m)*rbi(3,3,m))
     1   +rbi(1,3,m)*(s45-rbi(2,2,m)*rbi(1,3,m))
      ui1s=ui1**2
      q=sqrt(-min(o3*ui2-ui1s,zero))
      rp=.50*(ui3-ui1*ui2)+ui1*ui1s
      xmod=q**3
      scl1=.50+sign(.5*unit,xmod-1.0e-30)
      scl2=.50+sign(.5*unit,xmod-abs(rp))
      scl0=  min(scl1,scl2)
      scl1=1.0-scl0
      sdetm=acos(rp/(xmod+scl1))*o3
      q=scl0*q
      ct3=q*cos(sdetm)
      st3=q*root3*sin(sdetm)
      sdetm=scl1*sqrt(  max(zero,rp))
      el1=2.0*(ct3+sdetm)+ui1
      el2=-ct3+st3-sdetm+ui1
      el3=-ct3-st3-sdetm+ui1
      el=  min(el1,el2,el3)
c
c     translational stiffness doubled to account for effective
c     stiffness in symmetric modes
      omeg2=  max(omeg2,2.0*stifft/rbm(m),stiffr/el)
c
 1000 continue
c
      fold=1.
      scp=.04/omeg2
c
      return
      end
      subroutine ldcsti(fval,npc,p,tt,ilcf)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute current value of load curves
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      dimension fval(1),npc(1),p(1),ilcf(1)
c
      if (nlcur.eq.0) return
c
      do 10 n=1,nlcur
      ierr=0
      xmag=1.0
      loc=npc(n)
      npoint=(npc(n+1)-loc)/2
      ierr=0
      f=0.
      call interp (p(loc),tt,npoint,f,xmag,ierr)
      if (ilcf(n).eq.idrflg.or.ilcf(n).eq.2) fval(n)=f
      if (ierr.eq.1) fval(n)=0.0
   10 continue
c
      return
c
      end
      subroutine tempri(told,temp1,tnew,temp2,numelh,numnp,
     1                  tmode,tbase,lct,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and interpolate (if necessary) temperature states
c     taco3d database assumed
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     & iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),         vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      dimension temp1(1),temp2(1),tmode(1),tbase(1),lct(1),fval(1)
c
c     interpolate
c
      if (itemp.eq.-9999) go to 20
      if (itemp.ne.-2) then
      itherm=64+3*numnp+9*numelh
      call rdabsg (iob11,tnew,numnp+1,itherm,1)
      call riosta (iob11)
      itherm=itherm+numnp+1
      do 10 j=1,numnp
   10 temp1(j)=temp2(j)
      told=tnew
      return
      else
      do 15 j=1,numnp
      temp2(j)=0.
   15 temp1(j)=temp2(j)
      told=tnew
      return
      endif
c
   20 do 50 j=1,numnp
      temp1(j)=tmode(j)*fval(lct(j))+tbase(j)
      temp2(j)=temp1(j)
   50 continue
      told=tnew
      return
      end
      subroutine setvli(timo,dispo,timn,dispn,vel,neq)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      dimension dispo(1),dispn(1),vel(1)
c
      n=neq+1
      call rdabsf(njoydf,timo,n,nwdadd,ioerr)
      nwdadd=nwdadd+n
      call rdabsf(njoydf,timn,n,nwdadd,ioerr)
      nwdadd=nwdadd+n
      call riosta (njoydf)
      dt=timn-timo
      do 30 i=1,neq
   30 vel(i)=(dispn(i)-dispo(i))/dt
      return
c
   40 format(45x,e20.0)
   50 format(5x,3e20.0)
      end
      subroutine hipac1(nc,auxvec)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/aux(100)
      dimension auxvec(1)
      do 10 i=1,nc
   10 aux(i)=auxvec(i)
      do 20 i=1,7
   20 aux(i)=0.
      return
      end
      subroutine hipac2(nc,auxvec,lav)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/auxstr/aux(100)
      dimension auxvec(1)
      do 10 i=1,nc
   10 auxvec(i)=aux(i)
      lav=lav+nc
      return
   20 format(5x,4e16.0)
      end
      subroutine hexint(iel,nhex)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... routine to read nike3d stress initialization data for solids
c
      common/auxstr/aux(100)
      common/rwint/intiad,ncint1,nhint1,nsint1
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      dimension nhex(*)
c
      intloc=intiad+(nhex(iel)-1)*7
c
      call rdabsg(iob7,aux,7,intloc,1)
      call riosta(iob7)
      return
      end
      subroutine inse2(x,cm,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize elastic-orthotropic material
c
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(27)
      common/auxstr/sig(6),px(4),py(4),pz(4),xl1,xm1,xn1,xl2,xm2,xn2,
     1 tref,otmp,ctmp
      dimension x(3,*),cm(*),tnew(*),fval(*)
c
      sig(1)=0.
      sig(2)=0.
      sig(3)=0.
      sig(4)=0.
      sig(5)=0.
      sig(6)=0.
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
   10 xx3(i)=x(3,ix(i))
c
      call prtl (px,py,pz)
c
      if (cm(1).eq.1.) go to 30
      if (cm(1).eq.2.) go to 50
      k1=cm(4)
      k2=cm(5)
      k3=cm(6)
      if (k1.eq.0) k1=1
      if (k2.eq.0) k2=2
      if (k3.eq.0) k3=3
c
      xl1=xx1(k2)-xx1(k1)
      xm1=xx2(k2)-xx2(k1)
      xn1=xx3(k2)-xx3(k1)
      xl2=xx1(k3)-xx1(k1)
      xm2=xx2(k3)-xx2(k1)
      xn2=xx3(k3)-xx3(k1)
c
   20 xl3=xm1*xn2-xm2*xn1
      xm3=xn1*xl2-xn2*xl1
      xn3=xl1*xm2-xl2*xm1
c
      xmag=sqrt(xl1**2+xm1**2+xn1**2)
      zmag=sqrt(xl3**2+xm3**2+xn3**2)
c
      xl1=xl1/xmag
      xm1=xm1/xmag
      xn1=xn1/xmag
      xl3=xl3/zmag
      xm3=xm3/zmag
      xn3=xn3/zmag
c
      xl2=xm3*xn1-xm1*xn3
      xm2=xn3*xl1-xn1*xl3
      xn2=xl3*xm1-xl1*xm3
      if (itemp.eq.0) then
      return
      elseif (itemp.gt.0) then
      ctmp=fval(itemp)
      elseif (itemp.lt.0) then
      ctmp=.125*(tnew(ix(1))+tnew(ix(2))+tnew(ix(3))+tnew(ix(4))+
     1           tnew(ix(5))+tnew(ix(6))+tnew(ix(7))+tnew(ix(8)))
      endif
      tref=ctmp
      otmp=ctmp
      return
   30 xc=0.
      yc=0.
      zc=0.
      do 40 i=1,8
      xc=xc+.125*xx1(i)
      yc=yc+.125*xx2(i)
   40 zc=zc+.125*xx3(i)
      xl1=xc-cm(4)
      xm1=yc-cm(5)
      xn1=zc-cm(6)
      xl2=0.
      xm2=0.
      xn2=1.
      go to 20
   50 xl1=cm(4)
      xm1=cm(5)
      xn1=cm(6)
      xl2=cm(7)
      xm2=cm(8)
      xn2=cm(9)
      go to 20
      end
      subroutine inse3(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/aux(100)
      dimension cm(1)
      do 10 i=1,20
   10 aux(i)=0.
      return
      end
      subroutine inse4(cm,tnew,fval)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk14/mx,ix(27)
      common/auxstr/sig(7),aux(93)
      dimension tnew(1),fval(1)
      dimension cm(1)
      if (itemp.lt.0) go to 10
      ctmp=fval(itemp)
      go to 20
   10 ctmp=.125*(tnew(ix(1))+tnew(ix(2))+tnew(ix(3))+tnew(ix(4))+
     1           tnew(ix(5))+tnew(ix(6))+tnew(ix(7))+tnew(ix(8)))
   20 aux(1)=ctmp
      aux(2)=ctmp
      aux(3)=ctmp
      return
      end
      subroutine inse6(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/aux(100)
      dimension cm(1)
      do 10 i=1,100
   10 aux(i)=0.
      return
      end
      subroutine inse7(x,ihg)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(27)
      common/auxstr/sig(6),px(4),py(4),pz(4)
      dimension x(3,1)
c
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
   10 xx3(i)=x(3,ix(i))
c
      if (ihg.ne.3.and.ihg.lt.5) then
      call prtl (px,py,pz)
      else
      call prtlf(px,py,pz)
      endif
c
      return
      end
      subroutine inse8(rvl,cm,eosp,vol,x,burn,ix,ndtpts,mx,nlt)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(6),tb,aux(93)
      common /hyghen/ xcent,ycent,dtpntx,dtpnty,alt,detvel
      dimension cm(*),x(3,1),burn(5,1),ix(*)
      data mprint/0/
      tb=1000000000.
      xc=0.
      yc=0.
      zc=0.
      do 10 i=1,8
      xc=xc+.125*x(1,ix(i))
      yc=yc+.125*x(2,ix(i))
   10 zc=zc+.125*x(3,ix(i))
      if (ndtpts.eq.0) go to 30
      if (cm(3).eq.1.0) go to 30
      do 20 i=1,ndtpts
      mtl=burn(2,i)
      if (mtl.ne.mx.and.mtl.ne.0) go to 20
      alt=burn(1,i)
      xlp=burn(3,i)
      ylp=burn(4,i)
      zlp=burn(5,i)
      detvel   = cm(30)
      dl=sqrt((xc-xlp)**2+(yc-ylp)**2+(zc-zlp)**2)
      bt=alt+dl/detvel
      if (bt.lt.abs(tb)) tb=-bt
   20 continue
      go to 40
   30 tb=0.
   40 aux(1)=eosp*vol
      aux(2)=0.
      aux(3)=rvl*vol
      mprint=mprint+1
      if (mprint.eq.1) write(13,50)
      ttt=abs(tb)
      write(13,60) nlt,ttt
      if (mprint.eq.48) mprint=0
      return
c
   50 format('1 d e t o n a t i o n   t i m e s   f o r   h. e.  e l e',
     1' m e n t s'/)
   60 format(5x,'element',i6,' is lit at time',e15.5)
      end
      subroutine inse10(rvl,eosp,vol,pc)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      aux(1)=eosp*vol
      aux(2)=0.
      aux(3)=rvl*vol
      aux(4)=pc
      aux(5)=0.0
      return
      end
      subroutine inse11 (rvl,e0,vol,pc,gamo)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      aux(1)=e0*vol
      aux(2)=0.
      aux(3)=rvl*vol
      aux(4)=pc
      sig(7)=gamo
      return
      end
      subroutine inse13(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(*)
      aux(1)=cm(9)
      return
      end
      subroutine inse14(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(*)
      aux(1)=cm(6)
      return
      end
      subroutine inse15(rvl,eosp,vol,pc)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      aux(1)=eosp*vol
      aux(2)=0.
      aux(3)=rvl*vol
      aux(4)=pc
      aux(5)=0.0
      aux(6)=0.0
      return
      end
      subroutine inse17(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(1)
      do 10 i=1,10
      aux(i)=0.
   10 continue
      aux(4)=-2.
      return
      end
      subroutine inse19(cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(1)
      aux(1)=1.
      aux(2)=cm(1)
      return
      end
      subroutine inse22(x,cm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize elastic-orthotropic material
c
      common/bk11/ux(20),uy(20),uz(20),
     1 x1,x2,x3,x4,x5,x6,x7,x8,xxs1(12),
     1 y1,y2,y3,y4,y5,y6,y7,y8,yys1(12),
     1 z1,z2,z3,z4,z5,z6,z7,z8,zzs1(12)
      common/bk14/mx,ix(27)
      common/auxstr/sig(7),aux(8),q11,q12,q13,q31,q32,q33
      dimension x(3,1),cm(*)
      dimension xx1(20),xx2(20),xx3(20)
      equivalence (x1,xx1),(y1,xx2),(z1,xx3)
c
      sig(1)=0.
      sig(2)=0.
      sig(3)=0.
      sig(4)=0.
      sig(5)=0.
      sig(6)=0.
c
c     gather element coordinates
c
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
   10 xx3(i)=x(3,ix(i))
c
c     initialize history variables
c
      do 20 i=1,14
   20 aux(i)=0.0
      aux(1)=1.
      aux(2)=1.
      aux(3)=1.
      aux(4)=1.
      aux(5)=1.
      aux(6)=1.
      aux(7)=1.
      aux(8)=1.
c
c      compute local element system
c
      x21=x2-x1
      y21=y2-y1
      z21=z2-z1
      x31=x3-x1
      y31=y3-y1
      z31=z3-z1
      x41=x4-x1
      y41=y4-y1
      z41=z4-z1
      x42=x4-x2
      y42=y4-y2
      z42=z4-z2
      c1=y31*z42-z31*y42
      c2=z31*x42-x31*z42
      c3=x31*y42-y31*x42
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s31=c1*xl
      s32=c2*xl
      s33=c3*xl
      xl=x21*s31+y21*s32+z21*s33
      c1=x21-s31*xl
      c2=y21-s32*xl
      c3=z21-s33*xl
      xl=1./sqrt(c1*c1+c2*c2+c3*c3)
      s11=c1*xl
      s12=c2*xl
      s13=c3*xl
      s21=s32*s13-s33*s12
      s22=s33*s11-s31*s13
      s23=s31*s12-s32*s11
c
      if (cm(1).eq.0.0) then
c
c     material axis along 1-2 side
c
      r11=s11
      r12=s12
      r13=s13
      r21=s21
      r22=s22
      r23=s23
c
      elseif (cm(1).eq.1.0) then
c
c     material axis based on direction of line thru fixed point and
c     element center
c
      xc=0.
      yc=0.
      zc=0.
      do 40 i=1,8
      xc=xc+.125*xx1(i)
      yc=yc+.125*xx2(i)
   40 zc=zc+.125*xx3(i)
      r11=xc-cm(4)
      r12=yc-cm(5)
      r13=zc-cm(6)
      r21=0.
      r22=0.
      r23=1.
      elseif (cm(1).eq.2.0) then
c
c     globally defined material axes
c
      r11=cm(4)
      r12=cm(5)
      r13=cm(6)
      r21=cm(7)
      r22=cm(8)
      r23=cm(9)
      endif
      r31=r12*r23-r22*r13
      r32=r13*r21-r23*r11
      r33=r11*r22-r21*r12
c
      xmag=sqrt(r11**2+r12**2+r13**2)
      zmag=sqrt(r31**2+r32**2+r33**2)
c
      r11=r11/xmag
      r12=r12/xmag
      r13=r13/xmag
      r31=r31/zmag
      r32=r32/zmag
      r33=r33/zmag
      r21=r32*r13-r12*r33
      r22=r33*r11-r13*r31
      r23=r31*r12-r11*r32
c
      if (cm(2).eq.2.0) then
      r1s=r21
      r2s=r22
      r3s=r23
      r21=r11
      r22=r12
      r23=r13
      r11=-r1s
      r12=-r2s
      r13=-r3s
      elseif (cm(2).eq.3.0) then
      r1s=r31
      r2s=r32
      r3s=r33
      r31=r11
      r32=r12
      r33=r13
      r11=-r1s
      r12=-r2s
      r13=-r3s
      endif
      q11=s11*r11+s12*r12+s13*r13
      q12=s21*r11+s22*r12+s23*r13
      q13=s31*r11+s32*r12+s33*r13
      q21=s11*r21+s12*r22+s13*r23
      q22=s21*r21+s22*r22+s23*r23
      q23=s31*r21+s32*r22+s33*r23
      q31=s11*r31+s12*r32+s13*r33
      q32=s21*r31+s22*r32+s23*r33
      q33=s31*r31+s32*r32+s33*r33
c
      return
      end
      subroutine inse25 (cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(1)
      capint=cm(15)
      do 10 i=1,7
      sig(i)=0.0
   10 continue
      aux(1)=capint
      do 20 i=2,6
      aux(i)=0.0
   20 continue
      return
      end
      subroutine inse26 (cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/auxstr/sig(7),aux(93)
      dimension cm(1)
      sig(7)=1.0
      return
      end
      subroutine intes7 (cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/auxstr/sig(7),aux(3),tmp,dedv,cv,fc,beta,eta1,eta2,csq,qq
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      dimension cm(1),cme(30)
      equivalence (a,cme)
      do 10 i=1,30
   10 cme(i)=cm(i)
      tmp=temsc(8)
      cv=cvr
      eta1=.999999
      etac=1.000
c
      call rctes
c
      beta=1.
      eta2=1.e-06
      cv=cvract
c
      call prodes
c
      dedv=dedv1
      heat=enq-en2+en1
      dpdfc=dpdt1*heat/cv
      dpde=dpdt1/cv
      dpdmu=bth1+dpdt1*dedv/(cv*etac*etac)
      csq=dpdmu
      qq=1.0
      return
      end
      subroutine prodes
c     implicit double precision (a-h,o-z)                                    dp
c
c     product eos
c     p=a*exp(-xp1/eta)+b*exp(-xp2/eta)+g*eta*tmp
c     cvp is product heat capacity
c
      common/auxstr/sig(7),aux(3),tmp,dedv,cv,fc,beta,eta1,eta2,csq,qq
      common/bk61/dedv2,pprod,cvprod,bth2,dpdt2,en2
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
c
      trans1=a*exp(-xp1/eta2)
      trans2=b*exp(-xp2/eta2)
      dedv2=-trans1-trans2
      pprod=-dedv2+g*eta2*tmp
      cvprod=cvp
      bth2=g*tmp+(xp1*trans1+xp2*trans2)/eta2**2
      dpdt2=g*eta2
      en2=trans1/xp1+trans2/xp2+cvprod*tmp
      return
      end
      subroutine rctes
c     implicit double precision (a-h,o-z)                                    dp
c
c     reactant eos
c     p=r1*exp(-r5/eta)+r2*exp(-r6/eta)+r3*eta*tmp
c     cvr is reactant heat capacity
c
      common/auxstr/sig(7),aux(3),tmp,dedv,cv,fc,beta,eta1,eta2,csq,qq
      common/bk62/dedv1,preact,cvract,bth1,dpdt1,en1
      common/bk55/a,b,xp1,xp2,frer,g,r1,r2,r3,r5,r6,fcrit,freq,grow,em,
     1 ar,es,cvp,cvr,eeta2,qcrit,enq,temsc(8)
      trans1=r1*exp(-r5/eta1)
      trans2=r2*exp(-r6/eta1)
      dedv1=-trans1-trans2
      preact=-dedv1+r3*eta1*tmp
      bth1=r3*tmp+(r5*trans1+r6*trans2)/eta1**2
      dpdt1=r3*eta1
      en1=trans1/r5+trans2/r6+cvract*tmp
      return
      end
      subroutine gravs(vol,x,s,p,an,rhodp,mt,nes)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk14/mx,ix(27)
      common/auxstr/sig(7),aux(93)
      dimension x(3,*),s(*),p(*),an(*),rhodp(2,*)
c
      zc=0.
      do 10 i=1,8
   10 zc=zc+.125*x(idirgv,ix(i))
      press=0.0
      do 30 i=2,numdp
      if (zc.gt.rhodp(2,i)) go to 20
      press=press-.5*(rhodp(2,i)-rhodp(2,i-1))*(rhodp(1,i)+rhodp(1,i-1))
     1 *grvity
      go to 30
   20 dz=rhodp(2,i-1)-zc
      d1=rhodp(2,i-1)-rhodp(2,i)
      d2=rhodp(1,i)-rhodp(1,i-1)
      press=press+.5*dz*(2.*rhodp(1,i-1)+dz*d2/d1)*grvity
      go to 40
   30 continue
   40 continue
      if (mt.eq.1 .or. mt.eq.3 .or. mt.eq.12 .or. mt.eq.13) then
           vols=-3.*press/(an(2)+2.*an(1))
      elseif (mt.eq.5 .or. mt.eq. 14 .or. nes.eq.8 .or. nes.eq.9) then
           n=an(1)
           if (press.lt.p(1).or.press.gt.p(n)) then
                write(13,100) mt,nes
                call adios(2)
           endif
           do 50 mm=2,n
           m=mm
           if (press-p(m).le.0.0) go to 60
   50      continue
   60      dt=press-p(m-1)
           d1=p(m)-p(m-1)
           d2=s(m)-s(m-1)
           vols=s(m-1)+dt*d2/d1
      else
      write(*,110) mt
      write(13,110) mt
  110 format(///5x,'gravity initialization not available'
     1 ' for material type: ',i5,///)
      call adios(2)
           return
      endif
      vol=vol*exp(-vols)
      sig(1)=-press
      sig(2)=-press
      sig(3)=-press
      return
c
  100 format(' fatal error in gravity initialization subroutine - -'/
     1 ' pressure due to gravity is out of range (mat. model',i3,' eos'
     2 ,i3,') ')
      end
      subroutine prtlb(x)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk10/px(20),py(20),pz(20),vx(20),vy(20),vz(20)
      common/bk11/ux(20),uy(20),uz(20),xx1(8),xo1(12),xx2(8),xo2(12),
     1            xx3(8),xo3(12)
      common/bk14/mx,ix(27)
c
      dimension x(3,1)
c
      do 10 i=1,8
      j=ix(i)
      xx1(i)=x(1,j)
      xx2(i)=x(2,j)
   10 xx3(i)=x(3,j)
c
      call prtl (px,py,pz)
c
      return
      end
      subroutine prtl (px,py,pz)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),
     1 x1,x2,x3,x4,x5,x6,x7,x8,xx1(12),
     2 y1,y2,y3,y4,y5,y6,y7,y8,xx2(12),
     3 z1,z2,z3,z4,z5,z6,z7,z8,xx3(12)
      dimension px(*),py(*),pz(*)
      x17=x7-x1
      x28=x8-x2
      x35=x5-x3
      x46=x6-x4
      aj1=x17+x28-x35-x46
      x17=x17+x46
      x28=x28+x35
      aj4=x17+x28
      aj7=x17-x28
      y17=y7-y1
      y28=y8-y2
      y35=y5-y3
      y46=y6-y4
      aj2=y17+y28-y35-y46
      y17=y17+y46
      y28=y28+y35
      aj5=y17+y28
      aj8=y17-y28
      z17=z7-z1
      z28=z8-z2
      z35=z5-z3
      z46=z6-z4
      aj3=z17+z28-z35-z46
      z17=z17+z46
      z28=z28+z35
      aj6=z17+z28
      aj9=z17-z28
c
c     jacobian
c
      aj5968=aj5*aj9-aj6*aj8
      aj6749=aj6*aj7-aj4*aj9
      aj4857=aj4*aj8-aj5*aj7
      det=aj1*aj5968+aj2*aj6749+aj3*aj4857
      det=0.0156250*det
      dett=0.0156250/det
c
c     inverse of the jacobian matrix
c
      aji1=dett*aj5968
      aji4=dett*aj6749
      aji7=dett*aj4857
      aji2=dett*(-aj2*aj9+aj3*aj8)
      aji5=dett*(aj1*aj9-aj3*aj7)
      aji8=dett*(-aj1*aj8+aj2*aj7)
      aji3=dett*(aj2*aj6-aj3*aj5)
      aji6=dett*(-aj1*aj6+aj3*aj4)
      aji9=dett*(aj1*aj5-aj2*aj4)
c
      aj12=aji1-aji2
      aj45=aji4-aji5
      aj78=aji7-aji8
      px(3)=+aj12+aji3
      py(3)=+aj45+aji6
      pz(3)=+aj78+aji9
      px(4)=+aj12-aji3
      py(4)=+aj45-aji6
      pz(4)=+aj78-aji9
      aj12=aji1+aji2
      aj45=aji4+aji5
      aj78=aji7+aji8
      px(1)=-aj12-aji3
      py(1)=-aj45-aji6
      pz(1)=-aj78-aji9
      px(2)=-aj12+aji3
      py(2)=-aj45+aji6
      pz(2)=-aj78+aji9
c
      return
      end
      subroutine prtlf (px,py,pz)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute strain-displacement matrix
c
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),
     1 x1,x2,x3,x4,x5,x6,x7,x8,xx1(12),
     2 y1,y2,y3,y4,y5,y6,y7,y8,xx2(12),
     3 z1,z2,z3,z4,z5,z6,z7,z8,xx3(12)
      dimension px(*),py(*),pz(*)
c
      a45=z4-z5
      a24=z2-z4
      a52=z5-z2
      a16=z1-z6
      a31=z3-z1
      a63=z6-z3
      a27=z2-z7
      a74=z7-z4
      a38=z3-z8
      a81=z8-z1
      a86=z8-z6
      a57=z5-z7
      a6345=a63-a45
      a5238=a52-a38
      a8624=a86-a24
      a7416=a74-a16
      a5731=a57-a31
      a8127=a81-a27
      px(1) =( y2*a6345+y3*a24-y4*a5238+y5*a8624
     &      +y6*a52+y8*a45)/12.
      px(2) =( y3*a7416+y4*a31-y1*a6345+y6*a5731
     &      +y7*a63+y5*a16)/12.
      px(3) =( y4*a8127-y1*a24-y2*a7416-y7*a8624
     &      +y8*a74+y6*a27)/12.
      px(4) =( y1*a5238-y2*a31-y3*a8127-y8*a5731
     &      +y5*a81+y7*a38)/12.
      px(13)=(-y8*a7416+y7*a86+y6*a8127-y1*a8624
     &      -y4*a81-y2*a16)/12.
      px(14)=(-y5*a8127+y8*a57+y7*a5238-y2*a5731
     &      -y1*a52-y3*a27)/12.
      px(15)=(-y6*a5238-y5*a86+y8*a6345+y3*a8624
     &      -y2*a63-y4*a38)/12.
      px(16)=(-y7*a6345-y6*a57+y5*a7416+y4*a5731
     &      -y3*a74-y1*a45)/12.
      a45=x4-x5
      a24=x2-x4
      a52=x5-x2
      a16=x1-x6
      a31=x3-x1
      a63=x6-x3
      a27=x2-x7
      a74=x7-x4
      a38=x3-x8
      a81=x8-x1
      a86=x8-x6
      a57=x5-x7
      a6345=a63-a45
      a5238=a52-a38
      a8624=a86-a24
      a7416=a74-a16
      a5731=a57-a31
      a8127=a81-a27
      py(1) =( z2*a6345+z3*a24-z4*a5238+z5*a8624
     &      +z6*a52+z8*a45)/12.
      py(2) =( z3*a7416+z4*a31-z1*a6345+z6*a5731
     &      +z7*a63+z5*a16)/12.
      py(3) =( z4*a8127-z1*a24-z2*a7416-z7*a8624
     &      +z8*a74+z6*a27)/12.
      py(4) =( z1*a5238-z2*a31-z3*a8127-z8*a5731
     &      +z5*a81+z7*a38)/12.
      py(13)=(-z8*a7416+z7*a86+z6*a8127-z1*a8624
     &      -z4*a81-z2*a16)/12.
      py(14)=(-z5*a8127+z8*a57+z7*a5238-z2*a5731
     &      -z1*a52-z3*a27)/12.
      py(15)=(-z6*a5238-z5*a86+z8*a6345+z3*a8624
     &      -z2*a63-z4*a38)/12.
      py(16)=(-z7*a6345-z6*a57+z5*a7416+z4*a5731
     &      -z3*a74-z1*a45)/12.
      a45=y4-y5
      a24=y2-y4
      a52=y5-y2
      a16=y1-y6
      a31=y3-y1
      a63=y6-y3
      a27=y2-y7
      a74=y7-y4
      a38=y3-y8
      a81=y8-y1
      a86=y8-y6
      a57=y5-y7
      a6345=a63-a45
      a5238=a52-a38
      a8624=a86-a24
      a7416=a74-a16
      a5731=a57-a31
      a8127=a81-a27
      pz(1) =( x2*a6345+x3*a24-x4*a5238+x5*a8624
     &      +x6*a52+x8*a45)/12.
      pz(2) =( x3*a7416+x4*a31-x1*a6345+x6*a5731
     &      +x7*a63+x5*a16)/12.
      pz(3) =( x4*a8127-x1*a24-x2*a7416-x7*a8624
     &      +x8*a74+x6*a27)/12.
      pz(4) =( x1*a5238-x2*a31-x3*a8127-x8*a5731
     &      +x5*a81+x7*a38)/12.
      pz(13)=(-x8*a7416+x7*a86+x6*a8127-x1*a8624
     &      -x4*a81-x2*a16)/12.
      pz(14)=(-x5*a8127+x8*a57+x7*a5238-x2*a5731
     &      -x1*a52-x3*a27)/12.
      pz(15)=(-x6*a5238-x5*a86+x8*a6345+x3*a8624
     &      -x2*a63-x4*a38)/12.
      pz(16)=(-x7*a6345-x6*a57+x5*a7416+x4*a5731
     &      -x3*a74-x1*a45)/12.
c
      vol=px(1) *x1+px(2) *x2+px(3) *x3+px(4) *x4
     &   +px(13)*x5+px(14)*x6+px(15)*x7+px(16)*x8
      do 10 i=1,24
   10 px(i)=px(i)/vol
      return
c
      end
      subroutine forceg(e)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk10/px(20),py(20),pz(20),vx(20),vy(20),vz(20)
      common/bk14/mx,ix(27)
      common/auxstr/sig(7),aux(93)
      dimension e(3,1),e1(4),e2(4),e3(4),ip(4)
      data   ip/7,8,5,6/
c
      sig1=sig(1)*vol
      sig2=sig(2)*vol
      sig3=sig(3)*vol
c
      do 10 i=1,4
      e1(i)=sig1*px(i)
      e2(i)=sig2*py(i)
      e3(i)=sig3*pz(i)
   10 continue
c
      do 20 i=1,4
      j=ip(i)
      l=ix(i)
      m=ix(j)
      e(1,l)=e(1,l)+e1(i)
      e(2,l)=e(2,l)+e2(i)
      e(3,l)=e(3,l)+e3(i)
      e(1,m)=e(1,m)-e1(i)
      e(2,m)=e(2,m)-e2(i)
      e(3,m)=e(3,m)-e3(i)
   20 continue
      return
      end
      subroutine slavi0(x,irect,nseg,lc,nsv,msr,iloc,nmn,nsn,nrt,
     1                  itemp,nty)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize slide-line logic
c
      common/aux2/x1(128),x2(128),x3(128),cms(128),dms(128)
c
      dimension x(3,1),irect(4,1),lc(1),msr(1),nsv(1),iloc(1),
     1          nseg(1),itemp(1)
c
c     initialize slide and void logic
c
      if (nrt.eq.0.and.nty.lt.5) return
      ip=0
      ilen=128
      nelg=nsn/128
      irem=nsn-128*nelg
      if (irem.eq.0) go to 10
      ilen=irem
      nelg=nelg+1
c
   10 do 100 n=1,nelg
c
      do 30 i=1,ilen
      ii=nsv(i+ip)
      x1(i)=x(1,ii)
      x2(i)=x(2,ii)
      x3(i)=x(3,ii)
      cms(i)=1.e20
   30 continue
c
      do 80 jj=1,nmn
      j=msr(jj)
      y1=x(1,j)
      y2=x(2,j)
      y3=x(3,j)
c
      do 40 i=1,ilen
      dms(i)=(x1(i)-y1)**2+(x2(i)-y2)**2+(x3(i)-y3)**2
   40 continue
      do 50 i=1,ilen
      if (cms(i).gt.dms(i)) iloc(i+ip)=jj
   50 continue
      do 60 i=1,ilen
      cms(i)=  min(cms(i),dms(i))
   60 continue
c
   80 continue
c
      ip=ip+ilen
      ilen=128
c
  100 continue
c
      if (nrt.eq.0) return
c
      do 120 i=1,nsn
      itemp(nsv(i))=i
  120 continue
c
      do 170 i=1,nrt
      do 160 j=1,4
      if (irect(j,i).eq.0) go to 160
      il=itemp(irect(j,i))
      ii=0
      locn=nseg(il)
      nsgt=nseg(il+1)-locn
      do 150 l=1,nsgt
      if (lc(locn+l-1).ne.0) ii=ii+1
  150 continue
      lc(locn+ii)=i
  160 continue
  170 continue
c
      do 190 i=1,nsn
      itemp(nsv(i))=0
  190 continue
c
      return
c
      end
      subroutine stiffs(x,irect,stf,bh,ipss,cm,matype,eosp,ieost,numelh,
     1numels,numelt,nrt,nmmat,ro,zf,thicks,ethik,nty,chrlen,ipst,
     1jx1,jx7,nsv,nsn,stfvg,thk,sftr)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),
     1 xn1(8),xc(8),xx1(4),
     2 yn2(8),yc(8),xx2(4),
     3 zn3(8),zc(8),xx3(4)
      common/bk14/my,iy(9),mx,ix(10),xs1,ys1,zs1,sig(3),epx
      common/nwixa/nwcon
      common/shlopt/istrn,istupd,ibelyt,miter
c     dimension irect(4,*),stf(*),bh(*),ipss(2,*),x(3,*),               cray1
      dimension irect(4,*),stf(*),bh(*),ipss(5,*),x(3,*),               vax75
     1 matype(*),cm(*),ethik(*),thk(*),
     1 ipst(*),eosp(*),ieost(*),ro(*),thicks(9,*),bkm(600)
      dimension jx1(*),jx7(*),nsv(*)
      logical ishell
      integer et
      data istyp,llnum/0,0/,iutyp/0/
      bulkmx=0.
      stfvg =0.
      llnum=llnum+1
      if (nrt.eq.0) return
      chrlen=-1.
      do 10 mx=1,nmmat
      bkm(mx)=0.
      mt=matype(mx)
      nes=ieost(mx)
      rho=ro(mx)
      mx48m1=48*(mx-1)
      if (mt.eq.1) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.2) bkm(mx)=cm(mx48m1+11)/(3.*(1.-2.*cm(mx48m1+12)))
      if (mt.eq.3) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.4) bkm(mx)=cm(mx48m1+9)/(3.*(1.-2.*cm(mx48m1+17)))
      if (mt.eq.5) bkm(mx)=cm(mx48m1+2)
      if (mt.eq.6) bkm(mx)=cm(mx48m1+1)
      if (mt.eq.7) bkm(mx)=cm(mx48m1+1)*2.*(1.+cm(mx48m1+2))/
     1             (3.*(1.-2*cm(mx48m1+2)))
      if (mt.eq.12)bkm(mx)=cm(mx48m1+6)
      if (mt.eq.13)bkm(mx)=cm(mx48m1+6)
      if (mt.eq.14)bkm(mx)=cm(mx48m1+2)
      if (mt.eq.18)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.19)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.20)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.21)bkm(mx)=cm(mx48m1+11)/(3.*(1.-2.*cm(mx48m1+12)))
      if (mt.eq.22)bkm(mx)=cm(mx48m1+21)
      if (mt.eq.23)bkm(mx)=cm(mx48m1+21)
      if (mt.eq.24)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.25)bkm(mx)=cm(mx48m1+1)
      if (mt.eq.26)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.27)bkm(mx)=(4./3.)*(cm(mx48m1+1)+cm(mx48m1+2))
     1 *(1.+cm(mx48m1+3))/(1.-2.*cm(mx48m1+3))
      if (mt.eq.28)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.30)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.31)bkm(mx)=2.*(cm(mx48m1+7)+cm(mx48m1+2)/3.)
      if (mt.eq.32)bkm(mx)=cm(mx48m1+21)
      if (mt.eq.34) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.35) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+9)))
      if((mt.gt.7.and.mt.lt.12).or.(mt.gt.14.and.mt.lt.18)) then
           call speos (bkm(mx),mx48m1,eosp,rho,nes)
      endif
   10 bulkmx=  max(bulkmx,bkm(mx))
      do 20 mx=1,nmmat
   20 if (bkm(mx).le.1.e-09) bkm(mx)=bulkmx
      if (istyp.eq.2) istyp=0
      istyp=istyp+1
      ittyp=(llnum+1)/2
      do 150 i=1,nrt
      dx=1.e10
      l=1
      ishell=.false.
      if (numelh.eq.0) go to 51
      call fndseg(irect(1,i),numelh,bh,j1,jx1,jx7)
      if (j1.ne.0) go to 60
   51 if (numelt.ne.0) then
      l=1
      do 54 j=1,numelt
      m=0
      n=1
      j1=j
      i1=irect(n,i)
      call unpk(mx,ix,ipst(l),3)
      k=1
   52 if (ix(k).ne.i1) go to 53
      m=m+1
      n=n+1
      i1=irect(n,i)
      k=1
      if (m.eq.3) go to 60
      go to 52
   53 k=k+1
      if (k.lt.9) go to 52
   54 l=l+nwcon
      endif
      if (numels.eq.0) go to 58
      ishell=.true.
      do 57 j=1,numels
      m=0
      n=1
      j1=j
      i1=irect(n,i)
      call unpk(mx,ix,ipss(1,j),2)
      mt=matype(mx)
      k=1
   55 if(ix(k).ne.i1) go to 56
      m=m+1
      n=n+1
      i1=irect(n,i)
      k=1
      if(m.eq.3) go to 75
      go to 55
   56 k=k+1
      if(k.lt.5) go to 55
   57 continue
   58 if (istyp.eq.1) write(13,190) i,ittyp
      if (istyp.eq.2) write(13,200) i,ittyp
      if (istyp.eq.1) write ( *,190) i,ittyp
      if (istyp.eq.2) write ( *,200) i,ittyp
      stf(i)=0.0
      go to 150
   60 continue
      do 70 j=1,8
      ii=ix(j)
      xc(j)=x(1,ii)
      yc(j)=x(2,ii)
   70 zc(j)=x(3,ii)
      call vlmass (xc,yc,zc)
   75 do 80 j=1,4
      ii=irect(j,i)
      if (ii.eq.0.and.j.eq.4) ii=irect(3,i)
      xn1(j)=x(1,ii)
      yn2(j)=x(2,ii)
      zn3(j)=x(3,ii)
   80 continue
      call areal (dx,area)
      if(ishell.and.ibelyt.eq.1)
     1dx=.25*(thicks(1,j1)+thicks(2,j1)+thicks(3,j1)+thicks(4,j1))
      if (ishell.and.ibelyt.ne.1) dx=thicks(1,j1)
      stf(i)=sftr*slsfac*area*bkm(mx)/dx
      if(ishell) then
      vol=dx*area
      diagm1=(xn1(1)-xn1(3))**2+(yn2(1)-yn2(3))**2+(zn3(1)-zn3(3))**2
      diagm2=(xn1(2)-xn1(4))**2+(yn2(2)-yn2(4))**2+(zn3(2)-zn3(4))**2
      diagm =sqrt(  min(diagm1,diagm2))
      if (diagm.gt.dx) stf(i)=stf(i)*dx/diagm
      endif
      stfvg=stfvg+stf(i)
      if (nty.eq.4) then
      alen1=(xn1(1)-xn1(3))**2+(yn2(1)-yn2(3))**2+(zn3(1)-zn3(3))**2
      alen2=(xn1(2)-xn1(4))**2+(yn2(2)-yn2(4))**2+(zn3(2)-zn3(4))**2
      chrlen=  max(chrlen,.75*sqrt(  max(alen1,alen2)))
      endif
      if (nty.lt.3.or..not.ishell) go to 87
      do 85 jj=1,4
      call fndnn(nsv,nsn,ix(jj),nn)
      if (nn.le.0) go to 85
      if (mt.ne.20) then
      if (ibelyt.eq.1) ethik(nn)=  max(ethik(nn),thicks(jj,j1))
      if (ibelyt.ne.1) ethik(nn)=  max(ethik(nn),thicks(1,j1))
      endif
   85 continue
   87 zf=zf+vol*ro(mx)
      if (ishell) go to 150
      xs1=0.
      ys1=0.
      zs1=0.
      do 90 jj=1,4
      nn=irect(jj,i)
      iy(jj)=nn
      xx1(jj)=x(1,nn)
      xx2(jj)=x(2,nn)
      xx3(jj)=x(3,nn)
      xs1=xs1+.25*x(1,nn)
      ys1=ys1+.25*x(2,nn)
   90 zs1=zs1+.25*x(3,nn)
      ierr=0
      call st (xnn1,xnn2,xnn3,ss,tt,ierr)
      do 140 jj=1,8
      j=ix(jj)
      do 100 kk=1,4
      if (irect(kk,i).eq.j) go to 140
  100 continue
      dds=xnn1*(x(1,j)-xs1)+xnn2*(x(2,j)-ys1)+xnn3*(x(3,j)-zs1)
      if (dds.lt.0) go to 150
      if (iy(3).eq.iy(4)) go to 120
      do 110 kk=1,4
  110 irect(kk,i)=iy(4-kk+1)
      go to 130
  120 irect(1,i)=iy(2)
      irect(2,i)=iy(1)
  130 iutyp=iutyp+1
      if (iutyp.eq.1) write(13,180)
      go to 150
  140 continue
  150 thk(i)=dx
      stfvg=stfvg/nrt
      return
  180 format('1',' s l a v e   a n d   m a s t e r   r e n u m b e r'//)
  190 format('cannot find element of slave segment',i6,' of interface',
     1i5)
  200 format('cannot find element of master segment',i5,' of interface',
     1i5)
      end
      subroutine mdstf3(stf,n,stfo,avgs,iside,nm)
c     implicit double precision (a-h,o-z)                                    dp
      dimension stf(1)
      if (n.eq.0) return
      scal=avgs/stfo
      do 10 i=1,n
   10 stf(i)=scal*stf(i)
      if(iside.eq.1) write( *,20) nm,scal
      if(iside.eq.2) write( *,30) nm,scal
      if(iside.eq.1) write(13,20) nm,scal
      if(iside.eq.2) write(13,30) nm,scal
      return
   20 format('slave stiffness of sliding interface',i4,' is scaled by',
     1 e14.5)
   30 format('master stiffness of sliding interface',i4,' is scaled by',
     1 e14.5)
      end
      subroutine mdstf9(stf,n,avgs)
c     implicit double precision (a-h,o-z)                                    dp
      dimension stf(1)
      if (n.eq.0) return
      do 10 i=1,n
      if (stf(i).lt.1.e-06) stf(i)=avgs
   10 continue
      return
      end
      subroutine nbsint (irect,bulk,shear,nrt,cm,matype,eosp,ieost,
     1 numelh,bh,jx1,jx7,x,ro,nmmat)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),
     1 xn1(8),xc(8),xx1(4),
     2 yn2(8),yc(8),xx2(4),
     3 zn3(8),zc(8),xx3(4)
      common/bk14/my,iy(9),mx,ix(10),xs1,ys1,zs1,sig(3),epx
      common/nwixa/nwcon
      common/shlopt/istrn,istupd,ibelyt,miter
      dimension irect(4,1),bulk(1),shear(1),cm(1),matype(1),eosp(1),
     & ieost(1),bh(1),jx1(1),xj7(1),x(3,1),ro(1),bkm(600),shm(120)
      bulkmx=0.
      do 10 mx=1,nmmat
      bkm(mx)=0.
      shm(mx)=0.
      mt=matype(mx)
      nes=ieost(mx)
      rho=ro(mx)
      if (mt.eq.1)  then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+6)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+6)))
      endif
      if (mt.eq.2)  then
      bkm(mx)=cm(48*(mx-1)+11)/(3.*(1.-2.*cm(48*(mx-1)+12)))
      shm(mx)=cm(48*(mx-1)+11)/(2.*(1.+cm(48*(mx-1)+12)))
      endif
      if (mt.eq.3.or.mt.eq.28.or.mt.eq.30)  then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+6)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+6)))
      endif
      if (mt.eq.4)  then
      bkm(mx)=cm(48*(mx-1)+9)/(3.*(1.-2.*cm(48*(mx-1)+17)))
      shm(mx)=cm(48*(mx-1)+9)/(2.*(1.+cm(48*(mx-1)+17)))
      endif
      if (mt.eq.5)  then
      bkm(mx)=cm(48*(mx-1)+2)
      shm(mx)=cm(48*(mx-1)+1)
      endif
      if (mt.eq.6)  then
      bkm(mx)=cm(48*(mx-1)+1)
      shm(mx)=cm(48*(mx-1)+6)
      endif
      if (mt.eq.7)  then
      bkm(mx)=cm(48*(mx-1)+1)*2.*(1.+cm(48*(mx-1)+2))/
     1             (3.*(1.-2.*cm(48*(mx-1)+2)))
      shm(mx)=cm(48*(mx-1)+1)
      endif
      if( mt.gt.7.and.mt.lt.12)  then
      call speos (bkm(mx),48*(mx-1),eosp,rho,nes)
      if (mt.eq.10) shm(mx)=cm(48*(mx-1)+1)
      if (mt.eq.11) shm(mx)=cm(48*(mx-1)+1)
      endif
      if (mt.eq.12) then
      bkm(mx)=cm(48*(mx-1)+6)
      shm(mx)=cm(48*(mx-1)+1)
      endif
      if (mt.eq.13) then
      bkm(mx)=cm(48*(mx-1)+6)
      shm(mx)=cm(48*(mx-1)+1)
      endif
      if (mt.eq.14) then
      bkm(mx)=cm(48*(mx-1)+2)
      shm(mx)=cm(48*(mx-1)+1)
      endif
      if (mt.eq.15) then
      bkm(mx)=cm(48*(mx-1)+1)+.66667*cm(48*(mx-1)+2)
      shm(mx)=cm(48*(mx-1)+2)
      endif
      if (mt.eq.16) then
      call speos(bkm(mx),48*(mx-1),eosp,rho,nes)
      shm(mx)=-cm(48*(mx-1)+1)
      if (shm(mx).lt..0) shm(mx)=bkm(mx)*((1.5-3.*shm(mx))/(1.+shm(mx)))
      endif
      if (mt.eq.17) then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+6)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+6)))
      endif
      if (mt.eq.18.or.mt.eq.26) then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+2)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+2)))
      endif
      if (mt.eq.19) then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+2)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+2)))
      endif
      if (mt.eq.20) then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+6)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+6)))
      endif
      if (mt.eq.24) then
      bkm(mx)=cm(48*(mx-1)+1)/(3.*(1.-2.*cm(48*(mx-1)+2)))
      shm(mx)=cm(48*(mx-1)+1)/(2.*(1.+cm(48*(mx-1)+2)))
      endif
      if (mt.eq.25) then
      bkm(mx)=cm(48*(mx-1)+1)
      shm(mx)=cm(48*(mx-1)+2)
      endif
      if (mt.eq.27) then
      bkm(mx)=(4./3.)*(cm(48*(mx-1)+1)+cm(48*(mx-1)+2))
     1 *(1.+cm(48*(mx-1)+3))/(1.-2.*cm(48*(mx-1)+3))
      shm(mx)=cm(48*(mx-1)+1)+cm(48*(mx-1)+2)
      endif
      if (mt.eq.32) then
      bkm(mx)=cm(48*(mx-1)+21)
      shm(mx)=.25*cm(48*(mx-1)+6)/cm(48*(mx-1)+1)
      endif
      if(mt.eq.21 .or. mt.eq.22 .or. mt.eq.23
     1   .or. mt.eq.29 .or. mt.eq.31) then
      write( *,250) mt
      write(13,250) mt
      call adios(2)
      endif
      bulkmx=  max(bulkmx,bkm(mx))
      if (bulkmx.eq.bkm(mx)) then
      romax=ro(mx)
      shrmx=shm(mx)
      endif
   10 continue
      bulkmx=romax*sqrt(bulkmx/romax)
      do 20 mx=1,nmmat
      bkm(mx)=ro(mx)*sqrt((bkm(mx)+4.*shm(mx)/3.)/ro(mx))
      shm(mx)=ro(mx)*sqrt((shm(mx)/4.)/ro(mx))
   20 if (bkm(mx).le.1.e-09) bkm(mx)=bulkmx
      do 150 i=1,nrt
      l=1
      call fndseg(irect(1,i),numelh,bh,j1,jx1,jx7)
      if (j1.eq.0) then
      write ( *,190) i
      write (13,190) i
      bulk(i) =0.
      shear(i)=0.
      go to 150
      else
      bulk(i) =bkm(mx)
      shear(i)=shm(mx)
      endif
      xs1=0.
      ys1=0.
      zs1=0.
      do 90 jj=1,4
      nn=irect(jj,i)
      iy(jj)=nn
      xn1(jj)=x(1,nn)
      yn2(jj)=x(2,nn)
      zn3(jj)=x(3,nn)
      xs1=xs1+.25*x(1,nn)
      ys1=ys1+.25*x(2,nn)
   90 zs1=zs1+.25*x(3,nn)
      ierr=0
      call st (xnn1,xnn2,xnn3,ss,tt,ierr)
      do 140 jj=1,8
      j=ix(jj)
      do 100 kk=1,4
      if (irect(kk,i).eq.j) go to 140
  100 continue
      dds=xnn1*(x(1,j)-xs1)+xnn2*(x(2,j)-ys1)+xnn3*(x(3,j)-zs1)
      if (dds.lt.0) go to 150
      if (iy(3).eq.iy(4)) go to 120
      do 110 kk=1,4
  110 irect(kk,i)=iy(4-kk+1)
      go to 150
  120 irect(1,i)=iy(2)
      irect(2,i)=iy(1)
      go to 150
  140 continue
  150 continue
      return
  190 format('cannot find element of non-reflecting segment',i6)
  250 format(///'error:  material',i3,' is not available for',
     1      /5x,'nonreflecting boundary segments in this version')
      end
      subroutine fndnn(nsv,nsn,ix,nn)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nsv(1)
      nn=-1
      do 10 i=1,nsn
      if (nsv(i).ne.ix) go to 10
      nn=i
      return
   10 continue
      return
      end
      subroutine elgen(numelh,ibins1,ibins2,locsav,locend,ipsh)
c     implicit double precision (a-h,o-z)                                    dp
      common/    /b(1)
      dimension ipsh(*)
      locsav=locend
      num2=numelh*2
      ibins1=locend
      ibins2=ibins1+num2
      locend=ibins2+num2
      call expndm(locend)
      call el17(b(ibins1),numelh,ipsh)
      call ivsort(b(ibins2),numelh)
      call vsort(b(ibins1),b(ibins2),num2)
      return
      end
      subroutine ivsort(ibin,numelh)
      dimension ibin(*)
      do 10 i=1,numelh
      ibin(i)=i
      ibin(i+numelh)=i
   10 continue
      return
      end
      subroutine vsort(a, b, n)
c     implicit double precision (a-h,o-z)                                    dp
c
c***  supplied by don calahan
c
c   partition sorting algorithm
c reference collected algorithms of the acm - 63,64,65
c
      dimension ihigh(32), ilow(32)
      integer a(*), b(*)
c initialize
      nsegs = 1
      il = 1
      ih = n
c if no elements in this segment do nothing
   10 if (il .ge. ih) go to 80
c choose isep (separation entry):
c  make a(il) <= a((il+ih)/2) <= a(ih) by interchange
c  set isep= a((il+ih)/2)
   20 isepx = (ih + il) / 2
      isep = a(isepx)
c ixl is lower segment index (current)
      ixl = il
c make a(il) <= a(isepx)
      if (a(il) .le. isep) go to 30
      a(isepx) = a(il)
      a(il) = isep
      isep = a(isepx)
      it = b(isepx)
      b(isepx) = b(il)
      b(il) = it
c ixh is highest segment index (current)
   30 ixh = ih
c make a(ih) >= a(isepx)
      if (a(ih) .ge. isep) go to 50
      a(isepx) = a(ih)
      a(ih) = isep
      isep = a(isepx)
      it = b(isepx)
      b(isepx) = b(ih)
      b(ih) = it
c make a(il) <= a(isepx)
      if (a(il) .le. isep) go to 50
      a(isepx) = a(il)
      a(il) = isep
      isep = a(isepx)
      it = b(isepx)
      b(isepx) = b(il)
      b(il) = it
      go to 50
c exchange low part entry which is greater than separator with high
c part entry which is less than or equal to the separator value.
   40 it = a(ixh)
      a(ixh) = a(ixl)
      a(ixl) = it
      it = b(ixh)
      b(ixh) = b(ixl)
      b(ixl) = it
c move down upper segment as far as we can
   50 ixh = ixh - 1
      if (a(ixh) .gt. isep) go to 50
c move up lower segment as far as we can
   60 ixl = ixl + 1
      if (a(ixl) .lt. isep) go to 60
c nothing to do if both segments have at most one entry in common
      if (ixl .le. ixh) go to 40
c if both segments overlap then they are separated
c in this case continue with shorter segment, storing the longer
      if (ixh - il .le. ih - ixl) go to 70
c lower segment longer, contin with upper after saving lower
      ilow(nsegs) = il
      ihigh(nsegs) = ixh
      il = ixl
      nsegs = nsegs + 1
      go to 90
c upper segment longer, contin with lower after saving upper
   70 ilow(nsegs) = ixl
      ihigh(nsegs) = ih
      ih = ixh
      nsegs = nsegs + 1
      go to 90
c get another segment for processing if there are any more
   80 nsegs = nsegs - 1
      if (nsegs .eq. 0) return
      il = ilow(nsegs)
      ih = ihigh(nsegs)
c continue to segment as long as length is greater than 11
   90 if (ih - il .ge. 11) go to 20
      if (il .eq. 1) go to 10
      go to 110
c sort elements within segment by interchange of adjacent pairs
  100 il = il + 1
  110 if (il .eq. ih) go to 80
      isep = a(il + 1)
      if (a(il) .le. isep) go to 100
      it = b(il + 1)
      ixl = il
  120 a(ixl + 1) = a(ixl)
      b(ixl + 1) = b(ixl)
      ixl = ixl - 1
      if (isep .lt. a(ixl)) go to 120
      a(ixl + 1) = isep
      b(ixl + 1) = it
      go to 100
      end
      subroutine el17(jx1,numelh,ipsh)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/nwixa/nwcon
      dimension jx1(*),ipsh(*)
      ioff=0
      ipnt=1
      nmel=128
      nelg=numelh/128
      irem=numelh-128*nelg
      if (irem.eq.0) go to 10
      nelg=nelg+1
      nmel=irem
   10 do 100 n=1,nelg
      call unpki(ipsh(ipnt),nwcon,nmel)
      do 20 i=1,nmel
      jx1(i+ioff)=ix1(i)
      jx1(numelh+i+ioff)=ix7(i)
   20 continue
      ioff=ioff+nmel
      ipnt=ipnt+nwcon*nmel
      nmel=128
  100 continue
      return
      end
      subroutine fndseg(ir,numelh,ipsh,iflag,jx1,jxp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/nwixa/nwcon
      common/bk14/my,iy(9),mx,ix(10),xs1,ys1,zs1,sig(3),epx
c
      dimension ir(*),ipsh(*),jx1(*),jxp(*)
c
      iflag=0
      num2=2*numelh
      rnum2=num2
      iter=log(rnum2)/log(2.)+1
c
      do 100 i=1,4
      call binsrc(ir(i),mid,ilo,ihi,jx1,num2,iter)
      if (mid.eq.-1) go to 100
c
      do 50 j=ilo,ihi
c
      iaddrs=nwcon*(jxp(j)-1)+1
      call unpk(mx,ix,ipsh(iaddrs),3)
c
      if (ir(1).eq.ix(1).or.ir(1).eq.ix(2).or.
     1    ir(1).eq.ix(3).or.ir(1).eq.ix(4).or.
     2    ir(1).eq.ix(5).or.ir(1).eq.ix(6).or.
     3    ir(1).eq.ix(7).or.ir(1).eq.ix(8)) go to 20
      go to 50
   20 if (ir(2).eq.ix(1).or.ir(2).eq.ix(2).or.
     1    ir(2).eq.ix(3).or.ir(2).eq.ix(4).or.
     2    ir(2).eq.ix(5).or.ir(2).eq.ix(6).or.
     3    ir(2).eq.ix(7).or.ir(2).eq.ix(8)) go to 30
      go to 50
   30 if (ir(3).eq.ix(1).or.ir(3).eq.ix(2).or.
     1    ir(3).eq.ix(3).or.ir(3).eq.ix(4).or.
     2    ir(3).eq.ix(5).or.ir(3).eq.ix(6).or.
     3    ir(3).eq.ix(7).or.ir(3).eq.ix(8)) go to 40
      go to 50
   40 if (ir(4).eq.ix(1).or.ir(4).eq.ix(2).or.
     1    ir(4).eq.ix(3).or.ir(4).eq.ix(4).or.
     2    ir(4).eq.ix(5).or.ir(4).eq.ix(6).or.
     3    ir(4).eq.ix(7).or.ir(4).eq.ix(8)) go to 45
      go to 50
c
   45 iflag=jxp(j)
      return
   50 continue
c
  100 continue
c
      return
      end
      subroutine binsrc(it,mid,ilo,ihi,il,nmax,iter)
c     implicit double precision (a-h,o-z)                                    dp
      dimension il(1)
      ilo=1
      ihi=nmax
      do 10 i=1,iter
      mid=(ilo+ihi)/2
      if (il(mid).gt.it) then
      ihi=mid
      else
      ilo=mid
      endif
   10 continue
      if (il(mid).eq.it) go to 30
      if (il(ihi).eq.it) go to 15
      if (il(ilo).eq.it) go to 20
      mid=-1
      return
   15 mid=ihi
      go to 30
   20 mid=ilo
   30 mp1=mid+1
      mm1=mid-1
      ihi=nmax
      ilo=1
      do 40 i=mp1,nmax
      if (il(i).eq.it) go to 40
      ihi=i-1
      go to 45
   40 continue
   45 do 50 j=1,mm1
      i=mid-j
      if (il(i).eq.it) go to 50
      ilo=i+1
      return
   50 continue
      return
      end
      subroutine sscnv(irect,nrts,nsv,nsn,iscr)
c     implicit double precision (a-h,o-z)                                    dp
c
c***  converts irect from node numbers to pointers
c
c***  called from initlz
c
      dimension irect(4,1),nsv(1),iscr(1)
c
      do 10 i=1,nsn
      iscr(nsv(i))=i
   10 continue
      do 20 i=1,nrts
      irect(1,i)=iscr(irect(1,i))
      irect(2,i)=iscr(irect(2,i))
      irect(3,i)=iscr(irect(3,i))
      irect(4,i)=iscr(irect(4,i))
   20 continue
      do 30 i=1,nsn
      iscr(nsv(i))=0
   30 continue
c
      return
      end
      subroutine nffcs(sfail,fails,irect,nrts,nsv,nsn,fscr)
c     implicit double precision (a-h,o-z)                                    dp
c
c***  converts irect from node numbers to pointers
c
c***  called from initlz
c
      dimension sfail(2,1),fails(5,1),irect(4,1),nsv(1),fscr(1)
c
      do 10 i=1,nsn
      fscr(nsv(i))=0.0
   10 continue
      do 20 i=1,nrts
      fscr(irect(1,i))=fscr(irect(1,i))+.25*sfail(1,i)
      fscr(irect(2,i))=fscr(irect(2,i))+.25*sfail(1,i)
      fscr(irect(3,i))=fscr(irect(3,i))+.25*sfail(1,i)
      fscr(irect(4,i))=fscr(irect(4,i))+.25*sfail(1,i)
   20 continue
      do 30 i=1,nsn
      fails(1,i)=fscr(nsv(i))
   30 continue
      do 40 i=1,nsn
      fscr(nsv(i))=0.0
   40 continue
      do 50 i=1,nrts
      fscr(irect(1,i))=fscr(irect(1,i))+.25*sfail(2,i)
      fscr(irect(2,i))=fscr(irect(2,i))+.25*sfail(2,i)
      fscr(irect(3,i))=fscr(irect(3,i))+.25*sfail(2,i)
      fscr(irect(4,i))=fscr(irect(4,i))+.25*sfail(2,i)
   50 continue
      do 60 i=1,nsn
      fails(2,i)=fscr(nsv(i))
      fails(3,i)=2.0
      fails(4,i)=2.0
   60 continue
      do 70 i=1,nsn
      fscr(nsv(i))=0.0
   70 continue
c
      return
      end
      subroutine speos(blkmd,mx,eosp,rho,nes)
c     implicit double precision (a-h,o-z)                                    dp
      dimension eosp(*)
      go to (10,20,30,40,50,10,60,70,80,100,110), nes
   10 call speos1 (blkmd,mx,eosp)
      return
   20 call speos2 (blkmd,mx,eosp)
      return
   30 continue
      return
   40 call speos4 (blkmd,mx,eosp,rho)
      return
   50 call speos5 (blkmd,mx,eosp)
      return
   60 continue
      return
   70 call speos8 (blkmd,mx,eosp)
      return
   80 call speos9 (blkmd,mx,eosp)
  100 return
  110 call spes11 ( eosp(mx), blkmd )
      return
      end
      subroutine speos1 (blkmd,mx,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/eos10/c1,c2,c3,c4,c5,c6,c7,e0,v0,xcv
      dimension  eosp(1),ct(1)
      equivalence (c1,ct)
      do 10 i=1,10
   10 ct(i)=eosp(mx+i)
      blkmd=c2
      return
      end
      subroutine speos2 (blkmd,mx,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/eos10/b1,b2,r1,r2,w1,e0,df
      dimension  eosp(1),ct(1)
      equivalence (b1,ct)
      do 10 i=1,7
   10 ct(i)=eosp(mx+i)
      bw1dr1=b1*w1/r1
      bw2dr2=b2*w1/r2
      r1v=bw1dr1/df
      r2v=bw2dr2/df
      wdr1v=b1-r1v
      wdr2v=b2-r2v
      dr1v=w1*e0
      er1v=exp(-r1*df)
      er2v=exp(-r2*df)
      blkmd=df*df*((wdr1v*r1+r1v/df)*er1v+(wdr2v*r2+r2v/df)*er2v+dr1v
     1 /df)
      return
      end
      subroutine speos4 (blkmd,mx,eosp,rho)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/eos10/c,s1,s2,s3,g0,sa
      dimension  eosp(1),ct(6)
      equivalence (c,ct)
      do 10 i=1,6
   10 ct(i)=eosp(mx+i)
      blkmd=rho*c**2
      return
      end
      subroutine speos5 (blkmd,mx,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/ratp/a10,a11,a12,a13,a20,a21,a22,a23,
     1            a30,a31,a32,a33,a40,a41,a42,a43,
     2            a50,a51,a52,a53,a60,a61,a62,a63,
     3            a70,a71,a72,a73,alfa,a12bta,a14,a24
      dimension  eosp(1),ct(1)
      equivalence (a10,ct)
      do 10 i=1,32
   10 ct(i)=eosp(mx+i)
      bb=1./a50
      blkmd=bb*(a11+a10*(alfa-bb*a51))
      return
      end
      subroutine speos8 (blkmd,mx,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      dimension  eosp(1)
c
      do 10 i=1,40
   10 rv(i)=eosp(mx+i)
      n=eosp(mx+48)
      blkmd=.5*(bu(1)+bu(n))
      return
      end
      subroutine speos9 (blkmd,mx,eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/eos8/rv(10),ce(10),te(10),bu(10),n,nc,gamma
      dimension  eosp(1)
c
      do 10 i=1,30
   10 rv(i)=eosp(mx+i)
      n=eosp(mx+48)
      blkmd=abs((ce(n)-ce(n-1))/(rv(n)-rv(n-1)))
      return
      end
      subroutine spes11 ( eosp, bulkmod )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     s  p  e  o  s  1  1                          *
c *                                                                  *
c ********************************************************************
c
      parameter  (  ncrv = 64  )
      common / /  a(1)
c
      lld = int ( eosp(48) )
      bulkmod = a(lld+2*ncrv+1) * ( a(lld+1) + 1. )
      return
      end
      subroutine slavi2(x,irect,lmsr,msr,nsv,iloc,irtl,stf,
     1 nsn,nmn,nty,thic,nseg,isurf)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/sl19/llc,lrsort,resltl,result,i,k,m,n
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(10),iy(10),xs1,ys1,zs1,sig(3),epx
      common/bk21/amx,amy,amz,fs1,fs2,fs3,ft1,ft2,ft3,sp,sm,tp,tm
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/slv2/thk,isrch
      common/rwint/intiad,ncint1,nhint1,nsint1
c
      dimension x(3,*),irect(4,*),lmsr(*),msr(*),nsv(*),iloc(*),
     1          irtl(*),stf(*),thic(*),nseg(*)
c
      ipen=0
      ansmax=0.
c
      do 70 ii=1,nsn
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
c
c     check for penetration
c
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
      if (detv.gt.0.) go to 70
c
c     compute contact point and unit normal to master segment
c
      ierr=0
      call stex (xn1,xn2,xn3,ss,tt,ierr,0)
      if (ierr.eq.1) go to 70
c
c     check for penetration
c
      ans=xn1*(xs1-amx)+xn2*(ys1-amy)+xn3*(zs1-amz)
      if (ierr.eq.0.and.ans.gt.0.) go to 70
      if (isrch.eq.1) call thkgs(xx1,xx2,xx3,thk)
      if (ierr.ne.0.or.ans.lt.-thk)
     1 call salvex(ans,i,ierr,xn1,xn2,xn3,ss,tt,l,x,irect,lmsr,
     1             msr,nseg,nmn)
      if (ierr.ne.0.or.ans.ge.0..or.ans.lt.-thk) go to 70
      if ((istrnt.eq.0).or.(istrnt.ne.0.and.i.gt.ncint1)) then
c
c.... don't move nodes involved in stress initialization
c
      if (ipen.eq.0) then
      write( *,1005) isurf
      write(13,1000) isurf
      ipen=1
      endif
      write(13,1010) i,l,abs(ans)
      call empty(13)
      ansmax=max(ansmax,abs(ans))
c
   60 x(1,i)=x(1,i)-1.10*ans*xn1
      x(2,i)=x(2,i)-1.10*ans*xn2
      x(3,i)=x(3,i)-1.10*ans*xn3
      endif
   70 continue
      if (ipen.ne.0) then
      write( *,1015) isurf,ansmax
      write(13,1015) isurf,ansmax
      write( *,1020)
      write(13,1020)
      endif
c
      return
 1005 format(//5x,'***  intial interpenetration detected',
     1       ' in  slide surface:  ',i8,' ***',
     1  /10x,'(see hsp file for detailed information)')
 1000 format(//5x,'***  intial interpenetration detected',
     1       ' in  slide surface:  ',i8,' ***',
     1/'       node    penetrated segment        distance node moved')
 1010 format(5x,i8,5x,i8,20x,e14.4)
 1015 format(/5x,'maximum penetration in slide surface ',i8,
     1 ' was ',e14.4)
 1020 format(//5x,'nodal coordinates of penetrating nodes ',
     1 'have been modified')
      end
      subroutine areal(dx,area)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),x(20),y(20),z(20)
      fs1=-x(1)+x(2)+x(3)-x(4)
      fs2=-y(1)+y(2)+y(3)-y(4)
      fs3=-z(1)+z(2)+z(3)-z(4)
      ft1=-x(1)-x(2)+x(3)+x(4)
      ft2=-y(1)-y(2)+y(3)+y(4)
      ft3=-z(1)-z(2)+z(3)+z(4)
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=sqrt((e*g-f*f)/16.)
      dx=vol/area
c
      return
c
      end
      subroutine itbrk1(x,im,is,im1,im2,alpha,nsn,nmn)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,1),im(1),is(1),im1(1),im2(1),alpha(1)
      do 200 i=1,nsn
      dsqr=1.e6
      xi=x(1,is(i))
      yi=x(2,is(i))
      zi=x(3,is(i))
      do 100 j=1,nmn
      d2=(x(1,im(j))-xi)**2+(x(2,im(j))-yi)**2+(x(3,im(j))-zi)**2
      if (d2.gt.dsqr) go to 100
      jmnod=j
      dsqr=d2
  100 continue
      k=min0(nmn,jmnod+1)
      d3=(x(1,im(k))-xi)**2+(x(2,im(k))-yi)**2+(x(3,im(k))-zi)**2
      l=max0(1,jmnod-1)
      d4=(x(1,im(l))-xi)**2+(x(2,im(l))-yi)**2+(x(3,im(l))-zi)**2
      jnxt=k
      if (d4.lt.d3) jnxt=l
      if (jnxt.eq.jmnod) jnxt=k+l-jnxt
      im1(i)=im(jmnod)
      im2(i)=im(jnxt)
      m1=im1(i)
      m2=im2(i)
      a1=xi-x(1,m1)
      a2=yi-x(2,m1)
      a3=zi-x(3,m1)
      b1=x(1,m2)-x(1,m1)
      b2=x(2,m2)-x(2,m1)
      b3=x(3,m2)-x(3,m1)
      alpha(i)=1.-(a1*b1+a2*b2+a3*b3)/(b1**2+b2**2+b3**2)
  200 continue
      return
      end
      subroutine itbrk2(is,ip,ixp,locend,ipj,numels,nsn,ib)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux33/ix(128,4),mxt(128)
c     dimension is(*),ip(*),ixp(2,*),ib(*)                              cray1
      dimension is(*),ip(*),ixp(5,*),ib(*)                              vax75
      ipj=locend
      ipjm1=ipj-1
      locend=locend+10*nsn
      call expndm(locend)
      nelg=(numels-1)/128+1
      irem=numels-128*(nelg-1)
      ipnt=1
      if (irem.eq.0) irem=128
      do 200 i=1,nsn
      inode=is(i)
      ip(i)=ipnt
      len=irem
      nn=1
      do 100 ng=1,nelg
      call unpcts(ixp(1,nn),len)
      do 50 j=1,4
      do 40 k=1,len
      if (ix(k,j).ne.inode) go to 40
      ib(ipnt)=nn+k-1
      ipnt=ipnt+1
   40 continue
   50 continue
      nn=nn+len
      len=128
  100 continue
  200 continue
      ip(nsn+1)=ipnt
      locend=ipj+ipnt
      return
      end
      subroutine massb(x,ro,xms,rbm,ip,rvl,ihg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute lumped mass vector
c
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(20),sig(6),epx
c
      dimension x(3,*),xms(*)
c
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
      xx3(i)=x(3,ix(i))
   10 continue
      if (ihg.ne.3.and.ihg.lt.5) call vlmass (xx1,xx2,xx3)
      if (ihg.eq. 3.or.ihg.eq.5) call vlmffb (xx1,xx2,xx3)
      if (vol.le.0.0) go to 30
      vol=vol/rvl
      rovol=ro*vol
      aminc=.125*rovol
      do 20 i=1,8
      j=ix(i)
      rbm=rbm+aminc
      xms(j)=xms(j)+aminc
   20 continue
      return
   30 write(13,40) ip,(ix(i),xx1(i),xx2(i),xx3(i),i=1,8)
      write ( *,40) ip,(ix(i),xx1(i),xx2(i),xx3(i),i=1,8)
      call adios (2)
c
   40 format ('negative or zero determinant - element',i6/
     1 '     nodes ',i6,' (',2(1pe12.5,','),1pe12.5,')'/
     2 7(11x,i6,' (',2(1pe12.5,','),1pe12.5,')'/))
      end
      subroutine miprph(x,ro,gmass,xlmass,ginrt,xlinrt,ip,rvl,ihg,
     1 rbcorl,rbcorg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute inertia tensor and mass by material for solid element
c
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(20),sig(6),epx
c
      dimension x(3,*),ginrt(3,3),rbcorl(3),rbcorg(3)
      dimension  xlinrt(3,3)
c
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
      xx3(i)=x(3,ix(i))
   10 continue
      if (ihg.ne.3.and.ihg.lt.5) call vlmass (xx1,xx2,xx3)
      if (ihg.eq. 3.or.ihg.eq.5) call vlmffb (xx1,xx2,xx3)
      vol=vol/rvl
      rovol=ro*vol
      aminc=.125*rovol
      call smi(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,aminc,8)
      return
      end
      subroutine miprps(x,ro,gmass,xlmass,ginrt,xlinrt,matype,ipss,fibl,
     1 mtnbr,ishlfm,rbcorl,rbcorg,active,numels)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute inertia tensor and mass by material for shell element
c
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(20),sig(6),epx
      dimension xlinrt(3,3)
      dimension x(3,*),ginrt(3,3),matype(*),ipss(5,*),fibl(9,*),        vax75
c     dimension x(3,*),ginrt(3,3),matype(*),ipss(2,*),fibl(9,*),        cray1
     1 ishlfm(*),rbcorl(*),rbcorg(*)
      logical active
c
      do 30 n=1,numels
      call unpk(mx,ix,ipss(1,n),2)
      if (mx.ne.mtnbr) go to 30
      active=.true.
      do 10 i=1,4
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
      xx3(i)=x(3,ix(i))
   10 continue
      iop=ishlfm(mx)
      if (iop.ne.1) then
      thick=fibl(1,n)
      else
      thick=.25*(fibl(1,n)+fibl(2,n)+fibl(3,n)+fibl(4,n))
      endif
      fs1=-xx1(1)+xx1(2)+xx1(3)-xx1(4)
      fs2=-xx2(1)+xx2(2)+xx2(3)-xx2(4)
      fs3=-xx3(1)+xx3(2)+xx3(3)-xx3(4)
      ft1=-xx1(1)-xx1(2)+xx1(3)+xx1(4)
      ft2=-xx2(1)-xx2(2)+xx2(3)+xx2(4)
      ft3=-xx3(1)-xx3(2)+xx3(3)+xx3(4)
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=sqrt((e*g-f*f)/16.)
      if (iop.eq.3.or.iop.eq.4) then
      nbf=3
      else
      nbf=4
      endif
      aminc=ro*area*thick/float(nbf)
      call smi(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,aminc,nbf)
   30 continue
      return
      end
      subroutine miprpb(x,ro,gmass,xlmass,ginrt,xlinrt,matype,ipsb,fibl,
     1 mtnbr,ibemfm,rbcorl,rbcorg,numelb,csprop,rule,mpubr)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute inertia tensor and mass by material for beam element
c
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(20),sig(6),epx
      dimension x(3,*),ginrt(3,3),matype(*),ipsb(4,*),fibl(5,*),        vax75
c     dimension x(3,*),ginrt(3,3),matype(*),ipsb(2,*),fibl(5,*),        cray1
     1 ibemfm(*),rbcorl(*),rbcorg(*),csprop(24,*),rule(mpubr,3,*)
      dimension  xlinrt(3,3)
      data pi/3.141592654/
      do 30 n=1,numelb
      call unpk(mx,ix,ipsb(1,n),2)
      irl=nint(csprop(4,mx))
      irl=iabs(irl)
      if (mtnbr.ne.mx) go to 30
      do 10 i=1,3
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
      xx3(i)=x(3,ix(i))
   10 continue
      iop=ibemfm(mx)
      a1=xx1(2)-xx1(1)
      a2=xx2(2)-xx2(1)
      a3=xx3(2)-xx3(1)
      dl=sqrt(a1*a1+a2*a2+a3*a3)
      dlrho=dl*ro
      if (iop.eq.1) then
      if (csprop(3,mx).eq.0.) then
      aminc=.5*dlrho*fibl(1,n)*fibl(3,n)
      elseif (csprop(3,mx).eq.1.) then
      aminc=.125*dlrho*pi*(fibl(1,n)**2-fibl(3,n)**2)
      elseif (csprop(3,mx).eq.2.) then
      aminc=.5*rule(mpubr,2,irl)*dlrho*fibl(1,n)*fibl(3,n)
      endif
      elseif (iop.eq.2) then
      aminc=.50*fibl(1,n)*dlrho
      elseif (iop.eq.3) then
      aminc=.50*dlrho*fibl(1,n)
      endif
      call smi(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,aminc,2)
   30 continue
c
c.... handle torsional mom of inertia=0 for beam elmt
c
      fac=1.e-3
      xinavg=(xlinrt(1,1)+xlinrt(2,2)+xlinrt(3,3))/3.
      if (xlinrt(1,1).eq.0.0) xlinrt(1,1)=xinavg*fac
      if (xlinrt(2,2).eq.0.0) xlinrt(2,2)=xinavg*fac
      if (xlinrt(3,3).eq.0.0) xlinrt(3,3)=xinavg*fac
      return
      end
      subroutine smi(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,aminc,n)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      dimension ginrt(3,3),rbcorl(*),rbcorg(*)
      dimension  xlinrt(3,3)
      do 10 i=1,n
      gmass   =gmass+aminc
      xlmass   =xlmass+aminc
      xc2     =aminc*xx1(i)*xx1(i)
      yc2     =aminc*xx2(i)*xx2(i)
      zc2     =aminc*xx3(i)*xx3(i)
      rbcorl(1)=rbcorl(1)+xx1(i)*aminc
      rbcorl(2)=rbcorl(2)+xx2(i)*aminc
      rbcorl(3)=rbcorl(3)+xx3(i)*aminc
      rbcorg(1)=rbcorg(1)+xx1(i)*aminc
      rbcorg(2)=rbcorg(2)+xx2(i)*aminc
      rbcorg(3)=rbcorg(3)+xx3(i)*aminc
      xlinrt(1,1)=xlinrt(1,1)+yc2+zc2
      xlinrt(2,2)=xlinrt(2,2)+xc2+zc2
      xlinrt(3,3)=xlinrt(3,3)+xc2+yc2
      xlinrt(1,2)=xlinrt(1,2)-xx1(i)*xx2(i)*aminc
      xlinrt(1,3)=xlinrt(1,3)-xx1(i)*xx3(i)*aminc
      xlinrt(2,3)=xlinrt(2,3)-xx2(i)*xx3(i)*aminc
      ginrt(1,1)=ginrt(1,1)+yc2+zc2
      ginrt(2,2)=ginrt(2,2)+xc2+zc2
      ginrt(3,3)=ginrt(3,3)+xc2+yc2
      ginrt(1,2)=ginrt(1,2)-xx1(i)*xx2(i)*aminc
      ginrt(1,3)=ginrt(1,3)-xx1(i)*xx3(i)*aminc
      ginrt(2,3)=ginrt(2,3)-xx2(i)*xx3(i)*aminc
   10 continue
      return
      end
      subroutine adlump(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,nodm,
     1 xmass,radius,x,nmmass)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      dimension ginrt(3,3),rbcorl(3),rbcorg(3),nodm(*),xmass(*),
     1 radius(*),x(3,*)
      dimension xlinrt(3,3)
      do 10 i=1,nmmass
      node=nodm(i)
      xx1(1)=x(1,node)
      xx2(1)=x(2,node)
      xx3(1)=x(3,node)
      aminc=xmass(i)
      call smi(gmass,xlmass,ginrt,xlinrt,rbcorl,rbcorg,aminc,1)
   10 continue
      return
      end
      subroutine sumupi(x,xms,rm,rbi,rbcor,n,nrb,xx1,xx2,xx3,xc2,yc2,
     1 zc2,rbm)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension x(3,1),xms(*),rbi(3,3),rbcor(*),nrb(*),xx1(*),
     1 xx2(1),xx3(1),xc2(1),yc2(1),zc2(1),rbm(1)
c
      rm=0.0
      do 10 i=1,n
      rbm(i)=xms(nrb(i))
      rm    =rm+rbm(i)
      xx1(i)=x(1,nrb(i))
      xx2(i)=x(2,nrb(i))
      xx3(i)=x(3,nrb(i))
   10 continue
      do 20 i=1,n
      xc2(i)   =rbm(i)*xx1(i)*xx1(i)
      yc2(i)   =rbm(i)*xx2(i)*xx2(i)
      zc2(i)   =rbm(i)*xx3(i)*xx3(i)
      rbcor(1)=rbcor(1)+xx1(i)*rbm(i)
      rbcor(2)=rbcor(2)+xx2(i)*rbm(i)
      rbcor(3)=rbcor(3)+xx3(i)*rbm(i)
      rbi(1,1) =rbi(1,1)+yc2(i)+zc2(i)
      rbi(2,2) =rbi(2,2)+xc2(i)+zc2(i)
      rbi(3,3) =rbi(3,3)+xc2(i)+yc2(i)
      rbi(1,2) =rbi(1,2)-xx1(i)*xx2(i)*rbm(i)
      rbi(1,3) =rbi(1,3)-xx1(i)*xx3(i)*rbm(i)
      rbi(2,3) =rbi(2,3)-xx2(i)*xx3(i)*rbm(i)
   20 continue
c
c.... handle torsional mom of inertia=0 for beam elmt
c
      fac=1.e-3
      xinavg=(rbi(1,1)+rbi(2,2)+rbi(3,3))/3.
      if (rbi(1,1).eq.0.0) rbi(1,1)=xinavg*fac
      if (rbi(2,2).eq.0.0) rbi(2,2)=xinavg*fac
      if (rbi(3,3).eq.0.0) rbi(3,3)=xinavg*fac
c
      return
      end
      subroutine inrta (rbm,rbi,rbcor)
c     implicit double precision (a-h,o-z)                                    dp
      dimension rbi(3,3),rbcor(*)
      xmc=rbcor(1)/rbm
      ymc=rbcor(2)/rbm
      zmc=rbcor(3)/rbm
      xmc2=xmc*xmc
      ymc2=ymc*ymc
      zmc2=zmc*zmc
      rbi(1,1)=rbi(1,1)-rbm*(ymc2+zmc2)
      rbi(2,2)=rbi(2,2)-rbm*(xmc2+zmc2)
      rbi(3,3)=rbi(3,3)-rbm*(xmc2+ymc2)
      rbi(1,2)=rbi(1,2)+rbm*xmc*ymc
      rbi(1,3)=rbi(1,3)+rbm*xmc*zmc
      rbi(2,1)=rbi(1,2)
      rbi(2,3)=rbi(2,3)+rbm*ymc*zmc
      rbi(3,1)=rbi(1,3)
      rbi(3,2)=rbi(2,3)
      rbcor(1)=xmc
      rbcor(2)=ymc
      rbcor(3)=zmc
      return
      end
      subroutine sumupm(rbv,rbcor,xyz,v,xms,n,nrb,x,y,z,
     1 vx,vy,vz,rm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute linear and angular momentum to initialize rigid
c     body translational and angular velocity
c
c     level 1,rbv,rbcor,x,v
      dimension rbv(*),rbcor(3),xyz(3,1),v(3,1),x(*),y(*),z(*),
     1 nrb(1),vx(1),vy(1),vz(1),rm(1),xms(1)
c
      do 10 i=1,n
      rm(i)=xms(nrb(i))
      x(i) =xyz(1,nrb(i))
      y(i) =xyz(2,nrb(i))
      z(i) =xyz(3,nrb(i))
      vx(i)=v(1,nrb(i))
      vy(i)=v(2,nrb(i))
   10 vz(i)=v(3,nrb(i))
      do 20 i=1,n
      x(i)=rm(i)*(x(i)-rbcor(1))
      y(i)=rm(i)*(y(i)-rbcor(2))
   20 z(i)=rm(i)*(z(i)-rbcor(3))
      rbv(1)=0.
      rbv(2)=0.
      rbv(3)=0.
      do 30 i=1,n
      rbv(1)=rbv(1)+rm(i)*vx(i)
      rbv(2)=rbv(2)+rm(i)*vy(i)
   30 rbv(3)=rbv(3)+rm(i)*vz(i)
      rbv(4)=0.
      rbv(5)=0.
      rbv(6)=0.
      do 40 i=1,n
      rbv(4)=rbv(4)+y(i)*vz(i)-z(i)*vy(i)
      rbv(5)=rbv(5)+z(i)*vx(i)-x(i)*vz(i)
   40 rbv(6)=rbv(6)+x(i)*vy(i)-y(i)*vx(i)
c
      return
      end
      subroutine inirbv (rbi,rbm,rbv,rbcods,bcs)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize rigid body velocity array, rbv
c
      dimension rbi(3,3),rbv(6),rbcods(2),bcs(1)
c
c     inverse of inertia matrix
c
      cf5968=rbi(2,2)*rbi(3,3)-rbi(3,2)*rbi(2,3)
      cf6749=rbi(3,2)*rbi(1,3)-rbi(1,2)*rbi(3,3)
      cf4857=rbi(1,2)*rbi(2,3)-rbi(2,2)*rbi(1,3)
      deti=1./(rbi(1,1)*cf5968+rbi(2,1)*cf6749+rbi(3,1)*cf4857)
      rbi11=deti*cf5968
      rbi12=deti*cf6749
      rbi13=deti*cf4857
      rbi21=deti*(-rbi(2,1)*rbi(3,3)+rbi(3,1)*rbi(2,3))
      rbi22=deti*( rbi(1,1)*rbi(3,3)-rbi(3,1)*rbi(1,3))
      rbi23=deti*(-rbi(1,1)*rbi(2,3)+rbi(2,1)*rbi(1,3))
      rbi31=deti*( rbi(2,1)*rbi(3,2)-rbi(3,1)*rbi(2,2))
      rbi32=deti*(-rbi(1,1)*rbi(3,2)+rbi(3,1)*rbi(1,2))
      rbi33=deti*( rbi(1,1)*rbi(2,2)-rbi(2,1)*rbi(1,2))
c
c     initial translational and rotational velocities
c
      rbv4=rbi11*rbv(4)+rbi12*rbv(5)+rbi13*rbv(6)
      rbv5=rbi21*rbv(4)+rbi22*rbv(5)+rbi23*rbv(6)
      rbv6=rbi31*rbv(4)+rbi32*rbv(5)+rbi33*rbv(6)
      rbv(1)=rbv(1)/rbm
      rbv(2)=rbv(2)/rbm
      rbv(3)=rbv(3)/rbm
      rbv(4)=rbv4
      rbv(5)=rbv5
      rbv(6)=rbv6
      ib=rbcods(1)
      if (ib.ne.0) call bound (bcs,rbv(1),rbv(2),rbv(3),ib)
      ib=rbcods(2)
      if (ib.ne.0) call bound (bcs,rbv(4),rbv(5),rbv(6),ib)
c
      return
      end
      subroutine setrbv(rbv,rbcor,xyz,v,n,nrb,x,y,z,vx,vy,vz)
c     implicit double precision (a-h,o-z)                                    dp
c
c     set nodal rigid body velocities for rigid bodies
c
      dimension rbv(*),rbcor(3),xyz(3,1),v(3,1),x(*),y(*),z(*),
     1 nrb(1),vx(1),vy(1),vz(1)
c
      do 10 i=1,n
      x(i) =xyz(1,nrb(i))-rbcor(1)
      y(i) =xyz(2,nrb(i))-rbcor(2)
   10 z(i) =xyz(3,nrb(i))-rbcor(3)
      do 20 i=1,n
      vx(i)=rbv(1)+rbv(5)*z(i)-y(i)*rbv(6)
      vy(i)=rbv(2)+rbv(6)*x(i)-z(i)*rbv(4)
   20 vz(i)=rbv(3)+rbv(4)*y(i)-x(i)*rbv(5)
      do 30 i=1,n
      v(1,nrb(i))=vx(i)
      v(2,nrb(i))=vy(i)
   30 v(3,nrb(i))=vz(i)
c
      return
      end
      subroutine penstf (x,nrb,xyzkcn,rbncod,ipsh,ipss,ipsb,cm,matype,
     1 eosp,ieost,numelh,numels,numelb,nmmat,ro,zf,thicks,fibers)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),
     1 xn1(8),xc(8),xx1(4),
     2 yn2(8),yc(8),xx2(4),
     3 zn3(8),zc(8),xx3(4)
      common /bk14/ my, iy(9), mx, ix(10), xs1, ys1, zs1, sig(3), epx
      common /nwixa/ nwcon
      common /rigidb/ nodtot, ncflag,numrbi
      common/shlopt/istrn,istupd,ibelyt
c
c     dimension nrb(*),xyzkcn(6,*),rbncod(*),ipsh(*),ipss(2,*),         cray1
c    1 ipsb(2,*),                                                       cray1
      dimension nrb(*),xyzkcn(6,*),rbncod(*),ipsh(*),ipss(5,*),         vax75
     1 ipsb(4,*),                                                       vax75
     1 x(3,*), matype(*), cm(*), eosp(*), ieost(*), ro(*), thicks(9,*),
     2 fibers(5,*), bkm(600)
      logical ishell
      data istyp, llnum /0,0/, iutyp /0/
c
      if (ncflag.eq.0) return
c
      bulkmx=0.
      do 10 mx=1,nmmat
      bkm(mx)=0.
      mt=matype(mx)
      nes=ieost(mx)
      rho=ro(mx)
      mx48m1=48*(mx-1)
      if (mt.eq.1) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.2) bkm(mx)=cm(mx48m1+11)/(3.*(1.-2.*cm(mx48m1+12)))
      if (mt.eq.3) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.4) bkm(mx)=cm(mx48m1+9)/(3.*(1.-2.*cm(mx48m1+17)))
      if (mt.eq.5) bkm(mx)=cm(mx48m1+2)
      if (mt.eq.6) bkm(mx)=cm(mx48m1+1)
      if (mt.eq.7) bkm(mx)=cm(mx48m1+1)*2.*(1.+cm(mx48m1+2))/
     1             (3.*(1.-2*cm(mx48m1+2)))
      if (mt.eq.12)bkm(mx)=cm(mx48m1+6)
      if (mt.eq.13)bkm(mx)=cm(mx48m1+6)
      if (mt.eq.14)bkm(mx)=cm(mx48m1+2)
      if (mt.eq.18)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.19)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.20)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.21)bkm(mx)=cm(mx48m1+11)/(3.*(1.-2.*cm(mx48m1+12)))
      if (mt.eq.22)bkm(mx)=cm(mx48m1+21)
      if (mt.eq.23)bkm(mx)=cm(mx48m1+21)
      if (mt.eq.24)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.25)bkm(mx)=cm(mx48m1+1)
      if (mt.eq.26)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+2)))
      if (mt.eq.28)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.30)bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.32) bkm(mx)=cm(mx48m1+21)
      if (mt.eq.34) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+6)))
      if (mt.eq.35) bkm(mx)=cm(mx48m1+1)/(3.*(1.-2.*cm(mx48m1+9)))
      if((mt.gt.7.and.mt.lt.12).or.(mt.gt.14.and.mt.lt.18)) then
           call speos (bkm(mx),mx48m1,eosp,rho,nes)
      endif
   10 bulkmx=  max(bulkmx,bkm(mx))
      do 20 mx=1,nmmat
   20 if (bkm(mx).le.1.e-09) bkm(mx)=bulkmx
      llnum=llnum+1
      if (istyp.eq.2) istyp=0
      istyp=istyp+1
      ittyp=(llnum+1)/2
      lcrbn=0
      do 200 i=1,nodtot
      if (rbncod(i).eq.0) go to 200
      i1=nrb(i)
      lcrbn=lcrbn+1
      xyzkcn(1,lcrbn)=x(1,i1)
      xyzkcn(2,lcrbn)=x(2,i1)
      xyzkcn(3,lcrbn)=x(3,i1)
      xyzkcn(5,lcrbn)=rbncod(i)
      xyzkcn(6,lcrbn)=float(i1)+1.e-06
      l=1
      ishell=.false.
      if (numelh.eq.0) go to 60
      do 50 j=1,numelh
      j1=j
      call unpk(mx,ix,ipsh(l),3)
      k=1
   30 if (ix(k).ne.i1) go to 40
      go to 150
   40 k=k+1
      if (k.lt.9) go to 30
   50 l=l+nwcon
   60 if (numels.eq.0) go to 100
      do 90 j=1,numels
      j1=j
      call unpk(mx,ix,ipss(1,j),2)
      k=1
   70 if (ix(k).ne.i1) go to 80
      ishell=.true.
      go to 170
   80 k=k+1
      if (k.lt.5) go to 70
   90 continue
  100 if (numelb.eq.0) go to 140
      do 130 j=1,numelb
      j1=j
      call unpk(mx,ix,ipsb(1,j),2)
      k=1
  110 if (ix(k).ne.i1) go to 120
      a1=x(1,ix(2))-x(1,ix(1))
      a2=x(2,ix(2))-x(2,ix(1))
      a3=x(3,ix(2))-x(3,ix(1))
      dx=sqrt(a1*a1+a2*a2+a3*a3)
      area=.5*(fibers(1,j1)*fibers(3,j1)+fibers(2,j1)*fibers(4,j1))
      go to 190
  120 k=k+1
      if (k.lt.3) go to 110
  130 continue
  140 call adios (2)
  150 continue
      do 160 j=1,8
      ii=ix(j)
      xc(j)=x(1,ii)
      yc(j)=x(2,ii)
  160 zc(j)=x(3,ii)
      call vlmass (xc,yc,zc)
  170 do 180 j=1,4
      ii=ix(j)
      if (ii.eq.0.and.j.eq.4) ii=ix(3)
      xn1(j)=x(1,ii)
      yn2(j)=x(2,ii)
      zn3(j)=x(3,ii)
  180 continue
      call areal (dx,area)
      if(ishell) then
      if(ibelyt.eq.1)
     1dx=.25*(thicks(1,j1)+thicks(2,j1)+thicks(3,j1)+thicks(4,j1))
      if(ibelyt.ne.1) dx=thicks(1,j1)
      vol=dx*area
      diagm1=(xn1(1)-xn1(3))**2+(yn2(1)-yn2(3))**2+(zn3(1)-zn3(3))**2
      diagm2=(xn1(2)-xn1(4))**2+(yn2(2)-yn2(4))**2+(zn3(2)-zn3(4))**2
      diagm =sqrt(  min(diagm1,diagm2))
      if (diagm.gt.dx) dx=diagm
      endif
  190 xyzkcn(4,lcrbn)=area*bkm(mx)/dx
c
  200 continue
      return
      end
      subroutine sbim(jne,nblk,x,xm,b)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux33/ix(128,8),is(128),len
      common/aux14/
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &y1(128),y2(128),y3(128),am(128),aj(128),amb(128),
     &dot(128)
      common/nwixa/nwcon
c
      dimension jne(3,*),x(*),xm(*),b(*)
c
      ipnt=1
      do 2000 lo=1,nblk
      nelg=jne(1,lo)/128
      irem=jne(1,lo)-128*nelg
      len=128
      nnodes=jne(2,lo)
      ibp=jne(3,1)
      if (irem.eq.0) go to 100
      len=irem
      nelg=nelg+1
  100 continue
c
      call sbims(nelg,nnodes,ipnt,b(ibp),x,xm)
c
 2000 continue
      return
      end
      subroutine sbims(nelg,nnodes,ipnt,ib,x,xm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/aux33/ix(128,8),is(128),len
      common/aux14/
     &x1(128),x2(128),x3(128),x4(128),x5(128),x6(128),
     &y1(128),y2(128),y3(128),am(128),aj(128),amb(128),
     &dot(128)
      common/nwixa/nwcon
c
      dimension x(*),xm(*),ib(*)
c
      do 1000 ng=1,nelg
c
      call unpki(ib(ipnt),nwcon,len)
c
      do 120 i=1,len
      am(i)=0.
      aj(i)=0.
  120 continue
c
      do 130 i=1,len
      ip=3*is(i)
      x3(i)=x(ip)
      x2(i)=x(ip-1)
      x1(i)=x(ip-2)
  130 continue
c
      do 140 i=1,len
      aj(i)=xm(is(i)+numnp)/xm(is(i))
  140 continue
c
      do 200 nn=1,nnodes
c
      do 150 i=1,len
      ip=3*ix(i,nn)
      y3(i)=x(ip)
      y2(i)=x(ip-1)
      y1(i)=x(ip-2)
      amb(i)=xm(ix(i,nn))
  150 continue
c
      do 160 i=1,len
      am(i)=am(i)+amb(i)
c     aj(i)=aj(i)+((y1(i)-x1(i))**2+(y2(i)-x2(i))**2
c    1            +(y3(i)-x3(i))**2)*amb(i)
  160 continue
c
  200 continue
c
      do 230 i=1,len
      xm(is(i))=xm(is(i))+am(i)
      xm(is(i)+numnp)=xm(is(i)+numnp)+aj(i)*am(i)
  230 continue
c
      ipnt=ipnt+nwcon*len
      len=128
c
 1000 continue
c
      return
      end
      subroutine vlmass (x,y,z)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      dimension x(*),y(*),z(*)
c
c     jacobian matrix
c
      aj(1)=-x(1)-x(2)+x(3)+x(4)-x(5)-x(6)+x(7)+x(8)
      aj(4)=-x(1)-x(2)-x(3)-x(4)+x(5)+x(6)+x(7)+x(8)
      aj(7)=-x(1)+x(2)+x(3)-x(4)-x(5)+x(6)+x(7)-x(8)
      aj(2)=-y(1)-y(2)+y(3)+y(4)-y(5)-y(6)+y(7)+y(8)
      aj(5)=-y(1)-y(2)-y(3)-y(4)+y(5)+y(6)+y(7)+y(8)
      aj(8)=-y(1)+y(2)+y(3)-y(4)-y(5)+y(6)+y(7)-y(8)
      aj(3)=-z(1)-z(2)+z(3)+z(4)-z(5)-z(6)+z(7)+z(8)
      aj(6)=-z(1)-z(2)-z(3)-z(4)+z(5)+z(6)+z(7)+z(8)
      aj(9)=-z(1)+z(2)+z(3)-z(4)-z(5)+z(6)+z(7)-z(8)
c
c     jacobian
c
      vol=aj(1)*aj(5)*aj(9)+aj(2)*aj(6)*aj(7)+aj(3)*aj(4)*aj(8)-aj(3)*aj
     1 (5)*aj(7)-aj(2)*aj(4)*aj(9)-aj(1)*aj(6)*aj(8)
      vol=0.0156250*vol
c
      return
      end
      subroutine vlmffb (x,y,z)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      dimension x(*),y(*),z(*)
c
      a45=z(4)-z(5)
      a24=z(2)-z(4)
      a52=z(5)-z(2)
      a16=z(1)-z(6)
      a31=z(3)-z(1)
      a63=z(6)-z(3)
      a27=z(2)-z(7)
      a74=z(7)-z(4)
      a38=z(3)-z(8)
      a81=z(8)-z(1)
      a86=z(8)-z(6)
      a57=z(5)-z(7)
      a6345=a63-a45
      a5238=a52-a38
      a8624=a86-a24
      a7416=a74-a16
      a5731=a57-a31
      a8127=a81-a27
      px1= y(2)*a6345+y(3)*a24-y(4)*a5238+y(5)*a8624+y(6)*a52+y(8)*a45
      px2= y(3)*a7416+y(4)*a31-y(1)*a6345+y(6)*a5731+y(7)*a63+y(5)*a16
      px3= y(4)*a8127-y(1)*a24-y(2)*a7416-y(7)*a8624+y(8)*a74+y(6)*a27
      px4= y(1)*a5238-y(2)*a31-y(3)*a8127-y(8)*a5731+y(5)*a81+y(7)*a38
      px5=-y(8)*a7416+y(7)*a86+y(6)*a8127-y(1)*a8624-y(4)*a81-y(2)*a16
      px6=-y(5)*a8127+y(8)*a57+y(7)*a5238-y(2)*a5731-y(1)*a52-y(3)*a27
      px7=-y(6)*a5238-y(5)*a86+y(8)*a6345+y(3)*a8624-y(2)*a63-y(4)*a38
      px8=-y(7)*a6345-y(6)*a57+y(5)*a7416+y(4)*a5731-y(3)*a74-y(1)*a45
      vol=(px1*x(1)+px2*x(2)+px3*x(3)+px4*x(4)+px5*x(5)
     1    +px6*x(6)+px7*x(7)+px8*x(8))/12.0
c
      return
      end
      subroutine pecfit(cm,c,coef,rho,nes)
c     implicit double precision (a-h,o-z)                                    dp
c
c     fit nt term polynomial to cold compression energy curve
c
      common/bk24/cmu,y0,yb,yc,gamo,ywhmx,au,ye,bu,yp,
     1            ayz,tmyz,gzyz,wyz,pcoff,ccc(7)
c
      dimension ss(22),xj(257),yj(257),rhs(257),d(10,10),c(7),cm(1),
     1 coef(10),t(10)
c
      equivalence (ss,cmu)
c
      data r/.00008314/,nt/10/,np/257/
c
c     set material constants
c
      do 10 i=1,22
   10 ss(i)=cm(i)
c
      rp=r*rho/ayz
c
c     integral p(x)dx for x between 0.0 and 0.80
c     using trapezoidal rule
c
      x=0.0
      e=0.0
      q=0.0
      u=1.0
      p=0.0
      xint=0.0
      dx=.000125
      xj(1)=0.0
      yj(1)=-900.*rp
      np1=np-1
      do 30 i=1,np1
      do 20 j=1,25
c
c     x=1-1./eta=1-v
c
      x=x+dx
      v=1-x
      y=1./v-1.
c
      if (nes.eq.1) call eos1 (c,y,v,u,p,e)
      if (nes.eq.4) call eos4 (c,y,v,u,p,e,rho)
      if (nes.eq.5) call eos5 (c,y,v,u,p,e)
      if (nes.eq.6) call eos1 (c,y,v,u,p,e)
c
      xint=xint+.5*dx*(p+q)
c
      q=p
   20 u=v
      xj(i+1)=(1./v-1.)/.05
      yj(i+1)=xint-900.*rp*exp(wyz*x)/v**(gzyz-wyz)
   30 continue
c
c     define coefficient matrix for least squares fit
c
      do 60 i=1,nt
      do 50 j=i,nt
      d(i,j)=1.0
      ipower=(i-1)+(j-1)
      do 40 k=2,np
   40 d(i,j)=d(i,j)+xj(k)**ipower
   50 d(j,i)=d(i,j)
      rhs(i)=yj(1)
      ipower=i-1
      do 60 k=2,np
   60 rhs(i)=rhs(i)+yj(k)*xj(k)**ipower
c
c     invert coefficient matrix
c
      do 120 n=1,nt
      dd=d(n,n)
      do 70 j=1,nt
   70 d(n,j)=-d(n,j)/dd
      do 110 i=1,nt
      if (n-i) 80,110,80
   80 do 100 j=1,nt
      if (n-j) 90,100,90
   90 d(i,j)=d(i,j)+d(i,n)*d(n,j)
  100 continue
  110 d(i,n)=d(i,n)/dd
      d(n,n)=1.0/dd
  120 continue
c
c     compute coefficients for polynomial
c
      do 140 i=1,nt
      t(i)=0.0
      do 130 j=1,nt
  130 t(i)=t(i)+d(i,j)*rhs(j)
  140 continue
      do 150 i=2,10
  150 t(i)=t(i)/.05**(i-1)
c
      iflag = 0
      if (ccc(5) .eq. 1.0) iflag = 1
      if (coef(1).ne.0.0)  go to 160
c
      coef(1)=t(1)-t(2)+t(3)-t(4)+t(5)-t(6)+t(7)-t(8)+t(9)-t(10)
      coef(2)=t(2)-2.*t(3)+3.*t(4)-4.*t(5)+5.*t(6)-6.*t(7)+7.*t(8)-8.*t(
     1 9)+9.*t(10)
      coef(3)=t(3)-3.*t(4)+6.*t(5)-10.*t(6)+15.*t(7)-21.*t(8)+28.*t(9)-3
     1 6.*t(10)
      coef(4)=t(4)-4.*t(5)+10.*t(6)-20.*t(7)+35.*t(8)-56.*t(9)+84.*t(10)
      coef(5)=t(5)-5.*t(6)+15.*t(7)-35.*t(8)+70.*t(9)-126.*t(10)
      coef(6)=t(6)-6.*t(7)+21.*t(8)-56.*t(9)+126.*t(10)
      coef(7)=t(7)-7.*t(8)+28.*t(9)-84.*t(10)
      coef(8)=t(8)-8.*t(9)+36.*t(10)
      coef(9)=t(9)-9.*t(10)
      coef(10)=t(10)
c
  160 write(13,190) (i,coef(i),i=1,nt)
      write(13,200)
      nt1=nt-1
c
c if iflag = 1 then  we are using supplied (mu) coeff.
c for the ec coefficients . if iflag = 0 then the
c user supplied (eta) coefficients or the code will compute
c these coefficients , but they will be in (eta) format . therefore
c the following change has been made :
c
c   if using (mu) coeff. then  v=xj(i)*.05
c   else  for (eta) v=xj(i)*.05+1.
c
      if (iflag .eq. 1) then
c
      do 180 i=1,np
      v=xj(i)*.05
      ec=coef(1)
      do 170 j=1,nt1
  170 ec=ec+v**j*coef(j+1)
      ed=yj(i)
      write(13,210) v,ec,ed
  180 continue
c
      else
c
      do 185 i=1,np
      v=xj(i)*.05+1.
      ec=coef(1)
      do 175 j=1,nt1
  175 ec=ec+v**j*coef(j+1)
      ed=yj(i)
      write(13,210) v,ec,ed
  185 continue
c
      endif
c
      return
c
c
  190 format('1',//' c o e f f i c i e n t s   f o r    f i t'
     1       ,'   o f   c o l d   ',/'       c o m p r e s s i o n'
     2       ,'   e n e r g y   c u r v e',//(i5,e14.6))
  200 format(////'  evaluation of polynomial with coefficients',/
     1       '        x      polynomial         exact',/)
  210 format(3e14.6)
      end
      subroutine eos1(c,y,v,u,p,e)
c     implicit double precision (a-h,o-z)                                    dp
      dimension c(7)
      a=c(1)+y*(c(2)+y*(c(3)+y*c(4)))
      b=c(5)+y*(c(6)+y*c(7))
      e=(e-.5*(a+p)*(v-u))/(1.+.5*b*(v-u))
      p=a+b*e
      return
      end
      subroutine eos4(c,y,v,u,p,e,ro)
c     implicit double precision (a-h,o-z)                                    dp
      dimension c(7)
      y2=y*y
      xnum=ro*c(1)**2*y*(1.+(1.-.5*c(5))*y-.5*c(6)*y2)
      xdem=(1.-(c(2)-1.)*y-c(3)*v*y2-c(4)*y2*y*v**2)**2
      a=xnum/xdem
      b=c(5)+c(6)*y
      e=(e-.5*(a+p)*(v-u))/(1.+.5*b*(v-u))
      p=a+b*e
      return
      end
      subroutine eos5(eosp,xmu,v,u,p,e)
c     implicit double precision (a-h,o-z)                                    dp
      common/ratp/a10,a11,a12,a13,a20,a21,a22,a23,
     1            a30,a31,a32,a33,a40,a41,a42,a43,
     2            a50,a51,a52,a53,a60,a61,a62,a63,
     3            a70,a71,a72,a73,alfa,a12bta,a14,a24
      dimension  eosp(1),ct(1)
      equivalence (a10,ct)
      do 10 i=1,32
   10 ct(i)=eosp(i)
      xmu2=xmu*xmu
      xmu3=xmu*xmu2
      xmu4=xmu*xmu3
      f1=a10+a11*xmu+a12*xmu2+a13*xmu3+a14*xmu4
      f2=a20+a21*xmu+a22*xmu2+a23*xmu3+a24*xmu4
      f3=a30+a31*xmu+a32*xmu2+a33*xmu3
      f4=a40+a41*xmu+a42*xmu2+a43*xmu3
      f5=a50+a51*xmu+a52*xmu2+a53*xmu3
      f6=a60+a61*xmu+a62*xmu2+a63*xmu3
      f7=a70+a71*xmu+a72*xmu2+a73*xmu3
      fa=1.+alfa*xmu
      p=(f1+e*(f2+e*(f3+e*f4)))*fa/(f5+e*(f6+e*f7))
      et=e-p*(v-u)
      p=(f1+et*(f2+et*(f3+et*f4)))*fa/(f5+et*(f6+et*f7))
      e=e-p*(v-u)
      return
      end
      subroutine ibmsh
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize beam and shell elements
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
c
      common /   / a(1)
      dimension ix(10)
c
      if (numelb.eq.0) go to 10
c
      call gausbm
c
      call in3db(a(n1+nmmat),a(lc11),a(lc1b),a(nb03),a(nb05),a(n1),
     1 a(n2),a(n4f),a(lc10),a(lc14),a(lc14+numnp),numelb,ix,ix(2),
     2 a(n53),a(nb06),a(n4g),mpubr)
c
      if (numels.eq.0) return
c
   10 call gusshl
c
      call azero (a(ns05+9*numels),numels)
      call in3dis(a(lc11),a(n1),a(n2),a(n3),a(n4f),a(lc1s),
     1 a(ns03),a(ns05),a(ns06),a(lc10),a(lc10+numnp),a(lc14),
     2 a(lc14+numnp),numels,ix,ix(2),a(n53),a(ns01),a(ns04),
     3 a(n19),a(ntmp1),a(n1+nmmat),a(ns17))
c
c     get normals to shell reference surface
c
      call setnva(numnp,a(lc10),a(lc10+numnp),a(lc10+4*numnp))
c
c     set normals at the element level
c
      call setrcn(a(lc1s),a(ns03),a(lc10),a(lc10+numnp),
     1 a(lc10+4*numnp),numels,ix,ix(2))
c
      neq2=2*neq
      do 20 i=1,neq2
   20 a(lc10+i-1)=0.0
c
      return
      end
      subroutine gausbm
c     implicit double precision (a-h,o-z)                                    dp
c
c     set gauss integration parameters for beam element
c
      common/beam/ pid4,xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      dimension ja(5)
      data ja/1,4,9,9,16/
      sr35=sqrt(.6000000)
      fvnh=25./81.
      fvet=40./81.
      onnh=1.0/9.0
      fret=4.0/9.0
      sr33=sqrt(3.)/3.
      call azero(pid4,483)
      gw=2.0
      eta(2,1,1)   =-sr33
      eta(2,2,1)   = sr33
      eta(2,3,1)   = sr33
      eta(2,4,1)   =-sr33
      eta(3,1,1)   =-sr35
      eta(3,2,1)   = sr35
      eta(3,3,1)   = sr35
      eta(3,4,1)   =-sr35
      eta(3,6,1)   = sr35
      eta(3,8,1)   =-sr35
      eta(4,1,1)   =-1.0
      eta(4,2,1)   = 1.0
      eta(4,3,1)   = 1.0
      eta(4,4,1)   =-1.0
      eta(4,6,1)   = 1.0
      eta(4,8,1)   =-1.0
      eta(5,1,1)   =-0.86113631159405
      eta(5,2,1)   = 0.86113631159405
      eta(5,3,1)   = 0.86113631159405
      eta(5,4,1)   =-0.86113631159405
      eta(5,5,1)   =-0.33998104358486
      eta(5,6,1)   = 0.33998104358486
      eta(5,7,1)   = 0.86113631159405
      eta(5,8,1)   = 0.86113631159405
      eta(5,9,1)   = 0.33998104358486
      eta(5,10,1)  =-0.33998104358486
      eta(5,11,1)  =-0.86113631159405
      eta(5,12,1)  =-0.86113631159405
      eta(5,13,1)  =-0.33998104358486
      eta(5,14,1)  = 0.33998104358486
      eta(5,15,1)  = 0.33998104358486
      eta(5,16,1)  =-0.33998104358486
      zeta(2,1,1)  =-sr33
      zeta(2,2,1)  =-sr33
      zeta(2,3,1)  = sr33
      zeta(2,4,1)  = sr33
      zeta(3,1,1)  =-sr35
      zeta(3,2,1)  =-sr35
      zeta(3,3,1)  = sr35
      zeta(3,4,1)  = sr35
      zeta(3,5,1)  =-sr35
      zeta(3,7,1)  = sr35
      zeta(4,1,1)  =-1.0
      zeta(4,2,1)  =-1.0
      zeta(4,3,1)  = 1.0
      zeta(4,4,1)  = 1.0
      zeta(4,5,1)  =-1.0
      zeta(4,7,1)  = 1.0
      zeta(5,1,1)  =-0.86113631159405
      zeta(5,2,1)  =-0.86113631159405
      zeta(5,3,1)  = 0.86113631159405
      zeta(5,4,1)  = 0.86113631159405
      zeta(5,5,1)  =-0.86113631159405
      zeta(5,6,1)  =-0.86113631159405
      zeta(5,7,1)  =-0.33998104358486
      zeta(5,8,1)  = 0.33998104358486
      zeta(5,9,1)  = 0.86113631159405
      zeta(5,10,1) = 0.86113631159405
      zeta(5,11,1) = 0.33998104358486
      zeta(5,12,1) =-0.33998104358486
      zeta(5,13,1) =-0.33998104358486
      zeta(5,14,1) =-0.33998104358486
      zeta(5,15,1) = 0.33998104358486
      zeta(5,16,1) = 0.33998104358486
      weight(1,1,1)=4.0
      weight(2,1,1)=1.0
      weight(2,2,1)=1.0
      weight(2,3,1)=1.0
      weight(2,4,1)=1.0
      weight(3,1,1)=fvnh
      weight(3,2,1)=fvnh
      weight(3,3,1)=fvnh
      weight(3,4,1)=fvnh
      weight(3,5,1)=fvet
      weight(3,6,1)=fvet
      weight(3,7,1)=fvet
      weight(3,8,1)=fvet
      weight(3,9,1)=64./81.
      weight(4,1,1)=onnh
      weight(4,2,1)=onnh
      weight(4,3,1)=onnh
      weight(4,4,1)=onnh
      weight(4,5,1)=fret
      weight(4,6,1)=fret
      weight(4,7,1)=fret
      weight(4,8,1)=fret
      weight(4,9,1)=16./9.0
      gw1=0.34785484513745
      gw2=0.65214515486255
      gw11=gw1*gw1
      gw22=gw2*gw2
      gw12=gw1*gw2
      weight(5,1,1) =gw11
      weight(5,2,1) =gw11
      weight(5,3,1) =gw11
      weight(5,4,1) =gw11
      weight(5,5,1) =gw12
      weight(5,6,1) =gw12
      weight(5,7,1) =gw12
      weight(5,8,1) =gw12
      weight(5,9,1) =gw12
      weight(5,10,1)=gw12
      weight(5,11,1)=gw12
      weight(5,12,1)=gw12
      weight(5,13,1)=gw22
      weight(5,14,1)=gw22
      weight(5,15,1)=gw22
      weight(5,16,1)=gw22
c
c     tubular integration coefficients
c
      pi=acos(-1.)
      pid4=.25*pi
      eta(1,1,2)=0.
      zeta(1,1,2)=0.
      weight(1,1,2)=4.
      do 40 j=2,5
      ni2=ja(j)
      w=4./ni2
      dang=2.*pi/ni2
      ang=.5*dang
      do 20 i=1,ni2
      eta(j,i,2)=cos(ang)
      zeta(j,i,2)=sin(ang)
      weight(j,i,2)=w
      ang=ang+dang
   20 continue
   40 continue
c
      return
      end
      subroutine in3db(ibemfm,x,ipsb,vhat,fibl,matype,ro,
     1 csprop,icnt,xmst,xmsr,numelb,mx,ix,rbm,auxvec,rule,mpubr)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
c     dimension x(3,1),ipsb(2,1),xyz(9),vhat(12,1),fibl(5,1),matype(1), cray1
      dimension x(3,1),ipsb(4,1),xyz(9),vhat(12,1),fibl(5,1),matype(1), vax75
     1 ro(1),csprop(24,1),icnt(1),xmst(1),xmsr(1),ix(*),xm(4),alpha(2),
     2 rbm(1),auxvec(1),ni2a(6),rule(mpubr,3,1),ibemfm(1),xx(3,4)
      character*80 txts,mssg
      equivalence (xyz,xx)
      data ni2a/1,4,9,9,16,25/
      lav=0
      do 160 n=1,numelb
      call unpk(mx,ix,ipsb(1,n),2)
      nip=csprop(2,mx)
      irl=nint(csprop(4,mx))
      irl=iabs(irl)
      if (csprop(4,mx).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      mte=matype(mx)
      nmtcon=npt*(7+nconst(mte))
      if (mte.eq.20) nmtcon=0
      icnt(ix(1))=5
      icnt(ix(2))=5
      do 110 i=1,3
      i2=3*i
      xyz(i2-2)=x(1,ix(i))
      xyz(i2-1)=x(2,ix(i))
  110 xyz(i2)  =x(3,ix(i))
      rho=ro(mx)
      iop=ibemfm(mx)
      if (iop.eq.1) then
      call bmck01(xyz,n,vhat(1,n),vhat(7,n),fibl(1,n),fibl(3,n),xm,rho,
     1 alpha,csprop(13,mx),csprop(15,mx),rbm(mx),csprop(3,mx),
     2 rule(mpubr,2,irl))
      elseif (iop.eq.2) then
      call bmck02(xyz,n,vhat(1,n),vhat(7,n),fibl(1,n),fibl(3,n),xm,rho,
     1 alpha,csprop(13,mx),csprop(15,mx),rbm(mx))
      elseif (iop.eq.3) then
      a1=xx(1,2)-xx(1,1)
      a2=xx(2,2)-xx(2,1)
      a3=xx(3,2)-xx(3,1)
      dl=sqrt(a1*a1+a2*a2+a3*a3)
      xm(1)=.50*dl*rho*fibl(1,n)
      xm(2)=xm(1)
      alpha(1)=0.0
      alpha(2)=0.0
      endif
      xmst(ix(1))=xmst(ix(1))+xm(1)
      xmst(ix(2))=xmst(ix(2))+xm(2)
      xmsr(ix(1))=xmsr(ix(1))+alpha(1)*xm(1)
      xmsr(ix(2))=xmsr(ix(2))+alpha(2)*xm(2)
  160 lav=lav+nmtcon
      return
  164 call termin (txts,mssg,lcount,0)
  170 format(8e10.0)
      end
      subroutine bmck01(xx,nbeam,yhat,zhat,fibly,fiblz,xm,rho,
     1 alpha,ybar,zbar,rbm,circ,factor)
c     implicit double precision (a-h,o-z)                                    dp
      common/beamp/a1,a2,a3,b1,b2,b3
      dimension xx(3,*),yhat(6),zhat(6),fibly(*),fiblz(*),xm(*),
     1 alpha(*),ybar(1),zbar(1)
      data pi/3.141592654/
      a1=xx(1,2)-xx(1,1)
      a2=xx(2,2)-xx(2,1)
      a3=xx(3,2)-xx(3,1)
      dl=sqrt(a1*a1+a2*a2+a3*a3)
      if(dl.eq.0) then
      write(13,50) nbeam
      write( *,50) nbeam
      call adios(2)
      endif
      dlrho=dl*rho
      xc=.5*(xx(1,2)+xx(1,1))
      yc=.5*(xx(2,2)+xx(2,1))
      zc=.5*(xx(3,2)+xx(3,1))
      xc2=dlrho*xc*xc
      yc2=dlrho*yc*yc
      zc2=dlrho*zc*zc
c
      if (circ.eq.0) then
      area1=fibly(1)*fiblz(1)
      area2=fibly(2)*fiblz(2)
      elseif (circ.eq.1) then
      area1=.25*pi*(fibly(1)**2-fiblz(1)**2)
      area2=.25*pi*(fibly(2)**2-fiblz(2)**2)
      elseif (circ.eq.3) then
      area1=factor*fibly(1)*fiblz(1)
      area2=factor*fibly(2)*fiblz(2)
      endif
      volm=.25*(area1+area2)
      vold=.125*(area1-area2)
      xm(1)=dlrho*(volm+vold)
      xm(2)=dlrho*(volm-vold)
      rbm=rbm+xm(1)+xm(2)
c
      alp1=fibly(1)**2/4.0+fibly(1)**2*abs(ybar(1))/12.0
      alp2=fiblz(1)**2/4.0+fiblz(1)**2*abs(zbar(1))/12.0
      acon=  max(fibly(1),fiblz(1))*dl/.50
      alpha(1)=  max(alp1,alp2,acon)
      alp1=fibly(2)**2/4.0+fibly(2)**2*abs(ybar(2))/12.0
      alp2=fiblz(2)**2/4.0+fiblz(2)**2*abs(zbar(2))/12.0
      acon=  max(fibly(2),fiblz(2))*dl/.50
      alpha(2)=  max(alp1,alp2,acon)
c
      b1=xx(1,3)-xx(1,1)
      b2=xx(2,3)-xx(2,1)
      b3=xx(3,3)-xx(3,1)
      aa=a1*a1+a2*a2+a3*a3
      ab=a1*b1+a2*b2+a3*b3
      u1=aa*b1-ab*a1
      u2=aa*b2-ab*a2
      u3=aa*b3-ab*a3
      uu=u1*u1+u2*u2+u3*u3
      uu=sqrt(uu)
      if (uu.eq.0.) then
      write(13,60) nbeam
      write( *,60) nbeam
      call adios(2)
      endif
      call crssbm(a1,b1,zhat)
      call crssbm(zhat,a1,yhat)
      call blkcpy(zhat,zhat(4),3)
      call blkcpy(yhat,yhat(4),3)
      return
   50 format(/' beam',i5,' has zero length. execution terminated')
   60 format(/' beam no ',i5, '     k node on beam x-axis ',
     & ' .......execution terminated')
      end
      subroutine bmck02(xx,nbeam,yhat,zhat,fibly,fiblz,xm,rho,
     1 alpha,ybar,zbar,rbm)
c     implicit double precision (a-h,o-z)                                    dp
      common/beamp/a1,a2,a3,b1,b2,b3
      dimension xx(3,*),yhat(6),zhat(6),fibly(*),fiblz(*),xm(*),
     1 alpha(*),ybar(1),zbar(1)
      data pi/3.141592654/
      a1=xx(1,2)-xx(1,1)
      a2=xx(2,2)-xx(2,1)
      a3=xx(3,2)-xx(3,1)
      dl=sqrt(a1*a1+a2*a2+a3*a3)
      dlrho=dl*rho
      xc=.5*(xx(1,2)+xx(1,1))
      yc=.5*(xx(2,2)+xx(2,1))
      zc=.5*(xx(3,2)+xx(3,1))
      xc2=dlrho*xc*xc
      yc2=dlrho*yc*yc
      zc2=dlrho*zc*zc
      xm(1)=.50*fibly(1)*dlrho
      xm(2)=xm(1)
      rbm  =rbm+2.*xm(1)
      xi0=sqrt(fibly(1))*dl/.5
      xi1=dl*dl/12.0+rho*dl*fibly(2)/(2.*xm(1))
      xi2=dl*dl/12.0+rho*dl*fibly(3)/(2.*xm(1))
      xi3=rho*fibly(4)*dl/xm(1)
      alpha(1)=  max(xi0,xi1,xi2,xi3)
      alpha(2)=alpha(1)
      if(dl.gt.0) go to 20
      write(13,50) nbeam
      write( *,50) nbeam
      call adios(2)
   20 b1=xx(1,3)-xx(1,1)
      b2=xx(2,3)-xx(2,1)
      b3=xx(3,3)-xx(3,1)
      aa=a1*a1+a2*a2+a3*a3
      ab=a1*b1+a2*b2+a3*b3
      u1=aa*b1-ab*a1
      u2=aa*b2-ab*a2
      u3=aa*b3-ab*a3
      uu=u1*u1+u2*u2+u3*u3
      uu=sqrt(uu)
      if (uu.gt.0.) go to 30
      write(13,60) nbeam
      write( *,60) nbeam
      call adios(2)
   30 call crssbm(a1,b1,zhat)
      call crssbm(zhat,a1,yhat)
      call blkcpy(zhat,zhat(4),3)
      call blkcpy(yhat,yhat(4),3)
      return
   50 format(/' beam',i5,' has zero length. execution terminated')
   60 format(/' beam no ',i5, '     k node on beam x-axis ',
     & ' .......execution terminated')
      end
      subroutine crssbm(x,y,e)
c     implicit double precision (a-h,o-z)                                    dp
c
c     form a unit vector e = (x cross y)/norm(x cross y)
c
      dimension x(3),y(3),e(3)
c
      e(1) = x(2)*y(3) - y(2)*x(3)
      e(2) = x(3)*y(1) - y(3)*x(1)
      e(3) = x(1)*y(2) - y(1)*x(2)
      enrm = sqrt(e(1)*e(1) + e(2)*e(2) + e(3)*e(3))
      e(1) = e(1)/enrm
      e(2) = e(2)/enrm
      e(3) = e(3)/enrm
      return
      end
      subroutine gusshl
c     implicit double precision (a-h,o-z)                                    dp
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
      gw1 =0.34785484513745
      gw2 =0.65214515486255
      sr35=sqrt(.6000000)
      sr33=sqrt(3.)/3.
      wgts(1,1)= 2.
      wgts(1,2)= 1.0
      wgts(2,2)= 1.0
      wgts(1,3)= 8./9.
      wgts(2,3)= 5./9.
      wgts(3,3)= 5./9.
      wgts(1,4)= gw1
      wgts(2,4)= gw2
      wgts(3,4)= gw2
      wgts(4,4)= gw1
      wgts(1,5)= .568888888888889
      wgts(2,5)= .236926885056189
      wgts(3,5)= .478628670499366
      wgts(4,5)= .478628670499366
      wgts(5,5)= .236926885056189
      zeta(1,1)= 0.0
      zeta(1,2)=-sr33
      zeta(2,2)= sr33
      zeta(1,3)= 0.0
      zeta(2,3)=-sr35
      zeta(3,3)= sr35
      zeta(1,4)=-0.86113631159405
      zeta(2,4)=-0.33998104358486
      zeta(3,4)= 0.33998104358486
      zeta(4,4)= 0.86113631159405
      zeta(1,5)= .000000000000000
      zeta(2,5)=-.906179845938664
      zeta(3,5)=-.538469310105683
      zeta(4,5)= .538469310105683
      zeta(5,5)= .906179845938664
      return
      end
      subroutine in3dis(x,matype,den,prop,csprop,ipss,yhat,fibl,
     1 auxvec,icnt,xnrvec,xmst,xmsr,numels,mx,ix,rbm,strain,beta,
     2 fval,tnew,ishlfm,nshl)
c     implicit double precision (a-h,o-z)                                    dp
c
c     a shell element
c
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/shlopt/istrn,istupd,ibelyt,miter
      common/rwint/intiad,ncint1,nhint1,nsint1
      common/   /b(1)
      dimension x(3,*),xyz(12),matype(*),den(*),prop(48,*),strain(*),
c    1 csprop(24,*),ipss(2,*),id(6),fibl(9,*),alpha(4),rbm(*),auxvec(*),cray1
     1 csprop(24,*),ipss(5,*),id(6),fibl(9,*),alpha(4),rbm(*),auxvec(*),vax75
     2 yhat(12,*),icnt(*),xnrvec(3,*),xmst(*),xmsr(*),ix(*),xmass(8),
     3 beta(*),fval(*),ishlfm(*),tnew(2),nshl(*)
      character*80 txts,mssg
      lav=0
      if (istrn.ne.0) call azero(strain,12*numels)
      if ((istrnt.ne.0).and.(nsint1.gt.0)) then
      call rdabsg(iob7,nsint,1,intiad,0)
      call riosta(iob7)
      intiad=intiad+1
      write( *,1010) nsint
 1010 format(//5x,' Reading NIKE3D stress initialization for',i8,
     1    ' shell elements')
      if (nsint.ne.nsint1) then
      write(13,1020) nsint,nsint1
      write( *,1020) nsint,nsint1
 1020 format(//5x,'Error - inconsistent Nike3d stress file',
     1        /5x,'actual number of shell elements:  ',i8,
     2        /5x,'expected no. of shell elements:   ',i8)
      stop 'nsint error'
      endif
      endif
      do 160 n=1,numels
      call unpk(mx,ix,ipss(1,n),2)
      icnt(ix(1))=icnt(ix(1))+1
      icnt(ix(2))=icnt(ix(2))+1
      icnt(ix(3))=icnt(ix(3))+1
      icnt(ix(4))=icnt(ix(4))+1
      do 10 i=1,4
      i2=3*i
      xyz(i2-2)=x(1,ix(i))
      xyz(i2-1)=x(2,ix(i))
   10 xyz(i2)  =x(3,ix(i))
      nip=csprop(2,mx)
      mte=matype(mx)
      npi=nconst(mte)+7
      iop=ishlfm(mx)
      if (mte.eq.20) npi=0
      nmtcon=nip*npi
      call fibvec(xyz,yhat(1,n),fibl(1,n),prop(1,mx),csprop(1,mx),
     1den(mx),xmass,ix,xnrvec,alpha,csprop(13,mx),rbm(mx),icnt,volume,
     2iop)
      if (istrnt.gt.0) then
      if ((istrnt.ne.0).and.(nshl(n).le.nsint)) then
      iel=nshl(n)
      intloc=intiad+(iel-1)*7*nip
      lav1=lav
      do 15 j=1,nip
      call rdabsg(iob7,auxvec(lav1+1),7,intloc,1)
      call riosta(iob7)
      intloc=intloc+7
      if (iop.ne.1) call gtlocl(auxvec(lav1+1),ix,x,7,1)
   15 lav1=lav1+npi
      endif
      else
      if (mte.eq.1.or.mte.eq.3.or.mte.eq.12.or.mte.eq.24
     1   .or.mte.eq.33.or.mte.eq.34.or.mte.eq.35) then
      do 16 i=1,nmtcon
   16 auxvec(i+lav)=0.0
      endif
      if (mte.eq.2) then
      call insh2 (beta(n),xyz,prop(13,mx),auxvec(lav+1),npi,nip,
     1 prop(1,mx),prop(4,mx),prop(5,mx),prop(6,mx),prop(7,mx))
      endif
      if (mte.eq.4) then
      call insh4 (prop(1,mx),tnew(2),fval,auxvec(lav+1),npi,nip,ix)
      endif
      if (mte.eq.15) then
      call insh15 (prop(1,mx),auxvec(lav+1),npi,nip,volume)
      endif
      if (mte.eq.19) then
      call insh19 (prop(1,mx),auxvec(lav+1),npi,nip)
      endif
      if (mte.eq.21)then
      call insh21(beta(n),xyz,prop(13,mx),auxvec(lav+1),npi,nip,
     1 prop(1,mx),prop(4,mx),prop(5,mx),prop(6,mx),prop(7,mx),fval,
     2 tnew(2),ix)
      endif
      if (mte.gt.21) then
      if (iop.ne. 3) then
      if (mte.eq.22)then
      langle=nint(prop(48,mx))
      call insh22(beta(n),xyz,auxvec(lav+1),npi,nip,
     1 prop(10,mx),prop(13,mx),prop(14,mx),prop(15,mx),b(langle),
     2 ix)
      endif
      if (mte.eq.23)then
      langle=nint(prop(48,mx))
      call insh23(beta(n),xyz,prop(13,mx),auxvec(lav+1),npi,nip,
     1 prop(1,mx),prop(4,mx),prop(5,mx),prop(6,mx),fval,
     2 tnew(2),b(langle+624),ix)
      endif
      else
      lavtmp=lav
      niptmp=nip/3
      do 18 i=1,3
      if (mte.eq.22)then
      langle=nint(prop(48,mx))
      call insh22(beta(n),xyz,auxvec(lavtmp+1),npi,niptmp,
     1 prop(10,mx),prop(13,mx),prop(14,mx),prop(15,mx),b(langle),
     2 ix)
      endif
      if (mte.eq.23)then
      langle=nint(prop(48,mx))
      call insh23(beta(n),xyz,prop(13,mx),auxvec(lavtmp+1),npi,niptmp,
     1 prop(1,mx),prop(4,mx),prop(5,mx),prop(6,mx),fval,
     2 tnew(2),b(langle+624),ix)
      endif
   18 lavtmp=lavtmp+nmtcon/3
      endif
      endif
      endif
      if (iop.eq.3.or.iop.eq.4) then
      nbf=3
      xmass(1)=1.33333333333333*xmass(1)
      xmass(2)=1.33333333333333*xmass(2)
      xmass(3)=1.33333333333333*xmass(3)
      icnt(ix(1))=icnt(ix(1))+4
      icnt(ix(2))=icnt(ix(2))+4
      icnt(ix(3))=icnt(ix(3))+4
      icnt(ix(4))=icnt(ix(4))+4
      else
      nbf=4
      endif
      do 20 i=1,nbf
      xmst(ix(i))=xmst(ix(i))+xmass(i)
   20 xmsr(ix(i))=xmsr(ix(i))+alpha(i)*xmass(i)
  160 lav=lav+nmtcon
      if (istrnt.ne.0) intiad=intiad+7*nsint*nip
      return
  164 call termin (txts,mssg,lcount,0)
  170 format(8e10.0)
      end
      subroutine insh2 (bta,xyz,prop,sig,npi,nip,option,v1,v2,v3,ang)
c     implicit double precision (a-h,o-z)                                    dp
      common/ortho/d11,d12,d13,d14,d15,d16,d21,d22,d23,d24,d25,d26,
     1             d31,d32,d33,d34,d35,d36,d41,d42,d43,d44,d45,d46,
     2             d51,d52,d53,d54,d55,d56,d61,d62,d63,d64,d65,d66,
     3             c11,c12,c13,c14,c22,c23,c24,c33,c34,c44,c55,c56,c66
      dimension xyz(3,*),prop(*),sig(npi,*)
      if (option.eq.3.0.and.bta.eq.0.0) then
      beta=ang
      else
      beta=bta
      endif
      x21 =xyz(1,2)-xyz(1,1)
      y21 =xyz(2,2)-xyz(2,1)
      z21 =xyz(3,2)-xyz(3,1)
      x31 =xyz(1,3)-xyz(1,1)
      y31 =xyz(2,3)-xyz(2,1)
      z31 =xyz(3,3)-xyz(3,1)
      x41 =xyz(1,4)-xyz(1,1)
      y41 =xyz(2,4)-xyz(2,1)
      z41 =xyz(3,4)-xyz(3,1)
      x42 =xyz(1,4)-xyz(1,2)
      y42 =xyz(2,4)-xyz(2,2)
      z42 =xyz(3,4)-xyz(3,2)
      c1  =y31*z42-z31*y42
      c2  =z31*x42-x31*z42
      c3  =x31*y42-y31*x42
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl13=c1*xl
      gl23=c2*xl
      gl33=c3*xl
      xl  =x21*gl13+y21*gl23+z21*gl33
      c1  =x21-gl13*xl
      c2  =y21-gl23*xl
      c3  =z21-gl33*xl
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl11=c1*xl
      gl21=c2*xl
      gl31=c3*xl
      gl12=gl23*gl31-gl33*gl21
      gl22=gl33*gl11-gl13*gl31
      gl32=gl13*gl21-gl23*gl11
      if (option.eq.3.0) then
      x21=v2*gl33-v3*gl23
      y21=v3*gl13-v1*gl33
      z21=v1*gl23-v2*gl13
      endif
      x2  =gl11*x21+gl21*y21+gl31*z21
      y2  =gl12*x21+gl22*y21+gl32*z21
      call blkcpy(prop,d11,36)
      q3=atan2(y2,x2)
      if (q3.lt.0.0) then
      q3=2.*atan2(0.,-1.)+q3
      endif
      q3=q3+beta
      q1=sin(q3)
      q2=cos(q3)
      d1112=d11-d12
      d1222=d12-d22
      d1323=d13-d23
      t11=q2**2
      t12=q1**2
      t14=q2*q1
      t41=-t14*2.0
      t44=t11-t12
      u11=t11*d11+t12*d12
      u12=t11*d12+t12*d22
      c13=t11*d13+t12*d23
      u14=t41*d44
      u21=t12*d11+t11*d12
      u22=t12*d12+t11*d22
      c23=t12*d13+t11*d23
      u24=-t41*d44
      c33=d33
      u41=t14*d1112
      u42=t14*d1222
      c34=t14*d1323
      u44=t44*d44
      c11=u11*t11+u12*t12+u14*t41
      c12=u11*t12+u12*t11-u14*t41
      c14=u11*t14-u12*t14+u14*t44
      c22=u21*t12+u22*t11-u24*t41
      c24=u21*t14-u22*t14+u24*t44
      c44=u41*t14-u42*t14+u44*t44
      c55=d55*t11+d66*t12
      c56=(d55-d66)*t14
      c66=d55*t12+d66*t11
      do 20 j=1,nip
      do 10 i=1,6
   10 sig(i,j)=0.0
   20 call blkcpy (c11,sig(7,j),13)
      return
      end
      subroutine insh4(prop,tnew,fval,sig,npi,nip,ix)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      dimension prop(1),tnew(1),fval(1),sig(npi,1),ix(*)
      if (itemp.lt.0) go to 10
      ctmp=fval(itemp)
      go to 20
   10 ctmp=.250*(tnew(ix(1))+tnew(ix(2))+tnew(ix(3))+tnew(ix(4)))
   20 do 40 j=1,nip
      do 30 i=1,7
   30 sig(i,j) =0.0
      sig(8,j) =ctmp
      sig(9,j) =ctmp
   40 sig(10,j)=ctmp
      return
      end
      subroutine insh15(prop,sig,npi,nip,volume)
c     implicit double precision (a-h,o-z)                                    dp
      dimension prop(1),sig(npi,1)
      do 40 j=1,nip
      do 30 i=1,7
   30 sig(i,j) =0.0
      sig(8,j) =1.0
      sig(9,j) =1./volume
      sig(10,j)=prop(11)
      sig(11,j)=0.0
   40 continue
      return
      end
      subroutine insh19(prop,sig,npi,nip)
c     implicit double precision (a-h,o-z)                                    dp
      dimension prop(1),sig(npi,1)
      do 40 j=1,nip
      do 30 i=1,7
   30 sig(i,j) =0.0
      sig(8,j) =1.0
   40 sig(9,j) =prop(1)
      return
      end
      subroutine insh21(bta,xyz,prop,sig,npi,nip,option,v1,v2,v3,ang,
     1 fval,tnew,ix)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/ortho/d11,d12,d13,d14,d15,d16,d21,d22,d23,d24,d25,d26,
     1             d31,d32,d33,d34,d35,d36,d41,d42,d43,d44,d45,d46,
     2             d51,d52,d53,d54,d55,d56,d61,d62,d63,d64,d65,d66,
     3             c11,c12,c13,c14,c22,c23,c24,c33,c34,c44,c55,c56,c66
      dimension xyz(3,*),prop(*),sig(npi,*),fval(*),tnew(*),ix(*),c(6,6)
      equivalence (d11,c)
      if (option.eq.3.0.and.bta.eq.0.0) then
      beta=ang
      else
      beta=bta
      endif
      x21 =xyz(1,2)-xyz(1,1)
      y21 =xyz(2,2)-xyz(2,1)
      z21 =xyz(3,2)-xyz(3,1)
      x31 =xyz(1,3)-xyz(1,1)
      y31 =xyz(2,3)-xyz(2,1)
      z31 =xyz(3,3)-xyz(3,1)
      x41 =xyz(1,4)-xyz(1,1)
      y41 =xyz(2,4)-xyz(2,1)
      z41 =xyz(3,4)-xyz(3,1)
      x42 =xyz(1,4)-xyz(1,2)
      y42 =xyz(2,4)-xyz(2,2)
      z42 =xyz(3,4)-xyz(3,2)
      c1  =y31*z42-z31*y42
      c2  =z31*x42-x31*z42
      c3  =x31*y42-y31*x42
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl13=c1*xl
      gl23=c2*xl
      gl33=c3*xl
      xl  =x21*gl13+y21*gl23+z21*gl33
      c1  =x21-gl13*xl
      c2  =y21-gl23*xl
      c3  =z21-gl33*xl
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl11=c1*xl
      gl21=c2*xl
      gl31=c3*xl
      gl12=gl23*gl31-gl33*gl21
      gl22=gl33*gl11-gl13*gl31
      gl32=gl13*gl21-gl23*gl11
      if (option.eq.3.0) then
      x21=v2*gl33-v3*gl23
      y21=v3*gl13-v1*gl33
      z21=v1*gl23-v2*gl13
      endif
      x2  =gl11*x21+gl21*y21+gl31*z21
      y2  =gl12*x21+gl22*y21+gl32*z21
      k=1
      do 10 i=1,6
      do 10 j=i,6
      c(i,j)=prop(k)
      c(j,i)=prop(k)
   10 k=k+1
      q3=atan2(y2,x2)
      if (q3.lt.0.0) then
      q3=2.*atan2(0.,-1.)+q3
      endif
      q3=q3+bta
      q1=sin(q3)
      q2=cos(q3)
      d1112=d11-d12
      d1222=d12-d22
      d1323=d13-d23
      t11=q2**2
      t12=q1**2
      t14=q2*q1
      t41=-t14*2.0
      t44=t11-t12
      u11=t11*d11+t12*d12
      u12=t11*d12+t12*d22
      c13=t11*d13+t12*d23
      u14=t41*d44
      u21=t12*d11+t11*d12
      u22=t12*d12+t11*d22
      c23=t12*d13+t11*d23
      u24=-t41*d44
      c33=d33
      u41=t14*d1112
      u42=t14*d1222
      c34=t14*d1323
      u44=t44*d44
      c11=u11*t11+u12*t12+u14*t41
      c12=u11*t12+u12*t11-u14*t41
      c14=u11*t14-u12*t14+u14*t44
      c22=u21*t12+u22*t11-u24*t41
      c24=u21*t14-u22*t14+u24*t44
      c44=u41*t14-u42*t14+u44*t44
      c55=d55*t11+d66*t12
      c56=(d55-d66)*t14
      c66=d55*t12+d66*t11
      if (itemp.ge.0) then
      ctmp=fval(itemp)
      else
      ctmp=.250*(tnew(ix(1))+tnew(ix(2))+tnew(ix(3))+tnew(ix(4)))
      endif
      do 30 j=1,nip
      do 20 i=1,6
   20 sig(i,j)=0.0
      call blkcpy (c11,sig(7,j),13)
      sig(20,j)=ctmp
      sig(21,j)=ctmp
      sig(22,j)=ctmp
      sig(23,j)=t11
      sig(24,j)=t12
      sig(25,j)=t14
   30 continue
      return
      end
      subroutine insh22(bta,xyz,sig,npi,nip,option,v1,v2,v3,ang,ix)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xyz(3,*),sig(npi,*),ix(*),ang(*)
      x21 =xyz(1,2)-xyz(1,1)
      y21 =xyz(2,2)-xyz(2,1)
      z21 =xyz(3,2)-xyz(3,1)
      x31 =xyz(1,3)-xyz(1,1)
      y31 =xyz(2,3)-xyz(2,1)
      z31 =xyz(3,3)-xyz(3,1)
      x41 =xyz(1,4)-xyz(1,1)
      y41 =xyz(2,4)-xyz(2,1)
      z41 =xyz(3,4)-xyz(3,1)
      x42 =xyz(1,4)-xyz(1,2)
      y42 =xyz(2,4)-xyz(2,2)
      z42 =xyz(3,4)-xyz(3,2)
      c1  =y31*z42-z31*y42
      c2  =z31*x42-x31*z42
      c3  =x31*y42-y31*x42
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl13=c1*xl
      gl23=c2*xl
      gl33=c3*xl
      xl  =x21*gl13+y21*gl23+z21*gl33
      c1  =x21-gl13*xl
      c2  =y21-gl23*xl
      c3  =z21-gl33*xl
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl11=c1*xl
      gl21=c2*xl
      gl31=c3*xl
      gl12=gl23*gl31-gl33*gl21
      gl22=gl33*gl11-gl13*gl31
      gl32=gl13*gl21-gl23*gl11
      if (option.eq.3.0) then
      x21=v2*gl33-v3*gl23
      y21=v3*gl13-v1*gl33
      z21=v1*gl23-v2*gl13
      endif
      x2  =gl11*x21+gl21*y21+gl31*z21
      y2  =gl12*x21+gl22*y21+gl32*z21
      q3=atan2(y2,x2)
      if (q3.lt.0.0) then
      q3=2.*atan2(0.,-1.)+q3
      endif
      q3=q3+bta
      do 30 j=1,nip
      do 20 i=1,6
   20 sig(i,j)=0.0
      sig(7,j)=1.0
      sig(8,j)=1.0
      sig(9,j)=1.0
      sig(10,j)= cos(q3+ang(j))
      sig(11,j)=-sin(q3+ang(j))
      sig(12,j)=1.0
   30 continue
      return
      end
      subroutine insh23(bta,xyz,prop,sig,npi,nip,option,v1,v2,v3,
     1 fval,tnew,ang,ix)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/ortho/d11,d12,d13,d14,d15,d16,d21,d22,d23,d24,d25,d26,
     1             d31,d32,d33,d34,d35,d36,d41,d42,d43,d44,d45,d46,
     2             d51,d52,d53,d54,d55,d56,d61,d62,d63,d64,d65,d66,
     3             c11,c12,c13,c14,c22,c23,c24,c33,c34,c44,c55,c56,c66
      dimension xyz(3,*),prop(*),sig(npi,*),fval(*),tnew(*),ix(*),
     1 c(6,6),ang(1)
      equivalence (d11,c)
      x21 =xyz(1,2)-xyz(1,1)
      y21 =xyz(2,2)-xyz(2,1)
      z21 =xyz(3,2)-xyz(3,1)
      x31 =xyz(1,3)-xyz(1,1)
      y31 =xyz(2,3)-xyz(2,1)
      z31 =xyz(3,3)-xyz(3,1)
      x41 =xyz(1,4)-xyz(1,1)
      y41 =xyz(2,4)-xyz(2,1)
      z41 =xyz(3,4)-xyz(3,1)
      x42 =xyz(1,4)-xyz(1,2)
      y42 =xyz(2,4)-xyz(2,2)
      z42 =xyz(3,4)-xyz(3,2)
      c1  =y31*z42-z31*y42
      c2  =z31*x42-x31*z42
      c3  =x31*y42-y31*x42
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl13=c1*xl
      gl23=c2*xl
      gl33=c3*xl
      xl  =x21*gl13+y21*gl23+z21*gl33
      c1  =x21-gl13*xl
      c2  =y21-gl23*xl
      c3  =z21-gl33*xl
      xl  =1./sqrt(c1*c1+c2*c2+c3*c3)
      gl11=c1*xl
      gl21=c2*xl
      gl31=c3*xl
      gl12=gl23*gl31-gl33*gl21
      gl22=gl33*gl11-gl13*gl31
      gl32=gl13*gl21-gl23*gl11
      if (option.eq.3.0) then
      x21=v2*gl33-v3*gl23
      y21=v3*gl13-v1*gl33
      z21=v1*gl23-v2*gl13
      endif
      x2  =gl11*x21+gl21*y21+gl31*z21
      y2  =gl12*x21+gl22*y21+gl32*z21
      k=1
      do 10 i=1,6
      do 10 j=i,6
      c(i,j)=prop(k)
      c(j,i)=prop(k)
   10 k=k+1
      q3=atan2(y2,x2)
      if (q3.lt.0.0) then
      q3=2.*atan2(0.,-1.)+q3
      endif
      q3=q3+bta
      if (itemp.ge.0) then
      ctmp=fval(itemp)
      else
      ctmp=.250*(tnew(ix(1))+tnew(ix(2))+tnew(ix(3))+tnew(ix(4)))
      endif
      do 30 j=1,nip
      do 20 i=1,6
   20 sig(i,j)=0.0
      sig(7,j)=ctmp
      sig(8,j)= cos(q3+ang(j))
      sig(9,j)=-sin(q3+ang(j))
   30 continue
      return
      end
      subroutine fibvec(xyz,yhat,fibl,prop,csprop,den,xmass,ix,xnrvec,
     1 alpha,zbar,rbm,icnt,volume,iop)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xyz(3,4),yhat(3,4),fibl(*),prop(*),csprop(*),ix(*),
     1 ehat(3,4),xnrvec(3,*),xnd(3,4),xgjaci(3,3),ecov(3,3),xmass(*),
     2 alpha(*),zbar(*),icnt(*)
      call unitvi(xyz(1,2),xyz(1,1),ehat(1,1),1)
      call unitvi(xyz(1,3),xyz(1,2),ehat(1,2),1)
      call unitvi(xyz(1,4),xyz(1,1),ehat(1,4),1)
      call crosvi(ehat(1,1),ehat(1,4),yhat(1,1))
      call crosvi(ehat(1,1),ehat(1,2),yhat(1,2))
      if (ix(3).ne.ix(4)) then
      call unitvi(xyz(1,3),xyz(1,4),ehat(1,3),1)
      call crosvi(ehat(1,3),ehat(1,2),yhat(1,3))
      call crosvi(ehat(1,3),ehat(1,4),yhat(1,4))
      else
      call crosvi(ehat(1,4),ehat(1,2),yhat(1,3))
      call crosvi(ehat(1,4),ehat(1,2),yhat(1,4))
      endif
      do 10 i = 1,4
      zi = 0.5*fibl(i)*(-csprop(12+i))
      xnd(1,i) = xyz(1,i) + zi*yhat(1,i)
      xnd(2,i) = xyz(2,i) + zi*yhat(2,i)
      xnd(3,i) = xyz(3,i) + zi*yhat(3,i)
      alpha(i)=fibl(i)**2*(1.0+3.*abs(zbar(i)))/12.0
      xmag=sqrt(xnrvec(1,ix(i))**2+xnrvec(2,ix(i))**2
     1    +xnrvec(3,ix(i))**2)
      if (icnt(ix(i)).gt.1.and.xmag.gt.0.0) then
      xhat1=xnrvec(1,ix(i))/xmag
      xhat2=xnrvec(2,ix(i))/xmag
      xhat3=xnrvec(3,ix(i))/xmag
      xmag=xhat1*yhat(1,i)+xhat2*yhat(2,i)+xhat3*yhat(3,i)
      else
      xhat1=yhat(1,i)
      xhat2=yhat(2,i)
      xhat3=yhat(3,i)
      xmag=1.0
      endif
      xnrvec(1,ix(i))=xnrvec(1,ix(i))+xmag*xhat1
      xnrvec(2,ix(i))=xnrvec(2,ix(i))+xmag*xhat2
      xnrvec(3,ix(i))=xnrvec(3,ix(i))+xmag*xhat3
10    continue
      shp1 = .125*fibl(1)
      shp2 = .125*fibl(2)
      shp3 = .125*fibl(3)
      shp4 = .125*fibl(4)
      x11 = .25*(xnd(1,2) - xnd(1,1) + xnd(1,3) - xnd(1,4))
      x12 = .25*(xnd(2,2) - xnd(2,1) + xnd(2,3) - xnd(2,4))
      x13 = .25*(xnd(3,2) - xnd(3,1) + xnd(3,3) - xnd(3,4))
      x21 = .25*(xnd(1,4) - xnd(1,1) + xnd(1,3) - xnd(1,2))
      x22 = .25*(xnd(2,4) - xnd(2,1) + xnd(2,3) - xnd(2,2))
      x23 = .25*(xnd(3,4) - xnd(3,1) + xnd(3,3) - xnd(3,2))
      x31 = shp1*yhat(1,1) + shp2*yhat(1,2) + shp3*yhat(1,3)
     1    + shp4*yhat(1,4)
      x32 = shp1*yhat(2,1) + shp2*yhat(2,2) + shp3*yhat(2,3)
     1    + shp4*yhat(2,4)
      x33 = shp1*yhat(3,1) + shp2*yhat(3,2) + shp3*yhat(3,3)
     1    + shp4*yhat(3,4)
      det = x11*x22*x33+x12*x23*x31+x13*x21*x32
     1    - x13*x22*x31-x11*x23*x32-x12*x21*x33
      xmassi = 2.0*det*den
      xmass(1)=xmassi
      xmass(2)=xmassi
      xmass(3)=xmassi
      xmass(4)=xmassi
      rbm=rbm+4.*xmassi
      volume=(xmass(1)+xmass(2)+xmass(3)+xmass(4))/den
      aconst=volume/(2.0*(fibl(1)+fibl(2)+fibl(3)+fibl(4)))
      do 30 i=1,4
   30 alpha(i)=  max(alpha(i),aconst)
      if (iop.ne.1) then
      thick=.25*(fibl(1)+fibl(2)+fibl(3)+fibl(4))
      fibl(1)=thick
      fibl(2)=0.
      fibl(3)=0.
      fibl(4)=0.
      fibl(5)=0.
      fibl(6)=0.
      endif
      return
      end
      subroutine setnva(numnp,icnt,xnrvec,xmag)
c     implicit double precision (a-h,o-z)                                    dp
      dimension icnt(*),xnrvec(3,*),xmag(*)
      do 10 i=1,numnp
      if(icnt(i).eq.0) go to 10
      if(icnt(i).gt.4) go to 10
      xmag(i)=sqrt(xnrvec(1,i)**2+xnrvec(2,i)**2+xnrvec(3,i)**2)
      if  (xmag(i).lt.1.e-20) then
      icnt(i)=5
      go to 10
      endif
      xnrvec(1,i)=xnrvec(1,i)/xmag(i)
      xnrvec(2,i)=xnrvec(2,i)/xmag(i)
      xnrvec(3,i)=xnrvec(3,i)/xmag(i)
   10 continue
      return
      end
      subroutine unitvi(x,y,e,iflg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     form a unit vector 'e' in the direction x - y   if iflg = 1
c                                             x + y   if iflg = 2
      dimension x(3),y(3),e(3)
c
      go to (1,2),iflg
1     e(1) = x(1) - y(1)
      e(2) = x(2) - y(2)
      e(3) = x(3) - y(3)
      go to 3
2     e(1) = x(1) + y(1)
      e(2) = x(2) + y(2)
      e(3) = x(3) + y(3)
3     enrm = sqrt(e(1)*e(1) + e(2)*e(2) + e(3)*e(3))
      e(1) = e(1)/enrm
      e(2) = e(2)/enrm
      e(3) = e(3)/enrm
      return
      end
      subroutine crosvi(x,y,e)
c     implicit double precision (a-h,o-z)                                    dp
c
c     form a unit vector e = (x cross y)/norm(x cross y)
c
      dimension x(3),y(3),e(3)
c
      e(1) = x(2)*y(3) - y(2)*x(3)
      e(2) = x(3)*y(1) - y(3)*x(1)
      e(3) = x(1)*y(2) - y(1)*x(2)
      enrm = sqrt(e(1)*e(1) + e(2)*e(2) + e(3)*e(3))
      e(1) = e(1)/enrm
      e(2) = e(2)/enrm
      e(3) = e(3)/enrm
      return
      end
      subroutine setrcn(ipss,yhat,icnt,xnrvct,xmag,numels,mx,ix)
c     implicit double precision (a-h,o-z)                                    dp
c
c     dimension ipss(2,*),yhat(3,4,*),icnt(1),xnrvct(3,*),              cray1
      dimension ipss(5,*),yhat(3,4,*),icnt(1),xnrvct(3,*),              vax75
     1 xmag(*),ix(*)
c
      do 20 n=1,numels
      call unpk(mx,ix,ipss(1,n),2)
      do 10 i=1,4
      if(icnt(ix(i)).gt.4) go to 10
      if(xmag(ix(i)).lt.0.85*float(icnt(ix(i)))) go to 10
      yhat(1,i,n)=xnrvct(1,ix(i))
      yhat(2,i,n)=xnrvct(2,ix(i))
      yhat(3,i,n)=xnrvct(3,ix(i))
   10 continue
   20 continue
      return
      end
      subroutine itsel
c     implicit double precision (a-h,o-z)                                    dp
c
c     another shell element
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common /   / a(1)
      lc0n=lc0+numelh
      npmx=(nt11-nt06)/numelt
      call itshel (a(n1),a(n2),a(n3),a(n4f),a(lc0n),a(lc10),a(lc9),
     1 a(lc11),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),
     2 a(nt01),a(nt02),a(nt03),a,a(nt04),a(lc14),a(n53),a(nt06),
     3 npmx,a(lc1t),a(nt17))
      return
      end
      subroutine itshel (mtype,ro,cm,csprop,u,a,v,x,ieost,eosp,
     1 ihgq,hgq,iqtype,bkqs,nsubgv,mtnum,nfegp,b,auxvec,xms,rbm,thks,
     2 npmx,ipst,ntsl)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk09/det,hh(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk12/b12,b2,qhg
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(7)
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/aux12/
     & x1(128),x2(128),x3(128),x4(128),
     & x5(128),x6(128),x7(128),x8(128),
     & y1(128),y2(128),y3(128),y4(128),
     & y5(128),y6(128),y7(128),y8(128),
     & z1(128),z2(128),z3(128),z4(128),
     & z5(128),z6(128),z7(128),z8(128)
      common/aux13/
     1 x12(128),x34(128),x56(128),x78(128),y12(128),y34(128),
     2 y56(128),y78(128),z12(128),z34(128),z56(128),z78(128),
     3 x14(128),x23(128),x58(128),x67(128),y14(128),y23(128),
     4 y58(128),y67(128),z14(128),z23(128),z58(128),z67(128),
     6 x15(128),x26(128),x37(128),x48(128),y15(128),y26(128),
     7 y37(128),y48(128),z15(128),z26(128),z37(128),z48(128),
     8 x1234(128),x5678(128),x1423(128),x5867(128),
     9 y1234(128),y5678(128),y1423(128),y5867(128),
     & z1234(128),z5678(128),z1423(128),z5867(128)
      common/aux14/
     1 sig1(128),sig2(128),sig3(128),sig4(128),
     2 sig5(128),sig6(128),epx1(128),epx2(128),
     3 epx3(128),epx4(128),epx5(128),epx6(128),aux(128,59)
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/aux35/rhoa(128),cxxa(128),q1a(128),cxa(128)
      common/aux36/lft,llt
      common/nwixa/nwcon
      common/ssbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),zeta(5,5)
c
      dimension mtype(*),ro(*),cm(48,*),csprop(24,*),u(*),a(*),v(*),
     1  x(*),ieost(*),eosp(*),ihgq(*),hgq(*),iqtype(*),bkqs(3,*),
     2  nsubgv(*),mtnum(*),nfegp(*),b(*),auxvec(*),xms(*),rbm(*),
     3  thks(npmx,*),ipst(*),ntsl(*)
      character*80 txts,mssg
      data zero/0.0/
c
      call gusshl
      k=1
      do 20 i=1,5
      do 10 j=1,k
      call basis8(zero,zero,zeta(j,i),h(1,j,i),pr(1,j,i),ps(1,j,i),
     1pt(1,j,i))
   10 continue
      k=k+1
   20 continue
c
      l11=1
      lav=0
      do 300 n=1,numelt
      call unpk(mx,ix,ipst(l11),3)
      if (mx.eq.0) go to 290
      mt=mtype(mx)
      loc=48*(mx-1)
      nes=ieost(mx)
      rvl=1.0
      nmecon=7+nconst(mt)
      if (nes.eq.0) go to 150
      nmecon=nmecon+ncneos(nes)
      if (nes.eq.1) locv=loc+9
      if (nes.eq.2) locv=loc+7
      if (nes.eq.3) locv=loc+7
      if (nes.eq.4) locv=loc+8
      if (nes.eq.5) locv=loc+34
      if (nes.eq.6) locv=loc+9
      if (nes.eq.7) locv=loc+34
      if (nes.eq.8) locv=loc+43
      if (nes.eq.9) locv=loc+33
      rvl=eosp(locv)
  150 ymd=cm(1,mx)
      nip=csprop(2,mx)
      if (istrnt.eq.0) then
      call massbt (x,ro(mx),xms,rbm(mx),ntsl(n),rvl,thks(1,n),ymd,nip)
      u(n)=det
      endif
      if (mt.eq.20) go to 290
      nmconl=nip*nmecon
      do 250 i=1,nmconl
  250 auxvec(lav+i)=0.
      lav=lav+nmconl
  290 l11=l11+nwcon
  300 continue
c
      return
  306 call termin (txts,mssg,lcount,0)
  310 format(8e10.0)
      end
      subroutine basis8 (r,s,t,h,pr,ps,pt)
c     implicit double precision (a-h,o-z)                                    dp
      dimension h(1),pr(1),ps(1),pt(1)
      rp=1.0+r
      sp=1.0+s
      tp=1.0+t
      rm=1.0-r
      sm=1.0-s
      tm=1.0-t
      h(1)=.125*rm*sm*tm
      h(2)=.125*rp*sm*tm
      h(3)=.125*rp*sp*tm
      h(4)=.125*rm*sp*tm
      h(5)=.125*rm*sm*tp
      h(6)=.125*rp*sm*tp
      h(7)=.125*rp*sp*tp
      h(8)=.125*rm*sp*tp
      pr(1)=-.125*sm*tm
      pr(2)=-pr(1)
      pr(3)= .125*sp*tm
      pr(4)=-pr(3)
      pr(5)=-.125*sm*tp
      pr(6)=-pr(5)
      pr(7)= .125*sp*tp
      pr(8)=-pr(7)
      ps(1)=-.125*rm*tm
      ps(2)=-.125*rp*tm
      ps(3)=-ps(2)
      ps(4)=-ps(1)
      ps(5)=-.125*rm*tp
      ps(6)=-.125*rp*tp
      ps(7)=-ps(6)
      ps(8)=-ps(5)
      pt(1)=-.125*rm*sm
      pt(2)=-.125*rp*sm
      pt(3)=-.125*rp*sp
      pt(4)=-.125*rm*sp
      pt(5)=-pt(1)
      pt(6)=-pt(2)
      pt(7)=-pt(3)
      pt(8)=-pt(4)
c
      return
      end
      subroutine massbt(x,ro,xms,rbm,ip,rvl,thks,ym,nip)
c     implicit double precision (a-h,o-z)                                    dp
c
c     compute lumped mass vector
c
      common/bk09/vol,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/bk11/ux(20),uy(20),uz(20),xx1(20),xx2(20),xx3(20)
      common/bk14/mx,ix(20),sig(6),epx
c
      dimension x(3,1),xms(1),thks(1)
c
      do 10 i=1,8
      xx1(i)=x(1,ix(i))
      xx2(i)=x(2,ix(i))
      xx3(i)=x(3,ix(i))
   10 continue
      call vlmass (xx1,xx2,xx3)
      do 15 i=1,nip
   15 thks(i)=0.
      if (vol.le.0.0) go to 30
      vol=vol/rvl
      rovol=ro*vol
      aminc=.125*rovol
      do 20 i=1,8
      j=ix(i)
      rbm=rbm+aminc
      xms(j)=xms(j)+aminc
   20 continue
      return
   30 write (13,40) ip,(ix(i),xx1(i),xx2(i),xx3(i),i=1,8)
      write ( *,40) ip,(ix(i),xx1(i),xx2(i),xx3(i),i=1,8)
      call adios (2)
c
   40 format ('negative or zero determinant - element',i6/
     1 '     nodes ',i6,' (',2(1pe12.5,','),1pe12.5,')'/
     2 7(11x,i6,' (',2(1pe12.5,','),1pe12.5,')'))
      end
      subroutine int1ds (x)
c     implicit double precision (a-h,o-z)                                    dp
      common/   / a(1)
      common / bk00 / numnp
      common / bmsc / numsl, locp, lenp, lhp, buf(1),
     .                nmn, nsn, lmsr, lnsv, lilocs, lfshr, lssm, lsa,
     .                ldmg, radius, fcci, shearm, umax, ibond, hdmg
      loc = locp - 1
      do 20 isl = 1, numsl
c       . load slideline parameters and pointers
        do 10 i = 1, lenp
   10     buf(i+1) = a(loc+i)
c       . initialize slideline no. isl
        call sl1di1 (x, a(lmsr), a(lnsv), a(lilocs),
     .                a(lssm), a(lsa), a(lfshr), a(ldmg))
        loc = loc + lenp
   20   continue
c
c     --  clear array hp  --
c      void = memclr (a(lhp), numnp)
      do 30 i = 0, numnp-1
        a(lhp+i) = 0.
   30 continue
      return
      end
      subroutine sl1di1 (x, msr, nsv, ilocs, ssm, sa, fshr, dmg)
c     implicit double precision (a-h,o-z)                                    dp
      common / bmsc / numsl, locp, lenp, lhp, buf(1),
     .                nmn, nsn, lmsr, lnsv, lilocs, lfshr, lssm, lsa,
     .                ldmg, radius, fcci, shearm, umax, ibond, hdmg
      dimension x(3,1), msr(1), nsv(1), ilocs(1), ssm(1),
     .          sa(1), fshr(1), dmg(1)
c
      do 20 is = 1, nsn
        js = nsv(is)
        y1 = x(1,js)
        y2 = x(2,js)
        y3 = x(3,js)
c       --  find master node closest to slave node  --
        d2min = 1.e32
        km = 0
        do 10 im = 2, (nmn-1)
          jm = msr(im)
          dist2 = (x(1,jm)-y1)**2 + (x(2,jm)-y2)**2 + (x(3,jm)-y3)**2
          if (dist2.lt.d2min) then
            km = im
            d2min = dist2
          end if
   10   continue
c       --  find on which side slave node lays  --
        jm = msr(km)
        j1 = msr(km-1)
        call sl1di2(y1,y2,y3, x(1,jm),x(2,jm),x(3,jm),
     .            x(1,j1),x(2,j1),x(3,j1), s1, d1, mid1)
        j2 = msr(km+1)
        call sl1di2(y1,y2,y3, x(1,jm),x(2,jm),x(3,jm),
     .            x(1,j2),x(2,j2),x(3,j2), s2, d2, mid2)
        if (mid1.eq.1 .and. mid2.eq.0) then
          km = km - 1
          jj = j1
          s = s1
        end if
        if (mid1.eq.0 .and. mid2.eq.1) then
          jj = j2
          s = s2
        end if
        if (mid1.eq.0 .and. mid2.eq.0) then
c         --  move slave node [is] to corner node [km]  --
          jj = j2
          s = 0.
        end if
        if (km.eq.2 .and. s1.gt.1. .and. mid2.eq.0) then
c         --  move slave node [is] over master node [j1]  --
          km = km -1
          jj = j1
          s = 1.
        end if
        if (km.eq.(nmn-1) .and. s2.gt.1. .and. mid1.eq.0) then
c         --  move slave node [is] over master node [j2]  --
          jj = j2
          s = 1.
        end if
        if (mid1.eq.1 .and. mid2.eq.1) then
c         --  move slave node [is] to closer master segment  --
          if (d2.gt.d1) then
            km = km - 1
            jj = j1
            s = s1
          else
            jj = j2
            s = s2
          end if
        end if
        ilocs(is) = km
        if (jj.eq.j1) ssm(is) = 1. - s
        if (jj.eq.j2) ssm(is) = s
c       --  move slave node [is] exactly over master segment [km]  --
        x(1,js) = (1.-s)*x(1,jm) + s*x(1,jj)
        x(2,js) = (1.-s)*x(2,jm) + s*x(2,jj)
        x(3,js) = (1.-s)*x(3,jm) + s*x(3,jj)
c       --  initialize damage parameter  --
        dmg(is) = 0.
   20 continue
c
      sa(1) = 0.
      fshr(1) = 0.
      do 30 is = 2, nsn
        j1 = nsv(is-1)
        j2 = nsv(is)
        dx1 = x(1,j2) - x(1,j1)
        dx2 = x(2,j2) - x(2,j1)
        dx3 = x(3,j2) - x(3,j1)
        dist = sqrt(dx1*dx1 + dx2*dx2 + dx3*dx3)
c       --  sainc = half surface area  --
        sainc = 3.14159 * radius * dist * shearm
        sa(is-1) = sa(is-1) + sainc
        sa(is) = sainc
        fshr(is) = 0.
   30 continue
      return
      end
      subroutine sl1di2 (y1,y2,y3, xa1,xa2,xa3, xb1,xb2,xb3, ss, d, mid)
c     implicit double precision (a-h,o-z)                                    dp
      xbxa1 = xb1 - xa1
      xbxa2 = xb2 - xa2
      xbxa3 = xb3 - xa3
      xbxal2 = xbxa1*xbxa1 + xbxa2*xbxa2 + xbxa3*xbxa3
      xbxali = 0.
      if (xbxal2.gt.0) xbxali = 1. / sqrt(xbxal2)
      yxa1 = y1 - xa1
      yxa2 = y2 - xa2
      yxa3 = y3 - xa3
      xn1 = xbxa1 * xbxali
      xn2 = xbxa2 * xbxali
      xn3 = xbxa3 * xbxali
      ss = (yxa1*xn1 + yxa2*xn2 + yxa3*xn3) * xbxali
      mid = 0
      if (ss.ge.-0.001 .and. ss.le.1.001) mid = 1
c     . distance of point a from segment a-b
c     . d = | e_ijk n_j (y_k - a_k)|
      d1 = xn2*yxa3 - xn3*yxa2
      d2 = xn3*yxa1 - xn1*yxa3
      d3 = xn1*yxa2 - xn2*yxa1
      d = sqrt(d1*d1 + d2*d2 + d3*d3)
      return
      end
      subroutine soltn
c     implicit double precision (a-h,o-z)                                    dp
c***********************************************************************
c                                                                      *
c           ******   *****   *        *******  *     *                 *
c          *        *     *  *           *     **    *                 *
c          *        *     *  *           *     * *   *                 *
c           *****   *     *  *           *     *  *  *                 *
c                *  *     *  *           *     *   * *                 *
c                *  *     *  *           *     *    **                 *
c          ******    *****   *******     *     *     *                 *
c                                                                      *
c***********************************************************************
c
c     solution phase
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk02/iburn,dt1,dt2,isdo
      common/bk05/
     1 nh01,nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),itts,time(2,8)
      common/bk07/n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/tbsysp/numrcf,n6e,n6f,n6g
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      logical idrvon
      common/driver/ idrvon,idrvdm(10)
c
      common /   / a(1)
c
      nttls=(n32-n31)/2
      nttlm=(n33-n32)/2
      nrtls=(n42-n41)/2
      nrtlm=(n43-n42)/2
      isf=(ns08-ns07)/(max0(2*numels,1))
      if (.not.idrvon) then
      call fem3d (a(n1),a(n2),a(n3),a(n6),a(n7),a(n8),a(n9),a(n10),a(n11
     $ ),a(n12),a(n13),a(n14),a(n15),a(n16),a(n17),a(n18),a(n19),a(n20)
     $ ,a(n21),a(n22),a(n23),a(n24),a(n25),a(n30),a(n31),a(n32),a(n33),a
     $ (n34),a(n35),a(n36),a(n37),a(n38),a(n39),a(n40),a(n41),a(n42),a
     $ (n43),a(n44),a(n45),a(n46),a(n47),a,a(lc7),a(lc0),a(lc9),a(lc10)
     $ ,a(lc11),a(lc14),a(lc10),a(n26),a(n27),a(n28),a(lc15),a(n4),
     $ a(n48),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),a(n49),
     $ a(n50),a(n51),a(n52),a(n53),a(n54),a(n55),a(n56),a(n57),a(n58),
     $ a(n59),a(n60),a(n61),a(n62),a(n63),a(n64),a(n65),a(n66),a(n68),
     $ a(n69),a(n77),a(n70),a(n71),a(n73),a(n74),a(n72),a(n75),a(n76),
     $ a(n78),a(n79),a(n31+nttls),a(n32+nttlm),a(n8+nlcur+1),a(lc7a),
     $ a(lc7b),a(n7a),a(n7b),a(n7c),a(n25+7),a(lc13),a(n72+3*numsv),
     $ a(ju),a(jv),a(ns07+numels),isf,a(n5),a(n6e),a(n41+nrtls),
     $ a(n42+nrtlm))
c
      else
      dt1=pltc
      dt2=1.e+06
      itts=idrvdm(1)
      nhv=idrvdm(2)
      maxp=idrvdm(3)
      lc30=lc15+nhv
      lc31=lc15+nhv+itts
      lc32=lc15+nhv+2*itts
      lc33=lc15+nhv+3*itts
      call drver(a(n1),a(n2),a(n3),a(n6),a(n7),a(n8),a(n9),a(n10),a(n11
     $ ),a(n12),a(n13),a(n14),a(n15),a(n16),a(n17),a(n18),a(n19),a(n20)
     $ ,a(n21),a(n22),a(n23),a(n24),a(n25),a(n30),a(n31),a(n32),a(n33),a
     $ (n34),a(n35),a(n36),a(n37),a(n38),a(n39),a(n40),a(n41),a(n42),a
     $ (n43),a(n44),a(n45),a(n46),a(n47),a,a(lc7),a(lc0),a(lc9),a(lc10)
     $ ,a(lc11),a(lc14),a(lc10),a(n26),a(n27),a(n28),a(lc15),a(n4),
     $ a(n48),a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),a(n49),
     $ a(n50),a(n51),a(n52),a(n53),a(n54),a(n55),a(n56),a(n57),a(n58),
     $ a(n59),a(n60),a(n61),a(n62),a(n63),a(n64),a(n65),a(n66),a(n68),
     $ a(n69),a(n77),a(n70),a(n71),a(n73),a(n74),a(n72),a(n75),a(n76),
     $ a(n78),a(n79),a(n31+nttls),a(n32+nttlm),a(n8+nlcur+1),a(lc7a),
     $ a(lc7b),a(n7a),a(n7b),a(n7c),a(n25+7),a(lc13),a(n72+3*numsv),
     $ a(ju),a(jv),a(ns07+numels),isf,a(n5),a(n6e),a(n41+nrtls),
     $ a(n42+nrtlm),a(lc30),a(lc31),a(lc32),a(lc33),a(n10+numcl),
     $ nhv,maxp)
      endif
c
      return
      end
      subroutine fem3d(mtype,ro,cm,ic,bcs,npc,pld,nod,idirn,ncur,clfac,
     1lc,nvel,vx,vy,vz,fval,rd,ilcw,nsw,numtp,nodtie,tim,iparm,irects,
     2irectm,nsv,msr,nsegs,nsegm,lnsv,lmsr,ilocs,ilocm,stfs,stfm,
     3irtls,irtlm,xmsm,e,crst,b,tcode,u,v,a,x,xms,ac,nsubgv,mtnum,
     4nfegp,auxvec,rhsi,zfac,ieost,eosp,ihgq,hgq,iqtype,bkqs,rbu,rbv,
     5rba,rbi,rbm,rbcor,nrbn,nrba,nrb,xrb,yrb,zrb,axrb,ayrb,azrb,
     6rbfx,rbfy,rbfz,rbcods,mxrb,xyzkcn,lpntbk,lbcket,chrlen,
     7ethik,fric,iseg,fdat,fthik,icls,irctsi,irctmi,ilcf,itcode,
     8atcode,ifo,slvfrc,msrfrc,ener,rots,fails,sfail,fl9s,tfail,isf,
     9drdsps,nnfpln,thkslv,thkmsr)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm,itemp,ntmp0,ntmp1
      common/bk02/iburn,dt1,dt2,isdo
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
     5 n78,n79,n80,locend,iname
      common/bk09/det,h(20),p1(20),p2(20),p3(20),aj(9),eps(9)
      common/b10a/inpsd,nmmtde,nmelde,nmmass
      common/b10b/ld01,ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(7)
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco
      common/bk25/iflg,dfavg,detavg,davg,ielmtc,ityptc
      common/bk26/nint
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/tmstp/tsmin,hmax,lctm
      character*4 mess                                                  vax75
      common/aux38/mess
      common/energy/xinen
      common/rigidb/ nodtot, ncflag,numrbi
      common/nwixa/nwcon
      common/sbic/ijne,nblk
      common/rn/irnxx
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/tint/itsc,itbs,itlc
      common/slv2/thk,isrch
      common/bttn/ntnwf,ixa(10)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/shlopt/istrn(1),istupd,ibelyt,miter
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/swmke/swxmom,swymom,swzmom,swkeng
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/eltspf/ietspf
      common/tbsysp/numrcf,n6e,n6f,n6g
      character*6 elname
      common/elnme/ elname(5)
      common/bmsc/numsl(20)
      common/raydmp/dampm,ndamp
      common/thfl/iaddth(3)
      common/extwrk/locaxa,exwk,iewflg
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/drelax/ladd,idrint
      common/fixnod/numfx1,numfx2,numfx3,numfx4,numfx5,numfx6
     1 ,numfx7,numfx8,numfx9,numfx10,numfx11,numfx12
      common/double/iprec,ncpw,unit
c
c
      dimension ilcf(*)
      dimension mtype(*),ro(*),cm(48,*),ic(*),bcs(4,*),npc(*),nfegp(*),
     1pld(*),nod(*),idirn(*),ncur(*),clfac(*),lc(*),nvel(*),vx(*),
     2vy(*),vz(*),fval(*),rd(14,*),ilcw(*),nsw(*),numtp(*),nodtie(*),
     3tim(*),iparm(8,*),irects(*),irectm(*),nsv(*),msr(*),nsegs(*),
     4nsegm(*),lnsv(*),lmsr(*),ilocs(*),ilocm(*),stfs(*),stfm(*),
     5irtls(*),irtlm(*),xmsm(*),e(*),crst(2,*),nsubgv(*),mtnum(*),b(*),
     6tcode(*),u(*),v(*),a(*),x(*),xms(*),ac(3,*),auxvec(*),ener(*),
     7timusd(5),rhsi(*),zfac(*),ieost(*),eosp(48,*),ihgq(*),hgq(*),
     8iqtype(*),bkqs(3,*),rbu(6,*),rbv(6,*),rba(6,*),rbi(3,3,*),rots(*),
     9rbm(*),rbcor(3,*),nrbn(*),nrba(*),nrb(*),xrb(*),yrb(*),zrb(*),
     aaxrb(*),ayrb(*),azrb(*),rbfx(*),rbfy(*),rbfz(*),rbcods(2,*),
     bmxrb(*),xyzkcn(*),lpntbk(*),lbcket(*),chrlen(*),irctsi(*),
c    c  ethik(*),fric(3,*),iseg(*),fdat(5,*),fthik(*),icls(*),irctmi(*) cray1
     cethik(*),fric(3,*),iseg(*),fdat(5,*),fthik(*),icls(3,*),irctmi(*) vax75
      dimension itcode(*),atcode(9,*),ifo(2,*),slvfrc(3,*),msrfrc(3,*),
     1nnfpln(*),fails(*),sfail(2,*),fl9s(5,*),tfail(*),drdsps(*),
     2thkslv(*),thkmsr(*)
      logical pifo
c
      data pifo/.true./
c
      output=.false.
      ncedit=0
c
c.... check for variable plot interval, get load curve number
c
      ipltlc=0
      if( pltc .lt. 0. ) then
      ipltlc=abs(pltc)
      endif
      emax=0.
      if (idrflg.ne.0) then
      write( *,710)
      write(13,710)
      if (numsnc.ne.0) then
      call blkcpy (x,drdsps,3*numnp)
      numdp=-2
      endif
      endif
      nmmat2=5*nmmat
      numnp2 =numnp
      if (numelb+numels.ne.0) numnp2=numnp+numnp
      neq2  =3*numnp2
c
      swxmom=0.0
      swymom=0.0
      swzmom=0.0
      swkeng=0.0
      if (numrw.gt.0) then
      ik=1
      jk=1+numrw
      jl=3*numrw
      do 8 n=1,numrw
      call rigidw (x,a,v,rd(1,n),nsw(ik),ilcw(n),ilcw(jk),ilcw(jk+1),
     1 xms,tim(7+n+2*nmmat),ilcw(jl+n))
      ik=ik+ilcw(n)
    8 jk=jk+2
      endif
      call moment (tim,a,v,xms)
      write ( *,670)
     & tt,dt2,tim(2),tim(3),tim(4),exwk,tim(5),tim(6),tim(7)
      third=1.0/3.0
c      call timuse (timusd)
      timusd(5)=timusd(1)
      dx=0.
      nel7=7*numelh
      nbgn=numnp+1
      neqp1=neq+1
      nxx=3*numnp+1
      issflg=1
      irnrm=0
      if (numels.ne.0) then
      call sgscat(b(lc1s),b(ns03),x)
      if (tt.eq.0.0.and.irnxx.gt.1) call renorm
      endif
      if (nblk.ne.0) call sbinit(b(ijne),nblk,x,b)
      if (idrflg.ne.0) call dvkeep(x,v,b(idvkx),b(idvkv),
     1 neq2,numsnc,1)
c
      if (ncycle.eq.0) then
      if (idrflg.eq.0) then
      if (ietspf.eq.1) then
      write (13,560)
      endif
      endif
      endif
c
      go to 180
c
c     momentum deposition
c
   10 call addmom (v,xms,b,b(lc3),b(lc4),b(lc5),nelmd,tt)
c
      issflg=issflg+1
      if (issflg.gt.10) issflg=1
      irnrm=irnrm+1
      if (irnrm.gt.irnxx.and.irnxx.gt.1) irnrm=1
      if (irnrm.eq.1.and.irnxx.gt.1) call renorm
      if (ntbsl.ne.0) call tbrk(xms,xms(nbgn),a,a(neqp1),
     1    b(ipa),b(ipc),b(ipd),b(ipe),b(iph),b(ipi),b(ipk),
     2    b(ipg),b(ipb),b(ipj),b(ips),b(ipm),b(ipf),numels,
     3    ntbsl,nods,nodm)
      if (ntnwf.ne.0) call tnwf(xms,xms(nbgn),a,a(neqp1),
     1    b(ixa(4)),b(ixa(7)),b(ixa(8)),b(ixa(5)),b(ixa(3)),
     2    b(ixa(6)),b(ixa(9)),numels,ntnwf,b(ixa(1)),b(ixa(2)))
c
c     compute accelerations and apply displacement b.c.'s
c
   15 do 20 n=1,numnp2
      ivp=3*(n-1)
      ac(1,n)=ac(1,n)*xms(n) - dampm*v(ivp+1)
      ac(2,n)=ac(2,n)*xms(n) - dampm*v(ivp+2)
   20 ac(3,n)=ac(3,n)*xms(n) - dampm*v(ivp+3)
c
c     impose boundary conditions
c
      if (numfx1.ne.0) then
      call zfubcs(ac,b(numfx3),b(numfx4),numfx1)
      endif
      if (numfx2.ne.0) then
      call zfubds(ac,b(numfx5),b(numfx6),numfx2,bcs)
      endif
      if (numfx7.ne.0) then
      call zfubes(atcode,ac,itcode,b(numfx9),b(numfx8),b(numfx10),
     1  numfx7)
      endif
c
c     move interface with joy
c
      if (numsnc.ne.0.and.idrflg.eq.0) then
      call movinf (b(n6a+1),b(n6b+1),b(n6c),b(n6d),tt
     1 ,b(n6a),b(n6b),x,v,ac,drdsps)
      endif
      if (nextra.gt.0) then
      nmnext=3*(numnp-nextra)+1
      call infmov (b(nif5+1),tt,b(nif5),x(nmnext),v(nmnext))
      endif
c
c     impose nodal constraints
c
      if (numcc.gt.0) call cnstr0 (ic,a,numtp,numtp(2+numcc),nodtie,xms
     1 )
c
      if (numsv.eq.0) go to 70
c
c     treat tied interfaces
c
      k0=1
      k1=1
      k2=1
      k3=1
      k4=1
      k5=1
      k6=1
      k7=1
      k8=1
      k9=1
      k10=1
c
      do 60 n=1,numsv
      nrts=iparm(1,n)
      nrtm=iparm(2,n)
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      nty=iparm(5,n)
      nst=iparm(6,n)
      mst=iparm(7,n)
      zfc=zfac(n)
c
      if (nty.eq.2.or.nty.eq.6) then
      call tied2 (x,a,e,xmsm,irectm(k6),lmsr(k8),crst(1,k0),msr(k5),nsv
     1 (k4),ilocs(k4),irtlm(k4),tcode,v,xms)
      call tied3 (x,bcs,a,e,xmsm,irectm(k6),lmsr(k8),crst(1,k0),msr(k5)
     1 ,nsv(k4),ilocs(k4),irtlm(k4),tcode,v)
      go to 50
      endif
c
      if (nty.eq.7) then
      call tiedr2 (x,a,a(1+neq),e,e(1+3*nmn),xmsm,xmsm(1+3*nmn),
     &irectm(k6),lmsr(k8),crst(1,k0),msr(k5),
     &nsv(k4),ilocs(k4),irtlm(k4),tcode,tcode(1+numnp),v,v(1+neq),
     &xms,xms(numnp+1))
      call tiedr3 (x,bcs,a,a(1+neq),e,e(1+3*nmn),xmsm,xmsm(1+3*nmn),
     &irectm(k6),lmsr(k8),crst(1,k0),
     &msr(k5),nsv(k4),ilocs(k4),irtlm(k4),tcode,tcode(1+numnp),
     &v,v(1+neq))
      go to 50
      endif
c
      if (nty.eq.1) then
      call slave1 (x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),irtlm
     1 (k4),nsegm(k7),nsn,nmn)
      call slave1 (x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),irtls
     1 (k5),nsegs(k2),nmn,nsn)
      call slide1 (x,a,irects(k1),lnsv(k3),nsv(k4),nsn,xms,xmsm,
     1 nsegs(k2),e,tcode,e(1+3*max0(nsn,nmn)),bcs)
      call slide2 (x,a,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5)
     1 ,irtls(k5),nmn,nsn,tcode,bcs,xmsm,xmsm(1+nsn),irectm(k6),lmsr(k8)
     2 ,nsegm(k7),nrtm,xms,zfc,e,e(1+nsn),irctsi(k1),irctmi(k6))
      call slide3 (x,a,v,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4)
     1 ,irtlm(k4),nsn,nmn,tcode,bcs,nsegm(k7),irects(k1),lnsv(k3)
     2 ,nsegs(k2),e(1+3*max0(nsn,nmn)))
      endif
c
   50 k1=k1+4*nrts
      k2=k2+1+nsn
      k3=k3+nst
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+4*nrtm
      k7=k7+1+nmn
      k8=k8+mst
      k9=k9+nrts
      k10=k10+nrtm
      if (nty.eq.2.or.nty.gt.5) then
      k0=k0+nsn
      endif
   60 continue
c
   70 if (numcc.gt.0) then
      call cnstr1 (ic,a,numtp,numtp(2+numcc),nodtie,xms,x)
      endif
c
c
c     check for impact with rigid wall
c
      swxmom=0.0
      swymom=0.0
      swzmom=0.0
      swkeng=0.0
      if (numrw.gt.0) then
      ik=1
      jk=1+numrw
      jl=3*numrw
      do 80 n=1,numrw
      call rigidw (x,a,v,rd(1,n),nsw(ik),ilcw(n),ilcw(jk),ilcw(jk+1),
     1 xms,tim(7+n+2*nmmat),ilcw(jl+n))
      ik=ik+ilcw(n)
   80 jk=jk+2
      endif
c
      if (numrcf.ne.0) then
      ilc0=n6f
      ilc1=n6g
      do 84 n=1,numrcf
      ilc2=ilc1+nnfpln(n)
      call symwfl(x,a,v,b(ilc0),b(ilc1),nnfpln(n),xms,b(ilc2))
      ilc0=ilc0+6
      ilc1=ilc1+2*nnfpln(n)
   84 continue
      endif
c
      dtx=0.5*(dt1+dt2)
      nint=nint+1
      if (nint.eq.1) then
      tsmin=tsmin*dt2
      endif
c
c     set specified velocities
c
      ttmid=tt+.50*dt2
      call ldcst (fval,npc,pld,ttmid,ilcf)
      call velth (lc(1+numvc),x,a,v,nvel,vx,vy,vz,lc,fval,ilcf,
     1 rbv,lc(1+2*numvc))
c
c
c
c     dynamic relaxation
c
      if (idrflg.eq.0) go to 95
      call moment(tim,a,v,xms)
      enow=tim(2)-.5*summss*(tim(5)**2+tim(6)**2+tim(7)**2)
      emax=  max(enow,emax)
      ncy=ncycle/nrcyck
      irem=ncycle-ncy*nrcyck
      if (irem.ne.0.or.ncy.eq.0) go to 95
      write( *,88) tt,enow,emax,tim(2)
   88 format(1x/' dynamic relaxation progress:'/
     1          ' time ....................... ',1pe14.7/
     1          ' current distortional ke .... ',1pe14.7/
     1          ' maximum distortional ke .... ',1pe14.7/
     1          ' current total ke ........... ',1pe14.7/)
c
c     write taurus database during dynamic relaxation
c
      if (idrint.gt.0) then
      call moment (tim,a,v,xms)
      call prtdat (-1,nfegp,nsubgv,u,b,x,mtnum,cm,a)
      endif
c
      if (enow.gt.drtol*emax) go to 95
      idrflg=0
      idrint=0
      if (numsnc.ne.0) then
      do 89 i=1,neq
   89 drdsps(i)=x(i)-drdsps(i)
      endif
      write( *,720) tt,ncycle
      write(13,720) tt,ncycle
      tt=0.
      ncycle=0
      write (13,560)
      tssfac=tssfdr
      call dvkeep(x,v,b(idvkx),b(idvkv),neq2,numsnc,2)
      go to 180
c
c
c
c     interogate dyna3d by typing 'sw2.'
c
c   95 call timuse (timusd)
95    continue
      if (ncedit.lt.ikedit) then
      if (mess.eq.'sw2.')   then
      call moment (tim,a,v,xms)
      write ( *,670)
     & tt,dt2,tim(2),tim(3),tim(4),exwk,tim(5),tim(6),tim(7)
      write ( *,680) nint,elname(ityptc),ielmtc
      cyctim=1.e+06*(timusd(1)-timusd(5))/(numelh+numelb+numels+numelt)
      icycle=cyctim
      write ( *,690) icycle
      endif
      endif
c
c     print problem status into printer file
c
      ncedit=ncedit+1
      if (ncedit.ge.ikedit) then
      call moment (tim,a,v,xms)
      write (13,670) tt,dt2,tim(2),tim(3),tim(4),exwk,
     1 tim(5),tim(6),tim(7)
      write (13,680) nint,elname(ityptc),ielmtc
      cyctim=1.e+06*(timusd(1)-timusd(5))/(numelh+numelb+numels+numelt)
      icycle=cyctim
      write (13,690) icycle
      ncedit=0
      endif
      timusd(5)=timusd(1)
c
c     print current time and last time step
c
      call timing (2)
      if (idrflg.ne.0) go to 160
c
      if((tt+dt2.ge.pltout).or.(tt.ge.prtout).or.(mess.eq.'sw4.')) then
      do 154 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 154
      lrbn=nrba(n)
      call stvlut(rbcor(1,n),x,v,a,rbv(1,n),dt2,
     1 nrbn(n),nrb(lrbn),xrb,yrb,zrb,axrb,ayrb,azrb,dtx)
  154 continue
      endif
      if (mess.eq.'sw4.') then
      write ( *,700) tt
      go to 105
      endif
      if (tt+dt2.lt.pltout) go to 150
      if(ipltlc .ne. 0) then
      pltc=fval(ipltlc)
      endif
      pltout=pltout+pltc
c     call chef                                                         cray1
  105 call moment (tim,a,v,xms)
c
c     write taurus database
c
      call prtdat (-1,nfegp,nsubgv,u,b,x,mtnum,cm,a)
c
c     taurus database for sliding interface/contact surfaces
c
      if (ffifp) then
      call wrabsf (iob8,v,3*numnp*iprec,0)
      call riosta (iob8)
      call wrabsf (iob8,tcode,numnp*iprec,3*numnp*iprec)
      call riosta (iob8)
      k1=1
      k4=1
      k5=1
      k6=1
      do 107 n=1,numsv
      nrts=iparm(1,n)
      nrtm=iparm(2,n)
      nsn =iparm(3,n)
      nmn =iparm(4,n)
      nty =iparm(5,n)
      ntz=nty
      if (nty.eq.6) ntz=2
      call getfrc (slvfrc(1,k4),nsv(k4),a,xms,nsn,v,irects(k1),x,nrts,
     1 tcode,ntz)
      call getfrc (msrfrc(1,k5),msr(k5),a,xms,nmn,v,irectm(k6),x,nrtm,
     1 tcode,ntz)
      k1=k1+4*nrts
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+4*nrtm
  107 continue
      call rdabsf (iob8,v,3*numnp*iprec,0,ioerr)
      call riosta (iob8)
      call rdabsf (iob8,tcode,numnp*iprec,3*numnp*iprec,ioerr)
      call riosta (iob8)
      endif
c
      do 110 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 110
c
c     print rigid body parameters
c
      call prntrb(rbcor(1,n),rbu(1,n),rbv(1,n),rba(1,n),nint,tt,n)
c
  110 continue
c
      call timing (3)
c
  150 if (tt.gt.endtim) pltout=endtim
c
      if (numifs.gt.0) then
      call outifs (b(nif2),b(nif3),x,tt)
      endif
c
c
      if (tt.lt.prtout) go to 160
      prtout=prtout+prtc
      if (prtc.gt.endtim) go to 160
c
c     print accelerations,velocities,displacements and stresses
c
c     write  interface force data
c
      if (pifo) then
      ksch=0
      do 151 n=1,numsv
      if (ifo(1,n).eq.1) ksch=ksch+1
      if (ifo(2,n).eq.1) ksch=ksch+1
  151 continue
      if (ksch.gt.0) then
      k1=1
      k4=1
      k5=1
      k6=1
      call wrabsf (iob8,v,3*numnp*iprec,0)
      call riosta (iob8)
      call wrabsf (iob8,tcode,numnp*iprec,3*numnp)
      call riosta (iob8)
      do 155 n=1,numsv
      nrts=iparm(1,n)
      nrtm=iparm(2,n)
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      nty=iparm(5,n)
      if (ifo(1,n).gt.0)  then
      ntz=nty
      if (nty.eq.6) ntz=2
      call gtfrcp (slvfrc(1,k4),nsv(k4),a,xms,nsn,x,n,1,tt,
     1 v,tcode,irects(k1),nrts,ntz)
      endif
      if (ifo(2,n).gt.0.and.nrtm.gt.0)  then
      ntz=nty
      if (nty.eq.6) ntz=2
      call gtfrcp (msrfrc(1,k5),msr(k5),a,xms,nmn,x,n,2,tt,
     1 v,tcode,irectm(k6),nrtm,ntz)
      endif
      k1=k1+4*nrts
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+4*nrtm
  155 continue
      call rdabsf (iob8,v,3*numnp*iprec,0,ioerr)
      call riosta (iob8)
      call rdabsf (iob8,tcode,numnp*iprec,3*numnp,ioerr)
      call riosta (iob8)
      endif
      endif
c
c     write hsp data or time history file
c
c
      if (mkthf.eq.0) then
      call prtdat (+1,nfegp,nsubgv,u,b,x,mtnum,cm,a)
      else
      call moment (tim,a,v,xms)
      tim(8+numrw+6*nmmat)=dt2
      call datout (b(lc1h),x,v,tt,nwcon,mtype,ieost,cm,auxvec,u,b(lc1s),
     1 a,b(ndthl),b(nsthl),b(nstsl),tim,8+numrw+6*nmmat)
      call prtdat (+1,nfegp,nsubgv,u,b,x,mtnum,cm,a)
      endif
      if (isf.eq.1) then
      ncisf=0
      do 158 i=1,numels
      if (tfail(i).eq.0.) go to 158
      ncisf=ncisf+1
  158 continue
      if (ncisf.ne.0) then
      write (13,730) ncisf,tt
      do 159 i=1,numels
      if (tfail(i).eq.0.) go to 159
      write (13,740) i,tfail(i)
  159 continue
      endif
      endif
      call timing (4)
c
c     write cross section force database
c
  160 if (output) then
      output=.false.
      call outcsd (b(ncs9),numcsd)
      endif
      if (numcsd.gt.0) then
      if (tt+dt2.gt.csdout)then
      output=.true.
      csdout=csdout+csdinc
      call azero (b(ncs9),9*numcsd)
      endif
      endif
      tt=tt+dt2
c
c     update accelerations of rigid body nodal points
c
      do 168 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 168
      lrbn=nrba(n)
      call setvlo (rbcor(1,n),x,v,a,rbv(1,n),dt2,
     1 nrbn(n),nrb(lrbn),xrb,yrb,zrb,axrb,ayrb,azrb)
  168 continue
c
c***  update nodes at shell-solid interface
      if (nblk.ne.0) call sbix(b(ijne),nblk,x,v,a,b)
c
c     update velocity and geometry
c
      if (idrflg.eq.0) go to 171
      do 170 n=1,neq
      v(n)=drfctr*v(n)+dtx*a(n)
  170 x(n)=x(n)+dt2*v(n)
      go to 173
  171 do 172 n=1,neq
      v(n)=v(n)+dtx*a(n)
      x(n)=x(n)+dt2*v(n)
  172 continue
c
  173 if (numels+numelb.eq.0) go to 178
      if (idrflg.eq.0) go to 175
      do 174 n=neqp1,neq2
      v(n)=drfctr*v(n)+dtx*a(n)
  174 continue
      go to 177
  175 do 176 n=neqp1,neq2
      v(n)=v(n)+dtx*a(n)
  176 continue
  177 call yhintg(v(nxx),v(nxx+1),v(nxx+2),x(nxx),x(nxx+1),x(nxx+2),
     1            nbgn,numnp2)
      if (mkthf.eq.0.and.(ndth+nmst.gt.0)) then
      do 179 n=neqp1,neq2
  179 rots(n)=rots(n)+dt2*v(n)
      endif
c
c
c     write restart dump
c
  178 mess='    '
c     call intrup  (mess,nwr,1)                                         ltss
      call intrup  (mess,nwr,1)                                         vax75
      if (ncedit+1.eq.ikedit) mess='sw2.'
c
      if (mess.eq.'?   ') mess='sw2.'
      if (idrflg.ne.0) go to 180
      if (tt.gt.endtim) call femdmp (2)
      if ((mess.eq.'sw1.').or.(mess.eq.'fin ')) call femdmp (2)
      if (mess.eq.'sw3.') call femdmp (1)
c     if (mess.eq.'panic')call panic                                    ltss
      if(dt2.lt.tsmin) then
      write(13,570)
      tsmin=tsmin*.01
      call femdmp(2)
      endif
c
c     write intermittant dump file
c
      idump=idump+1
      if (idump.ge.irdeck) then
      call femdmp (1)
      idump=0
      endif
c
c     write running dump file
c
      jdump=jdump+1
      if (jdump.ge.ncbrrf) then
      call runrsd
      jdump=0
      endif
c
c
  180 xinen=0.
      do 190 i=1,nmmat2
  190 ener(i)=0.0
      dt1=dt2
      dt2=1.0e+6
c
      call ldcst (fval,npc,pld,tt,ilcf)
c
c     update surface tractions
c
      call load (b(lc6),b(lc6+4),b(lc6+5),b(lc6+6),x,fval,pld,npc,
     1 tt,a,a,rhsi,neq2)
c
c     non-reflecting boundary segments
c
      call nbscal (x,v,b(lrb1),b(lrb2),b(lrb3),a,nnrbs)
c
c     concentrated loads
c
      if (numcl.gt.0) then
      call loadcn (x,nod,idirn,ncur,clfac,clfac(1+numcl),fval,a,
     1 a(1+neq),tt,numcl)
      endif
c
c     body force loads
c
      call lodbfe (x,xms,a,fval)
c
c     compute external work
c
      if (iewflg.eq.1) then
      call exwork(b(locaxa),a,v,dt1,neq,exwk)
      call blkcpy (a,b(locaxa),neq)
      endif
c
c     get temperature state from topaz3d if itemp.lt.0
c
      if (itemp.lt.0.and.(idrflg.eq.0.or.ncycle.eq.0))
     1 call tempr (b(ntmp0),b(ntmp0+1),b(ntmp1),b(ntmp1+1
     1 ),tt,numnp,b(itsc),b(itbs),b(itlc),fval)
c
c
c * * * * p r o c e s s    e l e m e n t s    g r o u p s
c
c
c     process solid elements
c
      if (numelh.ne.0) call solide
c
c     process beam elements
c
      if (numelb.ne.0) call beamel
c
c     process shell elements
c
      if (numels.ne.0) call ishlel
c
c     process thick shell elements
c
      if (numelt.ne.0) call tshell
c
c     process discrete elements
c
      if (nmelde.ne.0) then
      call addstf(b(ld01),b(ld02),b(ld03),b(ld04),b(ld05),
     1 nmmtde,nmelde,a,x,v,npc,pld,xms,tcode)
      endif
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      if (ncedit+1.eq.ikedit) mess='    '
c
c     contact - impact  algorithm
c
      if (numsv.eq.0) go to 445
c
      if (ffifp) then
      k4=1
      k5=1
      do 240 n=1,numsv
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      call savdvg (slvfrc(1,k4),nsv(k4),a,nsn)
      call savdvg (msrfrc(1,k5),msr(k5),a,nmn)
      k4=k4+nsn
      k5=k5+nmn
  240 continue
      elseif (pifo) then
      k4=1
      k5=1
      pifo=.false.
      do 250 n=1,numsv
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      if (ifo(1,n).gt.0)  then
      pifo=.true.
      call savdvg (slvfrc(1,k4),nsv(k4),a,nsn)
      endif
      if (ifo(2,n).gt.0)  then
      pifo=.true.
      call savdvg (msrfrc(1,k5),msr(k5),a,nmn)
      endif
      k4=k4+nsn
      k5=k5+nmn
  250 continue
      endif
c
      k0=1
      k1=1
      k2=1
      k3=1
      k4=1
      k5=1
      k6=1
      k7=1
      k8=1
      k9=1
      k10=1
      k11=1
      k12=1
      k13=1
      k14=1
      k15=1
c
      do 440 n=1,numsv
      nrts=iparm(1,n)
      nrtm=iparm(2,n)
      nsn=iparm(3,n)
      nmn=iparm(4,n)
      nty=iparm(5,n)
      nst=iparm(6,n)
      mst=iparm(7,n)
      isrch=iparm(8,n)
      if (isrch.eq.0) thk=1.e10
      fcoeff=fric(1,n)+fric(2,n)
c
      if (nty.lt.3.or.nty.eq.6.or.nty.eq.7) go to 430
c
c     check for deleted elements
c     and delete any contact surfaces
c
      if (isf.ne.0)    then
      if (numels.ne.0) then
      do 260 nel=1,numels
      if (tfail(nel).lt.0.0) then
      call dslnsg (b(lc1s),irects(k1),stfs(k9), nrts,nel)
      call dslnsg (b(lc1s),irectm(k6),stfm(k10),nrtm,nel)
      tfail(nel)=abs(tfail(nel))
      endif
  260 continue
      endif
      endif
c
      if (nty.eq.4) go to 425
c
      call slave1 (x,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4),irtlm
     1 (k4),nsegm(k7),nsn,nmn)
      if (nty.ne.8) then
      if (nty.lt.5.or.nty.eq.9) then
      call slave1 (x,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5),irtls
     1 (k5),nsegs(k2),nmn,nsn)
      endif
      if (nty.ne.9) then
      call slave2 (x,a,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4)
     1 ,irtlm(k4),stfm(k10),nsn,nmn,nty,fdat(1,k11),iseg(k11),fric(1,n),
     1 nsegm(k7),thkmsr(k10))
      else
      call slavf2 (x,a,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4)
     1 ,irtlm(k4),stfm(k10),nsn,nmn,nty,fdat(1,k11),iseg(k11),fric(1,n),
     1 nsegm(k7),fl9s(1,k14),crst(1,k0))
      k14=k14+nsn
      endif
      endif
      if (nty.eq.8) then
      call slavf2 (x,a,irectm(k6),lmsr(k8),msr(k5),nsv(k4),ilocs(k4)
     1 ,irtlm(k4),stfm(k10),nsn,nmn,nty,fdat(1,k11),iseg(k11),fric(1,n),
     1 nsegm(k7),fails(k12),crst(1,k0))
      endif
      if (fcoeff.ne.0.) k11=k11+nsn
      if (nty.lt.5.or.nty.eq.9) then
      call slave2 (x,a,irects(k1),lnsv(k3),nsv(k4),msr(k5),ilocm(k5)
     1 ,irtls(k5),stfs(k9),nmn,nsn,nty,fdat(1,k11),iseg(k11),fric(1,n),
     1 nsegs(k2),thkslv(k9))
      endif
      if (fcoeff.ne.0.) k11=k11+nmn
      go to 430
c
  425 call sscntc(e,x,a,irects(k1),lnsv(k3),nsv(k4),ilocs(k4),nrts,
     1 irtlm(k4),nsegs(k2),stfs(k9),ethik(k4),chrlen(n),lpntbk,lbcket,
c    2 nsn,fdat(1,k11),iseg(k11),fric(1,n),icls(k15),issflg,            cray1
     2 nsn,fdat(1,k11),iseg(k11),fric(1,n),icls(1,k15),issflg,          vax75
     3 thkslv(k9))
      if (fcoeff.ne.0.) k11=k11+nsn
      k15=k15+nsn
c
  430 k1=k1+4*nrts
      k2=k2+1+nsn
      k3=k3+nst
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+4*nrtm
      k7=k7+1+nmn
      k8=k8+mst
      k9=k9+nrts
      k10=k10+nrtm
      if (nty.ge.5) k12=k12+5*nsn
      if (nty.eq.2.or.nty.gt.5) then
      k0=k0+nsn
      endif
  440 continue
c
  445 if (njt.ne.0) call rbjcon(x,a)
c
c     1d slidelines
c
      if (numsl(1).ne.0) then
      call bmslid(x,a)
      endif
c
c
c***  shell-solid interface force addition
      if (nblk.ne.0) call sbif(b(ijne),nblk,x,a,b)
c
c     sum force and moment resultants on rigid bodies
c
      call cnstrs(x,xyzkcn,a,bcs,ncflag)
c
      call azero (rba,6*nmmat)
c
      hmax=1.e20
      if (lctm.gt.0) hmax=fval(lctm)
      dt2=tssfac*dt2
      dt2=  min(dt2,1.1*dt2old,hmax)
      dt2old=dt2
      ncycle=ncycle+1
      do 470 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 470
      if (rbm(n).ne.0.0) then
      lrbn=nrba(n)
      call sumupb (rba(1,n),rbcor(1,n),x,a,xrb,yrb,zrb,axrb,ayrb,
     1 azrb,rbfx,rbfy,rbfz,nrbn(n),nrb(lrbn))
c
c     update rigid body parameters
c
      call rbudat (rbi(1,1,n),rbm(n),rbcor(1,n),rbu(1,n),
     1 rbv(1,n),rba(1,n),rbcods(1,n),dt1,dt2,bcs)
c
      endif
  470 continue
c
      go to 10
c
  560 format(/////' initial time step by element number',/,
     &            '     controlling element is in square brackets')
  570 format(///' minimum time step size reached')
  670 format('  time=',e11.4,' dt=',e11.4,' k.e.=',e11.4/
     1       '  i.e.=',e11.4,' te=',e11.4,' e.w.=',e11.4/
     2       '    xm=',e11.4,' ym=',e11.4,'   zm=',e11.4/)
  680 format(/' dt of time step',i8,' is controlled by ',
     1        a6,' element',i6/)
  690 format(' time per element-step=',i6,' microseconds'/)
  700 format(/' plotstate dumped at time = ',e14.5,//)
  710 format(/' initialization by dynamic relaxation'/)
  720 format(/' initialization complete at'/
     1        '    pseudo-time = ',e11.4/
     2        '    step = ',i8//)
  730 format(/' f a i l e d   s h e l l   e l e m e n t   r e p o r t',/
     1   '      number of failed elements =',i5,'  at time=',e13.3,///)
  740 format ('           element # ',i6,'   failed at time ',e13.3)
      end
