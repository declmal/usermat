      subroutine sl1ds2 (last, ssm)
c     implicit double precision (a-h,o-z)                                    dp
      dimension ssm(*)
      parameter (nel = 128)
      common /aux14 /  y1(nel), y2(nel), y3(nel),xa1(nel),xa2(nel),
     .                 xa3(nel),xb1(nel),xb2(nel),xb3(nel), xn1(nel),
     .                 xn2(nel),xn3(nel), ss(nel),d(nel),dmax(nel),
     .                 hpa(nel),hpb(nel),pf(nel),press(nel),
     .                 xbxa1(nel),xbxa2(nel),xbxa3(nel),xbxal(nel),
     .                 yxa1(nel),yxa2(nel),yxa3(nel),xbxali(nel),
     .                 ydist1(nel),ydist2(nel),ydist3(nel),
     .                 f1(nel),f2(nel),f3(nel),fa1(nel),fa2(nel),
     .                 fa3(nel),fb1(nel),fb2(nel),fb3(nel)
c
c     . given a point y and two points xa and xb, this subroutine
c       calculates:
c       1) the unitary vector parallel to the vector xb-xa,
c       2) the curvilinear coordinate s of the projection of point y
c          along the axis aligned with vector xb-xa with origin in xa
c          and scale 1 = | xb-xa |
c          s = [ (y-xa) * (xb-xa) ] / [ (xb-xa) * (xb-xa) ]
c       3) the distance vector of y from line (xb-xa)
c
      do 10 i = 1, last
        xbxa1(i) = xb1(i) - xa1(i)
        xbxa2(i) = xb2(i) - xa2(i)
        xbxa3(i) = xb3(i) - xa3(i)
        xbxal(i) = xbxa1(i)*xbxa1(i) + xbxa2(i)*xbxa2(i) +
     .              xbxa3(i)*xbxa3(i)
        xbxal(i) = sqrt(xbxal(i))
        xbxali(i) = 1. / (xbxal(i) + 1.e-10)
        yxa1(i) = y1(i) - xa1(i)
        yxa2(i) = y2(i) - xa2(i)
        yxa3(i) = y3(i) - xa3(i)
        xn1(i) = xbxa1(i) * xbxali(i)
        xn2(i) = xbxa2(i) * xbxali(i)
        xn3(i) = xbxa3(i) * xbxali(i)
        ss(i) = (yxa1(i)*xn1(i) + yxa2(i)*xn2(i) + yxa3(i)*xn3(i))
     .          * xbxali(i)
        ydist1(i) = yxa1(i) - ss(i) * xbxa1(i)
        ydist2(i) = yxa2(i) - ss(i) * xbxa2(i)
        ydist3(i) = yxa3(i) - ss(i) * xbxa3(i)
        d(i) = (ss(i) - ssm(i)) * xbxal(i)
        ssm(i) = ss(i)
   10 continue
      return
      end
      subroutine sl1ds3 (x, nsv, msr, ssm, ilocs, i, is)
c     implicit double precision (a-h,o-z)                                    dp
      parameter (nel = 128)
      dimension x(3,*), nsv(*), msr(*), ssm(*), ilocs(*)
      common /aux14 /  y1(nel), y2(nel), y3(nel),xa1(nel),xa2(nel),
     .                 xa3(nel),xb1(nel),xb2(nel),xb3(nel), xn1(nel),
     .                 xn2(nel),xn3(nel), ss(nel),d(nel),dmax(nel),
     .                 hpa(nel),hpb(nel),pf(nel),press(nel),
     .                 xbxa1(nel),xbxa2(nel),xbxa3(nel),xbxal(nel),
     .                 yxa1(nel),yxa2(nel),yxa3(nel),xbxali(nel),
     .                 ydist1(nel),ydist2(nel),ydist3(nel),
     .                 f1(nel),f2(nel),f3(nel),fa1(nel),fa2(nel),
     .                 fa3(nel),fb1(nel),fb2(nel),fb3(nel)
c     --  update vector n and coordinate ss for slave node ss  --
      js  = nsv(is)
      im  = ilocs(is)
      jma = msr(im)
      jmb = msr(im+1)
      xbxa1(i) = x(1,jmb) - x(1,jma)
      xbxa2(i) = x(2,jmb) - x(2,jma)
      xbxa3(i) = x(3,jmb) - x(3,jma)
      xbxal(i) = xbxa1(i)*xbxa1(i) + xbxa2(i)*xbxa2(i) +
     .           xbxa3(i)*xbxa3(i)
      xbxal(i) = sqrt(xbxal(i))
      xbxali(i) = 1. / (xbxal(i) + 1.e-10)
      xn1(i) = xbxa1(i) * xbxali(i)
      xn2(i) = xbxa2(i) * xbxali(i)
      xn3(i) = xbxa3(i) * xbxali(i)
      yxa1(i) = y1(i) - x(1,jma)
      yxa2(i) = y2(i) - x(2,jma)
      yxa3(i) = y3(i) - x(3,jma)
      ss(i) = (yxa1(i)*xn1(i) + yxa2(i)*xn2(i) + yxa3(i)*xn3(i)) *
     .        xbxali(i)
      ydist1(i) = yxa1(i) - ss(i) * xbxa1(i)
      ydist2(i) = yxa2(i) - ss(i) * xbxa2(i)
      ydist3(i) = yxa3(i) - ss(i) * xbxa3(i)
      ssm(is) = ss(i)
      return
      end
      subroutine rstart
c     implicit double precision (a-h,o-z)                                    dp
c***********************************************************************
c                                                                      *
c  ******   *******   ******  *******    ***    ******   *******       *
c   *    *  *        *           *      *   *    *    *     *          *
c   *    *  *        *           *     *     *   *    *     *          *
c   *****   ****      *****      *     *******   *****      *          *
c   *  *    *              *     *     *     *   *  *       *          *
c   *   *   *              *     *     *     *   *   *      *          *
c   *    *  *******  ******      *     *     *   *    *     *          *
c                                                                      *
c***********************************************************************
c
c     restart from last dump
c
      common/bk00/numnp(1),numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk01/itherm(1),itemp,ntmp0,ntmp1
      common/bk02/iburn(1),dt1,dt2,isdo
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout(1),pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      common/bk05/
     1 nh01(1),nh02,nh03,nh04,nh05,nh06,nh07,nh08,nh09,nh10,
     2 nb01,nb02,nb03,nb04,nb05,nb06,nb07,nb08,nb09,nb10,
     3 ns01,ns02,ns03,ns04,ns05,ns06,ns07,ns08,ns09,ns10,
     4 nt01,nt02,nt03,nt04,nt05,nt06,nt07,nt08,nt09,nt10
      real*8 head                                                       vax75
      common/bk06/idmmy,iaddp(1),ifil,maxsiz,head(12),ncycle,time(2,8)
      common/bk07/n1(1),n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,
     1 n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,
     2 n32,n33,n34,n35,n36,n37,n38,n39,n40,n41,n42,n43,n44,n45,
     3 n46,n47,n48,n49,n50,n51,n52,n53,n54,n55,n56,n57,n58,n59,n60,n61,
     4 n62,n63,n64,n65,n66,n67,n68,n69,n70,n71,n72,n73,n74,n75,n76,n77,
     5 n78,n79,n80,locend,iname,lendf
      common/bk08/n4a(1),n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/b10a/inpsd(1),nmmtde,nmelde,nmmass
      common/b10b/ld01(1),ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk16/numcc(1),numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk20/numsv(1),ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk26/nint(1)
      common/bk27/irestr(1),irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss(1),xke,xpe,tt
      common/bk76/nthpx(1),nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax(1),gay,gaz,slx,sly,slz
      common/bk78/njoydf(1),nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/thfl/madd(1),ifth,maddbg
      common/bttn/ntnwf(1),ixa(10)
      common/bktb/ntbsl(1),nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/sbc0/yield(1),hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal(1),shob,range(64),stoa(64),pkp(64)
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/beam/ pid4(1),xi,gw,eta(5,16,2),zeta(5,16,2),weight(5,16,2)
      common/rn/irnxx(1)
      common/tsbsis/h(8,5,5),pr(8,5,5),ps(8,5,5),pt(8,5,5),ipt,
     1 nip,wgts(5,5),szeta(5,5)
      common/ssbsis/sh(8,5,5),spr(8,5,5),sps(8,5,5),spt(8,5,5),ipts,
     1 nips,swgts(5,5),sszeta(5,5)
      common/rigidb/ nodtot(1), ncflag,numrbi
      common/shlopt/istrn(1),istupd,ibelyt,miter
      common/strhx/nmsi(1),matsi(3,400),mx,ix(8),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/csforc/ncs1(1),ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      logical ffifp
      common/slfl/nadd(1),numsls,ffifp
      common/tmstp/tsmin(1),hmax,lctm
      common/tint/itsc(1),itbs,itlc
      common/tbsysp/numrcf(1),n6e,n6f,n6g
      common/bmsc/numsl(20)
      common/raydmp/dampm,ndamp
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      common/extwrk/locaxa,exwork,iewflg
      common/tsarry/tslimt,tsarry(144)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/fixnod/numfx1,numfx2,numfx3,numfx4,numfx5,numfx6
     1 ,numfx7,numfx8,numfx9,numfx10,numfx11,numfx12
      common/double/iprec,ncpw,unit
      character*8 namef,rname,keep                                      vax75
c
      common /   / b(1)
c
c     set restart flag
c
      irestr(1)=1
c
c     write restart file name to keep user happy
c
      call getnam  (3,namef)
      write ( *,30) namef
c
      call famnum(namef,ifam,rname)                                     vax75
c
      iadd=ifam*maxsiz                                                  vax75
      keep='keep'                                                       vax75
      call rwabsf (iob3,keep,0,0,0)                                     vax75
      ifsiz=2621440                                                     unics
c     ifsiz=262144                                                      wkstn
c     ifsiz=262144                                                      vms
      call rwabsf(iob3, rname, 3, 512, ifsiz)                           vax75
c
c     read common block data
c
c     iadd=0                                                            ltss
      iaddsv=iadd
      len=19+1*iprec
      call rdabsf(iob3,numnp,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,itherm,4,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+4
      len=2+2*iprec
      call rdabsf(iob3,iburn,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=6+3*iprec
      call rdabsf(iob3,ndthl,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=8+5*iprec
      call rdabsf(iob3,prtout,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,nh01,40,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+40
c     len=32                                                            cray1
      len=28+16*iprec                                                   vax75
      call rdabsf(iob3,iaddp,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,n1,83,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+83
      call rdabsf(iob3,n4a,14,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+14
      call rdabsf(iob3,lc0,26,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+26
      call rdabsf(iob3,numcc,8,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+8
      call rdabsf(iob3,nconst,76,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+76
      call rdabsf(iob3,numsv,10,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+10
      call rdabsf(iob3,nint,1,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+1
      call rdabsf(iob3,irestr,6,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+6
      len=4*iprec
      call rdabsf(iob3,summss,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,nthpx,6,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+6
      len=6*iprec
      call rdabsf(iob3,gax,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,njoydf,7,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+7
      call rdabsf(iob3,madd,2,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+2
      len=2+9*iprec
      call rdabsf(iob3,yield,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=194*iprec
      call rdabsf(iob3,scal,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=483*iprec
      call rdabsf(iob3,pid4,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=2+850*iprec
      call rdabsf(iob3,h,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=2+850*iprec
      call rdabsf(iob3,sh,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,nodtot,2,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+2
      call rdabsf(iob3,inpsd,4,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+4
      call rdabsf(iob3,ld01,9,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+9
      call rdabsf(iob3,istrn,4,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+4
      call rdabsf(iob3,ntnwf,11,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+11
      call rdabsf(iob3,ntbsl,16,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+16
      call rdabsf(iob3,irnxx,1,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+1
      len=1+2*iprec
      call rdabsf(iob3,tsmin,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,nadd,3,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+3
      call rdabsf(iob3,itsc,3,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+3
      call rdabsf(iob3,numrcf,4,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+4
      call rdabsf(iob3,numsl,20,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+20
      len=1+iprec
      call rdabsf(iob3,dampm,len,iadd,ioerr)
      call riosta(iob3)
      iadd=iadd+len
      len=10+12*iprec
      call rdabsf(iob3,ncs1,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=13+2*iprec
      call rdabsf(iob3,ilink,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      call rdabsf(iob3,nnc,30,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+30
      len=145*iprec
      call rdabsf(iob3,tslimt,len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
      len=2+1*iprec
      call rdabsf(iob3,locaxa,len,iadd,ioerr)
      call riosta(iob3)
      iadd=iadd+len
      call rdabsf(iob3,n30a,7,iadd,ioerr)
      call riosta(iob3)
      iadd=iadd+7
      call rdabsf(iob3,numfx1,12,iadd,ioerr)
      call riosta(iob3)
      iadd=iadd+12
c
c     set correct family size for dumpfile
c
      call famsiz(iob2,lendf)
      call famsiz(iob3,lendf)
c
c     expand large core memory
c
      call expndm (locend+20)
c
c     read blank common
c
      len=locend*iprec
      call rdabsf (iob3,b(1),len,iadd,ioerr)
      call riosta (iob3)
      iadd=iadd+len
c
      write( *,70) iadd-iaddsv
      call getnam  (1,namef)
c
c     if input file name, namef, equals default file name
c     no input file for restarting is expected
c
      if (namef.ne.'xyz123') then
      call rstari(b(ns13),b(nh13),b(nb13),b(nt13),b(lc1h),b(lc1s),
     1  b(lc1b),b(lc1t))
      endif
c
c
c     if restart with stress initialization contract memory
c
      if (nmsi(1).ne.0) then
      call expndm(1)
      nadd(1) =0
      madd(1) =0
      iaddp(1)=0
      endif
c
      return
c
c
   30 format (' restart file',1x,a8,' opened')
   70 format('     ',i8,' words read')
      end
      subroutine rstari (nshpnt,nhxpnt,nbmpnt,ntspnt,ipsh,ipss,
     1  ipsb,ipst)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
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
      common/bk14/mx,ix(20),sig(6),epx
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/aux33/ix1(128),ix2(128),ix3(128),ix4(128),ix5(128),
     1             ix6(128),ix7(128),ix8(128),mxt(128),nmel
      common/nwixa/nwcon
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/strhx/nmsi,matsi(3,400),mxix(9),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/double/iprec,ncpw,unit
      common /   / b(1)
      character*5 formts
      logical large
      common/bigprb/large
      logical dltshl,dltbms,dltths,dltbks
c
      dimension title(12),neb(16),nhxpnt(*),nshpnt(*),nbmpnt(*),
     1 ntspnt(*),ipsh(*),ipss(*),ipsb(*),ipst(*)
      character*80 txts,mssg
      data dltshl,dltbms,dltths,dltbks/.false.,.false.,.false.,.false./
c
      izero=0
      zero=0.0
      call gttxsg (txts,lcount)
      mssg=' error reading title card for restart'
      read (unit=txts,fmt=190,err=138) title,formts
      large=.false.
      if (formts.eq.'large') large=.not.large
c
      call gttxsg (txts,lcount)
      mssg=' error reading control card #1 for restart'
      read (unit=txts,fmt=160,err=138)
     1 ti2,tid,pid,nsle,neblkm,neblke,neblkb,neblks,neblkt
      call gttxsg (txts,lcount)
      mssg=' error reading control card #2 for restart'
      read (unit=txts,fmt=165,err=138)
     1 tssf,ncbcc,nmsi,irdck,ncbrf
c
      if (ti2.ne.0.0) endtim=ti2
      if (tid.ne.0.0) then
      prtout=prtout-prtc+tid
      prtout=  max(tt,prtout)
      prtc=tid
      endif
      if (pid.ne.0.0) then
      pltout=pltout-pltc+pid
      pltout=  max(tt,pltout)
      pltc=pid
      endif
      if (irdck.gt.0) then
      irdeck=irdck
      idump =0
      endif
      if (ncbrf.gt.0) then
      ncbrrf=ncbrf
      jdump =0
      endif
c
      tssf=  min(.90*unit,tssf)
      if (tssf.ne.0.0) tssfac=tssf
c
      write(13,200) title,endtim,prtc,pltc,nsle,neblkm,
     1 neblke,neblkb,neblks,neblkt,tssf,ncbcc,nmsi,irdeck,
     2 ncbrrf
c
c     delete sliding interfaces if nsle.gt.0
c
      if (nsle.gt.0) call slelim (b(n30),nsle)
c
c
c     delete elements by material number
c
      if (neblkm.gt.0) then
      nnline=16
      if(large) nnline=10
      nn=neblkm/nnline
      if (nn*nnline.ne.neblkm) nn=nn+1
      write(13,150)
      mssg=' error reading number of material blocks for deletion'
      do 20 i=1,nn
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=141,err=138) (neb(ii),ii=1,nnline)
      else
      read (unit=txts,fmt=140,err=138) (neb(ii),ii=1,nnline)
      endif
      do 20 j=1,nnline
      mt=neb(j)
      if (mt.eq.0) go to 30
      write(13,180) mt
      if (numelh.gt.0) then
      do 10 m=1,numelh
      k=nhxpnt(m)
      loc=1+nwcon*(k-1)
      call unpk(mx,ix,ipsh(loc),3)
      if (mx.ne.mt) go to 10
      mx=0
      dltbks=.true.
      b(nh15+k-1)=zero
      call pckeri  (mx,ix,ipsh(loc),3)
   10 continue
      go to 20
      endif
      if (numelb.gt.0) then
      do 12 m=1,numelb
      k=nbmpnt(m)
c     loc=1+2*(k-1)                                                     cray1
      loc=1+4*(k-1)                                                     vax75
      call unpk(mx,ix,ipsb(loc),2)
      if (mx.ne.mt) go to 12
      mx=0
      dltbms=.true.
      b(nb15+k-1)=zero
      call pckeri  (mx,ix,ipsb(loc),2)
   12 continue
      go to 20
      endif
      if (numels.gt.0) then
      do 14 m=1,numels
      k=nshpnt(m)
c     loc=1+2*(k-1)                                                     cray1
      loc=1+5*(k-1)                                                     vax75
      call unpk(mx,ix,ipss(loc),2)
      if (mx.ne.mt) go to 14
      mx=0
      dltshl=.true.
      b(ns15+k-1)=zero
      call pckeri  (mx,ix,ipss(loc),2)
   14 continue
      go to 20
      endif
      if (numelt.gt.0) then
      do 16 m=1,numelt
      k=ntspnt(m)
      loc=1+nwcon*(k-1)
      call unpk(mx,ix,ipst(loc),3)
      if (mx.ne.mt) go to 16
      mx=0
      dltths=.true.
      b(nt15+k-1)=zero
      call pckeri  (mx,ix,ipst(loc),3)
   16 continue
      endif
   20 continue
      endif
c
   30 if (neblke.gt.0) then
      nlrg=8
      llrg=16
      if(large) nlrg=4
      if(large) llrg=8
      nn=neblke/nlrg
      if (nn*nlrg.ne.neblke) nn=nn+1
      write(13,150)
      mssg=' error reading # of solid element blocks for deletion'
      do 50 i=1,nn
      call gttxsg (txts,lcount)
      if (large) then
      read (unit=txts,fmt=141,err=138) (neb(ii),ii=1,llrg)
      else
      read (unit=txts,fmt=140,err=138) (neb(ii),ii=1,llrg)
      endif
      dltbks=.true.
      do 50 j=1,nlrg
      kl=neb(2*j-1)
      ku=neb(2*j)
      if (kl.eq.0) go to 60
      nblk=(i-1)*nlrg+j
      write(13,170) nblk,kl,ku
      do 40 m=kl,ku
      k=nhxpnt(m)
      loc=1+nwcon*(k-1)
      call unpk(mx,ix,ipsh(loc),3)
      mx=0
      b(nh15+k-1)=zero
      call pckeri  (mx,ix,ipsh(loc),3)
   40 continue
   50 continue
      endif
c
c     redefine solid element blocks
c
   60 if (dltbks) then
      nelmg=n27-n26
      nsubg=0
      nsubh=0
      call defbrk (b(lc1h),b(n1),b(nh15),nsubg,nsubh,nwcon,numelh)
      n28=n27+nsubg
      call egcryi (b(lc1h),b(n1),b(n26),b(n27),b(n28),nsubg,nelmg,
     1 numelh,nwcon)
      nh12=nh11+nsubh
      call egcryj(b(lc1h),b(nh11),b(nh12),nelmg,numelh,b(nh15),nwcon)
      endif
c
c     delete beam elements by blocks
c
      if (neblkb.gt.0) then
      nlrg=8
      llrg=16
      if(large) nlrg=4
      if(large) llrg=8
      nn=neblkb/nlrg
      if (nn*nlrg.ne.neblkb) nn=nn+1
      write(13,150)
      mssg=' error reading # of beam element blocks for deletion'
      dltbms=.true.
      do 66 i=1,nn
      call gttxsg (txts,lcount)
      if (large) then
      read (unit=txts,fmt=141,err=138) (neb(ii),ii=1,llrg)
      else
      read (unit=txts,fmt=140,err=138) (neb(ii),ii=1,llrg)
      endif
      do 66 j=1,nlrg
      kl=neb(2*j-1)
      ku=neb(2*j)
      if (kl.eq.0) go to 68
      nblk=(i-1)*nlrg+j
      write(13,170) nblk,kl,ku
      do 64 m=kl,ku
      k=nbmpnt(m)
c     loc=1+2*(k-1)                                                     cray1
      loc=1+4*(k-1)                                                     vax75
      call unpk(mx,ix,ipsb(loc),2)
      mx=0
      b(nb15+k-1)=zero
      call pckeri  (mx,ix,b(loc),2)
   64 continue
   66 continue
      endif
c
c     redefine beam element blocks
c
   68 if (dltbms) then
      nelms=nb09-nb08
      nsubb=0
      nsubc=0
c     call defbrk (b(lc1b),b(n1),b(nb15),nsubb,nsubc,2,numelb)          cray1
      call defbrk (b(lc1b),b(n1),b(nb15),nsubb,nsubc,4,numelb)          vax75
      nb10=nb09+nsubb
      nb12=nb11+nsubc
      call egcryi (b(lc1b),b(n1),b(nb08),b(nb09),b(nb10),nsubb,nelms,
c    2 numelb,2)                                                        cray1
     2 numelb,4)                                                        vax75
c     call egcryj(b(lc1b),b(nb11),b(nb12),nelms,numelb,b(nb15),2)       cray1
      call egcryj(b(lc1b),b(nb11),b(nb12),nelms,numelb,b(nb15),4)       vax75
      endif
c
c     delete shell elements by blocks
c
      if (neblks.gt.0) then
      nlrg=8
      llrg=16
      if(large) nlrg=4
      if(large) llrg=8
      nn=neblks/nlrg
      if (nn*nlrg.ne.neblks) nn=nn+1
      write(13,150)
      mssg=' error reading # of shell element blocks for deletion'
      dltshl=.true.
      do 80 i=1,nn
      call gttxsg (txts,lcount)
      if (large) then
      read (unit=txts,fmt=141,err=138) (neb(ii),ii=1,llrg)
      else
      read (unit=txts,fmt=140,err=138) (neb(ii),ii=1,llrg)
      endif
      do 80 j=1,nlrg
      kl=neb(2*j-1)
      ku=neb(2*j)
      if (kl.eq.0) go to 90
      nblk=(i-1)*nlrg+j
      write(13,170) nblk,kl,ku
      do 70 m=kl,ku
      k=nshpnt(m)
c     loc=1+2*(k-1)                                                     cray1
      loc=1+5*(k-1)                                                     vax75
      call unpk(mx,ix,ipss(loc),2)
      if (mx.ne.0) mx=mx+1000
      b(ns15+k-1)=zero
      call pckeri  (mx,ix,ipss(loc),2)
   70 continue
   80 continue
      endif
c
c     redefine shell element blocks
c
   90 if (dltshl) then
      nelms=ns09-ns08
      nsubs=0
      nsubt=0
c     call defbrk (b(lc1s),b(n1),b(ns15),nsubs,nsubt,2,numels)          cray1
      call defbrk (b(lc1s),b(n1),b(ns15),nsubs,nsubt,5,numels)          vax75
      ns10=ns09+nsubs
      call egcryi (b(lc1s),b(n1),b(ns08),b(ns09),b(ns10),nsubs,nelms,
c    2 numels,2)                                                        cray1
     2 numels,5)                                                        vax75
      ns12=ns11+nsubt
c     call egcryj(b(lc1s),b(ns11),b(ns12),nelms,numels,b(ns15),2)       cray1
      call egcryj(b(lc1s),b(ns11),b(ns12),nelms,numels,b(ns15),5)       vax75
      endif
c
c
c     delete thick shell elements by blocks
c
      if (neblkt.gt.0) then
      nlrg=8
      llrg=16
      if(large) nlrg=4
      if(large) llrg=8
      nn=neblkt/nlrg
      if (nn*nlrg.ne.neblkt) nn=nn+1
      write(13,150)
      mssg=' error reading # of shell element blocks for deletion'
      dltths=.true.
      do 100 i=1,nn
      call gttxsg (txts,lcount)
      if (large) then
      read (unit=txts,fmt=141,err=138) (neb(ii),ii=1,llrg)
      else
      read (unit=txts,fmt=140,err=138) (neb(ii),ii=1,llrg)
      endif
      do 100 j=1,nlrg
      kl=neb(2*j-1)
      ku=neb(2*j)
      if (kl.eq.0) go to 110
      nblk=(i-1)*nlrg+j
      write(13,170) nblk,kl,ku
      do 95 m=kl,ku
      k=ntspnt(m)
      loc=1+nwcon*(k-1)
      call unpk(mx,ix,ipst(loc),2)
      if (mx.ne.0) mx=mx+1000
      b(nt15+k-1)=zero
      call pckeri  (mx,ix,ipst(loc),2)
   95 continue
  100 continue
      endif
c
c     redefine thick shell element blocks
c
  110 if (dltths) then
      nelms=nt02-nt01
      nsubs=0
      nsubt=0
      call defbrk (b(lc1t),b(n1),b(nt15),nsubs,nsubt,nwcon,numelt)
      nt03=nt02+nsubs
      call egcryi (b(lc1t),b(n1),b(nt01),b(nt02),b(nt03),nsubs,
     2 nelms,numelt,nwcon)
      nt12=nt11+nsubt
      call egcryj(b(lc1t),b(nt11),b(nt12),nelms,numelt,b(nt15),nwcon)
      endif
c
  120 if (ncbcc.ne.0) then
      nn=(ncbcc-1)/16+1
      write(13,220)
      mssg=' error reading number of changed boundary cond. codes'
      do 130 i=1,ncbcc
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=215,err=138)  node,tcoden
      else
      read(unit=txts,fmt=210,err=138)  node,tcoden
      endif
      write(13,230) node,tcoden
      call fxnode(node,tcoden,b(lc7),b(lc9),b(n7))
  130 continue
      endif
c
      if (nmsi.ne.0) then
      do 134 i=1,nmmat
      matsi(1,i)=0
      matsi(2,i)=0
      matsi(3,i)=0
  134 continue
      if (nmsi.gt.0) then
      write(13,240)
      mssg='error reading number of materials for stress initialization'
      do 135 i=1,nmsi
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=140,err=138) matold,matnew
      write(13,250) matold,matnew
      matsi(1,matnew)=matold
  135 continue
      else
      nmsi=nmmat
      do 137 i=1,nmsi
      matsi(1,i)=i
  137 continue
      endif
      call wrtsif (b,b(lc11),b(lc9),b(n1),b(n4a),b(ns01),b(ns03),
     1 b(ns05),b(ns06),b(n4f),b(ns07),b(nb03),b(nb05),b(nb06),
     2 b(nt04),b(nt05),ipsh,ipss,ipsb,ipst)
      endif
c
      return
c
  138 call termin (txts,mssg,lcount,1)
  140 format(16i5)
  141 format(10i8)
  150 format('1',//' e l i m i n a t e d   e l e m e n t s')
  160 format(3e10.0,6i5)
  165 format(e10.0,4i5)
  170 format(//'  block ' ,i2,//
     1  4x,'first element in this block....................',i4//
     2  4x,'last  element in this block....................',i4/)
  180 format(//' all material',i4,' elements eliminated'/)
  190 format(12a6,3x,a5)
  200 format(///12a6//
     $  4x,'termination time...............................',e10.2//
     $  4x,'time step between dumps of time history data...',e10.2//
     $  4x,'time step between dumps of complete state data.',e10.2//
     $  4x,'number of slide-lines to be eliminated.........',   i5//
     $  4x,'number of materials to be eliminated...........',   i5//
     $  4x,'# of solid element blocks to be eliminated.....',   i5//
     $  4x,'# of beam element blocks to be eliminated......',   i5//
     $  4x,'# of shell element blocks to be eliminated.....',   i5//
     $  4x,'# of thick shell blocks to be eliminated.......',   i5//
     $  4x,'time step scale factor.........................',e10.2//
     $  4x,'# of changed boundary condition tcodes..........',  i5//
     $  4x,'# of materials to be initialized (all=-1).......',  i5//
     $  4x,'# of time steps between restart files...........',  i7//
     $  4x,'# of time steps between running restart dumps...',  i7/)
  210 format(i5,f5.0)
  215 format(i8,f5.0)
  220 format(//'c h a n g e d   b o u n d a r y   c o n d i t i o n s'/)
  230 format('     node number',i6,' has new boundary tcode',f8.0)
  240 format(///' m a t e r i a l   i n i t i a l i z a t i o n',/)
  250 format(/'     material',i3,' in restart file initializes',/,
     1        '     material',i3,' in new input.'/)
      end
      subroutine wrtsif(b,x,v,mtype,ieost,strain,yhtns,fibls,
     1 auxvcs,csprop,xies,yhtnb,fiblb,auxvcb,auxvct,thks,
     2 ipsh,ipss,ipsb,ipst)
c     implicit double precision (a-h,o-z)                                    dp
c
c     stress initialization data packed on disk
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk19/nconst(60),lenma,ncneos(15)
      common/nwixa/nwcon
      common/strhx/nmsi,matsi(3,400),mx,ix(8),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/strsh/my,iy(5),xs(6,4),vs(6,4),yhat(12),fibs(9),strns(12),
     1 xiess,hisv(1000)
      common/strbm/mxb,ixb(3),xb(6,2),vb(6,2),yhtb(12),fibb(5),
     1 hisvbm(200)
      common/strts/mxt,ixt(8),xt(3,8),vt(3,8),thk(5),hisvts(120)
      common/shlopt/istrn,istupd,ibelyt,miter
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vax75
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vax75
     &       iob14(1080),iob15(1080),iob16(1080)                        vax75
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/double/iprec,ncpw,unit
      dimension b(*),x(3,*),v(3,*),mtype(*),ieost(*),strain(12,*),
     1 yhtns(12,*),fibls(9,*),auxvcs(*),csprop(24,*),xies(*),
     2 fiblb(5,*),auxvcb(*),ni2a(6),auxvct(*),thks(5,*),ipsh(*),ipss(*),
     3 ipsb(*),ipst(*),yhtnb(12,*)
      data ni2a/1,4,9,9,16,25/
c
c     solid elements
c
      if (numelh.ne.0) then
      do 10 nn=1,numelh
      l11=1+nwcon*(nn-1)
      call unpk(mx,ix,ipsh(l11),3)
      if (mx.eq.0) go to 10
      mtyp=mtype(mx)
      nes=ieost(mx)
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) ncon=0
      matsi(3,mx+1)=matsi(3,mx+1)+ncon+57
   10 continue
      endif
c
c     shell elements
c
      if (numels.ne.0) then
      do 20 nn=1,numels
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+5*(nn-1)                                                    vax75
      call unpk(mx,ix,ipss(l11),2)
      if (mx.eq.0) go to 20
      nip=nint(csprop(2,mx))
      mtyp=mtype(mx)
      nes=ieost(mx)
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) ncon=0
      matsi(3,mx+1)=matsi(3,mx+1)+nip*ncon+88
   20 continue
      endif
c
c     beam elements
c
      if (numelb.ne.0) then
      do 30 nn=1,numelb
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+4*(nn-1)                                                    vax75
      call unpk(mx,ix,ipsb(l11),2)
      if (mx.eq.0) go to 30
      nip=nint(csprop(2,mx))
      mtyp=mtype(mx)
      nes=ieost(mx)
      if (csprop(4,mx).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) ncon=0
      matsi(3,mx+1)=matsi(3,mx+1)+npt*ncon+45
   30 continue
      endif
c
c     thick shell elements
c
      if (numelt.ne.0) then
      do 35 nn=1,numelt
      l11=1+nwcon*(nn-1)
      call unpk(mx,ix,ipst(l11),3)
      if (mx.eq.0) go to 35
      mtyp=mtype(mx)
      nes=ieost(mx)
      nip=nint(csprop(2,mx))
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) ncon=0
      matsi(3,mx+1)=matsi(3,mx+1)+nip*ncon+62
   35 continue
      endif
c
c     compute starting disk address
c
      do 40 i=1,nmmat
      matsi(3,i+1)=matsi(3,i)+matsi(3,i+1)
   40 matsi(2,i+1)=matsi(3,i+1)
c
      if (numelh.ne.0) then
      lavold=lc15
      do 60 nn=1,numelh
      l11=1+nwcon*(nn-1)
      call unpk(mx,ix,ipsh(l11),3)
      if (mx.eq.0) go to 60
      do 50 i=1,8
      ve(1,i)=v(1,ix(i))
      ve(2,i)=v(2,ix(i))
      ve(3,i)=v(3,ix(i))
      xe(1,i)=x(1,ix(i))
      xe(2,i)=x(2,ix(i))
      xe(3,i)=x(3,ix(i))
   50 continue
      mtyp=mtype(mx)
      nes=ieost(mx)
      ncon=7+nconst(mtyp)+ncneos(max0(nes,1))
      if (mtyp.eq.20) then
      ncon=0
      else
      call blkcpy(b(lavold),hisvar,ncon)
      lavold=lavold+ncon
      endif
      ncon=ncon*iprec+9+48*iprec
      call wrabsf(iob8,mx,ncon,matsi(2,mx))
      call riosta (iob8)
      matsi(2,mx)=matsi(2,mx)+ncon
   60 continue
      endif
c
      if (numels.ne.0) then
      lavold=1
      do 80 nn=1,numels
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+5*(nn-1)                                                    vax75
      call unpk(my,iy,ipss(l11),2)
      if (my.eq.0) go to 80
      do 70 i=1,4
      vs(1,i)=v(1,iy(i))
      vs(2,i)=v(2,iy(i))
      vs(3,i)=v(3,iy(i))
      vs(4,i)=v(1,iy(i)+numnp)
      vs(5,i)=v(2,iy(i)+numnp)
      vs(6,i)=v(3,iy(i)+numnp)
      xs(1,i)=x(1,iy(i))
      xs(2,i)=x(2,iy(i))
      xs(3,i)=x(3,iy(i))
      xs(4,i)=x(1,iy(i)+numnp)
      xs(5,i)=x(2,iy(i)+numnp)
      xs(6,i)=x(3,iy(i)+numnp)
   70 continue
      mtyp=mtype(my)
      nes=ieost(my)
      nip=nint(csprop(2,my))
      ncon=nip*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      else
      call blkcpy(auxvcs(lavold),hisv,ncon)
      lavold=lavold+ncon
      endif
      call blkcpy(strain(1,nn),strns,12)
      call blkcpy(yhtns(1,nn),yhat,12)
      call blkcpy(fibls(1,nn),fibs,9)
      xiess=xies(nn)
      ncon=ncon*iprec+7+81*iprec
      call wrabsf(iob8,my,ncon,matsi(2,my))
      call riosta (iob8)
      matsi(2,my)=matsi(2,my)+ncon
   80 continue
      endif
      if (numelb.ne.0) then
      lavold=1
      do 100 nn=1,numelb
c     l11=1+2*(nn-1)                                                    cray1
      l11=1+4*(nn-1)                                                    vax75
      call unpk(mxb,ixb,ipsb(l11),2)
      if (mxb.eq.0) go to 100
      do 90 i=1,2
      vb(1,i)=v(1,ixb(i))
      vb(2,i)=v(2,ixb(i))
      vb(3,i)=v(3,ixb(i))
      vb(4,i)=v(1,ixb(i)+numnp)
      vb(5,i)=v(2,ixb(i)+numnp)
      vb(6,i)=v(3,ixb(i)+numnp)
      xb(1,i)=x(1,ixb(i))
      xb(2,i)=x(2,ixb(i))
      xb(3,i)=x(3,ixb(i))
      xb(4,i)=x(1,ixb(i)+numnp)
      xb(5,i)=x(2,ixb(i)+numnp)
      xb(6,i)=x(3,ixb(i)+numnp)
   90 continue
      mtyp=mtype(mxb)
      nes=ieost(mxb)
      nip=nint(csprop(2,mxb))
      if (csprop(4,mxb).ge.0.0) then
      npt=ni2a(nip)
      else
      npt=nip
      endif
      ncon=npt*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      else
      call blkcpy(auxvcb(lavold),hisvbm,ncon)
      lavold=lavold+ncon
      endif
      call blkcpy(yhtnb(1,nn),yhtb,12)
      call blkcpy(fiblb(1,nn),fibb,5)
      ncon=4+41*iprec+iprec*ncon
      call wrabsf(iob8,mxb,ncon,matsi(2,mxb))
      call riosta (iob8)
      matsi(2,mxb)=matsi(2,mxb)+ncon
  100 continue
      endif
c
      if (numelt.ne.0) then
      lavold=1
      do 120 nn=1,numelt
      l11=1+nwcon*(nn-1)
      call unpk(mxt,ixt,ipst(l11),3)
      if (mxt.eq.0) go to 120
      do 110 i=1,8
      vt(1,i)=v(1,ixt(i))
      vt(2,i)=v(2,ixt(i))
      vt(3,i)=v(3,ixt(i))
      xt(1,i)=x(1,ixt(i))
      xt(2,i)=x(2,ixt(i))
      xt(3,i)=x(3,ixt(i))
  110 continue
      mtyp=mtype(mxt)
      nes=ieost(mxt)
      nip=nint(csprop(2,mxt))
      ncon=nip*(7+nconst(mtyp)+ncneos(max0(nes,1)))
      if (mtyp.eq.20) then
      ncon=0
      else
      call blkcpy(auxvct(lavold),hisvts,ncon)
      lavold=lavold+ncon
      endif
      call blkcpy(thks(1,nn),thk,5)
      ncon=9+iprec*53+iprec*ncon
      call wrabsf(iob8,mxt,ncon,matsi(2,mxt))
      call riosta (iob8)
      matsi(2,mxt)=matsi(2,mxt)+ncon
  120 continue
      endif
c
      do 130 i=1,nmmat
  130 matsi(2,i)=matsi(3,i)
      return
      end
      subroutine fxnode(node,tcoden,tcode,v,bcs)
c     implicit double precision (a-h,o-z)                                    dp
      dimension tcode(1),v(3,1),bcs(4,1)
      tcode(node)=tcoden
      ib=tcoden
      if (ib.ne.0) call bound(bcs,v(1,node),v(2,node),v(3,node),ib)
      return
      end
      subroutine slelim (iparm,nsle)
c     implicit double precision (a-h,o-z)                                    dp
c
c     zero 'iparm' array to denote deleted slide-lines
c
      dimension iparm(8,1),nls(48)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      nnds=16
      mssg=' error reading deleted sliding interfaces'
      if (large) nnds=10
      nsleb=(nsle-1)/nnds+1
      lfst=1
      llst=nnds
      do 8 j=1,nsleb
      if (nsle.eq.0) go to 20
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=31,err=24) (nls(i),i=lfst,llst)
      else
      read (unit=txts,fmt=30,err=24) (nls(i),i=lfst,llst)
      endif
      lfst=lfst+nnds
      llst=llst+nnds
    8 continue
      write(13,40) (nls(i),i=1,nsle)
      do 10 i=1,nsle
      n=nls(i)
      iparm(5,n)=0
   10 continue
c
   20 return
c
   24 call termin (txts,mssg,lcount,1)
c
   30 format(16i5)
   31 format(10i8)
   40 format(///' eliminated slide-lines'/(3x,i5/))
      end
      subroutine defbrk(ib,mtype,nphony,nsubg,nsubh,nwcon,numel)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk14/mx,ix(20),sig(6),epx
      dimension ib(*),nphony(*)
c
      nmel=128
      do 20 nn=1,numel
      l11=1+nwcon*(nn-1)
      call unpk(mx,ix,ib(l11),3)
      if (nmel.lt.128) go to 10
      nsubg=nsubg+2
      nsubh=nsubh+2
      mtyp=mx
      mcon=nphony(nn)
      nmel=0
   10 nmel=nmel+1
      mtyo=mtyp
      mtyp=mx
      mcoo=mcon
      mcon=nphony(nn)
      if (mtyo.ne.mtyp) nsubg=nsubg+1
      if (mcoo.ne.mcon) nsubh=nsubh+1
   20 continue
      return
      end
      subroutine egcryi(ib,mtype,nsubgv,mtnum,nfegp,nsbg,nelg,
     1 numel,nwcon)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize  arrays
c
      common/bk14/mx,ix(20),sig(6),epx
c
      dimension ib(*),mtype(*),nsubgv(*),mtnum(*),nfegp(*)
      nsubgt=0
      nmel  =128
      lcz   =0
      nml   =128
      if (nelg.eq.0) return
c
      do 30 nn=1,nelg
      if (nn.eq.nelg) nmel=numel-128*(nelg-1)
      l11=1+nwcon*nml*(nn-1)
      call unpk(mx,ix,ib(l11),nwcon)
      nsubg=1
      lcz=lcz+1
      mtypn=mx
      mtnum(lcz)=mtypn
      nfegp(lcz)=1
      if (nmel.eq.1) go to 20
      do 10 n=2,nmel
      l11=l11+nwcon
      call unpk(mx,ix,ib(l11),3)
      mtypo=mtypn
      mtypn=mx
      if (mtypn.eq.mtypo) go to 10
      lcz=lcz+1
      mtnum(lcz)=mtypn
      nfegp(lcz)=n
      nsubg=nsubg+1
   10 continue
   20 lcz=lcz+1
      nfegp(lcz)=nmel+1
      nsubgv(nn)=nsubg
      nsubgt=nsubgt+nsubg
   30 continue
c
      return
      end
      subroutine egcryj (ib,nsubgc,nfegc,nelg,numel,nphony,nwcon)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk14/mx,ix(20),sig(6),epx
      dimension ib(*),nsubgc(*),nfegc(*),nphony(*)
      nmel  =128
      lcz   =0
      ic    =0
      nml   =128
      if (nelg.eq.0) return
      do 30 nn=1,nelg
      if (nn.eq.nelg) nmel=numel-128*(nelg-1)
      l11=1+nwcon*nml*(nn-1)
      call unpk(mx,ix,ib(l11),nwcon)
      ic=ic+1
      nsubg=1
      nsubgc(ic)=nsubg
      itypn=mx
      locmt=1+(l11-1)/nwcon
      iconn=nphony(locmt)
      lcz=lcz+1
      nfegc(lcz)=1
      if (nmel.eq.1) go to 20
      do 10 n=2,nmel
      l11=l11+nwcon
      call unpk(mx,ix,ib(l11),3)
      itypo=itypn
      itypn=mx
      locmt=1+(l11-1)/nwcon
      icono=iconn
      iconn=nphony(locmt)
      if (iconn.eq.icono) go to 10
      lcz=lcz+1
      nfegc(lcz)=n
      if (itypo.eq.itypn) then
      nsubg=nsubg+1
      nsubgc(ic)=nsubg
      else
      ic=ic+1
      nsubg=1
      nsubgc(ic)=nsubg
      lcz=lcz+1
      nfegc(lcz)=n
      endif
   10 continue
   20 lcz=lcz+1
      nfegc(lcz)=nmel+1
   30 continue
      return
      end
      subroutine pckeri (mx,ixc,ixp,i)
c     implicit double precision (a-h,o-z)                                    dp
c
c     pack element connection data with three integers per word
c
      dimension ixc(*),ixp(*)
      data izero/0/
c     data ibits1,ibits2/21,42/                                         cray1
c
      mx    =mx    +izero
c     ixp(1)=mx                                                         cray1
c     ixp(1)=or(shift(ixc(1),ibits1),ixp(1))                            cray1
c     ixp(1)=or(shift(ixc(2),ibits2),ixp(1))                            cray1
c     ixp(2)=ixc(3)                                                     cray1
c     ixp(2)=or(shift(ixc(4),ibits1),ixp(2))                            cray1
c     if (i.eq.2) return                                                cray1
c     ixp(2)=or(shift(ixc(5),ibits2),ixp(2))                            cray1
c     ixp(3)=ixc(6)                                                     cray1
c     ixp(3)=or(shift(ixc(8),ibits1),ixp(3))                            cray1
c     ixp(3)=or(shift(ixc(9),ibits2),ixp(3))                            cray1
      ixp(1)=mx                                                         vax75
      ixp(2)=ixc(1)                                                     vax75
      ixp(3)=ixc(2)                                                     vax75
      ixp(4)=ixc(3)                                                     vax75
      ixp(5)=ixc(4)                                                     vax75
      if (i.eq.2) return                                                vax75
      ixp(6)=ixc(5)                                                     vax75
      ixp(7)=ixc(6)                                                     vax75
      ixp(8)=ixc(7)                                                     vax75
      ixp(9)=ixc(8)                                                     vax75
c
      return
c
      end
      subroutine fixnds(code,ncheck,numfx1,numfx2)
c     implicit double precision (a-h,o-z)                                    dp
      dimension code(*)
      numfx1=0
      numfx2=0
      do 10 i=1,ncheck
      if (code(i).gt.0..and.code(i).lt.8.) then
      numfx1=numfx1+1
      elseif (code(i).ge.8.) then
      numfx2=numfx2+1
      endif
   10 continue
      return
      end
      subroutine fixndt(code,ncheck,na1,na2,nb1,nb2)
c     implicit double precision (a-h,o-z)                                    dp
      dimension code(*),na1(*),na2(*),nb1(*),nb2(*)
      numfx1=0
      numfx2=0
      do 10 i=1,ncheck
      if (code(i).gt.0..and.code(i).lt.8.) then
      numfx1=numfx1+1
      na1(numfx1)=i
      na2(numfx1)=nint(code(i))
      elseif (code(i).ge.8.) then
      numfx2=numfx2+1
      nb1(numfx2)=i
      nb2(numfx2)=nint(code(i))-7
      endif
   10 continue
      return
      end
      subroutine zfubcs(ac,na1,na2,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension ac(3,1),na1(*),na2(*),bcs(3,7)
      data bcs/
     1 0.,1.,1.,1.,0.,1.,1.,1.,0.,
     2 0.,0.,1.,1.,0.,0.,0.,1.,0.,0.,0.,0./
cdir$ ivdep
      do 10 i=1,n
      ac(1,na1(i))=bcs(1,na2(i))*ac(1,na1(i))
      ac(2,na1(i))=bcs(2,na2(i))*ac(2,na1(i))
      ac(3,na1(i))=bcs(3,na2(i))*ac(3,na1(i))
   10 continue
      return
      end
      subroutine cntspc(tcode,numnp,numfx7)
c     implicit double precision (a-h,o-z)                                    dp
      dimension tcode(*)
      numfx7=0
      do 10 n=1,numnp
      if (nint(tcode(n)).eq.0) go to 10
      ispc=nint(tcode(n))/1000000
      if (ispc.gt.0) then
      numfx7=numfx7+2
      endif
   10 continue
      return
      end
      subroutine spcstr(tcode,itcode,numnp,numfx7,na3,nodes,icnt)
c     implicit double precision (a-h,o-z)                                    dp
      common/spcjnk/jj(3),kk(3)
      dimension tcode(*),itcode(*),na3(*),icnt(*),nodes(*)
      numfx7=0
      do 10 n=1,numnp
      if (nint(tcode(n)).eq.0) go to 10
      ispc=nint(tcode(n))/1000000
      if (ispc.gt.0) then
      call spcdec(itcode(ispc),jj,ic)
      if (jj(1)+jj(2)+jj(3).ne.0) then
      if (jj(1).eq.1.and.jj(2).eq.1.and.jj(3).eq.1) ibt=7.
      if (jj(1).eq.1.and.jj(2).eq.0.and.jj(3).eq.1) ibt=6.
      if (jj(1).eq.0.and.jj(2).eq.1.and.jj(3).eq.1) ibt=5.
      if (jj(1).eq.1.and.jj(2).eq.1.and.jj(3).eq.0) ibt=4.
      if (jj(1).eq.0.and.jj(2).eq.0.and.jj(3).eq.1) ibt=3.
      if (jj(1).eq.0.and.jj(2).eq.1.and.jj(3).eq.0) ibt=2.
      if (jj(1).eq.1.and.jj(2).eq.0.and.jj(3).eq.0) ibt=1.
      numfx7=numfx7+1
      na3(numfx7)=ibt
      nodes(numfx7)=n
      icnt(numfx7) =ic
      endif
      if (kk(1)+kk(2)+kk(3).ne.0) then
      if (kk(1).eq.1.and.kk(2).eq.1.and.kk(3).eq.1) ibt=7.
      if (kk(1).eq.1.and.kk(2).eq.0.and.kk(3).eq.1) ibt=6.
      if (kk(1).eq.0.and.kk(2).eq.1.and.kk(3).eq.1) ibt=5.
      if (kk(1).eq.1.and.kk(2).eq.1.and.kk(3).eq.0) ibt=4.
      if (kk(1).eq.0.and.kk(2).eq.0.and.kk(3).eq.1) ibt=3.
      if (kk(1).eq.0.and.kk(2).eq.1.and.kk(3).eq.0) ibt=2.
      if (kk(1).eq.1.and.kk(2).eq.0.and.kk(3).eq.0) ibt=1.
      numfx7=numfx7+1
      na3(numfx7)=ibt
      nodes(numfx7)=n+numnp
      icnt (numfx7)=ic
      endif
      endif
   10 continue
      return
      end
      subroutine zfubes(r,v,itcode,node,na3,ic,n)
c     implicit double precision (a-h,o-z)                                    dp
c
c      this routine applies the boundary condition on the local
c      coordinate system of v
c
c      r     = rotation matrix
c      v     = vector
c      itcode= spc code
c
      dimension r(9,*),v(3,*),itcode(*),node(*),na3(*),ic(*),bcs(3,7)
      data bcs/
     1 0.,1.,1.,1.,0.,1.,1.,1.,0.,
     2 0.,0.,1.,1.,0.,0.,0.,1.,0.,0.,0.,0./
cdir$ ivdep
      do 10 i=1,n
      v1=v(1,node(i))
      v2=v(2,node(i))
      v3=v(3,node(i))
      t1=bcs(1,na3(i))*(r(1,ic(i))*v1+r(2,ic(i))*v2+r(3,ic(i))*v3)
      t2=bcs(2,na3(i))*(r(4,ic(i))*v1+r(5,ic(i))*v2+r(6,ic(i))*v3)
      t3=bcs(3,na3(i))*(r(7,ic(i))*v1+r(8,ic(i))*v2+r(9,ic(i))*v3)
      v(1,node(i))= r(1,ic(i))*t1+r(4,ic(i))*t2+r(7,ic(i))*t3
      v(2,node(i))= r(2,ic(i))*t1+r(5,ic(i))*t2+r(8,ic(i))*t3
      v(3,node(i))= r(3,ic(i))*t1+r(6,ic(i))*t2+r(9,ic(i))*t3
   10 continue
      return
      end
      subroutine zfubds(ac,na1,na2,n,bcs)
c     implicit double precision (a-h,o-z)                                    dp
      dimension ac(3,1),na1(*),na2(*),bcs(4,*),dcs(3,7)
cdir$ ivdep
      do 10 i=1,n
      da=ac(1,na1(i))*bcs(1,na2(i))+ac(2,na1(i))*bcs(2,na2(i))
     &  +ac(3,na1(i))*bcs(3,na2(i))
      s1=1.-bcs(4,na2(i))
      s2=da*(2.*bcs(4,na2(i))-1.)
      ac(1,na1(i))=s1*ac(1,na1(i))+s2*bcs(1,na2(i))
      ac(2,na1(i))=s1*ac(2,na1(i))+s2*bcs(2,na2(i))
      ac(3,na1(i))=s1*ac(3,na1(i))+s2*bcs(3,na2(i))
   10 continue
      return
      end
      subroutine dynai
c     implicit double precision (a-h,o-z)                                    dp
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
      common/bk08/n4a,n4b,n4c,n4d,n4e,n4f,n4g,n4h,n7a,n7b,n7c,nusir,
     1 mpusr,mpubr
      common/b10a/inpsd,nmmtde,nmelde,nmmass
      common/b10b/ld01,ld02,ld03,ld04,ld05,ld06,ld07,ld08,ld09
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk16/numcc,numrc,numrw,nnrbs,lrb1,lrb2,lrb3,lrb4
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      common/bk28/summss,xke,xpe,tt
      common/thfl/madd,ifth,maddbg
      common/tmstp/tsmin,hmax,lctm
      common/bk76/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk78/njoydf,nwdadd,numsnc,n6a,n6b,n6c,n6d
      common/bk80/irdat,iidat,njt,nl(6),ll(6),lo(6)
      common/brdecm/yield,hob,tstart,xt,yt,gfunc(7)
      common/nwixa/nwcon
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      common/bttn/ntnwf,ixa(10)
      common/sbic/ijne,nblk
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/matnum/nmmath,nmmatb,nmmats,nmmatt
      common/rigidb/ nodtot, ncflag,numrbi
      common/tbsysp/numrcf,n6e,n6f,n6g
      common/shlopt/istrn,istupd,ibelyt,miter
      common/rwint/intiad,ncint1,nhint1,nsint1
      logical output,slnew
      common/csforc/ncs1,ncs2,ncs3,ncs4,ncs5,ncs6,ncs7,ncs8,ncs9,
     1 numcsd,csdinc,csdout,output,slnew,future(8)
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      common/tint/itsc,itbs,itlc
      common/aux14/ic(9085),icount
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      common/rn/irnxx
      logical ffifp
      common/slfl/nadd,numsls,ffifp
      common/strhx/nmsi,matsi(3,400),mxix(9),xe(3,8),ve(3,8),
     1 hisvar(100)
      common/bmsc/numsl(20)
      common/raydmp/dampm,ndamp
      common/fixnod/numfx1,numfx2,numfx3,numfx4,numfx5,numfx6
     1 ,numfx7,numfx8,numfx9,numfx10,numfx11,numfx12
      common/vsnum/vs,date
      character*8 vs                                                    vax75
      character*8 date                                                  vax75
      character*5 formts
      character*5 yr
      logical large
      common/bigprb/large
      common/eltspf/ietspf
      character*80 txts,mssg
      character*8 namef,iffnam
      common/sdopts/n30a,n30b,n30c,n30d,n30e,n30f,n30g
      common/extwrk/locaxa,exwork,iewflg
      common/tsarry/tslimt,tsarry(144)
      common/sorter/nnc,lczc,
     & ns11,ns12,ns13,ns14,ns15,ns16,ns17,
     & nh11,nh12,nh13,nh14,nh15,nh16,nh17,
     & nt11,nt12,nt13,nt14,nt15,nt16,nt17,
     & nb11,nb12,nb13,nb14,nb15,nb16,nb17
      common/drelax/ladd,idrint
      logical idrvon
      common/driver/ idrvon,idrvdm(10)
      common/double/iprec,ncpw,unit
      common/clogo/logo(10)
      character*63 logo
c
      common /   / a(1)
c
      character nif*5
      character*8 nifpf
      dimension istor(64)
      data istor/64*0/
      data nsubg,nptst,ntcc,ntsn,lenaxh/0,0,0,0,0/
      irnxx=0
      nmnmax=0
      nsnmax=0
      opifs =0.0
      opnext=0.0
      tslimt=0.0
      exwork=0.0
c
      call getnam (16,iffnam)
      if (iffnam.ne.'r$$$$$') then
      ilk16=0
      call rdabsf (iob16,nextra,2,ilk16,ioerr)
      call riosta (iob16)
      ilk16=ilk16+2
      else
      nextra=0
      endif
c
      call getnam  (9,nifpf)
      ffifp=.false.
      if (nifpf.ne.'disk9') ffifp=.not.ffifp
      numsl(1)=0
c
      yr='87'
      if (nmsi.eq.0) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=160) (head(i),i=1,12),yr,formts
      if (yr.ne.'88') yr='87'
      if (formts.eq.'     ') formts='small'
      write(13,165) (head(i),i=1,12),(logo(i),i=1,10),vs,date
      write(13,170) yr,formts
      large=.false.
      if (formts.eq.'large') large=.not.large
      endif
c
c     read and write control parameters
c
      csdout=0.0
      numrcf=0
      nstt  =0
      numcsd=0
      mssg =' error reading 1st control card'
      call gttxsg (txts,lcount)
      if (yr.eq.'87') then
      if(large) then
      read (unit=txts,fmt=185,err=400)
     1 nmmat,numnp,numelh,numelb,numels,numelt,numsnc,
     2 numrc,ndth,nsth
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=186,err=400) nstb,nsts,numdp,ibrode,nrbc,nif
      else
      read (unit=txts,fmt=180,err=400)
     1 nmmat,numnp,numelh,numelb,numels,numelt,numsnc,
     2 numrc,ndth,nsth,nstb,nsts,numdp,ibrode,nrbc,nif
      endif
      else
      read (unit=txts,fmt=189,err=400)
     1 nmmat,numnp,numelh,numelb,numels,numelt,numifs,opifs,tslimt
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=181,err=400) ndth,nsth,nstb,nsts,nstt,ikedit
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=188,err=400)
     1 numsnc,numrc,numrcf,numdp,ibrode,nrbc,nif,numcsd,csdinc
      endif
c
      call gttxsg (txts,lcount)
      mssg =' error reading 2nd control card'
      read (unit=txts,fmt=181,err=400)
     1 nlcur,numcl,numpc,numvc,numrw,numcc,initv,numsv,nthpx,nthpy,
     2 nthpz,nthsx,nthsy,nthsz,nelmd,ndtpts
      call gttxsg (txts,lcount)
      mssg =' error reading 3rd control card'
      read (unit=txts,fmt=182,err=400)
     1 endtim,prtc,pltc,irdeck,ncbrrf,dt2old,slsfac,itemp,idfrs,
     2 tssfac
      call gttxsg (txts,lcount)
      mssg =' error reading 4th control card'
      read (unit=txts,fmt=183,err=400)
     1 njt,nxtra,nblk,ntbsl,ntnwf,lctm,inpsd,numrbi,istrn,
     2 idumm,idumm2,irnxx,istupd,ibelyt,nnrbs,nspco
c
c     set defaults
c
c
c     check for external loads
c
      if ((numcl+numpc+nthpx+nthpy+nthpz+nthsx+nthsy+nthsz).gt.0) then
      iewflg=1
      else
      iewflg=0
      endif
      if ( numvc.ne.0) iewflg=0
      if (ikedit.eq.0) ikedit=1000
      nodspc=0
      nspcor=0
      nrcyck=0
      nubir =0
      mpubr =0
      drtol =0.
      tsmin =0.0
      nusir =0
      mpusr =0
      miter =1
      isdo  =0
      summss=0.0
      drfctr=.995
      tssfdr=0.0
      if (yr.eq.'88'.or.nspco.ne.0) then
      call gttxsg (txts,lcount)
      mssg =' error reading 5th control card'
      read (unit=txts,fmt=150,err=400)
     1 nodspc,nspcor,tsmin,nubir,mpubr,nusir,mpusr,nrcyck,
     2 drtol,drfctr,tssfdr,isdo
      endif
      if (yr.eq.'88'.or.nspco.eq.2) then
      call gttxsg (txts,lcount)
      mssg =' error reading 6th control card'
      read (unit=txts,fmt=152,err=400) miter,ietspf,numsl(1),
     1 idrint,dampm,ndamp
      if (miter.eq.0) miter=1
      endif
      if (nrcyck.eq.0) nrcyck=250
      if  (drtol.eq..0) drtol =.001
      if (drfctr.eq..0) drfctr=.995
c
      ndof=3
      nmst=nsth+nstb+nsts
      if (numelb+numels.gt.0) ndof=6
      call getnam  (7,namef)
      istrnt=0
      intiad=0
      if (namef.ne.'meshfl') istrnt=1
      if (ibelyt.eq.0) ibelyt=2
c
      if(nif.eq.'     ') nif='e10.0'
c
      icount=1
      dt2i=dt2old
      if (dt2old.eq.0.0) dt2old=10000000.
      if (slsfac.eq.0.0) slsfac=.10
      if (irnxx.eq.0) irnxx=-1
      tssf=tssfac
      if (tssf.eq.0.0.and.ndtpts.gt.0) tssfac=.667
      if (tssf.eq.0.0.and.slsfac.ge.1.) tssfac=.667
      if (tssf.eq.0.0.and.nnrbs  .gt.0) tssfac=.667
      if (tssf.eq.0.0.and.slsfac.ge.9.) tssfac=.333
      if (tssfac.eq.0.0) tssfac=.90
c
      if (tssfac.gt.1.0) then
      write ( *,189) tssfac
      write (13,189) tssfac
      endif
c
      if (tssfdr.eq.0.0) tssfdr=tssfac
      tssf  =tssfdr
      tssfac=tssfdr
      tssfdr=tssf
      nelmg=(numelh-1)/128+1
      nelmt=(numelt-1)/128+1
      nelmb=(numelb-1)/128+1
      nelms=(numels-1)/128+1
c
      if (irdeck.eq.0) irdeck=999999
      if (ncbrrf.eq.0) ncbrrf=999999
c
      output=.false.
      slnew =.false.
      write(13,190) nmmat,numnp,numelh,numelb,numels,numelt,numifs,
     1 opifs,tslimt
      write(13,194) ndth,nsth,nstb,nsts,nstt,ikedit
      write(13,196) numrc,numrcf,numdp,ibrode,nrbc,numcsd,csdinc,
     1 nlcur,numcl,numpc,numvc
      write(13,200) numrw,numcc,initv,numsv,nthpx,nthpy,nthpz
      write(13,210) nthsx,nthsy,nthsz,nelmd,ndtpts
      write(13,220) endtim,prtc,pltc,irdeck,ncbrrf,dt2i,slsfac,itemp
     1 ,idfrs
      write(13,225) tssfac,njt,nxtra,nblk,ntbsl,ntnwf,lctm
      write(13,226) inpsd,numrbi,istrn,irnxx,istupd
      write(13,227) ibelyt,nnrbs,nodspc,nspcor,tsmin,nubir,mpubr,nusir,
     1 mpusr,nrcyck,drtol
      write(13,2227) isdo
      write(13,228) miter,ietspf,numsl(1),dampm,ndamp
      if (miter.eq.3) miter=0
      if (istrnt.eq.2) istrnt=7
      if (irnxx.eq.-2) irnxx=0
      if (numsv.lt.0) then
      slnew=.true.
      numsv=iabs(numsv)
      endif
c
      call empty(13)
c
      if (numnp.eq.0.and.numelh.eq.0) then
      idrvon= .true.
      numnp =8
      numelh=1
      idrvdm(1)=1+nint(endtim/pltc)
      else
      idrvon=.false.
      endif
c
      numnp=numnp+nextra
      neq=3*numnp
      mpusr=mpusr+1
      mpubr=mpubr+1
c
c     allocate storage in small core
c
c     n1  :  mtype
c     n2  :  ro
c     n3  :  cm
c     n4  :  loadi
c     n5  :  rhodp
c     n6  :
c     n7  :  bcs
c     n8  :  npc
c     n9  :  pld
c     n10 :  nod
c     n11 :  idirn
c     n12 :  ncur
c     n13 :  fac
c     n14 :  lc
c     n15 :  nvel
c     n16 :  vx
c     n17 :  vy
c     n18 :  vz
c     n19 :
c     n20 :  rd
c     n21 :  ilcw
c     n22 :  nsw
c     n26 :  nsubgv
c     n27 :  mtnum
c     n28 :  nfegp
c
      n1 =1
      n2 =n1+2*nmmat
      n3 =n2+nmmat
      n4a=n3+48*nmmat
      n4b=n4a+nmmat
      n4c=n4b+48*nmmat
      n4d=n4c+2*nmmat
      n4e=n4d+nmmat
      n4f=n4e+3*nmmat
      n4g=n4f+24*nmmat
      n4h=n4g+3*nubir*mpubr
      ntmp0=n4h+3*nusir*mpusr
      ntmp1=ntmp0
      if (itemp.lt.0) ntmp1=ntmp1+1+numnp
      n4=ntmp1
      if (itemp.lt.0) n4=n4+1+numnp
      n5=n4
      if (numdp.ne.0) n5=n4+neq
      n6a=n5+2*numdp
      if (numsnc.ne.0) then
      n6a=n6a+neq
      endif
      n6b=n6a+1+3*numsnc
      n6c=n6b+1+3*numsnc
      n6d=n6c+1+3*numsnc
      n6e=n6d+numsnc
      n6f=n6e+numrcf
      n6 =n6f+6*numrcf
      n7=n6+2*numcc
c
c     allocate storage (formerly in large core)
c
c     lc1h :  element connection data for hexahedrons
c     lc1b :  element connection data for beams
c     lc1s :  element connection data for shells
c     lc2  :
c     lc3  :  momentum to be deposited into selected elements
c     lc4  :  deposition time
c     lc5  :  element numbers of recipient elements
c     lc6  :  pressure card data
c     lc7  :  tcode
c     lc7a :  spc code
c     lc7b :  spc rotation matrix
c     lc9  :  v
c     lc10 :  a
c     lc11 :  x
c     lc12 :
c     lc13 :
c     lc14 :  xms
c     lc15 :  auxvec
c
      ndthl=n7+4*numrc
      nsthl=ndthl+2*ndth
      nstbl=nsthl+2*nsth
      nstsl=nstbl+2*nstb
      nsttl=nstsl+2*nsts
      lc0  =nsttl+2*nstt
      lc1h=lc0+numelh+numelt
      lc1b=lc1h+nwcon*numelh
c     lc1s=lc1b+2*numelb                                                cray1
c     lc1t=lc1s+2*numels                                                cray1
      lc1s=lc1b+4*numelb                                                vax75
      lc1t=lc1s+5*numels                                                vax75
      lc2 =lc1t+nwcon*numelt
      lc3=lc2+966*2*nmmat
      call expndm (lc3)
c
c     zero a
c
      do 10 i=n5,lc3
   10 a(i)=0.0
c
c
c     read and write material properties for solid elements
c
      mmauxs=0
      iortho=0
      ifb   =0
      call matin (a(n4a),a(n4b),a(n4c),a(n4c+nmmat),a(n4d),a(n4e),
     1 a(n4f),a(n1),a(n1+nmmat),a(n2),a(n3),idfrs,nmmat,lc2,mmauxs,
     2 iortho,ifb,isf)
      call empty(13)
c
c     read in integration rules
c
c     for beam elements
c
      if (nubir.ne.0) then
      call intrlb(a(n4g),nubir,mpubr,a(n4f),nmmat)
      endif
c
c     for shell elements
c
      if (nusir.ne.0) then
      call intrls(a(n4h),nusir,mpusr,a(n4f),nmmat)
      endif
c
c
      lc3=lc2+966*mmauxs
      lc4=lc3+nwcon*nelmd
      lc5=lc4+3*nelmd
      lc6=lc5+nelmd
      lc7=lc6+7*numpc
      lc7a=lc7+ndof*numnp/3
      lc7b=lc7a+nodspc
      lc9 =lc7b+(nspcor+1)*9
      if (iewflg.gt.0) then
      locaxa=lc9
      lc9   =lc9+ndof*numnp
      endif
      numlb2=1+numelb/2
      numls2=1+numels/2
      nnpmax=max0(numnp,numlb2,numls2,numelt,numelh)
      lc10=lc9 +ndof*nnpmax
c     lc11=lc10+ndof*nnpmax                                             cray1
      lc11=lc10+ndof*numnp+max0(0,5*numelh-ndof*numnp)                  vax75
      lc12=lc11+numnp
      lc13=lc12+numnp+(ndof-3)*numnp+numnp
      lc14=lc13+iabs(mkthf-1)*ndof*numnp
      nh01=lc14+ndof*numnp/3
      nh02=nh01+ifb*12*numelh
      nh03=nh02+numelh
      nb02=nh03+numelh
      nb03=nb02+numelb
      nb04=nb03+numelb*12
      nb05=nb04+numelb*6
      ns01=nb05+numelb*5
      ns03=ns01+numels*12*min0(1,istrn)
      ns04=ns03+numels*12
      ns05=ns04+numels*iortho
      lc15=ns05+numels*10
c
c     expand memory
c
      call expndm (lc15+20)
c
c     zero a
c
      do 11 i=lc3,lc15
   11 a(i)=0.0
c
      if (istrnt.ne.0) then
      call rdabsg(iob7,ncint1,3,intiad,0)
      call riosta(iob7)
      intiad=intiad+3
c
      write( *,9108)ncint1,nhint1,nsint1
 9108 format(//5x,'Expecting Nike3d stress initialization data for',
     1        /5x,'nodal coords:  ',i8,
     2        /5x,'solid elemnts: ',i8,
     3        /5x,'shell elemnts: ',i8)
      endif
c
c     read and write nodal point data
c
      call nodein (a(lc11),a(lc7),a(lc7+numnp),nif,nextra,iextra,
     1 idrvon)
      call fixnds(a(lc7),(ndof*numnp)/3,numfx1,numfx2)
      numfx3=lc15
      numfx4=numfx3+numfx1
      numfx5=numfx4+numfx1
      numfx6=numfx5+numfx2
      lc15  =numfx6+numfx2
      call expndm(lc15+20)
      call fixndt(a(lc7),(ndof*numnp)/3,a(numfx3),a(numfx4),
     1 a(numfx5),a(numfx6))
c
c     write plot data
c
      istor(11)=4
      istor(12)=numnp
      istor(13)=6
      istor(14)=6+numrw+6*nmmat
      istor(15)=0
      istor(16)=1
      istor(17)=1
      istor(18)=1
      istor(19)=numelh+numelt
      istor(20)=nmmath+nmmatt
      istor(21)=0
      istor(22)=0
      istor(23)=7
      istor(24)=numelb
      istor(25)=nmmatb
      istor(26)=6
      istor(27)=numels
      istor(28)=nmmats
      istor(29)=33+12*min0(1,istrn)
c
      call wrabsg (iob5,head,10,iadd,0)
      call riosta (iob5)
      iadd=iadd+15
      call wrabsg (iob5,istor(11),49,iadd,0)
      call riosta (iob5)
      iadd=iadd+49
      call wrabsg (iob5,a(lc11),neq,iadd,1)
      call riosta (iob5)
      iadd=iadd+neq
c
      if (idrint.gt.0) then
      ladd=0
      call wrabsg (iob6,head,10,ladd,0)
      call riosta (iob6)
      ladd=ladd+15
      call wrabsg (iob6,istor(11),49,ladd,0)
      call riosta (iob6)
      ladd=ladd+49
      call wrabsg (iob6,a(lc11),neq,ladd,1)
      call riosta (iob6)
      ladd=ladd+neq
      endif
c
      istor(18)=0
c
      if (ffifp) then
c
c     write sliding surface interface data
c
      call wrabsg (iob9,head,10,nadd,0)
      call riosta (iob9)
      nadd=nadd+15
      call wrabsg (iob9,istor(11),49,nadd,0)
      call riosta (iob9)
      nadd=nadd+49
      call wrabsg (iob9,a(lc11),neq,nadd,1)
      call riosta (iob9)
      nadd=nadd+neq
      endif
c
      if (mkthf.ne.0) then
c
c     write taurus time history database
c
      madd  =64+2*ndth+2*nmst
      maddbg=madd
      call wrabsg (iob4,a(lc11),neq,madd,1)
      call riosta (iob4)
      madd=madd+neq
c
      else
c
      call blkcpy(a(lc11),a(lc13),3*numnp)
c
      endif
c
c     read solid element data
c
   20 isv1=iadd
      lenaxb=0
      lenaxs=0
      lenaxt=0
      nsays=10*maxsiz
      nsubg=0
      nsubh=0
      if (numelh.eq.0) go to 24
      call elemnh(a(lc1h),a(n4a),a(n1),numelh,nsubg,head,lenaxh,
     1 nsubh,a(lc10),a(lc10+numelh),a(lc10+2*numelh),a(lc9),
     2 numnp,a(lc9+numelh+1),idrvon)
      call dumpic
      isv1=iadd
      call wrabsf(iob8,a(lc10),numelh*iprec,nsays)
      call riosta(iob8)
      nsays=nsays+numelh*iprec
      call wrabsf (iob8,a(lc10+numelh),2*numelh*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+2*numelh*iprec
      call wrabsf (iob8,a(lc9),numelh*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+numelh*iprec
   24 iadd=iadd+9*numelt
      madd=iadd+2*(nmst+ndth)
c
c     beam elements
c
      nsubb=0
      nsubc=0
      if (numelb.gt.0) then
      call elemnb(numelb,a(lc1b),a(nb05),a(n1),a(n4f),lenaxb,nmintb,
     1 nsubb,a(n1+nmmat),a(nb02),a(lc11),nsubc,a(lc10),a(lc10+numelb),
     2 a(lc10+2*numelb),a(lc9),numnp,a(lc9+numelb+1))
      call dumpic
      call wrabsf(iob8,a(lc10),numelb*iprec,nsays)
      call riosta(iob8)
      nsays=nsays+numelb*iprec
      call wrabsf (iob8,a(lc10+numelb),2*numelb*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+2*numelb*iprec
      call wrabsf (iob8,a(lc9),numelb*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+numelb*iprec
      endif
c
c     shell elements
c
      nsubr=0
      nsubs=0
      if (numels.gt.0) then
      call elemns(numels,a(lc1s),a(ns05),a(n1),a(n4f),lenaxs,nmints,
     1 nsubr,a(ns04),iortho,nsubs,a(lc10),a(lc10+numels),
     2 a(lc10+2*numels),a(lc9),numnp,a(lc9+numels+1))
      call dumpic
      call wrabsf(iob8,a(lc10),numels*iprec,nsays)
      call riosta(iob8)
      nsays=nsays+numels*iprec
      call wrabsf (iob8,a(lc10+numels),2*numels*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+2*numels*iprec
      call wrabsf (iob8,a(lc9),numels*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+numels*iprec
      endif
      isv2=iadd
c
c     thick shell elements
c
      nsubt=0
      nsubu=0
      if (numelt.ne.0) then
      iadd=isv1
      madd=iadd+2*(nmst+ndth)
      call elemnt (a(lc1t),a(n4a),a(n1),numelt,nsubt,a(n4f),head,lenaxt,
     1 nsubu,a(lc10),a(lc10+numelt),a(lc10+2*numelt),a(lc9),
     2 numnp,a(lc9+numelt+1))
      call dumpic
      iadd=isv2
      call wrabsf(iob8,a(lc10),numelt*iprec,nsays)
      call riosta(iob8)
      nsays=nsays+numelt*iprec
      call wrabsf (iob8,a(lc10+numelt),2*numelt*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+2*numelt*iprec
      call wrabsf (iob8,a(lc9),numelt*iprec,nsays)
      call riosta (iob8)
      nsays=nsays+numelt*iprec
      madd=iadd+2*(nmst+ndth)
      endif
c
c     interface driver via linked segments
c
      nif1=lc15
      nif4=lc15
      if (numifs.ne.0) then
      opnext=0.0
      nif2=nif1+4*numifs
      call expndm (nif2)
      call dfifs(a(lc11),numnp,a(nif1),a(lc10),numifs,nsnifs)
      nif3=nif2+nsnifs
      nif4=nif3+3*nsnifs
      call expndm (nif4)
      call blkcpy (a(lc10),a(nif2),nsnifs)
      call bgnfil (numifs,nsnifs,a(nif1),a(nif2),a(nif3),a(lc11),ilink)
      lc15=nif4
      endif
      if (iextra.ne.0) then
      nif5=nif4+nextra
      nif6=nif5+1+3*nextra
      lc15=nif6
      endif
c
c     read nodal number for common interface nodes
c
      call joyinf (numsnc,a(n6d))
c
c     read spc cards
c
      if (nodspc+nspcor.ne.0) then
      call spcin (nodspc,nspcor,a(lc7),a(lc7a),a(lc7b))
c
c     set up arrays for vectorization
c
      call cntspc(a(lc7),numnp,numfx7)
      numfx8=lc15
      numfx9=numfx8+numfx7
      numfx10=numfx9+numfx7
      numfx11=numfx10+numfx7
      lc15=numfx11
      call expndm (lc15+20)
      call spcstr(a(lc7),a(lc7a),numnp,numfx7,a(numfx8),a(numfx9),
     1 a(numfx10))
      endif
c
c
c     read and write roller boundary condition cards
c
      call rbcc (a(n7),numrc)
c
      if (numrcf.ne.0) then
      n6g=lc15
      ilc1=n6g
      do 27 i=1,numrcf
      nm=i
      call azero (a(lc9),2*numnp)
      n6ei=n6e+i-1
      call gtsgnd(a(n6ei),a(lc9),a(lc9+numnp),nsgnds,a(n6f),a(lc11),
     1 numnp,nm)
      ilc2=ilc1+nsgnds
c
c     expand memory
c
      call expndm (ilc1+2*nsgnds+20)
c
      call stsgnd(a(lc9),a(lc9+numnp),a(n6ei),numnp,a(ilc1),a(ilc2))
      ilc1=ilc1+2*nsgnds
   27 continue
      lc15=ilc1
      call azero (a(lc9),2*numnp)
      endif
c
c     define node and element blocks for time histories
c
      call blkprt (a(ndthl),a(nsthl),a(nstbl),a(nstsl),a(nsttl))
c
c
c     define density vs. depth curve if gravity is considered
c
      if (numdp.ne.0) call rhodp (a(n5))
c
      mkthf=mkthf*(ndth+nmst)
c
      if (mkthf.ne.0) then
      mdsv=0
      call wrabsg (iob4,head,10,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+15
      istor(14)=7+numrw+6*nmmat
      istor(21)=ndth
      istor(18)=1
      istor(22)=nmst
      call wrabsg (iob4,istor(11),49,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+49
      iflagd=ndth+1000
      call wrabsg (iob4,iflagd,1,59,0)
      call riosta (iob4)
      call wrabsg (iob4,nsth, 1,60,0)
      call riosta (iob4)
      call wrabsg (iob4,nstb, 1,61,0)
      call riosta (iob4)
      call wrabsg (iob4,nsts, 1,62,0)
      call riosta (iob4)
      if (ndth.ne.0) then
      call wrabsg (iob4,a(ndthl),2*ndth,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+2*ndth
      endif
      if (nsth.ne.0)  then
      call wrabsg (iob4,a(nsthl),2*nsth,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+2*nsth
      endif
      if (nstb.ne.0)  then
      call wrabsg (iob4,a(nstbl),2*nstb,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+2*nstb
      endif
      if (nsts.ne.0)  then
      call wrabsg (iob4,a(nstsl),2*nsts,mdsv,0)
      call riosta (iob4)
      mdsv=mdsv+2*nsts
      endif
      endif
c
c
c     read in brode function data
c
      if (ibrode.ne.0) call brodei
c
      nh11=lc15+lenaxh
      if (idrvon) then
      idrvdm(2)=lenaxh+15
      nh11=nh11+(idrvdm(1)+1)*(lenaxh+28)
      endif
      nh12=nh11+nsubh
      nh13=nh12+nsubh+300
      nh14=nh13+numelh
      nh15=nh14+numelh
      nh16=nh15
      nh17=nh16+numelh
      nb06=nh17+numelh
      nb07=nb06+lenaxb
      nb08=nb07+numelb
      nb09=nb08+nelmb
      nb10=nb09+nsubb
      nb11=nb10+nsubb+300
      nb12=nb11+nsubc
      nb13=nb12+nsubc+300
      nb14=nb13+numelb
      nb15=nb14+numelb
      nb16=nb15
      nb17=nb16+numelb
      ns06=nb17+numelb
      ns07=ns06+lenaxs
      ns08=ns07+(1+isf)*numels
      ns09=ns08+nelms
      ns10=ns09+nsubr
      ns11=ns10+nsubr+300
      ns12=ns11+nsubs
      ns13=ns12+nsubs+300
      ns14=ns13+numels
      ns15=ns14+numels
      ns16=ns15
      ns17=ns16+numels
      nt01=ns17+numels
      if (tslimt.ne.0.0) then
      ns16=ns15+numels
      ns17=ns16+numels
      nt01=ns17+numels
      endif
      nt02=nt01+nelmt
      nt03=nt02+nsubt
      nt04=nt03+nsubt+300
      nt05=nt04+lenaxt
      nt06=nt05+numelt+1
      nt11=nt06+9*numelt
      nt12=nt11+nsubu
      nt13=nt12+nsubu+300
      nt14=nt13+numelt
      nt15=nt14+numelt
      nt16=nt15
      nt17=nt16+numelt
      lc16=nt17+numelt
      if (tslimt.ne.0.0) then
      nt16=nt15+numelt
      nt17=nt16+numelt
      lc16=nt17+numelt
      endif
      if (istupd.ne.0) then
      istupd=lc16
      lc16  =istupd+numnp
      endif
      if (numcsd.eq.0) then
      n8=lc16
      n9=n8+2*nlcur+1
c
      call expndm (n9+2000)
c
      nn=n9+1900
      do 70 i=lc15,nn
   70 a(i)=0.0
      nsays=10*maxsiz
      if (numelh.gt.0) then
      call rdabsf(iob8,a(nh17),numelh*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelh*iprec
      nshxs=nsays
      nsays=nsays+numelh*iprec
      call rdabsf (iob8,a(nh13),numelh*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelh*iprec
      call rdabsf (iob8,a(nh14),numelh*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelh*iprec
      endif
      if (numelb.gt.0) then
      call rdabsf(iob8,a(nb17),numelb*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelb*iprec
      nshxb=nsays
      nsays=nsays+numelb*iprec
      call rdabsf (iob8,a(nb13),numelb*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelb*iprec
      call rdabsf (iob8,a(nb14),numelb*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelb*iprec
      endif
      if (numels.gt.0) then
      call rdabsf(iob8,a(ns17),numels*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numels*iprec
      nsshs=nsays
      nsays=nsays+numels*iprec
      call rdabsf (iob8,a(ns13),numels*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numels*iprec
      call rdabsf (iob8,a(ns14),numels*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numels*iprec
      endif
      if (numelt.gt.0) then
      call rdabsf(iob8,a(nt17),numelt*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelt*iprec
      nshxt=nsays
      nsays=nsays+numelt*iprec
      call rdabsf (iob8,a(nt13),numelt*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelt*iprec
      call rdabsf (iob8,a(nt14),numelt*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelt*iprec
      endif
      else
      ncs1=lc16
      ncs2=ncs1+3*numcsd
      ncs3=ncs2+2*numcsd
      ncs4=ncs3+2*numcsd
      call expndm(ncs4)
      nsays=10*maxsiz
      if (numelh.gt.0) then
      call rdabsf(iob8,a(nh17),numelh*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelh*iprec
      nshxs=nsays
      nsays=nsays+numelh*iprec
      call rdabsf (iob8,a(nh13),numelh*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelh*iprec
      call rdabsf (iob8,a(nh14),numelh*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelh*iprec
      endif
      if (numelb.gt.0) then
      call rdabsf(iob8,a(nb17),numelb*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelb*iprec
      nshxb=nsays
      nsays=nsays+numelb*iprec
      call rdabsf (iob8,a(nb13),numelb*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelb*iprec
      call rdabsf (iob8,a(nb14),numelb*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelb*iprec
      endif
      if (numels.gt.0) then
      call rdabsf(iob8,a(ns17),numels*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numels*iprec
      nsshs=nsays
      nsays=nsays+numels*iprec
      call rdabsf (iob8,a(ns13),numels*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numels*iprec
      call rdabsf (iob8,a(ns14),numels*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numels*iprec
      endif
      if (numelt.gt.0) then
      call rdabsf(iob8,a(nt17),numelt*iprec,nsays,ioerr)
      call riosta(iob8)
      nsays=nsays+numelt*iprec
      nshxt=nsays
      nsays=nsays+numelt*iprec
      call rdabsf (iob8,a(nt13),numelt*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelt*iprec
      call rdabsf (iob8,a(nt14),numelt*iprec,nsays,ioerr)
      call riosta (iob8)
      nsays=nsays+numelt*iprec
      endif
      call getcsd (a(ncs1),numcsd,nndtot,nbmtot,nshtot)
      ncs5=ncs4+nndtot
      ncs6=ncs5+nbmtot
      ncs7=ncs6+nshtot
      ncs8=ncs7+2*nbmtot
      ncs9=ncs8+4*nshtot
      n8  =ncs9+9*numcsd
      n9  =n8+2*nlcur+1
      call expndm (n9+2000)
      nn=n9+1900
      call defcsd (a(ncs1),a(ncs2),a(ncs3),a(ncs4),a(ncs5),a(ncs6),
     1 numcsd,a(nb13),a(ns13))
      call flgnds (a(lc1b),a(lc1s),a(ncs1),a(ncs4),a(ncs5),a(ncs6),
     1 a(ncs7),a(ncs8),numcsd)
      endif
c
      if (tslimt.ne.0.0) then
      value=1.e+20
      call sarray(a(ns16),value,numels)
      call sarray(a(nt16),value,numelt)
      endif
c
c
c     read in time histories
c
      call timhis (a(n8),a(n9),nlcur,nptst,n9,a(n8+nlcur+1))
c
      idvkv=n9+nptst
      idvkx=idvkv
      ialoc=0
      if (idrflg.eq.0) go to 72
      idvkx=idvkv+ndof*numnp
      if (numsnc.ne.0) ialoc=ndof*numnp
   72 n10=idvkx+ialoc
      if (idrvon) then
      n11=n10+numcl+18*idrvdm(3)
      else
      n11=n10+numcl
      endif
      n12=n11+numcl
      n13=n12+numcl
      n14=n13+4*numcl
      n15=n14+3*numvc
      n16=n15+numvc
      n17=n16+numvc
      n18=n17+numvc
      n19=n18+numvc
      n20=n19+nlcur
      n21=n20+14*numrw
      n22=n21+4*numrw
c
      call expndm (n22+neq)
c
c     read and concentrated nodal load cards
c
      call conlod (a(n10),a(n11),a(n12),a(n13),a(n13+numcl),numcl)
c
c     read and write pressure b.c. data
c
      call loadp (a(lc6),numpc)
c
c     read and write velocity boundary condition data
c
      call velbci (a(n14+numvc),a(n14),a(n15),a(n16),a(n17),a(n18),
     1 a(n14+2*numvc))
c
c     read in rigid wall data
c
      call rgidwi (a(n20),a(n21),a(n22),numrw,ntsn,a(n21+numrw),
     1 a(n21+3*numrw))
c
      n23=n22+ntsn
      n24=n23+4*numcc+1
c
      call expndm (n24+3*nsubg+3000)
c
c     read in constraint condition cards
c
      call rigidi (a(n6),a(n23),a(n24),numcc,ntcc,a(lc11))
c
      n25=n24+ntcc
      n26=n25+7+6*nmmat+numrw+1
      n27=n26+nelmg
      n28=n27+nsubg
      n29=n28+nsubg+300
      n30a=n29+5*ndtpts
      n30b=n30a+numsv
      n30c=n30b+numsv
      n30d=n30c+numsv
      n30 =n30d+numsv
      n7a=n30+8*numsv
      n72=n7a+2*numsv
c
c     initialize  cray - 1  arrays
c
      call egcry (a(lc1h),a(n1),a(n26),a(n27),a(n28),nsubg,nelmg,
     1 numelh,nwcon)
      if (numelh.ne.0) then
      call rdabsf (iob8,a(nh15),numelh*iprec,nshxs,ioerr)
      call riosta (iob8)
      call egcrys(a(lc1h),a(nh11),a(nh12),nelmg,numelh,a(nh15),nwcon)
      endif
      call egcry (a(lc1b),a(n1),a(nb08),a(nb09),a(nb10),nsubb,nelmb,
c    1 numelb,2)                                                        cray1
     1 numelb,4)                                                        vax75
      if (numelb.ne.0) then
      call rdabsf (iob8,a(nb15),numelb*iprec,nshxb,ioerr)
      call riosta (iob8)
c     call egcrys(a(lc1b),a(nb11),a(nb12),nelmb,numelb,a(nb15),2)       cray1
      call egcrys(a(lc1b),a(nb11),a(nb12),nelmb,numelb,a(nb15),4)       vax75
      endif
      call egcry (a(lc1s),a(n1),a(ns08),a(ns09),a(ns10),nsubr,nelms,
c    2 numels,2)                                                        cray1
     2 numels,5)                                                        vax75
      if (numels.ne.0) then
      call rdabsf (iob8,a(ns15),numels*iprec,nsshs,ioerr)
      call riosta (iob8)
c     call egcrys(a(lc1s),a(ns11),a(ns12),nelms,numels,a(ns15),2)       cray1
      call egcrys(a(lc1s),a(ns11),a(ns12),nelms,numels,a(ns15),5)       vax75
      endif
      call egcry (a(lc1t),a(n1),a(nt01),a(nt02),a(nt03),nsubt,nelmt,
     1 numelt,nwcon)
      if (numelt.ne.0) then
      call rdabsf (iob8,a(nt15),numelt*iprec,nshxt,ioerr)
      call riosta (iob8)
      call egcrys(a(lc1t),a(nt11),a(nt12),nelmt,numelt,a(nt15),nwcon)
      endif
c
c     read and write initial velocity conditions
c
      nvlgth=nnpmax*ndof
      call azero(a(lc9),nvlgth)
      call initlv (a(lc11),a(lc9),initv,nextra)
c
      nrttlm=0
      nrttls=0
      issca =0
      it1sl =0
      ifl=0
c
      nsnws=0
      nsnxs=0
      if (numsv.ne.0) call sllin (a(n30),a(n72),nrttlm,
     1nrttls,numsv,issca,it1sl,a(n7a),nsnws,nsnxs,a(n30a))
c
      nmntl=0
      nsntl=0
c
c     allocate extra storage for slide and void arays
c
      ju =n72+3*numsv+nsnws
      n31= ju+nsnxs
      n32=n31+(4+it1sl)*nrttls
      n33=n32+(4+it1sl)*nrttlm
      n34=n33+4*max0(nrttlm,nrttls)
      n35=n34
c
      call expndm (n35+1000)
c
      if (numsv.eq.0) go to 120
c
c     read in and write out slide & void information
c
      k1=n30
      k1a=n72
      k1b=k1a+3*numsv
      k2=n31
      k3=n32
      k4=n33
      k5=ju
      nslmat=0
      numsls=0
      do 90 i=1,numsv
c
      n=i
      call slavin (a(k1),a(k1a),a(k2),a(k3),a(k4),a(lc11),ifl,nslmat,
     1 ffifp,numsls,a(k1b),a(k5))
c
      k1=k1+8
      k1a=k1a+3
      k2=k2+4*nrts
      k3=k3+4*nrtm
      nmnmax=max0(nmn,nmnmax)
      nsnmax=max0(nsn,nsnmax)
      if (nty.ge.5.and.nty.lt.9) then
      k1b=k1b+5*nsn
      endif
      if (nty.eq.9) k5=k5+2*nrts
      nsntl=nsntl+nsn
   90 nmntl=nmntl+nmn
      istor(11)=4
      istor(12)=numnp
      istor(13)=6
      istor(14)=6
      istor(15)=0
      istor(16)=1
      istor(17)=1
      istor(18)=0
      istor(19)=0
      istor(20)=0
      istor(21)=0
      istor(22)=0
      istor(23)=7
      istor(24)=0
      istor(25)=0
      istor(26)=6
      istor(27)=numsls
      istor(28)=nslmat
      istor(29)=16
c
      if (ffifp) then
      call wrabsg (iob9,istor(11),49,15,0)
      call riosta (iob9)
      call dmpslc
      endif
c
c     store master and slave node numbers
c
      n34=n33+nsntl
      n7b=n34+nmntl
      n7c=n7b+3*nsntl
      n35=n7c+3*nmntl
c
      call expndm (n35+1000)
c
      k1=n33
      k2=n34
      k3=n30
      k4=n31
      k5=n32
      do 100 i=n33,n35
  100 a(i)=0.0
      do 110 i=1,numsv
      call nodesm (a(k1),a(k2),a(k4),a(k5),a(k3))
      k1=k1+nsn
      k2=k2+nmn
      k3=k3+8
      k4=k4+4*nrts
      k5=k5+4*nrtm
  110 continue
  120 n36=n35+numsv+nsntl
      jv =n36+numsv+nmntl
c
      call expndm (jv +1000)
c
      nstl=0
      nmtl=0
      if (numsv.eq.0) go to 140
c
c     count number of segments containing each interface node
c
      k1=n30
      k2=n31
      k3=n32
      k4=n33
      k5=n34
      k6=n35
      k7=n36
      nsnxs=0
      do 130 i=1,numsv
c
      call nsgmnt (a(k1),a(k2),a(k3),a(k4),a(k5),a(k6),a(k7),nstl,nmtl,
     1 nsnxs)
c
      k1=k1+8
      k2=k2+4*nrts
      k3=k3+4*nrtm
      k4=k4+nsn
      k5=k5+nmn
      k6=k6+nsn
      k7=k7+nmn
      k6=k6+1
      k7=k7+1
  130 continue
  140 n37= jv+5*nsnxs
      n38=n37+nstl
      n39=n38+nmtl
      n40=n39+nsntl
      n41=n40+nmntl
      n42=n41+2*nrttls
      n43=n42+2*nrttlm
      n44=n43+nmntl
      n45=n44+nsntl
      n46=n45+6*max0(nmnmax,nsnmax)
      n47=n46+12*max0(nmnmax,nsnmax)
      n48=n47+2*nsntl
      locend=n48+numsv
c
c***  tie-breaking slidelines
      if (ntbsl.eq.0) go to 142
      ips=locend
      ipm=ips+ntbsl
      locend=ipm+ntbsl
      call expndm(locend)
      call tbin1(a(ips),a(ipm),locend,a,numels)
c
c***  tied nodes with failure
  142 if (ntnwf.eq.0) go to 143
      ixa(1)=locend
      ixa(2)=ixa(1)+ntnwf
      locend=ixa(2)+ntnwf
      call expndm(locend)
      call rtnwf1(a(ixa(1)),a(ixa(2)),locend,a,a(lc1s),numels,a(lc11))
c
c     pointers to rigid body arrays
c
c     n49  :  rigid body rotational and translational displacements
c     n50  :  rigid body rotational and translational velocities
c     n51  :  rigid body rotational and translational accelerations
c     n52  :  rotational inertia matrix
c     n53  :  translational mass
c     n54  :  global coordinates of mass center
c     n55  :  number of nodes in each rigid body
c     n56  :  pointer array to rigid body nodes
c     n57  :  rigid body nodes
c     n58-n66  :  working arrays for vectorization on crays
c     n67  :  boundary conditions on rigid body nodes
c     n68  :  global constraints on rigid bodies
c     n69  :  rigid body number
c     n70  :  constrained nodal coordinates and stiffness
c
c
  143 n49=locend
      n50=n49+6*nmmat
      n51=n50+12*nmmat
      n52=n51+6*nmmat
      n53=n52+18*nmmat
      n54=n53+6*nmmat
      n55=n54+6*nmmat
      n56=n55+nmmat
      n57=n56+nmmat+1
      locend=n57+numnp
c
      call expndm (locend)
c
      nodtot=0
      nodmax=0
      ncflag=0
c
      call setupn (a(n1),a(lc1h),a(lc1s),a(lc1b),a(lc1t),a(n57),a(n56),
     1 numnp,a(n55),a(n49),numelh,numels,numelb,numelt,nrbc,nmmat,
     2 nodtot,nodmax)
c
      if (nxtra.ne.0) call xtran(nxtra,a(n56),a(n55),a(n57),
     1                           nodtot,nodmax,a(n1))
c
      n58=n57+nodtot
      n59=n58+nodmax
      n60=n59+nodmax
      n61=n60+nodmax
      n62=n61+nodmax
      n63=n62+nodmax
      n64=n63+nodmax
      n65=n64+nodmax
      n66=n65+nodmax
      n67=n66+nodmax
      n68=n67+2*nodtot
      n69=n68+2*nmmat
      n70=n69+nmmat
      n71=n70+issca*(max0(nsntl,nmntl)+6)
      n73=n71+issca*(max0(nsntl,nmntl)+6)
      n74=n73+2*numsv
      n78=n74+nsntl
      n75=n78+nmntl
      ifl=max0(1,ifl)
      n76=n75+ifl
      n77=n76+5*ifl
      locend=n77
c
      call expndm (locend)
c
      call blkcpy (a(n49),a(n69),nmmat)
c
      call setrbc (a(n1),a(lc7),a(lc11),a(n55),a(n56),a(n57),a(n67),
     1 a(n67+nodmax),a(n68),a(n58),a(n69),nodtot,ncflag,a(n3))
c
      if (ncflag.ne.0) then
      locend=n77+6*ncflag
      call expndm (locend)
      endif
      n79=n77+6*ncflag
c     locend=n79+(nsntl+nmntl)                                          cray1
      locend=n79+3*(nsntl+nmntl)                                        vax75
      call expndm(locend)
c
      if (njt.eq.0) go to 145
      irdat=locend
      iidat=irdat+njt
      locend=iidat+8*njt
      call expndm(locend)
      call jrd(a(irdat),a(iidat),njt)
c
c
c     read body force loads
c
  145 call loadb
c
c     read in momentum to be deposited into zones
c
      call momdep (a(lc3),a(lc4),a(lc5),a(lc1h),nelmd,a(nh13))
c
c     read in detonaton points
c
      call rdetpt (a(n29),ndtpts)

c
c***  read in shell solid interfaces
      ijne=locend
      locend=ijne+3*nblk
      call expndm(locend)
      if (nblk.ne.0) call sbir(a(ijne),nblk,a(locend),locend)
c
c***  tie breaking slideline initialization
      if (ntbsl.ne.0) call itbrk2(a(ipa),a(ipb),a(lc1s),locend,
     1                            ipj,numels,nods,a(locend))
c
      if (inpsd.eq.0) go to 149
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=320,err=400) nmmtde,nmelde,nmmass
      mssg =' error reading control card for discrete element input'
      write(13,330)nmmtde,nmelde,nmmass
      ld01=locend
      ld02=ld01+nmmtde
      ld03=ld02+24*nmmtde
      ld04=ld03+3*nmelde
      ld05=ld04+nmelde
      ld06=ld05+9*nmelde
      ld07=ld06+nmmass
      ld08=ld07+nmmass
      ld09=ld08+nmmass
      locend=ld09
c
      call expndm (locend)
c
      call matnde(a(ld01),a(ld02),nmmtde)
      if (nmelde.ne.0) then
      call elmnde(a(ld03),a(ld04),a(ld05),a(lc11),nmelde)
      call initde(a(n8),a(n9),a(ld01),a(ld02),a(ld03),a(ld04),
     &   a(ld05),nmelde)
      endif
      if (nmmass.ne.0) then
      call massin (a(ld06),a(ld07),a(ld08),nmmass)
      endif
c
c     read in rigid body mass, inertia tensor, initial velocities
c
  149 if (numrbi.ne.0) call inrtap(a(n50+6*nmmat),a(n52+9*nmmat),
     1 a(n53+3*nmmat),a(n54+3*nmmat),numrbi)
c
c     nonreflecting boundary segments
c
      if (nnrbs.gt.0) then
      lrb1=locend
      lrb2=lrb1+4*nnrbs
      lrb3=lrb2+nnrbs
      lrb4=lrb3+nnrbs
      locend=lrb4
      call expndm (locend)
      call gtnrbs (a(lrb1),nnrbs,a(lc11))
      endif
c
      if (itemp.eq.-9999) then
      itsc=locend
      itbs=itsc+numnp
      itlc=itbs+numnp
      locend=itlc+numnp
      call expndm(locend)
      call treadr(a(itsc),a(itbs),a(itlc),numnp)
      endif
c
c     1d slidelines
c
      if (numsl(1).gt.0) then
      call bslinp (locend)
      endif
c
      n80=locend
      n80a=n80+nmmat
      n80b=n80a+nmmat
      locend=n80b+nmmat
      call expndm(locend)
      if (ndamp.ne.0) call rdrdmp(a(n80),a(n80a),a(n80b),ndamp,nmmat)
c
      write(13,260) locend
      write ( *,310) locend
c
      call empty(13)
c
      return
c
  150 format(2i5,e10.0,5i5,3e10.0,i5)
  152 format (4i5,e10.0,i5)
  160 format(12a6,a2,1x,a5)
  165 format('1',///1x,12a6,//1x,70('*'),//,10(5x,a63,/),//1x,70('*'),
     1   /10x,'dyna3d (version ',a8,')  compiled ',a8)
  170 format(///,
     1 '            code input version =19',a2,/,
     2 '            formats            =',a5,///)
  179 format (///' ******************************',///,
     13x,' warning a time step scale factor that exceeds',/,
     23x,' unity may cause instabilities.',
     3        ///' ******************************',//)
  180 format(15i5,a5)
  181 format(16i5)
  182 format(3e10.0,2i5,2e10.0,2i5,e10.0)
  183 format(16i5)
  184 format(e15.0)
  185 format(i5,6i10,3i5)
  186 format(5i5,a5)
  187 format(6i10)
  188 format(6i5,a5,i5,e10.0)
  189 format(i5,6i10,e10.0,e5.0)
  190 format(//' c o n t r o l   i n f o r m a t i o n'//
     1 4x,'number of materials............................',i7//
     2 4x,'number of nodes................................',i7//
     3 4x,'number of solid elements.......................',i7//
     4 4x,'number of beam elements........................',i7//
     5 4x,'number of shell elements.......................',i7//
     6 4x,'number of thick shell elements.................',i7//
     7 4x,'number of interface segments for linking.......',i7//
     8 4x,'output interval for interface file.............',e14.4//,
     9 4x,'factor for minimum shell element time step.....',e14.4/)
  194 format(
     1 4x,'number of nodal printout blocks................',i7//
     2 4x,'number of solid element printout blocks........',i7//
     3 4x,'number of beam element printout blocks.........',i7//
     4 4x,'number of shell element printout blocks........',i7//
     5 4x,'number of thick shell element printout blocks..',i7//
     6 4x,'problem status report interval in printer file.',i7/)
  196 format(
     5 4x,'number of sliding boundary planes..............',i7//
     5 4x,'number of sliding boundary planes w/ failure ..',i7//
     8 4x,'number of points in density vs. depth curve....',i7//
     9 4x,'overpressure option (eq.0 no,  eq.1 yes).......',i7//
     $ 4x,'number of rigid body merge cards...............',i7//
     $ 4x,'number of cross section definitions............',i7//
     $ 4x,'output interval for cross section forces.......',e10.2//
     $ 4x,'number of load curves..........................',i7//
     $ 4x,'number of concentrated load cards..............',i7//
     $ 4x,'number of traction cards.......................',i7//
     $ 4x,'number of prescribed velocity cards............',i7/)
  200 format(
     1 4x,'number of rigid wall definitions...............',i7//
     2 4x,'number of nodal constraint cards ..............',i7//
     3 4x,'initialization of velocities...................',i7/
     410x,'eq.0,  velocities are initialized to zero      ' /
     510x,'eq.1,  initial velocities are read in          ' //
     6 4x,'number of sliding interface definitions........',i7//
     7 4x,'x-dir base acceleration........................',i7/
     810x,'eq.0,  no                                      ',  /
     910x,'eq.1,  yes                                     ',  //
     $ 4x,'y-dir base acceleration........................',i7/
     $10x,'eq.0,  no                                      ',  /
     $10x,'eq.1,  yes                                     ',  //
     $ 4x,'z-dir base acceleration........................',i7/
     $10x,'eq.0,  no                                      ',  /
     $10x,'eq.1,  yes                                     ',  //)
  210 format(
     1 4x,'x-dir angular velocity.........................',i7/
     210x,'eq.0,  no                                      ',  /
     310x,'eq.1,  yes                                     ',  //
     4 4x,'y-dir angular velocity.........................',i7/
     510x,'eq.0,  no                                      ',  /
     610x,'eq.1,  yes                                     ',  //
     7 4x,'z-dir angular velocity.........................',i7/
     810x,'eq.0,  no                                      ',  /
     910x,'eq.1,  yes                                     ',  //
     $ 4x,'no. of solid elements for momentum depositon...',i7//
     $ 4x,'number of detonation points....................',i7//)
  220 format(
     $ 4x,'termination time...............................',e10.2//
     $ 4x,'time step between dumps of time history data...',e10.2//
     $ 4x,'time step between dumps of complete state data.',e10.2//
     $ 4x,'number of time steps between restart dumps.....',i6   //
     $ 4x,'no. of time steps between running rstrt dumps..',i6   //
     $ 4x,'initial time step size.........................',e10.2/
     $10x,'eq.0.0,  dyna3d picks initial step size        ',  //
     $ 4x,'scale factor for sliding interface penalties...',e10.2//
     $ 4x,'thermal effects option.........................',i7/
     $10x,'eq.0:  no thermal effects                      ',  /
     $10x,'eq.n:  nodal temps scaled by load ftn -n-      ',  /
     $10x,'lt.0:  nodal temps are on disk file            ',  //
     $10x,'eq.-9999:  nodal temps are in input            ',  //
     $ 4x,'viscosity reset option.........................',i7/
     $10x,'eq.0:  default viscosities set by dyna3d       ',  /
     $10x,'eq.1:  default viscosities read in             ',  //)
  225 format(
     $ 4x,'time step scale factor.........................',e10.2//
     $ 4x,'number of rigid body joints....................',i7//
     $ 4x,'number of extra node blocks....................',i7//
     $ 4x,'number of shell-solid interfaces...............',i7//
     $ 4x,'number of tie-breaking shell slidelines........',i7//
     $ 4x,'number of blocks of tied nodes with failure....',i7//
     $ 4x,'load curve for maximum timestep................',i7//)
  226 format(/
     &  4x,'spring-damper input option.....................',i7/
     & 10x,'eq.0:  no input                                ',  /
     & 10x,'eq.1:  discrete springs and dampers defined    ',  //
     &  4x,'number of rigid body inertia definitions.......',i7//
     &  4x,'shell strain dump flag.........................',i7//
     &  4x,'hughes-liu normal computation options..........',i7/
     & 10x,'eq.-2: unique nodal fibers per hughes-liu'/
     & 10x,'eq.-1: compute normals each time step'/
     & 10x,'eq.0 : default set to -1     '/
     & 10x,'eq.1 : compute on restart'/
     & 10x,'eq.n : compute on restart and every nth step'//,
     &  4x,'thickness modification for membrane strains....',i7/
     & 10x,'eq.0:  no                                      ',  /
     & 10x,'eq.1:  yes                                     ',  //)
  227 format(
     $ 4x,'shell formulation basis........................',i7/
     $10x,'eq.1:  hughes-liu shell theory                 ',  /
     $10x,'eq.2:  belytschko-lin-tsay shell theory        ',  /
     $10x,'eq.3:  bciz                             '/
     $10x,'eq.4:  c0-triangular element            '/
     $10x,'eq.5:  membrane element                 '/
     $10x,'eq.6:  yase                            '//
     $ 4x,'number of non-reflecting boundary segments.....',i7//
     $ 4x,'number of single point constraint nodes........',i7//
     $ 4x,'number of spc coordinate system definitions....',i7//
     $ 4x,'reduction factor for tsmin.....................',e10.2//
     $ 4x,'# of user specified beam integration rules.....',i7//
     $ 4x,'max number of integration points reqd (beams) .',i7//
     $ 4x,'# of user specified shell integration rules....',i7//
     $ 4x,'max number of integration points reqd (shells).',i7//
     $ 4x,'convergence check interval (dynamic relaxation)',i7//
     $ 4x,'convergence tolerance for dynamic relaxation...',e10.2/)
 2227 format(
     1 4x,'time step size calculation for 4-node shells...',i7/
     & 10x,'eq.0:  based on longest element side          ',  /
     & 10x,'eq.1:  based on longest element diagonal      ',  /
     & 10x,'eq.2:  based on bar wave speed max side       ',  /)
  228 format(
     $ 4x,'iterative plane stress plasticity for shells ..',i7/
     & 10x,'eq.1:  vectorized with three iterates         ',  /
     & 10x,'eq.2:  nonvectorized iterations as needed     ',  /
     & 10x,'eq.3:  noniterative approximate radial return ',  //
     &  4x,'time zero printout of element time step size .',i7/
     & 10x,'eq.0:  no printout                             ',  /
     & 10x,'eq.1:  print dt for each element at t=0        ',  //
     $ 4x,'number of 1d slidelines........................',i7//
     $ 4x,'mass coef. for rayleigh damping ...............',e10.2//
     $ 4x,'number of matls for stif. rayleigh damping.....',i7/)
  260 format('1'//,' s t o r a g e   a l l o c a t i o n   '///
     1 4x,'storage needed for solution....................',i9/)
  310 format (//' storage needed for solution=',i9//)
  320 format(3i5)
  330 format('1',//,' d i s c r e t e   e l e m e n t s',/,
     &  4x,'number of spring-damper material types.........',i7//
     &  4x,'number of discrete springs and dampers.........',i7//
     &  4x,'number of lumped masses........................',i7/)
c
c     error termination due to badly formatted data
c
  400 call termin (txts,mssg,lcount,1)
c
      end
      subroutine rdrdmp(dampk,ym,pr,ndamp,nmmat)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... read stiffness data for rayleigh damping
c
      character*80 txts
      dimension dampk(*),ym(*),pr(*)
c
      call azero(dampk,nmmat)
      call azero(ym,nmmat)
      call azero(pr,nmmat)
      do 10 n=1,ndamp
      call gttxsg(txts,icount)
      read(unit=txts,fmt=100,err=900) nmat,dampk(nmat),ym(nmat),
     1   pr(nmat)
   10 continue
c
      write(13,200)
      do 20 n=1,nmmat
      write(13,300) n,dampk(n),ym(n),pr(n)
   20 continue
      return
c
  900 write(*,910)
      write(13,910)
      call adios(2)
c
  100 format(i5,3e10.0)
  200 format(/5x,'stiffness proportional rayleigh damping',
     1 ' by material',
     2  /5x,'material',5x,'damping coef.',5x,
     3  'equiv. youngs modulus',5x,'equiv. poissons ratio')
  300 format(5x,i5,10x,e10.2,15x,e10.2,15x,e10.2)
  910 format(//5x,'error reading rayleigh damping data by material')
      end
      subroutine lnkcrd(x)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... routine to read coordinates from nike3d stress initialization file
c
      character*8 namef                                                 vax75
      common/rwint/intiad,ncint1,nhint1,nsint1
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
c
      dimension x(*)
c
c.... write link file name to terminal
c
      call getnam(7,namef)
      write( *,1000) namef
c
c.... read new coordinates from link file
c
      call rdabsg(iob7,x,3*ncint1,intiad,1)
      call riosta(iob7)
      intiad=intiad+3*ncint1
c
      write( *,1010) ncint1
 1010 format(/3x,i8,' nodal coordinates initialized from nike3d link')
 1000 format(/5x,'dyna3d initialized from NIKE3D file ',a8)
c
      return
      end
      subroutine inrtap(rbv,xinrta,xmass,rbc,numrbi)
c     implicit double precision (a-h,o-z)                                    dp
      dimension xinrta(3,3,*),xmass(*),rbc(3,*),rbv(6,*)
      character*80 txts,mssg
      write(13,20)
      mssg =' error reading rigid body definition cards'
      do 10 i=1,numrbi
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=30,err=60) n,(rbc(j,n),j=1,3),xmass(n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=40,err=60) xi11,xi12,xi13,xi22,xi23,xi33
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=40,err=60) (rbv(j,n),j=1,6)
      xinrta(1,1,n)=xi11
      xinrta(2,1,n)=xi12
      xinrta(3,1,n)=xi13
      xinrta(1,2,n)=xi12
      xinrta(2,2,n)=xi22
      xinrta(3,2,n)=xi23
      xinrta(1,3,n)=xi13
      xinrta(2,3,n)=xi23
      xinrta(3,3,n)=xi33
      write(13,50) n,(rbc(j,n),j=1,3),xmass(n),
     1 xi11,xi12,xi13,xi22,xi23,xi33,(rbv(j,n),j=1,6)
   10 continue
      return
   20 format(////' r i g i d   b o d y   i n e r t i a   ',/,
     1 20x,'a n d   v e l o c i t y   d e f i n i t i o n s'//)
   30 format(i5,4e10.0)
   40 format(6e10.0)
   50 format (/////' rigid body definition for body ',i3,///
     1 '     center of xmass - xc=',e14.4,'  yc=',e14.4,'  zc=',e14.4,//
     2 '     translational xmass = ',e14.4,//
     3 '     inertia tensor',/,
     4 '        ixx=',e14.4,'  ixy=',e14.4,'  ixz=',e14.4,/,
     5 '            ',14x,  '  iyy=',e14.4,'  iyz=',e14.4,/,
     6 '            ',14x,  '      ',14x,  '  izz=',e14.4,//,
     7 '     initial velocity',/,
     8 '        vx =',e14.4,'  vy =',e14.4,'  vz =',e14.4,/,
     9 '        wxx=',e14.4,'  wyy=',e14.4,'  wzz=',e14.4,////)
c
c     error termination due to badly formatted data
c
   60 call termin (txts,mssg,lcount,1)
c
      end
      subroutine sarray (a,v,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension a(*)
      if (n.ne.0) then
      do 10 i=1,n
   10 a(i)=v
      endif
      return
      end
      subroutine jrd(rdat,idat,njt)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      common/bk80/irdat,iidat,ndat,nl(6),ll(6),lo(6)
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      logical large
      common/bigprb/large
c
      character atype(6)*16,an(6)*4
      character*80 txts,mssg
      dimension rdat(1),idat(njt,1),nn(6)
c
      data nn/2,4,4,4,4,6/
      data atype/'spherical       ',
     2           'revolute        ',
     3           'cylinderical    ',
     4           'planar          ',
     5           'universal       ',
     6           'translational   '/
      data an/'nd 1','nd 2','nd 3','nd 4','nd 5','nd 6'/
c
      do 20 i=1,6
      nl(i)=0
   20 continue
c
      write(13,3)
      do 50 i=1,njt
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=6,err=140) rdat(i),(idat(i,j),j=1,7)
      else
      read(unit=txts,fmt=1,err=140) rdat(i),(idat(i,j),j=1,7)
      endif
      if (idat(i,1).le.6.and.idat(i,1).ge.1) go to 30
      write(13,2) i,idat(i,1)
      call adios(2)
   30 if (rdat(i).le.0.0) rdat(i)=1.0
      nl(idat(i,1))=nl(idat(i,1))+1
      idat(i,8)=i
   50 continue
c
      lo(1)=0
      do 70 i=2,6
      lo(i)=lo(i-1)+nl(i-1)
   70 continue
c
      do 80 i=1,6
      il=nl(i)/128
      ll(i)=nl(i)-128*il
      nl(i)=il
      if (ll(i).ne.0) nl(i)=nl(i)+1
   80 continue
c
      call jsort(rdat,idat,njt,njt,1,8,1,1)
c
      do 120 l=1,6
      if (nl(l).eq.0) go to 120
      write(13,4) atype(l),(an(ii),ii=1,nn(l))
      nj=128*(nl(l)-1)+ll(l)
      jpnt=lo(l)
      nnlp1=nn(l)+1
      call jsort(rdat,idat,njt,nj,1,8,jpnt+1,8)
      do 110 j=1,nj
      jj=j+jpnt
      write(13,5) idat(jj,8),rdat(jj),(idat(jj,ii),ii=2,nnlp1)
  110 continue
  120 continue
c
      return
c
c     error termination due to badly formatted data
c
  140 write (unit=mssg,fmt=7) i
      call termin (txts,mssg,lcount,1)
c
c
    1 format(e10.0,7i5)
    6 format(e10.0,i5,6i8)
    2 format(1x/' *** error(jrd) --- joint ',i5,' type is ',
     1       i5/'     the allowable values are 1 through 6')
    3 format('1 j o i n t   c o n s t r a i n t s')
    4 format(1x/1x,a16/1x,'joint',1x,' stiffness',6(2x,a4))
    5 format(1x,i5,1x,e10.3,6(1x,i5))
    7 format(' error reading joint card no. #', i5)
c
      end
      subroutine jsort(rdat,idat,kdat,n,ir,ij,jpnt,icol)
c     implicit double precision (a-h,o-z)                                    dp
c
c
c
      dimension rdat(kdat,1),idat(kdat,1)
c
      logical xchng
c
      igap=n
      ipnt=jpnt-1
c
   10 igap=igap/2
      imax=n-igap
c
   15 xchng=.false.
      do 40 i=1,imax
      if (idat(i+ipnt,icol).le.idat(i+igap+ipnt,icol)) go to 40
      xchng=.true.
c
      igap1=i+ipnt
      igap2=i+ipnt+igap
c
      do 20 j=1,ir
      rstore=rdat(igap1,j)
      rdat(igap1,j)=rdat(igap2,j)
      rdat(igap2,j)=rstore
   20 continue
c
      do 30 j=1,ij
      istore=idat(igap1,j)
      idat(igap1,j)=idat(igap2,j)
      idat(igap2,j)=istore
   30 continue
c
   40 continue
c
      if (xchng) go to 15
      if (igap.gt.1) go to 10
c
      return
      end
      subroutine intrlb(rule,nubir,mpubr,csprop,nmmat)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in user specified integration rules for beam elements
c
      character*80 txts,mssg
      dimension rule(mpubr,3,1),csprop(24,1)
c
      write(13,30)
      mssg =' error reading integration rules for beam elements'
      do 20 i=1,nubir
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=40,err=100) nip,rule(mpubr,2,i)
      rule(mpubr,1,i)=float(nip)
      write(13,50) i,nip,rule(mpubr,2,i)
      wght=0.0
      if (nip.gt.mpubr-1) then
      mpubr=mpubr-1
      write( *,90) nip,mpubr
      write(13,90) nip,mpubr
      call adios(2)
      endif
      do 10 j=1,nip
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=60,err=100)
     1 rule(j,1,i),rule(j,2,i),rule(j,3,i)
      if (rule(j,3,i).eq.0.0) rule(j,3,i)=1.0
      wght=wght+rule(j,3,i)
      write(13,70) j,rule(j,1,i),rule(j,2,i),rule(j,3,i)
   10 continue
      if (abs(wght-4.0).gt.1.e-05) then
      scale=4./wght
      do 15 j=1,nip
      rule(j,3,i)=scale*rule(j,3,i)
   15 continue
      endif
   20 continue
      do 25 i=1,nmmat
      if (csprop(4,i).lt.0.0) then
      irule=-csprop(4,i)
      csprop(2,i)=rule(mpubr,1,irule)
      endif
   25 continue
c
      return
c
   30 format('1',//,
     1' u s e r   s p e c i f i e d   i n t e g r a t i o n   r u l e s'
     2/,'                        (beam elements)                ',//)
   40 format (i5,e10.0)
   50 format (//,'  integration rule',i3,' has ',i3,' points with ',/,
     1 '  relative area in percent =',e14.4,///)
   60 format (3e10.0)
   70 format (' point=',i3,6x,'s=',e14.4,6x,'t=',e14.4,6x,'wgt=',e14.4)
   90 format(///'  number of integration points in user defined rule',
     1 i3,/,'  exceeds specified maximum (card 6):',i3,///)
c
c     terminate if badly formatted data
c
  100 call termin (txts,mssg,lcount,1)
c
      end
      subroutine intrls(rule,nusir,mpusr,csprop,nmmat)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in user specified integration rules for shell elements
c
      character*80 txts,mssg
      dimension rule(mpusr,3,1),csprop(24,1)
c
      write(13,30)
      mssg =' error reading integration rules for shell elements'
      do 20 i=1,nusir
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=40,err=100) nip,iesopt
      rule(mpusr,1,i)=float(nip)
      if (iesopt.eq.0) then
      write(13,50) i,nip
      else
      write(13,80) i,nip
      endif
      dist=2./float(nip)
      firp=dist/2.-1.0
      wght=0.0
      if (nip.gt.mpusr-1) then
      mpusr=mpusr-1
      write( *,90) nip,mpusr
      write(13,90) nip,mpusr
      call adios(2)
      endif
      do 10 j=1,nip
      if (iesopt.eq.0) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=60,err=100) rule(j,1,i),rule(j,2,i),ir
      rule(j,3,i)=float(ir)
      if (rule(j,2,i).eq.0.0) rule(j,2,i)=1.0
      else
      rule(j,1,i)=firp
      rule(j,2,i)=dist
      rule(j,3,i)=0.0
      firp=firp+dist
      endif
      wght=wght+rule(j,2,i)
      ir=nint(rule(j,3,i))
      write(13,70) j,rule(j,1,i),rule(j,2,i),ir
   10 continue
      if (abs(wght-2.0).gt.1.e-05) then
      scale=2./wght
      do 15 j=1,nip
      rule(j,2,i)=scale*rule(j,2,i)
   15 continue
      endif
   20 continue
      do 25 i=1,nmmat
      if (csprop(4,i).lt.0.0) then
      irule=-csprop(4,i)
      csprop(2,i)=rule(mpusr,1,irule)
      endif
   25 continue
c
      return
c
   30 format('1',//,
     1' u s e r   s p e c i f i e d   i n t e g r a t i o n   r u l e s'
     2/,'                        (shell elements)                ',//)
   40 format (2i5)
   50 format (//,'  integration rule',i3,' has ',i3,' points.',///)
   60 format (2e10.0,i5)
   70 format (' point=',i3,6x,'zeta=',e14.4,6x,'weight=',e14.4,6x,
     1 ' optional material #=',i3)
   80 format (//,'  integration rule',i3,' has ',i3,
     1           ' equally spaced points',///)
   90 format(///'  number of integration points in user defined rule',
     1 i3,/,'  exceeds specified maximum (card 6):',i3,///)
   95 format(' error in defining shell integration rule no. ',i3)
c
c     terminate if badly formatted data
c
  100 write (unit=mssg,fmt=95) i
      call termin (txts,mssg,lcount,1)
c
      end
      subroutine matin(ieost,eosp,ihgq,hgq,iqtype,bqs,csprop,
     1 mtype,ishlfm,ro,cm,idfrs,nmmat,lc2,mmauxs,iortho,ifb,isf)
c     implicit double precision (a-h,o-z)                                    dp
      parameter (nlen=966)
c
c     read material property data
c
      real*8 head                                                       vax75
      common/bk19/nconst(60),lenma,ncneos(15)
      common/bk22/prop(48)
      common/visc/ihgqdf,hgqcdf,ibqsdf,bqs1df,bqs2df
      common/matnum/nmmath,nmmatb,nmmats,nmmatt
      common/ingstr/nmigs,mtigs(12)
      common/shlopt/istrn,istupd,ibelyt,miter
      logical idrvon
      common/driver/ idrvon,idrvdm(10)
      common/rn/irnxx
      common /   / a(1)
      dimension mtype(*),ro(*),cm(48,*),ieost(*),eosp(48,*),ihgq(*),
     1 hgq(*),iqtype(*),bqs(3,*),csprop(24,*),ishlfm(*),head(2,12)
      character*80 txts,mssg
      logical ncf
c
      ncf=.false.
      isf=0
      if (idfrs.eq.1) then
      mssg =' error reading viscosity defaults before reading material c
     1ards'
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=210,err=200) ihgqdf,hgqcdf,ibqsdf,bqs1df
     1 ,bqs2df
      endif
      nmmath=0
      nmmatb=0
      nmmats=0
      nmmatt=0
      nmigs=0
      do 180 m=1,nmmat
c
c     read material control card
c
      mssg =' error reading material control card'
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=250,err=200) n,mtype(n),ro(n),ieost(n),ihgq(n)
     1 ,hgq(n),iqtype(n),(bqs(j,n),j=1,2),itype,igstr,ishlfm(n)
c
      if (mtype(n).eq.22.and.itype.eq.0) nconst(22)=14
      if (idrvon) idrvdm(4)=itype
c
      if (itype.eq.2.and.(mtype(n).eq.15.or.mtype(n).eq.19.or.mtype(n)
     1 .eq.22.or.mtype(n).eq.24.or.mtype(n).eq.35)) then
      isf=1
      endif
c
c.... check that an eos is specified if required and not if not
c
      if(itype.eq.0.and.ieost(n).eq.0.and.(mtype(n).eq.8.or.mtype(n)
     1 .eq.9.or.mtype(n).eq.10.or.mtype(n).eq.11.or.mtype(n).eq.15.or.
     2 mtype(n).eq.17)) then
      write(*,400) n,mtype(n),itype
      write(13,400) n,mtype(n),itype
      call adios(2)
      elseif(ieost(n).ne.0.and.(itype.ne.0.or.(itype.eq.0.and.
     1   (mtype(n).ne.8.and.mtype(n).ne.9.and.mtype(n).ne.10.and.
     2    mtype(n).ne.11.and.mtype(n).ne.15.and.mtype(n).ne.16.and.
     3    mtype(n).ne.17)))) then
      write(*,410) n,mtype(n),ieost(n),itype
      write(13,410) n,mtype(n),ieost(n),itype
      endif
      if (itype.eq.2) then
      if (mtype(n).eq.28.and.ibelyt.eq.1) then
      ishlfm(n)=2
      endif
      if (ishlfm(n).eq.0) then
      ishlfm(n)=ibelyt
      endif
      elseif (itype.eq.1) then
      if (ishlfm(n).eq.0) then
      ishlfm(n)=1
      endif
      endif
      ies=ieost(n)
      imt=mtype(n)
      nces=0
      if (ihgq(n).eq.3.and.itype.eq.0) ncf=.true.
      if (ihgq(n).eq.5.and.itype.eq.0) ncf=.true.
      bqs(3,n)=0.
      if (ies.ne.0) nces=ncneos(ies)
      if (ihgq(n).eq.0) ihgq(n)=ihgqdf
      if (hgq(n).eq.0.) hgq(n)=hgqcdf
      if (iqtype(n).eq.0) iqtype(n)=ibqsdf
      if (bqs(1,n).eq.0.0) bqs(1,n)=bqs1df
      if (bqs(2,n).eq.0.0) bqs(2,n)=bqs2df
      if (itype.eq.2.and.mtype(n).eq.2) iortho=1
      if (itype.eq.2.and.mtype(n).eq.21) iortho=1
      if (itype.eq.2.and.mtype(n).eq.22) iortho=1
      if (itype.eq.2.and.mtype(n).eq.23) iortho=1
      if (ihgq(n).eq.4.and.itype.eq.0) ifb=1
      if (ihgq(n).eq.5.and.itype.eq.0) ifb=1
      if (ro(n).gt.0.) go to 10
      write ( *,240) n
      call adios (2)
   10 if (n.gt.nmmat) go to 190
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=230,err=200) (head(1,j),j=1,12)
c
c     read material properties
c
      mssg =' error reading material property cards'
      if (mtype(n).eq.1.or.mtype(n).eq.3.or.mtype(n).eq.6.or.mtype(n).eq
     1 .7.or.mtype(n).eq.9.or.mtype(n).eq.12.or.mtype(n).eq.13.or.mtype
     2 (n).eq.17.or.mtype(n).eq.20.or.mtype(n).eq.28.or.mtype(n).eq.30.
     3 .or.mtype(n).eq.31.or.mtype(n).eq.32.or.mtype(n).eq.34) then
      ifst=1
      ilst=5
      do 20 j=1,6
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
   20 continue
      endif
c
      if (mtype(n).eq.2.or.mtype(n).eq.23) then
      ifst=1
      ilst=3
      do 30 j=1,6
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+3
      ilst=ilst+3
   30 continue
      endif
c
      if (mtype(n).eq.4.or.mtype(n).eq.8.or.mtype(n).eq.10.or.mtype(n)
     1 .eq.15.or.mtype(n).eq.18.or.mtype(n).eq.19.or.mtype(n).eq.26
     2 .or.mtype(n).eq.27.or.mtype(n).eq.29) then
      ifst=1
      ilst=8
      do 40 j=1,6
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+8
      ilst=ilst+8
   40 continue
      endif
c
      if (mtype(n).eq.5.or.mtype(n).eq.14) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (prop(i),i=1,6),cm(47,n),cm(48,n)
      ifst=6
      ilst=10
      do 50 j=1,5
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
   50 continue
      call sets5 (cm(1,n))
      endif
c
      if (mtype(n).eq.11) then
      ifst=1
      ilst=5
      do 60 j=1,3
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (prop(i),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
   60 continue
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (prop(i),i=16,22)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=270,err=200) (prop(i),i=23,27)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=270,err=200) (prop(i),i=28,32)
      call sets11 (cm(1,n),ro(n))
      endif
c
      if (mtype(n).eq.21) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=1,3)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=4,6),(cm(i,n),i=23,25)
      ifst=7
      ilst=9
      do 70 j=1,4
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+3
      ilst=ilst+3
   70 continue
      endif
c
      if (mtype(n).eq.22) then
      ifst=1
      ilst=3
      do 80 j=1,5
      call gttxsg (txts,lcount)
      if (j.eq.1) then
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst),cm(48,n),
     1 (cm(i,n),i=45,47)
      elseif (j.gt.1.and.j.lt.5) then
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      elseif (j.eq.5) then
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      endif
      ifst=ifst+3
      ilst=ilst+3
   80 continue
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=16,18),(cm(i,n),i=26,3
     1 0)
      endif
c
      if (mtype(n).eq.24) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(1,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(2,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(3,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(4,n),cm(5,n),cm(47,n),cm(48,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=6,13)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=14,21)
      endif
c
      if (mtype(n).eq.25) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(1,n),cm(2,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=3,7)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=8,10),
     1 cm(18,n),cm(19,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(11,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(12,n),cm(17,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(13,n)
      endif
c
      if (mtype(n).eq.33) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) cm(1,n),cm(2,n),cm(3,n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=4,8)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=9,11)
      call gttxsg (txts,lcount)
      call gttxsg (txts,lcount)
      call gttxsg (txts,lcount)
      endif
c
      if(mtype(n).eq.35) then
      ifst=1
      ilst=8
      do 85 j=1,6
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (cm(i,n),i=ifst,ilst)
      ifst=ifst+8
      ilst=ilst+8
   85 continue
      endif
c
c     set material constants
c
      if (mtype(n).eq.1) call sets1 (cm(1,n))
      if (mtype(n).eq.2) call sets2 (cm(1,n),cm(13,n))
      if (mtype(n).eq.3) call sets1 (cm(1,n))
      if (mtype(n).eq.6) call sets6 (cm(1,n))
      if (mtype(n).eq.8) call sets8 (cm(1,n),ro(n))
      if (mtype(n).eq.10) call sets10 (cm(1,n))
      if (mtype(n).eq.12) call sets12 (cm(1,n))
      if (mtype(n).eq.13) call sets12 (cm(1,n))
      if (mtype(n).eq.15) call sets15 (cm(1,n))
      if (mtype(n).eq.28) call sets1 (cm(1,n))
      if (mtype(n).eq.30) call sets1 (cm(1,n))
      if (mtype(n).eq.32) call sets6 (cm(1,n))
      if (mtype(n).eq.34) call sets1 (cm(1,n))
      if (mtype(n).eq.35) call sets35 (cm(1,n))
      if (mtype(n).eq.16) then
c     mmauxs=mmauxs+1
c     cm(48,n)=lc2+(mmauxs-1)*nlen
      call sets16 (cm(1,n),ieost(n))
      endif
      if (mtype(n).eq.17) call sets1 (cm(1,n))
      if (mtype(n).eq.20) call sets1 (cm(1,n))
      if (mtype(n).eq.21) call sets21 (cm(1,n))
c
      if (igstr.ne.0) then
      nmigs=nmigs+1
      mtigs(nmigs)=n
      endif
      if (mtype(n).eq.25) call sets25 (cm(1,n))
      call empty(13)
c
c     integrated truss and beam element
c
c     input order: kappa,npoints,iprint
c     fibly(1),fibly(2),fiblz(1),fiblz(2),ybar,zbar
c
c     integrated membrane and shell element
c
c     input order: kappa,npoints,iprint
c                  fibl(1),...,fibl(4),zbar
c
c     now read in cross-sectional data
c
      if (itype.eq.0) go to 110
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=230,err=200) (head(2,j),j=1,12)
      mssg =' error reading section property cards'
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (csprop(j,n),j=1,8)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (csprop(i,n),i=9,14)
c
      if (csprop(1,n).eq.0.0) csprop(1,n)=1.00
      if (csprop(2,n).eq.0.0) csprop(2,n)=2.00
      if (mtype(n).eq.28) csprop(2,n)=1.0
      if (mtype(n).eq.29) csprop(2,n)=1.0
      if (itype.eq.2.and.ishlfm(n).eq.3) then
      csprop(2,n)=3.000001*csprop(2,n)
      endif
      if (itype.ne.1) go to 90
      csprop(15,n)=csprop(14,n)
      csprop(16,n)=csprop(14,n)
      csprop(14,n)=csprop(13,n)
      nmmatb=nmmatb+1
      if (ishlfm(n).eq.3) csprop(2,n)=1.0
      csprop(4,n)=csprop(2,n)
      go to 120
   90 if (itype.ne.2) go to 100
      csprop(14,n)=0.0
      csprop(15,n)=0.0
      csprop(16,n)=0.0
      if (ibelyt.eq.1.and.irnxx.ge.0) then
      csprop(14,n)=csprop(13,n)
      csprop(15,n)=csprop(13,n)
      csprop(16,n)=csprop(13,n)
      elseif (csprop(13,n).ne.0.0) then
      csprop(13,n)=0.0
      if (ibelyt.eq.1) then
      write (13,280)
      write ( *,280)
      else
      write (13,290)
      write ( *,290)
      endif
      write (13,300)
      write ( *,300)
      endif
      nmmats=nmmats+1
      go to 120
  100 nmmatt=nmmatt+1
      go to 120
  110 nmmath=nmmath+1
c
  120 if (mtype(n).eq.22) then
      nip=nint(csprop(2,n))
      if (ishlfm(n).eq.3) nip=nip/3
      if (itype.eq.0) then
      nip=0
      endif
      mmauxs=mmauxs+1
      langle=lc2+(mmauxs-1)*nlen
      call sets22 (cm(1,n),a(langle),nip)
      if (itype.ne.0) then
      cm(48,n)=float(langle)+.010
      endif
      endif
      if (mtype(n).eq.23) then
      nip=nint(csprop(2,n))
      if (ishlfm(n).eq.3) nip=nip/3
      if (itype.eq.0) then
      nip=0
      nconst(23)=18
      endif
      mmauxs=mmauxs+1
      lthrpr=lc2+(mmauxs-1)*nlen
      cm(48,n)=float(lthrpr)+.1
      call sets23 (cm(1,n),a(lthrpr),nip)
      endif
      call empty(13)
      if (ieost(n).eq.0) go to 170
      mssg =' error reading eos property cards'
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=230,err=200) (head(2,j),j=1,12)
c
c     read in equation of state constants
c
      if (ieost(n).eq.1.or.ieost(n).eq.6) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (eosp(i,n),i=1,8)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (eosp(i,n),i=9,16)
      endif
c
      if (ieost(n).eq.2.or.ieost(n).eq.3.or.ieost(n).eq.4) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (eosp(i,n),i=1,8)
      endif
c
      if (ieost(n).eq.5) then
      ifst=1
      ilst=4
      do 130 j=1,9
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=270,err=200) (eosp(i,n),i=ifst,ilst)
      ifst=ifst+4
      ilst=ilst+4
  130 continue
      endif
c
      if (ieost(n).eq.7) then
      ifst=1
      ilst=5
      do 140 j=1,6
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=220,err=200) (eosp(i,n),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
  140 continue
      call setes7 (eosp(1,n))
      endif
c
      if (ieost(n).eq.8) then
      ifst=1
      ilst=5
      do 150 j=1,9
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=270,err=200) (eosp(i,n),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
  150 continue
      call setes8 (eosp(1,n))
      endif
c
      if (ieost(n).eq.9) then
      ifst=1
      ilst=5
      do 160 j=1,7
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=270,err=200) (eosp(i,n),i=ifst,ilst)
      ifst=ifst+5
      ilst=ilst+5
  160 continue
      if (ieost(n).eq.9) call setes9 (eosp(1,n))
      endif
c
      if (ieost(n).eq.11) then
      mmauxs=mmauxs+1
      eosp(48,n)=lc2+(mmauxs-1)*nlen
      call stes11 (eosp(1,n))
      endif
c
c     write out material properties
c
  170 call printm (n,mtype(n),ro(n),cm(1,n),ieost(n),eosp(1,n),ihgq(n)
     1 ,hgq(n),iqtype(n),bqs(1,n),csprop(1,n),head,itype,a(langle),a
     2 (lthrpr),nip,ishlfm(n))
c
  180 continue
c
      if (ncf) nconst(7)=24
      return
c
  190 write (13,260) n,nmmat
c
      call adios (2)
c
c
c     error termination due to improperly formatted data
c
  200 call termin (txts,mssg,lcount,1)
c
  210 format(i5,e10.0,i5,2e10.0)
  220 format(8e10.0)
  230 format(12a6)
  240 format(' material',i4,' density is zero-fatal')
  250 format(2i5,e10.0,2i5,e10.0,i5,2e10.0,3i5)
  260 format(' element material',i4,' is greater than nmmat=',i4)
  270 format(5e16.0)
  280 format(' ***warning*** if continuous normal updates are used',/,
     1  '               with the hughes-liu shell then the   ',/,
     2  '               reference surface must be the midsurface',//)
  290 format(' ***warning*** the reference surface must be the ',/,
     1  '              midsurface in the belytschko-tsay theory',//)
  300 format(' *** the reference surface has been redefined as the',/,
     1       '     midsurface',///)
  400 format(//5x,'Error in material definition',
     1 /10x,'material number:  ',i8,
     2 /10x,'  material type:  ',i8,
     3 /10x,'   element type:  ',i8,
     4 //5x,'An equation of state is required for this material type',
     5  /5x,'when used with solid elements, and no equation of state',
     6  /5x,'was defined.')
  410 format(//5x,'Error in material definition',
     1 /10x,'material number:  ',i8,
     2 /10x,'  material type:  ',i8,
     2 /10x,'       eos type:  ',i8,
     3 /10x,'   element type:  ',i8,
     4 //5x,'An equation of state is not permitted for this material',
     5  /5x,'type but was defined')
      end
      subroutine printm (n,mod,ro,cm,ieost,eosp,ihgq,hgq,iqtype,bqs,
     1 csprop,head,itype,angles,thrmpr,nip,ishlfm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     subroutine to print out material properties
c
      real*8 head                                                       vax75
      common/bk22/prop(48)
      common/visc/ihgqdf,hgqcdf,ibqsdf,bqs1df,bqs2df
      dimension cm(*),eosp(*),bqs(*),csprop(*),head(2,12),angles(*),
     1 thrmpr(48,*)
c
      if (n.eq.1) then
      write (13,310)
      write (13,320)
      write(13,321)
      write (13,330)
      write (13,340)
      write (13,350)
      write (13,360) ihgqdf,hgqcdf,ibqsdf,bqs1df,bqs2df
      endif
      write (13,370) (head(1,j),j=1,12)
      write (13,380) n,mod,ieost,ihgq,iqtype,ro,hgq,(bqs(j),j=1,2),itype
      if (itype.eq.2) then
      if (ihgq.le.3) ihgq=1
      if (ihgq.gt.3) ihgq=3
      endif
c
      go to (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,
     1 180,190,10,200,210,230,250,260,270,272,30,274,30,
     1   275,276,277,278,279), mod
c
c     model = 1    l i n e a r   i s o t r o p i c
c
   10 if (mod.ne.20) then
      write (13,390) (prop(i),i=1,2)
      else
      write (13,1000)(prop(i),i=1,5)
      endif
      go to 280
c
c     model = 2    l i n e a r   o r t h o t r o p i c
c
   20 write (13,400) (prop(i),i=1,10)
      write (13,440) (prop(i),i=20,22),(prop(i),i=11,17),(prop(i),i=23,2
     1 5)
      go to 280
c
c     model = 3    e l a s t o p l a s t i c (von mises yield criterion)
c
   30 write (13,480) (prop(i),i=1,5)
c
      go to 280
c
c     model = 4    t h e r m o e l a s t i c  -  p l a s t i c
c
   40 write (13,490) (cm(i),i=1,48)
c
      go to 280
c
c     model = 5    s o i l  a n d  c r u s h a b l e  f o a m
c
   50 write (13,500) (prop(i),i=1,6),(prop(i),i=11,19),(prop(i),i=21,29)
c
      go to 280
c
c     model = 6    v i s c o e l a s t i c
c
   60 if (prop(5).eq.0) then
      write (13,510) (prop(i),i=1,4)
      else
      write(13,512) (prop(i),i=1,4)
      endif
c
      go to 280
c
c     model = 7    f i n i t e   e l a s t i c   r u b b e r
c
   70 if (cm(2).eq.0.0) cm(2)=.463
      write (13,520) (cm(i),i=1,2)
c
      go to 280
c
c     model = 8    j w l   h i g h   e x p l o s i v e
c
   80 write (13,530) (prop(i),i=1,3)
c
      go to 280
c
c     model = 9    n u l l   m a t e r i a l
c
   90 write (13,540) (cm(i),i=1,2)
c
      go to 280
c
c     model = 10   h y d r o d y n a m i c
c                         e l a s t i c  -  p l a s t i c
c
  100 write (13,550) (prop(i),i=1,7),(prop(i),i=17,48)
c
      go to 280
c
c     model = 11   s t e i n b e r g - g u i n a n   m o d e l
c
  110 write (13,560) (prop(i),i=1,10)
      write (13,570) (prop(i),i=11,17)
      write (13,580) (prop(i),i=23,32)
c
      go to 280
c
c     model = 12    h y d r o d y n a m i c
c                         e l a s t i c  -  p l a s t i c
c
  120 write (13,760) (prop(i),i=1,3),prop(6)
c
      go to 280
c
c     model = 13    e l a s t i c - p l a s t i c   w i t h
c                         f a i l u r e    m o d e l
c
  130 write (13,770) (prop(i),i=1,6)
c
      go to 280
c
c     model = 14    s o i l   a n d   c r u s h a b l e   f o a m
c                            w i t h   f a i l u r e   m o d e l
c
  140 write (13,505) (prop(i),i=1,5),cm(47),prop(6),cm(48),
     1            (prop(i),i=11,19),(prop(i),i=21,29)
c
      go to 280
c
c     model = 15  j o h n s o n   p l a s t i c i t y   m o d e l
c
  150 write (13,610) (prop(i),i=1,13)
      write (13,620) (prop(i),i=17,21),
     1               (prop(i),i=41,43)
c
      go to 280
c
c     model = 16   p s e u d o   t e n s o r
c        c o n c r e t e / g e o l o g i c a l   m o d e l
c
  160 continue
      call prmt16 (13,cm,ieost,eosp,head)
c
      go to 280
c
c     model = 17    e l a s t o p l a s t i c (von mises yield criterion
c     with fracture
c
  170 write (13,820) (prop(i),i=1,5)
c
      go to 280
c
c     model = 18  p o w e r   l a w   p l a s t i c i t y
c
  180 write (13,840) (cm(i),i=1,4)
c
      go to 280
c
c     model = 19  s t r a i n r a t e   s e n s i t i v e
c                 p l a s t i c i t y
c
  190 write (13,850) (cm(i),i=1,8)
c
      go to 280
c
c     model = 21  t h e r m a l   o r t h o t r o p i c
c
  200 write (13,410) (prop(i),i=1,6),(prop(i),i=23,25),(prop(i),i=7,10)
      write (13,440) (prop(i),i=20,22),(prop(i),i=11,18),(prop(i),i=17,1
     1 9)
c
      go to 280
c
c     model = 22  o r t h o t r o p i c   d a m a g e   m o d e l
c
  210 write (13,400) (prop(i),i=1,10)
      if (itype.eq.0) then
      write (13,405)  prop(11)
      endif
      write (13,420) (prop(i),i=26,30),(prop(i),i=45,48)
      write (13,450) (prop(i),i=13,18)
      if (itype.ne.0) then
      write (13,460) (angles(i),i=1,nip)
      endif
      write (13,470)
      do 220 i=1,nip
      angles(i)=angles(i)/57.29577951
  220 continue
c
      go to 280
c
c     model = 23  t h e r m a l   o r t h o t r o p i c   c u r v e s
c
  230 write (13,430) (prop(i),i=1,6),prop(10)
      write (13,450) (prop(i),i=13,18)
      numpts=nint(prop(1))
      write (13,980) (thrmpr(i,1),i=1,numpts)
      write (13,860) (thrmpr(i,2),i=1,numpts)
      write (13,870) (thrmpr(i,3),i=1,numpts)
      write (13,880) (thrmpr(i,4),i=1,numpts)
      write (13,890) (thrmpr(i,5),i=1,numpts)
      write (13,900) (thrmpr(i,6),i=1,numpts)
      write (13,910) (thrmpr(i,7),i=1,numpts)
      write (13,920) (thrmpr(i,8),i=1,numpts)
      write (13,930) (thrmpr(i,9),i=1,numpts)
      write (13,940) (thrmpr(i,10),i=1,numpts)
      write (13,950) (thrmpr(i,11),i=1,numpts)
      write (13,960) (thrmpr(i,12),i=1,numpts)
      write (13,970) (thrmpr(i,13),i=1,numpts)
      if (nip.gt.0) then
      write (13,460) (thrmpr(i,14),i=1,nip)
      endif
      write (13,470)
      do 240 i=1,nip
      thrmpr(i,14)=thrmpr(i,14)/57.29577951
  240 continue
c
      go to 280
c
c     model = 24   i s o t r o p i c   p l a s t i c i t y
c
  250 write (13,990) (cm(i),i=1,21),cm(48),cm(47)
      go to 280
c
c     model = 25   t w o - i n v a r i a n t   c a p   m o d e l
c
  260 write (13,630) (cm(i),i=1,13),cm(16),cm(15),cm(17),
     1 cm(18),cm(19)
      call empty (13)
      return
c
  270 write (13,640) (cm(i),i=1,14)
      return
c
  272 write (13,644) (cm(i),i=1,3)
      go to 280
c
  274 write (13,645) (cm(i),i=1,2),(cm(i),i=9,16),(cm(i),i=17,24)
      go to 280
c
c     model = 31 f r a z e r   n a s h   r u b b e r
c
  275 write (13,1010) (cm(i),i=1,10)
      write (13,1020)  cm(11),cm(16),cm(17)
      return
c
c     model = 32    r a m b e r g  - o s g o o d   m o d e l
c
  276 continue
      write (13,1022) (prop(i),i=1,5)
c
      go to 280
c
c     model = 33 anisotropic plasticity  (roger logan model)
c
  277 write (13,995) (cm(i),i=1,11)
      go to 280
c
c     model = 34 anisotropic plasticity  (whirley-engelmann model)
c
  278 write (13,996) (cm(i),i=1,21,5)
      go to 280
c
c     model = 35 forming limit plasticity (logan model)
c
  279 write (13,997) cm(1),cm(9),cm(17),cm(25),cm(26),cm(10),
     1 cm(11),cm(12),cm(13),cm(18)
      go to 280
c
  280 if (itype.eq.0.and.ieost.eq.0) return
      if (itype.eq.0) go to 300
      if (csprop(1).gt.1.0.and.csprop(1).gt.csprop(2)) then
      csprop(2)=csprop(1)
      csprop(1)=1.0
      endif
      if (itype.eq.1) then
      if (csprop(2).lt.0.) csprop(3)=2.0
      write (13,780) (csprop(i),i=1,3)
      write (13,790) ishlfm,(csprop(i),i=9,16)
      return
      endif
      if (csprop(3).eq.0.) csprop(3)=3.0
      if (itype.eq.3) go to 290
      if (ishlfm.eq.3) then
      csprop(2)=csprop(2)/3.
      write (13,800) (csprop(i),i=1,4)
      csprop(2)=3.*csprop(2)
      else
      write (13,800) (csprop(i),i=1,4)
      endif
      write (13,810) ishlfm,(csprop(i),i=9,16)
      return
  290 if (csprop(2).eq.2.) csprop(2)=3.
      if (csprop(2).eq.4.) csprop(2)=5.
      write (13,830) (csprop(i),i=1,2)
      return
c
c     write out equation-of-state data when applicable
c
  300 write (13,650) (head(2,j),j=1,12)
      if (eosp(9).eq.0.0.and.ieost.eq.1) eosp(9)=1.0
      if (eosp(7).eq.0.0.and.ieost.eq.2) eosp(7)=1.0
      if (eosp(7).eq.0.0.and.ieost.eq.3) eosp(7)=1.0
      if (eosp(8).eq.0.0.and.ieost.eq.4) eosp(8)=1.0
      if (eosp(34).eq.0.0.and.ieost.eq.5) eosp(34)=1.0
      if (eosp(9).eq.0.0.and.ieost.eq.6) eosp(9)=1.0
      if (eosp(34).eq.0.0.and.ieost.eq.7) eosp(34)=1.0
c
      if (ieost.eq.1) write (13,660) (eosp(i),i=1,9)
      if (ieost.eq.2) write (13,670) (eosp(i),i=1,7)
      if (ieost.eq.3) write (13,680) (eosp(i),i=1,7)
      if (ieost.eq.4) write (13,690) (eosp(i),i=1,8)
      if (ieost.eq.5) write (13,700) (eosp(i),i=1,12)
      if (ieost.eq.5) write (13,710) (eosp(i),i=13,20)
      if (ieost.eq.5) write (13,720) (eosp(i),i=21,34)
      if (ieost.eq.6) write (13,730) (eosp(i),i=1,10)
      if (ieost.eq.7) write (13,590) (eosp(i),i=1,12)
      if (ieost.eq.7) write (13,600) (eosp(i),i=13,22),(eosp(j),j=30,32)
      if (ieost.eq.8) write (13,740) (eosp(i),i=1,43)
      if (ieost.eq.9) write (13,750) (eosp(i),i=1,33)
      if (ieost.eq.11) call pres11 (13,eosp)
c
      return
c
  310 format('1',//' m a t e r i a l   d e f i n i t i o n '///,
     1   ' material models                       '/,
     2   '     eq.1   isotropic elastic          '/,
     3   '     eq.2   orthotropic elastic        '/,
     4   '     eq.3   kinematic/isotropic elastic-plastic '/,
     5   '     eq.4   thermo-elastic-plastic     '/,
     6   '     eq.5   soil and crushable foam    '/,
     7   '     eq.6   viscoelastic         '/,
     8   '     eq.7   blatz-ko hyperelastic rubber  '/,
     9   '     eq.8   high explosive        '/,
     $   '     eq.9   fluid material              '/,
     $   '     eq.10  isotropic elastic-plastic hydrodynamic'/,
     $   '     eq.11  steinberg-guinan high-rate elastic-plastic'/,
     $   '     eq.12  isotropic elastic-plastic '/,
     $   '     eq.13  elastic-plastic with failure '/,
     $   '     eq.14  soil and crushable foam with failure ')
  320 format(
     1   '     eq.15  johnson/cook elastic-plastic '/,
     2   '     eq.16  concrete/geological material '/,
     3   '     eq.17  elastic-plastic with oriented cracks'/,
     4   '     eq.18  power law isotropic elastic-plastic  '/,
     5   '     eq.19  strain rate dependent elastic-plastic'/,
     6   '     eq.20  rigid      '/,
     7   '     eq.21  thermal orthotropic elastic'/,
     8   '     eq.22  fiber composite with  damage  '/,
     9   '     eq.23  thermal orthotropic elastic w/ variable props'/,
     $   '     eq.24  rate-dependent tabular elastic-plastic'/,
     $   '     eq.25  extended two-invariant geologic cap'/,
     $   '     eq.26  crushable metallic honeycomb    '/,
     $   '     eq.27  compressible mooney-rivlin hyperelastic rubber'/,
     $   '     eq.28  resultant plasticity '/,
     $   '     eq.29  not currently available')
  321 format(
     $   '     eq.30  closed form update elastic-plastic for shells '/,
     $   '     eq.31  frazer-nash hyperelastic rubber'/,
     $   '     eq.32  ramberg osgood elastic-plastic'/,
     $   '     eq.33  hill general anisotropic plasticity'/,
     $   '     eq.34  hill normal anisotropic plasticity for shells'/
     $   '     eq.35  elastic-plastic with forming limit diagram'/)
  330 format(
     1   ' equation-of-state models             '/,
     2   '     eq.1   linear polynomial         '/,
     3   '     eq.2   jwl high explosive        '/,
     4   '     eq.3   sack high explosive    '/,
     5   '     eq.4   gruneisen                 '/,
     6   '     eq.5   ratio of polynomials      '/,
     7   '     eq.6   linear polynomial with energy deposition'/,
     8   '     eq.7   initiation and reaction in he  '/,
     9   '     eq.8   compaction                '/,
     $   '     eq.9   tabulated                 '/,
     $   '     eq.10  not currently available'/,
     $   '     eq.11  pore collapse      '//)
  340 format(
     1   ' hourglass viscosity models                     '/,
     2   '     eq.1   standard                            '/,
     3   '     eq.2   flanagan-belytschko viscous form    '/,
     4   '     eq.3   full flanagan-belytschko viscous form    '/,
     5   '            (belytschko constant stress elements)    '/,
     6   '     eq.4   flanagan-belytschko stiffness form  '/,
     7   '     eq.5   full flanagan-belytschko stiffness form  '/,
     8   '            (belytschko constant stress elements)    '//)
  350 format(
     1   ' bulk viscosity models                '/,
     2   '     eq.1   standard                  '////)
  360 format(
     1   ' default viscosities                  '/,
     2   '     hourglass viscosity type..............',i5,/,
     3   '     hourglass viscosity coefficient.......',e12.4,/,
     4   '     bulk viscosity type...................',i5,/,
     5   '     quadratic bulk viscosity coefficient..',e12.4,/,
     6   '     linear bulk viscosity coefficient.....',e12.4,////)
  370 format(/1x,12a6/)
  380 format(' material constants set number .... ',i5//,
     1        10x,' material model ............. ',i5/,
     2        10x,' equation-of-state model .... ',i5/,
     3        10x,' hourglass viscosity model .. ',i5/,
     4        10x,' bulk viscosity model ....... ',i5//,
     5 5x,'den .............................. =', e12.4/,
     6 5x,'hourglass viscosity .............. =', e12.4/,
     7 5x,'quadratic bulk viscosity ......... =', e12.4/,
     8 5x,'linear bulk viscosity ............ =', e12.4/,
     9 5x,'element type ..................... =', i5/
     $ 5x,'     eq.0: 8-node solid element       '/
     $ 5x,'     eq.1: 2-node beam or truss element    '/
     $ 5x,'     eq.2: 4-node membrane or shell element'/
     $ 5x,'     eq.3: 8-node membrane or shell element'/)
  390 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4///)
  400 format(
     1 5x,'e(a) ............................. =', e12.4/,
     2 5x,'e(b) ............................. =', e12.4/,
     3 5x,'e(c) ............................. =', e12.4/,
     4 5x,'vnu(ba) .......................... =', e12.4/,
     5 5x,'vnu(ca) .......................... =', e12.4/,
     6 5x,'vnu(cb) .......................... =', e12.4/,
     7 5x,'g(ab) ............................ =', e12.4/,
     8 5x,'g(bc) ............................ =', e12.4/,
     9 5x,'g(ca) ............................ =', e12.4/,
     $ 5x,'material axes option ............. =', e12.4)
  405 format(
     $ 5x,'material axes change flag (solid). =', e12.4/
     $ 5x,'     eq.1: default                     '/
     $ 5x,'     eq.2: switch material axes a and b'/
     $ 5x,'     eq.3: switch material axes a and c'/)
  410 format(
     1 5x,'e(a) ............................. =', e12.4/,
     2 5x,'e(b) ............................. =', e12.4/,
     3 5x,'e(c) ............................. =', e12.4/,
     4 5x,'vnu(ba) .......................... =', e12.4/,
     5 5x,'vnu(ca) .......................... =', e12.4/,
     6 5x,'vnu(cb) .......................... =', e12.4/,
     7 5x,'alpha(a) ......................... =', e12.4/
     8 5x,'alpha(b) ......................... =', e12.4/
     9 5x,'alpha(c) ......................... =', e12.4/
     $ 5x,'g(ab) ............................ =', e12.4/,
     $ 5x,'g(bc) ............................ =', e12.4/,
     $ 5x,'g(ca) ............................ =', e12.4/,
     $ 5x,'material axes option ............. =', e12.4)
  420 format(
     $ 5x,'sc, shear strength ............... =', e12.4/,
     $ 5x,'xt, longitudinal tensile strength  =', e12.4/,
     $ 5x,'yt, transverse tensile strength .. =', e12.4/,
     $ 5x,'yc, transverse compressive strength=', e12.4/,
     $ 5x,'nonlinear shear stress prameter .. =', e12.4//,
     $ 5x,' **********************************************'/,
     $/2x,' the following input pertains to solid elements  '/,
     $ 5x,'sn, normal tensile strength ...... =', e12.4/,
     $ 5x,'syz, transverse shear strength ... =', e12.4/,
     $ 5x,'szx, transverse shear strength ... =', e12.4/,
     $ 5x,'kf, bulk modulus of failed material=', e12.4/,
     $ 5x,' **********************************************'/)
  430 format(
     & 5x,'number of points in curves ....... =', e12.4/,
     & 5x,'load curve # for radius vs. time . =', e12.4/,
     & 5x,'load curve # defining profile .... =', e12.4/,
     & 5x,'x-coordinate of spot center ...... =', e12.4/,
     & 5x,'y-coordinate of spot center ...... =', e12.4/,
     & 5x,'z-coordinate of spot center ...... =', e12.4/,
     & 5x,'material axes option ............. =', e12.4)
  440 format(
     1 5x,'node1 (option = 0.0) ............. =', e12.4/,
     2 5x,'node2 (option = 0.0) ............. =', e12.4/,
     3 5x,'node3 (option = 0.0) ............. =', e12.4/,
     4 5x,'xc    (option = 1.0) ............. =', e12.4/,
     5 5x,'yc    (option = 1.0) ............. =', e12.4/,
     6 5x,'zc    (option = 1.0) ............. =', e12.4/,
     7 5x,'l1    (option = 2.0 & 3.0) ....... =', e12.4/,
     8 5x,'l2    (option = 2.0 & 3.0) ....... =', e12.4/,
     9 5x,'l3    (option = 2.0 & 3.0) ....... =', e12.4/,
     $ 5x,'angle (option = 3.0) ............. =', e13.4/,
     $ 5x,'m1    (option = 2.0) ............. =', e12.4/,
     $ 5x,'m2    (option = 2.0) ............. =', e12.4/,
     $ 5x,'m3    (option = 2.0) ............. =', e12.4///)
  450 format(
     $ 5x,'xp,a1,v1 (option = 1,2,3) ........ =', e12.4/,
     $ 5x,'yp,a2,v2 (option = 1,2,3) ........ =', e12.4/,
     $ 5x,'zp,a3,v3 (option = 1,2,3) ........ =', e12.4/,
     $ 5x,'d1       (option =   2  ) ........ =', e12.4/,
     $ 5x,'d2       (option =   2  ) ........ =', e12.4/,
     $ 5x,'d3       (option =   2  ) ........ =', e12.4)
  460 format(
     1(5x,'material angles .................. =',3e11.2))
  470 format(///)
  480 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4/,
     3 5x,'yield ............................ =', e12.4/,
     4 5x,'e (harden) ....................... =', e12.4/,
     5 5x,'hardening parmeter ............... =', e12.4///)
  490 format(
     1 5x,'temp ............................. =',8(e9.2,1x),/,
     2 5x,'e ................................ =',8(e9.2,1x),/,
     3 5x,'vnu .............................. =',8(e9.2,1x),/,
     4 5x,'alpha ............................ =',8(e9.2,1x),/,
     5 5x,'yield ............................ =',8(e9.2,1x),/,
     6 5x,'e (harden) ....................... =',8(e9.2,1x),///)
  500 format(
     1 5x,'shear ............................ =', e12.4/,
     2 5x,'bulk ............................. =', e12.4/,
     3 5x,'a0 ............................... =', e12.4/,
     4 5x,'a1 ............................... =', e12.4/,
     5 5x,'a2 ............................... =', e12.4/,
     6 5x,'pressure cutoff .................. =', e12.4/,
     9 5x,'ln(v/v0) ......................... =',/9(e8.1,1x),/,
     a 5x,'pressure ......................... =',/9(e8.1,1x),///)
  505 format(
     1 5x,'shear ............................ =', e12.4/,
     2 5x,'bulk ............................. =', e12.4/,
     3 5x,'a0 ............................... =', e12.4/,
     4 5x,'a1 ............................... =', e12.4/,
     5 5x,'a2 ............................... =', e12.4/,
     7 5x,'failure flag ..................... =', e12.4/,
     7 5x,'    =0 pressure failure criterion'/,
     7 5x,'    =1 max prin stress failure criterion'/,
     6 5x,'pressure cutoff .................. =', e12.4/,
     6 5x,'max principal stress at failure .. =', e12.4/,
     9 5x,'ln(v/v0) ......................... =',/9(e8.1,1x),/,
     a 5x,'pressure ......................... =',/9(e8.1,1x),///)
  510 format(' standard dyna3d viscoelastic model',/
     1 5x,'bulk ............................. =', e12.4/,
     2 5x,'short time shear modulus ......... =', e12.4/,
     3 5x,'long time shear modulus .......... =', e12.4/,
     4 5x,'decay constant ................... =', e12.4///)
  512 format(' kelvin-voigt viscoelastic model ',/
     1 5x,'                                    ',/
     1 5x,'bulk ............................. =', e12.4/,
     2 5x,'short time shear modulus ......... =', e12.4/,
     3 5x,'long time shear modulus .......... =', e12.4/,
     4 5x,'decay constant ................... =', e12.4///)
  520 format(
     1 5x,'g ................................ =', e12.4/,
     2 5x,'pr ............................... =', e12.4///)
  530 format(
     1 5x,'detonation velocity .............. =', e12.4/,
     2 5x,'chapman - jouget pressure ........ =', e12.4/,
     3 5x,'beta burn flag ................... =', e12.4/,
     4 10x,'eq.0.0: programmed burn ',/
     5 10x,'eq.1.0: beta burn  ',///)
  540 format(
     1 5x,'pressure cutoff .................. =', e12.4/,
     2 5x,'viscosity coefficient ............ =', e12.4 )
  550 format(
     1 5x,'g ................................ =', e12.4/,
     2 5x,'yield ............................ =', e12.4/,
     3 5x,'plastic hardening modulus ........ =', e12.4/,
     4 5x,'pressure cutoff .................. =', e12.4/,
     5 5x,'pressure hardening coefficient a1  =', e12.4/,
     6 5x,'pressure hardening coefficient a2  =', e12.4/,
     7 5x,'spall type ....................... =', e12.4/,
     8 5x,'effective plastic strain ......... =',8(e8.1,1x),/,
     9 5x,'effective plastic strain ......... =',8(e8.1,1x),/,
     $ 5x,'effective stress ................. =',8(e8.1,1x),/,
     $ 5x,'effective stress ................. =',8(e8.1,1x),///)
  560 format(
     1 5x,'g ................................ =', e12.4/,
     2 5x,'y0 ............................... =', e12.4/,
     3 5x,'beta ............................. =', e12.4/,
     4 5x,'n ................................ =', e12.4/,
     5 5x,'gammai ........................... =', e12.4/,
     6 5x,'ywm .............................. =', e12.4/,
     7 5x,'b ................................ =', e12.4/,
     8 5x,'bp ............................... =', e12.4/,
     9 5x,'h ................................ =', e12.4/,
     $ 5x,'f ................................ =', e12.4)
  570 format(
     1 5x,'a ................................ =', e12.4/,
     2 5x,'tm0 .............................. =', e12.4/,
     3 5x,'gamma0 ........................... =', e12.4/,
     4 5x,'sa  .............................. =', e12.4/,
     5 5x,'pcoff ............................ =', e12.4/,
     6 5x,'spall type ....................... =', e12.4/,
     7 5x,'rp ............................... =', e12.4 )
  580 format(
     1 5x,'ec0 .............................. =', e12.4/,
     2 5x,'ec1 .............................. =', e12.4/,
     3 5x,'ec2 .............................. =', e12.4/,
     4 5x,'ec3 .............................. =', e12.4/,
     5 5x,'ec4 .............................. =', e12.4/,
     6 5x,'ec5 .............................. =', e12.4/,
     7 5x,'ec6 .............................. =', e12.4/,
     8 5x,'ec7 .............................. =', e12.4/,
     9 5x,'ec8 .............................. =', e12.4/,
     $ 5x,'ec9 .............................. =', e12.4//)
  590 format(
     1 5x,'a ................................ =', e12.4/,
     2 5x,'b ................................ =', e12.4/,
     3 5x,'xp1 .............................. =', e12.4/,
     4 5x,'xp2 .............................. =', e12.4/,
     5 5x,'frer ............................. =', e12.4/,
     6 5x,'g ................................ =', e12.4/,
     7 5x,'r1 ............................... =', e12.4/,
     8 5x,'r2 ............................... =', e12.4/,
     9 5x,'r3 ............................... =', e12.4/,
     $ 5x,'r5 ............................... =', e12.4/,
     $ 5x,'r6 ............................... =', e12.4/,
     $ 5x,'fcrit ............................ =', e12.4 )
  600 format(
     1 5x,'freq ............................. =', e12.4/,
     2 5x,'grow ............................. =', e12.4/,
     3 5x,'em ............................... =', e12.4/,
     4 5x,'ar ............................... =', e12.4/,
     5 5x,'es ............................... =', e12.4/,
     6 5x,'cpv .............................. =', e12.4/,
     7 5x,'cvr .............................. =', e12.4/,
     8 5x,'eeta2 ............................ =', e12.4/,
     9 5x,'qcrit ............................ =', e12.4/,
     $ 5x,'enq .............................. =', e12.4/,
     $ 5x,'temp0 ............................ =', e12.4/,
     $ 5x,'shear ............................ =', e12.4/,
     $ 5x,'yield ............................ =', e12.4////)
  610 format(
     1 5x,'g ................................ =', e12.4/
     2 5x,'a ................................ =', e12.4/
     3 5x,'b ................................ =', e12.4/
     4 5x,'n ................................ =', e12.4/
     5 5x,'c ................................ =', e12.4/
     6 5x,'m ................................ =', e12.4/
     7 5x,'melt temperature ................. =', e12.4/
     8 5x,'room temperature ................. =', e12.4/
     9 5x,'eps0 ............................. =', e12.4/
     $ 5x,'specific heat .................... =', e12.4/
     $ 5x,'pressure cutoff .................. =', e12.4/
     $ 5x,'spall type ....................... =', e12.4/
     $ 5x,'iteration option ................. =', e12.4)
  620 format(
     1 5x,'d1 ............................... =', e12.4/,
     2 5x,'d2 ............................... =', e12.4/,
     3 5x,'d3 ............................... =', e12.4/,
     4 5x,'d4 ............................... =', e12.4/,
     5 5x,'d5 ............................... =', e12.4/,
     6 5x,'e (shells only)................... =', e12.4/,
     8 5x,'vnu (shells only)................. =', e12.4/,
     9 5x,'time step size for element deletion=',e12.4///)
  630 format(
     1 5x,'bulk ............................. =', e12.4/,
     2 5x,'shear ............................ =', e12.4/,
     3 5x,'alpha ............................ =', e12.4/,
     4 5x,'theta ............................ =', e12.4/,
     5 5x,'gama ............................. =', e12.4/,
     6 5x,'beta ............................. =', e12.4/,
     7 5x,'r ................................ =', e12.4/,
     8 5x,'d ................................ =', e12.4/,
     9 5x,'w ................................ =', e12.4/,
     $ 5x,'xint ............................. =', e12.4/,
     $ 5x,'nplot ............................ =', e12.4/,
     $ 5x,'ltype ............................ =', e12.4/,
     $ 5x,'tcut ............................. =', e12.4/,
     $ 5x,'fcut ............................. =', e12.4/,
     $ 5x,'elint ............................ =', e12.4/,
     $ 5x,'ivec (=0 vectorized) ............. =', e12.4/,
     $ 5x,'cbar ............................. =', e12.4/,
     $ 5x,'N (failfac) ...................... =', e12.4///)
  640 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4/,
     3 5x,'yield ............................ =', e12.4/,
     4 5x,'curve # for sigx versus relvol ... =', e12.4/,
     5 5x,'curve # for sigy versus relvol ... =', e12.4/,
     6 5x,'curve # for sigz versus relvol ... =', e12.4/,
     7 5x,'curve # for tau  versus relvol ... =', e12.4/,
     8 5x,'fully compacted relative volume .. =', e12.4/,
     9 5x,'ex0 .............................. =', e12.4/,
     $ 5x,'ey0 .............................. =', e12.4/,
     $ 5x,'ez0 .............................. =', e12.4/,
     $ 5x,'gxy0 ............................. =', e12.4/,
     $ 5x,'gyz0 ............................. =', e12.4/,
     $ 5x,'gzx0 ............................. =', e12.4///)
  644 format(
     1 5x,'a ................................ =', e12.4/,
     2 5x,'b ................................ =', e12.4/,
     3 5x,'pr ............................... =', e12.4///)
  645 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4/,
     3 5x,'moment values for force vs. u .... =',8(e9.2,1x),/,
     4 5x,'force vs. moment curve numbers ... =',8(e9.2,1x),///)
  650 format(///,1x,12a6,//)
  660 format(
     1 5x,'c0 ............................... =', e12.4/,
     2 5x,'c1 ............................... =', e12.4/,
     3 5x,'c2 ............................... =', e12.4/,
     4 5x,'c3 ............................... =', e12.4/,
     5 5x,'c4 ............................... =', e12.4/,
     6 5x,'c5 ............................... =', e12.4/,
     7 5x,'c6 ............................... =', e12.4/,
     8 5x,'e0 ............................... =', e12.4/,
     9 5x,'initial relative volume .......... =', e12.4///)
  670 format(
     1 5x,'a ................................ =', e12.4/,
     2 5x,'b ................................ =', e12.4/,
     3 5x,'r1 ............................... =', e12.4/,
     4 5x,'r2 ............................... =', e12.4/,
     5 5x,'omega ............................ =', e12.4/,
     6 5x,'initial energy/volume ............ =', e12.4/,
     7 5x,'initial relative volume .......... =', e12.4///)
  680 format(
     1 5x,'a1 ............................... =', e12.4/,
     2 5x,'a2 ............................... =', e12.4/,
     3 5x,'a3 ............................... =', e12.4/,
     4 5x,'b1 ............................... =', e12.4/,
     5 5x,'b2 ............................... =', e12.4/,
     6 5x,'initial energy/volume ............ =', e12.4/,
     7 5x,'initial relative volume .......... =', e12.4///)
  690 format(
     1 5x,'c  ............................... =', e12.4/,
     2 5x,'s1 ............................... =', e12.4/,
     3 5x,'s2 ............................... =', e12.4/,
     4 5x,'s3 ............................... =', e12.4/,
     5 5x,'g0 ............................... =', e12.4/,
     6 5x,'a  ............................... =', e12.4/,
     7 5x,'e0 ............................... =', e12.4/,
     8 5x,'initial relative volume .......... =', e12.4///)
  700 format(
     1 5x,'a10 .............................. =', e12.4/,
     2 5x,'a11 .............................. =', e12.4/,
     3 5x,'a12 .............................. =', e12.4/,
     4 5x,'a13 .............................. =', e12.4/,
     5 5x,'a20 .............................. =', e12.4/,
     6 5x,'a21 .............................. =', e12.4/,
     7 5x,'a22 .............................. =', e12.4/,
     8 5x,'a23 .............................. =', e12.4/,
     9 5x,'a30 .............................. =', e12.4/,
     $ 5x,'a31 .............................. =', e12.4/,
     $ 5x,'a32 .............................. =', e12.4/,
     $ 5x,'a33 .............................. =', e12.4 )
  710 format(
     1 5x,'a40 .............................. =', e12.4/,
     2 5x,'a41 .............................. =', e12.4/,
     3 5x,'a42 .............................. =', e12.4/,
     4 5x,'a43 .............................. =', e12.4/,
     5 5x,'a50 .............................. =', e12.4/,
     6 5x,'a51 .............................. =', e12.4/,
     7 5x,'a52 .............................. =', e12.4/,
     8 5x,'a53 .............................. =', e12.4 )
  720 format(
     1 5x,'a60 .............................. =', e12.4/,
     2 5x,'a61 .............................. =', e12.4/,
     3 5x,'a62 .............................. =', e12.4/,
     4 5x,'a63 .............................. =', e12.4/,
     5 5x,'a70 .............................. =', e12.4/,
     6 5x,'a71 .............................. =', e12.4/,
     7 5x,'a72 .............................. =', e12.4/,
     8 5x,'a73 .............................. =', e12.4/,
     9 5x,'alfa ............................. =', e12.4/,
     $ 5x,'a12beta .......................... =', e12.4/,
     $ 5x,'a14 .............................. =', e12.4/,
     $ 5x,'a24 .............................. =', e12.4/,
     $ 5x,'e0 ............................... =', e12.4/,
     $ 5x,'initial relative volume .......... =', e12.4///)
  730 format(
     1 5x,'c0 ............................... =', e12.4/,
     2 5x,'c1 ............................... =', e12.4/,
     3 5x,'c2 ............................... =', e12.4/,
     4 5x,'c3 ............................... =', e12.4/,
     5 5x,'c4 ............................... =', e12.4/,
     6 5x,'c5 ............................... =', e12.4/,
     7 5x,'c6 ............................... =', e12.4/,
     8 5x,'e0 ............................... =', e12.4/,
     9 5x,'initial relative volume .......... =', e12.4/,
     $ 5x,'load curve for energy generation . =', e12.4///)
  740 format(
     1 5x,'volumetric strain ................ =',5(e13.4,1x),/,
     2 5x,'volumetric strain ................ =',5(e13.4,1x),/,
     3 5x,'pressure with e=0 ................ =',5(e13.4,1x),/,
     4 5x,'pressure with e=0 ................ =',5(e13.4,1x),/,
     5 5x,'multiplier of gamma*e ............ =',5(e13.4,1x),/,
     6 5x,'multiplier of gamma*e ............ =',5(e13.4,1x),/,
     7 5x,'unloading bulk modulus ........... =',5(e13.4,1x),/,
     8 5x,'unloading bulk modulus ........... =',5(e13.4,1x),/,
     9 5x,'gamma ............................ =', e12.4/,
     $ 5x,'e0 ............................... =', e12.4/,
     $ 5x,'initial relative volume .......... =', e12.4///)
  750 format(
     1 5x,'volumetric strain ................ =',5(e13.4,1x),/,
     2 5x,'volumetric strain ................ =',5(e13.4,1x),/,
     3 5x,'pressure with e=0 ................ =',5(e13.4,1x),/,
     4 5x,'pressure with e=0 ................ =',5(e13.4,1x),/,
     5 5x,'multiplier of gamma*e ............ =',5(e13.4,1x),/,
     6 5x,'multiplier of gamma*e ............ =',5(e13.4,1x),/,
     7 5x,'gamma ............................ =', e12.4/,
     8 5x,'e0 ............................... =', e12.4/,
     9 5x,'initial relative volume .......... =', e12.4///)
  760 format(
     1 5x,'g ................................ =', e12.4/,
     2 5x,'yield ............................ =', e12.4/,
     3 5x,'e (harden) ....................... =', e12.4/,
     4 5x,'k  ............................... =', e12.4///)
  770 format(
     1 5x,'g ................................ =', e12.4/,
     2 5x,'yield ............................ =', e12.4/,
     3 5x,'e (harden) ....................... =', e12.4/,
     4 5x,'failure strain ................... =', e12.4/,
     5 5x,'failure pressure ................. =', e12.4/,
     6 5x,'k  ............................... =', e12.4///)
  780 format(
     1 5x,'shear area factor ................ =', e12.4/
     2 5x,'cross-section integration rule ... =', e12.4/
     3 5x,'     lt.0.0:absolute value is specified rule #  '/
     4 5x,'     eq.1.0:1 x 1 gauss (truss)'/
     5 5x,'     eq.2.0:2 x 2 gauss   (4  point circle)'/
     6 5x,'     eq.3.0:3 x 3 gauss   (9  point circle)'/
     7 5x,'     eq.4.0:3 x 3 lobatto (9  point circle)'/
     8 5x,'     eq.5.0:4 x 4 gauss   (16 point circle)'//
     9 5x,'cross section type ............... =', e12.4/
     $ 5x,'     eq.0.0: rectangular'/5x,'     eq.1.0: (tubular)'/
     $ 5x,'     eq.2.0: arbitrary',//)
  790 format(
     $ 5x,'beam  formulation ..................... =',i5/
     $ 5x,'     eq. 1: hughes-liu                       '/
     $ 5x,'     eq. 2: belytschko-schwer                '/
     $ 5x,'     eq. 3: simple truss                     '//
     1 5x,'y-fiber lengths or area: node 1 ....... =', e12.4/
     2 5x,'(outer diameter) or iyy: node 2 ....... =', e12.4//
     3 5x,'z-fiber lengths  or izz: node 1 ....... =', e12.4/
     4 5x,'(inner diameter) or ixx: node 2 ....... =', e12.4//
     5 5x,'y-ref. surface or sarea: node 1 ....... =', e12.4/
     6 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
     7 5x,'     eq. 0.0:middle    '/5x,'     eq.-1.0:bottom'//
     8 5x,'z-ref. surface:          node 1 ....... =', e12.4/
     9 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
     $ 5x,'     eq. 0.0:middle    '/5x,'     eq.-1.0:bottom'//)
  800 format(
     1 5x,'shear area factor ................ =', e12.4/
     2 5x,'thickness integration rule ....... =', e12.4/
     3 5x,'     eq.1.0:1 point  (membrane)    '/
     4 5x,'     eq.2.0:2 points               '/
     5 5x,'     eq.3.0:3 points               '/
     6 5x,'     eq.4.0:4 points               '/
     7 5x,'     eq.5.0:5 points               '/
     8 5x,'     gt.5.0:# pts. in trapezoidal rule  '//
     9 5x,'print out option ................. =', e12.4/
     $ 5x,'     eq.1.0:average resultants & fiber lengths       '/
     $ 5x,'     eq.2.0:resultants at plan points & fiber lengths'/
     $ 5x,'     eq.3.0:resultants,stresses all points & fiber lengths'//
     $ 5x,'integration rule ................. =', e12.4/
     $ 5x,'     lt.0.0:absolute value is specified rule #  '/
     $ 5x,'     eq.0.0:gauss                          '/
     $ 5x,'     eq.1.0:trapezoidal rule          '///)
  810 format(
     $ 5x,'shell formulation ..................... =',i5/
     $ 5x,'     eq. 1: hughes-liu                       '/
     $ 5x,'     eq. 2: belytschko-tsay                  '/
     $ 5x,'     eq. 3: bciz                             '/
     $ 5x,'     eq. 4: c0-triangular element            '/
     $ 5x,'     eq. 5: membrane element                 '/
     $ 5x,'     eq. 6: yase                            '//
     1 5x,'fiber lengths:           node 1 ....... =', e12.4/
     2 5x,'                         node 2 ....... =', e12.4/
     3 5x,'                         node 3 ....... =', e12.4/
     4 5x,'                         node 4 ....... =', e12.4//
     5 5x,'reference surface:       node 1 ....... =', e12.4/
     6 5x,'     eq. 1.0:top         node 2 ....... =', e12.4/
     7 5x,'     eq. 0.0:middle      node 3 ....... =', e12.4/
     8 5x,'     eq.-1.0:bottom      node 4 ....... =', e12.4//)
  820 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4/,
     3 5x,'yield ............................ =', e12.4/,
     4 5x,'e (harden) ....................... =', e12.4/,
     5 5x,'fracture strength ................ =', e12.4///)
  830 format(
     1 5x,'shear area factor ................ =', e12.4/
     2 5x,'thickness integration rule ....... =', e12.4/
     3 5x,'     eq.1.0:1 pt. gauss (membrane)  '/
     4 5x,'     eq.2.0:2 pt. gauss        '/
     5 5x,'     eq.3.0:3 pt. gauss        '/
     6 5x,'     eq.4.0:4 pt. gauss        '/
     7 5x,'     eq.5.0:5 pt. gauss        '//)
  840 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'k ................................ =', e12.4/
     4 5x,'m ................................ =', e12.4///)
  850 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'initial yield by load curve ...... =', e12.4/
     4 5x,'tangent modulus .................. =', e12.4/
     5 5x,'e by optional load curve ......... =', e12.4/
     6 5x,'tangent modulus by optional curve  =', e12.4/
     7 5x,'effective stress at failure curve  =', e12.4/
     8 5x,'time step size for element deletion=',e12.4///)
  860 format(
     1(5x,'e(a) ............................. =', 3e11.2))
  870 format(
     1(5x,'e(b) ............................. =', 3e11.2))
  880 format(
     1(5x,'e(c) ............................. =', 3e11.2))
  890 format(
     1(5x,'vnu(ba) .......................... =', 3e11.2))
  900 format(
     1(5x,'vnu(ca) .......................... =', 3e11.2))
  910 format(
     1(5x,'vnu(cb) .......................... =', 3e11.2))
  920 format(
     1(5x,'alpha(a) ......................... =', 3e11.2))
  930 format(
     1(5x,'alpha(b) ......................... =', 3e11.2))
  940 format(
     1(5x,'alpha(c) ......................... =', 3e11.2))
  950 format(
     1(5x,'g(ab) ............................ =', 3e11.2))
  960 format(
     1(5x,'g(bc) ............................ =', 3e11.2))
  970 format(
     1(5x,'g(ca) ............................ =', 3e11.2))
  980 format(
     1(5x,'temps ............................ =', 3e11.2))
  990 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'yield ............................ =', e12.4/
     4 5x,'e (harden) ....................... =', e12.4/
     5 5x,'plastic strain at failure ........ =', e12.4/
     6 5x,'effective plastic strain ......... =',8(e9.2,1x)/
     7 5x,'effective stress ................. =',8(e9.2,1x)/
     8 5x,'curve # for strainrate effects ... =', e12.4/
     9 5x,'time step size for element deletion=',e12.4)
  995 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'g ................................ =', e12.4/
     4 5x,'r ................................ =', e12.4/
     4 5x,'p ................................ =', e12.4/
     4 5x,'qxy .............................. =', e12.4/
     4 5x,'qyz .............................. =', e12.4/
     4 5x,'qzx .............................. =', e12.4/
     5 5x,'yield stress ..................... =', e12.4/
     4 5x,'beta ............................. =', e12.4/
     5 5x,'hardening modulus ................ =', e12.4)
  996 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'yield stress ..................... =', e12.4/
     4 5x,'hardening modulus ................ =', e12.4/
     5 5x,'anisotropy parameter (r) ......... =', e12.4)
  997 format(
     1 5x,'e ................................ =', e12.4/,
     2 5x,'vnu .............................. =', e12.4/,
     3 5x,'yield ............................ =', e12.4/,
     4 5x,'e (harden) ....................... =', e12.4/,
     5 5x,'hardening parmeter ............... =', e12.4/,
     1 5x,'efvt ............................. =', e12.4/,
     2 5x,'efpt ............................. =', e12.4/,
     3 5x,'efbt ............................. =', e12.4/,
     4 5x,'efvc ............................. =', e12.4/,
     5 5x,'scldev ........................... =', e12.4///)
 1000 format(
     1 5x,'e ................................ =', e12.4/
     2 5x,'vnu .............................. =', e12.4/
     3 5x,'rigid body constraint option ..... =', e12.4/
     4 5x,'translational constraint ......... =', e12.4/
     5 5x,'rotational constraint ............ =', e12.4///)
 1010 format(
     1 5x,'coeff c001 ....................... =', e12.4/,
     2 5x,'coeff c010 ....................... =', e12.4/,
     3 5x,'coeff c020 ....................... =', e12.4/,
     4 5x,'coeff c100 ....................... =', e12.4/,
     5 5x,'coeff c101 ....................... =', e12.4/,
     6 5x,'coeff c110 ....................... =', e12.4/,
     7 5x,'coeff c200 ....................... =', e12.4/,
     8 5x,'coeff c210 ....................... =', e12.4/,
     9 5x,'coeff c300 ....................... =', e12.4/,
     $ 5x,'coeff c400   ....................... =', e12.4///)
 1020 format(
     1 5x,'exit or continue option ..........  ', e12.4/,
     2 5x,'maximum strain limit ............. =', e12.4/,
     3 5x,'minimum strain limit ............. =', e12.4///)
 1022 format(
     1 5x,'reference shear strain (gamma-y) . =', e12.4/,
     2 5x,'reference shear stress (tau-y) ... =', e12.4/,
     3 5x,'stress coefficient (alpha) ....... =', e12.4/,
     4 5x,'stress exponent (r) .............. =', e12.4/,
     5 5x,'elastic bulk modulus (k) ......... =', e14.4///)
      end
      subroutine sets1(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(5,*)
c
      prop(1)=cm(1,1)
      prop(2)=cm(1,2)
      prop(3)=cm(1,3)
      prop(4)=cm(1,4)
      prop(5)=cm(1,5)
      prop(6)=cm(2,1)
      prop(7)=cm(3,1)
      do 10 i=2,5
      do 10 j=1,5
   10 cm(i,j)=0.0
      q1=cm(1,1)*cm(1,2)/((1.0+cm(1,2))*(1.0-2.0*cm(1,2)))
      q2=cm(1,1)*0.5/(1.0+cm(1,2))
      q3=q1+2.0*q2
      cm(2,1)=q3
      cm(3,2)=q3
      cm(4,3)=q3
      cm(5,4)=q2
      cm(2,2)=q1
      cm(2,3)=q1
      cm(3,1)=q1
      cm(3,3)=q1
      cm(4,1)=q1
      cm(4,2)=q1
      cm(2,6)=cm(1,6)
      cm(1,6)=cm(1,1)*cm(1,4)/(cm(1,1)-cm(1,4))
      cm(1,7)=prop(6)
      cm(2,7)=prop(7)
c
      return
c
      end
      subroutine sets2(cm,c)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(*),c(6,6)
      do 10 i=1,25
   10 prop(i)=cm(i)
      do 20 i=1,6
      do 20 j=1,6
   20 c(j,i)=0.0
      s11=1./prop(1)
      s22=1./prop(2)
      s33=1./prop(3)
      s44=1./prop(7)
      s55=1./prop(8)
      s66=1./prop(9)
      s12=-prop(4)*s22
      s13=-prop(5)*s33
      s23=-prop(6)*s33
      s1122=s11*s22
      s1123=s11*s23
      s2213=s22*s13
      s1212=s12*s12
      s1223=s12*s23
      si=1./(s1122*s33-s1123*s23-s2213*s13-s33*s1212+2.*s1223*s13)
      c(1,1)=si*(s22*s33-s23*s23)
      c(1,2)=si*(s13*s23-s33*s12)
      c(2,2)=si*(s33*s11-s13*s13)
      c(2,3)=si*(s12*s13-s1123)
      c(3,3)=si*(s1122-s1212)
      c(1,3)=si*(s1223-s2213)
      c(2,1)=c(1,2)
      c(3,1)=c(1,3)
      c(3,2)=c(2,3)
      c(4,4)=1./s44
      c(5,5)=1./s55
      c(6,6)=1./s66
      do 90 i=1,9
   90 cm(i)=prop(9+i)
      prop(10)=cm(1)
      cm(10)=  max(c(1,1),c(2,2),c(3,3))
      cm(11)=  max(prop(1),prop(2),prop(3))
      cm(12)=  max(prop(4),prop(5),prop(6))
      if (prop(10).ne.0.) go to 110
      do 100 i=11,19
  100 prop(i)=0.
      prop(20)=cm(4)
      prop(21)=cm(5)
      prop(22)=cm(6)
      if (prop(20).eq.0.) prop(20)=1.
      if (prop(21).eq.0.) prop(21)=2.
      if (prop(22).eq.0.) prop(22)=4.
      return
  110 if (prop(10).ne.1.0) go to 130
      prop(11)=cm(4)
      prop(12)=cm(5)
      prop(13)=cm(6)
      do 120 i=14,19
  120 prop(i)=0.
      return
  130 do 140 i=11,13
  140 prop(i)=0.
      denm=sqrt(cm(4)**2+cm(5)**2+cm(6)**2)
      denn=sqrt(cm(7)**2+cm(8)**2+cm(9)**2)
      do 150 i=4,6
      cm(i)=cm(i)/denm
      if (prop(10).eq.3.0) go to 150
      cm(i+3)=cm(i+3)/denn
  150 continue
      do 160 i=14,16
  160 prop(i)=cm(i-10)
      prop(17)=0.0
      if (prop(10).eq.3.0) then
      prop(17)=cm(7)
      return
      endif
      do 170 i=17,19
  170 prop(i+6)=cm(i-10)
      return
      end
      subroutine sets21(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(*),c(6,6)
c
      do 10 i=1,48
   10 prop(i)=cm(i)
      do 20 i=1,6
      do 20 j=1,6
   20 c(j,i)=0.0
      s11=1./prop(1)
      s22=1./prop(2)
      s33=1./prop(3)
      s44=1./prop(7)
      s55=1./prop(8)
      s66=1./prop(9)
      s12=-prop(4)*s22
      s13=-prop(5)*s33
      s23=-prop(6)*s33
      s1122=s11*s22
      s1123=s11*s23
      s2213=s22*s13
      s1212=s12*s12
      s1223=s12*s23
      si=1./(s1122*s33-s1123*s23-s2213*s13-s33*s1212+2.*s1223*s13)
      c(1,1)=si*(s22*s33-s23*s23)
      c(1,2)=si*(s13*s23-s33*s12)
      c(2,2)=si*(s33*s11-s13*s13)
      c(2,3)=si*(s12*s13-s1123)
      c(3,3)=si*(s1122-s1212)
      c(1,3)=si*(s1223-s2213)
      c(2,1)=c(1,2)
      c(3,1)=c(1,3)
      c(3,2)=c(2,3)
      c(4,4)=1./s44
      c(5,5)=1./s55
      c(6,6)=1./s66
      do 90 i=1,9
   90 cm(i)=prop(9+i)
      prop(10)=cm(1)
      cm(10)=  max(c(1,1),c(2,2),c(3,3))
      cm(11)=  max(prop(1),prop(2),prop(3))
      cm(12)=  max(prop(4),prop(5),prop(6))
      if (prop(10).ne.0.) go to 110
      do 100 i=11,19
  100 prop(i)=0.
      prop(20)=cm(4)
      prop(21)=cm(5)
      prop(22)=cm(6)
      if (prop(20).eq.0.) prop(20)=1.
      if (prop(21).eq.0.) prop(21)=2.
      if (prop(22).eq.0.) prop(22)=4.
      go to 170
  110 if (prop(10).ne.1.0) go to 130
      prop(11)=cm(4)
      prop(12)=cm(5)
      prop(13)=cm(6)
      do 120 i=14,19
  120 prop(i)=0.
      go to 170
  130 do 140 i=11,13
  140 prop(i)=0.
      denm=sqrt(cm(4)**2+cm(5)**2+cm(6)**2)
      denn=sqrt(cm(7)**2+cm(8)**2+cm(9)**2)
      do 150 i=4,6
      cm(i)=cm(i)/denm
      if (prop(10).eq.3.0) go to 150
      cm(i+3)=cm(i+3)/denn
  150 continue
      do 160 i=14,19
  160 prop(i)=cm(i-10)
c
  170 k=13
      do 180 i=1,6
      do 180 j=i,6
      cm(k)=c(i,j)
  180 k=k+1
      cm(46)=prop(23)
      cm(47)=prop(24)
      cm(48)=prop(25)
      return
c
      end
      subroutine sets22(cm,angles,nip)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk22/prop(48)
      dimension cm(*),angles(*)
      character*80 txts,mssg
      call blkcpy (cm,prop,48)
      if (nip.gt.0) then
      nipl=(nip-1)/8+1
      lfst=1
      llst=8
      mssg =' error reading material angles for material type 22'
      do 90 i=1,nipl
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=180,err=170) (angles(j),j=lfst,llst)
      lfst=lfst+8
      llst=llst+8
   90 continue
      endif
      s11=1./prop(1)
      s22=1./prop(2)
      s33=1./prop(3)
      s12=-prop(4)*s22
      s13=-prop(5)*s33
      s23=-prop(6)*s33
      s1122=s11*s22
      s1123=s11*s23
      s2213=s22*s13
      s1212=s12*s12
      s1223=s12*s23
      si=1./(s1122*s33-s1123*s23-s2213*s13-s33*s1212+2.*s1223*s13)
      cm1=si*(s22*s33-s23*s23)
      cm3=si*(s1223-s2213)
      cm4=si*(s33*s11-s13*s13)
      cm5=si*(s12*s13-s1123)
      cm6=si*(s1122-s1212)
      cm(21)=  max(cm1,cm4,cm6)
      cm(22)=cm3/cm6
      cm(23)=cm5/cm6
      if (prop(10).le.1.0) return
      denm=sqrt(cm(13)**2+cm(14)**2+cm(15)**2)
      denn=sqrt(cm(16)**2+cm(17)**2+cm(18)**2)
      do 150 i=13,15
      cm(i)=cm(i)/denm
      if (cm(10).eq.3.0) go to 150
      cm(i+3)=cm(i+3)/denn
  150 continue
      if (cm(45).gt.0.0) then
      cm(45)=1./cm(45)**2
      else
      cm(45)=0.0
      endif
      if (cm(46).gt.0.0) then
      cm(46)=1./cm(46)**2
      else
      cm(46)=0.0
      endif
      if (cm(47).gt.0.0) then
      cm(47)=1./cm(47)**2
      else
      cm(47)=0.0
      endif
      return
c
c     terminate if badly formatted data
c
  170 call termin (txts,mssg,lcount,1)
c
  180 format(8e10.0)
      end
      subroutine sets23(cm,thrmpr,nip)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk22/prop(48)
      character*80 txts,mssg
      dimension cm(*),thrmpr(48,*)
      call blkcpy (cm,prop,48)
      numpts=nint(cm(1))
      if (numpts.gt.1.and.numpts.lt.49) then
      numpt1=(numpts-1)/8+1
      do 80 l=2,14
      i=l
      if (l.eq.14) i=1
      lfst=1
      llst=8
      mssg =' error reading material property data for material type 23'
      do 70 j=1,numpt1
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=180,err=170) (thrmpr(k,i),k=lfst,llst)
      lfst=lfst+8
      llst=llst+8
   70 continue
   80 continue
      if (nip.gt.0) then
      nipl=(nip-1)/8+1
      lfst=1
      llst=8
      mssg =' error reading material angles for material type 23'
      do 90 i=1,nipl
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=180,err=170) (thrmpr(j,14),j=lfst,llst)
      lfst=lfst+8
      llst=llst+8
   90 continue
      endif
      else
      write ( *,190) numpts
      write(13,190) numpts
      call adios (2)
      endif
      s11=1./thrmpr(1,2)
      s22=1./thrmpr(1,3)
      s33=1./thrmpr(1,4)
      s12=-thrmpr(1,5)*s22
      s13=-thrmpr(1,6)*s33
      s23=-thrmpr(1,7)*s33
      s1122=s11*s22
      s1123=s11*s23
      s2213=s22*s13
      s1212=s12*s12
      s1223=s12*s23
      si=1./(s1122*s33-s1123*s23-s2213*s13-s33*s1212+2.*s1223*s13)
      cm1=si*(s22*s33-s23*s23)
      cm3=si*(s1223-s2213)
      cm4=si*(s33*s11-s13*s13)
      cm5=si*(s12*s13-s1123)
      cm6=si*(s1122-s1212)
      cm(21)=  max(cm1,cm4,cm6)
      cm(22)=cm3/cm6
      cm(23)=cm5/cm6
      if (prop(10).le.1.0) return
      denm=sqrt(cm(13)**2+cm(14)**2+cm(15)**2)
      denn=sqrt(cm(16)**2+cm(17)**2+cm(18)**2)
      do 150 i=13,15
      cm(i)=cm(i)/denm
      if (cm(10).eq.3.0) go to 150
      cm(i+3)=cm(i+3)/denn
  150 continue
      return
c
c     terminate if badly formatted data
c
  170 call termin (txts,mssg,lcount,1)
c
  180 format(8e10.0)
  190 format(//' number of points in model 23',i3,' must lie between',/,
     1         ' 2 and 48 inclusive. ***error termination***',///)
      end
      subroutine sets25 (cm)
c     implicit double precision (a-h,o-z)                                    dp
      dimension cm(48)
      alpha = cm(3)
      theta = cm(4)
      gama = cm(5)
      beta = cm(6)
      r = cm(7)
c
      call initel (cm(12),cm(10),alpha,gama,beta,theta,r,capint,fcut)
c
      cm(15) = capint
      cm(16) = fcut
c
      return
      end
      subroutine initel(ltype,xint,alpha,gama,beta,theta,r,capint,fcut)
c     implicit double precision (a-h,o-z)                                    dp
c
c.... initialization of new cap model (newton algorithm)
c
      tol = 1.0e-3
      capint = 0.0
      delcap=1.e10
      delf=1.e10
      iter1=0
      iter2=0
      maxit=200
      fcut=0.0
    2 continue
      fe = alpha - gama*exp(-beta*capint) + theta*capint
      psik = xint - capint - r*fe
      err1=abs(psik)
      err2=abs(delcap)/(abs(capint)+1.e-15)
      if((err1.gt.tol).and.(err2.gt.tol)) then
c     if( abs(psik) .gt. tol) then
      fep = beta*gama*exp(-beta*capint) + theta
      dpsik = -1.0 - r*fep
      delcap= psik/dpsik
      capint = capint - delcap
      if( iter1.gt.maxit ) then
      write(*,100)
      write(13,100)
      call adios(2)
      endif
      iter1=iter1+1
      go to 2
      endif
    3 continue
      fe = alpha - gama*exp(-beta*fcut) + theta*fcut
      err1=abs(fe)
      err2=abs(delf)/(abs(fcut)+1.e-15)
      if( (err1.gt.tol).and.(err2.gt.tol) ) then
c     if( abs(fe) .gt. tol) then
      fep = gama*beta*exp(-beta*fcut) + theta
      delf = fe/fep
      fcut = fcut - delf
      if( iter2.gt.maxit) then
      write(*,110)
      write(13,110)
      call adios(2)
      endif
      iter2=iter2+1
      go to 3
      endif
      return
  100 format(///5x,'Iteration for initial kappa unconverged',
     1         /5x,'Selected value of x0 may be too large',
     2         /5x,'or other parameters inconsistent.',//)
  110 format(///5x,'Iteration for fcut unconverged',
     1         /5x,'failure surface may be too flat',
     2         /5x,'or other parameters inconsistent.',//)
      end
      subroutine sets5(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
c
      dimension  cm(*),strnx(11),py(11)
      data third/.333333333333/
c
      do 10 i=1,5
   10 cm(i)=prop(i)
      g=cm(1)
      bulk=cm(2)
      a0=cm(3)
      a1=cm(4)
      a2=cm(5)
      if (bulk.gt.0.0) go to 30
      write(13,300)
   20 call adios (2)
   30 pfrac=prop(6)
      if (pfrac.ne.0.0) go to 140
      a3=a1*a1-a0*a2*4.0
      if (a1.eq.0.0) go to 40
      if (abs(a3)/(a1*a1).lt.1.e-14) go to 60
   40 if (a3) 50,60,100
   50 write(13,310)
      go to 20
   60 if (a2) 50,70,90
   70 if (a0) 50,80,80
   80 pfrac=-100.*bulk
      go to 140
   90 pfrac=-sqrt(a0/a2)
      go to 140
  100 if (a2) 110,120,130
  110 pfrac=-.5*(a1-sqrt(a3))/a2
      go to 140
  120 pfrac=-a0/a1
      go to 140
  130 pfrac=-.5*(a1-sqrt(a3))/a2
  140 c1=1.0
      if (pfrac) 160,160,150
  150 write(13,320)
      go to 20
  160 if (cm(8)) 180,180,170
  170 c1=-1.0
  180 continue
      k=1
      if (cm(6)) 190,210,190
  190 k=2
      strnx(1)=0.0
      py(1)=0.0
      if (cm(28)) 200,230,200
  200 write(13,330)
      go to 20
  210 if (cm(7)) 220,230,220
  220 write(13,340)
      go to 20
  230 continue
      do 240 i=6,30,5
      strnx(k)=cm(i)*c1
      py(k)=cm(i+1)
      strnx(k+1)=cm(i+2)*c1
      py(k+1)=cm(i+3)
  240 k=k+2
      n=0
      do 250 i=1,10
      cm(i+10)=0.0
      cm(i+20)=0.0
      if (strnx(i).eq.0.0) go to 250
      n=n+1
  250 continue
      if (n.eq.0) go to 260
      n=n+1
      go to 270
  260 strnx(1)=0.0
      py(1)=0.0
      strnx(2)=-third
      py(2)=bulk
      n=2
  270 continue
      gg=g+g
      gg3=gg*third
      gg43=(gg+gg)*third
      cm(6)=pfrac
      cm(7)=n
      cm(8)=gg
      cm(9)=gg3
      cm(10)=gg43
      do 290 i=1,n
      if (strnx(i).le.0.0) go to 280
      write(13,350)
      go to 20
  280 continue
      prop(i+10)=strnx(i)
      prop(i+20)=py(i)
      cm(i+10)=strnx(i)
      cm(i+20)=py(i)
  290 continue
      return
c
  300 format(' fatal error  no bulk modulus supplied')
  310 format(' fatal error  imaginary yield surface try again')
  320 format(' fatal error  tensile fracture in compression')
  330 format(' fatal error  too many non-zero strain-pressure pairs')
  340 format(' fatal error  non zero initial pressure given')
  350 format(' fatal error  all strains do not have the same sign')
      end
      subroutine sets6(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(5,1)
c
      do 10 i=1,5
   10 prop(i)=cm(1,i)
c
      return
c
      end
      subroutine sets8(cm,ro)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize high explosive properties
c
      common/bk22/prop(48)
      dimension cm(*)
c
      do 10 i=1,20
   10 prop(i)=cm(i)
      cm(30)=cm(1)
      cm(1)=ro*cm(1)**2/cm(2)
c
      return
      end
      subroutine sets10(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(*)
c
      call blkcpy (cm,prop,48)
      if (prop(4).eq.0.0) prop(4)=-1.e30
      if (prop(8).eq.0.0) prop(8)=1.000000001
      do 10 i=1,48
   10 cm(i)=prop(i)
      do 30 i=2,16
      if(prop(i+15).lt.prop(i+16)) cm(16)=i
   30 continue
c
      return
      end
      subroutine sets11(cm,ro)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(*)
      character*80 txts,mssg
c
   10 if(prop(16).eq.0.0) prop(16)=2.000000001
      do 20 i=1,48
   20 cm(i)=prop(i)
      if(cm(17).ne.0.0) cm(11)=.00008314*ro/cm(17)
      cm(17)=0.
c
      return
c
   30 format (2e16.0)
   40 mssg =' error reading property data for material type 11'
      call termin (txts,mssg,lcount,1)
      end
      subroutine setes7(eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize variables for reactive flow he
c
      dimension eosp(*)
      eosp(31)=eosp(26)
      eosp(32)=eosp(27)
      eosp(30)=eosp(23)
      eosp(23)=0.0
      if (eosp(24).eq.0.0) eosp(24)=1.0
      if (eosp(25).eq.0.0) eosp(25)=0.001
      if (eosp(26).eq.0.0) eosp(26)=25.1
      if (eosp(27).eq.0.0) eosp(27)=1.e-6
      if (eosp(28).eq.0.0) eosp(28)=1.e10
      if (eosp(29).eq.0.0) eosp(29)=1.0
      eosp(33)=eosp(22)
      eosp(34)=1.0
      return
      end
      subroutine setes8(eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize variables for compaction model
c
      dimension eosp(*)
      anum=0.00001
      do 10 i=2,10
   10 if (eosp(i).ne.0.0) anum=anum+1.0
      eosp(48)=anum
      if (eosp(43).eq.0.0) eosp(43)=1.0
      return
      end
      subroutine setes9(eosp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize variables for compaction model
c
      dimension eosp(*)
      anum=0.00001
      do 10 i=2,10
   10 if (eosp(i).ne.0.0) anum=anum+1.0
      eosp(48)=anum
      if (eosp(33).eq.0.0) eosp(33)=1.0
      return
      end
      subroutine stes11 (  eosp )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     s  e  t  e  o  s  1  1                       *
c *                                                                  *
c ********************************************************************
c *                                                                  *
c *                                                                  *
c *   purpose  seteos11 reads in the tensor pore collapse equation-  *
c *            of-state and sets up necessary variables for use      *
c *            by the routine eqos11.                                *
c *                                                                  *
c *                                                                  *
c *   author   roger a. crawfis                                      *
c *            x3-1320                                               *
c *            asd division                                          *
c *            computation department                                *
c *            lawrence livermore national laboratory                *
c *                                                                  *
c *                                                                  *
c *   date     august 28, 1985                                       *
c *                                                                  *
c *                                                                  *
c ********************************************************************
c
      external  rdes11, cubic, erexit, switch
c
      parameter  ( ncrv = 64,  nlen = 5*ncrv )
      dimension eosp(*)
      common/ /a(1)
c
      lld = int (eosp(48) )
      lcr = lld + nlen
      lin = lcr + nlen
      call rdes11(  nld, a(lld+1), a(lld+ncrv+1), ncr, a(lcr+1),
     1              a(lcr+ncrv+1), eosp(3), eosp(4), eosp(9), eosp(10) )
c
c     **  increment the number of points in the
c     **  curve to reflect points for linear extrapolation.
c
      nld = nld + 2
      ncr = ncr + 2
      eosp(1) = float ( nld )
      eosp(2) = float ( ncr )
c
c     **  calculate the initial relative volume  **
c
      eosp(10) = 1. / ( 1. + eosp(10) )
c
c     interpolate the loading curve, and calculate the needed
c     coefficients for latter use by the evaluator intrpld
c
      call cubic ( nld, a(lld), a(lld+ncrv), a(lld+2*ncrv),
     1             a(lld+3*ncrv), a(lld+4*ncrv)  )
c
c     interpolate the completely crushed curve, and calculate the needed
c     coefficients for latter use by the evaluator intrpcr
c
      call cubic ( ncr, a(lcr), a(lcr+ncrv), a(lcr+2*ncrv),
     1             a(lcr+3*ncrv), a(lcr+4*ncrv)  )
c
c     interpolate the unloading curve, and calculate the needed
c     coefficients for latter use by the evaluator intrpun
c
c     **  copy the inverse of the completely crushed curve to  **
c     **     the unloading curve.  **
      call switch ( ncr, a(lcr), a(lcr+ncrv), a(lin), a(lin+ncrv) )
c
      call cubic ( ncr, a(lin), a(lin+ncrv), a(lin+2*ncrv),
     1             a(lin+3*ncrv), a(lin+4*ncrv)  )
c
      return
      end
      subroutine rdes11 (  nld, xld, fld, ncr, xcr, fcr,
     1                     u1, u2, energy, rlcomp )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     r  d  e  o  s  1  1                          *
c *                                                                  *
c ********************************************************************
c
c
c     routine to read in the neccessary parameters for the
c     tensor pore collapse equation-of-state.
c
c     author:  roger a. crawfis
c
c     date:  august 23, 1985
c
c
      parameter  (  ncrv = 64,  small = 1.e-30  )
      integer   nld, ncr
      character*80 txts,mssg
      dimension xld(*),fld(*),xcr(*),fcr(*)
      external  erexit
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=60,err=30)  nld, ncr, u1, u2, energy, rlcomp
      if ( u1 .eq. 0.0 )  u1 = small
      if ( (nld .gt. ncrv-2) .or. (nld .le. 2) )  then
         ierr = -6
         call erexit ( ierr )
      endif
      do 10  i = 1, nld
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=50,err=30) xld(i),fld(i)
   10 continue
c
      if ( (ncr .gt. ncrv-2) .or. (ncr .le. 2) )  then
         ierr = -6
         call erexit ( ierr )
      endif
c     **  read the completely crushed curve  **
      do 20  i = 1, ncr
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=50,err=30) xcr(i),fcr(i)
   20 continue
      return
   30 mssg=' error reading parameters for eos type 11'
      call termin (txts,mssg,lcount,1)
   50 format(2e15.0)
   60 format(2i5,4e10.0)
      end
      subroutine switch ( n1, x1, f1, x2, f2 )
c     implicit double precision (a-h,o-z)                                    dp
      dimension x1(*),f1(*),x2(*),f2(*)
      do 10  i = 1, n1
         x2(i) = f1(i)
         f2(i) = x1(i)
   10 continue
      return
      end
      subroutine cubic ( n, x, f, d, c2, c3 )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     c  u  b  i  c                                *
c *                                                                  *
c ********************************************************************
c
      parameter  (  ncrv = 64  )
      dimension x(*),f(*),d(*),c2(*),c3(*)
      dimension h(ncrv),delta(ncrv)
      external  pchim, erexit
c
c     **  add an extra point to the front and end
c         for linear extrapolation.  **
c
      x(1) = x(2) - ( x(3) - x(2) )
      f(1) = f(2) - ( f(3) - f(2) )
      x(n) = x(n-1) + ( x(n-1) - x(n-2) )
      f(n) = f(n-1) + ( f(n-1) - f(n-2) )
c
c     **  calculate the coefficients for the cubic spline.
c
      call pchim ( n, x, f, d, 1, ierr )
      if ( ierr .lt. 0 )  then
c        **  error detected by pchim  **
         call erexit (ierr)
      endif
c     **  calculate the coefficients  c2  and c3  **
      do 10  i = 1, n-1
         h(i) = x(i+1) - x(i)
         delta(i) = ( f(i+1) - f(i) ) / h(i)
         c2(i) = -( d(i) + d(i) + d(i+1) - 3.0*delta(i) ) / h(i)
         c3(i) =  ( d(i) + d(i+1) - 2.0*delta(i) ) / ( h(i)*h(i) )
   10 continue
      return
      end
      subroutine erexit ( ierr )
c     implicit double precision (a-h,o-z)                                    dp
c     author:  roger a. crawfis
c
c     date:  august 23, 1985
c
c     external  exit
c
      write (13,1001)  ierr
 1001 format (//'  error in eos 11,  ierr =',i3 )
      call exita (1)
      return
      end
      subroutine pres11 ( iowrt, eosp )
c     implicit double precision (a-h,o-z)                                    dp
c ********************************************************************
c *                                                                  *
c *                     p  r  e  o  s  1  1                          *
c *                                                                  *
c ********************************************************************
c
      common / /  a(1)
      parameter  (  ncrv = 64  )
c
c
      write (iowrt, 1000) eosp(3), eosp(4), eosp(10)
 1000 format (
     1        5x, 'point of initial pore collapse ... =', e12.4/
     2        5x, 'loading, unloading merge point ... =', e12.4/
     9        5x, 'initial relative volume .......... =', e12.4// )
      nld = int ( eosp(1) )
      ncr = int ( eosp(2) )
      n = min ( nld, ncr )
      lld  = int ( eosp(48) )
      lcr  = lld + 5*ncrv
      write (iowrt, 1004)
      do 10  i = 1, n
         write (iowrt, 1005)  a(lld+i-1), a(lld+ncrv+i-1),
     1                          a(lcr+i-1), a(lcr+ncrv+i-1)
   10 continue
      if ( nld .gt. n )  then
         do 20  i = n+1, nld
            write (iowrt, 1006)  a(lld+i-1), a(lld+ncrv+i-1)
   20    continue
      elseif ( ncr .gt. n )  then
         do 30  i = n+1, ncr
            write (iowrt, 1007)  a(lcr+i-1), a(lcr+ncrv+i-1)
   30    continue
      endif
      return
 1004 format (/7x,'virgin loading curve',18x,'completely crushed curve'
     1        /6x,'excess',34x,'excess'
     2        /3x,2('compression',7x,'pressure',15x)
     3        /3x,2('-----------',7x,'--------',15x)   )
 1005 format ( 1x,e13.4,3x,e13.4,11x,e13.4,3x,e13.4 )
 1006 format ( 1x,e13.4,3x,e13.4 )
 1007 format (41x,e13.4,3x,e13.4 )
      end
      subroutine pchim(n,x,f,d,incfd,ierr)
c     implicit double precision (a-h,o-z)                                    dp
c   subroutine / function  pchim*
c
c
cc+------------------------------------------------------------------+cc
cc+                                                                  +cc
cc+                lawrence livermore national laboratory            +cc
cc+                                                                  +cc
cc+        mathematics and statistics division software library      +cc
cc+                                                                  +cc
cc+------------------------------------------------------------------+cc
cc+                                                                  +cc
cc+  each class one routine has been thoroughly tested by msd and    +cc
cc+  meets certain documentation and programming standards.          +cc
cc+                                                                  +cc
cc+  at least one consultant is available to answer questions and    +cc
cc+  respond to reported errors or inadequacies in a class one       +cc
cc+  routine.                                                        +cc
cc+                                                                  +cc
cc+  +-----------------------------------------------------------+   +cc
cc+  +                        n o t i c e                        +   +cc
cc+  +                                                           +   +cc
cc+  +  this report was prepared as an account of work sponsored +   +cc
cc+  +  by the united states government.  neither the united     +   +cc
cc+  +  states government nor any of their employees, nor any of +   +cc
cc+  +  their contractors, subcontractors, or their employees,   +   +cc
cc+  +  makes any warranty, expressed or implied, or assumes any +   +cc
cc+  +  legal liability or responsibility for the accuracy,      +   +cc
cc+  +  completeness or usefulness of any information, apparatus,+   +cc
cc+  +  product or process disclosed, or represents that its use +   +cc
cc+  +  would not infringe privately-owned rights.               +   +cc
cc+  +                                                           +   +cc
cc+  +-----------------------------------------------------------+   +cc
cc+                                                                  +cc
cc+  please report any suspected errors in this routine immediately  +cc
cc+  to the msd consulting office, x-32976.                          +cc
cc+                                                                  +cc
cc+------------------------------------------------------------------+cc
c
c***begin prologue  pchim
c***date written   811103   (yymmdd)
c***revision date  820803   (yymmdd)
c***category no.  e1b
c***keywords  cubic hermite interpolation,monotone interpolation,
c             piecewise cubic interpolation
c***author  fritsch, f. n., (llnl)
c             mathematics and statistics division
c             lawrence livermore national laboratory
c             p.o. box 808  (l-316)
c             livermore, ca  94550
c             fts 532-4275, (415) 422-4275
c***purpose  sets derivatives needed to determine a monotone piecewise
c            cubic hermite interpolant to given data.  boundary values
c            are provided which are compatible with monotonicity.  the
c            interpolant will have an extremum at each point where mono-
c            tonicity switches direction.  (see pchic if user control is
c            desired over boundary or switch conditions.)
c***description
c
c          pchim:  piecewise cubic hermite interpolation to
c                  monotone data.
c
c     sets derivatives needed to determine a monotone piecewise cubic
c     hermite interpolant to the data given in x and f.
c
c     default boundary conditions are provided which are compatible
c     with monotonicity.  (see pchic if user control of boundary con-
c     ditions is desired.)
c
c     if the data are only piecewise monotonic, the interpolant will
c     have an extremum at each point where monotonicity switches direc-
c     tion.  (see pchic if user control is desired in such cases.)
c
c     to facilitate two-dimensional applications, includes an increment
c     between successive values of the f- and d-arrays.
c
c     the resulting piecewise cubic hermite function may be evaluated
c     by pchfe or pchfd.
c
c ----------------------------------------------------------------------
c
c  calling sequence:
c
c           call  pchim (n, x, f, d, incfd, ierr)
c
c   parameters:
c
c     n -- (input) number of data points.  (error return if n.lt.2 .)
c           if n=2, simply does linear interpolation.
c
c     x -- (input) real array of independent variable values.  the
c           elements of x must be strictly increasing:
c                x(i-1) .lt. x(i),  i = 2(1)n.
c           (error return if not.)
c
c     f -- (input) real array of dependent variable values to be inter-
c           polated.  f(1+(i-1)*incfd) is value corresponding to x(i).
c           pchim is designed for monotonic data, but it will work for
c           any f-array.  it will force extrema at points where mono-
c           tonicity switches direction.  if some other treatment of
c           switch points is desired, pchic should be used instead.
c                                     -----
c     d -- (output) real array of derivative values at the data points.
c           if the data are monotonic, these values will determine a
c           a monotone cubic hermite function.
c           the value corresponding to x(i) is stored in
c                d(1+(i-1)*incfd),  i=1(1)n.
c           no other entries in d are changed.
c
c     incfd -- (input) increment between successive values in f and d.
c           this argument is provided primarily for 2-d applications.
c           (error return if  incfd.lt.1 .)
c
c     ierr -- (output) error flag.
c           normal return:
c              ierr = 0  (no errors).
c           warning error:
c              ierr.gt.0  means that ierr switches in the direction
c                 of monotonicity were detected.
c           'recoverable' errors:
c              ierr = -1  if n.lt.2 .
c              ierr = -2  if incfd.lt.1 .
c              ierr = -3  if the x-array is not strictly increasing.
c             (the d-array has not been changed in any of these cases.)
c               note:  the above errors are checked in the order listed,
c                   and following arguments have **not** been validated.
c
c ----------------------------------------------------------------------
c
c
c  change record:
c     82-02-01   1. introduced  pchst  to reduce possible over/under-
c                   flow problems.
c                2. rearranged derivative formula for same reason.
c     82-06-02   1. modified end conditions to be continuous functions
c                   of data when monotonicity switches in next interval.
c                2. modified formulas so end conditions are less prone
c                   of over/underflow problems.
c     82-08-03   minor cosmetic changes for release 1.
c
c ----------------------------------------------------------------------
c
c  programming notes:
c
c     1. the function  pchst(arg1,arg2)  is assumed to return zero if
c        either argument is zero, +1 if they are of the same sign, and
c        -1 if they are of opposite sign.
c     2. to produce a double precision version, simply:
c        a. change pchim to dpchim wherever it occurs,
c        b. change pchst to dpchst wherever it occurs,
c        c. change all references to the fortran intrinsics to their
c           double precision equivalents,
c        d. change the real declarations to double precision, and
c        e. change the constants zero and three to double precision.
c***references  1. f.n.fritsch and r.e.carlson, 'monotone piecewise
c                 cubic interpolation,' siam j.numer.anal. 17, 2 (april
c                 1980), 238-246.
c               2. f.n.fritsch and j.butland, 'a method for constructing
c                 local monotone piecewise cubic interpolants,' llnl
c                 preprint ucrl-87559 (april 1982).
c***routines called  pchst,xzeror
c***end prologue  pchim
      dimension  x(*), f(incfd,*), d(incfd,*)
c
      data  zero /0./,  three /3./
c
c  validity-check arguments.
c
c***first executable statement  pchim
      if ( n.lt.2 )  go to 5001
      if ( incfd.lt.1 )  go to 5002
      do 1  i = 2, n
         if ( x(i).le.x(i-1) )  go to 5003
    1 continue
c
c  function definition is ok, go on.
c
      ierr = 0
      nless1 = n - 1
      h1 = x(2) - x(1)
      del1 = (f(1,2) - f(1,1))/h1
      dsave = del1
c
c  special case n=2 -- use linear interpolation.
c
      if (nless1 .gt. 1)  go to 10
      d(1,1) = del1
      d(1,n) = del1
      go to 5000
c
c  normal case  (n .ge. 3).
c
   10 continue
      h2 = x(3) - x(2)
      del2 = (f(1,3) - f(1,2))/h2
c
c  set d(1) via non-centered three-point formula, adjusted to be
c     shape-preserving.
c
      hsum = h1 + h2
      w1 = (h1 + hsum)/hsum
      w2 = -h1/hsum
      d(1,1) = w1*del1 + w2*del2
      if ( pchst(d(1,1),del1) .le. zero)  then
         d(1,1) = zero
      else if ( pchst(del1,del2) .lt. zero)  then
c        need do this check only if monotonicity switches.
         dmax = three*del1
         if (abs(d(1,1)) .gt. abs(dmax))  d(1,1) = dmax
      endif
c
c  loop through interior points.
c
      do 50  i = 2, nless1
         if (i .eq. 2)  go to 40
c
         h1 = h2
         h2 = x(i+1) - x(i)
         hsum = h1 + h2
         del1 = del2
         del2 = (f(1,i+1) - f(1,i))/h2
   40    continue
c
c        set d(i)=0 unless data are strictly monotonic.
c
         d(1,i) = zero
         if ( pchst(del1,del2) )  42, 41, 45
c
c        count number of changes in direction of monotonicity.
c
   41    continue
         if (del2 .eq. zero)  go to 50
         if ( pchst(dsave,del2) .lt. zero)  ierr = ierr + 1
         dsave = del2
         go to 50
c
   42    continue
         ierr = ierr + 1
         dsave = del2
         go to 50
c
c        use brodlie modification of butland formula.
c
   45    continue
         hsumt3 = hsum+hsum+hsum
         w1 = (hsum + h1)/hsumt3
         w2 = (hsum + h2)/hsumt3
         dmax =   max( abs(del1), abs(del2) )
         dmin =   min( abs(del1), abs(del2) )
         drat1 = del1/dmax
         drat2 = del2/dmax
         d(1,i) = dmin/(w1*drat1 + w2*drat2)
c
   50 continue
c
c  set d(n) via non-centered three-point formula, adjusted to be
c     shape-preserving.
c
      w1 = -h2/hsum
      w2 = (h2 + hsum)/hsum
      d(1,n) = w1*del1 + w2*del2
      if ( pchst(d(1,n),del2) .le. zero)  then
         d(1,n) = zero
      else if ( pchst(del1,del2) .lt. zero)  then
c        need do this check only if monotonicity switches.
         dmax = three*del2
         if (abs(d(1,n)) .gt. abs(dmax))  d(1,n) = dmax
      endif
c
c  normal return.
c
 5000 continue
      return
c
c  error returns.
c
 5001 continue
c     n.lt.2 return.
      ierr = -1
      call xzeror ('pchim -- number of data points less than two'
     *           , 44, ierr, 1)
      return
c
 5002 continue
c     incfd.lt.1 return.
      ierr = -2
      call xzeror ('pchim -- increment less than one'
     *           , 32, ierr, 1)
      return
c
 5003 continue
c     x-array not strictly increasing.
      ierr = -3
      call xzeror ('pchim -- x-array not strictly increasing'
     *           , 40, ierr, 1)
      return
c------------- last line of pchim follows ------------------------------
      end
      function pchst(arg1,arg2)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  pchst
c***refer to  pchce,pchci,pchcs,pchim
c
c         pchst:  pchip sign-testing routine.
c
c
c     returns:
c        -1. if arg1 and arg2 are of opposite sign.
c         0. if either argument is zero.
c        +1. if arg1 and arg2 are of the same sign.
c
c     the object is to do this without multiplying arg1*arg2, to avoid
c     possible over/underflow problems.
c
c  fortran intrinsics used:  sign.
c
c ----------------------------------------------------------------------
c
c  programmed by:  fred n. fritsch,  fts 532-4275, (415) 422-4275,
c                  mathematics and statistics division,
c                  lawrence livermore national laboratory.
c
c  change record:
c     82-08-05   converted to slatec library version.
c
c ----------------------------------------------------------------------
c
c  programming notes:
c
c     to produce a double precision version, simply:
c        a. change pchst to dpchst wherever it occurs,
c        b. change all references to the fortran intrinsics to their
c           double presision equivalents,
c        c. change the real declarations to double precision, and
c        d. change the constants  zero  and  one  to double precision.
c***routines called  (none)
c***end prologue  pchst
c
c  declare local variables.
c
      data  zero /0./,  one /1./
c
c  perform the test.
c
c***first executable statement  pchst
      pchst = sign(one,arg1) * sign(one,arg2)
      if ((arg1.eq.zero) .or. (arg2.eq.zero))  pchst = zero
c
      return
c------------- last line of pchst follows ------------------------------
      end
