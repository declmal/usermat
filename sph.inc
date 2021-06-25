c
c sph memory pointers
      integer  ldsph00,ldsph01,ldsph02,ldsph03,ldsph04,ldsph05,
     1 ldsph06,ldsph07,ldsph08,ldsph09,ldsph10,ldsph11,ldsph12,
     2 ldsph13,ldsph14,ldsph15,ldsph16,ldsph17,ldsph18,ldsph19,
     3 ldsph20,ldsph21,ldsph22,ldsph23,ldsph24,ldsph25,ldsph26,
     4 ldsph27,ldsph28,ldsph29,ldsph30,ldsph31,ldsph32,ldsph33,
     5 ldsph34,ldsph35,ldsph36

      common/sph17/ldsph00,ldsph01,ldsph02,ldsph03,ldsph04,ldsph05,
     1 ldsph06,ldsph07,ldsph08,ldsph09,ldsph10,ldsph11,ldsph12,
     2 ldsph13,ldsph14,ldsph15,ldsph16,ldsph17,ldsph18,ldsph19,
     3 ldsph20,ldsph21,ldsph22,ldsph23,ldsph24,ldsph25,ldsph26,
     4 ldsph27,ldsph28,ldsph29,ldsph30,ldsph31,ldsph32,ldsph33,
     5 ldsph34,ldsph35,ldsph36
c
c sph bk01
      real bgnsph,sphxl,sphxu,sphyl,sphyu,sphzl,sphzu,
     & dtsphini,expdmsph,strtm,xvsph,fsphshw,psmoot,
     & sph_boxc(3),sph_gl(3,3)
      integer iactsph,nsph_extra,ispherd,isphcont,nzsphcpl,
     & nsphdecpl,nsph_react,nsph_reg,isphtmp(11),
     & isphterode,isphavisc,isphshw,msph_extra,
     & nsph_ntot,irf_sph
      common/sphbk01/bgnsph,sphxl,sphxu,sphyl,sphyu,sphzl,sphzu,
     & dtsphini,expdmsph,strtm,xvsph,iactsph,nsph_extra,ispherd,
     & isphcont,nzsphcpl,nsph_react,isphtmp,isphterode,isphavisc,
     & isphshw,fsphshw,msph_extra,nsph_ntot,nsphdecpl,nsph_reg,
     & psmoot,sph_boxc,sph_gl,irf_sph
c
c sph bk03 adaptice blocks
      real    adpsph
      integer nfstsph,nzadpsph,iadpsph,ifailcouple
      common /sphbk03/ nfstsph,nzadpsph,adpsph(4),iadpsph(4),
     1 ifailcouple
c
c sph bk04 sph shape functions for adp
      real    sphh
      common /sphbk04/ sphh(8,243)
c
c sph extra variables for MPP
      integer mpp_sphflow,mpp_sphflw,mpp_sphtmp(20)
      common /mpp_sphbk01/ mpp_sphflow,mpp_sphflw,mpp_sphtmp
c sph variable s for axisymmetry and 2.5 D option
      integer itwoh
      common /sphaxisym/itwoh
c
c sph extran memory handlers
      integer isphtempp,isphtbch,isphtbfh,isphbulkq,isphbqsz,
     . nsphtempp,hlistsmp,nlistsmp,hsph_smp,nsph_smp,isphtbcch,
     . isphtbrh,nsph_01
      common/sphtempt/isphtempp,isphtbch,isphtbfh,isphbulkq,isphbqsz,
     .     nsphtempp,hlistsmp,nlistsmp,hsph_smp,nsph_smp,isphtbcch,
     .     isphtbrh,nsph_01
