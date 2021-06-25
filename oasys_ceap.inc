c
c   If any parameters in this file are modified, then please also
c   modify them in oasys_ceap_kw.inc.
c
      character*256 fn_damp
      common /ceap_damp_1/fn_damp
      integer iceap_ctl_len
      parameter (iceap_ctl_len=120)
      real ceap_r_1
      common /ceap_damp_2/  ceap_r_1(iceap_ctl_len)
      integer  ceap_i_cnt
      common /ceap_damp_3/  ceap_i_cnt(iceap_ctl_len)
c
c   The contents of ceap_i_cnt and ceap_r_1 are described in 
c   oasys_ceap_array_contents.inc. These inc files have been
c   split to prevent the need to recompile many overlays 
c   just because a comment line has changed in oasys_ceap.inc.
c
      integer iceap_on, icpf, icpf_atype, icpf_output, ipwptime, icpa,
     . icpc,icpa_prt
      common /iceap_ctrl/ 
     .                       iceap_on,
     .                       icpf,
     .                       icpf_atype,
     .                       icpf_output,
     .                       ipwptime,
     .                       icpa,
     .                       icpc,
     .                       icpa_prt
c
      integer nnm1_store
      common /iceap_store/ nnm1_store
c
      real cpf_datum, cpf_wtable, cpf_pfrho, cpf_grav, cpf_pfbkm,
     .     cpf_tmf, event_time, event_mult, cpf_parho, cpf_pair,
     .     cpf_rta
      common /rceap_ctrl/
     .                       cpf_datum,
     .                       cpf_wtable,
     .                       cpf_pfrho,
     .                       cpf_grav,
     .                       cpf_pfbkm,
     .                       cpf_tmf,
     .                       event_time,
     .                       event_mult,
     .                       cpf_parho,cpf_pair,cpf_rta
c
c icpf       - pore water pressure switch - 0 => off, 1 => on
c icpf_atype - pwp type - 0 => none, 
c                       - 1 => undrained,
c                       - 2 => drained,
c                       - 3 => TDC,
c                       - 4 => Consolidate to steady state,
c                       - 5 => Drained in DR, undrained in transient
c cpf_datum  - z-datum for pwp calculations
c cpf_wtable - default z coord of water table
c cpf_pfrho  - default pore fluid density
c cpf_grav   - gravity loading applied to pore fluid
c cpf_pfbkm  - default pore fluid bulk modulus
c cpf_parho  - default pore air density
c cpf_rair   - default pore air gas constant (Joule/Kg/Deg)
c cpf_tair   - default pore air temperature
c cpf_pair   - atmospheric pressure
c icpf_output- switch output to effective stress tensor
c
c
c number of pore water pressure history variables per solid element
c (now reduced from 15 to 5) - npwpcon initialized to 5 in init_once
      integer npwpcon
c      parameter (npwpcon=5)
      common/ceap_pwp1/npwpcon,npwpnod
c number of pore water pressure history variables per node          
      integer npwpnod
c      parameter (npwpnod=25)
c
c
c CEAP Arrays
c
      common /ceap_pnt/mp_cer,mp_cei
      common /ceap_ar1/ piceap,prceap
      pointer(mp_cei,iceap)
      pointer(mp_cer,rceap)
      integer iicpnt_len
      parameter (iicpnt_len=100)
      integer ircpnt_len
      parameter (ircpnt_len=100)
      integer piceap(iicpnt_len),prceap(ircpnt_len)
      integer iceap(1)
      real    rceap(1)
      integer ihcpnt_len
      parameter (ihcpnt_len=500)
      integer ihceap(ihcpnt_len)
      common/ceap_handles/ihceap
c
c     iceap      - main integer ceap array
c     rceap      - main real ceap array
c
c     mp_cei     - memory pointer to base of iceap
c     mp_cer     - memory pointer to base of rceap
c
c     iicpnt_len - length of ceap 'pointers to integers' array 
c                  (piceap)
c     ircpnt_len - length of ceap 'pointers to reals' array (prceap)
c     ihcpnt_len - length of ceap handles (ihceap)
c
c     piceap     - pointers to iceap (listed in oasys_ceap_array_contents.inc)
c     prceap     - pointers to rceap (listed in oasys_ceap_array_contents.inc)
c     ihceap     - handles to ceap storage in rmem                            
