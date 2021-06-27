      subroutine xzgtua(iunit,n)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xzgtua
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  r3c
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  returns unit number(s) to which error messages are being
c            sent
c***description
c
c     abstract
c        xzgtua may be called to determine the unit number or numbers
c        to which error messages are being sent.
c        these unit numbers may have been set by a call to xsetun,
c        or a call to xsetua, or may be a default value.
c
c     description of parameters
c      --output--
c        iunit - an array of one to five unit numbers, depending
c                on the value of n.  a value of zero refers to the
c                default unit, as defined by the i1mach machine
c                constant routine.  only iunit(1),...,iunit(n) are
c                defined by xzgtua.  the values of iunit(n+1),...,
c                iunit(5) are not defined (for n .lt. 5) or altered
c                in any way by xzgtua.
c        n     - the number of units to which copies of the
c                error messages are being sent.  n will be in the
c                range from 1 to 5.
c
c     written by ron jones, with slatec common math library subcommittee
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  j4save
c***end prologue  xzgtua
c
      dimension iunit(5)
c***first executable statement  xzgtua
      n = j4save(5,0,.false.)
      do 30 i=1,n
         index = i+4
         if (i.eq.1) index = 3
         iunit(i) = j4save(index,0,.false.)
   30 continue
      return
      end
      subroutine xerprt(messg,nmessg)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xerprt
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  z
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  prints error messages
c***description
c
c     abstract
c        print the hollerith message in messg, of length nmessg,
c        on each file indicated by xzgtua.
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  i1mach,s88fmt,xzgtua
c***end prologue  xerprt
c
      dimension lun(5)
      character*1 f(10),g(14),la,lcom,lblank
      dimension messg(nmessg)
      data f(1),f(2),f(3),f(4),f(5),f(6),f(7),f(8),f(9),f(10)
     1   / '(' ,'1' ,'x' ,',' ,' ' ,' ' ,'a' ,' ' ,' ' ,')'/
      data g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10)
     1   / '(' ,'1' ,'x' ,' ' ,' ' ,' ' ,' ' ,' ' ,' ' ,' '/
      data g(11),g(12),g(13),g(14)
     1   / '  ' ,'  ' ,'  ' ,')' /
      data la/'a'/,lcom/','/,lblank/' '/
c    prepare format for whole lines
c***first executable statement  xerprt
      nchar = i1mach(6)
      nfield = 72/nchar
      call s88fmt(2,nfield,f(5))
      call s88fmt(2,nchar,f(8))
c     prepare format for last, partial line, if needed
      ncharl = nfield*nchar
      nlines = nmessg/ncharl
      nword  = nlines*nfield
      nchrem = nmessg - nlines*ncharl
      if (nchrem.le.0) go to 40
         do 10 i=4,13
10          g(i) = lblank
         nfield = nchrem/nchar
         if (nfield.le.0) go to 20
c        prepare whole word fields
            g(4) = lcom
            call s88fmt(2,nfield,g(5))
            g(7) = la
            call s88fmt(2,nchar,g(8))
20       continue
         nchlst = mod(nchrem,nchar)
         if (nchlst.le.0) go to 30
c        prepare partial word field
            g(10) = lcom
            g(11) = la
            call s88fmt(2,nchlst,g(12))
30       continue
40    continue
c     print the message
      nword1 = nword+1
      nword2 = (nmessg+nchar-1)/nchar
      call xzgtua(lun,nunit)
      do 50 kunit = 1,nunit
         iunit = lun(kunit)
         if (iunit.eq.0) iunit = i1mach(4)
         if (nword.gt.0) write (iunit,f) (messg(i),i=1,nword)
         if (nchrem.gt.0) write (iunit,g) (messg(i),i=nword1,nword2)
50    continue
      return
      end
      subroutine xzeror(messg,nmessg,nerr,level)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xzeror
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  r3c
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  processes an error (diagnostic) message
c***description
c
c     abstract
c        xzeror processes a diagnostic message, in a manner
c        determined by the value of level and the current value
c        of the library error control flag, kontrl.
c        (see subroutine xsetf for details.)
c
c     description of parameters
c      --input--
c        messg - the hollerith message to be processed, containing
c                no more than 72 characters.
c        nmessg- the actual number of characters in messg.
c        nerr  - the error number associated with this message.
c                nerr must not be zero.
c        level - error category.
c                =2 means this is an unconditionally fatal error.
c                =1 means this is a recoverable error.  (i.e., it is
c                   non-fatal if xsetf has been appropriately called.)
c                =0 means this is a warning message only.
c                =-1 means this is a warning message which is to be
c                   printed at most once, regardless of how many
c                   times this call is executed.
c
c     examples
c        call xzeror(23hsmooth -- num was zero.,23,1,2)
c        call xzeror(43hinteg  -- less than full accuracy achieved.,
c                    43,2,1)
c        call xzeror(65hrooter -- actual zero of f found before interval
c    1 fully collapsed.,65,3,0)
c        call xzeror(39hexp    -- underflows being set to zero.,39,1,-1)
c
c     written by ron jones, with slatec common math library subcommittee
c     latest revision ---  7 feb 1979
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  xzerwv
c***end prologue  xzeror
c
      dimension messg(nmessg)
c***first executable statement  xzeror
      call xzerwv(messg,nmessg,nerr,level,0,0,0,0,0.,0.)
      return
      end
      subroutine xzerwv(messg,nmessg,nerr,level,ni,i1,i2,nr,r1,r2)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xzerwv
c***date written   800319   (yymmdd)
c***revision date  840405   (yymmdd)
c***category no.  r3c
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  processes error message allowing 2 integer and two real
c            values to be included in the message.
c***description
c
c     abstract
c        xzerwv processes a diagnostic message, in a manner
c        determined by the value of level and the current value
c        of the library error control flag, kontrl.
c        (see subroutine xsetf for details.)
c        in addition, up to two integer values and two real
c        values may be printed along with the message.
c
c     description of parameters
c      --input--
c        messg - the hollerith message to be processed.
c        nmessg- the actual number of characters in messg.
c        nerr  - the error number associated with this message.
c                nerr must not be zero.
c        level - error category.
c                =2 means this is an unconditionally fatal error.
c                =1 means this is a recoverable error.  (i.e., it is
c                   non-fatal if xsetf has been appropriately called.)
c                =0 means this is a warning message only.
c                =-1 means this is a warning message which is to be
c                   printed at most once, regardless of how many
c                   times this call is executed.
c        ni    - number of integer values to be printed. (0 to 2)
c        i1    - first integer value.
c        i2    - second integer value.
c        nr    - number of real values to be printed. (0 to 2)
c        r1    - first real value.
c        r2    - second real value.
c
c     examples
c        call xzeror(29hsmooth -- num (=i1) was zero.,29,1,2,
c    1   1,num,0,0,0.,0.)
c        call xzerwv(54hquadxy -- requested error (r1) less than minimum
c    1 (r2).,54,77,1,0,0,0,2,errreq,errmin)
c
c     written by ron jones, with slatec common math library subcommittee
c     latest revision ---  19 mar 1980
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  fdump,i1mach,j4save,xerabt,xerctl,xerprt,xersav,
c                    xzgtua
c***end prologue  xzerwv
c
      dimension messg(nmessg),lun(5)
c***first executable statement  xzerwv
      lkntrl = j4save(2,0,.false.)
      maxmes = j4save(4,0,.false.)
c     check for valid input
      if ((nmessg.gt.0).and.(nerr.ne.0).and.
     1    (level.ge.(-1)).and.(level.le.2)) go to 10
         if (lkntrl.gt.0) call xerprt('fatal error in...',17)
         call xerprt('xzeror -- invalid input',23)
         if (lkntrl.gt.0) call fdump
         if (lkntrl.gt.0) call xerprt('job abort due to fatal error.',
     1   29)
         if (lkntrl.gt.0) call xersav(' ',0,0,0,kdummy)
         call xerabt(' xzeror -- invalid input',23)
         return
   10 continue
c     record message
      junk = j4save(1,nerr,.true.)
      call xersav(messg,nmessg,nerr,level,kount)
c     let user override
      lfirst = messg(1)
      lmessg = nmessg
      lerr = nerr
      llevel = level
      call xerctl(lfirst,lmessg,lerr,llevel,lkntrl)
c     reset to original values
      lmessg = nmessg
      lerr = nerr
      llevel = level
      lkntrl = max0(-2,min0(2,lkntrl))
      mkntrl = iabs(lkntrl)
c     decide whether to print message
      if ((llevel.lt.2).and.(lkntrl.eq.0)) go to 100
      if (((llevel.eq.(-1)).and.(kount.gt.min0(1,maxmes)))
     1.or.((llevel.eq.0)   .and.(kount.gt.maxmes))
     2.or.((llevel.eq.1)   .and.(kount.gt.maxmes).and.(mkntrl.eq.1))
     3.or.((llevel.eq.2)   .and.(kount.gt.max0(1,maxmes)))) go to 100
         if (lkntrl.le.0) go to 20
            call xerprt(' ',1)
c           introduction
            if (llevel.eq.(-1)) call xerprt
     1('warning message...this message will only be printed once.',57)
            if (llevel.eq.0) call xerprt('warning in...',13)
            if (llevel.eq.1) call xerprt
     1      ('recoverable error in...',23)
            if (llevel.eq.2) call xerprt('fatal error in...',17)
   20    continue
c        message
         call xerprt(messg,lmessg)
         call xzgtua(lun,nunit)
         do 50 kunit=1,nunit
            iunit = lun(kunit)
            if (iunit.eq.0) iunit = i1mach(4)
            if (ni.ge.1) write (iunit,22) i1
            if (ni.ge.2) write (iunit,23) i2
            if (nr.ge.1) write (iunit,24) r1
            if (nr.ge.2) write (iunit,25) r2
   22       format (11x, 'in above message, i1=',i10)
   23       format (11x, 'in above message, i2=',i10)
   24       format (11x, 'in above message, r1=',e20.10)
   25       format (11x, 'in above message, r2=',e20.10)
            if (lkntrl.le.0) go to 40
c              error number
               write (iunit,30) lerr
   30          format (' error number =',i10)
   40       continue
   50    continue
c        trace-back
         call fdump
  100 continue
      ifatal = 0
      if ((llevel.eq.2).or.((llevel.eq.1).and.(mkntrl.eq.2)))
     1ifatal = 1
c     quit here if message is not fatal
      if (ifatal.le.0) return
      if ((lkntrl.le.0).or.(kount.gt.max0(1,maxmes))) go to 120
c        print reason for abort
         if (llevel.eq.1) call xerprt
     1   ('job abort due to unrecovered error.',35)
         if (llevel.eq.2) call xerprt
     1   ('job abort due to fatal error.',29)
c        print error summary
         call xersav(' ',-1,0,0,kdummy)
  120 continue
c     abort
      if ((llevel.eq.2).and.(kount.gt.max0(1,maxmes))) lmessg = 0
      call xerabt(messg,lmessg)
      return
      end
      subroutine xersav(messg,nmessg,nerr,level,icount)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xersav
c***date written   800319   (yymmdd)
c***revision date  840405   (yymmdd)
c***category no.  z
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  records that an error occurred.
c***description
c
c     abstract
c        record that this error occurred.
c
c     description of parameters
c     --input--
c       messg, nmessg, nerr, level are as in xzeror,
c       except that when nmessg=0 the tables will be
c       dumped and cleared, and when nmessg is less than zero the
c       tables will be dumped and not cleared.
c     --output--
c       icount will be the number of times this message has
c       been seen, or zero if the table has overflowed and
c       does not contain this message specifically.
c       when nmessg=0, icount will not be altered.
c
c     written by ron jones, with slatec common math library subcommittee
c     latest revision ---  19 mar 1980
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                1978.
c***routines called  i1mach,s88fmt,xzgtua
c***end prologue  xersav
c
      dimension lun(5)
      character*1 f(17)
      dimension messg(1)
      dimension mestab(10),nertab(10),levtab(10),kount(10)
      data mestab(1),mestab(2),mestab(3),mestab(4),mestab(5),
     1     mestab(6),mestab(7),mestab(8),mestab(9),mestab(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      data nertab(1),nertab(2),nertab(3),nertab(4),nertab(5),
     1     nertab(6),nertab(7),nertab(8),nertab(9),nertab(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      data levtab(1),levtab(2),levtab(3),levtab(4),levtab(5),
     1     levtab(6),levtab(7),levtab(8),levtab(9),levtab(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      data kount(1),kount(2),kount(3),kount(4),kount(5),
     1     kount(6),kount(7),kount(8),kount(9),kount(10)
     2     /0,0,0,0,0,0,0,0,0,0/
      data kountx/0/
      data f(1),f(2),f(3),f(4),f(5),f(6),f(7),f(8),f(9),f(10),
     1     f(11),f(12),f(13),f(14),f(15),f(16),f(17)
     2     /'(' ,'1' ,'x' ,',' ,'a' ,' ' ,' ' ,',' ,'i' ,' ' ,
     3      ' ' ,',' ,'2' ,'i' ,'1' ,'0' ,')' /
c***first executable statement  xersav
      if (nmessg.gt.0) go to 80
c     dump the table
         if (kount(1).eq.0) return
c        prepare format
         nchar = i1mach(6)
         call s88fmt(2,nchar,f(6))
         ncol = 20 - nchar
         call s88fmt(2,ncol,f(10))
c        print to each unit
         call xzgtua(lun,nunit)
         do 60 kunit=1,nunit
            iunit = lun(kunit)
            if (iunit.eq.0) iunit = i1mach(4)
c           print table header
            write (iunit,10)
   10       format ('0          error message summary'/
     1              ' first word      nerr     level     count')
c           print body of table
            do 20 i=1,10
               if (kount(i).eq.0) go to 30
               write (iunit,f) mestab(i),nertab(i),levtab(i),kount(i)
   20       continue
   30       continue
c           print number of other errors
            if (kountx.ne.0) write (iunit,40) kountx
   40       format ('0other errors not individually tabulated=',i10)
            write (iunit,50)
   50       format (1x)
   60    continue
         if (nmessg.lt.0) return
c        clear the error tables
         do 70 i=1,10
   70       kount(i) = 0
         kountx = 0
         return
   80 continue
c     process a message...
c     search for this messg, or else an empty slot for this messg,
c     or else determine that the error table is full.
      do 90 i=1,10
         ii = i
         if (kount(i).eq.0) go to 110
         if (messg(1).ne.mestab(i)) go to 90
         if (nerr.ne.nertab(i)) go to 90
         if (level.ne.levtab(i)) go to 90
         go to 100
   90 continue
c     three possible cases...
c     table is full
         kountx = kountx+1
         icount = 1
         return
c     message found in table
  100    kount(ii) = kount(ii) + 1
         icount = kount(ii)
         return
c     empty slot found for new message
  110    mestab(ii) = messg(1)
         nertab(ii) = nerr
         levtab(ii) = level
         kount(ii)  = 1
         icount = 1
         return
      end
      subroutine xerctl(messg1,nmessg,nerr,level,kontrl)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xerctl
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  r3c
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  allows user control over handling of individual errors
c***description
c
c     abstract
c        allows user control over handling of individual errors.
c        just after each message is recorded, but before it is
c        processed any further (i.e., before it is printed or
c        a decision to abort is made), a call is made to xerctl.
c        if the user has provided his own version of xerctl, he
c        can then override the value of kontrol used in processing
c        this message by redefining its value.
c        kontrl may be set to any value from -2 to 2.
c        the meanings for kontrl are the same as in xsetf, except
c        that the value of kontrl changes only for this message.
c        if kontrl is set to a value outside the range from -2 to 2,
c        it will be moved back into that range.
c
c     description of parameters
c
c      --input--
c        messg1 - the first word (only) of the error message.
c        nmessg - same as in the call to xzeror or xzerwv.
c        nerr   - same as in the call to xzeror or xzerwv.
c        level  - same as in the call to xzeror or xzerwv.
c        kontrl - the current value of the control flag as set
c                 by a call to xsetf.
c
c      --output--
c        kontrl - the new value of kontrl.  if kontrl is not
c                 defined, it will remain at its original value.
c                 this changed value of control affects only
c                 the current occurrence of the current message.
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  (none)
c***end prologue  xerctl
c
c***first executable statement  xerctl
      return
      end
      subroutine xerabt(messg,nmessg)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  xerabt
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  r3c
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  aborts program execution and prints error message
c***description
c
c     abstract
c        ***note*** machine dependent routine
c        xerabt aborts the execution of the program.
c        the error message causing the abort is given in the calling
c        sequence, in case one needs it for printing on a dayfile,
c        for example.
c
c     description of parameters
c        messg and nmessg are as in xzeror, except that nmessg may
c        be zero, in which case no message is being supplied.
c     written by ron jones, with slatec common math library subcommittee
c     latest revision ---  7 june 1978
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  (none)
c***end prologue  xerabt
c
      dimension messg(nmessg)
c***first executable statement  xerabt
      stop
c     return
      end
      subroutine s88fmt(n,ivalue,ifmt)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  s88fmt
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  z
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  integer to character conversion
c***description
c
c     abstract
c        s88fmt replaces ifmt(1), ... ,ifmt(n) with the
c        characters corresponding to the n least significant
c        digits of ivalue.
c
c     taken from the bell laboratories port library error handler
c     latest revision ---  7 june 1978
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  (none)
c***end prologue  s88fmt
c
      character*1 ifmt(n),idigit(10)
      data idigit(1),idigit(2),idigit(3),idigit(4),idigit(5),
     1     idigit(6),idigit(7),idigit(8),idigit(9),idigit(10)
     2     /'0','1','2','3','4','5','6','7','8','9'/
c***first executable statement  s88fmt
      nt = n
      it = ivalue
   10    if (nt .eq. 0) return
         index = mod(it,10)
         ifmt(nt) = idigit(index+1)
         it = it/10
         nt = nt - 1
         go to 10
      end
      function j4save(iwhich,ivalue,iset)
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  j4save
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  z
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  saves and recalls some global variables needed by library
c            error handling routines
c***description
c
c     abstract
c        j4save saves and recalls several global variables needed
c        by the library error handling routines.
c
c     description of parameters
c      --input--
c        iwhich - index of item desired.
c                 = 1 refers to current error number.
c                 = 2 refers to current error control flag.
c                 = 3 refers to current unit number to which error
c                     messages are to be sent.  (0 means use standard.)
c                 = 4 refers to the maximum number of times any
c                     message is to be printed (as set by xermax).
c                 = 5 refers to the total number of units to which
c                     each error message is to be written.
c                 = 6 refers to the 2nd unit for error messages
c                 = 7 refers to the 3rd unit for error messages
c                 = 8 refers to the 4th unit for error messages
c                 = 9 refers to the 5th unit for error messages
c        ivalue - the value to be set for the iwhich-th parameter,
c                 if iset is .true. .
c        iset   - if iset=.true., the iwhich-th parameter will be
c                 given the value, ivalue.  if iset=.false., the
c                 iwhich-th parameter will be unchanged, and ivalue
c                 is a dummy parameter.
c      --output--
c        the (old) value of the iwhich-th parameter will be returned
c        in the function value, j4save.
c
c     written by ron jones, with slatec common math library subcommittee
c     adapted from bell laboratories port library error handler
c     latest revision ---  23 may 1979
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  (none)
c***end prologue  j4save
c
      logical iset
      dimension iparam(9)
      data iparam(1),iparam(2),iparam(3),iparam(4)/0,1,0,10/
      data iparam(5)/1/
      data iparam(6),iparam(7),iparam(8),iparam(9)/0,0,0,0/
c***first executable statement  j4save
      j4save = iparam(iwhich)
      if (iset) iparam(iwhich) = ivalue
      return
      end
      integer function i1mach(i)
c***begin prologue  i1mach
c***date written   750101   (yymmdd)
c***revision date  840405   (yymmdd)
c***category no.  r1
c***keywords  machine constants
c***author  fox, p. a., (bell labs)
c           hall, a. d., (bell labs)
c           schryer, n. l., (bell labs)
c***purpose  returns integer machine dependent constants
c***description
c
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c   these machine constant routines must be activated for
c   a particular environment.
c * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
c
c     i1mach can be used to obtain machine-dependent parameters
c     for the local machine environment.  it is a function
c     subroutine with one (input) argument, and can be called
c     implicit double precision (a-h,o-z)                                    dp
c     as follows, for example
c
c          k = i1mach(i)
c
c     where i=1,...,16.  the (output) value of k above is
c     determined by the (input) value of i.  the results for
c     various values of i are discussed below.
c
c  i/o unit numbers.
c    i1mach( 1) = the standard input unit.
c    i1mach( 2) = the standard output unit.
c    i1mach( 3) = the standard punch unit.
c    i1mach( 4) = the standard error message unit.
c
c  words.
c    i1mach( 5) = the number of bits per integer storage unit.
c    i1mach( 6) = the number of characters per integer storage unit.
c
c  integers.
c    assume integers are represented in the s-digit, base-a form
c
c               sign ( x(s-1)*a**(s-1) + ... + x(1)*a + x(0) )
c
c               where 0 .le. x(i) .lt. a for i=0,...,s-1.
c    i1mach( 7) = a, the base.
c    i1mach( 8) = s, the number of base-a digits.
c    i1mach( 9) = a**s - 1, the largest magnitude.
c
c  floating-point numbers.
c    assume floating-point numbers are represented in the t-digit,
c    base-b form
c               sign (b**e)*( (x(1)/b) + ... + (x(t)/b**t) )
c
c               where 0 .le. x(i) .lt. b for i=1,...,t,
c               0 .lt. x(1), and emin .le. e .le. emax.
c    i1mach(10) = b, the base.
c
c  single-precision
c    i1mach(11) = t, the number of base-b digits.
c    i1mach(12) = emin, the smallest exponent e.
c    i1mach(13) = emax, the largest exponent e.
c
c  double-precision
c    i1mach(14) = t, the number of base-b digits.
c    i1mach(15) = emin, the smallest exponent e.
c    i1mach(16) = emax, the largest exponent e.
c
c  to alter this function for a particular environment,
c  the desired set of data statements should be activated by
c  removing the c from column 1.  also, the values of
c  i1mach(1) - i1mach(4) should be checked for consistency
c  with the local operating system.
c***references  fox p.a., hall a.d., schryer n.l.,*framework for a
c                 portable library*, acm transactions on mathematical
c                 software, vol. 4, no. 2, june 1978, pp. 177-188.
c***routines called  (none)
c***end prologue  i1mach
c
      dimension imach(16)
c
c     machine constants for the burroughs 1700 system.
c
c     data imach( 1) /    7 /
c     data imach( 2) /    2 /
c     data imach( 3) /    2 /
c     data imach( 4) /    2 /
c     data imach( 5) /   36 /
c     data imach( 6) /    4 /
c     data imach( 7) /    2 /
c     data imach( 8) /   33 /
c     data imach( 9) / z1ffffffff /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -256 /
c     data imach(13) /  255 /
c     data imach(14) /   60 /
c     data imach(15) / -256 /
c     data imach(16) /  255 /
c
c     machine constants for the burroughs 5700 system.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  48 /
c     data imach( 6) /   6 /
c     data imach( 7) /   2 /
c     data imach( 8) /  39 /
c     data imach( 9) / o0007777777777777 /
c     data imach(10) /   8 /
c     data imach(11) /  13 /
c     data imach(12) / -50 /
c     data imach(13) /  76 /
c     data imach(14) /  26 /
c     data imach(15) / -50 /
c     data imach(16) /  76 /
c
c     machine constants for the burroughs 6700/7700 systems.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  48 /
c     data imach( 6) /   6 /
c     data imach( 7) /   2 /
c     data imach( 8) /  39 /
c     data imach( 9) / o0007777777777777 /
c     data imach(10) /   8 /
c     data imach(11) /  13 /
c     data imach(12) / -50 /
c     data imach(13) /  76 /
c     data imach(14) /  26 /
c     data imach(15) / -32754 /
c     data imach(16) /  32780 /
c
c     machine constants for the cdc 6000/7000 series.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    7 /
c     data imach( 4) /6loutput/
c     data imach( 5) /   60 /
c     data imach( 6) /   10 /
c     data imach( 7) /    2 /
c     data imach( 8) /   48 /
c     data imach( 9) / 00007777777777777777b /
c     data imach(10) /    2 /
c     data imach(11) /   47 /
c     data imach(12) / -929 /
c     data imach(13) / 1070 /
c     data imach(14) /   94 /
c     data imach(15) / -929 /
c     data imach(16) / 1069 /
c
c     machine constants for the cray 1
c
c     data imach( 1) /     1 /                                          cray1
c     data imach( 2) /     2 /                                          cray1
c     data imach( 3) /    99 /                                          cray1
c     data imach( 4) /    59 /                                          cray1
c     data imach( 5) /    64 /                                          cray1
c     data imach( 6) /     8 /                                          cray1
c     data imach( 7) /     2 /                                          cray1
c     data imach( 8) /    63 /                                          cray1
c     data imach( 9) /  777777777777777777777b /                        cray1
c     data imach(10) /     2 /                                          cray1
c     data imach(11) /    47 /                                          cray1
c     data imach(12) / -8189 /                                          cray1
c     data imach(13) /  8190 /                                          cray1
c     data imach(14) /    94 /                                          cray1
c     data imach(15) / -8099 /                                          cray1
c     data imach(16) /  8190 /                                          cray1
c
c     machine constants for the data general eclipse s/200
c
c     data imach( 1) /   11 /
c     data imach( 2) /   12 /
c     data imach( 3) /    8 /
c     data imach( 4) /   10 /
c     data imach( 5) /   16 /
c     data imach( 6) /    2 /
c     data imach( 7) /    2 /
c     data imach( 8) /   15 /
c     data imach( 9) /32767 /
c     data imach(10) /   16 /
c     data imach(11) /    6 /
c     data imach(12) /  -64 /
c     data imach(13) /   63 /
c     data imach(14) /   14 /
c     data imach(15) /  -64 /
c     data imach(16) /   63 /
c
c     machine constants for the harris 220
c
c     data imach( 1) /       5 /
c     data imach( 2) /       6 /
c     data imach( 3) /       0 /
c     data imach( 4) /       6 /
c     data imach( 5) /      24 /
c     data imach( 6) /       3 /
c     data imach( 7) /       2 /
c     data imach( 8) /      23 /
c     data imach( 9) / 8388607 /
c     data imach(10) /       2 /
c     data imach(11) /      23 /
c     data imach(12) /    -127 /
c     data imach(13) /     127 /
c     data imach(14) /      38 /
c     data imach(15) /    -127 /
c     data imach(16) /     127 /
c
c     machine constants for the honeywell 600/6000 series.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /   43 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    6 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / o377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   63 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
c     machine constants for the hp 2100
c     3 word double precision option with ftn4
c
c     data imach(1) /      5/
c     data imach(2) /      6 /
c     data imach(3) /      4 /
c     data imach(4) /      1 /
c     data imach(5) /     16 /
c     data imach(6) /      2 /
c     data imach(7) /      2 /
c     data imach(8) /     15 /
c     data imach(9) /  32767 /
c     data imach(10)/      2 /
c     data imach(11)/     23 /
c     data imach(12)/   -128 /
c     data imach(13)/    127 /
c     data imach(14)/     39 /
c     data imach(15)/   -128 /
c     data imach(16)/    127 /
c
c     machine constants for the hp 2100
c     4 word double precision option with ftn4
c
c     data imach(1) /      5 /
c     data imach(2) /      6 /
c     data imach(3) /      4 /
c     data imach(4) /      1 /
c     data imach(5) /     16 /
c     data imach(6) /      2 /
c     data imach(7) /      2 /
c     data imach(8) /     15 /
c     data imach(9) /  32767 /
c     data imach(10)/      2 /
c     data imach(11)/     23 /
c     data imach(12)/   -128 /
c     data imach(13)/    127 /
c     data imach(14)/     55 /
c     data imach(15)/   -128 /
c     data imach(16)/    127 /
c
c     machine constants for the ibm 360/370 series,
c     the xerox sigma 5/7/9, the sel systems 85/86, and
c     the perkin elmer (interdata) 7/32.
c
c     data imach( 1) /   5 /
c     data imach( 2) /   6 /
c     data imach( 3) /   7 /
c     data imach( 4) /   6 /
c     data imach( 5) /  32 /
c     data imach( 6) /   4 /
c     data imach( 7) /  16 /
c     data imach( 8) /  31 /
c     data imach( 9) / z7fffffff /
c     data imach(10) /  16 /
c     data imach(11) /   6 /
c     data imach(12) / -64 /
c     data imach(13) /  63 /
c     data imach(14) /  14 /
c     data imach(15) / -64 /
c     data imach(16) /  63 /
c
c     machine constants for the pdp-10 (ka processor).
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    5 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / '377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   54 /
c     data imach(15) / -101 /
c     data imach(16) /  127 /
c
c     machine constants for the pdp-10 (ki processor).
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    5 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / '377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   62 /
c     data imach(15) / -128 /
c     data imach(16) /  127 /
c
c     machine constants for pdp-11 fortran supporting
c     32-bit integer arithmetic.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   32 /
c     data imach( 6) /    4 /
c     data imach( 7) /    2 /
c     data imach( 8) /   31 /
c     data imach( 9) / 2147483647 /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   56 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
c     machine constants for pdp-11 fortran supporting
c     16-bit integer arithmetic.
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    5 /
c     data imach( 4) /    6 /
c     data imach( 5) /   16 /
c     data imach( 6) /    2 /
c     data imach( 7) /    2 /
c     data imach( 8) /   15 /
c     data imach( 9) / 32767 /
c     data imach(10) /    2 /
c     data imach(11) /   24 /
c     data imach(12) / -127 /
c     data imach(13) /  127 /
c     data imach(14) /   56 /
c     data imach(15) / -127 /
c     data imach(16) /  127 /
c
c     machine constants for the univac 1100 series. ftn compiler
c
c
c     data imach( 1) /    5 /
c     data imach( 2) /    6 /
c     data imach( 3) /    1 /
c     data imach( 4) /    6 /
c     data imach( 5) /   36 /
c     data imach( 6) /    4 /
c     data imach( 7) /    2 /
c     data imach( 8) /   35 /
c     data imach( 9) / o377777777777 /
c     data imach(10) /    2 /
c     data imach(11) /   27 /
c     data imach(12) / -128 /
c     data imach(13) /  127 /
c     data imach(14) /   60 /
c     data imach(15) /-1024 /
c     data imach(16) / 1023 /
c
c
c     machine constants for the vax 11/780
c
      data imach(1) /    5 /                                            vax75
      data imach(2) /    6 /                                            vax75
      data imach(3) /    5 /                                            vax75
      data imach(4) /    6 /                                            vax75
      data imach(5) /   32 /                                            vax75
      data imach(6) /    4 /                                            vax75
      data imach(7) /    2 /                                            vax75
      data imach(8) /   31 /                                            vax75
      data imach(9) /2147483647 /                                       vax75
      data imach(10)/    2 /                                            vax75
      data imach(11)/   24 /                                            vax75
      data imach(12)/ -127 /                                            vax75
      data imach(13)/  127 /                                            vax75
      data imach(14)/   56 /                                            vax75
      data imach(15)/ -127 /                                            vax75
      data imach(16)/  127 /                                            vax75
c
c     machine constants for the z80 microprocessor
c
c     data imach( 1) /     1/
c     data imach( 2) /     1/
c     data imach( 3) /     0/
c     data imach( 4) /     1/
c     data imach( 5) /    16/
c     data imach( 6) /     2/
c     data imach( 7) /     2/
c     data imach( 8) /    15/
c     data imach( 9) / 32767/
c     data imach(10) /     2/
c     data imach(11) /    24/
c     data imach(12) /  -127/
c     data imach(13) /   127/
c     data imach(14) /    56/
c     data imach(15) /  -127/
c     data imach(16) /   127/
c
c
c***first executable statement  i1mach
      if (i .lt. 1  .or.  i .gt. 16) go to 10
c
      i1mach=imach(i)
      return
c
   10 continue
      write(imach(4),9000)
9000  format('1error    1 in i1mach - i out of bounds ')
c
c     call fdump
c
c
      stop
      end
      subroutine fdump
c     implicit double precision (a-h,o-z)                                    dp
c***begin prologue  fdump
c***date written   790801   (yymmdd)
c***revision date  820801   (yymmdd)
c***category no.  z
c***keywords  error,xzeror package
c***author  jones, r. e., (snla)
c***purpose  symbolic dump (should be locally written)
c***description
c
c        ***note*** machine dependent routine
c        fdump is intended to be replaced by a locally written
c        version which produces a symbolic dump.  failing this,
c        it should be replaced by a version which prints the
c        subprogram nesting list.  note that this dump must be
c        printed on each of up to five files, as indicated by the
c        xzgtua routine.  see xsetua and xzgtua for details.
c
c     written by ron jones, with slatec common math library subcommittee
c     latest revision ---  23 may 1979
c***references  jones r.e., *slatec common mathematical library error
c                 handling package*, sand78-1189, sandia laboratories,
c                 1978.
c***routines called  (none)
c***end prologue  fdump
c
c***first executable statement  fdump
      return
      end
      subroutine sets12(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
c
      dimension cm(*)
      do 10 i=1,30
   10 prop(i)=cm(i)
      if (cm(7).eq.0.0) cm(7)=cm(6)
      if (cm(2).eq.0.0) cm(2)=1.e20
      pr=(3.*cm(7)-2.*cm(1))/(2.*(cm(1)+3.*cm(7)))
      ym=cm(1)*(2.+2.*pr)
      et=cm(3)
      cm(3)=0.0
      if (ym.ne.0.0) cm(3)=ym*et/(ym-et)
      cm(4)=4.*cm(1)/3.+cm(7)
      cm(8)=prop(4)
      cm(9)=prop(5)
      cm(10)=ym
      cm(11)=pr
c
      return
      end
      subroutine sets15 (cm)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk22/prop(48)
      dimension cm(48)
c
      if (cm(11).eq.0.0) cm(11)=-1.e30
      if (cm(12).eq.0.0) cm(12)=1.000000001
      do 10 i=1,13
   10 prop(i)=cm(i)
      cm(22)=abs(cm(17))+abs(cm(18))+abs(cm(19))+abs(cm(20))+abs(cm(21))
      do 20 i=17,43
   20 prop(i)=cm(i)
c
      return
      end
      subroutine sets35(cm)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk22/prop(48)
      dimension cm(*)
c
      prop(1)=cm(1)
      prop(2)=cm(9)
      prop(3)=cm(17)
      prop(4)=cm(25)
      prop(5)=cm(26)
      q1=cm(1)*cm(9)/((1.0+cm(9))*(1.0-2.0*cm(9)))
      q2=cm(1)*0.5/(1.0+cm(9))
      q3=q1+2.0*q2
      cm(8)=q3
      cm(16)=cm(1)*cm(25)/(cm(1)-cm(25))
c
      return
c
      end
      subroutine nodein(x,tcode,rcode,nif,nextra,iextra,idrvon)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and write nodal data
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk27/irestr,irdeck,idump,ncbrrf,jdump,istrnt
      logical large
      common/bigprb/large
      logical idrvon
c
      dimension x(3,1),tcode(1),rcode(1),xcr(8),ycr(8),zcr(8)
      character nif*5
      character*80 txts,mssg
c
      character nifmt*24
      data radian/.01745329252/,id/1/,i/0/,iadd/0/,ntsn/0/,nd/0/
      data xcr/0.,1.,1.,0.,0.,1.,1.,0./
      data ycr/0.,0.,1.,1.,0.,0.,1.,1./
      data zcr/0.,0.,0.,0.,1.,1.,1.,1./
c
      numnp=numnp-nextra
      mssg=' error reading nodal point input'
      nifmt='(i5,e5.0,3'//nif//',i5,e5.0)'
      if (large) then
      nifmt='(i8,e5.0,3'//nif//',e5.0)'
      endif
      if (idrvon) then
      do 5 i=1,8
      tcode(i)=0.
      x(1,i)=xcr(i)
      x(2,i)=ycr(i)
      x(3,i)=zcr(i)
    5 continue
      else
   10 continue
      call gttxsg(txts,lcount)
      if (.not.large) then
      read (unit=txts,fmt=nifmt,err=220)
     1 n,tcode(n),x(1,n),x(2,n),x(3,n),nd,rcd
      else
      read (unit=txts,fmt=nifmt,err=220)
     1 n,tcode(n),x(1,n),x(2,n),x(3,n),rcd
      endif
      if (ndof.eq.6) rcode(n)=rcd
      if (nd) 40,50,40
   40 id=nd
   50 if (i) 60,160,60
   60 nl=n-i
      if (nl-1) 160,150,70
   70 nl=nl/id
      if (i+nl*id-n) 200,80,200
   80 if (nl-1) 150,150,90
   90 anl=nl
c
c     generate in cartesian or cylindrical coordinates
c
      dx=(x(1,n)-x(1,i))/anl
      dy=(x(2,n)-x(2,i))/anl
      dz=(x(3,n)-x(3,i))/anl
      nl=n-2*id
      do 140 j=i,nl,id
      i1=j+id
      x(1,i1)=x(1,j)+dx
      x(2,i1)=x(2,j)+dy
      x(3,i1)=x(3,j)+dz
      if (tcode(i).eq.tcode(n)) tcode(i1)=tcode(n)
      if(ndof.eq.6.and.rcode(i).eq.rcode(n))
     1rcode(i1)=rcode(n)
  140 continue
  150 if (numnp-n) 210,170,160
  160 i=n
      go to 10
  170 j=0
      if (istrnt.ne.0) call lnkcrd(x(1,1))
      do 190 n=1,numnp,50
      j=j+50
      if (j.gt.numnp) j=numnp
      write(13,230)
      do 180 i=n,j
      if (tcode(i).lt.0) tcode(i)=-tcode(i)
      if (ndof.eq.6) rcd=rcode(i)
  180 write(13,240) i,tcode(i),x(1,i),x(2,i),x(3,i),rcd
  190 continue
      if (nextra.ne.0) then
      call bgnfim (iextra,nextra,x(1,numnp+1),tcode(numnp+1))
      numnp=numnp+nextra
      endif
      endif
c
      return
c
c
  200 write(13,250) i,nd,n
      call adios (2)
  210 write(13,260) n,numnp
      call adios (2)
c
c     error in formatted data
c
  220 call termin (txts,mssg,lcount,1)
c
  230 format('1 n o d a l  p o i n t  c o o r d i n a t e s '///
     1 8x,'np',4x,'type',9x,'x-ord',11x,'y-ord',11x,'z-ord'/)
  240 format(1x,i9,f8.1,3e15.4,f8.1)
  250 format('1incrementing from np',i8,' by',i3,',s will not reach',
     1 ' nodal point',i8)
  260 format('1n.p.',i8,' is greater than numnp=',i8)
      end
      subroutine elemnh(ib,ieost,mtype,numel,nsubg,head,lenaxh,
     1 nsubh,nhex,nphony,nhxpnt,lochvh,numnp,iwa,idrvon)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in and store element data
c
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(6),epx
      common/bk19/nconst(60),lenma,ncneos(15)
      common/nwixa/nwcon
      logical large,idrvon
      common/bigprb/large
c
      character nifmt*24
      character*80 txts,mssg
      dimension iy(20),head(12),ib(*),ieost(*),mtype(*),nhex(*),
     1 nphony(*),nhxpnt(*),lochvh(*),iwa(*)
      dimension iysv(9)
      data iysv/1,1,2,3,4,5,6,7,8/
      data mprint,i,nmel/0,0,128/
c
      nmel=nmel+1
      nifmt='(11i5)'
      lochvh(1)=1
      if (large) then
      nifmt='(i8,i5,8i8)'
      endif
      inc=1
      if (idrvon) then
      do 5 i=1,9
    5 iy(i)=iysv(i)
c     call packer (iy,ib(1),3)                                          cray1
      call packer(iy,ib(1),1,9)                                         vax75
      call wrtp(iy,9)
      else
   10 call gttxsg (txts,lcount)
      if (.not.large) then
      read (unit=txts,fmt=nifmt,err=138) m,mx,inc,(ix(j),j=1,8)
      else
      read (unit=txts,fmt=nifmt,err=138) m,mx,(ix(j),j=1,8)
      endif
      if (ix(1).eq.ix(2).or.ix(1).eq.ix(3).or.ix(1).eq.ix(4).
     & or.ix(2).eq.ix(3).or.ix(2).eq.ix(4).or.ix(3).eq.ix(4)) then
      write (  *,180) m
      write (13,180) m
      call adios(2)
      endif
      if (m.eq.0) m=i+1
      if (inc.eq.0) inc=1
   20 i=i+1
      if (m-i) 139,50,30
   30 do 40 j=2,9
   40 if (iy(j).ne.0) iy(j)=iy(j)+inc0
      go to 70
   50 iy(1)=mx
      do 60 j=1,8
   60 iy(j+1)=ix(j)
      inc0=inc
   70 loc=1+nwcon*(i-1)
c
c     pack connection data in 3 words
c
c     call packer (iy,ib(loc),3)                                        cray1
      call packer(iy,ib(loc),1,9)                                       vax75
      call wrtp(iy,9)
c
      if (mprint.gt.0) go to 80
      mprint=50
      write(13,150) (head(j),j=1,12)
   80 write(13,140) i,(iy(j),j=1,9)
      mprint=mprint-1
      if (m-i) 139,91,20
   91 if (numel-i) 92,92,10
      endif
   92 continue
      call sorthe(numel,ib,iortho,nhex,nphony,nwcon,iwa,
     1 numnp)
      do 250 i=1,numel
  250 nhxpnt(nhex(i))=i
      do 280 i=1,numel
      loc=1+nwcon*(i-1)
      call unpk(mx,ix,ib(loc),3)
      iy(1)=mx
      ix(9)=nphony(i)
      do 260 j=1,9
  260 iy(j+1)=ix(j)
      if (nmel.lt.128) go to 270
      nsubg=nsubg+2
      nsubh=nsubh+2
      mtyp=iy(1)
      mcon=iy(10)
      nmel=0
  270 nmel=nmel+1
      mtyo=mtyp
      mtyp=iy(1)
      mcoo=mcon
      mcon=iy(10)
      ncmt=1
      nces=1
      if (mtyo.ne.mtyp) nsubg=nsubg+1
      if (mcoo.ne.mcon) nsubh=nsubh+1
      if (mtyp.ne.0) ncmt=mtype(mtyp)
      if (mtyp.ne.0) nces=max0(1,ieost(mtyp))
      if (ncmt.ne.20.and.mtyp.ne.0)
     1lenaxh=lenaxh+7+nconst(ncmt)+ncneos(max0(nces,1))
      lochvh(i+1)=1+lenaxh
  280 continue
      return
c
c     terminate due to badly formatted data
c
  138 j=i+1
      write (unit=mssg,fmt=170) j
      call termin (txts,mssg,lcount,1)
c
  139 write(13,160) i
      call adios (2)
c
  140 format(i9,i7,2x,8i7)
  150 format('1'/' e i g h t  n o d e  b r i c k  e l e m e n t s'//
     1    1x,12a6//2x,'element material  node1  node2  node3',
     2       '  node4  node5  node6  node7  node8')
  160 format(' element card error ,n=',i7)
  170 format(' error reading element card for solid element ',i6)
  180 format(' error in solid element connectivity, element #=',i6,
     &      /' the first 4 nodes must be unique.'/)
      end
      subroutine sorthe(numel,ipsh,iortho,nhex,nphony,nwcon,iwa,
     1 numnp)
c     implicit double precision (a-h,o-z)                                    dp
      dimension
     1 ix(9),iy(9),iz(9),ibufy(9),ibufz(9),nhex(*),nphony(*),ipsh(*)
      dimension iwa(*)
c
      do 5 i =1,numel
    5 nhex(i)=i
      i   =1
      ifin=1
   10 if(i.gt.numel) go to 150
      locx=nwcon*(i-1)+1
      call unpk(mx,ix,ipsh(locx),3)
      i1=i+1
      do 90 j=i1,numel
      locy=nwcon*(j-1)+1
      call unpk(my,iy,ipsh(locy),3)
      if (my.ne.mx) go to 90
      ifin=ifin+1
      k   =ifin
      locz=nwcon*(k-1)+1
      if (locz.eq.locy) go to 90
      call unpk(mz,iz,ipsh(locz),3)
      ibufy(1)=my
      ibufz(1)=mz
      do 60 l=1,8
      ibufy(l+1)=iy(l)
   60 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipsh(locz),3)                                   cray1
c     call packer(ibufz,ipsh(locy),3)                                   cray1
      call packer(ibufy,ipsh(locz),1,9)                                 vax75
      call packer(ibufz,ipsh(locy),1,9)                                 vax75
      itmp    =nhex(j)
      nhex(j) =nhex(k)
      nhex(k) =itmp
   90 continue
      ifin=ifin+1
      i   =ifin
      go to 10
  150 iflag=0
      ifin =0
      i    =ifin+1
      ibeg =i
      ifin =i
  200 if (i.gt.numel) go to 300
      locx=nwcon*(i-1)+1
      do 210 k=1,numnp
  210 iwa(k)=0
      call unpk(mx,ix,ipsh(locx),3)
      iflag=iflag+1
      nphony(i)=iflag
      i1=i+1
      iwa(ix(1))=1
      iwa(ix(2))=1
      iwa(ix(3))=1
      iwa(ix(4))=1
      iwa(ix(5))=1
      iwa(ix(6))=1
      iwa(ix(7))=1
      iwa(ix(8))=1
      do 290 j=i1,numel
      locy=nwcon*(j-1)+1
      call unpk(my,iy,ipsh(locy),3)
      if (my.ne.mx) then
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
      endif
      j1=iwa(iy(1))
      j2=iwa(iy(2))
      j3=iwa(iy(3))
      j4=iwa(iy(4))
      j5=iwa(iy(5))
      j6=iwa(iy(6))
      j7=iwa(iy(7))
      j8=iwa(iy(8))
      if (max0(j1,j2,j3,j4,j5,j6,j7,j8).gt.0) go to 290
      iwa(iy(1))=1
      iwa(iy(2))=1
      iwa(iy(3))=1
      iwa(iy(4))=1
      iwa(iy(5))=1
      iwa(iy(6))=1
      iwa(iy(7))=1
      iwa(iy(8))=1
      ifin=ifin+1
      locz=nwcon*(ifin-1)+1
      call unpk(mz,iz,ipsh(locz),3)
      ibufy(1)=my
      ibufz(1)=mz
      do 260 l=1,8
      ibufy(l+1)=iy(l)
  260 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipsh(locz),3)                                   cray1
c     call packer(ibufz,ipsh(locy),3)                                   cray1
      call packer(ibufy,ipsh(locz),1,9)                                 vax75
      call packer(ibufz,ipsh(locy),1,9)                                 vax75
      nphony(ifin)=iflag
      k=ifin
      itmp    =nhex(j)
      nhex(j) =nhex(k)
      nhex(k) =itmp
  290 continue
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
  300 continue
      return
      end
      subroutine elemnb(numel,ipsb,thickb,mtype,csprop,lenaxb,nmintb,
     1 nsubb,ifmbm,xlngth,x,nsubc,nbms,nphony,nbmpnt,lochvb,numnp,iwa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     input beam elements
c
      common/bk19/nconst(60),lenma,ncneos(15)
      logical large
      common/bigprb/large
      character nifmt*24
      dimension intpts(6),ipsb(*),iy(6),ix(5),thickb(5,*),csprop(24,*),
     1 mtype(*),ifmbm(*),xlngth(*),x(3,*),nbms(*),nphony(*),nbmpnt(*),
     2 lochvb(*),iwa(*)
      character*80 txts,mssg
      data intpts/1,4,9,9,16,0/
      data nmel/128/
      data iy/6*0/
c
      nmel=nmel+1
      if(numel.eq.0) return
      lochvb(1)=1
      nprint=0
      i=0
      nifmt='(6i5,5e10.0)'
      if (large) then
      nifmt='(i8,i5,3i8,5e8.0)'
      endif
      inc=1
   10 call gttxsg (txts,lcount)
      if (.not.large) then
      read (unit=txts,fmt=nifmt,err=234)
     1 m,mx,inc,(ix(j),j=1,3),thic1,thic2,thic3,thic4,thic5
      else
      read (unit=txts,fmt=nifmt,err=234)
     1 m,mx,(ix(j),j=1,3),thic1,thic2,thic3,thic4,thic5
      endif
      if (m.eq.0) m=i+1
      xlngth(m)=sqrt((x(1,ix(1))-x(1,ix(2)))**2+(x(2,ix(1))-
     1x(2,ix(2)))**2+(x(3,ix(1))-x(3,ix(2)))**2)
      iop=ifmbm(mx)
      if(thic1.eq.0.0) thic1=csprop( 9,mx)
      if(thic2.eq.0.0) thic2=csprop(10,mx)
      if(thic3.eq.0.0) thic3=csprop(11,mx)
      if(thic4.eq.0.0) thic4=csprop(12,mx)
      if(thic5.eq.0.0) thic5=csprop(13,mx)
      thicks=.5*(thic1+thic2)
      thickt=.5*(thic3+thic4)
      if(inc.eq.0) inc=1
   20 i=i+1
      if (m-i) 230,50,30
   30 iy(2)=iy(2)+inc0
      iy(3)=iy(3)+inc0
      xlngth(i)=sqrt((x(1,iy(2))-x(1,iy(3)))**2+(x(2,iy(2))-
     1x(2,iy(3)))**2+(x(3,iy(2))-x(3,iy(3)))**2)
      go to 70
   50 iy(1)=mx
      do 60 j=1,3
   60 iy(j+1)=ix(j)
      inc0=inc
c  70 loc=1+2*(i-1)                                                     cray1
   70 loc=1+4*(i-1)                                                     vax75
c     call packer(iy,ipsb(loc),2)                                       cray1
      call packer(iy,ipsb(loc),1,4)                                     vax75
      call wrtp(iy,6)
      if(nprint.gt.0) go to 80
      nprint=50
      call header
      if (iop.eq.1) then
      write(13,290)(j,j=1,3)
      else
      write(13,310)(j,j=1,3)
      endif
   80 nprint=nprint-1
      if (iop.le.1) then
      thickb(1,i)=thicks
      thickb(2,i)=thicks
      thickb(3,i)=thickt
      thickb(4,i)=thickt
      else
      thickb(1,i)=thic1
      thickb(2,i)=thic2
      thickb(3,i)=thic3
      thickb(4,i)=thic4
      thickb(5,i)=thic5
      endif
      write(13,300) i,iy(1),(iy(j),j=2,4),thic1,thic2,thic3,thic4
      if (m-i) 230,220,20
  220 if (numel-i) 240,240,10
  230 write(13,350) i
      write( *,350) i
      call adios(2)
c
c     terminate due to badly formatted data
c
  234 j=i+1
      write (unit=mssg,fmt=360) j
      call termin (txts,mssg,lcount,1)
c
  240 continue
      call sortbe(numel,ipsb,thickb,xlngth,nbms,nphony,numnp,iwa)
      do 250 i=1,numel
  250 nbmpnt(nbms(i))=i
      nw=2
      do 280 i=1,numel
c     loc=2*(i-1)+1                                                     cray1
      loc=4*(i-1)+1                                                     vax75
      call unpk(mx,ix,ipsb(loc),nw)
      iy(1)=mx
      ix(5)=nphony(i)
      do 260 j=1,5
  260 iy(j+1)=ix(j)
      if(nmel.lt.128) go to 270
      nsubb=nsubb+2
      nsubc=nsubc+2
      mtyp=iy(1)
      mcon=iy(6)
      nmel=0
  270 nmel=nmel+1
      mtyo=mtyp
      mtyp=iy(1)
      mcoo=mcon
      mcon=iy(6)
      if (mtyo.ne.mtyp) nsubb=nsubb+1
      if (mcoo.ne.mcon) nsubc=nsubc+1
      iptt=csprop(2,iy(1))
      nmintb=nmintb+intpts(iptt)
      ncmt=1
      if (iy(1).ne.0) ncmt=mtype(iy(1))
      if (csprop(4,iy(1)).ge.0.0) then
      if (ncmt.ne.20.and.mtyp.ne.0)
     1lenaxb=lenaxb+intpts(iptt)*(nconst(ncmt)+7)
      else
      if (ncmt.ne.20.and.mtyp.ne.0)
     1lenaxb=lenaxb+iptt*(nconst(ncmt)+7)
      endif
      lochvb(i+1)=1+lenaxb
  280 continue
      return
  290 format(///' b e a m   e l e m e n t   i n f o r m a t i o n'/
     & /'    m      mtyp',3(3x,'node',i1),
     & '   thicks1      thicks2      thickt1      thickt2')
  300 format(1x,i6,i7,3(1x,i7),4(1x,e11.3,1x))
  310 format(///' b e a m   e l e m e n t   i n f o r m a t i o n'/
     & /'    m      mtyp',3(3x,'node',i1),
     & '     area         iyy          izz          ixx  ')
  350 format(/' element card error, beam/truss element #=',i5)
  360 format(' error reading element card for beam element',i6)
      end
      subroutine sortbe(numel,ipsb,thickb,xlngth,nbms,nphony,numnp,iwa)
c     implicit double precision (a-h,o-z)                                    dp
      dimension thickb(5,*),xlngth(*),ipsb(*),
     1 ix(9),iy(9),iz(9),ibufy(9),ibufz(9),nbms(*),nphony(*)
      dimension iwa(*)
c
      do 5 i =1,numel
    5 nbms(i)=i
      nw=2
c     ndim=2                                                            cray1
      ndim=4                                                            vax75
      i   =1
      ifin=1
   10 if(i.gt.numel) go to 150
      locx=ndim*(i-1)+1
      call unpk(mx,ix,ipsb(locx),nw)
      i1=i+1
      do 90 j=i1,numel
      locy=ndim*(j-1)+1
      call unpk(my,iy,ipsb(locy),nw)
      if (my.ne.mx) go to 90
      ifin=ifin+1
      k   =ifin
      locz=ndim*(k-1)+1
      if (locz.eq.locy) go to 90
      call unpk(mz,iz,ipsb(locz),nw)
      ibufy(1)=my
      ibufz(1)=mz
      do 60 l=1,4
      ibufy(l+1)=iy(l)
   60 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipsb(locz),2)                                   cray1
c     call packer(ibufz,ipsb(locy),2)                                   cray1
      call packer(ibufy,ipsb(locz),1,4)                                 vax75
      call packer(ibufz,ipsb(locy),1,4)                                 vax75
      itmp    =nbms(j)
      nbms(j) =nbms(k)
      nbms(k) =itmp
      do 70 l=1,5
      temp       =thickb(l,j)
      thickb(l,j)=thickb(l,k)
   70 thickb(l,k)=temp
      temp     =xlngth(j)
      xlngth(j)=xlngth(k)
      xlngth(k)=temp
   90 continue
      ifin=ifin+1
      i   =ifin
      go to 10
  150 iflag=0
      ifin =0
      i    =ifin+1
      ibeg =i
      ifin =i
  200 if (i.gt.numel) go to 300
      do 210 k=1,numnp
  210 iwa(k)=0
      locx=ndim*(i-1)+1
      call unpk(mx,ix,ipsb(locx),nw)
      iflag=iflag+1
      nphony(i)=iflag
      i1=i+1
      iwa(ix(1))=1
      iwa(ix(2))=1
      do 290 j=i1,numel
      locy=ndim*(j-1)+1
      call unpk(my,iy,ipsb(locy),nw)
      if (my.ne.mx) then
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
      endif
      j1=iwa(iy(1))
      j2=iwa(iy(2))
      if (max0(j1,j2).gt.0) go to 290
      iwa(iy(1))=1
      iwa(iy(2))=1
      ifin=ifin+1
      locz=ndim*(ifin-1)+1
      call unpk(mz,iz,ipsb(locz),nw)
      ibufy(1)=my
      ibufz(1)=mz
      do 260 l=1,4
      ibufy(l+1)=iy(l)
  260 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipsb(locz),2)                                   cray1
c     call packer(ibufz,ipsb(locy),2)                                   cray1
      call packer(ibufy,ipsb(locz),1,4)                                 vax75
      call packer(ibufz,ipsb(locy),1,4)                                 vax75
      nphony(ifin)=iflag
      k=ifin
      itmp    =nbms(j)
      nbms(j) =nbms(k)
      nbms(k) =itmp
      do 270 l=1,5
      temp       =thickb(l,j)
      thickb(l,j)=thickb(l,k)
  270 thickb(l,k)=temp
      temp     =xlngth(j)
      xlngth(j)=xlngth(k)
      xlngth(k)=temp
  290 continue
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
  300 continue
      return
      end
      subroutine elemns(numel,ipss,thicks,mtype,csprop,lenaxs,nmints,
     1 nsubr,angle,iortho,nsubs,nshl,nphony,nshpnt,lochvs,numnp,iwa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     input shell elements
c
      common/bk19/nconst(60),lenma,ncneos(15)
      logical large
      common/bigprb/large
      dimension ipss(*),iy(6),ix(5),thicks(9,*),mtype(*),lochvs(*),
     1 iwa(*),csprop(24,*),angle(*),nshl(*),nphony(*),nshpnt(*)
      character*24 nifmt,nifml1,nifml2
      character*80 txts,mssg
      data nprint,i/0,0/
      data nmel/128/
c
      nmel=nmel+1
      lochvs(1)=1
      nmints=0
      if(numel.eq.0) return
      nifmt='(7i5,4e10.0,e5.0)'
      nifml1='(i8,i5,4i8)'
      nifml2='(5e10.0)'
      inc=1
   10 call gttxsg (txts,lcount)
      if (.not.large) then
      read (unit=txts,fmt=nifmt,err=234)
     1 m,mx,inc,(ix(j),j=1,4),thic1,thic2,thic3,thic4,ang
      else
      read (unit=txts,fmt=nifml1,err=234)
     1 m,mx,(ix(j),j=1,4)
      call gttxsg (txts,lcount)
      read(unit=txts,fmt=nifml2,err=234)
     1     thic1,thic2,thic3,thic4,ang
      endif
      if (m.eq.0) m=i+1
      if(thic1.eq.0.0) thic1=csprop( 9,mx)
      if(thic2.eq.0.0) thic2=csprop(10,mx)
      if(thic3.eq.0.0) thic3=csprop(11,mx)
      if(thic4.eq.0.0) thic4=csprop(12,mx)
      if (iortho.eq.1) then
      angle(m)=ang/57.29577951
      endif
      if(inc.eq.0) inc=1
   20 i=i+1
      if (m-i) 230,50,30
   30 do 40 j=2,5
      iy(j)=iy(j)+inc0
   40 continue
      if (iortho.eq.1) angle(i)=ang/57.29577951
      go to 70
   50 iy(1)=mx
      do 60 j=1,4
   60 iy(j+1)=ix(j)
      inc0=inc
c  70 loc=1+2*(i-1)                                                     cray1
   70 loc=1+5*(i-1)                                                     vax75
c     call packer(iy,ipss(loc),2)                                       cray1
      call packer(iy,ipss(loc),1,5)                                     vax75
      call wrtp(iy,5)
      if(nprint.gt.0) go to 80
      nprint=50
      call header
      write(13,290)(j,j=1,4)
   80 nprint=nprint-1
      thicks(1,i)=thic1
      thicks(2,i)=thic2
      thicks(3,i)=thic3
      thicks(4,i)=thic4
      thicks(5,i)=0.0
      thicks(6,i)=0.0
      thicks(7,i)=0.0
      thicks(8,i)=0.0
      thicks(9,i)=0.0
      write(13,300) i,iy(1),(iy(j),j=2,5),(thicks(j,i),j=1,4),ang
      if (m-i) 230,220,20
  220 if (numel-i) 240,240,10
  230 nprint=0
      i=0
      go to 20
c
c     terminate due to badly formatted data
c
  234 j=i+1
      write (unit=mssg,fmt=360) j
      call termin (txts,mssg,lcount,1)
c
  240 continue
      call sortse(numel,ipss,thicks,angle,iortho,nshl,nphony,numnp,iwa)
      do 250 i=1,numel
  250 nshpnt(nshl(i))=i
      nw=2
      do 280 i=1,numel
c     loc=2*(i-1)+1                                                     cray1
      loc=5*(i-1)+1                                                     vax75
      call unpk(mx,ix,ipss(loc),nw)
      iy(1)=mx
      ix(5)=nphony(i)
      do 260 j=1,5
  260 iy(j+1)=ix(j)
      if(nmel.lt.128) go to 270
      nsubr=nsubr+2
      nsubs=nsubs+2
      mtyp=iy(1)
      mcon=iy(6)
      nmel=0
  270 nmel=nmel+1
      mtyo=mtyp
      mtyp=iy(1)
      mcoo=mcon
      mcon=iy(6)
      if (mtyo.ne.mtyp) nsubr=nsubr+1
      if (mcoo.ne.mcon) nsubs=nsubs+1
      iptt=csprop(2,iy(1))
      ncmt=1
      if (iy(1).ne.0) ncmt=mtype(iy(1))
      if (ncmt.ne.20.and.mtyp.ne.0)
     1lenaxs=lenaxs+iptt*(nconst(ncmt)+7)
      lochvs(i+1)=1+lenaxs
  280 continue
      return
  290 format(///' e l e m e n t   i n f o r m a t i o n'/
     & /'    m      mtyp',4(3x,'node',i1),
     & '    thick1       thick2       thick3       thick4     angle')
  300 format(1x,i6,i7,4(3x,i5),5(1x,e11.3,1x))
  350 format(/' element card error, n=',i5)
  360 format(' error reading element card for shell element',i6)
      end
      subroutine egcrys (ib,nsubgc,nfegc,nelg,numel,nphony,nwcon)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk14/mx,ix(20),sig(6),epx
      dimension ib(*),nsubgc(*),nfegc(*),nphony(*)
      lc1=1
      nmel  =128
      lcz   =0
      ic    =0
      nml   =128
      if (nelg.eq.0) return
      do 30 nn=1,nelg
      if (nn.eq.nelg) nmel=numel-128*(nelg-1)
      l11=lc1+nwcon*nml*(nn-1)
      call unpk(mx,ix,ib(l11),nwcon)
      ic=ic+1
      nsubg=1
      nsubgc(ic)=nsubg
      itypn=mx
      locmt=1+(l11-lc1)/nwcon
      iconn=nphony(locmt)
      lcz=lcz+1
      nfegc(lcz)=1
      if (nmel.eq.1) go to 20
      do 10 n=2,nmel
      l11=l11+nwcon
      call unpk(mx,ix,ib(l11),3)
      itypo=itypn
      itypn=mx
      locmt=1+(l11-lc1)/nwcon
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
      subroutine sortse(numel,ipss,thicks,angle,iortho,nshl,nphony,
     1 numnp,iwa)
c     implicit double precision (a-h,o-z)                                    dp
      dimension thicks(9,*),angle(*),ipss(*),
     1 ix(9),iy(9),iz(9),ibufy(9),ibufz(9),nshl(*),nphony(*)
      dimension iwa(*)
c
      do 5 i =1,numel
    5 nshl(i)=i
      nw=2
c     ndim=2                                                            cray1
      ndim=5                                                            vax75
      i   =1
      ifin=1
   10 if(i.gt.numel) go to 150
      locx=ndim*(i-1)+1
      call unpk(mx,ix,ipss(locx),nw)
      i1=i+1
      do 90 j=i1,numel
      locy=ndim*(j-1)+1
      call unpk(my,iy,ipss(locy),nw)
      if (my.ne.mx) go to 90
      ifin=ifin+1
      k   =ifin
      locz=ndim*(k-1)+1
      if (locz.eq.locy) go to 90
      call unpk(mz,iz,ipss(locz),nw)
      ibufy(1)=my
      ibufz(1)=mz
      do 60 l=1,4
      ibufy(l+1)=iy(l)
   60 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipss(locz),2)                                   cray1
c     call packer(ibufz,ipss(locy),2)                                   cray1
      call packer(ibufy,ipss(locz),1,5)                                 vax75
      call packer(ibufz,ipss(locy),1,5)                                 vax75
      if (iortho.eq.1) then
      temp    =angle(j)
      angle(j)=angle(k)
      angle(k)=temp
      endif
      itmp    =nshl(j)
      nshl(j) =nshl(k)
      nshl(k) =itmp
      do 70 l=1,4
      temp       =thicks(l,j)
      thicks(l,j)=thicks(l,k)
   70 thicks(l,k)=temp
   90 continue
      ifin=ifin+1
      i   =ifin
      go to 10
  150 iflag=0
      ifin =0
      i    =ifin+1
      ibeg =i
      ifin =i
  200 if (i.gt.numel) go to 300
      do 210 k=1,numnp
  210 iwa(k)=0
      locx=ndim*(i-1)+1
      call unpk(mx,ix,ipss(locx),nw)
      iflag=iflag+1
      nphony(i)=iflag
      i1=i+1
      iwa(ix(1))=1
      iwa(ix(2))=1
      iwa(ix(3))=1
      iwa(ix(4))=1
      do 290 j=i1,numel
      locy=ndim*(j-1)+1
      call unpk(my,iy,ipss(locy),nw)
      if (my.ne.mx) then
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
      endif
      j1=iwa(iy(1))
      j2=iwa(iy(2))
      j3=iwa(iy(3))
      j4=iwa(iy(4))
      if (max0(j1,j2,j3,j4).gt.0) go to 290
      iwa(iy(1))=1
      iwa(iy(2))=1
      iwa(iy(3))=1
      iwa(iy(4))=1
      ifin=ifin+1
      locz=ndim*(ifin-1)+1
      call unpk(mz,iz,ipss(locz),nw)
      ibufy(1)=my
      ibufz(1)=mz
      do 260 l=1,4
      ibufy(l+1)=iy(l)
  260 ibufz(l+1)=iz(l)
c     call packer(ibufy,ipss(locz),2)                                   cray1
c     call packer(ibufz,ipss(locy),2)                                   cray1
      call packer(ibufy,ipss(locz),1,5)                                 vax75
      call packer(ibufz,ipss(locy),1,5)                                 vax75
      nphony(ifin)=iflag
      k=ifin
      if (iortho.eq.1) then
      temp    =angle(j)
      angle(j)=angle(k)
      angle(k)=temp
      endif
      itmp    =nshl(j)
      nshl(j) =nshl(k)
      nshl(k) =itmp
      do 270 l=1,4
      temp       =thicks(l,j)
      thicks(l,j)=thicks(l,k)
  270 thicks(l,k)=temp
  290 continue
      i   =ifin+1
      ibeg=i
      ifin=i
      go to 200
  300 continue
      return
      end
      subroutine elemnt(ipst,ieost,mtype,numel,nsubg,csprop,head,lenaxt,
     1 nsubh,ntsl,nphony,nhxpnt,lochvh,numnp,iwa)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in and store element data
c
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk14/mx,ix(20),sig(6),epx
      common/bk19/nconst(60),lenma,ncneos(15)
      common/nwixa/nwcon
      logical large
      common/bigprb/large
      dimension iy(20),head(12),ipst(*),ieost(*),csprop(24,*),mtype(*),
     1 ntsl(*),nphony(*),nhxpnt(*),lochvh(*),iwa(*)
      character nifmt*24
      character*80 txts,mssg
      data mprint,i,nmel/0,0,128/
c
      nmel=nmel+1
      lochvh(1)=1
      nifmt='(11i5)'
      if (large) then
      nifmt='(i8,i5,8i8)'
      endif
      inc=1
   10 call gttxsg (txts,lcount)
      if (.not.large) then
      read (unit=txts,fmt=nifmt,err=138) m,mx,inc,(ix(j),j=1,8)
      else
      read (unit=txts,fmt=nifmt,err=138) m,mx,(ix(j),j=1,8)
      endif
      if (m.eq.0) m=i+1
      if (inc.eq.0) inc=1
   20 i=i+1
      if (m-i) 139,50,30
   30 do 40 j=2,9
   40 if (iy(j).ne.0) iy(j)=iy(j)+inc0
      go to 70
   50 iy(1)=mx
      do 60 j=1,8
   60 iy(j+1)=ix(j)
      inc0=inc
   70 loc=1+nwcon*(i-1)
c
c     pack connection data in 3 words
c
c     call packer (iy,ipst(loc),3)                                      cray1
      call packer(iy,ipst(loc),1,9)                                     vax75
      call wrtp(iy,9)
c
      if (mprint.gt.0) go to 80
      mprint=50
      write (13,150) (head(j),j=1,12)
   80 write (13,140) i,(iy(j),j=1,9)
      mprint=mprint-1
      if (m-i) 139,91,20
   91 if (numel-i) 92,92,10
   92 continue
      call sorthe(numel,ipst,iortho,ntsl,nphony,nwcon,iwa,
     1 numnp)
      do 250 i=1,numel
  250 nhxpnt(ntsl(i))=i
      do 280 i=1,numel
      loc=1+nwcon*(i-1)
      call unpk(mx,ix,ipst(loc),3)
      iy(1)=mx
      ix(9)=nphony(i)
      do 260 j=1,9
  260 iy(j+1)=ix(j)
      if (nmel.lt.128) go to 270
      nsubg=nsubg+2
      nsubh=nsubh+2
      mtyp=iy(1)
      mcon=iy(10)
      nmel=0
  270 nmel=nmel+1
      mtyo=mtyp
      mtyp=iy(1)
      mcoo=mcon
      mcon=iy(10)
      ncmt=1
      nces=1
      if (mtyo.ne.mtyp) nsubg=nsubg+1
      if (mcoo.ne.mcon) nsubh=nsubh+1
      if (mtyp.ne.0) ncmt=mtype(mtyp)
      if (mtyp.ne.0) nces=max0(1,ieost(mtyp))
      iptt=csprop(2,iy(1))
      if (ncmt.ne.20)
     1lenaxt=lenaxt+iptt*(7+nconst(ncmt)
     1      +ncneos(max0(nces,1)))
      lochvh(i+1)=1+lenaxt
  280 continue
      return
c
c     terminate due to badly formatted data
c
  138 j=i+1
      write (unit=mssg,fmt=170) j
      call termin (txts,mssg,lcount,1)
c
  139 write(13,160) i
      call adios (2)
c
  140 format(i8,i7,2x,8i7)
  150 format('1'/' e i g h t  n o d e  s h e l l  e l e m e n t s'//
     1       12a6//1x,'element material  node1  node2  node3',
     2       '  node4  node5  node6  node7  node8')
  160 format(' element card error ,n=',i5)
  170 format(' error reading element card for thick shell element',i6)
      end
      subroutine wrtp(iy,n)
c     implicit double precision (a-h,o-z)                                    dp
c
c     buffer connectivity data out to disk unpacked
c
      common/aux14/ic(9085),icount
      dimension iy(*),iyy(12)
c
c     n1,n2,n3,---,mat
c
      iyy(n)=iy(1)
      do 10 i=2,n
 10   iyy(i-1)=iy(i)
c     call blkcpy(iyy,ic(icount),n)                                     cray1
      call blkcpi(iyy,ic(icount),n)                                     vaxcs
      icount=icount+n
      if (icount.lt.9000) then
      return
      else
      call dumpic
      endif
      return
      end
      subroutine dumpic
c     implicit double precision (a-h,o-z)                                    dp
      real*8 head                                                       vax75
      common/bk03/ndthl,nsthl,nstsl,nstbl,nsttl,endtim,prtc,pltc,mkthf
      common/bk06/idmmy,iadd,ifil,maxsiz,head(12),ncycle,time(2,8)
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/aux14/ic(9085),icount
      common/thfl/madd,ifth,maddbg
      common/drelax/ladd,idrint
      if (icount.eq.1) return
      call wrabsg (iob5,ic,icount-1,iadd,0)
      call riosta (iob5)
      iadd=iadd+icount-1
      if (mkthf.ne.0) then
      call wrabsg (iob4,ic,icount-1,madd,0)
      call riosta (iob4)
      madd=madd+icount-1
      endif
      if (idrint.gt.0) then
      call wrabsg (iob6,ic,icount-1,ladd,0)
      call riosta (iob6)
      ladd=ladd+icount-1
      endif
      icount=1
      return
      end
      subroutine egcry(ib,mtype,nsubgv,mtnum,nfegp,nsbg,nelg,
     1 numel,nwcon)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize  cray - 1  arrays   (allows 7600 restart file
c     on cray and visa versa)
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
c     subroutine packer(ixc,ixp,ipw)                                    cray1
c     implicit double precision (a-h,o-z)                               cray1dp
      subroutine packer(ixc,ixp,j,k)                                    vax75
c     implicit double precision (a-h,o-z)                               vax75dp
c
c     pack element connection data with three integers per word
c
      dimension ixc(1),ixp(1)
      data izero/0/
c     data ibits1,ibits2/21,42/                                         cray1
c
      ixc(1)=ixc(1)+izero
c     ixp(1)=ixc(1)                                                     cray1
c     ixp(1)=or(shift(ixc(2),ibits1),ixp(1))                            cray1
c     ixp(1)=or(shift(ixc(3),ibits2),ixp(1))                            cray1
c     ixp(2)=ixc(4)                                                     cray1
c     ixp(2)=or(shift(ixc(5),ibits1),ixp(2))                            cray1
c     if (ipw.eq.2) return                                              cray1
c     ixp(2)=or(shift(ixc(6),ibits2),ixp(2))                            cray1
c     ixp(3)=ixc(7)                                                     cray1
c     ixp(3)=or(shift(ixc(8),ibits1),ixp(3))                            cray1
c     ixp(3)=or(shift(ixc(9),ibits2),ixp(3))                            cray1
      do 10 i=j,k                                                       vax75
   10 ixp(i)=ixc(i)                                                     vax75
c
      return
c
      end
      subroutine joyinf(maxs,nifn)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension nifn(1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      if (maxs.eq.0) return
c
      write(13,80)
   10 call gttxsg (txts,lcount)
      mssg =' error reading joy interface data'
      if(large) then
      read (unit=txts,fmt=91,err=75) n,nifnn
      else
      read (unit=txts,fmt=90,err=75) n,nifnn
      endif
      nifn(n)=nifnn
      nl=n-i
      nk=nl-1
      if (nk) 50,40,20
   20 nd=(nifn(n)-nifn(i))/nl
      nk=nk+i-1
      do 30 j=i,nk
      j1=j+1
   30 nifn(j1)=nifn(j)+nd
   40 if (maxs-n) 70,60,50
   50 i=n
      go to 10
   60 write(13,100) (j,nifn(j),j=1,maxs)
      return
c
   70 write(13,110)
      call adios (2)
   75 call termin (txts,mssg,lcount,1)
c
   80 format('1 j o y  i n t e f a c e    d e f i n i t i o n'//)
   90 format(2i5)
   91 format(2i8)
  100 format(11x,'interface nodes'/(15x,i4,'.',i5))
  110 format('error in specifying joy interface nodes')
      end
      subroutine spcin(nodspc,nspcor,tcode,itcode,atcode)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and write nodal data
c
      dimension tcode(1),itcode(1),atcode(9,1)
      dimension va(3),vb(3),vc(3),icode(6)
      common/bkspc/ jbase(7)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      if(nodspc.eq.0) go to 105
      write(13,1001)
      mssg=' error reading single point constraint data'
      i=0
  100 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=1004,err=210) n,ispcor,jcode,iend,inc
      else
      read (unit=txts,fmt=1002,err=210) n,ispcor,jcode,iend,inc
      endif
      if (ispcor.eq.0) ispcor=nspcor+1
c
c     we store spc number into tcode
c
      if (iend.eq.0) then
      iend=n
      inc =1
      endif
c
      ifst=n
      do 90 n=ifst,iend,inc
      i=i+1
      it = tcode(n)
      ii = i * 1000000.
      if(it.ge.0) it = it + ii
      if(it.lt.0) it = it - ii
      tcode(n) = it
c
c     pack and unpack ispcor and icode
c
      itcode(i)=ispcor*1000000+jcode
      call spcdec(itcode(i),icode,jspcor)
      write(13,1003) n,jspcor,icode
   90 continue
      if (i.ne.nodspc) then
      go to 100
      endif
c
105   if(nspcor.eq.0) go to 999
      write(13,2001)
      do 200 i = 1, nspcor
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=2002,err=210) n,va,vb
c
c     default x
c
      rx = va(1)*va(1) + va(2)*va(2) + va(3)*va(3)
      if(rx.gt.1.e-10) go to 110
      va(1)=1.
      va(2)=0.
      va(3)=0.
      rx = 1.
c
c     default y
c
110   ry = vb(1)*vb(1) + vb(2)*vb(2) + vb(3)*vb(3)
      if(ry.gt.1.e-10) go to 120
      vb(1)=0.
      vb(2)=1.
      vb(3)=0.
c
120   write(13,2003) n,va,vb
c
c     normalize x vector
c
      rx = sqrt(rx)
      atcode(1,n) = va(1)/rx
      atcode(2,n) = va(2)/rx
      atcode(3,n) = va(3)/rx
c
c     z vector = va x vb
c
130   vc(1) = va(2)*vb(3) - va(3)*vb(2)
      vc(2) = va(3)*vb(1) - va(1)*vb(3)
      vc(3) = va(1)*vb(2) - va(2)*vb(1)
      rz = vc(1)*vc(1) + vc(2)*vc(2) + vc(3)*vc(3)
      if(rz.lt.1.e-10) go to 901
      rz = sqrt(rz)
      atcode(7,n) = vc(1)/rz
      atcode(8,n) = vc(2)/rz
      atcode(9,n) = vc(3)/rz
c
c    y vector = z x x
c
      vb(1) = vc(2)*va(3) - vc(3)*va(2)
      vb(2) = vc(3)*va(1) - vc(1)*va(3)
      vb(3) = vc(1)*va(2) - vc(2)*va(1)
      ry = vb(1)*vb(1) + vb(2)*vb(2) + vb(3)*vb(3)
      if(ry.lt.1.e-10) go to 901
      ry = sqrt(ry)
      atcode(4,n) = vb(1)/ry
      atcode(5,n) = vb(2)/ry
      atcode(6,n) = vb(3)/ry
200   continue
      n=nspcor+1
      atcode(1,n)=1.
      atcode(2,n)=0.
      atcode(3,n)=0.
      atcode(4,n)=0.
      atcode(5,n)=1.
      atcode(6,n)=0.
      atcode(7,n)=0.
      atcode(8,n)=0.
      atcode(9,n)=1.
c
      go to 999
c
c     terminate due to badly formatted data
c
  210 call termin (txts,mssg,lcount,1)
c
1001  format('1 n o d a l  s p c '///
     1 8x,'node',6x,'spc coor. no.',4x,'boundary conditions')
1002  format(2i5,4x,i6,2i5)
1004  format(i8,i5,4x,i6,2i8)
1003  format(2x,2i10,10x,6i2)
2001  format('  s p c   d e f i n i t i o n'///
     1 6x,'no.',6x,'x-vector',31x,'y-vector'/)
2002  format(i5,6f10.3)
2003  format(3x,i5,4x,6g13.4)
c
901   write(13,991)
991   format(///' **** error in defining local coordinate system ***')
      stop
999   continue
      return
      end
      subroutine rbcc(bcs,numrc)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and write roller boundary condition cards
c
      dimension bcs(4,1)
      character*80 txts,mssg
c
      if (numrc.eq.0) return
c
      write(13,30)
      do 10 n=1,numrc
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=20,err=15) (bcs(i,n),i=1,4)
      write(13,40) n,(bcs(i,n),i=1,4)
      bl=sqrt(bcs(1,n)**2+bcs(2,n)**2+bcs(3,n)**2)
      bcs(1,n)=bcs(1,n)/bl
      bcs(2,n)=bcs(2,n)/bl
   10 bcs(3,n)=bcs(3,n)/bl
c
      return
c
   15 write(unit=mssg,fmt=50) n
      call termin (txts,mssg,lcount,1)
c
   20 format(5e10.0)
   30 format('1'//1x,'b o u n d a r y  c o n d i t i o n  c a r d s',/
     1        15x,1hi,14x,1hj,14x,1hk/)
   40 format(i5,4e14.6)
   50 format(' error reading boundary condition card',i4)
      end
      subroutine gtsgnd(nnfpln,countr,failvl,nsgnds,rd,x,numnp,nm)
c     implicit double precision (a-h,o-z)                                    dp
      common/tbsysp/numrcf,n6e,n6f,n6g
      dimension countr(*),failvl(*),rd(6,*),x(3,*)
      logical large
      common/bigprb/large
      character*80 txts,mssg
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=150,err=110) nsegs,(rd(j,nm),j=1,6),deflt
      write(13,120) nm,nsegs,(rd(j,nm),rd(j+3,nm),j=1,3)
      write (13,130)
      do 80 nn=1,nsegs
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=200,err=110) m,i1,i2,i3,i4,sfln
      inc = 0
      else
      read (unit=txts,fmt=160,err=110) m,i1,i2,i3,i4,sfln
      endif
      if (sfln.eq.0.0) sfln=deflt
      write (13,140) m,i1,i2,i3,i4,sfln
      fs1=-x(1,i1)+x(1,i2)+x(1,i3)-x(1,i4)
      fs2=-x(2,i1)+x(2,i2)+x(2,i3)-x(2,i4)
      fs3=-x(3,i1)+x(3,i2)+x(3,i3)-x(3,i4)
      ft1=-x(1,i1)-x(1,i2)+x(1,i3)+x(1,i4)
      ft2=-x(2,i1)-x(2,i2)+x(2,i3)+x(2,i4)
      ft3=-x(3,i1)-x(3,i2)+x(3,i3)+x(3,i4)
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=sfln*sqrt((e*g-f*f)/16.)
      countr(i1)=countr(i1)+1.00
      countr(i2)=countr(i2)+1.00
      countr(i3)=countr(i3)+1.00
      countr(i4)=countr(i4)+1.00
      failvl(i1)=failvl(i1)+area
      failvl(i2)=failvl(i2)+area
      failvl(i3)=failvl(i3)+area
      failvl(i4)=failvl(i4)+area
   80 continue
      nsgnds=0
      do 90 nn=1,numnp
      if (countr(nn).ne.0.0) then
      nsgnds=nsgnds+1
      failvl(nn)=failvl(nn)/countr(nn)
      endif
   90 continue
      nnfpln=nsgnds
      rx=rd(4,nm)-rd(1,nm)
      ry=rd(5,nm)-rd(2,nm)
      rz=rd(6,nm)-rd(3,nm)
      rl=sqrt(rx*rx+ry*ry+rz*rz)
      rd(4,nm)=rx/rl
      rd(5,nm)=ry/rl
      rd(6,nm)=rz/rl
      return
c
  110 mssg=' error reading symmetry failure plane interface segments'
      call termin (txts,mssg,lcount,1)
c
  120 format(
     1   /////5x,'failure surface #=',i5,
     2       /5x,'# of segments    =',i5,
     3      //5x,'coordinates of normal vector are'/31x,'i',14x,
     4       'j'/(22x,2e14.4))
  130 format(///'  s u r f a c e  s e g m e n t s'//
     1       /'  segment node1 node2 node3 node4')
  140 format(i7,4i6,'   failure stress=',e14.4)
  150 format(i5,7e10.0)
  160 format(5i5,e10.0)
  200 format(5i8,e10.0)
      end
      subroutine stsgnd(countr,failvl,nnfpln,numnp,nodes,fforce)
c     implicit double precision (a-h,o-z)                                    dp
      dimension countr(*),failvl(*),nodes(*),fforce(*)
      j=1
      do 10 i=1,numnp
      if (countr(i).ne.0.0) then
      nodes(j) =i
      fforce(j)=failvl(i)
      j=j+1
      endif
   10 continue
      return
      end
      subroutine blkprt (ndsout,nsout1,nsout2,nsout3,nsout4)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk04/prtout,pltout,dt2old,slsfac,tssfac,ihydro,
     1 ndth,nmst,nsth,nsts,nstb,nstt,ikedit
      logical large
      common/bigprb/large
      dimension ndsout(*),nsout1(*),nsout2(*),nsout3(*),nsout4(*)
      character*80 txts,mssg
      nbpc=8
      if(large) nbpc=5
c
c     read nodal print information
c
      if (ndth.eq.0) go to 30
      write (13,160)
      ndthb=(ndth-1)/nbpc+1
      lfst=1
      llst=nbpc
      mssg=' error reading nodal time history blocks'
      do 10 j=1,ndthb
      call gttxsg (txts,lcount)
      llst=min(llst,ndth)
      if (large) then
      read (unit=txts,fmt=150,err=130) (ndsout(2*i-1),ndsout(2*i),i=lfst
     1 ,llst)
      else
      read (unit=txts,fmt=140,err=130) (ndsout(2*i-1),ndsout(2*i),i=lfst
     1 ,llst)
      endif
      lfst=lfst+nbpc
      llst=llst+nbpc
   10 continue
      do 20 i=1,ndth
   20 write (13,180) i,ndsout(2*i-1),ndsout(2*i)
c
c     read element print information
c
   30 if (nmst.eq.0) go to 120
      write (13,170)
c
      if (nsth.eq.0) go to 60
c
      mssg=' error reading solid element time history blocks'
      nsthb=(nsth-1)/nbpc+1
      lfst=1
      llst=nbpc
      do 40 j=1,nsthb
      call gttxsg (txts,lcount)
      llst=min(llst,nsth)
      if (large) then
      read (unit=txts,fmt=150,err=130) (nsout1(2*i-1),nsout1(2*i),i=lfst
     1 ,llst)
      else
      read (unit=txts,fmt=140,err=130) (nsout1(2*i-1),nsout1(2*i),i=lfst
     1 ,llst)
      endif
      lfst=lfst+nbpc
      llst=llst+nbpc
   40 continue
      itbop=nsth
      do 50 i=1,nsth
      itbop=itbop+nsout1(2*i)-nsout1(2*i-1)
      if (nsout1(2*i-1).gt.nsout1(2*i)) then
      write (13,200) i,nsout1(2*i-1),nsout1(2*i)
      write ( *,200) i,nsout1(2*i-1),nsout1(2*i)
      call adios (2)
      endif
   50 write (13,190) i,nsout1(2*i-1),nsout1(2*i)
      if (itbop.gt.2000) then
      write (13,210) itbop
      write ( *,210) itbop
      call adios (2)
      endif
c
   60 if (nstb.eq.0) go to 90
c
      mssg=' error reading beam element time history blocks'
      nstbb=(nstb-1)/nbpc+1
      lfst=1
      llst=nbpc
      do 70 j=1,nstbb
      call gttxsg (txts,lcount)
      llst=min(llst,nstb)
      if (large) then
      read (unit=txts,fmt=150,err=130) (nsout2(2*i-1),nsout2(2*i),i=lfst
     1 ,llst)
      else
      read (unit=txts,fmt=140,err=130) (nsout2(2*i-1),nsout2(2*i),i=lfst
     1 ,llst)
      endif
      lfst=lfst+nbpc
      llst=llst+nbpc
   70 continue
      itbop=nstb
      do 80 i=1,nstb
      itbop=itbop+nsout2(2*i)-nsout2(2*i-1)
      if (nsout2(2*i-1).gt.nsout2(2*i)) then
      write (13,230) i,nsout2(2*i-1),nsout2(2*i)
      write ( *,230) i,nsout2(2*i-1),nsout2(2*i)
      call adios (2)
      endif
   80 write (13,220) i,nsout2(2*i-1),nsout2(2*i)
      if (itbop.gt.2000) then
      write (13,240) itbop
      write ( *,240) itbop
      call adios (2)
      endif
c
   90 if (nsts.eq.0) go to 120
c
      mssg=' error reading shell element time history blocks'
      nstsb=(nsts-1)/nbpc+1
      lfst=1
      llst=nbpc
      do 100 j=1,nstsb
      call gttxsg (txts,lcount)
      llst=min(llst,nsts)
      if (large) then
      read (unit=txts,fmt=150,err=130) (nsout3(2*i-1),nsout3(2*i),i=lfst
     1 ,llst)
      else
      read (unit=txts,fmt=140,err=130) (nsout3(2*i-1),nsout3(2*i),i=lfst
     1 ,llst)
      endif
      lfst=lfst+nbpc
      llst=llst+nbpc
  100 continue
      itbop=nsts
      do 110 i=1,nsts
      itbop=itbop+nsout3(2*i)-nsout3(2*i-1)
      if (nsout3(2*i-1).gt.nsout3(2*i)) then
      write (13,260) i,nsout3(2*i-1),nsout3(2*i)
      write ( *,260) i,nsout3(2*i-1),nsout3(2*i)
      call adios (2)
      endif
  110 write (13,250) i,nsout3(2*i-1),nsout3(2*i)
      if (itbop.gt.2000) then
      write (13,270) itbop
      write ( *,270) itbop
      call adios (2)
      endif
  120 return
c
c     error termination due to badly formatted data
c
  130 call termin (txts,mssg,lcount,1)
c
  140 format(16i5)
  150 format(10i8)
  160 format('1n o d a l   p o i n t   p r i n t o u t   b l o c k s')
  170 format('1e l e m e n t   p r i n t o u t   b l o c k s')
  180 format(//'  block ',i2,//
     1 4x,'first node in this block.......................',i7//
     2 4x,'last  node in this block.......................',i7/)
  190 format(//'  block ',i2,//
     1 4x,'first solid element in this block..............',i7//
     2 4x,'last  solid element in this block..............',i7/)
  200 format(///'**fatal error** in printout block',i3,' for solids',/,
     1 '   first element number in block',i6,' exceeds',/,
     2 '   last element number in block',i6,///)
  210 format(///'**fatal error** total number of elements in printout',
     1        /,'                blocks for solids =',i4,'exceeds the',
     2        /,'                maximum allowable of 2000',//////)
  220 format(//'  block ',i2,//
     1 4x,'first beam element in this block...............',i7//
     2 4x,'last  beam element in this block...............',i7/)
  230 format(///'**fatal error** in printout block',i3,' for beams',/,
     1 '   first element number in block',i6,' exceeds',/,
     2 '   last element number in block',i6,///)
  240 format(///'**fatal error** total number of elements in printout',
     1        /,'                blocks for beams  =',i4,'exceeds the',
     2        /,'                maximum allowable of 2000',//////)
  250 format(//'  block ',i2,//
     1 4x,'first shell element in this block..............',i7//
     2 4x,'last  shell element in this block..............',i7/)
  260 format(///'**fatal error** in printout block',i3,' for shells',/,
     1 '   first element number in block',i6,' exceeds',/,
     2 '   last element number in block',i6,///)
  270 format(///'**fatal error** total number of elements in printout',
     1        /,'                blocks for shells =',i4,'exceeds the',
     2        /,'                maximum allowable of 2000',//////)
      end
      subroutine rhodp(rdp)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in density vs. depth curve
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/ingstr/nmigs,mtigs(12)
      dimension rdp(2,1)
      character*80 txts,mssg
c
      call gttxsg (txts,lcount)
      mssg=' error reading density vs. depth control card'
      read (unit=txts,fmt=30,err=15)
     1 grvity,idirgv,nmigs,(mtigs(i),i=1,12)
      write(13,20) grvity,idirgv
      if (nmigs.eq.0) then
      write(13,22)
      else
      write(13,23)(mtigs(i),i=1,nmigs)
      endif
      write(13,25)numdp
c
      do 10 j=1,numdp
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=40,err=12) den,depth
      write(13,50) den,depth
c
      rdp(1,j)=den
      rdp(2,j)=depth
   10 continue
c
      return
c
   12 write (unit=mssg,fmt=60) j
   15 call termin (txts,mssg,lcount,1)
c
   20 format('1',///' g r a v i t y   i n f o r m a t i o n'//4x,
     1              ' gravitational constant =',e12.4//4x,
     2              ' direction              =',i5)
   22 format(//4x,  ' materials to initialize: all')
   23 format(//4x,  ' materials to initialize: ',12i5)
   25 format(//4x,  ' number of points       =',i5//4x,
     1              '   density         depth '/)
   30 format(e10.0,4x,i1,13i5)
   40 format(2e10.0)
   50 format(3x,f12.5,2x,e12.4)
   60 format(' error read density vs. depth curve card no.',i3)
      end
      subroutine brodei
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and print brode function data
c
      common/sbc0/yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits(3)
      common/sbc1/scal,shob,range(64),stoa(64),pkp(64)
c
c     data load default brode conversion factors
c        internal brode units are kft, ms, psi
c        default dyna units are m, s, pa
      dimension units(3)
      character*80 txts,mssg
c
      parameter (third=1./3.)
c     data bunits/304.8,.001,6.894757e+03/
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=100,err=90)
     1 yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=105,err=90) units
      if (units(1).ne.0.) bunits(1)=units(1)
      if (units(2).ne.0.) bunits(2)=units(2)
      if (units(3).ne.0.) bunits(3)=units(3)
      write(13,110) yield,hob,xb0,yb0,zb0,tb0,lctoa,lcyld,bunits
      hob=hob/bunits(1)
      if(yield.ne.0.) then
           scal=yield**(-third)
           shob=scal*hob
      else
           scal=1.
           shob=hob
      endif
c
      return
c
c     terminate due to badly formatted data
c
   90 mssg=' error reading brodei input data'
      call termin (txts,mssg,lcount,1)
c
  100 format(6e10.0,2i5)
  105 format(3e10.0)
  110 format(//' o v e r p r e s s u r e   c a l c u l a t i o n   '//
     1 4x,'yield..........................................',e11.4//
     2 4x,'hob............................................',e11.4//
     3 4x,'xb0............................................',e11.4//
     4 4x,'yb0............................................',e11.4//
     5 4x,'zb0............................................',e11.4//
     6 4x,'tb0............................................',e11.4//
     7 4x,'load curve specifying toa shift vs range.......',i7//
     8 4x,'load curve specifying yield scaling vs time....',i7//
     9 4x,'length units conversion factor.................',e11.4//
     * 4x,'time units conversion factor...................',e11.4//
     a 4x,'pressure units conversion factor...............',e11.4/)
      end
      subroutine getcsd(icsd,numcsd,nndtot,nbmtot,nshtot)
c     implicit double precision (a-h,o-z)                                    dp
      dimension icsd(3,*)
      character*80 txts,mssg
      write (13,40)
      nndtot=0
      nbmtot=0
      nshtot=0
      do 10 i=1,numcsd
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=60,err=20) (icsd(n,i),n=1,3)
      write (13,50) i,(icsd(n,i),n=1,3)
      nndtot=nndtot+icsd(1,i)
      nbmtot=nbmtot+icsd(2,i)
      nshtot=nshtot+icsd(3,i)
   10 continue
      return
   20 write (unit=mssg,fmt=70) i
      call termin (txts,mssg,lcount,1)
   40 format (////' c r o s s   s e c t i o n   d e f i n i t i o n s',
     1 //)
   50 format(////' d e f i n i t i o n  n o.',i5//
     &    '       no. of nodes.................................',i6/,
     &    '       no. of beam elements.........................',i6/,
     &    '       no. of shell elements........................',i6//)
   60 format(3i5)
   70 format(' error reading cross section control card no.',i3)
      end
      subroutine defcsd (icsd,mnmxb,mnmxs,nnnds,nbbms,nsshs,numcsd,
     1 nbmpnt,nshpnt)
c     implicit double precision (a-h,o-z)                                    dp
      dimension icsd(3,1),mnmxb(2,1),mnmxs(2,1),nnnds(1),nbbms(1),
     1 nsshs(1),nbmpnt(*),nshpnt(*)
      character*80 txts,mssg
      kn=0
      kb=0
      ks=0
      write (13,240)
      do 90 i=1,numcsd
      write (13,230) i
      write (13,200)
      nnds=icsd(1,i)
      nnbm=icsd(2,i)
      nnsh=icsd(3,i)
      n=(nnds-1)/8+1
      ifst=1
      ilst=8
      do 10 ii=1,n
      ilst=min0(ilst,nnds)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=160,err=120) (nnnds(kn+l),l=ifst,ilst)
      ifst=ifst+8
      ilst=ilst+8
   10 continue
      kn=kn+nnds
      k0=kn-nnds+1
      write (13,160)(nnnds(l),l=k0,kn)
      if (nnbm.ne.0) then
      n=(nnbm-1)/8+1
      ifst=1
      ilst=8
      write (13,210)
      do 20 ii=1,n
      ilst=min0(ilst,nnbm)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=160,err=130) (nbbms(kb+l),l=ifst,ilst)
      write (13,160)(nbbms(kb+l),l=ifst,ilst)
      do 15 l=ifst,ilst
      nbbms(kb+l)=nbmpnt(nbbms(kb+l))
   15 continue
      ifst=ifst+8
      ilst=ilst+8
   20 continue
      mnmxb(1,i)=nbbms(kb+1)
      mnmxb(2,i)=nbbms(kb+1)
      if (nnbm.gt.1) then
      do 30 l=2,nnbm
      mnmxb(1,i)=min0(mnmxb(1,i),nbbms(kb+l))
   30 mnmxb(2,i)=max0(mnmxb(2,i),nbbms(kb+l))
      endif
      kb=kb+nnbm
      endif
      if (nnsh.ne.0) then
      n=(nnsh-1)/8+1
      ifst=1
      ilst=8
      write (13,220)
      do 40 ii=1,n
      ilst=min0(ilst,nnsh)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=160,err=140) (nsshs(ks+l),l=ifst,ilst)
      write (13,160)(nsshs(ks+l),l=ifst,ilst)
      do 35 l=ifst,ilst
      nsshs(ks+l)=nshpnt(nsshs(ks+l))
   35 continue
      ifst=ifst+8
      ilst=ilst+8
   40 continue
      mnmxs(1,i)=nsshs(ks+1)
      mnmxs(2,i)=nsshs(ks+1)
      if (nnsh.gt.1) then
      do 50 l=2,nnsh
      mnmxs(1,i)=min0(mnmxs(1,i),nsshs(ks+l))
   50 mnmxs(2,i)=max0(mnmxs(2,i),nsshs(ks+l))
      endif
      ks=ks+nnsh
      endif
   90 continue
      return
  120 write (unit=mssg,fmt=170) i
      call termin (txts,mssg,lcount,1)
  130 write (unit=mssg,fmt=180) i
      call termin (txts,mssg,lcount,1)
  140 write (unit=mssg,fmt=190) i
      call termin (txts,mssg,lcount,1)
  160 format (8i10)
  170 format (' error reading cross section nodes from set #',i2)
  180 format (' error reading cross section beams from set #',i2)
  190 format (' error reading cross section shells from set#',i2)
  200 format (//' nodes defined for interface:')
  210 format (//' beam  elements defined for interface:')
  220 format (//' shell elements defined for interface:')
  230 format (////'  interface definition #',i3)
  240 format (////'  i n t e r f a c e   d e f i n i t i o n s'//)
      end
      subroutine flgnds (ixb,ixs,icsd,nnnds,nbbms,nsshs,nbm,nsh,numcsd)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk14/mx,ix(20),sig(6),epx
      dimension ixb(1),ixs(1),icsd(3,1),nbm(2,1),nsh(4,1),
     1 nnnds(1),nbbms(1),nsshs(1)
      kn=0
      kb=0
      ks=0
      do 100 i=1,numcsd
      nnds=icsd(1,i)
      nnbm=icsd(2,i)
      nnsh=icsd(3,i)
      k1=kn+1
      k2=kn+nnds
      if (nnbm.gt.0) then
      do 20 j=1,nnbm
      nb=nbbms(kb+j)
c     l11=1+2*(nb-1)                                                    cray1
      l11=1+4*(nb-1)                                                    vax75
      call unpk(mx,ix,ixb(l11),2)
      nbm(1,kb+j)=0
      nbm(2,kb+j)=0
      do 10 k=k1,k2
      if (ix(1).eq.nnnds(k)) then
      nbm(1,kb+j)=1
      go to 20
      endif
      if (ix(2).eq.nnnds(k)) then
      nbm(2,kb+j)=1
      go to 20
      endif
   10 continue
      write (13,130) j,i
      call adios(2)
   20 continue
      kb=kb+nnbm
      endif
      if (nnsh.gt.0) then
      do 50 j=1,nnsh
      ns=nsshs(ks+j)
c     l11=1+2*(ns-1)                                                    cray1
      l11=1+5*(ns-1)                                                    vax75
      call unpk(mx,ix,ixs(l11),2)
      iflag=0
      nsh(1,ks+j)=0
      nsh(2,ks+j)=0
      do 40 k=k1,k2
      if (ix(1).eq.nnnds(k)) then
      nsh(1,ks+j)=1
      iflag=iflag+1
      endif
      if (ix(2).eq.nnnds(k)) then
      nsh(2,ks+j)=1
      iflag=iflag+1
      endif
      if (ix(3).eq.nnnds(k)) then
      nsh(3,ks+j)=1
      iflag=iflag+1
      endif
      if (ix(4).eq.nnnds(k)) then
      nsh(4,ks+j)=1
      iflag=iflag+1
      endif
   40 continue
      if (iflag.eq.0) then
      write (13,140) j,i
      call adios(2)
      endif
   50 continue
      ks=ks+nnsh
      endif
      kn=kn+nnds
  100 continue
      return
  130 format(' beam #',i6,' does not contain a node of interface',i3,
     1/,'***********fatal error'//)
  140 format(' shell #',i6,' does not contain a node of interface',i3,
     1/,'***********fatal error'//)
      end
      subroutine timhis(npc,pld,nlcur,nptst,n9,ilcf)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in load time histories and determine storage
c
      common/sinit/idrflg,idvkx,idvkv,drtol,nrcyck,drfctr,tssfdr
      logical idrvon
      common/driver/ idrvon,idrvdm(10)
      dimension npc(1),pld(1),ilcf(1)
      character*80 txts,mssg
c
      if (nlcur.eq.0) return
c
      mp=0
      ilen=2000
      write(13,110) nlcur
      npc(1)=1
      do 50 l=1,nlcur
      write (unit=mssg,fmt=120) l
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=60,err=55) ll,npts,ilcf(l)
      if (ilcf(l).ne.0) idrflg=1
      npc(l+1)=npc(l)+2*npts
    5 if (npc(l+1).lt.ilen) go to 10
      ilen=ilen+2000
      call expndm (n9+ilen)
      go to 5
   10 if (l-ll) 20,30,20
   20 write(13,100)
      call adios (2)
   30 write(13,80) l,npts,ilcf(l)
      i=npc(l)
      do 40 j=1,npts
c
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=52) time,funct
      write(13,90) time,funct
c
      pld(i)=time
      i=i+1
      pld(i)=funct
   40 i=i+1
      mp=max(mp,npts)
   50 continue
c
      nptst=npc(nlcur+1)
      if (idrvon) idrvdm(3)=mp
c
      return
c
   52 write (unit=mssg,fmt=130) l,j
   55 call termin (txts,mssg,lcount,1)
c
   60 format(3i5)
   70 format(2f10.0)
   80 format(///4x,' load function number   =',i5/4x,
     1              ' number of time points  =',i5/4x,
     3              ' initialization flag    =',i5/4x,
     3              '    eq. 0 used for analysis only'/4x,
     3              '    eq. 1 used for initialization only'/4x,
     3              '    eq. 2 used for both'//4x,
     2              ' time value      function'/)
   90 format(3x,f12.5,2x,e12.4)
  100 format(' error   load curves out of order ')
  110 format('1  l o a d   c u r v e s  '//4x,
     1  ' number of load curves               =' ,i5 /)
  120 format(' error reading load curve control card no.',i3)
  130 format(' error read data for load curve definition',i3,' card no.'
     1 ,i3)
      end
      subroutine conlod(nod,idirn,ncur,fac,m,numcl)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and store concentrated load data
c
      dimension nod(1),idirn(1),ncur(1),fac(1),m(3,1)
      character*80 txts,mssg
      logical large
      common/bigprb/large
c
      if (numcl.eq.0) return
c
      write(13,30)
      do 10 i=1,numcl
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=21,err=15)
     1 nod(i),idirn(i),ncur(i),fac(i),m(1,i),m(2,i),m(3,i)
      else
      read (unit=txts,fmt=20,err=15)
     1 nod(i),idirn(i),ncur(i),fac(i),m(1,i),m(2,i),m(3,i)
      endif
      if (fac(i).eq.0.) fac(i)=1.0
      write(13,40)nod(i),idirn(i),ncur(i),fac(i),m(1,i),m(2,i),m(3,i)
   10 continue
c
      return
c
c     terminate if badly formatted data is encountered
c
   15 write (unit=mssg,fmt=50) i
      call termin (txts,mssg,lcount,1)
c
c
   20 format(3i5,e10.0,3i5)
   21 format(i8,2i5,e10.0,3i8)
   30 format('1',//' c o n c e n t r a t e d   l o a d s'//4x,
     1     ' node    direction    load curve   load curve multiplier',
     2      '   m1     m2     m3')
   40 format(3x,i6,4x,i7,9x,i5,8x,e12.4,4x,3i7)
   50 format(' error read concentrated nodal load card no.',i4)
      end
      subroutine loadp (ib,numpc)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and write pressure b.c. data
c
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk17/nodes(4),t,lc,pmult(4)
      logical large
      common/bigprb/large
c
      dimension iy(10),ib(*)
      character*80 txts,mssg
      data mprint,i/0,0/
c
      if (numpc.eq.0) return
c
      inc=1
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=141,err=135)
     1 lc,(iy(j),j=1,4),(pmult(j),j=1,4)
      m=0
      else
      read (unit=txts,fmt=140,err=135)
     1 m,lc,(iy(j),j=1,4),(pmult(j),j=1,4),t
      endif
      if (m.eq.0) m=i+1
      pavg=0.0
      do 20 j=1,4
      if (pmult(j).eq.0.0) pmult(j)=1.0
   20 pavg=pavg+.25*pmult(j)
      pmult(1)=pavg
      if (inc.eq.0) inc=1
   30 i=i+1
      if (m-i) 120,60,40
   40 do 50 j=1,4
   50 nodes(j)=nodes(j)+inc0
      go to 80
   60 do 70 j=1,4
   70 nodes(j)=iy(j)
      inc0=inc
   80 locc=1+7*(i-1)
      do 90 j=1,7
   90 ib(locc+j-1)=nodes(j)
      if (mprint.gt.0) go to 100
      mprint=45
      write(13,150)
  100 write(13,160) i,lc,t,(nodes(j),j=1,4),pmult(1)
      mprint=mprint-1
      if (m-i) 120,110,30
  110 if (numpc-i) 130,130,10
  120 write(13,170) i
      call adios (2)
  130 continue
c
      return
c
c     error termination due to badly formatted data
c
  135 m=i+1
      write (unit=mssg,fmt=180) m
      call termin (txts,mssg,lcount,1)
c
  140 format(6i5,5e10.0)
  141 format(i5,4i8,4e10.0)
  150 format('1',//,' p r e s s u r e  b. c.  c a r d s',//,2x,
     1 'card  load curve   start time   node 1 ',
     2 '   node 1    node 3    node 4  average pressure')
  160 format(i5,i9,3x,e12.3,4i10,e15.3)
  170 format('error on pressure card',i5)
  180 format (' error reading pressure card no.',i5)
      end
      subroutine velbci(tcode,lc,nvel,vx,vy,vz,iacflg)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in velocity cards
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      logical large
      common/bigprb/large
c
      dimension lc(1),nvel(1),vx(1),vy(1),vz(1),tcode(1),iacflg(1)
      character*80 txts,mssg
c
      if (numvc.eq.0) return
c
      write(13,30)
      do 10 k=1,numvc
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=21,err=15)
     1 n,lc(k),tcode(k),scale,vx(k),vy(k),vz(k),iacflg(k)
      else
      read (unit=txts,fmt=20,err=15)
     1 n,lc(k),tcode(k),scale,vx(k),vy(k),vz(k),iacflg(k)
      endif
      nvel(k)=n
      nabs=iabs(n)
      write(13,40) nabs,lc(k),tcode(k),scale,vx(k),vy(k),vz(k),iacflg(k)
      tck=tcode(k)
      if (tck.gt.4) tck=tck-4
      if (tck.gt.4) tck=tck-4
      if (tck.eq.1.0) vx(k)=1.0
      if (tck.eq.2.0) vy(k)=1.0
      if (tck.eq.3.0) vz(k)=1.0
      if (scale.eq.0.) scale=1.0
      scale=scale/sqrt(vx(k)**2+vy(k)**2+vz(k)**2)
      vx(k)=scale*vx(k)
      vy(k)=scale*vy(k)
      vz(k)=scale*vz(k)
      if (tcode(k).gt.8) go to 10
      if (tcode(k).le.4.or.numels+numelb.gt.0) go to 10
      write(13,1) k,tcode(k)
      call adios(2)
   10 continue
c
      return
c
   15 write (unit=mssg,fmt=50) k
      call termin (txts,mssg,lcount,1)
c
    1 format(1x/1x/' error(velbci) --- on vel. b. c. ',i5/
     1 ' dir.n ',i1,' specified with hexahedron element')
   20 format(2i5,f5.0,4e10.0,i5)
   21 format(i8,i5,f5.0,4e10.0,i5)
   30 format('1 v e l o c i t y   b. c.   c a r d s  '//,
     1 '    node',/,'       or ',/,' rigid body ',/,' material #',//,
     211x,'loadc   dir.',8x,'scale',9x,'vx',12x,'vy',14x,'vz')
   40 format(2i8,2x,f5.0,2x,4e14.6,' acceleration flag=',i1)
   50 format(' error reading velocity boundary condition card no.',i4)
      end
      subroutine rgidwi(rd,ilcw,nsw,numrw,ntsn,vmsw,istick)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in rigid wall data
c
      common/bk28/summss,xke,xpe,tt
      dimension rd(14,1),ilcw(1),nsw(1),vmsw(2,1),istick(1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      if (numrw.eq.0) return
c
      ik=0
      write(13,100)
      do 70 nm=1,numrw
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=91,err=85) maxs,(rd(j,nm),j=1,6),imswf,
     1 istick(nm),limit
      else
      read (unit=txts,fmt=90,err=85) maxs,(rd(j,nm),j=1,6),imswf,
     1 istick(nm),limit
      endif
      if (limit.eq.0) then
      rd(13,nm)=0.0
      rd(14,nm)=0.0
      else
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=150,err=85) (rd(j,nm),j=7,9),rd(13,nm),
     1 rd(14,nm)
      endif
      vmsw(1,nm)=0.0
      vmsw(2,nm)=0.0
      if (imswf.eq.1) then
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=150,err=85) vmsw(1,nm),vmsw(2,nm)
      summss=summss+vmsw(1,nm)
      endif
      ntsn=ntsn+maxs
      ilcw(nm)=maxs
      i=ik
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=121,err=85) n,nswn
      else
      read (unit=txts,fmt=120,err=85) n,nswn
      endif
      if (n.eq.0) n=i+1-ik
      n=n+ik
      nsw(n)=nswn
      nl=n-i
      nk=nl-1
      if (nk) 50,40,20
   20 nd=(nsw(n)-nsw(i))/nl
      nk=nk+i-1
      do 30 j=i,nk
      j1=j+1
   30 nsw(j1)=nsw(j)+nd
   40 if (maxs-n+ik) 80,60,50
   50 i=n
      go to 10
   60 write(13,110) nm,maxs,istick(nm),(rd(j,nm),rd(j+3,nm),j=1,3)
      if (limit.eq.1) then
      write (13,112) (rd(j,nm),rd(j+6,nm),j=1,3)
      write (13,114)  rd(13,nm),rd(14,nm)
      endif
      if (imswf.eq.1) then
      write(13,115) vmsw(1,nm),vmsw(2,nm)
      endif
      write(13,130) (j,nsw(j+ik),j=1,maxs)
      ik=ik+maxs
      rx=rd(4,nm)-rd(1,nm)
      ry=rd(5,nm)-rd(2,nm)
      rz=rd(6,nm)-rd(3,nm)
      rl=sqrt(rx*rx+ry*ry+rz*rz)
      rd(4,nm)=rx/rl
      rd(5,nm)=ry/rl
      rd(6,nm)=rz/rl
      if (limit.eq.1) then
      rx=rd(7,nm)-rd(1,nm)
      ry=rd(8,nm)-rd(2,nm)
      rz=rd(9,nm)-rd(3,nm)
      rl=sqrt(rx*rx+ry*ry+rz*rz)
      rd(7,nm)=rx/rl
      rd(8,nm)=ry/rl
      rd(9,nm)=rz/rl
      call crsprd (rd(4,nm),rd(7,nm),rd(10,nm))
      endif
   70 continue
c
      return
c
c
   80 write(13,140) nm
      call adios (2)
c
   85 write (unit=mssg,fmt=160) nm
      call termin (txts,mssg,lcount,1)
c
   90 format(i5,6e10.0,3i5)
   91 format(i8,6e10.0,2i5,i2)
  100 format('1  r i g i d  w a l l  d e f i n i t i o n s'//)
  110 format(/5x,'rigid wall number=',i5,
     1       /5x,'# of slave nodes =',i5,
     2       /5x,'stick condition  =',i5,
     3       /5x,'    eq.0: frictionless slideing after contact',
     4       /5x,'    eq.1: no sliding after contact           ',
     5      //15x,'coordinates of normal vector are'/31x,'i',14x,
     6       'j'/(22x,2e14.4))
  112 format(
     1      //15x,'coordinates of tangential vector are'/31x,'i',14x,
     2       'j'/(22x,2e14.4))
  114 format(15x,'x-extent of rigid wall=',e15.5,/,
     1       15x,'y-extent of rigid wall=',e15.5,/)
  115 format(15x,'mass of rigid wall    =',e15.5,/,
     1       15x,'velocity of rigid wall=',e15.5,/)
  120 format(2i5)
  121 format(2i8)
  130 format(15x,'slave nodes'/(15x,i4,'.',i5))
  140 format(' error on rigid wall definition ',i5)
  150 format(8e10.0)
  160 format (' error reading rigid wall data for rigid wall no.',i3)
      end
      subroutine crsprd(x,y,e)
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
      subroutine rigidi(ic,numtp,nodtie,numcc,ntcc,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension numtp(1),nodtie(1),ic(1),x(3,1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      if (numcc.eq.0) return
c
      write(13,30) numcc
      numtp(1)=1
      do 10 i=1,numcc
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=21,err=15) npts,ic(i)
      else
      read (unit=txts,fmt=20,err=15) npts,ic(i)
      endif
      numtp(i+1)=numtp(i)+npts
      j=numtp(i)
      k=numtp(i+1)-1
      if (large) then
      ndthb=(k-j)/10+1
      lfst=j
      llst=min0(k,j+9)
      else
      ndthb=(k-j)/16+1
      lfst=j
      llst=min0(k,j+15)
      endif
      do 5 m=1,ndthb
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=22,err=15) (nodtie(l),l=lfst,llst)
      lfst=lfst+10
      llst=min0(k,lfst+9)
      else
      read (unit=txts,fmt=20,err=15) (nodtie(l),l=lfst,llst)
      lfst=lfst+16
      llst=min0(k,lfst+15)
      endif
    5 continue
      write(13,40) i,ic(i),npts
      write(13,60) (nodtie(l),l=j,k)
      if (ic(i).eq.0) then
      nod1=nodtie(j)
      nod2=nodtie(k)
      dist2=(x(1,nod1)-x(1,nod2))**2+(x(2,nod1)-x(2,nod2))**2
     1     +(x(3,nod1)-x(3,nod2))**2
      if (dist2.lt.1.e-14) then
      write ( *,80) i
      write (13,80) i
      call adios(2)
      endif
      if (k-j.ne.1) then
      write ( *,90) i
      write (13,90) i
      call adios(2)
      endif
      endif
   10 continue
      ntcc=numtp(numcc+1)
c
      return
c
   15 write (unit=mssg,fmt=70) i
      call termin (txts,mssg,lcount,1)
c
   20 format(16i5)
   21 format(i8,2i5)
   22 format(10i8)
   30 format('1 r i g i d   c o n s t r a i n t s '//4x,
     1' number of sets                      =',i5 /)
   40 format(///4x,
     $' set number                                   =',i5//4x,
     $' tied degrees-of-freedom                      =',i5/4x,
     $'        eq.0: spotweld                  ',/4x,
     $'        eq.1: x displacement            ',/4x,
     $'        eq.2: y displacement            ',/4x,
     $'        eq.3: z displacement            ',/4x,
     $'        eq.4: x and y displacements     ',/4x,
     $'        eq.5: y and z displacements     ',/4x,
     $'        eq.6: z and x displacements     ',/4x,
     $'        eq.7: x, y, and z, displacements',//4x,
     $' number of tied points                        =',i5//4x,
     $' tied points: ')
   60 format(8i10)
   70 format(' error in reading nodal constraint card set no.',i5)
   80 format(' error in reading nodal constraint set number',i5,/,
     $ '   spotwelded nodes may not coincide'//)
   90 format(' error in reading nodal constraint set number',i5,/,
     $'    only 2 nodes may be spotwelded in a constraint set.',//)
      end
      subroutine initlv (x,v,initv,nextra)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read and write initial velocities
c
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      logical large
      common/bigprb/large
c
      dimension x(3,1),v(3,1)
      character*80 txts,mssg
c
      data       radian/.01745329252/
c
      vel=0.0
      i=0
      id=1
      numnp=numnp-nextra
      if (initv.eq.1) go to 20
      go to 200
   20 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=231,err=225) n,v(1,n),v(2,n),v(3,n),nd
      else
      read (unit=txts,fmt=230,err=225) n,v(1,n),v(2,n),v(3,n),nd
      endif
   40 vel=vel+abs(v(1,n))+abs(v(2,n))+abs(v(3,n))
      if (nd) 50,60,50
   50 id=nd
   60 if (i) 70,160,70
   70 nl=n-i
      if (nl-1) 160,150,80
   80 nl=nl/id
      if (i+nl*id-n) 220,90,220
   90 if (nl-1) 150,150,100
  100 anl=nl
c
c     generate in or cylindrical cartesian or cylindrical coordinates
c
      dx=(v(1,n)-v(1,i))/anl
      dy=(v(2,n)-v(2,i))/anl
      dz=(v(3,n)-v(3,i))/anl
  120 nl=n-2*id
      do 140 j=i,nl,id
      i1=j+id
      v(1,i1)=v(1,j)+dx
      v(2,i1)=v(2,j)+dy
  140 v(3,i1)=v(3,j)+dz
  150 if (numnp-n) 210,170,160
  160 i=n
      go to 20
  170 if (vel.ne.0.0) go to 180
      write(13,240)
      write(13,250)
      go to 200
  180 j=0
      do 190 n=1,numnp,50
      j=j+50
      write(13,240)
      if (j.gt.numnp) j=numnp
  190 write(13,260) (i,v(1,i),v(2,i),v(3,i),i=n,j)
c
  200 continue
      numnp=numnp+nextra
      return
c
  210 write(13,270) n,numnp
      call adios (2)
  220 write(13,280) i,nd,n
      call adios (2)
c
  225 i=i+1
      write (unit=mssg,fmt=290) i
      call termin (txts,mssg,lcount,1)
c
  230 format(i5,3e10.0,2i5)
  231 format(i8,3e10.0,2i5)
  240 format('1'//' n o d a l  p o i n t  v e l o c i t i e s'///6x,
     1 'np',11x,'x-vel',10x,'y-vel',10x,'z-vel'/)
  250 format(35x,'**all nodal point initial velocities are zero**')
  260 format(3x,i8,3e14.4)
  270 format('n.p.',i5,' is greater than numnp=',i5)
  280 format(' incrementing from n.p.',i5,' by',i3,'s will not reach ',
     1 'nodal point',i5)
  290 format(' error reading initial velocity cards, possibly card for n
     1ode',i6)
      end
      subroutine sllin(iparm,fric,nrttlm,nrttls,numsv,issca,it1sl,ifo,
     1 nsnws,nsnxs,cntrls)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      dimension iparm(8,1),fric(3,1),ifo(2,*),cntrls(numsv,*)
      logical large
      common/bigprb/large
      character*8 option
      character*80 txts,mssg
c
      write(13,20)
      do 10 i=1,numsv
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=50,err=15)
     1 nrts,nrtm,ntyp,f1,f2,f3,isrch,(ifo(j,i),j=1,2),cntrls(i,1),
     2 cntrls(i,2)
      else
      read (unit=txts,fmt=30,err=15)
     1 nrts,nrtm,ntyp,f1,f2,f3,isrch,(ifo(j,i),j=1,2),cntrls(i,1),
     2 cntrls(i,2)
      endif
      if (ntyp.eq.2.and.nrtm.eq.0) then
      nrtm=iextra
      nif7=i
      endif
      if (cntrls(i,1).eq.0.0) cntrls(i,1)=1.0
      if (cntrls(i,2).eq.0.0) cntrls(i,2)=1.0
      if (ntyp.ge.3) issca=1
      if (ntyp.eq.1) it1sl=4
      nrttls=nrttls+nrts
      nrttlm=nrttlm+nrtm
      iparm(1,i)=nrts
      iparm(2,i)=nrtm
      iparm(5,i)=ntyp
      iparm(8,i)=isrch
      fric(1,i)=f1
      fric(2,i)=f2
      fric(3,i)=f3
      if (ntyp.lt.5.or.ntyp.eq.9.or.ntyp.eq.10) then
      option='segments'
      else
      option='nodes...'
      iparm(1,i)=0
      iparm(3,i)=nrts
      nsnws=nsnws+5*nrts
      endif
      if (ntyp.eq.9) nsnxs=nsnxs+2*nrts
      write(13,40) i,ntyp
      write(13,60) option,nrts,
     1   nrtm,f1,f2,f3,isrch,(ifo(j,i),j=1,2),cntrls(i,1),cntrls(i,2)
      if (ntyp.eq.10) then
      cntrls(i,3)=1.00
      iparm(5,i)=3
      else
      cntrls(i,3)=0.00
      endif
   10 continue
c
      return
c
   15 write (unit=mssg,fmt=70) i
      call termin (txts,mssg,lcount,1)
c
   20 format('1',//' s l i d i n g  s u r f a c e s  &  v o i d s'// )
   30 format(3i5,3e10.0,3i5,2e10.0)
   40 format(////' d e f i n i t i o n  n o.',i5//
     &    '       slide line type..............................',i6//,
     &        12x,'type.eq.1   sliding                     '/,
     &        12x,'type.eq.2   tied sliding                '/,
     &        12x,'type.eq.3   sliding and voids           '/,
     &        12x,'type.eq.4   single surface              '/,
     &        12x,'type.eq.5   discrete nodes on surface   '/,
     &        12x,'type.eq.6   discrete nodes tied to surface   '/,
     &        12x,'type.eq.7   shell edge tied to shell surface '/,
     &        12x,'type.eq.8   spot welded nodes to surface     '/,
     &        12x,'type.eq.9   tie break interfaces             '/,
     &        12x,'type.eq.10  one-way sliding and voids        '/)
   50 format(2i8,i5,3e10.0,3i5,2e7.0)
   60 format(
     &    '       no. of slave ',a8,  '........................',i6/,
     &    '       no. of master segments.......................',i6/,
     &    '       static coefficient of friction ..............',e15.5/
     &    '       kinetic coefficient of friction..............',e15.5/
     &    '       exponential decay coefficient ...............',e15.5/
     &    '       small penetration in contact search .........',i6/
     &        12x,' = 0 default'/
     &        12x,' = 1 penetration.gt..1*thic of element ignored'//
     &    '       include slave side in printed interface file .',i6//,
     &    '       include master side in printed interface file ',i6//,
     &    '       scale factor on default slave stiffness .....',e15.5/,
     &    '       scale factor on default master stiffness ....',e15.5/)
   70 format(' error reading sliding interface control card no.',i3)
      end
      subroutine slavin(iparm,fric,irects,irectm,mnn,x,ifl,nslmat,ffifp,
     1 numsls,fails,sfail)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize slide & void logic
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/linkin/ilink,opifs,opnext,nsnifs,numifs,nif1,nif2,nif3,
     1 nif4,nif5,nif6,nif7,ilk16,nextra,iextra
      common/    /b(1000)
c
      dimension irects(4,1),irectm(4,1),mnn(*),iparm(*),x(3,1),
     1          fric(*),fails(5,1),sfail(1),ix(5)
      logical ffifp
c
      nrts=iparm(1)
      nrtm=iparm(2)
      nsn=iparm(3)
      nmn=iparm(4)
      nty=iparm(5)
      mprint=0
c
c     read in slave surface segments
c
      if (nty.lt.5.or.nty.eq.9) then
      call rdseg (irects,nrts,x,sfail,nty)
      iparm(1)=nrts
      else
      call sltyp5(nsn,mnn,fails,n,nty)
      endif
c
c     read in master surface segments
c
      if (nty.eq.2.and.n.eq.nif7) then
      call rdabsf (iob16,irectm,4*iextra,2,ioerr)
      call riosta (iob16)
      nmnp=numnp-nextra
      do 2 i=1,iextra
      irectm(1,i)=irectm(1,i)+nmnp
      irectm(2,i)=irectm(2,i)+nmnp
      irectm(3,i)=irectm(3,i)+nmnp
      irectm(4,i)=irectm(4,i)+nmnp
    2 continue
      ilk16=2+4*iextra+3*nextra
      call infmvi (b(nif5+1),b(nif5),x(1,nmnp+1))
      else
      call rdseg (irectm,nrtm,x,sfail,0)
      iparm(2)=nrtm
      endif
c
      if (nrts.gt.0) then
      write(13,30)
      write(13,40) (j,(irects(i,j),i=1,4),j=1,nrts)
      endif
      if (nrtm.gt.0) then
      write(13,50)
      write(13,40) (j,(irectm(i,j),i=1,4),j=1,nrtm)
      endif
c
      if (ffifp) then
      if (nrts.gt.0) then
      nslmat=nslmat+1
      do 3 i=1,nrts
      ix(1)=irects(1,i)
      ix(2)=irects(2,i)
      ix(3)=irects(3,i)
      ix(4)=irects(4,i)
      ix(5)=nslmat
    3 call wrtq(ix)
      endif
      if (nrtm.ne.0) then
      nslmat=nslmat+1
      do 4 i=1,nrtm
      ix(1)=irectm(1,i)
      ix(2)=irectm(2,i)
      ix(3)=irectm(3,i)
      ix(4)=irectm(4,i)
      ix(5)=nslmat
    4 call wrtq(ix)
      endif
      numsls=numsls+nrtm+nrts
      endif
c
c     count number of slave and master nodes
c
    5 nn=4*max0(nrts,nrtm)
      do 10 i=1,nn
   10 mnn(i)=0
      call countn (irects,mnn,nsn,nrts,b(lc10),numnp)
      do 20 i=1,nn
   20 mnn(i)=0
      call countn (irectm,mnn,nmn,nrtm,b(lc10),numnp)
c
      iparm(3)=nsn
      iparm(4)=nmn
c
      if (fric(1)**2+fric(2)**2+fric(3)**2.ne.0.) ifl=ifl+nsn+nmn
c
      return
c
   30 format(///' s l a v e  s u r f a c e  s e g m e n t s'//
     1       /'  segment node1 node2 node3 node4')
   40 format(i7,4i6)
   50 format(///' m a s t e r  s u r f a c e  s e g m e n t s'//
     1       /'  segment node1 node2 node3 node4')
      end
      subroutine sltyp5(nsn,nsv,fails,nm,nty)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read in a list of slave nodes
c
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/double/iprec,ncpw,unit
      dimension nsv(1),fails(5,1)
      character*80 txts,mssg
      save iadd
      logical large
      common/bigprb/large
      data iadd/0/
      ik=0
      il=0
      i=ik
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=171,err=155) n,nsvn,epsfln,epsfls,x,y
      else
      read (unit=txts,fmt=170,err=155) n,nsvn,epsfln,epsfls,x,y
      endif
      if (n.eq.0) n=i-ik+1
      n=n+ik
      nsv(n)=nsvn
      fails(1,n)=epsfln
      fails(2,n)=epsfls
      fails(3,n)=x
      fails(4,n)=y
      fails(5,n)=0.0
      nl=n-i
      nk=nl-1
      if (nk) 50,40,20
   20 ndif=nsv(n)-nsv(i)
      nd=ndif/nl
      if (nd*nl.ne.ndif) go to 140
      nk=nk+i-1
      do 30 j=i,nk
      j1=j+1
      nsv(j1)=nsv(j)+nd
      fails(1,j1)=fails(1,j)
      fails(2,j1)=fails(2,j)
      fails(3,j1)=fails(3,j)
      fails(4,j1)=fails(4,j)
      fails(5,j1)=fails(5,j)
   30 continue
   40 if (nsn-n+ik) 130,120,50
   50 i=n
      go to 10
  120 continue
      if (nty.gt.7) then
      write(13,180) nm,nsn
      write(13,230) (i,nsv(i),(fails(j,i),j=1,4),i=1,nsn)
      elseif (nty.eq.5) then
      write (13,181) nm, nsn
      write (13,240) (i,nsv(i),i=1,nsn)
      elseif (nty.eq.6) then
      write (13,182) nm, nsn
      write (13,240) (i,nsv(i),i=1,nsn)
      elseif (nty.eq.7) then
      write (13,183) nm, nsn
      write (13,240) (i,nsv(i),i=1,nsn)
      endif
      call wrabsf(iob8,nsv,nsn*iprec,iadd)
      call riosta(iob8)
      iadd=iadd+nsn*iprec
      return
  130 write(13,220) nm
      go to 150
  140 write(13,210) nm
  150 call adios (2)
c
  155 write (unit=mssg,fmt=250) nm
      call termin (txts,mssg,lcount,1)
c
  160 format(2e10.0)
  170 format(2i5,4e10.0)
  171 format(2i8,4e10.0)
  180 format(///
     1'1n o d e s   f o r   s p o t   w e l d e d   s u r f a c e',i5//
     19x,'number of slave nodes      = ',i5/)
  181 format(///
     1'1n o d e s    s l a v e   t o   m a s t e r   s u r f a c e',i5//
     19x,'number of slave nodes      = ',i5)
  182 format(///
     1'1n o d e s    t i e d   t o   m a s t e r   s u r f a c e',i5//
     19x,'number of slave nodes      = ',i5)
  183 format(///
     1'1n o d e s    t i e d   t o   s h e l l   s u r f a c e',i5//
     19x,'number of slave nodes      = ',i5)
  210 format(/' fatal input error on slide-line ',i5)
  220 format(/' error on slide-line  ',i5)
  230 format(/' slave nodes  '
     1  /(i10,'.',i7,'  failure constants=',4e14.4))
  240 format(/' slave nodes  '
     1  /(i10,'.',i7))
  250 format(' error reading in list of slave nodes for interface',i3)
      end
      subroutine wrtq(iy)
c     implicit double precision (a-h,o-z)                                    dp
      common/aux14/ic(9085),icount
      dimension iy(*)
c     call blkcpy(iy,ic(icount),5)                                      cray1
      call blkcpi(iy,ic(icount),5)                                      vaxcs
      icount=icount+5
      if (icount.lt.9000) then
      return
      else
      call dmpslc
      endif
      return
      end
      subroutine dmpslc
c     implicit double precision (a-h,o-z)                                    dp
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/aux14/ic(9085),icount
      common/slfl/nadd,numsls,ffifp
      if (icount.eq.1) return
      call wrabsg (iob9,ic,icount-1,nadd,0)
      call riosta (iob9)
      nadd=nadd+icount-1
      icount=1
      return
      end
      subroutine gtnrbs (irect,n,x)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read non-reflecting boundary segments
c
      dimension irect(4,*),x(3,*)
c
      call rdseg (irect,n,x,x,0)
      write(13,30)
      write(13,40) (j,(irect(i,j),i=1,4),j=1,n)
c
      return
c
   30 format(///' n o n - r e f l e c t i n g  s u r f a c e  s e g m e'
     &' n t s '///'  segment node1 node2 node3 node4')
   40 format(i7,4i8)
      end
      subroutine treadr(tmode,tbase,lct,numnp)
c     implicit double precision (a-h,o-z)                                    dp
      logical large
      common/bigprb/large
      dimension tmode(*),tbase(*),lct(*)
      character*80 txts,mssg
      nprint=0
   15 i=0
      ie=1
   20 call gttxsg (txts,lcount)
      if (large) then
      read(unit=txts,fmt=240,err=130) n,tmode(n),tbase(n),lct(n),nd
      else
      read(unit=txts,fmt=230,err=130) n,tmode(n),tbase(n),lct(n),nd
      endif
      if (nprint.gt.0) go to 25
      nprint=50
      write(13,210)
   25 nprint=nprint-1
      write(13,220) n,tmode(n),tbase(n),lct(n)
      if (nd) 30,40,30
   30 ie=nd
   40 if (i) 50,110,50
   50 nl=n-i
      if (nl-1) 110,100,60
   60 nl=nl/ie
      if (i+nl*ie-n) 170,70,170
   70 if (nl-1) 100,100,80
   80 anl=nl
      dtm=(tmode(n)-tmode(i))/anl
      dtb=(tbase(n)-tbase(i))/anl
      nl=n-2*ie
      do 90 j=i,nl,ie
      i1=j+ie
      tmode(i1)=tmode(j)+dtm
      tbase(i1)=tbase(j)+dtb
      lct(i1)=lct(n)
      if (nprint.gt.0) go to 85
      nprint=50
      write(13,210)
   85 nprint=nprint-1
      write(13,220) i1,tmode(i1),tbase(i1),lct(i1)
   90 continue
  100 if (numnp-n) 170,120,110
  110 i=n
      go to 20
  120 return
c
  130 mssg=' error reading termperature cards'
      call termin (txts,mssg,lcount,1)
c
  170 write(13,200)
      call adios(2)
  200 format(/' fatal error on initial temperature cards')
  210 format(///'1 t e m p e r a t u r e   d a t a'/1x,
     1          ' node',4x,' scaled temp. ',4x,'  base temp.  ',
     1         4x,'ld curve'/)
  220 format(1x,i5,4x,1pe14.7,4x,1pe14.7,5x,i5)
  230 format(i5,2e10.0,2i5)
  240 format(i8,2e10.0,2i8)
c
      end
      subroutine nodesm(nsv,msr,irects,irectm,iparm)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/    /b(1000)
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14                                                            ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      common/double/iprec,ncpw,unit
c
      dimension nsv(*),msr(*),irects(4,1),irectm(4,1),iparm(*)
      save iadd
      data iadd/0/
c
      nrts=iparm(1)
      nrtm=iparm(2)
      nsn=iparm(3)
      nmn=iparm(4)
      nty=iparm(5)
c
      if (nty.lt.5.or.nty.eq.9) then
      call countn (irects,nsv,k,nrts,b(lc10),numnp)
      else
      call rdabsf(iob8,nsv,nsn*iprec,iadd,ioerr)
      call riosta(iob8)
      iadd=iadd+nsn*iprec
      endif
      call countn (irectm,msr,k,nrtm,b(lc10),numnp)
      call sortnd (msr,nmn)
      call sortnd (nsv,nsn)
c
      return
      end
      subroutine sortnd(a,n)
c     implicit double precision (a-h,o-z)                                    dp
      integer a(1)
c
c***  supplied by don calahan
c
c   partition sorting algorithm
c reference collected algorithms of the acm - 63,64,65
c
      dimension ihigh(32), ilow(32)
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
c ixh is highest segment index (current)
   30 ixh = ih
c make a(ih) >= a(isepx)
      if (a(ih) .ge. isep) go to 50
      a(isepx) = a(ih)
      a(ih) = isep
      isep = a(isepx)
c make a(il) <= a(isepx)
      if (a(il) .le. isep) go to 50
      a(isepx) = a(il)
      a(il) = isep
      isep = a(isepx)
      go to 50
c exchange low part entry which is greater than separator with high
c part entry which is less than or equal to the separator value.
   40 it = a(ixh)
      a(ixh) = a(ixl)
      a(ixl) = it
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
      ixl = il
  120 a(ixl + 1) = a(ixl)
      ixl = ixl - 1
      if (isep .lt. a(ixl)) go to 120
      a(ixl + 1) = isep
      go to 100
      end
      subroutine nsgmnt(iparm,irects,irectm,nsv,msr,nsegs,nsegm,nstl,
     1                    nmtl,nsnxs)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      common/bk13/lc0,lc1h,lc1b,lc1s,lc1t,lc2,lc3,lc4,lc5,lc6,lc7,lc9,
     1   lc10,lc11,lc12,lc13,lc14,lc15,lc16,lc17,lc18,lb0,lb1,lb2,
     2   lc7a,lc7b
      common/bk20/numsv,ju,jv,nrtm,nrts,nmn,nsn,nty,nst,mst,noco,n
      common/    /b(1000)
c
      dimension iparm(*),irects(*),irectm(*),nsv(*),msr(*),nsegs(*),
     1          nsegm(*)
c
      nrts=iparm(1)
      nrtm=iparm(2)
      nsn=iparm(3)
      nmn=iparm(4)
      nty=iparm(5)
      nst=0
      mst=0
c
      do 10 i=1,nsn
   10 nsegs(i+1)=0
      nsegs(1)=1
      if (nrts.gt.0) then
c
      call counts (irects,nsv,nsegs(2),nrts,nsn,b(lc10))
c
      do 20 i=1,nsn
      nst=nst+nsegs(i+1)
      nstl=nstl+nsegs(i+1)
   20 nsegs(i+1)=nsegs(i)+nsegs(i+1)
      endif
      if (nty.eq.9) then
      nsnxs=nsnxs+nst
      endif
      iparm(6)=nst
c
      if(nmn.eq.0) return
c
      do 30 i=1,nmn
   30 nsegm(i+1)=0
      nsegm(1)=1
c
      call counts (irectm,msr,nsegm(2),nrtm,nmn,b(lc10))
c
      do 40 i=1,nmn
      mst=mst+nsegm(i+1)
      nmtl=nmtl+nsegm(i+1)
   40 nsegm(i+1)=nsegm(i)+nsegm(i+1)
c
      iparm(7)=mst
c
      return
      end
      subroutine counts(irect,mnn,nseg,nrt,nn,iscr)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension irect(4,1),mnn(1),nseg(1),iscr(1)
c
      do 10 i=1,nn
      iscr(mnn(i))=i
   10 continue
      do 20 i=1,nrt
      if (irect(1,i).ne.0) nseg(iscr(irect(1,i)))=
     1    nseg(iscr(irect(1,i)))+1
      if (irect(2,i).ne.0) nseg(iscr(irect(2,i)))=
     1    nseg(iscr(irect(2,i)))+1
      if (irect(3,i).ne.0) nseg(iscr(irect(3,i)))=
     1    nseg(iscr(irect(3,i)))+1
      if (irect(4,i).ne.0) nseg(iscr(irect(4,i)))=
     1    nseg(iscr(irect(4,i)))+1
   20 continue
      do 30 i=1,nn
      iscr(mnn(i))=0
   30 continue
c
      return
      end
      subroutine rdseg(irect,nrt,x,sfail,isv)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension sfail(2,1), irect(4,1), x(3,1), xl(4), yl(4), zl(4)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      if (nrt.eq.0) return
      if (isv.eq.9) write(13,220)
      i=0
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=200,err=155) m,i1,i2,i3,i4,sfln,sfls
      inc = 0
      else
      read (unit=txts,fmt=160,err=155) m,inc,i1,i2,i3,i4,sfln,sfls
      endif
      if (m.eq.0) m=i+1
      if (isv.eq.9) then
      sfail(1,m)=sfln
      sfail(2,m)=sfls
      write(13,210) m,sfln,sfls
      endif
      if (i1.ne.i2) go to 20
      i1=i3
      i3=i2
      i2=i4
      i4=i3
      go to 40
   20 if (i2.ne.i3) go to 30
      i2=i1
      i1=i4
      i4=i3
      go to 40
   30 if (i1.ne.i4) go to 40
      i1=i2
      i2=i3
      i3=i4
   40 irect(1,m)=i1
      irect(2,m)=i2
      irect(3,m)=i3
      irect(4,m)=i4
      if (inc.eq.0) inc=1
   50 i=i+1
      if (m-i) 110,80,60
   60 do 70 j=1,4
   70 irect(j,i)=irect(j,i-1)+inc0
      if (isv.eq.9) then
      sfail(1,i)=sfail(1,i-1)
      sfail(2,i)=sfail(2,i-1)
      endif
      go to 90
   80 inc0=inc
   90 if (m-i) 110,100,50
  100 if (nrt-i) 120,120,10
  110 write ( *,170) i
      call adios (2)
  120 i=0
      do 150 j=1,nrt
      do 130 k=1,4
      l=irect(k,j)
      xl(k)=x(1,l)
      yl(k)=x(2,l)
  130 zl(k)=x(3,l)
      call areack(xl,yl,zl,area)
      if (abs(area).lt.1.e-14) go to 140
      i=i+1
      irect(1,i)=irect(1,j)
      irect(2,i)=irect(2,j)
      irect(3,i)=irect(3,j)
      irect(4,i)=irect(4,j)
      if (isv.eq.9) then
      area=sqrt(area/16.00)
      sfail(1,i)=area*sfail(1,j)
      sfail(2,i)=area*sfail(2,j)
      endif
      go to 150
  140 write ( *,180) j,(irect(k,j),k=1,4)
      write(13,180) j,(irect(k,j),k=1,4)
  150 continue
      if (i.ne.nrt) write ( *,190) i,nrt
      if (i.ne.nrt) write(13,190) i,nrt
      nrt=i
      return
c
  155 mssg=' error reading sliding interface segments'
      call termin (txts,mssg,lcount,1)
c
  160 format(6i5,2e10.0)
  200 format(5i8,2e10.0)
  170 format(//' error in definition of segment card',i5)
  180 format('segment ',i5,' with nodes ',4i6,' has zero area and has be
     1en deleted')
  190 format ('number of segments is',i5,' reduced from',i5)
  220 format(/' tiebreak interface failure strengths ',
     1 /5x,'seg no.',5x,'normal failure stress    shear failure stress')
  210 format(5x,i5,10x,e14.4,10x,e14.4)
      end
      subroutine areack(x,y,z,area)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(*), y(*), z(*)
c
      fs1=-x(1)+x(2)+x(3)-x(4)
      fs2=-y(1)+y(2)+y(3)-y(4)
      fs3=-z(1)+z(2)+z(3)-z(4)
      ft1=-x(1)-x(2)+x(3)+x(4)
      ft2=-y(1)-y(2)+y(3)+y(4)
      ft3=-z(1)-z(2)+z(3)+z(4)
      e=fs1*fs1+fs2*fs2+fs3*fs3
      f=fs1*ft1+fs2*ft2+fs3*ft3
      g=ft1*ft1+ft2*ft2+ft3*ft3
      area=e*g-f*f
c
      return
      end
      subroutine countn(irect,mnn,n,nrt,iscr,numnp)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension irect(1),mnn(1),iscr(1)
c
c     initialize array of master nodes
c
      if (nrt.eq.0) return
      n=0
      nrt4=4*nrt
      do 10 i=1,numnp
      iscr(i)=0
   10 continue
      do 20 i=1,nrt4
      if (iscr(irect(i)).ne.0) go to 20
      n=n+1
      mnn(n)=irect(i)
      iscr(irect(i))=n
   20 continue
      do 30 i=1,n
      iscr(mnn(i))=0
   30 continue
c
      return
      end
      subroutine tbin1(nsn,nmn,locend,b,numels)
c     implicit double precision (a-h,o-z)                                    dp
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
      logical large
      common/bigprb/large
      dimension nsn(1),nmn(1),b(1)
      character*80 txts,mssg
      if (numels.eq.0) go to 100
      nods=0
      nodm=0
      mxla=numels
      do 10 i=1,ntbsl
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=3,err=110) nsn(i),nmn(i)
      else
      read(unit=txts,fmt=1,err=110) nsn(i),nmn(i)
      endif
      nods=nods+nsn(i)
      nodm=nodm+nmn(i)
      mxla=max0(mxla,nsn(i),nmn(i))
   10 continue
      ipa=locend
      ipb=ipa+nods
      ipc=ipb+nods+ntbsl
      ipd=ipc+nodm
      ipe=ipd+nods
      ipf=ipe+nods
      ipg=ipf+nods
      iph=ipg+nods
      ipi=iph+mxla
      ipk=ipi+mxla
      locend=ipk+nods
      call expndm(locend)
      ip1=ipa
      ip2=ipc
      ip3=ipg
      do 20 i=1,ntbsl
      call tbin2(nsn(i),nmn(i),b(ip2),b(ip1),b(ip3),i)
      ip1=ip1+nsn(i)
      ip2=ip2+nmn(i)
      ip3=ip3+nsn(i)
   20 continue
      return
  100 write(13,2)
      call adios(2)
      return
c
  110 write (unit=mssg,fmt=120) i
      call termin (txts,mssg,lcount,1)
c
  120 format(' error reading tie-breaking slideline control card',i4)
    1 format(2i5)
    3 format(2i8)
    2 format(1x///' error(tbin1) ---- no shell elements in data for'/
     1            ' tie-breaking slidelines.'///)
      end
      subroutine tbin2(nsn,nmn,msr,nsv,fails,nm)
c     implicit double precision (a-h,o-z)                                    dp
      dimension msr(1),nsv(1),fails(1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
      ik=0
      il=0
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=160,err=155) fleps
      i=ik
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=171,err=155) n,nsvn,epsfl
      else
      read (unit=txts,fmt=170,err=155) n,nsvn,epsfl
      endif
      if (n.eq.0) n=i-ik+1
      n=n+ik
      nsv(n)=nsvn
      fails(n)=epsfl
      nl=n-i
      nk=nl-1
      if (nk) 50,40,20
   20 ndif=nsv(n)-nsv(i)
      nd=ndif/nl
      if (nd*nl.ne.ndif) go to 140
      nk=nk+i-1
      do 30 j=i,nk
      j1=j+1
      nsv(j1)=nsv(j)+nd
      fails(j1)=fails(j)
   30 continue
   40 if (nsn-n+ik) 130,60,50
   50 i=n
      go to 10
   60 i=il
   70 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=171,err=155) n,msrn
      else
      read (unit=txts,fmt=170,err=155) n,msrn
      endif
      if (n.eq.0) n=i-il+1
      n=n+il
      msr(n)=msrn
      nl=n-i
      nk=nl-1
      if (nk) 110,100,80
   80 ndif=msr(n)-msr(i)
      nd=ndif/nl
      if (nd*nl.ne.ndif) go to 140
      nk=nk+i-1
      do 90 j=i,nk
      j1=j+1
      msr(j1)=msr(j)+nd
   90 continue
  100 if (nmn-n+il) 130,120,110
  110 i=n
      go to 70
  120 continue
      do 124 i=1,nsn
  124 if (fails(i).eq.0.0) fails(i)=fleps
      write(13,180) nm,nsn,nmn,fleps
      write(13,200) (i,msr(i),i=1,nmn)
      write(13,230) (i,nsv(i),fails(i),i=1,nsn)
      return
  130 write(13,220) nm
      go to 150
  140 write(13,210) nm
  150 call adios (2)
  155 mssg=' error reading tie breaking slideline data'
      call termin (txts,mssg,lcount,1)
  160 format(2e10.0)
  170 format(2i5,e10.0)
  171 format(2i8,e10.0)
  180 format(///'1t i e  b r e a k i n g  s l i d e  l i n e  ',i5//
     19x,'# of slave nodes  = ',i5/,9x,'# of master nodes = ',i5/,9x,
     3'failure strain    = ',e14.7)
  200 format(/' master nodes '/(i10,'.',i5))
  210 format(/' fatal input error on slide-line ',i5)
  220 format(/' error on slide-line  ',i5)
  230 format(/' slave nodes  '/(i10,'.',i5,'  failure strain=',e12.4))
      end
      subroutine rtnwf1(ntie,nl,locend,b,ixp,numels,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bttn/ntnwf,ixa(10)
      common/bktb/ntbsl,nods,nodm,ips,ipm,ipa,ipb,ipc,ipd,
     1            ipe,ipf,ipg,iph,ipi,ipj,ipk
c
      dimension ntie(*),nl(*),b(*),ixp(2,*),x(3,*)
      character*80 txts,mssg
c
c***  need shells
      if (numels.eq.0) go to 200
      nlen1=0
      nlen2=0
      maxl=0
      do 10 i=1,ntnwf
      call gttxsg (txts,lcount)
      read(unit=txts,fmt=1,err=210) ntie(i),nl(i)
      nlen1=nlen1+ntie(i)
      len=ntie(i)*nl(i)
      nlen2=nlen2+len
      maxl=max0(maxl,len)
   10 continue
c
c***  memory allocation
      ixa(3)=locend
      ixa(4)=ixa(3)+nlen1
      ixa(5)=ixa(4)+nlen2
      ixa(6)=ixa(5)+maxl
      ixa(7)=ixa(6)+nlen1+ntnwf
      ixa(8)=ixa(7)+numels
      ixa(9)=ixa(8)+numels
      locend=ixa(9)
      call expndm(locend)
      if (ntbsl.eq.0) then
      ipi=ixa(8)
      iph=ixa(7)
      endif
c
c***  read data and initialize
      i1=0
      i2=0
      do 100 i=1,ntnwf
      call rtnwf2(b(ixa(4)+i2),b(ixa(3)+i1),ntie(i),nl(i),i,x)
      call itnwf(b(ixa(4)+i2),b(ixa(6)+i1+i-1),ixp,locend,numels,
     1           ntie(i),nl(i),b(locend))
      i1=i1+ntie(i)
      i2=i2+ntie(i)*nl(i)
  100 continue
c
      return
c
  200 write(13,2)
      call adios(2)
      return
c
  210 write (unit=mssg,fmt=220) i
      call termin (txts,mssg,lcount,1)
c
  220 format(' error reading tie nodes with failure control card no',i3)
    1 format(2i5)
    2 format(1x//' error(rtnwf1) --- no shells for tied nodes ',
     1           'with failure')
c
      end
      subroutine rtnwf2(is,fail,nties,nl,ng,x)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension is(nties,1),fail(1),x(3,1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
c***  read in tied data
      call gttxsg (txts,lcount)
      mssg=' error reading default plastic failure strain'
      read(unit=txts,fmt=4,err=215) fails
      do 100 i=1,nties
      call gttxsg (txts,lcount)
      if(large) then
      nl12=min0(6,nl)
      read(unit=txts,fmt=230,err=210) f,(is(i,l),l=1,nl12)
      if (nl.gt.6) then
      nl12=(nl-7)/6+1
      lfst=7
      llst=16
      do 80 ii=1,nl12
      if (llst.gt.nl) llst=nl
      read(unit=txts,fmt=250,err=210) (is(i,l),l=lfst,llst)
      lfst=lfst+10
      llst=llst+10
   80 continue
      endif
      else
      nl12=min0(12,nl)
      read(unit=txts,fmt=220,err=210) f,(is(i,l),l=1,nl12)
      if (nl.gt.12) then
      nl12=(nl-13)/14+1
      lfst=13
      llst=26
      do 90 ii=1,nl12
      if (llst.gt.nl) llst=nl
      read(unit=txts,fmt=240,err=210) (is(i,l),l=lfst,llst)
      lfst=lfst+14
      llst=llst+14
   90 continue
      endif
      endif
      if (f.eq.0.) f=fails
      fail(i)=f
  100 continue
c
c***  echo data
      ip=0
      len=50
      len1=50
      nblk=nties/50
      irem=nties-50*nblk
      if (irem.eq.0) go to 110
      len1=irem
      nblk=nblk+1
  110 do 200 k=1,nblk
      if (k.eq.nblk) len=len1
      write(13,2) ng,fails
      do 120 i=1,len
      ipi=ip+i
      write(13,3) ipi,fail(ipi),(is(ipi,l),l=1,nl)
  120 continue
      ip=ip+len
  200 continue
c
      return
c
  210 write (unit=mssg,fmt=260) i
  215 call termin (txts,mssg,lcount,1)
c
  220 format(e10.0,12i5)
  230 format(e10.0,6x,6i8)
  240 format(14i5)
  250 format(10i8)
  260 format(' error reading tied nodes with failure set',i3)
    2 format(1x,/,'1t i e d   n o d e s   w i t h   f a i l u r e',
     1       '   b l o c k ',i5,/' default value of failure strain is'//
     2        ,1x,e14.7,/,8x,'failure'/' group   strain   nodes...')
    3 format(1x/i5,f10.6,10(1x,i5)/(17x,10(1x,i5)))
    4 format(e10.0)
c
      end
      subroutine itnwf(is,ip,ixp,locend,numels,nties,nl,ib)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/aux33/ix(128,4),mxt(128)
c
c     dimension is(nties,*),ip(*),ixp(2,*),ib(*)                        cray1
      dimension is(nties,*),ip(*),ixp(5,*),ib(*)                        vax75
c
c
c***  expand memory
      ipp=locend
      ipm1=ipp-1
      locend=locend+4*nties*nl
      call expndm(locend)
c
c***  find elements attached to node
      nelg=(numels-1)/128+1
      irem=numels-128*(nelg-1)
      ipnt=1
      if (irem.eq.0) irem=128
      do 200 i=1,nties
      ip(i)=ipnt
      do 180 l=1,nl
      inode=is(i,l)
c
c***  over each group
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
      if (ipm1+ipnt+1000.lt.locend) go to 100
      locend=locend+1000
      call expndm(locend)
  100 continue
c
  180 continue
c
  200 continue
c
c***  increase memory
      locend=ipp+ipnt-1
      call expndm(locend)
c
      ip(nties+1)=ipnt
c
      return
      end
      subroutine loadb
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk76/nthpx,nthpy,nthpz,nthsx,nthsy,nthsz
      common/bk77/gax,gay,gaz,slx,sly,slz
      character*80 txts,mssg
c
      if (nthpx+nthpy+nthpz+nthsx+nthsy+nthsz.gt.0) then
      write(13,60)
      endif
      if (nthpx.eq.0) go to 10
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthpx,gax
      if (gax.eq.0.0) gax=1.0
      write(13,75) nthpx,gax
   10 if (nthpy.eq.0) go to 20
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthpy,gay
      if (gay.eq.0.0) gay=1.0
      write(13,80) nthpy,gay
   20 if (nthpz.eq.0) go to 30
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthpz,gaz
      if (gaz.eq.0.0) gaz=1.0
      write(13,90) nthpz,gaz
   30 if (nthsx.eq.0) go to 40
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthsx,slx
      if (slx.eq.0.0) slx=1.0
      write(13,100) nthsx,slx
   40 if (nthsy.eq.0) go to 50
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthsy,sly
      if (sly.eq.0.0) sly=1.0
      write(13,110) nthsy,sly
   50 if (nthsz.eq.0) return
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=70,err=55) nthsz,slz
      if (slz.eq.0.0) slz=1.0
      write(13,120) nthsz,slz
c
      return
c
   55 write (unit=mssg,fmt=130)
      call termin (txts,mssg,lcount,1)
c
   60 format('1 b o d y  f o r c e  l o a d s '// )
   70 format(i5,e10.0)
   75 format(///' x - a c c e l e r a t i o n '//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
   80 format(///' y - a c c e l e r a t i o n '//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
   90 format(///' z - a c c e l e r a t i o n '//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
  100 format(///' x - a n g u l a r   v e l o c i t y'//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
  110 format(///' y - a n g u l a r   v e l o c i t y'//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
  120 format(///' z - a n g u l a r   v e l o c i t y'//5x,
     1       'load curve number=',i3,5x,'scale factor=',e10.3/)
  130 format(' error reading body force load cards.')
      end
      subroutine momdep(nelm,addm,tmadd,ib,nelmd,nhxpnt)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nelm(*),addm(3,*),ib(*),tmadd(*),nhxpnt(*)
      logical large
      common/bigprb/large
      common/nwixa/nwcon
      character*80 txts,mssg
      if (nelmd.eq.0) return
      write(13,20)
      do 10 i=1,nelmd
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=50,err=15) n,(addm(j,i),j=1,3),tmadd(i)
      else
      read (unit=txts,fmt=30,err=15) n,(addm(j,i),j=1,3),tmadd(i)
      endif
      locb=nwcon*(nhxpnt(n)-1)+1
      locdat=1+nwcon*(i-1)
c     call blkcpy (ib(locb),nelm(locdat),nwcon)                         cray1
      call blkcpi (ib(locb),nelm(locdat),nwcon)                         vaxcs
   10 write(13,40) n,(addm(j,i),j=1,3),tmadd(i)
      return
c
   15 write (unit=mssg,fmt=60) i
      call termin (txts,mssg,lcount,1)
c
   20 format('1 e l e m e n t s   s u b j e c t   t o    m o m',
     1 ' e n t u m   d e p o s i t i o n'//
     2 ' el. no.   x-mom.        y-mom.        z-mom.     '/)
   30 format(i5,4e10.0)
   40 format(i7,4e14.4)
   50 format(i8,4e10.0)
   60 format(' error reading momentum deposition data card no.',i3)
      end
      subroutine rdetpt(a,n)
c     implicit double precision (a-h,o-z)                                    dp
      dimension a(5,1)
      character*80 txts,mssg
      if (n.eq.0) return
      write(13,20)
      do 10 i=1,n
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=30,err=15) alt,mtl,xc,yc,zc
      write(13,40) i,alt,mtl,xc,yc,zc
      a(1,i)=alt
      a(2,i)=mtl
      a(3,i)=xc
      a(4,i)=yc
      a(5,i)=zc
   10 continue
      return
c
   15 write(unit=mssg,fmt=50) n
      call termin (txts,mssg,lcount,1)
c
   20 format('1 d e t o n a t i o n   p o i n t s'//)
   30 format(e10.0,i5,3e10.0)
   40 format(///'detonation point          =',i5,/5x,
     1          'lighting time             =',e14.4,/5x,
     2          'material to be lit        =',i5,/10x,
     3          'eq.0:all he material ',/5x,
     4          'x-coordinate              =',e14.4,/5x,
     5          'y-coordinate              =',e14.4,/5x,
     6          'z-coordinate              =',e14.4)
   50 format(' error reading detonation point',i5)
      end
      subroutine sbir(jne,nblk,ib,locend)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/nwixa/nwcon
      logical large
      common/bigprb/large
c
      dimension jne(3,1),b(1),ix(9)
      character*80 txts,mssg
c
      ipnt=1
      do 200 lo=1,nblk
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=1,err=210) jne(1,lo),jne(2,lo)
      jne(3,lo)=locend
      locend=locend+nwcon*jne(1,lo)
      call expndm(locend)
c
      len=jne(1,lo)
      nnodes=jne(2,lo)+1
c
      do 10 i=1,9
      ix(i)=0
   10 continue
c
      do 100 l=1,len
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=2,err=210) (ix(i),i=1,nnodes)
      else
      read (unit=txts,fmt=1,err=210) (ix(i),i=1,nnodes)
      endif
c     call packer(ix,b(ipnt),3)                                         cray1
      call packer(ix,b(ipnt),1,9)                                       vax75
      ipnt=ipnt+nwcon
  100 continue
c
  200 continue
c
      return
c
  210 write (unit=mssg,fmt=220) lo
      call termin (txts,mssg,lcount,1)
c
    1 format(10i5)
    2 format(10i8)
  220 format(' error read shell solid interface definition no.',i3)
c
      end
      subroutine setupn  (mtype,ixh,ixs,ixb,ixt,nrb,nrba,numnp,nrbn,
     1 mxrb,numelh,numels,numelb,numelt,nrbc,nmmat,nodtot,nodmax)
c     implicit double precision (a-h,o-z)                                    dp
c     dimension mtype(1),ixh(3,1),ixs(2,1),ixb(2,1),ixt(3,1),nrb(1),    cray1
      dimension mtype(1),ixh(9,1),ixs(5,1),ixb(4,1),ixt(9,1),nrb(1),    vax75
     1 nrba(1),nrbn(1),mxrb(1)
      character*80 txts,mssg
c
      do 10 i=1,nmmat
   10 mxrb(i)=i
      if (nrbc.eq.0) go to 30
      write(13,100)
      do 20 i=1,nrbc
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=110,err=90) m1,m2
      mxrb(m2)=mxrb(m1)
      if (mtype(m1).ne.20) go to 70
      if (mtype(m2).ne.20) go to 80
      write(13,120) m1,m2
   20 continue
   30 do 40 j=1,numnp
   40 nrb(j)=0
      nrba(1)=1
      do 60 i=1,nmmat
      irbn=i
      nrbn(i)=0
      lnrb=nrba(i)
      if (mtype(i).ne.20.or.mxrb(i).ne.i) go to 50
      call crbnod (ixh,nrb(lnrb),nrbn(i),irbn,mxrb,numelh,9,8)          vax75
      call crbnod (ixs,nrb(lnrb),nrbn(i),irbn,mxrb,numels,5,4)          vax75
      call crbnod (ixb,nrb(lnrb),nrbn(i),irbn,mxrb,numelb,4,2)          vax75
      call crbnod (ixt,nrb(lnrb),nrbn(i),irbn,mxrb,numelt,9,8)          vax75
c     call crbnod (ixh,nrb(lnrb),nrbn(i),irbn,mxrb,numelh,3,8)          cray1
c     call crbnod (ixs,nrb(lnrb),nrbn(i),irbn,mxrb,numels,2,4)          cray1
c     call crbnod (ixb,nrb(lnrb),nrbn(i),irbn,mxrb,numelb,2,2)          cray1
c     call crbnod (ixt,nrb(lnrb),nrbn(i),irbn,mxrb,numelt,3,8)          cray1
      nodmax=max0(nodmax,nrbn(i))
   50 nrba(i+1)=nrba(i)+nrbn(i)
   60 continue
      nodtot=nrba(nmmat+1)-1
c
      return
   70 write(13,130) i,m1
      call adios(2)
   80 write(13,130) i,m2
      call adios(2)
c
   90 write(unit=mssg,fmt=140) i
      call termin (txts,mssg,lcount,1)
c
  100 format(////'r i g i d   b o d y   m e r g e   c a r d s'/)
  110 format(2i5)
  120 format('     rigid body',i3,' is merged to rigid body',i3/)
  130 format(//'error on rigid body merge card number',i3,
     1/'     material',i3,' is not type 20-rigid')
  140 format(' error reading rigid body merge care no.',i3)
      end
      subroutine crbnod(ixp,nrb,n,mxe,mxrb,numel,nda,nbf)
c     implicit double precision (a-h,o-z)                                    dp
c
c     initialize array of rigid body nodes for material mxe
c
      common/bk14/mx,ix(20),sig(7)
      dimension ixp(nda,1),nrb(1),mxrb(1)
      if (numel.eq.0) return
      do 40 i=1,numel
      call unpk(mx,ix,ixp(1,i),nda)
      mx=mxrb(mx)
      if (mx.ne.mxe) go to 40
      if (n .ne.  0) go to 10
      nrb(1)=ix(1)
      n=1
   10 do 30 j=1,nbf
      do 20 k=1,n
   20 if (nrb(k).eq.ix(j)) go to 30
      n=n+1
      nrb(n)=ix(j)
   30 continue
   40 continue
c
      return
      end
      subroutine xtran(nxtra,lrbn,nrbn,nrb,nodtot,nodmax,mtype)
c     implicit double precision (a-h,o-z)                                    dp
c
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      logical large
      common/bigprb/large
c
      dimension lrbn(1),nrbn(1),nrb(1),mtype(1)
      character*80 txts,mssg
c
      nerr=0
      do 100 ixtra=1,nxtra
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=7,err=110) matl,num
      else
      read(unit=txts,fmt=1,err=110) matl,num
      endif
c
      if (matl.le.nmmat) go to 10
      write(13,2) matl,ixtra,nmmat
      call adios(2)
   10 if (mtype(matl).eq.20) go to 20
      write(13,3) matl,ixtra,mtype(matl)
      call adios(2)
c
   20 matlp1=matl+1
      nnm=0
      if (matlp1.gt.nmmat) go to 50
      jbgn=lrbn(matlp1)
      do 30 j=matlp1,nmmat
      nnm=nnm+nrbn(j)
      lrbn(j)=lrbn(j)+num
   30 continue
c
      jpnt=nodtot
      do 40 j=jbgn,nodtot
      nrb(jpnt+num)=nrb(jpnt)
      jpnt=jpnt-1
   40 continue
c
   50 jbgn=lrbn(matl)+nrbn(matl)-1
      numb=(num-1)/10+1
      lfst=1
      llst=10
      do 54 kkz=1,numb
      call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=8,err=110) (nrb(jbgn+j),j=lfst,llst)
      else
      read(unit=txts,fmt=4,err=110) (nrb(jbgn+j),j=lfst,llst)
      endif
      lfst=lfst+10
      llst=llst+10
   54 continue
      write(13,5) matl,(nrb(jbgn+j),j=1,num)
c
      do 60 j=1,num
      jpnt=jbgn+j
      if (nrb(jpnt).ge.1.and.nrb(jpnt).le.numnp) go to 60
      write(13,6) nrb(jpnt),matl,numnp
      nerr=nerr+1
   60 continue
c
      nrbn(matl)=nrbn(matl)+num
      nodmax=max0(nrbn(matl),nodmax)
      nodtot=nodtot+num
c
  100 continue
c
      if (nerr.ne.0) call adios(2)
c
      return
c
  110 write (unit=mssg,fmt=120) ixtra
      call termin (txts,mssg,lcount,1)
c
    1 format(2i5)
    7 format(i5,i8)
    2 format(1x/' error(nxtra) --- material ',i5,' specified on ',
     1       'rigid body card node card ',i3,/' is greater than ',
     2       ' the ',i3,' materials input')
    3 format(1x/' error(nxtra) --- material ',i5,' specified on ',
     1       'rigid body node card ',i3/' is type ',i3,' and not ',
     2       'type 20')
    4 format((10i5))
    8 format((10i8))
    5 format(1x/1x/' the  following nodes have been added to material',
     1       i3,':'/(10i5))
    6 format(1x/' error(nxtra) --- node ',i5,' added to material',
     1       i5,' is greater'/' than the ',i5,' nodes input')
c
  120 format(' error reading extra rigid body nodes--set no.',i3)
      end
      subroutine setrbc (mtype,code,x,nrbn,nrba,nrb,rbncdt,rbncdr,
     1 rbcods,nstore,mxrb,nodtot,ncflag,prop)
c     implicit double precision (a-h,o-z)                                    dp
      common/bk00/numnp,numpc,numlp,neq,ndof,nlcur,numcl,numvc,
     1  ndtpts,nelmd,nmmat,numelh,numelb,numels,numelt,numdp,
     2  grvity,idirgv,nodspc,nspcor
      dimension mtype(1),code(1),x(3,1),nrbn(1),nrba(1),nrb(1),
     1 rbncdt(1),rbncdr(1),rbcods(2,1),nstore(1),mxrb(1),prop(48,1)
      iprfmt=0
      do 10 i=1,nodtot
      rbncdr(i)=0.0
      if (numelb+numels.ne.0) then
      rbncdr(i)=code(nrb(i)+numnp)
      endif
   10 rbncdt(i)=code(nrb(i))
      do 20 i=1,nmmat
      rbcods(1,i)=0.
   20 rbcods(2,i)=0.
      do 30 n=1,nmmat
      if (mtype(n).ne.20.or.mxrb(n).ne.n) go to 30
      if (iprfmt.eq.0) then
      write (13,50)
      iprfmt=1
      endif
      if (prop(11,n).ne.1.0) then
      lnrb=nrba(n)
      call xcnstr(x,nrbn(n),nrb(lnrb),rbncdt(lnrb),rbcods(1,n),nstore,
     1 rbncdr(lnrb),numels+numelb)
      call ycnstr(x,nrbn(n),nrb(lnrb),rbncdt(lnrb),rbcods(1,n),nstore,
     1 rbncdr(lnrb),numels+numelb)
      call zcnstr(x,nrbn(n),nrb(lnrb),rbncdt(lnrb),rbcods(1,n),nstore,
     1 rbncdr(lnrb),numels+numelb)
      else
      rbcods(1,n)=prop(16,n)
      rbcods(2,n)=prop(21,n)
      endif
      write (13,60) n,rbcods(1,n),rbcods(2,n)
   30 continue
      do 40 i=1,nodtot
   40 if (rbncdt(i).ne.0.0) ncflag=ncflag+1
      return
   50 format(////' r i g i d   b o d y   c o n s t r a i n t s'/)
   60 format(    'rigid body # ',i2,/
     1           '     translational constraint = ',e9.2,/
     2           '     rotational    constraint = ',e9.2,////)
      end
      subroutine xcnstr(x,n,nrb,rbncdt,rbcods,nstore,rbncdr,iflag)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,1),nrb(*),rbncdt(*),rbncdr(*),rbcods(*),nstore(*)
      j=0
      do 10 i=1,n
      if (rbncdt(i).eq.1..or.rbncdt(i).eq.4..or.rbncdt(i).eq.6.
     1.or.rbncdt(i).eq.7.) then
      j=j+1
      nstore(j)=nrb(i)
      if (iflag.ne.0) then
      if (rbncdr(i).eq.5.0.or.rbncdr(i).eq.7.0) go to 40
      endif
      endif
   10 continue
      if (j.lt.3) return
      k=2
   20 yv=x(2,nstore(1))-x(2,nstore(k))
      zv=x(3,nstore(1))-x(3,nstore(k))
      xv=x(1,nstore(1))-x(1,nstore(k))
      xl=sqrt(yv**2+zv**2)
      if (xl.lt.1.e-20) then
      k=k+1
      if (abs(xv).gt.1.e-05) go to 40
      if (k.eq.j) return
      go to 20
      endif
      yv=yv/xl
      zv=zv/xl
      k=k+1
      do 30 i=k,j
      yu=x(2,nstore(1))-x(2,nstore(i))
      zu=x(3,nstore(1))-x(3,nstore(i))
      xl=sqrt(yu**2+zu**2)
      if (xl.lt.1.e-20) go to 30
      if (abs((yv*yu+zv*zu)/xl).lt.0.999) go to 40
   30 continue
      return
   40 rbcods(1)=1.
      rbcods(2)=5.
      do 50 i=1,n
      if (rbncdt(i).eq.1.0) rbncdt(i)=0.0
      if (rbncdt(i).eq.4.0) rbncdt(i)=2.0
      if (rbncdt(i).eq.6.0) rbncdt(i)=3.0
      if (rbncdt(i).eq.7.0) rbncdt(i)=5.0
   50 continue
      return
      end
      subroutine ycnstr(x,n,nrb,rbncdt,rbcods,nstore,rbncdr,iflag)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,1),nrb(*),rbncdt(*),rbncdr(*),rbcods(*),nstore(*)
      j=0
      do 10 i=1,n
      if (rbncdt(i).eq.2..or.rbncdt(i).eq.4..or.rbncdt(i).eq.5.
     1.or.rbncdt(i).eq.7.) then
      j=j+1
      nstore(j)=nrb(i)
      if (iflag.ne.0) then
      if (rbncdr(i).eq.6.0.or.rbncdr(i).eq.7.0) go to 40
      endif
      endif
   10 continue
      if (j.lt.3) return
      k=2
   20 xv=x(1,nstore(1))-x(1,nstore(k))
      zv=x(3,nstore(1))-x(3,nstore(k))
      yv=x(2,nstore(1))-x(2,nstore(k))
      xl=sqrt(xv**2+zv**2)
      if (xl.lt.1.e-20) then
      k=k+1
      if (abs(yv).gt.1.e-05) go to 40
      if (k.eq.j) return
      go to 20
      endif
      xv=xv/xl
      zv=zv/xl
      k=k+1
      do 30 i=k,j
      xu=x(1,nstore(1))-x(1,nstore(i))
      zu=x(3,nstore(1))-x(3,nstore(i))
      xl=sqrt(xu**2+zu**2)
      if (xl.lt.1.e-30) go to 30
      if (abs((xv*xu+zv*zu)/xl).lt.0.999) go to 40
   30 continue
      return
   40 if (rbcods(1).eq.1.0) rbcods(1)=4.0
      if (rbcods(1).eq.0.0) rbcods(1)=2.0
      if (rbcods(2).eq.5.0) rbcods(2)=7.0
      if (rbcods(2).eq.0.0) rbcods(2)=6.0
      do 50 i=1,n
      if (rbncdt(i).eq.2.0) rbncdt(i)=0.0
      if (rbncdt(i).eq.4.0) rbncdt(i)=1.0
      if (rbncdt(i).eq.5.0) rbncdt(i)=3.0
      if (rbncdt(i).eq.7.0) rbncdt(i)=6.0
   50 continue
      return
      end
      subroutine zcnstr(x,n,nrb,rbncdt,rbcods,nstore,rbncdr,iflag)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,1),nrb(*),rbncdt(*),rbncdr(*),rbcods(*),nstore(*)
      j=0
      do 10 i=1,n
      if (rbncdt(i).eq.3..or.rbncdt(i).eq.5..or.rbncdt(i).eq.6.
     1.or.rbncdt(i).eq.7.) then
      j=j+1
      nstore(j)=nrb(i)
      if (iflag.ne.0) then
      if (rbncdr(i).eq.4.0.or.rbncdr(i).eq.7.0) go to 40
      endif
      endif
   10 continue
      if (j.lt.3) return
      k=2
   20 xv=x(1,nstore(1))-x(1,nstore(k))
      yv=x(2,nstore(1))-x(2,nstore(k))
      zv=x(3,nstore(1))-x(3,nstore(k))
      xl=sqrt(xv**2+yv**2)
      if (xl.lt.1.e-20) then
      k=k+1
      if (abs(zv).gt.1.e-05) go to 40
      if (k.eq.j) return
      go to 20
      endif
      xv=xv/xl
      yv=yv/xl
      k=k+1
      do 30 i=k,j
      xu=x(1,nstore(1))-x(1,nstore(i))
      yu=x(2,nstore(1))-x(2,nstore(i))
      xl=sqrt(xu**2+yu**2)
      if (xl.lt.1.e-20) go to 30
      if (abs((xv*xu+yv*yu)/xl).lt.0.999) go to 40
   30 continue
      return
   40 if (rbcods(1).eq.1.0) rbcods(1)=6.0
      if (rbcods(2).eq.5.0) rbcods(2)=7.0
      if (rbcods(1).eq.0.0) rbcods(1)=3.0
      if (rbcods(2).eq.0.0) rbcods(2)=4.0
      if (rbcods(1).eq.2.0) rbcods(1)=5.0
      if (rbcods(1).eq.4.0) rbcods(1)=7.0
      if (rbcods(2).eq.6.0) rbcods(2)=7.0
      do 50 i=1,n
      if (rbncdt(i).eq.3.0) rbncdt(i)=0.0
      if (rbncdt(i).eq.5.0) rbncdt(i)=2.0
      if (rbncdt(i).eq.6.0) rbncdt(i)=1.0
      if (rbncdt(i).eq.7.0) rbncdt(i)=4.0
   50 continue
      return
      end
      subroutine matnde(mtypde,cmde,nmmtde)
c     implicit double precision (a-h,o-z)                                    dp
      dimension mtypde(1),cmde(24,1)
      character*80 txts,mssg
      if (nmmtde.eq.0) return
      write(13,100)
      do 50 i=1,nmmtde
      call gttxsg(txts,lcount)
      read (unit=txts,fmt=70,err=60) n,mtypde(n)
      if (n.le.0.or.n.gt.nmmtde) then
      write(13,190) n,nmmtde
      write( *,190) n,nmmtde
      call adios (2)
      endif
      write(13,90) n,mtypde(n)
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=80,err=60) (cmde(j,n),j=1,8)
      if (mtypde(n).eq.1) then
      write(13,110) cmde(1,n)
      go to 50
      endif
      if (mtypde(n).eq.2) then
      write(13,120) cmde(1,n)
      go to 50
      endif
      if (mtypde(n).eq.3) then
      write(13,130) (cmde(j,n),j=1,3)
      cmde(4,n)=(cmde(1,n)*cmde(2,n))/(cmde(1,n)-cmde(2,n))
      cmde(5,n)=cmde(1,n)+cmde(4,n)
      go to 50
      endif
      if (mtypde(n).eq.4) then
      write(13,140) cmde(1,n)
      go to 50
      endif
      if (mtypde(n).eq.5) then
      write(13,150) cmde(1,n)
      go to 50
      endif
      if (mtypde(n).eq.6) then
      if (cmde(5,n).eq.0.0) cmde(5,n)=cmde(4,n)
      write(13,160)(cmde(j,n),j=1,5)
      go to 50
      endif
      write(13,200) mtypde(n)
      write( *,200) mtypde(n)
      call adios(2)
   50 continue
      return
c
   60 write (unit=mssg,fmt=210) i
      call termin (txts,mssg,lcount,1)
c
   70 format(2i5)
   80 format(8e10.0)
   90 format(' spring-damper type set number .... ',i5//,
     &        10x,' spring-damper type .......... ',i5)
  100 format(/////' s p r i n g / d a m p e r   t y p e s'///,
     &        '                                           '/,
     &        '     eq.1   elastic                        '/,
     &        '     eq.2   linear viscous                 '/,
     &        '     eq.3   isotropic elastoplastic        '/,
     &        '     eq.4   nonlinear elastic              '/,
     &        '     eq.5   nonlinear viscous              '/,
     &        '     eq.6   general nonlinear              '//)
  110 format(
     & 5x,'elastic stiffness ................ =', e12.4///)
  120 format(
     & 5x,'damping constant ................. =', e12.4///)
  130 format(
     & 5x,'elastic stiffness ................ =', e12.4/,
     & 5x,'tangent stiffness ................ =', e12.4/,
     & 5x,'yield ............................ =', e12.4///)
  140 format(
     & 5x,'behavior described by load curve.. =', e12.4///)
  150 format(
     & 5x,'behavior described by load curve.. =', e12.4///)
  160 format(
     & 5x,'loading described by load curve .. =', e12.4/,
     & 5x,'unloading described by load curve  =', e12.4/,
     & 5x,'hardening parameter .............. =', e12.4/,
     & 5x,'initial yield (tensile) .......... =', e12.4/,
     & 5x,'initial yield (compressive) ...... =', e12.4///)
  190 format (//,' spring-damper type set number',i5,' is invalid.',/,
     1           ' the set number must not exceed',i5,//)
  200 format(//,' spring-damper type',i9,' is invalid---fatal',//)
  210 format(' error reading discrete element material set no.',i3)
      end
      subroutine elmnde(ixde,sclf,dehv,x,nmelde)
c     implicit double precision (a-h,o-z)                                    dp
      character*6 probid                                                vax75
      common/ident/probid(12)
      dimension ixde(3,1),sclf(1),dehv(9,1),x(3,1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
      i=0
      mprint=0
 10   call gttxsg (txts,lcount)
      if(large) then
      read(unit=txts,fmt=201,err=195) m,(ixde(j,m),j=1,3),sclf(m)
      else
      read(unit=txts,fmt=200,err=195) m,(ixde(j,m),j=1,3),sclf(m)
      endif
      if (sclf(m).eq.0.) sclf(m)=1.0
      inc=1
 20   i=i+1
      if (m-i) 180,50,30
 30   ixde(1,i)=ixde(1,i-1)+inc0
      ixde(2,i)=ixde(2,i-1)+inc0
      ixde(3,i)=ixde(3,i-1)
      sclf(i)  =sclf(i-1)
      go to 80
 50   inc0=inc
 80   if (mprint.gt.0) go to 100
      mprint=50
      write(13,230) probid
 100  write(13,220) i,(ixde(j,i),j=1,3),sclf(i)
      mprint=mprint-1
      if (m-i) 170,160,20
 160  if (nmelde-i) 180,180,10
 170  write(13,270) i
      write ( *,270) i
      call adios(2)
 180  do 190 i=1,nmelde
      dehv(1,i)=0.
      dehv(2,i)=0.
      dehv(3,i)=0.
      dehv(4,i)=0.
      dehv(5,i)=0.
      dehv(6,i)=0.
      dehv(8,i)=0.
      dehv(9,i)=0.
      j=ixde(1,i)
      k=ixde(2,i)
      dehv(7,i)=sqrt((x(1,j)-x(1,k))**2+(x(2,j)-x(2,k))**2
     1         +     (x(3,j)-x(3,k))**2)
  190 continue
      return
c
  195 mssg=' error reading discrete element connectivity cards.'
      call termin (txts,mssg,lcount,1)
c
 200  format (4i5,e10.0)
 201  format (4i8,e10.0)
 220  format (1i13,3i7,5x,e14.5)
 230  format('1 t w o   n o d e    s p r i n g    e l e m e n t s ',//,
     1     12a6,//,8x,'element ','  node1','  node2',' material',
     2 '    material scale factor')
 270  format (' spring-damper element card error, n= ',i5)
      end
      subroutine massin (nodm,xmass,radius,nmmass)
c     implicit double precision (a-h,o-z)                                    dp
      dimension nodm(1),xmass(1),radius(1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
c
      write(13,50)
      do 10 in=1,nmmass
      call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=70,err=30) n,xmass(in)
      else
      read (unit=txts,fmt=40,err=30) n,xmass(in)
      endif
      nodm(in)=n
 10   write(13,60) n,xmass(in)
      return
   30 write (unit=mssg,fmt=80) in
      call termin (txts,mssg,lcount,1)
   40 format(i5,2e10.0)
   50 format(///' n o d a l   m a s s   d a t a   '//
     1          '   node    lumped mass')
   60 format(i7,6e11.3)
   70 format(i8,2e10.0)
   80 format(' error reading nodal mass card no.',i4)
      end
      subroutine initde(npc,p,mtypde,cmde,ixde,sclf,dehv,nmelde)
c     implicit double precision (a-h,o-z)                                    dp
      data zro/0.0/
      dimension npc(1),p(1),mtypde(1),cmde(24,1),ixde(3,1),
     & sclf(1),dehv(9,1)
      do 10 i=1,nmelde
      matnum=ixde(3,i)
      mtyp  =mtypde(matnum)
      if (mtyp.eq.6) then
      dehv(8,i)=cmde(4,matnum)*sclf(i)
      dehv(9,i)=cmde(5,matnum)*sclf(i)
      ncur=nint(cmde(1,matnum))
      loc=npc(ncur)
      npoint=(npc(ncur+1)-loc)/2
      call itrpd2(p(loc),dehv(2,i),npoint,dehv(8,i),sclf(i),2,zro,slope)
      call itrpd2(p(loc),dehv(3,i),npoint,dehv(9,i),sclf(i),2,zro,slope)
      endif
   10 continue
      return
      end
      subroutine bslinp (locend)
c     implicit double precision (a-h,o-z)                                    dp
c=======================================================================
c
c
c        1-d slideline capability for modeling rebars
c
c
c     this slideline capability was developed at and made available
c     through:
c
c           ga technologies
c           10955 john jay hopkins drive
c           p.o. box 85608
c           san diego, california
c
c           by daniele pelessone  (619) 455-2501
c
c
c     . input 1-d slideline
      common/   / a(1)
      common / bk00 / numnp
      common / bmsc / numsl, locp, lenp, lhp, buf(1),
     .                nmn, nsn, lmsr, lnsv, lilocs, lfshr, lssm, lsa,
     .                ldmg, radius, fcci, shearm, umax, ibond, hdmg
      common / bk04 / dum(4), tssfac
      common/double/iprec,ncpw,unit
      dimension  s1dhdg(12)
      character*80 txts,mssg
      data  s1dhdg / 12*'    '/
c
c     . numsl : number of slide lines
c     . locp  : starting location in array a of slideline parameter data
c     . lenp  : length of parameter block for each slideline
c     . le    : pointer for array e used for master nodes unbal. forces
c     . lmsm  : pointer for array msm used for master nodes masses
c     . nmn   : number of master nodes in each slideline
c     . nsn   : number of master elements in each slideline
c     . lmsr  : pointer for master node array
c     . lnsv  : pointer for slave node array
c     . lsa   : pointer to node shear area
c     . ldmg  : pointer to damage parameter
c     . hp    : pointer to hydrostatic pressure
c     . lfshr : pointer to maximum allowable force at each slave node
c     . radius: radius of rod
c     . fcci  : inverse of compressive strength of concrete
c     . shearm: bond shear modulus
c     . umax  : maximum recoverable slippage
c     . ibond : 0=perfectly elasto plastic  1=with hardening
c     . hdmg  : exponent in bond damage relation
c
      lenp = 17
      locp = locend
      locend = locend + numsl*lenp
      call expndm (locend)
      loc = locp - 1
      nmnmax = 0
      nsnmax = 0
c     --  input data for each slide-line  --
      do 40 isl = 1, numsl
      call gttxsg (txts,lcount)
      read (unit=txts,fmt=1000,err=910) nsn,nmn,radius,fcc,shearm,
     1 umax,hdmg,ibond
c       --  check input line  --
        if (nmn.le.2) then
          void = s1dwrn (isl, 1)
          write(13,'('' --  number of master nodes must be greater'',
     .          '' than 2  --'')')
          write( *,'('' --  number of master nodes must be greater'',
     .          '' than 2  --'')')
        end if
        if (nsn.le.2) then
          void = s1dwrn (isl, 1)
          write(13,'('' --  number of slave nodes must be greater'',
     .          '' than 2  --'')')
          write( *,'('' --  number of slave nodes must be greater'',
     .          '' than 2  --'')')
        end if
        if (radius.le.0) then
          void = s1dwrn (isl, 1)
          write(13,'('' --  radius must be positive  --'')')
          write( *,'('' --  radius must be positive  --'')')
        end if
        if (fcc.gt.0) then
          fcci = 1. / fcc
        else
          void = s1dwrn (isl, 1)
          write(13,'('' --  fcc must be positive  --'')')
          write( *,'('' --  fcc must be positive  --'')')
        end if
        if (ibond.ne.1) ibond = 0
c       --  set pointers for isl-th slideline  --
        lmsr   = locend
        lnsv   = lmsr   + nmn
        lilocs = lnsv   + nsn
        lssm   = lilocs + nsn
        lsa    = lssm   + nsn
        lfshr  = lsa    + nsn
        ldmg   = lfshr  + nsn
        locend = ldmg   + nsn
c       -- transfer slideline parameters to array a  --
        do 10 i = 1, lenp
          a(loc+i) = buf(i+1)
   10     continue
c       --  expand memory  --
        call expndm (locend)
        nmnmax = max(nmnmax,nmn)
        nsnmax = max(nsnmax,nsn)
c
c       --  write out slideline data  --
c
        write (13,410) isl, (s1dhdg(i),i=1,12), nmn, nsn, radius, fcc,
     .        shearm, umax, hdmg
c
c       --  input master and slave nodes  --
c
      call onedsl (nsn,nmn,a(lmsr),a(lnsv),isl)
c
        loc = loc + lenp
c       --  check master nodes  --
c       do 20 i = 0, nmn-1
c         nod = ia(lmsr+i)
c         if (nod.le.0 .or. nod.gt.numnp) then
c           void = s1dwrn (isl, 2)
c           write (13,'('' --  node'',i6,'' out of range  --'')') nod
c           write ( *,'('' --  node'',i6,'' out of range  --'')') nod
c         end if
c  20   continue
c       --  check master nodes  --
c       do 30 i = 0, nsn-1
c         nod = ia(lnsv+i)
c         if (nod.le.0 .or. nod.gt.numnp) then
c           void = s1dwrn (isl, 3)
c           write (13,'('' --  node'',i6,'' out of range  --'')') nod
c           write ( *,'('' --  node'',i6,'' out of range  --'')') nod
c         end if
c  30   continue
  410   format (//,
     .     ' 1d slideline number ... ',i5,': ',12a4/
     .     '     number of master nodes .......... =',i4/
     .     '     number of slave nodes ........... =',i4/
     .     '     external radius of rebar ........ =',e12.4/
     .     '     failure strength of matrix ...... =',e12.4/
     .     '     bond shear modulus .............. =',e12.4/
     .     '     maximum shear displacement ...... =',e12.4/
     .     '     exponent in damage curve (hdmg) . =',e12.4)
   40   continue
      lhp    = locend
      locend = lhp    + numnp
      tssfac =   min(tssfac,0.666666*unit)
      return
c
c     --  error messages  --
c
  910 mssg=' error reading slideline control card for 1d slidelines'
      call termin (txts,mssg,lcount,1)
 1000 format(2i5,5e10.0,i5)
      end
c=======================================================================
      function s1dwrn (islno, ncard)
c     implicit double precision (a-h,o-z)                                    dp
      common / warngs / numwrn, maxwrn
      maxwrn = 10
      write (13,'(''0warning: slideline number'',i3,'', card number'',
     .      i3)') islno, ncard
      write ( *,'(''0warning: slideline number'',i3,'', card number'',
     .      i3)') islno, ncard
      numwrn = numwrn + 1
      if (numwrn.gt.maxwrn) then
        write (13,'(''0error: over 10 warnings cause program '',
     .        ''to abort'')')
        call adios (2)
      end if
      s1dwrn = 0.
      return
      end
c=======================================================================
c
      subroutine onedsl(nsn,nmn,msr,nsv,nm)
c     implicit double precision (a-h,o-z)                                    dp
c
c     read 1d slideline data
c
      dimension msr(1),nsv(1)
      logical large
      common/bigprb/large
      character*80 txts,mssg
      ik=0
      il=0
      i=ik
   10 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=171,err=155) n,nsvn
      else
      read (unit=txts,fmt=170,err=155) n,nsvn
      endif
      if (n.eq.0) n=i-ik+1
      n=n+ik
      nsv(n)=nsvn
      nl=n-i
      nk=nl-1
      if (nk) 50,40,20
   20 ndif=nsv(n)-nsv(i)
      nd=ndif/nl
      if (nd*nl.ne.ndif) go to 140
      nk=nk+i-1
      do 30 j=i,nk
      j1=j+1
      nsv(j1)=nsv(j)+nd
   30 continue
   40 if (nsn-n+ik) 130,60,50
   50 i=n
      go to 10
   60 i=il
   70 call gttxsg (txts,lcount)
      if(large) then
      read (unit=txts,fmt=171,err=155) n,msrn
      else
      read (unit=txts,fmt=170,err=155) n,msrn
      endif
      if (n.eq.0) n=i-il+1
      n=n+il
      msr(n)=msrn
      nl=n-i
      nk=nl-1
      if (nk) 110,100,80
   80 ndif=msr(n)-msr(i)
      nd=ndif/nl
      if (nd*nl.ne.ndif) go to 140
      nk=nk+i-1
      do 90 j=i,nk
      j1=j+1
      msr(j1)=msr(j)+nd
   90 continue
  100 if (nmn-n+il) 130,120,110
  110 i=n
      go to 70
  120 continue
      write(13,200) (i,msr(i),i=1,nmn)
      write(13,230) (i,nsv(i),i=1,nsn)
      return
  130 write(13,220) nm
      go to 150
  140 write(13,210) nm
  150 call adios (2)
  155 mssg=' error reading one dimensional slideline data'
      call termin (txts,mssg,lcount,1)
  160 format(2e10.0)
  170 format(2i5,e10.0)
  171 format(2i8,e10.0)
  200 format(/' master nodes '/(i10,'.',i5))
  210 format(/' fatal input error on slide-line ',i5)
  220 format(/' error on slide-line  ',i5)
  230 format(/' slave nodes  '/(i10,'.',i5))
      end
      subroutine dfifs (x,numnp,irects,nsv,numifs,nsn)
c     implicit double precision (a-h,o-z)                                    dp
      dimension x(3,*),irects(4,*),nsv(*),sfail(2)
c
      call rdseg (irects,numifs,x,sfail,0)
c
      write(13,30)
      write(13,40) (j,(irects(i,j),i=1,4),j=1,numifs)
c
c     count number of slave and master nodes
c
      nn=numnp+numnp
      do 10 i=1,nn
   10 nsv(i)=0
      call cntifs (irects,nsv,nsn,numifs,nsv(1+numnp),numnp)
c
      return
   30 format(///' l i n k e d  s u r f a c e  s e g m e n t s'//
     1       /'  segment node1 node2 node3 node4')
   40 format(i7,4i6)
      end
      subroutine cntifs(irects,nsv,nsn,numifs,iscr,numnp)
c     implicit double precision (a-h,o-z)                                    dp
c
      dimension irects(1),nsv(1),iscr(1)
c
c     initialize array of master nodes
c
      if (numifs.eq.0) return
      n=0
      nmifs4=4*numifs
      do 10 i=1,numnp
      iscr(i)=0
   10 continue
      do 20 i=1,nmifs4
      if (iscr(irects(i)).ne.0) go to 20
      n=n+1
      nsv(n)=irects(i)
      iscr(irects(i))=n
   20 continue
      do 30 i=1,n
      iscr(nsv(i))=0
   30 continue
      nsn=n
c
      call sscnv (irects,numifs,nsv,nsn,iscr)
      return
      end
      subroutine bgnfil (numifs,nsn,irects,nsv,buffer,x,ilink)
c     implicit double precision (a-h,o-z)                                    dp
c     common/iobuf/ioerr,iob2,iob3,iob4,iob5,iob6,iob7,iob8,iob9,iob11, ltss
c    1 iob14,iob15,iob16                                                ltss
      common/iobuf/ioerr,iob2(1080),iob3(1080),iob4(1080),iob5(1080),   vaxcs
     &       iob6(1080),iob7(1080),iob8(1080),iob9(1080),iob11(1080),   vaxcs
     &       iob14(1080),iob15(1080),iob16(1080)                        vaxcs
      dimension irects(4,*),nsv(*),buffer(3,*),x(3,*)
      ilink=0
      call wrabsf (iob15,nsn,1,ilink)
      call riosta (iob15)
      ilink=ilink+1
      call wrabsf (iob15,numifs,1,ilink)
      call riosta (iob15)
      ilink=ilink+1
      call wrabsf (iob15,irects,4*numifs,ilink)
      call riosta (iob15)
      ilink=ilink+4*numifs
      do 10 i=1,nsn
      buffer(1,i)=x(1,nsv(i))
      buffer(2,i)=x(2,nsv(i))
      buffer(3,i)=x(3,nsv(i))
   10 continue
      call wrabsf (iob15,buffer,3*nsn,ilink)
      call riosta (iob15)
      ilink=ilink+3*nsn
      return
      end
