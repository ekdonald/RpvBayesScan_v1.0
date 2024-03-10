      LOGICAL FUNCTION DISNAN(DIN)
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DIN
!     ..
!
!  Purpose
!  =======
!
!  DISNAN returns .TRUE. if its argument is NaN, and .FALSE.
!  otherwise.  To be replaced by the Fortran 2003 intrinsic in the
!  future.
!
!  Arguments
!  =========
!
!  DIN      (input) DOUBLE PRECISION
!          Input to test for NaN.
!
!  =====================================================================
!
!  .. External Functions ..
      LOGICAL DLAISNAN
      EXTERNAL DLAISNAN
!  ..
!  .. Executable Statements ..
      DISNAN = DLAISNAN(DIN,DIN)
      RETURN
      END FUNCTION DISNAN
      
      
      
      
      
      SUBROUTINE DLAE2( A, B, C, RT1, RT2 )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, RT1, RT2
!     ..
!
!  Purpose
!  =======
!
!  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
!     [  A   B  ]
!     [  B   C  ].
!  On return, RT1 is the eigenvalue of larger absolute value, and RT2
!  is the eigenvalue of smaller absolute value.
!
!  Arguments
!  =========
!
!  A       (input) DOUBLE PRECISION
!          The (1,1) element of the 2-by-2 matrix.
!
!  B       (input) DOUBLE PRECISION
!          The (1,2) and (2,1) elements of the 2-by-2 matrix.
!
!  C       (input) DOUBLE PRECISION
!          The (2,2) element of the 2-by-2 matrix.
!
!  RT1     (output) DOUBLE PRECISION
!          The eigenvalue of larger absolute value.
!
!  RT2     (output) DOUBLE PRECISION
!          The eigenvalue of smaller absolute value.
!
!  Further Details
!  ===============
!
!  RT1 is accurate to a few ulps barring over/underflow.
!
!  RT2 may be inaccurate if there is massive cancellation in the
!  determinant A*C-B*B; higher precision or correctly rounded or
!  correctly truncated arithmetic would be needed to compute RT2
!  accurately in all cases.
!
!  Overflow is possible only if RT1 is within a factor of 5 of overflow.
!  Underflow is harmless if the input data is 0 or exceeds
!     underflow_threshold / macheps.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   AB, ACMN, ACMX, ADF, DF, RT, SM, TB
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
!     ..
!     .. Executable Statements ..
!
!     Compute the eigenvalues
!
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      ENDIF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
!
!        Includes case AB=ADF=0
!
         RT = AB*SQRT( TWO )
      ENDIF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
!
!        Includes case RT1 = RT2 = 0
!
         RT1 = HALF*RT
         RT2 = -HALF*RT
      ENDIF
	RETURN
!
!     End of DLAE2
!
      END SUBROUTINE DLAE2
      
      
      
      
      
      SUBROUTINE DLAEBZ( IJOB, NITMAX, N, MMAX, MINP, NBMIN, ABSTOL, &
                         RELTOL, PIVMIN, D, E, E2, NVAL, AB, C, MOUT, &
                         NAB, WORK, IWORK, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IJOB, INFO, MINP, MMAX, MOUT, N, NBMIN, NITMAX
      DOUBLE PRECISION   ABSTOL, PIVMIN, RELTOL
!     ..
!     .. Array Arguments ..
      INTEGER            IWORK( * ), NAB( MMAX, * ), NVAL( * )
      DOUBLE PRECISION   AB( MMAX, * ), C( * ), D( * ), E( * ), E2( * ), &
                         WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAEBZ contains the iteration loops which compute and use the
!  function N(w), which is the count of eigenvalues of a symmetric
!  tridiagonal matrix T less than or equal to its argument  w.  It
!  performs a choice of two types of loops:
!
!  IJOB=1, followed by
!  IJOB=2: It takes as input a list of intervals and returns a list of
!          sufficiently small intervals whose union contains the same
!          eigenvalues as the union of the original intervals.
!          The input intervals are (AB(j,1),AB(j,2)], j=1,...,MINP.
!          The output interval (AB(j,1),AB(j,2)] will contain
!          eigenvalues NAB(j,1)+1,...,NAB(j,2), where 1 <= j <= MOUT.
!
!  IJOB=3: It performs a binary search in each input interval
!          (AB(j,1),AB(j,2)] for a point  w(j)  such that
!          N(w(j))=NVAL(j), and uses  C(j)  as the starting point of
!          the search.  If such a w(j) is found, then on output
!          AB(j,1)=AB(j,2)=w.  If no such w(j) is found, then on output
!          (AB(j,1),AB(j,2)] will be a small interval containing the
!          point where N(w) jumps through NVAL(j), unless that point
!          lies outside the initial interval.
!
!  Note that the intervals are in all cases half-open intervals,
!  i.e., of the form  (a,b] , which includes  b  but not  a .
!
!  To avoid underflow, the matrix should be scaled so that its largest
!  element is no greater than  overflow**(1/2) * underflow**(1/4)
!  in absolute value.  To assure the most accurate computation
!  of small eigenvalues, the matrix should be scaled to be
!  not much smaller than that, either.
!
!  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
!  Matrix", Report CS41, Computer Science Dept., Stanford
!  University, July 21, 1966
!
!  Note: the arguments are, in general, *not* checked for unreasonable
!  values.
!
!  Arguments
!  =========
!
!  IJOB    (input) INTEGER
!          Specifies what is to be done:
!          = 1:  Compute NAB for the initial intervals.
!          = 2:  Perform bisection iteration to find eigenvalues of T.
!          = 3:  Perform bisection iteration to invert N(w), i.e.,
!                to find a point which has a specified number of
!                eigenvalues of T to its left.
!          Other values will cause DLAEBZ to return with INFO=-1.
!
!  NITMAX  (input) INTEGER
!          The maximum number of "levels" of bisection to be
!          performed, i.e., an interval of width W will not be made
!          smaller than 2^(-NITMAX) * W.  If not all intervals
!          have converged after NITMAX iterations, then INFO is set
!          to the number of non-converged intervals.
!
!  N       (input) INTEGER
!          The dimension n of the tridiagonal matrix T.  It must be at
!          least 1.
!
!  MMAX    (input) INTEGER
!          The maximum number of intervals.  If more than MMAX intervals
!          are generated, then DLAEBZ will quit with INFO=MMAX+1.
!
!  MINP    (input) INTEGER
!          The initial number of intervals.  It may not be greater than
!          MMAX.
!
!  NBMIN   (input) INTEGER
!          The smallest number of intervals that should be processed
!          using a vector loop.  If zero, then only the scalar loop
!          will be used.
!
!  ABSTOL  (input) DOUBLE PRECISION
!          The minimum (absolute) width of an interval.  When an
!          interval is narrower than ABSTOL, or than RELTOL times the
!          larger (in magnitude) endpoint, then it is considered to be
!          sufficiently small, i.e., converged.  This must be at least
!          zero.
!
!  RELTOL  (input) DOUBLE PRECISION
!          The minimum relative width of an interval.  When an interval
!          is narrower than ABSTOL, or than RELTOL times the larger (in
!          magnitude) endpoint, then it is considered to be
!          sufficiently small, i.e., converged.  Note: this should
!          always be at least radix*machine epsilon.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum absolute value of a "pivot" in the Sturm
!          sequence loop.  This *must* be at least  max |e(j)**2| *
!          safe_min  and at least safe_min, where safe_min is at least
!          the smallest number that can divide one without overflow.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T.
!
!  E       (input) DOUBLE PRECISION array, dimension (N)
!          The offdiagonal elements of the tridiagonal matrix T in
!          positions 1 through N-1.  E(N) is arbitrary.
!
!  E2      (input) DOUBLE PRECISION array, dimension (N)
!          The squares of the offdiagonal elements of the tridiagonal
!          matrix T.  E2(N) is ignored.
!
!  NVAL    (input/output) INTEGER array, dimension (MINP)
!          If IJOB=1 or 2, not referenced.
!          If IJOB=3, the desired values of N(w).  The elements of NVAL
!          will be reordered to correspond with the intervals in AB.
!          Thus, NVAL(j) on output will not, in general be the same as
!          NVAL(j) on input, but it will correspond with the interval
!          (AB(j,1),AB(j,2)] on output.
!
!  AB      (input/output) DOUBLE PRECISION array, dimension (MMAX,2)
!          The endpoints of the intervals.  AB(j,1) is  a(j), the left
!          endpoint of the j-th interval, and AB(j,2) is b(j), the
!          right endpoint of the j-th interval.  The input intervals
!          will, in general, be modified, split, and reordered by the
!          calculation.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (MMAX)
!          If IJOB=1, ignored.
!          If IJOB=2, workspace.
!          If IJOB=3, then on input C(j) should be initialized to the
!          first search point in the binary search.
!
!  MOUT    (output) INTEGER
!          If IJOB=1, the number of eigenvalues in the intervals.
!          If IJOB=2 or 3, the number of intervals output.
!          If IJOB=3, MOUT will equal MINP.
!
!  NAB     (input/output) INTEGER array, dimension (MMAX,2)
!          If IJOB=1, then on output NAB(i,j) will be set to N(AB(i,j)).
!          If IJOB=2, then on input, NAB(i,j) should be set.  It must
!             satisfy the condition:
!             N(AB(i,1)) <= NAB(i,1) <= NAB(i,2) <= N(AB(i,2)),
!             which means that in interval i only eigenvalues
!             NAB(i,1)+1,...,NAB(i,2) will be considered.  Usually,
!             NAB(i,j)=N(AB(i,j)), from a previous call to DLAEBZ with
!             IJOB=1.
!             On output, NAB(i,j) will contain
!             max(na(k),min(nb(k),N(AB(i,j)))), where k is the index of
!             the input interval that the output interval
!             (AB(j,1),AB(j,2)] came from, and na(k) and nb(k) are the
!             the input values of NAB(k,1) and NAB(k,2).
!          If IJOB=3, then on output, NAB(i,j) contains N(AB(i,j)),
!             unless N(w) > NVAL(i) for all search points  w , in which
!             case NAB(i,1) will not be modified, i.e., the output
!             value will be the same as the input value (modulo
!             reorderings -- see NVAL and AB), or unless N(w) < NVAL(i)
!             for all search points  w , in which case NAB(i,2) will
!             not be modified.  Normally, NAB should be set to some
!             distinctive value(s) before DLAEBZ is called.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (MMAX)
!          Workspace.
!
!  IWORK   (workspace) INTEGER array, dimension (MMAX)
!          Workspace.
!
!  INFO    (output) INTEGER
!          = 0:       All intervals converged.
!          = 1--MMAX: The last INFO intervals did not converge.
!          = MMAX+1:  More than MMAX intervals were generated.
!
!  Further Details
!  ===============
!
!      This routine is intended to be called only by other LAPACK
!  routines, thus the interface is less user-friendly.  It is intended
!  for two purposes:
!
!  (a) finding eigenvalues.  In this case, DLAEBZ should have one or
!      more initial intervals set up in AB, and DLAEBZ should be called
!      with IJOB=1.  This sets up NAB, and also counts the eigenvalues.
!      Intervals with no eigenvalues would usually be thrown out at
!      this point.  Also, if not all the eigenvalues in an interval i
!      are desired, NAB(i,1) can be increased or NAB(i,2) decreased.
!      For example, set NAB(i,1)=NAB(i,2)-1 to get the largest
!      eigenvalue.  DLAEBZ is then called with IJOB=2 and MMAX
!      no smaller than the value of MOUT returned by the call with
!      IJOB=1.  After this (IJOB=2) call, eigenvalues NAB(i,1)+1
!      through NAB(i,2) are approximately AB(i,1) (or AB(i,2)) to the
!      tolerance specified by ABSTOL and RELTOL.
!
!  (b) finding an interval (a',b'] containing eigenvalues w(f),...,w(l).
!      In this case, start with a Gershgorin interval  (a,b).  Set up
!      AB to contain 2 search intervals, both initially (a,b).  One
!      NVAL element should contain  f-1  and the other should contain  l
!      , while C should contain a and b, resp.  NAB(i,1) should be -1
!      and NAB(i,2) should be N+1, to flag an error if the desired
!      interval does not lie in (a,b).  DLAEBZ is then called with
!      IJOB=3.  On exit, if w(f-1) < w(f), then one of the intervals --
!      j -- will have AB(j,1)=AB(j,2) and NAB(j,1)=NAB(j,2)=f-1, while
!      if, to the specified tolerance, w(f-k)=...=w(f+r), k > 0 and r
!      >= 0, then the interval will have  N(AB(j,1))=NAB(j,1)=f-k and
!      N(AB(j,2))=NAB(j,2)=f+r.  The cases w(l) < w(l+1) and
!      w(l-r)=...=w(l+k) are handled similarly.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, TWO = 2.0D0, &
                         HALF = 1.0D0 / TWO )
!     ..
!     .. Local Scalars ..
      INTEGER            ITMP1, ITMP2, J, JI, JIT, JP, KF, KFNEW, KL, &
                         KLNEW
      DOUBLE PRECISION   TMP1, TMP2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Check for Errors
!
      INFO = 0
      IF( IJOB.LT.1 .OR. IJOB.GT.3 ) THEN
         INFO = -1
         RETURN
      ENDIF
!
!     Initialize NAB
!
      IF( IJOB.EQ.1 ) THEN
!
!        Compute the number of eigenvalues in the initial intervals.
!
         MOUT = 0
!DIR$ NOVECTOR
         DO 30 JI = 1, MINP
            DO 20 JP = 1, 2
               TMP1 = D( 1 ) - AB( JI, JP )
               IF( ABS( TMP1 ).LT.PIVMIN ) &
                  TMP1 = -PIVMIN
               NAB( JI, JP ) = 0
               IF( TMP1.LE.ZERO ) &
                  NAB( JI, JP ) = 1
!
               DO 10 J = 2, N
                  TMP1 = D( J ) - E2( J-1 ) / TMP1 - AB( JI, JP )
                  IF( ABS( TMP1 ).LT.PIVMIN ) &
                     TMP1 = -PIVMIN
                  IF( TMP1.LE.ZERO ) &
                     NAB( JI, JP ) = NAB( JI, JP ) + 1
   10          CONTINUE
   20       CONTINUE
            MOUT = MOUT + NAB( JI, 2 ) - NAB( JI, 1 )
   30    CONTINUE
         RETURN
      ENDIF
!
!     Initialize for loop
!
!     KF and KL have the following meaning:
!        Intervals 1,...,KF-1 have converged.
!        Intervals KF,...,KL  still need to be refined.
!
      KF = 1
      KL = MINP
!
!     If IJOB=2, initialize C.
!     If IJOB=3, use the user-supplied starting point.
!
      IF( IJOB.EQ.2 ) THEN
         DO 40 JI = 1, MINP
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
   40    CONTINUE
      ENDIF
!
!     Iteration loop
!
      DO 130 JIT = 1, NITMAX
!
!        Loop over intervals
!
         IF( KL-KF+1.GE.NBMIN .AND. NBMIN.GT.0 ) THEN
!
!           Begin of Parallel Version of the loop
!
            DO 60 JI = KF, KL
!
!              Compute N(c), the number of eigenvalues less than c
!
               WORK( JI ) = D( 1 ) - C( JI )
               IWORK( JI ) = 0
               IF( WORK( JI ).LE.PIVMIN ) THEN
                  IWORK( JI ) = 1
                  WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
               ENDIF
!
               DO 50 J = 2, N
                  WORK( JI ) = D( J ) - E2( J-1 ) / WORK( JI ) - C( JI )
                  IF( WORK( JI ).LE.PIVMIN ) THEN
                     IWORK( JI ) = IWORK( JI ) + 1
                     WORK( JI ) = MIN( WORK( JI ), -PIVMIN )
                  ENDIF
   50          CONTINUE
   60       CONTINUE
!
            IF( IJOB.LE.2 ) THEN
!
!              IJOB=2: Choose all intervals containing eigenvalues.
!
               KLNEW = KL
               DO 70 JI = KF, KL
!
!                 Insure that N(w) is monotone
!
                  IWORK( JI ) = MIN( NAB( JI, 2 ), &
                                MAX( NAB( JI, 1 ), IWORK( JI ) ) )
!
!                 Update the Queue -- add intervals if both halves
!                 contain eigenvalues.
!
                  IF( IWORK( JI ).EQ.NAB( JI, 2 ) ) THEN
!
!                    No eigenvalue in the upper interval:
!                    just use the lower interval.
!
                     AB( JI, 2 ) = C( JI )
!
                  ELSE IF( IWORK( JI ).EQ.NAB( JI, 1 ) ) THEN
!
!                    No eigenvalue in the lower interval:
!                    just use the upper interval.
!
                     AB( JI, 1 ) = C( JI )
                  ELSE
                     KLNEW = KLNEW + 1
                     IF( KLNEW.LE.MMAX ) THEN
!
!                       Eigenvalue in both intervals -- add upper to
!                       queue.
!
                        AB( KLNEW, 2 ) = AB( JI, 2 )
                        NAB( KLNEW, 2 ) = NAB( JI, 2 )
                        AB( KLNEW, 1 ) = C( JI )
                        NAB( KLNEW, 1 ) = IWORK( JI )
                        AB( JI, 2 ) = C( JI )
                        NAB( JI, 2 ) = IWORK( JI )
                     ELSE
                        INFO = MMAX + 1
                     ENDIF
                  ENDIF
   70          CONTINUE
               IF( INFO.NE.0 ) &
                  RETURN
               KL = KLNEW
            ELSE
!
!              IJOB=3: Binary search.  Keep only the interval containing
!                      w   s.t. N(w) = NVAL
!
               DO 80 JI = KF, KL
                  IF( IWORK( JI ).LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = C( JI )
                     NAB( JI, 1 ) = IWORK( JI )
                  ENDIF
                  IF( IWORK( JI ).GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = C( JI )
                     NAB( JI, 2 ) = IWORK( JI )
                  ENDIF
   80          CONTINUE
            ENDIF
!
         ELSE
!
!           End of Parallel Version of the loop
!
!           Begin of Serial Version of the loop
!
            KLNEW = KL
            DO 100 JI = KF, KL
!
!              Compute N(w), the number of eigenvalues less than w
!
               TMP1 = C( JI )
               TMP2 = D( 1 ) - TMP1
               ITMP1 = 0
               IF( TMP2.LE.PIVMIN ) THEN
                  ITMP1 = 1
                  TMP2 = MIN( TMP2, -PIVMIN )
               ENDIF
!
!              A series of compiler directives to defeat vectorization
!              for the next loop
!
!$PL$ CMCHAR=' '
!DIR$          NEXTSCALAR
!$DIR          SCALAR
!DIR$          NEXT SCALAR
!VD$L          NOVECTOR
!DEC$          NOVECTOR
!VD$           NOVECTOR
!VDIR          NOVECTOR
!VOCL          LOOP,SCALAR
!IBM           PREFER SCALAR
!$PL$ CMCHAR='*'
!
               DO 90 J = 2, N
                  TMP2 = D( J ) - E2( J-1 ) / TMP2 - TMP1
                  IF( TMP2.LE.PIVMIN ) THEN
                     ITMP1 = ITMP1 + 1
                     TMP2 = MIN( TMP2, -PIVMIN )
                  ENDIF
   90          CONTINUE
!
               IF( IJOB.LE.2 ) THEN
!
!                 IJOB=2: Choose all intervals containing eigenvalues.
!
!                 Insure that N(w) is monotone
!
                  ITMP1 = MIN( NAB( JI, 2 ), &
                          MAX( NAB( JI, 1 ), ITMP1 ) )
!
!                 Update the Queue -- add intervals if both halves
!                 contain eigenvalues.
!
                  IF( ITMP1.EQ.NAB( JI, 2 ) ) THEN
!
!                    No eigenvalue in the upper interval:
!                    just use the lower interval.
!
                     AB( JI, 2 ) = TMP1
!
                  ELSE IF( ITMP1.EQ.NAB( JI, 1 ) ) THEN
!
!                    No eigenvalue in the lower interval:
!                    just use the upper interval.
!
                     AB( JI, 1 ) = TMP1
                  ELSE IF( KLNEW.LT.MMAX ) THEN
!
!                    Eigenvalue in both intervals -- add upper to queue.
!
                     KLNEW = KLNEW + 1
                     AB( KLNEW, 2 ) = AB( JI, 2 )
                     NAB( KLNEW, 2 ) = NAB( JI, 2 )
                     AB( KLNEW, 1 ) = TMP1
                     NAB( KLNEW, 1 ) = ITMP1
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  ELSE
                     INFO = MMAX + 1
                     RETURN
                  ENDIF
               ELSE
!
!                 IJOB=3: Binary search.  Keep only the interval
!                         containing  w  s.t. N(w) = NVAL
!
                  IF( ITMP1.LE.NVAL( JI ) ) THEN
                     AB( JI, 1 ) = TMP1
                     NAB( JI, 1 ) = ITMP1
                  ENDIF
                  IF( ITMP1.GE.NVAL( JI ) ) THEN
                     AB( JI, 2 ) = TMP1
                     NAB( JI, 2 ) = ITMP1
                  ENDIF
               ENDIF
  100       CONTINUE
            KL = KLNEW
!
!           End of Serial Version of the loop
!
         ENDIF
!
!        Check for convergence
!
         KFNEW = KF
         DO 110 JI = KF, KL
            TMP1 = ABS( AB( JI, 2 )-AB( JI, 1 ) )
            TMP2 = MAX( ABS( AB( JI, 2 ) ), ABS( AB( JI, 1 ) ) )
            IF( TMP1.LT.MAX( ABSTOL, PIVMIN, RELTOL*TMP2 ) .OR. &
                NAB( JI, 1 ).GE.NAB( JI, 2 ) ) THEN
!
!              Converged -- Swap with position KFNEW,
!                           then increment KFNEW
!
               IF( JI.GT.KFNEW ) THEN
                  TMP1 = AB( JI, 1 )
                  TMP2 = AB( JI, 2 )
                  ITMP1 = NAB( JI, 1 )
                  ITMP2 = NAB( JI, 2 )
                  AB( JI, 1 ) = AB( KFNEW, 1 )
                  AB( JI, 2 ) = AB( KFNEW, 2 )
                  NAB( JI, 1 ) = NAB( KFNEW, 1 )
                  NAB( JI, 2 ) = NAB( KFNEW, 2 )
                  AB( KFNEW, 1 ) = TMP1
                  AB( KFNEW, 2 ) = TMP2
                  NAB( KFNEW, 1 ) = ITMP1
                  NAB( KFNEW, 2 ) = ITMP2
                  IF( IJOB.EQ.3 ) THEN
                     ITMP1 = NVAL( JI )
                     NVAL( JI ) = NVAL( KFNEW )
                     NVAL( KFNEW ) = ITMP1
                  ENDIF
               ENDIF
               KFNEW = KFNEW + 1
            ENDIF
  110    CONTINUE
         KF = KFNEW
!
!        Choose Midpoints
!
         DO 120 JI = KF, KL
            C( JI ) = HALF*( AB( JI, 1 )+AB( JI, 2 ) )
  120    CONTINUE
!
!        If no more intervals to refine, quit.
!
         IF( KF.GT.KL ) &
            GO TO 140
  130 CONTINUE
!
!     Converged
!
  140 CONTINUE
      INFO = MAX( KL+1-KF, 0 )
      MOUT = KL
!
      RETURN
!
!     End of DLAEBZ
!
	END SUBROUTINE DLAEBZ
      
      
      
      
      
      SUBROUTINE DLAEV2( A, B, C, RT1, RT2, CS1, SN1 )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   A, B, C, CS1, RT1, RT2, SN1
!     ..
!
!  Purpose
!  =======
!
!  DLAEV2 computes the eigendecomposition of a 2-by-2 symmetric matrix
!     [  A   B  ]
!     [  B   C  ].
!  On return, RT1 is the eigenvalue of larger absolute value, RT2 is the
!  eigenvalue of smaller absolute value, and (CS1,SN1) is the unit right
!  eigenvector for RT1, giving the decomposition
!
!     [ CS1  SN1 ] [  A   B  ] [ CS1 -SN1 ]  =  [ RT1  0  ]
!     [-SN1  CS1 ] [  B   C  ] [ SN1  CS1 ]     [  0  RT2 ].
!
!  Arguments
!  =========
!
!  A       (input) DOUBLE PRECISION
!          The (1,1) element of the 2-by-2 matrix.
!
!  B       (input) DOUBLE PRECISION
!          The (1,2) element and the conjugate of the (2,1) element of
!          the 2-by-2 matrix.
!
!  C       (input) DOUBLE PRECISION
!          The (2,2) element of the 2-by-2 matrix.
!
!  RT1     (output) DOUBLE PRECISION
!          The eigenvalue of larger absolute value.
!
!  RT2     (output) DOUBLE PRECISION
!          The eigenvalue of smaller absolute value.
!
!  CS1     (output) DOUBLE PRECISION
!  SN1     (output) DOUBLE PRECISION
!          The vector (CS1, SN1) is a unit right eigenvector for RT1.
!
!  Further Details
!  ===============
!
!  RT1 is accurate to a few ulps barring over/underflow.
!
!  RT2 may be inaccurate if there is massive cancellation in the
!  determinant A*C-B*B; higher precision or correctly rounded or
!  correctly truncated arithmetic would be needed to compute RT2
!  accurately in all cases.
!
!  CS1 and SN1 are accurate to a few ulps barring over/underflow.
!
!  Overflow is possible only if RT1 is within a factor of 5 of overflow.
!  Underflow is harmless if the input data is 0 or exceeds
!     underflow_threshold / macheps.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      DOUBLE PRECISION   TWO
      PARAMETER          ( TWO = 2.0D0 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   HALF
      PARAMETER          ( HALF = 0.5D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            SGN1, SGN2
      DOUBLE PRECISION   AB, ACMN, ACMX, ACS, ADF, CS, CT, DF, RT, SM, &
                         TB, TN
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, SQRT
!     ..
!     .. Executable Statements ..
!
!     Compute the eigenvalues
!
      SM = A + C
      DF = A - C
      ADF = ABS( DF )
      TB = B + B
      AB = ABS( TB )
      IF( ABS( A ).GT.ABS( C ) ) THEN
         ACMX = A
         ACMN = C
      ELSE
         ACMX = C
         ACMN = A
      ENDIF
      IF( ADF.GT.AB ) THEN
         RT = ADF*SQRT( ONE+( AB / ADF )**2 )
      ELSE IF( ADF.LT.AB ) THEN
         RT = AB*SQRT( ONE+( ADF / AB )**2 )
      ELSE
!
!        Includes case AB=ADF=0
!
         RT = AB*SQRT( TWO )
      ENDIF
      IF( SM.LT.ZERO ) THEN
         RT1 = HALF*( SM-RT )
         SGN1 = -1
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE IF( SM.GT.ZERO ) THEN
         RT1 = HALF*( SM+RT )
         SGN1 = 1
!
!        Order of execution important.
!        To get fully accurate smaller eigenvalue,
!        next line needs to be executed in higher precision.
!
         RT2 = ( ACMX / RT1 )*ACMN - ( B / RT1 )*B
      ELSE
!
!        Includes case RT1 = RT2 = 0
!
         RT1 = HALF*RT
         RT2 = -HALF*RT
         SGN1 = 1
      ENDIF
!
!     Compute the eigenvector
!
      IF( DF.GE.ZERO ) THEN
         CS = DF + RT
         SGN2 = 1
      ELSE
         CS = DF - RT
         SGN2 = -1
      ENDIF
      ACS = ABS( CS )
      IF( ACS.GT.AB ) THEN
         CT = -TB / CS
         SN1 = ONE / SQRT( ONE+CT*CT )
         CS1 = CT*SN1
      ELSE
         IF( AB.EQ.ZERO ) THEN
            CS1 = ONE
            SN1 = ZERO
         ELSE
            TN = -CS / TB
            CS1 = ONE / SQRT( ONE+TN*TN )
            SN1 = TN*CS1
         ENDIF
      ENDIF
      IF( SGN1.EQ.SGN2 ) THEN
         TN = CS1
         CS1 = -SN1
         SN1 = TN
      ENDIF
      RETURN
!
!     End of DLAEV2
!
      END SUBROUTINE DLAEV2
      
      
      
      
      
      SUBROUTINE DLAGTF( N, A, LAMBDA, B, C, TOL, D, IN, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, N
      DOUBLE PRECISION   LAMBDA, TOL
!     ..
!     .. Array Arguments ..
      INTEGER            IN( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), D( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAGTF factorizes the matrix (T - lambda*I), where T is an n by n
!  tridiagonal matrix and lambda is a scalar, as
!
!     T - lambda*I = PLU,
!
!  where P is a permutation matrix, L is a unit lower tridiagonal matrix
!  with at most one non-zero sub-diagonal elements per column and U is
!  an upper triangular matrix with at most two non-zero super-diagonal
!  elements per column.
!
!  The factorization is obtained by Gaussian elimination with partial
!  pivoting and implicit row scaling.
!
!  The parameter LAMBDA is included in the routine so that DLAGTF may
!  be used, in conjunction with DLAGTS, to obtain eigenvectors of T by
!  inverse iteration.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix T.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, A must contain the diagonal elements of T.
!
!          On exit, A is overwritten by the n diagonal elements of the
!          upper triangular matrix U of the factorization of T.
!
!  LAMBDA  (input) DOUBLE PRECISION
!          On entry, the scalar lambda.
!
!  B       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, B must contain the (n-1) super-diagonal elements of
!          T.
!
!          On exit, B is overwritten by the (n-1) super-diagonal
!          elements of the matrix U of the factorization of T.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, C must contain the (n-1) sub-diagonal elements of
!          T.
!
!          On exit, C is overwritten by the (n-1) sub-diagonal elements
!          of the matrix L of the factorization of T.
!
!  TOL     (input) DOUBLE PRECISION
!          On entry, a relative tolerance used to indicate whether or
!          not the matrix (T - lambda*I) is nearly singular. TOL should
!          normally be chose as approximately the largest relative error
!          in the elements of T. For example, if the elements of T are
!          correct to about 4 significant figures, then TOL should be
!          set to about 5*10**(-4). If TOL is supplied as less than eps,
!          where eps is the relative machine precision, then the value
!          eps is used in place of TOL.
!
!  D       (output) DOUBLE PRECISION array, dimension (N-2)
!          On exit, D is overwritten by the (n-2) second super-diagonal
!          elements of the matrix U of the factorization of T.
!
!  IN      (output) INTEGER array, dimension (N)
!          On exit, IN contains details of the permutation matrix P. If
!          an interchange occurred at the kth step of the elimination,
!          then IN(k) = 1, otherwise IN(k) = 0. The element IN(n)
!          returns the smallest positive integer j such that
!
!             abs( u(j,j) ).le. norm( (T - lambda*I)(j) )*TOL,
!
!          where norm( A(j) ) denotes the sum of the absolute values of
!          the jth row of the matrix A. If no such j exists then IN(n)
!          is returned as zero. If IN(n) is returned as positive, then a
!          diagonal element of U is small, indicating that
!          (T - lambda*I) is singular or nearly singular,
!
!  INFO    (output) INTEGER
!          = 0   : successful exit
!          .lt. 0: if INFO = -k, the kth argument had an illegal value
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            K
      DOUBLE PRECISION   EPS, MULT, PIV1, PIV2, SCALE1, SCALE2, TEMP, TL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DLAGTF', -INFO )
         RETURN
      ENDIF
!
      IF( N.EQ.0 ) &
         RETURN
!
      A( 1 ) = A( 1 ) - LAMBDA
      IN( N ) = 0
      IF( N.EQ.1 ) THEN
         IF( A( 1 ).EQ.ZERO ) &
            IN( 1 ) = 1
         RETURN
      ENDIF
!
      EPS = DLAMCH( 'Epsilon' )
!
      TL = MAX( TOL, EPS )
      SCALE1 = ABS( A( 1 ) ) + ABS( B( 1 ) )
      DO 10 K = 1, N - 1
         A( K+1 ) = A( K+1 ) - LAMBDA
         SCALE2 = ABS( C( K ) ) + ABS( A( K+1 ) )
         IF( K.LT.( N-1 ) ) &
            SCALE2 = SCALE2 + ABS( B( K+1 ) )
         IF( A( K ).EQ.ZERO ) THEN
            PIV1 = ZERO
         ELSE
            PIV1 = ABS( A( K ) ) / SCALE1
         ENDIF
         IF( C( K ).EQ.ZERO ) THEN
            IN( K ) = 0
            PIV2 = ZERO
            SCALE1 = SCALE2
            IF( K.LT.( N-1 ) ) &
               D( K ) = ZERO
         ELSE
            PIV2 = ABS( C( K ) ) / SCALE2
            IF( PIV2.LE.PIV1 ) THEN
               IN( K ) = 0
               SCALE1 = SCALE2
               C( K ) = C( K ) / A( K )
               A( K+1 ) = A( K+1 ) - C( K )*B( K )
               IF( K.LT.( N-1 ) ) &
                  D( K ) = ZERO
            ELSE
               IN( K ) = 1
               MULT = A( K ) / C( K )
               A( K ) = C( K )
               TEMP = A( K+1 )
               A( K+1 ) = B( K ) - MULT*TEMP
               IF( K.LT.( N-1 ) ) THEN
                  D( K ) = B( K+1 )
                  B( K+1 ) = -MULT*D( K )
               ENDIF
               B( K ) = TEMP
               C( K ) = MULT
            ENDIF
         ENDIF
         IF( ( MAX( PIV1, PIV2 ).LE.TL ) .AND. ( IN( N ).EQ.0 ) ) &
            IN( N ) = K
   10 CONTINUE
      IF( ( ABS( A( N ) ).LE.SCALE1*TL ) .AND. ( IN( N ).EQ.0 ) ) &
         IN( N ) = N
!
      RETURN
!
!     End of DLAGTF
!
	END SUBROUTINE DLAGTF
      
      
      
      
      
      SUBROUTINE DLAGTS( JOB, N, A, B, C, D, IN, Y, TOL, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, JOB, N
      DOUBLE PRECISION   TOL
!     ..
!     .. Array Arguments ..
      INTEGER            IN( * )
      DOUBLE PRECISION   A( * ), B( * ), C( * ), D( * ), Y( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAGTS may be used to solve one of the systems of equations
!
!     (T - lambda*I)*x = y   or   (T - lambda*I)'*x = y,
!
!  where T is an n by n tridiagonal matrix, for x, following the
!  factorization of (T - lambda*I) as
!
!     (T - lambda*I) = P*L*U ,
!
!  by routine DLAGTF. The choice of equation to be solved is
!  controlled by the argument JOB, and in each case there is an option
!  to perturb zero or very small diagonal elements of U, this option
!  being intended for use in applications such as inverse iteration.
!
!  Arguments
!  =========
!
!  JOB     (input) INTEGER
!          Specifies the job to be performed by DLAGTS as follows:
!          =  1: The equations  (T - lambda*I)x = y  are to be solved,
!                but diagonal elements of U are not to be perturbed.
!          = -1: The equations  (T - lambda*I)x = y  are to be solved
!                and, if overflow would otherwise occur, the diagonal
!                elements of U are to be perturbed. See argument TOL
!                below.
!          =  2: The equations  (T - lambda*I)'x = y  are to be solved,
!                but diagonal elements of U are not to be perturbed.
!          = -2: The equations  (T - lambda*I)'x = y  are to be solved
!                and, if overflow would otherwise occur, the diagonal
!                elements of U are to be perturbed. See argument TOL
!                below.
!
!  N       (input) INTEGER
!          The order of the matrix T.
!
!  A       (input) DOUBLE PRECISION array, dimension (N)
!          On entry, A must contain the diagonal elements of U as
!          returned from DLAGTF.
!
!  B       (input) DOUBLE PRECISION array, dimension (N-1)
!          On entry, B must contain the first super-diagonal elements of
!          U as returned from DLAGTF.
!
!  C       (input) DOUBLE PRECISION array, dimension (N-1)
!          On entry, C must contain the sub-diagonal elements of L as
!          returned from DLAGTF.
!
!  D       (input) DOUBLE PRECISION array, dimension (N-2)
!          On entry, D must contain the second super-diagonal elements
!          of U as returned from DLAGTF.
!
!  IN      (input) INTEGER array, dimension (N)
!          On entry, IN must contain details of the matrix P as returned
!          from DLAGTF.
!
!  Y       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the right hand side vector y.
!          On exit, Y is overwritten by the solution vector x.
!
!  TOL     (input/output) DOUBLE PRECISION
!          On entry, with  JOB .lt. 0, TOL should be the minimum
!          perturbation to be made to very small diagonal elements of U.
!          TOL should normally be chosen as about eps*norm(U), where eps
!          is the relative machine precision, but if TOL is supplied as
!          non-positive, then it is reset to eps*max( abs( u(i,j) ) ).
!          If  JOB .gt. 0  then TOL is not referenced.
!
!          On exit, TOL is changed as described above, only if TOL is
!          non-positive on entry. Otherwise TOL is unchanged.
!
!  INFO    (output) INTEGER
!          = 0   : successful exit
!          .lt. 0: if INFO = -i, the i-th argument had an illegal value
!          .gt. 0: overflow would occur when computing the INFO(th)
!                  element of the solution vector x. This can only occur
!                  when JOB is supplied as positive and either means
!                  that a diagonal element of U is very small, or that
!                  the elements of the right-hand side vector y are very
!                  large.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            K
      DOUBLE PRECISION   ABSAK, AK, BIGNUM, EPS, PERT, SFMIN, TEMP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SIGN
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      IF( ( ABS( JOB ).GT.2 ) .OR. ( JOB.EQ.0 ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ENDIF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLAGTS', -INFO )
         RETURN
      ENDIF
!
      IF( N.EQ.0 ) &
         RETURN
!
      EPS = DLAMCH( 'Epsilon' )
      SFMIN = DLAMCH( 'Safe minimum' )
      BIGNUM = ONE / SFMIN
!
      IF( JOB.LT.0 ) THEN
         IF( TOL.LE.ZERO ) THEN
            TOL = ABS( A( 1 ) )
            IF( N.GT.1 ) &
               TOL = MAX( TOL, ABS( A( 2 ) ), ABS( B( 1 ) ) )
            DO 10 K = 3, N
               TOL = MAX( TOL, ABS( A( K ) ), ABS( B( K-1 ) ), &
                     ABS( D( K-2 ) ) )
   10       CONTINUE
            TOL = TOL*EPS
            IF( TOL.EQ.ZERO ) &
               TOL = EPS
         ENDIF
      ENDIF
!
      IF( ABS( JOB ).EQ.1 ) THEN
         DO 20 K = 2, N
            IF( IN( K-1 ).EQ.0 ) THEN
               Y( K ) = Y( K ) - C( K-1 )*Y( K-1 )
            ELSE
               TEMP = Y( K-1 )
               Y( K-1 ) = Y( K )
               Y( K ) = TEMP - C( K-1 )*Y( K )
            ENDIF
   20    CONTINUE
         IF( JOB.EQ.1 ) THEN
            DO 30 K = N, 1, -1
               IF( K.LE.N-2 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 ) - D( K )*Y( K+2 )
               ELSE IF( K.EQ.N-1 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 )
               ELSE
                  TEMP = Y( K )
               ENDIF
               AK = A( K )
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK ) &
                          THEN
                        INFO = K
                        RETURN
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     ENDIF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     INFO = K
                     RETURN
                  ENDIF
               ENDIF
               Y( K ) = TEMP / AK
   30       CONTINUE
         ELSE
            DO 50 K = N, 1, -1
               IF( K.LE.N-2 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 ) - D( K )*Y( K+2 )
               ELSE IF( K.EQ.N-1 ) THEN
                  TEMP = Y( K ) - B( K )*Y( K+1 )
               ELSE
                  TEMP = Y( K )
               ENDIF
               AK = A( K )
               PERT = SIGN( TOL, AK )
   40          CONTINUE
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK ) &
                          THEN
                        AK = AK + PERT
                        PERT = 2*PERT
                        GO TO 40
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     ENDIF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     AK = AK + PERT
                     PERT = 2*PERT
                     GO TO 40
                  ENDIF
               ENDIF
               Y( K ) = TEMP / AK
   50       CONTINUE
         ENDIF
      ELSE
!
!        Come to here if  JOB = 2 or -2
!
         IF( JOB.EQ.2 ) THEN
            DO 60 K = 1, N
               IF( K.GE.3 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 ) - D( K-2 )*Y( K-2 )
               ELSE IF( K.EQ.2 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 )
               ELSE
                  TEMP = Y( K )
               ENDIF
               AK = A( K )
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK ) &
                          THEN
                        INFO = K
                        RETURN
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     ENDIF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     INFO = K
                     RETURN
                  ENDIF
               ENDIF
               Y( K ) = TEMP / AK
   60       CONTINUE
         ELSE
            DO 80 K = 1, N
               IF( K.GE.3 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 ) - D( K-2 )*Y( K-2 )
               ELSE IF( K.EQ.2 ) THEN
                  TEMP = Y( K ) - B( K-1 )*Y( K-1 )
               ELSE
                  TEMP = Y( K )
               ENDIF
               AK = A( K )
               PERT = SIGN( TOL, AK )
   70          CONTINUE
               ABSAK = ABS( AK )
               IF( ABSAK.LT.ONE ) THEN
                  IF( ABSAK.LT.SFMIN ) THEN
                     IF( ABSAK.EQ.ZERO .OR. ABS( TEMP )*SFMIN.GT.ABSAK ) &
                          THEN
                        AK = AK + PERT
                        PERT = 2*PERT
                        GO TO 70
                     ELSE
                        TEMP = TEMP*BIGNUM
                        AK = AK*BIGNUM
                     ENDIF
                  ELSE IF( ABS( TEMP ).GT.ABSAK*BIGNUM ) THEN
                     AK = AK + PERT
                     PERT = 2*PERT
                     GO TO 70
                  ENDIF
               ENDIF
               Y( K ) = TEMP / AK
   80       CONTINUE
         ENDIF
!
         DO 90 K = N, 2, -1
            IF( IN( K-1 ).EQ.0 ) THEN
               Y( K-1 ) = Y( K-1 ) - C( K-1 )*Y( K )
            ELSE
               TEMP = Y( K-1 )
               Y( K-1 ) = Y( K )
               Y( K ) = TEMP - C( K-1 )*Y( K )
            ENDIF
   90    CONTINUE
      ENDIF
!
!     End of DLAGTS
!
	END SUBROUTINE DLAGTS
      
      
      
      
      
      LOGICAL FUNCTION DLAISNAN(DIN1,DIN2)
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DIN1,DIN2
!     ..
!
!  Purpose
!  =======
!
!  This routine is not for general use.  It exists solely to avoid
!  over-optimization in DISNAN.
!
!  DLAISNAN checks for NaNs by comparing its two arguments for
!  inequality.  NaN is the only floating-point value where NaN != NaN
!  returns .TRUE.  To check for NaNs, pass the same variable as both
!  arguments.
!
!  Strictly speaking, Fortran does not allow aliasing of function
!  arguments. So a compiler must assume that the two arguments are
!  not the same variable, and the test will not be optimized away.
!  Interprocedural or whole-program optimization may delete this
!  test.  The ISNAN functions will be replaced by the correct
!  Fortran 03 intrinsic once the intrinsic is widely available.
!
!  Arguments
!  =========
!
!  DIN1     (input) DOUBLE PRECISION
!  DIN2     (input) DOUBLE PRECISION
!          Two numbers to compare for inequality.
!
!  =====================================================================
!
!  .. Executable Statements ..
      DLAISNAN = (DIN1.NE.DIN2)
      RETURN
      END FUNCTION DLAISNAN
      
      
      
      
      
      FUNCTION DLANEG( N, D, LLD, SIGMA, PIVMIN, R )
      IMPLICIT NONE
      INTEGER DLANEG
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            N, R
      DOUBLE PRECISION   PIVMIN, SIGMA
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), LLD( * )
!     ..
!
!  Purpose
!  =======
!
!  DLANEG computes the Sturm count, the number of negative pivots
!  encountered while factoring tridiagonal T - sigma I = L D L^T.
!  This implementation works directly on the factors without forming
!  the tridiagonal matrix T.  The Sturm count is also the number of
!  eigenvalues of T less than sigma.
!
!  This routine is called from DLARRB.
!
!  The current routine does not use the PIVMIN parameter but rather
!  requires IEEE-754 propagation of Infinities and NaNs.  This
!  routine also has no input range restrictions but does require
!  default exception handling such that x/0 produces Inf when x is
!  non-zero, and Inf/Inf produces NaN.  For more information, see:
!
!    Marques, Riedy, and Voemel, "Benefits of IEEE-754 Features in
!    Modern Symmetric Tridiagonal Eigensolvers," SIAM Journal on
!    Scientific Computing, v28, n5, 2006.  DOI 10.1137/050641624
!    (Tech report version in LAWN 172 with the same title.)
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of the diagonal matrix D.
!
!  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
!          The (N-1) elements L(i)*L(i)*D(i).
!
!  SIGMA   (input) DOUBLE PRECISION
!          Shift amount in T - sigma I = L D L^T.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot in the Sturm sequence.  May be used
!          when zero pivots are encountered on non-IEEE-754
!          architectures.
!
!  R       (input) INTEGER
!          The twist index for the twisted factorization that is used
!          for the negcount.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!     Jason Riedy, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0 )
!     Some architectures propagate Infinities and NaNs very slowly, so
!     the code computes counts in BLKLEN chunks.  Then a NaN can
!     propagate at most BLKLEN columns before being detected.  This is
!     not a general tuning parameter; it needs only to be just large
!     enough that the overhead is tiny in common cases.
      INTEGER BLKLEN
      PARAMETER ( BLKLEN = 128 )
!     ..
!     .. Local Scalars ..
      INTEGER            BJ, J, NEG1, NEG2, NEGCNT
      DOUBLE PRECISION   BSAV, DMINUS, DPLUS, GAMMA, P, T, TMP
      LOGICAL SAWNAN
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MIN, MAX
!     ..
!     .. External Functions ..
      LOGICAL DISNAN
      EXTERNAL DISNAN
!     ..
!     .. Executable Statements ..

      NEGCNT = 0

!     I) upper part: L D L^T - SIGMA I = L+ D+ L+^T
      T = -SIGMA
      DO 210 BJ = 1, R-1, BLKLEN
         NEG1 = 0
         BSAV = T
         DO 21 J = BJ, MIN(BJ+BLKLEN-1, R-1)
            DPLUS = D( J ) + T
            IF( DPLUS.LT.ZERO ) NEG1 = NEG1 + 1
            TMP = T / DPLUS
            T = TMP * LLD( J ) - SIGMA
 21      CONTINUE
         SAWNAN = DISNAN( T )
!     Run a slower version of the above loop if a NaN is detected.
!     A NaN should occur only with a zero pivot after an infinite
!     pivot.  In that case, substituting 1 for T/DPLUS is the
!     correct limit.
         IF( SAWNAN ) THEN
            NEG1 = 0
            T = BSAV
            DO 22 J = BJ, MIN(BJ+BLKLEN-1, R-1)
               DPLUS = D( J ) + T
               IF( DPLUS.LT.ZERO ) NEG1 = NEG1 + 1
               TMP = T / DPLUS
               IF (DISNAN(TMP)) TMP = ONE
               T = TMP * LLD(J) - SIGMA
 22         CONTINUE
         ENDIF
         NEGCNT = NEGCNT + NEG1
 210  CONTINUE
!
!     II) lower part: L D L^T - SIGMA I = U- D- U-^T
      P = D( N ) - SIGMA
      DO 230 BJ = N-1, R, -BLKLEN
         NEG2 = 0
         BSAV = P
         DO 23 J = BJ, MAX(BJ-BLKLEN+1, R), -1
            DMINUS = LLD( J ) + P
            IF( DMINUS.LT.ZERO ) NEG2 = NEG2 + 1
            TMP = P / DMINUS
            P = TMP * D( J ) - SIGMA
 23      CONTINUE
         SAWNAN = DISNAN( P )
!     As above, run a slower version that substitutes 1 for Inf/Inf.
!
         IF( SAWNAN ) THEN
            NEG2 = 0
            P = BSAV
            DO 24 J = BJ, MAX(BJ-BLKLEN+1, R), -1
               DMINUS = LLD( J ) + P
               IF( DMINUS.LT.ZERO ) NEG2 = NEG2 + 1
               TMP = P / DMINUS
               IF (DISNAN(TMP)) TMP = ONE
               P = TMP * D(J) - SIGMA
 24         CONTINUE
         ENDIF
         NEGCNT = NEGCNT + NEG2
 230  CONTINUE
!
!     III) Twist index
!       T was shifted by SIGMA initially.
      GAMMA = (T + SIGMA) + P
      IF( GAMMA.LT.ZERO ) NEGCNT = NEGCNT+1

      DLANEG = NEGCNT
      END FUNCTION DLANEG
      
      
      
      
      
      DOUBLE PRECISION FUNCTION DLANST( NORM, N, D, E )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          NORM
      INTEGER            N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
!     ..
!
!  Purpose
!  =======
!
!  DLANST  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  real symmetric tridiagonal matrix A.
!
!  Description
!  ===========
!
!  DLANST returns the value
!
!     DLANST = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!
!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in DLANST as described
!          above.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.  When N = 0, DLANST is
!          set to zero.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of A.
!
!  E       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) sub-diagonal or super-diagonal elements of A.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   ANORM, SCALE, SUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLASSQ
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
      IF( N.LE.0 ) THEN
         ANORM = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
!
!        Find max(abs(A(i,j))).
!
         ANORM = ABS( D( N ) )
         DO 10 I = 1, N - 1
            ANORM = MAX( ANORM, ABS( D( I ) ) )
            ANORM = MAX( ANORM, ABS( E( I ) ) )
   10    CONTINUE
      ELSE IF( LSAME( NORM, 'O' ) .OR. NORM.EQ.'1' .OR. &
               LSAME( NORM, 'I' ) ) THEN
!
!        Find norm1(A).
!
         IF( N.EQ.1 ) THEN
            ANORM = ABS( D( 1 ) )
         ELSE
            ANORM = MAX( ABS( D( 1 ) )+ABS( E( 1 ) ), &
                    ABS( E( N-1 ) )+ABS( D( N ) ) )
            DO 20 I = 2, N - 1
               ANORM = MAX( ANORM, ABS( D( I ) )+ABS( E( I ) )+ &
                       ABS( E( I-1 ) ) )
   20       CONTINUE
         ENDIF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
!
!        Find normF(A).
!
         SCALE = ZERO
         SUM = ONE
         IF( N.GT.1 ) THEN
            CALL DLASSQ( N-1, E, 1, SCALE, SUM )
            SUM = 2*SUM
         ENDIF
         CALL DLASSQ( N, D, 1, SCALE, SUM )
         ANORM = SCALE*SQRT( SUM )
      ENDIF
!
      DLANST = ANORM
      RETURN
!
!     End of DLANST
!
      END FUNCTION DLANST
      
      
      
      
      
      DOUBLE PRECISION FUNCTION DLANSY( NORM, UPLO, N, A, LDA, WORK )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          NORM, UPLO
      INTEGER            LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DLANSY  returns the value of the one norm,  or the Frobenius norm, or
!  the  infinity norm,  or the  element of  largest absolute value  of a
!  real symmetric matrix A.
!
!  Description
!  ===========
!
!  DLANSY returns the value
!
!     DLANSY = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!              (
!              ( norm1(A),         NORM = '1', 'O' or 'o'
!              (
!              ( normI(A),         NORM = 'I' or 'i'
!              (
!              ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!
!  where  norm1  denotes the  one norm of a matrix (maximum column sum),
!  normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!  normF  denotes the  Frobenius norm of a matrix (square root of sum of
!  squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!
!  Arguments
!  =========
!
!  NORM    (input) CHARACTER*1
!          Specifies the value to be returned in DLANSY as described
!          above.
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is to be referenced.
!          = 'U':  Upper triangular part of A is referenced
!          = 'L':  Lower triangular part of A is referenced
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.  When N = 0, DLANSY is
!          set to zero.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
!          The symmetric matrix A.  If UPLO = 'U', the leading n by n
!          upper triangular part of A contains the upper triangular part
!          of the matrix A, and the strictly lower triangular part of A
!          is not referenced.  If UPLO = 'L', the leading n by n lower
!          triangular part of A contains the lower triangular part of
!          the matrix A, and the strictly upper triangular part of A is
!          not referenced.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(N,1).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (MAX(1,LWORK)),
!          where LWORK >= N when NORM = 'I' or '1' or 'O'; otherwise,
!          WORK is not referenced.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   ABSA, SCALE, SUM, VALUE
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLASSQ
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
      IF( N.EQ.0 ) THEN
         VALUE = ZERO
      ELSE IF( LSAME( NORM, 'M' ) ) THEN
!
!        Find max(abs(A(i,j))).
!
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 20 J = 1, N
               DO 10 I = 1, J
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   10          CONTINUE
   20       CONTINUE
         ELSE
            DO 40 J = 1, N
               DO 30 I = J, N
                  VALUE = MAX( VALUE, ABS( A( I, J ) ) )
   30          CONTINUE
   40       CONTINUE
         ENDIF
      ELSE IF( ( LSAME( NORM, 'I' ) ) .OR. ( LSAME( NORM, 'O' ) ) .OR. &
               ( NORM.EQ.'1' ) ) THEN
!
!        Find normI(A) ( = norm1(A), since A is symmetric).
!
         VALUE = ZERO
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 60 J = 1, N
               SUM = ZERO
               DO 50 I = 1, J - 1
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   50          CONTINUE
               WORK( J ) = SUM + ABS( A( J, J ) )
   60       CONTINUE
            DO 70 I = 1, N
               VALUE = MAX( VALUE, WORK( I ) )
   70       CONTINUE
         ELSE
            DO 80 I = 1, N
               WORK( I ) = ZERO
   80       CONTINUE
            DO 100 J = 1, N
               SUM = WORK( J ) + ABS( A( J, J ) )
               DO 90 I = J + 1, N
                  ABSA = ABS( A( I, J ) )
                  SUM = SUM + ABSA
                  WORK( I ) = WORK( I ) + ABSA
   90          CONTINUE
               VALUE = MAX( VALUE, SUM )
  100       CONTINUE
         ENDIF
      ELSE IF( ( LSAME( NORM, 'F' ) ) .OR. ( LSAME( NORM, 'E' ) ) ) THEN
!
!        Find normF(A).
!
         SCALE = ZERO
         SUM = ONE
         IF( LSAME( UPLO, 'U' ) ) THEN
            DO 110 J = 2, N
               CALL DLASSQ( J-1, A( 1, J ), 1, SCALE, SUM )
  110       CONTINUE
         ELSE
            DO 120 J = 1, N - 1
               CALL DLASSQ( N-J, A( J+1, J ), 1, SCALE, SUM )
  120       CONTINUE
         ENDIF
         SUM = 2*SUM
         CALL DLASSQ( N, A, LDA+1, SCALE, SUM )
         VALUE = SCALE*SQRT( SUM )
      ENDIF
!
      DLANSY = VALUE
      RETURN
!
!     End of DLANSY
!
      END FUNCTION DLANSY
      
      
      
      
      
      DOUBLE PRECISION FUNCTION DLAPY2( X, Y )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION   X, Y
!     ..
!
!  Purpose
!  =======
!
!  DLAPY2 returns sqrt(x**2+y**2), taking care not to cause unnecessary
!  overflow.
!
!  Arguments
!  =========
!
!  X       (input) DOUBLE PRECISION
!  Y       (input) DOUBLE PRECISION
!          X and Y specify the values x and y.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
!     ..
!     .. Local Scalars ..
      DOUBLE PRECISION   W, XABS, YABS, Z
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      XABS = ABS( X )
      YABS = ABS( Y )
      W = MAX( XABS, YABS )
      Z = MIN( XABS, YABS )
      IF( Z.EQ.ZERO ) THEN
         DLAPY2 = W
      ELSE
         DLAPY2 = W*SQRT( ONE+( Z / W )**2 )
      ENDIF
      RETURN
!
!     End of DLAPY2
!
      END FUNCTION DLAPY2
      
      
      
      
      
      SUBROUTINE DLAR1V( N, B1, BN, LAMBDA, D, L, LD, LLD, &
                 PIVMIN, GAPTOL, Z, WANTNC, NEGCNT, ZTZ, MINGMA, &
                 R, ISUPPZ, NRMINV, RESID, RQCORR, WORK )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      LOGICAL            WANTNC
      INTEGER   B1, BN, N, NEGCNT, R
      DOUBLE PRECISION   GAPTOL, LAMBDA, MINGMA, NRMINV, PIVMIN, RESID, &
                         RQCORR, ZTZ
!     ..
!     .. Array Arguments ..
      INTEGER            ISUPPZ( * )
      DOUBLE PRECISION   D( * ), L( * ), LD( * ), LLD( * ), &
                        WORK( * )
      DOUBLE PRECISION Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAR1V computes the (scaled) r-th column of the inverse of
!  the sumbmatrix in rows B1 through BN of the tridiagonal matrix
!  L D L^T - sigma I. When sigma is close to an eigenvalue, the
!  computed vector is an accurate eigenvector. Usually, r corresponds
!  to the index where the eigenvector is largest in magnitude.
!  The following steps accomplish this computation :
!  (a) Stationary qd transform,  L D L^T - sigma I = L(+) D(+) L(+)^T,
!  (b) Progressive qd transform, L D L^T - sigma I = U(-) D(-) U(-)^T,
!  (c) Computation of the diagonal elements of the inverse of
!      L D L^T - sigma I by combining the above transforms, and choosing
!      r as the index where the diagonal of the inverse is (one of the)
!      largest in magnitude.
!  (d) Computation of the (scaled) r-th column of the inverse using the
!      twisted factorization obtained by combining the top part of the
!      the stationary and the bottom part of the progressive transform.
!
!  Arguments
!  =========
!
!  N        (input) INTEGER
!           The order of the matrix L D L^T.
!
!  B1       (input) INTEGER
!           First index of the submatrix of L D L^T.
!
!  BN       (input) INTEGER
!           Last index of the submatrix of L D L^T.
!
!  LAMBDA    (input) DOUBLE PRECISION
!           The shift. In order to compute an accurate eigenvector,
!           LAMBDA should be a good approximation to an eigenvalue
!           of L D L^T.
!
!  L        (input) DOUBLE PRECISION array, dimension (N-1)
!           The (n-1) subdiagonal elements of the unit bidiagonal matrix
!           L, in elements 1 to N-1.
!
!  D        (input) DOUBLE PRECISION array, dimension (N)
!           The n diagonal elements of the diagonal matrix D.
!
!  LD       (input) DOUBLE PRECISION array, dimension (N-1)
!           The n-1 elements L(i)*D(i).
!
!  LLD      (input) DOUBLE PRECISION array, dimension (N-1)
!           The n-1 elements L(i)*L(i)*D(i).
!
!  PIVMIN   (input) DOUBLE PRECISION
!           The minimum pivot in the Sturm sequence.
!
!  GAPTOL   (input) DOUBLE PRECISION
!           Tolerance that indicates when eigenvector entries are negligible
!           w.r.t. their contribution to the residual.
!
!  Z        (input/output) DOUBLE PRECISION array, dimension (N)
!           On input, all entries of Z must be set to 0.
!           On output, Z contains the (scaled) r-th column of the
!           inverse. The scaling is such that Z(R) equals 1.
!
!  WANTNC   (input) LOGICAL
!           Specifies whether NEGCNT has to be computed.
!
!  NEGCNT   (output) INTEGER
!           If WANTNC is .TRUE. then NEGCNT = the number of pivots < pivmin
!           in the  matrix factorization L D L^T, and NEGCNT = -1 otherwise.
!
!  ZTZ      (output) DOUBLE PRECISION
!           The square of the 2-norm of Z.
!
!  MINGMA   (output) DOUBLE PRECISION
!           The reciprocal of the largest (in magnitude) diagonal
!           element of the inverse of L D L^T - sigma I.
!
!  R        (input/output) INTEGER
!           The twist index for the twisted factorization used to
!           compute Z.
!           On input, 0 <= R <= N. If R is input as 0, R is set to
!           the index where (L D L^T - sigma I)^{-1} is largest
!           in magnitude. If 1 <= R <= N, R is unchanged.
!           On output, R contains the twist index used to compute Z.
!           Ideally, R designates the position of the maximum entry in the
!           eigenvector.
!
!  ISUPPZ   (output) INTEGER array, dimension (2)
!           The support of the vector in Z, i.e., the vector Z is
!           nonzero only in elements ISUPPZ(1) through ISUPPZ( 2 ).
!
!  NRMINV   (output) DOUBLE PRECISION
!           NRMINV = 1/SQRT( ZTZ )
!
!  RESID    (output) DOUBLE PRECISION
!           The residual of the FP vector.
!           RESID = ABS( MINGMA )/SQRT( ZTZ )
!
!  RQCORR   (output) DOUBLE PRECISION
!           The Rayleigh Quotient correction to LAMBDA.
!           RQCORR = MINGMA*TMP
!
!  WORK     (workspace) DOUBLE PRECISION array, dimension (4*N)
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )

!     ..
!     .. Local Scalars ..
      LOGICAL            SAWNAN1, SAWNAN2
      INTEGER            I, INDLPL, INDP, INDS, INDUMN, NEG1, NEG2, R1, &
                         R2
      DOUBLE PRECISION   DMINUS, DPLUS, EPS, S, TMP
!     ..
!     .. External Functions ..
      LOGICAL DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DISNAN, DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      EPS = DLAMCH( 'Precision' )


      IF( R.EQ.0 ) THEN
         R1 = B1
         R2 = BN
      ELSE
         R1 = R
         R2 = R
      ENDIF

!     Storage for LPLUS
      INDLPL = 0
!     Storage for UMINUS
      INDUMN = N
      INDS = 2*N + 1
      INDP = 3*N + 1

      IF( B1.EQ.1 ) THEN
         WORK( INDS ) = ZERO
      ELSE
         WORK( INDS+B1-1 ) = LLD( B1-1 )
      ENDIF

!
!     Compute the stationary transform (using the differential form)
!     until the index R2.
!
      SAWNAN1 = .FALSE.
      NEG1 = 0
      S = WORK( INDS+B1-1 ) - LAMBDA
      DO 50 I = B1, R1 - 1
         DPLUS = D( I ) + S
         WORK( INDLPL+I ) = LD( I ) / DPLUS
         IF(DPLUS.LT.ZERO) NEG1 = NEG1 + 1
         WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
         S = WORK( INDS+I ) - LAMBDA
 50   CONTINUE
      SAWNAN1 = DISNAN( S )
      IF( SAWNAN1 ) GOTO 60
      DO 51 I = R1, R2 - 1
         DPLUS = D( I ) + S
         WORK( INDLPL+I ) = LD( I ) / DPLUS
         WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
         S = WORK( INDS+I ) - LAMBDA
 51   CONTINUE
      SAWNAN1 = DISNAN( S )
!
 60   CONTINUE
      IF( SAWNAN1 ) THEN
!        Runs a slower version of the above loop if a NaN is detected
         NEG1 = 0
         S = WORK( INDS+B1-1 ) - LAMBDA
         DO 70 I = B1, R1 - 1
            DPLUS = D( I ) + S
            IF(ABS(DPLUS).LT.PIVMIN) DPLUS = -PIVMIN
            WORK( INDLPL+I ) = LD( I ) / DPLUS
            IF(DPLUS.LT.ZERO) NEG1 = NEG1 + 1
            WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
            IF( WORK( INDLPL+I ).EQ.ZERO ) &
                            WORK( INDS+I ) = LLD( I )
            S = WORK( INDS+I ) - LAMBDA
 70      CONTINUE
         DO 71 I = R1, R2 - 1
            DPLUS = D( I ) + S
            IF(ABS(DPLUS).LT.PIVMIN) DPLUS = -PIVMIN
            WORK( INDLPL+I ) = LD( I ) / DPLUS
            WORK( INDS+I ) = S*WORK( INDLPL+I )*L( I )
            IF( WORK( INDLPL+I ).EQ.ZERO ) &
                            WORK( INDS+I ) = LLD( I )
            S = WORK( INDS+I ) - LAMBDA
 71      CONTINUE
      ENDIF
!
!     Compute the progressive transform (using the differential form)
!     until the index R1
!
      SAWNAN2 = .FALSE.
      NEG2 = 0
      WORK( INDP+BN-1 ) = D( BN ) - LAMBDA
      DO 80 I = BN - 1, R1, -1
         DMINUS = LLD( I ) + WORK( INDP+I )
         TMP = D( I ) / DMINUS
         IF(DMINUS.LT.ZERO) NEG2 = NEG2 + 1
         WORK( INDUMN+I ) = L( I )*TMP
         WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - LAMBDA
 80   CONTINUE
      TMP = WORK( INDP+R1-1 )
      SAWNAN2 = DISNAN( TMP )

      IF( SAWNAN2 ) THEN
!        Runs a slower version of the above loop if a NaN is detected
         NEG2 = 0
         DO 100 I = BN-1, R1, -1
            DMINUS = LLD( I ) + WORK( INDP+I )
            IF(ABS(DMINUS).LT.PIVMIN) DMINUS = -PIVMIN
            TMP = D( I ) / DMINUS
            IF(DMINUS.LT.ZERO) NEG2 = NEG2 + 1
            WORK( INDUMN+I ) = L( I )*TMP
            WORK( INDP+I-1 ) = WORK( INDP+I )*TMP - LAMBDA
            IF( TMP.EQ.ZERO ) &
                WORK( INDP+I-1 ) = D( I ) - LAMBDA
 100     CONTINUE
      ENDIF
!
!     Find the index (from R1 to R2) of the largest (in magnitude)
!     diagonal element of the inverse
!
      MINGMA = WORK( INDS+R1-1 ) + WORK( INDP+R1-1 )
      IF( MINGMA.LT.ZERO ) NEG1 = NEG1 + 1
      IF( WANTNC ) THEN
         NEGCNT = NEG1 + NEG2
      ELSE
         NEGCNT = -1
      ENDIF
      IF( ABS(MINGMA).EQ.ZERO ) &
         MINGMA = EPS*WORK( INDS+R1-1 )
      R = R1
      DO 110 I = R1, R2 - 1
         TMP = WORK( INDS+I ) + WORK( INDP+I )
         IF( TMP.EQ.ZERO ) &
            TMP = EPS*WORK( INDS+I )
         IF( ABS( TMP ).LE.ABS( MINGMA ) ) THEN
            MINGMA = TMP
            R = I + 1
         ENDIF
 110  CONTINUE
!
!     Compute the FP vector: solve N^T v = e_r
!
      ISUPPZ( 1 ) = B1
      ISUPPZ( 2 ) = BN
      Z( R ) = ONE
      ZTZ = ONE
!
!     Compute the FP vector upwards from R
!
      IF( .NOT.SAWNAN1 .AND. .NOT.SAWNAN2 ) THEN
         DO 210 I = R-1, B1, -1
            Z( I ) = -( WORK( INDLPL+I )*Z( I+1 ) )
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL ) &
                 THEN
               Z( I ) = ZERO
               ISUPPZ( 1 ) = I + 1
               GOTO 220
            ENDIF
            ZTZ = ZTZ + Z( I )*Z( I )
 210     CONTINUE
 220     CONTINUE
      ELSE
!        Run slower loop if NaN occurred.
         DO 230 I = R - 1, B1, -1
            IF( Z( I+1 ).EQ.ZERO ) THEN
               Z( I ) = -( LD( I+1 ) / LD( I ) )*Z( I+2 )
            ELSE
               Z( I ) = -( WORK( INDLPL+I )*Z( I+1 ) )
            ENDIF
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL ) &
                 THEN
               Z( I ) = ZERO
               ISUPPZ( 1 ) = I + 1
               GO TO 240
            ENDIF
            ZTZ = ZTZ + Z( I )*Z( I )
 230     CONTINUE
 240     CONTINUE
      ENDIF

!     Compute the FP vector downwards from R in blocks of size BLKSIZ
      IF( .NOT.SAWNAN1 .AND. .NOT.SAWNAN2 ) THEN
         DO 250 I = R, BN-1
            Z( I+1 ) = -( WORK( INDUMN+I )*Z( I ) )
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL ) &
               THEN
               Z( I+1 ) = ZERO
               ISUPPZ( 2 ) = I
               GO TO 260
            ENDIF
            ZTZ = ZTZ + Z( I+1 )*Z( I+1 )
 250     CONTINUE
 260     CONTINUE
      ELSE
!        Run slower loop if NaN occurred.
         DO 270 I = R, BN - 1
            IF( Z( I ).EQ.ZERO ) THEN
               Z( I+1 ) = -( LD( I-1 ) / LD( I ) )*Z( I-1 )
            ELSE
               Z( I+1 ) = -( WORK( INDUMN+I )*Z( I ) )
            ENDIF
            IF( (ABS(Z(I))+ABS(Z(I+1)))* ABS(LD(I)).LT.GAPTOL ) &
                 THEN
               Z( I+1 ) = ZERO
               ISUPPZ( 2 ) = I
               GO TO 280
            ENDIF
            ZTZ = ZTZ + Z( I+1 )*Z( I+1 )
 270     CONTINUE
 280     CONTINUE
      ENDIF
!
!     Compute quantities for convergence test
!
      TMP = ONE / ZTZ
      NRMINV = SQRT( TMP )
      RESID = ABS( MINGMA )*NRMINV
      RQCORR = MINGMA*TMP
!
!
      RETURN
!
!     End of DLAR1V
!
      END SUBROUTINE DLAR1V
      
      
      
      
      
      SUBROUTINE DLARF( SIDE, M, N, V, INCV, TAU, C, LDC, WORK )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE
      INTEGER            INCV, LDC, M, N
      DOUBLE PRECISION   TAU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), V( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DLARF applies a real elementary reflector H to a real m by n matrix
!  C, from either the left or the right. H is represented in the form
!
!        H = I - tau * v * v'
!
!  where tau is a real scalar and v is a real vector.
!
!  If tau = 0, then H is taken to be the unit matrix.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': form  H * C
!          = 'R': form  C * H
!
!  M       (input) INTEGER
!          The number of rows of the matrix C.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C.
!
!  V       (input) DOUBLE PRECISION array, dimension
!                     (1 + (M-1)*abs(INCV)) if SIDE = 'L'
!                  or (1 + (N-1)*abs(INCV)) if SIDE = 'R'
!          The vector v in the representation of H. V is not used if
!          TAU = 0.
!
!  INCV    (input) INTEGER
!          The increment between elements of v. INCV <> 0.
!
!  TAU     (input) DOUBLE PRECISION
!          The value tau in the representation of H.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
!          or C * H if SIDE = 'R'.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension
!                         (N) if SIDE = 'L'
!                      or (M) if SIDE = 'R'
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMV, DGER
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Executable Statements ..
!
      IF( LSAME( SIDE, 'L' ) ) THEN
!
!        Form  H * C
!
         IF( TAU.NE.ZERO ) THEN
!
!           w := C' * v
!
            CALL DGEMV( 'Transpose', M, N, ONE, C, LDC, V, INCV, ZERO, &
                        WORK, 1 )
!
!           C := C - v * w'
!
            CALL DGER( M, N, -TAU, V, INCV, WORK, 1, C, LDC )
         ENDIF
      ELSE
!
!        Form  C * H
!
         IF( TAU.NE.ZERO ) THEN
!
!           w := C * v
!
            CALL DGEMV( 'No transpose', M, N, ONE, C, LDC, V, INCV, &
                        ZERO, WORK, 1 )
!
!           C := C - w * v'
!
            CALL DGER( M, N, -TAU, WORK, 1, V, INCV, C, LDC )
         ENDIF
      ENDIF
      RETURN
!
!     End of DLARF
!
      END SUBROUTINE DLARF
      
      
      
      
      
      SUBROUTINE DLARFB( SIDE, TRANS, DIRECT, STOREV, M, N, K, V, LDV, &
                         T, LDT, C, LDC, WORK, LDWORK )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIRECT, SIDE, STOREV, TRANS
      INTEGER            K, LDC, LDT, LDV, LDWORK, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   C( LDC, * ), T( LDT, * ), V( LDV, * ), &
                         WORK( LDWORK, * )
!     ..
!
!  Purpose
!  =======
!
!  DLARFB applies a real block reflector H or its transpose H' to a
!  real m by n matrix C, from either the left or the right.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply H or H' from the Left
!          = 'R': apply H or H' from the Right
!
!  TRANS   (input) CHARACTER*1
!          = 'N': apply H (No transpose)
!          = 'T': apply H' (Transpose)
!
!  DIRECT  (input) CHARACTER*1
!          Indicates how H is formed from a product of elementary
!          reflectors
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!
!  STOREV  (input) CHARACTER*1
!          Indicates how the vectors which define the elementary
!          reflectors are stored:
!          = 'C': Columnwise
!          = 'R': Rowwise
!
!  M       (input) INTEGER
!          The number of rows of the matrix C.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C.
!
!  K       (input) INTEGER
!          The order of the matrix T (= the number of elementary
!          reflectors whose product defines the block reflector).
!
!  V       (input) DOUBLE PRECISION array, dimension
!                                (LDV,K) if STOREV = 'C'
!                                (LDV,M) if STOREV = 'R' and SIDE = 'L'
!                                (LDV,N) if STOREV = 'R' and SIDE = 'R'
!          The matrix V. See further details.
!
!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C' and SIDE = 'L', LDV >= max(1,M);
!          if STOREV = 'C' and SIDE = 'R', LDV >= max(1,N);
!          if STOREV = 'R', LDV >= K.
!
!  T       (input) DOUBLE PRECISION array, dimension (LDT,K)
!          The triangular k by k matrix T in the representation of the
!          block reflector.
!
!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by H*C or H'*C or C*H or C*H'.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDA >= max(1,M).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (LDWORK,K)
!
!  LDWORK  (input) INTEGER
!          The leading dimension of the array WORK.
!          If SIDE = 'L', LDWORK >= max(1,N);
!          if SIDE = 'R', LDWORK >= max(1,M).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      CHARACTER          TRANST
      INTEGER            I, J
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY, DGEMM, DTRMM
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( M.LE.0 .OR. N.LE.0 ) &
         RETURN
!
      IF( LSAME( TRANS, 'N' ) ) THEN
         TRANST = 'T'
      ELSE
         TRANST = 'N'
      ENDIF
!
      IF( LSAME( STOREV, 'C' ) ) THEN
!
         IF( LSAME( DIRECT, 'F' ) ) THEN
!
!           Let  V =  ( V1 )    (first K rows)
!                     ( V2 )
!           where  V1  is unit lower triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
!
!              W := C1'
!
               DO 10 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
   10          CONTINUE
!
!              W := W * V1
!
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N, &
                           K, ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
!
!                 W := W + C2'*V2
!
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K, &
                              ONE, C( K+1, 1 ), LDC, V( K+1, 1 ), LDV, &
                              ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T'  or  W * T
!
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V * W'
!
               IF( M.GT.K ) THEN
!
!                 C2 := C2 - V2 * W'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K, &
                              -ONE, V( K+1, 1 ), LDV, WORK, LDWORK, ONE, &
                              C( K+1, 1 ), LDC )
               ENDIF
!
!              W := W * V1'
!
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K, &
                           ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W'
!
               DO 30 J = 1, K
                  DO 20 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
   20             CONTINUE
   30          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
!
!              W := C1
!
               DO 40 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
   40          CONTINUE
!
!              W := W * V1
!
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M, &
                           K, ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
!
!                 W := W + C2 * V2
!
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K, &
                              ONE, C( 1, K+1 ), LDC, V( K+1, 1 ), LDV, &
                              ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T  or  W * T'
!
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V'
!
               IF( N.GT.K ) THEN
!
!                 C2 := C2 - W * V2'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K, &
                              -ONE, WORK, LDWORK, V( K+1, 1 ), LDV, ONE, &
                              C( 1, K+1 ), LDC )
               ENDIF
!
!              W := W * V1'
!
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K, &
                           ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 60 J = 1, K
                  DO 50 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
   50             CONTINUE
   60          CONTINUE
            ENDIF
!
         ELSE
!
!           Let  V =  ( V1 )
!                     ( V2 )    (last K rows)
!           where  V2  is unit upper triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
!              W := C' * V  =  (C1'*V1 + C2'*V2)  (stored in WORK)
!
!              W := C2'
!
               DO 70 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
   70          CONTINUE
!
!              W := W * V2
!
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N, &
                           K, ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
!
!                 W := W + C1'*V1
!
                  CALL DGEMM( 'Transpose', 'No transpose', N, K, M-K, &
                              ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T'  or  W * T
!
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V * W'
!
               IF( M.GT.K ) THEN
!
!                 C1 := C1 - V1 * W'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M-K, N, K, &
                              -ONE, V, LDV, WORK, LDWORK, ONE, C, LDC )
               ENDIF
!
!              W := W * V2'
!
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K, &
                           ONE, V( M-K+1, 1 ), LDV, WORK, LDWORK )
!
!              C2 := C2 - W'
!
               DO 90 J = 1, K
                  DO 80 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
   80             CONTINUE
   90          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
!              W := C * V  =  (C1*V1 + C2*V2)  (stored in WORK)
!
!              W := C2
!
               DO 100 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  100          CONTINUE
!
!              W := W * V2
!
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M, &
                           K, ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
!
!                 W := W + C1 * V1
!
                  CALL DGEMM( 'No transpose', 'No transpose', M, K, N-K, &
                              ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T  or  W * T'
!
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V'
!
               IF( N.GT.K ) THEN
!
!                 C1 := C1 - W * V1'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M, N-K, K, &
                              -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               ENDIF
!
!              W := W * V2'
!
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K, &
                           ONE, V( N-K+1, 1 ), LDV, WORK, LDWORK )
!
!              C2 := C2 - W
!
               DO 120 J = 1, K
                  DO 110 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  110             CONTINUE
  120          CONTINUE
            ENDIF
         ENDIF
!
      ELSE IF( LSAME( STOREV, 'R' ) ) THEN
!
         IF( LSAME( DIRECT, 'F' ) ) THEN
!
!           Let  V =  ( V1  V2 )    (V1: first K columns)
!           where  V1  is unit upper triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
!
!              W := C1'
!
               DO 130 J = 1, K
                  CALL DCOPY( N, C( J, 1 ), LDC, WORK( 1, J ), 1 )
  130          CONTINUE
!
!              W := W * V1'
!
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', N, K, &
                           ONE, V, LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
!
!                 W := W + C2'*V2'
!
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE, &
                              C( K+1, 1 ), LDC, V( 1, K+1 ), LDV, ONE, &
                              WORK, LDWORK )
               ENDIF
!
!              W := W * T'  or  W * T
!
               CALL DTRMM( 'Right', 'Upper', TRANST, 'Non-unit', N, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V' * W'
!
               IF( M.GT.K ) THEN
!
!                 C2 := C2 - V2' * W'
!
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE, &
                              V( 1, K+1 ), LDV, WORK, LDWORK, ONE, &
                              C( K+1, 1 ), LDC )
               ENDIF
!
!              W := W * V1
!
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', N, &
                           K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W'
!
               DO 150 J = 1, K
                  DO 140 I = 1, N
                     C( J, I ) = C( J, I ) - WORK( I, J )
  140             CONTINUE
  150          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
!
!              W := C1
!
               DO 160 J = 1, K
                  CALL DCOPY( M, C( 1, J ), 1, WORK( 1, J ), 1 )
  160          CONTINUE
!
!              W := W * V1'
!
               CALL DTRMM( 'Right', 'Upper', 'Transpose', 'Unit', M, K, &
                           ONE, V, LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
!
!                 W := W + C2 * V2'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K, &
                              ONE, C( 1, K+1 ), LDC, V( 1, K+1 ), LDV, &
                              ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T  or  W * T'
!
               CALL DTRMM( 'Right', 'Upper', TRANS, 'Non-unit', M, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V
!
               IF( N.GT.K ) THEN
!
!                 C2 := C2 - W * V2
!
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K, &
                              -ONE, WORK, LDWORK, V( 1, K+1 ), LDV, ONE, &
                              C( 1, K+1 ), LDC )
               ENDIF
!
!              W := W * V1
!
               CALL DTRMM( 'Right', 'Upper', 'No transpose', 'Unit', M, &
                           K, ONE, V, LDV, WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 180 J = 1, K
                  DO 170 I = 1, M
                     C( I, J ) = C( I, J ) - WORK( I, J )
  170             CONTINUE
  180          CONTINUE
!
            ENDIF
!
         ELSE
!
!           Let  V =  ( V1  V2 )    (V2: last K columns)
!           where  V2  is unit lower triangular.
!
            IF( LSAME( SIDE, 'L' ) ) THEN
!
!              Form  H * C  or  H' * C  where  C = ( C1 )
!                                                  ( C2 )
!
!              W := C' * V'  =  (C1'*V1' + C2'*V2') (stored in WORK)
!
!              W := C2'
!
               DO 190 J = 1, K
                  CALL DCOPY( N, C( M-K+J, 1 ), LDC, WORK( 1, J ), 1 )
  190          CONTINUE
!
!              W := W * V2'
!
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', N, K, &
                           ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
               IF( M.GT.K ) THEN
!
!                 W := W + C1'*V1'
!
                  CALL DGEMM( 'Transpose', 'Transpose', N, K, M-K, ONE, &
                              C, LDC, V, LDV, ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T'  or  W * T
!
               CALL DTRMM( 'Right', 'Lower', TRANST, 'Non-unit', N, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - V' * W'
!
               IF( M.GT.K ) THEN
!
!                 C1 := C1 - V1' * W'
!
                  CALL DGEMM( 'Transpose', 'Transpose', M-K, N, K, -ONE, &
                              V, LDV, WORK, LDWORK, ONE, C, LDC )
               ENDIF
!
!              W := W * V2
!
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', N, &
                           K, ONE, V( 1, M-K+1 ), LDV, WORK, LDWORK )
!
!              C2 := C2 - W'
!
               DO 210 J = 1, K
                  DO 200 I = 1, N
                     C( M-K+J, I ) = C( M-K+J, I ) - WORK( I, J )
  200             CONTINUE
  210          CONTINUE
!
            ELSE IF( LSAME( SIDE, 'R' ) ) THEN
!
!              Form  C * H  or  C * H'  where  C = ( C1  C2 )
!
!              W := C * V'  =  (C1*V1' + C2*V2')  (stored in WORK)
!
!              W := C2
!
               DO 220 J = 1, K
                  CALL DCOPY( M, C( 1, N-K+J ), 1, WORK( 1, J ), 1 )
  220          CONTINUE
!
!              W := W * V2'
!
               CALL DTRMM( 'Right', 'Lower', 'Transpose', 'Unit', M, K, &
                           ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
               IF( N.GT.K ) THEN
!
!                 W := W + C1 * V1'
!
                  CALL DGEMM( 'No transpose', 'Transpose', M, K, N-K, &
                              ONE, C, LDC, V, LDV, ONE, WORK, LDWORK )
               ENDIF
!
!              W := W * T  or  W * T'
!
               CALL DTRMM( 'Right', 'Lower', TRANS, 'Non-unit', M, K, &
                           ONE, T, LDT, WORK, LDWORK )
!
!              C := C - W * V
!
               IF( N.GT.K ) THEN
!
!                 C1 := C1 - W * V1
!
                  CALL DGEMM( 'No transpose', 'No transpose', M, N-K, K, &
                              -ONE, WORK, LDWORK, V, LDV, ONE, C, LDC )
               ENDIF
!
!              W := W * V2
!
               CALL DTRMM( 'Right', 'Lower', 'No transpose', 'Unit', M, &
                           K, ONE, V( 1, N-K+1 ), LDV, WORK, LDWORK )
!
!              C1 := C1 - W
!
               DO 240 J = 1, K
                  DO 230 I = 1, M
                     C( I, N-K+J ) = C( I, N-K+J ) - WORK( I, J )
  230             CONTINUE
  240          CONTINUE
!
            ENDIF
!
         ENDIF
      ENDIF
!
      RETURN
!
!     End of DLARFB
!
      END SUBROUTINE DLARFB
      
      
      
      
      
      SUBROUTINE DLARFG( N, ALPHA, X, INCX, TAU )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   ALPHA, TAU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!     ..
!
!  Purpose
!  =======
!
!  DLARFG generates a real elementary reflector H of order n, such
!  that
!
!        H * ( alpha ) = ( beta ),   H' * H = I.
!            (   x   )   (   0  )
!
!  where alpha and beta are scalars, and x is an (n-1)-element real
!  vector. H is represented in the form
!
!        H = I - tau * ( 1 ) * ( 1 v' ) ,
!                      ( v )
!
!  where tau is a real scalar and v is a real (n-1)-element
!  vector.
!
!  If the elements of x are all zero, then tau = 0 and H is taken to be
!  the unit matrix.
!
!  Otherwise  1 <= tau <= 2.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the elementary reflector.
!
!  ALPHA   (input/output) DOUBLE PRECISION
!          On entry, the value alpha.
!          On exit, it is overwritten with the value beta.
!
!  X       (input/output) DOUBLE PRECISION array, dimension
!                         (1+(N-2)*abs(INCX))
!          On entry, the vector x.
!          On exit, it is overwritten with the vector v.
!
!  INCX    (input) INTEGER
!          The increment between elements of X. INCX > 0.
!
!  TAU     (output) DOUBLE PRECISION
!          The value tau.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            J, KNT
      DOUBLE PRECISION   BETA, RSAFMN, SAFMIN, XNORM
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLAPY2, DNRM2
      EXTERNAL           DLAMCH, DLAPY2, DNRM2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN
!     ..
!     .. External Subroutines ..
      EXTERNAL           DSCAL
!     ..
!     .. Executable Statements ..
!
      IF( N.LE.1 ) THEN
         TAU = ZERO
         RETURN
      ENDIF
!
      XNORM = DNRM2( N-1, X, INCX )
!
      IF( XNORM.EQ.ZERO ) THEN
!
!        H  =  I
!
         TAU = ZERO
      ELSE
!
!        general case
!
         BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
         SAFMIN = DLAMCH( 'S' ) / DLAMCH( 'E' )
         IF( ABS( BETA ).LT.SAFMIN ) THEN
!
!           XNORM, BETA may be inaccurate; scale X and recompute them
!
            RSAFMN = ONE / SAFMIN
            KNT = 0
   10       CONTINUE
            KNT = KNT + 1
            CALL DSCAL( N-1, RSAFMN, X, INCX )
            BETA = BETA*RSAFMN
            ALPHA = ALPHA*RSAFMN
            IF( ABS( BETA ).LT.SAFMIN ) &
               GO TO 10
!
!           New BETA is at most 1, at least SAFMIN
!
            XNORM = DNRM2( N-1, X, INCX )
            BETA = -SIGN( DLAPY2( ALPHA, XNORM ), ALPHA )
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
!
!           If ALPHA is subnormal, it may lose relative accuracy
!
            ALPHA = BETA
            DO 20 J = 1, KNT
               ALPHA = ALPHA*SAFMIN
   20       CONTINUE
         ELSE
            TAU = ( BETA-ALPHA ) / BETA
            CALL DSCAL( N-1, ONE / ( ALPHA-BETA ), X, INCX )
            ALPHA = BETA
         ENDIF
      ENDIF
!
      RETURN
!
!     End of DLARFG
!
      END SUBROUTINE DLARFG
      
      
      
      
      
      SUBROUTINE DLARFT( DIRECT, STOREV, N, K, V, LDV, TAU, T, LDT )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          DIRECT, STOREV
      INTEGER            K, LDT, LDV, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   T( LDT, * ), TAU( * ), V( LDV, * )
!     ..
!
!  Purpose
!  =======
!
!  DLARFT forms the triangular factor T of a real block reflector H
!  of order n, which is defined as a product of k elementary reflectors.
!
!  If DIRECT = 'F', H = H(1) H(2) . . . H(k) and T is upper triangular;
!
!  If DIRECT = 'B', H = H(k) . . . H(2) H(1) and T is lower triangular.
!
!  If STOREV = 'C', the vector which defines the elementary reflector
!  H(i) is stored in the i-th column of the array V, and
!
!     H  =  I - V * T * V'
!
!  If STOREV = 'R', the vector which defines the elementary reflector
!  H(i) is stored in the i-th row of the array V, and
!
!     H  =  I - V' * T * V
!
!  Arguments
!  =========
!
!  DIRECT  (input) CHARACTER*1
!          Specifies the order in which the elementary reflectors are
!          multiplied to form the block reflector:
!          = 'F': H = H(1) H(2) . . . H(k) (Forward)
!          = 'B': H = H(k) . . . H(2) H(1) (Backward)
!
!  STOREV  (input) CHARACTER*1
!          Specifies how the vectors which define the elementary
!          reflectors are stored (see also Further Details):
!          = 'C': columnwise
!          = 'R': rowwise
!
!  N       (input) INTEGER
!          The order of the block reflector H. N >= 0.
!
!  K       (input) INTEGER
!          The order of the triangular factor T (= the number of
!          elementary reflectors). K >= 1.
!
!  V       (input/output) DOUBLE PRECISION array, dimension
!                               (LDV,K) if STOREV = 'C'
!                               (LDV,N) if STOREV = 'R'
!          The matrix V. See further details.
!
!  LDV     (input) INTEGER
!          The leading dimension of the array V.
!          If STOREV = 'C', LDV >= max(1,N); if STOREV = 'R', LDV >= K.
!
!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i).
!
!  T       (output) DOUBLE PRECISION array, dimension (LDT,K)
!          The k by k triangular factor T of the block reflector.
!          If DIRECT = 'F', T is upper triangular; if DIRECT = 'B', T is
!          lower triangular. The rest of the array is not used.
!
!  LDT     (input) INTEGER
!          The leading dimension of the array T. LDT >= K.
!
!  Further Details
!  ===============
!
!  The shape of the matrix V and the storage of the vectors which define
!  the H(i) is best illustrated by the following example with n = 5 and
!  k = 3. The elements equal to 1 are not stored; the corresponding
!  array elements are modified but restored on exit. The rest of the
!  array is not used.
!
!  DIRECT = 'F' and STOREV = 'C':         DIRECT = 'F' and STOREV = 'R':
!
!               V = (  1       )                 V = (  1 v1 v1 v1 v1 )
!                   ( v1  1    )                     (     1 v2 v2 v2 )
!                   ( v1 v2  1 )                     (        1 v3 v3 )
!                   ( v1 v2 v3 )
!                   ( v1 v2 v3 )
!
!  DIRECT = 'B' and STOREV = 'C':         DIRECT = 'B' and STOREV = 'R':
!
!               V = ( v1 v2 v3 )                 V = ( v1 v1  1       )
!                   ( v1 v2 v3 )                     ( v2 v2 v2  1    )
!                   (  1 v2 v3 )                     ( v3 v3 v3 v3  1 )
!                   (     1 v3 )
!                   (        1 )
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, J
      DOUBLE PRECISION   VII
!     ..
!     .. External Subroutines ..
      EXTERNAL           DGEMV, DTRMV
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( N.EQ.0 ) &
         RETURN
!
      IF( LSAME( DIRECT, 'F' ) ) THEN
         DO 20 I = 1, K
            IF( TAU( I ).EQ.ZERO ) THEN
!
!              H(i)  =  I
!
               DO 10 J = 1, I
                  T( J, I ) = ZERO
   10          CONTINUE
            ELSE
!
!              general case
!
               VII = V( I, I )
               V( I, I ) = ONE
               IF( LSAME( STOREV, 'C' ) ) THEN
!
!                 T(1:i-1,i) := - tau(i) * V(i:n,1:i-1)' * V(i:n,i)
!
                  CALL DGEMV( 'Transpose', N-I+1, I-1, -TAU( I ), &
                              V( I, 1 ), LDV, V( I, I ), 1, ZERO, &
                              T( 1, I ), 1 )
               ELSE
!
!                 T(1:i-1,i) := - tau(i) * V(1:i-1,i:n) * V(i,i:n)'
!
                  CALL DGEMV( 'No transpose', I-1, N-I+1, -TAU( I ), &
                              V( 1, I ), LDV, V( I, I ), LDV, ZERO, &
                              T( 1, I ), 1 )
               ENDIF
               V( I, I ) = VII
!
!              T(1:i-1,i) := T(1:i-1,1:i-1) * T(1:i-1,i)
!
               CALL DTRMV( 'Upper', 'No transpose', 'Non-unit', I-1, T, &
                           LDT, T( 1, I ), 1 )
               T( I, I ) = TAU( I )
            ENDIF
   20    CONTINUE
      ELSE
         DO 40 I = K, 1, -1
            IF( TAU( I ).EQ.ZERO ) THEN
!
!              H(i)  =  I
!
               DO 30 J = I, K
                  T( J, I ) = ZERO
   30          CONTINUE
            ELSE
!
!              general case
!
               IF( I.LT.K ) THEN
                  IF( LSAME( STOREV, 'C' ) ) THEN
                     VII = V( N-K+I, I )
                     V( N-K+I, I ) = ONE
!
!                    T(i+1:k,i) :=
!                            - tau(i) * V(1:n-k+i,i+1:k)' * V(1:n-k+i,i)
!
                     CALL DGEMV( 'Transpose', N-K+I, K-I, -TAU( I ), &
                                 V( 1, I+1 ), LDV, V( 1, I ), 1, ZERO, &
                                 T( I+1, I ), 1 )
                     V( N-K+I, I ) = VII
                  ELSE
                     VII = V( I, N-K+I )
                     V( I, N-K+I ) = ONE
!
!                    T(i+1:k,i) :=
!                            - tau(i) * V(i+1:k,1:n-k+i) * V(i,1:n-k+i)'
!
                     CALL DGEMV( 'No transpose', K-I, N-K+I, -TAU( I ), &
                                 V( I+1, 1 ), LDV, V( I, 1 ), LDV, ZERO, &
                                 T( I+1, I ), 1 )
                     V( I, N-K+I ) = VII
                  ENDIF
!
!                 T(i+1:k,i) := T(i+1:k,i+1:k) * T(i+1:k,i)
!
                  CALL DTRMV( 'Lower', 'No transpose', 'Non-unit', K-I, &
                              T( I+1, I+1 ), LDT, T( I+1, I ), 1 )
               ENDIF
               T( I, I ) = TAU( I )
            ENDIF
   40    CONTINUE
      ENDIF
      RETURN
!
!     End of DLARFT
!
      END SUBROUTINE DLARFT
      
      
      
      
      
      SUBROUTINE DLARNV( IDIST, ISEED, N, X )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IDIST, N
!     ..
!     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   X( * )
!     ..
!
!  Purpose
!  =======
!
!  DLARNV returns a vector of n random real numbers from a uniform or
!  normal distribution.
!
!  Arguments
!  =========
!
!  IDIST   (input) INTEGER
!          Specifies the distribution of the random numbers:
!          = 1:  uniform (0,1)
!          = 2:  uniform (-1,1)
!          = 3:  normal (0,1)
!
!  ISEED   (input/output) INTEGER array, dimension (4)
!          On entry, the seed of the random number generator; the array
!          elements must be between 0 and 4095, and ISEED(4) must be
!          odd.
!          On exit, the seed is updated.
!
!  N       (input) INTEGER
!          The number of random numbers to be generated.
!
!  X       (output) DOUBLE PRECISION array, dimension (N)
!          The generated random numbers.
!
!  Further Details
!  ===============
!
!  This routine calls the auxiliary routine DLARUV to generate random
!  real numbers from a uniform (0,1) distribution, in batches of up to
!  128 using vectorisable code. The Box-Muller method is used to
!  transform numbers from a uniform to a normal distribution.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, TWO
      PARAMETER          ( ONE = 1.0D+0, TWO = 2.0D+0 )
      INTEGER            LV
      PARAMETER          ( LV = 128 )
      DOUBLE PRECISION   TWOPI
      PARAMETER          ( TWOPI = 6.2831853071795864769252867663D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IL, IL2, IV
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   U( LV )
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          COS, LOG, MIN, SQRT
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLARUV
!     ..
!     .. Executable Statements ..
!
      DO 40 IV = 1, N, LV / 2
         IL = MIN( LV / 2, N-IV+1 )
         IF( IDIST.EQ.3 ) THEN
            IL2 = 2*IL
         ELSE
            IL2 = IL
         ENDIF
!
!        Call DLARUV to generate IL2 numbers from a uniform (0,1)
!        distribution (IL2 <= LV)
!
         CALL DLARUV( ISEED, IL2, U )
!
         IF( IDIST.EQ.1 ) THEN
!
!           Copy generated numbers
!
            DO 10 I = 1, IL
               X( IV+I-1 ) = U( I )
   10       CONTINUE
         ELSE IF( IDIST.EQ.2 ) THEN
!
!           Convert generated numbers to uniform (-1,1) distribution
!
            DO 20 I = 1, IL
               X( IV+I-1 ) = TWO*U( I ) - ONE
   20       CONTINUE
         ELSE IF( IDIST.EQ.3 ) THEN
!
!           Convert generated numbers to normal (0,1) distribution
!
            DO 30 I = 1, IL
               X( IV+I-1 ) = SQRT( -TWO*LOG( U( 2*I-1 ) ) )* &
                             COS( TWOPI*U( 2*I ) )
   30       CONTINUE
         ENDIF
   40 CONTINUE
      RETURN
!
!     End of DLARNV
!
      END SUBROUTINE DLARNV
      
      
      
      
      
      SUBROUTINE DLARRA( N, D, E, E2, SPLTOL, TNRM, &
                          NSPLIT, ISPLIT, INFO )
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, N, NSPLIT
      DOUBLE PRECISION    SPLTOL, TNRM
!     ..
!     .. Array Arguments ..
      INTEGER            ISPLIT( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * )
!     ..
!
!  Purpose
!  =======
!
!  Compute the splitting points with threshold SPLTOL.
!  DLARRA sets any "small" off-diagonal elements to zero.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix. N > 0.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          On entry, the N diagonal elements of the tridiagonal
!          matrix T.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the first (N-1) entries contain the subdiagonal
!          elements of the tridiagonal matrix T; E(N) need not be set.
!          On exit, the entries E( ISPLIT( I ) ), 1 <= I <= NSPLIT,
!          are set to zero, the other entries of E are untouched.
!
!  E2      (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the first (N-1) entries contain the SQUARES of the
!          subdiagonal elements of the tridiagonal matrix T;
!          E2(N) need not be set.
!          On exit, the entries E2( ISPLIT( I ) ),
!          1 <= I <= NSPLIT, have been set to zero
!
!  SPLTOL (input) DOUBLE PRECISION
!          The threshold for splitting. Two criteria can be used:
!          SPLTOL<0 : criterion based on absolute off-diagonal value
!          SPLTOL>0 : criterion that preserves relative accuracy
!
!  TNRM (input) DOUBLE PRECISION
!          The norm of the matrix.
!
!  NSPLIT  (output) INTEGER
!          The number of blocks T splits into. 1 <= NSPLIT <= N.
!
!  ISPLIT  (output) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into blocks.
!          The first block consists of rows/columns 1 to ISPLIT(1),
!          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
!          etc., and the NSPLIT-th consists of rows/columns
!          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
!
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I
      DOUBLE PRECISION   EABS, TMP1

!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      INFO = 0

!     Compute splitting points
      NSPLIT = 1
      IF(SPLTOL.LT.ZERO) THEN
!        Criterion based on absolute off-diagonal value
         TMP1 = ABS(SPLTOL)* TNRM
         DO 9 I = 1, N-1
            EABS = ABS( E(I) )
            IF( EABS .LE. TMP1) THEN
               E(I) = ZERO
               E2(I) = ZERO
               ISPLIT( NSPLIT ) = I
               NSPLIT = NSPLIT + 1
            ENDIF
 9       CONTINUE
      ELSE
!        Criterion that guarantees relative accuracy
         DO 10 I = 1, N-1
            EABS = ABS( E(I) )
            IF( EABS .LE. SPLTOL * SQRT(ABS(D(I)))*SQRT(ABS(D(I+1))) ) &
            THEN
               E(I) = ZERO
               E2(I) = ZERO
               ISPLIT( NSPLIT ) = I
               NSPLIT = NSPLIT + 1
            ENDIF
 10      CONTINUE
      ENDIF
      ISPLIT( NSPLIT ) = N

      RETURN
!
!     End of DLARRA
!
      END SUBROUTINE DLARRA
      
      
      
      
      
      SUBROUTINE DLARRB( N, D, LLD, IFIRST, ILAST, RTOL1, &
                         RTOL2, OFFSET, W, WGAP, WERR, WORK, IWORK, &
                         PIVMIN, SPDIAM, TWIST, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N, OFFSET, TWIST
      DOUBLE PRECISION   PIVMIN, RTOL1, RTOL2, SPDIAM
!     ..
!     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), LLD( * ), W( * ), &
                         WERR( * ), WGAP( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  Given the relatively robust representation(RRR) L D L^T, DLARRB
!  does "limited" bisection to refine the eigenvalues of L D L^T,
!  W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
!  guesses for these eigenvalues are input in W, the corresponding estimate
!  of the error in these guesses and their gaps are input in WERR
!  and WGAP, respectively. During bisection, intervals
!  [left, right] are maintained by storing their mid-points and
!  semi-widths in the arrays W and WERR respectively.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of the diagonal matrix D.
!
!  LLD     (input) DOUBLE PRECISION array, dimension (N-1)
!          The (N-1) elements L(i)*L(i)*D(i).
!
!  IFIRST  (input) INTEGER
!          The index of the first eigenvalue to be computed.
!
!  ILAST   (input) INTEGER
!          The index of the last eigenvalue to be computed.
!
!  RTOL1   (input) DOUBLE PRECISION
!  RTOL2   (input) DOUBLE PRECISION
!          Tolerance for the convergence of the bisection intervals.
!          An interval [LEFT,RIGHT] has converged if
!          RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
!          where GAP is the (estimated) distance to the nearest
!          eigenvalue.
!
!  OFFSET  (input) INTEGER
!          Offset for the arrays W, WGAP and WERR, i.e., the IFIRST-OFFSET
!          through ILAST-OFFSET elements of these arrays are to be used.
!
!  W       (input/output) DOUBLE PRECISION array, dimension (N)
!          On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
!          estimates of the eigenvalues of L D L^T indexed IFIRST throug
!          ILAST.
!          On output, these estimates are refined.
!
!  WGAP    (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On input, the (estimated) gaps between consecutive
!          eigenvalues of L D L^T, i.e., WGAP(I-OFFSET) is the gap between
!          eigenvalues I and I+1. Note that if IFIRST.EQ.ILAST
!          then WGAP(IFIRST-OFFSET) must be set to ZERO.
!          On output, these gaps are refined.
!
!  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
!          On input, WERR( IFIRST-OFFSET ) through WERR( ILAST-OFFSET ) are
!          the errors in the estimates of the corresponding elements in W.
!          On output, these errors are refined.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
!          Workspace.
!
!  IWORK   (workspace) INTEGER array, dimension (2*N)
!          Workspace.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot in the Sturm sequence.
!
!  SPDIAM  (input) DOUBLE PRECISION
!          The spectral diameter of the matrix.
!
!  TWIST   (input) INTEGER
!          The twist index for the twisted factorization that is used
!          for the negcount.
!          TWIST = N: Compute negcount from L D L^T - LAMBDA I = L+ D+ L+^T
!          TWIST = 1: Compute negcount from L D L^T - LAMBDA I = U- D- U-^T
!          TWIST = R: Compute negcount from L D L^T - LAMBDA I = N(r) D(r) N(r)
!
!  INFO    (output) INTEGER
!          Error flag.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, TWO, HALF
      PARAMETER        ( ZERO = 0.0D0, TWO = 2.0D0, &
                         HALF = 0.5D0 )
      INTEGER   MAXITR
!     ..
!     .. Local Scalars ..
      INTEGER            I, I1, II, IP, ITER, K, NEGCNT, NEXT, NINT, &
                         OLNINT, PREV, R
      DOUBLE PRECISION   BACK, CVRGD, GAP, LEFT, LGAP, MID, MNWDTH, &
                         RGAP, RIGHT, TMP, WIDTH
!     ..
!     .. External Functions ..
      INTEGER            DLANEG
      EXTERNAL           DLANEG
!
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
!     ..
!     .. Executable Statements ..
!
      INFO = 0
!
      MAXITR = INT( ( LOG( SPDIAM+PIVMIN )-LOG( PIVMIN ) ) / &
                 LOG( TWO ) ) + 2
      MNWDTH = TWO * PIVMIN
!
      R = TWIST
      IF((R.LT.1).OR.(R.GT.N)) R = N
!
!     Initialize unconverged intervals in [ WORK(2*I-1), WORK(2*I) ].
!     The Sturm Count, Count( WORK(2*I-1) ) is arranged to be I-1, while
!     Count( WORK(2*I) ) is stored in IWORK( 2*I ). The integer IWORK( 2*I-1 )
!     for an unconverged interval is set to the index of the next unconverged
!     interval, and is -1 or 0 for a converged interval. Thus a linked
!     list of unconverged intervals is set up.
!
      I1 = IFIRST
!     The number of unconverged intervals
      NINT = 0
!     The last unconverged interval found
      PREV = 0

      RGAP = WGAP( I1-OFFSET )
      DO 75 I = I1, ILAST
         K = 2*I
         II = I - OFFSET
         LEFT = W( II ) - WERR( II )
         RIGHT = W( II ) + WERR( II )
         LGAP = RGAP
         RGAP = WGAP( II )
         GAP = MIN( LGAP, RGAP )

!        Make sure that [LEFT,RIGHT] contains the desired eigenvalue
!        Compute negcount from dstqds facto L+D+L+^T = L D L^T - LEFT
!
!        Do while( NEGCNT(LEFT).GT.I-1 )
!
         BACK = WERR( II )
 20      CONTINUE
         NEGCNT = DLANEG( N, D, LLD, LEFT, PIVMIN, R )
         IF( NEGCNT.GT.I-1 ) THEN
            LEFT = LEFT - BACK
            BACK = TWO*BACK
            GO TO 20
         ENDIF
!
!        Do while( NEGCNT(RIGHT).LT.I )
!        Compute negcount from dstqds facto L+D+L+^T = L D L^T - RIGHT
!
         BACK = WERR( II )
 50      CONTINUE

         NEGCNT = DLANEG( N, D, LLD, RIGHT, PIVMIN, R )
          IF( NEGCNT.LT.I ) THEN
             RIGHT = RIGHT + BACK
             BACK = TWO*BACK
             GO TO 50
          ENDIF
         WIDTH = HALF*ABS( LEFT - RIGHT )
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )
         CVRGD = MAX(RTOL1*GAP,RTOL2*TMP)
         IF( WIDTH.LE.CVRGD .OR. WIDTH.LE.MNWDTH ) THEN
!           This interval has already converged and does not need refinement.
!           (Note that the gaps might change through refining the
!            eigenvalues, however, they can only get bigger.)
!           Remove it from the list.
            IWORK( K-1 ) = -1
!           Make sure that I1 always points to the first unconverged interval
            IF((I.EQ.I1).AND.(I.LT.ILAST)) I1 = I + 1
            IF((PREV.GE.I1).AND.(I.LE.ILAST)) IWORK( 2*PREV-1 ) = I + 1
         ELSE
!           unconverged interval found
            PREV = I
            NINT = NINT + 1
            IWORK( K-1 ) = I + 1
            IWORK( K ) = NEGCNT
         ENDIF
         WORK( K-1 ) = LEFT
         WORK( K ) = RIGHT
 75   CONTINUE

!
!     Do while( NINT.GT.0 ), i.e. there are still unconverged intervals
!     and while (ITER.LT.MAXITR)
!
      ITER = 0
 80   CONTINUE
      PREV = I1 - 1
      I = I1
      OLNINT = NINT

      DO 100 IP = 1, OLNINT
         K = 2*I
         II = I - OFFSET
         RGAP = WGAP( II )
         LGAP = RGAP
         IF(II.GT.1) LGAP = WGAP( II-1 )
         GAP = MIN( LGAP, RGAP )
         NEXT = IWORK( K-1 )
         LEFT = WORK( K-1 )
         RIGHT = WORK( K )
         MID = HALF*( LEFT + RIGHT )

!        semiwidth of interval
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )
         CVRGD = MAX(RTOL1*GAP,RTOL2*TMP)
         IF( ( WIDTH.LE.CVRGD ) .OR. ( WIDTH.LE.MNWDTH ).OR. &
             ( ITER.EQ.MAXITR ) )THEN
!           reduce number of unconverged intervals
            NINT = NINT - 1
!           Mark interval as converged.
            IWORK( K-1 ) = 0
            IF( I1.EQ.I ) THEN
               I1 = NEXT
            ELSE
!              Prev holds the last unconverged interval previously examined
               IF(PREV.GE.I1) IWORK( 2*PREV-1 ) = NEXT
            ENDIF
            I = NEXT
            GO TO 100
         ENDIF
         PREV = I
!
!        Perform one bisection step
!
         NEGCNT = DLANEG( N, D, LLD, MID, PIVMIN, R )
         IF( NEGCNT.LE.I-1 ) THEN
            WORK( K-1 ) = MID
         ELSE
            WORK( K ) = MID
         ENDIF
         I = NEXT
 100  CONTINUE
      ITER = ITER + 1
!     do another loop if there are still unconverged intervals
!     However, in the last iteration, all intervals are accepted
!     since this is the best we can do.
      IF( ( NINT.GT.0 ).AND.(ITER.LE.MAXITR) ) GO TO 80
!
!
!     At this point, all the intervals have converged
      DO 110 I = IFIRST, ILAST
         K = 2*I
         II = I - OFFSET
!        All intervals marked by '0' have been refined.
         IF( IWORK( K-1 ).EQ.0 ) THEN
            W( II ) = HALF*( WORK( K-1 )+WORK( K ) )
            WERR( II ) = WORK( K ) - W( II )
         ENDIF
 110  CONTINUE
!
      DO 111 I = IFIRST+1, ILAST
         K = 2*I
         II = I - OFFSET
         WGAP( II-1 ) = MAX( ZERO, &
                           W(II) - WERR (II) - W( II-1 ) - WERR( II-1 ))
 111  CONTINUE

      RETURN
!
!     End of DLARRB
!
      END SUBROUTINE DLARRB
      
      
      
      
      
      SUBROUTINE DLARRC( JOBT, N, VL, VU, D, E, PIVMIN, &
                                  EIGCNT, LCNT, RCNT, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          JOBT
      INTEGER            EIGCNT, INFO, LCNT, N, RCNT
      DOUBLE PRECISION   PIVMIN, VL, VU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
!     ..
!
!  Purpose
!  =======
!
!  Find the number of eigenvalues of the symmetric tridiagonal matrix T
!  that are in the interval (VL,VU] if JOBT = 'T', and of L D L^T
!  if JOBT = 'L'.
!
!  Arguments
!  =========
!
!  JOBT    (input) CHARACTER*1
!          = 'T':  Compute Sturm count for matrix T.
!          = 'L':  Compute Sturm count for matrix L D L^T.
!
!  N       (input) INTEGER
!          The order of the matrix. N > 0.
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          The lower and upper bounds for the eigenvalues.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          JOBT = 'T': The N diagonal elements of the tridiagonal matrix T.
!          JOBT = 'L': The N diagonal elements of the diagonal matrix D.
!
!  E       (input) DOUBLE PRECISION array, dimension (N)
!          JOBT = 'T': The N-1 offdiagonal elements of the matrix T.
!          JOBT = 'L': The N-1 offdiagonal elements of the matrix L.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot in the Sturm sequence for T.
!
!  EIGCNT  (output) INTEGER
!          The number of eigenvalues of the symmetric tridiagonal matrix T
!          that are in the interval (VL,VU]
!
!  LCNT    (output) INTEGER
!  RCNT    (output) INTEGER
!          The left and right negcounts of the interval.
!
!  INFO    (output) INTEGER
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I
      LOGICAL            MATT
      DOUBLE PRECISION   LPIVOT, RPIVOT, SL, SU, TMP, TMP2

!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      LCNT = 0
      RCNT = 0
      EIGCNT = 0
      MATT = LSAME( JOBT, 'T' )


      IF (MATT) THEN
!        Sturm sequence count on T
         LPIVOT = D( 1 ) - VL
         RPIVOT = D( 1 ) - VU
         IF( LPIVOT.LE.ZERO ) THEN
            LCNT = LCNT + 1
         ENDIF
         IF( RPIVOT.LE.ZERO ) THEN
            RCNT = RCNT + 1
         ENDIF
         DO 10 I = 1, N-1
            TMP = E(I)**2
            LPIVOT = ( D( I+1 )-VL ) - TMP/LPIVOT
            RPIVOT = ( D( I+1 )-VU ) - TMP/RPIVOT
            IF( LPIVOT.LE.ZERO ) THEN
               LCNT = LCNT + 1
            ENDIF
            IF( RPIVOT.LE.ZERO ) THEN
               RCNT = RCNT + 1
            ENDIF
 10      CONTINUE
      ELSE
!        Sturm sequence count on L D L^T
         SL = -VL
         SU = -VU
         DO 20 I = 1, N - 1
            LPIVOT = D( I ) + SL
            RPIVOT = D( I ) + SU
            IF( LPIVOT.LE.ZERO ) THEN
               LCNT = LCNT + 1
            ENDIF
            IF( RPIVOT.LE.ZERO ) THEN
               RCNT = RCNT + 1
            ENDIF
            TMP = E(I) * D(I) * E(I)
!
            TMP2 = TMP / LPIVOT
            IF( TMP2.EQ.ZERO ) THEN
               SL =  TMP - VL
            ELSE
               SL = SL*TMP2 - VL
            ENDIF
!
            TMP2 = TMP / RPIVOT
            IF( TMP2.EQ.ZERO ) THEN
               SU =  TMP - VU
            ELSE
               SU = SU*TMP2 - VU
            ENDIF
 20      CONTINUE
         LPIVOT = D( N ) + SL
         RPIVOT = D( N ) + SU
         IF( LPIVOT.LE.ZERO ) THEN
            LCNT = LCNT + 1
         ENDIF
         IF( RPIVOT.LE.ZERO ) THEN
            RCNT = RCNT + 1
         ENDIF
      ENDIF
      EIGCNT = RCNT - LCNT

      RETURN
!
!     end of DLARRC
!
      END SUBROUTINE DLARRC
      
      
      
      
      
      SUBROUTINE DLARRD( RANGE, ORDER, N, VL, VU, IL, IU, GERS, &
                          RELTOL, D, E, E2, PIVMIN, NSPLIT, ISPLIT, &
                          M, W, WERR, WL, WU, IBLOCK, INDEXW, &
                          WORK, IWORK, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          ORDER, RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION    PIVMIN, RELTOL, VL, VU, WL, WU
!     ..
!     .. Array Arguments ..
      INTEGER            IBLOCK( * ), INDEXW( * ), &
                         ISPLIT( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * ), &
                         GERS( * ), W( * ), WERR( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DLARRD computes the eigenvalues of a symmetric tridiagonal
!  matrix T to suitable accuracy. This is an auxiliary code to be
!  called from DSTEMR.
!  The user may ask for all eigenvalues, all eigenvalues
!  in the half-open interval (VL, VU], or the IL-th through IU-th
!  eigenvalues.
!
!  To avoid overflow, the matrix must be scaled so that its
!  largest element is no greater than overflow**(1/2) *
!  underflow**(1/4) in absolute value, and for greatest
!  accuracy, it should not be much smaller than that.
!
!  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
!  Matrix", Report CS41, Computer Science Dept., Stanford
!  University, July 21, 1966.
!
!  Arguments
!  =========
!
!  RANGE   (input) CHARACTER
!          = 'A': ("All")   all eigenvalues will be found.
!          = 'V': ("Value") all eigenvalues in the half-open interval
!                           (VL, VU] will be found.
!          = 'I': ("Index") the IL-th through IU-th eigenvalues (of the
!                           entire matrix) will be found.
!
!  ORDER   (input) CHARACTER
!          = 'B': ("By Block") the eigenvalues will be grouped by
!                              split-off block (see IBLOCK, ISPLIT) and
!                              ordered from smallest to largest within
!                              the block.
!          = 'E': ("Entire matrix")
!                              the eigenvalues for the entire matrix
!                              will be ordered from smallest to
!                              largest.
!
!  N       (input) INTEGER
!          The order of the tridiagonal matrix T.  N >= 0.
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          If RANGE='V', the lower and upper bounds of the interval to
!          be searched for eigenvalues.  Eigenvalues less than or equal
!          to VL, or greater than VU, will not be returned.  VL < VU.
!          Not referenced if RANGE = 'A' or 'I'.
!
!  IL      (input) INTEGER
!  IU      (input) INTEGER
!          If RANGE='I', the indices (in ascending order) of the
!          smallest and largest eigenvalues to be returned.
!          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
!          Not referenced if RANGE = 'A' or 'V'.
!
!  GERS    (input) DOUBLE PRECISION array, dimension (2*N)
!          The N Gerschgorin intervals (the i-th Gerschgorin interval
!          is (GERS(2*i-1), GERS(2*i)).
!
!  RELTOL  (input) DOUBLE PRECISION
!          The minimum relative width of an interval.  When an interval
!          is narrower than RELTOL times the larger (in
!          magnitude) endpoint, then it is considered to be
!          sufficiently small, i.e., converged.  Note: this should
!          always be at least radix*machine epsilon.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The n diagonal elements of the tridiagonal matrix T.
!
!  E       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) off-diagonal elements of the tridiagonal matrix T.
!
!  E2      (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot allowed in the Sturm sequence for T.
!
!  NSPLIT  (input) INTEGER
!          The number of diagonal blocks in the matrix T.
!          1 <= NSPLIT <= N.
!
!  ISPLIT  (input) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into submatrices.
!          The first submatrix consists of rows/columns 1 to ISPLIT(1),
!          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
!          etc., and the NSPLIT-th consists of rows/columns
!          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
!          (Only the first NSPLIT elements will actually be used, but
!          since the user cannot know a priori what value NSPLIT will
!          have, N words must be reserved for ISPLIT.)
!
!  M       (output) INTEGER
!          The actual number of eigenvalues found. 0 <= M <= N.
!          (See also the description of INFO=2,3.)
!
!  W       (output) DOUBLE PRECISION array, dimension (N)
!          On exit, the first M elements of W will contain the
!          eigenvalue approximations. DLARRD computes an interval
!          I_j = (a_j, b_j] that includes eigenvalue j. The eigenvalue
!          approximation is given as the interval midpoint
!          W(j)= ( a_j + b_j)/2. The corresponding error is bounded by
!          WERR(j) = abs( a_j - b_j)/2
!
!  WERR    (output) DOUBLE PRECISION array, dimension (N)
!          The error bound on the corresponding eigenvalue approximation
!          in W.
!
!  WL      (output) DOUBLE PRECISION
!  WU      (output) DOUBLE PRECISION
!          The interval (WL, WU] contains all the wanted eigenvalues.
!          If RANGE='V', then WL=VL and WU=VU.
!          If RANGE='A', then WL and WU are the global Gerschgorin bounds
!                        on the spectrum.
!          If RANGE='I', then WL and WU are computed by DLAEBZ from the
!                        index range specified.
!
!  IBLOCK  (output) INTEGER array, dimension (N)
!          At each row/column j where E(j) is zero or small, the
!          matrix T is considered to split into a block diagonal
!          matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
!          block (from 1 to the number of blocks) the eigenvalue W(i)
!          belongs.  (DLARRD may use the remaining N-M elements as
!          workspace.)
!
!  INDEXW  (output) INTEGER array, dimension (N)
!          The indices of the eigenvalues within each block (submatrix);
!          for example, INDEXW(i)= j and IBLOCK(i)=k imply that the
!          i-th eigenvalue W(i) is the j-th eigenvalue in block k.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
!
!  IWORK   (workspace) INTEGER array, dimension (3*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  some or all of the eigenvalues failed to converge or
!                were not computed:
!                =1 or 3: Bisection failed to converge for some
!                        eigenvalues; these eigenvalues are flagged by a
!                        negative block number.  The effect is that the
!                        eigenvalues may not be as accurate as the
!                        absolute and relative tolerances.  This is
!                        generally caused by unexpectedly inaccurate
!                        arithmetic.
!                =2 or 3: RANGE='I' only: Not all of the eigenvalues
!                        IL:IU were found.
!                        Effect: M < IU+1-IL
!                        Cause:  non-monotonic arithmetic, causing the
!                                Sturm sequence to be non-monotonic.
!                        Cure:   recalculate, using RANGE='A', and pick
!                                out eigenvalues IL:IU.  In some cases,
!                                increasing the PARAMETER "FUDGE" may
!                                make things work.
!                = 4:    RANGE='I', and the Gershgorin interval
!                        initially used was too small.  No eigenvalues
!                        were computed.
!                        Probable cause: your machine has sloppy
!                                        floating-point arithmetic.
!                        Cure: Increase the PARAMETER "FUDGE",
!                              recompile, and try again.
!
!  Internal Parameters
!  ===================
!
!  FUDGE   DOUBLE PRECISION, default = 2
!          A "fudge factor" to widen the Gershgorin intervals.  Ideally,
!          a value of 1 should work, but on machines with sloppy
!          arithmetic, this needs to be larger.  The default for
!          publicly released versions should be large enough to handle
!          the worst machine around.  Note that this has no effect
!          on accuracy of the solution.
!
!  Based on contributions by
!     W. Kahan, University of California, Berkeley, USA
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF, FUDGE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, &
                           TWO = 2.0D0, HALF = ONE/TWO, &
                           FUDGE = TWO )
      INTEGER   ALLRNG, VALRNG, INDRNG
      PARAMETER ( ALLRNG = 1, VALRNG = 2, INDRNG = 3 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NCNVRG, TOOFEW
      INTEGER            I, IB, IBEGIN, IDISCL, IDISCU, IE, IEND, IINFO, &
                         IM, IN, IOFF, IOUT, IRANGE, ITMAX, ITMP1, &
                         ITMP2, IW, IWOFF, J, JBLK, JDISC, JE, JEE, NB, &
                         NWL, NWU
      DOUBLE PRECISION   ATOLI, EPS, GL, GU, RTOLI, SPDIAM, TMP1, TMP2, &
                         TNORM, UFLOW, WKILL, WLU, WUL

!     ..
!     .. Local Arrays ..
      INTEGER            IDUMMA( 1 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, ILAENV, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLAEBZ
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN
!     ..
!     .. Executable Statements ..
!
      INFO = 0
!
!     Decode RANGE
!
      IF( LSAME( RANGE, 'A' ) ) THEN
         IRANGE = ALLRNG
      ELSE IF( LSAME( RANGE, 'V' ) ) THEN
         IRANGE = VALRNG
      ELSE IF( LSAME( RANGE, 'I' ) ) THEN
         IRANGE = INDRNG
      ELSE
         IRANGE = 0
      ENDIF
!
!     Check for Errors
!
      IF( IRANGE.LE.0 ) THEN
         INFO = -1
      ELSE IF( .NOT.(LSAME(ORDER,'B').OR.LSAME(ORDER,'E')) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( IRANGE.EQ.VALRNG ) THEN
         IF( VL.GE.VU ) &
            INFO = -5
      ELSE IF( IRANGE.EQ.INDRNG .AND. &
              ( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) ) THEN
         INFO = -6
      ELSE IF( IRANGE.EQ.INDRNG .AND. &
              ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) THEN
         INFO = -7
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         RETURN
      ENDIF

!     Initialize error flags
      INFO = 0
      NCNVRG = .FALSE.
      TOOFEW = .FALSE.

!     Quick return if possible
      M = 0
      IF( N.EQ.0 ) RETURN

!     Simplification:
      IF( IRANGE.EQ.INDRNG .AND. IL.EQ.1 .AND. IU.EQ.N ) IRANGE = 1

!     Get machine constants
      EPS = DLAMCH( 'P' )
      UFLOW = DLAMCH( 'U' )


!     Special Case when N=1
!     Treat case of 1x1 matrix for quick return
      IF( N.EQ.1 ) THEN
         IF( (IRANGE.EQ.ALLRNG).OR. &
             ((IRANGE.EQ.VALRNG).AND.(D(1).GT.VL).AND.(D(1).LE.VU)).OR. &
             ((IRANGE.EQ.INDRNG).AND.(IL.EQ.1).AND.(IU.EQ.1)) ) THEN
            M = 1
            W(1) = D(1)
!           The computation error of the eigenvalue is zero
            WERR(1) = ZERO
            IBLOCK( 1 ) = 1
            INDEXW( 1 ) = 1
         ENDIF
         RETURN
      ENDIF

!     NB is the minimum vector length for vector bisection, or 0
!     if only scalar is to be done.
      NB = ILAENV( 1, 'DSTEBZ', ' ', N, -1, -1, -1 )
      IF( NB.LE.1 ) NB = 0

!     Find global spectral radius
      GL = D(1)
      GU = D(1)
      DO 5 I = 1,N
         GL =  MIN( GL, GERS( 2*I - 1))
         GU = MAX( GU, GERS(2*I) )
 5    CONTINUE
!     Compute global Gerschgorin bounds and spectral diameter
      TNORM = MAX( ABS( GL ), ABS( GU ) )
      GL = GL - FUDGE*TNORM*EPS*N - FUDGE*TWO*PIVMIN
      GU = GU + FUDGE*TNORM*EPS*N + FUDGE*TWO*PIVMIN
      SPDIAM = GU - GL
!     Input arguments for DLAEBZ:
!     The relative tolerance.  An interval (a,b] lies within
!     "relative tolerance" if  b-a < RELTOL*max(|a|,|b|),
      RTOLI = RELTOL
!     Set the absolute tolerance for interval convergence to zero to force
!     interval convergence based on relative size of the interval.
!     This is dangerous because intervals might not converge when RELTOL is
!     small. But at least a very small number should be selected so that for
!     strongly graded matrices, the code can get relatively accurate
!     eigenvalues.
      ATOLI = FUDGE*TWO*UFLOW + FUDGE*TWO*PIVMIN

      IF( IRANGE.EQ.INDRNG ) THEN

!        RANGE='I': Compute an interval containing eigenvalues
!        IL through IU. The initial interval [GL,GU] from the global
!        Gerschgorin bounds GL and GU is refined by DLAEBZ.
         ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) / &
                 LOG( TWO ) ) + 2
         WORK( N+1 ) = GL
         WORK( N+2 ) = GL
         WORK( N+3 ) = GU
         WORK( N+4 ) = GU
         WORK( N+5 ) = GL
         WORK( N+6 ) = GU
         IWORK( 1 ) = -1
         IWORK( 2 ) = -1
         IWORK( 3 ) = N + 1
         IWORK( 4 ) = N + 1
         IWORK( 5 ) = IL - 1
         IWORK( 6 ) = IU
!
         CALL DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN, &
               D, E, E2, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT, &
                      IWORK, W, IBLOCK, IINFO )
         IF( IINFO .NE. 0 ) THEN
            INFO = IINFO
            RETURN
         ENDIF
!        On exit, output intervals may not be ordered by ascending negcount
         IF( IWORK( 6 ).EQ.IU ) THEN
            WL = WORK( N+1 )
            WLU = WORK( N+3 )
            NWL = IWORK( 1 )
            WU = WORK( N+4 )
            WUL = WORK( N+2 )
            NWU = IWORK( 4 )
         ELSE
            WL = WORK( N+2 )
            WLU = WORK( N+4 )
            NWL = IWORK( 2 )
            WU = WORK( N+3 )
            WUL = WORK( N+1 )
            NWU = IWORK( 3 )
         ENDIF
!        On exit, the interval [WL, WLU] contains a value with negcount NWL,
!        and [WUL, WU] contains a value with negcount NWU.
         IF( NWL.LT.0 .OR. NWL.GE.N .OR. NWU.LT.1 .OR. NWU.GT.N ) THEN
            INFO = 4
            RETURN
         ENDIF

      ELSEIF( IRANGE.EQ.VALRNG ) THEN
         WL = VL
         WU = VU

      ELSEIF( IRANGE.EQ.ALLRNG ) THEN
         WL = GL
         WU = GU
      ENDIF



!     Find Eigenvalues -- Loop Over blocks and recompute NWL and NWU.
!     NWL accumulates the number of eigenvalues .le. WL,
!     NWU accumulates the number of eigenvalues .le. WU
      M = 0
      IEND = 0
      INFO = 0
      NWL = 0
      NWU = 0
!
      DO 70 JBLK = 1, NSPLIT
         IOFF = IEND
         IBEGIN = IOFF + 1
         IEND = ISPLIT( JBLK )
         IN = IEND - IOFF
!
         IF( IN.EQ.1 ) THEN
!           1x1 block
            IF( WL.GE.D( IBEGIN )-PIVMIN ) &
               NWL = NWL + 1
            IF( WU.GE.D( IBEGIN )-PIVMIN ) &
               NWU = NWU + 1
            IF( IRANGE.EQ.ALLRNG .OR. &
                 ( WL.LT.D( IBEGIN )-PIVMIN &
                   .AND. WU.GE. D( IBEGIN )-PIVMIN ) ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               WERR(M) = ZERO
!              The gap for a single block doesn't matter for the later
!              algorithm and is assigned an arbitrary large value
               IBLOCK( M ) = JBLK
               INDEXW( M ) = 1
            ENDIF

!        Disabled 2x2 case because of a failure on the following matrix
!        RANGE = 'I', IL = IU = 4
!          Original Tridiagonal, d = [
!           -0.150102010615740E+00
!           -0.849897989384260E+00
!           -0.128208148052635E-15
!            0.128257718286320E-15
!          ];
!          e = [
!           -0.357171383266986E+00
!           -0.180411241501588E-15
!           -0.175152352710251E-15
!          ];
!
!         ELSE IF( IN.EQ.2 ) THEN
!*           2x2 block
!            DISC = SQRT( (HALF*(D(IBEGIN)-D(IEND)))**2 + E(IBEGIN)**2 )
!            TMP1 = HALF*(D(IBEGIN)+D(IEND))
!            L1 = TMP1 - DISC
!            IF( WL.GE. L1-PIVMIN )
!     $         NWL = NWL + 1
!            IF( WU.GE. L1-PIVMIN )
!     $         NWU = NWU + 1
!            IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L1-PIVMIN .AND. WU.GE.
!     $          L1-PIVMIN ) ) THEN
!               M = M + 1
!               W( M ) = L1
!*              The uncertainty of eigenvalues of a 2x2 matrix is very small
!               WERR( M ) = EPS * ABS( W( M ) ) * TWO
!               IBLOCK( M ) = JBLK
!               INDEXW( M ) = 1
!            ENDIF
!            L2 = TMP1 + DISC
!            IF( WL.GE. L2-PIVMIN )
!     $         NWL = NWL + 1
!            IF( WU.GE. L2-PIVMIN )
!     $         NWU = NWU + 1
!            IF( IRANGE.EQ.ALLRNG .OR. ( WL.LT.L2-PIVMIN .AND. WU.GE.
!     $          L2-PIVMIN ) ) THEN
!               M = M + 1
!               W( M ) = L2
!*              The uncertainty of eigenvalues of a 2x2 matrix is very small
!               WERR( M ) = EPS * ABS( W( M ) ) * TWO
!               IBLOCK( M ) = JBLK
!               INDEXW( M ) = 2
!            ENDIF
         ELSE
!           General Case - block of size IN >= 2
!           Compute local Gerschgorin interval and use it as the initial
!           interval for DLAEBZ
            GU = D( IBEGIN )
            GL = D( IBEGIN )
            TMP1 = ZERO

            DO 40 J = IBEGIN, IEND
               GL =  MIN( GL, GERS( 2*J - 1))
               GU = MAX( GU, GERS(2*J) )
   40       CONTINUE
            SPDIAM = GU - GL
            GL = GL - FUDGE*SPDIAM*EPS*IN - FUDGE*PIVMIN
            GU = GU + FUDGE*SPDIAM*EPS*IN + FUDGE*PIVMIN
!
            IF( IRANGE.GT.1 ) THEN
               IF( GU.LT.WL ) THEN
!                 the local block contains none of the wanted eigenvalues
                  NWL = NWL + IN
                  NWU = NWU + IN
                  GO TO 70
               ENDIF
!              refine search interval if possible, only range (WL,WU] matters
               GL = MAX( GL, WL )
               GU = MIN( GU, WU )
               IF( GL.GE.GU ) &
                  GO TO 70
            ENDIF

!           Find negcount of initial interval boundaries GL and GU
            WORK( N+1 ) = GL
            WORK( N+IN+1 ) = GU
            CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN, &
                         D( IBEGIN ), E( IBEGIN ), E2( IBEGIN ), &
                         IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM, &
                         IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = IINFO
               RETURN
            ENDIF
!
            NWL = NWL + IWORK( 1 )
            NWU = NWU + IWORK( IN+1 )
            IWOFF = M - IWORK( 1 )

!           Compute Eigenvalues
            ITMAX = INT( ( LOG( GU-GL+PIVMIN )-LOG( PIVMIN ) ) / &
                    LOG( TWO ) ) + 2
            CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN, &
                         D( IBEGIN ), E( IBEGIN ), E2( IBEGIN ), &
                         IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT, &
                         IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = IINFO
               RETURN
            ENDIF
!
!           Copy eigenvalues into W and IBLOCK
!           Use -JBLK for block number for unconverged eigenvalues.
!           Loop over the number of output intervals from DLAEBZ
            DO 60 J = 1, IOUT
!              eigenvalue approximation is middle point of interval
               TMP1 = HALF*( WORK( J+N )+WORK( J+IN+N ) )
!              semi length of error interval
               TMP2 = HALF*ABS( WORK( J+N )-WORK( J+IN+N ) )
               IF( J.GT.IOUT-IINFO ) THEN
!                 Flag non-convergence.
                  NCNVRG = .TRUE.
                  IB = -JBLK
               ELSE
                  IB = JBLK
               ENDIF
               DO 50 JE = IWORK( J ) + 1 + IWOFF, &
                       IWORK( J+IN ) + IWOFF
                  W( JE ) = TMP1
                  WERR( JE ) = TMP2
                  INDEXW( JE ) = JE - IWOFF
                  IBLOCK( JE ) = IB
   50          CONTINUE
   60       CONTINUE
!
            M = M + IM
         ENDIF
   70 CONTINUE

!     If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
!     If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
      IF( IRANGE.EQ.INDRNG ) THEN
         IDISCL = IL - 1 - NWL
         IDISCU = NWU - IU
!
         IF( IDISCL.GT.0 ) THEN
            IM = 0
            DO 80 JE = 1, M
!              Remove some of the smallest eigenvalues from the left so that
!              at the end IDISCL =0. Move all eigenvalues up to the left.
               IF( W( JE ).LE.WLU .AND. IDISCL.GT.0 ) THEN
                  IDISCL = IDISCL - 1
               ELSE
                  IM = IM + 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               ENDIF
 80         CONTINUE
            M = IM
         ENDIF
         IF( IDISCU.GT.0 ) THEN
!           Remove some of the largest eigenvalues from the right so that
!           at the end IDISCU =0. Move all eigenvalues up to the left.
            IM=M+1
            DO 81 JE = M, 1, -1
               IF( W( JE ).GE.WUL .AND. IDISCU.GT.0 ) THEN
                  IDISCU = IDISCU - 1
               ELSE
                  IM = IM - 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               ENDIF
 81         CONTINUE
            JEE = 0
            DO 82 JE = IM, M
               JEE = JEE + 1
               W( JEE ) = W( JE )
               WERR( JEE ) = WERR( JE )
               INDEXW( JEE ) = INDEXW( JE )
               IBLOCK( JEE ) = IBLOCK( JE )
 82         CONTINUE
            M = M-IM+1
         ENDIF

         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
!           Code to deal with effects of bad arithmetic. (If N(w) is
!           monotone non-decreasing, this should never happen.)
!           Some low eigenvalues to be discarded are not in (WL,WLU],
!           or high eigenvalues to be discarded are not in (WUL,WU]
!           so just kill off the smallest IDISCL/largest IDISCU
!           eigenvalues, by marking the corresponding IBLOCK = 0
            IF( IDISCL.GT.0 ) THEN
               WKILL = WU
               DO 100 JDISC = 1, IDISCL
                  IW = 0
                  DO 90 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND. &
                          ( W( JE ).LT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     ENDIF
 90               CONTINUE
                  IBLOCK( IW ) = 0
 100           CONTINUE
            ENDIF
            IF( IDISCU.GT.0 ) THEN
               WKILL = WL
               DO 120 JDISC = 1, IDISCU
                  IW = 0
                  DO 110 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND. &
                          ( W( JE ).GE.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     ENDIF
 110              CONTINUE
                  IBLOCK( IW ) = 0
 120           CONTINUE
            ENDIF
!           Now erase all eigenvalues with IBLOCK set to zero
            IM = 0
            DO 130 JE = 1, M
               IF( IBLOCK( JE ).NE.0 ) THEN
                  IM = IM + 1
                  W( IM ) = W( JE )
                  WERR( IM ) = WERR( JE )
                  INDEXW( IM ) = INDEXW( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               ENDIF
 130        CONTINUE
            M = IM
         ENDIF
         IF( IDISCL.LT.0 .OR. IDISCU.LT.0 ) THEN
            TOOFEW = .TRUE.
         ENDIF
      ENDIF
!
      IF(( IRANGE.EQ.ALLRNG .AND. M.NE.N ).OR. &
         ( IRANGE.EQ.INDRNG .AND. M.NE.IU-IL+1 ) ) THEN
         TOOFEW = .TRUE.
      ENDIF

!     If ORDER='B', do nothing the eigenvalues are already sorted by
!        block.
!     If ORDER='E', sort the eigenvalues from smallest to largest

      IF( LSAME(ORDER,'E') .AND. NSPLIT.GT.1 ) THEN
         DO 150 JE = 1, M - 1
            IE = 0
            TMP1 = W( JE )
            DO 140 J = JE + 1, M
               IF( W( J ).LT.TMP1 ) THEN
                  IE = J
                  TMP1 = W( J )
               ENDIF
  140       CONTINUE
            IF( IE.NE.0 ) THEN
               TMP2 = WERR( IE )
               ITMP1 = IBLOCK( IE )
               ITMP2 = INDEXW( IE )
               W( IE ) = W( JE )
               WERR( IE ) = WERR( JE )
               IBLOCK( IE ) = IBLOCK( JE )
               INDEXW( IE ) = INDEXW( JE )
               W( JE ) = TMP1
               WERR( JE ) = TMP2
               IBLOCK( JE ) = ITMP1
               INDEXW( JE ) = ITMP2
            ENDIF
  150    CONTINUE
      ENDIF
!
      INFO = 0
      IF( NCNVRG ) &
         INFO = INFO + 1
      IF( TOOFEW ) &
         INFO = INFO + 2
      RETURN
!
!     End of DLARRD
!
      END SUBROUTINE DLARRD
      
      
      
      
      
      SUBROUTINE DLARRE( RANGE, N, VL, VU, IL, IU, D, E, E2, &
                          RTOL1, RTOL2, SPLTOL, NSPLIT, ISPLIT, M, &
                          W, WERR, WGAP, IBLOCK, INDEXW, GERS, PIVMIN, &
                          WORK, IWORK, INFO )
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION  PIVMIN, RTOL1, RTOL2, SPLTOL, VL, VU
!     ..
!     .. Array Arguments ..
      INTEGER            IBLOCK( * ), ISPLIT( * ), IWORK( * ), &
                         INDEXW( * )
      DOUBLE PRECISION   D( * ), E( * ), E2( * ), GERS( * ), &
                         W( * ),WERR( * ), WGAP( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  To find the desired eigenvalues of a given real symmetric
!  tridiagonal matrix T, DLARRE sets any "small" off-diagonal
!  elements to zero, and for each unreduced block T_i, it finds
!  (a) a suitable shift at one end of the block's spectrum,
!  (b) the base representation, T_i - sigma_i I = L_i D_i L_i^T, and
!  (c) eigenvalues of each L_i D_i L_i^T.
!  The representations and eigenvalues found are then used by
!  DSTEMR to compute the eigenvectors of T.
!  The accuracy varies depending on whether bisection is used to
!  find a few eigenvalues or the dqds algorithm (subroutine DLASQ2) to
!  conpute all and then discard any unwanted one.
!  As an added benefit, DLARRE also outputs the n
!  Gerschgorin intervals for the matrices L_i D_i L_i^T.
!
!  Arguments
!  =========
!
!  RANGE   (input) CHARACTER
!          = 'A': ("All")   all eigenvalues will be found.
!          = 'V': ("Value") all eigenvalues in the half-open interval
!                           (VL, VU] will be found.
!          = 'I': ("Index") the IL-th through IU-th eigenvalues (of the
!                           entire matrix) will be found.
!
!  N       (input) INTEGER
!          The order of the matrix. N > 0.
!
!  VL      (input/output) DOUBLE PRECISION
!  VU      (input/output) DOUBLE PRECISION
!          If RANGE='V', the lower and upper bounds for the eigenvalues.
!          Eigenvalues less than or equal to VL, or greater than VU,
!          will not be returned.  VL < VU.
!          If RANGE='I' or ='A', DLARRE computes bounds on the desired
!          part of the spectrum.
!
!  IL      (input) INTEGER
!  IU      (input) INTEGER
!          If RANGE='I', the indices (in ascending order) of the
!          smallest and largest eigenvalues to be returned.
!          1 <= IL <= IU <= N.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the N diagonal elements of the tridiagonal
!          matrix T.
!          On exit, the N diagonal elements of the diagonal
!          matrices D_i.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the first (N-1) entries contain the subdiagonal
!          elements of the tridiagonal matrix T; E(N) need not be set.
!          On exit, E contains the subdiagonal elements of the unit
!          bidiagonal matrices L_i. The entries E( ISPLIT( I ) ),
!          1 <= I <= NSPLIT, contain the base points sigma_i on output.
!
!  E2      (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the first (N-1) entries contain the SQUARES of the
!          subdiagonal elements of the tridiagonal matrix T;
!          E2(N) need not be set.
!          On exit, the entries E2( ISPLIT( I ) ),
!          1 <= I <= NSPLIT, have been set to zero
!
!  RTOL1   (input) DOUBLE PRECISION
!  RTOL2   (input) DOUBLE PRECISION
!           Parameters for bisection.
!           An interval [LEFT,RIGHT] has converged if
!           RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
!
!  SPLTOL (input) DOUBLE PRECISION
!          The threshold for splitting.
!
!  NSPLIT  (output) INTEGER
!          The number of blocks T splits into. 1 <= NSPLIT <= N.
!
!  ISPLIT  (output) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into blocks.
!          The first block consists of rows/columns 1 to ISPLIT(1),
!          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
!          etc., and the NSPLIT-th consists of rows/columns
!          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
!
!  M       (output) INTEGER
!          The total number of eigenvalues (of all L_i D_i L_i^T)
!          found.
!
!  W       (output) DOUBLE PRECISION array, dimension (N)
!          The first M elements contain the eigenvalues. The
!          eigenvalues of each of the blocks, L_i D_i L_i^T, are
!          sorted in ascending order ( DLARRE may use the
!          remaining N-M elements as workspace).
!
!  WERR    (output) DOUBLE PRECISION array, dimension (N)
!          The error bound on the corresponding eigenvalue in W.
!
!  WGAP    (output) DOUBLE PRECISION array, dimension (N)
!          The separation from the right neighbor eigenvalue in W.
!          The gap is only with respect to the eigenvalues of the same block
!          as each block has its own representation tree.
!          Exception: at the right end of a block we store the left gap
!
!  IBLOCK  (output) INTEGER array, dimension (N)
!          The indices of the blocks (submatrices) associated with the
!          corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
!          W(i) belongs to the first block from the top, =2 if W(i)
!          belongs to the second block, etc.
!
!  INDEXW  (output) INTEGER array, dimension (N)
!          The indices of the eigenvalues within each block (submatrix);
!          for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
!          i-th eigenvalue W(i) is the 10-th eigenvalue in block 2
!
!  GERS    (output) DOUBLE PRECISION array, dimension (2*N)
!          The N Gerschgorin intervals (the i-th Gerschgorin interval
!          is (GERS(2*i-1), GERS(2*i)).
!
!  PIVMIN  (output) DOUBLE PRECISION
!          The minimum pivot in the Sturm sequence for T.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (6*N)
!          Workspace.
!
!  IWORK   (workspace) INTEGER array, dimension (5*N)
!          Workspace.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          > 0:  A problem occured in DLARRE.
!          < 0:  One of the called subroutines signaled an internal problem.
!                Needs inspection of the corresponding parameter IINFO
!                for further information.
!
!          =-1:  Problem in DLARRD.
!          = 2:  No base representation could be found in MAXTRY iterations.
!                Increasing MAXTRY and recompilation might be a remedy.
!          =-3:  Problem in DLARRB when computing the refined root
!                representation for DLASQ2.
!          =-4:  Problem in DLARRB when preforming bisection on the
!                desired part of the spectrum.
!          =-5:  Problem in DLASQ2.
!          =-6:  Problem in DLASQ2.
!
!  Further Details
!  The base representations are required to suffer very little
!  element growth and consequently define all their eigenvalues to
!  high relative accuracy.
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   FAC, FOUR, FOURTH, FUDGE, HALF, HNDRD, &
                         MAXGROWTH, ONE, PERT, TWO, ZERO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, &
                           TWO = 2.0D0, FOUR=4.0D0, &
                           HNDRD = 100.0D0, &
                           PERT = 8.0D0, &
                           HALF = ONE/TWO, FOURTH = ONE/FOUR, FAC= HALF, &
                           MAXGROWTH = 64.0D0, FUDGE = 2.0D0 )
      INTEGER            MAXTRY, ALLRNG, INDRNG, VALRNG
      PARAMETER          ( MAXTRY = 6, ALLRNG = 1, INDRNG = 2, &
                           VALRNG = 3 )
!     ..
!     .. Local Scalars ..
      LOGICAL            FORCEB, NOREP, USEDQD
      INTEGER            CNT, CNT1, CNT2, I, IBEGIN, IDUM, IEND, IINFO, &
                         IN, INDL, INDU, IRANGE, J, JBLK, MB, MM, &
                         WBEGIN, WEND
      DOUBLE PRECISION   AVGAP, BSRTOL, CLWDTH, DMAX, DPIVOT, EABS, &
                         EMAX, EOLD, EPS, GL, GU, ISLEFT, ISRGHT, RTL, &
                         RTOL, S1, S2, SAFMIN, SGNDEF, SIGMA, SPDIAM, &
                         TAU, TMP, TMP1


!     ..
!     .. Local Arrays ..
      INTEGER            ISEED( 4 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION            DLAMCH
      EXTERNAL           DLAMCH, LSAME

!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY, DLARNV, DLARRA, DLARRB, DLARRC, DLARRD, &
                         DLASQ2
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN

!     ..
!     .. Executable Statements ..
!

      INFO = 0

!
!     Decode RANGE
!
      IF( LSAME( RANGE, 'A' ) ) THEN
         IRANGE = ALLRNG
      ELSE IF( LSAME( RANGE, 'V' ) ) THEN
         IRANGE = VALRNG
      ELSE IF( LSAME( RANGE, 'I' ) ) THEN
         IRANGE = INDRNG
      ENDIF

      M = 0

!     Get machine constants
      SAFMIN = DLAMCH( 'S' )
      EPS = DLAMCH( 'P' )

!     Set parameters
      RTL = SQRT(EPS)
      BSRTOL = SQRT(EPS)

!     Treat case of 1x1 matrix for quick return
      IF( N.EQ.1 ) THEN
         IF( (IRANGE.EQ.ALLRNG).OR. &
             ((IRANGE.EQ.VALRNG).AND.(D(1).GT.VL).AND.(D(1).LE.VU)).OR. &
             ((IRANGE.EQ.INDRNG).AND.(IL.EQ.1).AND.(IU.EQ.1)) ) THEN
            M = 1
            W(1) = D(1)
!           The computation error of the eigenvalue is zero
            WERR(1) = ZERO
            WGAP(1) = ZERO
            IBLOCK( 1 ) = 1
            INDEXW( 1 ) = 1
            GERS(1) = D( 1 )
            GERS(2) = D( 1 )
         ENDIF
!        store the shift for the initial RRR, which is zero in this case
         E(1) = ZERO
         RETURN
      ENDIF

!     General case: tridiagonal matrix of order > 1
!
!     Init WERR, WGAP. Compute Gerschgorin intervals and spectral diameter.
!     Compute maximum off-diagonal entry and pivmin.
      GL = D(1)
      GU = D(1)
      EOLD = ZERO
      EMAX = ZERO
      E(N) = ZERO
      DO 5 I = 1,N
         WERR(I) = ZERO
         WGAP(I) = ZERO
         EABS = ABS( E(I) )
         IF( EABS .GE. EMAX ) THEN
            EMAX = EABS
         ENDIF
         TMP1 = EABS + EOLD
         GERS( 2*I-1) = D(I) - TMP1
         GL =  MIN( GL, GERS( 2*I - 1))
         GERS( 2*I ) = D(I) + TMP1
         GU = MAX( GU, GERS(2*I) )
         EOLD  = EABS
 5    CONTINUE
!     The minimum pivot allowed in the Sturm sequence for T
      PIVMIN = SAFMIN * MAX( ONE, EMAX**2 )
!     Compute spectral diameter. The Gerschgorin bounds give an
!     estimate that is wrong by at most a factor of SQRT(2)
      SPDIAM = GU - GL

!     Compute splitting points
      CALL DLARRA( N, D, E, E2, SPLTOL, SPDIAM, &
                          NSPLIT, ISPLIT, IINFO )

!     Can force use of bisection instead of faster DQDS.
!     Option left in the code for future multisection work.
      FORCEB = .FALSE.

      IF( (IRANGE.EQ.ALLRNG) .AND. (.NOT. FORCEB) ) THEN
!        Set interval [VL,VU] that contains all eigenvalues
         VL = GL
         VU = GU
      ELSE
!        We call DLARRD to find crude approximations to the eigenvalues
!        in the desired range. In case IRANGE = INDRNG, we also obtain the
!        interval (VL,VU] that contains all the wanted eigenvalues.
!        An interval [LEFT,RIGHT] has converged if
!        RIGHT-LEFT.LT.RTOL*MAX(ABS(LEFT),ABS(RIGHT))
!        DLARRD needs a WORK of size 4*N, IWORK of size 3*N
         CALL DLARRD( RANGE, 'B', N, VL, VU, IL, IU, GERS, &
                          BSRTOL, D, E, E2, PIVMIN, NSPLIT, ISPLIT, &
                          MM, W, WERR, VL, VU, IBLOCK, INDEXW, &
                          WORK, IWORK, IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = -1
            RETURN
         ENDIF
!        Make sure that the entries M+1 to N in W, WERR, IBLOCK, INDEXW are 0
         DO 14 I = MM+1,N
            W( I ) = ZERO
            WERR( I ) = ZERO
            IBLOCK( I ) = 0
            INDEXW( I ) = 0
 14      CONTINUE
      ENDIF


!**
!     Loop over unreduced blocks
      IBEGIN = 1
      WBEGIN = 1
      DO 170 JBLK = 1, NSPLIT
         IEND = ISPLIT( JBLK )
         IN = IEND - IBEGIN + 1

!        1 X 1 block
         IF( IN.EQ.1 ) THEN
            IF( (IRANGE.EQ.ALLRNG).OR.( (IRANGE.EQ.VALRNG).AND. &
               ( D( IBEGIN ).GT.VL ).AND.( D( IBEGIN ).LE.VU ) ) &
              .OR. ( (IRANGE.EQ.INDRNG).AND.(IBLOCK(WBEGIN).EQ.JBLK)) &
              ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               WERR(M) = ZERO
!              The gap for a single block doesn't matter for the later
!              algorithm and is assigned an arbitrary large value
               WGAP(M) = ZERO
               IBLOCK( M ) = JBLK
               INDEXW( M ) = 1
               WBEGIN = WBEGIN + 1
            ENDIF
!           E( IEND ) holds the shift for the initial RRR
            E( IEND ) = ZERO
            IBEGIN = IEND + 1
            GO TO 170
         ENDIF
!
!        Blocks of size larger than 1x1
!
!        E( IEND ) will hold the shift for the initial RRR, for now set it =0
         E( IEND ) = ZERO
!
!        Find local outer bounds GL,GU for the block
         GL = D(IBEGIN)
         GU = D(IBEGIN)
         DO 15 I = IBEGIN , IEND
            GL = MIN( GERS( 2*I-1 ), GL )
            GU = MAX( GERS( 2*I ), GU )
 15      CONTINUE
         SPDIAM = GU - GL

         IF(.NOT. ((IRANGE.EQ.ALLRNG).AND.(.NOT.FORCEB)) ) THEN
!           Count the number of eigenvalues in the current block.
            MB = 0
            DO 20 I = WBEGIN,MM
               IF( IBLOCK(I).EQ.JBLK ) THEN
                  MB = MB+1
               ELSE
                  GOTO 21
               ENDIF
 20         CONTINUE
 21         CONTINUE

            IF( MB.EQ.0) THEN
!              No eigenvalue in the current block lies in the desired range
!              E( IEND ) holds the shift for the initial RRR
               E( IEND ) = ZERO
               IBEGIN = IEND + 1
               GO TO 170
            ELSE

!              Decide whether dqds or bisection is more efficient
               USEDQD = ( (MB .GT. FAC*IN) .AND. (.NOT.FORCEB) )
               WEND = WBEGIN + MB - 1
!              Calculate gaps for the current block
!              In later stages, when representations for individual
!              eigenvalues are different, we use SIGMA = E( IEND ).
               SIGMA = ZERO
               DO 30 I = WBEGIN, WEND - 1
                  WGAP( I ) = MAX( ZERO, &
                              W(I+1)-WERR(I+1) - (W(I)+WERR(I)) )
 30            CONTINUE
               WGAP( WEND ) = MAX( ZERO, &
                           VU - SIGMA - (W( WEND )+WERR( WEND )))
!              Find local index of the first and last desired evalue.
               INDL = INDEXW(WBEGIN)
               INDU = INDEXW( WEND )
            ENDIF
         ENDIF
         IF(( (IRANGE.EQ.ALLRNG) .AND. (.NOT. FORCEB) ).OR.USEDQD) THEN
!           Case of DQDS
!           Find approximations to the extremal eigenvalues of the block
            CALL DLARRK( IN, 1, GL, GU, D(IBEGIN), &
                     E2(IBEGIN), PIVMIN, RTL, TMP, TMP1, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = -1
               RETURN
            ENDIF
            ISLEFT = MAX(GL, TMP - TMP1 &
                     - HNDRD * EPS* ABS(TMP - TMP1))

            CALL DLARRK( IN, IN, GL, GU, D(IBEGIN), &
                     E2(IBEGIN), PIVMIN, RTL, TMP, TMP1, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = -1
               RETURN
            ENDIF
            ISRGHT = MIN(GU, TMP + TMP1 &
                       + HNDRD * EPS * ABS(TMP + TMP1))
!           Improve the estimate of the spectral diameter
            SPDIAM = ISRGHT - ISLEFT
         ELSE
!           Case of bisection
!           Find approximations to the wanted extremal eigenvalues
            ISLEFT = MAX(GL, W(WBEGIN) - WERR(WBEGIN) &
                        - HNDRD * EPS*ABS(W(WBEGIN)- WERR(WBEGIN) ))
            ISRGHT = MIN(GU,W(WEND) + WERR(WEND) &
                        + HNDRD * EPS * ABS(W(WEND)+ WERR(WEND)))
         ENDIF


!        Decide whether the base representation for the current block
!        L_JBLK D_JBLK L_JBLK^T = T_JBLK - sigma_JBLK I
!        should be on the left or the right end of the current block.
!        The strategy is to shift to the end which is "more populated"
!        Furthermore, decide whether to use DQDS for the computation of
!        the eigenvalue approximations at the end of DLARRE or bisection.
!        dqds is chosen if all eigenvalues are desired or the number of
!        eigenvalues to be computed is large compared to the blocksize.
         IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
!           If all the eigenvalues have to be computed, we use dqd
            USEDQD = .TRUE.
!           INDL is the local index of the first eigenvalue to compute
            INDL = 1
            INDU = IN
!           MB =  number of eigenvalues to compute
            MB = IN
            WEND = WBEGIN + MB - 1
!           Define 1/4 and 3/4 points of the spectrum
            S1 = ISLEFT + FOURTH * SPDIAM
            S2 = ISRGHT - FOURTH * SPDIAM
         ELSE
!           DLARRD has computed IBLOCK and INDEXW for each eigenvalue
!           approximation.
!           choose sigma
            IF( USEDQD ) THEN
               S1 = ISLEFT + FOURTH * SPDIAM
               S2 = ISRGHT - FOURTH * SPDIAM
            ELSE
               TMP = MIN(ISRGHT,VU) -  MAX(ISLEFT,VL)
               S1 =  MAX(ISLEFT,VL) + FOURTH * TMP
               S2 =  MIN(ISRGHT,VU) - FOURTH * TMP
            ENDIF
         ENDIF

!        Compute the negcount at the 1/4 and 3/4 points
         IF(MB.GT.1) THEN
            CALL DLARRC( 'T', IN, S1, S2, D(IBEGIN), &
                          E(IBEGIN), PIVMIN, CNT, CNT1, CNT2, IINFO)
         ENDIF

         IF(MB.EQ.1) THEN
            SIGMA = GL
            SGNDEF = ONE
         ELSEIF( CNT1 - INDL .GE. INDU - CNT2 ) THEN
            IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
               SIGMA = MAX(ISLEFT,GL)
            ELSEIF( USEDQD ) THEN
!              use Gerschgorin bound as shift to get pos def matrix
!              for dqds
               SIGMA = ISLEFT
            ELSE
!              use approximation of the first desired eigenvalue of the
!              block as shift
               SIGMA = MAX(ISLEFT,VL)
            ENDIF
            SGNDEF = ONE
         ELSE
            IF( ( IRANGE.EQ.ALLRNG ) .AND. (.NOT.FORCEB) ) THEN
               SIGMA = MIN(ISRGHT,GU)
            ELSEIF( USEDQD ) THEN
!              use Gerschgorin bound as shift to get neg def matrix
!              for dqds
               SIGMA = ISRGHT
            ELSE
!              use approximation of the first desired eigenvalue of the
!              block as shift
               SIGMA = MIN(ISRGHT,VU)
            ENDIF
            SGNDEF = -ONE
         ENDIF


!        An initial SIGMA has been chosen that will be used for computing
!        T - SIGMA I = L D L^T
!        Define the increment TAU of the shift in case the initial shift
!        needs to be refined to obtain a factorization with not too much
!        element growth.
         IF( USEDQD ) THEN
!           The initial SIGMA was to the outer end of the spectrum
!           the matrix is definite and we need not retreat.
            TAU = SPDIAM*EPS*N + TWO*PIVMIN
         ELSE
            IF(MB.GT.1) THEN
               CLWDTH = W(WEND) + WERR(WEND) - W(WBEGIN) - WERR(WBEGIN)
               AVGAP = ABS(CLWDTH / DBLE(WEND-WBEGIN))
               IF( SGNDEF.EQ.ONE ) THEN
                  TAU = HALF*MAX(WGAP(WBEGIN),AVGAP)
                  TAU = MAX(TAU,WERR(WBEGIN))
               ELSE
                  TAU = HALF*MAX(WGAP(WEND-1),AVGAP)
                  TAU = MAX(TAU,WERR(WEND))
               ENDIF
            ELSE
               TAU = WERR(WBEGIN)
            ENDIF
         ENDIF
!
         DO 80 IDUM = 1, MAXTRY
!           Compute L D L^T factorization of tridiagonal matrix T - sigma I.
!           Store D in WORK(1:IN), L in WORK(IN+1:2*IN), and reciprocals of
!           pivots in WORK(2*IN+1:3*IN)
            DPIVOT = D( IBEGIN ) - SIGMA
            WORK( 1 ) = DPIVOT
            DMAX = ABS( WORK(1) )
            J = IBEGIN
            DO 70 I = 1, IN - 1
               WORK( 2*IN+I ) = ONE / WORK( I )
               TMP = E( J )*WORK( 2*IN+I )
               WORK( IN+I ) = TMP
               DPIVOT = ( D( J+1 )-SIGMA ) - TMP*E( J )
               WORK( I+1 ) = DPIVOT
               DMAX = MAX( DMAX, ABS(DPIVOT) )
               J = J + 1
 70         CONTINUE
!           check for element growth
            IF( DMAX .GT. MAXGROWTH*SPDIAM ) THEN
               NOREP = .TRUE.
            ELSE
               NOREP = .FALSE.
            ENDIF
            IF( USEDQD .AND. .NOT.NOREP ) THEN
!              Ensure the definiteness of the representation
!              All entries of D (of L D L^T) must have the same sign
               DO 71 I = 1, IN
                  TMP = SGNDEF*WORK( I )
                  IF( TMP.LT.ZERO ) NOREP = .TRUE.
 71            CONTINUE
            ENDIF
            IF(NOREP) THEN
!              Note that in the case of IRANGE=ALLRNG, we use the Gerschgorin
!              shift which makes the matrix definite. So we should end up
!              here really only in the case of IRANGE = VALRNG or INDRNG.
               IF( IDUM.EQ.MAXTRY-1 ) THEN
                  IF( SGNDEF.EQ.ONE ) THEN
!                    The fudged Gerschgorin shift should succeed
                     SIGMA = &
                          GL - FUDGE*SPDIAM*EPS*N - FUDGE*TWO*PIVMIN
                  ELSE
                     SIGMA = &
                          GU + FUDGE*SPDIAM*EPS*N + FUDGE*TWO*PIVMIN
                  ENDIF
               ELSE
                  SIGMA = SIGMA - SGNDEF * TAU
                  TAU = TWO * TAU
               ENDIF
            ELSE
!              an initial RRR is found
               GO TO 83
            ENDIF
 80      CONTINUE
!        if the program reaches this point, no base representation could be
!        found in MAXTRY iterations.
         INFO = 2
         RETURN

 83      CONTINUE
!        At this point, we have found an initial base representation
!        T - SIGMA I = L D L^T with not too much element growth.
!        Store the shift.
         E( IEND ) = SIGMA
!        Store D and L.
         CALL DCOPY( IN, WORK, 1, D( IBEGIN ), 1 )
         CALL DCOPY( IN-1, WORK( IN+1 ), 1, E( IBEGIN ), 1 )


         IF(MB.GT.1 ) THEN
!
!           Perturb each entry of the base representation by a small
!           (but random) relative amount to overcome difficulties with
!           glued matrices.
!
            DO 122 I = 1, 4
               ISEED( I ) = 1
 122        CONTINUE

            CALL DLARNV(2, ISEED, 2*IN-1, WORK(1))
            DO 125 I = 1,IN-1
               D(IBEGIN+I-1) = D(IBEGIN+I-1)*(ONE+EPS*PERT*WORK(I))
               E(IBEGIN+I-1) = E(IBEGIN+I-1)*(ONE+EPS*PERT*WORK(IN+I))
 125        CONTINUE
            D(IEND) = D(IEND)*(ONE+EPS*FOUR*WORK(IN))
!
         ENDIF
!
!        Don't update the Gerschgorin intervals because keeping track
!        of the updates would be too much work in DLARRV.
!        We update W instead and use it to locate the proper Gerschgorin
!        intervals.

!        Compute the required eigenvalues of L D L' by bisection or dqds
         IF ( .NOT.USEDQD ) THEN
!           If DLARRD has been used, shift the eigenvalue approximations
!           according to their representation. This is necessary for
!           a uniform DLARRV since dqds computes eigenvalues of the
!           shifted representation. In DLARRV, W will always hold the
!           UNshifted eigenvalue approximation.
            DO 134 J=WBEGIN,WEND
               W(J) = W(J) - SIGMA
               WERR(J) = WERR(J) + ABS(W(J)) * EPS
 134        CONTINUE
!           call DLARRB to reduce eigenvalue error of the approximations
!           from DLARRD
            DO 135 I = IBEGIN, IEND-1
               WORK( I ) = D( I ) * E( I )**2
 135        CONTINUE
!           use bisection to find EV from INDL to INDU
            CALL DLARRB(IN, D(IBEGIN), WORK(IBEGIN), &
                        INDL, INDU, RTOL1, RTOL2, INDL-1, &
                        W(WBEGIN), WGAP(WBEGIN), WERR(WBEGIN), &
                        WORK( 2*N+1 ), IWORK, PIVMIN, SPDIAM, &
                        IN, IINFO )
            IF( IINFO .NE. 0 ) THEN
               INFO = -4
               RETURN
            ENDIF
!           DLARRB computes all gaps correctly except for the last one
!           Record distance to VU/GU
            WGAP( WEND ) = MAX( ZERO, &
                 ( VU-SIGMA ) - ( W( WEND ) + WERR( WEND ) ) )
            DO 138 I = INDL, INDU
               M = M + 1
               IBLOCK(M) = JBLK
               INDEXW(M) = I
 138        CONTINUE
         ELSE
!           Call dqds to get all eigs (and then possibly delete unwanted
!           eigenvalues).
!           Note that dqds finds the eigenvalues of the L D L^T representation
!           of T to high relative accuracy. High relative accuracy
!           might be lost when the shift of the RRR is subtracted to obtain
!           the eigenvalues of T. However, T is not guaranteed to define its
!           eigenvalues to high relative accuracy anyway.
!           Set RTOL to the order of the tolerance used in DLASQ2
!           This is an ESTIMATED error, the worst case bound is 4*N*EPS
!           which is usually too large and requires unnecessary work to be
!           done by bisection when computing the eigenvectors
            RTOL = LOG(DBLE(IN)) * FOUR * EPS
            J = IBEGIN
            DO 140 I = 1, IN - 1
               WORK( 2*I-1 ) = ABS( D( J ) )
               WORK( 2*I ) = E( J )*E( J )*WORK( 2*I-1 )
               J = J + 1
  140       CONTINUE
            WORK( 2*IN-1 ) = ABS( D( IEND ) )
            WORK( 2*IN ) = ZERO
            CALL DLASQ2( IN, WORK, IINFO )
            IF( IINFO .NE. 0 ) THEN
!              If IINFO = -5 then an index is part of a tight cluster
!              and should be changed. The index is in IWORK(1) and the
!              gap is in WORK(N+1)
               INFO = -5
               RETURN
            ELSE
!              Test that all eigenvalues are positive as expected
               DO 149 I = 1, IN
                  IF( WORK( I ).LT.ZERO ) THEN
                     INFO = -6
                     RETURN
                  ENDIF
 149           CONTINUE
            ENDIF
            IF( SGNDEF.GT.ZERO ) THEN
               DO 150 I = INDL, INDU
                  M = M + 1
                  W( M ) = WORK( IN-I+1 )
                  IBLOCK( M ) = JBLK
                  INDEXW( M ) = I
 150           CONTINUE
            ELSE
               DO 160 I = INDL, INDU
                  M = M + 1
                  W( M ) = -WORK( I )
                  IBLOCK( M ) = JBLK
                  INDEXW( M ) = I
 160           CONTINUE
            ENDIF

            DO 165 I = M - MB + 1, M
!              the value of RTOL below should be the tolerance in DLASQ2
               WERR( I ) = RTOL * ABS( W(I) )
 165        CONTINUE
            DO 166 I = M - MB + 1, M - 1
!              compute the right gap between the intervals
               WGAP( I ) = MAX( ZERO, &
                                W(I+1)-WERR(I+1) - (W(I)+WERR(I)) )
 166        CONTINUE
            WGAP( M ) = MAX( ZERO, &
                 ( VU-SIGMA ) - ( W( M ) + WERR( M ) ) )
         ENDIF
!        proceed with next block
         IBEGIN = IEND + 1
         WBEGIN = WEND + 1
 170  CONTINUE
!

      RETURN
!
!     end of DLARRE
!
      END SUBROUTINE DLARRE
      
      
      
      
      
      SUBROUTINE DLARRF( N, D, L, LD, CLSTRT, CLEND, &
                         W, WGAP, WERR, &
                         SPDIAM, CLGAPL, CLGAPR, PIVMIN, SIGMA, &
                         DPLUS, LPLUS, WORK, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!*
!     .. Scalar Arguments ..
      INTEGER            CLSTRT, CLEND, INFO, N
      DOUBLE PRECISION   CLGAPL, CLGAPR, PIVMIN, SIGMA, SPDIAM
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), DPLUS( * ), L( * ), LD( * ), &
                LPLUS( * ), W( * ), WGAP( * ), WERR( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  Given the initial representation L D L^T and its cluster of close
!  eigenvalues (in a relative measure), W( CLSTRT ), W( CLSTRT+1 ), ...
!  W( CLEND ), DLARRF finds a new relatively robust representation
!  L D L^T - SIGMA I = L(+) D(+) L(+)^T such that at least one of the
!  eigenvalues of L(+) D(+) L(+)^T is relatively isolated.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix (subblock, if the matrix splitted).
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of the diagonal matrix D.
!
!  L       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (N-1) subdiagonal elements of the unit bidiagonal
!          matrix L.
!
!  LD      (input) DOUBLE PRECISION array, dimension (N-1)
!          The (N-1) elements L(i)*D(i).
!
!  CLSTRT  (input) INTEGER
!          The index of the first eigenvalue in the cluster.
!
!  CLEND   (input) INTEGER
!          The index of the last eigenvalue in the cluster.
!
!  W       (input) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
!          The eigenvalue APPROXIMATIONS of L D L^T in ascending order.
!          W( CLSTRT ) through W( CLEND ) form the cluster of relatively
!          close eigenalues.
!
!  WGAP    (input/output) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
!          The separation from the right neighbor eigenvalue in W.
!
!  WERR    (input) DOUBLE PRECISION array, dimension >=  (CLEND-CLSTRT+1)
!          WERR contain the semiwidth of the uncertainty
!          interval of the corresponding eigenvalue APPROXIMATION in W
!
!  SPDIAM (input) estimate of the spectral diameter obtained from the
!          Gerschgorin intervals
!
!  CLGAPL, CLGAPR (input) absolute gap on each end of the cluster.
!          Set by the calling routine to protect against shifts too close
!          to eigenvalues outside the cluster.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot allowed in the Sturm sequence.
!
!  SIGMA   (output) DOUBLE PRECISION
!          The shift used to form L(+) D(+) L(+)^T.
!
!  DPLUS   (output) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of the diagonal matrix D(+).
!
!  LPLUS   (output) DOUBLE PRECISION array, dimension (N-1)
!          The first (N-1) elements of LPLUS contain the subdiagonal
!          elements of the unit bidiagonal matrix L(+).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
!          Workspace.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   FOUR, MAXGROWTH1, MAXGROWTH2, ONE, QUART, TWO, &
                         ZERO
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0, &
                           FOUR = 4.0D0, QUART = 0.25D0, &
                           MAXGROWTH1 = 8.D0, &
                           MAXGROWTH2 = 8.D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL   DORRR1, FORCER, NOFAIL, SAWNAN1, SAWNAN2, TRYRRR1
      INTEGER            I, INDX, KTRY, KTRYMAX, SLEFT, SRIGHT, SHIFT
      PARAMETER          ( KTRYMAX = 1, SLEFT = 1, SRIGHT = 2 )
      DOUBLE PRECISION   AVGAP, BESTSHIFT, CLWDTH, EPS, FACT, FAIL, &
                         FAIL2, GROWTHBOUND, LDELTA, LDMAX, LSIGMA, &
                         MAX1, MAX2, MINGAP, OLDP, PROD, RDELTA, RDMAX, &
                         RRR1, RRR2, RSIGMA, S, SMLGROWTH, TMP, ZNM2
!     ..
!     .. External Functions ..
      LOGICAL DISNAN
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DISNAN, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      INFO = 0
      FACT = DBLE(2**KTRYMAX)
      EPS = DLAMCH( 'Precision' )
      SHIFT = 0
      FORCER = .FALSE.


!     Note that we cannot guarantee that for any of the shifts tried,
!     the factorization has a small or even moderate element growth.
!     There could be Ritz values at both ends of the cluster and despite
!     backing off, there are examples where all factorizations tried
!     (in IEEE mode, allowing zero pivots & infinities) have INFINITE
!     element growth.
!     For this reason, we should use PIVMIN in this subroutine so that at
!     least the L D L^T factorization exists. It can be checked afterwards
!     whether the element growth caused bad residuals/orthogonality.

!     Decide whether the code should accept the best among all
!     representations despite large element growth or signal INFO=1
      NOFAIL = .TRUE.
!

!     Compute the average gap length of the cluster
      CLWDTH = ABS(W(CLEND)-W(CLSTRT)) + WERR(CLEND) + WERR(CLSTRT)
      AVGAP = CLWDTH / DBLE(CLEND-CLSTRT)
      MINGAP = MIN(CLGAPL, CLGAPR)
!     Initial values for shifts to both ends of cluster
      LSIGMA = MIN(W( CLSTRT ),W( CLEND )) - WERR( CLSTRT )
      RSIGMA = MAX(W( CLSTRT ),W( CLEND )) + WERR( CLEND )

!     Use a small fudge to make sure that we really shift to the outside
      LSIGMA = LSIGMA - ABS(LSIGMA)* FOUR * EPS
      RSIGMA = RSIGMA + ABS(RSIGMA)* FOUR * EPS

!     Compute upper bounds for how much to back off the initial shifts
      LDMAX = QUART * MINGAP + TWO * PIVMIN
      RDMAX = QUART * MINGAP + TWO * PIVMIN

      LDELTA = MAX(AVGAP,WGAP( CLSTRT ))/FACT
      RDELTA = MAX(AVGAP,WGAP( CLEND-1 ))/FACT
!
!     Initialize the record of the best representation found
!
      S = DLAMCH( 'S' )
      SMLGROWTH = ONE / S
      FAIL = DBLE(N-1)*MINGAP/(SPDIAM*EPS)
      FAIL2 = DBLE(N-1)*MINGAP/(SPDIAM*SQRT(EPS))
      BESTSHIFT = LSIGMA
!
!     while (KTRY <= KTRYMAX)
      KTRY = 0
      GROWTHBOUND = MAXGROWTH1*SPDIAM

 5    CONTINUE
      SAWNAN1 = .FALSE.
      SAWNAN2 = .FALSE.
!     Ensure that we do not back off too much of the initial shifts
      LDELTA = MIN(LDMAX,LDELTA)
      RDELTA = MIN(RDMAX,RDELTA)

!     Compute the element growth when shifting to both ends of the cluster
!     accept the shift if there is no element growth at one of the two ends

!     Left end
      S = -LSIGMA
      DPLUS( 1 ) = D( 1 ) + S
      IF(ABS(DPLUS(1)).LT.PIVMIN) THEN
         DPLUS(1) = -PIVMIN
!        Need to set SAWNAN1 because refined RRR test should not be used
!        in this case
         SAWNAN1 = .TRUE.
      ENDIF
      MAX1 = ABS( DPLUS( 1 ) )
      DO 6 I = 1, N - 1
         LPLUS( I ) = LD( I ) / DPLUS( I )
         S = S*LPLUS( I )*L( I ) - LSIGMA
         DPLUS( I+1 ) = D( I+1 ) + S
         IF(ABS(DPLUS(I+1)).LT.PIVMIN) THEN
            DPLUS(I+1) = -PIVMIN
!           Need to set SAWNAN1 because refined RRR test should not be used
!           in this case
            SAWNAN1 = .TRUE.
         ENDIF
         MAX1 = MAX( MAX1,ABS(DPLUS(I+1)) )
 6    CONTINUE
      SAWNAN1 = SAWNAN1 .OR.  DISNAN( MAX1 )

      IF( FORCER .OR. &
         (MAX1.LE.GROWTHBOUND .AND. .NOT.SAWNAN1 ) ) THEN
         SIGMA = LSIGMA
         SHIFT = SLEFT
         GOTO 100
      ENDIF

!     Right end
      S = -RSIGMA
      WORK( 1 ) = D( 1 ) + S
      IF(ABS(WORK(1)).LT.PIVMIN) THEN
         WORK(1) = -PIVMIN
!        Need to set SAWNAN2 because refined RRR test should not be used
!        in this case
         SAWNAN2 = .TRUE.
      ENDIF
      MAX2 = ABS( WORK( 1 ) )
      DO 7 I = 1, N - 1
         WORK( N+I ) = LD( I ) / WORK( I )
         S = S*WORK( N+I )*L( I ) - RSIGMA
         WORK( I+1 ) = D( I+1 ) + S
         IF(ABS(WORK(I+1)).LT.PIVMIN) THEN
            WORK(I+1) = -PIVMIN
!           Need to set SAWNAN2 because refined RRR test should not be used
!           in this case
            SAWNAN2 = .TRUE.
         ENDIF
         MAX2 = MAX( MAX2,ABS(WORK(I+1)) )
 7    CONTINUE
      SAWNAN2 = SAWNAN2 .OR.  DISNAN( MAX2 )

      IF( FORCER .OR. &
         (MAX2.LE.GROWTHBOUND .AND. .NOT.SAWNAN2 ) ) THEN
         SIGMA = RSIGMA
         SHIFT = SRIGHT
         GOTO 100
      ENDIF
!     If we are at this point, both shifts led to too much element growth

!     Record the better of the two shifts (provided it didn't lead to NaN)
      IF(SAWNAN1.AND.SAWNAN2) THEN
!        both MAX1 and MAX2 are NaN
         GOTO 50
      ELSE
         IF( .NOT.SAWNAN1 ) THEN
            INDX = 1
            IF(MAX1.LE.SMLGROWTH) THEN
               SMLGROWTH = MAX1
               BESTSHIFT = LSIGMA
            ENDIF
         ENDIF
         IF( .NOT.SAWNAN2 ) THEN
            IF(SAWNAN1 .OR. MAX2.LE.MAX1) INDX = 2
            IF(MAX2.LE.SMLGROWTH) THEN
               SMLGROWTH = MAX2
               BESTSHIFT = RSIGMA
            ENDIF
         ENDIF
      ENDIF

!     If we are here, both the left and the right shift led to
!     element growth. If the element growth is moderate, then
!     we may still accept the representation, if it passes a
!     refined test for RRR. This test supposes that no NaN occurred.
!     Moreover, we use the refined RRR test only for isolated clusters.
      IF((CLWDTH.LT.MINGAP/DBLE(128)) .AND. &
         (MIN(MAX1,MAX2).LT.FAIL2) &
        .AND.(.NOT.SAWNAN1).AND.(.NOT.SAWNAN2)) THEN
         DORRR1 = .TRUE.
      ELSE
         DORRR1 = .FALSE.
      ENDIF
      TRYRRR1 = .TRUE.
      IF( TRYRRR1 .AND. DORRR1 ) THEN
      IF(INDX.EQ.1) THEN
         TMP = ABS( DPLUS( N ) )
         ZNM2 = ONE
         PROD = ONE
         OLDP = ONE
         DO 15 I = N-1, 1, -1
            IF( PROD .LE. EPS ) THEN
               PROD = &
               ((DPLUS(I+1)*WORK(N+I+1))/(DPLUS(I)*WORK(N+I)))*OLDP
            ELSE
               PROD = PROD*ABS(WORK(N+I))
            ENDIF
            OLDP = PROD
            ZNM2 = ZNM2 + PROD**2
            TMP = MAX( TMP, ABS( DPLUS( I ) * PROD ))
 15      CONTINUE
         RRR1 = TMP/( SPDIAM * SQRT( ZNM2 ) )
         IF (RRR1.LE.MAXGROWTH2) THEN
            SIGMA = LSIGMA
            SHIFT = SLEFT
            GOTO 100
         ENDIF
      ELSE IF(INDX.EQ.2) THEN
         TMP = ABS( WORK( N ) )
         ZNM2 = ONE
         PROD = ONE
         OLDP = ONE
         DO 16 I = N-1, 1, -1
            IF( PROD .LE. EPS ) THEN
               PROD = ((WORK(I+1)*LPLUS(I+1))/(WORK(I)*LPLUS(I)))*OLDP
            ELSE
               PROD = PROD*ABS(LPLUS(I))
            ENDIF
            OLDP = PROD
            ZNM2 = ZNM2 + PROD**2
            TMP = MAX( TMP, ABS( WORK( I ) * PROD ))
 16      CONTINUE
         RRR2 = TMP/( SPDIAM * SQRT( ZNM2 ) )
         IF (RRR2.LE.MAXGROWTH2) THEN
            SIGMA = RSIGMA
            SHIFT = SRIGHT
            GOTO 100
         ENDIF
      ENDIF
      ENDIF

 50   CONTINUE

      IF (KTRY.LT.KTRYMAX) THEN
!        If we are here, both shifts failed also the RRR test.
!        Back off to the outside
         LSIGMA = MAX( LSIGMA - LDELTA, &
           LSIGMA - LDMAX)
         RSIGMA = MIN( RSIGMA + RDELTA, &
           RSIGMA + RDMAX )
         LDELTA = TWO * LDELTA
         RDELTA = TWO * RDELTA
         KTRY = KTRY + 1
         GOTO 5
      ELSE
!        None of the representations investigated satisfied our
!        criteria. Take the best one we found.
         IF((SMLGROWTH.LT.FAIL).OR.NOFAIL) THEN
            LSIGMA = BESTSHIFT
            RSIGMA = BESTSHIFT
            FORCER = .TRUE.
            GOTO 5
         ELSE
            INFO = 1
            RETURN
         ENDIF
      ENDIF

 100  CONTINUE
      IF (SHIFT.EQ.SLEFT) THEN
      ELSEIF (SHIFT.EQ.SRIGHT) THEN
!        store new L and D back into DPLUS, LPLUS
         CALL DCOPY( N, WORK, 1, DPLUS, 1 )
         CALL DCOPY( N-1, WORK(N+1), 1, LPLUS, 1 )
      ENDIF

      RETURN
!
!     End of DLARRF
!
      END SUBROUTINE DLARRF
      
      
      
      
      
      SUBROUTINE DLARRJ( N, D, E2, IFIRST, ILAST, &
                         RTOL, OFFSET, W, WERR, WORK, IWORK, &
                         PIVMIN, SPDIAM, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            IFIRST, ILAST, INFO, N, OFFSET
      DOUBLE PRECISION   PIVMIN, RTOL, SPDIAM
!     ..
!     .. Array Arguments ..
      INTEGER            IWORK( * )
      DOUBLE PRECISION   D( * ), E2( * ), W( * ), &
                         WERR( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  Given the initial eigenvalue approximations of T, DLARRJ
!  does  bisection to refine the eigenvalues of T,
!  W( IFIRST-OFFSET ) through W( ILAST-OFFSET ), to more accuracy. Initial
!  guesses for these eigenvalues are input in W, the corresponding estimate
!  of the error in these guesses in WERR. During bisection, intervals
!  [left, right] are maintained by storing their mid-points and
!  semi-widths in the arrays W and WERR respectively.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of T.
!
!  E2      (input) DOUBLE PRECISION array, dimension (N-1)
!          The Squares of the (N-1) subdiagonal elements of T.
!
!  IFIRST  (input) INTEGER
!          The index of the first eigenvalue to be computed.
!
!  ILAST   (input) INTEGER
!          The index of the last eigenvalue to be computed.
!
!  RTOL   (input) DOUBLE PRECISION
!          Tolerance for the convergence of the bisection intervals.
!          An interval [LEFT,RIGHT] has converged if
!          RIGHT-LEFT.LT.RTOL*MAX(|LEFT|,|RIGHT|).
!
!  OFFSET  (input) INTEGER
!          Offset for the arrays W and WERR, i.e., the IFIRST-OFFSET
!          through ILAST-OFFSET elements of these arrays are to be used.
!
!  W       (input/output) DOUBLE PRECISION array, dimension (N)
!          On input, W( IFIRST-OFFSET ) through W( ILAST-OFFSET ) are
!          estimates of the eigenvalues of L D L^T indexed IFIRST through
!          ILAST.
!          On output, these estimates are refined.
!
!  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
!          On input, WERR( IFIRST-OFFSET ) through WERR( ILAST-OFFSET ) are
!          the errors in the estimates of the corresponding elements in W.
!          On output, these errors are refined.
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (2*N)
!          Workspace.
!
!  IWORK   (workspace) INTEGER array, dimension (2*N)
!          Workspace.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot in the Sturm sequence for T.
!
!  SPDIAM  (input) DOUBLE PRECISION
!          The spectral diameter of T.
!
!  INFO    (output) INTEGER
!          Error flag.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF
      PARAMETER        ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0, &
                         HALF = 0.5D0 )
      INTEGER   MAXITR
!     ..
!     .. Local Scalars ..
      INTEGER            CNT, I, I1, I2, II, ITER, J, K, NEXT, NINT, &
                         OLNINT, P, PREV, SAVI1
      DOUBLE PRECISION   DPLUS, FAC, LEFT, MID, RIGHT, S, TMP, WIDTH
!
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
!     ..
!     .. Executable Statements ..
!
      INFO = 0
!
      MAXITR = INT( ( LOG( SPDIAM+PIVMIN )-LOG( PIVMIN ) ) / &
                 LOG( TWO ) ) + 2
!
!     Initialize unconverged intervals in [ WORK(2*I-1), WORK(2*I) ].
!     The Sturm Count, Count( WORK(2*I-1) ) is arranged to be I-1, while
!     Count( WORK(2*I) ) is stored in IWORK( 2*I ). The integer IWORK( 2*I-1 )
!     for an unconverged interval is set to the index of the next unconverged
!     interval, and is -1 or 0 for a converged interval. Thus a linked
!     list of unconverged intervals is set up.
!

      I1 = IFIRST
      I2 = ILAST
!     The number of unconverged intervals
      NINT = 0
!     The last unconverged interval found
      PREV = 0
      DO 75 I = I1, I2
         K = 2*I
         II = I - OFFSET
         LEFT = W( II ) - WERR( II )
         MID = W(II)
         RIGHT = W( II ) + WERR( II )
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )

!        The following test prevents the test of converged intervals
         IF( WIDTH.LT.RTOL*TMP ) THEN
!           This interval has already converged and does not need refinement.
!           (Note that the gaps might change through refining the
!            eigenvalues, however, they can only get bigger.)
!           Remove it from the list.
            IWORK( K-1 ) = -1
!           Make sure that I1 always points to the first unconverged interval
            IF((I.EQ.I1).AND.(I.LT.I2)) I1 = I + 1
            IF((PREV.GE.I1).AND.(I.LE.I2)) IWORK( 2*PREV-1 ) = I + 1
         ELSE
!           unconverged interval found
            PREV = I
!           Make sure that [LEFT,RIGHT] contains the desired eigenvalue
!
!           Do while( CNT(LEFT).GT.I-1 )
!
            FAC = ONE
 20         CONTINUE
            CNT = 0
            S = LEFT
            DPLUS = D( 1 ) - S
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
            DO 30 J = 2, N
               DPLUS = D( J ) - S - E2( J-1 )/DPLUS
               IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 30         CONTINUE
            IF( CNT.GT.I-1 ) THEN
               LEFT = LEFT - WERR( II )*FAC
               FAC = TWO*FAC
               GO TO 20
            ENDIF
!
!           Do while( CNT(RIGHT).LT.I )
!
            FAC = ONE
 50         CONTINUE
            CNT = 0
            S = RIGHT
            DPLUS = D( 1 ) - S
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
            DO 60 J = 2, N
               DPLUS = D( J ) - S - E2( J-1 )/DPLUS
               IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 60         CONTINUE
            IF( CNT.LT.I ) THEN
               RIGHT = RIGHT + WERR( II )*FAC
               FAC = TWO*FAC
               GO TO 50
            ENDIF
            NINT = NINT + 1
            IWORK( K-1 ) = I + 1
            IWORK( K ) = CNT
         ENDIF
         WORK( K-1 ) = LEFT
         WORK( K ) = RIGHT
 75   CONTINUE


      SAVI1 = I1
!
!     Do while( NINT.GT.0 ), i.e. there are still unconverged intervals
!     and while (ITER.LT.MAXITR)
!
      ITER = 0
 80   CONTINUE
      PREV = I1 - 1
      I = I1
      OLNINT = NINT

      DO 100 P = 1, OLNINT
         K = 2*I
         II = I - OFFSET
         NEXT = IWORK( K-1 )
         LEFT = WORK( K-1 )
         RIGHT = WORK( K )
         MID = HALF*( LEFT + RIGHT )

!        semiwidth of interval
         WIDTH = RIGHT - MID
         TMP = MAX( ABS( LEFT ), ABS( RIGHT ) )

         IF( ( WIDTH.LT.RTOL*TMP ) .OR. &
            (ITER.EQ.MAXITR) )THEN
!           reduce number of unconverged intervals
            NINT = NINT - 1
!           Mark interval as converged.
            IWORK( K-1 ) = 0
            IF( I1.EQ.I ) THEN
               I1 = NEXT
            ELSE
!              Prev holds the last unconverged interval previously examined
               IF(PREV.GE.I1) IWORK( 2*PREV-1 ) = NEXT
            ENDIF
            I = NEXT
            GO TO 100
         ENDIF
         PREV = I
!
!        Perform one bisection step
!
         CNT = 0
         S = MID
         DPLUS = D( 1 ) - S
         IF( DPLUS.LT.ZERO ) CNT = CNT + 1
         DO 90 J = 2, N
            DPLUS = D( J ) - S - E2( J-1 )/DPLUS
            IF( DPLUS.LT.ZERO ) CNT = CNT + 1
 90      CONTINUE
         IF( CNT.LE.I-1 ) THEN
            WORK( K-1 ) = MID
         ELSE
            WORK( K ) = MID
         ENDIF
         I = NEXT

 100  CONTINUE
      ITER = ITER + 1
!     do another loop if there are still unconverged intervals
!     However, in the last iteration, all intervals are accepted
!     since this is the best we can do.
      IF( ( NINT.GT.0 ).AND.(ITER.LE.MAXITR) ) GO TO 80
!
!
!     At this point, all the intervals have converged
      DO 110 I = SAVI1, ILAST
         K = 2*I
         II = I - OFFSET
!        All intervals marked by '0' have been refined.
         IF( IWORK( K-1 ).EQ.0 ) THEN
            W( II ) = HALF*( WORK( K-1 )+WORK( K ) )
            WERR( II ) = WORK( K ) - W( II )
         ENDIF
 110  CONTINUE
!

      RETURN
!
!     End of DLARRJ
!
      END SUBROUTINE DLARRJ
      
      
      
      
      
      SUBROUTINE DLARRK( N, IW, GL, GU, &
                          D, E2, PIVMIN, RELTOL, W, WERR, INFO)
      IMPLICIT NONE
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER   INFO, IW, N
      DOUBLE PRECISION    PIVMIN, RELTOL, GL, GU, W, WERR
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E2( * )
!     ..
!
!  Purpose
!  =======
!
!  DLARRK computes one eigenvalue of a symmetric tridiagonal
!  matrix T to suitable accuracy. This is an auxiliary code to be
!  called from DSTEMR.
!
!  To avoid overflow, the matrix must be scaled so that its
!  largest element is no greater than overflow**(1/2) *
!  underflow**(1/4) in absolute value, and for greatest
!  accuracy, it should not be much smaller than that.
!
!  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
!  Matrix", Report CS41, Computer Science Dept., Stanford
!  University, July 21, 1966.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the tridiagonal matrix T.  N >= 0.
!
!  IW      (input) INTEGER
!          The index of the eigenvalues to be returned.
!
!  GL      (input) DOUBLE PRECISION
!  GU      (input) DOUBLE PRECISION
!          An upper and a lower bound on the eigenvalue.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The n diagonal elements of the tridiagonal matrix T.
!
!  E2      (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) squared off-diagonal elements of the tridiagonal matrix T.
!
!  PIVMIN  (input) DOUBLE PRECISION
!          The minimum pivot allowed in the Sturm sequence for T.
!
!  RELTOL  (input) DOUBLE PRECISION
!          The minimum relative width of an interval.  When an interval
!          is narrower than RELTOL times the larger (in
!          magnitude) endpoint, then it is considered to be
!          sufficiently small, i.e., converged.  Note: this should
!          always be at least radix*machine epsilon.
!
!  W       (output) DOUBLE PRECISION
!
!  WERR    (output) DOUBLE PRECISION
!          The error bound on the corresponding eigenvalue approximation
!          in W.
!
!  INFO    (output) INTEGER
!          = 0:       Eigenvalue converged
!          = -1:      Eigenvalue did NOT converge
!
!  Internal Parameters
!  ===================
!
!  FUDGE   DOUBLE PRECISION, default = 2
!          A "fudge factor" to widen the Gershgorin intervals.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   FUDGE, HALF, TWO, ZERO
      PARAMETER          ( HALF = 0.5D0, TWO = 2.0D0, &
                           FUDGE = TWO, ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER   I, IT, ITMAX, NEGCNT
      DOUBLE PRECISION   ATOLI, EPS, LEFT, MID, RIGHT, RTOLI, TMP1, &
                         TMP2, TNORM
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL   DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX
!     ..
!     .. Executable Statements ..
!
!     Get machine constants
      EPS = DLAMCH( 'P' )

      TNORM = MAX( ABS( GL ), ABS( GU ) )
      RTOLI = RELTOL
      ATOLI = FUDGE*TWO*PIVMIN

      ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) / &
                 LOG( TWO ) ) + 2

      INFO = -1

      LEFT = GL - FUDGE*TNORM*EPS*N - FUDGE*TWO*PIVMIN
      RIGHT = GU + FUDGE*TNORM*EPS*N + FUDGE*TWO*PIVMIN
      IT = 0

 10   CONTINUE
!
!     Check if interval converged or maximum number of iterations reached
!
      TMP1 = ABS( RIGHT - LEFT )
      TMP2 = MAX( ABS(RIGHT), ABS(LEFT) )
      IF( TMP1.LT.MAX( ATOLI, PIVMIN, RTOLI*TMP2 ) ) THEN
         INFO = 0
         GOTO 30
      ENDIF
      IF(IT.GT.ITMAX) &
         GOTO 30

!
!     Count number of negative pivots for mid-point
!
      IT = IT + 1
      MID = HALF * (LEFT + RIGHT)
      NEGCNT = 0
      TMP1 = D( 1 ) - MID
      IF( ABS( TMP1 ).LT.PIVMIN ) &
         TMP1 = -PIVMIN
      IF( TMP1.LE.ZERO ) &
         NEGCNT = NEGCNT + 1
!
      DO 20 I = 2, N
         TMP1 = D( I ) - E2( I-1 ) / TMP1 - MID
         IF( ABS( TMP1 ).LT.PIVMIN ) &
            TMP1 = -PIVMIN
         IF( TMP1.LE.ZERO ) &
            NEGCNT = NEGCNT + 1
 20   CONTINUE

      IF(NEGCNT.GE.IW) THEN
         RIGHT = MID
      ELSE
         LEFT = MID
      ENDIF
      GOTO 10

 30   CONTINUE
!
!     Converged or maximum number of iterations reached
!
      W = HALF * (LEFT + RIGHT)
      WERR = HALF * ABS( RIGHT - LEFT )

      RETURN
!
!     End of DLARRK
!
      END SUBROUTINE DLARRK
      
      
      
      
      
      SUBROUTINE DLARRR( N, D, E, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            N, INFO
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
!     ..
!
!
!  Purpose
!  =======
!
!  Perform tests to decide whether the symmetric tridiagonal matrix T
!  warrants expensive computations which guarantee high relative accuracy
!  in the eigenvalues.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix. N > 0.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The N diagonal elements of the tridiagonal matrix T.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the first (N-1) entries contain the subdiagonal
!          elements of the tridiagonal matrix T; E(N) is set to ZERO.
!
!  INFO    (output) INTEGER
!          INFO = 0(default) : the matrix warrants computations preserving
!                              relative accuracy.
!          INFO = 1          : the matrix warrants computations guaranteeing
!                              only absolute accuracy.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, RELCOND
      PARAMETER          ( ZERO = 0.0D0, &
                           RELCOND = 0.999D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I
      LOGICAL            YESREL
      DOUBLE PRECISION   EPS, SAFMIN, SMLNUM, RMIN, TMP, TMP2, &
                OFFDIG, OFFDIG2

!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
!     As a default, do NOT go for relative-accuracy preserving computations.
      INFO = 1

      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      RMIN = SQRT( SMLNUM )

!     Tests for relative accuracy
!
!     Test for scaled diagonal dominance
!     Scale the diagonal entries to one and check whether the sum of the
!     off-diagonals is less than one
!
!     The sdd relative error bounds have a 1/(1- 2*x) factor in them,
!     x = max(OFFDIG + OFFDIG2), so when x is close to 1/2, no relative
!     accuracy is promised.  In the notation of the code fragment below,
!     1/(1 - (OFFDIG + OFFDIG2)) is the condition number.
!     We don't think it is worth going into "sdd mode" unless the relative
!     condition number is reasonable, not 1/macheps.
!     The threshold should be compatible with other thresholds used in the
!     code. We set  OFFDIG + OFFDIG2 <= .999 =: RELCOND, it corresponds
!     to losing at most 3 decimal digits: 1 / (1 - (OFFDIG + OFFDIG2)) <= 1000
!     instead of the current OFFDIG + OFFDIG2 < 1
!
      YESREL = .TRUE.
      OFFDIG = ZERO
      TMP = SQRT(ABS(D(1)))
      IF (TMP.LT.RMIN) YESREL = .FALSE.
      IF(.NOT.YESREL) GOTO 11
      DO 10 I = 2, N
         TMP2 = SQRT(ABS(D(I)))
         IF (TMP2.LT.RMIN) YESREL = .FALSE.
         IF(.NOT.YESREL) GOTO 11
         OFFDIG2 = ABS(E(I-1))/(TMP*TMP2)
         IF(OFFDIG+OFFDIG2.GE.RELCOND) YESREL = .FALSE.
         IF(.NOT.YESREL) GOTO 11
         TMP = TMP2
         OFFDIG = OFFDIG2
 10   CONTINUE
 11   CONTINUE

      IF( YESREL ) THEN
         INFO = 0
         RETURN
      ELSE
      ENDIF
!

!
!     *** MORE TO BE IMPLEMENTED ***
!

!
!     Test if the lower bidiagonal matrix L from T = L D L^T
!     (zero shift facto) is well conditioned
!

!
!     Test if the upper bidiagonal matrix U from T = U D U^T
!     (zero shift facto) is well conditioned.
!     In this case, the matrix needs to be flipped and, at the end
!     of the eigenvector computation, the flip needs to be applied
!     to the computed eigenvectors (and the support)
!

!
      RETURN
!
!     END OF DLARRR
!
      END SUBROUTINE DLARRR
      
      
      
      
      
      SUBROUTINE DLARRV( N, VL, VU, D, L, PIVMIN, &
                         ISPLIT, M, DOL, DOU, MINRGP, &
                         RTOL1, RTOL2, W, WERR, WGAP, &
                         IBLOCK, INDEXW, GERS, Z, LDZ, ISUPPZ, &
                         WORK, IWORK, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            DOL, DOU, INFO, LDZ, M, N
      DOUBLE PRECISION   MINRGP, PIVMIN, RTOL1, RTOL2, VL, VU
!     ..
!     .. Array Arguments ..
      INTEGER            IBLOCK( * ), INDEXW( * ), ISPLIT( * ), &
                         ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), GERS( * ), L( * ), W( * ), WERR( * ), &
                         WGAP( * ), WORK( * )
      DOUBLE PRECISION  Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  DLARRV computes the eigenvectors of the tridiagonal matrix
!  T = L D L^T given L, D and APPROXIMATIONS to the eigenvalues of L D L^T.
!  The input eigenvalues should have been computed by DLARRE.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          Lower and upper bounds of the interval that contains the desired
!          eigenvalues. VL < VU. Needed to compute gaps on the left or right
!          end of the extremal eigenvalues in the desired RANGE.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the N diagonal elements of the diagonal matrix D.
!          On exit, D may be overwritten.
!
!  L       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the (N-1) subdiagonal elements of the unit
!          bidiagonal matrix L are in elements 1 to N-1 of L
!          (if the matrix is not splitted.) At the end of each block
!          is stored the corresponding shift as given by DLARRE.
!          On exit, L is overwritten.
!
!  PIVMIN  (in) DOUBLE PRECISION
!          The minimum pivot allowed in the Sturm sequence.
!
!  ISPLIT  (input) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into blocks.
!          The first block consists of rows/columns 1 to
!          ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
!          through ISPLIT( 2 ), etc.
!
!  M       (input) INTEGER
!          The total number of input eigenvalues.  0 <= M <= N.
!
!  DOL     (input) INTEGER
!  DOU     (input) INTEGER
!          If the user wants to compute only selected eigenvectors from all
!          the eigenvalues supplied, he can specify an index range DOL:DOU.
!          Or else the setting DOL=1, DOU=M should be applied.
!          Note that DOL and DOU refer to the order in which the eigenvalues
!          are stored in W.
!          If the user wants to compute only selected eigenpairs, then
!          the columns DOL-1 to DOU+1 of the eigenvector space Z contain the
!          computed eigenvectors. All other columns of Z are set to zero.
!
!  MINRGP  (input) DOUBLE PRECISION
!
!  RTOL1   (input) DOUBLE PRECISION
!  RTOL2   (input) DOUBLE PRECISION
!           Parameters for bisection.
!           An interval [LEFT,RIGHT] has converged if
!           RIGHT-LEFT.LT.MAX( RTOL1*GAP, RTOL2*MAX(|LEFT|,|RIGHT|) )
!
!  W       (input/output) DOUBLE PRECISION array, dimension (N)
!          The first M elements of W contain the APPROXIMATE eigenvalues for
!          which eigenvectors are to be computed.  The eigenvalues
!          should be grouped by split-off block and ordered from
!          smallest to largest within the block ( The output array
!          W from DLARRE is expected here ). Furthermore, they are with
!          respect to the shift of the corresponding root representation
!          for their block. On exit, W holds the eigenvalues of the
!          UNshifted matrix.
!
!  WERR    (input/output) DOUBLE PRECISION array, dimension (N)
!          The first M elements contain the semiwidth of the uncertainty
!          interval of the corresponding eigenvalue in W
!
!  WGAP    (input/output) DOUBLE PRECISION array, dimension (N)
!          The separation from the right neighbor eigenvalue in W.
!
!  IBLOCK  (input) INTEGER array, dimension (N)
!          The indices of the blocks (submatrices) associated with the
!          corresponding eigenvalues in W; IBLOCK(i)=1 if eigenvalue
!          W(i) belongs to the first block from the top, =2 if W(i)
!          belongs to the second block, etc.
!
!  INDEXW  (input) INTEGER array, dimension (N)
!          The indices of the eigenvalues within each block (submatrix);
!          for example, INDEXW(i)= 10 and IBLOCK(i)=2 imply that the
!          i-th eigenvalue W(i) is the 10-th eigenvalue in the second block.
!
!  GERS    (input) DOUBLE PRECISION array, dimension (2*N)
!          The N Gerschgorin intervals (the i-th Gerschgorin interval
!          is (GERS(2*i-1), GERS(2*i)). The Gerschgorin intervals should
!          be computed from the original UNshifted matrix.
!
!  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
!          If INFO = 0, the first M columns of Z contain the
!          orthonormal eigenvectors of the matrix T
!          corresponding to the input eigenvalues, with the i-th
!          column of Z holding the eigenvector associated with W(i).
!          Note: the user must ensure that at least max(1,M) columns are
!          supplied in the array Z.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          JOBZ = 'V', LDZ >= max(1,N).
!
!  ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
!          The support of the eigenvectors in Z, i.e., the indices
!          indicating the nonzero elements in Z. The I-th eigenvector
!          is nonzero only in elements ISUPPZ( 2*I-1 ) through
!          ISUPPZ( 2*I ).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (12*N)
!
!  IWORK   (workspace) INTEGER array, dimension (7*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!
!          > 0:  A problem occured in DLARRV.
!          < 0:  One of the called subroutines signaled an internal problem.
!                Needs inspection of the corresponding parameter IINFO
!                for further information.
!
!          =-1:  Problem in DLARRB when refining a child's eigenvalues.
!          =-2:  Problem in DLARRF when computing the RRR of a child.
!                When a child is inside a tight cluster, it can be difficult
!                to find an RRR. A partial remedy from the user's point of
!                view is to make the parameter MINRGP smaller and recompile.
!                However, as the orthogonality of the computed vectors is
!                proportional to 1/MINRGP, the user should be aware that
!                he might be trading in precision when he decreases MINRGP.
!          =-3:  Problem in DLARRB when refining a single eigenvalue
!                after the Rayleigh correction was rejected.
!          = 5:  The Rayleigh Quotient Iteration failed to converge to
!                full accuracy in MAXITR steps.
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            MAXITR
      PARAMETER          ( MAXITR = 10 )
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE, FOUR, HALF
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, &
                           TWO = 2.0D0, THREE = 3.0D0, &
                           FOUR = 4.0D0, HALF = 0.5D0)
!     ..
!     .. Local Scalars ..
      LOGICAL            ESKIP, NEEDBS, STP2II, TRYRQC, USEDBS, USEDRQ
      INTEGER            DONE, I, IBEGIN, IDONE, IEND, II, IINDC1, &
                         IINDC2, IINDR, IINDWK, IINFO, IM, IN, INDEIG, &
                         INDLD, INDLLD, INDWRK, ISUPMN, ISUPMX, ITER, &
                         ITMP1, J, JBLK, K, MINIWSIZE, MINWSIZE, NCLUS, &
                         NDEPTH, NEGCNT, NEWCLS, NEWFST, NEWFTT, NEWLST, &
                         NEWSIZ, OFFSET, OLDCLS, OLDFST, OLDIEN, OLDLST, &
                         OLDNCL, P, PARITY, Q, WBEGIN, WEND, WINDEX, &
                         WINDMN, WINDPL, ZFROM, ZTO, ZUSEDL, ZUSEDU, &
                         ZUSEDW
      DOUBLE PRECISION   BSTRES, BSTW, EPS, FUDGE, GAP, GAPTOL, GL, GU, &
                         LAMBDA, LEFT, LGAP, MINGMA, NRMINV, RESID, &
                         RGAP, RIGHT, RQCORR, RQTOL, SAVGAP, SGNDEF, &
                         SIGMA, SPDIAM, SSIGMA, TAU, TMP, TOL, ZTZ
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAR1V, DLARRB, DLARRF, DLASET, &
                         DSCAL
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC ABS, DBLE, MAX, MIN
!     ..
!     .. Executable Statements ..
!     ..

!     The first N entries of WORK are reserved for the eigenvalues
      INDLD = N+1
      INDLLD= 2*N+1
      INDWRK= 3*N+1
      MINWSIZE = 12 * N

      DO 5 I= 1,MINWSIZE
         WORK( I ) = ZERO
 5    CONTINUE

!     IWORK(IINDR+1:IINDR+N) hold the twist indices R for the
!     factorization used to compute the FP vector
      IINDR = 0
!     IWORK(IINDC1+1:IINC2+N) are used to store the clusters of the current
!     layer and the one above.
      IINDC1 = N
      IINDC2 = 2*N
      IINDWK = 3*N + 1

      MINIWSIZE = 7 * N
      DO 10 I= 1,MINIWSIZE
         IWORK( I ) = 0
 10   CONTINUE

      ZUSEDL = 1
      IF(DOL.GT.1) THEN
!        Set lower bound for use of Z
         ZUSEDL = DOL-1
      ENDIF
      ZUSEDU = M
      IF(DOU.LT.M) THEN
!        Set lower bound for use of Z
         ZUSEDU = DOU+1
      ENDIF
!     The width of the part of Z that is used
      ZUSEDW = ZUSEDU - ZUSEDL + 1


      CALL DLASET( 'Full', N, ZUSEDW, ZERO, ZERO, &
                          Z(1,ZUSEDL), LDZ )

      EPS = DLAMCH( 'Precision' )
      RQTOL = TWO * EPS
!
!     Set expert flags for standard code.
      TRYRQC = .TRUE.

      IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
      ELSE
!        Only selected eigenpairs are computed. Since the other evalues
!        are not refined by RQ iteration, bisection has to compute to full
!        accuracy.
         RTOL1 = FOUR * EPS
         RTOL2 = FOUR * EPS
      ENDIF

!     The entries WBEGIN:WEND in W, WERR, WGAP correspond to the
!     desired eigenvalues. The support of the nonzero eigenvector
!     entries is contained in the interval IBEGIN:IEND.
!     Remark that if k eigenpairs are desired, then the eigenvectors
!     are stored in k contiguous columns of Z.

!     DONE is the number of eigenvectors already computed
      DONE = 0
      IBEGIN = 1
      WBEGIN = 1
      DO 170 JBLK = 1, IBLOCK( M )
         IEND = ISPLIT( JBLK )
         SIGMA = L( IEND )
!        Find the eigenvectors of the submatrix indexed IBEGIN
!        through IEND.
         WEND = WBEGIN - 1
 15      CONTINUE
         IF( WEND.LT.M ) THEN
            IF( IBLOCK( WEND+1 ).EQ.JBLK ) THEN
               WEND = WEND + 1
               GO TO 15
            ENDIF
         ENDIF
         IF( WEND.LT.WBEGIN ) THEN
            IBEGIN = IEND + 1
            GO TO 170
         ELSEIF( (WEND.LT.DOL).OR.(WBEGIN.GT.DOU) ) THEN
            IBEGIN = IEND + 1
            WBEGIN = WEND + 1
            GO TO 170
         ENDIF

!        Find local spectral diameter of the block
         GL = GERS( 2*IBEGIN-1 )
         GU = GERS( 2*IBEGIN )
         DO 20 I = IBEGIN+1 , IEND
            GL = MIN( GERS( 2*I-1 ), GL )
            GU = MAX( GERS( 2*I ), GU )
 20      CONTINUE
         SPDIAM = GU - GL

!        OLDIEN is the last index of the previous block
         OLDIEN = IBEGIN - 1
!        Calculate the size of the current block
         IN = IEND - IBEGIN + 1
!        The number of eigenvalues in the current block
         IM = WEND - WBEGIN + 1

!        This is for a 1x1 block
         IF( IBEGIN.EQ.IEND ) THEN
            DONE = DONE+1
            Z( IBEGIN, WBEGIN ) = ONE
            ISUPPZ( 2*WBEGIN-1 ) = IBEGIN
            ISUPPZ( 2*WBEGIN ) = IBEGIN
            W( WBEGIN ) = W( WBEGIN ) + SIGMA
            WORK( WBEGIN ) = W( WBEGIN )
            IBEGIN = IEND + 1
            WBEGIN = WBEGIN + 1
            GO TO 170
         ENDIF

!        The desired (shifted) eigenvalues are stored in W(WBEGIN:WEND)
!        Note that these can be approximations, in this case, the corresp.
!        entries of WERR give the size of the uncertainty interval.
!        The eigenvalue approximations will be refined when necessary as
!        high relative accuracy is required for the computation of the
!        corresponding eigenvectors.
         CALL DCOPY( IM, W( WBEGIN ), 1, WORK( WBEGIN ), 1 )

!        We store in W the eigenvalue approximations w.r.t. the original
!        matrix T.
         DO 30 I=1,IM
            W(WBEGIN+I-1) = W(WBEGIN+I-1)+SIGMA
 30      CONTINUE


!        NDEPTH is the current depth of the representation tree
         NDEPTH = 0
!        PARITY is either 1 or 0
         PARITY = 1
!        NCLUS is the number of clusters for the next level of the
!        representation tree, we start with NCLUS = 1 for the root
         NCLUS = 1
         IWORK( IINDC1+1 ) = 1
         IWORK( IINDC1+2 ) = IM

!        IDONE is the number of eigenvectors already computed in the current
!        block
         IDONE = 0
!        loop while( IDONE.LT.IM )
!        generate the representation tree for the current block and
!        compute the eigenvectors
   40    CONTINUE
         IF( IDONE.LT.IM ) THEN
!           This is a crude protection against infinitely deep trees
            IF( NDEPTH.GT.M ) THEN
               INFO = -2
               RETURN
            ENDIF
!           breadth first processing of the current level of the representation
!           tree: OLDNCL = number of clusters on current level
            OLDNCL = NCLUS
!           reset NCLUS to count the number of child clusters
            NCLUS = 0
!
            PARITY = 1 - PARITY
            IF( PARITY.EQ.0 ) THEN
               OLDCLS = IINDC1
               NEWCLS = IINDC2
            ELSE
               OLDCLS = IINDC2
               NEWCLS = IINDC1
            ENDIF
!           Process the clusters on the current level
            DO 150 I = 1, OLDNCL
               J = OLDCLS + 2*I
!              OLDFST, OLDLST = first, last index of current cluster.
!                               cluster indices start with 1 and are relative
!                               to WBEGIN when accessing W, WGAP, WERR, Z
               OLDFST = IWORK( J-1 )
               OLDLST = IWORK( J )
               IF( NDEPTH.GT.0 ) THEN
!                 Retrieve relatively robust representation (RRR) of cluster
!                 that has been computed at the previous level
!                 The RRR is stored in Z and overwritten once the eigenvectors
!                 have been computed or when the cluster is refined

                  IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
!                    Get representation from location of the leftmost evalue
!                    of the cluster
                     J = WBEGIN + OLDFST - 1
                  ELSE
                     IF(WBEGIN+OLDFST-1.LT.DOL) THEN
!                       Get representation from the left end of Z array
                        J = DOL - 1
                     ELSEIF(WBEGIN+OLDFST-1.GT.DOU) THEN
!                       Get representation from the right end of Z array
                        J = DOU
                     ELSE
                        J = WBEGIN + OLDFST - 1
                     ENDIF
                  ENDIF
                  CALL DCOPY( IN, Z( IBEGIN, J ), 1, D( IBEGIN ), 1 )
                  CALL DCOPY( IN-1, Z( IBEGIN, J+1 ), 1, L( IBEGIN ), &
                     1 )
                  SIGMA = Z( IEND, J+1 )

!                 Set the corresponding entries in Z to zero
                  CALL DLASET( 'Full', IN, 2, ZERO, ZERO, &
                               Z( IBEGIN, J), LDZ )
               ENDIF

!              Compute DL and DLL of current RRR
               DO 50 J = IBEGIN, IEND-1
                  TMP = D( J )*L( J )
                  WORK( INDLD-1+J ) = TMP
                  WORK( INDLLD-1+J ) = TMP*L( J )
   50          CONTINUE

               IF( NDEPTH.GT.0 ) THEN
!                 P and Q are index of the first and last eigenvalue to compute
!                 within the current block
                  P = INDEXW( WBEGIN-1+OLDFST )
                  Q = INDEXW( WBEGIN-1+OLDLST )
!                 Offset for the arrays WORK, WGAP and WERR, i.e., th P-OFFSET
!                 thru' Q-OFFSET elements of these arrays are to be used.
!                  OFFSET = P-OLDFST
                  OFFSET = INDEXW( WBEGIN ) - 1
!                 perform limited bisection (if necessary) to get approximate
!                 eigenvalues to the precision needed.
                  CALL DLARRB( IN, D( IBEGIN ), &
                               WORK(INDLLD+IBEGIN-1), &
                               P, Q, RTOL1, RTOL2, OFFSET, &
                               WORK(WBEGIN),WGAP(WBEGIN),WERR(WBEGIN), &
                               WORK( INDWRK ), IWORK( IINDWK ), &
                               PIVMIN, SPDIAM, IN, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     INFO = -1
                     RETURN
                  ENDIF
!                 We also recompute the extremal gaps. W holds all eigenvalues
!                 of the unshifted matrix and must be used for computation
!                 of WGAP, the entries of WORK might stem from RRRs with
!                 different shifts. The gaps from WBEGIN-1+OLDFST to
!                 WBEGIN-1+OLDLST are correctly computed in DLARRB.
!                 However, we only allow the gaps to become greater since
!                 this is what should happen when we decrease WERR
                  IF( OLDFST.GT.1) THEN
                     WGAP( WBEGIN+OLDFST-2 ) = &
                   MAX(WGAP(WBEGIN+OLDFST-2), &
                       W(WBEGIN+OLDFST-1)-WERR(WBEGIN+OLDFST-1) &
                       - W(WBEGIN+OLDFST-2)-WERR(WBEGIN+OLDFST-2) )
                  ENDIF
                  IF( WBEGIN + OLDLST -1 .LT. WEND ) THEN
                     WGAP( WBEGIN+OLDLST-1 ) = &
                     MAX(WGAP(WBEGIN+OLDLST-1), &
                         W(WBEGIN+OLDLST)-WERR(WBEGIN+OLDLST) &
                         - W(WBEGIN+OLDLST-1)-WERR(WBEGIN+OLDLST-1) )
                  ENDIF
!                 Each time the eigenvalues in WORK get refined, we store
!                 the newly found approximation with all shifts applied in W
                  DO 53 J=OLDFST,OLDLST
                     W(WBEGIN+J-1) = WORK(WBEGIN+J-1)+SIGMA
 53               CONTINUE
               ENDIF

!              Process the current node.
               NEWFST = OLDFST
               DO 140 J = OLDFST, OLDLST
                  IF( J.EQ.OLDLST ) THEN
!                    we are at the right end of the cluster, this is also the
!                    boundary of the child cluster
                     NEWLST = J
                  ELSE IF ( WGAP( WBEGIN + J -1).GE. &
                          MINRGP* ABS( WORK(WBEGIN + J -1) ) ) THEN
!                    the right relative gap is big enough, the child cluster
!                    (NEWFST,..,NEWLST) is well separated from the following
                     NEWLST = J
                   ELSE
!                    inside a child cluster, the relative gap is not
!                    big enough.
                     GOTO 140
                  ENDIF

!                 Compute size of child cluster found
                  NEWSIZ = NEWLST - NEWFST + 1

!                 NEWFTT is the place in Z where the new RRR or the computed
!                 eigenvector is to be stored
                  IF((DOL.EQ.1).AND.(DOU.EQ.M)) THEN
!                    Store representation at location of the leftmost evalue
!                    of the cluster
                     NEWFTT = WBEGIN + NEWFST - 1
                  ELSE
                     IF(WBEGIN+NEWFST-1.LT.DOL) THEN
!                       Store representation at the left end of Z array
                        NEWFTT = DOL - 1
                     ELSEIF(WBEGIN+NEWFST-1.GT.DOU) THEN
!                       Store representation at the right end of Z array
                        NEWFTT = DOU
                     ELSE
                        NEWFTT = WBEGIN + NEWFST - 1
                     ENDIF
                  ENDIF

                  IF( NEWSIZ.GT.1) THEN
!
!                    Current child is not a singleton but a cluster.
!                    Compute and store new representation of child.
!
!
!                    Compute left and right cluster gap.
!
!                    LGAP and RGAP are not computed from WORK because
!                    the eigenvalue approximations may stem from RRRs
!                    different shifts. However, W hold all eigenvalues
!                    of the unshifted matrix. Still, the entries in WGAP
!                    have to be computed from WORK since the entries
!                    in W might be of the same order so that gaps are not
!                    exhibited correctly for very close eigenvalues.
                     IF( NEWFST.EQ.1 ) THEN
                        LGAP = MAX( ZERO, &
                             W(WBEGIN)-WERR(WBEGIN) - VL )
                    ELSE
                        LGAP = WGAP( WBEGIN+NEWFST-2 )
                     ENDIF
                     RGAP = WGAP( WBEGIN+NEWLST-1 )
!
!                    Compute left- and rightmost eigenvalue of child
!                    to high precision in order to shift as close
!                    as possible and obtain as large relative gaps
!                    as possible
!
                     DO 55 K =1,2
                        IF(K.EQ.1) THEN
                           P = INDEXW( WBEGIN-1+NEWFST )
                        ELSE
                           P = INDEXW( WBEGIN-1+NEWLST )
                        ENDIF
                        OFFSET = INDEXW( WBEGIN ) - 1
                        CALL DLARRB( IN, D(IBEGIN), &
                             WORK( INDLLD+IBEGIN-1 ),P,P, &
                             RQTOL, RQTOL, OFFSET, &
                             WORK(WBEGIN),WGAP(WBEGIN), &
                             WERR(WBEGIN),WORK( INDWRK ), &
                             IWORK( IINDWK ), PIVMIN, SPDIAM, &
                             IN, IINFO )
 55                  CONTINUE
!
                     IF((WBEGIN+NEWLST-1.LT.DOL).OR. &
                        (WBEGIN+NEWFST-1.GT.DOU)) THEN
!                       if the cluster contains no desired eigenvalues
!                       skip the computation of that branch of the rep. tree
!
!                       We could skip before the refinement of the extremal
!                       eigenvalues of the child, but then the representation
!                       tree could be different from the one when nothing is
!                       skipped. For this reason we skip at this place.
                        IDONE = IDONE + NEWLST - NEWFST + 1
                        GOTO 139
                     ENDIF
!
!                    Compute RRR of child cluster.
!                    Note that the new RRR is stored in Z
!
!                    DLARRF needs LWORK = 2*N
                     CALL DLARRF( IN, D( IBEGIN ), L( IBEGIN ), &
                               WORK(INDLD+IBEGIN-1), &
                               NEWFST, NEWLST, WORK(WBEGIN), &
                               WGAP(WBEGIN), WERR(WBEGIN), &
                               SPDIAM, LGAP, RGAP, PIVMIN, TAU, &
                               Z(IBEGIN, NEWFTT),Z(IBEGIN, NEWFTT+1), &
                               WORK( INDWRK ), IINFO )
                     IF( IINFO.EQ.0 ) THEN
!                       a new RRR for the cluster was found by DLARRF
!                       update shift and store it
                        SSIGMA = SIGMA + TAU
                        Z( IEND, NEWFTT+1 ) = SSIGMA
!                       WORK() are the midpoints and WERR() the semi-width
!                       Note that the entries in W are unchanged.
                        DO 116 K = NEWFST, NEWLST
                           FUDGE = &
                                THREE*EPS*ABS(WORK(WBEGIN+K-1))
                           WORK( WBEGIN + K - 1 ) = &
                                WORK( WBEGIN + K - 1) - TAU
                           FUDGE = FUDGE + &
                                FOUR*EPS*ABS(WORK(WBEGIN+K-1))
!                          Fudge errors
                           WERR( WBEGIN + K - 1 ) = &
                                WERR( WBEGIN + K - 1 ) + FUDGE
!                          Gaps are not fudged. Provided that WERR is small
!                          when eigenvalues are close, a zero gap indicates
!                          that a new representation is needed for resolving
!                          the cluster. A fudge could lead to a wrong decision
!                          of judging eigenvalues 'separated' which in
!                          reality are not. This could have a negative impact
!                          on the orthogonality of the computed eigenvectors.
 116                    CONTINUE

                        NCLUS = NCLUS + 1
                        K = NEWCLS + 2*NCLUS
                        IWORK( K-1 ) = NEWFST
                        IWORK( K ) = NEWLST
                     ELSE
                        INFO = -2
                        RETURN
                     ENDIF
                  ELSE
!
!                    Compute eigenvector of singleton
!
                     ITER = 0
!
                     TOL = FOUR * LOG(DBLE(IN)) * EPS
!
                     K = NEWFST
                     WINDEX = WBEGIN + K - 1
                     WINDMN = MAX(WINDEX - 1,1)
                     WINDPL = MIN(WINDEX + 1,M)
                     LAMBDA = WORK( WINDEX )
                     DONE = DONE + 1
!                    Check if eigenvector computation is to be skipped
                     IF((WINDEX.LT.DOL).OR. &
                        (WINDEX.GT.DOU)) THEN
                        ESKIP = .TRUE.
                        GOTO 125
                     ELSE
                        ESKIP = .FALSE.
                     ENDIF
                     LEFT = WORK( WINDEX ) - WERR( WINDEX )
                     RIGHT = WORK( WINDEX ) + WERR( WINDEX )
                     INDEIG = INDEXW( WINDEX )
!                    Note that since we compute the eigenpairs for a child,
!                    all eigenvalue approximations are w.r.t the same shift.
!                    In this case, the entries in WORK should be used for
!                    computing the gaps since they exhibit even very small
!                    differences in the eigenvalues, as opposed to the
!                    entries in W which might "look" the same.

                     IF( K .EQ. 1) THEN
!                       In the case RANGE='I' and with not much initial
!                       accuracy in LAMBDA and VL, the formula
!                       LGAP = MAX( ZERO, (SIGMA - VL) + LAMBDA )
!                       can lead to an overestimation of the left gap and
!                       thus to inadequately early RQI 'convergence'.
!                       Prevent this by forcing a small left gap.
                        LGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                     ELSE
                        LGAP = WGAP(WINDMN)
                     ENDIF
                     IF( K .EQ. IM) THEN
!                       In the case RANGE='I' and with not much initial
!                       accuracy in LAMBDA and VU, the formula
!                       can lead to an overestimation of the right gap and
!                       thus to inadequately early RQI 'convergence'.
!                       Prevent this by forcing a small right gap.
                        RGAP = EPS*MAX(ABS(LEFT),ABS(RIGHT))
                     ELSE
                        RGAP = WGAP(WINDEX)
                     ENDIF
                     GAP = MIN( LGAP, RGAP )
                     IF(( K .EQ. 1).OR.(K .EQ. IM)) THEN
!                       The eigenvector support can become wrong
!                       because significant entries could be cut off due to a
!                       large GAPTOL parameter in LAR1V. Prevent this.
                        GAPTOL = ZERO
                     ELSE
                        GAPTOL = GAP * EPS
                     ENDIF
                     ISUPMN = IN
                     ISUPMX = 1
!                    Update WGAP so that it holds the minimum gap
!                    to the left or the right. This is crucial in the
!                    case where bisection is used to ensure that the
!                    eigenvalue is refined up to the required precision.
!                    The correct value is restored afterwards.
                     SAVGAP = WGAP(WINDEX)
                     WGAP(WINDEX) = GAP
!                    We want to use the Rayleigh Quotient Correction
!                    as often as possible since it converges quadratically
!                    when we are close enough to the desired eigenvalue.
!                    However, the Rayleigh Quotient can have the wrong sign
!                    and lead us away from the desired eigenvalue. In this
!                    case, the best we can do is to use bisection.
                     USEDBS = .FALSE.
                     USEDRQ = .FALSE.
!                    Bisection is initially turned off unless it is forced
                     NEEDBS =  .NOT.TRYRQC
 120                 CONTINUE
!                    Check if bisection should be used to refine eigenvalue
                     IF(NEEDBS) THEN
!                       Take the bisection as new iterate
                        USEDBS = .TRUE.
                        ITMP1 = IWORK( IINDR+WINDEX )
                        OFFSET = INDEXW( WBEGIN ) - 1
                        CALL DLARRB( IN, D(IBEGIN), &
                             WORK(INDLLD+IBEGIN-1),INDEIG,INDEIG, &
                             ZERO, TWO*EPS, OFFSET, &
                             WORK(WBEGIN),WGAP(WBEGIN), &
                             WERR(WBEGIN),WORK( INDWRK ), &
                             IWORK( IINDWK ), PIVMIN, SPDIAM, &
                             ITMP1, IINFO )
                        IF( IINFO.NE.0 ) THEN
                           INFO = -3
                           RETURN
                        ENDIF
                        LAMBDA = WORK( WINDEX )
!                       Reset twist index from inaccurate LAMBDA to
!                       force computation of true MINGMA
                        IWORK( IINDR+WINDEX ) = 0
                     ENDIF
!                    Given LAMBDA, compute the eigenvector.
                     CALL DLAR1V( IN, 1, IN, LAMBDA, D( IBEGIN ), &
                          L( IBEGIN ), WORK(INDLD+IBEGIN-1), &
                          WORK(INDLLD+IBEGIN-1), &
                          PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ), &
                          .NOT.USEDBS, NEGCNT, ZTZ, MINGMA, &
                          IWORK( IINDR+WINDEX ), ISUPPZ( 2*WINDEX-1 ), &
                          NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                     IF(ITER .EQ. 0) THEN
                        BSTRES = RESID
                        BSTW = LAMBDA
                     ELSEIF(RESID.LT.BSTRES) THEN
                        BSTRES = RESID
                        BSTW = LAMBDA
                     ENDIF
                     ISUPMN = MIN(ISUPMN,ISUPPZ( 2*WINDEX-1 ))
                     ISUPMX = MAX(ISUPMX,ISUPPZ( 2*WINDEX ))
                     ITER = ITER + 1

!                    sin alpha <= |resid|/gap
!                    Note that both the residual and the gap are
!                    proportional to the matrix, so ||T|| doesn't play
!                    a role in the quotient

!
!                    Convergence test for Rayleigh-Quotient iteration
!                    (omitted when Bisection has been used)
!
                     IF( RESID.GT.TOL*GAP .AND. ABS( RQCORR ).GT. &
                          RQTOL*ABS( LAMBDA ) .AND. .NOT. USEDBS) &
                          THEN
!                       We need to check that the RQCORR update doesn't
!                       move the eigenvalue away from the desired one and
!                       towards a neighbor. -> protection with bisection
                        IF(INDEIG.LE.NEGCNT) THEN
!                          The wanted eigenvalue lies to the left
                           SGNDEF = -ONE
                        ELSE
!                          The wanted eigenvalue lies to the right
                           SGNDEF = ONE
                        ENDIF
!                       We only use the RQCORR if it improves the
!                       the iterate reasonably.
                        IF( ( RQCORR*SGNDEF.GE.ZERO ) &
                             .AND.( LAMBDA + RQCORR.LE. RIGHT) &
                             .AND.( LAMBDA + RQCORR.GE. LEFT) &
                             ) THEN
                           USEDRQ = .TRUE.
!                          Store new midpoint of bisection interval in WORK
                           IF(SGNDEF.EQ.ONE) THEN
!                             The current LAMBDA is on the left of the true
!                             eigenvalue
                              LEFT = LAMBDA
!                             We prefer to assume that the error estimate
!                             is correct. We could make the interval not
!                             as a bracket but to be modified if the RQCORR
!                             chooses to. In this case, the RIGHT side should
!                             be modified as follows:
!                              RIGHT = MAX(RIGHT, LAMBDA + RQCORR)
                           ELSE
!                             The current LAMBDA is on the right of the true
!                             eigenvalue
                              RIGHT = LAMBDA
!                             See comment about assuming the error estimate is
!                             correct above.
!                              LEFT = MIN(LEFT, LAMBDA + RQCORR)
                           ENDIF
                           WORK( WINDEX ) = &
                             HALF * (RIGHT + LEFT)
!                          Take RQCORR since it has the correct sign and
!                          improves the iterate reasonably
                           LAMBDA = LAMBDA + RQCORR
!                          Update width of error interval
                           WERR( WINDEX ) = &
                                   HALF * (RIGHT-LEFT)
                        ELSE
                           NEEDBS = .TRUE.
                        ENDIF
                        IF(RIGHT-LEFT.LT.RQTOL*ABS(LAMBDA)) THEN
!                             The eigenvalue is computed to bisection accuracy
!                             compute eigenvector and stop
                           USEDBS = .TRUE.
                           GOTO 120
                        ELSEIF( ITER.LT.MAXITR ) THEN
                           GOTO 120
                        ELSEIF( ITER.EQ.MAXITR ) THEN
                           NEEDBS = .TRUE.
                           GOTO 120
                        ELSE
                           INFO = 5
                           RETURN
                        ENDIF
                     ELSE
                        STP2II = .FALSE.
        IF(USEDRQ .AND. USEDBS .AND. &
                           BSTRES.LE.RESID) THEN
                           LAMBDA = BSTW
                           STP2II = .TRUE.
                        ENDIF
                        IF (STP2II) THEN
!                          improve error angle by second step
                           CALL DLAR1V( IN, 1, IN, LAMBDA, &
                                D( IBEGIN ), L( IBEGIN ), &
                                WORK(INDLD+IBEGIN-1), &
                                WORK(INDLLD+IBEGIN-1), &
                                PIVMIN, GAPTOL, Z( IBEGIN, WINDEX ), &
                                .NOT.USEDBS, NEGCNT, ZTZ, MINGMA, &
                                IWORK( IINDR+WINDEX ), &
                                ISUPPZ( 2*WINDEX-1 ), &
                                NRMINV, RESID, RQCORR, WORK( INDWRK ) )
                        ENDIF
                        WORK( WINDEX ) = LAMBDA
                     ENDIF
!
!                    Compute FP-vector support w.r.t. whole matrix
!
                     ISUPPZ( 2*WINDEX-1 ) = ISUPPZ( 2*WINDEX-1 )+OLDIEN
                     ISUPPZ( 2*WINDEX ) = ISUPPZ( 2*WINDEX )+OLDIEN
                     ZFROM = ISUPPZ( 2*WINDEX-1 )
                     ZTO = ISUPPZ( 2*WINDEX )
                     ISUPMN = ISUPMN + OLDIEN
                     ISUPMX = ISUPMX + OLDIEN
!                    Ensure vector is ok if support in the RQI has changed
                     IF(ISUPMN.LT.ZFROM) THEN
                        DO 122 II = ISUPMN,ZFROM-1
                           Z( II, WINDEX ) = ZERO
 122                    CONTINUE
                     ENDIF
                     IF(ISUPMX.GT.ZTO) THEN
                        DO 123 II = ZTO+1,ISUPMX
                           Z( II, WINDEX ) = ZERO
 123                    CONTINUE
                     ENDIF
                     CALL DSCAL( ZTO-ZFROM+1, NRMINV, &
                             Z( ZFROM, WINDEX ), 1 )
 125                 CONTINUE
!                    Update W
                     W( WINDEX ) = LAMBDA+SIGMA
!                    Recompute the gaps on the left and right
!                    But only allow them to become larger and not
!                    smaller (which can only happen through "bad"
!                    cancellation and doesn't reflect the theory
!                    where the initial gaps are underestimated due
!                    to WERR being too crude.)
                     IF(.NOT.ESKIP) THEN
                        IF( K.GT.1) THEN
                           WGAP( WINDMN ) = MAX( WGAP(WINDMN), &
                                W(WINDEX)-WERR(WINDEX) &
                                - W(WINDMN)-WERR(WINDMN) )
                        ENDIF
                        IF( WINDEX.LT.WEND ) THEN
                           WGAP( WINDEX ) = MAX( SAVGAP, &
                                W( WINDPL )-WERR( WINDPL ) &
                                - W( WINDEX )-WERR( WINDEX) )
                        ENDIF
                     ENDIF
                     IDONE = IDONE + 1
                  ENDIF
!                 here ends the code for the current child
!
 139              CONTINUE
!                 Proceed to any remaining child nodes
                  NEWFST = J + 1
 140           CONTINUE
 150        CONTINUE
            NDEPTH = NDEPTH + 1
            GO TO 40
         ENDIF
         IBEGIN = IEND + 1
         WBEGIN = WEND + 1
 170  CONTINUE
!

      RETURN
!
!     End of DLARRV
!
      END SUBROUTINE DLARRV
      
      
      
      
      
      SUBROUTINE DLARUV( ISEED, N, X )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            N
!     ..
!     .. Array Arguments ..
      INTEGER            ISEED( 4 )
      DOUBLE PRECISION   X( N )
!     ..
!
!  Purpose
!  =======
!
!  DLARUV returns a vector of n random real numbers from a uniform (0,1)
!  distribution (n <= 128).
!
!  This is an auxiliary routine called by DLARNV and ZLARNV.
!
!  Arguments
!  =========
!
!  ISEED   (input/output) INTEGER array, dimension (4)
!          On entry, the seed of the random number generator; the array
!          elements must be between 0 and 4095, and ISEED(4) must be
!          odd.
!          On exit, the seed is updated.
!
!  N       (input) INTEGER
!          The number of random numbers to be generated. N <= 128.
!
!  X       (output) DOUBLE PRECISION array, dimension (N)
!          The generated random numbers.
!
!  Further Details
!  ===============
!
!  This routine uses a multiplicative congruential method with modulus
!  2**48 and multiplier 33952834046453 (see G.S.Fishman,
!  'Multiplicative congruential random number generators with modulus
!  2**b: an exhaustive analysis for b = 32 and a partial analysis for
!  b = 48', Math. Comp. 189, pp 331-344, 1990).
!
!  48-bit integers are stored in 4 integer array elements with 12 bits
!  per element. Hence the routine is portable across machines with
!  integers of 32 bits or more.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D0 )
      INTEGER            LV, IPW2
      DOUBLE PRECISION   R
      PARAMETER          ( LV = 128, IPW2 = 4096, R = ONE / IPW2 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, I1, I2, I3, I4, IT1, IT2, IT3, IT4, J
!     ..
!     .. Local Arrays ..
      INTEGER            MM( LV, 4 )
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MIN, MOD
!     ..
!     .. Data statements ..
      DATA               ( MM( 1, J ), J = 1, 4 ) / 494, 322, 2508, &
                         2549 /
      DATA               ( MM( 2, J ), J = 1, 4 ) / 2637, 789, 3754, &
                         1145 /
      DATA               ( MM( 3, J ), J = 1, 4 ) / 255, 1440, 1766, &
                         2253 /
      DATA               ( MM( 4, J ), J = 1, 4 ) / 2008, 752, 3572, &
                         305 /
      DATA               ( MM( 5, J ), J = 1, 4 ) / 1253, 2859, 2893, &
                         3301 /
      DATA               ( MM( 6, J ), J = 1, 4 ) / 3344, 123, 307, &
                         1065 /
      DATA               ( MM( 7, J ), J = 1, 4 ) / 4084, 1848, 1297, &
                         3133 /
      DATA               ( MM( 8, J ), J = 1, 4 ) / 1739, 643, 3966, &
                         2913 /
      DATA               ( MM( 9, J ), J = 1, 4 ) / 3143, 2405, 758, &
                         3285 /
      DATA               ( MM( 10, J ), J = 1, 4 ) / 3468, 2638, 2598, &
                         1241 /
      DATA               ( MM( 11, J ), J = 1, 4 ) / 688, 2344, 3406, &
                         1197 /
      DATA               ( MM( 12, J ), J = 1, 4 ) / 1657, 46, 2922, &
                         3729 /
      DATA               ( MM( 13, J ), J = 1, 4 ) / 1238, 3814, 1038, &
                         2501 /
      DATA               ( MM( 14, J ), J = 1, 4 ) / 3166, 913, 2934, &
                         1673 /
      DATA               ( MM( 15, J ), J = 1, 4 ) / 1292, 3649, 2091, &
                         541 /
      DATA               ( MM( 16, J ), J = 1, 4 ) / 3422, 339, 2451, &
                         2753 /
      DATA               ( MM( 17, J ), J = 1, 4 ) / 1270, 3808, 1580, &
                         949 /
      DATA               ( MM( 18, J ), J = 1, 4 ) / 2016, 822, 1958, &
                         2361 /
      DATA               ( MM( 19, J ), J = 1, 4 ) / 154, 2832, 2055, &
                         1165 /
      DATA               ( MM( 20, J ), J = 1, 4 ) / 2862, 3078, 1507, &
                         4081 /
      DATA               ( MM( 21, J ), J = 1, 4 ) / 697, 3633, 1078, &
                         2725 /
      DATA               ( MM( 22, J ), J = 1, 4 ) / 1706, 2970, 3273, &
                         3305 /
      DATA               ( MM( 23, J ), J = 1, 4 ) / 491, 637, 17, &
                         3069 /
      DATA               ( MM( 24, J ), J = 1, 4 ) / 931, 2249, 854, &
                         3617 /
      DATA               ( MM( 25, J ), J = 1, 4 ) / 1444, 2081, 2916, &
                         3733 /
      DATA               ( MM( 26, J ), J = 1, 4 ) / 444, 4019, 3971, &
                         409 /
      DATA               ( MM( 27, J ), J = 1, 4 ) / 3577, 1478, 2889, &
                         2157 /
      DATA               ( MM( 28, J ), J = 1, 4 ) / 3944, 242, 3831, &
                         1361 /
      DATA               ( MM( 29, J ), J = 1, 4 ) / 2184, 481, 2621, &
                         3973 /
      DATA               ( MM( 30, J ), J = 1, 4 ) / 1661, 2075, 1541, &
                         1865 /
      DATA               ( MM( 31, J ), J = 1, 4 ) / 3482, 4058, 893, &
                         2525 /
      DATA               ( MM( 32, J ), J = 1, 4 ) / 657, 622, 736, &
                         1409 /
      DATA               ( MM( 33, J ), J = 1, 4 ) / 3023, 3376, 3992, &
                         3445 /
      DATA               ( MM( 34, J ), J = 1, 4 ) / 3618, 812, 787, &
                         3577 /
      DATA               ( MM( 35, J ), J = 1, 4 ) / 1267, 234, 2125, &
                         77 /
      DATA               ( MM( 36, J ), J = 1, 4 ) / 1828, 641, 2364, &
                         3761 /
      DATA               ( MM( 37, J ), J = 1, 4 ) / 164, 4005, 2460, &
                         2149 /
      DATA               ( MM( 38, J ), J = 1, 4 ) / 3798, 1122, 257, &
                         1449 /
      DATA               ( MM( 39, J ), J = 1, 4 ) / 3087, 3135, 1574, &
                         3005 /
      DATA               ( MM( 40, J ), J = 1, 4 ) / 2400, 2640, 3912, &
                         225 /
      DATA               ( MM( 41, J ), J = 1, 4 ) / 2870, 2302, 1216, &
                         85 /
      DATA               ( MM( 42, J ), J = 1, 4 ) / 3876, 40, 3248, &
                         3673 /
      DATA               ( MM( 43, J ), J = 1, 4 ) / 1905, 1832, 3401, &
                         3117 /
      DATA               ( MM( 44, J ), J = 1, 4 ) / 1593, 2247, 2124, &
                         3089 /
      DATA               ( MM( 45, J ), J = 1, 4 ) / 1797, 2034, 2762, &
                         1349 /
      DATA               ( MM( 46, J ), J = 1, 4 ) / 1234, 2637, 149, &
                         2057 /
      DATA               ( MM( 47, J ), J = 1, 4 ) / 3460, 1287, 2245, &
                         413 /
      DATA               ( MM( 48, J ), J = 1, 4 ) / 328, 1691, 166, &
                         65 /
      DATA               ( MM( 49, J ), J = 1, 4 ) / 2861, 496, 466, &
                         1845 /
      DATA               ( MM( 50, J ), J = 1, 4 ) / 1950, 1597, 4018, &
                         697 /
      DATA               ( MM( 51, J ), J = 1, 4 ) / 617, 2394, 1399, &
                         3085 /
      DATA               ( MM( 52, J ), J = 1, 4 ) / 2070, 2584, 190, &
                         3441 /
      DATA               ( MM( 53, J ), J = 1, 4 ) / 3331, 1843, 2879, &
                         1573 /
      DATA               ( MM( 54, J ), J = 1, 4 ) / 769, 336, 153, &
                         3689 /
      DATA               ( MM( 55, J ), J = 1, 4 ) / 1558, 1472, 2320, &
                         2941 /
      DATA               ( MM( 56, J ), J = 1, 4 ) / 2412, 2407, 18, &
                         929 /
      DATA               ( MM( 57, J ), J = 1, 4 ) / 2800, 433, 712, &
                         533 /
      DATA               ( MM( 58, J ), J = 1, 4 ) / 189, 2096, 2159, &
                         2841 /
      DATA               ( MM( 59, J ), J = 1, 4 ) / 287, 1761, 2318, &
                         4077 /
      DATA               ( MM( 60, J ), J = 1, 4 ) / 2045, 2810, 2091, &
                         721 /
      DATA               ( MM( 61, J ), J = 1, 4 ) / 1227, 566, 3443, &
                         2821 /
      DATA               ( MM( 62, J ), J = 1, 4 ) / 2838, 442, 1510, &
                         2249 /
      DATA               ( MM( 63, J ), J = 1, 4 ) / 209, 41, 449, &
                         2397 /
      DATA               ( MM( 64, J ), J = 1, 4 ) / 2770, 1238, 1956, &
                         2817 /
      DATA               ( MM( 65, J ), J = 1, 4 ) / 3654, 1086, 2201, &
                         245 /
      DATA               ( MM( 66, J ), J = 1, 4 ) / 3993, 603, 3137, &
                         1913 /
      DATA               ( MM( 67, J ), J = 1, 4 ) / 192, 840, 3399, &
                         1997 /
      DATA               ( MM( 68, J ), J = 1, 4 ) / 2253, 3168, 1321, &
                         3121 /
      DATA               ( MM( 69, J ), J = 1, 4 ) / 3491, 1499, 2271, &
                         997 /
      DATA               ( MM( 70, J ), J = 1, 4 ) / 2889, 1084, 3667, &
                         1833 /
      DATA               ( MM( 71, J ), J = 1, 4 ) / 2857, 3438, 2703, &
                         2877 /
      DATA               ( MM( 72, J ), J = 1, 4 ) / 2094, 2408, 629, &
                         1633 /
      DATA               ( MM( 73, J ), J = 1, 4 ) / 1818, 1589, 2365, &
                         981 /
      DATA               ( MM( 74, J ), J = 1, 4 ) / 688, 2391, 2431, &
                         2009 /
      DATA               ( MM( 75, J ), J = 1, 4 ) / 1407, 288, 1113, &
                         941 /
      DATA               ( MM( 76, J ), J = 1, 4 ) / 634, 26, 3922, &
                         2449 /
      DATA               ( MM( 77, J ), J = 1, 4 ) / 3231, 512, 2554, &
                         197 /
      DATA               ( MM( 78, J ), J = 1, 4 ) / 815, 1456, 184, &
                         2441 /
      DATA               ( MM( 79, J ), J = 1, 4 ) / 3524, 171, 2099, &
                         285 /
      DATA               ( MM( 80, J ), J = 1, 4 ) / 1914, 1677, 3228, &
                         1473 /
      DATA               ( MM( 81, J ), J = 1, 4 ) / 516, 2657, 4012, &
                         2741 /
      DATA               ( MM( 82, J ), J = 1, 4 ) / 164, 2270, 1921, &
                         3129 /
      DATA               ( MM( 83, J ), J = 1, 4 ) / 303, 2587, 3452, &
                         909 /
      DATA               ( MM( 84, J ), J = 1, 4 ) / 2144, 2961, 3901, &
                         2801 /
      DATA               ( MM( 85, J ), J = 1, 4 ) / 3480, 1970, 572, &
                         421 /
      DATA               ( MM( 86, J ), J = 1, 4 ) / 119, 1817, 3309, &
                         4073 /
      DATA               ( MM( 87, J ), J = 1, 4 ) / 3357, 676, 3171, &
                         2813 /
      DATA               ( MM( 88, J ), J = 1, 4 ) / 837, 1410, 817, &
                         2337 /
      DATA               ( MM( 89, J ), J = 1, 4 ) / 2826, 3723, 3039, &
                         1429 /
      DATA               ( MM( 90, J ), J = 1, 4 ) / 2332, 2803, 1696, &
                         1177 /
      DATA               ( MM( 91, J ), J = 1, 4 ) / 2089, 3185, 1256, &
                         1901 /
      DATA               ( MM( 92, J ), J = 1, 4 ) / 3780, 184, 3715, &
                         81 /
      DATA               ( MM( 93, J ), J = 1, 4 ) / 1700, 663, 2077, &
                         1669 /
      DATA               ( MM( 94, J ), J = 1, 4 ) / 3712, 499, 3019, &
                         2633 /
      DATA               ( MM( 95, J ), J = 1, 4 ) / 150, 3784, 1497, &
                         2269 /
      DATA               ( MM( 96, J ), J = 1, 4 ) / 2000, 1631, 1101, &
                         129 /
      DATA               ( MM( 97, J ), J = 1, 4 ) / 3375, 1925, 717, &
                         1141 /
      DATA               ( MM( 98, J ), J = 1, 4 ) / 1621, 3912, 51, &
                         249 /
      DATA               ( MM( 99, J ), J = 1, 4 ) / 3090, 1398, 981, &
                         3917 /
      DATA               ( MM( 100, J ), J = 1, 4 ) / 3765, 1349, 1978, &
                         2481 /
      DATA               ( MM( 101, J ), J = 1, 4 ) / 1149, 1441, 1813, &
                         3941 /
      DATA               ( MM( 102, J ), J = 1, 4 ) / 3146, 2224, 3881, &
                         2217 /
      DATA               ( MM( 103, J ), J = 1, 4 ) / 33, 2411, 76, &
                         2749 /
      DATA               ( MM( 104, J ), J = 1, 4 ) / 3082, 1907, 3846, &
                         3041 /
      DATA               ( MM( 105, J ), J = 1, 4 ) / 2741, 3192, 3694, &
                         1877 /
      DATA               ( MM( 106, J ), J = 1, 4 ) / 359, 2786, 1682, &
                         345 /
      DATA               ( MM( 107, J ), J = 1, 4 ) / 3316, 382, 124, &
                         2861 /
      DATA               ( MM( 108, J ), J = 1, 4 ) / 1749, 37, 1660, &
                         1809 /
      DATA               ( MM( 109, J ), J = 1, 4 ) / 185, 759, 3997, &
                         3141 /
      DATA               ( MM( 110, J ), J = 1, 4 ) / 2784, 2948, 479, &
                         2825 /
      DATA               ( MM( 111, J ), J = 1, 4 ) / 2202, 1862, 1141, &
                         157 /
      DATA               ( MM( 112, J ), J = 1, 4 ) / 2199, 3802, 886, &
                         2881 /
      DATA               ( MM( 113, J ), J = 1, 4 ) / 1364, 2423, 3514, &
                         3637 /
      DATA               ( MM( 114, J ), J = 1, 4 ) / 1244, 2051, 1301, &
                         1465 /
      DATA               ( MM( 115, J ), J = 1, 4 ) / 2020, 2295, 3604, &
                         2829 /
      DATA               ( MM( 116, J ), J = 1, 4 ) / 3160, 1332, 1888, &
                         2161 /
      DATA               ( MM( 117, J ), J = 1, 4 ) / 2785, 1832, 1836, &
                         3365 /
      DATA               ( MM( 118, J ), J = 1, 4 ) / 2772, 2405, 1990, &
                         361 /
      DATA               ( MM( 119, J ), J = 1, 4 ) / 1217, 3638, 2058, &
                         2685 /
      DATA               ( MM( 120, J ), J = 1, 4 ) / 1822, 3661, 692, &
                         3745 /
      DATA               ( MM( 121, J ), J = 1, 4 ) / 1245, 327, 1194, &
                         2325 /
      DATA               ( MM( 122, J ), J = 1, 4 ) / 2252, 3660, 20, &
                         3609 /
      DATA               ( MM( 123, J ), J = 1, 4 ) / 3904, 716, 3285, &
                         3821 /
      DATA               ( MM( 124, J ), J = 1, 4 ) / 2774, 1842, 2046, &
                         3537 /
      DATA               ( MM( 125, J ), J = 1, 4 ) / 997, 3987, 2107, &
                         517 /
      DATA               ( MM( 126, J ), J = 1, 4 ) / 2573, 1368, 3508, &
                         3017 /
      DATA               ( MM( 127, J ), J = 1, 4 ) / 1148, 1848, 3525, &
                         2141 /
      DATA               ( MM( 128, J ), J = 1, 4 ) / 545, 2366, 3801, &
                         1537 /
!     ..
!     .. Executable Statements ..
!
      I1 = ISEED( 1 )
      I2 = ISEED( 2 )
      I3 = ISEED( 3 )
      I4 = ISEED( 4 )
!
      DO 10 I = 1, MIN( N, LV )
!	  
  20     CONTINUE
!
!        Multiply the seed by i-th power of the multiplier modulo 2**48
!
         IT4 = I4*MM( I, 4 )
         IT3 = IT4 / IPW2
         IT4 = IT4 - IPW2*IT3
         IT3 = IT3 + I3*MM( I, 4 ) + I4*MM( I, 3 )
         IT2 = IT3 / IPW2
         IT3 = IT3 - IPW2*IT2
         IT2 = IT2 + I2*MM( I, 4 ) + I3*MM( I, 3 ) + I4*MM( I, 2 )
         IT1 = IT2 / IPW2
         IT2 = IT2 - IPW2*IT1
         IT1 = IT1 + I1*MM( I, 4 ) + I2*MM( I, 3 ) + I3*MM( I, 2 ) + &
               I4*MM( I, 1 )
         IT1 = MOD( IT1, IPW2 )
!
!        Convert 48-bit integer to a real number in the interval (0,1)
!
         X( I ) = R*( DBLE( IT1 )+R*( DBLE( IT2 )+R*( DBLE( IT3 )+R* &
                  DBLE( IT4 ) ) ) )
!
         IF (X( I ).EQ.1.0D0) THEN
!           If a real number has n bits of precision, and the first
!           n bits of the 48-bit integer above happen to be all 1 (which
!           will occur about once every 2**n calls), then X( I ) will
!           be rounded to exactly 1.0. 
!           Since X( I ) is not supposed to return exactly 0.0 or 1.0,
!           the statistically correct thing to do in this situation is
!           simply to iterate again.
!           N.B. the case X( I ) = 0.0 should not be possible.	
            I1 = I1 + 2
            I2 = I2 + 2
            I3 = I3 + 2
            I4 = I4 + 2
            GOTO 20
         ENDIF
!
   10 CONTINUE
!
!     Return final value of seed
!
      ISEED( 1 ) = IT1
      ISEED( 2 ) = IT2
      ISEED( 3 ) = IT3
      ISEED( 4 ) = IT4
      RETURN
!
!     End of DLARUV
!
      END SUBROUTINE DLARUV
      
      
      
      
      
      SUBROUTINE DLASCL( TYPE, KL, KU, CFROM, CTO, M, N, A, LDA, INFO )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          TYPE
      INTEGER            INFO, KL, KU, LDA, M, N
      DOUBLE PRECISION   CFROM, CTO
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DLASCL multiplies the M by N real matrix A by the real scalar
!  CTO/CFROM.  This is done without over/underflow as long as the final
!  result CTO*A(I,J)/CFROM does not over/underflow. TYPE specifies that
!  A may be full, upper triangular, lower triangular, upper Hessenberg,
!  or banded.
!
!  Arguments
!  =========
!
!  TYPE    (input) CHARACTER*1
!          TYPE indices the storage type of the input matrix.
!          = 'G':  A is a full matrix.
!          = 'L':  A is a lower triangular matrix.
!          = 'U':  A is an upper triangular matrix.
!          = 'H':  A is an upper Hessenberg matrix.
!          = 'B':  A is a symmetric band matrix with lower bandwidth KL
!                  and upper bandwidth KU and with the only the lower
!                  half stored.
!          = 'Q':  A is a symmetric band matrix with lower bandwidth KL
!                  and upper bandwidth KU and with the only the upper
!                  half stored.
!          = 'Z':  A is a band matrix with lower bandwidth KL and upper
!                  bandwidth KU.
!
!  KL      (input) INTEGER
!          The lower bandwidth of A.  Referenced only if TYPE = 'B',
!          'Q' or 'Z'.
!
!  KU      (input) INTEGER
!          The upper bandwidth of A.  Referenced only if TYPE = 'B',
!          'Q' or 'Z'.
!
!  CFROM   (input) DOUBLE PRECISION
!  CTO     (input) DOUBLE PRECISION
!          The matrix A is multiplied by CTO/CFROM. A(I,J) is computed
!          without over/underflow if the final result CTO*A(I,J)/CFROM
!          can be represented without over/underflow.  CFROM must be
!          nonzero.
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          The matrix to be multiplied by CTO/CFROM.  See TYPE for the
!          storage type.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
!  INFO    (output) INTEGER
!          0  - successful exit
!          <0 - if INFO = -i, the i-th argument had an illegal value.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            DONE
      INTEGER            I, ITYPE, J, K1, K2, K3, K4
      DOUBLE PRECISION   BIGNUM, CFROM1, CFROMC, CTO1, CTOC, MUL, SMLNUM
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, MIN
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
!
      IF( LSAME( TYPE, 'G' ) ) THEN
         ITYPE = 0
      ELSE IF( LSAME( TYPE, 'L' ) ) THEN
         ITYPE = 1
      ELSE IF( LSAME( TYPE, 'U' ) ) THEN
         ITYPE = 2
      ELSE IF( LSAME( TYPE, 'H' ) ) THEN
         ITYPE = 3
      ELSE IF( LSAME( TYPE, 'B' ) ) THEN
         ITYPE = 4
      ELSE IF( LSAME( TYPE, 'Q' ) ) THEN
         ITYPE = 5
      ELSE IF( LSAME( TYPE, 'Z' ) ) THEN
         ITYPE = 6
      ELSE
         ITYPE = -1
      ENDIF
!
      IF( ITYPE.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( CFROM.EQ.ZERO ) THEN
         INFO = -4
      ELSE IF( M.LT.0 ) THEN
         INFO = -6
      ELSE IF( N.LT.0 .OR. ( ITYPE.EQ.4 .AND. N.NE.M ) .OR. &
               ( ITYPE.EQ.5 .AND. N.NE.M ) ) THEN
         INFO = -7
      ELSE IF( ITYPE.LE.3 .AND. LDA.LT.MAX( 1, M ) ) THEN
         INFO = -9
      ELSE IF( ITYPE.GE.4 ) THEN
         IF( KL.LT.0 .OR. KL.GT.MAX( M-1, 0 ) ) THEN
            INFO = -2
         ELSE IF( KU.LT.0 .OR. KU.GT.MAX( N-1, 0 ) .OR. &
                  ( ( ITYPE.EQ.4 .OR. ITYPE.EQ.5 ) .AND. KL.NE.KU ) ) &
                   THEN
            INFO = -3
         ELSE IF( ( ITYPE.EQ.4 .AND. LDA.LT.KL+1 ) .OR. &
                  ( ITYPE.EQ.5 .AND. LDA.LT.KU+1 ) .OR. &
                  ( ITYPE.EQ.6 .AND. LDA.LT.2*KL+KU+1 ) ) THEN
            INFO = -9
         ENDIF
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASCL', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. M.EQ.0 ) &
         RETURN
!
!     Get machine parameters
!
      SMLNUM = DLAMCH( 'S' )
      BIGNUM = ONE / SMLNUM
!
      CFROMC = CFROM
      CTOC = CTO
!
   10 CONTINUE
      CFROM1 = CFROMC*SMLNUM
      CTO1 = CTOC / BIGNUM
      IF( ABS( CFROM1 ).GT.ABS( CTOC ) .AND. CTOC.NE.ZERO ) THEN
         MUL = SMLNUM
         DONE = .FALSE.
         CFROMC = CFROM1
      ELSE IF( ABS( CTO1 ).GT.ABS( CFROMC ) ) THEN
         MUL = BIGNUM
         DONE = .FALSE.
         CTOC = CTO1
      ELSE
         MUL = CTOC / CFROMC
         DONE = .TRUE.
      ENDIF
!
      IF( ITYPE.EQ.0 ) THEN
!
!        Full matrix
!
         DO 30 J = 1, N
            DO 20 I = 1, M
               A( I, J ) = A( I, J )*MUL
   20       CONTINUE
   30    CONTINUE
!
      ELSE IF( ITYPE.EQ.1 ) THEN
!
!        Lower triangular matrix
!
         DO 50 J = 1, N
            DO 40 I = J, M
               A( I, J ) = A( I, J )*MUL
   40       CONTINUE
   50    CONTINUE
!
      ELSE IF( ITYPE.EQ.2 ) THEN
!
!        Upper triangular matrix
!
         DO 70 J = 1, N
            DO 60 I = 1, MIN( J, M )
               A( I, J ) = A( I, J )*MUL
   60       CONTINUE
   70    CONTINUE
!
      ELSE IF( ITYPE.EQ.3 ) THEN
!
!        Upper Hessenberg matrix
!
         DO 90 J = 1, N
            DO 80 I = 1, MIN( J+1, M )
               A( I, J ) = A( I, J )*MUL
   80       CONTINUE
   90    CONTINUE
!
      ELSE IF( ITYPE.EQ.4 ) THEN
!
!        Lower half of a symmetric band matrix
!
         K3 = KL + 1
         K4 = N + 1
         DO 110 J = 1, N
            DO 100 I = 1, MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  100       CONTINUE
  110    CONTINUE
!
      ELSE IF( ITYPE.EQ.5 ) THEN
!
!        Upper half of a symmetric band matrix
!
         K1 = KU + 2
         K3 = KU + 1
         DO 130 J = 1, N
            DO 120 I = MAX( K1-J, 1 ), K3
               A( I, J ) = A( I, J )*MUL
  120       CONTINUE
  130    CONTINUE
!
      ELSE IF( ITYPE.EQ.6 ) THEN
!
!        Band matrix
!
         K1 = KL + KU + 2
         K2 = KL + 1
         K3 = 2*KL + KU + 1
         K4 = KL + KU + 1 + M
         DO 150 J = 1, N
            DO 140 I = MAX( K1-J, K2 ), MIN( K3, K4-J )
               A( I, J ) = A( I, J )*MUL
  140       CONTINUE
  150    CONTINUE
!
      ENDIF
!
      IF( .NOT.DONE ) &
         GO TO 10
!
      RETURN
!
!     End of DLASCL
!
      END SUBROUTINE DLASCL
      
      
      
      
      
      SUBROUTINE DLASET( UPLO, M, N, ALPHA, BETA, A, LDA )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, M, N
      DOUBLE PRECISION   ALPHA, BETA
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * )
!     ..
!
!  Purpose
!  =======
!
!  DLASET initializes an m-by-n matrix A to BETA on the diagonal and
!  ALPHA on the offdiagonals.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies the part of the matrix A to be set.
!          = 'U':      Upper triangular part is set; the strictly lower
!                      triangular part of A is not changed.
!          = 'L':      Lower triangular part is set; the strictly upper
!                      triangular part of A is not changed.
!          Otherwise:  All of the matrix A is set.
!
!  M       (input) INTEGER
!          The number of rows of the matrix A.  M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix A.  N >= 0.
!
!  ALPHA   (input) DOUBLE PRECISION
!          The constant to which the offdiagonal elements are to be set.
!
!  BETA    (input) DOUBLE PRECISION
!          The constant to which the diagonal elements are to be set.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On exit, the leading m-by-n submatrix of A is set as follows:
!
!          if UPLO = 'U', A(i,j) = ALPHA, 1<=i<=j-1, 1<=j<=n,
!          if UPLO = 'L', A(i,j) = ALPHA, j+1<=i<=m, 1<=j<=n,
!          otherwise,     A(i,j) = ALPHA, 1<=i<=m, 1<=j<=n, i.ne.j,
!
!          and, for all UPLO, A(i,i) = BETA, 1<=i<=min(m,n).
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,M).
!
! =====================================================================
!
!     .. Local Scalars ..
      INTEGER            I, J
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MIN
!     ..
!     .. Executable Statements ..
!
      IF( LSAME( UPLO, 'U' ) ) THEN
!
!        Set the strictly upper triangular or trapezoidal part of the
!        array to ALPHA.
!
         DO 20 J = 2, N
            DO 10 I = 1, MIN( J-1, M )
               A( I, J ) = ALPHA
   10       CONTINUE
   20    CONTINUE
!
      ELSE IF( LSAME( UPLO, 'L' ) ) THEN
!
!        Set the strictly lower triangular or trapezoidal part of the
!        array to ALPHA.
!
         DO 40 J = 1, MIN( M, N )
            DO 30 I = J + 1, M
               A( I, J ) = ALPHA
   30       CONTINUE
   40    CONTINUE
!
      ELSE
!
!        Set the leading m-by-n submatrix to ALPHA.
!
         DO 60 J = 1, N
            DO 50 I = 1, M
               A( I, J ) = ALPHA
   50       CONTINUE
   60    CONTINUE
      ENDIF
!
!     Set the first min(M,N) diagonal elements to BETA.
!
      DO 70 I = 1, MIN( M, N )
         A( I, I ) = BETA
   70 CONTINUE
!
      RETURN
!
!     End of DLASET
!
      END SUBROUTINE DLASET
      
      
      
      
      
      SUBROUTINE DLASQ2( N, Z, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     Modified to call DLAZQ3 in place of DLASQ3, 13 Feb 03, SJH.
!
!     .. Scalar Arguments ..
      INTEGER            INFO, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLASQ2 computes all the eigenvalues of the symmetric positive 
!  definite tridiagonal matrix associated with the qd array Z to high
!  relative accuracy are computed to high relative accuracy, in the
!  absence of denormalization, underflow and overflow.
!
!  To see the relation of Z to the tridiagonal matrix, let L be a
!  unit lower bidiagonal matrix with subdiagonals Z(2,4,6,,..) and
!  let U be an upper bidiagonal matrix with 1's above and diagonal
!  Z(1,3,5,,..). The tridiagonal is L*U or, if you prefer, the
!  symmetric tridiagonal to which it is similar.
!
!  Note : DLASQ2 defines a logical variable, IEEE, which is true
!  on machines which follow ieee-754 floating-point standard in their
!  handling of infinities and NaNs, and false otherwise. This variable
!  is passed to DLAZQ3.
!
!  Arguments
!  =========
!
!  N     (input) INTEGER
!        The number of rows and columns in the matrix. N >= 0.
!
!  Z     (workspace) DOUBLE PRECISION array, dimension ( 4*N )
!        On entry Z holds the qd array. On exit, entries 1 to N hold
!        the eigenvalues in decreasing order, Z( 2*N+1 ) holds the
!        trace, and Z( 2*N+2 ) holds the sum of the eigenvalues. If
!        N > 2, then Z( 2*N+3 ) holds the iteration count, Z( 2*N+4 )
!        holds NDIVS/NIN^2, and Z( 2*N+5 ) holds the percentage of
!        shifts that failed.
!
!  INFO  (output) INTEGER
!        = 0: successful exit
!        < 0: if the i-th argument is a scalar and had an illegal
!             value, then INFO = -i, if the i-th argument is an
!             array and the j-entry had an illegal value, then
!             INFO = -(i*100+j)
!        > 0: the algorithm failed
!              = 1, a split was marked by a positive value in E
!              = 2, current block of Z not diagonalized after 30*N
!                   iterations (in inner while loop)
!              = 3, termination criterion of outer while loop not met 
!                   (program created more than N unreduced blocks)
!
!  Further Details
!  ===============
!  Local Variables: I0:N0 defines a current unreduced segment of Z.
!  The shifts are accumulated in SIGMA. Iteration count is in ITER.
!  Ping-pong is controlled by PP (alternates between 0 and 1).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   CBIAS
      PARAMETER          ( CBIAS = 1.50D0 )
      DOUBLE PRECISION   ZERO, HALF, ONE, TWO, FOUR, HUNDRD
      PARAMETER          ( ZERO = 0.0D0, HALF = 0.5D0, ONE = 1.0D0, &
                           TWO = 2.0D0, FOUR = 4.0D0, HUNDRD = 100.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            IEEE
      INTEGER            I0, I4, IINFO, IPN4, ITER, IWHILA, IWHILB, K,  &
                         N0, NBIG, NDIV, NFAIL, PP, SPLT, TTYPE
      DOUBLE PRECISION   D, DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, E, &
                         EMAX, EMIN, EPS, OLDEMN, QMAX, QMIN, S, SAFMIN, &
                         SIGMA, T, TAU, TEMP, TOL, TOL2, TRACE, ZMAX
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLAZQ3, DLASRT, XERBLA
!     ..
!     .. External Functions ..
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH, ILAENV
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!      
!     Test the input arguments.
!     (in case DLASQ2 is not called by DLASQ1)
!
      INFO = 0
      EPS = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL = EPS*HUNDRD
      TOL2 = TOL**2
!
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DLASQ2', 1 )
         RETURN
      ELSE IF( N.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
!
!        1-by-1 case.
!
         IF( Z( 1 ).LT.ZERO ) THEN
            INFO = -201
            CALL XERBLA( 'DLASQ2', 2 )
         ENDIF
         RETURN
      ELSE IF( N.EQ.2 ) THEN
!
!        2-by-2 case.
!
         IF( Z( 2 ).LT.ZERO .OR. Z( 3 ).LT.ZERO ) THEN
            INFO = -2
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         ELSE IF( Z( 3 ).GT.Z( 1 ) ) THEN
            D = Z( 3 )
            Z( 3 ) = Z( 1 )
            Z( 1 ) = D
         ENDIF
         Z( 5 ) = Z( 1 ) + Z( 2 ) + Z( 3 )
         IF( Z( 2 ).GT.Z( 3 )*TOL2 ) THEN
            T = HALF*( ( Z( 1 )-Z( 3 ) )+Z( 2 ) ) 
            S = Z( 3 )*( Z( 2 ) / T )
            IF( S.LE.T ) THEN
               S = Z( 3 )*( Z( 2 ) / ( T*( ONE+SQRT( ONE+S / T ) ) ) )
            ELSE
               S = Z( 3 )*( Z( 2 ) / ( T+SQRT( T )*SQRT( T+S ) ) )
            ENDIF
            T = Z( 1 ) + ( S+Z( 2 ) )
            Z( 3 ) = Z( 3 )*( Z( 1 ) / T )
            Z( 1 ) = T
         ENDIF
         Z( 2 ) = Z( 3 )
         Z( 6 ) = Z( 2 ) + Z( 1 )
         RETURN
      ENDIF
!
!     Check for negative data and compute sums of q's and e's.
!
      Z( 2*N ) = ZERO
      EMIN = Z( 2 )
      QMAX = ZERO
      ZMAX = ZERO
      D = ZERO
      E = ZERO
!
      DO 10 K = 1, 2*( N-1 ), 2
         IF( Z( K ).LT.ZERO ) THEN
            INFO = -( 200+K )
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         ELSE IF( Z( K+1 ).LT.ZERO ) THEN
            INFO = -( 200+K+1 )
            CALL XERBLA( 'DLASQ2', 2 )
            RETURN
         ENDIF
         D = D + Z( K )
         E = E + Z( K+1 )
         QMAX = MAX( QMAX, Z( K ) )
         EMIN = MIN( EMIN, Z( K+1 ) )
         ZMAX = MAX( QMAX, ZMAX, Z( K+1 ) )
   10 CONTINUE
      IF( Z( 2*N-1 ).LT.ZERO ) THEN
         INFO = -( 200+2*N-1 )
         CALL XERBLA( 'DLASQ2', 2 )
         RETURN
      ENDIF
      D = D + Z( 2*N-1 )
      QMAX = MAX( QMAX, Z( 2*N-1 ) )
      ZMAX = MAX( QMAX, ZMAX )
!
!     Check for diagonality.
!
      IF( E.EQ.ZERO ) THEN
         DO 20 K = 2, N
            Z( K ) = Z( 2*K-1 )
   20    CONTINUE
         CALL DLASRT( 'D', N, Z, IINFO )
         Z( 2*N-1 ) = D
         RETURN
      ENDIF
!
      TRACE = D + E
!
!     Check for zero data.
!
      IF( TRACE.EQ.ZERO ) THEN
         Z( 2*N-1 ) = ZERO
         RETURN
      ENDIF
!         
!     Check whether the machine is IEEE conformable.
!         
      IEEE = ILAENV( 10, 'DLASQ2', 'N', 1, 2, 3, 4 ).EQ.1 .AND. &
             ILAENV( 11, 'DLASQ2', 'N', 1, 2, 3, 4 ).EQ.1      
!         
!     Rearrange data for locality: Z=(q1,qq1,e1,ee1,q2,qq2,e2,ee2,...).
!
      DO 30 K = 2*N, 2, -2
         Z( 2*K ) = ZERO 
         Z( 2*K-1 ) = Z( K ) 
         Z( 2*K-2 ) = ZERO 
         Z( 2*K-3 ) = Z( K-1 ) 
   30 CONTINUE
!
      I0 = 1
      N0 = N
!
!     Reverse the qd-array, if warranted.
!
      IF( CBIAS*Z( 4*I0-3 ).LT.Z( 4*N0-3 ) ) THEN
         IPN4 = 4*( I0+N0 )
         DO 40 I4 = 4*I0, 2*( I0+N0-1 ), 4
            TEMP = Z( I4-3 )
            Z( I4-3 ) = Z( IPN4-I4-3 )
            Z( IPN4-I4-3 ) = TEMP
            TEMP = Z( I4-1 )
            Z( I4-1 ) = Z( IPN4-I4-5 )
            Z( IPN4-I4-5 ) = TEMP
   40    CONTINUE
      ENDIF
!
!     Initial split checking via dqd and Li's test.
!
      PP = 0
!
      DO 80 K = 1, 2
!
         D = Z( 4*N0+PP-3 )
         DO 50 I4 = 4*( N0-1 ) + PP, 4*I0 + PP, -4
            IF( Z( I4-1 ).LE.TOL2*D ) THEN
               Z( I4-1 ) = -ZERO
               D = Z( I4-3 )
            ELSE
               D = Z( I4-3 )*( D / ( D+Z( I4-1 ) ) )
            ENDIF
   50    CONTINUE
!
!        dqd maps Z to ZZ plus Li's test.
!
         EMIN = Z( 4*I0+PP+1 )
         D = Z( 4*I0+PP-3 )
         DO 60 I4 = 4*I0 + PP, 4*( N0-1 ) + PP, 4
            Z( I4-2*PP-2 ) = D + Z( I4-1 )
            IF( Z( I4-1 ).LE.TOL2*D ) THEN
               Z( I4-1 ) = -ZERO
               Z( I4-2*PP-2 ) = D
               Z( I4-2*PP ) = ZERO
               D = Z( I4+1 )
            ELSE IF( SAFMIN*Z( I4+1 ).LT.Z( I4-2*PP-2 ) .AND. &
                     SAFMIN*Z( I4-2*PP-2 ).LT.Z( I4+1 ) ) THEN
               TEMP = Z( I4+1 ) / Z( I4-2*PP-2 )
               Z( I4-2*PP ) = Z( I4-1 )*TEMP
               D = D*TEMP
            ELSE
               Z( I4-2*PP ) = Z( I4+1 )*( Z( I4-1 ) / Z( I4-2*PP-2 ) )
               D = Z( I4+1 )*( D / Z( I4-2*PP-2 ) )
            ENDIF
            EMIN = MIN( EMIN, Z( I4-2*PP ) )
   60    CONTINUE 
         Z( 4*N0-PP-2 ) = D
!
!        Now find qmax.
!
         QMAX = Z( 4*I0-PP-2 )
         DO 70 I4 = 4*I0 - PP + 2, 4*N0 - PP - 2, 4
            QMAX = MAX( QMAX, Z( I4 ) )
   70    CONTINUE
!
!        Prepare for the next iteration on K.
!
         PP = 1 - PP
   80 CONTINUE
!
!     Initialise variables to pass to DLAZQ3
!
      TTYPE = 0
      DMIN1 = ZERO
      DMIN2 = ZERO
      DN    = ZERO
      DN1   = ZERO
      DN2   = ZERO
      TAU   = ZERO
!
      ITER = 2
      NFAIL = 0
      NDIV = 2*( N0-I0 )
!
      DO 140 IWHILA = 1, N + 1
         IF( N0.LT.1 )  &
            GO TO 150
!
!        While array unfinished do 
!
!        E(N0) holds the value of SIGMA when submatrix in I0:N0
!        splits from the rest of the array, but is negated.
!      
         DESIG = ZERO
         IF( N0.EQ.N ) THEN
            SIGMA = ZERO
         ELSE
            SIGMA = -Z( 4*N0-1 )
         ENDIF
         IF( SIGMA.LT.ZERO ) THEN
            INFO = 1
            RETURN
         ENDIF
!
!        Find last unreduced submatrix's top index I0, find QMAX and
!        EMIN. Find Gershgorin-type bound if Q's much greater than E's.
!
         EMAX = ZERO 
         IF( N0.GT.I0 ) THEN
            EMIN = ABS( Z( 4*N0-5 ) )
         ELSE
            EMIN = ZERO
         ENDIF
         QMIN = Z( 4*N0-3 )
         QMAX = QMIN
         DO 90 I4 = 4*N0, 8, -4
            IF( Z( I4-5 ).LE.ZERO ) &
               GO TO 100
            IF( QMIN.GE.FOUR*EMAX ) THEN
               QMIN = MIN( QMIN, Z( I4-3 ) )
               EMAX = MAX( EMAX, Z( I4-5 ) )
            ENDIF
            QMAX = MAX( QMAX, Z( I4-7 )+Z( I4-5 ) )
            EMIN = MIN( EMIN, Z( I4-5 ) )
   90    CONTINUE
         I4 = 4 
!
  100    CONTINUE
         I0 = I4 / 4
!
!        Store EMIN for passing to DLAZQ3.
!
         Z( 4*N0-1 ) = EMIN
!
!        Put -(initial shift) into DMIN.
!
         DMIN = -MAX( ZERO, QMIN-TWO*SQRT( QMIN )*SQRT( EMAX ) )
!
!        Now I0:N0 is unreduced. PP = 0 for ping, PP = 1 for pong.
!
         PP = 0 
!
         NBIG = 30*( N0-I0+1 )
         DO 120 IWHILB = 1, NBIG
            IF( I0.GT.N0 )  &
               GO TO 130
!
!           While submatrix unfinished take a good dqds step.
!
            CALL DLAZQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL, &
                         ITER, NDIV, IEEE, TTYPE, DMIN1, DMIN2, DN, DN1, &
                         DN2, TAU )
!
            PP = 1 - PP
!
!           When EMIN is very small check for splits.
!
            IF( PP.EQ.0 .AND. N0-I0.GE.3 ) THEN
               IF( Z( 4*N0 ).LE.TOL2*QMAX .OR. &
                   Z( 4*N0-1 ).LE.TOL2*SIGMA ) THEN
                  SPLT = I0 - 1
                  QMAX = Z( 4*I0-3 )
                  EMIN = Z( 4*I0-1 )
                  OLDEMN = Z( 4*I0 )
                  DO 110 I4 = 4*I0, 4*( N0-3 ), 4
                     IF( Z( I4 ).LE.TOL2*Z( I4-3 ) .OR. &
                         Z( I4-1 ).LE.TOL2*SIGMA ) THEN
                        Z( I4-1 ) = -SIGMA
                        SPLT = I4 / 4
                        QMAX = ZERO
                        EMIN = Z( I4+3 )
                        OLDEMN = Z( I4+4 )
                     ELSE
                        QMAX = MAX( QMAX, Z( I4+1 ) )
                        EMIN = MIN( EMIN, Z( I4-1 ) )
                        OLDEMN = MIN( OLDEMN, Z( I4 ) )
                     ENDIF
  110             CONTINUE
                  Z( 4*N0-1 ) = EMIN
                  Z( 4*N0 ) = OLDEMN
                  I0 = SPLT + 1
               ENDIF
            ENDIF
!
  120    CONTINUE
!
         INFO = 2
         RETURN
!
!        end IWHILB
!
  130    CONTINUE
!
  140 CONTINUE
!
      INFO = 3
      RETURN
!
!     end IWHILA   
!
  150 CONTINUE
!      
!     Move q's to the front.
!      
      DO 160 K = 2, N
         Z( K ) = Z( 4*K-3 )
  160 CONTINUE
!      
!     Sort and compute sum of eigenvalues.
!
      CALL DLASRT( 'D', N, Z, IINFO )
!
      E = ZERO
      DO 170 K = N, 1, -1
         E = E + Z( K )
  170 CONTINUE
!
!     Store trace, sum(eigenvalues) and information on performance.
!
      Z( 2*N+1 ) = TRACE 
      Z( 2*N+2 ) = E
      Z( 2*N+3 ) = DBLE( ITER )
      Z( 2*N+4 ) = DBLE( NDIV ) / DBLE( N**2 )
      Z( 2*N+5 ) = HUNDRD*NFAIL / DBLE( ITER )
      RETURN
!
!     End of DLASQ2
!
      END SUBROUTINE DLASQ2
      
      
      
      
      
      SUBROUTINE DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN, &
                         DNM1, DNM2, IEEE )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2, TAU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLASQ5 computes one dqds transform in ping-pong form, one
!  version for IEEE machines another for non IEEE machines.
!
!  Arguments
!  =========
!
!  I0    (input) INTEGER
!        First index.
!
!  N0    (input) INTEGER
!        Last index.
!
!  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
!        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
!        an extra argument.
!
!  PP    (input) INTEGER
!        PP=0 for ping, PP=1 for pong.
!
!  TAU   (input) DOUBLE PRECISION
!        This is the shift.
!
!  DMIN  (output) DOUBLE PRECISION
!        Minimum value of d.
!
!  DMIN1 (output) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ).
!
!  DMIN2 (output) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
!
!  DN    (output) DOUBLE PRECISION
!        d(N0), the last value of d.
!
!  DNM1  (output) DOUBLE PRECISION
!        d(N0-1).
!
!  DNM2  (output) DOUBLE PRECISION
!        d(N0-2).
!
!  IEEE  (input) LOGICAL
!        Flag for IEEE or non IEEE arithmetic.
!
!  =====================================================================
!
!     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, TEMP
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MIN
!     ..
!     .. Executable Statements ..
!
      IF( ( N0-I0-1 ).LE.0 ) &
         RETURN
!
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 )
      D = Z( J4 ) - TAU
      DMIN = D
      DMIN1 = -Z( J4 )
!
      IF( IEEE ) THEN
!
!        Code for IEEE arithmetic.
!
         IF( PP.EQ.0 ) THEN
            DO 10 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-2 ) = D + Z( J4-1 )
               TEMP = Z( J4+1 ) / Z( J4-2 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4 ) = Z( J4-1 )*TEMP
               EMIN = MIN( Z( J4 ), EMIN )
   10       CONTINUE
         ELSE
            DO 20 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-3 ) = D + Z( J4 )
               TEMP = Z( J4+2 ) / Z( J4-3 )
               D = D*TEMP - TAU
               DMIN = MIN( DMIN, D )
               Z( J4-1 ) = Z( J4 )*TEMP
               EMIN = MIN( Z( J4-1 ), EMIN )
   20       CONTINUE
         ENDIF
!
!        Unroll last two steps.
!
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DNM1 )
!
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         DMIN = MIN( DMIN, DN )
!
      ELSE
!
!        Code for non IEEE arithmetic.
!
         IF( PP.EQ.0 ) THEN
            DO 30 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-2 ) = D + Z( J4-1 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
                  D = Z( J4+1 )*( D / Z( J4-2 ) ) - TAU
               ENDIF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4 ) )
   30       CONTINUE
         ELSE
            DO 40 J4 = 4*I0, 4*( N0-3 ), 4
               Z( J4-3 ) = D + Z( J4 )
               IF( D.LT.ZERO ) THEN
                  RETURN
               ELSE
                  Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
                  D = Z( J4+2 )*( D / Z( J4-3 ) ) - TAU
               ENDIF
               DMIN = MIN( DMIN, D )
               EMIN = MIN( EMIN, Z( J4-1 ) )
   40       CONTINUE
         ENDIF
!
!        Unroll last two steps.
!
         DNM2 = D
         DMIN2 = DMIN
         J4 = 4*( N0-2 ) - PP
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM2 + Z( J4P2 )
         IF( DNM2.LT.ZERO ) THEN
            RETURN
         ELSE
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) ) - TAU
         ENDIF
         DMIN = MIN( DMIN, DNM1 )
!
         DMIN1 = DMIN
         J4 = J4 + 4
         J4P2 = J4 + 2*PP - 1
         Z( J4-2 ) = DNM1 + Z( J4P2 )
         IF( DNM1.LT.ZERO ) THEN
            RETURN
         ELSE
            Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
            DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) ) - TAU
         ENDIF
         DMIN = MIN( DMIN, DN )
!
      ENDIF
!
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
!
!     End of DLASQ5
!
      END SUBROUTINE DLASQ5
      
      
      
      
      
      SUBROUTINE DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN, &
                         DNM1, DNM2 )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            I0, N0, PP
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DNM1, DNM2
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLASQ6 computes one dqd (shift equal to zero) transform in
!  ping-pong form, with protection against underflow and overflow.
!
!  Arguments
!  =========
!
!  I0    (input) INTEGER
!        First index.
!
!  N0    (input) INTEGER
!        Last index.
!
!  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
!        Z holds the qd array. EMIN is stored in Z(4*N0) to avoid
!        an extra argument.
!
!  PP    (input) INTEGER
!        PP=0 for ping, PP=1 for pong.
!
!  DMIN  (output) DOUBLE PRECISION
!        Minimum value of d.
!
!  DMIN1 (output) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ).
!
!  DMIN2 (output) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
!
!  DN    (output) DOUBLE PRECISION
!        d(N0), the last value of d.
!
!  DNM1  (output) DOUBLE PRECISION
!        d(N0-1).
!
!  DNM2  (output) DOUBLE PRECISION
!        d(N0-2).
!
!  =====================================================================
!
!     .. Parameter ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            J4, J4P2
      DOUBLE PRECISION   D, EMIN, SAFMIN, TEMP
!     ..
!     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MIN
!     ..
!     .. Executable Statements ..
!
      IF( ( N0-I0-1 ).LE.0 ) &
         RETURN
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      J4 = 4*I0 + PP - 3
      EMIN = Z( J4+4 ) 
      D = Z( J4 )
      DMIN = D
!
      IF( PP.EQ.0 ) THEN
         DO 10 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-2 ) = D + Z( J4-1 ) 
            IF( Z( J4-2 ).EQ.ZERO ) THEN
               Z( J4 ) = ZERO
               D = Z( J4+1 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+1 ).LT.Z( J4-2 ) .AND. &
                     SAFMIN*Z( J4-2 ).LT.Z( J4+1 ) ) THEN
               TEMP = Z( J4+1 ) / Z( J4-2 )
               Z( J4 ) = Z( J4-1 )*TEMP
               D = D*TEMP
            ELSE 
               Z( J4 ) = Z( J4+1 )*( Z( J4-1 ) / Z( J4-2 ) )
               D = Z( J4+1 )*( D / Z( J4-2 ) )
            ENDIF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4 ) )
   10    CONTINUE
      ELSE
         DO 20 J4 = 4*I0, 4*( N0-3 ), 4
            Z( J4-3 ) = D + Z( J4 ) 
            IF( Z( J4-3 ).EQ.ZERO ) THEN
               Z( J4-1 ) = ZERO
               D = Z( J4+2 )
               DMIN = D
               EMIN = ZERO
            ELSE IF( SAFMIN*Z( J4+2 ).LT.Z( J4-3 ) .AND. &
                     SAFMIN*Z( J4-3 ).LT.Z( J4+2 ) ) THEN
               TEMP = Z( J4+2 ) / Z( J4-3 )
               Z( J4-1 ) = Z( J4 )*TEMP
               D = D*TEMP
            ELSE 
               Z( J4-1 ) = Z( J4+2 )*( Z( J4 ) / Z( J4-3 ) )
               D = Z( J4+2 )*( D / Z( J4-3 ) )
            ENDIF
            DMIN = MIN( DMIN, D )
            EMIN = MIN( EMIN, Z( J4-1 ) )
   20    CONTINUE
      ENDIF
!
!     Unroll last two steps. 
!
      DNM2 = D
      DMIN2 = DMIN
      J4 = 4*( N0-2 ) - PP
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM2 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DNM1 = Z( J4P2+2 )
         DMIN = DNM1
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND. &
               SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DNM1 = DNM2*TEMP
      ELSE
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DNM1 = Z( J4P2+2 )*( DNM2 / Z( J4-2 ) )
      ENDIF
      DMIN = MIN( DMIN, DNM1 )
!
      DMIN1 = DMIN
      J4 = J4 + 4
      J4P2 = J4 + 2*PP - 1
      Z( J4-2 ) = DNM1 + Z( J4P2 )
      IF( Z( J4-2 ).EQ.ZERO ) THEN
         Z( J4 ) = ZERO
         DN = Z( J4P2+2 )
         DMIN = DN
         EMIN = ZERO
      ELSE IF( SAFMIN*Z( J4P2+2 ).LT.Z( J4-2 ) .AND. &
               SAFMIN*Z( J4-2 ).LT.Z( J4P2+2 ) ) THEN
         TEMP = Z( J4P2+2 ) / Z( J4-2 )
         Z( J4 ) = Z( J4P2 )*TEMP
         DN = DNM1*TEMP
      ELSE
         Z( J4 ) = Z( J4P2+2 )*( Z( J4P2 ) / Z( J4-2 ) )
         DN = Z( J4P2+2 )*( DNM1 / Z( J4-2 ) )
      ENDIF
      DMIN = MIN( DMIN, DN )
!
      Z( J4+2 ) = DN
      Z( 4*N0-PP ) = EMIN
      RETURN
!
!     End of DLASQ6
!
      END SUBROUTINE DLASQ6
      
      
      
      
      
      SUBROUTINE DLASRT( ID, N, D, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          ID
      INTEGER            INFO, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * )
!     ..
!
!  Purpose
!  =======
!
!  Sort the numbers in D in increasing order (if ID = 'I') or
!  in decreasing order (if ID = 'D' ).
!
!  Use Quick Sort, reverting to Insertion sort on arrays of
!  size <= 20. Dimension of STACK limits N to about 2**32.
!
!  Arguments
!  =========
!
!  ID      (input) CHARACTER*1
!          = 'I': sort D in increasing order;
!          = 'D': sort D in decreasing order.
!
!  N       (input) INTEGER
!          The length of the array D.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the array to be sorted.
!          On exit, D has been sorted into increasing order
!          (D(1) <= ... <= D(N) ) or into decreasing order
!          (D(1) >= ... >= D(N) ), depending on ID.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            SELECT
      PARAMETER          ( SELECT = 20 )
!     ..
!     .. Local Scalars ..
      INTEGER            DIR, ENDD, I, J, START, STKPNT
      DOUBLE PRECISION   D1, D2, D3, DMNMX, TMP
!     ..
!     .. Local Arrays ..
      INTEGER            STACK( 2, 32 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           XERBLA
!     ..
!     .. Executable Statements ..
!
!     Test the input paramters.
!
      INFO = 0
      DIR = -1
      IF( LSAME( ID, 'D' ) ) THEN
         DIR = 0
      ELSE IF( LSAME( ID, 'I' ) ) THEN
         DIR = 1
      ENDIF
      IF( DIR.EQ.-1 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ENDIF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DLASRT', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( N.LE.1 ) &
         RETURN
!
      STKPNT = 1
      STACK( 1, 1 ) = 1
      STACK( 2, 1 ) = N
   10 CONTINUE
      START = STACK( 1, STKPNT )
      ENDD = STACK( 2, STKPNT )
      STKPNT = STKPNT - 1
      IF( ENDD-START.LE.SELECT .AND. ENDD-START.GT.0 ) THEN
!
!        Do Insertion sort on D( START:ENDD )
!
         IF( DIR.EQ.0 ) THEN
!
!           Sort into decreasing order
!
            DO 30 I = START + 1, ENDD
               DO 20 J = I, START + 1, -1
                  IF( D( J ).GT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 30
                  ENDIF
   20          CONTINUE
   30       CONTINUE
!
         ELSE
!
!           Sort into increasing order
!
            DO 50 I = START + 1, ENDD
               DO 40 J = I, START + 1, -1
                  IF( D( J ).LT.D( J-1 ) ) THEN
                     DMNMX = D( J )
                     D( J ) = D( J-1 )
                     D( J-1 ) = DMNMX
                  ELSE
                     GO TO 50
                  ENDIF
   40          CONTINUE
   50       CONTINUE
!
         ENDIF
!
      ELSE IF( ENDD-START.GT.SELECT ) THEN
!
!        Partition D( START:ENDD ) and stack parts, largest one first
!
!        Choose partition entry as median of 3
!
         D1 = D( START )
         D2 = D( ENDD )
         I = ( START+ENDD ) / 2
         D3 = D( I )
         IF( D1.LT.D2 ) THEN
            IF( D3.LT.D1 ) THEN
               DMNMX = D1
            ELSE IF( D3.LT.D2 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D2
            ENDIF
         ELSE
            IF( D3.LT.D2 ) THEN
               DMNMX = D2
            ELSE IF( D3.LT.D1 ) THEN
               DMNMX = D3
            ELSE
               DMNMX = D1
            ENDIF
         ENDIF
!
         IF( DIR.EQ.0 ) THEN
!
!           Sort into decreasing order
!
            I = START - 1
            J = ENDD + 1
   60       CONTINUE
   70       CONTINUE
            J = J - 1
            IF( D( J ).LT.DMNMX ) &
               GO TO 70
   80       CONTINUE
            I = I + 1
            IF( D( I ).GT.DMNMX ) &
               GO TO 80
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 60
            ENDIF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            ENDIF
         ELSE
!
!           Sort into increasing order
!
            I = START - 1
            J = ENDD + 1
   90       CONTINUE
  100       CONTINUE
            J = J - 1
            IF( D( J ).GT.DMNMX ) &
               GO TO 100
  110       CONTINUE
            I = I + 1
            IF( D( I ).LT.DMNMX ) &
               GO TO 110
            IF( I.LT.J ) THEN
               TMP = D( I )
               D( I ) = D( J )
               D( J ) = TMP
               GO TO 90
            ENDIF
            IF( J-START.GT.ENDD-J-1 ) THEN
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
            ELSE
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = J + 1
               STACK( 2, STKPNT ) = ENDD
               STKPNT = STKPNT + 1
               STACK( 1, STKPNT ) = START
               STACK( 2, STKPNT ) = J
            ENDIF
         ENDIF
      ENDIF
      IF( STKPNT.GT.0 ) &
         GO TO 10
      RETURN
!
!     End of DLASRT
!
      END SUBROUTINE DLASRT
      
      
      
      
      
      SUBROUTINE DLASSQ( N, X, INCX, SCALE, SUMSQ )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INCX, N
      DOUBLE PRECISION   SCALE, SUMSQ
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   X( * )
!     ..
!
!  Purpose
!  =======
!
!  DLASSQ  returns the values  scl  and  smsq  such that
!
!     ( scl**2 )*smsq = x( 1 )**2 +...+ x( n )**2 + ( scale**2 )*sumsq,
!
!  where  x( i ) = X( 1 + ( i - 1 )*INCX ). The value of  sumsq  is
!  assumed to be non-negative and  scl  returns the value
!
!     scl = max( scale, abs( x( i ) ) ).
!
!  scale and sumsq must be supplied in SCALE and SUMSQ and
!  scl and smsq are overwritten on SCALE and SUMSQ respectively.
!
!  The routine makes only one pass through the vector x.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The number of elements to be used from the vector X.
!
!  X       (input) DOUBLE PRECISION array, dimension (N)
!          The vector for which a scaled sum of squares is computed.
!             x( i )  = X( 1 + ( i - 1 )*INCX ), 1 <= i <= n.
!
!  INCX    (input) INTEGER
!          The increment between successive values of the vector X.
!          INCX > 0.
!
!  SCALE   (input/output) DOUBLE PRECISION
!          On entry, the value  scale  in the equation above.
!          On exit, SCALE is overwritten with  scl , the scaling factor
!          for the sum of squares.
!
!  SUMSQ   (input/output) DOUBLE PRECISION
!          On entry, the value  sumsq  in the equation above.
!          On exit, SUMSQ is overwritten with  smsq , the basic sum of
!          squares from which  scl  has been factored out.
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            IX
      DOUBLE PRECISION   ABSXI
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS
!     ..
!     .. Executable Statements ..
!
      IF( N.GT.0 ) THEN
         DO 10 IX = 1, 1 + ( N-1 )*INCX, INCX
            IF( X( IX ).NE.ZERO ) THEN
               ABSXI = ABS( X( IX ) )
               IF( SCALE.LT.ABSXI ) THEN
                  SUMSQ = 1 + SUMSQ*( SCALE / ABSXI )**2
                  SCALE = ABSXI
               ELSE
                  SUMSQ = SUMSQ + ( ABSXI / SCALE )**2
               ENDIF
            ENDIF
   10    CONTINUE
      ENDIF
      RETURN
!
!     End of DLASSQ
!
      END SUBROUTINE DLASSQ
      
      
      
      
      
      SUBROUTINE DLATRD( UPLO, N, NB, A, LDA, E, TAU, W, LDW )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDW, N, NB
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), E( * ), TAU( * ), W( LDW, * )
!     ..
!
!  Purpose
!  =======
!
!  DLATRD reduces NB rows and columns of a real symmetric matrix A to
!  symmetric tridiagonal form by an orthogonal similarity
!  transformation Q' * A * Q, and returns the matrices V and W which are
!  needed to apply the transformation to the unreduced part of A.
!
!  If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
!  matrix, of which the upper triangle is supplied;
!  if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
!  matrix, of which the lower triangle is supplied.
!
!  This is an auxiliary routine called by DSYTRD.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U': Upper triangular
!          = 'L': Lower triangular
!
!  N       (input) INTEGER
!          The order of the matrix A.
!
!  NB      (input) INTEGER
!          The number of rows and columns to be reduced.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          n-by-n upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n-by-n lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit:
!          if UPLO = 'U', the last NB columns have been reduced to
!            tridiagonal form, with the diagonal elements overwriting
!            the diagonal elements of A; the elements above the diagonal
!            with the array TAU, represent the orthogonal matrix Q as a
!            product of elementary reflectors;
!          if UPLO = 'L', the first NB columns have been reduced to
!            tridiagonal form, with the diagonal elements overwriting
!            the diagonal elements of A; the elements below the diagonal
!            with the array TAU, represent the  orthogonal matrix Q as a
!            product of elementary reflectors.
!          See Further Details.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= (1,N).
!
!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
!          elements of the last NB columns of the reduced matrix;
!          if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
!          the first NB columns of the reduced matrix.
!
!  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
!          The scalar factors of the elementary reflectors, stored in
!          TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
!          See Further Details.
!
!  W       (output) DOUBLE PRECISION array, dimension (LDW,NB)
!          The n-by-nb matrix W required to update the unreduced part
!          of A.
!
!  LDW     (input) INTEGER
!          The leading dimension of the array W. LDW >= max(1,N).
!
!  Further Details
!  ===============
!
!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(n) H(n-1) . . . H(n-nb+1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(i:n) = 0 and v(i-1) = 1; v(1:i-1) is stored on exit in A(1:i-1,i),
!  and tau in TAU(i-1).
!
!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(1) H(2) . . . H(nb).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
!  and tau in TAU(i).
!
!  The elements of the vectors v together form the n-by-nb matrix V
!  which is needed, with W, to apply the transformation to the unreduced
!  part of the matrix, using a symmetric rank-2k update of the form:
!  A := A - V*W' - W*V'.
!
!  The contents of A on exit are illustrated by the following examples
!  with n = 5 and nb = 2:
!
!  if UPLO = 'U':                       if UPLO = 'L':
!
!    (  a   a   a   v4  v5 )              (  d                  )
!    (      a   a   v4  v5 )              (  1   d              )
!    (          a   1   v5 )              (  v1  1   a          )
!    (              d   1  )              (  v1  v2  a   a      )
!    (                  d  )              (  v1  v2  a   a   a  )
!
!  where d denotes a diagonal element of the reduced matrix, a denotes
!  an element of the original matrix that is unchanged, and vi denotes
!  an element of the vector defining H(i).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, HALF
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, HALF = 0.5D+0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, IW
      DOUBLE PRECISION   ALPHA
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DGEMV, DLARFG, DSCAL, DSYMV
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MIN
!     ..
!     .. Executable Statements ..
!
!     Quick return if possible
!
      IF( N.LE.0 ) &
         RETURN
!
      IF( LSAME( UPLO, 'U' ) ) THEN
!
!        Reduce last NB columns of upper triangle
!
         DO 10 I = N, N - NB + 1, -1
            IW = I - N + NB
            IF( I.LT.N ) THEN
!
!              Update A(1:i,i)
!
               CALL DGEMV( 'No transpose', I, N-I, -ONE, A( 1, I+1 ), &
                           LDA, W( I, IW+1 ), LDW, ONE, A( 1, I ), 1 )
               CALL DGEMV( 'No transpose', I, N-I, -ONE, W( 1, IW+1 ), &
                           LDW, A( I, I+1 ), LDA, ONE, A( 1, I ), 1 )
            ENDIF
            IF( I.GT.1 ) THEN
!
!              Generate elementary reflector H(i) to annihilate
!              A(1:i-2,i)
!
               CALL DLARFG( I-1, A( I-1, I ), A( 1, I ), 1, TAU( I-1 ) )
               E( I-1 ) = A( I-1, I )
               A( I-1, I ) = ONE
!
!              Compute W(1:i-1,i)
!
               CALL DSYMV( 'Upper', I-1, ONE, A, LDA, A( 1, I ), 1, &
                           ZERO, W( 1, IW ), 1 )
               IF( I.LT.N ) THEN
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, W( 1, IW+1 ), &
                              LDW, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE, &
                              A( 1, I+1 ), LDA, W( I+1, IW ), 1, ONE, &
                              W( 1, IW ), 1 )
                  CALL DGEMV( 'Transpose', I-1, N-I, ONE, A( 1, I+1 ), &
                              LDA, A( 1, I ), 1, ZERO, W( I+1, IW ), 1 )
                  CALL DGEMV( 'No transpose', I-1, N-I, -ONE, &
                              W( 1, IW+1 ), LDW, W( I+1, IW ), 1, ONE, &
                              W( 1, IW ), 1 )
               ENDIF
               CALL DSCAL( I-1, TAU( I-1 ), W( 1, IW ), 1 )
               ALPHA = -HALF*TAU( I-1 )*DDOT( I-1, W( 1, IW ), 1, &
                       A( 1, I ), 1 )
               CALL DAXPY( I-1, ALPHA, A( 1, I ), 1, W( 1, IW ), 1 )
            ENDIF
!
   10    CONTINUE
      ELSE
!
!        Reduce first NB columns of lower triangle
!
         DO 20 I = 1, NB
!
!           Update A(i:n,i)
!
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, A( I, 1 ), &
                        LDA, W( I, 1 ), LDW, ONE, A( I, I ), 1 )
            CALL DGEMV( 'No transpose', N-I+1, I-1, -ONE, W( I, 1 ), &
                        LDW, A( I, 1 ), LDA, ONE, A( I, I ), 1 )
            IF( I.LT.N ) THEN
!
!              Generate elementary reflector H(i) to annihilate
!              A(i+2:n,i)
!
               CALL DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1, &
                            TAU( I ) )
               E( I ) = A( I+1, I )
               A( I+1, I ) = ONE
!
!              Compute W(i+1:n,i)
!
               CALL DSYMV( 'Lower', N-I, ONE, A( I+1, I+1 ), LDA, &
                           A( I+1, I ), 1, ZERO, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, W( I+1, 1 ), LDW, &
                           A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, A( I+1, 1 ), &
                           LDA, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DGEMV( 'Transpose', N-I, I-1, ONE, A( I+1, 1 ), LDA, &
                           A( I+1, I ), 1, ZERO, W( 1, I ), 1 )
               CALL DGEMV( 'No transpose', N-I, I-1, -ONE, W( I+1, 1 ), &
                           LDW, W( 1, I ), 1, ONE, W( I+1, I ), 1 )
               CALL DSCAL( N-I, TAU( I ), W( I+1, I ), 1 )
               ALPHA = -HALF*TAU( I )*DDOT( N-I, W( I+1, I ), 1, &
                       A( I+1, I ), 1 )
               CALL DAXPY( N-I, ALPHA, A( I+1, I ), 1, W( I+1, I ), 1 )
            ENDIF
!
   20    CONTINUE
      ENDIF
!
      RETURN
!
!     End of DLATRD
!
      END SUBROUTINE DLATRD
      
      
      
      
      
      SUBROUTINE DLAZQ3( I0, N0, Z, PP, DMIN, SIGMA, DESIG, QMAX, NFAIL, &
                         ITER, NDIV, IEEE, TTYPE, DMIN1, DMIN2, DN, DN1, &
                         DN2, TAU )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      LOGICAL            IEEE
      INTEGER            I0, ITER, N0, NDIV, NFAIL, PP, TTYPE
      DOUBLE PRECISION   DESIG, DMIN, DMIN1, DMIN2, DN, DN1, DN2, QMAX, &
                         SIGMA, TAU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAZQ3 checks for deflation, computes a shift (TAU) and calls dqds.
!  In case of failure it changes shifts, and tries again until output
!  is positive.
!
!  Arguments
!  =========
!
!  I0     (input) INTEGER
!         First index.
!
!  N0     (input) INTEGER
!         Last index.
!
!  Z      (input) DOUBLE PRECISION array, dimension ( 4*N )
!         Z holds the qd array.
!
!  PP     (input) INTEGER
!         PP=0 for ping, PP=1 for pong.
!
!  DMIN   (output) DOUBLE PRECISION
!         Minimum value of d.
!
!  SIGMA  (output) DOUBLE PRECISION
!         Sum of shifts used in current segment.
!
!  DESIG  (input/output) DOUBLE PRECISION
!         Lower order part of SIGMA
!
!  QMAX   (input) DOUBLE PRECISION
!         Maximum value of q.
!
!  NFAIL  (output) INTEGER
!         Number of times shift was too big.
!
!  ITER   (output) INTEGER
!         Number of iterations.
!
!  NDIV   (output) INTEGER
!         Number of divisions.
!
!  IEEE   (input) LOGICAL
!         Flag for IEEE or non IEEE arithmetic (passed to DLASQ5).
!
!  TTYPE  (input/output) INTEGER
!         Shift type.  TTYPE is passed as an argument in order to save
!         its value between calls to DLAZQ3
!
!  DMIN1  (input/output) REAL
!  DMIN2  (input/output) REAL
!  DN     (input/output) REAL
!  DN1    (input/output) REAL
!  DN2    (input/output) REAL
!  TAU    (input/output) REAL
!         These are passed as arguments in order to save their values
!         between calls to DLAZQ3
!
!  This is a thread safe version of DLASQ3, which passes TTYPE, DMIN1,
!  DMIN2, DN, DN1. DN2 and TAU through the argument list in place of
!  declaring them in a SAVE statment.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   CBIAS
      PARAMETER          ( CBIAS = 1.50D0 )
      DOUBLE PRECISION   ZERO, QURTR, HALF, ONE, TWO, HUNDRD
      PARAMETER          ( ZERO = 0.0D0, QURTR = 0.250D0, HALF = 0.5D0, &
                           ONE = 1.0D0, TWO = 2.0D0, HUNDRD = 100.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            IPN4, J4, N0IN, NN
      DOUBLE PRECISION   EPS, G, S, SAFMIN, T, TEMP, TOL, TOL2
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLASQ5, DLASQ6, DLAZQ4
!     ..
!     .. External Function ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      N0IN   = N0
      EPS    = DLAMCH( 'Precision' )
      SAFMIN = DLAMCH( 'Safe minimum' )
      TOL    = EPS*HUNDRD
      TOL2   = TOL**2
      G      = ZERO
!
!     Check for deflation.
!
   10 CONTINUE
!
      IF( N0.LT.I0 ) &
         RETURN
      IF( N0.EQ.I0 ) &
         GO TO 20
      NN = 4*N0 + PP
      IF( N0.EQ.( I0+1 ) ) &
         GO TO 40
!
!     Check whether E(N0-1) is negligible, 1 eigenvalue.
!
      IF( Z( NN-5 ).GT.TOL2*( SIGMA+Z( NN-3 ) ) .AND. &
          Z( NN-2*PP-4 ).GT.TOL2*Z( NN-7 ) ) &
         GO TO 30
!
   20 CONTINUE
!
      Z( 4*N0-3 ) = Z( 4*N0+PP-3 ) + SIGMA
      N0 = N0 - 1
      GO TO 10
!
!     Check  whether E(N0-2) is negligible, 2 eigenvalues.
!
   30 CONTINUE
!
      IF( Z( NN-9 ).GT.TOL2*SIGMA .AND. &
          Z( NN-2*PP-8 ).GT.TOL2*Z( NN-11 ) ) &
         GO TO 50
!
   40 CONTINUE
!
      IF( Z( NN-3 ).GT.Z( NN-7 ) ) THEN
         S = Z( NN-3 )
         Z( NN-3 ) = Z( NN-7 )
         Z( NN-7 ) = S
      ENDIF
      IF( Z( NN-5 ).GT.Z( NN-3 )*TOL2 ) THEN
         T = HALF*( ( Z( NN-7 )-Z( NN-3 ) )+Z( NN-5 ) )
         S = Z( NN-3 )*( Z( NN-5 ) / T )
         IF( S.LE.T ) THEN
            S = Z( NN-3 )*( Z( NN-5 ) / &
                ( T*( ONE+SQRT( ONE+S / T ) ) ) )
         ELSE
            S = Z( NN-3 )*( Z( NN-5 ) / ( T+SQRT( T )*SQRT( T+S ) ) )
         ENDIF
         T = Z( NN-7 ) + ( S+Z( NN-5 ) )
         Z( NN-3 ) = Z( NN-3 )*( Z( NN-7 ) / T )
         Z( NN-7 ) = T
      ENDIF
      Z( 4*N0-7 ) = Z( NN-7 ) + SIGMA
      Z( 4*N0-3 ) = Z( NN-3 ) + SIGMA
      N0 = N0 - 2
      GO TO 10
!
   50 CONTINUE
!
!     Reverse the qd-array, if warranted.
!
      IF( DMIN.LE.ZERO .OR. N0.LT.N0IN ) THEN
         IF( CBIAS*Z( 4*I0+PP-3 ).LT.Z( 4*N0+PP-3 ) ) THEN
            IPN4 = 4*( I0+N0 )
            DO 60 J4 = 4*I0, 2*( I0+N0-1 ), 4
               TEMP = Z( J4-3 )
               Z( J4-3 ) = Z( IPN4-J4-3 )
               Z( IPN4-J4-3 ) = TEMP
               TEMP = Z( J4-2 )
               Z( J4-2 ) = Z( IPN4-J4-2 )
               Z( IPN4-J4-2 ) = TEMP
               TEMP = Z( J4-1 )
               Z( J4-1 ) = Z( IPN4-J4-5 )
               Z( IPN4-J4-5 ) = TEMP
               TEMP = Z( J4 )
               Z( J4 ) = Z( IPN4-J4-4 )
               Z( IPN4-J4-4 ) = TEMP
   60       CONTINUE
            IF( N0-I0.LE.4 ) THEN
               Z( 4*N0+PP-1 ) = Z( 4*I0+PP-1 )
               Z( 4*N0-PP ) = Z( 4*I0-PP )
            ENDIF
            DMIN2 = MIN( DMIN2, Z( 4*N0+PP-1 ) )
            Z( 4*N0+PP-1 ) = MIN( Z( 4*N0+PP-1 ), Z( 4*I0+PP-1 ), &
                                  Z( 4*I0+PP+3 ) )
            Z( 4*N0-PP ) = MIN( Z( 4*N0-PP ), Z( 4*I0-PP ), &
                                Z( 4*I0-PP+4 ) )
            QMAX = MAX( QMAX, Z( 4*I0+PP-3 ), Z( 4*I0+PP+1 ) )
            DMIN = -ZERO
         ENDIF
      ENDIF
!
      IF( DMIN.LT.ZERO .OR. SAFMIN*QMAX.LT.MIN( Z( 4*N0+PP-1 ), &
          Z( 4*N0+PP-9 ), DMIN2+Z( 4*N0-PP ) ) ) THEN
!
!        Choose a shift.
!
         CALL DLAZQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN, DN1, &
                      DN2, TAU, TTYPE, G )
!
!        Call dqds until DMIN > 0.
!
   80    CONTINUE
!
         CALL DLASQ5( I0, N0, Z, PP, TAU, DMIN, DMIN1, DMIN2, DN, &
                      DN1, DN2, IEEE )
!
         NDIV = NDIV + ( N0-I0+2 )
         ITER = ITER + 1
!
!        Check status.
!
         IF( DMIN.GE.ZERO .AND. DMIN1.GT.ZERO ) THEN
!
!           Success.
!
            GO TO 100
!
         ELSE IF( DMIN.LT.ZERO .AND. DMIN1.GT.ZERO .AND. &
                  Z( 4*( N0-1 )-PP ).LT.TOL*( SIGMA+DN1 ) .AND. &
                  ABS( DN ).LT.TOL*SIGMA ) THEN
!
!           Convergence hidden by negative DN.
!
            Z( 4*( N0-1 )-PP+2 ) = ZERO
            DMIN = ZERO
            GO TO 100
         ELSE IF( DMIN.LT.ZERO ) THEN
!
!           TAU too big. Select new TAU and try again.
!
            NFAIL = NFAIL + 1
            IF( TTYPE.LT.-22 ) THEN
!
!              Failed twice. Play it safe.
!
               TAU = ZERO
            ELSE IF( DMIN1.GT.ZERO ) THEN
!
!              Late failure. Gives excellent shift.
!
               TAU = ( TAU+DMIN )*( ONE-TWO*EPS )
               TTYPE = TTYPE - 11
            ELSE
!
!              Early failure. Divide by 4.
!
               TAU = QURTR*TAU
               TTYPE = TTYPE - 12
            ENDIF
            GO TO 80
         ELSE IF( DMIN.NE.DMIN ) THEN
!
!           NaN.
!
            TAU = ZERO
            GO TO 80
         ELSE
!
!           Possible underflow. Play it safe.
!
            GO TO 90
         ENDIF
      ENDIF
!
!     Risk of underflow.
!
   90 CONTINUE
      CALL DLASQ6( I0, N0, Z, PP, DMIN, DMIN1, DMIN2, DN, DN1, DN2 )
      NDIV = NDIV + ( N0-I0+2 )
      ITER = ITER + 1
      TAU = ZERO
!
  100 CONTINUE
      IF( TAU.LT.SIGMA ) THEN
         DESIG = DESIG + TAU
         T = SIGMA + DESIG
         DESIG = DESIG - ( T-SIGMA )
      ELSE
         T = SIGMA + TAU
         DESIG = SIGMA - ( T-TAU ) + DESIG
      ENDIF
      SIGMA = T
!
      RETURN
!
!     End of DLAZQ3
!
      END SUBROUTINE DLAZQ3
      
      
      
      
      
      SUBROUTINE DLAZQ4( I0, N0, Z, PP, N0IN, DMIN, DMIN1, DMIN2, DN, &
                         DN1, DN2, TAU, TTYPE, G )
!
!  -- LAPACK auxiliary routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            I0, N0, N0IN, PP, TTYPE
      DOUBLE PRECISION   DMIN, DMIN1, DMIN2, DN, DN1, DN2, G, TAU
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   Z( * )
!     ..
!
!  Purpose
!  =======
!
!  DLAZQ4 computes an approximation TAU to the smallest eigenvalue 
!  using values of d from the previous transform.
!
!  I0    (input) INTEGER
!        First index.
!
!  N0    (input) INTEGER
!        Last index.
!
!  Z     (input) DOUBLE PRECISION array, dimension ( 4*N )
!        Z holds the qd array.
!
!  PP    (input) INTEGER
!        PP=0 for ping, PP=1 for pong.
!
!  N0IN  (input) INTEGER
!        The value of N0 at start of EIGTEST.
!
!  DMIN  (input) DOUBLE PRECISION
!        Minimum value of d.
!
!  DMIN1 (input) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ).
!
!  DMIN2 (input) DOUBLE PRECISION
!        Minimum value of d, excluding D( N0 ) and D( N0-1 ).
!
!  DN    (input) DOUBLE PRECISION
!        d(N)
!
!  DN1   (input) DOUBLE PRECISION
!        d(N-1)
!
!  DN2   (input) DOUBLE PRECISION
!        d(N-2)
!
!  TAU   (output) DOUBLE PRECISION
!        This is the shift.
!
!  TTYPE (output) INTEGER
!        Shift type.
!
!  G     (input/output) DOUBLE PRECISION
!        G is passed as an argument in order to save its value between
!        calls to DLAZQ4
!
!  Further Details
!  ===============
!  CNST1 = 9/16
!
!  This is a thread safe version of DLASQ4, which passes G through the
!  argument list in place of declaring G in a SAVE statment.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   CNST1, CNST2, CNST3
      PARAMETER          ( CNST1 = 0.5630D0, CNST2 = 1.010D0, &
                         CNST3 = 1.050D0 )
      DOUBLE PRECISION   QURTR, THIRD, HALF, ZERO, ONE, TWO, HUNDRD
      PARAMETER          ( QURTR = 0.250D0, THIRD = 0.3330D0, &
                         HALF = 0.50D0, ZERO = 0.0D0, ONE = 1.0D0, &
                         TWO = 2.0D0, HUNDRD = 100.0D0 )
!     ..
!     .. Local Scalars ..
      INTEGER            I4, NN, NP
      DOUBLE PRECISION   A2, B1, B2, GAM, GAP1, GAP2, S
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
!     A negative DMIN forces the shift to take that absolute value
!     TTYPE records the type of shift.
!
      IF( DMIN.LE.ZERO ) THEN
         TAU = -DMIN
         TTYPE = -1
         RETURN
      ENDIF
!       
      NN = 4*N0 + PP
      IF( N0IN.EQ.N0 ) THEN
!
!        No eigenvalues deflated.
!
         IF( DMIN.EQ.DN .OR. DMIN.EQ.DN1 ) THEN
!
            B1 = SQRT( Z( NN-3 ) )*SQRT( Z( NN-5 ) )
            B2 = SQRT( Z( NN-7 ) )*SQRT( Z( NN-9 ) )
            A2 = Z( NN-7 ) + Z( NN-5 )
!
!           Cases 2 and 3.
!
            IF( DMIN.EQ.DN .AND. DMIN1.EQ.DN1 ) THEN
               GAP2 = DMIN2 - A2 - DMIN2*QURTR
               IF( GAP2.GT.ZERO .AND. GAP2.GT.B2 ) THEN
                  GAP1 = A2 - DN - ( B2 / GAP2 )*B2
               ELSE
                  GAP1 = A2 - DN - ( B1+B2 )
               ENDIF
               IF( GAP1.GT.ZERO .AND. GAP1.GT.B1 ) THEN
                  S = MAX( DN-( B1 / GAP1 )*B1, HALF*DMIN )
                  TTYPE = -2
               ELSE
                  S = ZERO
                  IF( DN.GT.B1 ) &
                     S = DN - B1
                  IF( A2.GT.( B1+B2 ) ) &
                     S = MIN( S, A2-( B1+B2 ) )
                  S = MAX( S, THIRD*DMIN )
                  TTYPE = -3
               ENDIF
            ELSE
!
!              Case 4.
!
               TTYPE = -4
               S = QURTR*DMIN
               IF( DMIN.EQ.DN ) THEN
                  GAM = DN
                  A2 = ZERO
                  IF( Z( NN-5 ) .GT. Z( NN-7 ) ) &
                     RETURN
                  B2 = Z( NN-5 ) / Z( NN-7 )
                  NP = NN - 9
               ELSE
                  NP = NN - 2*PP
                  B2 = Z( NP-2 )
                  GAM = DN1
                  IF( Z( NP-4 ) .GT. Z( NP-2 ) ) &
                     RETURN
                  A2 = Z( NP-4 ) / Z( NP-2 )
                  IF( Z( NN-9 ) .GT. Z( NN-11 ) ) &
                     RETURN
                  B2 = Z( NN-9 ) / Z( NN-11 )
                  NP = NN - 13
               ENDIF
!
!              Approximate contribution to norm squared from I < NN-1.
!
               A2 = A2 + B2
               DO 10 I4 = NP, 4*I0 - 1 + PP, -4
                  IF( B2.EQ.ZERO ) &
                     GO TO 20
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) ) &
                     RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 )  &
                     GO TO 20
   10          CONTINUE
   20          CONTINUE
               A2 = CNST3*A2
!
!              Rayleigh quotient residual bound.
!
               IF( A2.LT.CNST1 ) &
                  S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
            ENDIF
         ELSE IF( DMIN.EQ.DN2 ) THEN
!
!           Case 5.
!
            TTYPE = -5
            S = QURTR*DMIN
!
!           Compute contribution to norm squared from I > NN-2.
!
            NP = NN - 2*PP
            B1 = Z( NP-2 )
            B2 = Z( NP-6 )
            GAM = DN2
            IF( Z( NP-8 ).GT.B2 .OR. Z( NP-4 ).GT.B1 ) &
               RETURN
            A2 = ( Z( NP-8 ) / B2 )*( ONE+Z( NP-4 ) / B1 )
!
!           Approximate contribution to norm squared from I < NN-2.
!
            IF( N0-I0.GT.2 ) THEN
               B2 = Z( NN-13 ) / Z( NN-15 )
               A2 = A2 + B2
               DO 30 I4 = NN - 17, 4*I0 - 1 + PP, -4
                  IF( B2.EQ.ZERO ) &
                     GO TO 40
                  B1 = B2
                  IF( Z( I4 ) .GT. Z( I4-2 ) ) &
                     RETURN
                  B2 = B2*( Z( I4 ) / Z( I4-2 ) )
                  A2 = A2 + B2
                  IF( HUNDRD*MAX( B2, B1 ).LT.A2 .OR. CNST1.LT.A2 )  &
                     GO TO 40
   30          CONTINUE
   40          CONTINUE
               A2 = CNST3*A2
            ENDIF
!
            IF( A2.LT.CNST1 ) &
               S = GAM*( ONE-SQRT( A2 ) ) / ( ONE+A2 )
         ELSE
!
!           Case 6, no information to guide us.
!
            IF( TTYPE.EQ.-6 ) THEN
               G = G + THIRD*( ONE-G )
            ELSE IF( TTYPE.EQ.-18 ) THEN
               G = QURTR*THIRD
            ELSE
               G = QURTR
            ENDIF
            S = G*DMIN
            TTYPE = -6
         ENDIF
!
      ELSE IF( N0IN.EQ.( N0+1 ) ) THEN
!
!        One eigenvalue just deflated. Use DMIN1, DN1 for DMIN and DN.
!
         IF( DMIN1.EQ.DN1 .AND. DMIN2.EQ.DN2 ) THEN 
!
!           Cases 7 and 8.
!
            TTYPE = -7
            S = THIRD*DMIN1
            IF( Z( NN-5 ).GT.Z( NN-7 ) ) &
               RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO ) &
               GO TO 60
            DO 50 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               A2 = B1
               IF( Z( I4 ).GT.Z( I4-2 ) ) &
                  RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*MAX( B1, A2 ).LT.B2 )  &
                  GO TO 60
   50       CONTINUE
   60       CONTINUE
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN1 / ( ONE+B2**2 )
            GAP2 = HALF*DMIN2 - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE 
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
               TTYPE = -8
            ENDIF
         ELSE
!
!           Case 9.
!
            S = QURTR*DMIN1
            IF( DMIN1.EQ.DN1 ) &
               S = HALF*DMIN1
            TTYPE = -9
         ENDIF
!
      ELSE IF( N0IN.EQ.( N0+2 ) ) THEN
!
!        Two eigenvalues deflated. Use DMIN2, DN2 for DMIN and DN.
!
!        Cases 10 and 11.
!
         IF( DMIN2.EQ.DN2 .AND. TWO*Z( NN-5 ).LT.Z( NN-7 ) ) THEN 
            TTYPE = -10
            S = THIRD*DMIN2
            IF( Z( NN-5 ).GT.Z( NN-7 ) ) &
               RETURN
            B1 = Z( NN-5 ) / Z( NN-7 )
            B2 = B1
            IF( B2.EQ.ZERO ) &
               GO TO 80
            DO 70 I4 = 4*N0 - 9 + PP, 4*I0 - 1 + PP, -4
               IF( Z( I4 ).GT.Z( I4-2 ) ) &
                  RETURN
               B1 = B1*( Z( I4 ) / Z( I4-2 ) )
               B2 = B2 + B1
               IF( HUNDRD*B1.LT.B2 ) &
                  GO TO 80
   70       CONTINUE
   80       CONTINUE
            B2 = SQRT( CNST3*B2 )
            A2 = DMIN2 / ( ONE+B2**2 )
            GAP2 = Z( NN-7 ) + Z( NN-9 ) - &
                   SQRT( Z( NN-11 ) )*SQRT( Z( NN-9 ) ) - A2
            IF( GAP2.GT.ZERO .AND. GAP2.GT.B2*A2 ) THEN
               S = MAX( S, A2*( ONE-CNST2*A2*( B2 / GAP2 )*B2 ) )
            ELSE 
               S = MAX( S, A2*( ONE-CNST2*B2 ) )
            ENDIF
         ELSE
            S = QURTR*DMIN2
            TTYPE = -11
         ENDIF
      ELSE IF( N0IN.GT.( N0+2 ) ) THEN
!
!        Case 12, more than two eigenvalues deflated. No information.
!
         S = ZERO 
         TTYPE = -12
      ENDIF
!
      TAU = S
      RETURN
!
!     End of DLAZQ4
!
      END SUBROUTINE DLAZQ4
      
      
      
      
      
      SUBROUTINE DORM2L( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                         WORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DORM2L overwrites the general real m by n matrix C with
!
!        Q * C  if SIDE = 'L' and TRANS = 'N', or
!
!        Q'* C  if SIDE = 'L' and TRANS = 'T', or
!
!        C * Q  if SIDE = 'R' and TRANS = 'N', or
!
!        C * Q' if SIDE = 'R' and TRANS = 'T',
!
!  where Q is a real orthogonal matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(k) . . . H(2) H(1)
!
!  as returned by DGEQLF. Q is of order m if SIDE = 'L' and of order n
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q' from the Left
!          = 'R': apply Q or Q' from the Right
!
!  TRANS   (input) CHARACTER*1
!          = 'N': apply Q  (No transpose)
!          = 'T': apply Q' (Transpose)
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          DGEQLF in the last k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQLF.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension
!                                   (N) if SIDE = 'L',
!                                   (M) if SIDE = 'R'
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, MI, NI, NQ
      DOUBLE PRECISION   AII
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
!
!     NQ is the order of Q
!
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      ENDIF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ENDIF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2L', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) &
         RETURN
!
      IF( ( LEFT .AND. NOTRAN ) .OR. ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) &
           THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      ENDIF
!
      IF( LEFT ) THEN
         NI = N
      ELSE
         MI = M
      ENDIF
!
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
!
!           H(i) is applied to C(1:m-k+i,1:n)
!
            MI = M - K + I
         ELSE
!
!           H(i) is applied to C(1:m,1:n-k+i)
!
            NI = N - K + I
         ENDIF
!
!        Apply H(i)
!
         AII = A( NQ-K+I, I )
         A( NQ-K+I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( 1, I ), 1, TAU( I ), C, LDC, &
                     WORK )
         A( NQ-K+I, I ) = AII
   10 CONTINUE
      RETURN
!
!     End of DORM2L
!
      END SUBROUTINE DORM2L
      
      
      
      
      
      SUBROUTINE DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                         WORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DORM2R overwrites the general real m by n matrix C with
!
!        Q * C  if SIDE = 'L' and TRANS = 'N', or
!
!        Q'* C  if SIDE = 'L' and TRANS = 'T', or
!
!        C * Q  if SIDE = 'R' and TRANS = 'N', or
!
!        C * Q' if SIDE = 'R' and TRANS = 'T',
!
!  where Q is a real orthogonal matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(1) H(2) . . . H(k)
!
!  as returned by DGEQRF. Q is of order m if SIDE = 'L' and of order n
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q' from the Left
!          = 'R': apply Q or Q' from the Right
!
!  TRANS   (input) CHARACTER*1
!          = 'N': apply Q  (No transpose)
!          = 'T': apply Q' (Transpose)
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          DGEQRF in the first k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQRF.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the m by n matrix C.
!          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension
!                                   (N) if SIDE = 'L',
!                                   (M) if SIDE = 'R'
!
!  INFO    (output) INTEGER
!          = 0: successful exit
!          < 0: if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, NOTRAN
      INTEGER            I, I1, I2, I3, IC, JC, MI, NI, NQ
      DOUBLE PRECISION   AII
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      EXTERNAL           LSAME
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLARF, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
!
!     NQ is the order of Q
!
      IF( LEFT ) THEN
         NQ = M
      ELSE
         NQ = N
      ENDIF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ENDIF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORM2R', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) &
         RETURN
!
      IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. ( .NOT.LEFT .AND. NOTRAN ) ) &
           THEN
         I1 = 1
         I2 = K
         I3 = 1
      ELSE
         I1 = K
         I2 = 1
         I3 = -1
      ENDIF
!
      IF( LEFT ) THEN
         NI = N
         JC = 1
      ELSE
         MI = M
         IC = 1
      ENDIF
!
      DO 10 I = I1, I2, I3
         IF( LEFT ) THEN
!
!           H(i) is applied to C(i:m,1:n)
!
            MI = M - I + 1
            IC = I
         ELSE
!
!           H(i) is applied to C(1:m,i:n)
!
            NI = N - I + 1
            JC = I
         ENDIF
!
!        Apply H(i)
!
         AII = A( I, I )
         A( I, I ) = ONE
         CALL DLARF( SIDE, MI, NI, A( I, I ), 1, TAU( I ), C( IC, JC ), &
                     LDC, WORK )
         A( I, I ) = AII
   10 CONTINUE
      RETURN
!
!     End of DORM2R
!
      END SUBROUTINE DORM2R
      
      
      
      
      
      SUBROUTINE DORMQL( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                         WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DORMQL overwrites the general real M-by-N matrix C with
!
!                  SIDE = 'L'     SIDE = 'R'
!  TRANS = 'N':      Q * C          C * Q
!  TRANS = 'T':      Q**T * C       C * Q**T
!
!  where Q is a real orthogonal matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(k) . . . H(2) H(1)
!
!  as returned by DGEQLF. Q is of order M if SIDE = 'L' and of order N
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q**T from the Left;
!          = 'R': apply Q or Q**T from the Right.
!
!  TRANS   (input) CHARACTER*1
!          = 'N':  No transpose, apply Q;
!          = 'T':  Transpose, apply Q**T.
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          DGEQLF in the last k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQLF.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.
!          If SIDE = 'L', LWORK >= max(1,N);
!          if SIDE = 'R', LWORK >= max(1,M).
!          For optimum performance LWORK >= N*NB if SIDE = 'L', and
!          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
!          blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IINFO, IWS, LDWORK, LWKOPT, &
                         MI, NB, NBMIN, NI, NQ, NW
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORM2L, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
!
!     NQ is the order of Q and NW is the minimum dimension of WORK
!
      IF( LEFT ) THEN
         NQ = M
         NW = MAX( 1, N )
      ELSE
         NQ = N
         NW = MAX( 1, M )
      ENDIF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ENDIF
!
      IF( INFO.EQ.0 ) THEN
         IF( M.EQ.0 .OR. N.EQ.0 ) THEN
            LWKOPT = 1
         ELSE
!
!           Determine the block size.  NB may be at most NBMAX, where
!           NBMAX is used to define the local array T.
!
            NB = MIN( NBMAX, ILAENV( 1, 'DORMQL', SIDE // TRANS, M, N, &
                                     K, -1 ) )
            LWKOPT = NW*NB
         ENDIF
         WORK( 1 ) = LWKOPT
!
         IF( LWORK.LT.NW .AND. .NOT.LQUERY ) THEN
            INFO = -12
         ENDIF
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMQL', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 ) THEN
         RETURN
      ENDIF
!
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMQL', SIDE // TRANS, M, N, K, &
                    -1 ) )
         ENDIF
      ELSE
         IWS = NW
      ENDIF
!
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
!
!        Use unblocked code
!
         CALL DORM2L( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK, &
                      IINFO )
      ELSE
!
!        Use blocked code
!
         IF( ( LEFT .AND. NOTRAN ) .OR. &
             ( .NOT.LEFT .AND. .NOT.NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         ENDIF
!
         IF( LEFT ) THEN
            NI = N
         ELSE
            MI = M
         ENDIF
!
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
!
!           Form the triangular factor of the block reflector
!           H = H(i+ib-1) . . . H(i+1) H(i)
!
            CALL DLARFT( 'Backward', 'Columnwise', NQ-K+I+IB-1, IB, &
                         A( 1, I ), LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
!
!              H or H' is applied to C(1:m-k+i+ib-1,1:n)
!
               MI = M - K + I + IB - 1
            ELSE
!
!              H or H' is applied to C(1:m,1:n-k+i+ib-1)
!
               NI = N - K + I + IB - 1
            ENDIF
!
!           Apply H or H'
!
            CALL DLARFB( SIDE, TRANS, 'Backward', 'Columnwise', MI, NI, &
                         IB, A( 1, I ), LDA, T, LDT, C, LDC, WORK, &
                         LDWORK )
   10    CONTINUE
      ENDIF
      WORK( 1 ) = LWKOPT
      RETURN
!
!     End of DORMQL
!
      END SUBROUTINE DORMQL
      
      
      
      
      
      SUBROUTINE DORMQR( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, &
                         WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS
      INTEGER            INFO, K, LDA, LDC, LWORK, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DORMQR overwrites the general real M-by-N matrix C with
!
!                  SIDE = 'L'     SIDE = 'R'
!  TRANS = 'N':      Q * C          C * Q
!  TRANS = 'T':      Q**T * C       C * Q**T
!
!  where Q is a real orthogonal matrix defined as the product of k
!  elementary reflectors
!
!        Q = H(1) H(2) . . . H(k)
!
!  as returned by DGEQRF. Q is of order M if SIDE = 'L' and of order N
!  if SIDE = 'R'.
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q**T from the Left;
!          = 'R': apply Q or Q**T from the Right.
!
!  TRANS   (input) CHARACTER*1
!          = 'N':  No transpose, apply Q;
!          = 'T':  Transpose, apply Q**T.
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  K       (input) INTEGER
!          The number of elementary reflectors whose product defines
!          the matrix Q.
!          If SIDE = 'L', M >= K >= 0;
!          if SIDE = 'R', N >= K >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension (LDA,K)
!          The i-th column must contain the vector which defines the
!          elementary reflector H(i), for i = 1,2,...,k, as returned by
!          DGEQRF in the first k columns of its array argument A.
!          A is modified by the routine but restored on exit.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          If SIDE = 'L', LDA >= max(1,M);
!          if SIDE = 'R', LDA >= max(1,N).
!
!  TAU     (input) DOUBLE PRECISION array, dimension (K)
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DGEQRF.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.
!          If SIDE = 'L', LWORK >= max(1,N);
!          if SIDE = 'R', LWORK >= max(1,M).
!          For optimum performance LWORK >= N*NB if SIDE = 'L', and
!          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
!          blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Parameters ..
      INTEGER            NBMAX, LDT
      PARAMETER          ( NBMAX = 64, LDT = NBMAX+1 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, NOTRAN
      INTEGER            I, I1, I2, I3, IB, IC, IINFO, IWS, JC, LDWORK, &
                         LWKOPT, MI, NB, NBMIN, NI, NQ, NW
!     ..
!     .. Local Arrays ..
      DOUBLE PRECISION   T( LDT, NBMAX )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLARFB, DLARFT, DORM2R, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      NOTRAN = LSAME( TRANS, 'N' )
      LQUERY = ( LWORK.EQ.-1 )
!
!     NQ is the order of Q and NW is the minimum dimension of WORK
!
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      ENDIF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.NOTRAN .AND. .NOT.LSAME( TRANS, 'T' ) ) THEN
         INFO = -2
      ELSE IF( M.LT.0 ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( K.LT.0 .OR. K.GT.NQ ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      ENDIF
!
      IF( INFO.EQ.0 ) THEN
!
!        Determine the block size.  NB may be at most NBMAX, where NBMAX
!        is used to define the local array T.
!
         NB = MIN( NBMAX, ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N, K, &
              -1 ) )
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMQR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. K.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      ENDIF
!
      NBMIN = 2
      LDWORK = NW
      IF( NB.GT.1 .AND. NB.LT.K ) THEN
         IWS = NW*NB
         IF( LWORK.LT.IWS ) THEN
            NB = LWORK / LDWORK
            NBMIN = MAX( 2, ILAENV( 2, 'DORMQR', SIDE // TRANS, M, N, K, &
                    -1 ) )
         ENDIF
      ELSE
         IWS = NW
      ENDIF
!
      IF( NB.LT.NBMIN .OR. NB.GE.K ) THEN
!
!        Use unblocked code
!
         CALL DORM2R( SIDE, TRANS, M, N, K, A, LDA, TAU, C, LDC, WORK, &
                      IINFO )
      ELSE
!
!        Use blocked code
!
         IF( ( LEFT .AND. .NOT.NOTRAN ) .OR. &
             ( .NOT.LEFT .AND. NOTRAN ) ) THEN
            I1 = 1
            I2 = K
            I3 = NB
         ELSE
            I1 = ( ( K-1 ) / NB )*NB + 1
            I2 = 1
            I3 = -NB
         ENDIF
!
         IF( LEFT ) THEN
            NI = N
            JC = 1
         ELSE
            MI = M
            IC = 1
         ENDIF
!
         DO 10 I = I1, I2, I3
            IB = MIN( NB, K-I+1 )
!
!           Form the triangular factor of the block reflector
!           H = H(i) H(i+1) . . . H(i+ib-1)
!
            CALL DLARFT( 'Forward', 'Columnwise', NQ-I+1, IB, A( I, I ), &
                         LDA, TAU( I ), T, LDT )
            IF( LEFT ) THEN
!
!              H or H' is applied to C(i:m,1:n)
!
               MI = M - I + 1
               IC = I
            ELSE
!
!              H or H' is applied to C(1:m,i:n)
!
               NI = N - I + 1
               JC = I
            ENDIF
!
!           Apply H or H'
!
            CALL DLARFB( SIDE, TRANS, 'Forward', 'Columnwise', MI, NI, &
                         IB, A( I, I ), LDA, T, LDT, C( IC, JC ), LDC, &
                         WORK, LDWORK )
   10    CONTINUE
      ENDIF
      WORK( 1 ) = LWKOPT
      RETURN
!
!     End of DORMQR
!
      END SUBROUTINE DORMQR
      
      
      
      
      
      SUBROUTINE DORMTR( SIDE, UPLO, TRANS, M, N, A, LDA, TAU, C, LDC, &
                         WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          SIDE, TRANS, UPLO
      INTEGER            INFO, LDA, LDC, LWORK, M, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), C( LDC, * ), TAU( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DORMTR overwrites the general real M-by-N matrix C with
!
!                  SIDE = 'L'     SIDE = 'R'
!  TRANS = 'N':      Q * C          C * Q
!  TRANS = 'T':      Q**T * C       C * Q**T
!
!  where Q is a real orthogonal matrix of order nq, with nq = m if
!  SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
!  nq-1 elementary reflectors, as returned by DSYTRD:
!
!  if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
!
!  if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
!
!  Arguments
!  =========
!
!  SIDE    (input) CHARACTER*1
!          = 'L': apply Q or Q**T from the Left;
!          = 'R': apply Q or Q**T from the Right.
!
!  UPLO    (input) CHARACTER*1
!          = 'U': Upper triangle of A contains elementary reflectors
!                 from DSYTRD;
!          = 'L': Lower triangle of A contains elementary reflectors
!                 from DSYTRD.
!
!  TRANS   (input) CHARACTER*1
!          = 'N':  No transpose, apply Q;
!          = 'T':  Transpose, apply Q**T.
!
!  M       (input) INTEGER
!          The number of rows of the matrix C. M >= 0.
!
!  N       (input) INTEGER
!          The number of columns of the matrix C. N >= 0.
!
!  A       (input) DOUBLE PRECISION array, dimension
!                               (LDA,M) if SIDE = 'L'
!                               (LDA,N) if SIDE = 'R'
!          The vectors which define the elementary reflectors, as
!          returned by DSYTRD.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.
!          LDA >= max(1,M) if SIDE = 'L'; LDA >= max(1,N) if SIDE = 'R'.
!
!  TAU     (input) DOUBLE PRECISION array, dimension
!                               (M-1) if SIDE = 'L'
!                               (N-1) if SIDE = 'R'
!          TAU(i) must contain the scalar factor of the elementary
!          reflector H(i), as returned by DSYTRD.
!
!  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
!          On entry, the M-by-N matrix C.
!          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
!
!  LDC     (input) INTEGER
!          The leading dimension of the array C. LDC >= max(1,M).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.
!          If SIDE = 'L', LWORK >= max(1,N);
!          if SIDE = 'R', LWORK >= max(1,M).
!          For optimum performance LWORK >= N*NB if SIDE = 'L', and
!          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
!          blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  =====================================================================
!
!     .. Local Scalars ..
      LOGICAL            LEFT, LQUERY, UPPER
      INTEGER            I1, I2, IINFO, LWKOPT, MI, NB, NI, NQ, NW
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. External Subroutines ..
      EXTERNAL           DORMQL, DORMQR, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. Executable Statements ..
!
!     Test the input arguments
!
      INFO = 0
      LEFT = LSAME( SIDE, 'L' )
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
!
!     NQ is the order of Q and NW is the minimum dimension of WORK
!
      IF( LEFT ) THEN
         NQ = M
         NW = N
      ELSE
         NQ = N
         NW = M
      ENDIF
      IF( .NOT.LEFT .AND. .NOT.LSAME( SIDE, 'R' ) ) THEN
         INFO = -1
      ELSE IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -2
      ELSE IF( .NOT.LSAME( TRANS, 'N' ) .AND. .NOT.LSAME( TRANS, 'T' ) ) &
                THEN
         INFO = -3
      ELSE IF( M.LT.0 ) THEN
         INFO = -4
      ELSE IF( N.LT.0 ) THEN
         INFO = -5
      ELSE IF( LDA.LT.MAX( 1, NQ ) ) THEN
         INFO = -7
      ELSE IF( LDC.LT.MAX( 1, M ) ) THEN
         INFO = -10
      ELSE IF( LWORK.LT.MAX( 1, NW ) .AND. .NOT.LQUERY ) THEN
         INFO = -12
      ENDIF
!
      IF( INFO.EQ.0 ) THEN
         IF( UPPER ) THEN
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M-1, N, M-1, &
                    -1 )
            ELSE
               NB = ILAENV( 1, 'DORMQL', SIDE // TRANS, M, N-1, N-1, &
                    -1 )
            ENDIF
         ELSE
            IF( LEFT ) THEN
               NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M-1, N, M-1, &
                    -1 )
            ELSE
               NB = ILAENV( 1, 'DORMQR', SIDE // TRANS, M, N-1, N-1, &
                    -1 )
            ENDIF
         ENDIF
         LWKOPT = MAX( 1, NW )*NB
         WORK( 1 ) = LWKOPT
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DORMTR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( M.EQ.0 .OR. N.EQ.0 .OR. NQ.EQ.1 ) THEN
         WORK( 1 ) = 1
         RETURN
      ENDIF
!
      IF( LEFT ) THEN
         MI = M - 1
         NI = N
      ELSE
         MI = M
         NI = N - 1
      ENDIF
!
      IF( UPPER ) THEN
!
!        Q was determined by a call to DSYTRD with UPLO = 'U'
!
         CALL DORMQL( SIDE, TRANS, MI, NI, NQ-1, A( 1, 2 ), LDA, TAU, C, &
                      LDC, WORK, LWORK, IINFO )
      ELSE
!
!        Q was determined by a call to DSYTRD with UPLO = 'L'
!
         IF( LEFT ) THEN
            I1 = 2
            I2 = 1
         ELSE
            I1 = 1
            I2 = 2
         ENDIF
         CALL DORMQR( SIDE, TRANS, MI, NI, NQ-1, A( 2, 1 ), LDA, TAU, &
                      C( I1, I2 ), LDC, WORK, LWORK, IINFO )
      ENDIF
      WORK( 1 ) = LWKOPT
      RETURN
!
!     End of DORMTR
!
      END SUBROUTINE DORMTR
      
      
      
      
      
      SUBROUTINE DSTEBZ( RANGE, ORDER, N, VL, VU, IL, IU, ABSTOL, D, E, &
                         M, NSPLIT, W, IBLOCK, ISPLIT, WORK, IWORK, &
                         INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!     8-18-00:  Increase FUDGE factor for T3E (eca)
!
!     .. Scalar Arguments ..
      CHARACTER          ORDER, RANGE
      INTEGER            IL, INFO, IU, M, N, NSPLIT
      DOUBLE PRECISION   ABSTOL, VL, VU
!     ..
!     .. Array Arguments ..
      INTEGER            IBLOCK( * ), ISPLIT( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DSTEBZ computes the eigenvalues of a symmetric tridiagonal
!  matrix T.  The user may ask for all eigenvalues, all eigenvalues
!  in the half-open interval (VL, VU], or the IL-th through IU-th
!  eigenvalues.
!
!  To avoid overflow, the matrix must be scaled so that its
!  largest element is no greater than overflow**(1/2) *
!  underflow**(1/4) in absolute value, and for greatest
!  accuracy, it should not be much smaller than that.
!
!  See W. Kahan "Accurate Eigenvalues of a Symmetric Tridiagonal
!  Matrix", Report CS41, Computer Science Dept., Stanford
!  University, July 21, 1966.
!
!  Arguments
!  =========
!
!  RANGE   (input) CHARACTER*1
!          = 'A': ("All")   all eigenvalues will be found.
!          = 'V': ("Value") all eigenvalues in the half-open interval
!                           (VL, VU] will be found.
!          = 'I': ("Index") the IL-th through IU-th eigenvalues (of the
!                           entire matrix) will be found.
!
!  ORDER   (input) CHARACTER*1
!          = 'B': ("By Block") the eigenvalues will be grouped by
!                              split-off block (see IBLOCK, ISPLIT) and
!                              ordered from smallest to largest within
!                              the block.
!          = 'E': ("Entire matrix")
!                              the eigenvalues for the entire matrix
!                              will be ordered from smallest to
!                              largest.
!
!  N       (input) INTEGER
!          The order of the tridiagonal matrix T.  N >= 0.
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          If RANGE='V', the lower and upper bounds of the interval to
!          be searched for eigenvalues.  Eigenvalues less than or equal
!          to VL, or greater than VU, will not be returned.  VL < VU.
!          Not referenced if RANGE = 'A' or 'I'.
!
!  IL      (input) INTEGER
!  IU      (input) INTEGER
!          If RANGE='I', the indices (in ascending order) of the
!          smallest and largest eigenvalues to be returned.
!          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
!          Not referenced if RANGE = 'A' or 'V'.
!
!  ABSTOL  (input) DOUBLE PRECISION
!          The absolute tolerance for the eigenvalues.  An eigenvalue
!          (or cluster) is considered to be located if it has been
!          determined to lie in an interval whose width is ABSTOL or
!          less.  If ABSTOL is less than or equal to zero, then ULP*|T|
!          will be used, where |T| means the 1-norm of T.
!
!          Eigenvalues will be computed most accurately when ABSTOL is
!          set to twice the underflow threshold 2*DLAMCH('S'), not zero.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The n diagonal elements of the tridiagonal matrix T.
!
!  E       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) off-diagonal elements of the tridiagonal matrix T.
!
!  M       (output) INTEGER
!          The actual number of eigenvalues found. 0 <= M <= N.
!          (See also the description of INFO=2,3.)
!
!  NSPLIT  (output) INTEGER
!          The number of diagonal blocks in the matrix T.
!          1 <= NSPLIT <= N.
!
!  W       (output) DOUBLE PRECISION array, dimension (N)
!          On exit, the first M elements of W will contain the
!          eigenvalues.  (DSTEBZ may use the remaining N-M elements as
!          workspace.)
!
!  IBLOCK  (output) INTEGER array, dimension (N)
!          At each row/column j where E(j) is zero or small, the
!          matrix T is considered to split into a block diagonal
!          matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
!          block (from 1 to the number of blocks) the eigenvalue W(i)
!          belongs.  (DSTEBZ may use the remaining N-M elements as
!          workspace.)
!
!  ISPLIT  (output) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into submatrices.
!          The first submatrix consists of rows/columns 1 to ISPLIT(1),
!          the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
!          etc., and the NSPLIT-th consists of rows/columns
!          ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
!          (Only the first NSPLIT elements will actually be used, but
!          since the user cannot know a priori what value NSPLIT will
!          have, N words must be reserved for ISPLIT.)
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (4*N)
!
!  IWORK   (workspace) INTEGER array, dimension (3*N)
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  some or all of the eigenvalues failed to converge or
!                were not computed:
!                =1 or 3: Bisection failed to converge for some
!                        eigenvalues; these eigenvalues are flagged by a
!                        negative block number.  The effect is that the
!                        eigenvalues may not be as accurate as the
!                        absolute and relative tolerances.  This is
!                        generally caused by unexpectedly inaccurate
!                        arithmetic.
!                =2 or 3: RANGE='I' only: Not all of the eigenvalues
!                        IL:IU were found.
!                        Effect: M < IU+1-IL
!                        Cause:  non-monotonic arithmetic, causing the
!                                Sturm sequence to be non-monotonic.
!                        Cure:   recalculate, using RANGE='A', and pick
!                                out eigenvalues IL:IU.  In some cases,
!                                increasing the PARAMETER "FUDGE" may
!                                make things work.
!                = 4:    RANGE='I', and the Gershgorin interval
!                        initially used was too small.  No eigenvalues
!                        were computed.
!                        Probable cause: your machine has sloppy
!                                        floating-point arithmetic.
!                        Cure: Increase the PARAMETER "FUDGE",
!                              recompile, and try again.
!
!  Internal Parameters
!  ===================
!
!  RELFAC  DOUBLE PRECISION, default = 2.0e0
!          The relative tolerance.  An interval (a,b] lies within
!          "relative tolerance" if  b-a < RELFAC*ulp*max(|a|,|b|),
!          where "ulp" is the machine precision (distance from 1 to
!          the next larger floating point number.)
!
!  FUDGE   DOUBLE PRECISION, default = 2
!          A "fudge factor" to widen the Gershgorin intervals.  Ideally,
!          a value of 1 should work, but on machines with sloppy
!          arithmetic, this needs to be larger.  The default for
!          publicly released versions should be large enough to handle
!          the worst machine around.  Note that this has no effect
!          on accuracy of the solution.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, HALF
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0, &
                         HALF = 1.0D0 / TWO )
      DOUBLE PRECISION   FUDGE, RELFAC
      PARAMETER          ( FUDGE = 2.1D0, RELFAC = 2.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            NCNVRG, TOOFEW
      INTEGER            IB, IBEGIN, IDISCL, IDISCU, IE, IEND, IINFO, &
                         IM, IN, IOFF, IORDER, IOUT, IRANGE, ITMAX, &
                         ITMP1, IW, IWOFF, J, JB, JDISC, JE, NB, NWL, &
                         NWU
      DOUBLE PRECISION   ATOLI, BNORM, GL, GU, PIVMIN, RTOLI, SAFEMN, &
                         TMP1, TMP2, TNORM, ULP, WKILL, WL, WLU, WU, WUL
!     ..
!     .. Local Arrays ..
      INTEGER            IDUMMA( 1 )
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           LSAME, ILAENV, DLAMCH
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLAEBZ, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, INT, LOG, MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
      INFO = 0
!
!     Decode RANGE
!
      IF( LSAME( RANGE, 'A' ) ) THEN
         IRANGE = 1
      ELSE IF( LSAME( RANGE, 'V' ) ) THEN
         IRANGE = 2
      ELSE IF( LSAME( RANGE, 'I' ) ) THEN
         IRANGE = 3
      ELSE
         IRANGE = 0
      ENDIF
!
!     Decode ORDER
!
      IF( LSAME( ORDER, 'B' ) ) THEN
         IORDER = 2
      ELSE IF( LSAME( ORDER, 'E' ) ) THEN
         IORDER = 1
      ELSE
         IORDER = 0
      ENDIF
!
!     Check for Errors
!
      IF( IRANGE.LE.0 ) THEN
         INFO = -1
      ELSE IF( IORDER.LE.0 ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( IRANGE.EQ.2 ) THEN
         IF( VL.GE.VU ) &
            INFO = -5
      ELSE IF( IRANGE.EQ.3 .AND. ( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) ) &
                THEN
         INFO = -6
      ELSE IF( IRANGE.EQ.3 .AND. ( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) ) &
                THEN
         INFO = -7
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEBZ', -INFO )
         RETURN
      ENDIF
!
!     Initialize error flags
!
      INFO = 0
      NCNVRG = .FALSE.
      TOOFEW = .FALSE.
!
!     Quick return if possible
!
      M = 0
      IF( N.EQ.0 ) &
         RETURN
!
!     Simplifications:
!
      IF( IRANGE.EQ.3 .AND. IL.EQ.1 .AND. IU.EQ.N ) &
         IRANGE = 1
!
!     Get machine constants
!     NB is the minimum vector length for vector bisection, or 0
!     if only scalar is to be done.
!
      SAFEMN = DLAMCH( 'S' )
      ULP = DLAMCH( 'P' )
      RTOLI = ULP*RELFAC
      NB = ILAENV( 1, 'DSTEBZ', ' ', N, -1, -1, -1 )
      IF( NB.LE.1 ) &
         NB = 0
!
!     Special Case when N=1
!
      IF( N.EQ.1 ) THEN
         NSPLIT = 1
         ISPLIT( 1 ) = 1
         IF( IRANGE.EQ.2 .AND. ( VL.GE.D( 1 ) .OR. VU.LT.D( 1 ) ) ) THEN
            M = 0
         ELSE
            W( 1 ) = D( 1 )
            IBLOCK( 1 ) = 1
            M = 1
         ENDIF
         RETURN
      ENDIF
!
!     Compute Splitting Points
!
      NSPLIT = 1
      WORK( N ) = ZERO
      PIVMIN = ONE
!
!DIR$ NOVECTOR
      DO 10 J = 2, N
         TMP1 = E( J-1 )**2
         IF( ABS( D( J )*D( J-1 ) )*ULP**2+SAFEMN.GT.TMP1 ) THEN
            ISPLIT( NSPLIT ) = J - 1
            NSPLIT = NSPLIT + 1
            WORK( J-1 ) = ZERO
         ELSE
            WORK( J-1 ) = TMP1
            PIVMIN = MAX( PIVMIN, TMP1 )
         ENDIF
   10 CONTINUE
      ISPLIT( NSPLIT ) = N
      PIVMIN = PIVMIN*SAFEMN
!
!     Compute Interval and ATOLI
!
      IF( IRANGE.EQ.3 ) THEN
!
!        RANGE='I': Compute the interval containing eigenvalues
!                   IL through IU.
!
!        Compute Gershgorin interval for entire (split) matrix
!        and use it as the initial interval
!
         GU = D( 1 )
         GL = D( 1 )
         TMP1 = ZERO
!
         DO 20 J = 1, N - 1
            TMP2 = SQRT( WORK( J ) )
            GU = MAX( GU, D( J )+TMP1+TMP2 )
            GL = MIN( GL, D( J )-TMP1-TMP2 )
            TMP1 = TMP2
   20    CONTINUE
!
         GU = MAX( GU, D( N )+TMP1 )
         GL = MIN( GL, D( N )-TMP1 )
         TNORM = MAX( ABS( GL ), ABS( GU ) )
         GL = GL - FUDGE*TNORM*ULP*N - FUDGE*TWO*PIVMIN
         GU = GU + FUDGE*TNORM*ULP*N + FUDGE*PIVMIN
!
!        Compute Iteration parameters
!
         ITMAX = INT( ( LOG( TNORM+PIVMIN )-LOG( PIVMIN ) ) / &
                 LOG( TWO ) ) + 2
         IF( ABSTOL.LE.ZERO ) THEN
            ATOLI = ULP*TNORM
         ELSE
            ATOLI = ABSTOL
         ENDIF
	!FF: need largest eigenvalue within 1%
	ATOLI = TNORM*0.001d0
!
         WORK( N+1 ) = GL
         WORK( N+2 ) = GL
         WORK( N+3 ) = GU
         WORK( N+4 ) = GU
         WORK( N+5 ) = GL
         WORK( N+6 ) = GU
         IWORK( 1 ) = -1
         IWORK( 2 ) = -1
         IWORK( 3 ) = N + 1
         IWORK( 4 ) = N + 1
         IWORK( 5 ) = IL - 1
         IWORK( 6 ) = IU
!
         CALL DLAEBZ( 3, ITMAX, N, 2, 2, NB, ATOLI, RTOLI, PIVMIN, D, E, &
                      WORK, IWORK( 5 ), WORK( N+1 ), WORK( N+5 ), IOUT, &
                      IWORK, W, IBLOCK, IINFO )
!
         IF( IWORK( 6 ).EQ.IU ) THEN
            WL = WORK( N+1 )
            WLU = WORK( N+3 )
            NWL = IWORK( 1 )
            WU = WORK( N+4 )
            WUL = WORK( N+2 )
            NWU = IWORK( 4 )
         ELSE
            WL = WORK( N+2 )
            WLU = WORK( N+4 )
            NWL = IWORK( 2 )
            WU = WORK( N+3 )
            WUL = WORK( N+1 )
            NWU = IWORK( 3 )
         ENDIF
!
         IF( NWL.LT.0 .OR. NWL.GE.N .OR. NWU.LT.1 .OR. NWU.GT.N ) THEN
            INFO = 4
            RETURN
         ENDIF
      ELSE
!
!        RANGE='A' or 'V' -- Set ATOLI
!
         TNORM = MAX( ABS( D( 1 ) )+ABS( E( 1 ) ), &
                 ABS( D( N ) )+ABS( E( N-1 ) ) )
!
         DO 30 J = 2, N - 1
            TNORM = MAX( TNORM, ABS( D( J ) )+ABS( E( J-1 ) )+ &
                    ABS( E( J ) ) )
   30    CONTINUE
!
         IF( ABSTOL.LE.ZERO ) THEN
            ATOLI = ULP*TNORM
         ELSE
            ATOLI = ABSTOL
         ENDIF
	!FF: need largest eigenvalue within 1%
	ATOLI = TNORM*0.001d0
!
         IF( IRANGE.EQ.2 ) THEN
            WL = VL
            WU = VU
         ELSE
            WL = ZERO
            WU = ZERO
         ENDIF
      ENDIF
!
!     Find Eigenvalues -- Loop Over Blocks and recompute NWL and NWU.
!     NWL accumulates the number of eigenvalues .le. WL,
!     NWU accumulates the number of eigenvalues .le. WU
!
      M = 0
      IEND = 0
      INFO = 0
      NWL = 0
      NWU = 0
!
      DO 70 JB = 1, NSPLIT
         IOFF = IEND
         IBEGIN = IOFF + 1
         IEND = ISPLIT( JB )
         IN = IEND - IOFF
!
         IF( IN.EQ.1 ) THEN
!
!           Special Case -- IN=1
!
            IF( IRANGE.EQ.1 .OR. WL.GE.D( IBEGIN )-PIVMIN ) &
               NWL = NWL + 1
            IF( IRANGE.EQ.1 .OR. WU.GE.D( IBEGIN )-PIVMIN ) &
               NWU = NWU + 1
            IF( IRANGE.EQ.1 .OR. ( WL.LT.D( IBEGIN )-PIVMIN .AND. WU.GE. &
                D( IBEGIN )-PIVMIN ) ) THEN
               M = M + 1
               W( M ) = D( IBEGIN )
               IBLOCK( M ) = JB
            ENDIF
         ELSE
!
!           General Case -- IN > 1
!
!           Compute Gershgorin Interval
!           and use it as the initial interval
!
            GU = D( IBEGIN )
            GL = D( IBEGIN )
            TMP1 = ZERO
!
            DO 40 J = IBEGIN, IEND - 1
               TMP2 = ABS( E( J ) )
               GU = MAX( GU, D( J )+TMP1+TMP2 )
               GL = MIN( GL, D( J )-TMP1-TMP2 )
               TMP1 = TMP2
   40       CONTINUE
!
            GU = MAX( GU, D( IEND )+TMP1 )
            GL = MIN( GL, D( IEND )-TMP1 )
            BNORM = MAX( ABS( GL ), ABS( GU ) )
            GL = GL - FUDGE*BNORM*ULP*IN - FUDGE*PIVMIN
            GU = GU + FUDGE*BNORM*ULP*IN + FUDGE*PIVMIN
!
!           Compute ATOLI for the current submatrix
!
            IF( ABSTOL.LE.ZERO ) THEN
               ATOLI = ULP*MAX( ABS( GL ), ABS( GU ) )
            ELSE
               ATOLI = ABSTOL
            ENDIF
	!FF: need largest eigenvalue within 1%
	ATOLI = MAX( ABS( GL ), ABS( GU ) )*0.001d0
!
            IF( IRANGE.GT.1 ) THEN
               IF( GU.LT.WL ) THEN
                  NWL = NWL + IN
                  NWU = NWU + IN
                  GO TO 70
               ENDIF
               GL = MAX( GL, WL )
               GU = MIN( GU, WU )
               IF( GL.GE.GU ) &
                  GO TO 70
            ENDIF
!
!           Set Up Initial Interval
!
            WORK( N+1 ) = GL
            WORK( N+IN+1 ) = GU
            CALL DLAEBZ( 1, 0, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN, &
                         D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ), &
                         IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IM, &
                         IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
!
            NWL = NWL + IWORK( 1 )
            NWU = NWU + IWORK( IN+1 )
            IWOFF = M - IWORK( 1 )
!
!           Compute Eigenvalues
!
            ITMAX = INT( ( LOG( GU-GL+PIVMIN )-LOG( PIVMIN ) ) / &
                    LOG( TWO ) ) + 2
            CALL DLAEBZ( 2, ITMAX, IN, IN, 1, NB, ATOLI, RTOLI, PIVMIN, &
                         D( IBEGIN ), E( IBEGIN ), WORK( IBEGIN ), &
                         IDUMMA, WORK( N+1 ), WORK( N+2*IN+1 ), IOUT, &
                         IWORK, W( M+1 ), IBLOCK( M+1 ), IINFO )
!
!           Copy Eigenvalues Into W and IBLOCK
!           Use -JB for block number for unconverged eigenvalues.
!
            DO 60 J = 1, IOUT
               TMP1 = HALF*( WORK( J+N )+WORK( J+IN+N ) )
!
!              Flag non-convergence.
!
               IF( J.GT.IOUT-IINFO ) THEN
                  NCNVRG = .TRUE.
                  IB = -JB
               ELSE
                  IB = JB
               ENDIF
               DO 50 JE = IWORK( J ) + 1 + IWOFF, &
                       IWORK( J+IN ) + IWOFF
                  W( JE ) = TMP1
                  IBLOCK( JE ) = IB
   50          CONTINUE
   60       CONTINUE
!
            M = M + IM
         ENDIF
   70 CONTINUE
!
!     If RANGE='I', then (WL,WU) contains eigenvalues NWL+1,...,NWU
!     If NWL+1 < IL or NWU > IU, discard extra eigenvalues.
!
      IF( IRANGE.EQ.3 ) THEN
         IM = 0
         IDISCL = IL - 1 - NWL
         IDISCU = NWU - IU
!
         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
            DO 80 JE = 1, M
               IF( W( JE ).LE.WLU .AND. IDISCL.GT.0 ) THEN
                  IDISCL = IDISCL - 1
               ELSE IF( W( JE ).GE.WUL .AND. IDISCU.GT.0 ) THEN
                  IDISCU = IDISCU - 1
               ELSE
                  IM = IM + 1
                  W( IM ) = W( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               ENDIF
   80       CONTINUE
            M = IM
         ENDIF
         IF( IDISCL.GT.0 .OR. IDISCU.GT.0 ) THEN
!
!           Code to deal with effects of bad arithmetic:
!           Some low eigenvalues to be discarded are not in (WL,WLU],
!           or high eigenvalues to be discarded are not in (WUL,WU]
!           so just kill off the smallest IDISCL/largest IDISCU
!           eigenvalues, by simply finding the smallest/largest
!           eigenvalue(s).
!
!           (If N(w) is monotone non-decreasing, this should never
!               happen.)
!
            IF( IDISCL.GT.0 ) THEN
               WKILL = WU
               DO 100 JDISC = 1, IDISCL
                  IW = 0
                  DO 90 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND. &
                         ( W( JE ).LT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     ENDIF
   90             CONTINUE
                  IBLOCK( IW ) = 0
  100          CONTINUE
            ENDIF
            IF( IDISCU.GT.0 ) THEN
!
               WKILL = WL
               DO 120 JDISC = 1, IDISCU
                  IW = 0
                  DO 110 JE = 1, M
                     IF( IBLOCK( JE ).NE.0 .AND. &
                         ( W( JE ).GT.WKILL .OR. IW.EQ.0 ) ) THEN
                        IW = JE
                        WKILL = W( JE )
                     ENDIF
  110             CONTINUE
                  IBLOCK( IW ) = 0
  120          CONTINUE
            ENDIF
            IM = 0
            DO 130 JE = 1, M
               IF( IBLOCK( JE ).NE.0 ) THEN
                  IM = IM + 1
                  W( IM ) = W( JE )
                  IBLOCK( IM ) = IBLOCK( JE )
               ENDIF
  130       CONTINUE
            M = IM
         ENDIF
         IF( IDISCL.LT.0 .OR. IDISCU.LT.0 ) THEN
            TOOFEW = .TRUE.
         ENDIF
      ENDIF
!
!     If ORDER='B', do nothing -- the eigenvalues are already sorted
!        by block.
!     If ORDER='E', sort the eigenvalues from smallest to largest
!
      IF( IORDER.EQ.1 .AND. NSPLIT.GT.1 ) THEN
         DO 150 JE = 1, M - 1
            IE = 0
            TMP1 = W( JE )
            DO 140 J = JE + 1, M
               IF( W( J ).LT.TMP1 ) THEN
                  IE = J
                  TMP1 = W( J )
               ENDIF
  140       CONTINUE
!
            IF( IE.NE.0 ) THEN
               ITMP1 = IBLOCK( IE )
               W( IE ) = W( JE )
               IBLOCK( IE ) = IBLOCK( JE )
               W( JE ) = TMP1
               IBLOCK( JE ) = ITMP1
            ENDIF
  150    CONTINUE
      ENDIF
!
      INFO = 0
      IF( NCNVRG ) &
         INFO = INFO + 1
      IF( TOOFEW ) &
         INFO = INFO + 2
      RETURN
!
!     End of DSTEBZ
!
      END SUBROUTINE DSTEBZ
      
      
      
      
      
      SUBROUTINE DSTEIN( N, D, E, M, W, IBLOCK, ISPLIT, Z, LDZ, WORK, &
                         IWORK, IFAIL, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, LDZ, M, N
!     ..
!     .. Array Arguments ..
      INTEGER            IBLOCK( * ), IFAIL( * ), ISPLIT( * ), &
                         IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * ), Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  DSTEIN computes the eigenvectors of a real symmetric tridiagonal
!  matrix T corresponding to specified eigenvalues, using inverse
!  iteration.
!
!  The maximum number of iterations allowed for each eigenvector is
!  specified by an internal parameter MAXITS (currently set to 5).
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.
!
!  D       (input) DOUBLE PRECISION array, dimension (N)
!          The n diagonal elements of the tridiagonal matrix T.
!
!  E       (input) DOUBLE PRECISION array, dimension (N-1)
!          The (n-1) subdiagonal elements of the tridiagonal matrix
!          T, in elements 1 to N-1.
!
!  M       (input) INTEGER
!          The number of eigenvectors to be found.  0 <= M <= N.
!
!  W       (input) DOUBLE PRECISION array, dimension (N)
!          The first M elements of W contain the eigenvalues for
!          which eigenvectors are to be computed.  The eigenvalues
!          should be grouped by split-off block and ordered from
!          smallest to largest within the block.  ( The output array
!          W from DSTEBZ with ORDER = 'B' is expected here. )
!
!  IBLOCK  (input) INTEGER array, dimension (N)
!          The submatrix indices associated with the corresponding
!          eigenvalues in W; IBLOCK(i)=1 if eigenvalue W(i) belongs to
!          the first submatrix from the top, =2 if W(i) belongs to
!          the second submatrix, etc.  ( The output array IBLOCK
!          from DSTEBZ is expected here. )
!
!  ISPLIT  (input) INTEGER array, dimension (N)
!          The splitting points, at which T breaks up into submatrices.
!          The first submatrix consists of rows/columns 1 to
!          ISPLIT( 1 ), the second of rows/columns ISPLIT( 1 )+1
!          through ISPLIT( 2 ), etc.
!          ( The output array ISPLIT from DSTEBZ is expected here. )
!
!  Z       (output) DOUBLE PRECISION array, dimension (LDZ, M)
!          The computed eigenvectors.  The eigenvector associated
!          with the eigenvalue W(i) is stored in the i-th column of
!          Z.  Any vector which fails to converge is set to its current
!          iterate after MAXITS iterations.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= max(1,N).
!
!  WORK    (workspace) DOUBLE PRECISION array, dimension (5*N)
!
!  IWORK   (workspace) INTEGER array, dimension (N)
!
!  IFAIL   (output) INTEGER array, dimension (M)
!          On normal exit, all elements of IFAIL are zero.
!          If one or more eigenvectors fail to converge after
!          MAXITS iterations, then their indices are stored in
!          array IFAIL.
!
!  INFO    (output) INTEGER
!          = 0: successful exit.
!          < 0: if INFO = -i, the i-th argument had an illegal value
!          > 0: if INFO = i, then i eigenvectors failed to converge
!               in MAXITS iterations.  Their indices are stored in
!               array IFAIL.
!
!  Internal Parameters
!  ===================
!
!  MAXITS  INTEGER, default = 5
!          The maximum number of iterations performed.
!
!  EXTRA   INTEGER, default = 2
!          The number of iterations performed after norm growth
!          criterion is satisfied, should be at least 1.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TEN, ODM3, ODM1
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TEN = 1.0D+1, &
                         ODM3 = 1.0D-3, ODM1 = 1.0D-1 )
      INTEGER            MAXITS, EXTRA
      PARAMETER          ( MAXITS = 5, EXTRA = 2 )
!     ..
!     .. Local Scalars ..
      INTEGER            B1, BLKSIZ, BN, GPIND, I, IINFO, INDRV1, &
                         INDRV2, INDRV3, INDRV4, INDRV5, ITS, J, J1, &
                         JBLK, JMAX, NBLK, NRMCHK
      DOUBLE PRECISION   DTPCRT, EPS, EPS1, NRM, ONENRM, ORTOL, PERTOL, &
                         SCL, SEP, TOL, XJ, XJM, ZTR
!     ..
!     .. Local Arrays ..
      INTEGER            ISEED( 4 )
!     ..
!     .. External Functions ..
      INTEGER            IDAMAX
      DOUBLE PRECISION   DASUM, DDOT, DLAMCH, DNRM2
      EXTERNAL           IDAMAX, DASUM, DDOT, DLAMCH, DNRM2
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DCOPY, DLAGTF, DLAGTS, DLARNV, DSCAL, &
                         XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
      DO 10 I = 1, M
         IFAIL( I ) = 0
   10 CONTINUE
!
      IF( N.LT.0 ) THEN
         INFO = -1
      ELSE IF( M.LT.0 .OR. M.GT.N ) THEN
         INFO = -4
      ELSE IF( LDZ.LT.MAX( 1, N ) ) THEN
         INFO = -9
      ELSE
         DO 20 J = 2, M
            IF( IBLOCK( J ).LT.IBLOCK( J-1 ) ) THEN
               INFO = -6
               GO TO 30
            ENDIF
            IF( IBLOCK( J ).EQ.IBLOCK( J-1 ) .AND. W( J ).LT.W( J-1 ) ) &
                 THEN
               INFO = -5
               GO TO 30
            ENDIF
   20    CONTINUE
   30    CONTINUE
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSTEIN', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( N.EQ.0 .OR. M.EQ.0 ) THEN
         RETURN
      ELSE IF( N.EQ.1 ) THEN
         Z( 1, 1 ) = ONE
         RETURN
      ENDIF
!
!     Get machine constants.
!
      EPS = DLAMCH( 'Precision' )
!
!     Initialize seed for random number generator DLARNV.
!
      DO 40 I = 1, 4
         ISEED( I ) = 1
   40 CONTINUE
!
!     Initialize pointers.
!
      INDRV1 = 0
      INDRV2 = INDRV1 + N
      INDRV3 = INDRV2 + N
      INDRV4 = INDRV3 + N
      INDRV5 = INDRV4 + N
!
!     Compute eigenvectors of matrix blocks.
!
      J1 = 1
      DO 160 NBLK = 1, IBLOCK( M )
!
!        Find starting and ending indices of block nblk.
!
         IF( NBLK.EQ.1 ) THEN
            B1 = 1
         ELSE
            B1 = ISPLIT( NBLK-1 ) + 1
         ENDIF
         BN = ISPLIT( NBLK )
         BLKSIZ = BN - B1 + 1
         IF( BLKSIZ.EQ.1 ) &
            GO TO 60
         GPIND = B1
!
!        Compute reorthogonalization criterion and stopping criterion.
!
         ONENRM = ABS( D( B1 ) ) + ABS( E( B1 ) )
         ONENRM = MAX( ONENRM, ABS( D( BN ) )+ABS( E( BN-1 ) ) )
         DO 50 I = B1 + 1, BN - 1
            ONENRM = MAX( ONENRM, ABS( D( I ) )+ABS( E( I-1 ) )+ &
                     ABS( E( I ) ) )
   50    CONTINUE
         ORTOL = ODM3*ONENRM
!
         DTPCRT = SQRT( ODM1 / BLKSIZ )
!
!        Loop through eigenvalues of block nblk.
!
   60    CONTINUE
         JBLK = 0
         DO 150 J = J1, M
            IF( IBLOCK( J ).NE.NBLK ) THEN
               J1 = J
               GO TO 160
            ENDIF
            JBLK = JBLK + 1
            XJ = W( J )
!
!           Skip all the work if the block size is one.
!
            IF( BLKSIZ.EQ.1 ) THEN
               WORK( INDRV1+1 ) = ONE
               GO TO 120
            ENDIF
!
!           If eigenvalues j and j-1 are too close, add a relatively
!           small perturbation.
!
            IF( JBLK.GT.1 ) THEN
               EPS1 = ABS( EPS*XJ )
               PERTOL = TEN*EPS1
               SEP = XJ - XJM
               IF( SEP.LT.PERTOL ) &
                  XJ = XJM + PERTOL
            ENDIF
!
            ITS = 0
            NRMCHK = 0
!
!           Get random starting vector.
!
            CALL DLARNV( 2, ISEED, BLKSIZ, WORK( INDRV1+1 ) )
!
!           Copy the matrix T so it won't be destroyed in factorization.
!
            CALL DCOPY( BLKSIZ, D( B1 ), 1, WORK( INDRV4+1 ), 1 )
            CALL DCOPY( BLKSIZ-1, E( B1 ), 1, WORK( INDRV2+2 ), 1 )
            CALL DCOPY( BLKSIZ-1, E( B1 ), 1, WORK( INDRV3+1 ), 1 )
!
!           Compute LU factors with partial pivoting  ( PT = LU )
!
            TOL = ZERO
            CALL DLAGTF( BLKSIZ, WORK( INDRV4+1 ), XJ, WORK( INDRV2+2 ), &
                         WORK( INDRV3+1 ), TOL, WORK( INDRV5+1 ), IWORK, &
                         IINFO )
!
!           Update iteration count.
!
   70       CONTINUE
            ITS = ITS + 1
            IF( ITS.GT.MAXITS ) &
               GO TO 100
!
!           Normalize and scale the righthand side vector Pb.
!
            SCL = BLKSIZ*ONENRM*MAX( EPS, &
                  ABS( WORK( INDRV4+BLKSIZ ) ) ) / &
                  DASUM( BLKSIZ, WORK( INDRV1+1 ), 1 )
            CALL DSCAL( BLKSIZ, SCL, WORK( INDRV1+1 ), 1 )
!
!           Solve the system LU = Pb.
!
            CALL DLAGTS( -1, BLKSIZ, WORK( INDRV4+1 ), WORK( INDRV2+2 ), &
                         WORK( INDRV3+1 ), WORK( INDRV5+1 ), IWORK, &
                         WORK( INDRV1+1 ), TOL, IINFO )
!
!           Reorthogonalize by modified Gram-Schmidt if eigenvalues are
!           close enough.
!
            IF( JBLK.EQ.1 ) &
               GO TO 90
            IF( ABS( XJ-XJM ).GT.ORTOL ) &
               GPIND = J
            IF( GPIND.NE.J ) THEN
               DO 80 I = GPIND, J - 1
                  ZTR = -DDOT( BLKSIZ, WORK( INDRV1+1 ), 1, Z( B1, I ), &
                        1 )
                  CALL DAXPY( BLKSIZ, ZTR, Z( B1, I ), 1, &
                              WORK( INDRV1+1 ), 1 )
   80          CONTINUE
            ENDIF
!
!           Check the infinity norm of the iterate.
!
   90       CONTINUE
            JMAX = IDAMAX( BLKSIZ, WORK( INDRV1+1 ), 1 )
            NRM = ABS( WORK( INDRV1+JMAX ) )
!
!           Continue for additional iterations after norm reaches
!           stopping criterion.
!
            IF( NRM.LT.DTPCRT ) &
               GO TO 70
            NRMCHK = NRMCHK + 1
            IF( NRMCHK.LT.EXTRA+1 ) &
               GO TO 70
!
            GO TO 110
!
!           If stopping criterion was not satisfied, update info and
!           store eigenvector number in array ifail.
!
  100       CONTINUE
            INFO = INFO + 1
            IFAIL( INFO ) = J
!
!           Accept iterate as jth eigenvector.
!
  110       CONTINUE
            SCL = ONE / DNRM2( BLKSIZ, WORK( INDRV1+1 ), 1 )
            JMAX = IDAMAX( BLKSIZ, WORK( INDRV1+1 ), 1 )
            IF( WORK( INDRV1+JMAX ).LT.ZERO ) &
               SCL = -SCL
            CALL DSCAL( BLKSIZ, SCL, WORK( INDRV1+1 ), 1 )
  120       CONTINUE
            DO 130 I = 1, N
               Z( I, J ) = ZERO
  130       CONTINUE
            DO 140 I = 1, BLKSIZ
               Z( B1+I-1, J ) = WORK( INDRV1+I )
  140       CONTINUE
!
!           Save the shift to check eigenvalue spacing at next
!           iteration.
!
            XJM = XJ
!
  150    CONTINUE
  160 CONTINUE
!
      RETURN
!
!     End of DSTEIN
!
      END SUBROUTINE DSTEIN
      
      
      
      
      
      SUBROUTINE DSTEMR( JOBZ, RANGE, N, D, E, VL, VU, IL, IU, &
                         M, W, Z, LDZ, NZC, ISUPPZ, TRYRAC, WORK, LWORK, &
                         IWORK, LIWORK, INFO )
      IMPLICIT NONE
!
!  -- LAPACK computational routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE
      LOGICAL            TRYRAC
      INTEGER            IL, INFO, IU, LDZ, NZC, LIWORK, LWORK, M, N
      DOUBLE PRECISION VL, VU
!     ..
!     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   D( * ), E( * ), W( * ), WORK( * )
      DOUBLE PRECISION   Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  DSTEMR computes selected eigenvalues and, optionally, eigenvectors
!  of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
!  a well defined set of pairwise different real eigenvalues, the corresponding
!  real eigenvectors are pairwise orthogonal.
!
!  The spectrum may be computed either completely or partially by specifying
!  either an interval (VL,VU] or a range of indices IL:IU for the desired
!  eigenvalues.
!
!  Depending on the number of desired eigenvalues, these are computed either
!  by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
!  computed by the use of various suitable L D L^T factorizations near clusters
!  of close eigenvalues (referred to as RRRs, Relatively Robust
!  Representations). An informal sketch of the algorithm follows.
!
!  For each unreduced block (submatrix) of T,
!     (a) Compute T - sigma I  = L D L^T, so that L and D
!         define all the wanted eigenvalues to high relative accuracy.
!         This means that small relative changes in the entries of D and L
!         cause only small relative changes in the eigenvalues and
!         eigenvectors. The standard (unfactored) representation of the
!         tridiagonal matrix T does not have this property in general.
!     (b) Compute the eigenvalues to suitable accuracy.
!         If the eigenvectors are desired, the algorithm attains full
!         accuracy of the computed eigenvalues only right before
!         the corresponding vectors have to be computed, see steps c) and d).
!     (c) For each cluster of close eigenvalues, select a new
!         shift close to the cluster, find a new factorization, and refine
!         the shifted eigenvalues to suitable accuracy.
!     (d) For each eigenvalue with a large enough relative separation compute
!         the corresponding eigenvector by forming a rank revealing twisted
!         factorization. Go back to (c) for any clusters that remain.
!
!  For more details, see:
!  - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
!    to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
!    Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
!  - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
!    Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
!    2004.  Also LAPACK Working Note 154.
!  - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
!    tridiagonal eigenvalue/eigenvector problem",
!    Computer Science Division Technical Report No. UCB/CSD-97-971,
!    UC Berkeley, May 1997.
!
!  Notes:
!  1.DSTEMR works only on machines which follow IEEE-754
!  floating-point standard in their handling of infinities and NaNs.
!  This permits the use of efficient inner loops avoiding a check for
!  zero divisors.
!
!  Arguments
!  =========
!
!  JOBZ    (input) CHARACTER*1
!          = 'N':  Compute eigenvalues only;
!          = 'V':  Compute eigenvalues and eigenvectors.
!
!  RANGE   (input) CHARACTER*1
!          = 'A': all eigenvalues will be found.
!          = 'V': all eigenvalues in the half-open interval (VL,VU]
!                 will be found.
!          = 'I': the IL-th through IU-th eigenvalues will be found.
!
!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the N diagonal elements of the tridiagonal matrix
!          T. On exit, D is overwritten.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the (N-1) subdiagonal elements of the tridiagonal
!          matrix T in elements 1 to N-1 of E. E(N) need not be set on
!          input, but is used internally as workspace.
!          On exit, E is overwritten.
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          If RANGE='V', the lower and upper bounds of the interval to
!          be searched for eigenvalues. VL < VU.
!          Not referenced if RANGE = 'A' or 'I'.
!
!  IL      (input) INTEGER
!  IU      (input) INTEGER
!          If RANGE='I', the indices (in ascending order) of the
!          smallest and largest eigenvalues to be returned.
!          1 <= IL <= IU <= N, if N > 0.
!          Not referenced if RANGE = 'A' or 'V'.
!
!  M       (output) INTEGER
!          The total number of eigenvalues found.  0 <= M <= N.
!          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
!
!  W       (output) DOUBLE PRECISION array, dimension (N)
!          The first M elements contain the selected eigenvalues in
!          ascending order.
!
!  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M) )
!          If JOBZ = 'V', and if INFO = 0, then the first M columns of Z
!          contain the orthonormal eigenvectors of the matrix T
!          corresponding to the selected eigenvalues, with the i-th
!          column of Z holding the eigenvector associated with W(i).
!          If JOBZ = 'N', then Z is not referenced.
!          Note: the user must ensure that at least max(1,M) columns are
!          supplied in the array Z; if RANGE = 'V', the exact value of M
!          is not known in advance and can be computed with a workspace
!          query by setting NZC = -1, see below.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          JOBZ = 'V', then LDZ >= max(1,N).
!
!  NZC     (input) INTEGER
!          The number of eigenvectors to be held in the array Z.
!          If RANGE = 'A', then NZC >= max(1,N).
!          If RANGE = 'V', then NZC >= the number of eigenvalues in (VL,VU].
!          If RANGE = 'I', then NZC >= IU-IL+1.
!          If NZC = -1, then a workspace query is assumed; the
!          routine calculates the number of columns of the array Z that
!          are needed to hold the eigenvectors.
!          This value is returned as the first entry of the Z array, and
!          no error message related to NZC is issued by XERBLA.
!
!  ISUPPZ  (output) INTEGER ARRAY, dimension ( 2*max(1,M) )
!          The support of the eigenvectors in Z, i.e., the indices
!          indicating the nonzero elements in Z. The i-th computed eigenvector
!          is nonzero only in elements ISUPPZ( 2*i-1 ) through
!          ISUPPZ( 2*i ). This is relevant in the case when the matrix
!          is split. ISUPPZ is only accessed when JOBZ is 'V' and N > 0.
!
!  TRYRAC  (input/output) LOGICAL
!          If TRYRAC.EQ..TRUE., indicates that the code should check whether
!          the tridiagonal matrix defines its eigenvalues to high relative
!          accuracy.  If so, the code uses relative-accuracy preserving
!          algorithms that might be (a bit) slower depending on the matrix.
!          If the matrix does not define its eigenvalues to high relative
!          accuracy, the code can uses possibly faster algorithms.
!          If TRYRAC.EQ..FALSE., the code is not required to guarantee
!          relatively accurate eigenvalues and can use the fastest possible
!          techniques.
!          On exit, a .TRUE. TRYRAC will be set to .FALSE. if the matrix
!          does not define its eigenvalues to high relative accuracy.
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
!          On exit, if INFO = 0, WORK(1) returns the optimal
!          (and minimal) LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK. LWORK >= max(1,18*N)
!          if JOBZ = 'V', and LWORK >= max(1,12*N) if JOBZ = 'N'.
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  IWORK   (workspace/output) INTEGER array, dimension (LIWORK)
!          On exit, if INFO = 0, IWORK(1) returns the optimal LIWORK.
!
!  LIWORK  (input) INTEGER
!          The dimension of the array IWORK.  LIWORK >= max(1,10*N)
!          if the eigenvectors are desired, and LIWORK >= max(1,8*N)
!          if only the eigenvalues are to be computed.
!          If LIWORK = -1, then a workspace query is assumed; the
!          routine only calculates the optimal size of the IWORK array,
!          returns this value as the first entry of the IWORK array, and
!          no error message related to LIWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          On exit, INFO
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  if INFO = 1X, internal error in DLARRE,
!                if INFO = 2X, internal error in DLARRV.
!                Here, the digit X = ABS( IINFO ) < 10, where IINFO is
!                the nonzero error code returned by DLARRE or
!                DLARRV, respectively.
!
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Beresford Parlett, University of California, Berkeley, USA
!     Jim Demmel, University of California, Berkeley, USA
!     Inderjit Dhillon, University of Texas, Austin, USA
!     Osni Marques, LBNL/NERSC, USA
!     Christof Voemel, University of California, Berkeley, USA
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, FOUR, MINRGP
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, &
                           FOUR = 4.0D0, &
                           MINRGP = 1.0D-3 )
!     ..
!     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LQUERY, VALEIG, WANTZ, ZQUERY
      INTEGER            I, IBEGIN, IEND, IFIRST, IIL, IINDBL, IINDW, &
                         IINDWK, IINFO, IINSPL, IIU, ILAST, IN, INDD, &
                         INDE2, INDERR, INDGP, INDGRS, INDWRK, ITMP, &
                         ITMP2, J, JBLK, JJ, LIWMIN, LWMIN, NSPLIT, &
                         NZCMIN, OFFSET, WBEGIN, WEND
      DOUBLE PRECISION   BIGNUM, CS, EPS, PIVMIN, R1, R2, RMAX, RMIN, &
                         RTOL1, RTOL2, SAFMIN, SCALE, SMLNUM, SN, &
                         THRESH, TMP, TNRM, WL, WU
!     ..
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, DLANST
      EXTERNAL           LSAME, DLAMCH, DLANST
!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY, DLAE2, DLAEV2, DLARRC, DLARRE, DLARRJ, &
                         DLARRR, DLARRV, DLASRT, DSCAL, DSWAP, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT


!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
!
      LQUERY = ( ( LWORK.EQ.-1 ).OR.( LIWORK.EQ.-1 ) )
      ZQUERY = ( NZC.EQ.-1 )
      TRYRAC = ( INFO.NE.0 )

!     DSTEMR needs WORK of size 6*N, IWORK of size 3*N.
!     In addition, DLARRE needs WORK of size 6*N, IWORK of size 5*N.
!     Furthermore, DLARRV needs WORK of size 12*N, IWORK of size 7*N.
      IF( WANTZ ) THEN
         LWMIN = 18*N
         LIWMIN = 10*N
      ELSE
!        need less workspace if only the eigenvalues are wanted
         LWMIN = 12*N
         LIWMIN = 8*N
      ENDIF

      WL = ZERO
      WU = ZERO
      IIL = 0
      IIU = 0

      IF( VALEIG ) THEN
!        We do not reference VL, VU in the cases RANGE = 'I','A'
!        The interval (WL, WU] contains all the wanted eigenvalues.
!        It is either given by the user or computed in DLARRE.
         WL = VL
         WU = VU
      ELSEIF( INDEIG ) THEN
!        We do not reference IL, IU in the cases RANGE = 'V','A'
         IIL = IL
         IIU = IU
      ENDIF
!
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( N.LT.0 ) THEN
         INFO = -3
      ELSE IF( VALEIG .AND. N.GT.0 .AND. WU.LE.WL ) THEN
         INFO = -7
      ELSE IF( INDEIG .AND. ( IIL.LT.1 .OR. IIL.GT.N ) ) THEN
         INFO = -8
      ELSE IF( INDEIG .AND. ( IIU.LT.IIL .OR. IIU.GT.N ) ) THEN
         INFO = -9
      ELSE IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
         INFO = -13
      ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -17
      ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
         INFO = -19
      ENDIF
!
!     Get machine constants.
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
!
      IF( INFO.EQ.0 ) THEN
         WORK( 1 ) = LWMIN
         IWORK( 1 ) = LIWMIN
!
         IF( WANTZ .AND. ALLEIG ) THEN
            NZCMIN = N
         ELSE IF( WANTZ .AND. VALEIG ) THEN
            CALL DLARRC( 'T', N, VL, VU, D, E, SAFMIN, &
                                  NZCMIN, ITMP, ITMP2, INFO )
         ELSE IF( WANTZ .AND. INDEIG ) THEN
            NZCMIN = IIU-IIL+1
         ELSE
!           WANTZ .EQ. FALSE.
            NZCMIN = 0
         ENDIF
         IF( ZQUERY .AND. INFO.EQ.0 ) THEN
            Z( 1,1 ) = NZCMIN
         ELSE IF( NZC.LT.NZCMIN .AND. .NOT.ZQUERY ) THEN
            INFO = -14
         ENDIF
      ENDIF

      IF( INFO.NE.0 ) THEN
!
         CALL XERBLA( 'DSTEMR', -INFO )
!
         RETURN
      ELSE IF( LQUERY .OR. ZQUERY ) THEN
         RETURN
      ENDIF
!
!     Handle N = 0, 1, and 2 cases immediately
!
      M = 0
      IF( N.EQ.0 ) &
         RETURN
!
      IF( N.EQ.1 ) THEN
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = D( 1 )
         ELSE
            IF( WL.LT.D( 1 ) .AND. WU.GE.D( 1 ) ) THEN
               M = 1
               W( 1 ) = D( 1 )
            ENDIF
         ENDIF
         IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
            Z( 1, 1 ) = ONE
            ISUPPZ(1) = 1
            ISUPPZ(2) = 1
         ENDIF
         RETURN
      ENDIF
!
      IF( N.EQ.2 ) THEN
         IF( .NOT.WANTZ ) THEN
            CALL DLAE2( D(1), E(1), D(2), R1, R2 )
         ELSE IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
            CALL DLAEV2( D(1), E(1), D(2), R1, R2, CS, SN )
         ENDIF
         IF( ALLEIG.OR. &
            (VALEIG.AND.(R2.GT.WL).AND. &
                        (R2.LE.WU)).OR. &
            (INDEIG.AND.(IIL.EQ.1)) ) THEN
            M = M+1
            W( M ) = R2
            IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
               Z( 1, M ) = -SN
               Z( 2, M ) = CS
!              Note: At most one of SN and CS can be zero.
               IF (SN.NE.ZERO) THEN
                  IF (CS.NE.ZERO) THEN
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 2
                  ELSE
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 1
                  ENDIF
               ELSE
                  ISUPPZ(2*M-1) = 2
                  ISUPPZ(2*M) = 2
               ENDIF
            ENDIF
         ENDIF
         IF( ALLEIG.OR. &
            (VALEIG.AND.(R1.GT.WL).AND. &
                        (R1.LE.WU)).OR. &
            (INDEIG.AND.(IIU.EQ.2)) ) THEN
            M = M+1
            W( M ) = R1
            IF( WANTZ.AND.(.NOT.ZQUERY) ) THEN
               Z( 1, M ) = CS
               Z( 2, M ) = SN
!              Note: At most one of SN and CS can be zero.
               IF (SN.NE.ZERO) THEN
                  IF (CS.NE.ZERO) THEN
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 2
                  ELSE
                     ISUPPZ(2*M-1) = 1
                     ISUPPZ(2*M-1) = 1
                  ENDIF
               ELSE
                  ISUPPZ(2*M-1) = 2
                  ISUPPZ(2*M) = 2
               ENDIF
            ENDIF
         ENDIF
         RETURN
      ENDIF

!     Continue with general N

      INDGRS = 1
      INDERR = 2*N + 1
      INDGP = 3*N + 1
      INDD = 4*N + 1
      INDE2 = 5*N + 1
      INDWRK = 6*N + 1
!
      IINSPL = 1
      IINDBL = N + 1
      IINDW = 2*N + 1
      IINDWK = 3*N + 1
!
!     Scale matrix to allowable range, if necessary.
!     The allowable range is related to the PIVMIN parameter; see the
!     comments in DLARRD.  The preference for scaling small values
!     up is heuristic; we expect users' matrices not to be close to the
!     RMAX threshold.
!
      SCALE = ONE
      TNRM = DLANST( 'M', N, D, E )
      IF( TNRM.GT.ZERO .AND. TNRM.LT.RMIN ) THEN
         SCALE = RMIN / TNRM
      ELSE IF( TNRM.GT.RMAX ) THEN
         SCALE = RMAX / TNRM
      ENDIF
      IF( SCALE.NE.ONE ) THEN
         CALL DSCAL( N, SCALE, D, 1 )
         CALL DSCAL( N-1, SCALE, E, 1 )
         TNRM = TNRM*SCALE
         IF( VALEIG ) THEN
!           If eigenvalues in interval have to be found,
!           scale (WL, WU] accordingly
            WL = WL*SCALE
            WU = WU*SCALE
         ENDIF
      ENDIF
!
!     Compute the desired eigenvalues of the tridiagonal after splitting
!     into smaller subblocks if the corresponding off-diagonal elements
!     are small
!     THRESH is the splitting parameter for DLARRE
!     A negative THRESH forces the old splitting criterion based on the
!     size of the off-diagonal. A positive THRESH switches to splitting
!     which preserves relative accuracy.
!
      IF( TRYRAC ) THEN
!        Test whether the matrix warrants the more expensive relative approach.
         CALL DLARRR( N, D, E, IINFO )
      ELSE
!        The user does not care about relative accurately eigenvalues
         IINFO = -1
      ENDIF
!     Set the splitting criterion
      IF (IINFO.EQ.0) THEN
         THRESH = EPS
      ELSE
         THRESH = -EPS
!        relative accuracy is desired but T does not guarantee it
         TRYRAC = .FALSE.
      ENDIF
!
      IF( TRYRAC ) THEN
!        Copy original diagonal, needed to guarantee relative accuracy
         CALL DCOPY(N,D,1,WORK(INDD),1)
      ENDIF
!     Store the squares of the offdiagonal values of T
      DO 5 J = 1, N-1
         WORK( INDE2+J-1 ) = E(J)**2
 5    CONTINUE

!     Set the tolerance parameters for bisection
      IF( .NOT.WANTZ ) THEN
!        DLARRE computes the eigenvalues to full precision.
         RTOL1 = FOUR * EPS
         RTOL2 = FOUR * EPS
      ELSE
!        DLARRE computes the eigenvalues to less than full precision.
!        DLARRV will refine the eigenvalue approximations, and we can
!        need less accurate initial bisection in DLARRE.
!        Note: these settings do only affect the subset case and DLARRE
         RTOL1 = SQRT(EPS)
         RTOL2 = MAX( SQRT(EPS)*5.0D-3, FOUR * EPS )
      ENDIF
      CALL DLARRE( RANGE, N, WL, WU, IIL, IIU, D, E, &
                   WORK(INDE2), RTOL1, RTOL2, THRESH, NSPLIT, &
                   IWORK( IINSPL ), M, W, WORK( INDERR ), &
                   WORK( INDGP ), IWORK( IINDBL ), &
                   IWORK( IINDW ), WORK( INDGRS ), PIVMIN, &
                   WORK( INDWRK ), IWORK( IINDWK ), IINFO )
      IF( IINFO.NE.0 ) THEN
         INFO = 10 + ABS( IINFO )
         RETURN
      ENDIF
!     Note that if RANGE .NE. 'V', DLARRE computes bounds on the desired
!     part of the spectrum. All desired eigenvalues are contained in
!     (WL,WU]


      IF( WANTZ ) THEN
!
!        Compute the desired eigenvectors corresponding to the computed
!        eigenvalues
!
         CALL DLARRV( N, WL, WU, D, E, &
                      PIVMIN, IWORK( IINSPL ), M, &
                      1, M, MINRGP, RTOL1, RTOL2, &
                      W, WORK( INDERR ), WORK( INDGP ), IWORK( IINDBL ), &
                      IWORK( IINDW ), WORK( INDGRS ), Z, LDZ, &
                      ISUPPZ, WORK( INDWRK ), IWORK( IINDWK ), IINFO )
         IF( IINFO.NE.0 ) THEN
            INFO = 20 + ABS( IINFO )
            RETURN
         ENDIF
      ELSE
!        DLARRE computes eigenvalues of the (shifted) root representation
!        DLARRV returns the eigenvalues of the unshifted matrix.
!        However, if the eigenvectors are not desired by the user, we need
!        to apply the corresponding shifts from DLARRE to obtain the
!        eigenvalues of the original matrix.
         DO 20 J = 1, M
            ITMP = IWORK( IINDBL+J-1 )
            W( J ) = W( J ) + E( IWORK( IINSPL+ITMP-1 ) )
 20      CONTINUE
      ENDIF
!

      IF ( TRYRAC ) THEN
!        Refine computed eigenvalues so that they are relatively accurate
!        with respect to the original matrix T.
         IBEGIN = 1
         WBEGIN = 1
         DO 39  JBLK = 1, IWORK( IINDBL+M-1 )
            IEND = IWORK( IINSPL+JBLK-1 )
            IN = IEND - IBEGIN + 1
            WEND = WBEGIN - 1
!           check if any eigenvalues have to be refined in this block
 36         CONTINUE
            IF( WEND.LT.M ) THEN
               IF( IWORK( IINDBL+WEND ).EQ.JBLK ) THEN
                  WEND = WEND + 1
                  GO TO 36
               ENDIF
            ENDIF
            IF( WEND.LT.WBEGIN ) THEN
               IBEGIN = IEND + 1
               GO TO 39
            ENDIF

            OFFSET = IWORK(IINDW+WBEGIN-1)-1
            IFIRST = IWORK(IINDW+WBEGIN-1)
            ILAST = IWORK(IINDW+WEND-1)
            RTOL2 = FOUR * EPS
            CALL DLARRJ( IN, &
                         WORK(INDD+IBEGIN-1), WORK(INDE2+IBEGIN-1), &
                         IFIRST, ILAST, RTOL2, OFFSET, W(WBEGIN), &
                         WORK( INDERR+WBEGIN-1 ), &
                         WORK( INDWRK ), IWORK( IINDWK ), PIVMIN, &
                         TNRM, IINFO )
            IBEGIN = IEND + 1
            WBEGIN = WEND + 1
 39      CONTINUE
      ENDIF
!
!     If matrix was scaled, then rescale eigenvalues appropriately.
!
      IF( SCALE.NE.ONE ) THEN
         CALL DSCAL( M, ONE / SCALE, W, 1 )
      ENDIF
!
!     If eigenvalues are not in increasing order, then sort them,
!     possibly along with eigenvectors.
!
      IF( NSPLIT.GT.1 ) THEN
         IF( .NOT. WANTZ ) THEN
            CALL DLASRT( 'I', M, W, IINFO )
            IF( IINFO.NE.0 ) THEN
               INFO = 3
               RETURN
            ENDIF
         ELSE
            DO 60 J = 1, M - 1
               I = 0
               TMP = W( J )
               DO 50 JJ = J + 1, M
                  IF( W( JJ ).LT.TMP ) THEN
                     I = JJ
                     TMP = W( JJ )
                  ENDIF
 50            CONTINUE
               IF( I.NE.0 ) THEN
                  W( I ) = W( J )
                  W( J ) = TMP
                  IF( WANTZ ) THEN
                     CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
                     ITMP = ISUPPZ( 2*I-1 )
                     ISUPPZ( 2*I-1 ) = ISUPPZ( 2*J-1 )
                     ISUPPZ( 2*J-1 ) = ITMP
                     ITMP = ISUPPZ( 2*I )
                     ISUPPZ( 2*I ) = ISUPPZ( 2*J )
                     ISUPPZ( 2*J ) = ITMP
                  ENDIF
               ENDIF
 60         CONTINUE
         ENDIF
      ENDIF
!
!
      WORK( 1 ) = LWMIN
      IWORK( 1 ) = LIWMIN
      RETURN
!
!     End of DSTEMR
!
      END SUBROUTINE DSTEMR
      
      
      
      
      
      SUBROUTINE DSTERF( N, D, E, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      INTEGER            INFO, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   D( * ), E( * )
!     ..
!
!  Purpose
!  =======
!
!  DSTERF computes all eigenvalues of a symmetric tridiagonal matrix
!  using the Pal-Walker-Kahan variant of the QL or QR algorithm.
!
!  Arguments
!  =========
!
!  N       (input) INTEGER
!          The order of the matrix.  N >= 0.
!
!  D       (input/output) DOUBLE PRECISION array, dimension (N)
!          On entry, the n diagonal elements of the tridiagonal matrix.
!          On exit, if INFO = 0, the eigenvalues in ascending order.
!
!  E       (input/output) DOUBLE PRECISION array, dimension (N-1)
!          On entry, the (n-1) subdiagonal elements of the tridiagonal
!          matrix.
!          On exit, E has been destroyed.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  the algorithm failed to find all of the eigenvalues in
!                a total of 30*N iterations; if INFO = i, then i
!                elements of E have not converged to zero.
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO, THREE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0, &
                         THREE = 3.0D0 )
      INTEGER            MAXIT
      PARAMETER          ( MAXIT = 30 )
!     ..
!     .. Local Scalars ..
      INTEGER            I, ISCALE, JTOT, L, L1, LEND, LENDSV, LSV, M, &
                         NMAXIT
      DOUBLE PRECISION   ALPHA, ANORM, BB, C, EPS, EPS2, GAMMA, OLDC, &
                         OLDGAM, P, R, RT1, RT2, RTE, S, SAFMAX, SAFMIN, &
                         SIGMA, SSFMAX, SSFMIN
!     ..
!     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DLANST, DLAPY2
      EXTERNAL           DLAMCH, DLANST, DLAPY2
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLAE2, DLASCL, DLASRT, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          ABS, SIGN, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      INFO = 0
!
!     Quick return if possible
!
      IF( N.LT.0 ) THEN
         INFO = -1
         CALL XERBLA( 'DSTERF', -INFO )
         RETURN
      ENDIF
      IF( N.LE.1 ) &
         RETURN
!
!     Determine the unit roundoff for this environment.
!
      EPS = DLAMCH( 'E' )
      EPS2 = EPS**2
      SAFMIN = DLAMCH( 'S' )
      SAFMAX = ONE / SAFMIN
      SSFMAX = SQRT( SAFMAX ) / THREE
      SSFMIN = SQRT( SAFMIN ) / EPS2
!
!     Compute the eigenvalues of the tridiagonal matrix.
!
      NMAXIT = N*MAXIT
      SIGMA = ZERO
      JTOT = 0
!
!     Determine where the matrix splits and choose QL or QR iteration
!     for each block, according to whether top or bottom diagonal
!     element is smaller.
!
      L1 = 1
!
   10 CONTINUE
      IF( L1.GT.N ) &
         GO TO 170
      IF( L1.GT.1 ) &
         E( L1-1 ) = ZERO
      DO 20 M = L1, N - 1
         IF( ABS( E( M ) ).LE.( SQRT( ABS( D( M ) ) )*SQRT( ABS( D( M+ &
             1 ) ) ) )*EPS ) THEN
            E( M ) = ZERO
            GO TO 30
         ENDIF
   20 CONTINUE
      M = N
!
   30 CONTINUE
      L = L1
      LSV = L
      LEND = M
      LENDSV = LEND
      L1 = M + 1
      IF( LEND.EQ.L ) &
         GO TO 10
!
!     Scale submatrix in rows and columns L to LEND
!
      ANORM = DLANST( 'I', LEND-L+1, D( L ), E( L ) )
      ISCALE = 0
      IF( ANORM.GT.SSFMAX ) THEN
         ISCALE = 1
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L+1, 1, D( L ), N, &
                      INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMAX, LEND-L, 1, E( L ), N, &
                      INFO )
      ELSE IF( ANORM.LT.SSFMIN ) THEN
         ISCALE = 2
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L+1, 1, D( L ), N, &
                      INFO )
         CALL DLASCL( 'G', 0, 0, ANORM, SSFMIN, LEND-L, 1, E( L ), N, &
                      INFO )
      ENDIF
!
      DO 40 I = L, LEND - 1
         E( I ) = E( I )**2
   40 CONTINUE
!
!     Choose between QL and QR iteration
!
      IF( ABS( D( LEND ) ).LT.ABS( D( L ) ) ) THEN
         LEND = LSV
         L = LENDSV
      ENDIF
!
      IF( LEND.GE.L ) THEN
!
!        QL Iteration
!
!        Look for small subdiagonal element.
!
   50    CONTINUE
         IF( L.NE.LEND ) THEN
            DO 60 M = L, LEND - 1
               IF( ABS( E( M ) ).LE.EPS2*ABS( D( M )*D( M+1 ) ) ) &
                  GO TO 70
   60       CONTINUE
         ENDIF
         M = LEND
!
   70    CONTINUE
         IF( M.LT.LEND ) &
            E( M ) = ZERO
         P = D( L )
         IF( M.EQ.L ) &
            GO TO 90
!
!        If remaining matrix is 2 by 2, use DLAE2 to compute its
!        eigenvalues.
!
         IF( M.EQ.L+1 ) THEN
            RTE = SQRT( E( L ) )
            CALL DLAE2( D( L ), RTE, D( L+1 ), RT1, RT2 )
            D( L ) = RT1
            D( L+1 ) = RT2
            E( L ) = ZERO
            L = L + 2
            IF( L.LE.LEND ) &
               GO TO 50
            GO TO 150
         ENDIF
!
         IF( JTOT.EQ.NMAXIT ) &
            GO TO 150
         JTOT = JTOT + 1
!
!        Form shift.
!
         RTE = SQRT( E( L ) )
         SIGMA = ( D( L+1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
!
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
!
!        Inner loop
!
         DO 80 I = M - 1, L, -1
            BB = E( I )
            R = P + BB
            IF( I.NE.M-1 ) &
               E( I+1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I+1 ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            ENDIF
   80    CONTINUE
!
         E( L ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 50
!
!        Eigenvalue found.
!
   90    CONTINUE
         D( L ) = P
!
         L = L + 1
         IF( L.LE.LEND ) &
            GO TO 50
         GO TO 150
!
      ELSE
!
!        QR Iteration
!
!        Look for small superdiagonal element.
!
  100    CONTINUE
         DO 110 M = L, LEND + 1, -1
            IF( ABS( E( M-1 ) ).LE.EPS2*ABS( D( M )*D( M-1 ) ) ) &
               GO TO 120
  110    CONTINUE
         M = LEND
!
  120    CONTINUE
         IF( M.GT.LEND ) &
            E( M-1 ) = ZERO
         P = D( L )
         IF( M.EQ.L ) &
            GO TO 140
!
!        If remaining matrix is 2 by 2, use DLAE2 to compute its
!        eigenvalues.
!
         IF( M.EQ.L-1 ) THEN
            RTE = SQRT( E( L-1 ) )
            CALL DLAE2( D( L ), RTE, D( L-1 ), RT1, RT2 )
            D( L ) = RT1
            D( L-1 ) = RT2
            E( L-1 ) = ZERO
            L = L - 2
            IF( L.GE.LEND ) &
               GO TO 100
            GO TO 150
         ENDIF
!
         IF( JTOT.EQ.NMAXIT ) &
            GO TO 150
         JTOT = JTOT + 1
!
!        Form shift.
!
         RTE = SQRT( E( L-1 ) )
         SIGMA = ( D( L-1 )-P ) / ( TWO*RTE )
         R = DLAPY2( SIGMA, ONE )
         SIGMA = P - ( RTE / ( SIGMA+SIGN( R, SIGMA ) ) )
!
         C = ONE
         S = ZERO
         GAMMA = D( M ) - SIGMA
         P = GAMMA*GAMMA
!
!        Inner loop
!
         DO 130 I = M, L - 1
            BB = E( I )
            R = P + BB
            IF( I.NE.M ) &
               E( I-1 ) = S*R
            OLDC = C
            C = P / R
            S = BB / R
            OLDGAM = GAMMA
            ALPHA = D( I+1 )
            GAMMA = C*( ALPHA-SIGMA ) - S*OLDGAM
            D( I ) = OLDGAM + ( ALPHA-GAMMA )
            IF( C.NE.ZERO ) THEN
               P = ( GAMMA*GAMMA ) / C
            ELSE
               P = OLDC*BB
            ENDIF
  130    CONTINUE
!
         E( L-1 ) = S*P
         D( L ) = SIGMA + GAMMA
         GO TO 100
!
!        Eigenvalue found.
!
  140    CONTINUE
         D( L ) = P
!
         L = L - 1
         IF( L.GE.LEND ) &
            GO TO 100
         GO TO 150
!
      ENDIF
!
!     Undo scaling if necessary
!
  150 CONTINUE
      IF( ISCALE.EQ.1 ) &
         CALL DLASCL( 'G', 0, 0, SSFMAX, ANORM, LENDSV-LSV+1, 1, &
                      D( LSV ), N, INFO )
      IF( ISCALE.EQ.2 ) &
         CALL DLASCL( 'G', 0, 0, SSFMIN, ANORM, LENDSV-LSV+1, 1, &
                      D( LSV ), N, INFO )
!
!     Check for no convergence to an eigenvalue after a total
!     of N*MAXIT iterations.
!
      IF( JTOT.LT.NMAXIT ) &
         GO TO 10
      DO 160 I = 1, N - 1
         IF( E( I ).NE.ZERO ) &
            INFO = INFO + 1
  160 CONTINUE
      GO TO 180
!
!     Sort eigenvalues in increasing order.
!
  170 CONTINUE
      CALL DLASRT( 'I', N, D, INFO )
!
  180 CONTINUE
      RETURN
!
!     End of DSTERF
!
      END SUBROUTINE DSTERF
      
      
      
      
      
      SUBROUTINE DSYEVR_fast( JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, &
                         ABSTOL, M, W, Z, LDZ, ISUPPZ, WORK, LWORK, &
                         IWORK, LIWORK, INFO )
!
!  -- LAPACK driver routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          JOBZ, RANGE, UPLO
      INTEGER            IL, INFO, IU, LDA, LDZ, LIWORK, LWORK, M, N
      DOUBLE PRECISION   ABSTOL, VL, VU
!     ..
!     .. Array Arguments ..
      INTEGER            ISUPPZ( * ), IWORK( * )
      DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * ), Z( LDZ, * )
!     ..
!
!  Purpose
!  =======
!
!  DSYEVR computes selected eigenvalues and, optionally, eigenvectors
!  of a real symmetric matrix A.  Eigenvalues and eigenvectors can be
!  selected by specifying either a range of values or a range of
!  indices for the desired eigenvalues.
!
!  DSYEVR first reduces the matrix A to tridiagonal form T with a call
!  to DSYTRD.  Then, whenever possible, DSYEVR calls DSTEMR to compute
!  the eigenspectrum using Relatively Robust Representations.  DSTEMR
!  computes eigenvalues by the dqds algorithm, while orthogonal
!  eigenvectors are computed from various "good" L D L^T representations
!  (also known as Relatively Robust Representations). Gram-Schmidt
!  orthogonalization is avoided as far as possible. More specifically,
!  the various steps of the algorithm are as follows.
!
!  For each unreduced block (submatrix) of T,
!     (a) Compute T - sigma I  = L D L^T, so that L and D
!         define all the wanted eigenvalues to high relative accuracy.
!         This means that small relative changes in the entries of D and L
!         cause only small relative changes in the eigenvalues and
!         eigenvectors. The standard (unfactored) representation of the
!         tridiagonal matrix T does not have this property in general.
!     (b) Compute the eigenvalues to suitable accuracy.
!         If the eigenvectors are desired, the algorithm attains full
!         accuracy of the computed eigenvalues only right before
!         the corresponding vectors have to be computed, see steps c) and d).
!     (c) For each cluster of close eigenvalues, select a new
!         shift close to the cluster, find a new factorization, and refine
!         the shifted eigenvalues to suitable accuracy.
!     (d) For each eigenvalue with a large enough relative separation compute
!         the corresponding eigenvector by forming a rank revealing twisted
!         factorization. Go back to (c) for any clusters that remain.
!
!  The desired accuracy of the output can be specified by the input
!  parameter ABSTOL.
!
!  For more details, see DSTEMR's documentation and:
!  - Inderjit S. Dhillon and Beresford N. Parlett: "Multiple representations
!    to compute orthogonal eigenvectors of symmetric tridiagonal matrices,"
!    Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
!  - Inderjit Dhillon and Beresford Parlett: "Orthogonal Eigenvectors and
!    Relative Gaps," SIAM Journal on Matrix Analysis and Applications, Vol. 25,
!    2004.  Also LAPACK Working Note 154.
!  - Inderjit Dhillon: "A new O(n^2) algorithm for the symmetric
!    tridiagonal eigenvalue/eigenvector problem",
!    Computer Science Division Technical Report No. UCB/CSD-97-971,
!    UC Berkeley, May 1997.
!
!
!  Note 1 : DSYEVR calls DSTEMR when the full spectrum is requested
!  on machines which conform to the ieee-754 floating point standard.
!  DSYEVR calls DSTEBZ and SSTEIN on non-ieee machines and
!  when partial spectrum requests are made.
!
!  Normal execution of DSTEMR may create NaNs and infinities and
!  hence may abort due to a floating point exception in environments
!  which do not handle NaNs and infinities in the ieee standard default
!  manner.
!
!  Arguments
!  =========
!
!  JOBZ    (input) CHARACTER*1
!          = 'N':  Compute eigenvalues only;
!          = 'V':  Compute eigenvalues and eigenvectors.
!
!  RANGE   (input) CHARACTER*1
!          = 'A': all eigenvalues will be found.
!          = 'V': all eigenvalues in the half-open interval (VL,VU]
!                 will be found.
!          = 'I': the IL-th through IU-th eigenvalues will be found.
!********* For RANGE = 'V' or 'I' and IU - IL < N - 1, DSTEBZ and
!********* DSTEIN are called
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  Upper triangle of A is stored;
!          = 'L':  Lower triangle of A is stored.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA, N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the
!          leading N-by-N upper triangular part of A contains the
!          upper triangular part of the matrix A.  If UPLO = 'L',
!          the leading N-by-N lower triangular part of A contains
!          the lower triangular part of the matrix A.
!          On exit, the lower triangle (if UPLO='L') or the upper
!          triangle (if UPLO='U') of A, including the diagonal, is
!          destroyed.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  VL      (input) DOUBLE PRECISION
!  VU      (input) DOUBLE PRECISION
!          If RANGE='V', the lower and upper bounds of the interval to
!          be searched for eigenvalues. VL < VU.
!          Not referenced if RANGE = 'A' or 'I'.
!
!  IL      (input) INTEGER
!  IU      (input) INTEGER
!          If RANGE='I', the indices (in ascending order) of the
!          smallest and largest eigenvalues to be returned.
!          1 <= IL <= IU <= N, if N > 0; IL = 1 and IU = 0 if N = 0.
!          Not referenced if RANGE = 'A' or 'V'.
!
!  ABSTOL  (input) DOUBLE PRECISION
!          The absolute error tolerance for the eigenvalues.
!          An approximate eigenvalue is accepted as converged
!          when it is determined to lie in an interval [a,b]
!          of width less than or equal to
!
!                  ABSTOL + EPS *   max( |a|,|b| ) ,
!
!          where EPS is the machine precision.  If ABSTOL is less than
!          or equal to zero, then  EPS*|T|  will be used in its place,
!          where |T| is the 1-norm of the tridiagonal matrix obtained
!          by reducing A to tridiagonal form.
!
!          See "Computing Small Singular Values of Bidiagonal Matrices
!          with Guaranteed High Relative Accuracy," by Demmel and
!          Kahan, LAPACK Working Note #3.
!
!          If high relative accuracy is important, set ABSTOL to
!          DLAMCH( 'Safe minimum' ).  Doing so will guarantee that
!          eigenvalues are computed to high relative accuracy when
!          possible in future releases.  The current code does not
!          make any guarantees about high relative accuracy, but
!          future releases will. See J. Barlow and J. Demmel,
!          "Computing Accurate Eigensystems of Scaled Diagonally
!          Dominant Matrices", LAPACK Working Note #7, for a discussion
!          of which matrices define their eigenvalues to high relative
!          accuracy.
!
!  M       (output) INTEGER
!          The total number of eigenvalues found.  0 <= M <= N.
!          If RANGE = 'A', M = N, and if RANGE = 'I', M = IU-IL+1.
!
!  W       (output) DOUBLE PRECISION array, dimension (N)
!          The first M elements contain the selected eigenvalues in
!          ascending order.
!
!  Z       (output) DOUBLE PRECISION array, dimension (LDZ, max(1,M))
!          If JOBZ = 'V', then if INFO = 0, the first M columns of Z
!          contain the orthonormal eigenvectors of the matrix A
!          corresponding to the selected eigenvalues, with the i-th
!          column of Z holding the eigenvector associated with W(i).
!          If JOBZ = 'N', then Z is not referenced.
!          Note: the user must ensure that at least max(1,M) columns are
!          supplied in the array Z; if RANGE = 'V', the exact value of M
!          is not known in advance and an upper bound must be used.
!          Supplying N columns is always safe.
!
!  LDZ     (input) INTEGER
!          The leading dimension of the array Z.  LDZ >= 1, and if
!          JOBZ = 'V', LDZ >= max(1,N).
!
!  ISUPPZ  (output) INTEGER array, dimension ( 2*max(1,M) )
!          The support of the eigenvectors in Z, i.e., the indices
!          indicating the nonzero elements in Z. The i-th eigenvector
!          is nonzero only in elements ISUPPZ( 2*i-1 ) through
!          ISUPPZ( 2*i ).
!********* Implemented only for RANGE = 'A' or 'I' and IU - IL = N - 1
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= max(1,26*N).
!          For optimal efficiency, LWORK >= (NB+6)*N,
!          where NB is the max of the blocksize for DSYTRD and DORMTR
!          returned by ILAENV.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  IWORK   (workspace/output) INTEGER array, dimension (MAX(1,LIWORK))
!          On exit, if INFO = 0, IWORK(1) returns the optimal LWORK.
!
!  LIWORK  (input) INTEGER
!          The dimension of the array IWORK.  LIWORK >= max(1,10*N).
!
!          If LIWORK = -1, then a workspace query is assumed; the
!          routine only calculates the optimal size of the IWORK array,
!          returns this value as the first entry of the IWORK array, and
!          no error message related to LIWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!          > 0:  Internal error
!
!  Further Details
!  ===============
!
!  Based on contributions by
!     Inderjit Dhillon, IBM Almaden, USA
!     Osni Marques, LBNL/NERSC, USA
!     Ken Stanley, Computer Science Division, University of
!       California at Berkeley, USA
!     Jason Riedy, Computer Science Division, University of
!       California at Berkeley, USA
!
! =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE, TWO
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0, TWO = 0.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            ALLEIG, INDEIG, LOWER, LQUERY, VALEIG, WANTZ, &
                         TRYRAC
      CHARACTER          ORDER
      INTEGER            I, IEEEOK, IINFO, IMAX, INDD, INDDD, INDE, &
                         INDEE, INDIBL, INDIFL, INDISP, INDIWO, INDTAU, &
                         INDWK, INDWKN, ISCALE, J, JJ, LIWMIN, &
                         LLWORK, LLWRKN, LWKOPT, LWMIN, NB, NSPLIT
      DOUBLE PRECISION   ABSTLL, ANRM, BIGNUM, EPS, RMAX, RMIN, SAFMIN, &
                         SIGMA, SMLNUM, TMP1, VLL, VUU
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      DOUBLE PRECISION   DLAMCH, DLANSY
      EXTERNAL           LSAME, ILAENV, DLAMCH, DLANSY
!     ..
!     .. External Subroutines ..
      EXTERNAL           DCOPY, DORMTR, DSCAL, DSTEBZ, DSTEMR, DSTEIN, &
                         DSTERF, DSWAP, DSYTRD, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN, SQRT
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters.
!
      IEEEOK = ILAENV( 10, 'DSYEVR', 'N', 1, 2, 3, 4 )
!
      LOWER = LSAME( UPLO, 'L' )
      WANTZ = LSAME( JOBZ, 'V' )
      ALLEIG = LSAME( RANGE, 'A' )
      VALEIG = LSAME( RANGE, 'V' )
      INDEIG = LSAME( RANGE, 'I' )
!
      LQUERY = ( ( LWORK.EQ.-1 ) .OR. ( LIWORK.EQ.-1 ) )
!
      LWMIN = MAX( 1, 26*N )
      LIWMIN = MAX( 1, 10*N )
!
      INFO = 0
      IF( .NOT.( WANTZ .OR. LSAME( JOBZ, 'N' ) ) ) THEN
         INFO = -1
      ELSE IF( .NOT.( ALLEIG .OR. VALEIG .OR. INDEIG ) ) THEN
         INFO = -2
      ELSE IF( .NOT.( LOWER .OR. LSAME( UPLO, 'U' ) ) ) THEN
         INFO = -3
      ELSE IF( N.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -6
      ELSE
         IF( VALEIG ) THEN
            IF( N.GT.0 .AND. VU.LE.VL ) &
               INFO = -8
         ELSE IF( INDEIG ) THEN
            IF( IL.LT.1 .OR. IL.GT.MAX( 1, N ) ) THEN
               INFO = -9
            ELSE IF( IU.LT.MIN( N, IL ) .OR. IU.GT.N ) THEN
               INFO = -10
            ENDIF
         ENDIF
      ENDIF
      IF( INFO.EQ.0 ) THEN
         IF( LDZ.LT.1 .OR. ( WANTZ .AND. LDZ.LT.N ) ) THEN
            INFO = -15
         ELSE IF( LWORK.LT.LWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -18
         ELSE IF( LIWORK.LT.LIWMIN .AND. .NOT.LQUERY ) THEN
            INFO = -20
         ENDIF
      ENDIF
!
      IF( INFO.EQ.0 ) THEN
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         NB = MAX( NB, ILAENV( 1, 'DORMTR', UPLO, N, -1, -1, -1 ) )
         LWKOPT = MAX( ( NB+1 )*N, LWMIN )
         WORK( 1 ) = LWKOPT
         IWORK( 1 ) = LIWMIN
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYEVR', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      ENDIF
!
!     Quick return if possible
!
      M = 0
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      ENDIF
!
      IF( N.EQ.1 ) THEN
         WORK( 1 ) = 7
         IF( ALLEIG .OR. INDEIG ) THEN
            M = 1
            W( 1 ) = A( 1, 1 )
         ELSE
            IF( VL.LT.A( 1, 1 ) .AND. VU.GE.A( 1, 1 ) ) THEN
               M = 1
               W( 1 ) = A( 1, 1 )
            ENDIF
         ENDIF
         IF( WANTZ ) &
            Z( 1, 1 ) = ONE
         RETURN
      ENDIF
!
!     Get machine constants.
!
      SAFMIN = DLAMCH( 'Safe minimum' )
      EPS = DLAMCH( 'Precision' )
      SMLNUM = SAFMIN / EPS
      BIGNUM = ONE / SMLNUM
      RMIN = SQRT( SMLNUM )
      RMAX = MIN( SQRT( BIGNUM ), ONE / SQRT( SQRT( SAFMIN ) ) )
!
!     Scale matrix to allowable range, if necessary.
!
      ISCALE = 0
      ABSTLL = ABSTOL
      VLL = VL
      VUU = VU
      ANRM = DLANSY( 'M', UPLO, N, A, LDA, WORK )
      IF( ANRM.GT.ZERO .AND. ANRM.LT.RMIN ) THEN
         ISCALE = 1
         SIGMA = RMIN / ANRM
      ELSE IF( ANRM.GT.RMAX ) THEN
         ISCALE = 1
         SIGMA = RMAX / ANRM
      ENDIF
      IF( ISCALE.EQ.1 ) THEN
         IF( LOWER ) THEN
            DO 10 J = 1, N
               CALL DSCAL( N-J+1, SIGMA, A( J, J ), 1 )
   10       CONTINUE
         ELSE
            DO 20 J = 1, N
               CALL DSCAL( J, SIGMA, A( 1, J ), 1 )
   20       CONTINUE
         ENDIF
         IF( ABSTOL.GT.0 ) &
            ABSTLL = ABSTOL*SIGMA
         IF( VALEIG ) THEN
            VLL = VL*SIGMA
            VUU = VU*SIGMA
         ENDIF
      ENDIF

!     Initialize indices into workspaces.  Note: The IWORK indices are
!     used only if DSTERF or DSTEMR fail.

!     WORK(INDTAU:INDTAU+N-1) stores the scalar factors of the
!     elementary reflectors used in DSYTRD.
      INDTAU = 1
!     WORK(INDD:INDD+N-1) stores the tridiagonal's diagonal entries.
      INDD = INDTAU + N
!     WORK(INDE:INDE+N-1) stores the off-diagonal entries of the
!     tridiagonal matrix from DSYTRD.
      INDE = INDD + N
!     WORK(INDDD:INDDD+N-1) is a copy of the diagonal entries over
!     -written by DSTEMR (the DSTERF path copies the diagonal to W).
      INDDD = INDE + N
!     WORK(INDEE:INDEE+N-1) is a copy of the off-diagonal entries over
!     -written while computing the eigenvalues in DSTERF and DSTEMR.
      INDEE = INDDD + N
!     INDWK is the starting offset of the left-over workspace, and
!     LLWORK is the remaining workspace size.
      INDWK = INDEE + N
      LLWORK = LWORK - INDWK + 1

!     IWORK(INDIBL:INDIBL+M-1) corresponds to IBLOCK in DSTEBZ and
!     stores the block indices of each of the M<=N eigenvalues.
      INDIBL = 1
!     IWORK(INDISP:INDISP+NSPLIT-1) corresponds to ISPLIT in DSTEBZ and
!     stores the starting and finishing indices of each block.
      INDISP = INDIBL + N
!     IWORK(INDIFL:INDIFL+N-1) stores the indices of eigenvectors
!     that corresponding to eigenvectors that fail to converge in
!     DSTEIN.  This information is discarded; if any fail, the driver
!     returns INFO > 0.
      INDIFL = INDISP + N
!     INDIWO is the offset of the remaining integer workspace.
      INDIWO = INDISP + N

!
!     Call DSYTRD to reduce symmetric matrix to tridiagonal form.
!
      CALL DSYTRD( UPLO, N, A, LDA, WORK( INDD ), WORK( INDE ), &
                   WORK( INDTAU ), WORK( INDWK ), LLWORK, IINFO )
!
!     If all eigenvalues are desired
!     then call DSTERF or DSTEMR and DORMTR.
!
      IF( ( ALLEIG .OR. ( INDEIG .AND. IL.EQ.1 .AND. IU.EQ.N ) ) .AND. &
          IEEEOK.EQ.1 ) THEN
         IF( .NOT.WANTZ ) THEN
            CALL DCOPY( N, WORK( INDD ), 1, W, 1 )
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DSTERF( N, W, WORK( INDEE ), INFO )
         ELSE
            CALL DCOPY( N-1, WORK( INDE ), 1, WORK( INDEE ), 1 )
            CALL DCOPY( N, WORK( INDD ), 1, WORK( INDDD ), 1 )
!
            IF (ABSTOL .LE. TWO*N*EPS) THEN
               TRYRAC = .TRUE.
            ELSE
               TRYRAC = .FALSE.
            ENDIF
	    !FF: don't want exquisite precision
	    TRYRAC = .FALSE.
            CALL DSTEMR( JOBZ, 'A', N, WORK( INDDD ), WORK( INDEE ), &
                         VL, VU, IL, IU, M, W, Z, LDZ, N, ISUPPZ, &
                         TRYRAC, WORK( INDWK ), LWORK, IWORK, LIWORK, &
                         INFO )
!
!
!
!        Apply orthogonal matrix used in reduction to tridiagonal
!        form to eigenvectors returned by DSTEIN.
!
            IF( WANTZ .AND. INFO.EQ.0 ) THEN
               INDWKN = INDE
               LLWRKN = LWORK - INDWKN + 1
               CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, &
                            WORK( INDTAU ), Z, LDZ, WORK( INDWKN ), &
                            LLWRKN, IINFO )
            ENDIF
         ENDIF
!
!
         IF( INFO.EQ.0 ) THEN
!           Everything worked.  Skip DSTEBZ/DSTEIN.  IWORK(:) are
!           undefined.
            M = N
            GO TO 30
         ENDIF
         INFO = 0
      ENDIF
!
!     Otherwise, call DSTEBZ and, if eigenvectors are desired, DSTEIN.
!     Also call DSTEBZ and DSTEIN if DSTEMR fails.
!
      IF( WANTZ ) THEN
         ORDER = 'B'
      ELSE
         ORDER = 'E'
      ENDIF

      CALL DSTEBZ( RANGE, ORDER, N, VLL, VUU, IL, IU, ABSTLL, &
                   WORK( INDD ), WORK( INDE ), M, NSPLIT, W, &
                   IWORK( INDIBL ), IWORK( INDISP ), WORK( INDWK ), &
                   IWORK( INDIWO ), INFO )
!
      IF( WANTZ ) THEN
         CALL DSTEIN( N, WORK( INDD ), WORK( INDE ), M, W, &
                      IWORK( INDIBL ), IWORK( INDISP ), Z, LDZ, &
                      WORK( INDWK ), IWORK( INDIWO ), IWORK( INDIFL ), &
                      INFO )
!
!        Apply orthogonal matrix used in reduction to tridiagonal
!        form to eigenvectors returned by DSTEIN.
!
         INDWKN = INDE
         LLWRKN = LWORK - INDWKN + 1
         CALL DORMTR( 'L', UPLO, 'N', N, M, A, LDA, WORK( INDTAU ), Z, &
                      LDZ, WORK( INDWKN ), LLWRKN, IINFO )
      ENDIF
!
!     If matrix was scaled, then rescale eigenvalues appropriately.
!
!  Jump here if DSTEMR/DSTEIN succeeded.
   30 CONTINUE
      IF( ISCALE.EQ.1 ) THEN
         IF( INFO.EQ.0 ) THEN
            IMAX = M
         ELSE
            IMAX = INFO - 1
         ENDIF
         CALL DSCAL( IMAX, ONE / SIGMA, W, 1 )
      ENDIF
!
!     If eigenvalues are not in order, then sort them, along with
!     eigenvectors.  Note: We do not sort the IFAIL portion of IWORK.
!     It may not be initialized (if DSTEMR/DSTEIN succeeded), and we do
!     not return this detailed information to the user.
!
      IF( WANTZ ) THEN
         DO 50 J = 1, M - 1
            I = 0
            TMP1 = W( J )
            DO 40 JJ = J + 1, M
               IF( W( JJ ).LT.TMP1 ) THEN
                  I = JJ
                  TMP1 = W( JJ )
               ENDIF
   40       CONTINUE
!
            IF( I.NE.0 ) THEN
               W( I ) = W( J )
               W( J ) = TMP1
               CALL DSWAP( N, Z( 1, I ), 1, Z( 1, J ), 1 )
            ENDIF
   50    CONTINUE
      ENDIF
!
!     Set WORK(1) to optimal workspace size.
!
      WORK( 1 ) = LWKOPT
      IWORK( 1 ) = LIWMIN
!
      RETURN
!
!     End of DSYEVR
!
      END SUBROUTINE DSYEVR_fast
      
      
      
      
      
      SUBROUTINE DSYTD2( UPLO, N, A, LDA, D, E, TAU, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAU( * )
!     ..
!
!  Purpose
!  =======
!
!  DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
!  form T by an orthogonal similarity transformation: Q' * A * Q = T.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          Specifies whether the upper or lower triangular part of the
!          symmetric matrix A is stored:
!          = 'U':  Upper triangular
!          = 'L':  Lower triangular
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          n-by-n upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading n-by-n lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit, if UPLO = 'U', the diagonal and first superdiagonal
!          of A are overwritten by the corresponding elements of the
!          tridiagonal matrix T, and the elements above the first
!          superdiagonal, with the array TAU, represent the orthogonal
!          matrix Q as a product of elementary reflectors; if UPLO
!          = 'L', the diagonal and first subdiagonal of A are over-
!          written by the corresponding elements of the tridiagonal
!          matrix T, and the elements below the first subdiagonal, with
!          the array TAU, represent the orthogonal matrix Q as a product
!          of elementary reflectors. See Further Details.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  D       (output) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T:
!          D(i) = A(i,i).
!
!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          The off-diagonal elements of the tridiagonal matrix T:
!          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
!
!  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
!          The scalar factors of the elementary reflectors (see Further
!          Details).
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value.
!
!  Further Details
!  ===============
!
!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(n-1) . . . H(2) H(1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
!  A(1:i-1,i+1), and tau in TAU(i).
!
!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(1) H(2) . . . H(n-1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
!  and tau in TAU(i).
!
!  The contents of A on exit are illustrated by the following examples
!  with n = 5:
!
!  if UPLO = 'U':                       if UPLO = 'L':
!
!    (  d   e   v2  v3  v4 )              (  d                  )
!    (      d   e   v3  v4 )              (  e   d              )
!    (          d   e   v4 )              (  v1  e   d          )
!    (              d   e  )              (  v1  v2  e   d      )
!    (                  d  )              (  v1  v2  v3  e   d  )
!
!  where d and e denote diagonal and off-diagonal elements of T, and vi
!  denotes an element of the vector defining H(i).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE, ZERO, HALF
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0, &
                         HALF = 1.0D0 / 2.0D0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            UPPER
      INTEGER            I
      DOUBLE PRECISION   ALPHA, TAUI
!     ..
!     .. External Subroutines ..
      EXTERNAL           DAXPY, DLARFG, DSYMV, DSYR2, XERBLA
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DDOT
      EXTERNAL           LSAME, DDOT
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ENDIF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTD2', -INFO )
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( N.LE.0 ) &
         RETURN
!
      IF( UPPER ) THEN
!
!        Reduce the upper triangle of A
!
         DO 10 I = N - 1, 1, -1
!
!           Generate elementary reflector H(i) = I - tau * v * v'
!           to annihilate A(1:i-1,i+1)
!
            CALL DLARFG( I, A( I, I+1 ), A( 1, I+1 ), 1, TAUI )
            E( I ) = A( I, I+1 )
!
            IF( TAUI.NE.ZERO ) THEN
!
!              Apply H(i) from both sides to A(1:i,1:i)
!
               A( I, I+1 ) = ONE
!
!              Compute  x := tau * A * v  storing x in TAU(1:i)
!
               CALL DSYMV( UPLO, I, TAUI, A, LDA, A( 1, I+1 ), 1, ZERO, &
                           TAU, 1 )
!
!              Compute  w := x - 1/2 * tau * (x'*v) * v
!
               ALPHA = -HALF*TAUI*DDOT( I, TAU, 1, A( 1, I+1 ), 1 )
               CALL DAXPY( I, ALPHA, A( 1, I+1 ), 1, TAU, 1 )
!
!              Apply the transformation as a rank-2 update:
!                 A := A - v * w' - w * v'
!
               CALL DSYR2( UPLO, I, -ONE, A( 1, I+1 ), 1, TAU, 1, A, &
                           LDA )
!
               A( I, I+1 ) = E( I )
            ENDIF
            D( I+1 ) = A( I+1, I+1 )
            TAU( I ) = TAUI
   10    CONTINUE
         D( 1 ) = A( 1, 1 )
      ELSE
!
!        Reduce the lower triangle of A
!
         DO 20 I = 1, N - 1
!
!           Generate elementary reflector H(i) = I - tau * v * v'
!           to annihilate A(i+2:n,i)
!
            CALL DLARFG( N-I, A( I+1, I ), A( MIN( I+2, N ), I ), 1, &
                         TAUI )
            E( I ) = A( I+1, I )
!
            IF( TAUI.NE.ZERO ) THEN
!
!              Apply H(i) from both sides to A(i+1:n,i+1:n)
!
               A( I+1, I ) = ONE
!
!              Compute  x := tau * A * v  storing y in TAU(i:n-1)
!
               CALL DSYMV( UPLO, N-I, TAUI, A( I+1, I+1 ), LDA, &
                           A( I+1, I ), 1, ZERO, TAU( I ), 1 )
!
!              Compute  w := x - 1/2 * tau * (x'*v) * v
!
               ALPHA = -HALF*TAUI*DDOT( N-I, TAU( I ), 1, A( I+1, I ), &
                       1 )
               CALL DAXPY( N-I, ALPHA, A( I+1, I ), 1, TAU( I ), 1 )
!
!              Apply the transformation as a rank-2 update:
!                 A := A - v * w' - w * v'
!
               CALL DSYR2( UPLO, N-I, -ONE, A( I+1, I ), 1, TAU( I ), 1, &
                           A( I+1, I+1 ), LDA )
!
               A( I+1, I ) = E( I )
            ENDIF
            D( I ) = A( I, I )
            TAU( I ) = TAUI
   20    CONTINUE
         D( N ) = A( N, N )
      ENDIF
!
      RETURN
!
!     End of DSYTD2
!
      END SUBROUTINE DSYTD2
      
      
      
      
      
      SUBROUTINE DSYTRD( UPLO, N, A, LDA, D, E, TAU, WORK, LWORK, INFO )
!
!  -- LAPACK routine (version 3.1) --
!     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!     November 2006
!
!     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INFO, LDA, LWORK, N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), D( * ), E( * ), TAU( * ), &
                         WORK( * )
!     ..
!
!  Purpose
!  =======
!
!  DSYTRD reduces a real symmetric matrix A to real symmetric
!  tridiagonal form T by an orthogonal similarity transformation:
!  Q**T * A * Q = T.
!
!  Arguments
!  =========
!
!  UPLO    (input) CHARACTER*1
!          = 'U':  Upper triangle of A is stored;
!          = 'L':  Lower triangle of A is stored.
!
!  N       (input) INTEGER
!          The order of the matrix A.  N >= 0.
!
!  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
!          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
!          N-by-N upper triangular part of A contains the upper
!          triangular part of the matrix A, and the strictly lower
!          triangular part of A is not referenced.  If UPLO = 'L', the
!          leading N-by-N lower triangular part of A contains the lower
!          triangular part of the matrix A, and the strictly upper
!          triangular part of A is not referenced.
!          On exit, if UPLO = 'U', the diagonal and first superdiagonal
!          of A are overwritten by the corresponding elements of the
!          tridiagonal matrix T, and the elements above the first
!          superdiagonal, with the array TAU, represent the orthogonal
!          matrix Q as a product of elementary reflectors; if UPLO
!          = 'L', the diagonal and first subdiagonal of A are over-
!          written by the corresponding elements of the tridiagonal
!          matrix T, and the elements below the first subdiagonal, with
!          the array TAU, represent the orthogonal matrix Q as a product
!          of elementary reflectors. See Further Details.
!
!  LDA     (input) INTEGER
!          The leading dimension of the array A.  LDA >= max(1,N).
!
!  D       (output) DOUBLE PRECISION array, dimension (N)
!          The diagonal elements of the tridiagonal matrix T:
!          D(i) = A(i,i).
!
!  E       (output) DOUBLE PRECISION array, dimension (N-1)
!          The off-diagonal elements of the tridiagonal matrix T:
!          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
!
!  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
!          The scalar factors of the elementary reflectors (see Further
!          Details).
!
!  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!
!  LWORK   (input) INTEGER
!          The dimension of the array WORK.  LWORK >= 1.
!          For optimum performance LWORK >= N*NB, where NB is the
!          optimal blocksize.
!
!          If LWORK = -1, then a workspace query is assumed; the routine
!          only calculates the optimal size of the WORK array, returns
!          this value as the first entry of the WORK array, and no error
!          message related to LWORK is issued by XERBLA.
!
!  INFO    (output) INTEGER
!          = 0:  successful exit
!          < 0:  if INFO = -i, the i-th argument had an illegal value
!
!  Further Details
!  ===============
!
!  If UPLO = 'U', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(n-1) . . . H(2) H(1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
!  A(1:i-1,i+1), and tau in TAU(i).
!
!  If UPLO = 'L', the matrix Q is represented as a product of elementary
!  reflectors
!
!     Q = H(1) H(2) . . . H(n-1).
!
!  Each H(i) has the form
!
!     H(i) = I - tau * v * v'
!
!  where tau is a real scalar, and v is a real vector with
!  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
!  and tau in TAU(i).
!
!  The contents of A on exit are illustrated by the following examples
!  with n = 5:
!
!  if UPLO = 'U':                       if UPLO = 'L':
!
!    (  d   e   v2  v3  v4 )              (  d                  )
!    (      d   e   v3  v4 )              (  e   d              )
!    (          d   e   v4 )              (  v1  e   d          )
!    (              d   e  )              (  v1  v2  e   d      )
!    (                  d  )              (  v1  v2  v3  e   d  )
!
!  where d and e denote diagonal and off-diagonal elements of T, and vi
!  denotes an element of the vector defining H(i).
!
!  =====================================================================
!
!     .. Parameters ..
      DOUBLE PRECISION   ONE
      PARAMETER          ( ONE = 1.0D+0 )
!     ..
!     .. Local Scalars ..
      LOGICAL            LQUERY, UPPER
      INTEGER            I, IINFO, IWS, J, KK, LDWORK, LWKOPT, NB, &
                         NBMIN, NX
!     ..
!     .. External Subroutines ..
      EXTERNAL           DLATRD, DSYR2K, DSYTD2, XERBLA
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC          MAX
!     ..
!     .. External Functions ..
      LOGICAL            LSAME
      INTEGER            ILAENV
      EXTERNAL           LSAME, ILAENV
!     ..
!     .. Executable Statements ..
!
!     Test the input parameters
!
      INFO = 0
      UPPER = LSAME( UPLO, 'U' )
      LQUERY = ( LWORK.EQ.-1 )
      IF( .NOT.UPPER .AND. .NOT.LSAME( UPLO, 'L' ) ) THEN
         INFO = -1
      ELSE IF( N.LT.0 ) THEN
         INFO = -2
      ELSE IF( LDA.LT.MAX( 1, N ) ) THEN
         INFO = -4
      ELSE IF( LWORK.LT.1 .AND. .NOT.LQUERY ) THEN
         INFO = -9
      ENDIF
!
      IF( INFO.EQ.0 ) THEN
!
!        Determine the block size.
!
         NB = ILAENV( 1, 'DSYTRD', UPLO, N, -1, -1, -1 )
         LWKOPT = N*NB
         WORK( 1 ) = LWKOPT
      ENDIF
!
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DSYTRD', -INFO )
         RETURN
      ELSE IF( LQUERY ) THEN
         RETURN
      ENDIF
!
!     Quick return if possible
!
      IF( N.EQ.0 ) THEN
         WORK( 1 ) = 1
         RETURN
      ENDIF
!
      NX = N
      IWS = 1
      IF( NB.GT.1 .AND. NB.LT.N ) THEN
!
!        Determine when to cross over from blocked to unblocked code
!        (last block is always handled by unblocked code).
!
         NX = MAX( NB, ILAENV( 3, 'DSYTRD', UPLO, N, -1, -1, -1 ) )
         IF( NX.LT.N ) THEN
!
!           Determine if workspace is large enough for blocked code.
!
            LDWORK = N
            IWS = LDWORK*NB
            IF( LWORK.LT.IWS ) THEN
!
!              Not enough workspace to use optimal NB:  determine the
!              minimum value of NB, and reduce NB or force use of
!              unblocked code by setting NX = N.
!
               NB = MAX( LWORK / LDWORK, 1 )
               NBMIN = ILAENV( 2, 'DSYTRD', UPLO, N, -1, -1, -1 )
               IF( NB.LT.NBMIN ) &
                  NX = N
            ENDIF
         ELSE
            NX = N
         ENDIF
      ELSE
         NB = 1
      ENDIF
!
      IF( UPPER ) THEN
!
!        Reduce the upper triangle of A.
!        Columns 1:kk are handled by the unblocked method.
!
         KK = N - ( ( N-NX+NB-1 ) / NB )*NB
         DO 20 I = N - NB + 1, KK + 1, -NB
!
!           Reduce columns i:i+nb-1 to tridiagonal form and form the
!           matrix W which is needed to update the unreduced part of
!           the matrix
!
            CALL DLATRD( UPLO, I+NB-1, NB, A, LDA, E, TAU, WORK, &
                         LDWORK )
!
!           Update the unreduced submatrix A(1:i-1,1:i-1), using an
!           update of the form:  A := A - V*W' - W*V'
!
            CALL DSYR2K( UPLO, 'No transpose', I-1, NB, -ONE, A( 1, I ), &
                         LDA, WORK, LDWORK, ONE, A, LDA )
!
!           Copy superdiagonal elements back into A, and diagonal
!           elements into D
!
            DO 10 J = I, I + NB - 1
               A( J-1, J ) = E( J-1 )
               D( J ) = A( J, J )
   10       CONTINUE
   20    CONTINUE
!
!        Use unblocked code to reduce the last or only block
!
         CALL DSYTD2( UPLO, KK, A, LDA, D, E, TAU, IINFO )
      ELSE
!
!        Reduce the lower triangle of A
!
         DO 40 I = 1, N - NX, NB
!
!           Reduce columns i:i+nb-1 to tridiagonal form and form the
!           matrix W which is needed to update the unreduced part of
!           the matrix
!
            CALL DLATRD( UPLO, N-I+1, NB, A( I, I ), LDA, E( I ), &
                         TAU( I ), WORK, LDWORK )
!
!           Update the unreduced submatrix A(i+ib:n,i+ib:n), using
!           an update of the form:  A := A - V*W' - W*V'
!
            CALL DSYR2K( UPLO, 'No transpose', N-I-NB+1, NB, -ONE, &
                         A( I+NB, I ), LDA, WORK( NB+1 ), LDWORK, ONE, &
                         A( I+NB, I+NB ), LDA )
!
!           Copy subdiagonal elements back into A, and diagonal
!           elements into D
!
            DO 30 J = I, I + NB - 1
               A( J+1, J ) = E( J )
               D( J ) = A( J, J )
   30       CONTINUE
   40    CONTINUE
!
!        Use unblocked code to reduce the last or only block
!
         CALL DSYTD2( UPLO, N-I+1, A( I, I ), LDA, D( I ), E( I ), &
                      TAU( I ), IINFO )
      ENDIF
!
      WORK( 1 ) = LWKOPT
      RETURN
!
!     End of DSYTRD
!
      END SUBROUTINE DSYTRD
