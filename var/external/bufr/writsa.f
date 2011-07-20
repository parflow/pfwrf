      SUBROUTINE WRITSA(LUNXX,MSGT,MSGL)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRITSA
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 1994-01-06
C
C ABSTRACT: THIS SUBROUTINE SHOULD ONLY BE CALLED WHEN LOGICAL UNIT
C   ABS(LUNXX) HAS BEEN OPENED FOR OUTPUT OPERATIONS.
C
C   WHEN LUNXX IS GREATER THAN ZERO, IT PACKS UP THE CURRENT SUBSET
C   WITHIN MEMORY AND THEN TRIES TO ADD IT TO THE BUFR MESSAGE THAT IS
C   CURRENTLY OPEN WITHIN MEMORY FOR ABS(LUNXX).  THE DETERMINATION AS
C   TO WHETHER OR NOT THE SUBSET CAN BE ADDED TO THE MESSAGE IS MADE
C   VIA AN INTERNAL CALL TO ONE OF THE BUFR ARCHIVE LIBRARY SUBROUTINES
C   WRCMPS OR MSGUPD, DEPENDING UPON WHETHER OR NOT THE MESSAGE IS
C   COMPRESSED.  IF IT TURNS OUT THAT THE SUBSET CANNOT BE ADDED TO THE
C   CURRENTLY OPEN MESSAGE, THEN THAT MESSAGE IS FLUSHED TO ABS(LUNXX)
C   AND A NEW ONE IS CREATED IN ORDER TO HOLD THE SUBSET.  AS LONG AS
C   LUNXX IS GREATER THAN ZERO, WRITSA FUNCTIONS EXACTLY LIKE BUFR
C   ARCHIVE LIBRARY SUBROUTINE WRITSB, EXCEPT THAT WRITSA ALSO RETURNS
C   A COPY OF EACH COMPLETED BUFR MESSAGE TO THE APPLICATION PROGRAM
C   IN THE FIRST MSGL WORDS OF ARRAY MSGT.
C
C   ALTERNATIVELY, WHEN LUNXX IS LESS THAN ZERO, THIS IS A SIGNAL TO
C   FORCE ANY CURRENT MESSAGE IN MEMORY TO BE FLUSHED TO ABS(LUNXX) AND
C   RETURNED IN ARRAY MSGT.  IN SUCH CASES, ANY CURRENT SUBSET IN MEMORY
C   IS IGNORED.  THIS OPTION IS NECESSARY BECAUSE ANY MESSAGE RETURNED
C   IN MSGT FROM A CALL TO THIS ROUTINE NEVER CONTAINS THE ACTUAL SUBSET
C   THAT WAS PACKED UP AND STORED DURING THE SAME CALL TO THIS ROUTINE.
C   THEREFORE, THE ONLY WAY TO ENSURE THAT EVERY LAST BUFR SUBSET IS
C   RETURNED WITHIN A BUFR MESSAGE IN MSGT BEFORE, E.G., EXITING THE
C   APPLICATION PROGRAM, IS TO DO ONE FINAL CALL TO THIS ROUTINE WITH
C   LUNXX LESS THAN ZERO IN ORDER TO FORCIBLY FLUSH OUT AND RETURN ONE
C   FINAL BUFR MESSAGE.
C
C PROGRAM HISTORY LOG:
C 1994-01-06  J. WOOLLEN -- ORIGINAL AUTHOR
C 1998-07-08  J. WOOLLEN -- REPLACED CALL TO CRAY LIBRARY ROUTINE
C                           "ABORT" WITH CALL TO NEW INTERNAL BUFRLIB
C                           ROUTINE "BORT"
C 2000-09-19  J. WOOLLEN -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           10,000 TO 20,000 BYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION (INCLUDING HISTORY); OUTPUTS
C                           MORE COMPLETE DIAGNOSTIC INFO WHEN ROUTINE
C                           TERMINATES ABNORMALLY
C 2004-08-18  J. ATOR    -- ADD POST-MSGUPD CHECK FOR AND RETURN OF
C                           MESSAGE WITHIN MSGT IN ORDER TO PREVENT
C                           LOSS OF MESSAGE IN CERTAIN SITUATIONS;
C                           MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-03-09  J. ATOR    -- ADDED CAPABILITY FOR COMPRESSED MESSAGES
C
C USAGE:    CALL WRITSA (LUNXX, MSGT, MSGL)
C   INPUT ARGUMENT LIST:
C     LUNXX    - INTEGER: ABSOLUTE VALUE IS FORTRAN LOGICAL UNIT NUMBER
C                FOR BUFR FILE {IF LUNXX IS LESS THAN ZERO, THEN ANY
C                CURRENT MESSAGE IN MEMORY WILL BE FORCIBLY FLUSHED TO
C                ABS(LUNXX) AND TO ARRAY MSGT}
C
C   OUTPUT ARGUMENT LIST:
C     MSGT     - INTEGER: *-WORD PACKED BINARY ARRAY CONTAINING BUFR
C                MESSAGE (FIRST MSGL WORDS FILLED)
C     MSGL     - INTEGER: NUMBER OF WORDS FILLED IN MSGT
C                       0 = no message was returned
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     CLOSMG   MSGUPD   STATUS
C                               WRCMPS   WRTREE
C    THIS ROUTINE IS CALLED BY: None 
C                               Normally called only by application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BUFRMG/ MSGLEN,MSGTXT(MXMSGLD4)
      COMMON /MSGCMP/ CCMF

      CHARACTER*1 CCMF

      DIMENSION MSGT(*)

C----------------------------------------------------------------------
C----------------------------------------------------------------------

      LUNIT = ABS(LUNXX)

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  IF LUNXX < 0, FORCE MEMORY MSG TO BE WRITTEN (W/O ANY CURRENT SUBSET)
C  ---------------------------------------------------------------------

      IF(LUNXX.LT.0) CALL CLOSMG(LUNIT)

C  IS THERE A COMPLETED BUFR MESSAGE TO BE RETURNED?
C  -------------------------------------------------

      IF(MSGLEN.GT.0) THEN
         MSGL = MSGLEN
         DO N=1,MSGL
         MSGT(N) = MSGTXT(N)
         ENDDO
         MSGLEN = 0
      ELSE
         MSGL = 0
      ENDIF

      IF(LUNXX.LT.0) GOTO 100

C  PACK UP THE SUBSET AND PUT IT INTO THE MESSAGE
C  ----------------------------------------------

      CALL WRTREE(LUN)
      IF( CCMF.EQ.'Y' ) THEN
          CALL WRCMPS(LUNIT)
      ELSE
          CALL MSGUPD(LUNIT,LUN)
      ENDIF

C  IF THE JUST-COMPLETED CALL TO WRCMPS OR MSGUPD FOR THIS SUBSET CAUSED
C  A PREVIOUS MESSAGE TO BE FLUSHED TO ABS(LUNXX), THEN RETRIEVE AND
C  RETURN THAT MESSAGE NOW.  OTHERWISE, WE RUN THE RISK THAT THE NEXT
C  CALL TO OPENMB OR OPENMG MIGHT CAUSE A NEWER MESSAGE (WHICH WOULD
C  CONTAIN THE CURRENT SUBSET!) TO BE FLUSHED AND THUS OVERWRITE THE
C  PREVIOUS MESSAGE WITHIN ARRAY MSGTXT BEFORE WE HAD THE CHANCE TO
C  RETRIEVE IT DURING THE NEXT CALL TO WRITSA!

C  NOTE ALSO THAT, IF THE MOST RECENT CALL TO OPENMB OR OPENMG HAD
C  CAUSED A MESSAGE TO BE FLUSHED, IT WOULD HAVE DONE SO IN ORDER TO
C  CREATE A NEW MESSAGE TO HOLD THE CURRENT SUBSET.  THUS, IN SUCH
C  CASES, IT SHOULD NOT BE POSSIBLE THAT THE JUST-COMPLETED CALL TO
C  WRCMPS OR MSGUPD (FOR THIS SAME SUBSET!) WOULD HAVE ALSO CAUSED A
C  MESSAGE TO BE FLUSHED, AND THUS IT SHOULD NOT BE POSSIBLE TO HAVE
C  TWO (2) SEPARATE BUFR MESSAGES RETURNED FROM ONE (1) CALL TO WRITSA!

      IF(MSGLEN.GT.0) THEN
         IF(MSGL.NE.0) GOTO 903
         MSGL = MSGLEN
         DO N=1,MSGL
         MSGT(N) = MSGTXT(N)
         ENDDO
         MSGLEN = 0
      ENDIF

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITSA - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITSA - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: WRITSA - TWO BUFR MESSAGES WERE RETRIEVED '//
     . 'BY ONE CALL TO THIS ROUTINE')
      END
