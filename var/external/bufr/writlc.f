      SUBROUTINE WRITLC(LUNIT,CHR,STR)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    WRITLC
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2003-11-04
C
C ABSTRACT: THIS SUBROUTINE PACKS A CHARACTER DATA ELEMENT ASSOCIATED
C   WITH A PARTICULAR SUBSET MNEMONIC FROM THE INTERNAL MESSAGE BUFFER
C   (ARRAY MBAY IN COMMON BLOCK /BITBUF/).  IT IS DESIGNED TO BE USED
C   TO STORE CHARACTER ELEMENTS GREATER THAN THE USUAL LENGTH OF EIGHT
C   BYTES.
C
C PROGRAM HISTORY LOG:
C 2003-11-04  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  D. KEYSER  -- UNIFIED/PORTABLE FOR WRF; ADDED
C                           DOCUMENTATION; OUTPUTS MORE COMPLETE
C                           DIAGNOSTIC INFO WHEN ROUTINE TERMINATES
C                           ABNORMALLY
C 2004-08-09  J. ATOR    -- MAXIMUM MESSAGE LENGTH INCREASED FROM
C                           20,000 TO 50,000 BYTES
C 2005-11-29  J. ATOR    -- USE GETLENS
C 2007-01-19  J. ATOR    -- REPLACED CALL TO PARSEQ WITH CALL TO PARSTR
C
C USAGE:    CALL WRITLC (LUNIT, CHR, STR)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     CHR      - CHARACTER*(*): UNPACKED CHARACTER STRING (I.E.,
C                CHARACTER DATA ELEMENT GREATER THAN EIGHT BYTES)
C     STR      - CHARACTER*(*): STRING (I.E., MNEMONIC)
C
C REMARKS:
C    THIS ROUTINE CALLS:        BORT     GETLENS  PARSTR   PKC
C                               STATUS   UPB      UPBB     USRTPL
C    THIS ROUTINE IS CALLED BY: None (currently)
C                               Normally not called by any application
C                               programs.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      INCLUDE 'bufrlib.prm'

      COMMON /BITBUF/ MAXBYT,IBIT,IBAY(MXMSGLD4),MBYT(NFILES),
     .                MBAY(MXMSGLD4,NFILES)
      COMMON /TABLES/ MAXTAB,NTAB,TAG(MAXJL),TYP(MAXJL),KNT(MAXJL),
     .                JUMP(MAXJL),LINK(MAXJL),JMPB(MAXJL),
     .                IBT(MAXJL),IRF(MAXJL),ISC(MAXJL),
     .                ITP(MAXJL),VALI(MAXJL),KNTI(MAXJL),
     .                ISEQ(MAXJL,2),JSEQ(MAXJL)
      COMMON /MSGCWD/ NMSG(NFILES),NSUB(NFILES),MSUB(NFILES),
     .                INODE(NFILES),IDATE(NFILES)
      COMMON /USRINT/ NVAL(NFILES),INV(MAXJL,NFILES),VAL(MAXJL,NFILES)

      CHARACTER*(*) CHR,STR
      CHARACTER*128 BORT_STR
      CHARACTER*10  TAG,TGS(100)
      CHARACTER*8   CTAG
      CHARACTER*3   TYP
      REAL*8        VAL

      DATA MAXTG /100/

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C  CHECK THE FILE STATUS
C  ---------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.LT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902

C  CHECK FOR TAGS (MNEMONICS) IN INPUT STRING (THERE CAN ONLY BE ONE)
C  ------------------------------------------------------------------

      CALL PARSTR(STR,TGS,MAXTG,NTG,' ',.TRUE.)
      IF(NTG.GT.1) GOTO 903
      CTAG = TGS(1)

C  CHECK THAT THE INPUT TAG IS A CHARACTER STRING
C  ----------------------------------------------

      INOD = INODE(LUN)
      DO NOD=INOD,ISC(INOD)
      IF(CTAG.EQ.TAG(NOD)) GOTO 1
      ENDDO
      GOTO 904
1     IF(TYP(NOD).NE.'CHR') GOTO 905

C  LOCATE THE BEGINNING OF THE DATA IN SECTION 4 (MBYTE)
C  -----------------------------------------------------

      CALL GETLENS(MBAY(1,LUN),3,LEN0,LEN1,LEN2,LEN3,L4,L5)

      MBYTE = LEN0 + LEN1 + LEN2 + LEN3 + 4
      NSUBS = 1

C  FIND THE MOST RECENTLY WRITTEN SUBSET IN THE MESSAGE
C  ----------------------------------------------------

      DO WHILE(NSUBS.LT.NSUB(LUN))
         IBIT = MBYTE*8
         CALL UPB(NBYT,16,MBAY(1,LUN),IBIT)
         MBYTE = MBYTE + NBYT
         NSUBS = NSUBS + 1
      ENDDO

      IF(NSUBS.NE.NSUB(LUN)) GOTO 906

C  LOCATE THE STRING ELEMENT TO WRITE
C  ----------------------------------

      MBIT = MBYTE*8 + 16
      NBIT = 0
      N = 1
      CALL USRTPL(LUN,N,N)
20    IF(N+1.LE.NVAL(LUN)) THEN
         N = N+1
         NODE = INV(N,LUN)
         MBIT = MBIT+NBIT
         NBIT = IBT(NODE)
         IF(ITP(NODE).EQ.1) THEN
            CALL UPBB(IVAL,NBIT,MBIT,MBAY(1,LUN))
            CALL USRTPL(LUN,N,IVAL)
            GO TO 20
         ENDIF
         IF(NOD.EQ.NODE) THEN
            IF(ITP(NODE).EQ.3) THEN
               NCHR = NBIT/8
               IBIT = MBIT
               DO N=1,NCHR
               CALL PKC(' ',1,MBAY(1,LUN),IBIT)
               ENDDO
               CALL PKC(CHR,NCHR,MBAY(1,LUN),MBIT)
               CALL USRTPL(LUN,1,1)
               GOTO 100
            ENDIF
         ENDIF
         GOTO 20
      ENDIF
      GOTO 907

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR OUTPUT')
901   CALL BORT('BUFRLIB: WRITLC - OUTPUT BUFR FILE IS OPEN FOR '//
     . 'INPUT, IT MUST BE OPEN FOR OUTPUT')
902   CALL BORT('BUFRLIB: WRITLC - A MESSAGE MUST BE OPEN IN OUTPUT '//
     . 'BUFR FILE, NONE ARE')
903   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THERE CANNOT BE MORE THAN '//
     . ' ONE MNEMONIC IN THE INPUT STRING (",A,") (HERE THERE ARE",I4'//
     . ',")")') STR,NTG
      CALL BORT(BORT_STR)
904   WRITE(BORT_STR,'("BUFRLIB: WRITLC - MNEMONIC ",A," NOT LOCATED '//
     . 'IN REPORT SUBSET")') CTAG
      CALL BORT(BORT_STR)
905   WRITE(BORT_STR,'("BUFRLIB: WRITLC - MNEMONIC ",A," DOES NOT '//
     . 'REPRESENT A CHARACTER ELEMENT (TYP=",A,")")') CTAG,TYP(NOD)
      CALL BORT(BORT_STR)
906   WRITE(BORT_STR,'("BUFRLIB: WRITLC - THE MOST RECENTLY WRITTEN '//
     . ' SUBSET NO. (",I3,") IN MSG .NE. THE STORED VALUE FOR THE NO.'//
     . ' OF SUBSETS (",I3,") IN MSG")') NSUBS,NSUB(LUN)
      CALL BORT(BORT_STR)
907   WRITE(BORT_STR,'("BUFRLB: WRITLC - UNABLE TO FIND ",A," IN '//
     . 'SUBSET")') CTAG
      CALL BORT(BORT_STR)
      END
