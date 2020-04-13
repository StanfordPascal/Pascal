* $$ JOB   JNM=LEI386T,CLASS=C,DISP=D
* $$ LST   CLASS=Z,FNO=COB
* $$ PUN   CLASS=Z,RBS=1000
// JOB     KVSONST (LEI386T)
// ASSGN SYS008,SYSLST
// EXEC    XEROX01
^XERPROG4  0011LEI386T
-----------------------------------------------------------
PROZEDUR:               %PU / 12.03.20 / 10:41:05

UMWANDLUNGSLISTE:       LEI386T / HAUPT / COB / OHNE
KATALOGISIERT AUF:      SDK.TEST
ERSTELLT VON:           XO2
-----------------------------------------------------------
/*
-INC       VIO
   LIBDEF  *,CATALOG=SDK.TEST,TEMP
// OPTION  CATAL
/*
// EXEC    IGYCRCTL,SIZE=IGYCRCTL
 PROCESS TEST(ALL,SYM)
 PROCESS APOST,NOADV,NODYNAM,RENT

       IDENTIFICATION DIVISION.
      ******************************************************************
      * -------------------------------------------------------------- *
      *  L E I 3 8 6           ** BILDEN LEIF3DX MIT 4 VERTRAGSJAHREN  *
      * -------------------------------------------------------------- *
      *                           E I N G A B E ---------------------- *
      *                           LEIEIN1     : LEIF3DX AKTUELL (3-J)  *
      *                           LEIEIN2     : LEIF3DX VORJAHR (3-J)  *
      *                                                                *
      *                           A U S G A B E ---------------------- *
      *                           LEIVSAM     : LEIF3DX ZIELDATEI      *
      * -------------------------------------------------------------- *
      *  26/06/06 SCHMID          ERSTELLUNG                           *
      *  29/12/06                 ANPASSEN UNTERGRENZE                 *
      *  28/10/11                 LEIF3D MIT 300 VARIABLEN             *
      * -------------------------------------------------------------- *
      ******************************************************************
       PROGRAM-ID. LEI386T.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       SPECIAL-NAMES.
-INC SPECNAME

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT LEIEIN1 ASSIGN TO LEIEIN1
              RECORD KEY LE1CSCHL
              ORGANIZATION INDEXED
              ACCESS MODE SEQUENTIAL
              FILE STATUS STATUS-BYTE.

           SELECT LEIEIN2 ASSIGN TO LEIEIN2
              RECORD KEY LE2CSCHL
              ORGANIZATION INDEXED
              ACCESS MODE SEQUENTIAL
              FILE STATUS STATUS-BYTE.

           SELECT LEIVSAM ASSIGN TO LEIVSAM
              RECORD KEY LEICSCHL
              ORGANIZATION INDEXED
              ACCESS MODE SEQUENTIAL
              FILE STATUS STATUS-BYTE.


       DATA DIVISION.

       FILE SECTION.

       FD  LEIEIN1
           LABEL RECORDS STANDARD.
-INC LEICSA3X

       FD  LEIEIN2
           LABEL RECORDS STANDARD.
-INC LEICSA4X

       FD  LEIVSAM
           LABEL RECORDS STANDARD.
-INC LEICSATX


       WORKING-STORAGE SECTION.

       01  FILLER.
           05  INDX                  PIC S9(9) COMP-3 VALUE ZERO.
           05  ZCPRS                 PIC S9(9) COMP-3 VALUE ZERO.
           05  VVVJAHR               PIC 99        VALUE ZERO.
           05  VVOJAHR               PIC 99        VALUE ZERO.
           05  MAXZDVA               PIC 9(3)      VALUE ZERO.
           05  MAXSCHL               PIC X(4)      VALUE ZERO.
      *
      *    -----  STEUERTABELLE ZUM NACHTRAG VORVORVORJAHR
      *           NOCP      1 = NICHT VERDICHTEN, DA MEHRERE ZEITRÑUME
      *           SCHA      1 = ZEITRAUM GEHT ÅBER VORVORVORJAHR HINAUS
      *           TRAN      TRANSFER VORVORVORJAHR IN LEIF3DX-AKTUELL
      *           ANZA      ANZAHL EINTRÑGE  FÅR  MERKZEIT
      *           ZEIT      EINZIGER ZEITRAUM IN  VORVORVORJAHR
      *
       01  MERKDATN.
           05  MERKNOCP              PIC X.
           05  MERKSCHA              PIC X.
           05  MERKTRAN              PIC X.
           05  MERKANZA              PIC 9(3).
           05  MERKZEIT.
               10  MERKVOND.
                   15  MERKVONJ      PIC 99.
                   15  MERKVONM      PIC 99.
               10  MERKBISD.
                   15  MERKBISJ      PIC 99.
                   15  MERKBISM      PIC 99.

       01  ALTELEIC                  PIC 9(17)     VALUE ZERO.
       01  FILLER                    REDEFINES ALTELEIC.
           05  ALTEZEIT.
               10  ALTEVOND.
                   15  ALTEVONJ      PIC 99.
                   15  ALTEVONM      PIC 99.
               10  ALTEBISD.
                   15  ALTEBISJ      PIC 99.
                   15  ALTEBISM      PIC 99.
           05  ALTETASN              PIC 9(3).
           05  ALTEGESL              PIC 9.
           05  ALTEANZA              PIC 9(3)V99.

       01  NEUELEIC                  PIC 9(17)     VALUE ZERO.
       01  FILLER                    REDEFINES NEUELEIC.
           05  NEUEZEIT.
               10  NEUEVOND.
                   15  NEUEVONJ      PIC 99.
                   15  NEUEVONM      PIC 99.
               10  NEUEBISD.
                   15  NEUEBISJ      PIC 99.
                   15  NEUEBISM      PIC 99.
           05  NEUETASN              PIC 9(3).
           05  NEUEGESL              PIC 9.
           05  NEUEANZA              PIC 9(3)V99.

-INC U100C80
-INC WSDATUM
-INC VSSTATUS
+INC TOL.LEICWORK
-INC WSLEIPK

      ******************************************************************
      *    --------------------------------------------------------    *
      *    GRUPPENWECHSEL-STEUERFELDER    -INC GRUPPWS     (SCHMID)    *
      *    --------------------------------------------------------    *
      *           LDATEI#  = LESESCHALTER            (# = DATEI-NR)    *
      *                      0 = NICHT LESEN (DATEI NICHT AM ZUG)      *
      *                      1 = LESEN                                 *
      *                      9 = NICHT LESEN (DATEI AT END)            *
      *                                                                *
      *           VDATEI#  = VERGLEICHSFELDER 1..10  (# = DATEI-NR)    *
      *                      BEI EINTRAG-1    BEGINNEN                 *
      *    --------------------------------------------------------    *
      *    MODULE GRUPPPD  = STEUERUNG FUER 2 SEQ-DATEIEN              *
      *                      MIT 1 GRUPPENWECHSELSTUFE                 *
      *    --------------------------------------------------------    *
      ******************************************************************
       01  FILLER.

           05  LDATEI1               PIC X         VALUE '1'.
           05  LDATEI2               PIC X         VALUE '1'.

           05  VDATEI1               PIC X(150)    VALUE LOW-VALUE.
           05  FILLER                REDEFINES VDATEI1.
               10  VAFELD1           PIC X(15) OCCURS 10.

           05  VDATEI2               PIC X(150)    VALUE LOW-VALUE.
           05  FILLER                REDEFINES VDATEI2.
               10  VAFELD2           PIC X(15) OCCURS 10.


       PROCEDURE DIVISION.

-INC PDDATUM

           CALL 'UPRO100' USING U100SATZ.

           OPEN INPUT LEIEIN1.
           IF NOT STATUS-OK
              DISPLAY '** Fehler OPEN LEIEIN1  **' STATUS-BYTE
              STOP '** Fehler OPEN LEIEIN1 - ABBRECHEN'
              GO TO A99.

           OPEN INPUT LEIEIN2.
           IF NOT STATUS-OK
              DISPLAY '** Fehler OPEN LEIEIN2  **' STATUS-BYTE
              STOP '** Fehler OPEN LEIEIN2 - ABBRECHEN'
              GO TO A99.

           OPEN OUTPUT LEIVSAM.
           IF NOT STATUS-OK
              DISPLAY '** Fehler OPEN LEIVSAM  **' STATUS-BYTE
              STOP '** Fehler OPEN LEIVSAM - ABBRECHEN'
              GO TO A99.

           COMPUTE VVOJAHR = JJ - 2.
           DISPLAY 'VORVORJAHR    =' VVOJAHR.
           COMPUTE VVVJAHR = JJ - 3.
           DISPLAY 'VORVORVORJAHR =' VVVJAHR.

       A01.

      *-----------------------------------------------------------
      *    STEUERUNG: DATEI1=LEIF3DX-AKTJ / DATEI2=LEIF3DX-VORJ
      *-----------------------------------------------------------

           PERFORM M00.

       A99.

           MOVE MAXSCHL TO PACKKOMP.
           PERFORM UNP.
           DISPLAY 'HîCHSTER TABELLENWERT = ' MAXZDVA ' BEI ' PACKUNKO.
           DISPLAY 'ANZAHL-VERDICHTUNGEN  = ' ZCPRS.
           CLOSE LEIEIN1 LEIEIN2 LEIVSAM.
           STOP RUN.



      ******************************************************************
      *    formatting test ...                                         *
      ******************************************************************

       TEST SECTION.
       TEST1.

           IF CONDITION1 AND
              CONDITION1-1 AND
              CONDITION1-2
           STOP 'ANYTHING'
              PERFORM UNTIL CONDITION2
                 IF CONDITION3
                    MOVE A TO B  END-IF
              END-PERFORM
           END-IF.

           IF CONDITION1 AND
              CONDITION1-1 AND
              CONDITION1-2
           READ LEIEIN1
           NEXT
           AT END
           MOVE '9' TO LDATEI1
           END-READ
              PERFORM UNTIL CONDITION2
                 IF CONDITION3
                    MOVE A TO B
                 END-IF
              END-PERFORM
           END-IF.

           if condition1 and
              condition1-1 and
              condition1-2
           READ LEIEIN1 invalid key
           MOVE '9' TO LDATEI1
           END-READ
              perform until condition2
                 if condition3
                    move a to B
                 end-if
              end-perform
           end-if.

           EVALUATE VWST        WHEN 2      ADD LEIOGESV TO SUMRVBT
                                            ADD LEIOGESR TO SUMRRBT
                                            ADD LEIOGESE TO SUMRERT
                                WHEN 3      ADD LEIOGESV TO SUMFVBT
                                            ADD LEIOGESR TO SUMFRBT
                                            ADD LEIOGESE TO SUMFERT
                                WHEN 4      ADD LEIOGESV TO SUMAVBT
                                            ADD LEIOGESR TO SUMARBT
                                            ADD LEIOGESE TO SUMAERT
                                WHEN OTHER  ADD LEIOGESV TO SUMSVBT
                                            ADD LEIOGESR TO SUMSRBT
                                            ADD LEIOGESE TO SUMSERT
                                                END-EVALUATE.

           IF     VTYP     =    1           ADD 1        TO ANZXERL
                  EVALUATE VWST WHEN 2      ADD 1        TO ANZRERL
                                WHEN 3      ADD 1        TO ANZFERL
                                WHEN 4      ADD 1        TO ANZAERL
                                WHEN OTHER  ADD 1        TO ANZSERL
                                                END-EVALUATE
           ELSE                             ADD 1        TO ANZXFEL
                  EVALUATE VWST WHEN 2      ADD 1        TO ANZRFEL
                                WHEN 3      ADD 1        TO ANZFFEL
                                WHEN 4      ADD 1        TO ANZAFEL
                                WHEN OTHER  ADD 1        TO ANZSFEL
                                                END-EVALUATE.




      ******************************************************************
      *    M01 LESEN LEIF3DX-AKTJ                                      *
      ******************************************************************

       M01 SECTION.
       M01A.
           READ LEIEIN1 NEXT
                AT END
                   MOVE '9' TO LDATEI1
                   GO TO M01Z.
           MOVE LE1CSCHL TO VAFELD1 (1).
       M01Z.
           EXIT.



      ******************************************************************
      *    M02 LESEN LEIF3DX-VORJ                                      *
      ******************************************************************

       M02 SECTION.
       M02A.
           READ LEIEIN2
                AT END
                   MOVE '9' TO LDATEI2
                   GO TO M02Z.
           MOVE LE2CSCHL TO VAFELD2 (1).

      *-----------------------------------------------------------
      *    ANPASSEN UNTERGRENZE
      *-----------------------------------------------------------

           PERFORM VARYING INDX FROM 1 BY 1
                   UNTIL INDX GREATER LE2CZDVA

              MOVE LE2CDATN (INDX) TO ZWISLEIC

              IF ZWISVONJ LESS VVVJAHR AND
              ZWISBISJ NOT LESS VVVJAHR
                 MOVE VVVJAHR TO ZWISVONJ
                 MOVE 01 TO ZWISVONM
                 MOVE ZWISLEIC TO LE2CDATN (INDX)
              END-IF

           END-PERFORM.

       M02Z.
           EXIT.



      ******************************************************************
      *    M03 NUR LEIF3DX-AKTJ                                        *
      ******************************************************************

       M03 SECTION.
       M03A.
           MOVE SPACE TO LEICSATZ.
           MOVE LE1CZDVA TO LEICZDVA.
           MOVE LE1CSATZ TO LEICSATZ.
           MOVE LEICSCHL TO PACKKOMP.
           PERFORM W01.
       M03Z.
           EXIT.



      ******************************************************************
      *    M04 SATZ IN BEIDEN DATEIEN                                  *
      ******************************************************************

       M04 SECTION.
       M04A.
           MOVE ZERO TO LEICZDVA.
           MOVE LE1CSATZ TO LEICSATZ.
           MOVE SPACE TO LEICTABE.
           MOVE ZERO TO LEICZDVA.

           INITIALIZE MERKDATN.

      *-----------------------------------------------------------
      *    SÑTZE AUS VORVORVORJAHR ÅBERTRAGEN
      *-----------------------------------------------------------

           PERFORM VARYING INDX FROM 1 BY 1
           UNTIL INDX GREATER LE2CZDVA

              MOVE LE2CDATN (INDX) TO ZWISLEIC
              PERFORM U01

           END-PERFORM.

           IF MERKNOCP NOT = '1' AND
              MERKSCHA = '1' AND
              MERKZEIT GREATER ZERO AND
              MERKVONJ = VVVJAHR AND
              MERKANZA GREATER ZERO AND
              MERKANZA = LEICZDVA AND
              MERKANZA NOT GREATER LE1CZDVA
              NEXT SENTENCE
           ELSE
              GO TO M04B.

      *-----------------------------------------------------------
      *    PRÅFEN AUF VERDICHTEN
      *-----------------------------------------------------------

           MOVE '1' TO MERKTRAN.

           PERFORM VARYING INDX FROM 1 BY 1
           UNTIL (INDX GREATER MERKANZA OR
           MERKTRAN NOT = '1')
              MOVE LEICDATN (INDX) TO ALTELEIC
              MOVE LE1CDATN (INDX) TO NEUELEIC
              IF ALTEBISJ NOT = VVVJAHR OR
                 ALTEBISM NOT = 12 OR
                 NEUEVONJ NOT = VVOJAHR OR
                 NEUEVONM NOT = 01 OR
                 ALTETASN NOT = NEUETASN OR
                 ALTEGESL NOT = NEUEGESL OR
                 ALTEANZA NOT = NEUEANZA MOVE '0' TO MERKTRAN
              END-IF
           END-PERFORM.

           IF MERKTRAN NOT = '1'
              GO TO M04B.

      *-----------------------------------------------------------
      *    VERDICHTEN
      *-----------------------------------------------------------

           MOVE SPACE TO LEICTABE.
           MOVE ZERO TO LEICZDVA.

           PERFORM VARYING INDX FROM 1 BY 1
           UNTIL INDX GREATER MERKANZA

              MOVE LE1CDATN (INDX) TO NEUELEIC
              MOVE MERKVOND TO NEUEVOND
              MOVE NEUELEIC TO LE1CDATN (INDX)

           END-PERFORM.

           ADD 1 TO ZCPRS.

       M04B.

      *-----------------------------------------------------------
      *    SÑTZE AUS VORVORJAHR - LFDJAHR ÅBERTRAGEN
      *-----------------------------------------------------------

           PERFORM VARYING INDX FROM 1 BY 1
           UNTIL INDX GREATER LE1CZDVA
              PERFORM U02
           END-PERFORM.

           PERFORM W01.
       M04Z.
           EXIT.



      ******************************************************************
      *    M05 NUR LEIF3DX-VORJ                                        *
      ******************************************************************
       M05 SECTION.
       M05A.
           MOVE SPACE TO LEICSATZ.
           MOVE LE2CZDVA TO LEICZDVA.
           MOVE LE2CSATZ TO LEICSATZ.
           MOVE LEICSCHL TO PACKKOMP.
           PERFORM W01.
       M05Z.
           EXIT.



      ******************************************************************
      *    ----------------------------------------------------------  *
      *    GRUPPENWECHSEL-STEUERUNG FUER 2 DATEIEN MIT  E I N E M      *
      *    GRUPPENWECHSEL-BEGRIFF         -INC GRUPPPD    (SCHMID)     *
      *    ----------------------------------------------------------  *
      *    MODULE M00 = HAUPTSTEUERUNG                           INT.  *
      *           M01 = LESEN DATEI-1  ABHAENGIG VON LDATEI1 = 1       *
      *                 SETZEN VERGLEICHSPARAMETER VON DATEI-1         *
      *                 SETZEN LDATEI1 = 9 AM ENDE VON DATEI-1   EXT.  *
      *           M02 = LESEN DATEI-2  ABHAENGIG VON LDATEI2 = 1       *
      *                 SETZEN VERGLEICHSPARAMETER VON DATEI-2         *
      *                 SETZEN LDATEI2 = 9 AM ENDE VON DATEI-2   EXT.  *
      *           M03 = VERARBEITUNG VDATEI1 KLEINER   VDATEI2   EXT.  *
      *           M04 = VERARBEITUNG VDATEI1 GLEICH    VDATEI2   EXT.  *
      *           M05 = VERARBEITUNG VDATEI1 GROESSER  VDATEI2   EXT.  *
      *    ----------------------------------------------------------  *
      ******************************************************************
       M00 SECTION.
       M00A.
           IF LDATEI1 = '1' PERFORM M01.
           IF LDATEI2 = '1' PERFORM M02.

           IF LDATEI1 = '9' AND
              LDATEI2 = '9' GO TO M00Z.

           IF LDATEI1 = '9' AND
              VDATEI1 NOT = HIGH-VALUE
              MOVE HIGH-VALUE TO VDATEI1.
           IF LDATEI2 = '9' AND
              VDATEI2 NOT = HIGH-VALUE
              MOVE HIGH-VALUE TO VDATEI2.

           IF LDATEI2 = '9' OR
              VDATEI1 LESS VDATEI2 PERFORM M03
              MOVE '1' TO LDATEI1
              IF LDATEI2 = '9' GO TO M00A
              ELSE MOVE '0' TO LDATEI2
                 GO TO M00A.

           IF LDATEI1 = '9' OR
              VDATEI2 LESS VDATEI1 PERFORM M05
              MOVE '1' TO LDATEI2
              IF LDATEI1 = '9' GO TO M00A
              ELSE MOVE '0' TO LDATEI1
                 GO TO M00A.

           PERFORM M04.
           MOVE '1' TO LDATEI1 LDATEI2.
           GO TO M00A.
       M00Z.
           EXIT.



      *****************************************************************
      *    U01 ZEITRÑUME AUS VORVORVORJAHR VORANSTELLEN               *
      *****************************************************************
       U01 SECTION.
       U01A.
      *
      *    &----- KORREKTUR ZEITRAUMENDE / NUR VVVJAHR ZULASSEN
      *
           IF ZWISVONJ = VVVJAHR
              IF ZWISBISJ GREATER VVVJAHR MOVE VVVJAHR TO ZWISBISJ
                 MOVE 12 TO ZWISBISM
                 MOVE '1' TO MERKSCHA
                 GO TO U01B
              ELSE GO TO U01B
              ELSE GO TO U01Z.
       U01B.
      *
      *    &----- EINSTELLEN IN LEICTABE
      *
           ADD 1 TO LEICZDVA.
           IF LEICZDVA GREATER 300
              MOVE 300 TO LEICZDVA
              MOVE LEICSCHL TO PACKKOMP
              PERFORM UNP
              DISPLAY 'MEHR ALS 300 VARIABLEN BEI ' PACKUNVP.

           MOVE ZWISLEIC TO LEICDATN (LEICZDVA).
      *
      *    &----- ZEITRAUMPRÅFUNG: 1 ZEITRAUM FÅR ANSCHLUSS VORHANDEN ?
      *
           IF MERKNOCP = '1' GO TO U01Z.

           IF MERKZEIT = ZERO MOVE ZWISZEIT TO MERKZEIT.

           IF ZWISZEIT = MERKZEIT ADD 1 TO MERKANZA.

           IF ZWISZEIT NOT = MERKZEIT OR
              MERKSCHA NOT = '1' OR
              ZWISBISM NOT = 12 MOVE '1' TO MERKNOCP.
       U01Z.
           EXIT.



      *****************************************************************
      *    U02 ZEITRÑUME AUS VORVORJAHR - LFDJAHR ÅBERTRAGEN     INDX *
      *****************************************************************
       U02 SECTION.
       U02A.
      *
      *    &----- EINSTELLEN IN LEICTABE
      *
           ADD 1 TO LEICZDVA.
           IF LEICZDVA GREATER 300
              MOVE 300 TO LEICZDVA
              MOVE LEICSCHL TO PACKKOMP
              PERFORM UNP
              DISPLAY 'MEHR ALS 300 VARIABLEN BEI ' PACKUNVP.

           MOVE LE1CDATN (INDX) TO LEICDATN (LEICZDVA).
       U02Z.
           EXIT.



      *****************************************************************
      *    W01 SCHREIBEN LEICSATZ (NEU)                               *
      *****************************************************************
       W01 SECTION.
       W01A.
           IF LEICZDVA GREATER MAXZDVA MOVE LEICZDVA TO MAXZDVA
              MOVE LEICSCHL TO MAXSCHL.

           WRITE LEICSATZ.
           IF NOT STATUS-OK
              IF KEY-DOPPELT
                 PERFORM UNP
                 DISPLAY '** DUPKEY (IGN): ' PACKUNVS PACKUNPO
                 GO TO M03Z
              ELSE DISPLAY '** Fehler  WRITE  LEIF3DX **' STATUS-BYTE
                 STOP '** Fehler WRITE LEIF3DX - ABBRECHEN'
                 GO TO A99.
       W01Z.
           EXIT.

-INC PDLEIPK

/*
// IF $RC GT 4 THEN
// GOTO    NOCAT
// EXEC LNKEDT
/. NOCAT
/&
* $$ EOJ
