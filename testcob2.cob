
PROCESS APOST,NOADV,NODYNAM,NOFLAGSTD,WORD(RWT)
       ID DIVISION.
      *----------------------------------------------------------------
      * LEIX275   ARC=L001   * 25.02.13 10:33:24 KLS
      *----------------------------------------------------------------
      ******************************************************************
      * -------------------------------------------------------------- *
      *  L E I X 2 7 5         ** STATISTIK NICHT AUSGEZAHLTER FAELLE  *
      * -------------------------------------------------------------- *
      *                           STAND ENDE LETZTER ARBEITSTAG/MONAT  *
      *                           LEIOHERK-TEMP-VERW: (K=KV A=AR)      *
      *                                                                *
      *                           E I N G A B E ---------------------- *
      *                           LEIEIN      : LEIF1D-KOPIE           *
1385  *                           BZDA8/ADRDA7: VERWALTUNGSSTELLE      *
      *                                                                *
      *                           A U S G A B E ---------------------- *
      *                           LEIAUS      : STATISTIK-DATEI        *
      * -------------------------------------------------------------- *
      *  SCHALTER                 UPSI-0-ON:    ALLE                   *
      *                           UPSI-1-ON:    KV-MASCHINELL          *
      *                           UPSI-2-ON:    KV-MANUELL             *
      *                           UPSI-3-ON:    AR-MASCHINELL          *
      *                           UPSI-4-ON:    AR-MANUELL             *
      * -------------------------------------------------------------- *
      *  06/03/92 SCHMID          ERSTELLUNG                           *
      *  04/06/93                 AENDERUNG ADRESSDATEN / PLZ          *
      *  20/02/01                 UPSI FÅR DIFFERENZIERUNG             *
      *  29/03/06                 ADR7                                 *
     *  24/06/08                 NEUE DATENSTRUKTUREN                 *
P1385 *  25/02/13 KLS             UMSTELLUNG VON BZDA7 AUF BZDA8       *
      * -------------------------------------------------------------- *
      ******************************************************************
       PROGRAM-ID. LEIX275.
       ENVIRONMENT   DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
-INC SPECNAME                                                                  %
       INPUT-OUTPUT  SECTION.
       FILE-CONTROL.
           SELECT ADRDA7  ASSIGN TO    ADRDA7
                          ORGANIZATION INDEXED
                          ACCESS       RANDOM
                          FILE STATUS  STATUS-BYTE
                          RECORD KEY   A-KEY.
1385       SELECT BZDA8   ASSIGN TO    BZDA8
                          ORGANIZATION INDEXED
                          ACCESS       RANDOM
                          FILE STATUS  STATUS-BYTE
                          RECORD KEY   BZ-KEY.
           SELECT LEIEIN  ASSIGN TO    LEIEIN.
           SELECT LEIAUS  ASSIGN TO    LEIAUS.
       DATA DIVISION.
       FILE SECTION.
       FD         ADRDA7
                  LABEL RECORDS   STANDARD.
-INC ADRSATZN                                                                  %

1385   FD         BZDA8
                  LABEL RECORDS   STANDARD.
-INC BZSATZ8                                                                   %

       FD         LEIEIN
                  RECORDING     F
                  RECORD      220 CHARACTERS
                  BLOCK      7920 CHARACTERS
                  LABEL RECORDS   STANDARD.
-INC LEIOSATX                                                                  %

       FD         LEIAUS
                  RECORDING     F
                  RECORD       80 CHARACTERS
                  BLOCK      8000 CHARACTERS
                  LABEL RECORDS   STANDARD.
       01         AUSGABE         PIC X(80).

       WORKING-STORAGE SECTION.

       01          FILLER.
           05      ANZUPSI         PIC 9                   VALUE ZERO.
           05      VWST            PIC 9                   VALUE ZERO.
           05      VTYP            PIC 9                   VALUE ZERO.
           05      ANZXERL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZSERL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZRERL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZFERL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZAERL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZXFEL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZSFEL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZRFEL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZFFEL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      ANZAFEL         PIC S9(5)        COMP-3 VALUE ZERO.
           05      SUMXVBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMSVBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMRVBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMFVBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMAVBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMXRBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMSRBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMRRBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMFRBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMARBT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMXERT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMSERT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMRERT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMFERT         PIC S9(9)V99     COMP-3 VALUE ZERO.
           05      SUMAERT         PIC S9(9)V99     COMP-3 VALUE ZERO.

       01          POST.
           05      POSTTXT1        PIC X(9).
           05      POSTTXT2        PIC X(9).
           05      POSTFERT        PIC Z(6)-.
           05      POSTFEHL        PIC Z(6)-.
           05      POSTRBTR        PIC Z(7)9,99-.
           05      POSTERBT        PIC Z(7)9,99-.
           05      POSTVBTR        PIC Z(7)9,99-.

-INC U100C80                                                                   %
-INC VSSTATUS                                                                  %
-INC WSLEIBA                                                                   %
-INC WSLEIBV                                                                   %
-INC WSDATUM                                                                   %

       PROCEDURE DIVISION.

-INC PDDATUM                                                                   %

           CALL  'UPRO100' USING U100SATZ.

           OPEN   INPUT LEIEIN OUTPUT LEIAUS.

           OPEN   INPUT ADRDA7.
           IF     NOT   STATUS-OK
                  STOP '** FEHLER OPEN ADRDA7 - ABBRECHEN **'
                  GO TO A99.
1385       OPEN   INPUT BZDA8.
           IF     NOT   STATUS-OK
1385              STOP '** FEHLER OPEN BZDA8  - ABBRECHEN **'
                  GO TO A99.

           IF     UPSI-0-ON       ADD 1 TO ANZUPSI.
           IF     UPSI-1-ON       ADD 1 TO ANZUPSI.
           IF     UPSI-2-ON       ADD 1 TO ANZUPSI.
           IF     UPSI-3-ON       ADD 1 TO ANZUPSI.
           IF     UPSI-4-ON       ADD 1 TO ANZUPSI.

           IF     ANZUPSI   NOT = 1
                  STOP '** FEHLER UPSI-KONST. - ABBRECHEN **'
                  GO TO A99.
       A01.
           READ   LEIEIN AT END                      GO TO A99.
      *
      *    &----- ZULÑSSIGKEIT GENERELL / UPSI
      *
           IF     LEIOSART NOT  =  ZERO              GO TO A01.
           IF     LEIOVSNR      =  ZERO              GO TO A01.
           IF     LEIORMON      =  MM                GO TO A01.
           IF     LEIOINAC NOT  =  ZERO              GO TO A01.
           IF     LEIOTRAN NOT  =  ZERO              GO TO A01.
           IF     LEIOTDAT NOT  =  ZERO              GO TO A01.

           IF     LEIOMANU      =  'R' OR 'S' MOVE  'X' TO LEIOMANU.

           IF     UPSI-0-ON                          GO TO A01A.

           IF     UPSI-1-ON
            IF    LEIOHERK      =  'K'        AND
                  LEIOMANU NOT  =  'X'               GO TO A01A
            ELSE                                     GO TO A01.

           IF     UPSI-2-ON
            IF    LEIOHERK      =  'K'        AND
                  LEIOMANU      =  'X'               GO TO A01A
            ELSE                                     GO TO A01.

           IF     UPSI-3-ON
            IF    LEIOHERK NOT  =  'K'        AND
                  LEIOMANU NOT  =  'X'               GO TO A01A
            ELSE                                     GO TO A01.

           IF     UPSI-4-ON
            IF    LEIOHERK NOT  =  'K'        AND
                  LEIOMANU      =  'X'               GO TO A01A
            ELSE                                     GO TO A01.

           GO TO A01.
       A01A.
           MOVE   1 TO VWST VTYP.
           IF     LEIOFSCH      =  '1'               MOVE 2 TO VTYP.

           IF     LEIOSOND      =  '1'               GO TO A02.

           MOVE   LEIOVSNR TO ADBAVSNR.
           PERFORM U06.
           IF     ADBASTAT = '2' OR '4'
                  STOP '** FEHLER LESEN BZ/ADR - ABBRECHEN **'
                  GO TO A99.

           IF     ADBASTAT NOT = ZERO                GO TO A02.

           MOVE   A-KREIS         TO BEZVERW.
           IF     RAVEN    MOVE 2 TO VWST.
           IF     FREIB    MOVE 3 TO VWST.
           IF     AUGSB    MOVE 4 TO VWST.
       A02.
      *
      *    &----- AUFADDIEREN RBTR/VBTR
      *
           ADD    LEIOGESV TO SUMXVBT.
           ADD    LEIOGESR TO SUMXRBT.
           ADD    LEIOGESE TO SUMXERT.

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

           GO TO A01.
       A99.
           MOVE   SPACE         TO  POST.

           IF     UPSI-0-ON         MOVE 'ALLE'           TO POSTTXT1.
           IF     UPSI-1-ON         MOVE 'KV-MASCH'       TO POSTTXT1.
           IF     UPSI-2-ON         MOVE 'KV-MAN'         TO POSTTXT1.
           IF     UPSI-3-ON         MOVE 'AR-MASCH'       TO POSTTXT1.
           IF     UPSI-4-ON         MOVE 'AR-MAN'         TO POSTTXT1.

           MOVE  'FELLBACH'     TO POSTTXT2.
           MOVE   ANZSERL       TO POSTFERT.
           MOVE   ANZSFEL       TO POSTFEHL.
           MOVE   SUMSVBT       TO POSTVBTR.
           MOVE   SUMSRBT       TO POSTRBTR.
           MOVE   SUMSERT       TO POSTERBT.
           PERFORM WRT.

           MOVE  'RAVENSBG'     TO POSTTXT2.
           MOVE   ANZRERL       TO POSTFERT.
           MOVE   ANZRFEL       TO POSTFEHL.
           MOVE   SUMRVBT       TO POSTVBTR.
           MOVE   SUMRRBT       TO POSTRBTR.
           MOVE   SUMRERT       TO POSTERBT.
           PERFORM WRT.

           MOVE  'FREIBURG'     TO POSTTXT2.
           MOVE   ANZFERL       TO POSTFERT.
           MOVE   ANZFFEL       TO POSTFEHL.
           MOVE   SUMFVBT       TO POSTVBTR.
           MOVE   SUMFRBT       TO POSTRBTR.
           MOVE   SUMFERT       TO POSTERBT.
           PERFORM WRT.

           MOVE  'AUGSBURG'     TO POSTTXT2.
           MOVE   ANZAERL       TO POSTFERT.
           MOVE   ANZAFEL       TO POSTFEHL.
           MOVE   SUMAVBT       TO POSTVBTR.
           MOVE   SUMARBT       TO POSTRBTR.
           MOVE   SUMAERT       TO POSTERBT.
           PERFORM WRT.

           MOVE  'GESAMT--'     TO POSTTXT2.
           MOVE   ANZXERL       TO POSTFERT.
           MOVE   ANZXFEL       TO POSTFEHL.
           MOVE   SUMXVBT       TO POSTVBTR.
           MOVE   SUMXRBT       TO POSTRBTR.
           MOVE   SUMXERT       TO POSTERBT.
           PERFORM WRT.

           MOVE   ALL '-'       TO POST.
           PERFORM WRT.

1385       CLOSE  LEIEIN LEIAUS BZDA8 ADRDA7.
           STOP   RUN.
      *****************************************************************
      *    WRT AUSGABESATZ SCHREIBEN                                  *
      *****************************************************************
       WRT SECTION.
       WRTA.
           WRITE AUSGABE FROM POST.
       WRTZ.
           EXIT.

-INC PDLEIB8                                                                   %
-INC UPRO100X                                                                  %
       END PROGRAM LEIX275.
