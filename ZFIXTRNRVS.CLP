000100980708/*---------------------------------------------------------------*/
000200980708/* This cl program reverses all the transactions (TRANS) that    */
000300980708/* have been saved in the subfile 'TEMP'.                        */
000400980708/*                                                               */
000500980709/* The following are descriptions and explanations of what each  */
000600980709/* of the fields and the file, TEMP, should contain.             */
000700980709/*                                                               */
000800980709/*      TEMP       - Is a subfile with the same layout as        */
000900980709/*                   the TRANS file and must only contain all    */
001000980709/*                   the TRANS records you want reversed!!       */
001100980709/*      &F0000     - PLACEMENT-DATE OF TEMP                      */
001200980709/*      &F0002     - TRANS-NO OF TEMP                            */
001300980709/*      &PROCDATE  - The current process date.                   */
001400980709/*                   AS-AT-DATE OF PROCESS-DATE   (DON'T CHANGE) */
001500980709/*      &P0        - Rejection Code,         '00' (DON'T CHANGE) */
001600980709/*      &P1        - User,                   'SYSRVS'            */
001700980709/*      &P2        - Reversal Code,          'PROC'              */
001800980709/*                   Check the REVERSAL-REASON table and         */
001900980709/*                   select a code that best describes why       */
002000980709/*                   you are reversing the transactions.         */
002100980709/*      &P3        - Suppress Confirmation,   'Y' (USUALLY)      */
002200980709/*      &P4        - Hidden Trans,            'Y' (USUALLY)      */
002300980709/*                                                               */
002400980709/*---------------------------------------------------------------*/
002500980709/*                                                               */
002600980709/*          NOTE:    *****  IMPORTANT!!!!    ******              */
002700980709/*          The process date (&PROCDATE) that must be used       */
002800980709/*          to pass to the COBOL program, TRNRVS, is the         */
002900980709/*          AS-AT-DATE of PROCESS-DATE file and                  */
003000980709/*          NOT the process date of the transaction.             */
003100980709/*                                                               */
003200980708/*---------------------------------------------------------------*/
003300980709
003400950705             PGM
003500950705             DCL        VAR(&P0) TYPE(*CHAR) LEN(02) VALUE('00')
003600950705             DCL        VAR(&P1) TYPE(*CHAR) LEN(10) VALUE('SYSRVS')
003700980610             DCL        VAR(&P2) TYPE(*CHAR) LEN(04) VALUE('PROC')
003800950705             DCL        VAR(&P3) TYPE(*CHAR) LEN(01) VALUE('Y')
003900950705             DCL        VAR(&P4) TYPE(*CHAR) LEN(01) VALUE('Y')
004000980522             DCL        VAR(&PT) TYPE(*CHAR) LEN(08)
004100980522             DCL        VAR(&TN) TYPE(*CHAR) LEN(07)
004200980522             DCL        VAR(&PD) TYPE(*CHAR) LEN(08)
004300980610             DCL        VAR(&PROCDATE)  TYPE(*DEC)  LEN(08)
004400950705             DCLF       FILE(TEMP)
004500980709
004600980709/*---------------------------------------------------------------*/
004700980709/* Set the process date to the current process date on the       */
004800980709/* PROCESS-DATE file.                                            */
004900980709/*---------------------------------------------------------------*/
005000980709
005100980610             RTVDTAARA  DTAARA(MFAPRCDTP (1 8))  RTNVAR(&PD)
005200980610             CHGVAR     VAR(&PROCDATE) VALUE(&PD)
005300980709
005400950705 START:      RCVF
005500950705             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDPGM))
005600980610             CALL       PGM(TRNRVS) PARM(&F0000 &F0002 &PROCDATE +
005700980610                                         &P0 &P1 &P2 &P3 &P4)
005800980709
005900980709/*---------------------------------------------------------------*/
006000980709/* The following logic is for reporting/displaying any           */
006100980709/* transactions that did not get processed/reversed, meaning     */
006200980709/* the rejection code was not equal to '00'.                     */
006300980709/*---------------------------------------------------------------*/
006400980709
006500980522             CHGVAR     VAR(&PT) VALUE(&F0000)
006600980522             CHGVAR     VAR(&TN) VALUE(&F0002)
006700980610             CHGVAR     VAR(&PD) VALUE(&PROCDATE)
006800980522             CHGDTAARA  DTAARA(*LDA (1 8)) VALUE(&PT)
006900980522             CHGDTAARA  DTAARA(*LDA (9 7)) VALUE(&TN)
007000980522             CHGDTAARA  DTAARA(*LDA (17 8)) VALUE(&PD)
007100980522             CHGDTAARA  DTAARA(*LDA (25 2)) VALUE(&P0)
007200980522             CHGDTAARA  DTAARA(*LDA (27 10)) VALUE(&P1)
007300980522             CHGDTAARA  DTAARA(*LDA (37 4)) VALUE(&P2)
007400980522             CHGDTAARA  DTAARA(*LDA (41 1)) VALUE(&P3)
007500980522             CHGDTAARA  DTAARA(*LDA (42 1)) VALUE(&P4)
007600980522
007700010105             IF COND(&P0 *EQ '00') THEN(GOTO CMDLBL(START))
007800980522             DSPDTAARA  DTAARA(*LDA)
007900950705             GOTO CMDLBL(START)
008000980709
008100980522 ENDPGM:     RETURN
