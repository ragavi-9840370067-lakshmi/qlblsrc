000100130610/*******************************************************************/
000200130610/*PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE                */
000300130610/*******************************************************************/
000400130610/*******************************************************************/
000500130610/* Boopathy D* 2013/05/14   * RFS 117488 - Variable '&Active' will */
000600130610/*                            be set to 'N' for the below two con- */
000700130610/*                            ditions:                             */
000800130610/*                            - When DTAQ TRNPSTO is not present   */
000900130610/*                            - When job 'XXXGICSVR' is not active */
001000130610/*                               where XXX - Company Code          */
001100130610/*******************************************************************/
001200130610
001300130610             PGM        PARM(&ACTIVE)
001400000720             DCL        VAR(&ACTIVE) TYPE(*CHAR) LEN(1) VALUE('Y')
001500130610/* RFS117488 - Start */
001600130610             DCL        VAR(&JOB)       TYPE(*CHAR) LEN(10)
001700130610             DCL        VAR(&USER)      TYPE(*CHAR) LEN(10)
001800130610             DCL        VAR(&NBR)       TYPE(*CHAR) LEN(6)
001900130610             DCL        VAR(&CMPCDP)  TYPE(*CHAR) LEN(3)
002000130610             DCL        VAR(&JOBNAME) TYPE(*CHAR) LEN(10)
002100130610             DCL        VAR(&INJOBNAME) TYPE(*CHAR) LEN(10)
002200130610             DCL        VAR(&RETCDEJOB) TYPE(*CHAR) LEN(2) VALUE('00')
002300130610             DCLF       FILE(USRJOBI)
002400130610             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCDP)
002500130610             CHGVAR     VAR(&JOBNAME) VALUE(&CMPCDP *CAT 'GICSVR')
002600130610/* RFS117488 - End */
002700000720             CHGVAR VAR(&ACTIVE) VALUE('Y')
002800000720             CHKOBJ     OBJ(TRNPSTI) OBJTYPE(*DTAQ)
002900000720             MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&ACTIVE) +
003000000720                          VALUE('N'))
003100130610/* RFS117488 - Start */
003200130610             CHKOBJ     OBJ(TRNPSTO) OBJTYPE(*DTAQ)
003300130610             MONMSG     MSGID(CPF9801) EXEC(DO)
003400130610
003500130610               CHGVAR VAR(&ACTIVE) VALUE('N')
003600130610
003700130610               GOTO CMDLBL(ENDCLPGM)
003800130610
003900130610             ENDDO
004000130610
004100130610             CHGVAR     VAR(&INJOBNAME) VALUE(&JOBNAME)
004200130610             CALL       PGM(FXUSRJOBI7) PARM(&INJOBNAME &RETCDEJOB)
004300130610
004400130610             RCVF
004500130610             MONMSG     MSGID(CPF0864) EXEC(DO)
004600130610
004700130610                 CHGVAR VAR(&ACTIVE) VALUE('N')
004800130610                 GOTO CMDLBL(ENDCLPGM)
004900130610
005000130610             ENDDO
005100130610
005200130610/* RFS117488 - End */
005300000720             RETURN
005400130610ENDCLPGM:                   /* RFS117488 */
005500000720             ENDPGM
