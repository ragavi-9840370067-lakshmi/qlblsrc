000100190401   /*****************************************************************/
000200190401   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
000300190401   /*****************************************************************/
000400190401   /*****************************************************************/
000500190401   /* JOHN M     * 2005/11/20   *  RFS 31432 - Moved reports into a */
000600190401   /*                           *  CL program as fix for V5R3 and   */
000700190401   /*                              a Powerhouse issue.              */
000800190401   /* BALU B     * 2018/03/29   *  RFS 181848- QUIZ REWRITE FOR     */
000900190401   /*                              DERGNLS1 AND DERGNLS             */
001000190521   /* BALU B     * 2019/05/14   *  RFS183849- Override USERDATA with*/
001100190717   /*            *              *             report name DERGNLS   */
001200190717   /* PRIYA N    * 2019/07/17   *  RFS1016281- Modified OVRPRTF     */
001300190717   /*            *              *  command with SPLFNAME to change  */
001400190717   /*            *              *  naming convention of empty report*/
001500190717   /*            *              *  to report data name to push to   */
001600190717   /*            *              *  client case folders.             */
001700190401   /*****************************************************************/
001800190401
001900190401PGM
002000190401
002100190401/* RFS181848 - BEGIN */
002200190401DCL        VAR(&SPEXIST)  TYPE(*CHAR) LEN(1) VALUE(' ')
002300190401DCL        VAR(&PHEXIST)  TYPE(*CHAR) LEN(1) VALUE(' ')
002400190401DCL        VAR(&OFRECCNT) TYPE(*DEC)  LEN(10 0)
002500190401
002600190401CHGVAR     VAR(&SPEXIST) VALUE('Y')
002700190401RTVDTAARA  DTAARA(PHEXIST *ALL) RTNVAR(&PHEXIST)
002800190401
002900190401DSPDTAARA  DTAARA(*LDA)
003000190401
003100190401QTP  AUTO(PTDERGNLS1)
003200190401
003300190401CHKOBJ     OBJ(SPDERGNLS) OBJTYPE(*PGM)
003400190401MONMSG     MSGID(CPF9801) EXEC(CHGVAR VAR(&SPEXIST) VALUE('N'))
003500190401   IF         COND(&PHEXIST = ' ' *AND &SPEXIST = 'Y') THEN(DO)
003600190401      RUNSQL     SQL('CALL SPDERGNLS')
003700190401      RTVMBRD    FILE(OFDERGNLS) NBRCURRCD(&OFRECCNT)
003800190401      MONMSG     MSGID(CPF0000)
003900190401      IF         COND(&OFRECCNT > 0) THEN(DO)
004000190401         RUNCMD  CMD('OVRPRTF FILE(SQLPRT1) MAXRCDS(*NOMAX) +
004100190401                 USRDTA(&SPLFNAME); REPORT REPORT(RPDERGNLS)  +
004200190401                 VIEW(VWDERGNLS) SAVE(*YES) SETVAR((&&DEALRREPCD +
004300190401                 &DEALRREPCD))') SQL('SELECT DISTINCT DEALRREPCD +
004400190401                 NAME(DEALRREPCD), "DERGNLS" NAME(SPLFNAME) FROM +
004500190401                 OFDERGNLS')
004600190401      ENDDO
004700190401      ELSE       (DO)
004800190401         CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('DERGNLS   ')
004900190401         RUNSQL     SQL('CALL SPNORECRPT')
005000190514/*RFS183849  STARTS  */
005100190513/*       REPORT     REPORT(RPNORECRPT) SAVE(*YES)                     */
005200190513/*       CHGSPLFA   FILE(RPNORECRPT) SPLNBR(*LAST) USRDTA('DERGNLS')  */
005300190717/*RFS1016281 STARTS  */
005400190717/*       OVRPRTF    FILE(SQLPRT1) MAXRCDS(*NOMAX) USRDTA('DERGNLS') */
005500190717         OVRPRTF    FILE(SQLPRT1) MAXRCDS(*NOMAX) USRDTA('DERGNLS') +
005600190717                    SPLFNAME(RPDERGNLS)
005700190717/*RFS1016281 ENDS  */
005800190513         REPORT     REPORT(RPNORECRPT)
005900190517         DLTOVR     FILE(SQLPRT1)
006000190514/*RFS183849 ENDS */
006100190401      ENDDO
006200190401   ENDDO
006300190401   ELSE       CMD(DO)
006400190401/* RFS181848 - END   */
006500190401
006600190401   QUIZ AUTO(DERGNLS1)
006700190401  ENDDO
006800190401ENDPGM
