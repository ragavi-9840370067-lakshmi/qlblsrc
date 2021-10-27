000001170313     /* %ATTR USRPRF(*OWNER) */
000101061011/* ******************************************************************/
000201061011/* Title     : Check libraries for object lock and allocate         */
000301061011/* Called By : FUNDSDOWN.                                           */
000401061011/*             JOBBCKAFTA                                           */
000501061011/*             JOBBCKBEFA                                           */
000601061011/*             JOBBCKBEFD                                           */
000701061011/*             JOBENVJRN                                            */
000801061011/* ******************************************************************/
000901061011/* MODIFICATIONS                                                    */
001001061011/* ******************************************************************/
001101061011/* Truong L.  * 2006/10/11   * RFS 35017 * Change the run priority  */
001201061012/*                           * and replace QTEMP for DLCF according */
001301061012/*                           * to Aldon standard.                   */
001401061026/* Truong L.  * 2006/10/26   * RFS 38014 * Change Aldon's creation  */
001501061026/*                           * option from *USER to *OWNER.         */
001601070108/* Ahmed Z.   * 2007/01/08   * RFS 39505 * Roll back the changes    */
001701070108/*            *              * done under RFS38716 by taking the    */
001801070108/*            *              * last version from archive.           */
001802070109/* Philip Ma  * 2007/01/09   * RFS 39504 - Insert modified version  */
001803070109/*            *              * of RFS38716 logic.                   */
001804071106/* Pawel A.   * 2007/09/06   * RFS 38974 Additional parameter       */
001805071106/*            *              * added to call SMOKEJOB               */
001901061011/* ******************************************************************/
002001970806PGM
002101970806             DCL        VAR(&CLEANAL) TYPE(*CHAR) LEN(1)
002201970806             DCL        VAR(&DBASE) TYPE(*CHAR) LEN(10)
002301970806             DCL        VAR(&TABLES) TYPE(*CHAR) LEN(10)
002401970806             DCL        VAR(&AUDIT) TYPE(*CHAR) LEN(10)
002402070109             DCL        VAR(&OWNER) TYPE(*CHAR) LEN(10) /*38716.1*/
002403071106 /*RFS 38974 Begin   */
002404071106             DCL        VAR(&LCKTYP) TYPE(*CHAR) LEN(10) VALUE(' ')
002405071106 /*RFS 38974 End     */
002406071106
002601061012/*R35017     DCLF       FILE(QTEMP/TEMP)   No QTEMP in Aldon */
002701061012/*R35017*/   DCLF       FILE(QAFDBASI)
002801061011
002901061011/*R35017*/   CHGJOB     RUNPTY(12)
003001061011
003101970806             RTVDTAARA  DTAARA(MFADBASE) RTNVAR(&DBASE)
003201970806             RTVDTAARA  DTAARA(MFAAUDIT) RTNVAR(&AUDIT)
003301970806             RTVDTAARA  DTAARA(MFATABLES) RTNVAR(&TABLES)
003302070109             RTVOBJD    OBJ(&DBASE/MFATRNP) OBJTYPE(*FILE) OWNER(&OWNER) +
003303070109                          /*38716.1*/
003401970806
003501970806             DSPFD      FILE(&DBASE/*ALL) TYPE(*BASATR) +
003601970806                          OUTPUT(*OUTFILE) FILEATR(*PF) +
003701970806                          OUTFILE(QTEMP/TEMP)
003801970806             DSPFD      FILE(&TABLES/*ALL) TYPE(*BASATR) +
003901970806                          OUTPUT(*OUTFILE) FILEATR(*PF) +
004001970806                          OUTFILE(QTEMP/TEMP) OUTMBR(*FIRST *ADD)
004101970806             DSPFD      FILE(&AUDIT/*ALL) TYPE(*BASATR) +
004201970806                          OUTPUT(*OUTFILE) FILEATR(*PF) +
004301970806                          OUTFILE(QTEMP/TEMP) OUTMBR(*FIRST *ADD)
004401061012
004501061012/*R35017*/   OVRDBF     FILE(QAFDBASI) TOFILE(QTEMP/TEMP)
004601061012
004701970806 NEXT:       RCVF
004801061012/*R35017     MONMSG     MSGID(CPF0000) EXEC(RETURN)    */
004901061012/*R35017*/   MONMSG     MSGID(CPF0864) EXEC(GOTO CMDLBL(PGM_EXIT))
005001970806             IF COND(&ATFILE *EQ 'MFAUSRSCP') THEN(DO)
005101970806               GOTO CMDLBL(NEXT)
005201970806             ENDDO
005301970806 ALLOCATE:   CHGVAR VAR(&CLEANAL) VALUE('Y')
005401970806             ALCOBJ     OBJ((&ATLIB/&ATFILE *FILE *EXCL))
005402070109
005403070111             /* RFS 39504 & 38716.1 - Begins */
005501070109             /* MONMSG     MSGID(CPF0000) EXEC(CHGVAR VAR(&CLEANAL) +
005601070109                          VALUE('N'))   */
005701070109             MONMSG     MSGID(CPF0000) EXEC(DO)
005703070109                CHGVAR     VAR(&CLEANAL) VALUE('N')
005705070109                GOTO       CMDLBL(SMOKEJOB)
005707070109             ENDDO
005709070109
005710070111             CHGOBJOWN  OBJ(&ATLIB/&ATFILE) OBJTYPE(*FILE) NEWOWN(&OWNER) +
005711070111                          /*38716.1*/
005714070111             MONMSG     MSGID(CPF22BE CPF2226 CPF3202) EXEC(CHGVAR +
005715070111                          VAR(&CLEANAL) VALUE('N')) /*38716.1*/
005719070111             /* RFS 39504 & 38716.1 - Ends   */
005720070109
005801070109 SMOKEJOB:   IF         COND(&CLEANAL *NE 'Y') THEN(DO)
005802071106 /*RFS 38974 Begin   */
005803071106     /*         CALL       PGM(SMOKEJOB) PARM(&ATFILE &ATLIB)  */
005804071106             CALL       PGM(SMOKEALL) PARM(&ATFILE &ATLIB &LCKTYP)
005805071106 /*RFS 38974 End     */
006001970806             GOTO CMDLBL(ALLOCATE)
006101970806             ENDDO
006201970806             GOTO CMDLBL(NEXT)
006301061012
006401061012 /*R35017 - Start*/
006501061012 PGM_EXIT:
006601061012             DLTOVR     FILE(QAFDBASI)
006701061012             MONMSG     MSGID(CPF9841)
006801061012 /*R35017 - End */
006901061012
007001061012             ENDPGM
