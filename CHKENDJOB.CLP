000100050330/*------------------------------------------------------------------*/
000101070306/* Temp file USRJOBI created by FXUSRJOBI.                          */
000200070228/* To compile: create QTEMP/USRJOBI file:                           */
000300070305/* 1. CALL PGM(FXUSRJOBI) PARM('ORDERENTRY' 'QTEMP' 'USRJOBI' '00') */
000400070228/* 2. Copy QTEMP/USRJOBI to your library.                           */
000500070228/*------------------------------------------------------------------*/
000600070228/*                                                                  */
000700050404/* TITLE        : Check or End Jobs for a given Job Name and User ID*/
000800050404/*              :                                                   */
000900050330/* PROGRAM TYPE : CL                                                */
001000050330/* PROGRAMMER   : Karen Eng                                         */
001100050404/* PROGRAM      : CHKENDJOB                                         */
001200050404/* DESCRIPTION  : This program will check or end jobs that matches  */
001300050404/*              : the given JobName and UserID (default to *ALL).   */
001400050404/*              : If the UserID is not applicable, leave it blank,  */
001500050404/*              : the program will check all this jobs for any user.*/
001600050404/*              : Action: CHK - Check the status of the given Job   */
001700050404/*              :         Name for the User.                        */
001800050407/*              :             - Check the status of a given JobName */
001900050407/*              :               leave the User blank.               */
002000050404/*              :         END - End the given Job Name for the User.*/
002100050407/*              :               To end a JobName for all the users, */
002200050407/*              :               leave the User blank.               */
002300050330/* DATE CREATED : March 30, 2005                                    */
002400050330/*------------------ MODIFICATION HISTORY --------------------------*/
002500050330/* PROGRAMMER¦YYMMDD¦RFSNO¦                COMMENTS                 */
002600050330/*------------------¦-----------------------------------------------*/
002700050330/* Karen Eng ¦050330¦26354¦Creation of the program                  */
002800050330/*------------------------------------------------------------------*/
002900061129/* C.Lentini ¦061129¦38295¦ENDJOB cmd changed to use option *CNTRLD */
003000061129/*           ¦      ¦     ¦Use more specific monitor msg for ENGJOB */
003100061129/*------------------------------------------------------------------*/
003200070221/* Shu Yin   ¦070221¦34809¦Replaced the WRKACTJOB command with      */
003300070221/*           ¦      ¦     ¦FXUSRJOBI to use API programs.           */
003400070305/* Shu Yin   ¦070305¦34809¦Added &RTNCODE parm to call FXUSRJOBI    */
003500070305/*           ¦      ¦     ¦for future use.                          */
003501070614/* Shu Yin   ¦070611¦42539¦Changed the MONMSG at ENDJOB command     */
003502070614/*           ¦      ¦     ¦to monitor the message of "Job XXX/XXX/  */
003503070614/*           ¦      ¦     ¦XXX is already ending *CNTRLD."          */
003600070221/*------------------------------------------------------------------*/
003700050404             PGM        PARM(&ACTION &INJOBNAME &INJOBUSER +
003800050404                          &RETURNCODE)
003900050404
004000050404             DCL        VAR(&ACTION)      TYPE(*CHAR) LEN(3)
004100050404             DCL        VAR(&INJOBUSER)  TYPE(*CHAR) LEN(10)
004200050404             DCL        VAR(&INJOBNAME)  TYPE(*CHAR) LEN(10)
004300050404             DCL        VAR(&RETURNCODE) TYPE(*CHAR) LEN(1)
004400050404
004500050404             DCL        VAR(&JOBNAME)     TYPE(*CHAR) LEN(10)
004600050404             DCL        VAR(&JOBUSER)     TYPE(*CHAR) LEN(10)
004700050404             DCL        VAR(&JOBNUMBER)   TYPE(*CHAR) LEN(6)
004800070221/* RFS 34809 - Start */
004900070305             DCL        VAR(&RTNCODE) TYPE(*CHAR) LEN(2) VALUE('00')
005000070221/*           DCLF       FILE(TMPFIL150) */
005100070305             DCLF       FILE(USRJOBI)
005200070223
005300070223             CHGVAR     VAR(&RETURNCODE) VALUE(' ')
005400070223
005500070221/* RFS 34809 - End   */
005600050404
005700050404/* Default value: Action to CHK; User to *ALL.  */
005800050404             IF         COND(&ACTION *NE 'END') THEN(CHGVAR +
005900050404                          VAR(&ACTION) VALUE('CHK'))
006000050404             IF         COND(&INJOBUSER *EQ '  ') THEN(CHGVAR +
006100050404                          VAR(&INJOBUSER) VALUE('*ALL'))
006200050404
006300070221/* RFS 34809 - Start */
006400070221/*           DLTF       FILE(QTEMP/TMPFIL150)                    */
006500070221/*           MONMSG     MSGID(CPF0000)                           */
006600070221/*           CRTPF      FILE(QTEMP/TMPFIL150) RCDLEN(150)        */
006700070221/*           WRKACTJOB  OUTPUT(*PRINT)                           */
006800070221/*           CPYSPLF    FILE(QPDSPAJB) TOFILE(QTEMP/TMPFIL150) + */
006900070221/*                        SPLNBR(*LAST)                          */
007000050405
007100070221/*           DLTSPLF    FILE(QPDSPAJB) SPLNBR(*LAST)             */
007200050404
007300070221/*           OVRDBF     FILE(QPDSPAJB) TOFILE(QTEMP/TMPFIL150)   */
007400070221
007500070226/* Retrieve job info into work file QTEMP/USRJOBI.               */
007600070226             CALL       PGM(FXUSRJOBI) PARM(&INJOBNAME 'QTEMP' +
007700070305                          'USRJOBI' &RTNCODE)
007800070221
007900070221/* RFS 34809 - End   */
008000050330
008100050330 NEXT:       RCVF
008200050330             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDOFPGM))
008300050330
008400050330/* Build JobName, JobNumber, JobUser variables */
008500070221/* RFS 34809 - Start */
008600070221/*           CHGVAR     VAR(&JOBNAME) VALUE(%SST(&TMPFIL150 4 10))  */
008700070221/*           CHGVAR     VAR(&JOBUSER) VALUE(%SST(&TMPFIL150 17 10)) */
008800070221/*           CHGVAR     VAR(&JOBNUMBER) VALUE(%SST(&TMPFIL150 29 6))*/
008900070305             CHGVAR     VAR(&JOBNAME) VALUE(&QJOBNAME)
009000070305             CHGVAR     VAR(&JOBUSER) VALUE(&QJOBUSR)
009100070305             CHGVAR     VAR(&JOBNUMBER) VALUE(&QJOBNUM)
009200070221/* RFS 34809 - End   */
009300050404             IF         COND(&JOBNAME *NE &INJOBNAME) THEN(GOTO +
009400050330                          CMDLBL(NEXT))
009500050404
009600050404             IF         COND(&INJOBUSER *NE '*ALL' *AND &JOBUSER *NE +
009700050404                          &INJOBUSER) THEN(GOTO CMDLBL(NEXT))
009800050404
009900050404 CHKTHEJOB:
010000050404             IF         COND(&ACTION *EQ 'CHK') THEN(DO)
010100050404             CHGVAR     VAR(&RETURNCODE) VALUE('A')
010200050404             GOTO ENDOFPGM
010300050404             ENDDO
010400050330 ENDTHEJOB:
010500061129             ENDJOB     JOB(&JOBNUMBER/&JOBUSER/&JOBNAME) +
010600061129/*R38295 - Begin */       OPTION(*CNTRLD)
010700061129          /* MONMSG     MSGID(CPF0000) */
010800061129             MONMSG     MSGID(CPF1362 CPF1321)  +
010900061129                          EXEC(GOTO CMDLBL(NEXT))
010901070614/*RFS 42539 - Begin - Changed below monitored message.        */
011000070614/*           MONMSG     MSGID(CPC1231) EXEC(DO)               */
011001070614             MONMSG     MSGID(CPF1363) EXEC(DO)
011002070614/*RFS 42539 - End                                             */
011100061129                DLYJOB     DLY(10)
011200061129                GOTO       CMDLBL(ENDTHEJOB)
011300061129             ENDDO
011400061129 /*R38295 - End */
011500050404
011600050330             GOTO       CMDLBL(NEXT)
011700050330ENDOFPGM:
011800050404             ENDPGM
