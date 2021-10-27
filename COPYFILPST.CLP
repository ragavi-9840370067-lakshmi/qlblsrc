000100080314 COPYFILPST:  PGM
000200080314
000300080314     /*****************************************************************/
000400080314     /*                                                               */
000500080314     /*    COPYFILPST: Canada Post Load                               */
000600080314     /*                                                               */
000700080314     /*    PURPOSE:    Copy new Canada post file and create a new     */
000800080314     /*                updated version of file MFAADDSAP.             */
000900080314     /*                                                               */
001000080314     /*    CREATED:    03/25/98(JSI)                                  */
001100080314     /*    CHANGED:    03/25/98(TH)                                   */
001200080314     /*                                                               */
001300080314     /*****************************************************************/
001400080314
001500080314     /*****************************************************************/
001600080314     /*                                                               */
001700080314     /*               C H A N G E   H I S T O R Y                     */
001800080314     /*                                                               */
001900080314     /*****************************************************************/
002000080314     /*****************************************************************/
002100080314     /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
002200080314     /*****************************************************************/
002300080314     /* T.HAMID    *  1998/03/25  * RFS LOG # 2115                    */
002400080314     /*            *              * - Modified to create duplicate    */
002500080314     /*            *              *   object and call Cobol PGM       */
002600080314     /*            *              *   that check for duplicate record */
002700080314     /*            *              *   and copy file with records that */
002800080314     /*                           *   manually entered                */
002900080314     /* RICHARD G  *  2007/10/31  * RFS LOG # 40867                   */
003000080314     /*            *              * - Added logic to prevent locking  */
003100080314     /* RICHARD G  *  2007/12/10  * RFS LOG # 46143                   */
003200080314     /*            *              * Added logic to change library for */
003300080314     /*            *              * Canada Post to prevent locking    */
003400080314     /*****************************************************************/
003500080314
003600080314 /*****                                                              */
003700080314 /*****    Work items                                                */
003800080314 /*****                                                              */
003900080314
004000080314 /*40867*/   DCL        VAR(&COUNTER) TYPE(*DEC) LEN(2 0) VALUE(0)
004100080314 /*46143*/   DCL        VAR(&PSTCD)     TYPE(*CHAR) LEN(8)
004200080314 /*46143*/   DCL        VAR(&SYSDAT)    TYPE(*CHAR) LEN(08)
004300080314
004400080314             DCL        VAR(&KEY)   +
004500080314                        TYPE(*CHAR) +
004600080314                        LEN(4)
004700080314
004800080314             DCL        VAR(&REPLY) +
004900080314                        TYPE(*CHAR) +
005000080314                        LEN(1)
005100080314
005200080314             DCL        VAR(&MSG)   +
005300080314                        TYPE(*CHAR) +
005400080314                        LEN(200)
005500080314
005600080314 /*40867*/   CHGVAR     VAR(&COUNTER) VALUE(0)
005700080314 /*****                                                               */
005800080314 /*****    Check to see if object exist or not                        */
005900080314 /*****                                                               */
006000080314
006100080314 LBL_0010:
006200080314
006300080314             CHKOBJ     OBJ(MFASYSWIDE/MFAADDSAP) +
006400080314                        OBJTYPE(*FILE)            +
006500080314                        MBR(*FIRST)
006600080314
006700080314             MONMSG     MSGID(CPF9801)            +
006800080314                        EXEC(GOTO CMDLBL(LBL_0050))
006900080314
007000080314 /*****                                                               */
007100080314 /*****    Copying the new Canada Post file from library TRANSFER     */
007200080314 /*****    to library MFASYSWIDE replacing old records.               */
007300080314 /*****                                                               */
007400080314
007500080314 LBL_0020:
007600080314
007700080314              CPYF       FROMFILE(TRANSFER/MFAADDSAP) +
007800080314                            TOFILE(MFASYSWIDE/MFAADDSAP) +
007900080314                            MBROPT(*REPLACE) +
008000080314                            INCCHAR(*RCD 180 *NE 'D')
008100080314
008200080314              MONMSG     MSGID(CPF2817 CPF2869) +
008300080314                         EXEC(GOTO CMDLBL(LBL_0030))
008400080314
008500080314 /*40867*/    MONMSG     MSGID(CPC1231 CPF2909) EXEC(DO)
008600080314 /*40867*/    IF COND(&COUNTER     < 10)    THEN(DO)
008700080314 /*40867*/       DLYJOB     DLY(5)
008800080314 /*40867*/       CHGVAR VAR(&COUNTER) VALUE(&COUNTER + 1)
008900080314 /*40867*/       GOTO       CMDLBL(LBL_0020)
009000080314 /*40867*/       ENDDO
009100080314 /*40867*/    ELSE
009200080314 /*40867*/       GOTO       CMDLBL(LBL_0060)
009300080314 /*40867*/    ENDDO
009400080314
009500080314 /*****                                                               */
009600080314 /*****    Delete TEMPFILE. TEMPFILE was created by previous run.     */
009700080314 /*****    By deleting TEMPFILE program can create duplicate object.  */
009800080314 /*****    TEMPFILE is a temp file created to be used by CBL program  */
009900080314 /*****    CALL later in this program.                                */
010000080314 /*****                                                               */
010100080314
010200080314              DLTF       FILE(QTEMP/TEMPFILE)
010300080314              MONMSG     (CPF2105)
010400080314              CRTDUPOBJ  OBJ(MFAADDSAP) FROMLIB(MFASYSWIDE) +
010500080314                            OBJTYPE(*FILE) TOLIB(QTEMP) +
010600080314                            NEWOBJ(TEMPFILE) DATA(*NO)
010700080314
010800080314 /*****                                                           */
010900080314 /*****    EXECUTE CBL program to copy previously entered         */
011000080314 /*****    manual address.                                        */
011100080314 /*****                                                           */
011200080314
011300080314 /*40867*/   CHGVAR     VAR(&COUNTER) VALUE(0)
011400080314 LBL_0025:                                                 /*40867*/
011500080314              CLRPFM     FILE(MFASYSWIDE/MFAADDSDP)
011600080314/*R46143*/   OVRDBF     FILE(MFAADDSMP) TOFILE(MFASYSWIDE/MFAADDSMP) +
011700080314/*R46143*/                OVRSCOPE(*JOB)
011800080314/*R46143*/   OVRDBF     FILE(MFAADDSAP) TOFILE(MFASYSWIDE/MFAADDSAP) +
011900080314/*R46143*/                OVRSCOPE(*JOB)
012000080314/*R46143*/   OVRDBF     FILE(MFAADDSDP) TOFILE(MFASYSWIDE/MFAADDSDP) +
012100080314/*R46143*/                OVRSCOPE(*JOB)
012200080314              CALL       PGM(ADDRUPDATE)
012300080314/*R46143*/   DLTOVR     FILE(MFAADDSMP) LVL(*JOB)
012400080314/*R46143*/   DLTOVR     FILE(MFAADDSAP) LVL(*JOB)
012500080314/*R46143*/   DLTOVR     FILE(MFAADDSDP) LVL(*JOB)
012600080314 LBL_0027:                                                 /*40867*/
012700080314              CPYF       FROMFILE(QTEMP/TEMPFILE) +
012800080314                            TOFILE(MFASYSWIDE/MFAADDSMP) +
012900080314                            MBROPT(*REPLACE)
013000080314
013100080314              MONMSG     MSGID(CPF2817 CPF2869) +
013200080314                            EXEC(DO)
013300080314              CLRPFM     FILE(MFASYSWIDE/MFAADDSMP)
013400080314              ENDDO
013500080314
013600080314 /*40867*/    MONMSG     MSGID(CPC1231 CPF2909) EXEC(DO)
013700080314 /*40867*/    IF COND(&COUNTER     < 10) THEN(DO)
013800080314 /*40867*/       DLYJOB     DLY(5)
013900080314 /*40867*/       CHGVAR VAR(&COUNTER) VALUE(&COUNTER + 1)
014000080314 /*40867*/       GOTO       CMDLBL(LBL_0027)
014100080314 /*40867*/       ENDDO
014200080314 /*40867*/    ELSE
014300080314 /*40867*/       GOTO       CMDLBL(LBL_0070)
014400080314 /*40867*/    ENDDO
014500080314 /*46143*/    CALL       PGM(GETSYSDATE) PARM(&SYSDAT)
014600080314 /*46143*/    CHGDTAARA  DTAARA(MFASYSWIDE/MFAPSTCD) VALUE(&SYSDAT)
014700080314              GOTO       ENDCLPGM
014800080314
014900080314 /*****                                                               */
015000080314 /*****    If  file MFAADDSAP in library is empty, send message to    */
015100080314 /*****    SYSOPR to CANCEL or RETRY.                                 */
015200080314 /*****                                                               */
015300080314
015400080314 LBL_0030:
015500080314
015600080314              CHGVAR     VAR(&MSG) +
015700080314                         VALUE('File MFAADDSAP in +
015800080314                               library TRANSFER is EMPTY +
015900080314                               Enter "C" to CANCEL')
016000080314              SNDPGMMSG  MSG(&MSG) +
016100080314                         TOMSGQ(*SYSOPR) +
016200080314                         MSGTYPE(*INQ) +
016300080314                         KEYVAR(&KEY)
016400080314              RCVMSG     MSGQ(*PGMQ) +
016500080314                         MSGTYPE(*RPY) +
016600080314                         MSGKEY(&KEY) +
016700080314                         WAIT(*MAX) +
016800080314                         RMV(*NO) +
016900080314                         MSG(&REPLY)
017000080314
017100080314              IF         COND(&REPLY *EQ 'R'  +
017200080314                         *OR  &REPLY *EQ 'r') +
017300080314                         THEN(DO)
017400080314              GOTO       CMDLBL(LBL_0020)
017500080314              ENDDO
017600080314
017700080314              IF         COND(&REPLY *EQ 'C'  +
017800080314                         *OR  &REPLY *EQ 'c') +
017900080314                         THEN(DO)
018000080314              GOTO       CMDLBL(ENDCLPGM)
018100080314              ENDDO
018200080314              GOTO       CMDLBL(LBL_0030)
018300080314
018400080314 LBL_0050:
018500080314
018600080314 /*****                                                           */
018700080314 /*****   Send message to SYSOPR if object MFAADDSAP is not       */
018800080314 /*****   found. SYSOPR can RETRY or CANCEL job.                  */
018900080314 /*****                                                           */
019000080314
019100080314             CHGVAR     VAR(&MSG) +
019200080314                        VALUE('Object MFAADDSAP in library MFASYSWIDE +
019300080314                               is not found   +
019400080314                               Enter "R" to RETRY +
019500080314                               or "C" to CANCEL.')
019600080314
019700080314             SNDPGMMSG  MSG(&MSG) +
019800080314                        TOMSGQ(*SYSOPR) +
019900080314                        MSGTYPE(*INQ) +
020000080314                        KEYVAR(&KEY)
020100080314             RCVMSG     MSGQ(*PGMQ) +
020200080314                        MSGTYPE(*RPY) +
020300080314                        MSGKEY(&KEY) +
020400080314                        WAIT(*MAX) +
020500080314                        RMV(*NO) +
020600080314                        MSG(&REPLY)
020700080314
020800080314             IF         COND(&REPLY *EQ 'C'  +
020900080314                        *OR  &REPLY *EQ 'c') +
021000080314                        THEN(DO)
021100080314             GOTO       CMDLBL(ENDCLPGM)
021200080314             ENDDO
021300080314
021400080314
021500080314             IF         COND(&REPLY *EQ 'R'  +
021600080314                        *OR  &REPLY *EQ 'r') +
021700080314                        THEN(DO)
021800080314             GOTO       CMDLBL(LBL_0010)
021900080314             ENDDO
022000080314             GOTO       CMDLBL(LBL_0050)
022100080314
022200080314 /*40867 --- ADD -------------------------------------------------*/
022300080314 LBL_0060:
022400080314
022500080314 /*****                                                           */
022600080314 /*****   Send message to SYSOPR if object MFAADDSAP is locked    */
022700080314 /*****          SYSOPR can RETRY or CANCEL job.                  */
022800080314 /*****                                                           */
022900080314
023000080314             CHGVAR     VAR(&MSG) +
023100080314                        VALUE('Object MFAADDSAP in library MFASYSWIDE +
023200080314                               is locked  +
023300080314                               Enter "R" to RETRY +
023400080314                               or "C" to CANCEL.')
023500080314
023600080314             SNDPGMMSG  MSG(&MSG) +
023700080314                        TOMSGQ(*SYSOPR) +
023800080314                        MSGTYPE(*INQ) +
023900080314                        KEYVAR(&KEY)
024000080314             RCVMSG     MSGQ(*PGMQ) +
024100080314                        MSGTYPE(*RPY) +
024200080314                        MSGKEY(&KEY) +
024300080314                        WAIT(*MAX) +
024400080314                        RMV(*NO) +
024500080314                        MSG(&REPLY)
024600080314
024700080314             IF         COND(&REPLY *EQ 'C'  +
024800080314                        *OR  &REPLY *EQ 'c') +
024900080314                        THEN(DO)
025000080314             GOTO       CMDLBL(ENDCLPGM)
025100080314             ENDDO
025200080314
025300080314
025400080314             IF         COND(&REPLY *EQ 'R'  +
025500080314                        *OR  &REPLY *EQ 'r') +
025600080314                        THEN(DO)
025700080314             GOTO       CMDLBL(LBL_0020)
025800080314             ENDDO
025900080314             GOTO       CMDLBL(LBL_0060)
026000080314
026100080314 LBL_0070:
026200080314
026300080314 /*****                                                           */
026400080314 /*****   Send message to SYSOPR if object MFAADDSMP is locked    */
026500080314 /*****          SYSOPR can RETRY or CANCEL job.                  */
026600080314 /*****                                                           */
026700080314
026800080314             CHGVAR     VAR(&MSG) +
026900080314                        VALUE('Object MFAADDSMP in library MFASYSWIDE +
027000080314                               is locked  +
027100080314                               Enter "R" to RETRY +
027200080314                               or "C" to CANCEL.')
027300080314
027400080314             SNDPGMMSG  MSG(&MSG) +
027500080314                        TOMSGQ(*SYSOPR) +
027600080314                        MSGTYPE(*INQ) +
027700080314                        KEYVAR(&KEY)
027800080314             RCVMSG     MSGQ(*PGMQ) +
027900080314                        MSGTYPE(*RPY) +
028000080314                        MSGKEY(&KEY) +
028100080314                        WAIT(*MAX) +
028200080314                        RMV(*NO) +
028300080314                        MSG(&REPLY)
028400080314
028500080314             IF         COND(&REPLY *EQ 'C'  +
028600080314                        *OR  &REPLY *EQ 'c') +
028700080314                        THEN(DO)
028800080314             GOTO       CMDLBL(ENDCLPGM)
028900080314             ENDDO
029000080314
029100080314
029200080314             IF         COND(&REPLY *EQ 'R'  +
029300080314                        *OR  &REPLY *EQ 'r') +
029400080314                        THEN(DO)
029500080314             GOTO       CMDLBL(LBL_0027)
029600080314             ENDDO
029700080314             GOTO       CMDLBL(LBL_0070)
029800080314 /*40867 --- END ADD ---------------------------------------------*/
029900080314
030000080314 ENDCLPGM:
030100080314             RETURN
030200080314             ENDPGM
