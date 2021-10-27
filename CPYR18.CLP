000100971128/*-------------------------------------------------------------------*/
000200971128/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION                           */
000300971128/*            * YYYY/MM/DD   *                                       */
000400971128/* DAISY LY   * 1997/11/28   * RFS #1758 - MFAR18P FILE FORMAT       */
000500971128/*            *              * HAS CHANGE NEED TO RECOMPILE THE      */
000600971128/*            *              * PROGRAMS                              */
000700990119/* DAISY LY   * 1999/01/19   * RFS #3749 - MFAR18P   FILE FORMAT     */
000800990119/*            *              * HAS CHANGE NEED TO RECOMPILE THE      */
000900990119/*            *              * PROGRAMS                              */
001000991019/* CHRISTIN B.* 1999/10/04   * RFS #4941 - CHANGE FILE NAME TO       */
001100991019/*            *              *            'R18_' *CAT &LASTYEAR     */
001200031223/* Reece Tam  * 2003/12/23   * RFS 19802 - File expands from 575     */
001300031224/*            *              * to 625, and uses block length 31250.  */
001400971128/*-------------------------------------------------------------------*/
001500941109             PGM
001600941109             DCL        VAR(&WKSTN) TYPE(*CHAR) LEN(10)
001700931201             DCL        VAR(&PDATE) TYPE(*CHAR) LEN(6)
001800931201             DCL        VAR(&PGMMSG) TYPE(*CHAR) LEN(300)
001900940914             DCL        VAR(&TAPDEV) TYPE(*CHAR) LEN(10) VALUE('TAP01')
002000940914             DCL        VAR(&MFAPRCDTP) TYPE(*CHAR) LEN(169)
002100940914             DCL        VAR(&MFACMPCDP) TYPE(*CHAR) LEN(43)
002200940914             DCL        VAR(&ENV) TYPE(*CHAR) LEN(3)
002300940914             DCL        VAR(&ENVNME) TYPE(*CHAR) LEN(40)
002400940914             DCL        VAR(&TXT) TYPE(*CHAR) LEN(50)
002500991007             DCL        VAR(&MFAEFTLIB) TYPE(*CHAR) LEN(10)
002600991007             DCL        VAR(&LASTYEAR)  TYPE(*CHAR) LEN(4)
002700991007             DCL        VAR(&FILENAME)  TYPE(*CHAR) LEN(10)
002800941109
002900940914             CHGJOB     LOG(4 00 *SECLVL) LOGCLPGM(*YES)
003000941109             RTVJOBA    SBMMSGQ(&WKSTN)
003100940914             RTVDTAARA  DTAARA(MFAPRCDTP) RTNVAR(&MFAPRCDTP)
003200940914             RTVDTAARA  DTAARA(MFACMPCDP) RTNVAR(&MFACMPCDP)
003300970212             RTVDTAARA  DTAARA(MFATAPDEV) RTNVAR(&TAPDEV)
003400991007             RTVDTAARA  DTAARA(MFAEFTLIB *ALL) RTNVAR(&MFAEFTLIB)
003500991007             RTVDTAARA  DTAARA(MFAPRCDTP (94 4)) RTNVAR(&LASTYEAR)
003600991019/*           CHGVAR     VAR(&FILENAME) VALUE('R18 TAPE_' *CAT &LASTYEAR)  */
003700991019             CHGVAR     VAR(&FILENAME) VALUE('R18_' *CAT &LASTYEAR)
003800941109             CHGVAR     VAR(&PDATE) VALUE(%SST(&MFAPRCDTP 96 6))
003900940914             CHGVAR     VAR(&ENV) VALUE(%SST(&MFACMPCDP 1 3))
004000940914             CHGVAR     VAR(&ENVNME) VALUE(%SST(&MFACMPCDP 4 40))
004100970421             CHGVAR     VAR(&TXT) VALUE('R18 tape' *BCAT &ENV *BCAT +
004200941109                        &PDATE)
004300941109
004400031223/* rfs 19802 - start                                                 */
004500031223/*CPYFILE:    CPYTOTAP   FROMFILE(&MFAEFTLIB/&FILENAME) TOFILE(QTAPE) +
004600970421                          TOSEQNBR(01) TODEV(&TAPDEV) TOREELS(*SL) +
004700970421                          TORCDLEN(575) TOENDOPT(*UNLOAD) +
004800031223                          TOBLKLEN(31625) TORCDBLK(*FB)              */
004900031223 CPYFILE:    CPYTOTAP   FROMFILE(&MFAEFTLIB/&FILENAME) TOFILE(QTAPE) +
005000031223                          TOSEQNBR(01) TODEV(&TAPDEV) TOREELS(*SL) +
005100031223                          TORCDLEN(625) TOENDOPT(*UNLOAD) +
005200031224                          TOBLKLEN(31250) TORCDBLK(*FB)
005300031223/* rfs 19802 - end                                                   */
005400941109
005500031223/* rfs 19802 - start                                                 */
005600031223/*           CHGVAR     VAR(&PGMMSG) VALUE('Please remove tape on ' +
005700970421                        *CAT &TAPDEV *TCAT '. ' *CAT 'LABEL tape +
005800970421                        as to: REVENUE QUEBEC R18 TAPE' *BCAT +
005900970421                        &PDATE *BCAT 'for' *BCAT +
006000970421                        &ENV *BCAT &ENVNME *TCAT '. Record Len= +
006100031223                        575 Block Len=31625')                        */
006200031223             CHGVAR     VAR(&PGMMSG) VALUE('Please remove tape on ' +
006300031223                        *CAT &TAPDEV *TCAT '. ' *CAT 'LABEL tape +
006400031223                        as to: REVENUE QUEBEC R18 TAPE' *BCAT +
006500031223                        &PDATE *BCAT 'for' *BCAT +
006600031223                        &ENV *BCAT &ENVNME *TCAT '. Record Len= +
006700031224                        625 Block Len=31250')
006800031223/* rfs 19802 - end                                                   */
006900941109
007000931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
007100970421             CHGVAR     VAR(&PGMMSG) VALUE('R18 tape creation +
007200941109                          has completed successfully.')
007300931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(&WKSTN) MSGTYPE(*INFO)
007400931201             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDCLPGM))
007500931201 ENDCLPGM:   RETURN
007600931201             ENDPGM
