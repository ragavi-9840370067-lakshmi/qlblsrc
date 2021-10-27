000100080507/*------------------------------------------------------------------*/
000200080507/* TITLE        : Check record length in a file                     */
000300080507/* SYSTEM       : JSI                                               */
000400080507/* PROGRAM TYPE : CL                                                */
000500080507/* PROGRAMMER   : JEWELSTONE SYSTEMS                                */
000600080507/* PROGRAM      : CHECKRCLEN                                        */
000700080507/* DESCRIPTION  :                                                   */
000800080507/* DATE CREATED :                                                   */
000900080507/*------------------ MODIFICATION HISTORY --------------------------*/
001000080507/* PROGRAMMER      | DATE      |           COMMENTS                 */
001100080507/*-----------------|-----------|------------------------------------*/
001200080507/* O.AGUINALDO     ¦2008/05/07 ¦CM - REMOVE QTEMP CODING            */
001300080507/*-----------------¦-----------¦------------------------------------*/
001400080507/*                 ¦           ¦                                    */
001500080507/*------------------------------------------------------------------*/
001600961128             PGM        PARM(&FILENAME &LIBRNAME &RECLEN)
001700080507
001800080508/*080507*/   DCLF       FILE(QAFDRFMT)
001900940725             DCL        VAR(&FILENAME) TYPE(*CHAR) LEN(10)
002000940725             DCL        VAR(&LIBRNAME) TYPE(*CHAR) LEN(10)
002100961128             DCL        VAR(&RECLEN) TYPE(*DEC) LEN(10)
002200080507
002300961128             CHGVAR     VAR(&RFLEN) VALUE(0)
002400080508
002500961128             DLTF       FILE(QTEMP/RCDFMT)
002600950320             MONMSG     MSGID(CPF0000)
002700961128             DSPFD      FILE(&LIBRNAME/&FILENAME) TYPE(*RCDFMT) +
002800961128                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/RCDFMT)
002900080603
003000080603/*080508*/   OVRDBF     FILE(QAFDRFMT) TOFILE(QTEMP/RCDFMT)
003100080603
003200940613 READNEXT:
003300940613             RCVF
003400961128             CHGVAR     VAR(&RECLEN) VALUE(&RFLEN)
003500931004             ENDPGM
