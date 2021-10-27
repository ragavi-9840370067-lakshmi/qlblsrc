000100210528/********************************************************************/
000200210528/*                                                                  */
000300210528/* TITLE        : Find the mismatch of Fundserv file expected and   */
000400210528/*                created                                           */
000500210528/* SYSTEM       : L&T Infotech Financial Services Technology Inc.   */
000600210528/* PROGRAM TYPE : CLP                                               */
000700210528/* PROGRAMMER   : Lavanya V                                         */
000800210528/* PROGRAM      : JBFSRVCHK                                         */
000900210528/* DESCRIPTION  : This prog is designed to send message to support  */
001000210528/*                team when the no of Fundserv files produced by dly*/
001100210528/*                Batch do not match with the expected files as per */
001200210528/*                the env setup.                                    */
001300210528/* ******************************************************************/
001400210528/*             C H A N G E   H I S T O R Y                          */
001500210528/* ******************************************************************/
001600210528/*  PROGRAMMER   *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001700210528/***************** yyyy/mm/dd ***************************************/
001800210528/* V Lavanya     * 2021/04/28 * RFS186712 - New Program             */
001900210528/********************************************************************/
002000210528
002100210528PGM
002200210528             DCL        VAR(&MESSAGE) TYPE(*CHAR) LEN(600)
002300210528
002400210528              DCL        VAR(&AsAtDate) TYPE(*CHAR) LEN(8)
002500210528              DCL        VAR(&REPLY)   TYPE(*CHAR) LEN(1)
002600210528              DCL        VAR(&Reccnt)  TYPE(*DEC) LEN(10)
002700210528
002800210528              RMVLIBLE   LIB(QTEMP)
002900210528              MONMSG     MSGID(CPF0000)
003000210528              ADDLIBLE   LIB(QTEMP)
003100210528              MONMSG     MSGID(CPF0000)
003200210528
003300210528              RTVDTAARA  DTAARA(MFAPRCDTP (1 8)) RTNVAR(&AsAtDate)
003400210528
003500210531              CHGVAR     VAR(&MESSAGE) VALUE('Mismatch between number of +
003501210531                           xml files expected vs generated.Please check +
003502210531                           WKFSRVCNT file.')
003800210528
003900210528             EXECUTE    SQL('select * from WKFSRVCNT WHERE +
004000210528                          Process_Date = ' || &ASATDATE || ' AND +
004100210528                          Actual_files <> Expected_Files') +
004200210528                          OUTFILE(QTEMP/RESULT)
004300210528
004400210528
004500210528              RTVMBRD    FILE(QTEMP/RESULT) NBRCURRCD(&Reccnt)
004600210528              IF         COND(&Reccnt *GT 0) THEN(DO)
004700210528              SNDUSRMSG  MSG(&MESSAGE) VALUES('C' 'c' 'I' 'i') +
004800210528                           TOMSGQ(*SYSOPR) MSGRPY(&REPLY)
004900210528
005000210528              SNDPGMMSG  MSG('Reply . . :   ' *CAT &REPLY)
005100210528              ENDDO
005200210528
005300210531ENDPGM
