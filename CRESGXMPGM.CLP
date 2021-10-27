000100040518   /*****************************************************************/
000200040518   /* TITLE        : Create XML program.                            */
000300040518   /*                                                               */
000400040518   /* DATE         : 2004/05/18                                     */
000500040518   /*                                                               */
000600040518   /* PROGRAM TYPE : CLP                                            */
000700040518   /*                                                               */
000800040518   /* PROGRAMMER   : Michael Mondoux                                */
000900040518   /*                                                               */
001000040518   /* PROGRAM      : CRESGXMPGM                                     */
001100040518   /*                                                               */
001200040518   /* DESCRIPTION  : Create the program that will be used for the   */
001300040518   /*                XML NFU process.                               */
001400040518   /*****************************************************************/
001500040518   /*    C H A N G E   H I S T O R Y                                */
001600040518   /*****************************************************************/
001700040518   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001800040518   /*****************************************************************/
001900040518   /* Michael    * 2004/05/18   * RFS 20032                         */
002000040518   /* Mondoux    *              * - Add new OE service program.     */
002100040518   /*****************************************************************/
002200040518   /*            *              *                                   */
002300040518   /*            *              *                                   */
002400040518   /*            *              *                                   */
002500040518   /*****************************************************************/
002600040518             PGM        PARM(&OBJLIB)
002700000000
002800040518             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
002900040518
003000000000             /* Create program ESGPARSER                      */
003100040518             CRTPGM     PGM(&OBJLIB/ESGPARSER) MODULE(*PGM) +
003200040518                          TEXT('ESGPARSER in ILE') +
003300040518                          BNDSRVPGM(&OBJLIB/ESGXMSRVPG +
003400040518                          &OBJLIB/OESRVPGM15) ACTGRP(ESGXMLNFU) +
003500040518                          OPTION(*DUPPROC) /*R20032*/
003600000000
003700030429             /* Create program ESGLOOKUP                      */
003800040518             CRTPGM     PGM(&OBJLIB/ESGLOOKUP) TEXT('Account +
003900040518                          Lookup') BNDSRVPGM(&OBJLIB/ESGXMSRVPG +
004000040518                          &OBJLIB/OESRVPGM15) OPTION(*DUPPROC) +
004100040518                          /*R20032*/
004200030429
004300000000             ENDPGM
