000100040518   /*****************************************************************/
000200040518   /* TITLE        : Create XML service program.                    */
000300040518   /*                                                               */
000400040518   /* DATE         : 2004/05/18                                     */
000500040518   /*                                                               */
000600040518   /* PROGRAM TYPE : CLP                                            */
000700040518   /*                                                               */
000800040518   /* PROGRAMMER   : Michael Mondoux                                */
000900040518   /*                                                               */
001000040518   /* PROGRAM      : CRESGXMSRV                                     */
001100040518   /*                                                               */
001200040518   /* DESCRIPTION  : Create the service program that will be used   */
001300040518   /*                for the XML NFU process.                       */
001400040518   /*****************************************************************/
001500040518   /*    C H A N G E   H I S T O R Y                                */
001600040518   /*****************************************************************/
001700040518   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001800050918   /*****************************************************************/
001900050918   /* Tony       * 2005/09/18   * RFS 26360 - Added moduled         */
002000050918   /* Punzalan   *              *   GETDACTL                        */
002100050918   /*            *              *                                   */
002200040518   /*****************************************************************/
002300040518   /* Michael    * 2004/05/18   * RFS 20032                         */
002400040518   /* Mondoux    *              * - Add new modules and changed     */
002500040518   /*            *              *   Activation Group to *Caller.    */
002600040518   /*            *              * - Add new OE service program.     */
002700040518   /*****************************************************************/
002800050411   /* John Rab   * 2005/04/11   * RFS 26004 - ESG 16 - Advisor NFU  */
002900050411   /*            *              * - Add 2 new modules:              */
003000050411   /*            *              * 1. ESGPREPDTL (process AddModRep) */
003100050411   /*            *              * 2. ESGPINAREP (process InaRep)    */
003101070315   /*****************************************************************/
003102070315   /* Waldi R.   * 2007/03/15   * RFS 38247 - ESG 18 - NFU Changes: */
003103070319   /*            *              * - Added modules: FXIVRSPLT        */
003104070319   /*            *              *                  IVRSPLTCPY       */
003105070323   /*            *              *                  IVRSPLTADR       */
003106070329   /*            *              *                  IVRSPLTHHM       */
003107070329   /*            *              *                  IVRSPLTPRB       */
003108070402   /*            *              *                  IVRSPLTPAC       */
003109070322   /*            *              *                  IVRSPLTLSF       */
003200050411   /*****************************************************************/
003300040518   /*            *              *                                   */
003400040518   /*            *              *                                   */
003500040518   /*            *              *                                   */
003600040518   /*****************************************************************/
003700000000             PGM        PARM(&OBJLIB)
003800000000
003900000000             DCL        VAR(&OBJLIB) TYPE(*CHAR) LEN(10)
004000000000
004100040518             CRTSRVPGM  SRVPGM(&OBJLIB/ESGXMSRVPG) +
004200040518                          MODULE(&OBJLIB/ESGXMLERR +
004300040518                          &OBJLIB/ESGPCLTINF &OBJLIB/ESGPADDINF +
004400040518                          &OBJLIB/ESGPACCATT &OBJLIB/ESGPDISINF +
004500040518                          &OBJLIB/ESGPBENINF &OBJLIB/ESGPACCSET +
004600040518                          &OBJLIB/XXMPADD &OBJLIB/XXMPCLT +
004700040518                          &OBJLIB/XXMPDLR &OBJLIB/XXMPCHQ +
004800040518                          &OBJLIB/XXMPFND &OBJLIB/XXMPBNK +
004900040518                          &OBJLIB/XXMPACCINF &OBJLIB/XXMPNOM +
005000040518                          &OBJLIB/ESGLOOKUP &OBJLIB/ESGPADDPAC +
005100040512                          &OBJLIB/FXESGBOADR &OBJLIB/FXESGANADR +
005200040512                          &OBJLIB/FXESGJSADR &OBJLIB/FXESGBNADR +
005300050411                          &OBJLIB/ESGPREPDTL &OBJLIB/ESGPINAREP +
005400050511                          &OBJLIB/FXESGIAADR &OBJLIB/FXGETSALCD  +
005500050918                          &OBJLIB/FXXMLENT &OBJLIB/FXOVRFIL  +
005600060419/* R26360 */              &OBJLIB/GETDACTL  +
005601070315/* R29614 */              &OBJLIB/NFUDERCHG             +
005602070322/* R38247 */              &OBJLIB/FXIVRSPLT &OBJLIB/IVRSPLTCPY   +
005603070329/* R38247 */              &OBJLIB/IVRSPLTADR &OBJLIB/IVRSPLTHHM  +
005604070402/* R38247 */              &OBJLIB/IVRSPLTPRB &OBJLIB/IVRSPLTLSF  +
005605070402/* R38247 */              &OBJLIB/IVRSPLTPAC)                    +
005700070315                          EXPORT(*ALL) +
005800040512                          TEXT('XML Messaging Service Programs') +
005900040518                          BNDSRVPGM(&OBJLIB/OESRVPGM15) +
006000040518                          ACTGRP(*CALLER) /*R20032 */
006100000000
006200000000             ENDPGM
