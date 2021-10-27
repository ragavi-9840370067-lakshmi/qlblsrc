000100101013   /*****************************************************************/
000200101013   /* Title        : PROCESS AND CREATE PURCHASE TRANSACTION        */
000300101013   /*                CONTRIBUTION OF A PARTICULAR GROUP FOR EACH    */
000400101013   /*                ACCOUNT                                        */
000500101013   /*  PROGRAMMER   : UNITRAX                                       */
000600101013   /*                                                               */
000700101013   /*  PROGRAM      : JBGRPBTGEN                                    */
000800101013   /*                                                               */
000900101013   /*  DESCRIPTION  : THIS PROGRAM IS USED TO CALL GRPBTGEN FOR     */
001000101013   /*                 PROCESS AND CREATE PURCHASE TRANSACTION       */
001100101013   /*                 CONTRIBUTION OF A PARTICULAR GROUP FOR EACHS  */
001200101013   /*                 ACCOUNT.                                      */
001300101013   /*****************************************************************/
001400101013   /*    C H A N G E   H I S T O R Y                                */
001500101013   /*****************************************************************/
001600101013   /* MODIFICATIONS                                                 */
001700101013   /*****************************************************************/
001800101013             PGM        PARM(&TRDDTE &GRPCDE &BTCDTE &BTCCDE &CONTRCD)
001900101013/*----------------------------------------*/
002000101013/*  CALLING PARAMETERS                    */
002100101013/*----------------------------------------*/
002200101013             DCL        VAR(&TRDDTE) TYPE(*CHAR) LEN(8)
002300101013             DCL        VAR(&GRPCDE) TYPE(*CHAR) LEN(6)
002400101013             DCL        VAR(&BTCDTE) TYPE(*CHAR) LEN(8)
002500101013             DCL        VAR(&BTCCDE) TYPE(*CHAR) LEN(4)
002600101029             DCL        VAR(&CONTRCD) TYPE(*CHAR) LEN(6)
002700101013/*----------------------------------------*/
002800101013/*  CALL COBOL PROGRAM GRPBTGEN           */
002900101013/*----------------------------------------*/
003000101029             CALL       PGM(GRPBTGEN) PARM(&GRPCDE &TRDDTE &BTCDTE +
003100101029                          &BTCCDE &CONTRCD)
003200101013/*----------------------------------------*/
003300101013/*  CALL COBOL PROGRAM TRNMISC            */
003400101013/*----------------------------------------*/
003500101013             CALL       PGM(TRNMISC)
003600101013             ENDPGM
