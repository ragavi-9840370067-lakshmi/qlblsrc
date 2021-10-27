000100050130/*------------------------------------------------------------------*/
000200050130/* TITLE        : Check number of records in a file                 */
000300050130/* SYSTEM       : JSI                                               */
000400050130/* PROGRAM TYPE : CL                                                */
000500050130/* PROGRAMMER   : JEWELSTONE SYSTEMS                                */
000600050130/* PROGRAM      : CHECKRCCNT                                        */
000700050130/* DESCRIPTION  :                                                   */
000800050130/* DATE CREATED :                                                   */
000900050130/*------------------ MODIFICATION HISTORY --------------------------*/
001000050130/* PROGRAMMER      | DATE      |           COMMENTS                 */
001100050130/*-----------------|-----------|------------------------------------*/
001200050130/* R. Moroz         2004/09/29  RFS 22388 - Write rec cnt to *LDA   */
001300050130/* R. Moroz         2005/01/13  RFS 26144 - Change LDA position     */
001400080507/* O.AGUINALDO      2008/05/07  CM - REMOVE QTEMP CODING            */
001401110707/* Ewa K            2011/07/07  RFS 92308 - MonMsg CPF3012 & CPF3064*/
001500050130/*------------------------------------------------------------------*/
001600050130             PGM        PARM(&FILENAME &LIBRNAME &RECCNT)
001700080507
001800080508/*080508     DCLF       FILE(MBR)    */
001900080508/*080508*/   DCLF       FILE(QAFDMBR)
002000050130             DCL        VAR(&FILENAME) TYPE(*CHAR) LEN(10)
002100050130             DCL        VAR(&LIBRNAME) TYPE(*CHAR) LEN(10)
002200050130             DCL        VAR(&RECCNT) TYPE(*DEC) LEN(10)
002300080507
002400050130             CHGVAR     VAR(&MBNRCD) VALUE(0)
002500080507
002600080507             DLTF       FILE(QTEMP/MBR)
002700050130             MONMSG     MSGID(CPF0000)
002800080507
002900050130             DSPFD      FILE(&LIBRNAME/&FILENAME) TYPE(*MBR) +
003000050130                          OUTPUT(*OUTFILE) OUTFILE(QTEMP/MBR)
003001110707
003002110707/* RFS92308 start - monmsg when the file or library is not found */
003100110707             monmsg     msgid(CPF3012 CPF3064) exec(do)
003101110707             chgvar     VAR(&RECCNT) VALUE(0)
003102110707             chgdtaara  dtaara(*LDA (1024 1)) value(N)
003103110707             goto       cmdlbl(endpgm)
003104110707             enddo
003105110707/* RFS92308 end                                                  */
003106110707
003200080603             OVRDBF     FILE(QAFDMBR) TOFILE(QTEMP/MBR)
003300050130 READNEXT:
003400050130             RCVF
003500050130             CHGVAR     VAR(&RECCNT) VALUE(&MBNRCD)
003600050130
003700050130/* 22388     CHECK IF THE FILE HAS SOME RECORDS */
003800050130/* 22388 */  IF COND(&MBNRCD *EQ 0) THEN(DO)
003900050130/* 26144 */     CHGDTAARA  DTAARA(*LDA (1024 1)) VALUE(N)
004000050130/* 22388 */  ENDDO
004100050130/* 22388 */  IF COND(&MBNRCD *NE 0) THEN(DO)
004200050130/* 26144 */     CHGDTAARA  DTAARA(*LDA (1024 1)) VALUE(Y)
004300050130/* 22388 */  ENDDO
004400050130
004500110707 endpgm:     ENDPGM
