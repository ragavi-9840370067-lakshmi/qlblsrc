000100960404/*---------------------------------------------------------*/
000200190225/* Address with Hold Mail Reason - Adhoc report            */
000300190225/* CALLED FROM GENHLDR1(QTP)                               */
000301190225/* Richie M     | 2019/02/25 | RFS180781-Modified to accept*/
000302190124/*              |            | LDA values thats directly   */
000303190124/*              |            | passed as parameters with   */
000304190124/*              |            | '*' delimited               */
000400960404/*---------------------------------------------------------*/
000500190226             PGM
000600190225             DCL        VAR(&HLDREASCD) TYPE(*CHAR) LEN(4)
001200010307
001300970403             DSPDTAARA  DTAARA(*LDA)
001700010308
001800190225             RTVDTAARA  DTAARA(*LDA (1 4)) RTNVAR(&HLDREASCD)
001801190225
001802190225             CHGDTAARA  DTAARA(*LDA (1 607)) VALUE(' ')
001803190225             CHGDTAARA  DTAARA(*LDA (608 4)) VALUE(&HLDREASCD)
001804190225             DSPDTAARA  DTAARA(*LDA)
002300961003
003000190225             QTP        AUTO(PTGENHLDRA)
003001190225
003002190225             QUIZ       AUTO(PZGENHLDRC)
003400931102
003500990108ENDPGM
