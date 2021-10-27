000100960404/*---------------------------------------------------------*/
000200190227/* Address Hold Mail Date - Adhoc report                   */
000300190227/* CALLED FROM GENHLDDA(QTP)                               */
000301190225/* Richie M     | 2019/02/25 | RFS180781-Modified to accept*/
000302190124/*              |            | LDA values thats directly   */
000303190124/*              |            | passed as parameters with   */
000304190124/*              |            | '*' delimited               */
000400960404/*---------------------------------------------------------*/
000500190226             PGM
000501190227             DCL        VAR(&PRMSTRDTE) TYPE(*CHAR) LEN(8)
000600190227             DCL        VAR(&PRMENDDTE) TYPE(*CHAR) LEN(8)
001200010307
001300970403             DSPDTAARA  DTAARA(*LDA)
001700010308
001701190227             RTVDTAARA  DTAARA(*LDA (1 8)) RTNVAR(&PRMSTRDTE)
001800190227             RTVDTAARA  DTAARA(*LDA (9 8)) RTNVAR(&PRMENDDTE)
001801190225
001802190225             CHGDTAARA  DTAARA(*LDA (1 607)) VALUE(' ')
001803190227             CHGDTAARA  DTAARA(*LDA (608 8)) VALUE(&PRMSTRDTE)
001804190227             CHGDTAARA  DTAARA(*LDA (616 8)) VALUE(&PRMENDDTE)
001805190225             DSPDTAARA  DTAARA(*LDA)
002302190227
003000190227             QTP        AUTO(PTGENHLDD2)
003001190225
003002190227             QUIZ       AUTO(PZGENHLDDA)
003400931102
003500990108ENDPGM
