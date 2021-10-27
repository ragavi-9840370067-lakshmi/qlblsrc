000100191217/*---------------------------------------------------------------*/
000200191217/* PROGRAM      : WRKSUBACON                                     */
000300191217/*                                                               */
000400191217/* PROGRAM TYPE : CLP                                            */
000500191217/*                                                               */
000600191217/* PROGRAMMER   : MAYILSAMY D                                    */
000700191217/*                                                               */
000800191217/* CALLED BY    : JOBACCRCON/ACCRCON                             */
000900191217/*                                                               */
001000191217/* DESCRIPTION  : THIS PROGRAM CREATE OJ/TMPSUBACON SIMILAR      */
001100191217/*                QTEMP/SUBACON                                  */
001200191217/* ***************************************************************/
001300191217/*    C H A N G E   H I S T O R Y                                */
001400191217/* ***************************************************************/
001500191219/* MAYILSAMY D* 2019/12/17   * RFS185625 - CREATED PROGRAM       */
001600191217/*---------------------------------------------------------------*/
001700191217
001800191218             PGM
001900191217             DCL        VAR(&OJLIB)    TYPE(*CHAR) LEN(10)
002000191217
002100191217             RTVDTAARA  DTAARA(MFAOBJECTS *ALL) RTNVAR(&OJLIB)
002200191217
002300191217             CHKOBJ     OBJ(&OJLIB/TMPSUBACON) OBJTYPE(*FILE)
002400191217             MONMSG     MSGID(CPF9801) EXEC(DO)
002500191218             RUNSQL     SQL('CREATE TABLE ' || &OJLIB ||'/TMPSUBACON (+
002600191217                          PLACEMENT_DATE DEC ( 9)                +
002700191217                                   NOT NULL WITH DEFAULT,        +
002800191217                          TRANS_NO DEC ( 9)                      +
002900191217                                   NOT NULL WITH DEFAULT,        +
003000191217                          ACCOUNT_NO NUMERIC ( 9)                +
003100191217                                   NOT NULL WITH DEFAULT,        +
003200191217                          INVESTMENT_CODE CHAR ( 5)              +
003300191217                                   NOT NULL WITH DEFAULT,        +
003400191217                          TOTAL_NET_AMT DEC ( 13, 2)             +
003500191217                                   NOT NULL WITH DEFAULT,        +
003600191217                          REJECTION_CODE CHAR ( 3)               +
003700191217                                   NOT NULL WITH DEFAULT,        +
003800191217                          PROVINCIAL_RATE DEC ( 4, 2)            +
003900191217                                   NOT NULL WITH DEFAULT,        +
004000191217                          FEDERAL_RATE DEC ( 4, 2)               +
004100191217                                   NOT NULL WITH DEFAULT,        +
004200191217                          INVESTMENT_TYPE CHAR ( 3),             +
004300191217                          FORM_CODE CHAR ( 5),                   +
004400191217                          CONTR_REDEM_CODE_RULE CHAR ( 2)        +
004500191217                                   NOT NULL WITH DEFAULT,        +
004600191217                          ACCOUNT_TYPE_RULE CHAR ( 1)            +
004700191217                                   NOT NULL WITH DEFAULT,        +
004800191217                          TOTAL_AWD_COUNT NUMERIC ( 9)           +
004900191217                                   NOT NULL WITH DEFAULT,        +
005000191217                          PROVINCIAL_AMT  DEC (13,2)             +
005100191217                                   NOT NULL WITH DEFAULT,        +
005200191217                          FEDERAL_AMT  DEC (13, 2)               +
005300191217                                   NOT NULL WITH DEFAULT,        +
005400191217                          GROSS_AMT_TRANS DEC ( 13, 2)           +
005500191217                                   NOT NULL WITH DEFAULT,        +
005600191217                          TOTAL_GROSS_AWD DEC ( 13, 2)           +
005700191218                                   NOT NULL WITH DEFAULT)')      +
005800191218                                   COMMIT(*NONE)
005900191217             ENDDO
006000191217
006100191217
006200191217             ENDPGM
