000001170313     /* %ATTR OPTION(*SRCDBG) */
000101120623/*****************************************************************/
000201120623/* TITLE        : To Create GL financial transaction extract     */
000301120623/* SYSTEM       : AS/400                                         */
000401120623/* PROGRAM TYPE : CL                                             */
000501120623/* PROGRAMMER   : BHANU PRAKASH                                  */
000601120623/* PROGRAM      : JBGLFINTRN                                     */
000701120623/* DESCRIPTION  : To Create the GL financial transaction extract */
000801120623/*              : in the Export library                          */
000901120623/* DATE CREATED : 2012/06/22                                     */
001001120623/* RFS NUMBER   : RFS107686                                      */
001101120623/* CALLED BY    : NONE                                           */
001201120623/* PARAMETERS   : NONE                                           */
001301120623/* CALLS PROGRAM: JOBGENFTPR                                     */
001401120623/*****************************************************************/
001501120623/*****************************************************************/
001601120623/*                C H A N G E   H I S T O R Y                    */
001701120623/*****************************************************************/
001801120623/* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001901120623/*            * YYYY/MM/DD   * RFS ###### -                      */
002001120623/*****************************************************************/
002101120623/* Bhanu      * 2012/06/22   * RFS 107686 - Created The Program  */
002201121011/* Michael Fan* 2012/10/11   * RFS 114905 - Report leading 0 when*/
002301121011/*            *              * no record selected.               */
002401121026/* Brad Doyle * 2012/10/26   * RFS 115649 - Specify member name  */
002501121026/*            *              * on file generated for FTP         */
002601130314/* Phil Callas* 2013/03/14   * RFS 117663 - form code = GLFINTRN */
002701120623/*****************************************************************/
002801120731             PGM
002901120623             DCL        VAR(&AsAtDte)  TYPE(*CHAR) LEN(08)
003001120731             DCL        VAR(&Cntr1)    TYPE(*DEC)  LEN(1 0) VALUE(0)
003101120628             DCL        VAR(&Cntr)     TYPE(*CHAR) LEN(1)
003201120731             DCL        VAR(&Filno)    TYPE(*CHAR) LEN(1)
003301120623             DCL        VAR(&FileNme)  TYPE(*CHAR) LEN(10)
003401120704             DCL        VAR(&JobNme)   TYPE(*CHAR) LEN(10) +
003501120702                                         VALUE('JBGLFINTRN')
003601120704             DCL        VAR(&FrmCde)    TYPE(*CHAR) LEN(10) +
003701130314  /* 117663 */                           VALUE('GLFINTRN')
003801130314  /* 117663                              VALUE('GLFINTRAN')   */
003901120702             DCL        VAR(&MFAEFTLIB)  TYPE(*CHAR) LEN(10)
004001120704             DCL        VAR(&RhFile)   TYPE(*CHAR) LEN(30)
004101120623             DCL        VAR(&SysDte)   TYPE(*CHAR) LEN(08)
004201120623             DCL        VAR(&YYMMDD)   TYPE(*CHAR) LEN(06)
004301120704             DCL        VAR(&GLF)      TYPE(*CHAR) LEN(03) +
004401120704                                       VALUE('GLF')
004501120704             DCL        VAR(&UT)       TYPE(*CHAR) LEN(02) +
004601120704                                       VALUE('UT')
004701120810             DCL        VAR(&TRLREC)   TYPE(*CHAR) LEN(03) +
004801120810                                       VALUE('999')
004901120810             DCL        VAR(&RECCNT)   TYPE(*DEC) LEN(10)
005001120628
005101120703             RTVDTAARA  DTAARA(MFAEFTLIB (1 10))  RTNVAR(&MFAEFTLIB)
005201120703             RTVDTAARA  DTAARA(MFAPRCDTP (1 08))  RTNVAR(&AsAtDte)
005301120629             CHGVAR     VAR(&YYMMDD) VALUE(%SST(&ASATDTE 3 6))
005401120704             CHGVAR     VAR(&FileNme) VALUE(&GLF  *CAT &YYMMDD +
005501120629                                 *CAT '9')
005601120702             CHKOBJ     OBJ(&MFAEFTLIB/&Filenme) OBJTYPE(*FILE)
005701120629             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(LOOP))
005801120704             CHGVAR     VAR(&FileNme) VALUE(&GLF  *CAT &YYMMDD +
005901120629                                 *CAT '*')
006001120702             DLTF       FILE(&MFAEFTLIB/&FILENME)
006101120629             MONMSG     MSGID(CPF2105)
006201120628
006301120629  LOOP:      CHGVAR     VAR(&Cntr1) VALUE(&Cntr1 + 1)
006401120629             CHGVAR     VAR(&Cntr)  VALUE(&Cntr1)
006501120704             CHGVAR     VAR(&FileNme) VALUE(&GLF  *CAT &YYMMDD +
006601120625                                 *CAT &cntr)
006701120702             CHKOBJ     OBJ(&MFAEFTLIB/&Filenme) OBJTYPE(*FILE)
006801120625             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NEWFL))
006901120628
007001120629             IF         COND(&Cntr1 <= 8) THEN(GOTO CMDLBL(LOOP))
007101120625
007201120629 NEWFL:      DLTF       FILE(QTEMP/FILE1)
007301120629             MONMSG     MSGID(CPF2105)
007401120704             CRTPF      FILE(QTEMP/FILE1) RCDLEN(500) SIZE(*NOMAX)
007501120629             DLTF       FILE(QTEMP/GLFINHDRP)
007601120629             MONMSG     MSGID(CPF2105)
007701120625
007801120731             EXECUTE    VIEW(GLFINHDRV) OUTFILE(QTEMP/GLFINHDRP) +
007901120731                          MBROPT(*ADD) SETVAR((&FILNO &CNTR))
008001120731
008101120626             DLTF       FILE(QTEMP/GLFINDTLP)
008201120626             MONMSG     MSGID(CPF2105)
008301120810
008401120625             EXECUTE    VIEW(GLFINTRNV) OUTFILE(QTEMP/GLFINDTLP) +
008501120625                          MBROPT(*ADD)
008601120810             RTVMBRD    FILE(QTEMP/GLFINDTLP) NBRCURRCD(&RECCNT)
008701120810
008801120810             DLTF       FILE(QTEMP/GLFINTRLP)
008901120810             MONMSG     MSGID(CPF2105)
009001120810
009101120810             IF         COND(&RECCNT *GT 0) THEN(DO)
009201120626             EXECUTE    VIEW(GLFINTRLV) OUTFILE(QTEMP/GLFINTRLP) +
009301120810                          MBROPT(*ADD) SETVAR((&TRLREC &TRLREC))
009401120810             ENDDO
009501120810             ELSE  DO
009601121011/* 114905    EXECUTE    SQL('SELECT "999" LEN(3),"2" LEN(8), +
009701120813                        "000000000000000" LEN(15),+
009801120813                       "000000000000000" LEN(15),+
009901120813                       "000000000000000" LEN(15),+
010001120813                       "000000000000000" LEN(15) +
010101120810                          FROM SQLEXEC') VIEW(GLFINTRLV) +
010201121011                          OUTFILE(QTEMP/GLFINTRLP)         */
010301121011/* 114905 */ EXECUTE    SQL('SELECT "999" LEN(3),"00000002" LEN(8), +
010401121011                        "000000000000000" LEN(15),+
010501121011                       "000000000000000" LEN(15),+
010601121011                       "000000000000000" LEN(15),+
010701121011                       "000000000000000" LEN(15) +
010801121011                          FROM SQLEXEC') VIEW(GLFINTRLV) +
010901121011                          OUTFILE(QTEMP/GLFINTRLP)
011001120810             ENDDO
011101120810
011201120626             CPYF       FROMFILE(QTEMP/GLFINHDRP) +
011301120626                          TOFILE(QTEMP/FILE1) MBROPT(*ADD) +
011401120704                          FMTOPT(*NOCHK)
011501120626             MONMSG     MSGID(CPF2817)
011601120626
011701120626             CPYF       FROMFILE(QTEMP/GLFINDTLP) +
011801120626                          TOFILE(QTEMP/FILE1) MBROPT(*ADD) +
011901120626                           FMTOPT(*NOCHK)
012001120626             MONMSG     MSGID(CPF2817)
012101120626
012201120627             CPYF       FROMFILE(QTEMP/GLFINTRLP) +
012301120627                          TOFILE(QTEMP/FILE1) MBROPT(*ADD) +
012401120627                           FMTOPT(*NOCHK)
012501120627             MONMSG     MSGID(CPF2817)
012601120626
012701121026/* RFS115649 - Begin */
012801121026
012901121026         /*  CPYF       FROMFILE(QTEMP/FILE1) +
013001120702                          TOFILE(&MFAEFTLIB/&FILENME) MBROPT(*REPLACE) +
013101121026                           FMTOPT(*NOCHK) CRTFILE(*YES)  */
013201121114             CPYF       FROMFILE(QTEMP/FILE1) +
013301121114                          TOFILE(&MFAEFTLIB/&FILENME) +
013401121114                          TOMBR(&FILENME) MBROPT(*REPLACE) +
013501121114                          CRTFILE(*YES) FMTOPT(*NOCHK)
013601121026/* RFS115649 - end   */
013701121026
013801120704             MONMSG     MSGID(CPF2817)
013901120704             CHGVAR     VAR(&RhFile)   VALUE(&FileNme)
014001120704
014101120623             CALL       PGM(GETSYSDATE) PARM(&SysDte)
014201120623             CHGDTAARA  DTAARA(*LDA (900 10))  VALUE(&JobNme)
014301120704             CHGDTAARA  DTAARA(*LDA (910 10))  VALUE(&FrmCde)
014401120702             CHGDTAARA  DTAARA(*LDA (920 10))  VALUE(&MFAEFTLIB)
014501120623             CHGDTAARA  DTAARA(*LDA (930 10))  VALUE(&FileNme)
014601120704             CHGDTAARA  DTAARA(*LDA (940 10))  VALUE(&FileNme)
014701120625             CHGDTAARA  DTAARA(*LDA (960 30))  VALUE(&RhFile)
014801120623             CHGDTAARA  DTAARA(*LDA (990 8))   VALUE(&SysDte)
014901120704             CHGDTAARA  DTAARA(*LDA (998 2))   VALUE(&UT)
015001120623             DSPDTAARA  DTAARA(*LDA)
015101120625
015201120625             CALL       PGM(JOBGENFTPR)
015301120625
015401120625EXIT:        ENDPGM
