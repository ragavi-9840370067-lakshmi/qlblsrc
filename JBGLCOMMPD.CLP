000001170313     /* %ATTR OPTION(*LSTDBG) */
000101120623/*****************************************************************/
000201120627/* TITLE        : To Create GL Commissions Paid  Extract         */
000301120623/* SYSTEM       : AS/400                                         */
000401120623/* PROGRAM TYPE : CL                                             */
000501120623/* PROGRAMMER   : BHANU PRAKASH                                  */
000601120627/* PROGRAM      : JBGLCOMMPD                                     */
000701120627/* DESCRIPTION  : To Create the GL Commissions Paid Extract      */
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
002201120928/* Ganbold L  * 2012/09/28   * RFS 114469 - Add following logic  */
002301120928/*            *              * - To display negative sign in     */
002401120928/*            *              * dealer payment amount.            */
002501120928/*            *              * - To display leading zeroes in    */
002601120928/*            *              * trailer count.                    */
002701120928/*            *              * - To display negative sign in     */
002801120928/*            *              * total dealer payment amount if it */
002901120928/*            *              * is negative.                      */
003001121011/* Michael Fan* 2012/10/11   * RFS 114905 - Report leading 0 when*/
003101121011/*            *              * no record selected.               */
003201121026/* Brad Doyle * 2012/10/26   * RFS 115649 - Set member name on   */
003301121026/*            *              * FTP file                          */
003401131004/* RAJKUMAR A * 2013/09/27   * RFS116852-TO ADD INTERMEDIARY FILE*/
003501131004/*            *              * SORTING PURPOSE.                  */
003601120623/*****************************************************************/
003701120623   PGM
003801120623             DCL        VAR(&AsAtDte)  TYPE(*CHAR) LEN(08)
003901120731             DCL        VAR(&Cntr1)    TYPE(*DEC) LEN(1 0) VALUE(0)
004001120629             DCL        VAR(&Cntr)     TYPE(*CHAR) LEN(1)
004101120731             DCL        VAR(&Filno)    TYPE(*CHAR) LEN(1)
004201120623             DCL        VAR(&FileNme)  TYPE(*CHAR) LEN(10)
004301120623             DCL        VAR(&JobNme)   TYPE(*CHAR) LEN(10) +
004401120627                                       VALUE('JBGLCOMMPD')
004501120623             DCL        VAR(&FrmCde)   TYPE(*CHAR) LEN(10) +
004601120627                                       VALUE('GLCOMMPD')
004701120702             DCL        VAR(&MFAEFTLIB)   TYPE(*CHAR) LEN(10)
004801120623             DCL        VAR(&RhFile)   TYPE(*CHAR) LEN(30)
004901120623             DCL        VAR(&SysDte)   TYPE(*CHAR) LEN(08)
005001120623             DCL        VAR(&YYMMDD)   TYPE(*CHAR) LEN(06)
005101120704             DCL        VAR(&GLC)      TYPE(*CHAR) LEN(03) VALUE('GLC')
005201120704             DCL        VAR(&UT)       TYPE(*CHAR) LEN(02) VALUE('UT')
005301120810             DCL        VAR(&TRLREC)   TYPE(*CHAR) LEN(03) +
005401120810                                       VALUE('999')
005501120810             DCL        VAR(&RECCNT)   TYPE(*DEC) LEN(10)
005601120704
005701120703             RTVDTAARA  DTAARA(MFAEFTLIB (1  10)) RTNVAR(&MFAEFTLIB)
005801120703             RTVDTAARA  DTAARA(MFAPRCDTP (1 08))  RTNVAR(&AsAtDte)
005901120704
006001120629             CHGVAR     VAR(&YYMMDD) VALUE(%SST(&ASATDTE 3 6))
006101120704             CHGVAR     VAR(&FILENME) VALUE(&GLC  *CAT &YYMMDD +
006201120629                                 *CAT '9')
006301120702             CHKOBJ     OBJ(&MFAEFTLIB/&FILENME) OBJTYPE(*FILE)
006401120629             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(LOOP))
006501120704             CHGVAR     VAR(&FILENME) VALUE(&GLC  *CAT &YYMMDD +
006601120629                                 *CAT '*')
006701120704
006801120702             DLTF       FILE(&MFAEFTLIB/&FILENME)
006901120629             MONMSG     MSGID(CPF2105)
007001120627
007101120629  LOOP:      CHGVAR     VAR(&Cntr1) VALUE(&Cntr1 + 1)
007201120704             CHGVAR     VAR(&Cntr)  VALUE(&Cntr1)
007301120704             CHGVAR     VAR(&FileNme) VALUE(&GLC  *CAT &YYMMDD +
007401120730                                 *CAT &Cntr)
007501120702             CHKOBJ     OBJ(&MFAEFTLIB/&FILENME) OBJTYPE(*FILE)
007601120625             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NEWFL))
007701120629
007801120629             IF         COND(&Cntr1 <= 8 ) THEN(GOTO CMDLBL(LOOP))
007901120625
008001120704  NEWFL:     DLTF       FILE(QTEMP/FILE2)
008101120704             MONMSG     MSGID(CPF2105)
008201120704             CRTPF      FILE(QTEMP/FILE2) RCDLEN(500) SIZE(*NOMAX)
008301120704
008401120627             DLTF       FILE(QTEMP/GLCOMHDRP)
008501120625             MONMSG     MSGID(CPF2105)
008601120731             EXECUTE    VIEW(GLCOMMHDRV) OUTFILE(QTEMP/GLCOMHDRP) +
008701120813                        MBROPT(*ADD) SETVAR((&FILNO &CNTR))
008801120704
008901120627             DLTF       FILE(QTEMP/GLCOMDTLP)
009001120626             MONMSG     MSGID(CPF2105)
009101131004/* 116852 BEGINS  */
009201131004             DLTF       FILE(QTEMP/GLCMDTLSRT)
009301131004             MONMSG     MSGID(CPF2105)
009401131004/*           EXECUTE    VIEW(GLCOMMPDV) OUTFILE(QTEMP/GLCOMDTLP) +
009501131004                          MBROPT(*ADD)     */
009601131004/* ONE EXTRA COLN ADDED  TO VIEW GLCOMMPDV SOLELY FOR SORTING PURPOSE +
009701131004   SO THE BELOW EXECUTE IS CODED TO EXCLUDE THAT SORTORDER COLN       +
009801131004   AND POPULATE THE DETAIL RECORD IN THE EXISTING WAY */
009901131004
010001131004             EXECUTE    VIEW(GLCOMMPDV) OUTFILE(QTEMP/GLCMDTLSRT) +
010101131004                          MBROPT(*ADD)
010201131004             EXECUTE    SQL('SELECT DETAILRECO   ,  +
010301131004                                    RECORDTYPE   ,  +
010401131004                                    DEALERCODE   ,  +
010501131004                                    PAYMENTDAT   ,  +
010601131004                                    PAYMENTTYP   ,  +
010701131004                                    DLRPAYAMT    ,  +
010801131004                                    ADJAMT       ,  +
010901131004                                    ADJCOMMENT      +
011001131004                               FROM QTEMP/GLCMDTLSRT')         +
011101131004                          OUTFILE(QTEMP/GLCOMDTLP) MBROPT(*ADD)
011201131004/* 116852 ENDS    */
011301120810             RTVMBRD    FILE(QTEMP/GLCOMDTLP) NBRCURRCD(&RECCNT)
011401120810
011501120627             DLTF       FILE(QTEMP/GLCOMTRLP)
011601120626             MONMSG     MSGID(CPF2105)
011701120810
011801120810             IF  COND(&RECCNT *GT 0) THEN(DO)
011901120810
012001120627             EXECUTE    VIEW(GLCOMMTRLV) OUTFILE(QTEMP/GLCOMTRLP) +
012101120810                          MBROPT(*ADD)  SETVAR((&TRLREC &TRLREC))
012201120810             ENDDO
012301120810
012401120810             ELSE  DO
012501121011/* 114905    EXECUTE    SQL('SELECT "999" LEN(3),"2" LEN(8), +
012601120813                        "000000000000000" LEN(15),  +
012701120813                       "000000000000000" LEN(15)  +
012801120810                          FROM SQLEXEC') VIEW(GLCOMMTRLV) +
012901121011                          OUTFILE(QTEMP/GLCOMTRLP)              */
013001121011/* 114905 */ EXECUTE    SQL('SELECT "999" LEN(3),"00000002" LEN(8), +
013101121011                        "000000000000000" LEN(15),  +
013201121011                       "000000000000000" LEN(15)  +
013301121011                          FROM SQLEXEC') VIEW(GLCOMMTRLV) +
013401121011                          OUTFILE(QTEMP/GLCOMTRLP)
013501120810             ENDDO
013601120627             CPYF       FROMFILE(QTEMP/GLCOMHDRP) +
013701120704                          TOFILE(QTEMP/FILE2) MBROPT(*ADD) +
013801120704                           FMTOPT(*NOCHK)
013901120626             MONMSG     MSGID(CPF2817)
014001120626
014101120928/* RFS 114469 Start */
014201120928
014301120928             DLTF       FILE(QTEMP/GLCOMDTLP1)
014401120928             MONMSG     MSGID(CPF2105)
014501120928
014601120928             EXECUTE    VIEW(GLCOMMPDV1) OUTFILE(QTEMP/GLCOMDTLP1) +
014701120928                          MBROPT(*ADD)
014801120928
014901120928          /* CPYF       FROMFILE(QTEMP/GLCOMDTLP) + */
015001120928             CPYF       FROMFILE(QTEMP/GLCOMDTLP1) +
015101120928                          TOFILE(QTEMP/FILE2) MBROPT(*ADD) +
015201120928                          FMTOPT(*NOCHK)
015301120626             MONMSG     MSGID(CPF2817)
015401120928
015501120928/* RFS 114469 End   */
015601120626
015701120627             CPYF       FROMFILE(QTEMP/GLCOMTRLP) +
015801120704                          TOFILE(QTEMP/FILE2) MBROPT(*ADD) +
015901120627                           FMTOPT(*NOCHK)
016001120627             MONMSG     MSGID(CPF2817)
016101120626
016201121026/* RFS 115649 - Begin */
016301121026
016401121026       /*    CPYF       FROMFILE(QTEMP/FILE2) +
016501120702                          TOFILE(&MFAEFTLIB/&FILENME) MBROPT(*REPLACE) +
016601121026                           FMTOPT(*NOCHK) CRTFILE(*YES)   */
016701121026
016801121026             CPYF       FROMFILE(QTEMP/FILE2) +
016901121026                          TOFILE(&MFAEFTLIB/&FILENME) +
017001121113                          TOMBR(&FILENME) MBROPT(*REPLACE) +
017101121026                          CRTFILE(*YES) FMTOPT(*NOCHK)
017201121107/* RFS 115649 - End   */
017301120704             MONMSG     MSGID(CPF2817)
017401120704             CHGVAR     VAR(&RhFile)   VALUE(&FileNme)
017501120627
017601120623             CALL       PGM(GETSYSDATE) PARM(&SysDte)
017701120623             CHGDTAARA  DTAARA(*LDA (900 10))  VALUE(&JobNme)
017801120704             CHGDTAARA  DTAARA(*LDA (910 10))  VALUE(&FrmCde)
017901120702             CHGDTAARA  DTAARA(*LDA (920 10))  VALUE(&MFAEFTLIB)
018001120623             CHGDTAARA  DTAARA(*LDA (930 10))  VALUE(&FileNme)
018101120623             CHGDTAARA  DTAARA(*LDA (940 10))  VALUE(&FileNme)
018201120625             CHGDTAARA  DTAARA(*LDA (960 30))  VALUE(&RhFile)
018301120623             CHGDTAARA  DTAARA(*LDA (990 8))   VALUE(&SysDte)
018401120704             CHGDTAARA  DTAARA(*LDA (998 2))   VALUE(&UT)
018501120623             DSPDTAARA  DTAARA(*LDA)
018601120625
018701120625             CALL       PGM(JOBGENFTPR)
018801120625
018901120625EXIT:        ENDPGM
