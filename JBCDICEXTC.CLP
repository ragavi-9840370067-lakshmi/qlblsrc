000001170313     /* %ATTR OPTION(*LSTDBG) */
000101111019
000201111019     /*****************************************************************/
000301111019     /*                                                               */
000401111128     /*    JOBCDICEXC :  CDIC Extract.                                */
000501111019     /*                                                               */
000601111019     /*    PURPOSE:    This job will be called from JOBCDICEXT, if    */
000701111019     /*                report table (MFAPRORPP) is set up to          */
000801111019     /*                generate consolidated version (all extracts    */
000901111019     /*                in one file).                                  */
001001111128     /*                                                               */
001101111124     /*    DEVELOPER : THILAGA K.                                     */
001201111128     /*                                                               */
001301111019     /*    CREATED:    19/OCT/2011                                    */
001401111019     /*                (RFS 97125)                                    */
001501111019     /*    CHANGED:                                                   */
001601111019     /*                                                               */
001701111019     /*****************************************************************/
001801111019     /*****************************************************************/
001901111019     /*                                                               */
002001111019     /*    C H A N G E   H I S T O R Y                                */
002101111019     /*                                                               */
002201111019     /*****************************************************************/
002301111019     /*****************************************************************/
002401111019     /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
002501111019     /*****************************************************************/
002601120208     /* RICHARD G  *   120208   * ADD NEW SEQUEL FOR MAX PROCESS      */
002701120208     /*            * RFS105765  * DATE ON TRANS                       */
002801120412     /*Alan Andrade* 2012/04/12 * RFS108199 - Add FTP process         */
002901131011     /*TAMILSELVI  * 2013/07/26 * RFS125894 - RECOMPILE FOR MFAPRORPP */
003001111019     /*****************************************************************/
003101111019             PGM
003201111104             DCL        VAR(&ASATDT)      TYPE(*CHAR)  LEN(8)
003301111104             DCL        VAR(&MGTCDE)      TYPE(*CHAR)  LEN(3)
003401111104             DCL        VAR(&MFAEFTLIB)   TYPE(*CHAR)  LEN(10)
003501111125             DCL        VAR(&PROCESS_DT)  TYPE(*DEC)   LEN(08)
003601111128             DCL        VAR(&PROCDATE)    TYPE(*DEC)   LEN(08)
003701111125             DCL        VAR(&PROC_DT)     TYPE(*CHAR)  LEN(08)
003801111104             DCL        VAR(&YYMMDD)      TYPE(*CHAR)  LEN(06)
003901111104             DCL        VAR(&DTLCNT)      TYPE(*DEC)   LEN(10)
004001111104             DCL        VAR(&FILENAME)    TYPE(*CHAR)  LEN(10)
004101111104             DCL        VAR(&MEMBER)      TYPE(*CHAR)  LEN(10)
004201111108             DCL        VAR(&DTLCNTSTR)   TYPE(*CHAR)  LEN(10)
004301111110             DCL        VAR(&HREC)        TYPE(*CHAR)  LEN(15)
004401111110             DCL        VAR(&TREC)        TYPE(*CHAR)  LEN(15)
004501111110             DCL        VAR(&SPACE2)      TYPE(*CHAR)  LEN(02) +
004601111110                                       VALUE('  ')
004701120412             DCL        VAR(&RHFILE)     TYPE(*CHAR) LEN(30)
004801120412             DCL        VAR(&SYSDAT)     TYPE(*CHAR) LEN(08)
004901111025
005001111125             RTVDTAARA  DTAARA(MFAPRCDTP (1 8)) RTNVAR(&PROC_DT)
005101111019             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&MGTCDE)
005201111019             RTVDTAARA  DTAARA(MFAEFTLIB  *ALL) RTNVAR(&MFAEFTLIB)
005301111125             CHGVAR VAR(&PROCESS_DT) VALUE(&PROC_DT)
005401111128             CHGVAR VAR(&ASATDT)     VALUE(&PROC_DT)
005501111025
005601111110             DLTF       FILE(QTEMP/TMPINVRV)
005701111025             MONMSG     MSGID(CPF0000)
005801111202
005901111202             DLTF       FILE(QTEMP/IVRADDR)
006001111202             MONMSG     MSGID(CPF0000)
006101111202
006201111202             EXECUTE    VIEW(IVRADDR) OUTFILE(QTEMP/IVRADDR) +
006301111202                        MBROPT(*REPLACE)
006401111025
006501111128             EXECUTE    VIEW(CDICINVRV) OUTFILE(QTEMP/TMPINVRV) +
006601111128                          MBROPT(*ADD) SETVAR((&PROCESS_DT &ASATDT))
006701111104
006801111107             DLTF       FILE(QTEMP/TMPIVRTV)
006901111104             MONMSG     MSGID(CPF0000)
007001111025
007101111107             EXECUTE    VIEW(CDICIVRTV) OUTFILE(QTEMP/TMPIVRTV) +
007201111107                        MBROPT(*ADD)
007301111104
007401111104             DLTF       FILE(QTEMP/TMPACCTV)
007501111104             MONMSG     MSGID(CPF0000)
007601111103
007701111104             EXECUTE    VIEW(CDICACCTV) OUTFILE(QTEMP/TMPACCTV) +
007801111104                        MBROPT(*REPLACE)
007901111104
008001120208     /*- R105765 ---- START ------------------------------------------*/
008101120208             DLTF       FILE(QTEMP/TMPTRNP)
008201120208             MONMSG     MSGID(CPF0000)
008301120208
008401120208             EXECUTE    VIEW(CDICTRNPV) OUTFILE(QTEMP/TMPTRNP) +
008501120208                        MBROPT(*REPLACE)
008601120208     /*- R105765 ---- END   ------------------------------------------*/
008701120208
008801111104             DLTF       FILE(QTEMP/TMPFUNDV)
008901111104             MONMSG     MSGID(CPF0000)
009001111028
009101111104             EXECUTE    VIEW(CDICFUNDV) OUTFILE(QTEMP/TMPFUNDV) +
009201111104                        MBROPT(*REPLACE)
009301111104
009401111104             DLTF       FILE(QTEMP/TMPBENFV)
009501111104             MONMSG     MSGID(CPF0000)
009601111028
009701111104             EXECUTE    VIEW(CDICBENFV) OUTFILE(QTEMP/TMPBENFV) +
009801111104                        MBROPT(*REPLACE)
009901111104
010001111104             DLTF       FILE(QTEMP/TMPASSNV)
010101111104             MONMSG     MSGID(CPF0000)
010201111028
010301111104             EXECUTE    VIEW(CDICASSNV) OUTFILE(QTEMP/TMPASSNV) +
010401111104                        MBROPT(*REPLACE)
010501111104
010601111104             DLTF       FILE(QTEMP/TMPTRNSV)
010701111104             MONMSG     MSGID(CPF0000)
010801111028
010901111128             EXECUTE    VIEW(CDICTRNSV) OUTFILE(QTEMP/TMPTRNSV) +
011001111128                          MBROPT(*REPLACE) SETVAR((&PROCESS_DT +
011101111128                          &ASATDT))
011201111104
011301111124             CPYF       FROMFILE(QTEMP/TMPIVRTV) +
011401111124                          TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD) +
011501111124                          FMTOPT(*NOCHK)
011601111124             MONMSG      MSGID(CPF0000)
011701111124
011801111124             CPYF       FROMFILE(QTEMP/TMPACCTV) +
011901111124                        TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD)   +
012001111124                        FMTOPT(*NOCHK)
012101111124             MONMSG      MSGID(CPF0000)
012201111124             CPYF       FROMFILE(QTEMP/TMPFUNDV) +
012301111124                        TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD)   +
012401111124                        FMTOPT(*NOCHK)
012501111124             MONMSG      MSGID(CPF0000)
012601111124             CPYF       FROMFILE(QTEMP/TMPBENFV) +
012701111124                        TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD)   +
012801111124                        FMTOPT(*NOCHK)
012901111124             MONMSG      MSGID(CPF0000)
013001111124             CPYF       FROMFILE(QTEMP/TMPASSNV) +
013101111124                        TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD)   +
013201111124                        FMTOPT(*NOCHK)
013301111124            MONMSG      MSGID(CPF0000)
013401111124             CPYF       FROMFILE(QTEMP/TMPTRNSV) +
013501111124                        TOFILE(QTEMP/TMPINVRV) MBROPT(*ADD)   +
013601111124                        FMTOPT(*NOCHK)
013701111124             MONMSG      MSGID(CPF0000)
013801111104
013901111124             RTVMBRD    FILE(QTEMP/TMPINVRV) NBRCURRCD(&DTLCNT)
014001111125
014101111124             CHGVAR     VAR(&DTLCNTSTR) VALUE(&DTLCNT)
014201111124
014301111124             DLTF       FILE(QTEMP/HEADER)
014401111028             MONMSG     MSGID(CPF0000)
014501111110
014601111124             DLTF       FILE(QTEMP/CONDETAIL)
014701111124             MONMSG     MSGID(CPF0000)
014801111110
014901111124             DLTF       FILE(QTEMP/TRAILER)
015001111124             MONMSG     MSGID(CPF0000)
015101111124
015201111124             CRTPF FILE(QTEMP/CONDETAIL) RCDLEN(700)  SIZE(*NOMAX)
015301111124             MONMSG      MSGID(CPF0000)
015401111125
015501111125
015601111125             CHGVAR VAR(&HREC) VALUE("H"  || &SPACE2 || &PROC_DT)
015701111124
015801111124             INSERT INTO(QTEMP/CONDETAIL)  SQL('SELECT "' *CAT &HREC +
015901111124                    *CAT '" FROM SQLEXEC')
016001111124
016101111124            CPYF       FROMFILE(QTEMP/TMPINVRV) +
016201111124                         TOFILE(QTEMP/CONDETAIL) MBROPT(*ADD) +
016301111124                         FMTOPT(*NOCHK)
016401111124            MONMSG      MSGID(CPF0000)
016501111124
016601111124            CHGVAR VAR(&TREC) VALUE("T" || &SPACE2 || &DTLCNTSTR)
016701111124
016801111124            INSERT INTO(QTEMP/CONDETAIL)  SQL('SELECT "' *CAT &TREC +
016901111124                  *CAT '" FROM SQLEXEC')
017001111124
017101111124
017201111125             CHGVAR     VAR(&YYMMDD)    VALUE(%SST(&PROC_DT 3 6))
017301111104             CHGVAR     VAR(&FILENAME) VALUE('CDICEXT' *CAT &MGTCDE)
017401111102             CHGVAR     VAR(&MEMBER) VALUE('CDIC' *CAT &YYMMDD)
017501111102
017601111102
017701111104    /* Check the existance of the object in the EFT Library */
017801111104    /* If it Does not exist, create the file and set the max members to */
017901111102    /*                                                  *NO  */
018001111102
018101111102              CHKOBJ    OBJ(&MFAEFTLIB/&FILENAME) OBJTYPE(*FILE)
018201120119             MONMSG     MSGID(CPF9801) EXEC(CRTPF +
018301120119                          FILE(&MFAEFTLIB/&FILENAME) RCDLEN(700) +
018401120119                          TEXT('CDIC DEPOSITOR LIABILITY EXTRACT') +
018501120119                          MAXMBRS(*NOMAX) SIZE(*NOMAX))
018601111102
018701111104    /* Copy the Temp Consolidated detail file to EFT file */
018801111102
018901111110             CPYF       FROMFILE(QTEMP/CONDETAIL) +
019001111110                          TOFILE(&MFAEFTLIB/&FILENAME) +
019101111110                          TOMBR(&MEMBER) MBROPT(*REPLACE) +
019201111110                           FMTOPT(*NOCHK)
019301111110              MONMSG      MSGID(CPF0000)
019401120412
019501120412/*R108199 - Start */
019601120412             CALL       PGM(GETSYSDATE) PARM(&SYSDAT)
019701120412             CHGVAR     VAR(&RHFILE) VALUE(&FILENAME *TCAT '.' +
019801120412                          *TCAT &MEMBER)
019901120412 FTPFILE:
020001120412              CHGDTAARA DTAARA(*LDA (900 100)) VALUE(' ')
020101120412/*&JOBNAME*/  CHGDTAARA DTAARA(*LDA (900 10))  VALUE('JBCDICEXTC')
020201120412/*&FORMCD*/   CHGDTAARA DTAARA(*LDA (910 10))  VALUE('CDICEXT')
020301120412/*&LIBRARY*/  CHGDTAARA DTAARA(*LDA (920 10))  VALUE(&MFAEFTLIB)
020401120412/*&FILE*/     CHGDTAARA DTAARA(*LDA (930 10))  VALUE(&FILENAME)
020501120412/*&MEMBER*/   CHGDTAARA DTAARA(*LDA (940 10))  VALUE(&MEMBER)
020601120412/*&RHFILE*/   CHGDTAARA DTAARA(*LDA (960 30))  VALUE(&RHFILE)
020701120412/*&CRDATE*/   CHGDTAARA DTAARA(*LDA (990 8))   VALUE(&SYSDAT)
020801120412/*&JOBMODE*/  CHGDTAARA DTAARA(*LDA (998 2))   VALUE('UT')
020901120412
021001120412              CALL PGM(JOBGENFTPR)
021101120412
021201120412/*R108199 - End   */
021301111107
021401111102 EXIT: ENDPGM
