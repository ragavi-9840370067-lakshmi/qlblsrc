000001170313     /* %ATTR OPTION(*SRCDBG) */
000101130612    /*****************************************************************/
000201130624    /* TITLE        : JOB TO GENERATE ACCOUNT AUDIT LETTER EXTRACT   */
000301130612    /*                                                               */
000401130624    /* SYSTEM       : L&T FINANCIAL SERVICES TECHNOLOGY INC.         */
000501130612    /*                                                               */
000601130612    /* PROGRAM TYPE : CL                                             */
000701130612    /*                                                               */
000801130624    /* PROGRAMMER   : ANTONY S                                       */
000901130612    /*                                                               */
001001130612    /* PROGRAM      : JBACCAUDLE                                     */
001101130624    /*                                                               */
001201130624    /* CALLED BY    : NONE                                           */
001301130624    /*                                                               */
001401130624    /* PARAMETERS   : NONE                                           */
001501130624    /*                                                               */
001601130624    /* CALLS PROGRAM: ACCAUDEXT                                      */
001701130612    /*                                                               */
001801130612    /* DESCRIPTION  : THE PROCESS IS USE TO CONTROL THE GENERATION   */
001901130612    /*                OF ACCOUNT AUDIT LETTER EXTRACT TO CLIENT'S    */
002001130612    /*                EFT LIBRARY.                                   */
002101130612    /*****************************************************************/
002201130612    /*****************************************************************/
002301130612    /*    C H A N G E   H I S T O R Y                                */
002401130612    /*****************************************************************/
002501130612    /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
002601130612    /*****************************************************************/
002701130612    /* ANTONY S   * 2013/06/12   * RFS116890 INITIAL CODING          */
002801140528    /* Kamal T    * 2014/05/28   * RFS 137052 - Adding SIZE parameter*/
002901140528    /*            *              * as *NOMAX in CRTPF COMMAND        */
003001130612    /*****************************************************************/
003101130612             PGM
003201130612             DCL        VAR(&EFTLIBNAME) TYPE(*CHAR) LEN(10)
003301130624             DCL        VAR(&CMPCODE)    TYPE(*CHAR) LEN(3)
003401130624             DCL        VAR(&CURRDATE)   TYPE(*CHAR) LEN(8)
003501130613             DCL        VAR(&EXTFILNAME) TYPE(*CHAR) LEN(10)
003601130624             DCL        VAR(&AGETHRSLD)  TYPE(*CHAR) LEN(2)
003701130620             DCL        VAR(&DATETHRSLD) TYPE(*CHAR) LEN(3)
003801130624             DCL        VAR(&RETCODE)    TYPE(*CHAR) LEN(2)
003901130612
004001130612             RTVDTAARA  DTAARA(MFAEFTLIB (1 10)) RTNVAR(&EFTLIBNAME)
004101130612
004201130612             IF         COND(&EFTLIBNAME *NE ' ') THEN(DO)
004301130613
004401130612             CHKOBJ     OBJ(&EFTLIBNAME) OBJTYPE(*LIB)
004501130613             MONMSG     MSGID(CPF9801) EXEC(DO)
004601130613
004701130613             SNDPGMMSG  MSG('EFT library is not exist')
004801130613             GOTO       CMDLBL(END)
004901130613
005001130613             ENDDO
005101130613
005201130613             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCODE)
005301130613
005401130613             CALL       PGM(GETDAT) PARM(&CURRDATE)
005501130613
005601130613             CHGVAR     VAR(&EXTFILNAME) VALUE('LE' *CAT &CURRDATE)
005701130613
005801130613             DLTF       FILE(QTEMP/&EXTFILNAME)
005901130613             MONMSG     MSGID(CPF2105)
006001130613
006101140528 /* RFS137052 Begin */
006201140528 /*          CRTPF      FILE(QTEMP/&EXTFILNAME) RCDLEN(1040) +   */
006301140528 /*                       TEXT('Account Audit Letter Extract')   */
006401140528             CRTPF      FILE(QTEMP/&EXTFILNAME) RCDLEN(1040) +
006501140528                          TEXT('Account Audit Letter Extract') +
006601140528                          SIZE(*NOMAX)
006701140528 /* RFS137052 End   */
006801130613
006901130613             RTVDTAARA  DTAARA(MFAACAUDTH (1 2)) RTNVAR(&AGETHRSLD)
007001130613             RTVDTAARA  DTAARA(MFAACAUDTH (3 3)) RTNVAR(&DATETHRSLD)
007101130613
007201130620             CALL       PGM(ACCAUDEXT) PARM(&AGETHRSLD &DATETHRSLD +
007301130620                          &CURRDATE &CMPCODE &EXTFILNAME &RETCODE)
007401130613
007501130613             IF         COND(&RETCODE *EQ '00') THEN(DO)
007601130613             CPYF       FROMFILE(QTEMP/&EXTFILNAME) +
007701130613                          TOFILE(&EFTLIBNAME/&EXTFILNAME) +
007801130613                          MBROPT(*REPLACE) CRTFILE(*YES)
007901130613             ENDDO
008001130613
008101130613             IF         COND(&RETCODE *EQ '99') THEN(DO)
008201130613             SNDPGMMSG  MSG('Error occurs while generate the extract +
008301130613                          file. Check the job log for more details.')
008401130613             ENDDO
008501130613             ENDDO
008601130613
008701130613             IF         COND(&EFTLIBNAME *EQ ' ') THEN(DO)
008801130613             SNDPGMMSG  MSG('EFT Library is not exist')
008901130613             GOTO       CMDLBL(END)
009001130613             ENDDO
009101130613
009201130613 END:        ENDPGM
