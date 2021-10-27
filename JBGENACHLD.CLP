000100170313     /* %ATTR OPTION(*LSTDBG) */
000200111214    /*****************************************************************/
000300111214    /*                                                               */
000400201228    /*    JBGENACHLD :  JOB TO UPDATE ACCOUNT ASSIGNMENT TABLE       */
000500201228    /*                                                               */
000600201228    /*    PURPOSE:    THIS JOB WILL BE CALLED FROM ASNHLD,           */
000700111214    /*                TO UPDATE ACCOUNT ASSIGNMENT TABLE             */
000800111214    /*                BASED ON FILE FEED.                            */
000900111214    /*                                                               */
001000111214    /*    DEVELOPER : THILAGA K.                                     */
001100111214    /*                                                               */
001200210128    /*    CREATED:    15/DEC/2011                                    */
001300111227    /*                                                               */
001400111214    /*    CHANGED:                                                   */
001500111214    /*                                                               */
001600111214    /*****************************************************************/
001700111214    /*****************************************************************/
001800111214    /*                                                               */
001900111214    /*    C H A N G E   H I S T O R Y                                */
002000111214    /*                                                               */
002100111214    /*****************************************************************/
002200111214    /*****************************************************************/
002300111214    /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
002400111214    /*****************************************************************/
002500111227    /* THILAGA K. * 2011/12/15   * RFS 99486 NEW PROGRAM             */
002600120220    /* THILAGA K. * 2012/02/07   * RFS 102778 ADD CALL TO CDICHLDEX  */
002700120411    /* THILAGA K. * 2012/03/12   * RFS 107180 JBGENACHLD IN MESSAGE  */
002800120417    /*                           *            WAIT ON: CIBCACC       */
002900120411    /* SURESH  G. * 2012/04/04   * RFS107721 -To Retrieves File From */
003000120411    /*            *              *            IFS Directory or EFT   */
003100120411    /*            *              *            Library & Calls Program*/
003200120411    /*            *              *            to Update Assignments. */
003300120423    /* THILAGA K. * 2012/03/12   * RFS 108490  JBGENACHLD IN MESSAGE */
003400120423    /*                           * WAIT MAXIMUM RECORD IN CDHF       */
003500131021    /* BHANU D    * 2013/10/15   * RFS 117248 To add logic to the    */
003600131021    /*            *              * DJT Environment                   */
003700201007    /* Mayilsamy D* 2020/10/07   * RFS185121 - Replace the hard      */
003800201007    /*            *              * coded edit codes for DJT and NWT. */
003900111214    /*****************************************************************/
004000111214             PGM
004100111214             DCL        VAR(&ASATDT)      TYPE(*CHAR)  LEN(8)
004200111215             DCL        VAR(&YYYYMMDD)    TYPE(*CHAR)  LEN(8)
004300111216             DCL        VAR(&YYMMDD)      TYPE(*CHAR)  LEN(6)
004400111214             DCL        VAR(&CMPCDE)      TYPE(*CHAR)  LEN(3)
004500131114/*117248*/   DCL        VAR(&CMPCODE)     TYPE(*CHAR)  LEN(3) +
004600131114/*117248*/                                VALUE('DJT')
004700111214             DCL        VAR(&MFAEFTLIB)   TYPE(*CHAR)  LEN(10)
004800111215             DCL        VAR(&OBJLIB)      TYPE(*CHAR)  LEN(10)
004900111216             DCL        VAR(&FROMIFS)     TYPE(*CHAR)  LEN(300)
005000111220             DCL        VAR(&TOIFS)       TYPE(*CHAR)  LEN(100)
005100111214             DCL        VAR(&CMPNME)      TYPE(*CHAR)  LEN(30)
005200111216             DCL        VAR(&MESSAGE)     TYPE(*CHAR)  LEN(30)
005300111216             DCL        VAR(&FILENAME)    TYPE(*CHAR)  LEN(30)
005400111220             DCL        VAR(&NEWFILE)     TYPE(*CHAR)  LEN(30)
005500120411             DCL        VAR(&ATL)         TYPE(*CHAR)  LEN(3)    +
005600120411                                                  VALUE('ATL')
005700120411             DCL        VAR(&CBL)         TYPE(*CHAR)  LEN(3)    +
005800120411                                                  VALUE('CBL')
005900120411   /* RFS 107180 - START */
006000120411             DCL        VAR(&AT1)         TYPE(*CHAR)  LEN(3)    +
006100120411                                                   VALUE('AT1')
006200120411             DCL        VAR(&AT2)         TYPE(*CHAR)  LEN(3)    +
006300120411                                                   VALUE('AT2')
006400120411             DCL        VAR(&AT4)         TYPE(*CHAR)  LEN(3)    +
006500120411                                                   VALUE('AT4')
006600120411   /* RFS 107180 - END   */
006700120411             DCL        VAR(&SpfName)     TYPE(*CHAR)  LEN(30)       +
006800111216                                                VALUE("CDICEXP")
006900111216             DCL        VAR(&REPLY)       TYPE(*CHAR) LEN(1)
007000111230             DCL        VAR(&EFTEXST)     TYPE(*CHAR) LEN(1)
007100120411   /*RFS107721 Start */
007200120411            DCL        VAR(&CDHFFND)     TYPE(*CHAR)  LEN(1)         +
007300120411                                                   VALUE(' ')
007400120411            DCL        VAR(&RDHFFND)     TYPE(*CHAR)  LEN(1)         +
007500120411                                                   VALUE(' ')
007600120411            DCL        VAR(&HFILE)       TYPE(*CHAR)  LEN(4)         +
007700120411                                                   VALUE(' ')
007800120411   /*RFS107721 End   */
007900201007   /* RFS185121 - STARTS */
008000201007            DCL        VAR(&DJTNWTSCR)   TYPE(*CHAR) LEN(8)
008100201007            DCL        VAR(&DJTNWTEDT)   TYPE(*CHAR) LEN(8)
008200201007            DCL        VAR(&DJTNWTLVL)   TYPE(*CHAR) LEN(8)
008300201007            DCL        VAR(&DJTNWTFLG)   TYPE(*CHAR) LEN(1)
008400201007   /* RFS185121 - ENDS   */
008500111216
008600120411
008700111214    /* RETRIEVE PROCESS DATE                                         */
008800111214             RTVDTAARA  DTAARA(MFAPRCDTP (1 8)) RTNVAR(&ASATDT)
008900111227             CHGVAR     VAR(&YYYYMMDD)    VALUE(&ASATDT)
009000111216             CHGVAR     VAR(&YYMMDD)    VALUE(%SST(&ASATDT 3 6))
009100111216
009200111215
009300111214    /* RETRIEVE COMPANY CODE                                         */
009400111214             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCDE)
009500111214
009600111214    /* RETRIEVE EFT LIBRARY NAME                                     */
009700111214             RTVDTAARA  DTAARA(MFAEFTLIB  *ALL) RTNVAR(&MFAEFTLIB)
009800111214
009900111214    /* RETRIEVE COMPANY NAME                                         */
010000111214             RTVDTAARA  DTAARA(MFACMPNMA (1 30)) RTNVAR(&CMPNME)
010100111214
010200111227    /* GET THE CLIENT'S OJ LIBRARY. */
010300111227           RTVDTAARA  DTAARA(MFAOBJECTS (1 10)) RTNVAR(&OBJLIB)
010400111227
010500111214    /* CLEAR FILE   WRKACCHLD AND CDICRPTP                           */
010600120102RETRY:       CLRPFM     FILE(&OBJLIB/WRKACCHLD)
010700111214             MONMSG     MSGID(CPF0000)
010800111227             CLRPFM     FILE(&OBJLIB/CDICRPTP)
010900111214             MONMSG     MSGID(CPF0000)
011000111214
011100111214    /* CHECK FOR FILE WRKACCHLD                                      */
011200120102             CHKOBJ     OBJ(&OBJLIB/WRKACCHLD) OBJTYPE(*FILE)
011300111214             MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(EXIT))
011400111214
011500111228    /*CHECK OBJECT CDHF IN EFT LIBRARY                */
011600111230             CHGVAR     VAR(&EFTEXST) VALUE(' ')
011700111228             CHKOBJ     OBJ(&MFAEFTLIB/CDHF) OBJTYPE(*FILE)
011800120424    /*RFS107721 START */
011900120424    /*       MONMSG     MSGID(CPF9801)                   +
012000120424                        EXEC(CHGVAR VAR(&EFTEXST) VALUE('N') )       */
012100111228             MONMSG     MSGID(CPF9801)                   +
012200120424                        EXEC(CHGVAR VAR(&CDHFFND) VALUE('N'))
012300120424             IF COND(&CDHFFND *NE 'N') THEN(DO)
012400120424                CHGVAR VAR(&HFILE) VALUE('CDHF')
012500120424             ENDDO
012600120424             IF COND(&CDHFFND *EQ 'N') THEN(DO)
012700120424                CHKOBJ     OBJ(&MFAEFTLIB/RDHF) OBJTYPE(*FILE)
012800120424                MONMSG     MSGID(CPF9801)                            +
012900120424                           EXEC(CHGVAR VAR(&RDHFFND) VALUE('N'))
013000120424                IF COND(&RDHFFND *NE 'N') THEN(DO)
013100120424                   CHGVAR VAR(&HFILE) VALUE('RDHF')
013200120424                ENDDO
013300120424             ENDDO
013400120424             IF COND(&CDHFFND *EQ 'N' *AND &RDHFFND *EQ 'N')         +
013500120424               THEN(CHGVAR VAR(&EFTEXST) VALUE('N'))
013600120424    /*RFS107721 END   */
013700120424
013800111228    /* IF NOT EXISTS CHECK IN IFS FOLDER                             */
013900111228             IF        COND(&EFTEXST *EQ 'N') THEN(DO)
014000131021    /* 117248 - START  */
014100201007 /*RFS185121 START */
014200201007 /* ------------------------------------------------------------------*/
014300201007 /* RFS 185121: DJTNWT EDIT CODE */
014400201007 /* ------------------------------------------------------------------*/
014500201007 DJTSTSUP:
014600201007                CHGVAR     VAR(&DJTNWTSCR) VALUE('DJTCMP')
014700201007                CHGVAR     VAR(&DJTNWTEDT) VALUE('DJTNWT')
014800201007                CHGVAR     VAR(&DJTNWTLVL) VALUE('N')
014900201007                CALL       PGM(FXGTDJTNWT) PARM(&DJTNWTSCR &DJTNWTEDT +
015000201007                             &DJTNWTLVL &DJTNWTFLG)
015100201007 /* RFS185121- ENDS*/
015200131021    /* FOR DJT                                                      */
015300201007    /*RFS185121 - Starts                                            */
015400201008    /*          IF COND(&CMPCODE *EQ  &CMPCDE )  +                  */
015500201008    /*             THEN(DO)                                         */
015600201008                IF COND(&DJTNWTFLG *EQ 'Y')  THEN(DO)
015700201007    /*RFS185121 -Ends                                               */
015800131021                 CHGVAR     VAR(&FILENAME) VALUE('CDICDJTHOLD' +
015900131021                               || &YYYYMMDD || '.DAT')
016000131021                ENDDO
016100201007    /*RFS185121 - Starts                                            */
016200201007    /*          IF COND(&CMPCODE *EQ  &CMPCDE ) THEN(DO)            */
016300201008                IF COND(&DJTNWTFLG *EQ 'Y')     THEN(DO)
016400201007    /*RFS185121 - Ends                                              */
016500131021                   CHGVAR     VAR(&FROMIFS)                           +
016600131021                             VALUE('/ATL/FD_INBOUND/' ||  &FILENAME)
016700131021                ENDDO
016800111215    /* FOR ATL                                                       */
016900120411    /* RFS107180 - Start  */
017000120411                IF COND(&CMPCDE *EQ  &ATL *OR   &CMPCDE *EQ  &AT1     +
017100120411                   *OR   &CMPCDE *EQ  &AT2  *OR   &CMPCDE *EQ  &AT4)  +
017200120411                                                  THEN(DO)
017300120411                   CHGVAR     VAR(&FILENAME) VALUE('CDICURHISAHOLD'   +
017400120411                                          ||    &YYYYMMDD || '.DAT')
017500120411                ENDDO
017600120411                IF COND(&CMPCDE *EQ  &ATL ) THEN(DO)
017700120411                   CHGVAR     VAR(&FROMIFS)                           +
017800120411                             VALUE('/ATL/FD_INBOUND/' ||  &FILENAME)
017900120411                ENDDO
018000120411                IF COND(&CMPCDE *EQ  &AT1 ) THEN(DO)
018100120411                   CHGVAR     VAR(&FROMIFS)                           +
018200120411                             VALUE('/AT1/FD_INBOUND/' ||  &FILENAME)
018300120411                ENDDO
018400120411                IF COND(&CMPCDE *EQ  &AT2 ) THEN(DO)
018500120411                   CHGVAR     VAR(&FROMIFS)                           +
018600120411                             VALUE('/AT2/FD_INBOUND/' ||  &FILENAME)
018700120411                ENDDO
018800120411                IF COND(&CMPCDE *EQ  &AT4 ) THEN(DO)
018900120411                   CHGVAR     VAR(&FROMIFS)                       +
019000120411                             VALUE('/AT4/FD_INBOUND/' ||  &FILENAME)
019100120411                ENDDO
019200120416    /*          IF COND(&CMPCDE *EQ  &ATL ) THEN(DO)                 */
019300120416    /*             CHGVAR     VAR(&FILENAME) VALUE('CDICURHISAHOLD'  +
019400120411                                          ||    &YYYYMMDD || '.DAT') */
019500120416    /*             CHGVAR     VAR(&FROMIFS)                       +
019600120411                             VALUE('/HOME/UNISEN/ATL/FD_INBOUND'     +
019700120411                                           ||  &FILENAME)            */
019800120416    /*          ENDDO                                                */
019900111215    /* FOR CBL                                                       */
020000120411                IF COND(&CMPCDE *EQ &CBL) THEN(DO)
020100120411                   CHGVAR    VAR(&FILENAME) VALUE('CDICUCITIHOLD' || +
020200120411                                           &YYYYMMDD || '.DAT')
020300120411    /* RFS 107180 - END   */
020400120411    /* 107180      CHGVAR     VAR(&FROMIFS)                          +
020500120411                              VALUE("/HOME/UNISEN/ATL/FD_INBOUND" || +
020600120411                                                &FILENAME)           */
020700120411                   CHGVAR     VAR(&FROMIFS)                          +
020800120411                              VALUE("/CBL/FD_INBOUND/" || +
020900120411                                                &FILENAME)
021000111219                ENDDO
021100120425    /*RFS107721 Start */
021200120425                IF COND(&CMPCDE *EQ  &ATL *OR   &CMPCDE *EQ  &AT1    +
021300120425                   *OR   &CMPCDE *EQ  &AT2  *OR   &CMPCDE *EQ  &AT4  +
021400131021/*117248           *OR   &CMPCDE *EQ  &CBL)      THEN(DO)       */  +
021500131107/*117248*/         *OR   &CMPCDE *EQ  &CBL  *OR    +
021600201008 /*RFS185121 - STARTS      */                      +
021700201008/*117248*//*             &CMPCODE *EQ  &CMPCDE ) */+
021800201008                         &DJTNWTFLG *EQ 'Y' )      +
021900201008 /*RFS185121 - ENDS        */                      +
022000131021/*117248*/         THEN(DO)
022100120425    /*RFS107721 End   */
022200120425                   CRTPF  FILE(&MFAEFTLIB/CDHF) RCDLEN(700)          +
022300120425                          TEXT('CDIC ASSIGNMENT HOLD')               +
022400120425 /*RFS108490*/            MAXMBRS(*NOMAX) SIZE(*NOMAX)
022500111228
022600120425                   CHGVAR VAR(&TOIFS) VALUE('/QSYS.LIB/' *TCAT       +
022700120425                          &MFAEFTLIB *TCAT '.LIB/CDHF.FILE/CDHF.MBR')
022800111228
022900120425                   CPYFRMSTMF FROMSTMF(&FROMIFS)                     +
023000120425                              TOMBR(&TOIFS  )                        +
023100120425                              MBROPT(*REPLACE) CVTDTA(*AUTO)         +
023200120425                              STMFCODPAG(*STMF) ENDLINFMT(*CRLF)
023300120425 /*RFS107721*/  ENDDO
023400120425
023500111228           ENDDO
023600111228
023700111219     /*CHECK OBJECT CDHF IN EFT LIBRARY                */
023800120424   /*RFS107721 Start */
023900120424             IF COND(&EFTEXST *EQ 'N') THEN(DO)
024000120424                CHKOBJ     OBJ(&MFAEFTLIB/CDHF) OBJTYPE(*FILE)
024100120424                MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(USERMSG))
024200120425                CHGVAR     VAR(&HFILE) VALUE('CDHF')
024300120424             ENDDO
024400120424   /*        MONMSG     MSGID(CPF9801)                               +
024500120424                        EXEC(CHGVAR VAR(&CDHFFND) VALUE('N'))           */
024600120424   /*        IF COND(&CDHFFND *NE 'N') THEN(DO)                         */
024700120424   /*           CHGVAR VAR(&HFILE) VALUE('CDHF')                        */
024800120424   /*        ENDDO                                                      */
024900120424   /*        IF COND(&CDHFFND *EQ 'N') THEN(DO)                         */
025000120424   /*           CHKOBJ     OBJ(&MFAEFTLIB/RDHF) OBJTYPE(*FILE)          */
025100120424   /*           MONMSG     MSGID(CPF9801)                            +
025200120424                           EXEC(CHGVAR VAR(&RDHFFND) VALUE('N'))        */
025300120424   /*           IF COND(&RDHFFND *NE 'N') THEN(DO)                      */
025400120424   /*              CHGVAR VAR(&HFILE) VALUE('RDHF')                     */
025500120424   /*           ENDDO                                                   */
025600120424   /*        ENDDO                                                      */
025700120424   /*        IF COND(&CDHFFND *EQ 'N' *AND &RDHFFND *EQ 'N')         +
025800120424                THEN(GOTO CMDLBL(USERMSG))                              */
025900120424   /*RFS107721 End   */
026000120424
026100120411    /* COPY FILE FROM EFT LIBRARY TO WRKACCHLD.                      */
026200120411   /*RFS107721 Start */
026300120411/*COPY:     CPYF      FROMFILE(&MFAEFTLIB/CDHF)                      +
026400120411                      TOFILE(&OBJLIB/WRKACCHLD)                      +
026500120411                        MBROPT(*REPLACE)  FMTOPT(*NOCHK)             */
026600120411COPY:       CPYF      FROMFILE(&MFAEFTLIB/&HFILE)                    +
026700120411                      TOFILE(&OBJLIB/WRKACCHLD)                      +
026800120411                      MBROPT(*REPLACE) FMTOPT(*NOCHK)
026900120411   /*RFS107721 End   */
027000111215           MONMSG     MSGID(CPF2817) EXEC(GOTO CMDLBL(USERMSG))
027100111216
027200111216           CALL       PGM(ACCHLDUPD)
027300120411   /*RFS107721 Start */
027400120411   /*      CHGVAR     VAR(&NEWFILE) VALUE('CDHF' || &YYMMDD )        */
027500120411           CHGVAR     VAR(&NEWFILE) VALUE(&HFILE || &YYMMDD )
027600120411   /*      CPYF       FROMFILE(&MFAEFTLIB/CDHF)                      +
027700120411                      TOFILE(&MFAEFTLIB/&NEWFILE)                    +
027800120411                        MBROPT(*REPLACE) CRTFILE(*YES) FMTOPT(*NOCHK)*/
027900120411           CPYF       FROMFILE(&MFAEFTLIB/&HFILE)                    +
028000120411                      TOFILE(&MFAEFTLIB/&NEWFILE)                    +
028100120411                      MBROPT(*REPLACE) CRTFILE(*YES) FMTOPT(*NOCHK)
028200120411   /*      DLTF       FILE(&MFAEFTLIB/CDHF)                          */
028300120411           DLTF       FILE(&MFAEFTLIB/&HFILE)
028400120411   /*RFS107721 End   */
028500120411           OVRPRTF    FILE(SQLPRT1) SAVE(*YES) MAXRCDS(*NOMAX)       +
028600111216                      USRDTA(&SpfName)
028700111220        /* REPORT    REPORT(CDICHFERPT) VIEW(CDICHFEV)            */
028800111220             CALL       PGM(JOBSEQLRUN) PARM('CDICHFERPT' 'CDICHFEV' +
028900111220                          ' ' ' ' ' ')
029000120207        /* RFS102778 -START */
029100120229             CALL       PGM(CDICHLDEX) PARM('H')
029200120207        /* RFS102778 -END */
029300120207
029400111226           GOTO CMDLBL(EXIT)
029500111226
029600111215USERMSG:
029700111216           CHGVAR    VAR(&MESSAGE) VALUE('File Not Found')
029800111216           SNDUSRMSG  MSG(&MESSAGE) VALUES('C' 'c' 'R' 'c' 'I' 'i') +
029900111215                           TOMSGQ(*SYSOPR) MSGRPY(&REPLY)
030000111215
030100111216           IF         COND(&REPLY *EQ 'C' *OR &REPLY *EQ 'c' *OR   +
030200111216                           &REPLY *EQ 'I' *OR &REPLY *EQ 'i') THEN(+
030300111216                      GOTO CMDLBL(EXIT))
030400111216
030500111216           IF         COND(&REPLY *EQ 'R' *OR &REPLY *EQ 'r') +
030600111230                      THEN(GOTO CMDLBL(RETRY))
030700111215
030800111216EXIT:
030900111216           ENDPGM
031000111214
