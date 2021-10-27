000100060109/*----------------------------------------------------------------*/
000200060109/* PROGRAM     : CMSOBJUPD                                        */
000300060109/* TYPE        : CLP                                              */
000400060109/* PROGRAMMER  : KEVIN TROTMAN                                    */
000500060109/*----------------------------------------------------------------*/
000600060109/* CREATED ON  : DEC 2005                                         */
000700060109/* CALLED BY   :                                                  */
000800060109/* DESCRIPTION : ALDONCMS can re-map objects within its own       */
000900060109/*               repository , but cannot affect objects in the    */
001000060109/*               client data libraries.  This utility will        */
001100060109/*               accomplish that task.                            */
001200060109/*                                                                */
001300060109/* Parameters  : RFS  - RFS #                                     */
001400060109/*               OBJ  - Object name                               */
001500060109/*               TYP  - Object Type                               */
001600060109/*               FLIB - From Library (Use the dataarea)           */
001700060109/*               TLIB - To Library   (Use the dataarea)           */
001800060109/*               MOD  - Mode - M-Move object                      */
001900060109/*                           - D-Delete object                    */
002000060109/*                           - C-Copy Object                      */
002100060109/*                           - R-Generate Report of activity      */
002200060109/*                                                                */
002300060109/* Note: This procedure will only copy within the confines of a   */
002400060109/*       client environment MFADBASE to MFATABLES.  It uses the   */
002500060109/*       data areas of the environment to establish the what libs */
002600060109/*       to use.  Ownership & Authority are maintained            */
002700060109/*                                                                */
002800060109/* Also: When the object type is a file ALL logicals are included */
002900060109/*       in the action. If a Logical file is specified to be      */
003000060109/*       copied or moved the target physical must exsist          */
003100060109/*                                                                */
003200060109/* Warn: The process will fail under the following conditions     */
003300060109/*       - object locked                                          */
003400060109/*       - object missing                                         */
003500060109/*       - to/from library missing                                */
003600060109/*       - files are part of a join logical                       */
003700060109/*       - invalid to/from data area name parm                    */
003800060109/*       - invalid mode                                           */
003900060109/*                                                                */
004000060811/*----------------------------------------------------------------*/
004100060811/*  P R O G R A M   M O D I F I C A T I O N S                     */
004200060811/*----------------------------------------------------------------*/
004300060811/* DATE     ¦ PROGRAMMER      ¦ DESCRIPTION                       */
004400060811/*----------------------------------------------------------------*/
004500061003/* 20060801 ¦ Odette A.       ¦ RFS37438 - DLTOBJ cannot use pgm  */
004600060811/*          ¦                 ¦ variable for Type - need to       */
004700060811/*          ¦                 ¦ provide the literal. For objects  */
004800060811/*          ¦                 ¦ only.                             */
004900140528/* 20140528 ¦ Kamal T         ¦ RFS 137052 - Adding SIZE parameter*/
005000140528/*          ¦                 ¦ as *NOMAX in CRTPF COMMAND        */
005100060811/*----------------------------------------------------------------*/
005200060109CMSOBJUPD:  PGM        PARM(&RFS &OBJ &TYP &FLIB &TLIB &MOD)
005300060109/* Input Parameters         */
005400060109             DCL        VAR(&RFS)  TYPE(*CHAR) LEN(10)
005500060109             DCL        VAR(&OBJ)  TYPE(*CHAR) LEN(10)
005600060109             DCL        VAR(&TYP)  TYPE(*CHAR) LEN(10)
005700060109             DCL        VAR(&FLIB) TYPE(*CHAR) LEN(10)
005800060109             DCL        VAR(&TLIB) TYPE(*CHAR) LEN(10)
005900060109             DCL        VAR(&MOD)  TYPE(*CHAR) LEN(01)
006000060109/* Working Storage          */
006100060109             DCL        VAR(&ATR)  TYPE(*CHAR) LEN(10)
006200060109             DCL        VAR(&FLB)  TYPE(*CHAR) LEN(10)
006300060109             DCL        VAR(&TLB)  TYPE(*CHAR) LEN(10)
006400060109             DCL        VAR(&CNT)  TYPE(*DEC)  LEN(05)
006500060109             DCL        VAR(&CMD)  TYPE(*CHAR) LEN(500)
006600060109             DCL        VAR(&OWN)  TYPE(*CHAR) LEN(10)
006700060109             DCL        VAR(&MSG)  TYPE(*CHAR) LEN(80)
006800060109             DCL        VAR(&QTE) TYPE(*CHAR) LEN(2) VALUE('"')
006900060109
007000060109             DCL        VAR(&IRFS)  TYPE(*CHAR) LEN(15)
007100060109             DCL        VAR(&IOBJ)  TYPE(*CHAR) LEN(15)
007200060109             DCL        VAR(&ITYP)  TYPE(*CHAR) LEN(15)
007300060109             DCL        VAR(&IFLIB) TYPE(*CHAR) LEN(15)
007400060109             DCL        VAR(&ITLIB) TYPE(*CHAR) LEN(15)
007500060109             DCL        VAR(&IMOD)  TYPE(*CHAR) LEN(05)
007600060109
007700060109/* File Declaration         */
007800060111             DCLF       FILE(QADSPDBR) OPNID(DB1)
007900060111             DCLF       FILE(QADSPDBR) OPNID(DB2)
008000060111             DCLF       FILE(QADSPDBR) OPNID(DB3)
008100060111             DCLF       FILE(QADSPDBR) OPNID(DB4)
008200060109/* Procedure Division       */
008300060109             ADDLIBLE   LIB(SEQUEL)
008400060109             MONMSG     MSGID(CPF0000)
008500060109
008600060109/*-- Create temp reporting dataarea */
008700060109             CRTDTAARA  DTAARA(QTEMP/MSG) TYPE(*CHAR) LEN(80)
008800060109             MONMSG     MSGID(CPF1023) EXEC( +
008900060109                CHGDTAARA  DTAARA(QTEMP/MSG) VALUE(' '))
009000060109
009100060109/*-- Create record dataarea */
009200060109             CRTDTAARA  DTAARA(QTEMP/REC) TYPE(*DEC) LEN(05)
009300060109             MONMSG     MSGID(CPF1023) EXEC( +
009400060109                CHGDTAARA  DTAARA(QTEMP/REC) VALUE(0))
009500060109
009600060109/*-- Create SPLF work file  */
009700140528/* RFS137052 Begin */
009800140528/*           CRTPF      FILE(QTEMP/SPLF) RCDLEN(150)   */
009900140528             CRTPF      FILE(QTEMP/SPLF) RCDLEN(150) SIZE(*NOMAX)
010000140528/* RFS137052 End   */
010100060109             MONMSG     MSGID(CPF0000) EXEC(CLRPFM FILE(QTEMP/SPLF))
010200060109
010300060109/*-- Create report file  */
010400060109             CHKOBJ     OBJ(QTEMP/RPTF) OBJTYPE(*FILE)
010500060109             MONMSG     MSGID(CPF0000) EXEC(DO)
010600060109             DYNSQL     SQL('CREATE TABLE QTEMP/RPTFILP +
010700060109                          (RFSNO FOR COLUMN RFS CHAR ( 10) +
010800060109                                    NOT NULL WITH DEFAULT,  +
010900060109                          OBJNAME FOR COLUMN OBJ CHAR ( 10) +
011000060109                          NOT NULL WITH DEFAULT, OBJTYPE FOR +
011100060109                          COLUMN TYP CHAR ( 10) NOT NULL WITH +
011200060109                          DEFAULT, FROM_LIB FOR COLUMN FLIB +
011300060109                          CHAR ( 10) NOT NULL WITH DEFAULT, +
011400060109                          TO_LIB FOR COLUMN TLIB CHAR ( 10) NOT +
011500060109                          NULL WITH DEFAULT, ACT_MODE FOR COLUMN +
011600060109                          MOD CHAR ( 1) NOT NULL WITH DEFAULT, +
011700060109                          ACT_MSG FOR COLUMN MSG CHAR (80 ) NOT +
011800060109                          NULL WITH DEFAULT)')
011900060109             ENDDO
012000060109
012100060109/*-- Validate & retrieve Source dataarea library*/
012200060109             IF (&MOD *NE 'D' *AND +
012300060109                 &MOD *NE 'M' *AND +
012400060109                 &MOD *NE 'C' *AND +
012500060109                 &MOD *NE 'R') +
012600060109             THEN(DO)
012700060109             CHGDTAARA  DTAARA(MSG) VALUE('Invalid MODE :' *TCAT &MOD)
012800060109             DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
012900060109             GOTO LOGIT
013000060109             ENDDO
013100060109
013200060109             IF (&MOD *EQ 'R' ) THEN(GOTO REPORTIT)
013300060109
013400060109/*-- Validate & retrieve Source dataarea library*/
013500060109             RTVDTAARA  DTAARA(&FLIB) RTNVAR(&FLB)
013600060109             MONMSG     MSGID(CPF1015) EXEC(DO)
013700060109             CHGDTAARA  DTAARA(MSG) VALUE('Invalid Source Data +
013800060109                          Area:' *TCAT &FLIB)
013900060109             DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
014000060109             GOTO LOGIT
014100060109             ENDDO
014200060109
014300060109/*-- Validate & retrieve Destination dataarea library*/
014400060109             IF (&TLIB *NE ' ') THEN(DO)
014500060109                RTVDTAARA  DTAARA(&TLIB) RTNVAR(&TLB)
014600060109                MONMSG     MSGID(CPF1015) EXEC(DO)
014700060109             CHGDTAARA  DTAARA(MSG) VALUE('Invalid Destination Data +
014800060109                          Area:' *TCAT &TLIB)
014900060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
015000060109                GOTO LOGIT
015100060109                ENDDO
015200060109
015300060109/*--- Validate Source & Destination library are NOT the same*/
015400060109                IF (&TLB *EQ &FLB) THEN(DO)
015500060109             CHGDTAARA  DTAARA(MSG) VALUE('Source & Destination the +
015600060109                          same:' *TCAT &TLB)
015700060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
015800060109                GOTO LOGIT
015900060109                ENDDO
016000060109
016100060109/*--- Validate Destination library existence */
016200060109                CHKOBJ     OBJ(QSYS/&TLB) OBJTYPE(*LIB)
016300060109                MONMSG     MSGID(CPF1015) EXEC(DO)
016400060109             CHGDTAARA  DTAARA(MSG) VALUE('Destination Library +
016500060109                          Missing:' *TCAT &TLB)
016600060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
016700060109                GOTO LOGIT
016800060109                ENDDO
016900060109             ENDDO
017000060109
017100060109/*-- Validate Source library existence */
017200060109                CHKOBJ     OBJ(QSYS/&FLB) OBJTYPE(*LIB)
017300060109                MONMSG     MSGID(CPF1015) EXEC(DO)
017400060109             CHGDTAARA  DTAARA(MSG) VALUE('Source Library Missing:' +
017500060109                          *TCAT &FLB)
017600060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
017700060109                GOTO LOGIT
017800060109                ENDDO
017900060109/*-- Check Object Exists */
018000060109             IF         COND(&MOD *EQ 'M' *OR &MOD *EQ 'C') THEN(DO)
018100060109                CHKOBJ     OBJ(&FLB/&OBJ) OBJTYPE(&TYP)
018200060109                MONMSG     MSGID(CPF9801) EXEC(DO)
018300060109                   CHKOBJ     OBJ(&TLB/&OBJ) OBJTYPE(&TYP)
018400060109                   MONMSG     MSGID(CPF9801) EXEC(DO)
018500060109                   CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
018600060109                          *BCAT ' not found ')
018700060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
018800060109                   GOTO LOGIT
018900060109                   ENDDO
019000060109                   CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
019100060109                          *BCAT ' already moved ')
019200060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
019300060109                   GOTO LOGIT
019400060109                ENDDO
019500060109                IF (&MOD *EQ 'C') THEN(DO)
019600060109                   CHKOBJ     OBJ(&TLB/&OBJ) OBJTYPE(&TYP)
019700060109                   MONMSG     MSGID(CPF9801) EXEC(DO)
019800060109                     GOTO NEXTCHK
019900060109                   ENDDO
020000060109                   CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
020100060109                          *BCAT ' already COPIED')
020200060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
020300060109                   GOTO LOGIT
020400060109                ENDDO
020500060109             ENDDO
020600060109 NEXTCHK:
020700060109             IF         COND(&MOD *EQ 'D') THEN(DO)
020800060109                CHKOBJ     OBJ(&FLB/&OBJ) OBJTYPE(&TYP)
020900060109                MONMSG     MSGID(CPF9801) EXEC(DO)
021000060109                   CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
021100060109                          *BCAT ' already deleted ')
021200060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
021300060109                GOTO LOGIT
021400060109                ENDDO
021500060109             ENDDO
021600060109
021700060109
021800060109/*-- Check for Object Locks */
021900060109             WRKOBJLCK  OBJ(&FLB/&OBJ) OBJTYPE(&TYP) OUTPUT(*PRINT)
022000060109             CPYSPLF FILE(QPDSPOLK) TOFILE(QTEMP/SPLF) SPLNBR(*LAST)
022100060109             CHGDTAARA  DTAARA(QTEMP/REC) VALUE(0)
022200060109             RUNCMD     CMD('CHGDTAARA  DTAARA(QTEMP/REC) +
022300060109                          VALUE(&CNT)') SQL('SELECT COUNT(*) +
022400060109                          NAME(CNT) LEN(5,0) FROM QTEMP/SPLF WHERE +
022500060109                          SST(SPLF,37,1) = "*"')
022600060109
022700060109             RTVDTAARA  DTAARA(QTEMP/REC) RTNVAR(&CNT)
022800060109             IF (&CNT *GT 0) THEN(DO)
022900060109                CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
023000060109                          *BCAT ' is locked ')
023100060109                   DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
023200060109                GOTO LOGIT
023300060109             ENDDO
023400060109             DLTSPLF    FILE(QPDSPOLK) SPLNBR(*LAST)
023500060109
023600060109 /*-- Retrieve Object Attribute */
023700060109             RTVOBJD    OBJ(&FLB/&OBJ) OBJTYPE(&TYP) OBJATR(&ATR) +
023800060109                          OWNER(&OWN)
023900060109
024000060109 /*-- Process - Files --*/
024100060109             IF         COND(&TYP *EQ '*FILE' *AND &ATR *EQ 'PF') +
024200060109                          THEN(DO)
024300060111             DLTF       FILE(QTEMP/DBRLST*)
024400060110             MONMSG     MSGID(CPF0000)
024500060110
024600060109             DSPDBR     FILE(&FLB/&OBJ) OUTPUT(*OUTFILE) +
024700060109                          OUTFILE(QTEMP/DBRLST)
024800060109             MONMSG     MSGID(CPF0000)
024900060111             CHGPF      FILE(QTEMP/DBRLST) MAXMBRS(*NOMAX)
025000060111             MONMSG     MSGID(CPF0000)
025100060111             CPYF       FROMFILE(QTEMP/DBRLST) TOFILE(QTEMP/DBRLST) +
025200060111                          FROMMBR(DBRLST) TOMBR(DB1) MBROPT(*ADD)
025300060111             CPYF       FROMFILE(QTEMP/DBRLST) TOFILE(QTEMP/DBRLST) +
025400060111                          FROMMBR(DBRLST) TOMBR(DB2) MBROPT(*ADD)
025500060111             CPYF       FROMFILE(QTEMP/DBRLST) TOFILE(QTEMP/DBRLST) +
025600060111                          FROMMBR(DBRLST) TOMBR(DB3) MBROPT(*ADD)
025700060111             CPYF       FROMFILE(QTEMP/DBRLST) TOFILE(QTEMP/DBRLST) +
025800060111                          FROMMBR(DBRLST) TOMBR(DB4) MBROPT(*ADD)
025900060109
026000060109             CHGDTAARA  DTAARA(QTEMP/REC) VALUE(0)
026100060109             RUNCMD     CMD('CHGDTAARA  DTAARA(QTEMP/REC) +
026200060109                          VALUE(&CNT); MONMSG MSGID(CPF0000)') +
026300060109                          SQL('SELECT COUNT(*) NAME(CNT) LEN(5,0)  +
026400060109                          FROM QTEMP/DBRLST WHERE (WHRLI <> WHRELI +
026500060109                          AND WHRELI <> " ") OR  WHJREF <> 0 ')
026600060109
026700060109             RTVDTAARA  DTAARA(QTEMP/REC) RTNVAR(&CNT)
026800060109             IF         COND(&CNT *GT 0) THEN(DO)
026900060109                CHGDTAARA  DTAARA(MSG) VALUE('File:' *BCAT &OBJ +
027000060109                          *BCAT ' attached to Join logical')
027100060109                DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
027200060109                PRINT SQL('SELECT WHRLI , WHRFI , WHRELI, WHREFI +
027300060109                          FROM QTEMP/DBRLST WHERE (WHRLI <> WHRELI +
027400060109                          AND WHRELI <> " ") OR  WHJREF <> 0')
027500060109                GOTO LOGIT
027600060109             ENDDO
027700060109
027800060109/*--- Check Logicals for Locks */
027900060109             CLRPFM     FILE(QTEMP/SPLF)
028000060109             MONMSG     MSGID(CPF0000)
028100060109
028200060111             OVRDBF     FILE(QADSPDBR) TOFILE(QTEMP/DBRLST) MBR(DB1)
028300060111 GETDB1:     RCVF       OPNID(DB1)
028400060111                MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(GETDB1X))
028500060111                IF (&DB1_WHREFI *NE ' ') THEN(DO)
028600060111                    WRKOBJLCK  OBJ(&DB1_WHRELI/&DB1_WHREFI) +
028700060111                               OBJTYPE(*FILE) +
028800060111                               OUTPUT(*PRINT)
028900060111
029000060111                    CPYSPLF    FILE(QPDSPOLK) TOFILE(QTEMP/SPLF) +
029100060111                          SPLNBR(*LAST) MBROPT(*ADD)
029200060111
029300060111                    DLTSPLF    FILE(QPDSPOLK) SPLNBR(*LAST)
029400060111                ENDDO
029500060111                GOTO       CMDLBL(GETDB1)
029600060111 GETDB1X:
029700060111             DLTOVR     FILE(QADSPDBR)
029800060109
029900060109             CHGDTAARA  DTAARA(QTEMP/REC) VALUE(0)
030000060109             RUNCMD     CMD('CHGDTAARA  DTAARA(QTEMP/REC) +
030100060109                          VALUE(&CNT)') SQL('SELECT COUNT(*) +
030200060109                          NAME(CNT) LEN(5,0) FROM QTEMP/SPLF WHERE +
030300060109                          SST(SPLF,37,1) = "*"')
030400060109
030500060109             RTVDTAARA  DTAARA(QTEMP/REC) RTNVAR(&CNT)
030600060109             IF (&CNT *GT 0) THEN(DO)
030700060109                CHGDTAARA  DTAARA(MSG) VALUE('Object:' *BCAT &OBJ +
030800060109                          *BCAT ' is locked - Logicals')
030900060109                DSPDTAARA  DTAARA(MSG) OUTPUT(*PRINT)
031000060109                PRINT      SQL('SELECT * FROM QTEMP/SPLF WHERE +
031100060109                          SST(SPLF,37,1) = "*"')
031200060109                GOTO LOGIT
031300060109             ENDDO
031400060109
031500060109             IF         COND(&MOD *EQ 'M' *OR &MOD *EQ 'C') THEN(DO)
031600060109                 CPYF FROMFILE(&FLB/&OBJ) TOFILE(&TLB/&OBJ) +
031700060109                          MBROPT(*ADD) CRTFILE(*YES) PRINT(*COPIED)
031800060109
031900060111             OVRDBF     FILE(QADSPDBR) TOFILE(QTEMP/DBRLST) MBR(DB2)
032000060111 GETDB2:     RCVF       OPNID(DB2)
032100060111                MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(GETDB2X))
032200060111                IF (&DB2_WHREFI *NE ' ') THEN(DO)
032300060111                   CRTDUPOBJ  OBJ(&DB2_WHREFI) FROMLIB(&DB2_WHRELI) +
032400060111                          OBJTYPE(*FILE) TOLIB(&TLB)
032500060111                ENDDO
032600060111                GOTO       CMDLBL(GETDB2)
032700060111 GETDB2X:
032800060111             DLTOVR     FILE(QADSPDBR)
032900060109             ENDDO
033000060109
033100060109/*--- Remove from Old location         */
033200060109             IF         COND(&MOD *EQ 'M' *OR &MOD *EQ 'D') THEN(DO)
033300060109
033400060111             OVRDBF     FILE(QADSPDBR) TOFILE(QTEMP/DBRLST) MBR(DB3)
033500060111 GETDB3:     RCVF       OPNID(DB3)
033600060111                MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(GETDB3X))
033700060111                IF (&DB3_WHREFI *NE ' ') THEN( +
033800060111                    DLTF       FILE(&DB3_WHRELI/&DB3_WHREFI) )
033900060111                GOTO       CMDLBL(GETDB3)
034000060111 GETDB3X:
034100060111             DLTOVR     FILE(QADSPDBR)
034200060111
034300060109             DLTF       FILE(&FLB/&OBJ)
034400060109             ENDDO
034500060109
034600060109/*--- Change Ownership & set authority */
034700060109             IF         COND(&MOD *EQ 'M' *OR &MOD *EQ 'C') THEN(DO)
034800060109                GRTOBJAUT  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) +
034900060109                          REFOBJ(ACMSREFOBJ)
035000060209                CHGOBJOWN  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) NEWOWN(&OWN)
035100060109                DSPDBR     FILE(&TLB/&OBJ) OUTPUT(*OUTFILE) +
035200060109                          OUTFILE(QTEMP/DBRLST)
035300060109                MONMSG     MSGID(CPF0000)
035400060111             OVRDBF     FILE(QADSPDBR) TOFILE(QTEMP/DBRLST) MBR(DB4)
035500060111 GETDB4:     RCVF       OPNID(DB4)
035600060111                MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(GETDB4X))
035700060111                IF (&DB4_WHREFI *EQ ' ') THEN( GOTO GETDB4 )
035800060111                   GRTOBJAUT  OBJ(&TLB/&DB4_WHREFI) OBJTYPE(&TYP) +
035900060111                          REFOBJ(ACMSREFOBJ)
036000060209                   CHGOBJOWN  OBJ(&TLB/&DB4_WHREFI) OBJTYPE(&TYP) +
036100060209                          NEWOWN(&OWN)
036200060109
036300060111                GOTO       CMDLBL(GETDB4)
036400060109             ENDDO
036500060111 GETDB4X:
036600060111             DLTOVR     FILE(QADSPDBR)
036700060111
036800060109             GOTO       CMDLBL(EXIT)
036900060109
037000060109             ENDDO
037100060109
037200060109/*--- PROCESS - OBJECTS */
037300060109
037400060109             IF         COND(&MOD *EQ 'M') THEN(DO)
037500060109             MOVOBJ     OBJ(&FLB/&OBJ) OBJTYPE(&TYP) TOLIB(&TLB)
037600060109             GRTOBJAUT  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) REFOBJ(ACMSREFOBJ)
037700060209             CHGOBJOWN  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) NEWOWN(&OWN)
037800060109             ENDDO
037900060109
038000060109             IF         COND(&MOD *EQ 'C') THEN(DO)
038100060109             CRTDUPOBJ  OBJ(&OBJ) FROMLIB(&FLB) OBJTYPE(&TYP) +
038200060109                          TOLIB(&TLB)
038300060109             GRTOBJAUT  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) REFOBJ(ACMSREFOBJ)
038400060209             CHGOBJOWN  OBJ(&TLB/&OBJ) OBJTYPE(&TYP) NEWOWN(&OWN)
038500060109             ENDDO
038600060109
038700060109             IF         COND(&MOD *EQ 'D') THEN(DO)
038800060811/*RFS36261 BEGIN*/
038900060811                IF  COND(&TYP *EQ '*PGM') THEN(DO)
039000060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*PGM)
039100060811                ENDDO
039200060811                IF  COND(&TYP *EQ '*MODULE') THEN(DO)
039300060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*MODULE)
039400060811                ENDDO
039500060811                IF  COND(&TYP *EQ '*SRVPGM') THEN(DO)
039600060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*SRVPGM)
039700060811                ENDDO
039800060811                IF  COND(&TYP *EQ '*CMD') THEN(DO)
039900060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*CMD)
040000060811                ENDDO
040100060811                IF  COND(&TYP *EQ '*USRSPC') THEN(DO)
040200060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*USRSPC)
040300060811                ENDDO
040400060811                IF  COND(&TYP *EQ '*PNLGRP') THEN(DO)
040500060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*PNLGRP)
040600060811                ENDDO
040700060811                IF  COND(&TYP *EQ '*QMQRY') THEN(DO)
040800060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*QMQRY)
040900060811                ENDDO
041000060811                IF  COND(&TYP *EQ '*QRYDFN') THEN(DO)
041100060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*QRYDFN)
041200060811                ENDDO
041300060811                IF  COND(&TYP *EQ '*QRYDFN') THEN(DO)
041400060811                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*QRYDFN)
041500060811                ENDDO
041600060811                IF  COND(&TYP *EQ '*MENU') THEN(DO)
041700140814                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*MENU)
041800060811                ENDDO
041900060811                IF  COND(&TYP *EQ '*USRIDX') THEN(DO)
042000140814                    DLTOBJ     OBJ(&FLB/&OBJ) TYPE(*USRIDX)
042100060811                ENDDO
042200060811/*RFS36261 END*/
042300060109             ENDDO
042400060109
042500060109REPORTIT:    IF         COND(&MOD *EQ 'R') THEN(DO)
042600060109
042700060109             CHKOBJ     OBJ(QTEMP/RPTFILP) OBJTYPE(*FILE)
042800060109             MONMSG     MSGID(CPF0000) EXEC(RETURN)
042900060109             PRINT      SQL('SELECT * FROM QTEMP/RPTFILP ORDER BY +
043000060109                          OBJ')
043100060109             DLTF       FILE(QTEMP/RPTFILP)
043200060109             ENDDO
043300060109
043400060109             GOTO EXIT
043500060109
043600060109LOGIT:
043700060109             RTVDTAARA  DTAARA(QTEMP/MSG) RTNVAR(&MSG)
043800060109             CHGSPLFA FILE(QPDSPDTA) SPLNBR(*LAST) USRDTA(&RFS)
043900060109             CHGVAR     VAR(&IRFS)  VALUE(&QTE *TCAT &RFS  *TCAT &QTE)
044000060109             CHGVAR     VAR(&IOBJ)  VALUE(&QTE *TCAT &OBJ  *TCAT &QTE)
044100060109             CHGVAR     VAR(&ITYP)  VALUE(&QTE *TCAT &TYP  *TCAT &QTE)
044200060109             CHGVAR     VAR(&IFLIB) VALUE(&QTE *TCAT &FLIB *TCAT &QTE)
044300060109             CHGVAR     VAR(&ITLIB) VALUE(&QTE *TCAT &TLIB *TCAT &QTE)
044400060109             CHGVAR     VAR(&IMOD)  VALUE(&QTE *TCAT &MOD  *TCAT &QTE)
044500060109             CHGVAR     VAR(&MSG)   VALUE(&QTE *TCAT &MSG  *TCAT &QTE)
044600060109             INSERT     INTO(QTEMP/RPTFILP) VALUES(&IRFS &IOBJ &ITYP +
044700060109                          &IFLIB &ITLIB &IMOD &MSG)
044800060109
044900060109/* End of Programs          */
045000060109 EXIT:       ENDPGM
