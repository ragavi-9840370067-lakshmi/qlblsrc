000100020125   /*****************************************************************/
000200020125   /* TITLE        : CHECK MODULE ID FOR DOLLAR DSC                 */
000300020125   /*                                                               */
000400020125   /* SYSTEM       : JSI                                            */
000500020125   /*                                                               */
000600020125   /* PROGRAM TYPE : CL                                             */
000700020125   /*                                                               */
000800020125   /* PROGRAMMER   : UNITRAX                                        */
000900020125   /*                                                               */
001000020125   /* PROGRAM      : CHKDOLLDSC                                     */
001100020125   /*                                                               */
001200020125   /*****************************************************************/
001300020125   /*    C H A N G E   H I S T O R Y                                */
001400020125   /*****************************************************************/
001500020125   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001600020125   /*****************************************************************/
001700020125   /*R. Hollander* 2002/01/15   * RFS 8066 - Account Based DSC      */
001800020125   /*            *              * calculation PHASE 2               */
001801020527   /*Lev O.      * 2002/04/19   * RFS 13471- Add check authorization*/
001802020527   /*            *              * for MGMFEE                        */
001803020524   /*R. Hollander  2002/05/24   * Percent Free treated as dollars   */
001804020524   /*                           * Convert to new CBL fmt RFS13468   */
001900020125   /*****************************************************************/
002000020125
002100020115             PGM        PARM(&ACLVDSC &ACLVFRD &ACTPCTF &COMRCVY +
002200020524                          &RSTATPS &EXTTRNF &MGMFEE &FRDODSC)
002300010605
002401020425             DCL        VAR(&CMPCDP) TYPE(*CHAR) LEN(3)
002500010605
002600010605             DCL        VAR(&ACLVDSC) TYPE(*CHAR) LEN(1)
002700010605             DCL        VAR(&ACLVFRD) TYPE(*CHAR) LEN(1)
002800010605             DCL        VAR(&ACTPCTF) TYPE(*CHAR) LEN(1)
002900020115             DCL        VAR(&COMRCVY) TYPE(*CHAR) LEN(1)
003000020115             DCL        VAR(&RSTATPS) TYPE(*CHAR) LEN(1)
003100020115             DCL        VAR(&EXTTRNF) TYPE(*CHAR) LEN(1)
003101020524             DCL        VAR(&MGMFEE) TYPE(*CHAR) LEN(1)
003102020524             DCL        VAR(&FRDODSC) TYPE(*CHAR) LEN(1)
003103020425
003104020425             DCL        VAR(&MODE) TYPE(*CHAR)
003105020425             DCL        VAR(&P1) TYPE(*CHAR) LEN(10) VALUE(' ')
003106020425             DCL        VAR(&P2) TYPE(*CHAR) LEN(10) VALUE(' ')
003107020425
003108020425             DCL        VAR(&P3) TYPE(*CHAR) LEN(1000) VALUE(' ')
003109020425             DCL        VAR(&P4) TYPE(*DEC) LEN(9) VALUE(0)
003110020425             DCL        VAR(&P5) TYPE(*CHAR) LEN(1) VALUE(' ')
003111020425
003112020425             DCL        VAR(&P6) TYPE(*CHAR) LEN(1000) VALUE(' ')
003113020425             DCL        VAR(&P7) TYPE(*DEC) LEN(9) VALUE(0)
003114020425             DCL        VAR(&P8) TYPE(*CHAR) LEN(1) VALUE(' ')
003115020425
003116020425             DCL        VAR(&P9) TYPE(*CHAR) LEN(1000) VALUE(' ')
003117020425             DCL        VAR(&P10) TYPE(*DEC) LEN(9) VALUE(0)
003118020425             DCL        VAR(&P11) TYPE(*CHAR) LEN(1) VALUE(' ')
003119020425
003120020425             DCL        VAR(&P12) TYPE(*CHAR) LEN(1000) VALUE(' ')
003121020425             DCL        VAR(&P13) TYPE(*DEC) LEN(9) VALUE(0)
003122020425             DCL        VAR(&P14) TYPE(*CHAR) LEN(1) VALUE(' ')
003123020425
003124020425             DCL        VAR(&P15) TYPE(*CHAR) LEN(1000) VALUE(' ')
003125020425             DCL        VAR(&P16) TYPE(*DEC) LEN(9) VALUE(0)
003126020425             DCL        VAR(&P17) TYPE(*CHAR) LEN(1) VALUE(' ')
003127020425
003128020425             DCL        VAR(&P18) TYPE(*CHAR) LEN(1000) VALUE(' ')
003129020425             DCL        VAR(&P19) TYPE(*DEC) LEN(9) VALUE(0)
003130020425             DCL        VAR(&P20) TYPE(*CHAR) LEN(1) VALUE(' ')
003131020425
003132020425             DCL        VAR(&P21) TYPE(*CHAR) LEN(1) VALUE(' ')
003133020425
003134020425             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCDP)
003200010605
003201020425             CHGVAR     VAR(&P1) VALUE('MFASPMODA')
003202020425             CHGVAR     VAR(&P2) VALUE('*LIBL')
003203020425             CHGVAR     VAR(&MODE) VALUE('R')
003204020425             CHGVAR     VAR(&P4) VALUE(10)
003205020425             CHGVAR     VAR(&P5) VALUE('K')
003206020425             CHGVAR     VAR(&P7) VALUE(3)
003207020425             CHGVAR     VAR(&P8) VALUE('K')
003208020425
003209020425         /******************************************************************/
003210020425             CHGVAR     VAR(&P3)  VALUE('ACLVDSC')
003211020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
003212020425
003213020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
003214020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
003215020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
003900020425             CHGVAR VAR(&ACLVDSC) VALUE(&P21)
003901020425         /******************************************************************/
003902020425         /******************************************************************/
003903020425             CHGVAR     VAR(&P3)  VALUE('ACLVFRD')
003904020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
003905020425
003906020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
003907020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
003908020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
003909020425             CHGVAR VAR(&ACLVFRD) VALUE(&P21)
003910020425         /******************************************************************/
004000010605
004001020425         /******************************************************************/
004002020425             CHGVAR     VAR(&P3)  VALUE('ACTPCTF')
004003020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
004004020425
004005020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
004006020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
004007020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
004008020425             CHGVAR VAR(&ACTPCTF) VALUE(&P21)
004009020425         /******************************************************************/
005400020115
005401020425         /******************************************************************/
005402020425             CHGVAR     VAR(&P3)  VALUE('COMRCVY')
005403020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
005404020425
005405020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
005406020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
005407020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
005408020425             CHGVAR VAR(&COMRCVY) VALUE(&P21)
005409020425         /******************************************************************/
006100020115
006101020425         /******************************************************************/
006102020425             CHGVAR     VAR(&P3)  VALUE('RSTATPS')
006103020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
006104020425
006105020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
006106020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
006107020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
006108020425             CHGVAR VAR(&RSTATPS) VALUE(&P21)
006109020425         /******************************************************************/
006800020115
006801020425         /******************************************************************/
006802020425             CHGVAR     VAR(&P3)  VALUE('EXTTRNF')
006803020425             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
006804020425
006805020425             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
006806020425                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
006807020425                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
006808020425             CHGVAR VAR(&EXTTRNF) VALUE(&P21)
006809020425         /******************************************************************/
006810020524
006811020524         /******************************************************************/
006812020524             CHGVAR     VAR(&P3)  VALUE('MGMFEE')
006813020524             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
006814020524
006815020524             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
006816020524                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
006817020524                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
006818020524             CHGVAR VAR(&MGMFEE) VALUE(&P21)
006819020524         /******************************************************************/
006820020524
006821020524         /******************************************************************/
006822020524             CHGVAR     VAR(&P3)  VALUE('FRDODSC')
006823020524             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
006824020524
006825020524             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
006826020524                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
006827020524                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
006828020524             CHGVAR VAR(&FRDODSC) VALUE(&P21)
006829020524         /******************************************************************/
007700010605             RETURN
007800010605             ENDPGM
