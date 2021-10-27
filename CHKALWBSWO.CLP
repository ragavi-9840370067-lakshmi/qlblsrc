000001020524   /*****************************************************************/
000002020524   /* TITLE        : CHECK MODULE ID FOR ALLOW SWO FEE              */
000003020524   /*                                                               */
000004020524   /* SYSTEM       : JSI                                            */
000005020524   /*                                                               */
000006020524   /* PROGRAM TYPE : CL                                             */
000007020524   /*                                                               */
000008020524   /* PROGRAMMER   : UNITRAX                                        */
000009020524   /*                                                               */
000010020524   /* PROGRAM      : CHKALWBSWO                                     */
000011020524   /*                                                               */
000012020524   /*                                                               */
000013020524   /*****************************************************************/
000014020524   /*                                                               */
000015020524   /*    C H A N G E   H I S T O R Y                                */
000016020524   /*                                                               */
000017020524   /*****************************************************************/
000018020524   /*****************************************************************/
000019020524   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
000020020524   /*****************************************************************/
000021020524   /*R. Hollander  2002/05/24   * Percent Free treated as dollars   */
000022020524   /*                           * Convert to new CBL fmt RFS13468   */
000023020524
000100020524             PGM        PARM(&ALWBSWO)
000101020429             DCL        VAR(&CMPCDP) TYPE(*CHAR) LEN(3)
000500010808             DCL        VAR(&ALWBSWO) TYPE(*CHAR) LEN(1)
000501020429
000502020429             DCL        VAR(&MODE) TYPE(*CHAR)
000503020429             DCL        VAR(&P1) TYPE(*CHAR) LEN(10) VALUE(' ')
000504020429             DCL        VAR(&P2) TYPE(*CHAR) LEN(10) VALUE(' ')
000505020429             DCL        VAR(&P3) TYPE(*CHAR) LEN(1000) VALUE(' ')
000506020429             DCL        VAR(&P4) TYPE(*DEC) LEN(9) VALUE(0)
000507020429             DCL        VAR(&P5) TYPE(*CHAR) LEN(1) VALUE(' ')
000508020429
000509020429             DCL        VAR(&P6) TYPE(*CHAR) LEN(1000) VALUE(' ')
000510020429             DCL        VAR(&P7) TYPE(*DEC) LEN(9) VALUE(0)
000511020429             DCL        VAR(&P8) TYPE(*CHAR) LEN(1) VALUE(' ')
000512020429
000513020429             DCL        VAR(&P9) TYPE(*CHAR) LEN(1000) VALUE(' ')
000514020429             DCL        VAR(&P10) TYPE(*DEC) LEN(9) VALUE(0)
000515020429             DCL        VAR(&P11) TYPE(*CHAR) LEN(1) VALUE(' ')
000516020429
000517020429             DCL        VAR(&P12) TYPE(*CHAR) LEN(1000) VALUE(' ')
000518020429             DCL        VAR(&P13) TYPE(*DEC) LEN(9) VALUE(0)
000519020429             DCL        VAR(&P14) TYPE(*CHAR) LEN(1) VALUE(' ')
000520020429
000521020429             DCL        VAR(&P15) TYPE(*CHAR) LEN(1000) VALUE(' ')
000522020429             DCL        VAR(&P16) TYPE(*DEC) LEN(9) VALUE(0)
000523020429             DCL        VAR(&P17) TYPE(*CHAR) LEN(1) VALUE(' ')
000524020429
000525020429             DCL        VAR(&P18) TYPE(*CHAR) LEN(1000) VALUE(' ')
000526020429             DCL        VAR(&P19) TYPE(*DEC) LEN(9) VALUE(0)
000527020429             DCL        VAR(&P20) TYPE(*CHAR) LEN(1) VALUE(' ')
000528020429
000529020429             DCL        VAR(&P21) TYPE(*CHAR) LEN(1) VALUE(' ')
000530020429
000531020429             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCDP)
000532020429
000533020429             CHGVAR     VAR(&P1) VALUE('MFASPMODA')
000534020429             CHGVAR     VAR(&P2) VALUE('*LIBL')
000535020429             CHGVAR     VAR(&MODE) VALUE('R')
000536020429             CHGVAR     VAR(&P4) VALUE(10)
000537020429             CHGVAR     VAR(&P5) VALUE('K')
000538020429             CHGVAR     VAR(&P7) VALUE(3)
000539020429             CHGVAR     VAR(&P8) VALUE('K')
000540020429
000541020429         /******************************************************************/
000542020429             CHGVAR     VAR(&P3)  VALUE('ALWBSWO')
000543020429             CHGVAR     VAR(&P6)  VALUE(&CMPCDP)
000544020429
000545020429             CALL       PGM(UIDXCTL) PARM(&P1 &P2 &MODE &P3 &P4 &P5 +
000546020429                          &P6 &P7 &P8 &P9 &P10 &P11 &P12 &P13 &P14 +
000547020429                          &P15 &P16 &P17 &P18 &P19 &P20 &P21)
000548020429             CHGVAR VAR(&ALWBSWO) VALUE(&P21)
000600010605
001600010605             RETURN
001700010605             ENDPGM
