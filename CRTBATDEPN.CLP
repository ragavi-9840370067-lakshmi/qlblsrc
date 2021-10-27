000100111212    /*---------------------------------------------------------------*/
000200111129    /*****************************************************************/
000300111129    /*****************************************************************/
000400120301    /* TITLE        : CASE MANAGER INTERFACE.                        */
000500111129    /* SYSTEM       : MUTUAL FUND                                    */
000600111129    /* PROGRAM TYPE : CLP                                            */
000700120225    /* PROGRAMMER   : DIVINA ZACHARIA                                */
000800120221    /* PROGRAM      : CRTBATDEPN                                     */
000900120402    /* DESCRIPTION  : TO SEND EMAIL NOTIFICATION TO ESB              */
001000120221    /*                                                               */
001100120221    /* DATE CREATED : 2012/02/21                                     */
001200120221    /* RFS NUMBER   : RFS71716                                       */
001300120301    /* CALLED BY    : CRTBATDEP                                      */
001400120402    /* PARAMETERS   : emailMSG                                       */
001500120305    /* CALLS PROGRAM: NONE                                           */
001600120221    /*                                                               */
001700111207    /*                                                               */
001800111129    /*****************************************************************/
001900111129    /*    C H A N G E   H I S T O R Y                                */
002000111129    /*****************************************************************/
002100111129    /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
002200111129    /*            * YYYY/MM/DD   * RFS ##### -                       */
002300120312    /* DIVINA Z.  * 2012/02/21   * RFS 71716 - CREATED PROGRAM       */
002400111129    /*****************************************************************/
002500120402             PGM        PARM(&emailMSG)
002600120402             DCL        VAR(&email1)     TYPE(*CHAR) LEN(45)
002700120402             DCL        VAR(&email2)     TYPE(*CHAR) LEN(45)
002800120402             DCL        VAR(&emailADDR)  TYPE(*CHAR) LEN(45)
002900120402             DCL        VAR(&emailDEV)   TYPE(*CHAR) LEN(45)
003000120402             DCL        VAR(&emailTST)   TYPE(*CHAR) LEN(45)
003100120402             DCL        VAR(&emailMSG)   TYPE(*CHAR) LEN(300)
003200120402             DCL        VAR(&emailSUBJ)  TYPE(*CHAR) LEN(80)      +
003300120306                                         VALUE('NOTIFICATION ')
003400120225             DCL        VAR(&ENV)        TYPE(*CHAR) LEN(1)
003500120225             DCL        VAR(&CMPENV)     TYPE(*CHAR) LEN(7)
003600120305             DCL        VAR(&CMPCODE)    TYPE(*CHAR) LEN(3)
003700120402             DCL        VAR(&SYSNAME)    TYPE(*CHAR) LEN(10)
003800111129
003900120402             CHGVAR     VAR(&emailDEV)                           +
004000120402                           VALUE('Divina.Zacharia@lntinfotech.com')
004100120402             CHGVAR     VAR(&email2)                              +
004200120402                           VALUE('emmanuel.yala@lntinfotech.com')
004300120402             CHGVAR     VAR(&email1)                              +
004400120402                           VALUE('ajay.dhandapani@lntinfotech.com')
004500120402             CHGVAR     VAR(&emailTST)                              +
004600120402                           VALUE('abhijit.bhatta@lntinfotech.com')
004700120221
004800120402     /* Create email subject and message with company code and env */
004900120224
005000120305             RTVDTAARA  DTAARA(MFACMPCDP (1 3)) RTNVAR(&CMPCODE)
005100120305             RTVDTAARA  DTAARA(MFADBASE (4 1))  RTNVAR(&ENV)
005200120402             RTVNETA    SYSNAME(&SYSNAME)
005300120402
005400120402             CHGVAR VAR(&emailADDR) VALUE('confirmation@citi.com')
005500120221             IF  COND(&ENV = 'P') THEN(DO)
005600120307                CHGVAR  VAR(&CMPENV)    VALUE(&CMPCODE *CAT '-'   +
005700120307                                        *CAT 'PRD')
005800120221             ENDDO
005900120221             ELSE CMD(DO)
006000120307                CHGVAR  VAR(&CMPENV)    VALUE(&CMPCODE *CAT '-'   +
006100120307                                        *CAT 'ACC')
006200120221             ENDDO
006300120221
006400120402             CHGVAR     VAR(&emailSUBJ) VALUE(&emailSUBJ *BCAT &CMPENV)
006500120225
006600120402             CHGVAR     VAR(&emailMSG)  VALUE('Hello ' *BCAT      +
006700120402                                        &CMPENV *CAT ',' *BCAT &emailMSG)
006800120225
006900120225
007000120402     /*      SEND email NOTIFICATION                                 */
007100120221
007200120307             ADDLIBLE   LIB(ESEND)      POSITION(*LAST)
007300120221             MONMSG     MSGID(CPF2103)
007400120402             IF COND(&SYSNAME *EQ 'DEVSQA') THEN(DO)
007500120402               CHGVAR VAR(&emailADDR) VALUE(&emailDEV)
007600120402               ESNDMAIL RECIPIENT(&email2) SUBJECT(&emailSUBJ) +
007700120402                        MSG(&emailMSG)
007800120402               ESNDMAIL RECIPIENT(&email1) SUBJECT(&emailSUBJ) +
007900120402                        MSG(&emailMSG)
008000120402             ENDDO
008100120402
008200120402             IF COND(&SYSNAME *EQ 'TESTING') THEN(DO)
008300120402                CHGVAR VAR(&emailADDR) VALUE(&emailTST)
008400120402             ENDDO
008500120402
008600120402             ESNDMAIL   RECIPIENT(&emailADDR) SUBJECT(&emailSUBJ) +
008700120402                        MSG(&emailMSG)
008800120225
008900111129             ENDPGM
