000100120829/****************************************************************************/
000200120829/* USE THIS CL TO CHECK THE EXISTENCE OF AN OBJECT FROM A                   */
000300120829/* SCREEN, ETC.                                                             */
000400120829/****************************************************************************/
000401120829/* Brad Doyle * 2012/08/29 * RFS113524 - Avoid MSGW if invalid filename     */
000403120829/****************************************************************************/
000500990528             PGM        PARM(&OBJECT &TYPE)
000600990528             DCL        VAR(&OBJECT)  TYPE(*CHAR) LEN(10)
000700990528             DCL        VAR(&TYPE)    TYPE(*CHAR) LEN(10)
000800990528
000801120829        /* RFS113524 - Begin */
000900120829 /* INIT:            CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('        ')   */
000901120829 INIT:       CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('NOTFOUND')
000902120829        /* RFS113524 - End */
001000990528
001100990528             CHKOBJ     OBJ(&OBJECT) OBJTYPE(&TYPE)
001101120829        /* RFS113524 - Begin */
001200120829         /*    MONMSG     MSGID(CPF9801) EXEC(GOTO CMDLBL(NOTFOUND))    */
001201120829             MONMSG     MSGID(CPF0000) EXEC(Return)
001202120829        /* RFS113524 - End */
001300990528
001400990528 FOUND:      CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('        ')
001500990528             GOTO       CMDLBL(END)
001600990528
001700990528 NOTFOUND:   CHGDTAARA  DTAARA(*LDA (1 10)) VALUE('NOTFOUND')
001800990528
001900990528 END:
002000970527             RETURN
002100970428             ENDPGM
