000100990428   /*****************************************************************/
000200021205   /* TITLE        : Create extract pgm's for client extract files  */
000300990428   /*                                                               */
000400030911   /* DATE         : 2003/09/11                                     */
000500990428   /*                                                               */
000600990428   /* PROGRAM TYPE : CLP                                            */
000700990428   /*                                                               */
000800030911   /* PROGRAMMER   : Bharti Mehta                                   */
000900990428   /*                                                               */
001000030911   /* PROGRAM      : CRTAAAPGM                                      */
001100990428   /*                                                               */
001200990428   /*****************************************************************/
001300990428   /*    C H A N G E   H I S T O R Y                                */
001400990428   /*****************************************************************/
001500990428   /* PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE             */
001600990428   /*****************************************************************/
001700990428   /*            *              *                                   */
001800990428   /*****************************************************************/
001900990616             PGM        PARM(&OJLIB)
002000990428
002100990616             DCL        VAR(&OJLIB) TYPE(*CHAR) LEN(10)
002200030827             DCL        VAR(&AASRVPGM) TYPE(*CHAR) LEN(10) +
002300030827                          VALUE('AASRVPGM')
002400990616
002500030827   /* ============================================================= */
002600030827   /* Start Process                                                 */
002700030827             DLTSRVPGM  SRVPGM(&OJLIB/&AASRVPGM)
002800030827             MONMSG     MSGID(CPF2105)
002900030827             CRTSRVPGM  SRVPGM(&OJLIB/&AASRVPGM) +
003000030827                        MODULE(FXAAAINV  +
003100030827                               CHKINVSTR +
003200030827                               GETDAT    +
003300030827                                       ) +
003400030827                        EXPORT(*ALL)
003500021205
003600990428
003700030828             CRTPGM     PGM(&OJLIB/AASWTTXN1) +
003800030911                          MODULE(&OJLIB/FXAAAINV +
003900030827                          &OJLIB/CHKINVSTR &OJLIB/GETDAT +
004000030911                          &OJLIB/FXAAHTRN) ENTMOD(*PGM)
004100030827
004200030827             CHGOBJOWN  OBJ(&OJLIB/AASWTTXN1) OBJTYPE(*PGM) +
004300030403                          NEWOWN(MFAMASTER)
004400030403
004500021205
004600030827             CRTPGM     PGM(&OJLIB/FXAAAINV) MODULE(&OJLIB/FXAAAINV) +
004700030827                          TEXT('Check/Update Active AA Account-Inv')
004800021205
004900030827             CHGOBJOWN  OBJ(&OJLIB/FXAAAINV) OBJTYPE(*PGM) +
005000030827                          NEWOWN(MFAMASTER)
005100030403
005200021205
005300030827             CRTPGM     PGM(&OJLIB/CHKINVSTR) MODULE(&OJLIB/CHKINVSTR) +
005400030827                          TEXT('Check Investment Structure ')
005500021205
005600030827             CHGOBJOWN  OBJ(&OJLIB/CHKINVSTR) OBJTYPE(*PGM) +
005700030403                          NEWOWN(MFAMASTER)
005800030403
005900030403
006000030403
006100990428             ENDPGM
