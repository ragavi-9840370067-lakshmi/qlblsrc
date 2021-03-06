000100931201             PGM        PARM(&FILENAME)
000200931201             DCL        VAR(&KEY) TYPE(*CHAR) LEN(4)
000300931201             DCL        VAR(&WKSTN) TYPE(*CHAR) LEN(10)
000400931201             DCL        VAR(&RUNTYP) TYPE(*CHAR) LEN(1) VALUE('G')
000500931201             DCL        VAR(&RUN1) TYPE(*CHAR) LEN(1) VALUE('1')
000600931201             DCL        VAR(&RUN2) TYPE(*CHAR) LEN(1) VALUE('2')
000700931201             DCL        VAR(&TAPE) TYPE(*CHAR) LEN(6)
000800931201             DCL        VAR(&PDATE) TYPE(*CHAR) LEN(6)
000900931201             DCL        VAR(&REPLY) TYPE(*CHAR) LEN(1)
001000931201             DCL        VAR(&PGMMSG) TYPE(*CHAR) LEN(300)
001100931201             DCL        VAR(&DLRNAM) TYPE(*CHAR) LEN(21)
001200931201             DCL        VAR(&DLRNUM) TYPE(*CHAR) LEN(5)
001300931201             DCL        VAR(&FILENAME) TYPE(*CHAR) LEN(10)
001400940914             DCL        VAR(&TAPDEV) TYPE(*CHAR) LEN(10) VALUE('TAP01')
001500931201             DCL        VAR(&SEQNO) TYPE(*DEC) LEN(2) VALUE(00)
001600931202             DCL        VAR(&BPI) TYPE(*CHAR) LEN(4) VALUE('6250')
001700940914             DCL        VAR(&MFAPRCDTP) TYPE(*CHAR) LEN(169)
001800940914             DCL        VAR(&MFACMPCDP) TYPE(*CHAR) LEN(43)
001900940914             DCL        VAR(&ENV) TYPE(*CHAR) LEN(3)
002000940914             DCL        VAR(&ENVNME) TYPE(*CHAR) LEN(40)
002100940914             DCL        VAR(&TXT) TYPE(*CHAR) LEN(50)
002200941025             DCL        VAR(&MFATAPE) TYPE(*CHAR) LEN(10)
002300950126             DCL        VAR(&LABEL) TYPE(*CHAR) LEN(12)
002400950126             DCL        VAR(&VOLSER) TYPE(*CHAR) LEN(6)
002500941025             RTVDTAARA  DTAARA(MFATAPE *ALL) RTNVAR(&MFATAPE)
002600940914             CHGJOB     LOG(4 00 *SECLVL) LOGCLPGM(*YES)
002700940914             RTVDTAARA  DTAARA(MFAPRCDTP) RTNVAR(&MFAPRCDTP)
002800940914             RTVDTAARA  DTAARA(MFACMPCDP) RTNVAR(&MFACMPCDP)
002900950126             RTVJOBA    SBMMSGQ(&WKSTN)
003000931201             CHGVAR     VAR(&DLRNUM) VALUE(%SST(&FILENAME 3 4))
003100940914             CHGVAR     VAR(&PDATE) VALUE(%SST(&MFAPRCDTP 72 6))
003200940914             CHGVAR     VAR(&ENV) VALUE(%SST(&MFACMPCDP 1 3))
003300940914             CHGVAR     VAR(&ENVNME) VALUE(%SST(&MFACMPCDP 4 40))
003400940914             CHGVAR     VAR(&TXT) VALUE('ME-Rec' *BCAT &ENV *BCAT +
003500940914                          &DLRNUM *BCAT &PDATE)
003600950412             CHGVAR     VAR(&LABEL) VALUE(%SST(&FILENAME 1 6) *CAT +
003700950126                          '.D' *CAT %SST(&FILENAME 7 4))
003800950126             CHGVAR     VAR(&VOLSER) VALUE(%SST(&FILENAME 1 6))
003900950126             RNMOBJ     OBJ(&MFATAPE/&FILENAME) OBJTYPE(*FILE) +
004000950126                          NEWOBJ(&VOLSER)
004100950209 CPYFILE:    CPYTOTAP   FROMFILE(&MFATAPE/&VOLSER) TOFILE(QTAPE) +
004200950209                          TOSEQNBR(01) TOLABEL(&LABEL) +
004300950209                          TODEV(&TAPDEV) TOREELS(*SL) TORCDLEN(320) +
004400950209                          TOENDOPT(*REWIND) TOBLKLEN(6400) +
004500950209                          TORCDBLK(*FB)
004600950228             OVRPRTF    FILE(QPTAPDSP) HOLD(*YES) SAVE(*YES)
004700950228             DSPTAP     DEV(TAP01) OUTPUT(*PRINT) ENDOPT(*UNLOAD)
004800950126             RNMOBJ     OBJ(&MFATAPE/&VOLSER) OBJTYPE(*FILE) +
004900950126                          NEWOBJ(&FILENAME)
005000940914             CHGVAR     VAR(&PGMMSG) VALUE('Please remove tape on ' +
005100940914                          *CAT &TAPDEV *TCAT '. ' *CAT 'LABEL tape +
005200940914                          as to: ' *BCAT &DLRNUM *BCAT 'for +
005300940914                          month-end' *BCAT &PDATE *BCAT 'for' *BCAT +
005400940914                          &ENV *BCAT &ENVNME *TCAT '. Please place +
005500940914                          tape in ATLAS/FINSCO bin.')
005600931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
005700940914             CHGVAR     VAR(&PGMMSG) VALUE('Month-End Reconciliation +
005800940914                          Tape creation has completed successfully +
005900940914                          for Dealer Number' *BCAT &DLRNUM *CAT '. +
006000940914                          Proceed to next tape if required.')
006100950126             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
006200931201             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDCLPGM))
006300931201 ENDCLPGM:   RETURN
006400931201             ENDPGM
