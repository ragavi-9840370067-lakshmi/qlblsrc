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
001400931202             DCL        VAR(&TAPDEV) TYPE(*CHAR) LEN(10) VALUE('TAP02')
001500931201             DCL        VAR(&SEQNO) TYPE(*DEC) LEN(2) VALUE(00)
001600931202             DCL        VAR(&BPI) TYPE(*CHAR) LEN(4) VALUE('6250')
001700931202             DCL        VAR(&MFAPRCDTP) TYPE(*CHAR) LEN(8)
001800941025             DCL        VAR(&MFATAPE) TYPE(*CHAR) LEN(10)
001900941025             RTVDTAARA  DTAARA(MFATAPE *ALL) RTNVAR(&MFATAPE)
002000931201             RTVDTAARA  DTAARA(MFAPRCDTP (1 8)) RTNVAR(&MFAPRCDTP)
002100931201
002200931201             RTVJOBA    SBMMSGQ(&WKSTN)
002300931201             CHGVAR     VAR(&DLRNUM) VALUE(%SST(&FILENAME 3 4))
002400931201             CHGVAR     VAR(&PDATE) VALUE(%SST(&MFAPRCDTP 3 6))
002500931201             CALL       PGM(BCMBKA) PARM(&RUNTYP &TAPE &PDATE &RUN1)
002600931201 MOUNTMSG:   CHGVAR     VAR(&PGMMSG) VALUE('Please mount tape ' *CAT +
002700931201                          &TAPE *CAT ' write ring enabled, on ' +
002800931201                          *BCAT &TAPDEV *BCAT '. Enter a ''G'' when +
002900931201                          the tape is mounted or a ''C'' if the the +
003000931201                          tape is not found')
003100931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INQ) +
003200931201                          KEYVAR(&KEY)
003300931201             RCVMSG     MSGQ(*PGMQ) MSGTYPE(*RPY) MSGKEY(&KEY) +
003400931201                          WAIT(*MAX) RMV(*NO) MSG(&REPLY)
003500931201             IF         COND(&REPLY *EQ 'c' *OR &REPLY *EQ 'C') +
003600931201                          THEN(DO)
003700931201             SNDPGMMSG  MSG('The tape requested ' *CAT &TAPE *CAT ' +
003800931201                          was not found') TOMSGQ(&WKSTN) MSGTYPE(*INFO)
003900931201             RETURN
004000931201             ENDDO
004100931201             IF         COND(&REPLY *NE 'G' *AND &REPLY *NE 'g') +
004200931201                          THEN(GOTO CMDLBL(MOUNTMSG))
004300931201 CHKTAP:     CHKTAP     DEV(&TAPDEV) VOL(&TAPE) ENDOPT(*REWIND)
004400931201             MONMSG     MSGID(CPF6720) EXEC(GOTO CMDLBL(WRGTAP))
004500931201             GOTO       CMDLBL(RIGHTTAPE)
004600931201 WRGTAP:     CHGVAR     VAR(&PGMMSG) VALUE('The requested tape ' +
004700931201                          *CAT &TAPE *CAT ' is not the one that is +
004800931201                          on the tape drive. Please mount the +
004900931201                          correct tape. Enter a ''G'' when the tape +
005000931201                          is mounted or a ''C'' if the the tape is +
005100931201                          not found')
005200931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INQ) +
005300931201                          KEYVAR(&KEY)
005400931201             RCVMSG     MSGQ(*PGMQ) MSGTYPE(*RPY) MSGKEY(&KEY) +
005500931201                          WAIT(*MAX) RMV(*NO) MSG(&REPLY)
005600931201             IF         COND(&REPLY *EQ 'c' *OR &REPLY *EQ 'C') +
005700931201                          THEN(DO)
005800931201             SNDPGMMSG  MSG('The tape requested ' *CAT &TAPE *CAT ' +
005900931201                          was not found') TOMSGQ(&WKSTN) MSGTYPE(*INFO)
006000931201             RETURN
006100931201             ENDDO
006200931201             GOTO       CMDLBL(CHKTAP)
006300931201 RIGHTTAPE:
006400940718             INZTAP     DEV(&TAPDEV) NEWVOL(&TAPE) CHECK(*NO) +
006500940718                          DENSITY(*DEVTYPE)
006600931201             MONMSG     MSGID(CPF6768) EXEC(GOTO CMDLBL(WRTRING))
006700931201             GOTO       CMDLBL(SAVFILE)
006800931201 WRTRING:    DLCOBJ     OBJ((&TAPDEV *DEVD *EXCLRD))
006900931201 WRMSG:      CHGVAR     VAR(&PGMMSG) VALUE('Please place a write +
007000931201                          ring on ' *CAT &TAPE *CAT '. This tape is +
007100931201                          currently on the drive. Thank you. Enter +
007200931201                          a ''G'' when the tape is write ring enabled')
007300931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INQ) +
007400931201                          KEYVAR(&KEY)
007500931201             RCVMSG     MSGQ(*PGMQ) MSGTYPE(*RPY) MSGKEY(&KEY) +
007600931201                          WAIT(*MAX) RMV(*NO) MSG(&REPLY)
007700931201             IF         COND(&REPLY *NE 'G' *AND &REPLY *NE 'g') +
007800931201                          THEN(GOTO CMDLBL(WRMSG))
007900931201             GOTO       CMDLBL(RIGHTTAPE)
008000931201 SAVFILE:    CHGVAR     VAR(&SEQNO) VALUE(&SEQNO + 1)
008100941025             CPYTOTAP   FROMFILE(&MFATAPE/&FILENAME) TOFILE(QTAPE) +
008200941025                          TOSEQNBR(01) TODEV(&TAPDEV) TOREELS(*SL) +
008300941025                          TORCDLEN(910) TOENDOPT(*REWIND) +
008400941025                          TOVOL(&TAPE) TOBLKLEN(31850) TORCDBLK(*FB)
008500931201             DLCOBJ     OBJ((&TAPDEV *DEVD *EXCLRD))
008600931201             CHGVAR     VAR(&PGMMSG) VALUE('Please remove tape on ' +
008700931201                          *CAT &TAPDEV *CAT '. ' *CAT 'Label tape +
008800931201                          as to: ' *CAT &DLRNUM *BCAT &DLRNAM *CAT +
008900931201                          'BPI: ' *CAT &BPI *BCAT 'Block Length: +
009000931209                          31850 Rec Length: 910.  Please prepare +
009100931201                          this tape for courier.')
009200931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
009300931201             CHGVAR     VAR(&PGMMSG) VALUE('Month-End Reconciliation +
009400931201                          Tape creation has completed successfully +
009500931201                          for Dealer Number ** ' *BCAT &DLRNUM +
009600931201                          *BCAT ' ** The TAPE NUMBER is ** ' *BCAT +
009700931201                          &TAPE *BCAT ' **  . Please write it down +
009800931201                          for future reference.')
009900931201             SNDPGMMSG  MSG(&PGMMSG) TOMSGQ(&WKSTN) MSGTYPE(*INFO)
010000931201             MONMSG     MSGID(CPF0000) EXEC(GOTO CMDLBL(ENDCLPGM))
010100931201 ENDCLPGM:   RETURN
010200931201             ENDPGM
