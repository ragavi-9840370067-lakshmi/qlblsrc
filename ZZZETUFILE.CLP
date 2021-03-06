000100931108             PGM        PARM(&FILENAME &LIBNAME)
000200931108
000300931108             DCL        VAR(&FILENAME) TYPE(*CHAR) LEN(10)
000400931108             DCL        VAR(&LIBNAME) TYPE(*CHAR) LEN(10)
000500931108             DCL        VAR(&FRM3X01) TYPE(*CHAR) LEN(29) +
000600931108                          VALUE('Kenny Tai:Files:Dealer Disks:')
000700931108             DCL        VAR(&FRM3X02) TYPE(*CHAR) LEN(1) VALUE(':')
000800931126             DCL        VAR(&FRM3X03) TYPE(*CHAR) LEN(8)
000900931124             DCL        VAR(&FRM3X04) TYPE(*CHAR) LEN(3) /* File +
001000931119                          Extension Code */
001100931108             DCL        VAR(&FRM3XVAR) TYPE(*CHAR) LEN(80)
001200931119             DCL        VAR(&FRM3XDEV) TYPE(*CHAR) LEN(10) +
001300931119                          VALUE('MACMIS004') /* Device Where Files +
001400931119                          been Download */
001500931108             DCL        VAR(&MSG)       TYPE(*CHAR) LEN(300)
001600931108
001700931108             ADDLIBLE   LIB(ETU400)
001800931117             MONMSG     MSGID(CPF2103) EXEC(GOTO CMDLBL(XLATEFILE)) +
001900931117                          /* Library ETU400 Already Exist */
002000931108
002100931108 XLATEFILE:  XLATETO    FRMFILE(&LIBNAME/&FILENAME) FRMMBR(*FIRST) +
002200931108                          PCDATA(QTEMP/&FILENAME)
002300931108             MONMSG     MSGID(CPF0000) EXEC(DO)
002400931108             CHGVAR     VAR(&MSG) VALUE('*** TRANSLATION ERROR *** +
002500931108                          File has has not been TRANSLATED file +
002600931108                          Name ' *CAT &FILENAME *CAT '.')
002700931108             SNDPGMMSG  MSG(&MSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
002800931108             RETURN
002900931108             ENDDO
003000931108
003100931119 DOWNLDFILE: CHGVAR     VAR(&FRM3X03) VALUE(%SST(&FILENAME 3 8))
003200931124             CHGVAR     VAR(&FRM3X04) VALUE('.' *CAT %SST(&FILENAME +
003300931124                          1 2))
003400931119             CHGVAR     VAR(&FRM3XVAR) VALUE(&FRM3X01 *CAT +
003500931119                          (%SST(&FILENAME 3 4)) *CAT &FRM3X02 *CAT +
003600931124                          &FRM3X03 *CAT &FRM3X04)
003700931108             PWRFROM3XB FRMFILE(QTEMP/&FILENAME *FIRST) +
003800931108                          QFNAME1(&FRM3XVAR) MICLST(&FRM3XDEV) +
003900931108                          TRANSLATE(*NONE)
004000931108             MONMSG     MSGID(CPF0000) EXEC(DO)
004100931119             CHGVAR     VAR(&MSG) VALUE('*** DOWNLOAD ERROR *** File +
004200931119                          has has not been DOWNLOADED - ' *CAT +
004300931119                          &FILENAME *CAT ' .')
004400931108             SNDPGMMSG  MSG(&MSG) TOMSGQ(QSYSOPR) MSGTYPE(*INFO)
004500931108             RETURN
004600931108             ENDDO
004700931108
004800931108             ENDPGM
