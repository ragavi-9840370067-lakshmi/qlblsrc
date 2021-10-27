000001170313     /* %ATTR USRPRF(*OWNER) */
000002070215/*********************************************************************/
000003070215/* TITLE        : Check User Profile Limit Capabilities option       */
000005070215/* PROGRAM      : CHKUSRAUT                                          */
000007070215/*********************************************************************/
000008070215/*********************************************************************/
000009070215/*PROGRAMMER     *DATE OF CHANGE* DESCRIPTION OF CHANGE              */
000010070215/*********************************************************************/
000011070215/* R Miguel      * 2007/02/15   * RFS 36813- MEM authority changes   */
000012070215/*               *              * as it was missed in RFS25570       */
000013070215/*                                                                   */
000014070215/*********************************************************************/
000101070215             PGM        PARM(&USRPROF &ALLWOPT)
000201951016             DCL        VAR(&LMTCPB) TYPE(*CHAR) LEN(10)
000202070215             DCL        VAR(&USRPROF) TYPE(*CHAR) LEN(10)
000301951016             DCL        VAR(&ALLWOPT) TYPE(*CHAR) LEN(1)
000401070215             RTVUSRPRF  USRPRF(&USRPROF) LMTCPB(&LMTCPB)
000501951016             IF COND(&LMTCPB *EQ '*NO') THEN(DO)
000601951016               CHGVAR VAR(&ALLWOPT) VALUE('Y')
000701951016               GOTO CMDLBL(EXIT)
000801951016             ENDDO
000901951016             CHGVAR VAR(&ALLWOPT) VALUE('N')
001001951016EXIT:
001101951016             ENDPGM
