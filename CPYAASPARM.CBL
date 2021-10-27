000100210902      **************************************************************
000200210902      *    CPYAASPARM: COPYBOOK FOR MFAAAPARM  DATA AREA           *
000300210902      *    USE TO CONTROL ACCOUNT LEVEL ASSET ALLOCATION REBALANCE *
000400210902      *    SETUP AND PROCESS                                       *
000500210902      *                                                            *
000600210902      *    LENGTH    : 4                                           *
000700210902      **************************************************************
000800210902      *                                                            *
000900210902      *    C H A N G E   H I S T O R Y                             *
001000210902      *                                                            *
001100210902      **************************************************************
001200210902      * PROGRAMMER *DATE OF CHANGE* DESCRIPTION OF CHANGE          *
001300210902      **************************************************************
001400210902      * VIGNESH    * 2021/09/02   * RFS 187304 NEW COPYBOOK        *
001500210902      **************************************************************
001600210902
001700210928       01 LC-MFAAAPARM-DTAARA.
001800210902
001900210902      * Current process information
002000210927          05 LC-REBAL-DEFAULT      PIC X(1).
002100210927          05 LC-AUTOACC-DEL-REBAL  PIC X(1).
002200210927          05 LC-NON-BUSDAY         PIC X(1).
002300210927          05 LC-REBAL-START-DATE   PIC X(1).
002400210902
