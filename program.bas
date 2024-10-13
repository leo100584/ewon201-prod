Rem --- eWON start section: Init Section
eWON_init_section:
	Rem --- eWON user (start)
	TSET 1, 60
	ONTIMER 1, "GOTO Mainstart"
	END
Mainstart:
  PRINT "Mainstart:"
  ///TSET 1, 0                                // Turn of Timer #1, to run once else, while(true)
  // Country call code
  callCode$ = "0047" 
  // SMSC service (standard=0, Com4=004794049005)
  smscnumber$ = "0"
  // First phone number group
  ONDATE 1,"00 03 * * *","GOTO ModemReboot"
  ONDATE 2,"*/30 * * * *","GOTO checkVpnConnection"
  // TSET 2,10
  //  ONTIMER 2,"GOTO SMS"
  TSET 3, 60
  ONTIMER 3, "GOSUB Startup"
  ONCHANGE "hist_log_interval_seconds_highpri",  "GOSUB Set_hist_timer_h"
  ONCHANGE "hist_log_interval_seconds_medpri",  "GOSUB Set_hist_timer_m"
  ONCHANGE "hist_log_interval_seconds_lowpri",  "GOSUB Set_hist_timer_l"
  // Finding number of tags
  NumberOfTags% = GETSYS PRG,"NBTAGS"
  PRINT "Number of taggs is " + STR$(NumberOfTags%)
  
  AlarmFileName$ = "file:/usr/AlarmFile.txt" 
  // Check IF files exists:
  ONERROR "GOTO TestFilErr"
  OPEN AlarmFileName$ FOR TEXT INPUT As 6
  ONERROR ""
  GOTO TestFileOk
  TestFilErr:
    PRINT "TestFilErr"
    CLOSE 6
    OPEN AlarmFileName$ FOR TEXT OUTPUT As 6
	
    TestFileOk:
		CLOSE 6
		
		GsmReady% = 0 
		FINS_Ready% = 0
		SmsStackQty% = 0
		MaxSmsStack% = 20
		DIM SmsStack$(MaxSmsStack% , 100)
		DIM SmsNumbers$(MaxSmsStack% , 40 )
		DIM SmsTag$(MaxSmsStack% , 40 )
		GOSUB Sett_Alle
	  // GOSUB Sett_Alle
  ONCHANGE "numberTag1",  "GOSUB Sett_1"
  ONCHANGE "numberTag2",  "GOSUB Sett_2"
  ONCHANGE "numberTag3",  "GOSUB Sett_3"
  ONCHANGE "numberTag4",  "GOSUB Sett_4"
  ONCHANGE "numberTag5",  "GOSUB Sett_5"
  ONCHANGE "numberTag6",  "GOSUB Sett_6"
  //  TSET 4, 4
  //  ONTIMER 4, "GOSUB TestGsmNetReg"
  //GOSUB TestGsmNetReg
  //  ONPPP "GOSUB TestGsmNetReg"
    
  TSET 2, 20
  ONTIMER 2, "GOSUB SetGsmNumber"
END
Startup:
  PRINT "Startup:"
  TSET 3, 0
  GOSUB Set_hist_timer_h
  GOSUB Set_hist_timer_m
  GOSUB Set_hist_timer_l
  PRINT "Startup COMPLETE"
  RETURN
END
SetGsmNumber:
  PRINT "SetGsmNumber:" 
  TSET 2, 0
  FINS_Ready% = 1
  GOSUB Sett_Alle
  TSET 2, 0
  GOSUB TestGsmNetReg
  ONPPP "GOSUB TestGsmNetReg"
  RETURN
END
TestGsmNetReg:
  PRINT "TestGsmNetReg:"
  // SETSYS INF,"load"
  // GsmNetReg$ = GETSYS INF,"GsmNetReg"                            
  //  IF GsmNetReg$ = "1" Or GsmNetReg$ = "5" THEN
  //      GsmReady% = 1
        
  //     TSET 4, 0 
  //      GOSUB EmtyLog
  //      PRINT "GSM Klar"
  // ELSE
  //   GsmReady% = 0
  //    PRINT "GSM er ikke Klar " + GsmNetReg$
  // ENDIF
  
  // PPPIP_STAT% = GETSYS PRG, "EVTINFO"
  // IF PPPIP_STAT% = 1 THEN 
  //  	PRINT "Online with address " ; GETSYS PRG, "PPPIP" 
  //	GsmReady% = 1
  //	GOSUB EmtyLog
  //  ELSE 
  //   PRINT "PPP Going offline (Stat : " + Str$(PPPIP_STAT%) + ")"
  //   GsmReady% = 0
  //  ENDIF 
  //  Return
  //  End
  PRINT "Sjekker status:"
  SETSYS INF,"load"
  PPPIP$ = GETSYS INF,"PPPIP"
  PRINT "PPPIP," + PPPIP$
  IF PPPIP$ = "0.0.0.0" THEN
    PPPipReady% = 0
  ELSE 
    PPPipReady% = 1
    PRINT "PPPipReady Status is Ready"
  ENDIF 
    
  SETSYS INF,"load"
  TfIp$ = GETSYS INF,"ppp_stat"
  PRINT "TfIp," + TfIp$
  // Example of ESTAT use
  SETSYS INF,"load"
  GsmNetReg$ = GETSYS INF,"GsmNetReg"
  PRINT "GsmNetReg," + GsmNetReg$
  IF GsmNetReg$ = "1" Or GsmNetReg$ = "5" THEN
    GsmNetRegready% = 1
    PRINT "GsmNetRegready status is ready"
  ELSE 
    GsmNetRegready% = 0
  ENDIF 
  SETSYS INF,"load"
  ModemInitStatus$ = GETSYS INF,"ModemInitStatus"
  PRINT "ModemInitStatus," + ModemInitStatus$
  IF ModemInitStatus$ = "0" THEN
    ModemInitStatusready% = 1
    PRINT "ModemInitStatusready status is ready"
  ELSE 
    ModemInitStatusready% = 0
  ENDIF 
  IF PPPipReady% = 1 And GsmNetRegready% = 1 And ModemInitStatusready% = 1 THEN
    GsmReady% = 1
    GOSUB EmtyLog
  ELSE
    GsmReady% = 0
  ENDIF
  
  PRINT "GsmReady = " + STR$(GsmReady%)
  Return
END
  
Set_hist_timer_h:
  NewLog% = hist_log_interval_seconds_highpri@
  HistPrefix$ = "Datalog_h_"
  HistPrefixLen%=LEN(HistPrefix$)
  Teller% = 0
  FOR i%=0 TO NumberOfTags% -1
    Setsys Tag,"load", -i%
    Navn$ = Getsys Tag,"Name"
    IF LEN(Navn$) > HistPrefixLen% THEN
      IF Navn$(1 TO HistPrefixLen%) = HistPrefix$ THEN  
        SETSYS TAG, "LogTimer",NewLog%
        SETSYS TAG, "SAVE"
        Teller% = Teller% + 1
      ENDIF
    ENDIF
    NEXT i%
    PRINT "The highpri log interval on " + STR$(Teller%) + " tags is set TO " + STR$(NewLog%)
  RETURN
Set_hist_timer_m:
  NewLog% = hist_log_interval_seconds_medpri@
  HistPrefix$ = "Datalog_m_"
  HistPrefixLen%=LEN(HistPrefix$)
  Teller% = 0
  FOR i%=0 TO NumberOfTags% -1
    Setsys Tag,"load", -i%
    Navn$ = Getsys Tag,"Name"
    IF LEN(Navn$) > HistPrefixLen% THEN
      IF Navn$(1 TO HistPrefixLen%) = HistPrefix$ THEN  
        SETSYS TAG, "LogTimer",NewLog%
        SETSYS TAG, "SAVE"
        Teller% = Teller% + 1
      ENDIF
    ENDIF
    NEXT i%
    PRINT "The medpri log interval on " + STR$(Teller%) + " tags is set TO " + STR$(NewLog%)
  RETURN
Set_hist_timer_l:
  NewLog% = hist_log_interval_seconds_lowpri@
  HistPrefix$ = "Datalog_l_"
  HistPrefixLen%=LEN(HistPrefix$)
  Teller% = 0
  FOR i%=0 TO NumberOfTags% -1
    Setsys Tag,"load", -i%
    Navn$ = Getsys Tag,"Name"
    IF LEN(Navn$) > HistPrefixLen% THEN
      
      IF Navn$(1 TO HistPrefixLen%) = HistPrefix$ THEN  
        SETSYS TAG, "LogTimer",NewLog%
        SETSYS TAG, "SAVE"
        Teller% = Teller% + 1
      ENDIF
    ENDIF
    NEXT i%
    PRINT "The lowpri log interval on " + STR$(Teller%) + " tags is set TO " + STR$(NewLog%)
  RETURN
SaveAlarms:
  PRINT "SaveAlarms:"
  AlarmCount% = 0
  FOR i%=0 TO NumberOfTags% -1
    SETSYS Tag,"load", -i%
    HaveAlarm$ = GETSYS Tag, "TagValue" 
    HaveAlarm% = VAL(HaveAlarm$)
    IF HaveAlarm% > 0 THEN
      Name$ = Getsys Tag,"Name"
      AlarmCount% = AlarmCount% +1
      IF AlarmCount% = 1 THEN
        OPEN AlarmFileName$ FOR TEXT OUTPUT As 6
        // Must maybe test IF Tag exist
      ENDIF
      PUT 6, Name$
    ENDIF
    Next i%
    IF AlarmCount% > 0 THEN
        CLOSE 6
    ENDIF
 Return
 
EmtyLog:
PRINT "EmtyLog:"
  OPEN AlarmFileName$ FOR TEXT INPUT As 6
  ReadNext6:
    IF EOF 6 THEN GOTO Read1Done
      AlarmTag$ = GET 6
    IF SmsStackQty% > 0 THEN 
      FOR L% = 1 TO SmsStackQty%
        IF AlarmTag$ = RTRIM SmsTag$(L%)  THEN
          PRINT "Not resend sms FOR alarm " + AlarmTag$
          SmsNumbers$(L%) = ""
          SmsStack$(L%) = ""
          SmsTag$(L%) = ""
        ENDIF
        NEXT L%
    ENDIF
    GOTO ReadNext6
Read1Done:
  CLOSE 6
  OPEN AlarmFileName$ FOR TEXT OUTPUT As 6
  CLOSE 6
  OldAlarmsEnd:
  IF SmsStackQty% > 0 THEN
    FOR L% = 1 TO SmsStackQty%
      IF RTRIM SmsNumbers$(L%) <> "" THEN
          SENDSMS RTRIM SmsNumbers$(L%), SmsStack$(L%)
          PRINT "(From table) Saved SMS message sent!"
          PRINT "(From table) Message text: " + SmsStack$(L%)
          PRINT "(from table) Sent TO: " + SmsNumbers$(L%)
      ENDIF
      NEXT L%
  ENDIF
  SmsStackQty% = 0
  RETURN
  
Sett_Alle:
  PRINT "Sett_Alle:"
  GOSUB Sett_1
  GOSUB Sett_2
  GOSUB Sett_3
  GOSUB Sett_4
  GOSUB Sett_5
  GOSUB Sett_6
  RETURN
  
Sett_1:
  numTxt$ = STR$(numberTag1@)
  PreFix$ = "1_"
  GOSUB Set_phone
  RETURN
  
Sett_2:
  numTxt$ = STR$(numberTag2@)
  PreFix$ = "2_"
  GOSUB Set_phone
  RETURN
  
Sett_3:
  numTxt$ = STR$(numberTag3@)
  PreFix$ = "3_"
  GOSUB Set_phone
  RETURN
  
Sett_4:
  numTxt$ = STR$(numberTag4@)
  PreFix$ = "4_"
  GOSUB Set_phone
  RETURN
  
Sett_5:
  numTxt$ = STR$(numberTag5@)
  PreFix$ = "5_"
  GOSUB Set_phone
  RETURN
  
Sett_6:
  numTxt$ = STR$(numberTag6@)
  PreFix$ = "6_"
  GOSUB Set_phone
  RETURN
  
Set_phone:
  PRINT "Set_phone:"
  Teller% = 0
  PreFixLen% = LEN(PreFix$)
  FOR i%=0 TO NumberOfTags% -1
    Setsys Tag,"load", -i%
    Navn$ = Getsys Tag,"Name"
	  IF Navn$(1 TO PreFixLen% ) = PreFix$ THEN  
		  IF FINS_Ready% > 0 THEN
			  IF LEN(numTxt$) >7  THEN  
				SETSYS TAG, "STO", callCode$ + numTxt$ + ",gsm," + smscnumber$
				SETSYS TAG, "SAVE"
				ONALARM -i% , "@send_cusTOm_sms()"
			  ELSE
				SETSYS TAG, "STO", "" 
				SETSYS TAG, "SAVE"
				ONALARM -i% , ""
			  ENDIF
			  Teller% = Teller% + 1
		  ELSE
			ONALARM -i% , "@send_cusTOm_sms()"
		  ENDIF
	  ENDIF
	  
    Next i%
    IF LEN(numTxt$) >7  THEN  
      PRINT "On " + STR$(Teller%) + " tags with " + PreFix$ + " the alarm is TO " + numTxt$
    ELSE
      PRINT "On " + STR$(Teller%) + " tags with " + PreFix$ + " the alarm is TO removed"  
    ENDIF
  RETURN
 
FUNCTION send_cusTOm_sms()   
  tagNum% = GETSYS PRG, "EVTINFO"  
  alState% = ALSTAT(tagNum%)
  IF (alState% = 2) THEN
      SETSYS TAG, "LOAD", tagNum%
      almTxt$ = GETSYS TAG, "Description"
      Navn$ = Getsys Tag,"Name"
      msgTxt$ = @ReplaceStr$(Time$,"/", ".") + " " + almTxt$ + " (" + Navn$(1 TO 2) + ")"
          
      SETSYS TAG, "LOAD", tagNum%
      numbers$ = GETSYS TAG, "STO"
      IF GsmReady% >0 THEN
      
        SENDSMS numbers$, msgTxt$
        PRINT "SMS message sent!"
        PRINT "Message text: " + msgTxt$
        PRINT "Sent TO: " + numbers$
      ELSE
	  
        IF SmsStackQty%<MaxSmsStack%  THEN
            SmsStackQty%=SmsStackQty% + 1
            SmsStack$(SmsStackQty%)=msgTxt$
            SmsNumbers$(SmsStackQty%)=numbers$
            SmsTag$(SmsStackQty%) = Navn$
            PRINT "Saved sms " + STR$(SmsStackQty%)
        ENDIF
      ENDIF
  ENDIF
  ENDFN
Function ReplaceStr$($InputString$, $SearchString$, $ReplaceByString$)
    $Loop:
      $posstr% = INSTR 1, $InputString$, $SearchString$
      IF $posstr% = 0 THEN GOTO $endloop
        $SearchStringEndPos% = $posstr% + LEN $SearchString$
        $LenInputString% = LEN $InputString$
        
        IF $SearchStringEndPos% > $LenInputString% THEN //SearchString is at the end of the InputString
            $ReplaceStr$ = $InputString$(1 TO $posstr%-1) + $ReplaceByString$
        ELSE
            $ReplaceStr$ = $InputString$(1 TO $posstr%-1) + $ReplaceByString$ + $InputString$($SearchStringEndPos% TO $LenInputString%)
        ENDIF  
    $InputString$ = $ReplaceStr$
    GOTO $Loop
$endloop:
	ENDFN
	END
 
ModemReboot:
    PRINT "ModemReboot:"
    GOSUB SaveAlarms
    REBOOT
END
checkVpnConnection:
    PRINT "checkVpnConnection:"
    A$ = GETSYS PRG,"VPNIP"
    IF (A$="0.0.0.0") THEN 
      GOSUB SaveAlarms
      REBOOT
   ENDIF
   
END
Rem --- eWON user (end)
End
Rem --- eWON end section: Init Section