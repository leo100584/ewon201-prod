Rem --- eWON start section: Init Section
eWON_init_section:
	Rem --- eWON user (start)
  
  //MQT connection:
  //---------------
  Cls  //clear screen/console terminal..
  SETSYS INF, "LOAD"
  PRINT "CLS - clearing console..."
  SerNum$ = "Anlegg240XX"  //hardcode facility name..
  SendTagValues% = 0
  //########   CONFIG   ###############
  MQTTBrokerURL$ = "tools.ewonsupport.biz"
  MQTTPort$ = "1883"
  TopicToPublishOn$ = "/topic/flexy/" + SerNum$ + "/data"
  TopicToSubscribe$ = "/topic/flexy/" + SerNum$ + "/command"
  MsgToPublish$ = "Hello From Flexy " + SerNum$
  //<--Uncomment the below line if you want to send your Tag values in a json format-->
  SendTagValues% = 1
  //To update Tags, you can send/publish the json {"tagname1":12.3,"tagname2":4.56}
  //######## END CONFIG ###############
  
  //START SCRIPT
  Last_ConnStatus% = 0
  //Configure MQTT Connection parameters
  CONNECTMQTT:
    PRINT "CONNECTMQTT"
    MQTT "OPEN", SerNum$ , MQTTBrokerURL$
    MQTT "SETPARAM", "PORT", MQTTPort$ 
    MQTT "SETPARAM", "KEEPALIVE", "10"
    MQTT "SETPARAM", "WILLTOPIC", "/willtopic"
    MQTT "SETPARAM", "WILLPAYLOAD", "Flexy " + SerNum$ + " is disconnected"
    MQTT "SUBSCRIBE",TopicToSubscribe$,1
    //Launch the MQTT process in the background
    SETSYS PRG,"RESUMENEXT",1  //Continue in case of error at MQTT "CONNECT"
    MQTT "CONNECT"
    PRINT 'CONNECTMQTT COMPLETE'
    //END
    //-------------
  
	TSET 1, 60
	ONTIMER 1, "GOTO Mainstart"
END
  
Mainstart:
  PRINT "Mainstart:"
  ///TSET 1, 0                                //Turn of Timer #1, to run once else, while(true)
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
  
  //If an error is raised --> Log a message
  ErrorReturned% = GETSYS PRG,"LSTERR"
  IF ErrorReturned% = 28 THEN @Log("[MQTT SCRIPT] WAN interface not yet ready")
  SETSYS PRG,"RESUMENEXT",0
  //When receiving a message from Broker, "GOTO MQTTRECEIVEMSG"
  ONMQTT "GOTO MqttRx"
  ONTIMER 4, "GOTO SENDDATA"
  TSET 4,30 //publish every 30 seconds
  
  PRINT "Mainstart COMPLETE"
  
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
SENDDATA:
  PRINT "SENDDATA"
  //Read MQTT Connection Status (5 = Connected, other values = Not connected)
  ConnStatus% = MQTT "STATUS"
  IF Last_ConnStatus% <> ConnStatus% THEN
    IF ConnStatus% = 5 THEN //Connection is back online
      @Log("[MQTT SCRIPT] Flexy connected to Broker")
    ELSE
      @Log("[MQTT SCRIPT] Flexy disconnected from Broker")
    ENDIF
    Last_ConnStatus% = ConnStatus%
  ENDIF
  //IF Connected --> Publish messages
  IF ConnStatus% = 5 THEN //If connected --> Publish
    IF SendTagValues% = 1 THEN
      NB% = GETSYS PRG,"NBTAGS"
      TagTime% = GETSYS PRG, "TIMESEC"
      MsgToPublish$ = '{"ts":' + STR$(TagTime%) + '000,"values":{'
      //MsgToPublish$ = "["
      FOR i% = 0 TO NB%-1
        SETSYS Tag, "load",-i%
        TagName$ = GETSYS Tag, "Name"
        result$ = @IsSubstring$(TagName$, "Datalog_")
        IF result$ = "true" THEN 
          TagValue$ = GETSYS Tag, "TagValue"
          IF i% = 0 THEN
            MsgToPublish$ = MsgToPublish$ + '"' + TagName$ + '": ' + TagValue$
          ELSE  
            MsgToPublish$ = MsgToPublish$ + ', "' + TagName$ + '": ' + TagValue$
          ENDIF
        ENDIF
      NEXT i%
      MsgToPublish$ = MsgToPublish$ + "}}"
    ENDIF
    MQTT "PUBLISH",  TopicToPublishOn$ , MsgToPublish$, 0,0
    PRINT "[MQTT SCRIPT] Message '" + MsgToPublish$ + "' sent on topic : " +  TopicToPublishOn$
  ELSE //If not connected --> Save message in file
    @Log("[MQTT SCRIPT] Flexy not connected")
  ENDIF
  END
  
MqttRx:
  PRINT "MqttRx" 
   //Executed when receiving messages from Broker
   //Example : {"TestMQTT":0,"string1":"test string","ana1":12.3} to update Tags
   MessageQty%=Mqtt "READ"  //Return the number of pending messages
   IF (MessageQty%>0) Then
      MsgTopic$= MQTT "MSGTOPIC"
      MsgData$ = MQTT "MSGDATA"
      Print "Subscribe Message Received:" +MsgTopic$
      Print "Message:" +MsgData$
      posJson% = 1
      
ReadJsonNext:
    PRINT "ReadJsonNext" 
      JsonPart$ = @SplitString$(MsgData$,posJson%, ",")
      //PRINT JsonPart$
      posJson% = posJson%+1
      IF JsonPart$ = "" THEN GOTO ReadJsonEnd //no more data to read
      JsonPart$ = JsonPart$ + "}" // to help to detect first and middle elements end
      Pos1stQuote% = INSTR 1,JsonPart$, '"'
      Pos1stQuote% = Pos1stQuote%+1
      Pos2stQuote% = INSTR Pos1stQuote%,JsonPart$, '"'
      TagNameToWrite$ = JsonPart$(Pos1stQuote% TO Pos2stQuote%-1)
   
      Pos1stColumn% = INSTR 1,JsonPart$, ':'
      Pos1stColumn% = Pos1stColumn%+1
      Pos2stBraket% = INSTR Pos1stColumn%,JsonPart$, '}'
      TagValueToWrite$ = JsonPart$(Pos1stColumn% TO Pos2stBraket%-1)
   
      //PRINT TagNameToWrite$ + " - " + TagValueToWrite$
      IF TagValueToWrite$ >"" THEN
        //Print "valid value"
        SETSYS PRG,"RESUMENEXT",1
        SETSYS Tag, "load", TagNameToWrite$
        LastError%= GETSYS PRG,"LSTERR"
        IF LastError% = -4 THEN SETSYS PRG,"RESUMENEXT",0 : GOTO ReadJsonNext
        myType$ = GETSYS TAG, "Type"
        If myType$ = "6" THEN
           TagValueToWrite$ = JsonPart$(Pos1stColumn%+1 TO Pos2stBraket%-2)  //Trim off quote marks
           //PRINT TagValueToWrite$
           SETIO TagNameToWrite$, TagValueToWrite$
        ELSE
          SETIO TagNameToWrite$, VAL TagValueToWrite$
        ENDIF
        SETSYS PRG,"RESUMENEXT",0
      ENDIF
      GOTO ReadJsonNext
      
ReadJsonEnd:   
      PRINT MsgData$ + " (Topic: " + MsgTopic$ + ")"
      GOTO MQTTRx
   ENDIF
END
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
FUNCTION SplitString$($StringToParse$,$Pos%, $char$)
  $e% = 1
  $loopnbr% = 0
  $NextItem:
  $f% = INSTR $e% , $StringToParse$ , $char$
  //PRINT STR$ $f%
  //LAST ELEMENT
  IF $f% = 0 THEN 
      $B$ = $StringToParse$( $e% TO LEN $StringToParse$)
      IF $Pos% > $loopnbr% + 1 THEN $B$ = ""
      GOTO $EndOfLine       
  ENDIF
  
  $loopnbr% = $loopnbr% + 1
  $B$ = $StringToParse$( $e% TO $f%-1)
  IF $Pos% = $loopnbr% THEN GOTO $EndOfLine
  $e% = $f% + 1 //REM Init for next loop/line
  
  GOTO $NextItem
  $EndOfLine:
  $SplitString$ = $B$
ENDFN
FUNCTION Log($Msg$)
  LOGEVENT  $Msg$ ,100
  PRINT $Msg$
ENDFN
FUNCTION GetTime$()
  $a$ = Time$
  $GetTime$ = $a$(7 To 10) + "-" + $a$(4 To 5) + "-" + $a$(1 To 2) + " " + $a$(12 To 13)+":"+$a$(15 To 16)+":"+$a$(18 To 19)
ENDFN
FUNCTION IsSubstring$($inputString$, $searchString$)
  // Initialize the return value to "false"
  $IsSubstring$ = "false"
  // Find the position of the searchString$ in the inputString$
  pos% = INSTR 1, $inputString$, $searchString$
  
  // If the position is greater than 0, searchString$ is a substring of inputString$
  IF pos% > 0 THEN
    $IsSubstring$ = "true"
  ENDIF
ENDFN
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