� �� 0�  �   BigFrame�  notebook,  page1. ServerIsWeasel1 ServerIsNotWeasel/ Page1DomainLabelC 
MailDomain%  R BatchLimitLabelQ 
BatchLimitH SMTPServerLabelE 
SMTPServerI SMTPPortLabelJ SMTPportP 	WeaselDirL POPServerLabelO EnableSMTPAuthenticationD 	POPServerM POPPortLabelN POPportn 	LogDetail- Page1ServerLabel3 	SMTPGroupK POPGroupA 
RootDirBox�  MailingLists� ListList� 
ListsTitle� AddListButton� EditListButton� RenameListButton� DeleteListButtonX  ELpage1z LoginNameLabel{ 	LoginNameq ELpage1PasswordLabelr POP3passwordh ELpage1AbbrevLabelY ListAbbreviationa ELpage1IntervalLabelb 
CheckTimeHc  d 
CheckTimeMe  f 
CheckTimeSg  i ErrorsIgnorej ErrorsMailTok ErrorMessageRecipientw 
FilterProgy ELpage1LangLabelx ListLanguageZ ELpage1ErrmesLabelv ELpage1FilterLabel�  
GetOneLine� OneLineEntry�  �  AdminDialogue� AdminUsernameLabel� AdminLoginnameLabel� AdminPasswordLabel� 	AdminName� AdminLoginName� AdminPassword� 
AdminReply� AdminIgnore� AdminFilter� AdminSpinHH� AdminHourLabel� AdminSpinMM� AdminMinuteLabel� AdminSpinSS� AdminSecondLabel� AdminLangLabel� AdminLanguage� AdminDefaultLangLabel� DefaultLanguage� AdminMIMELabel� MIMEcharset� AdminAccountLabel� AdminErrorResponse� AdminFilterLabel� AdminTimeLabel   	AboutPage! About1" About2$  %  &  ' About4(  # About3&  	ListFrame' ListNotebookL  ELpage4R MembersPageTitleN nameboxO 	AddmemberP RevisememberQ DeletememberS MemberCount�  MessagePage� MessagesTitle� NotSubscribed� HelpFile� PlainTextFile� MessagesNonsubLabel� MessagesHelpLabel� MessagesNotPlainLabel�  
PSDialogue� 	AuthDummy� AuthNone� AuthSMTPAUTH� AuthPOPbeforeSMTP� PSUsernameLabel� 
PSUsername� PSPasswordLabel� 
PSPassword� PSPortLabel1� PSPort& PSPortLabel2� PSHostnameLabel� 
PSHostname� PSDone� MethodTitle  ELpage2 KillAttachments# DMARCcompatible IsModerated" SuppressFrom EnableSubscribe EnableLongSubscribe EnableLongUnsubscribe 	EnableWho  charset 	RFC2919ID! Add2369Headers EnableControlled CharsetLabel RFC2919IDLabelx  ELpage5~ OwnersPageTitle OwnersNote1� OwnersNote2z listownerbox{ AddOwner| ReviseOwner} DeleteOwner�  ELpage6� 
ArcEnabled� NewArchiveLabel� ArcInterval� ArcTimeUnitd   FirstWindowf  LocalButtong  RemoteButtonh  GoButtoni  SetupButtonj  
ExitButtonk  Statuse   @  SetupDialogueA 
RHostLabelB 
ISHostnameC ISOKE 
RPortLabelD ISportG 
RPassLabelF 
ISpasswordI 	RDirLabelH ISWdir�  ELopt2� RequireConfirmation� ConfTimeLabel� ConfTime� ConfHoursLabel� OwnersMayConfUnsub� NonsubIgnore� NonsubReject� NonsubAccept� SaveRejects� 	RejectDir� ELopt2ConfBox� 	NonsubBox  
ELMessage1	 MessagePage1Title NotifyOwnerMessage NotifyOwner2Message� ConfReqMessage
 NotifyOwnerLabel NotifyOwner2Label ConfReqLabell  
ELMessage2m MessagePage2Title� WelcomeMessage� DepartureMessage� 
LeaderName� 
FooterName� WelcomeLabel� DepartureLabel� LeaderLabel� FooterLabel^  	FilesPage` TranslogLabela DiskLogb 	ScreenLogc TransLogFileLabeld TransLogFilee 
ArchiveDir_ TranslogBoxf ArchiveDirLabel� �� p�   �   �  ��           J   �  &� � ��K       (   O ��    � � P �� #    �                        `   8.Helvetica                                                                                     � �, �  �  �  ��           �   s 1 � ,���         � � � o 
 .���         � �� � q 
 1���         �� z S 	 /���          � p �  C-         5    � �  %6��        O� \ s 	 RT��        k �� ] ,  Qq��         �� F 0 
 H���         � �? F � 	 E��         � �� F  
 I���          �� G   J��          � 7 � 	 PB         J � # 0 
 LK��         p �@ : � 
 Oq��         � �? $ � 	 D��         � �� #  
 M���         � �� $   N���       
 � �  A 	 n��           � j � , -��         6  � 9 �   37��        P  � ! �  Kn��         �  � " � 4 A���               9.WarpSans             9.WarpSans             9.WarpSans             9.WarpSans    �               9.WarpSans   Text          9.WarpSans Entry          9.WarpSans  !                  9.WarpSans    !                  9.WarpSans          !                  9.WarpSans             9.WarpSans    !                  9.WarpSans          !                  9.WarpSans             9.WarpSans    !                  9.WarpSans          !                  9.WarpSans             9.WarpSans   Log detail          9.WarpSans             9.WarpSans             9.WarpSans   POP3 server for incoming mail          9.WarpSans             9.WarpSans   ���,0$  �DChanged�result = Text()
IF (RIGHT(result,1) <> '/') & (RIGHT(result,1) <> '\') THEN result = result||'/'
CALL INISetValue '$SYS', 'MailRoot', resultInit�ans = INIGetValue('$SYS', 'MailRoot')
IF (ans = 'ERROR:') | (ans = '') THEN
  DO
    ans = ThisDirectory || '\mailroot\'
    CALL INISetValue '$SYS', 'MailRoot', ans
  END
CALL Text ans�PChanged�result = Text()
IF (RIGHT(result,1) <> '/') & (RIGHT(result,1) <> '\') THEN result = result||'/'
CALL INISetValue '$SYS', 'MailRoot', resultInit�ans = INIGetValue('$SYS', 'MailRoot')
IF (ans = 'ERROR:') | (ans = '') THEN
  DO
    ans = ThisDirectory || '\mailroot\'
    CALL INISetValue '$SYS', 'MailRoot', ans
  END
CALL Text ans�EChanged�result = Text()
IF (RIGHT(result,1) <> '/') & (RIGHT(result,1) <> '\') THEN result = result||'/'
CALL INISetValue '$SYS', 'MailRoot', resultInit�ans = INIGetValue('$SYS', 'MailRoot')
IF (ans = 'ERROR:') | (ans = '') THEN
  DO
    ans = ThisDirectory || '\mailroot\'
    CALL INISetValue '$SYS', 'MailRoot', ans
  END
CALL Text ans�� ��     �  ��           �     1 � ����          �  �	  � � �����         �  �� � F 
 �� ��           �� a .  �����           �� I .  �����           �� 0 .  �����           ��  .  �����                         	       � �X �  �  �  ��           �   �  
� X���         ��  s 	 z����         � �z � �  {���         � � r s 	 q����         �� �z s �  r����         � � f � 	 h����         � �� g L  Y����         � � X � 
 a����          ��� X  
 b����        � �� X  
 c����          ��� X  
 d����        � �� X  
 e����          ��� X  
 f����        � �� X  
 g����         � � < , 	 i����         � �? < ( 	 j����         � �j = �  k���         � � ! �  w���         ��  M 	 y����         � �_  y  x����         �  � :   Z����         �  �    v����                   H  M  S                     � ��0w   w   �  ��           h     �  ���i          m  �  �  ���n          v   t   ( 
 �����            � �� �  �  �  ��              2 ) � ���         � � ? 
 � ��         7 � | ? 
 �8��         O � p ? 
 �P��         g �V � �  �h         � �V } �  ���         �� �V q �  ����        � �0 X J 
 ����        � �� X J 
 ����         � � C �  ��           � ,   ���        2 �0 , , 
 �4��          K �e ,   �L��        c �� , , 
 �e��          | �� ,   �}��        � �� , , 
 ����         ��  r 
 ����         � ��  |  ����         � �  r 
 ����         � ��  |  ����          �  r 
 ���         % ��  |  �&=         E  � l � / �F��        ]  � U �  �r��        �  � @ �  ����         �  � ( �  ����               9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans                 9.WarpSans                 9.WarpSans Reply          9.WarpSans Ignore          9.WarpSans           9.WarpSans                 9.WarpSans H          9.WarpSans           9.WarpSans M          9.WarpSans           9.WarpSans S          9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans  @               9.WarpSans Response to spammers          9.WarpSans Filter          9.WarpSans           9.WarpSans � �  z  z  �  ��              � $ �  ��         !�# � � 	 !����         " �; { � 
 "����        #�D X � 	 $0��        � �A K � 
 %���        � � > � 
 &��         V�! " � 
 '����       ! W �,  � 	 (����         y  � 8 � 4 #����       Peter Moylan P               <   9.Helvetica                                                 peter@pmoylan.org P               <   9.Helvetica                                                 http://www.pmoylan.org/ P               <   9.Helvetica                                                  http://www.pmoylan.org/pages/os2/  � �&p�   �   �  ��           J �    %� &��K       (   O ��    � 'P �� #    �                        `   8.Helvetica                                                                                     � �L �   �   �  ��           �    � f 
� L���          � � | � 
 R����         �  �  � t N����         �   �� ^ ,  O����         �   �� F ,  P����         �   �� / ,  Q����         �  ��  : 
 S����           � �� "  "  �  ��           �    h V � ����          �	 � � 	 �����          � a �  ���          � = �  ���          �  �  ���           � ]  �����            � 9  �����         !  �   �����                              � ��0    �  ��           �  ( Z � b ����         �   X ( 
 �����         � � J ) 
 �����         � �1 J @ 
 �����         � �u J Y 
 �����         �� 8 4 
 �����         � �= 9 �  ����          � , 4 
 �����         � �= - �  �����          �   4 
 �����          �= !   �����          �Y   w 
 &����          �  4 
 �����          �=  �  ���         �P  ( 
 �����           � F �  ����� #                            OK  � � :  :  �  ��           �   P 4 
� ���         � �  s 
 ���        � ��  z 
 #���         
 � u s 	 ��         " � j s 	 "#��         : �
 J v 
 ;��         R �
 > v 
 S��         j �� J v 
 k��         � �� > v 
 ���         � � ) �   ��         � �  �  ��         � �  � 
 !���         �  � <  " ���         
  � $   ��         "  �    #��               9.WarpSans Check          9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans                 9.WarpSans                 9.WarpSans           9.WarpSans           9.WarpSans           9.WarpSans � �x h  h  �  ��           �    0 M 
� x���          � | � 
 ~����          � n �  ��         4 � g �  �5��         d �  � Z z����         e  �� A ,  {����         f  �� ) ,  |����         g  ��  ,  }����       +         
         9.Helvetica             +         
         9.Helvetica                � �� �   �   �  ��           �    h F 
� ����          �  �; V � 
 �����         �  � 8 q 
 �����         �  �� 9 '  �����          �  �� 8 4 
 �����         � �d 0�  �  �  ��           �   � ? ~ 5 d ���           � * ?  f ��          �  ? 
 g ��         3 �F  6  h 4��         a  �  =  i b��         y  �F  6  j z��         �A �   |  k ���         �    C  e ���              9.WarpSans           9.WarpSans  )      !   20.Helvetica Bold                          9.WarpSans           9.WarpSans           9.WarpSans                    9.WarpSans � �@0d  d  �  ��    	       :  �H 6 � 1 @��;         ?� $ i 
 A����         @ �m % k  B��A        I   ]  ( 
 C����         N �  i 
 E����         O �m  k  D��P         X �  i 
 G����         Y� �m  k  F����         Z �  i 
 I����         [ �m  k  H��\ #           Push                   � �� �  �  �  ��           �   $ J 
� ����         � �$ k � 
 �����         � � ] � 
 ����         � �� ^ '  �����         � �� ] - 
 ����         � �$ N � 
 �����         � � ) < 
 �����         � �g ) < 
 �����         � �� ) < 
 �����         � �  � 
 �����         � �  �  ����         �  �
 H � : �����         �  �
  � 7 �����                                               � � "  "  �  ��           �     ( 
� ���          � ~ 	 	����          � _ �  ��          � @ �  ��          �   �  ���           � \   
����            � =   ����         !  �    ����                              � �l h  h  �  ��    	       :    = 
� l��;         ?� ~ 	 m����         @ � a �  ���A         I � D �  ���J         R � ( �  ���S         [ �  �  ���\         d  � ]   �����         e  � A   �����         f  � %   �����         g  � 	   �����                                      � �^ W  W  �  ��              p b � ^��         !� r w 
 `"��         2 �� r , 
 a����         3 �� r 3 
 b����        4 � e � 
 c����        9 � [ �  d��?        G � . �  e��M         U  �
 S � 1 _����         V  �
 ( �  f����                    Text Entry       Entry         ���^0T   �bClickICALL PutCardinal 'TransLevel', 0, DiskLog.Select() + 2*ScreenLog.Select()�