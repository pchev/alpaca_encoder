; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!
 
[Setup]
AppName=Alpaca_Encoder
AppVerName=Alpaca_Encoder V1
AppPublisherURL=https://github.com/pchev/alpaca_encoder
AppSupportURL=https://github.com/pchev/alpaca_encoder
AppUpdatesURL=https://github.com/pchev/alpaca_encoder
UsePreviousAppDir=true
DefaultDirName={commonpf}\Alpaca_Encoder
DefaultGroupName=Alpaca_Encoder
AllowNoIcons=true
InfoBeforeFile=Presetup\readme.txt
OutputDir=.\
OutputBaseFilename=alpaca_encoder-windows
Compression=lzma
SolidCompression=true
Uninstallable=true
UninstallLogMode=append
DirExistsWarning=no
ShowLanguageDialog=yes
AppID={{beb20602-8709-48c9-a902-c3870b8f5e6c}
ArchitecturesInstallIn64BitMode=x64

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}

[Files]
Source: "Prog\alpaca_encoder-x64.exe"; DestDir: "{app}"; DestName: "alpaca_encoder.exe"; Check: Is64BitInstallMode
Source: "Prog\alpaca_encoder.exe"; DestDir: "{app}"; Check: not Is64BitInstallMode
Source: Data\*; DestDir: {app}; Flags: ignoreversion recursesubdirs createallsubdirs restartreplace
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: {group}\Alpaca_Encoder; Filename: {app}\alpaca_encoder.exe; WorkingDir: {app}
Name: {commondesktop}\Alpaca_Encoder; Filename: {app}\alpaca_encoder.exe; WorkingDir: {app}; Tasks: desktopicon
 
