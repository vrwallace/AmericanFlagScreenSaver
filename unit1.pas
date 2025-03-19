unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, UniqueInstance, MultiMon, Winsock, contnrs, Math, DateUtils,MMSystem;

const
  DISPLAY_DEVICE_ACTIVE = $00000001;
  DISPLAY_DEVICE_MIRRORING_DRIVER = $00000008;

type
  TDisplayDevice = record
    cb: DWORD;
    DeviceName: array[0..31] of Char;
    DeviceString: array[0..127] of Char;
    StateFlags: DWORD;
    DeviceID: array[0..127] of Char;
    DeviceKey: array[0..127] of Char;
  end;

const
  WH_MOUSE_LL = 14;
  WH_KEYBOARD_LL = 13;
  IP_SUCCESS = 0;
  IP_BUF_TOO_SMALL = 11001;
  IP_REQ_TIMED_OUT = 11010;
  TH32CS_SNAPPROCESS = $00000002;
  WM_DISPLAYCHANGE = $007E;

  // American flag colors
  FLAG_RED = TColor($0000FF); // BGR format in Delphi
  FLAG_BLUE = TColor($FF0000); // BGR format
  FLAG_WHITE = TColor($FFFFFF);


   ES_SYSTEM_REQUIRED = $00000001;
  ES_DISPLAY_REQUIRED = $00000002;
  ES_CONTINUOUS = $80000000;

type
  // Text position record
  TTextPosition = record
    Char: WideChar;
    X, Y: Integer;
    FallingSpeed: Integer;
    TextColor: TColor;
  end;

  // Firework particle record
  TFireworkParticle = record
    X, Y: Double;
    VelX, VelY: Double;
    Color: TColor;
    Life: Integer;
  end;

  // Firework record
  TFirework = class
  public
    X, Y: Double;
    VelX, VelY: Double;
    Color: TColor;
    ExplosionTime: Integer;
    TimeToLive: Integer;
    HasExploded: Boolean;
    Particles: array of TFireworkParticle;
    ParticleCount: Integer;

    constructor Create;
    procedure Update;
    procedure Explode;
    procedure Draw(Canvas: TCanvas);
    function IsDead: Boolean;
  end;

  // Echo reply record for ping
  ICMP_ECHO_REPLY = packed record
    Address: DWORD;
    Status: DWORD;
    RoundTripTime: DWORD;
    DataSize: WORD;
    Reserved: WORD;
    Data: Pointer;
    Options: record
      Ttl: BYTE;
      Tos: BYTE;
      Flags: BYTE;
      OptionsSize: BYTE;
      OptionsData: Pointer;
    end;
  end;
  PICMP_ECHO_REPLY = ^ICMP_ECHO_REPLY;

  { TSystemInfoThread }
  TSystemInfoThread = class(TThread)
  private
    FOwner: TForm;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TForm);
  end;

  { TScreenSaverForm }
  TScreenSaverForm = class(TForm)
    Timer1: TTimer;
    TerminationTimer: TTimer;
    Timer2: TTimer;
    MonitorCountTimer: TTimer;
    UniqueInstance1: TUniqueInstance;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TerminationTimerTimer(Sender: TObject);
    procedure MonitorCountTimerTimer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; const Parameters: array of String);

    // New handlers for enhancements
    procedure FlagMoveTimerTimer(Sender: TObject);
    procedure WaveTimerTimer(Sender: TObject);
    procedure TwinkleTimerTimer(Sender: TObject);
    procedure QuoteTimerTimer(Sender: TObject);
    procedure FireworkTimerTimer(Sender: TObject);
    procedure WeatherTimerTimer(Sender: TObject);
    procedure LightingTimerTimer(Sender: TObject);
    procedure EagleTimerTimer(Sender: TObject);
    procedure FadeInTimerTimer(Sender: TObject);

  private
    // Original variables
    FTotalDiskSpace: Int64;
      FFreeDiskSpace: Int64;
    FDiskCheckCounter: Integer;
    HebrewText: WideString;
    TextStreams: array of array of TTextPosition;
    MouseMoved: Boolean;
    InitialMouseX, InitialMouseY: Integer;
    FormIndex: Integer;
    CpuUtilization: Integer;
    MemoryUtilization: Integer;
    DiskSpace: Int64;
    PrevIdleTime: Int64;
    PrevKernelTime: Int64;
    PrevUserTime: Int64;
    InfoX, InfoY: Integer;
    FTotalPhysicalMemory: Int64;
    FAvailablePhysicalMemory: Int64;
    FUsedDiskSpace: Int64;
    FPingTime: Integer;
    FSystemUptime: Int64;
    InitialMonitorCount: Integer;
    FFormattedDateTime: string;

    // Flag movement variables
    FlagOffsetX: Integer;
    FlagOffsetY: Integer;
    FlagMoveTimer: TTimer;
    FlagMoveDirection: Integer;

    // Flag and star images
    FlagImage: TBitmap;
    StarImage: TBitmap;
    FBaseStarImage: TBitmap;

    // Flag waving variables
    FWavePhase: Double;
    FWaveTimer: TTimer;
    FStaticFlagImage: TBitmap; // Original flag for wave calculations

    // Twinkling stars variables
    FTwinkleTimer: TTimer;
    FTwinkleStars: array of Boolean;
    FStarBrightness: array of Integer;
    FStarCount: Integer;

    // Patriotic quotes variables
    FQuotes: TStringList;
    FCurrentQuote: Integer;
    FQuoteTimer: TTimer;

    // Holiday detection variables
    FIsHoliday: Boolean;
    FHolidayName: string;
    FFireworksActive: Boolean;
    FFireworksList: TList;
    FFireworkTimer: TTimer;

    // Weather information variables
    FWeatherTemp: Integer;
    FWeatherCondition: string;
    FWeatherUpdateTimer: TTimer;
    FWeatherLastUpdate: TDateTime;

    // Time-based lighting variables
    FDayLightIntensity: Integer;
    FNightLightIntensity: Integer;
    FCurrentLightIntensity: Integer;
    FLightingTimer: TTimer;

    // Eagle animation variables
    FEagleImage: TBitmap;
    FEagleVisible: Boolean;
    FEagleX, FEagleY: Integer;
    FEagleFrame: Integer;
    FEagleFrameCount: Integer;
    FEagleTimer: TTimer;
    FEagleChance: Integer;
    FEagleDirection: Integer; // 1 = right, -1 = left

    // Visual indicators variables
    FUseGraphicalIndicators: Boolean;

    // Fade in transition variables
    FFadeInOpacity: Integer;
    FFadeInTimer: TTimer;

    // Original methods
    function GetCPUUsage: Integer;
    function GetPingTime(const IPAddress: string): Integer;
    procedure GetSystemInfo;
    function RetrieveSystemTimes(var IdleTime, KernelTime, UserTime: Int64): Boolean;

    procedure InitializeTextStreams;
    procedure UpdateTextPositions;
    procedure CheckMouseMovement(X, Y: Integer);
    procedure HideMouseCursor;
    procedure SetupTerminationTimer;
    procedure UpdateUI;

    // Flag and stars methods
    procedure CreateFlagImage;
    procedure CreateStarImage;
    procedure CreateBaseFlagImage;

    // Enhancement methods
    procedure CreateWavingFlagImage;
    procedure InitializeQuotes;
    procedure CheckForHolidays;
    procedure UpdateFireworks;
    procedure DrawFireworks(ACanvas: TCanvas);
    procedure UpdateWeatherInfo;
    procedure UpdateLighting;
    procedure InitializeEagle;
    procedure DrawEagle(ACanvas: TCanvas);
    procedure CreateSimpleEagleImage;
    procedure DrawSystemGauges(ACanvas: TCanvas; X, Y: Integer);
    function AdjustColorBrightness(AColor: TColor; Percent: Integer): TColor;
    function RandomFireworkColor: TColor;

  protected
    procedure WndProc(var Msg: TMessage); override;

  public
    constructor Create(AOwner: TComponent; AFormIndex: Integer); reintroduce;
    destructor Destroy; override;
  end;

  function GetKernelSystemTimes(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall; external 'kernel32.dll' name 'GetSystemTimes';
  function IcmpCreateFile: THandle; stdcall; external 'icmp.dll' name 'IcmpCreateFile';
  function IcmpCloseHandle(IcmpHandle: THandle): BOOL; stdcall; external 'icmp.dll' name 'IcmpCloseHandle';
  function IcmpSendEcho(IcmpHandle: THandle; DestinationAddress: DWORD; RequestData: Pointer;
    RequestSize: WORD; RequestOptions: Pointer; ReplyBuffer: Pointer;
    ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall; external 'icmp.dll' name 'IcmpSendEcho';
  function SetThreadExecutionState(esFlags: DWORD): DWORD; stdcall; external 'kernel32.dll';

procedure CreateFormsForAllMonitors;
function FormatUptime(Uptime: Int64): string;
function GetCurrentDateTimeFormatted: string;

var
  FormsList: array of TScreenSaverForm;
  ScreenWidths, ScreenHeights: array of Integer;
  hMouseHook: HHOOK;
  hKeyboardHook: HHOOK;

implementation

{$R *.lfm}

const
  BaseFallSpeed = 5;
  NumStreams = 15; // Number of text streams
  MouseSensitivity = 10; // Sensitivity threshold for mouse movement (in pixels)
  InfoTextSpacing = 40; // Vertical spacing between info texts

{ TFirework }

constructor TFirework.Create;
begin
  inherited;
  HasExploded := False;
  TimeToLive := 100 + Random(50);
  ParticleCount := 30 + Random(20);
  SetLength(Particles, ParticleCount);
end;

procedure TFirework.Update;
var
  i: Integer;
begin
  if not HasExploded then
  begin
    // Update rocket position
    X := X + VelX;
    Y := Y + VelY;

    // Apply gravity
    VelY := VelY + 0.1;

    // Check if it's time to explode
    Dec(ExplosionTime);
    if ExplosionTime <= 0 then
      Explode;
  end
  else
  begin
    // Update particles
    for i := 0 to ParticleCount - 1 do
    begin
      Particles[i].X := Particles[i].X + Particles[i].VelX;
      Particles[i].Y := Particles[i].Y + Particles[i].VelY;

      // Apply gravity
      Particles[i].VelY := Particles[i].VelY + 0.05;

      // Decrease life
      Dec(Particles[i].Life);
    end;

    // Decrease overall life
    Dec(TimeToLive);
  end;
end;

procedure TFirework.Explode;
var
  i: Integer;
  Angle, Speed: Double;
begin
  HasExploded := True;

  // Create particles in all directions
  for i := 0 to ParticleCount - 1 do
  begin
    Angle := 2 * Pi * Random;
    Speed := 1 + 2 * Random;

    Particles[i].X := X;
    Particles[i].Y := Y;
    Particles[i].VelX := Speed * Cos(Angle);
    Particles[i].VelY := Speed * Sin(Angle);
    Particles[i].Color := Color;
    Particles[i].Life := 30 + Random(30);
  end;
end;

procedure TFirework.Draw(Canvas: TCanvas);
var
  i: Integer;
  Alpha: Byte;
begin
  if not HasExploded then
  begin
    // Draw rocket
    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 2;
    Canvas.MoveTo(Round(X), Round(Y));
    Canvas.LineTo(Round(X - VelX * 2), Round(Y - VelY * 2));
  end
  else
  begin
    // Draw particles
    Canvas.Pen.Width := 1;
    for i := 0 to ParticleCount - 1 do
    begin
      if Particles[i].Life > 0 then
      begin
        // Calculate alpha based on remaining life
        Alpha := (Particles[i].Life * 255) div 30;
        if Alpha > 255 then Alpha := 255;

        Canvas.Pen.Color := Particles[i].Color;
        Canvas.Pixels[Round(Particles[i].X), Round(Particles[i].Y)] := Particles[i].Color;
      end;
    end;
  end;
end;

function TFirework.IsDead: Boolean;
begin
  Result := HasExploded and (TimeToLive <= 0);
end;

{ Mouse and Keyboard Hooks }

function MouseHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (nCode >= 0) and ((wParam = WM_MOUSEMOVE) or (wParam = WM_LBUTTONDOWN) or (wParam = WM_RBUTTONDOWN) or (wParam = WM_MBUTTONDOWN) or (wParam = WM_XBUTTONDOWN)) then
  begin
    Application.Terminate;
  end;
  Result := CallNextHookEx(hMouseHook, nCode, wParam, lParam);
end;

function KeyboardHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (nCode >= 0) and ((wParam = WM_KEYDOWN) or (wParam = WM_SYSKEYDOWN)) then
  begin
    Application.Terminate;
  end;
  Result := CallNextHookEx(hKeyboardHook, nCode, wParam, lParam);
end;

procedure CreateFormsForAllMonitors;
var
  I: Integer;
  MonitorInfo: TMonitorInfo;
  Form: TScreenSaverForm;
begin
  SetLength(FormsList, Screen.MonitorCount);
  SetLength(ScreenWidths, Screen.MonitorCount);
  SetLength(ScreenHeights, Screen.MonitorCount);

  for I := 0 to Screen.MonitorCount - 1 do
  begin
    MonitorInfo.cbSize := SizeOf(TMonitorInfo);
    GetMonitorInfo(Screen.Monitors[I].Handle, @MonitorInfo);

    ScreenWidths[I] := MonitorInfo.rcMonitor.Right - MonitorInfo.rcMonitor.Left;
    ScreenHeights[I] := MonitorInfo.rcMonitor.Bottom - MonitorInfo.rcMonitor.Top;

    OutputDebugString(PChar(Format('Monitor %d: Width=%d, Height=%d', [I, ScreenWidths[I], ScreenHeights[I]])));

    Form := TScreenSaverForm.Create(Application, I);
    Form.Left := MonitorInfo.rcMonitor.Left;
    Form.Top := MonitorInfo.rcMonitor.Top;
    Form.Width := ScreenWidths[I];
    Form.Height := ScreenHeights[I];

    FormsList[I] := Form;
    Form.Show;
  end;
end;

{ TSystemInfoThread }

constructor TSystemInfoThread.Create(AOwner: TForm);
begin
  inherited Create(True);
  FOwner := AOwner;
  FreeOnTerminate := True;
end;

procedure TSystemInfoThread.Execute;
var
  Form: TScreenSaverForm;
begin
  Form := TScreenSaverForm(FOwner);
  Form.GetSystemInfo;
  Synchronize(@Form.UpdateUI);
end;

{ TScreenSaverForm }

constructor TScreenSaverForm.Create(AOwner: TComponent; AFormIndex: Integer);
begin
  inherited Create(AOwner);
  FormIndex := AFormIndex;
  BorderStyle := bsNone;
  WindowState := wsMaximized;

  DiskSpace := 1; // Start with non-zero values
  FUsedDiskSpace := 0;
  FDiskCheckCounter := 6;

  // Initialize flag movement
  FlagOffsetX := 0;
  FlagOffsetY := 0;
  FlagMoveDirection := 0;

  // Create bitmap objects
  FlagImage := TBitmap.Create;
  StarImage := TBitmap.Create;
  FBaseStarImage := TBitmap.Create;
  FStaticFlagImage := TBitmap.Create;

  // Initialize flag movement timer
  FlagMoveTimer := TTimer.Create(Self);
  FlagMoveTimer.Interval := 5000; // Move every 5 seconds
  FlagMoveTimer.OnTimer := @FlagMoveTimerTimer;
  FlagMoveTimer.Enabled := false;

  // Initialize wave animation
  FWavePhase := 0;
  FWaveTimer := TTimer.Create(Self);
  FWaveTimer.Interval := 50; // Update wave every 50ms
  FWaveTimer.OnTimer := @WaveTimerTimer;
  FWaveTimer.Enabled := False; // Start this in FormCreate

  // Initialize star twinkling
  FTwinkleTimer := TTimer.Create(Self);
  FTwinkleTimer.Interval := 300; // Update every 300ms
  FTwinkleTimer.OnTimer := @TwinkleTimerTimer;
  FTwinkleTimer.Enabled := False; // Start this in FormCreate

  // Initialize quotes
  FQuotes := TStringList.Create;
  FCurrentQuote := 0;
  FQuoteTimer := TTimer.Create(Self);
  FQuoteTimer.Interval := 15000; // Change quotes every 8 seconds
  FQuoteTimer.OnTimer := @QuoteTimerTimer;
  FQuoteTimer.Enabled := False; // Start this in FormCreate

  // Initialize fireworks
  FFireworksList := TList.Create;
  FFireworksActive := False;
  FFireworkTimer := TTimer.Create(Self);
  FFireworkTimer.Interval := 30; // Update fireworks every 30ms
  FFireworkTimer.OnTimer := @FireworkTimerTimer;
  FFireworkTimer.Enabled := False; // Start if holiday detected

  // Initialize weather
  FWeatherTemp := 0;
  FWeatherCondition := 'Unknown';
  FWeatherUpdateTimer := TTimer.Create(Self);
  FWeatherUpdateTimer.Interval := 300000; // Update every 5 minutes
  FWeatherUpdateTimer.OnTimer := @WeatherTimerTimer;
  FWeatherUpdateTimer.Enabled := False; // Start this in FormCreate

  // Initialize lighting
  FDayLightIntensity := 100;
  FNightLightIntensity := 60;
  FCurrentLightIntensity := 100;
  FLightingTimer := TTimer.Create(Self);
  FLightingTimer.Interval := 60000; // Update lighting every minute
  FLightingTimer.OnTimer := @LightingTimerTimer;
  FLightingTimer.Enabled := False; // Start this in FormCreate

  // Initialize eagle
  FEagleImage := TBitmap.Create;
  FEagleVisible := False;
  FEagleX := 0;
  FEagleY := 0;
  FEagleFrame := 0;
  FEagleFrameCount := 8;
  FEagleChance := 1000; // 1 in 1000 chance per check
  FEagleDirection := 1; // Start moving right
  FEagleTimer := TTimer.Create(Self);
  FEagleTimer.Interval := 100; // Update every 100ms
  FEagleTimer.OnTimer := @EagleTimerTimer;
  FEagleTimer.Enabled := False; // Start this in FormCreate

  // Initialize fade-in effect
  FFadeInOpacity := 0;
  FFadeInTimer := TTimer.Create(Self);
  FFadeInTimer.Interval := 30;
  FFadeInTimer.OnTimer := @FadeInTimerTimer;
  FFadeInTimer.Enabled := False; // Start this in FormCreate

  // Initialize visual indicators
  FUseGraphicalIndicators := false;

  // Create star image and setup other components
  CreateStarImage;
end;

destructor TScreenSaverForm.Destroy;
var
  i: Integer;
begin
  // Free all timer objects
  FlagMoveTimer.Free;
  FWaveTimer.Free;
  FTwinkleTimer.Free;
  FQuoteTimer.Free;
  FFireworkTimer.Free;
  FWeatherUpdateTimer.Free;
  FLightingTimer.Free;
  FEagleTimer.Free;
  FFadeInTimer.Free;

  // Free all bitmap objects
  FlagImage.Free;
  StarImage.Free;
  FBaseStarImage.Free;
  FStaticFlagImage.Free;
  FEagleImage.Free;

  // Free quote list
  FQuotes.Free;

  // Free all fireworks
  if Assigned(FFireworksList) then
  begin
    for i := FFireworksList.Count - 1 downto 0 do
    begin
      TFirework(FFireworksList[i]).Free;
    end;
    FFireworksList.Free;
  end;

  inherited;
end;

procedure TScreenSaverForm.FlagMoveTimerTimer(Sender: TObject);
begin
  // Change direction periodically (0-7 for 8 directions)
  FlagMoveDirection := Random(8);

  // Update flag position
  case FlagMoveDirection of
    0: begin FlagOffsetX := FlagOffsetX + 2; FlagOffsetY := FlagOffsetY + 0; end;  // Right
    1: begin FlagOffsetX := FlagOffsetX + 2; FlagOffsetY := FlagOffsetY + 2; end;  // Down-Right
    2: begin FlagOffsetX := FlagOffsetX + 0; FlagOffsetY := FlagOffsetY + 2; end;  // Down
    3: begin FlagOffsetX := FlagOffsetX - 2; FlagOffsetY := FlagOffsetY + 2; end;  // Down-Left
    4: begin FlagOffsetX := FlagOffsetX - 2; FlagOffsetY := FlagOffsetY + 0; end;  // Left
    5: begin FlagOffsetX := FlagOffsetX - 2; FlagOffsetY := FlagOffsetY - 2; end;  // Up-Left
    6: begin FlagOffsetX := FlagOffsetX + 0; FlagOffsetY := FlagOffsetY - 2; end;  // Up
    7: begin FlagOffsetX := FlagOffsetX + 2; FlagOffsetY := FlagOffsetY - 2; end;  // Up-Right
  end;

  // Limit the maximum offset to prevent flag from moving too far
  if FlagOffsetX > 20 then FlagOffsetX := 20;
  if FlagOffsetX < -20 then FlagOffsetX := -20;
  if FlagOffsetY > 20 then FlagOffsetY := 20;
  if FlagOffsetY < -20 then FlagOffsetY := -20;

  // Force a repaint to show the new position
  Invalidate;
end;

procedure TScreenSaverForm.WaveTimerTimer(Sender: TObject);
begin
  // Update wave phase to animate the wave
  FWavePhase := FWavePhase + 0.1;
  if FWavePhase > 2 * Pi then
    FWavePhase := 0;

  // Redraw the flag with the new wave phase
  CreateWavingFlagImage;
  Invalidate;
end;

procedure TScreenSaverForm.TwinkleTimerTimer(Sender: TObject);
var
  i, StarsThatTwinkle: Integer;
begin
  // Determine how many stars should twinkle
  StarsThatTwinkle := FStarCount div 5; // About 20% of stars twinkle at once

  // Reset all stars to normal brightness
  for i := 0 to FStarCount - 1 do
  begin
    FTwinkleStars[i] := False;
    FStarBrightness[i] := 100; // 100% brightness
  end;

  // Randomly select stars to twinkle
  for i := 0 to StarsThatTwinkle - 1 do
  begin
    // Choose a random star
    FTwinkleStars[Random(FStarCount)] := True;
    // Assign a random brightness (50% to 150%)
    FStarBrightness[i] := 50 + Random(100);
  end;

  // Force redraw of the flag with twinkling stars
  CreateFlagImage;
  Invalidate;
end;

procedure TScreenSaverForm.QuoteTimerTimer(Sender: TObject);
begin
  // Move to the next quote
  Inc(FCurrentQuote);
  if FCurrentQuote >= FQuotes.Count then
    FCurrentQuote := 0;
  Invalidate; // Force repaint to show new quote
end;

procedure TScreenSaverForm.FireworkTimerTimer(Sender: TObject);
var
  i: Integer;
  Firework: TFirework;
begin
  if not FFireworksActive then Exit;

  // Randomly create new fireworks
  if (Random(100) < 5) and (FFireworksList.Count < 10) then // 5% chance each update, max 10 fireworks
  begin
    Firework := TFirework.Create;
    Firework.X := 100 + Random(ScreenWidths[FormIndex] - 200);
    Firework.Y := ScreenHeights[FormIndex];
    Firework.VelX := -2 + Random(4); // Random horizontal velocity
    Firework.VelY := -5 - Random(7); // Initial upward velocity
    Firework.Color := RandomFireworkColor;
    Firework.ExplosionTime := 30 + Random(50); // Frames until explosion
    FFireworksList.Add(Firework);
  end;

  // Update existing fireworks
  for i := FFireworksList.Count - 1 downto 0 do
  begin
    Firework := TFirework(FFireworksList[i]);
    Firework.Update;

    // Remove dead fireworks
    if Firework.IsDead then
    begin
      Firework.Free;
      FFireworksList.Delete(i);
    end;
  end;

  Invalidate; // Force redraw to show updated fireworks
end;

procedure TScreenSaverForm.WeatherTimerTimer(Sender: TObject);
begin
  UpdateWeatherInfo;
end;

procedure TScreenSaverForm.LightingTimerTimer(Sender: TObject);
begin
  UpdateLighting;
  CreateFlagImage; // Recreate flag with new lighting
  Invalidate;
end;

procedure TScreenSaverForm.EagleTimerTimer(Sender: TObject);
begin
  // Update eagle animation frame
  if FEagleVisible then
  begin
    Inc(FEagleFrame);

    // Move eagle - increase speed from 5 to 8
    FEagleX := FEagleX + 8 * FEagleDirection;

    // Only check if eagle has moved off screen, not frame count
    if (FEagleX > ScreenWidths[FormIndex] + 100) or (FEagleX < -100) then
      FEagleVisible := False;

    // Reset the frame counter if it gets too high, but don't end animation
    if FEagleFrame >= FEagleFrameCount * 5 then
      FEagleFrame := 0;
  end
  else
  begin
    // Random chance to start eagle animation
    if Random(FEagleChance) = 0 then
    begin
      FEagleVisible := True;
      FEagleFrame := 0;

      // Determine direction (left to right or right to left)
      if Random(2) = 0 then
      begin
        FEagleDirection := 1; // Right
        FEagleX := -100; // Start off-screen left
      end
      else
      begin
        FEagleDirection := -1; // Left
        FEagleX := ScreenWidths[FormIndex] + 100; // Start off-screen right
      end;

      FEagleY := 100 + Random(ScreenHeights[FormIndex] div 2); // Random height
    end;
  end;

  Invalidate;
end;
procedure TScreenSaverForm.FadeInTimerTimer(Sender: TObject);
begin
  Inc(FFadeInOpacity, 5);
  if FFadeInOpacity >= 255 then
  begin
    FFadeInOpacity := 255;
    FFadeInTimer.Enabled := False;
  end;
  Invalidate;
end;

function TScreenSaverForm.RandomFireworkColor: TColor;
begin
  // Colors for fireworks (patriotic theme: red, white, blue)
  case Random(3) of
    0: Result := FLAG_RED;
    1: Result := FLAG_WHITE;
    2: Result := FLAG_BLUE;
    else Result := clWhite;
  end;
end;

function TScreenSaverForm.AdjustColorBrightness(AColor: TColor; Percent: Integer): TColor;
var
  R, G, B: Byte;
begin
  // Extract RGB components
  R := AColor and $FF;
  G := (AColor shr 8) and $FF;
  B := (AColor shr 16) and $FF;

  // Apply brightness adjustment
  R := Min(255, Max(0, (R * Percent) div 100));
  G := Min(255, Max(0, (G * Percent) div 100));
  B := Min(255, Max(0, (B * Percent) div 100));

  // Recombine into TColor
  Result := R or (G shl 8) or (B shl 16);
end;

procedure TScreenSaverForm.CreateBaseFlagImage;
var
  StripeHeight, StripeWidth, UnionHeight, UnionWidth: Integer;
  i, j, x, y, StarSize, StarSpacingX, StarSpacingY: Integer;
begin
  // Set the bitmap to the screen size
  FStaticFlagImage.Width := ScreenWidths[FormIndex];
  FStaticFlagImage.Height := ScreenHeights[FormIndex];

  // Calculate dimensions based on standard flag proportions
  StripeHeight := FStaticFlagImage.Height div 13;
  StripeWidth := FStaticFlagImage.Width;

  // The union (blue field) is 7 stripes tall and about 40% of the width
  // U.S. Flag Code specifies the union as 7/13 of the height and 2/5 of the width
  UnionHeight := StripeHeight * 7;
  UnionWidth := Round(FStaticFlagImage.Width * 0.4);

  // Draw the 13 alternating stripes (red and white)
  for i := 0 to 12 do
  begin
    if (i mod 2) = 0 then
      FStaticFlagImage.Canvas.Brush.Color := AdjustColorBrightness(FLAG_RED, FCurrentLightIntensity) // Red stripes
    else
      FStaticFlagImage.Canvas.Brush.Color := AdjustColorBrightness(FLAG_WHITE, FCurrentLightIntensity); // White stripes

    FStaticFlagImage.Canvas.FillRect(Rect(0, i * StripeHeight, StripeWidth, (i + 1) * StripeHeight));
  end;

  // Draw the blue union (canton)
  FStaticFlagImage.Canvas.Brush.Color := AdjustColorBrightness(FLAG_BLUE, FCurrentLightIntensity);
  FStaticFlagImage.Canvas.FillRect(Rect(0, 0, UnionWidth, UnionHeight));

  // Arrange 50 stars (5 rows of 6 and 4 rows of 5) evenly across the blue field
  StarSize := Trunc(UnionHeight / 15); // Smaller stars

  // Calculate spacing to evenly distribute stars across the full width and height
  // of the blue field with proper margins
  StarSpacingX := (UnionWidth - StarSize) div 6;  // 6 columns (with space on edges)
  StarSpacingY := (UnionHeight - StarSize) div 9; // 9 rows (with space on edges)

  // Initialize star twinkling arrays if not already done
  if Length(FTwinkleStars) = 0 then
  begin
    FStarCount := 50; // 50 stars
    SetLength(FTwinkleStars, FStarCount);
    SetLength(FStarBrightness, FStarCount);

    for i := 0 to FStarCount - 1 do
    begin
      FTwinkleStars[i] := False;
      FStarBrightness[i] := 100; // 100% brightness
    end;
  end;

  // First draw 5 rows of 6 stars (rows 0, 2, 4, 6, 8)
  for i := 0 to 4 do
  begin
    y := i * 2 * StarSpacingY + StarSpacingY div 2;
    for j := 0 to 5 do
    begin
      x := j * StarSpacingX + StarSpacingX div 2;

      // Apply twinkling effect to this star
      if FTwinkleStars[i * 6 + j] then
      begin
        // Create a copy of the star image with adjusted brightness
        StarImage.Assign(FBaseStarImage);
        // Process StarImage to change brightness here...
      end;

      FStaticFlagImage.Canvas.Draw(x, y, StarImage);
    end;
  end;

  // Then draw 4 rows of 5 stars (rows 1, 3, 5, 7)
  for i := 0 to 3 do
  begin
    y := (i * 2 + 1) * StarSpacingY + StarSpacingY div 2;
    for j := 0 to 4 do
    begin
      // Center these rows by adding extra offset
      x := j * StarSpacingX + StarSpacingX;

      // Apply twinkling effect to this star
      if FTwinkleStars[30 + i * 5 + j] then
      begin
        // Create a copy of the star image with adjusted brightness
        StarImage.Assign(FBaseStarImage);
        // Process StarImage to change brightness here...
      end;

      FStaticFlagImage.Canvas.Draw(x, y, StarImage);
    end;
  end;
end;

{procedure TScreenSaverForm.CreateFlagImage;
begin
  // Create the base flag image with proper lighting
  CreateBaseFlagImage;

  // If wave animation is active, create the waving effect
  if FWaveTimer.Enabled then
    CreateWavingFlagImage
  else
    FlagImage.Assign(FStaticFlagImage);
end;}


procedure TScreenSaverForm.CreateFlagImage;
begin
  // Create the base flag image with proper lighting
  CreateBaseFlagImage;

  // Simply copy the static flag image without any wave effect
  FlagImage.Assign(FStaticFlagImage);
end;

procedure TScreenSaverForm.CreateWavingFlagImage;
var
  i, j: Integer;
  SrcX, SrcY, DestX, DestY: Integer;
  WaveAmplitude, WavePeriod: Integer;
  WaveOffset: Integer;
  TempBitmap: TBitmap;
begin
  // Create a temporary bitmap the same size as the flag
  TempBitmap := TBitmap.Create;
  try
    TempBitmap.Width := FStaticFlagImage.Width;
    TempBitmap.Height := FStaticFlagImage.Height;

    // Copy the static flag to the temp bitmap
    TempBitmap.Canvas.Draw(0, 0, FStaticFlagImage);

    // Clear the flag image for the waving version
    FlagImage.Width := FStaticFlagImage.Width;
    FlagImage.Height := FStaticFlagImage.Height;
    FlagImage.Canvas.Brush.Color := clBlack;
    FlagImage.Canvas.FillRect(Rect(0, 0, FlagImage.Width, FlagImage.Height));

    // Define wave properties
    WaveAmplitude := FlagImage.Width div 40; // Adjust for desired wave size
    WavePeriod := FlagImage.Height div 2;

    // Apply the wave transformation
    for i := 0 to FlagImage.Height - 1 do
    begin
      // Calculate wave offset for this row
      // Use sine function to create a natural wave look
      // FWavePhase changes over time to animate the wave
      WaveOffset := Round(WaveAmplitude * Sin((i / WavePeriod) * 2 * Pi + FWavePhase) *
                          (i / FlagImage.Height)); // Increase wave effect toward right side

      for j := 0 to FlagImage.Width - 1 do
      begin
        // Calculate source and destination coordinates
        SrcX := j;
        SrcY := i;
        DestX := j + Round(WaveOffset * (j / FlagImage.Width)); // More wave on right
        DestY := i;

        // Check bounds
        if (DestX >= 0) and (DestX < FlagImage.Width) and
           (SrcX >= 0) and (SrcX < TempBitmap.Width) and
           (SrcY >= 0) and (SrcY < TempBitmap.Height) then
        begin
          // Copy the pixel with wave displacement
          FlagImage.Canvas.Pixels[DestX, DestY] := TempBitmap.Canvas.Pixels[SrcX, SrcY];
        end;
      end;
    end;
  finally
    TempBitmap.Free;
  end;
end;

procedure TScreenSaverForm.CreateStarImage;
var
  StarSize, radius, x, y: Integer;
  angle: Double;
  Points: array[0..9] of TPoint;
  i: Integer;
begin
  // Create a bitmap for the star
  StarSize := 20; // Adjust size as needed
  FBaseStarImage.Width := StarSize;
  FBaseStarImage.Height := StarSize;
  StarImage.Width := StarSize;
  StarImage.Height := StarSize;

  // Make the background transparent
  FBaseStarImage.Transparent := True;
  FBaseStarImage.TransparentColor := clBlack;
  FBaseStarImage.Canvas.Brush.Color := clBlack;
  FBaseStarImage.Canvas.FillRect(Rect(0, 0, StarSize, StarSize));

  StarImage.Transparent := True;
  StarImage.TransparentColor := clBlack;
  StarImage.Canvas.Brush.Color := clBlack;
  StarImage.Canvas.FillRect(Rect(0, 0, StarSize, StarSize));

  // Calculate the points of a five-pointed star
  radius := StarSize div 2;
  x := StarSize div 2;
  y := StarSize div 2;

  for i := 0 to 9 do
  begin
    if i mod 2 = 0 then // Outer points
      angle := (i * 36) * (Pi / 180)
    else // Inner points
      angle := (i * 36) * (Pi / 180);

    if i mod 2 = 0 then // Outer points of the star
      Points[i] := Point(
        x + Round(radius * cos(angle)),
        y - Round(radius * sin(angle))
      )
    else // Inner points of the star (smaller radius)
      Points[i] := Point(
        x + Round((radius / 2.5) * cos(angle)),
        y - Round((radius / 2.5) * sin(angle))
      );
  end;

  // Draw the star
  FBaseStarImage.Canvas.Brush.Color := FLAG_WHITE;
  FBaseStarImage.Canvas.Pen.Color := FLAG_WHITE;
  FBaseStarImage.Canvas.Polygon(Points);

  // Copy to the main star image
  StarImage.Canvas.Draw(0, 0, FBaseStarImage);
end;

procedure TScreenSaverForm.CreateSimpleEagleImage;
var
  EagleWidth, EagleHeight: Integer;
  BodyX, BodyY, WingSpan, HeadSize: Integer;
  WingPoints: array[0..7] of TPoint;
  HeadPoints: array[0..7] of TPoint;
  TailPoints: array[0..5] of TPoint;
begin
  // Create a more detailed eagle silhouette
  EagleWidth := 120;
  EagleHeight := 80;
  FEagleImage.Width := EagleWidth;
  FEagleImage.Height := EagleHeight;

  // Make the background transparent
  FEagleImage.Transparent := True;
  FEagleImage.TransparentColor := clBlack;
  FEagleImage.Canvas.Brush.Color := clBlack;
  FEagleImage.Canvas.FillRect(Rect(0, 0, EagleWidth, EagleHeight));

  // Set drawing properties
  FEagleImage.Canvas.Pen.Color := clWhite;
  FEagleImage.Canvas.Brush.Color := clWhite;

  // Define center point for the eagle
  BodyX := EagleWidth div 2;
  BodyY := EagleHeight div 2;

  // Draw the body - more sleek, less circular
  FEagleImage.Canvas.Pen.Width := 1;
  FEagleImage.Canvas.Ellipse(
    BodyX - 15, BodyY - 8,
    BodyX + 15, BodyY + 8
  );

  // Draw a more realistic head
  HeadPoints[0] := Point(BodyX + 15, BodyY - 5);
  HeadPoints[1] := Point(BodyX + 25, BodyY - 12);
  HeadPoints[2] := Point(BodyX + 27, BodyY - 10);
  HeadPoints[3] := Point(BodyX + 28, BodyY - 5);  // Beak tip
  HeadPoints[4] := Point(BodyX + 26, BodyY);
  HeadPoints[5] := Point(BodyX + 23, BodyY + 2);
  HeadPoints[6] := Point(BodyX + 18, BodyY + 3);
  HeadPoints[7] := Point(BodyX + 15, BodyY + 2);

  FEagleImage.Canvas.Polygon(HeadPoints);

  // Eye
  FEagleImage.Canvas.Pen.Color := clBlack;
  FEagleImage.Canvas.Brush.Color := clBlack;
  FEagleImage.Canvas.Ellipse(
    BodyX + 22, BodyY - 9,
    BodyX + 24, BodyY - 7
  );
  FEagleImage.Canvas.Pen.Color := clWhite;
  FEagleImage.Canvas.Brush.Color := clWhite;

  // Draw wings - more detailed
  // Left upper wing
  WingPoints[0] := Point(BodyX - 15, BodyY - 5);
  WingPoints[1] := Point(BodyX - 25, BodyY - 20);
  WingPoints[2] := Point(BodyX - 35, BodyY - 25);
  WingPoints[3] := Point(BodyX - 45, BodyY - 10);
  WingPoints[4] := Point(BodyX - 40, BodyY);
  WingPoints[5] := Point(BodyX - 30, BodyY);
  WingPoints[6] := Point(BodyX - 20, BodyY - 3);
  WingPoints[7] := Point(BodyX - 15, BodyY - 3);

  FEagleImage.Canvas.Polygon(WingPoints);

  // Right upper wing
  WingPoints[0] := Point(BodyX + 15, BodyY - 5);
  WingPoints[1] := Point(BodyX + 25, BodyY - 20);
  WingPoints[2] := Point(BodyX + 35, BodyY - 25);
  WingPoints[3] := Point(BodyX + 45, BodyY - 10);
  WingPoints[4] := Point(BodyX + 40, BodyY);
  WingPoints[5] := Point(BodyX + 30, BodyY);
  WingPoints[6] := Point(BodyX + 20, BodyY - 3);
  WingPoints[7] := Point(BodyX + 15, BodyY - 3);

  FEagleImage.Canvas.Polygon(WingPoints);

  // Left lower wing
  WingPoints[0] := Point(BodyX - 15, BodyY + 3);
  WingPoints[1] := Point(BodyX - 25, BodyY + 15);
  WingPoints[2] := Point(BodyX - 35, BodyY + 20);
  WingPoints[3] := Point(BodyX - 40, BodyY + 5);
  WingPoints[4] := Point(BodyX - 30, BodyY);
  WingPoints[5] := Point(BodyX - 20, BodyY + 3);
  WingPoints[6] := Point(BodyX - 15, BodyY + 5);
  WingPoints[7] := Point(BodyX - 15, BodyY + 3);

  FEagleImage.Canvas.Polygon(WingPoints);

  // Right lower wing
  WingPoints[0] := Point(BodyX + 15, BodyY + 3);
  WingPoints[1] := Point(BodyX + 25, BodyY + 15);
  WingPoints[2] := Point(BodyX + 35, BodyY + 20);
  WingPoints[3] := Point(BodyX + 40, BodyY + 5);
  WingPoints[4] := Point(BodyX + 30, BodyY);
  WingPoints[5] := Point(BodyX + 20, BodyY + 3);
  WingPoints[6] := Point(BodyX + 15, BodyY + 5);
  WingPoints[7] := Point(BodyX + 15, BodyY + 3);

  FEagleImage.Canvas.Polygon(WingPoints);

  // Draw a more detailed tail
  TailPoints[0] := Point(BodyX - 15, BodyY);
  TailPoints[1] := Point(BodyX - 30, BodyY + 15);
  TailPoints[2] := Point(BodyX - 25, BodyY + 20);
  TailPoints[3] := Point(BodyX, BodyY + 25);
  TailPoints[4] := Point(BodyX + 25, BodyY + 20);
  TailPoints[5] := Point(BodyX + 30, BodyY + 15);

  FEagleImage.Canvas.Polygon(TailPoints);

  // Add some feather details to tail
  FEagleImage.Canvas.MoveTo(BodyX - 25, BodyY + 20);
  FEagleImage.Canvas.LineTo(BodyX - 20, BodyY + 22);
  FEagleImage.Canvas.MoveTo(BodyX - 15, BodyY + 23);
  FEagleImage.Canvas.LineTo(BodyX - 5, BodyY + 24);
  FEagleImage.Canvas.MoveTo(BodyX + 5, BodyY + 24);
  FEagleImage.Canvas.LineTo(BodyX + 15, BodyY + 23);
  FEagleImage.Canvas.MoveTo(BodyX + 20, BodyY + 22);
  FEagleImage.Canvas.LineTo(BodyX + 25, BodyY + 20);
end;

procedure TScreenSaverForm.DrawEagle(ACanvas: TCanvas);
var
  FrameWidth: Integer;
  ScaleFactor: Double;
  DestRect: TRect;
begin
  exit;

  if not FEagleVisible then Exit;

  // Calculate the current frame
  FrameWidth := FEagleImage.Width;

  // Scale the eagle image for display
  ScaleFactor := 2.0 + 0.2 * Sin(FEagleFrame / 4); // Slight size oscillation for wing flapping effect

  // Define destination rectangle with scaling
  DestRect.Left := FEagleX;
  DestRect.Top := FEagleY;
  DestRect.Right := FEagleX + Round(FEagleImage.Width * ScaleFactor);
  DestRect.Bottom := FEagleY + Round(FEagleImage.Height * ScaleFactor);

  // Draw the eagle with proper direction
  if FEagleDirection = 1 then
    // Draw normally for right movement
    ACanvas.StretchDraw(DestRect, FEagleImage)
  else
  begin
    // Flip horizontally for left movement
    // Note: Proper flipping would require more complex code
    // This is a simplified approach
    ACanvas.CopyRect(
      DestRect,
      FEagleImage.Canvas,
      Rect(FEagleImage.Width, 0, 0, FEagleImage.Height)
    );
  end;
end;

procedure TScreenSaverForm.DrawFireworks(ACanvas: TCanvas);
var
  i: Integer;
  Firework: TFirework;
begin
  if not FFireworksActive then Exit;

  // Draw all active fireworks
  for i := 0 to FFireworksList.Count - 1 do
  begin
    Firework := TFirework(FFireworksList[i]);
    Firework.Draw(ACanvas);
  end;
end;

procedure TScreenSaverForm.DrawSystemGauges(ACanvas: TCanvas; X, Y: Integer);
var
  GaugeWidth, GaugeHeight, Spacing: Integer;
  FilledWidth: Integer;
  GaugeRect, FilledRect: TRect;
  MemPercent: Integer;
  DiskPercent: Integer;
  TextColor: TColor;
begin
  GaugeWidth := 150;
  GaugeHeight := 20;
  Spacing := 30;
  TextColor := clWhite;

  // Calculate percentages
  MemPercent := MemoryUtilization;
  if FTotalPhysicalMemory > 0 then
    DiskPercent := (FUsedDiskSpace * 100) div (FUsedDiskSpace + DiskSpace)
  else
    DiskPercent := 0;

  // Draw background for all gauges
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clBlack;
  ACanvas.FillRect(Rect(X, Y, X + GaugeWidth + 200, Y + 3 * (GaugeHeight + Spacing)));

  // Title
  ACanvas.Font.Style := [fsBold];
  ACanvas.Font.Color := TextColor;
  ACanvas.TextOut(X, Y, 'System Status');
  Y := Y + 25;
  Canvas.Font.Style := [];

  // CPU Gauge
  ACanvas.TextOut(X, Y, 'CPU:');
  GaugeRect := Rect(X + 50, Y, X + 50 + GaugeWidth, Y + GaugeHeight);
  ACanvas.Brush.Color := $444444; // Dark gray
  ACanvas.FillRect(GaugeRect);

  FilledWidth := (CpuUtilization * GaugeWidth) div 100;
  FilledRect := Rect(X + 50, Y, X + 50 + FilledWidth, Y + GaugeHeight);

  // Color changes based on utilization
  if CpuUtilization < 60 then
    ACanvas.Brush.Color := clGreen
  else if CpuUtilization < 85 then
    ACanvas.Brush.Color := clYellow
  else
    ACanvas.Brush.Color := clRed;

  ACanvas.FillRect(FilledRect);

  // Draw border and text
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := TextColor;
  ACanvas.Rectangle(GaugeRect);
  ACanvas.TextOut(X + GaugeWidth + 60, Y, IntToStr(CpuUtilization) + '%');

  // Memory Gauge
  Y := Y + GaugeHeight + Spacing;
  ACanvas.TextOut(X, Y, 'MEM:');
  GaugeRect := Rect(X + 50, Y, X + 50 + GaugeWidth, Y + GaugeHeight);
  ACanvas.Brush.Color := $444444; // Dark gray
  ACanvas.FillRect(GaugeRect);

  FilledWidth := (MemPercent * GaugeWidth) div 100;
  FilledRect := Rect(X + 50, Y, X + 50 + FilledWidth, Y + GaugeHeight);

  // Color changes based on utilization
  if MemPercent < 60 then
    ACanvas.Brush.Color := clGreen
  else if MemPercent < 85 then
    ACanvas.Brush.Color := clYellow
  else
    ACanvas.Brush.Color := clRed;

  ACanvas.FillRect(FilledRect);

  // Draw border and text
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := TextColor;
  ACanvas.Rectangle(GaugeRect);
  ACanvas.TextOut(X + GaugeWidth + 60, Y, IntToStr(MemPercent) + '%');

  // Disk Space Gauge
  Y := Y + GaugeHeight + Spacing;
  ACanvas.TextOut(X, Y, 'DISK:');
  GaugeRect := Rect(X + 50, Y, X + 50 + GaugeWidth, Y + GaugeHeight);
  ACanvas.Brush.Color := $444444; // Dark gray
  ACanvas.FillRect(GaugeRect);

  FilledWidth := (DiskPercent * GaugeWidth) div 100;
  FilledRect := Rect(X + 50, Y, X + 50 + FilledWidth, Y + GaugeHeight);

  // Color changes based on utilization
  if DiskPercent < 60 then
    ACanvas.Brush.Color := clGreen
  else if DiskPercent < 85 then
    ACanvas.Brush.Color := clYellow
  else
    ACanvas.Brush.Color := clRed;

  ACanvas.FillRect(FilledRect);

  // Draw border and text
  ACanvas.Brush.Style := bsClear;
  ACanvas.Pen.Color := TextColor;
  ACanvas.Rectangle(GaugeRect);
  ACanvas.TextOut(X + GaugeWidth + 60, Y, IntToStr(DiskPercent) + '%');

  // Show additional info
  Y := Y + GaugeHeight + Spacing;
  ACanvas.TextOut(X, Y, Format('Ping: %d ms', [FPingTime]));
  Y := Y + 20;
  ACanvas.TextOut(X, Y, Format('Uptime: %s', [FormatUptime(FSystemUptime)]));

  // Show weather if available
  {if FWeatherCondition <> 'Unknown' then
  begin
    Y := Y + 20;
    ACanvas.TextOut(X, Y, Format('Weather: %s, %d°C', [FWeatherCondition, FWeatherTemp]));
  end;}
end;

procedure TScreenSaverForm.GetSystemInfo;
var
  MemoryStatus: TMemoryStatus;
  FreeBytesAvailable, TotalNumberOfBytes, TotalNumberOfFreeBytes: Int64;
  CpuUsage: Integer;
  PingTime: Integer;
  TotalPhysicalMemory, AvailablePhysicalMemory: Int64;
  UsedDiskSpace: Int64;
  SystemUptime: Int64;
begin
  // Initialize local variables
  FreeBytesAvailable := 0;
  TotalNumberOfBytes := 0;
  TotalNumberOfFreeBytes := 0;

  // Get CPU utilization
  CpuUsage := GetCPUUsage;
  CpuUtilization := CpuUsage;

  // Get memory utilization
  MemoryStatus.dwLength := SizeOf(MemoryStatus);
  GlobalMemoryStatus(MemoryStatus);
  MemoryUtilization := MemoryStatus.dwMemoryLoad;
  TotalPhysicalMemory := MemoryStatus.dwTotalPhys div (1024 * 1024); // Convert to MB
  AvailablePhysicalMemory := MemoryStatus.dwAvailPhys div (1024 * 1024); // Convert to MB

  // Only check disk space periodically
 Inc(FDiskCheckCounter);
if FDiskCheckCounter >= 6 then // Check every minute (6 × 10 seconds)
begin
  if GetDiskFreeSpaceEx('C:\', FreeBytesAvailable, TotalNumberOfBytes, @TotalNumberOfFreeBytes) then
  begin
    DiskSpace := TotalNumberOfFreeBytes div (1024 * 1024 * 1024); // Convert to GB
    UsedDiskSpace := (TotalNumberOfBytes - TotalNumberOfFreeBytes) div (1024 * 1024 * 1024); // Convert to GB

    // Store these values in class variables
    FUsedDiskSpace := UsedDiskSpace;
    FTotalDiskSpace := FUsedDiskSpace + DiskSpace;
  end
  else
  begin
    DiskSpace := 1; // Set to 1 to avoid division by zero
    FUsedDiskSpace := 0;
    FTotalDiskSpace := 1;
  end;
  FDiskCheckCounter := 0;
end;


  // Get ping time
  PingTime := GetPingTime('8.8.8.8');

  // Get system uptime
  SystemUptime := GetTickCount64 div 1000; // Convert to seconds

  // Store the values in class variables for later display
  FTotalPhysicalMemory := TotalPhysicalMemory;
  FAvailablePhysicalMemory := AvailablePhysicalMemory;
  //FUsedDiskSpace := UsedDiskSpace;
  FPingTime := PingTime;
  FSystemUptime := SystemUptime;

  // Add formatted date/time without seconds
  FFormattedDateTime := FormatDateTime('yyyy-mm-dd hh:nn', Now);
end;
function TScreenSaverForm.GetCPUUsage: Integer;
var
  IdleTime, KernelTime, UserTime: Int64;
  SystemTime, Usage: Int64;
begin
  Result := 0;  // Default result

  // Ensure PrevIdleTime, PrevKernelTime, and PrevUserTime are initialized
  if (PrevIdleTime = 0) and (PrevKernelTime = 0) and (PrevUserTime = 0) then
  begin
    if RetrieveSystemTimes(IdleTime, KernelTime, UserTime) then
    begin
      PrevIdleTime := IdleTime;
      PrevKernelTime := KernelTime;
      PrevUserTime := UserTime;
    end;
    Exit;  // Exit the function on first call
  end;

  // Check if RetrieveSystemTimes is successful
  if RetrieveSystemTimes(IdleTime, KernelTime, UserTime) then
  begin
    // Calculate the total system time since the last call
    SystemTime := (KernelTime + UserTime) - (PrevKernelTime + PrevUserTime);

    // Avoid division by zero
    if SystemTime > 0 then
    begin
      // Calculate CPU usage
      Usage := 100 - ((IdleTime - PrevIdleTime) * 100 div SystemTime);

      // Validate the result
      if (Usage >= 0) and (Usage <= 100) then
      begin
        Result := Usage;
      end;
    end;

    // Update previous times for next calculation
    PrevIdleTime := IdleTime;
    PrevKernelTime := KernelTime;
    PrevUserTime := UserTime;
  end;
end;

function TScreenSaverForm.GetPingTime(const IPAddress: string): Integer;
var
  IcmpHandle: THandle;
  ReplyBuffer: array[0..255] of Byte; // Increased buffer size for safety
  EchoReply: PICMP_ECHO_REPLY;
  IpAddr: DWORD;
  ReplyCount: DWORD;
begin
  Result := -1;  // Default result if ping fails
  IcmpHandle := IcmpCreateFile;
  if IcmpHandle = INVALID_HANDLE_VALUE then
    Exit;

  try
    IpAddr := inet_addr(PAnsiChar(AnsiString(IPAddress)));
    if IpAddr = INADDR_NONE then
      Exit;

    // Ensure the reply buffer is correctly set up
    FillChar(ReplyBuffer, SizeOf(ReplyBuffer), 0);
    EchoReply := PICMP_ECHO_REPLY(@ReplyBuffer);
    EchoReply^.DataSize := SizeOf(ReplyBuffer) - SizeOf(ICMP_ECHO_REPLY);
    EchoReply^.Data := @ReplyBuffer[SizeOf(ICMP_ECHO_REPLY)];

    ReplyCount := IcmpSendEcho(IcmpHandle, IpAddr, nil, 0, nil, @ReplyBuffer, SizeOf(ReplyBuffer), 1000);
    if ReplyCount = 0 then
      Exit;

    if EchoReply^.Status = IP_SUCCESS then
      Result := EchoReply^.RoundTripTime;
  finally
    IcmpCloseHandle(IcmpHandle);
  end;
end;

function TScreenSaverForm.RetrieveSystemTimes(var IdleTime, KernelTime, UserTime: Int64): Boolean;
var
  IdleTimeFileTime, KernelTimeFileTime, UserTimeFileTime: TFileTime;
begin
  Result := False;
  IdleTime := 0;
  KernelTime := 0;
  UserTime := 0;
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if GetKernelSystemTimes(IdleTimeFileTime, KernelTimeFileTime, UserTimeFileTime) then
    begin
      IdleTime := Int64(IdleTimeFileTime.dwLowDateTime) or (Int64(IdleTimeFileTime.dwHighDateTime) shl 32);
      KernelTime := Int64(KernelTimeFileTime.dwLowDateTime) or (Int64(KernelTimeFileTime.dwHighDateTime) shl 32);
      UserTime := Int64(UserTimeFileTime.dwLowDateTime) or (Int64(UserTimeFileTime.dwHighDateTime) shl 32);
      Result := True;
    end;
  end;
end;

procedure TScreenSaverForm.InitializeQuotes;
begin
  FQuotes.Clear;
  FQuotes.Add('Give me liberty, or give me death! - Patrick Henry');
  FQuotes.Add('I only regret that I have but one life to lose for my country. - Nathan Hale');
  FQuotes.Add('Ask not what your country can do for you, ask what you can do for your country. - John F. Kennedy');
  FQuotes.Add('The only thing we have to fear is fear itself. - Franklin D. Roosevelt');
  FQuotes.Add('Government of the people, by the people, for the people. - Abraham Lincoln');
  FQuotes.Add('We hold these truths to be self-evident, that all men are created equal. - Thomas Jefferson');
  FQuotes.Add('America was not built on fear. America was built on courage, imagination, and determination. - Harry S. Truman');
  FQuotes.Add('Freedom is one of the deepest and noblest aspirations of the human spirit. - Ronald Reagan');
  FQuotes.Add('The strength of a nation lies in the homes of its people. - Abraham Lincoln');
  FQuotes.Add('We must be the great arsenal of democracy. - Franklin D. Roosevelt');
  // Additional quotes
  FQuotes.Add('Where liberty dwells, there is my country. - Benjamin Franklin');
  FQuotes.Add('America is another name for opportunity. - Ralph Waldo Emerson');
  FQuotes.Add('This nation will remain the land of the free only so long as it is the home of the brave. - Elmer Davis');
  FQuotes.Add('May we think of freedom not as the right to do as we please, but as the opportunity to do what is right. - Peter Marshall');
  FQuotes.Add('America is a tune. It must be sung together. - Gerald Stanley Lee');
  FQuotes.Add('Patriotism is supporting your country all the time, and your government when it deserves it. - Mark Twain');
  FQuotes.Add('Freedom is nothing but a chance to be better. - Albert Camus');
  FQuotes.Add('I love America more than any other country in the world, and exactly for this reason, I insist on the right to criticize her perpetually. - James Baldwin');
  FQuotes.Add('My fellow Americans, ask not what your country can do for you, ask what you can do for your country. - John F. Kennedy');
  FQuotes.Add('The American, by nature, is optimistic. He is experimental, an inventor, and a builder who builds best when called upon to build greatly. - John F. Kennedy');
  FQuotes.Add('In the face of impossible odds, people who love this country can change it. - Barack Obama');
  FQuotes.Add('America was not built on fear. America was built on courage, on imagination and an unbeatable determination to do the job at hand. - Harry S. Truman');
  FQuotes.Add('The greatness of America lies not in being more enlightened than any other nation, but rather in her ability to repair her faults. - Alexis de Tocqueville');
  FQuotes.Add('A nation''s culture resides in the hearts and in the soul of its people. - Mahatma Gandhi');
  FQuotes.Add('America is hope. It is compassion. It is excellence. It is valor. - Paul Tsongas');
  FQuotes.Add('I like to see a man proud of the place in which he lives. I like to see a man live so that his place will be proud of him. - Abraham Lincoln');
  FQuotes.Add('America is a land of wonders, in which everything is in constant motion and every change seems an improvement. - Alexis de Tocqueville');
  FQuotes.Add('We on this continent should never forget that men first crossed the Atlantic not to find soil for their ploughs but to secure liberty for their souls. - Robert J. McCracken');
  FQuotes.Add('America is too great for small dreams. - Ronald Reagan');
  FQuotes.Add('The essence of America - that which really unites us - is not ethnicity, or nationality, or religion. It is an idea - and what an idea it is: that you can come from humble circumstances and do great things. - Condoleezza Rice');



  FCurrentQuote := 0;
end;

procedure TScreenSaverForm.CheckForHolidays;
var
  CurrentDate: TDateTime;
  Year, Month, Day: Word;
  MemorialDay: TDateTime;
begin
  CurrentDate := Date;
  DecodeDate(CurrentDate, Year, Month, Day);

  FIsHoliday := False;
  FHolidayName := '';

  // Check for Independence Day (July 4)
  if (Month = 7) and (Day = 4) then
  begin
    FIsHoliday := True;
    FHolidayName := 'Independence Day';
    FFireworksActive := True;
  end

  // Check for Memorial Day (last Monday in May)
  else if Month = 5 then
  begin
    // Calculate Memorial Day for current year
    MemorialDay := EncodeDate(Year, 5, 1);
    while DayOfWeek(MemorialDay) <> 2 do  // Find first Monday
      MemorialDay := MemorialDay + 1;
    MemorialDay := MemorialDay + 21;      // Last Monday is 3 weeks later or 4th Monday

    if Trunc(CurrentDate) = Trunc(MemorialDay) then
    begin
      FIsHoliday := True;
      FHolidayName := 'Memorial Day';
    end;
  end

  // Veterans Day (November 11)
  else if (Month = 11) and (Day = 11) then
  begin
    FIsHoliday := True;
    FHolidayName := 'Veterans Day';
  end

  // Presidents Day (third Monday in February)
  else if Month = 2 then
  begin
    // Similar calculation for Presidents Day
    // ...
  end

  // Labor Day (first Monday in September)
  else if Month = 9 then
  begin
    // Similar calculation for Labor Day
    // ...
  end;

  // If it's a holiday, enable fireworks
  if FIsHoliday then
    FFireworkTimer.Enabled := false;
end;

procedure TScreenSaverForm.UpdateFireworks;
var
  i: Integer;
  Firework: TFirework;
begin
  if not FFireworksActive then Exit;

  // Randomly create new fireworks
  if Random(100) < 5 then // 5% chance each update
  begin
    Firework := TFirework.Create;
    Firework.X := Random(ScreenWidths[FormIndex]);
    Firework.Y := ScreenHeights[FormIndex];
    Firework.VelX := -2 + Random(4);
    Firework.VelY := -5 - Random(10); // Initial upward velocity
    Firework.Color := RandomFireworkColor;
    Firework.ExplosionTime := 30 + Random(50); // Frames until explosion
    FFireworksList.Add(Firework);
  end;

  // Update existing fireworks
  for i := FFireworksList.Count - 1 downto 0 do
  begin
    Firework := TFirework(FFireworksList[i]);
    Firework.Update;

    // Remove dead fireworks
    if Firework.IsDead then
    begin
      Firework.Free;
      FFireworksList.Delete(i);
    end;
  end;
end;

procedure TScreenSaverForm.UpdateWeatherInfo;
begin
  // Simulate weather data (in a real implementation, you would connect to a weather API)
  FWeatherTemp := 10 + Random(25); // Random temperature between 10-35°C

  case Random(5) of
    0: FWeatherCondition := 'Sunny';
    1: FWeatherCondition := 'Partly Cloudy';
    2: FWeatherCondition := 'Cloudy';
    3: FWeatherCondition := 'Rainy';
    4: FWeatherCondition := 'Snowy';
  end;

  FWeatherLastUpdate := Now;
  Invalidate;
end;

procedure TScreenSaverForm.UpdateLighting;
var
  CurrentHour, CurrentMinute, CurrentSecond, Milliseconds: Word;
  TimeOfDay: Integer; // Minutes since midnight
  IsDaytime: Boolean;
  TransitionPercentage: Integer;
begin
  // Get current time
  DecodeTime(Time, CurrentHour, CurrentMinute, CurrentSecond, Milliseconds);
  TimeOfDay := CurrentHour * 60 + CurrentMinute;

  // Define daytime as 6:00 AM to 6:00 PM (360 - 1080 minutes)
  IsDaytime := (TimeOfDay >= 360) and (TimeOfDay <= 1080);

  // Set base light intensity
  if IsDaytime then
    FCurrentLightIntensity := 100 // Full brightness
  else
    FCurrentLightIntensity := 60;  // Dimmer at night

  // Handle dawn/dusk transitions (30 minutes)
  if (TimeOfDay >= 330) and (TimeOfDay < 360) then // Dawn
  begin
    // Transition from night to day
    TransitionPercentage := ((TimeOfDay - 330) * 100) div 30;
    FCurrentLightIntensity := 60 + (40 * TransitionPercentage) div 100;
  end
  else if (TimeOfDay >= 1080) and (TimeOfDay < 1110) then // Dusk
  begin
    // Transition from day to night
    TransitionPercentage := ((TimeOfDay - 1080) * 100) div 30;
    FCurrentLightIntensity := 100 - (40 * TransitionPercentage) div 100;
  end;

  // Apply lighting effect when drawing the flag
  CreateFlagImage;
  Invalidate;
end;

procedure TScreenSaverForm.InitializeEagle;
begin
  if not Assigned(FEagleImage) then
    FEagleImage := TBitmap.Create;

  CreateSimpleEagleImage;

  FEagleVisible := False;
  FEagleFrame := 0;
  FEagleFrameCount := 8; // Number of animation frames
  FEagleChance := 200; // 1 in 1000 chance per check

  FEagleTimer.Enabled := True;
end;

procedure TScreenSaverForm.FormCreate(Sender: TObject);
var
  WSAData: TWSAData;
  MousePos: TPoint;
  ParentWnd: HWND;
begin

   DoubleBuffered := True;

  FlagOffsetX := 0;
  FlagOffsetY := 0;


  // Initialize previous times for CPU usage calculation
  PrevIdleTime := 0;
  PrevKernelTime := 0;
  PrevUserTime := 0;

  // Initialize Winsock
  if WSAStartup(MAKEWORD(2, 2), WSAData) <> 0 then
  begin
    Application.Terminate;
    Exit;
  end;

  // Retrieve the initial system times
  if not RetrieveSystemTimes(PrevIdleTime, PrevKernelTime, PrevUserTime) then
  begin
    Application.Terminate;
    Exit;
  end;

  SetThreadExecutionState(ES_CONTINUOUS or ES_SYSTEM_REQUIRED or ES_DISPLAY_REQUIRED);

  // Request higher timer resolution
  timeBeginPeriod(1);

  // Short delay to let system reach full power
  Sleep(200);


  InitialMonitorCount := Screen.MonitorCount;
  MonitorCountTimer := TTimer.Create(Self);
  MonitorCountTimer.Interval := 1000; // Check every 1 second
  MonitorCountTimer.OnTimer := @MonitorCountTimerTimer;
  MonitorCountTimer.Enabled := True;

  // Initialize quotes
  InitializeQuotes;

  // Check for holidays
 CheckForHolidays;

  // Initialize weather information
  UpdateWeatherInfo;

  // Initialize lighting based on time of day
  UpdateLighting;

  // Initialize eagle animation
  InitializeEagle;

  // Create the American flag image
  CreateFlagImage;

  // Check for /s parameter to start the screensaver
  if (ParamCount > 0) and (ParamStr(1) = '/s') then
  begin
    UniqueInstance1.Enabled := true;
    // Hide the mouse cursor
    HideMouseCursor;

    // Load Hebrew-supporting font
    Font.Charset := HEBREW_CHARSET;
    Font.Name := 'Tahoma'; // Tahoma supports Hebrew characters
    Font.Size := 24; // Set a reasonable font size

    // Initialize text streams
    Randomize;
    HebrewText := WideString(#$05D9#$05E9#$05D5#$05E2); // Unicode values for י, ש, ו, ע
    InitializeTextStreams;
    Timer2.Enabled := True;
    Timer2.Interval := 10000;
    Timer2Timer(nil);

    // Set up timer and form properties
    Timer1.Interval := 50;
    Timer1.Enabled := True;
    ControlStyle := ControlStyle + [csOpaque]; // For flicker reduction

    // Get the initial mouse position
    GetCursorPos(MousePos);
    InitialMouseX := MousePos.X;
    InitialMouseY := MousePos.Y;

    // Enable all enhancement timers
    FWaveTimer.Enabled := false;
    FTwinkleTimer.Enabled := false;
    FQuoteTimer.Enabled := True;
    if FIsHoliday then
      FFireworkTimer.Enabled := false;
    FWeatherUpdateTimer.Enabled := false;
    FLightingTimer.Enabled := false;
    FEagleTimer.Enabled := false;
    FFadeInTimer.Enabled := True;

    // Initial paint (to display the initial text)
    Invalidate;
    BringToFront; // Ensure form is on top

    // Set global hooks
    hMouseHook := SetWindowsHookEx(WH_MOUSE_LL, @MouseHookProc, HInstance, 0);
    hKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @KeyboardHookProc, HInstance, 0);

    // Ensure the form is on top of everything
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    SetForegroundWindow(Handle);
  end
  else if (ParamCount > 1) and (ParamStr(1) = '/p') then
  begin
    UniqueInstance1.Enabled := false;
    // Preview mode using specified window handle
    ParentWnd := StrToInt(ParamStr(2));
    windows.SetParent(Handle, ParentWnd);

    // Adjust form style for preview
    BorderStyle := bsNone;
    Align := alClient;

    // Initialize text streams
    Randomize;
    HebrewText := WideString(#$05D9#$05E9#$05D5#$05E2); // Unicode values for י, ש, ו, ע
    InitializeTextStreams;

    // Set up timer and form properties
    Timer2.Interval := 30000;
    Timer2.Enabled := True;
    Timer1.Interval := 30;
    Timer1.Enabled := True;
    ControlStyle := ControlStyle + [csOpaque]; // For flicker reduction

    // Initial paint (to display the initial text)
    Invalidate;
    BringToFront; // Ensure form is on top

    // Check for /p parameter and set up termination timer
    SetupTerminationTimer;
  end
  else if (ParamCount > 0) and (ParamStr(1) = '/c') then
  begin
    UniqueInstance1.Enabled := false;
    // Just exit when called with /c parameter - no settings form
    Application.Terminate;
  end
  else
  begin
    Application.Terminate;
  end;
end;

procedure TScreenSaverForm.MonitorCountTimerTimer(Sender: TObject);
var
  I: Integer;
  DisplayDevice: TDisplayDevice;
  ActiveMonitorCount: Integer;
begin
  // Count the number of active monitors
  ActiveMonitorCount := 0;
  ZeroMemory(@DisplayDevice, SizeOf(DisplayDevice));
  DisplayDevice.cb := SizeOf(DisplayDevice);
  I := 0;
  while EnumDisplayDevices(nil, I, @DisplayDevice, 0) do
  begin
    if (DisplayDevice.StateFlags and DISPLAY_DEVICE_ACTIVE <> 0) and
       (DisplayDevice.StateFlags and DISPLAY_DEVICE_MIRRORING_DRIVER = 0) then
    begin
      Inc(ActiveMonitorCount);
    end;
    Inc(I);
  end;

  if ActiveMonitorCount <> InitialMonitorCount then
  begin
    OutputDebugString('Active monitor count changed. Terminating screen saver.');
    Application.Terminate;
  end;
end;

procedure TScreenSaverForm.SetupTerminationTimer;
begin
  TerminationTimer := TTimer.Create(Self);
  TerminationTimer.Interval := 10000; // 10 seconds
  TerminationTimer.OnTimer := @TerminationTimerTimer;
  TerminationTimer.Enabled := True;
end;

procedure TScreenSaverForm.TerminationTimerTimer(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TScreenSaverForm.FormDestroy(Sender: TObject);
begin
  if hMouseHook <> 0 then
    UnhookWindowsHookEx(hMouseHook);
  if hKeyboardHook <> 0 then
    UnhookWindowsHookEx(hKeyboardHook);

  // Clean up Winsock
  WSACleanup;
  timeEndPeriod(1);
end;

procedure TScreenSaverForm.Timer2Timer(Sender: TObject);
begin
  TSystemInfoThread.Create(Self).Start;
end;

procedure TScreenSaverForm.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
begin
  if (ParamCount > 0) and (ParamStr(1) = '/s') then
  begin
    Application.Terminate;
  end;
end;

procedure TScreenSaverForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  Application.Terminate;
end;

procedure TScreenSaverForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.Terminate;
end;

procedure TScreenSaverForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  CheckMouseMovement(X, Y);
end;

procedure TScreenSaverForm.CheckMouseMovement(X, Y: Integer);
var
  CurrentMousePos: TPoint;
begin
  GetCursorPos(CurrentMousePos);
  if (Abs(CurrentMousePos.X - InitialMouseX) > MouseSensitivity) or
     (Abs(CurrentMousePos.Y - InitialMouseY) > MouseSensitivity) then
  begin
    if not MouseMoved then
    begin
      MouseMoved := True;
      Application.Terminate;
    end;
  end;
end;

procedure TScreenSaverForm.HideMouseCursor;
begin
  ShowCursor(False);
end;

procedure TScreenSaverForm.InitializeTextStreams;
var
  i, j: Integer;
  Stripe: Integer;
begin
  SetLength(TextStreams, NumStreams);
  for i := 0 to High(TextStreams) do
  begin
    SetLength(TextStreams[i], Length(HebrewText));
    for j := 0 to High(TextStreams[i]) do
    begin
      TextStreams[i][j].Char := HebrewText[j + 1];
      // Adjust X position to ensure characters are fully visible
      TextStreams[i][j].X := Random(ScreenWidths[FormIndex] - Canvas.TextWidth(HebrewText[j + 1]));
      TextStreams[i][j].Y := -Random(ScreenHeights[FormIndex]); // Random Y position above the screen
      TextStreams[i][j].FallingSpeed := BaseFallSpeed + Random(3);

      // Determine which stripe the text is falling on and adjust color
      Stripe := (TextStreams[i][j].Y div (ScreenHeights[FormIndex] div 13)) mod 13;

      // For text over the flag union (blue field)
      if (TextStreams[i][j].X < Round(ScreenWidths[FormIndex] * 0.4)) and
         (TextStreams[i][j].Y < (ScreenHeights[FormIndex] div 13) * 7) then
        TextStreams[i][j].TextColor := FLAG_WHITE
      else if Stripe mod 2 = 0 then
        TextStreams[i][j].TextColor := FLAG_WHITE // White text on red stripes
      else
        TextStreams[i][j].TextColor := FLAG_BLUE; // Blue text on white stripes
    end;
  end;
end;

procedure TScreenSaverForm.UpdateTextPositions;
var
  i, j: Integer;
  Stripe: Integer;
  ScreenHeight, ScreenWidth: Integer;
  StripeHeight: Integer;
  BlueFieldWidth, BlueFieldHeight: Integer;
begin
  ScreenHeight := ScreenHeights[FormIndex];
  ScreenWidth := ScreenWidths[FormIndex];
  StripeHeight := ScreenHeight div 13;
  BlueFieldWidth := Round(ScreenWidth * 0.4);
  BlueFieldHeight := StripeHeight * 7;

  for i := 0 to High(TextStreams) do
  begin
    for j := 0 to High(TextStreams[i]) do
    begin
      // Update the Y position by adding the falling speed
      TextStreams[i][j].Y := TextStreams[i][j].Y + TextStreams[i][j].FallingSpeed;

      // Determine which stripe the text is on and adjust color
      Stripe := (TextStreams[i][j].Y div StripeHeight) mod 13;

      // For text over the flag union (blue field)
      if (TextStreams[i][j].X < BlueFieldWidth) and
         (TextStreams[i][j].Y < BlueFieldHeight) then
        TextStreams[i][j].TextColor := FLAG_WHITE
      else if Stripe mod 2 = 0 then
        TextStreams[i][j].TextColor := FLAG_WHITE // White text on red stripes
      else
        TextStreams[i][j].TextColor := FLAG_BLUE; // Blue text on white stripes

      // If the character goes below the screen height, reset its position
      if TextStreams[i][j].Y > ScreenHeight then
      begin
        // Set a new random X position within the screen width, accounting for character width
        TextStreams[i][j].X := Random(ScreenWidth - Canvas.TextWidth(TextStreams[i][j].Char));
        // Reset the Y position to start above the screen
        TextStreams[i][j].Y := -Random(100); // Reset Y position above the screen by up to 100 pixels
      end;
    end;
  end;

  // Always invalidate to ensure screen updates
  Invalidate;
end;
procedure TScreenSaverForm.Timer1Timer(Sender: TObject);
begin
  UpdateTextPositions;
  Invalidate;
end;

procedure TScreenSaverForm.UpdateUI;
var
  MaxInfoX, MaxInfoY: Integer;
  BoxWidth, BoxHeight: Integer;
  QuoteSpace: Integer;
  QuoteHeight: Integer;
  CurrentQuote: string;
  QuoteTextWidth, QuoteMaxWidth: Integer;
  LinesNeeded: Integer;
  InfoTextLines: Integer;
  InfoHeight, QuoteFontHeight: Integer;
  GaugeWidth: Integer;
begin
  Canvas.Font.Size := 24;

  // Calculate box dimensions as before
  if FUseGraphicalIndicators then
  begin
    BoxWidth := 150 + 200 + 40;
    BoxHeight := 3 * (20 + 30) + 25 + 100;
  end
  else
  begin
    InfoTextLines := 12;
    InfoHeight := Canvas.TextHeight('W');
    GaugeWidth := 100;
    BoxWidth := 400 + GaugeWidth;
    BoxHeight := InfoTextLines * (InfoHeight + 5) + 40;
  end;

  // Intelligently calculate space needed for the quote
  if (FQuotes.Count > 0) and (FCurrentQuote < FQuotes.Count) then
  begin
    // Set quote font properties to match what's used in FormPaint
    Canvas.Font.Size := Round(ScreenHeights[FormIndex] * 0.03);
    Canvas.Font.Style := [fsBold, fsItalic];
    QuoteFontHeight := Canvas.TextHeight('W');

    CurrentQuote := FQuotes[FCurrentQuote];
    QuoteTextWidth := Canvas.TextWidth(CurrentQuote);
    QuoteMaxWidth := Round(ScreenWidths[FormIndex] * 0.8);

    // Calculate how many lines the quote will need
    if QuoteTextWidth <= QuoteMaxWidth then
      LinesNeeded := 1
    else
    begin
      // Estimate number of lines (rough calculation)
      LinesNeeded := Ceil(QuoteTextWidth / QuoteMaxWidth);
      // Add a safety margin
      LinesNeeded := LinesNeeded + 1;
    end;

    // Calculate total height needed for quote
    QuoteHeight := LinesNeeded * (QuoteFontHeight + 5) + 20; // text height + line spacing + margin

    // Total space to reserve at bottom
    QuoteSpace := QuoteHeight + 50; // quote height + padding
  end
  else
    QuoteSpace := 70; // Default space if no quote

  // Calculate maximum positions, keeping status box away from the quote
  MaxInfoX := ScreenWidths[FormIndex] - BoxWidth - 20;
  MaxInfoY := ScreenHeights[FormIndex] - BoxHeight - 20 - QuoteSpace;

  // Ensure there's some margin
  if MaxInfoX > 0 then
    InfoX := Random(MaxInfoX)
  else
    InfoX := 10;

  if MaxInfoY > 0 then
    InfoY := Random(MaxInfoY)
  else
    InfoY := 10;

  Invalidate;
end;
procedure TScreenSaverForm.FormPaint(Sender: TObject);
var
  i, j: Integer;
  InfoHeight, BoxWidth, BoxHeight: Integer;
  InfoRect: TRect;
  FormattedUptime, CurrentDateTime: string;
  FontSize, QuoteFontSize: Integer; // Separate variable for quote font
  LineSpacing: Integer;
  QuoteY, QuoteWidth: Integer;
  HolidayY: Integer;
  InfoLines: array of string;
  MaxWidth, ActualLines: Integer;
  GaugeWidth, GaugeHeight: Integer;
  GaugeRect, FilledRect: TRect;
  FilledWidth: Integer;
  TextWidth: Integer;
  MemPercent, DiskPercent: Integer;
  TextY: Integer;

  // Variables for quote wrapping
  MaxQuoteWidth: Integer;
  Quote: string;
  Words, Lines: TStringList;
  CurrentWord, CurrentLine, TestLine: string;
  LineWidth: Integer;
begin
  // Calculate font size and line spacing early so it's available for quotes and info box
  FontSize := Round(ScreenHeights[FormIndex] * 0.03); // Adjust the multiplier as needed
  QuoteFontSize := Round(ScreenHeights[FormIndex] * 0.015); // Half-sized font for quotes
  LineSpacing := Round(FontSize * 0.4); // Adjust the multiplier as needed

  // Ensure the font size and line spacing are within reasonable limits
  if FontSize < 10 then
    FontSize := 10;
  if FontSize > 24 then
    FontSize := 24;
  if QuoteFontSize < 8 then
    QuoteFontSize := 8;
  if QuoteFontSize > 12 then
    QuoteFontSize := 12;
  if LineSpacing < 2 then
    LineSpacing := 2;
  if LineSpacing > 8 then
    LineSpacing := 8;

  // If fade-in animation is active, apply opacity effect
  if FFadeInOpacity < 255 then
  begin
    // Draw with reduced opacity
    // In a real implementation, we'd use AlphaBlend functions
    // For simplicity, we'll just skip this effect in this example
  end;

  // Draw the American flag background with anti-burn offset
  Canvas.Draw(FlagOffsetX, FlagOffsetY, FlagImage);

  // Draw fireworks if active
  if FFireworksActive then
    DrawFireworks(Canvas);

  // Draw the eagle if visible
  if FEagleVisible then
    DrawEagle(Canvas);

  // Set the background mode to transparent for text rendering
  SetBkMode(Canvas.Handle, TRANSPARENT);

  // Draw the falling text
  Canvas.Font.Assign(Font);

  for i := 0 to High(TextStreams) do
  begin
    for j := 0 to High(TextStreams[i]) do
    begin
      Canvas.Font.Color := TextStreams[i][j].TextColor;
      Canvas.TextOut(TextStreams[i][j].X, TextStreams[i][j].Y, TextStreams[i][j].Char);
    end;
  end;

  // Display the patriotic quote at the bottom of the screen
  if (FQuoteTimer.Enabled) and (FQuotes.Count > 0) and (FCurrentQuote < FQuotes.Count) then
  begin
    Canvas.Font.Size := QuoteFontSize; // Use half-sized font for quotes
    Canvas.Font.Style := [fsBold, fsItalic];
    Canvas.Font.Color := clWhite;
    Canvas.Brush.Style := bsClear;

    // Calculate max width for quotes (80% of screen width)
    MaxQuoteWidth := Round(ScreenWidths[FormIndex] * 0.8);

    // Check if quote needs wrapping
    QuoteWidth := Canvas.TextWidth(FQuotes[FCurrentQuote]);

    if QuoteWidth > MaxQuoteWidth then
    begin
      // Need to wrap the quote
      Quote := FQuotes[FCurrentQuote];
      Words := TStringList.Create;
      try
        // Split the quote into words
        CurrentWord := '';
        for i := 1 to Length(Quote) do
        begin
          if Quote[i] = ' ' then
          begin
            if CurrentWord <> '' then
            begin
              Words.Add(CurrentWord);
              CurrentWord := '';
            end;
          end
          else
            CurrentWord := CurrentWord + Quote[i];
        end;
        if CurrentWord <> '' then
          Words.Add(CurrentWord);

        // Create lines by combining words until max width is reached
        Lines := TStringList.Create;
        try
          CurrentLine := '';
          for i := 0 to Words.Count - 1 do
          begin
            TestLine := CurrentLine;
            if TestLine <> '' then
              TestLine := TestLine + ' ';
            TestLine := TestLine + Words[i];

            if Canvas.TextWidth(TestLine) > MaxQuoteWidth then
            begin
              // Line would be too long, start a new line
              if CurrentLine <> '' then
                Lines.Add(CurrentLine);
              CurrentLine := Words[i];
            end
            else
            begin
              // Add to current line
              CurrentLine := TestLine;
            end;
          end;
          if CurrentLine <> '' then
            Lines.Add(CurrentLine);

          // Draw each line
          QuoteY := ScreenHeights[FormIndex] - 50 - ((Lines.Count - 1) * (Canvas.TextHeight('W') + 5));

          for i := 0 to Lines.Count - 1 do
          begin
            LineWidth := Canvas.TextWidth(Lines[i]);

            // Draw shadow for this line
            Canvas.Font.Color := clBlack;
            Canvas.TextOut((ScreenWidths[FormIndex] - LineWidth) div 2 + 2,
                          QuoteY + 2,
                          Lines[i]);

            // Draw this line
            Canvas.Font.Color := clWhite;
            Canvas.TextOut((ScreenWidths[FormIndex] - LineWidth) div 2,
                          QuoteY,
                          Lines[i]);

            // Move to next line position
            QuoteY := QuoteY + Canvas.TextHeight('W') + 5;
          end;

        finally
          Lines.Free;
        end;

      finally
        Words.Free;
      end;
    end
    else
    begin
      // No wrapping needed, draw as before
      QuoteY := ScreenHeights[FormIndex] - 50;

      // Draw shadow effect for better readability
      Canvas.Font.Color := clBlack;
      Canvas.TextOut((ScreenWidths[FormIndex] - QuoteWidth) div 2 + 2,
                     QuoteY + 2,
                     FQuotes[FCurrentQuote]);

      // Draw the actual text
      Canvas.Font.Color := clWhite;
      Canvas.TextOut((ScreenWidths[FormIndex] - QuoteWidth) div 2,
                     QuoteY,
                     FQuotes[FCurrentQuote]);
    end;
  end;

  // Display holiday message if it's a holiday
  if FIsHoliday then
  begin
    Canvas.Font.Size := FontSize + 4;
    Canvas.Font.Style := [fsBold];
    Canvas.Font.Color := FLAG_RED;
    Canvas.Brush.Style := bsClear;

    HolidayY := 50;

    // Draw shadow for better readability
    Canvas.Font.Color := clBlack;
    Canvas.TextOut((ScreenWidths[FormIndex] - Canvas.TextWidth('Happy ' + FHolidayName)) div 2 + 2,
                  HolidayY + 2,
                  'Happy ' + FHolidayName);

    // Draw the actual text
    Canvas.Font.Color := FLAG_RED;
    Canvas.TextOut((ScreenWidths[FormIndex] - Canvas.TextWidth('Happy ' + FHolidayName)) div 2,
                  HolidayY,
                  'Happy ' + FHolidayName);
  end;

  // Draw the system information box LAST so it's on top of everything
  Canvas.Font.Size := FontSize;

  // Display system information using either text or graphical indicators
  if FUseGraphicalIndicators then
  begin
    // Draw graphical system status indicators
    DrawSystemGauges(Canvas, InfoX, InfoY);
  end
  else
  begin
    // Traditional text-based system info display with smart sizing
    FormattedUptime := FormatUptime(FSystemUptime);
    //CurrentDateTime := GetCurrentDateTimeFormatted;
      CurrentDateTime := FFormattedDateTime;
    // Calculate memory and disk percentages
    MemPercent := MemoryUtilization;
   if FTotalDiskSpace > 0 then
  DiskPercent := (FUsedDiskSpace * 100) div FTotalDiskSpace
else
  DiskPercent := 0;

    // Create an array of all text lines to be displayed
    SetLength(InfoLines, 12); // Allocate for maximum expected lines
    InfoLines[0] := 'System Information';
    InfoLines[1] := 'CPU Utilization: ' + IntToStr(CpuUtilization) + '%';
    InfoLines[2] := 'Memory Utilization: ' + IntToStr(MemoryUtilization) + '%';
    InfoLines[3] := 'Disk Space Free: ' + IntToStr(DiskSpace) + ' GB';
    InfoLines[4] := 'Disk Space Used: ' + IntToStr(FUsedDiskSpace) + ' GB';
    InfoLines[5] := 'Total Physical Memory: ' + IntToStr(FTotalPhysicalMemory) + ' MB';
    InfoLines[6] := 'Available Physical Memory: ' + IntToStr(FAvailablePhysicalMemory) + ' MB';
    InfoLines[7] := 'Ping Time: ' + IntToStr(FPingTime) + ' ms';
    InfoLines[8] := 'System Uptime: ' + FormattedUptime;
    InfoLines[9] := 'Current Date and Time: ' + CurrentDateTime;
    InfoLines[10] := '';  // Removed weather
    InfoLines[11] := 'VONWALLACE.COM';

    // Define gauge dimensions
    GaugeWidth := 100;  // Smaller than in DrawSystemGauges
    GaugeHeight := 15;  // Smaller height for inline display

    // Find the actual number of lines and the maximum width
    ActualLines := 0;
    MaxWidth := 0;
    for i := 0 to High(InfoLines) do
    begin
      if InfoLines[i] <> '' then
      begin
        ActualLines := i + 1;
        // For lines with gauges, add extra width
        if (i = 1) or (i = 2) or (i = 4) then  // CPU, Memory, Disk Used
          TextWidth := Canvas.TextWidth(InfoLines[i]) + GaugeWidth + 20
        else
          TextWidth := Canvas.TextWidth(InfoLines[i]);

        if TextWidth > MaxWidth then
          MaxWidth := TextWidth;
      end;
    end;

    // Calculate dimensions of the information box with some padding
    InfoHeight := Canvas.TextHeight('W');
    BoxWidth := MaxWidth + 60; // Add 60 pixels padding (30 on each side)
    BoxHeight := ActualLines * (InfoHeight + LineSpacing) + 40; // Add 40 pixels padding (20 on top and bottom)

    // Calculate the maximum positions to ensure the box stays within screen bounds
    if InfoX + BoxWidth > ScreenWidths[FormIndex] then
      InfoX := ScreenWidths[FormIndex] - BoxWidth - 20;
    if InfoY + BoxHeight > ScreenHeights[FormIndex] then
      InfoY := ScreenHeights[FormIndex] - BoxHeight - 20;

    // Draw the information box with a semi-transparent blue background
    InfoRect := Rect(InfoX, InfoY, InfoX + BoxWidth, InfoY + BoxHeight);
    Canvas.Brush.Color := FLAG_BLUE;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := FLAG_WHITE;
    Canvas.Rectangle(InfoRect);

    // Draw the system information text inside the box
    Canvas.Font.Color := FLAG_WHITE; // Set text color to white for visibility

    // Set the font to bold for the title
    Canvas.Font.Style := [fsBold];
    Canvas.TextOut(InfoX + 20, InfoY + 20, InfoLines[0]);

    // Set the font to normal for the rest of the information
    Canvas.Font.Style := [];
    for i := 1 to ActualLines - 1 do
    begin
      if InfoLines[i] <> '' then
      begin
        TextY := InfoY + 20 + i * (InfoHeight + LineSpacing);

        // Make VONWALLACE.COM bold
        if i = 11 then
          Canvas.Font.Style := [fsBold]
        else
          Canvas.Font.Style := [];

        Canvas.TextOut(InfoX + 20, TextY, InfoLines[i]);

        // Add gauges for CPU, Memory and Disk Used
        if i = 1 then  // CPU
        begin
          // Calculate position for gauge (after the text)
          GaugeRect := Rect(
            InfoX + BoxWidth - GaugeWidth - 20,
            TextY,
            InfoX + BoxWidth - 20,
            TextY + GaugeHeight
          );

          // Draw gauge background
          Canvas.Brush.Color := $444444; // Dark gray
          Canvas.FillRect(GaugeRect);

          // Draw filled portion
          FilledWidth := (CpuUtilization * GaugeWidth) div 100;
          FilledRect := Rect(
            GaugeRect.Left,
            GaugeRect.Top,
            GaugeRect.Left + FilledWidth,
            GaugeRect.Bottom
          );

          // Set color based on utilization
          if CpuUtilization < 60 then
            Canvas.Brush.Color := clGreen
          else if CpuUtilization < 85 then
            Canvas.Brush.Color := clYellow
          else
            Canvas.Brush.Color := clRed;

          Canvas.FillRect(FilledRect);

          // Draw border
          Canvas.Brush.Style := bsClear;
          Canvas.Pen.Color := clWhite;
          Canvas.Rectangle(GaugeRect);
        end
        else if i = 2 then  // Memory
        begin
          // Similar gauge for memory
          GaugeRect := Rect(
            InfoX + BoxWidth - GaugeWidth - 20,
            TextY,
            InfoX + BoxWidth - 20,
            TextY + GaugeHeight
          );

          Canvas.Brush.Color := $444444;
          Canvas.FillRect(GaugeRect);

          FilledWidth := (MemPercent * GaugeWidth) div 100;
          FilledRect := Rect(
            GaugeRect.Left,
            GaugeRect.Top,
            GaugeRect.Left + FilledWidth,
            GaugeRect.Bottom
          );

          if MemPercent < 60 then
            Canvas.Brush.Color := clGreen
          else if MemPercent < 85 then
            Canvas.Brush.Color := clYellow
          else
            Canvas.Brush.Color := clRed;

          Canvas.FillRect(FilledRect);

          Canvas.Brush.Style := bsClear;
          Canvas.Pen.Color := clWhite;
          Canvas.Rectangle(GaugeRect);
        end
        else if i = 4 then  // Disk Used
        begin
          // Similar gauge for disk
          GaugeRect := Rect(
            InfoX + BoxWidth - GaugeWidth - 20,
            TextY,
            InfoX + BoxWidth - 20,
            TextY + GaugeHeight
          );

          Canvas.Brush.Color := $444444;
          Canvas.FillRect(GaugeRect);

          FilledWidth := (DiskPercent * GaugeWidth) div 100;
          FilledRect := Rect(
            GaugeRect.Left,
            GaugeRect.Top,
            GaugeRect.Left + FilledWidth,
            GaugeRect.Bottom
          );

          if DiskPercent < 60 then
            Canvas.Brush.Color := clGreen
          else if DiskPercent < 85 then
            Canvas.Brush.Color := clYellow
          else
            Canvas.Brush.Color := clRed;

          Canvas.FillRect(FilledRect);

          Canvas.Brush.Style := bsClear;
          Canvas.Pen.Color := clWhite;
          Canvas.Rectangle(GaugeRect);
        end
      end;
    end;
  end;
end;
procedure TScreenSaverForm.WndProc(var Msg: TMessage);
begin
  inherited;
  if Msg.Msg = WM_ACTIVATE then
  begin
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    SetForegroundWindow(Handle);
  end;
end;

function FormatUptime(Uptime: Int64): string;
var
  Days, Hours, Minutes, Seconds: Int64;
begin
  Seconds := Uptime mod 60;
  Uptime := Uptime div 60;
  Minutes := Uptime mod 60;
  Uptime := Uptime div 60;
  Hours := Uptime mod 24;
  Days := Uptime div 24;

  Result := Format('%dd %dh %dm %ds', [Days, Hours, Minutes, Seconds]);
end;

function GetCurrentDateTimeFormatted: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
end;

end.
